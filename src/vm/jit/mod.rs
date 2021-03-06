mod emitter;
mod mem;
mod regalloc;

use crate::ast;
use crate::lir;
use crate::lir::VReg;

use super::builtins;
use super::heap::*;
use super::value;
use super::VMState;
use super::Value;
use emitter::{Reg, Scale, FP, RM, S};
use mem::ExecHeap;

use std::collections::HashMap;

/// Context in which a JIT function is compiled.
/// Contains the executable pages.
pub struct JitContext {
    heap: ExecHeap,
    dump_asm: bool,
}

impl JitContext {
    pub fn new(dump_asm: bool) -> JitContext {
        JitContext {
            heap: ExecHeap::new(),
            dump_asm,
        }
    }

    pub fn compile(
        vm: &mut super::VM,
        function: &lir::Function,
    ) -> Option<fn(*mut VMState) -> Value> {
        let dump = vm.jit.dump_asm;
        let mut jit = Jit::new(vm, function);
        let result = jit.compile()?;
        if dump {
            jit.dump();
        }
        Some(result)
    }
}

/// A Label is used as a placeholder to resolve relocations after generating
/// the code.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Label {
    BasicBlock(lir::BasicBlockIdx),
    Epilogue,
    Error,
}

const REG_STATE: Reg = Reg::RBX;

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum RelocKind {
    Int8,
    Int32,
}

#[derive(Debug)]
struct Reloc {
    /// Location of the source of the Reloc.
    address: usize,

    /// Destination of the jump.
    target: Label,

    kind: RelocKind,
}

impl Reloc {
    pub fn new(address: usize, target: Label, kind: RelocKind) -> Reloc {
        Reloc {
            address,
            target,
            kind,
        }
    }
}

/// Compiles a global function.
/// Construct a new instance for each function you want to compile.
struct Jit<'ctx, 'lir> {
    vm: &'ctx mut super::VM,
    e: emitter::Emitter<'ctx>,
    lir: &'lir lir::Function,
    alloc_map: Vec<regalloc::Slot>,
    /// Mapping from "label name" to address of label (as offset in emitter).
    labels: HashMap<Label, usize>,
    relocs: Vec<Reloc>,
}

impl<'ctx, 'lir> Jit<'_, '_> {
    fn new(
        vm: &'ctx mut super::VM,
        lir: &'lir lir::Function,
    ) -> Jit<'ctx, 'lir> {
        let buf: &'ctx mut [u8] = (&mut vm.jit.heap).alloc(4096);
        let alloc_map = regalloc::reg_alloc(lir);
        Jit {
            vm,
            e: emitter::Emitter::new(&mut *buf),
            lir,
            alloc_map,
            labels: HashMap::new(),
            relocs: vec![],
        }
    }

    pub fn dump(&mut self) {
        use capstone::arch::x86::*;
        use capstone::arch::*;
        use capstone::*;
        let cs = Capstone::new()
            .x86()
            .mode(ArchMode::Mode64)
            .build()
            .unwrap();
        // Two pass dump: first we count the number of instructions,
        // and then actually dump that number, because the capstone API
        // doesn't allow us to dump a certain number of bytes, only
        // a certain number of instructions.
        let instrs = cs.disasm_all(self.e.buf, 0).unwrap();
        let (mut bytes, mut count) = (0, 0);
        for inst in instrs.iter() {
            count += 1;
            bytes += inst.bytes().len();
            if bytes >= self.e.index {
                break;
            }
        }
        println!("{}", cs.disasm_count(self.e.buf, 0, count).unwrap());
    }

    const NUM_SCRATCH_SLOTS: u32 = 1;

    pub fn compile(&mut self) -> Option<fn(*mut VMState) -> Value> {
        self.emit_prologue();
        self.compile_function(self.lir);
        self.emit_epilogue();
        self.resolve_relocs();
        Some(unsafe { std::mem::transmute(self.e.buf.as_ptr()) })
    }

    fn compile_function(&mut self, function: &lir::Function) {
        for (idx, block) in function.blocks().iter().enumerate() {
            self.compile_block(block, lir::BasicBlockIdx(idx));
        }
    }

    fn compile_block(
        &mut self,
        block: &lir::BasicBlock,
        idx: lir::BasicBlockIdx,
    ) {
        self.labels.insert(Label::BasicBlock(idx), self.e.index);
        for inst in block.insts() {
            self.compile_inst(inst);
        }
    }

    fn compile_inst(&mut self, inst: &lir::Inst) {
        use lir::Opcode::*;
        match &inst.opcode {
            Mov(dst, src) => {
                self.mov_vreg_vreg(*dst, *src);
            }
            Ret(reg) => {
                self.mov_reg_vreg(Reg::RAX, *reg);
                self.e.jmp(emitter::OffsetType::Int32, 0);
                self.relocs.push(Reloc::new(
                    self.e.get_index() - 4,
                    Label::Epilogue,
                    RelocKind::Int32,
                ));
            }
            LoadBool(vreg, val) => {
                self.mov_vreg_imm(*vreg, Value::bool(*val));
            }
            LoadNil(reg) => {
                self.mov_vreg_imm(*reg, Value::nil());
            }
            LoadNumber(reg, n) => {
                self.mov_vreg_imm(*reg, Value::number(*n));
            }
            LoadString(reg, unique_string) => {
                let s: &str = &*unique_string.clone();
                let ptr = LoxString::new(&mut self.vm.state.heap, s);
                let idx: u32 = self.vm.state.string_table.add(ptr);
                self.e.mov_reg_reg(S::Q, Reg::RDI, REG_STATE);
                self.e
                    .mov_reg_imm(Reg::RSI, Value::number(idx as f64).raw());
                self.call_builtin(builtins::load_loxstring);
                self.mov_vreg_reg(*reg, Reg::RAX);
            }
            &Add(dst, r1, r2) => {
                self.mov_vreg_vreg(dst, r1);
                self.binop_vreg_vreg(ast::BinOpKind::Add, dst, r2);
            }
            &Sub(dst, r1, r2) => {
                self.mov_vreg_vreg(dst, r1);
                self.binop_vreg_vreg(ast::BinOpKind::Sub, dst, r2);
            }
            &Mul(dst, r1, r2) => {
                self.mov_vreg_vreg(dst, r1);
                self.binop_vreg_vreg(ast::BinOpKind::Mul, dst, r2);
            }
            &Div(dst, r1, r2) => {
                self.mov_vreg_vreg(dst, r1);
                self.binop_vreg_vreg(ast::BinOpKind::Div, dst, r2);
            }
            &Not(dst, src) => {
                self.e.mov_reg_reg(S::Q, Reg::RDI, REG_STATE);
                self.mov_reg_vreg(Reg::RSI, src);
                self.call_builtin(builtins::to_bool);
                self.e.xor_reg_imm(S::B, Reg::AL, 1i8);
                self.mov_vreg_reg(dst, Reg::RAX);
            }
            &Equal(dst, r1, r2) => {
                self.eq_vreg_vreg(emitter::CCode::E, dst, r1, r2);
            }
            &Less(dst, r1, r2) => {
                self.mov_vreg_vreg(dst, r1);
                self.compare_vreg_vreg(emitter::CCode::B, dst, r2);
            }
            &LessEqual(dst, r1, r2) => {
                self.mov_vreg_vreg(dst, r1);
                self.compare_vreg_vreg(emitter::CCode::BE, dst, r2);
            }
            &Greater(dst, r1, r2) => {
                self.mov_vreg_vreg(dst, r1);
                self.compare_vreg_vreg(emitter::CCode::A, dst, r2);
            }
            &GreaterEqual(dst, r1, r2) => {
                self.mov_vreg_vreg(dst, r1);
                self.compare_vreg_vreg(emitter::CCode::AE, dst, r2);
            }
            &Print(reg) => {
                self.e.mov_reg_reg(S::Q, Reg::RDI, REG_STATE);
                self.mov_reg_vreg(Reg::RSI, reg);
                self.call_builtin(builtins::println);
            }
            &Branch(bb) => {
                self.e.jmp(emitter::OffsetType::Int32, 0);
                self.relocs.push(Reloc::new(
                    self.e.get_index() - 4,
                    Label::BasicBlock(bb),
                    RelocKind::Int32,
                ));
            }
            &CondBranch(cond, bb_true, bb_false) => {
                self.e.mov_reg_reg(S::Q, Reg::RDI, REG_STATE);
                self.mov_reg_vreg(Reg::RSI, cond);
                self.call_builtin(builtins::to_bool);

                // Check bottom bits of rax to see if the bool is true or false.
                self.e.test_reg_reg(S::B, Reg::AL, Reg::AL);
                self.e
                    .cjump(emitter::CCode::E, emitter::OffsetType::Int32, 0);
                self.relocs.push(Reloc::new(
                    self.e.get_index() - 4,
                    Label::BasicBlock(bb_false),
                    RelocKind::Int32,
                ));

                self.e.jmp(emitter::OffsetType::Int32, 0);
                self.relocs.push(Reloc::new(
                    self.e.get_index() - 4,
                    Label::BasicBlock(bb_true),
                    RelocKind::Int32,
                ));
            }
            _ => unimplemented!("{:?}", &inst),
        };
    }

    /// Return the register allocated Slot for a given VReg.
    fn slot(&self, vreg: VReg) -> regalloc::Slot {
        self.alloc_map[vreg.0 as usize]
    }

    /// Return the offset for a stack slot used for scratch.
    /// `slot` is the index of the scratch slot.
    fn scratch_disp(&self, slot: u32) -> i32 {
        // Go one below because the stack grows up.
        (slot as i32 + 1) * -8
    }

    /// Return the RM for a register allocated slot, accounting for the offset
    /// imposed by scratch registers.
    fn stack_slot(&self, slot: u32) -> RM {
        (
            Reg::RBP,
            Reg::NoIndex,
            self.scratch_disp(slot + Self::NUM_SCRATCH_SLOTS),
        )
    }

    fn mov_vreg_vreg(&mut self, dst: VReg, src: VReg) {
        use regalloc::Slot;
        if dst == src {
            return;
        }
        match (self.slot(dst), self.slot(src)) {
            (Slot::Reg(dst), Slot::Reg(src)) => {
                self.e.mov_reg_reg(S::Q, dst, src)
            }
            (Slot::Reg(dst), Slot::Stack(src)) => self.e.mov_reg_rm(
                S::Q,
                Scale::NoScale,
                dst,
                self.stack_slot(src),
            ),
            (Slot::Stack(dst), Slot::Reg(src)) => self.e.mov_rm_reg(
                S::Q,
                Scale::NoScale,
                self.stack_slot(dst),
                src,
            ),
            (Slot::Stack(_), Slot::Stack(_)) => {
                self.mov_reg_vreg(Reg::R11, src);
                self.mov_vreg_reg(dst, Reg::R11);
            }
        }
    }

    fn eq_vreg_vreg(
        &mut self,
        op: emitter::CCode,
        dst: VReg,
        r1: VReg,
        r2: VReg,
    ) {
        use regalloc::Slot;
        match self.slot(dst) {
            Slot::Reg(reg) => {
                self.setup_bool(reg);
            }
            Slot::Stack(_) => {
                self.setup_bool(Reg::R12);
            }
        };
        match (self.slot(r1), self.slot(r2)) {
            (Slot::Reg(r1), Slot::Reg(r2)) => {
                self.e.cmp_reg_reg(S::Q, r1, r2);
                self.e.cset(op, Reg::AL);
            }
            (Slot::Reg(r1), Slot::Stack(r2)) => {
                self.e.cmp_reg_rm(
                    S::Q,
                    Scale::NoScale,
                    r1,
                    self.stack_slot(r2),
                );
            }
            (Slot::Stack(r1), Slot::Reg(r2)) => {
                self.e.cmp_rm_reg(
                    S::Q,
                    Scale::NoScale,
                    self.stack_slot(r1),
                    r2,
                );
            }
            (Slot::Stack(r1), Slot::Stack(r2)) => {
                self.e.mov_reg_rm(
                    S::Q,
                    Scale::NoScale,
                    Reg::R11,
                    self.stack_slot(r2),
                );
                self.e.cmp_rm_reg(
                    S::Q,
                    Scale::NoScale,
                    self.stack_slot(r1),
                    Reg::R11,
                );
            }
        };
        match self.slot(dst) {
            Slot::Reg(reg) => {
                self.e.cset(emitter::CCode::E, reg.low_byte());
            }
            Slot::Stack(_) => {
                self.e.cset(emitter::CCode::E, Reg::R12.low_byte());
                self.mov_vreg_reg(dst, Reg::R12);
            }
        }
    }

    fn compare_vreg_vreg(&mut self, op: emitter::CCode, dst: VReg, src: VReg) {
        use regalloc::Slot;
        match self.slot(dst) {
            Slot::Reg(reg) => {
                self.need_number(reg);
                self.fp_from_reg(FP::Double, Reg::XMM0, reg);
                self.setup_bool(reg);
            }
            Slot::Stack(slot) => {
                self.mov_reg_vreg(Reg::R11, dst);
                self.need_number(Reg::R11);
                self.e.mov_fp_rm(
                    FP::Double,
                    Scale::NoScale,
                    Reg::XMM0,
                    self.stack_slot(slot),
                );
                self.setup_bool(Reg::R12);
            }
        }
        let rm = match self.slot(src) {
            Slot::Reg(reg) => {
                self.need_number(reg);
                self.e.mov_rm_reg(
                    S::Q,
                    Scale::NoScale,
                    (Reg::RBP, Reg::NoIndex, self.scratch_disp(0)),
                    reg,
                );
                (Reg::RBP, Reg::NoIndex, self.scratch_disp(0))
            }
            Slot::Stack(slot) => {
                self.mov_reg_vreg(Reg::R11, src);
                self.need_number(Reg::R11);
                self.stack_slot(slot)
            }
        };
        self.e.ucomisd_reg_rm(Reg::XMM0, rm);
        match self.slot(dst) {
            Slot::Reg(reg) => {
                self.e.cset(op, reg.low_byte());
            }
            Slot::Stack(_) => {
                self.e.cset(op, Reg::R12.low_byte());
                self.mov_vreg_reg(dst, Reg::R12);
            }
        }
    }

    fn binop_vreg_vreg(&mut self, kind: ast::BinOpKind, dst: VReg, src: VReg) {
        use regalloc::Slot;
        match self.slot(dst) {
            Slot::Reg(reg) => {
                self.need_number(reg);
                self.fp_from_reg(FP::Double, Reg::XMM0, reg);
            }
            Slot::Stack(slot) => {
                self.mov_reg_vreg(Reg::R11, dst);
                self.need_number(Reg::R11);
                self.e.mov_fp_rm(
                    FP::Double,
                    Scale::NoScale,
                    Reg::XMM0,
                    self.stack_slot(slot),
                );
            }
        }
        let rm = match self.alloc_map[src.0 as usize] {
            Slot::Reg(reg) => {
                self.need_number(reg);
                self.e.mov_rm_reg(
                    S::Q,
                    Scale::NoScale,
                    (Reg::RBP, Reg::NoIndex, self.scratch_disp(0)),
                    reg,
                );
                (Reg::RBP, Reg::NoIndex, self.scratch_disp(0))
            }
            Slot::Stack(slot) => {
                self.mov_reg_vreg(Reg::R11, src);
                self.need_number(Reg::R11);
                self.stack_slot(slot)
            }
        };
        match kind {
            ast::BinOpKind::Add => {
                self.e.add_fp_rm(FP::Double, Scale::NoScale, Reg::XMM0, rm)
            }
            ast::BinOpKind::Sub => {
                self.e.sub_fp_rm(FP::Double, Scale::NoScale, Reg::XMM0, rm)
            }
            ast::BinOpKind::Mul => {
                self.e.mul_fp_rm(FP::Double, Scale::NoScale, Reg::XMM0, rm)
            }
            ast::BinOpKind::Div => {
                self.e.div_fp_rm(FP::Double, Scale::NoScale, Reg::XMM0, rm)
            }
            _ => unimplemented!("{:?}", kind),
        };
        match self.alloc_map[dst.0 as usize] {
            Slot::Reg(reg) => {
                self.reg_from_fp(FP::Double, reg, Reg::XMM0);
            }
            Slot::Stack(slot) => {
                self.e.mov_rm_fp(
                    FP::Double,
                    Scale::NoScale,
                    self.stack_slot(slot),
                    Reg::XMM0,
                );
            }
        }
    }

    fn mov_vreg_imm(&mut self, vreg: VReg, imm: Value) {
        use regalloc::Slot;
        match self.alloc_map[vreg.0 as usize] {
            Slot::Reg(dst) => {
                self.e.mov_reg_imm(dst, imm.raw());
            }
            Slot::Stack(slot) => {
                self.e.mov_reg_imm(Reg::R11, imm.raw());
                self.e.mov_rm_reg(
                    S::Q,
                    Scale::NoScale,
                    self.stack_slot(slot),
                    Reg::R11,
                );
            }
        };
    }

    fn mov_reg_vreg(&mut self, dst: Reg, src: VReg) {
        use regalloc::Slot;
        match self.alloc_map[src.0 as usize] {
            Slot::Reg(reg) => {
                self.e.mov_reg_reg(S::Q, dst, reg);
            }
            Slot::Stack(slot) => {
                self.e.mov_reg_rm(
                    S::Q,
                    Scale::NoScale,
                    dst,
                    self.stack_slot(slot),
                );
            }
        };
    }

    fn mov_vreg_reg(&mut self, dst: VReg, src: Reg) {
        use regalloc::Slot;
        match self.alloc_map[dst.0 as usize] {
            Slot::Reg(dst) => {
                self.e.mov_reg_reg(S::Q, dst, src);
            }
            Slot::Stack(slot) => {
                self.e.mov_rm_reg(
                    S::Q,
                    Scale::NoScale,
                    self.stack_slot(slot),
                    src,
                );
            }
        };
    }

    fn emit_prologue(&mut self) {
        self.e.pushq(Reg::RBP);
        self.e.mov_reg_reg(S::Q, Reg::RBP, Reg::RSP);

        self.e.pushq(REG_STATE);
        self.e.pushq(Reg::RCX);

        self.e.mov_reg_reg(S::Q, REG_STATE, Reg::RDI);

        // Make enough space to spill every local.
        // Stack pointers must always be 16-byte aligned.
        // We pushed rbp, State, and RCX manually,
        // and the return address was pushed as part of the call to get
        // into this code, so we're aligned prior to subtracting for
        // variable space.
        self.e.sub_reg_imm(
            S::Q,
            Reg::RSP,
            align(
                (Self::NUM_SCRATCH_SLOTS + self.lir.get_stack_size()) * 8,
                16,
            ),
        );
    }

    fn emit_epilogue(&mut self) {
        self.e.jmp(emitter::OffsetType::Int8, 0x00);
        self.relocs.push(Reloc::new(
            self.e.get_index() - 1,
            Label::Epilogue,
            RelocKind::Int8,
        ));
        self.labels.insert(Label::Error, self.e.get_index());
        self.e.mov_reg_imm(Reg::RDI, Value::bool(true).raw());
        self.e.mov_rm_reg(
            S::Q,
            Scale::NoScale,
            (
                REG_STATE,
                Reg::NoIndex,
                offset_of!(VMState, thrown_value) as i32,
            ),
            Reg::RDI,
        );
        self.labels.insert(Label::Epilogue, self.e.get_index());
        self.e.popq(Reg::RCX);
        self.e.popq(REG_STATE);
        self.e.leave();
        self.e.ret();
    }

    fn need_number(&mut self, src: Reg) {
        self.e.mov_rm_reg(
            S::Q,
            Scale::NoScale,
            (Reg::RBP, Reg::NoIndex, self.scratch_disp(0)),
            src,
        );
        self.e.cmp_rm_imm::<u32>(
            S::L,
            Scale::NoScale,
            (Reg::RBP, Reg::NoIndex, self.scratch_disp(0) + 4),
            (value::LAST_TAG << (value::NUM_DATA_BITS - 32)) as u32,
        );
        self.e
            .cjump(emitter::CCode::NB, emitter::OffsetType::Int32, 0);
        self.relocs.push(Reloc::new(
            self.e.get_index() - 4,
            Label::Error,
            RelocKind::Int32,
        ));
    }

    #[allow(dead_code)]
    fn fp_from_reg(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(dst.is_fp() && !src.is_fp());
        let rm = (Reg::RBP, Reg::NoIndex, self.scratch_disp(0));
        self.e.mov_rm_reg(S::Q, Scale::NoScale, rm, src);
        self.e.mov_fp_rm(fp, Scale::NoScale, dst, rm);
    }

    #[allow(dead_code)]
    fn reg_from_fp(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(!dst.is_fp() && src.is_fp());
        let rm = (Reg::RBP, Reg::NoIndex, self.scratch_disp(0));
        self.e.mov_rm_fp(fp, Scale::NoScale, rm, src);
        self.e.mov_reg_rm(S::Q, Scale::NoScale, dst, rm);
    }

    fn setup_bool(&mut self, reg: Reg) {
        // clear reg and store the result
        self.e.xor_reg_reg(S::Q, reg, reg);
        // Store the tag in the top 32 bits.
        self.e.mov_reg_imm::<u32>(
            reg.low_word(),
            (value::BOOL_TAG << (value::NUM_DATA_BITS - 32)) as u32,
        );
        self.e.shl_reg_imm(S::Q, reg, 32);
    }

    fn call_builtin(&mut self, func: builtins::BuiltinFunc) {
        self.e.mov_reg_imm(Reg::RAX, builtins::addr(func));
        self.e.call_reg(Reg::RAX);
    }

    fn resolve_relocs(&mut self) {
        let saved_index = self.e.get_index();
        for reloc in &self.relocs {
            self.e.seek(reloc.address);
            let offset: i32 =
                self.labels[&reloc.target] as i32 - reloc.address as i32;
            match reloc.kind {
                RelocKind::Int8 => self.e.emit_imm((offset - 1) as i8),
                RelocKind::Int32 => self.e.emit_imm((offset - 4) as i32),
            };
        }
        self.relocs.clear();
        self.e.seek(saved_index);
    }
}

/// Align `x` to `to`; set it to the smallest multiple of `to` that is >= `x`.
fn align(x: u32, to: u32) -> u32 {
    (x + (to - 1)) & !(to - 1)
}
