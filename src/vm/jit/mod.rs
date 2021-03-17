mod emitter;
mod mem;
mod regalloc;

use crate::ast;
use crate::ctx::UniqueString;
use crate::lir;
use crate::lir::VReg;
use crate::sem::SemInfo;

use super::builtins;
use super::heap::*;
use super::value;
use super::VMState;
use super::Value;
use emitter::{Reg, Scale, FP, S};
use mem::ExecHeap;

use std::collections::HashMap;

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

type Label = u32;
const EPILOGUE_LABEL: Label = Label::max_value();
const ERROR_LABEL: Label = EPILOGUE_LABEL - 1;

const REG_STATE: Reg = Reg::RBX;

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum RelocKind {
    Int8,
    Int32,
}

#[derive(Debug)]
struct Reloc {
    address: usize,
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
        self.compile_block(function.get_entry_block());
    }

    fn compile_block(&mut self, block: &lir::BasicBlock) {
        for inst in block.insts() {
            self.compile_inst(inst);
        }
    }

    fn compile_inst(&mut self, inst: &lir::Inst) {
        use lir::Opcode::*;
        match inst.opcode {
            Mov(dst, src) => {
                self.mov_vreg_vreg(dst, src);
            }
            Ret(reg) => {
                self.mov_reg_vreg(Reg::RAX, reg);
            }
            LoadBool(vreg, val) => {
                self.mov_vreg_imm(vreg, Value::bool(val));
            }
            LoadNil(reg) => {
                self.mov_vreg_imm(reg, Value::nil());
            }
            LoadNumber(reg, n) => {
                self.mov_vreg_imm(reg, Value::number(n));
            }
            Print(reg) => {
                self.e.mov_reg_reg(S::Q, Reg::RDI, REG_STATE);
                self.mov_reg_vreg(Reg::RSI, reg);
                self.call_builtin(builtins::println);
            }
            _ => unimplemented!("{:?}", &inst),
        };
    }

    fn compile_decl(&mut self, decl: &ast::Decl) {
        unimplemented!();
        // use ast::*;
        // match &decl.kind {
        //     DeclKind::Stmt(stmt) => self.compile_stmt(&stmt),
        //     DeclKind::Var(name, expr) => {
        //         let disp = self.var_disp(name);
        //         if let Some(e) = expr {
        //             self.compile_expr(&e);
        //             self.e.mov_rm_reg(
        //                 S::Q,
        //                 Scale::NoScale,
        //                 (Reg::RBP, Reg::NoIndex, disp),
        //                 Reg::RAX,
        //             );
        //         } else {
        //             self.e.mov_reg_imm(Reg::RAX, Value::nil().raw());
        //             self.e.mov_rm_reg(
        //                 S::Q,
        //                 Scale::NoScale,
        //                 (Reg::RBP, Reg::NoIndex, disp),
        //                 Reg::RAX,
        //             );
        //         }
        //     }
        // };
    }

    fn compile_stmt(&mut self, stmt: &ast::Stmt) {
        unimplemented!();
        // use ast::*;
        // match &stmt.kind {
        //     StmtKind::Expr(expr) => self.compile_expr(&expr),
        //     StmtKind::Print(expr) => {
        //         self.compile_expr(&expr);
        //         self.e.mov_reg_reg(S::Q, Reg::RDI, REG_STATE);
        //         self.e.mov_reg_reg(S::Q, Reg::RSI, Reg::RAX);
        //         self.call_builtin(builtins::println);
        //     }
        //     StmtKind::While(cond, body) => {
        //         let top_label = self.new_label();

        //         // rax <- to_bool(cond)
        //         self.compile_expr(&cond);
        //         self.e.mov_reg_reg(S::Q, Reg::RDI, REG_STATE);
        //         self.e.mov_reg_reg(S::Q, Reg::RSI, Reg::RAX);
        //         self.call_builtin(builtins::to_bool);

        //         // Check bottom bits of rax to see if the bool is true or false.
        //         self.e.test_reg_reg(S::B, Reg::AL, Reg::AL);
        //         self.e.cjump(
        //             emitter::CCode::E,
        //             emitter::OffsetType::Int32,
        //             0.into(),
        //         );
        //         let branch_addr = self.e.get_index();

        //         self.compile_block(&body);

        //         // Bottom of the loop, jump back up to the top to check
        //         // the condition again.
        //         self.e.jmp(emitter::OffsetType::Int32, 0.into());
        //         self.relocs.push(Reloc::new(
        //             self.e.get_index() - 4,
        //             top_label,
        //             RelocKind::Int32,
        //         ));

        //         // Done with the loop if we jumped here, now we can emplace
        //         // the reloc for the top of the loop.
        //         let done_label = self.new_label();
        //         self.relocs.push(Reloc::new(
        //             branch_addr - 4,
        //             done_label,
        //             RelocKind::Int32,
        //         ));
        //     }
        //     other => unimplemented!("{:?}", other),
        // };
    }

    /// Compile the expression and place the result in rax.
    fn compile_expr(&mut self, expr: &ast::Expr) {
        unimplemented!();
        // use ast::ExprKind::*;
        // match &expr.kind {
        //     Assign(left, right) => match &left.kind {
        //         Ident(name) => {
        //             let disp = self.var_disp(&name);
        //             self.compile_expr(&right);
        //             self.e.mov_rm_reg(
        //                 S::Q,
        //                 Scale::NoScale,
        //                 (Reg::RBP, Reg::NoIndex, disp),
        //                 Reg::RAX,
        //             );
        //         }
        //         _ => unreachable!("invalid ast"),
        //     },
        //     &BoolLiteral(b) => {
        //         self.e.mov_reg_imm(Reg::RAX, Value::bool(b).raw());
        //     }
        //     &NumberLiteral(x) => {
        //         self.e.mov_reg_imm(Reg::RAX, Value::number(x).raw());
        //     }
        //     StringLiteral(id) => {
        //         let s: &str = &*id;
        //         let ptr = LoxString::new(&mut self.vm.state.heap, s);
        //         let idx: u32 = self.vm.state.string_table.add(ptr);
        //         self.e.mov_reg_reg(S::Q, Reg::RDI, REG_STATE);
        //         self.e
        //             .mov_reg_imm(Reg::RSI, Value::number(idx as f64).raw());
        //         self.call_builtin(builtins::load_loxstring);
        //     }
        //     BinOp(op, x, y) => {
        //         self.compile_binop(*op, x, y);
        //     }
        //     Ident(name) => {
        //         let disp = self.var_disp(&name);
        //         self.e.mov_reg_rm(
        //             S::Q,
        //             Scale::NoScale,
        //             Reg::RAX,
        //             (Reg::RBP, Reg::NoIndex, disp),
        //         );
        //     }
        //     _ => unimplemented!(),
        // }
    }

    fn compile_binop(
        &mut self,
        op: ast::BinOpKind,
        x: &'lir ast::Expr,
        y: &'lir ast::Expr,
    ) {
        unimplemented!();
        // use ast::BinOpKind::*;
        // match op {
        //     Add | Sub | Mul | Div => {
        //         // Use the stack as temporary storage to do:
        //         // xmm0 <- check_number!(x)
        //         // [rbp - scratch(0)] <- check_number!(y)
        //         // op xmm0, xmm1
        //         self.compile_expr(&x);
        //         self.need_number(Reg::RAX);
        //         self.e.pushq(Reg::RAX);
        //         self.compile_expr(&y);
        //         self.need_number(Reg::RAX);
        //         self.e.mov_fp_rm(
        //             FP::Double,
        //             Scale::NoScale,
        //             Reg::XMM0,
        //             (Reg::RSP, Reg::NoIndex, 0),
        //         );
        //         let y_rm = (Reg::RBP, Reg::NoIndex, self.scratch_disp(0));
        //         self.e.mov_rm_reg(S::Q, Scale::NoScale, y_rm, Reg::RAX);
        //         self.e.popq(Reg::RAX);
        //         match op {
        //             Add => self.e.add_fp_rm(
        //                 FP::Double,
        //                 Scale::NoScale,
        //                 Reg::XMM0,
        //                 y_rm,
        //             ),
        //             Sub => self.e.sub_fp_rm(
        //                 FP::Double,
        //                 Scale::NoScale,
        //                 Reg::XMM0,
        //                 y_rm,
        //             ),
        //             Mul => self.e.mul_fp_rm(
        //                 FP::Double,
        //                 Scale::NoScale,
        //                 Reg::XMM0,
        //                 y_rm,
        //             ),
        //             Div => self.e.div_fp_rm(
        //                 FP::Double,
        //                 Scale::NoScale,
        //                 Reg::XMM0,
        //                 y_rm,
        //             ),
        //             _ => unreachable!(),
        //         }
        //         self.reg_from_fp(FP::Double, Reg::RAX, Reg::XMM0);
        //     }
        //     Equal | NotEqual => {
        //         // rbx <- x
        //         // rcx <- y
        //         self.compile_expr(&x);
        //         self.e.pushq(Reg::RAX);
        //         self.compile_expr(&y);
        //         self.e.mov_reg_reg(S::Q, Reg::RCX, Reg::RAX);
        //         self.e.popq(Reg::RBX);
        //         self.setup_bool(Reg::RAX);
        //         self.e.cmp_rm_reg(
        //             S::Q,
        //             Scale::RegScale,
        //             (Reg::RBX, Reg::NoIndex, 0),
        //             Reg::RCX,
        //         );
        //         let ccode = match op {
        //             Equal => emitter::CCode::E,
        //             NotEqual => emitter::CCode::NE,
        //             _ => unreachable!(),
        //         };
        //         self.e.cset(ccode, Reg::AL);
        //     }
        //     Greater | GreaterEqual | Less | LessEqual => {
        //         // xmm0 <- check_number!(x)
        //         // [rbp - scratch(0)] <- check_number!(y)
        //         self.compile_expr(&x);
        //         self.need_number(Reg::RAX);
        //         self.e.pushq(Reg::RAX);
        //         self.compile_expr(&y);
        //         self.need_number(Reg::RAX);
        //         self.e.mov_fp_rm(
        //             FP::Double,
        //             Scale::NoScale,
        //             Reg::XMM0,
        //             (Reg::RSP, Reg::NoIndex, 0),
        //         );
        //         let y_rm = (Reg::RBP, Reg::NoIndex, self.scratch_disp(0));
        //         self.e.mov_rm_reg(S::Q, Scale::NoScale, y_rm, Reg::RAX);
        //         self.e.popq(Reg::RAX);
        //         self.setup_bool(Reg::RAX);
        //         self.e.ucomisd_reg_rm(Reg::XMM0, y_rm);
        //         let ccode = match op {
        //             Greater => emitter::CCode::A,
        //             GreaterEqual => emitter::CCode::AE,
        //             Less => emitter::CCode::B,
        //             LessEqual => emitter::CCode::BE,
        //             _ => unreachable!(),
        //         };
        //         self.e.cset(ccode, Reg::AL);
        //     }
        // }
    }

    fn num_vars(&self) -> u32 {
        unimplemented!();
    }

    fn scratch_disp(&self, slot: u32) -> i32 {
        // Go one below because the stack grows up.
        (slot as i32 + 1) * -8
    }

    /// Return the offset from rbp at which the variable called `name`
    /// can be found. This is a negative integer, and multiple of 8.
    fn var_disp(&self, name: &UniqueString) -> i32 {
        unimplemented!();
        // let slot = self.sem.find_var(name).unwrap() as i32;
        // // Go one below because the stack grows up.
        // ((Self::NUM_SCRATCH_SLOTS as i32 + slot) + 1) * -8
    }

    fn mov_vreg_vreg(&mut self, dst: VReg, src: VReg) {
        self.mov_reg_vreg(Reg::R11, src);
        self.mov_vreg_reg(dst, Reg::R11);
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
                    (Reg::RBP, Reg::NoIndex, self.scratch_disp(slot)),
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
                    (Reg::RBP, Reg::NoIndex, self.scratch_disp(slot)),
                );
            }
        };
    }

    fn mov_vreg_reg(&mut self, dst: VReg, src: Reg) {
        use regalloc::Slot;
        match self.alloc_map[dst.0 as usize] {
            Slot::Reg(_reg) => unimplemented!(),
            Slot::Stack(slot) => {
                self.e.mov_rm_reg(
                    S::Q,
                    Scale::NoScale,
                    (Reg::RBP, Reg::NoIndex, self.scratch_disp(slot)),
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
            EPILOGUE_LABEL,
            RelocKind::Int8,
        ));
        self.labels.insert(ERROR_LABEL, self.e.get_index());
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
        self.labels.insert(EPILOGUE_LABEL, self.e.get_index());
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
            ERROR_LABEL,
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
        // clear rax and store the result
        self.e.xor_reg_reg(S::Q, reg, reg);
        // Store the tag in the top 32 bits.
        self.e.mov_reg_imm::<u32>(
            Reg::EAX,
            (value::BOOL_TAG << (value::NUM_DATA_BITS - 32)) as u32,
        );
        self.e.shl_reg_imm(S::Q, reg, 32);
    }

    fn call_builtin(&mut self, func: builtins::BuiltinFunc) {
        self.e.mov_reg_imm(Reg::RAX, builtins::addr(func));
        self.e.call_reg(Reg::RAX);
    }

    fn new_label(&mut self) -> Label {
        let label: Label = self.labels.len() as Label;
        self.labels.insert(label, self.e.get_index());
        label
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
