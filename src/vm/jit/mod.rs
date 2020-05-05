mod emitter;
mod mem;

use crate::ast;
use crate::ctx::UniqueString;
use crate::sem::SemInfo;

use super::builtins;
use super::Value;
use emitter::{Reg, Scale, FP, S};
use mem::ExecHeap;

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
        &mut self,
        ast: &ast::Func,
        sem: &SemInfo,
    ) -> Option<fn() -> Value> {
        let dump = self.dump_asm;
        let mut jit = Jit::new(self, ast, sem);
        let result = jit.compile()?;
        if dump {
            jit.dump();
        }
        Some(result)
    }
}

/// Compiles a global function.
/// Construct a new instance for each function you want to compile.
struct Jit<'ctx, 'ast> {
    e: emitter::Emitter<'ctx>,
    file: &'ast ast::Func,
    sem: &'ast SemInfo,
}

impl<'ctx, 'ast> Jit<'_, '_> {
    fn new(
        ctx: &'ctx mut JitContext,
        file: &'ast ast::Func,
        sem: &'ast SemInfo,
    ) -> Jit<'ctx, 'ast> {
        let buf: &'ctx mut [u8] = (&mut ctx.heap).alloc(4096);
        Jit {
            e: emitter::Emitter::new(&mut *buf),
            file,
            sem,
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
        println!(
            "{}",
            cs.disasm_count(self.e.buf, 0, count).unwrap()
        );
    }

    const NUM_SCRATCH_SLOTS: u32 = 1;

    pub fn compile(&mut self) -> Option<fn() -> Value> {
        use ast::*;
        self.emit_prologue();
        for decl in &self.file.decls {
            match &decl.kind {
                DeclKind::Stmt(stmt) => match &stmt.kind {
                    StmtKind::Expr(expr) => self.compile_expr(&expr),
                    StmtKind::Print(expr) => {
                        self.compile_expr(&expr);
                        self.e.mov_reg_reg(S::Q, Reg::RDI, Reg::RAX);
                        self.call_builtin(builtins::println);
                    }
                    _ => unimplemented!(),
                },
                DeclKind::Var(name, expr) => {
                    let disp = self.var_disp(name);
                    if let Some(e) = expr {
                        self.compile_expr(&e);
                        self.e.mov_rm_reg(
                            S::Q,
                            Scale::NoScale,
                            (Reg::RBP, Reg::NoIndex, disp),
                            Reg::RAX,
                        );
                    } else {
                        self.e.mov_reg_imm(Reg::RAX, Value::nil().raw());
                        self.e.mov_rm_reg(
                            S::Q,
                            Scale::NoScale,
                            (Reg::RBP, Reg::NoIndex, disp),
                            Reg::RAX,
                        );
                    }
                }
            };
        }
        self.emit_epilogue();
        Some(unsafe { std::mem::transmute(self.e.buf.as_ptr()) })
    }

    /// Compile the expression and place the result in rax.
    fn compile_expr(&mut self, expr: &ast::Expr) {
        use ast::ExprKind::*;
        match &expr.kind {
            Assign(left, right) => match &left.kind {
                Ident(name) => {
                    let disp = self.var_disp(&name);
                    self.compile_expr(&right);
                    self.e.mov_rm_reg(
                        S::Q,
                        Scale::NoScale,
                        (Reg::RBP, Reg::NoIndex, disp),
                        Reg::RAX,
                    );
                }
                _ => unreachable!("invalid ast"),
            },
            &BoolLiteral(b) => {
                self.e
                    .mov_reg_imm(Reg::RAX, Value::bool(b).raw());
            }
            &NumberLiteral(x) => {
                self.e
                    .mov_reg_imm(Reg::RAX, Value::number(x).raw());
            }
            BinOp(op, x, y) => {
                // Use rbx as temporary storage (consider it "callee-saved"
                // in some sense).
                // xmm0 <- to_number(x)
                // xmm1 <- to_number(y)
                // op xmm0, xmm1
                use ast::BinOpKind::*;
                self.compile_expr(&x);
                self.e.pushq(Reg::RAX);
                self.compile_expr(&y);
                self.e.popq(Reg::RBX);
                self.fp_from_reg(FP::Double, Reg::XMM0, Reg::RBX);
                self.fp_from_reg(FP::Double, Reg::XMM1, Reg::RAX);
                match op {
                    Add => self.e
                        .add_fp_fp(FP::Double, Reg::XMM0, Reg::XMM1),
                    Sub => self.e
                        .sub_fp_fp(FP::Double, Reg::XMM0, Reg::XMM1),
                    Mul => self.e
                        .mul_fp_fp(FP::Double, Reg::XMM0, Reg::XMM1),
                    Div => self.e
                        .div_fp_fp(FP::Double, Reg::XMM0, Reg::XMM1),
                }
                self.reg_from_fp(FP::Double, Reg::RAX, Reg::XMM0);
            }
            Ident(name) => {
                let disp = self.var_disp(&name);
                self.e.mov_reg_rm(
                    S::Q,
                    Scale::NoScale,
                    Reg::RAX,
                    (Reg::RBP, Reg::NoIndex, disp),
                );
            }
            _ => unimplemented!(),
        }
    }

    fn num_vars(&self) -> u32 {
        self.sem.vars.len() as u32
    }

    fn scratch_disp(&self, slot: u32) -> i32 {
        // Go one below because the stack grows up.
        (slot as i32 + 1) * -8
    }

    /// Return the offset from rbp at which the variable called `name`
    /// can be found. This is a negative integer, and multiple of 8.
    fn var_disp(&self, name: &UniqueString) -> i32 {
        let slot = self.sem.find_var(name).unwrap() as i32;
        // Go one below because the stack grows up.
        ((Self::NUM_SCRATCH_SLOTS as i32 + slot) + 1) * -8
    }

    fn emit_prologue(&mut self) {
        self.e.pushq(Reg::RBP);
        self.e.mov_reg_reg(S::Q, Reg::RBP, Reg::RSP);

        // Make enough space to spill every local.
        // Stack pointers must always be 16-byte aligned.
        // We pushed rbp manually, and the return address was pushed
        // as part of the call to get into this code, so we're aligned
        // prior to subtracting for variable space.
        self.e.sub_reg_imm(
            S::Q,
            Reg::RSP,
            align(
                (Self::NUM_SCRATCH_SLOTS + self.num_vars()) * 8,
                16,
            ),
        );
    }

    fn emit_epilogue(&mut self) {
        self.e.leave();
        self.e.ret();
    }

    fn fp_from_reg(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(dst.is_fp() && !src.is_fp());
        let rm = (Reg::RBP, Reg::NoIndex, self.scratch_disp(0));
        self.e.mov_rm_reg(S::Q, Scale::NoScale, rm, src);
        self.e.mov_fp_rm(fp, Scale::NoScale, dst, rm);
    }

    fn reg_from_fp(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(!dst.is_fp() && src.is_fp());
        let rm = (Reg::RBP, Reg::NoIndex, self.scratch_disp(0));
        self.e.mov_rm_fp(fp, Scale::NoScale, rm, src);
        self.e.mov_reg_rm(S::Q, Scale::NoScale, dst, rm);
    }

    fn call_builtin(&mut self, func: builtins::BuiltinFunc) {
        self.e
            .mov_reg_imm(Reg::RAX, builtins::addr(func));
        self.e.call_reg(Reg::RAX);
    }
}

/// Align `x` to `to`; set it to the smallest multiple of `to` that is >= `x`.
fn align(x: u32, to: u32) -> u32 {
    (x + (to - 1)) & !(to - 1)
}
