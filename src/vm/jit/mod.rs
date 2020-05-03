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
        ast: &ast::File,
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
    file: &'ast ast::File,
    sem: &'ast SemInfo,
}

impl<'ctx, 'ast> Jit<'_, '_> {
    fn new(
        ctx: &'ctx mut JitContext,
        file: &'ast ast::File,
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
                        self.e.mov_reg_imm(
                            Reg::RAX,
                            builtins::addr(builtins::println),
                        );
                        self.e.call_reg(Reg::RAX);
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
            &NumberLiteral(x) => {
                self.e
                    .mov_reg_imm(Reg::RAX, Value::number(x).raw());
            }
            BinOp(op, x, y) => {
                use ast::BinOpKind::*;
                self.e.pushq(Reg::RBX);
                self.compile_expr(&x);
                self.e.mov_reg_reg(S::Q, Reg::RBX, Reg::RAX);
                self.compile_expr(&y);
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
                self.e.popq(Reg::RBX);
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

    fn var_disp(&self, name: &UniqueString) -> i32 {
        let slot = self.sem.find_var(name).unwrap() as i32;
        // Slot 0 is at [RBP-8] because the stack grows up.
        (slot + 1) * -8
    }

    fn emit_prologue(&mut self) {
        self.e.pushq(Reg::RBP);
        self.e.mov_reg_reg(S::Q, Reg::RBP, Reg::RSP);

        // Make enough space to spill every local.
        if self.num_vars() > 0 {
            self.e
                .sub_reg_imm(S::Q, Reg::RSP, align(self.num_vars() * 8, 16));
        }
    }

    fn emit_epilogue(&mut self) {
        self.e.leave();
        self.e.ret();
    }

    fn fp_from_reg(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(dst.is_fp() && !src.is_fp());
        self.e.pushq(src);
        self.e.mov_fp_rm(
            fp,
            Scale::NoScale,
            dst,
            (Reg::RSP, Reg::NoIndex, 0),
        );
        self.e.popq(src);
    }

    fn reg_from_fp(&mut self, fp: FP, dst: Reg, src: Reg) {
        assert!(!dst.is_fp() && src.is_fp());
        self.e.pushq(dst);
        self.e.mov_rm_fp(
            fp,
            Scale::NoScale,
            (Reg::RSP, Reg::NoIndex, 0),
            src,
        );
        self.e.popq(dst);
    }
}

fn align(x: u32, to: u32) -> u32 {
    (x + (to - 1)) & !(to - 1)
}
