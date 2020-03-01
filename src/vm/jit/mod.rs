mod emitter;
mod mem;

use crate::ast;

use emitter::Reg;
use mem::ExecHeap;

pub struct JitContext {
    heap: ExecHeap,
}

impl JitContext {
    pub fn new() -> JitContext {
        JitContext {
            heap: ExecHeap::new(),
        }
    }

    pub fn compile(&mut self, ast: &ast::File) -> fn() -> u64 {
        let mut jit = Jit::new(self, ast);
        jit.compile()
    }
}

/// Compiles a global function.
/// Construct a new instance for each function you want to compile.
struct Jit<'ctx, 'ast> {
    ctx: &'ctx JitContext,
    e: emitter::Emitter<'ctx>,
    file: &'ast ast::File,
}

impl<'ctx, 'ast> Jit<'_, '_> {
    fn new(
        ctx: &'ctx mut JitContext,
        file: &'ast ast::File,
    ) -> Jit<'ctx, 'ast> {
        let buf: &'ctx mut [u8] = (&mut ctx.heap).alloc(4096);
        Jit {
            ctx: &*ctx,
            e: emitter::Emitter::new(&mut *buf),
            file,
        }
    }

    pub fn compile(&mut self) -> fn() -> u64 {
        use ast::*;
        self.emit_prologue(8 * 8);
        match &(self.file.decls[0]).kind {
            DeclKind::Stmt(stmt) => match &stmt.kind {
                StmtKind::Expr(expr) => self.compile_expr(&expr),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };
        self.emit_epilogue();
        unsafe { std::mem::transmute(self.e.buf.as_ptr()) }
    }

    fn compile_expr(&mut self, expr: &ast::Expr) {
        use ast::ExprKind::*;
        match expr.kind {
            NumberLiteral(x) => {
                self.e.mov_reg_imm(Reg::RAX, x);
            }
            _ => unimplemented!(),
        }
    }

    fn emit_prologue(&mut self, stack_size: usize) {
        use emitter::{Reg, S};
        self.e.pushq(Reg::RBP);
        self.e.mov_reg_reg(S::Q, Reg::RBP, Reg::RSP);
    }

    fn emit_epilogue(&mut self) {
        use emitter::Reg;
        self.e.popq(Reg::RBP);
        self.e.ret();
    }
}
