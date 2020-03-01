mod emitter;
mod mem;

use crate::ast;

use emitter::{Reg, S};
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

    pub fn compile(&mut self, ast: &ast::File) -> fn() -> u64 {
        let dump = self.dump_asm;
        let mut jit = Jit::new(self, ast);
        let result = jit.compile();
        if dump {
            jit.dump();
        }
        result
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
        let instrs = cs.disasm_count(self.e.buf, 0, count).unwrap();
        println!("{}", instrs);
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
        match &expr.kind {
            &NumberLiteral(x) => {
                self.e.mov_reg_imm(Reg::RAX, x as u64);
            }
            BinOp(op, x, y) => {
                use ast::BinOpKind::*;
                self.e.pushq(Reg::RBX);
                self.compile_expr(&x);
                self.e.mov_reg_reg(S::Q, Reg::RBX, Reg::RAX);
                self.compile_expr(&y);
                match op {
                    Add => self.e.add_reg_reg(S::Q, Reg::RAX, Reg::RBX),
                    _ => unimplemented!(),
                }
                self.e.popq(Reg::RBX);
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
