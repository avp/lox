mod jit;

use crate::ast;
use jit::JitContext;

pub struct VM {
    jit: JitContext,
}

impl VM {
    pub fn new(dump_asm: bool) -> VM {
        VM {
            jit: JitContext::new(dump_asm),
        }
    }

    pub fn run(&mut self, ast: ast::P<ast::File>) -> u64 {
        let fun: fn() -> u64 = self.jit.compile(&ast);
        fun()
    }
}
