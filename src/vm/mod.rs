mod jit;
mod value;

pub use value::Value;

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

    pub fn run(&mut self, ast: ast::P<ast::File>) -> Value {
        let fun = self.jit.compile(&ast);
        fun()
    }
}
