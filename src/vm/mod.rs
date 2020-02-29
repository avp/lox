mod jit;

use crate::ast;
use jit::JitContext;

struct VM {
    jit: JitContext,
}

impl VM {
    pub fn new() -> VM {
        VM {
            jit: JitContext::new(),
        }
    }

    pub fn run(ast: ast::P<ast::File>) {
        unimplemented!();
    }
}
