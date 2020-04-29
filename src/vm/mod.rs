mod builtins;
mod interpreter;
mod jit;
mod value;

pub use value::Value;

use crate::ast;
use jit::JitContext;

/// VM can execute a given AST.
/// Stores various contexts and heaps in order to allocate things
/// in a shared context.
pub struct VM {
    jit: JitContext,
    interp: interpreter::Interpreter,
}

impl VM {
    pub fn new(dump_asm: bool) -> VM {
        VM {
            jit: JitContext::new(dump_asm),
            interp: interpreter::Interpreter::new(),
        }
    }

    pub fn run(&mut self, ast: ast::P<ast::File>) -> Value {
        let fun_opt = self.jit.compile(&ast);
        match fun_opt {
            Some(fun) => fun(),
            _ => self.interp.run(&ast),
        }
    }
}
