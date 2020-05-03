mod builtins;
mod interpreter;
mod jit;
mod operations;
mod value;

pub use value::Value;

use crate::ast;
use crate::sem::SemInfo;
use jit::JitContext;

/// VM can execute a given AST.
/// Stores various contexts and heaps in order to allocate things
/// in a shared context.
pub struct VM {
    jit: JitContext,
}

impl VM {
    pub fn new(dump_asm: bool) -> VM {
        VM {
            jit: JitContext::new(dump_asm),
        }
    }

    pub fn run(&mut self, ast: ast::P<ast::File>, sem: &SemInfo) -> Value {
        let fun_opt = self.jit.compile(&ast, sem);
        match fun_opt {
            Some(fun) => fun(),
            _ => interpreter::Interpreter::run(&ast, sem),
        }
    }
}
