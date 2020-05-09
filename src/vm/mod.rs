mod builtins;
mod heap;
mod interpreter;
mod jit;
mod operations;
mod value;

pub use value::Value;

use crate::ast;
use crate::sem::SemInfo;
use jit::JitContext;

#[repr(C)]
pub struct VMState {
    thrown_value: Value,
    heap: heap::Heap,
}

/// VM can execute a given AST.
/// Stores various contexts and heaps in order to allocate things
/// in a shared context.
pub struct VM {
    jit: JitContext,
    state: VMState,
}

impl VM {
    pub fn new(dump_asm: bool) -> VM {
        VM {
            jit: JitContext::new(dump_asm),
            state: VMState {
                thrown_value: Value::nil(),
                heap: heap::Heap::new(0x1000),
            },
        }
    }

    pub fn run(
        &mut self,
        ast: ast::P<ast::Func>,
        sem: &SemInfo,
    ) -> Option<Value> {
        let fun_opt = self.jit.compile(&ast, sem);
        let result = match fun_opt {
            Some(fun) => fun(&mut self.state as *mut VMState),
            _ => interpreter::Interpreter::run(&ast, sem),
        };

        if self.state.thrown_value.get_tag() != value::Tag::Nil {
            println!("{:?}", self.state.thrown_value.get_tag());
            eprintln!("Error thrown during execution");
            return None;
        }

        Some(result)
    }
}
