mod builtins;
mod heap;
mod interpreter;
mod jit;
mod operations;
mod string_table;
mod value;

pub use value::Value;

use crate::lir;
use crate::sem::SemInfo;
use jit::JitContext;
use string_table::StringTable;

use std::mem::{self, MaybeUninit};

const STACK_SIZE: usize = 0x1000;

#[repr(C)]
pub struct VMState {
    thrown_value: Value,
    heap: heap::Heap,
    string_table: StringTable,
    stack: Box<[Value; STACK_SIZE]>,
    sp: usize,
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
        let stack = unsafe {
            let ptr: *mut [Value; STACK_SIZE] =
                std::mem::transmute(std::alloc::alloc(
                    std::alloc::Layout::from_size_align(STACK_SIZE, 8).unwrap(),
                ));
            Box::from_raw(ptr)
        };
        VM {
            jit: JitContext::new(dump_asm),
            state: VMState {
                thrown_value: Value::nil(),
                heap: heap::Heap::new(0x1000),
                string_table: StringTable::new(),
                // TODO: Avoid initializing all this memory.
                stack,
                sp: STACK_SIZE,
            },
        }
    }

    pub fn run<'ctx>(&mut self, ast: lir::Program<'ctx>) -> Option<Value> {
        let fun_opt = JitContext::compile(self, &ast);
        let result = match fun_opt {
            Some(fun) => fun(&mut self.state as *mut VMState),
            _ => interpreter::run(&mut self.state),
        };

        if self.state.thrown_value.get_tag() != value::Tag::Nil {
            println!("{:?}", self.state.thrown_value.get_tag());
            eprintln!("Error thrown during execution");
            return None;
        }

        Some(result)
    }
}
