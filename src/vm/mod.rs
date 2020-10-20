mod builtins;
mod heap;
mod interpreter;
mod jit;
mod operations;
mod string_table;
mod value;

pub use value::Value;

use crate::lir;
use jit::JitContext;
use string_table::StringTable;

const STACK_SIZE: usize = 0x1000;

#[repr(C)]
pub struct VMState {
    thrown_value: Value,
    heap: heap::Heap,
    string_table: StringTable,
    stack: Box<[Value; STACK_SIZE]>,
    sp: usize,
}

impl VMState {
    pub fn alloc_stack(&mut self, size: u32) -> &mut [Value] {
        let size = size as usize;
        assert!(self.sp > size, "Stack overflow");
        self.sp -= size;
        &mut self.stack[self.sp..self.sp + size]
    }
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

    pub fn run<'ctx>(&mut self, lir: lir::Program<'ctx>) -> Option<Value> {
        let fun_opt = JitContext::compile(self, &lir);
        let result = match fun_opt {
            Some(fun) => fun(&mut self.state as *mut VMState),
            _ => interpreter::run(&mut self.state, &lir),
        };

        if self.state.thrown_value.get_tag() != value::Tag::Nil {
            println!("{:?}", self.state.thrown_value.get_tag());
            eprintln!("Error thrown during execution");
            return None;
        }

        Some(result)
    }
}
