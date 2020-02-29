mod mem;

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
}

/// Compiles a global function.
/// Construct a new instance for each function you want to compile.
struct Jit<'ctx> {
    ctx: &'ctx JitContext,
}

impl<'ctx> Jit<'ctx> {
    fn new(ctx: &'ctx JitContext) -> Jit {
        Jit { ctx }
    }
}
