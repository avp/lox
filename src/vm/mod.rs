mod jit;

use jit::Jit;

struct VM {
    jit: Jit,
}

impl VM {
    fn new() -> VM {
        VM {
            jit: Jit::new(),
        }
    }
}
