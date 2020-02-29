use std::mem;

const PAGE_SIZE: usize = 4096;

pub struct Jit {
    mem: JitMem,
}

impl Jit {
    pub fn new() -> Jit {
        Jit {
            mem: JitMem::new(1usize << 20),
        }
    }
}

struct JitMem {
    buf: *mut u8,
}

impl JitMem {
    fn new(size: usize) -> JitMem {
        let buf: *mut u8;
        unsafe {
            let mut page: *mut libc::c_void =
                mem::MaybeUninit::uninit().as_mut_ptr();
            libc::posix_memalign(&mut page, PAGE_SIZE, size);
            buf = mem::transmute(page);
        }
        JitMem { buf }
    }
}
