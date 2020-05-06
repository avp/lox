use std::mem;

const PAGE_SIZE: usize = 4096;

pub struct ExecHeap {
    pools: Vec<Pool>,
}

impl ExecHeap {
    pub fn new() -> ExecHeap {
        ExecHeap { pools: vec![] }
    }

    pub fn alloc<'a, 'b>(&'a mut self, size: usize) -> &'b mut [u8] {
        for pool in &mut self.pools {
            if let Some(res) = pool.alloc(size) {
                return unsafe { std::slice::from_raw_parts_mut(res, size) };
            }
        }

        let mut pool = Pool::new();
        let res = pool.alloc(size).unwrap();
        self.pools.push(pool);
        unsafe { std::slice::from_raw_parts_mut(res, size) }
    }
}

struct Pool {
    buf: *mut u8,
    size: usize,
    cur: usize,
}

impl Pool {
    const POOL_SIZE: usize = 1 << 20;

    pub fn new() -> Pool {
        let buf: *mut u8;
        let size: usize = Self::POOL_SIZE;
        unsafe {
            let mut page: *mut libc::c_void =
                mem::MaybeUninit::uninit().as_mut_ptr();
            libc::posix_memalign(&mut page, PAGE_SIZE, size);
            libc::mprotect(
                page,
                size,
                libc::PROT_EXEC | libc::PROT_READ | libc::PROT_WRITE,
            );
            buf = page as *mut u8;
        }
        Pool {
            buf,
            size,
            cur: 0,
        }
    }

    pub fn alloc(&mut self, size: usize) -> Option<*mut u8> {
        if self.cur < self.size - size {
            let res: *mut u8;
            unsafe {
                res = self.buf.add(self.cur);
            }
            self.cur += size;
            Some(res)
        } else {
            None
        }
    }
}
