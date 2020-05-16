use std::alloc::{alloc, dealloc, Layout};

#[repr(C)]
pub struct Heap {
    start: *mut u8,
    free_ptr: *mut u8,
    end: *mut u8,
    layout: Layout,
}

impl Heap {
    pub fn new(size: u32) -> Heap {
        let layout = Layout::from_size_align(size as usize, 4096)
            .expect("Invalid heap size");
        unsafe {
            let start = alloc(layout);
            Heap {
                start,
                free_ptr: start,
                end: start.offset(size as isize),
                layout,
            }
        }
    }

    pub fn alloc(&mut self, size: usize) -> *mut u8 {
        let res = self.free_ptr;
        unsafe {
            self.free_ptr = self.free_ptr.offset(size as isize);
        }
        res
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe {
            dealloc(self.start, self.layout);
        }
    }
}

enum HeapData {}

#[repr(C)]
pub enum CellKind {
    LoxString,
}

pub trait Cell {
    fn get_kind() -> CellKind;
}

#[repr(C)]
pub struct LoxString {
    pub kind: CellKind,
    pub len: usize,
    data: HeapData,
}

impl Cell for LoxString {
    fn get_kind() -> CellKind {
        CellKind::LoxString
    }
}

impl LoxString {
    pub fn new<'heap>(
        heap: &'heap mut Heap,
        string: &str,
    ) -> &'heap mut LoxString {
        let ptr: *mut u8 = heap.alloc(Self::alloc_size(string.len()));
        let len = string.as_bytes().len();
        let refn: &mut LoxString = unsafe { std::mem::transmute(ptr) };
        refn.kind = Self::get_kind();
        refn.len = len;
        unsafe {
            let data: *mut u8 = std::mem::transmute(
                ptr.offset(offset_of!(Self, data) as isize),
            );
            std::ptr::copy(string.as_ptr(), data, len);
            std::mem::transmute(ptr)
        }
    }

    pub fn data<'heap>(&'heap self) -> &str {
        unsafe {
            let top: *const u8 = std::mem::transmute(self as *const Self);
            let data = top.offset(offset_of!(Self, data) as isize);
            let slice: &[u8] = std::slice::from_raw_parts(data, self.len);
            std::str::from_utf8(slice).unwrap()
        }
    }

    fn alloc_size(len: usize) -> usize {
        use std::mem::size_of;
        offset_of!(Self, data) + len * size_of::<u8>()
    }
}
