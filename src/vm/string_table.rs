use super::heap::*;

#[repr(C)]
pub struct StringTable {
    list: Vec<*const LoxString>,
}

impl StringTable {
    pub fn new() -> StringTable {
        StringTable { list: vec![] }
    }

    pub fn add(&mut self, s: &LoxString) -> u32 {
        let res = self.list.len();
        self.list.push(s);
        res as u32
    }

    pub fn get(&mut self, idx: u32) -> *const LoxString {
        self.list[idx as usize]
    }
}
