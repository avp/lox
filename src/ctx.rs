use std::collections::HashMap;
use string_cache::DefaultAtom;

pub struct Ctx {
    string_table: StringTable,
}

impl Ctx {
    pub fn new() -> Ctx {
        Ctx {
            string_table: StringTable::new(),
        }
    }

    pub fn unique_string(&mut self, s: &str) -> UniqueString {
        self.string_table.get_string(s)
    }
}

struct StringTable {
    table: HashMap<String, DefaultAtom>,
}

pub type UniqueString = DefaultAtom;

impl StringTable {
    fn new() -> StringTable {
        StringTable {
            table: HashMap::new(),
        }
    }

    fn get_string(&mut self, s: &str) -> UniqueString {
        if let Some(res) = self.table.get(s) {
            return res.clone();
        }
        let atom = DefaultAtom::from(s);
        self.table.entry(s.to_owned()).or_insert(atom).clone()
    }
}
