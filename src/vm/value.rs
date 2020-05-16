use super::heap::*;

use std::fmt;

#[derive(Debug, Clone)]
pub struct Value {
    raw: u64,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Tag {
    Nil,
    Bool,
    Number,
    LoxString,
}

const NUM_TAG_BITS: u64 = 16;
pub const NUM_DATA_BITS: u64 = 64 - NUM_TAG_BITS;

pub const NIL_TAG: u64 = 0xfff9;
pub const BOOL_TAG: u64 = 0xfff8;
pub const LOXSTRING_TAG: u64 = 0xfff7;

pub const LAST_TAG: u64 = 0xfff7;

impl Value {
    fn with_tag(val: u64, tag: u64) -> Value {
        Value {
            raw: val | (tag << NUM_DATA_BITS),
        }
    }

    pub fn nil() -> Value {
        Value::with_tag(0, NIL_TAG)
    }

    pub fn bool(b: bool) -> Value {
        Value::with_tag(if b { 1u64 } else { 0u64 }, BOOL_TAG)
    }

    pub fn number(num: f64) -> Value {
        Value { raw: num.to_bits() }
    }

    pub fn loxstring(ptr: *const LoxString) -> Value {
        Value::with_tag(ptr as u64, LOXSTRING_TAG)
    }

    pub fn get_tag(&self) -> Tag {
        match self.raw >> NUM_DATA_BITS {
            self::NIL_TAG => Tag::Nil,
            self::BOOL_TAG => Tag::Bool,
            self::LOXSTRING_TAG => Tag::LoxString,
            _ => Tag::Number,
        }
    }

    pub fn get_bool(&self) -> bool {
        assert_eq!(self.get_tag(), Tag::Bool);
        self.strip_tag() == 1
    }

    pub fn get_number(&self) -> f64 {
        assert_eq!(self.get_tag(), Tag::Number);
        f64::from_bits(self.raw)
    }

    pub fn get_loxstring(&self) -> *const LoxString {
        assert_eq!(self.get_tag(), Tag::LoxString);
        self.strip_tag() as *const LoxString
    }

    pub fn raw(&self) -> u64 {
        self.raw
    }

    fn strip_tag(&self) -> u64 {
        self.raw & ((1u64 << NUM_DATA_BITS) - 1)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.get_tag() {
            Tag::Number => write!(f, "{}", self.get_number())?,
            Tag::Nil => write!(f, "<nil>")?,
            Tag::Bool => write!(f, "{}", self.get_bool())?,
            Tag::LoxString => {
                write!(f, "{}", unsafe { (&*self.get_loxstring()).data() })?
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn number() {
        let v = Value::number(1.5f64);
        assert_eq!(v.raw(), 0x3FF8000000000000u64);
        assert_eq!(v.get_number(), 1.5f64);

        let v = Value::number(2f64);
        assert_eq!(v.get_number(), 2f64);
    }

    #[test]
    fn bool() {
        assert_eq!(Value::bool(true).get_bool(), true);
        assert_eq!(Value::bool(false).get_bool(), false);
    }

    #[test]
    fn nil() {
        let v = Value::nil();
        assert_eq!(v.raw(), 0xfff9000000000000u64);
        assert_eq!(v.get_tag(), Tag::Nil);
    }
}
