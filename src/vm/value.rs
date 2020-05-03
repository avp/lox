use std::fmt;

#[derive(Debug, Clone)]
pub struct Value {
    raw: u64,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Tag {
    Nil,
    Number,
}

const NUM_TAG_BITS: u64 = 16;
const NUM_DATA_BITS: u64 = 64 - NUM_TAG_BITS;

const NIL_TAG: u64 = 0xfff9;

impl Value {
    fn with_tag(val: u64, tag: u64) -> Value {
        Value {
            raw: val | (tag << NUM_DATA_BITS),
        }
    }
    pub fn nil() -> Value {
        Value::with_tag(0, NIL_TAG)
    }

    pub fn number(num: f64) -> Value {
        Value {
            raw: num.to_bits(),
        }
    }

    pub fn get_tag(&self) -> Tag {
        match self.raw >> NUM_DATA_BITS {
            self::NIL_TAG => Tag::Nil,
            _ => Tag::Number,
        }
    }

    pub fn get_number(&self) -> f64 {
        assert_eq!(self.get_tag(), Tag::Number);
        f64::from_bits(self.raw)
    }

    pub fn raw(&self) -> u64 {
        self.raw
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.get_tag() {
            Tag::Number => write!(f, "{}", self.get_number())?,
            Tag::Nil => write!(f, "<nil>")?,
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
    fn nil() {
        let v = Value::nil();
        assert_eq!(v.raw(), 0xfff9000000000000u64);
        assert_eq!(v.get_tag(), Tag::Nil);
    }
}
