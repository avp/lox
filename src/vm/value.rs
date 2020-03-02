use std::fmt;

#[derive(Debug)]
pub struct Value {
    raw: u64,
}

pub enum Tag {
    Number,
}

impl Value {
    pub fn number(num: f64) -> Value {
        Value {
            raw: num.to_bits(),
        }
    }

    pub fn get_tag(&self) -> Tag {
        Tag::Number
    }

    pub fn get_number(&self) -> f64 {
        f64::from_bits(self.raw)
    }

    pub fn raw(&self) -> u64 {
        self.raw
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.get_tag() {
            Tag::Number => write!(f, "<number: {}>", self.get_number())?,
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
        assert_eq!(v.raw(), 0x3FF8000000000000);
        assert_eq!(v.get_number(), 1.5f64);
    }
}
