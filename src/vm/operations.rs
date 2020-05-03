use super::value::Tag;
use super::value::Value;

pub fn to_bool(val: Value) -> bool {
    match val.get_tag() {
        Tag::Bool => val.get_bool(),
        Tag::Number => val.get_number() != 0.into(),
        Tag::Nil => true,
    }
}

pub fn to_number(val: Value) -> f64 {
    match val.get_tag() {
        Tag::Bool => if val.get_bool() { 1 } else { 0 }.into(),
        Tag::Number => val.get_number(),
        Tag::Nil => 0.into(),
    }
}
