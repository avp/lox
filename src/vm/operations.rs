use super::value::Tag;
use super::value::Value;

pub fn to_bool(val: Value) -> bool {
    match val.get_tag() {
        Tag::Bool => val.get_bool(),
        Tag::Number => val.get_number() != 0.into(),
        Tag::Nil => true,
    }
}
