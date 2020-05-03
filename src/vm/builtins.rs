use crate::vm::operations;
use crate::vm::Value;

pub type BuiltinFunc = extern "C" fn(Value) -> Value;

pub fn addr(f: BuiltinFunc) -> usize {
    unsafe { std::mem::transmute(f as *const u8) }
}

pub extern "C" fn to_bool(val: Value) -> Value {
    Value::bool(operations::to_bool(val))
}

pub extern "C" fn to_number(val: Value) -> Value {
    Value::number(operations::to_number(val))
}

pub extern "C" fn println(val: Value) -> Value {
    println!("{}", val);
    Value::nil()
}
