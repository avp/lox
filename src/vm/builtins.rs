use crate::vm::Value;

pub type BuiltinFunc = extern "C" fn(Value) -> Value;

pub fn addr(f: BuiltinFunc) -> usize {
    unsafe { std::mem::transmute(f as *const u8) }
}

pub extern "C" fn println(val: Value) -> Value {
    println!("{}", val);
    Value::nil()
}
