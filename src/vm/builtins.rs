use crate::vm::Value;

pub fn addr(f: extern "C" fn(Value) -> Value) -> usize {
    unsafe { std::mem::transmute(f as *const u8) }
}

pub extern "C" fn println(val: Value) -> Value {
    println!("{}", val);
    Value::nil()
}
