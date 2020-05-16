use super::heap::*;
use super::operations;
use super::VMState;
use super::Value;

pub type BuiltinFunc = extern "C" fn(&mut VMState, Value) -> Value;

pub fn addr(f: BuiltinFunc) -> usize {
    unsafe { std::mem::transmute(f as *const u8) }
}

pub extern "C" fn to_bool(_state: &mut VMState, val: Value) -> Value {
    Value::bool(operations::to_bool(val))
}

pub extern "C" fn println(_state: &mut VMState, val: Value) -> Value {
    println!("{}", val);
    Value::nil()
}

pub extern "C" fn load_loxstring(state: &mut VMState, val: Value) -> Value {
    let idx = val.get_number() as u32;
    let ptr: *const LoxString = state.string_table.get(idx);
    Value::loxstring(ptr)
}
