use super::{VMState, Value};
use crate::lir;

pub fn run(state: &mut VMState, func: &lir::Function) -> Value {
    use lir::{Opcode::*, VReg};

    let cur_bb = func.get_entry_block();
    let mut ip = 0;

    let frame = state.alloc_stack(func.get_stack_size());

    macro_rules! frame_reg {
        ($e:expr) => (frame[$e.0 as usize])
    };

    loop {
        match cur_bb.insts()[ip].opcode {
            LoadNil(reg) => {
                frame_reg!(reg) = Value::nil();
                ip += 1;
            }
            LoadNumber(reg, n) => {
                frame_reg!(reg) = Value::number(n);
                ip += 1;
            }
            Print(reg) => {
                println!("{}", frame_reg!(reg));
                ip += 1;
            }
            Ret(reg) => {
                return frame_reg!(reg).clone();
            }
            _ => unimplemented!(),
        };
    }
}
