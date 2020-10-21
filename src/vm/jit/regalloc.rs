use super::emitter::Reg;
use crate::lir;

#[derive(Debug, Copy, Clone)]
pub enum Slot {
    Reg(Reg),
    Stack(u32),
}

const USABLE_REGS: &'static [Reg] = &[Reg::RCX, Reg::RDX];

pub fn reg_alloc(func: &lir::Function) -> Vec<Slot> {
    let mut res = vec![Slot::Reg(Reg::NONE); func.get_stack_size() as usize];
    for i in 0..func.get_stack_size() {
        res[i as usize] = Slot::Stack(0);
    }
    res
}
