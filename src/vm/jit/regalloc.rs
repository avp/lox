use super::emitter::Reg;
use crate::lir;
use bitvec::prelude::*;
use bitvec::vec::BitVec;

#[derive(Debug, Copy, Clone)]
pub enum Slot {
    Reg(Reg),
    Stack(u32),
}

const USABLE_REGS: &'static [Reg] =
    &[Reg::RCX, Reg::RDX, Reg::R12, Reg::R13, Reg::R14, Reg::R15];

pub fn reg_alloc(func: &lir::Function) -> Vec<Slot> {
    let mut res = vec![Slot::Reg(Reg::NONE); func.get_stack_size() as usize];
    for i in 0..func.get_stack_size() {
        res[i as usize] = Slot::Stack(0);
    }
    res
}

struct RegAllocator<'lir> {
    func: &'lir lir::Function,
    allocations: Vec<Slot>,
    active_intervals: Vec<Option<Interval>>,
    block_liveness: Vec<BlockLiveness>,
}

impl RegAllocator<'_> {
    pub fn new(func: &lir::Function) -> RegAllocator {
        let stack_size = func.get_stack_size() as usize;
        let num_blocks = func.num_blocks();
        RegAllocator {
            func,
            allocations: vec![Slot::Reg(Reg::NONE); stack_size],
            active_intervals: vec![None; stack_size],
            block_liveness: vec![BlockLiveness::new(stack_size); num_blocks],
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Interval {
    start: usize,
    end: usize,
}

impl Interval {
    pub fn new(start: usize, end: usize) -> Interval {
        Interval { start, end }
    }
}

#[derive(Debug, Clone)]
struct BlockLiveness {
    /// Live values used in the block.
    gen: BitVec,

    /// Live values defined in the block.
    kill: BitVec,

    /// Live-in values: live into the block.
    live_in: BitVec,

    /// Live-out values: live when the terminator is executed.
    live_out: BitVec,
}

impl BlockLiveness {
    pub fn new(size: usize) -> BlockLiveness {
        BlockLiveness {
            gen: bitvec![0; size],
            kill: bitvec![0; size],
            live_in: bitvec![0; size],
            live_out: bitvec![0; size],
        }
    }
}
