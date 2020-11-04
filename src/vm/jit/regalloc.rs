use super::emitter::Reg;
use crate::lir;
use crate::support::slice_get_2_mut;
use bitvec::prelude::*;
use bitvec::vec::BitVec;

#[derive(Debug, Copy, Clone)]
pub enum Slot {
    Reg(Reg),
    Stack(u32),
}

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
    inst_intervals: Vec<Interval>,
    block_offsets: Vec<usize>,
    block_liveness: Vec<BlockLiveness>,
}

impl RegAllocator<'_> {
    pub fn new(func: &lir::Function) -> RegAllocator {
        let stack_size = func.get_stack_size() as usize;
        let num_blocks = func.blocks().len();

        let mut offset = 0;
        let mut block_offsets = Vec::with_capacity(num_blocks);
        for block in func.blocks() {
            block_offsets.push(offset);
            offset += block.insts().len();
        }

        RegAllocator {
            func,
            allocations: vec![Slot::Reg(Reg::NONE); stack_size],
            inst_intervals: vec![Interval::new(0, 0); offset],
            block_offsets,
            block_liveness: vec![BlockLiveness::new(stack_size); num_blocks],
        }
    }

    pub fn allocate(&mut self) {
        self.calc_local_liveness();
        self.calc_global_liveness();
    }

    fn calc_local_liveness(&mut self) {
        for (idx, bb) in self.func.blocks().iter().enumerate() {
            let live = &mut self.block_liveness[idx];
            for inst in bb.insts() {
                if let Some(out) = inst.def() {
                    live.kill.set(out.0 as usize, true);
                }
                inst.for_each_use(|vreg| {
                    live.gen.set(vreg.0 as usize, true);
                });
            }
        }
    }

    fn calc_global_liveness(&mut self) {
        let mut changed = false;

        for live in &mut self.block_liveness {
            live.live_in |= live.gen.iter().copied();
            live.live_in &= live.kill.iter().copied().map(|b| !b);
        }

        loop {
            for (bb_idx, bb) in self.func.blocks().iter().enumerate() {
                bb.for_each_succ(|succ_idx| {
                    let (live, succ_live) = unsafe {
                        slice_get_2_mut(
                            &mut self.block_liveness,
                            bb_idx,
                            succ_idx.0,
                        )
                    };
                    succ_live
                        .live_out
                        .iter_mut()
                        .zip(live.live_out.iter())
                        .for_each(|(mut b1, &b2)| {
                            changed |= !*b1 && b2;
                            *b1 |= b2;
                        });
                });

                // In = gen + (OUT - KILL)
                let live = &mut self.block_liveness[bb_idx];
                live.live_in.clone_from(&live.live_out);
            }

            if !changed {
                break;
            }
        }
    }

    fn get_inst_number(&self, bb: lir::BasicBlockIdx, inst_idx: usize) {
        self.block_offsets[bb.0] + inst_idx;
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
            gen: BitVec::repeat(false, size),
            kill: BitVec::repeat(false, size),
            live_in: BitVec::repeat(false, size),
            live_out: BitVec::repeat(false, size),
        }
    }
}

const USABLE_REGS: &'static [Reg] =
    &[Reg::RCX, Reg::RDX, Reg::R12, Reg::R13, Reg::R14, Reg::R15];

fn reg_position(reg: Reg) -> usize {
    match reg {
        Reg::RCX => 0,
        Reg::RDX => 1,
        Reg::R12 => 2,
        Reg::R13 => 3,
        Reg::R14 => 4,
        Reg::R15 => 5,
        _ => unreachable!("Invalid usable register"),
    }
}

#[derive(Debug)]
struct RegFile {
    regs: BitVec,
}

impl RegFile {
    pub fn new(size: usize) -> RegFile {
        RegFile {
            regs: BitVec::repeat(true, USABLE_REGS.len() + size),
        }
    }

    pub fn alloc(&mut self) -> Slot {
        let pos = self.regs.iter().position(|b| *b).unwrap();
        self.regs.set(pos, false);
        if pos < USABLE_REGS.len() {
            Slot::Reg(USABLE_REGS[pos])
        } else {
            Slot::Stack((pos - USABLE_REGS.len()) as u32)
        }
    }

    pub fn free(&mut self, slot: Slot) {
        let pos = match slot {
            Slot::Reg(reg) => reg_position(reg),
            Slot::Stack(n) => n as usize + USABLE_REGS.len(),
        };
        self.regs.set(pos, true);
    }
}
