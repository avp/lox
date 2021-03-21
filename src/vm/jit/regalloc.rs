use super::emitter::Reg;
use crate::lir;
use crate::support::slice_get_2_mut;
use bitvec::vec::BitVec;

#[derive(Debug, Copy, Clone)]
pub enum Slot {
    Reg(Reg),
    Stack(u32),
}

pub fn reg_alloc(func: &lir::Function) -> Vec<Slot> {
    let mut allocator = RegAllocator::new(func);
    allocator.allocate_fast();
    allocator.allocations
    // let mut res = vec![Slot::Reg(Reg::NONE); func.get_stack_size() as usize];
    // for i in 0..func.get_stack_size() {
    //     res[i as usize] = Slot::Stack(0);
    // }
    // res
}

struct RegAllocator<'lir> {
    func: &'lir lir::Function,
    allocations: Vec<Slot>,
    inst_intervals: Vec<Interval>,
    block_offsets: Vec<usize>,
    block_liveness: Vec<BlockLiveness>,
}

#[allow(dead_code)]
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
            inst_intervals: vec![Interval::empty(); offset],
            block_offsets,
            block_liveness: vec![BlockLiveness::new(stack_size); num_blocks],
        }
    }

    pub fn allocate_fast(&mut self) {
        let mut file = RegFile::new(self.allocations.len());
        for reg in self.allocations.iter_mut() {
            *reg = file.alloc();
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

    fn calc_liveness_intervals(&mut self) {
        // Start with intervals that contain only the segment.
        for (i, interval) in self.inst_intervals.iter_mut().enumerate() {
            // The value starts to be live at instruction `i + 1`.
            *interval = Interval::new(i + 1, i + 1);
        }

        for (bb_idx, &bb_start) in self.block_offsets.iter().enumerate() {
            let liveness = &self.block_liveness[bb_idx];
            let bb_idx = lir::BasicBlockIdx(bb_idx);
            let bb_end = bb_start + self.func.get_block(bb_idx).insts().len();

            // Check for registers which are not touched by this basic block.
            // The register at `i` is untouched by the block.
            for (i, (&leaves, &enters)) in
                liveness.live_out.iter().zip(&liveness.live_in).enumerate()
            {
                if leaves && enters {
                    self.inst_intervals[i]
                        .add_segment(Segment::new(bb_start, bb_end + 1));
                }
            }

            for (inst_idx, inst) in
                self.func.get_block(bb_idx).insts().iter().enumerate()
            {
                let inst_offset = self.get_inst_number(bb_idx, inst_idx);

                // If this register is leaving the basic block,
                // simply extend until the end of the basic block.
                if liveness.live_out[inst_offset] {}
            }
        }

        unimplemented!();
    }

    fn get_inst_number(
        &self,
        bb: lir::BasicBlockIdx,
        inst_idx: usize,
    ) -> usize {
        self.block_offsets[bb.0] + inst_idx
    }
}

#[derive(Debug, Copy, Clone)]
struct Segment {
    start: usize,
    end: usize,
}

impl Segment {
    pub fn new(start: usize, end: usize) -> Segment {
        Segment { start, end }
    }

    pub fn intersects(&self, other: Segment) -> bool {
        !(other.start >= self.end || self.start >= other.end)
    }

    pub fn adjacent(&self, other: Segment) -> bool {
        other.start == self.end || self.start == other.end
    }

    /// Merge the two segments into `self`, making self contain at least both.
    pub fn merge(&mut self, other: Segment) {
        use std::cmp::{max, min};
        self.start = min(self.start, other.start);
        self.end = max(self.end, other.end);
    }
}

#[derive(Debug, Clone)]
struct Interval {
    segments: Vec<Segment>,
}

impl Interval {
    pub fn empty() -> Interval {
        Interval { segments: vec![] }
    }

    pub fn new(start: usize, end: usize) -> Interval {
        Interval {
            segments: vec![Segment::new(start, end)],
        }
    }

    pub fn intersects_segment(&self, other: Segment) -> bool {
        for segment in self.segments.iter() {
            if segment.intersects(other) {
                return true;
            }
        }
        false
    }

    pub fn add_segment(&mut self, other: Segment) {
        for segment in self.segments.iter_mut() {
            if segment.intersects(other) || segment.adjacent(other) {
                segment.merge(other);
                return;
            }
        }
        self.segments.push(other);
    }

    pub fn add_interval(&mut self, other: &Interval) {
        for &segment in &other.segments {
            self.add_segment(segment);
        }
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

const USABLE_REGS: &'static [Reg] = &[];
// &[Reg::RCX, Reg::RDX, Reg::R12, Reg::R13, Reg::R14, Reg::R15];

fn reg_position(reg: Reg) -> usize {
    match reg {
        Reg::RCX => 0,
        Reg::RDX => 1,
        Reg::R12 => 2,
        Reg::R13 => 3,
        Reg::R14 => 4,
        Reg::R15 => 5,
        _ => unreachable!("Invalid usable register: {:?}", reg),
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
