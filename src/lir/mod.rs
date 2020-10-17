//! A simple linear IR meant to be easy to interpret and simple to JIT.
//! Can perform very simple optimizations maybe.

use crate::ctx::{Ctx, UniqueString};

mod inst;
pub use inst::{Inst, Opcode};

mod gen;
pub use gen::generate_lir;

pub struct Program<'ctx> {
    ctx: &'ctx mut Ctx,

    /// List of functions, where the first function is the global function.
    functions: Vec<Function>,
}

impl<'ctx> Program<'ctx> {
    pub fn get_global_function(&self) -> &Function {
        &self.functions[0]
    }
}

pub struct Function {
    /// List of basic blocks, where the first block is the entry point.
    blocks: Vec<BasicBlock>,
}

impl Function {
    fn get_block(&self, i: BasicBlockIdx) -> &BasicBlock {
        &self.blocks[i.0]
    }

    fn get_block_mut(&mut self, i: BasicBlockIdx) -> &mut BasicBlock {
        &mut self.blocks[i.0]
    }
}

pub struct BasicBlockIdx(usize);

pub struct BasicBlock {
    insts: Vec<Inst>,
}
