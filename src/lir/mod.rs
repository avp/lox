//! A simple linear IR meant to be easy to interpret and simple to JIT.
//! Can perform very simple optimizations maybe.

use crate::ctx::{Ctx, UniqueString};

mod inst;
pub use inst::{Inst, Opcode};

mod gen;
pub use gen::generate_lir;

pub struct Program<'ctx> {
    ctx: &'ctx Ctx,

    /// List of functions, where the first function is the global function.
    functions: Vec<Function>,
}

impl<'ctx> Program<'ctx> {
    pub fn new(ctx: &'ctx Ctx) -> Program<'ctx> {
        Program {
            ctx,
            functions: vec![],
        }
    }

    pub fn new_function(&mut self) -> FunctionIdx {
        let result = FunctionIdx(self.functions.len());
        self.functions.push(Function::new());
        result
    }

    pub fn get_global_function(&self) -> &Function {
        &self.functions[0]
    }

    #[inline]
    pub fn get_function(&self, i: FunctionIdx) -> &Function {
        &self.functions[i.0]
    }

    #[inline]
    pub fn get_function_mut(&mut self, i: FunctionIdx) -> &mut Function {
        &mut self.functions[i.0]
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FunctionIdx(usize);

#[derive(Debug)]
pub struct Function {
    /// List of basic blocks, where the first block is the entry point.
    blocks: Vec<BasicBlock>,
}

impl Function {
    pub fn new() -> Function {
        Function { blocks: vec![] }
    }

    pub fn new_block(&mut self) -> BasicBlockIdx {
        let result = BasicBlockIdx(self.blocks.len());
        self.blocks.push(BasicBlock::new());
        result
    }

    #[inline]
    pub fn get_block(&self, i: BasicBlockIdx) -> &BasicBlock {
        &self.blocks[i.0]
    }

    #[inline]
    pub fn get_block_mut(&mut self, i: BasicBlockIdx) -> &mut BasicBlock {
        &mut self.blocks[i.0]
    }
}

#[derive(Debug, Copy, Clone)]
pub struct BasicBlockIdx(usize);

#[derive(Debug)]
pub struct BasicBlock {
    insts: Vec<Inst>,
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock { insts: vec![] }
    }
}