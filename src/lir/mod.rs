//! A simple linear IR meant to be easy to interpret and simple to JIT.
//! Can perform very simple optimizations maybe.

use crate::ctx::{Ctx, UniqueString};
use std::fmt;

mod inst;
pub use inst::{Inst, Opcode, VReg};

mod gen;
pub use gen::generate_lir;

pub struct Program<'ctx> {
    ctx: &'ctx Ctx,

    /// List of functions, where the first function is the global function.
    functions: Vec<Function>,
}

impl<'ctx> fmt::Display for Program<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for func in &self.functions {
            write!(f, "{}", func)?;
        }
        Ok(())
    }
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

    pub fn get_functions(&self) -> &[Function] {
        &self.functions
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
pub struct FunctionIdx(pub usize);

#[derive(Debug)]
pub struct Function {
    /// List of basic blocks, where the first block is the entry point.
    blocks: Vec<BasicBlock>,

    /// Number of VRegs needed to run this Function.
    stack_size: u32,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (idx, bb) in self.blocks.iter().enumerate() {
            write!(f, "BB{}:\n", idx)?;
            write!(f, "{}", bb)?;
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl Function {
    pub fn new() -> Function {
        Function {
            blocks: vec![],
            stack_size: 0,
        }
    }

    pub fn new_block(&mut self) -> BasicBlockIdx {
        let result = BasicBlockIdx(self.blocks.len());
        self.blocks.push(BasicBlock::new());
        result
    }

    #[inline]
    pub fn get_stack_size(&self) -> u32 {
        self.stack_size
    }

    #[inline]
    pub fn blocks(&self) -> &[BasicBlock] {
        &self.blocks
    }

    #[inline]
    pub fn blocks_mut(&mut self) -> &mut [BasicBlock] {
        &mut self.blocks
    }

    #[inline]
    pub fn get_entry_block(&self) -> &BasicBlock {
        &self.blocks[0]
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BasicBlockIdx(pub usize);

#[derive(Debug)]
pub struct BasicBlock {
    insts: Vec<Inst>,
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (idx, inst) in self.insts.iter().enumerate() {
            write!(f, "{}: {:?}\n", idx, inst)?;
        }
        Ok(())
    }
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock { insts: vec![] }
    }

    pub fn insts(&self) -> &[Inst] {
        &self.insts
    }

    pub fn for_each_succ<F>(&self, mut cb: F)
    where
        F: FnMut(BasicBlockIdx),
    {
        use Opcode::*;
        let inst = self.insts().last();
        if inst.is_none() {
            return;
        }
        match inst.unwrap().opcode {
            Branch(bb) => {
                cb(bb);
            }
            CondBranch(_, bb1, bb2) => {
                cb(bb1);
                cb(bb2);
            }
            _ => {}
        }
    }
}
