//! Generator for LIR.

use std::rc::Rc;

use super::*;
use crate::ast::*;

struct Generator<'ctx> {
    ctx: &'ctx Ctx,

    builder: Builder<'ctx>,
}

impl<'ctx> Generator<'ctx> {
    pub fn gen(ctx: &'ctx mut Ctx, func: Func) -> Program<'ctx> {
        let generator = Self::new(ctx, func);
        unimplemented!();
    }

    fn new(ctx: &'ctx mut Ctx, func: Func) -> Generator<'ctx> {
        let builder = Builder::new(ctx);
        Generator { ctx, builder }
    }

    fn run() {}
}

struct Builder<'ctx> {
    program: Program<'ctx>,
    function_idx: FunctionIdx,
    block_idx: BasicBlockIdx,
}

impl<'ctx> Builder<'ctx> {
    pub fn new(ctx: &'ctx Ctx) -> Builder<'ctx> {
        let mut program = Program::new(ctx);
        let function_idx = program.new_function();
        let block_idx = program.get_function_mut(function_idx).new_block();
        Builder {
            program,
            function_idx,
            block_idx,
        }
    }

    pub fn make_function(&mut self) -> FunctionIdx {
        self.program.new_function()
    }

    pub fn set_function(&mut self, function_idx: FunctionIdx) {
        self.function_idx = function_idx
    }

    pub fn make_block(&mut self) -> BasicBlockIdx {
        self.program.get_function_mut(self.function_idx).new_block()
    }

    pub fn set_block(&mut self, block_idx: BasicBlockIdx) {
        self.block_idx = block_idx
    }
}

pub fn generate_lir(func: Func) {
    unimplemented!();
}
