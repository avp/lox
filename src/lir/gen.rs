//! Generator for LIR.

use std::collections::HashMap;

use super::inst::*;
use super::*;
use crate::ast;

use super::inst::Opcode::*;

struct Generator<'ctx> {
    ctx: &'ctx Ctx,

    builder: Builder<'ctx>,

    name_table: HashMap<UniqueString, VReg>,
}

impl<'ctx> Generator<'ctx> {
    pub fn gen(ctx: &'ctx Ctx, func: &ast::Function) -> Program<'ctx> {
        let mut generator = Self::new(ctx);
        generator.run(func);
        generator.builder.eject_program()
    }

    fn new(ctx: &'ctx Ctx) -> Generator<'ctx> {
        let builder = Builder::new(ctx);
        Generator {
            ctx,
            builder,
            name_table: HashMap::new(),
        }
    }

    fn run(&mut self, func: &ast::Function) {
        self.gen_function(&func);
    }

    fn alloc_vreg(&mut self) -> VReg {
        VReg(self.builder.alloc_stack_reg())
    }

    fn gen_function(&mut self, node: &ast::Function) {
        let function = self.builder.make_function();
        self.builder.set_function(function);
        let block = self.builder.make_block();
        self.builder.set_block(block);

        // TODO: Emit parameters
        self.gen_block(&node.body);

        let ret_reg = self.alloc_vreg();
        self.builder.make_inst(LoadNil(ret_reg));
        self.builder.make_inst(Ret(ret_reg));
    }

    fn gen_block(&mut self, node: &ast::Block) {
        for decl in node.decls.iter() {
            self.gen_decl(decl);
        }
    }

    fn gen_decl(&mut self, node: &ast::Decl) {
        match &node.kind {
            ast::DeclKind::Var(name, init) => {
                let vreg = match &init {
                    None => self.alloc_vreg(),
                    Some(init) => self.gen_expr(init),
                };
                self.name_table.insert(name.clone(), vreg);
            }
            ast::DeclKind::Stmt(stmt) => self.gen_stmt(stmt),
        }
    }

    fn gen_stmt(&mut self, node: &ast::Stmt) {
        match &node.kind {
            ast::StmtKind::Print(expr) => {
                let vreg = self.gen_expr(expr);
                self.builder.make_inst(Print(vreg));
            }
            _ => unimplemented!(),
        };
    }

    fn gen_expr(&mut self, node: &ast::Expr) -> VReg {
        match &node.kind {
            ast::ExprKind::NumberLiteral(n) => {
                let vreg = self.alloc_vreg();
                self.builder.make_inst(LoadNumber(vreg, *n));
                vreg
            }
            _ => unimplemented!(),
        }
    }
}

struct Builder<'ctx> {
    program: Program<'ctx>,
    function_idx: FunctionIdx,
    block_idx: BasicBlockIdx,
}

impl<'ctx> Builder<'ctx> {
    pub fn new(ctx: &'ctx Ctx) -> Builder<'ctx> {
        Builder {
            program: Program::new(ctx),
            function_idx: FunctionIdx(usize::MAX),
            block_idx: BasicBlockIdx(usize::MAX),
        }
    }

    pub fn eject_program(self) -> Program<'ctx> {
        self.program
    }

    pub fn alloc_stack_reg(&mut self) -> u32 {
        let res = self.get_function_mut().stack_size;
        self.get_function_mut().stack_size += 1;
        res
    }

    pub fn make_function(&mut self) -> FunctionIdx {
        self.program.new_function()
    }

    pub fn set_function(&mut self, function_idx: FunctionIdx) {
        self.function_idx = function_idx
    }

    pub fn get_function(&self) -> &Function {
        self.program.get_function(self.function_idx)
    }

    pub fn get_function_mut(&mut self) -> &mut Function {
        self.program.get_function_mut(self.function_idx)
    }

    pub fn make_block(&mut self) -> BasicBlockIdx {
        self.program.get_function_mut(self.function_idx).new_block()
    }

    pub fn set_block(&mut self, block_idx: BasicBlockIdx) {
        self.block_idx = block_idx
    }

    fn get_block_mut(&mut self) -> &mut BasicBlock {
        self.program
            .get_function_mut(self.function_idx)
            .get_block_mut(self.block_idx)
    }

    pub fn make_inst(&mut self, opcode: Opcode) {
        self.get_block_mut().insts.push(Inst::opcode(opcode));
    }
}

pub fn generate_lir<'ctx>(
    ctx: &'ctx Ctx,
    func: &'ctx ast::Function,
) -> Program<'ctx> {
    Generator::gen(ctx, func)
}
