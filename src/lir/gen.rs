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

    next_vreg: VReg,
}

impl<'ctx> Generator<'ctx> {
    pub fn gen(ctx: &'ctx Ctx, func: ast::Function) -> Program<'ctx> {
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
            next_vreg: VReg(0),
        }
    }

    fn run(&mut self, func: ast::Function) {
        self.gen_function(&func);
    }

    fn alloc_vreg(&mut self) -> VReg {
        let result = self.next_vreg;
        self.next_vreg = VReg(result.0 + 1);
        result
    }

    fn gen_function(&mut self, node: &ast::Function) {
        let function = self.builder.make_function();
        self.builder.set_function(function);
        let block = self.builder.make_block();
        self.builder.set_block(block);

        // TODO: Emit parameters

        self.gen_block(&node.body);
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
                self.builder.make_inst(Inst::opcode(Print(vreg)));
            }
            _ => unimplemented!(),
        };
    }

    fn gen_expr(&mut self, node: &ast::Expr) -> VReg {
        match &node.kind {
            ast::ExprKind::NumberLiteral(n) => {
                let vreg = self.alloc_vreg();
                self.builder.make_inst(Inst::opcode(LoadNumber(vreg, *n)));
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

    fn get_block_mut(&mut self) -> &mut BasicBlock {
        self.program
            .get_function_mut(self.function_idx)
            .get_block_mut(self.block_idx)
    }

    pub fn make_inst(&mut self, inst: Inst) {
        let mut block = self.get_block_mut();
        block.insts.push(inst);
    }
}

pub fn generate_lir<'ctx>(
    ctx: &'ctx Ctx,
    func: ast::Function,
) -> Program<'ctx> {
    Generator::gen(ctx, func)
}
