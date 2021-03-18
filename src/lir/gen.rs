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
            ast::StmtKind::Expr(expr) => {
                self.gen_expr(expr);
            }
            ast::StmtKind::While(cond, body) => {
                let bb_cond = self.builder.make_block();
                let bb_body = self.builder.make_block();
                let bb_next = self.builder.make_block();

                self.builder.make_inst(Branch(bb_cond));

                // Condition block.
                self.builder.set_block(bb_cond);
                let cond_vreg = self.gen_expr(cond);
                self.builder
                    .make_inst(CondBranch(cond_vreg, bb_body, bb_next));

                // Body of the loop.
                self.builder.set_block(bb_body);
                self.gen_block(body);
                self.builder.make_inst(Branch(bb_cond));

                // Continue.
                self.builder.set_block(bb_next);
            }
            ast::StmtKind::Return(expr) => {
                let vreg = self.gen_expr(expr);
                self.builder.make_inst(Ret(vreg));
            }
            ast::StmtKind::Print(expr) => {
                let vreg = self.gen_expr(expr);
                self.builder.make_inst(Print(vreg));
            }
        };
    }

    fn gen_expr(&mut self, node: &ast::Expr) -> VReg {
        match &node.kind {
            ast::ExprKind::NumberLiteral(n) => {
                let vreg = self.alloc_vreg();
                self.builder.make_inst(LoadNumber(vreg, *n));
                vreg
            }
            ast::ExprKind::Assign(left, right) => {
                let lhs = self.gen_expr(left);
                let rhs = self.gen_expr(right);
                self.builder.make_inst(Mov(lhs, rhs));
                lhs
            }
            ast::ExprKind::BinOp(kind, left, right) => {
                let vreg = self.alloc_vreg();
                let lhs = self.gen_expr(left);
                let rhs = self.gen_expr(right);
                match kind {
                    ast::BinOpKind::Add => {
                        self.builder.make_inst(Add(vreg, lhs, rhs))
                    }
                    ast::BinOpKind::Sub => {
                        self.builder.make_inst(Sub(vreg, lhs, rhs))
                    }
                    ast::BinOpKind::Mul => {
                        self.builder.make_inst(Mul(vreg, lhs, rhs))
                    }
                    ast::BinOpKind::Div => {
                        self.builder.make_inst(Div(vreg, lhs, rhs))
                    }
                    ast::BinOpKind::Equal => {
                        self.builder.make_inst(Equal(vreg, lhs, rhs))
                    }
                    ast::BinOpKind::NotEqual => {
                        self.builder.make_inst(Equal(vreg, lhs, rhs));
                        self.builder.make_inst(Not(vreg, vreg));
                    }
                    ast::BinOpKind::Greater => {
                        self.builder.make_inst(LessEqual(vreg, lhs, rhs));
                        self.builder.make_inst(Not(vreg, vreg));
                    }
                    ast::BinOpKind::GreaterEqual => {
                        self.builder.make_inst(Less(vreg, lhs, rhs));
                        self.builder.make_inst(Not(vreg, vreg));
                    }
                    ast::BinOpKind::Less => {
                        self.builder.make_inst(Less(vreg, lhs, rhs))
                    }
                    ast::BinOpKind::LessEqual => {
                        self.builder.make_inst(LessEqual(vreg, lhs, rhs))
                    }
                }
                vreg
            }
            ast::ExprKind::UnOp(_kind, _expr) => unimplemented!(),
            ast::ExprKind::StringLiteral(val) => {
                let vreg = self.alloc_vreg();
                self.builder.make_inst(LoadString(vreg, val.clone()));
                vreg
            }
            ast::ExprKind::BoolLiteral(val) => {
                let vreg = self.alloc_vreg();
                self.builder.make_inst(LoadBool(vreg, *val));
                vreg
            }
            ast::ExprKind::Ident(name) => {
                *self.name_table.get(name).expect("unresolved variable")
            }
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
