use crate::ast;
use crate::ast::Visitor;
use crate::sem::SemInfo;
use crate::vm::Value;

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn run(&mut self, file: &ast::File, sem: &SemInfo) -> Value {
        match self.visit_file(file) {
            Ok(v) => v,
            Err(v) => v,
        }
    }
}

type InterpResult = std::result::Result<Value, Value>;

impl<'ast> ast::Visitor<'ast, InterpResult> for Interpreter {
    fn visit_file(&mut self, file: &ast::File) -> InterpResult {
        let mut result = Value::number(0f64);
        for decl in &file.decls {
            // Immediately use the return value if returning from a function,
            // but make sure to never return Err.
            result = match self.visit_decl(&decl) {
                Ok(v) => v,
                Err(v) => return Ok(v),
            }
        }
        Ok(result)
    }

    fn visit_decl(&mut self, decl: &ast::Decl) -> InterpResult {
        match &decl.kind {
            ast::DeclKind::Stmt(s) => self.visit_stmt(&s),
            ast::DeclKind::Var(_, _) => unimplemented!(),
        }
    }

    fn visit_stmt(&mut self, stmt: &ast::Stmt) -> InterpResult {
        match &stmt.kind {
            ast::StmtKind::Expr(e) => self.visit_expr(&e),
            ast::StmtKind::Return(e) => Err(self.visit_expr(&e)?),
            ast::StmtKind::Print(e) => {
                println!("{}", self.visit_expr(e)?);
                Ok(Value::nil())
            }
        }
    }

    fn visit_expr(&mut self, expr: &ast::Expr) -> InterpResult {
        use ast::BinOpKind;
        use ast::UnOpKind;
        use crate::vm::value::Tag;
        match &expr.kind {
            ast::ExprKind::BinOp(op, left, right) => {
                let (l, r) = (
                    self.visit_expr(&left)?,
                    self.visit_expr(&right)?,
                );
                let result = match (l.get_tag(), r.get_tag()) {
                    (Tag::Number, Tag::Number) => Value::number(match op {
                        BinOpKind::Add => l.get_number() + r.get_number(),
                        BinOpKind::Sub => l.get_number() - r.get_number(),
                        BinOpKind::Mul => l.get_number() * r.get_number(),
                        BinOpKind::Div => l.get_number() / r.get_number(),
                    }),
                    _ => Value::nil(),
                };
                Ok(result)
            }
            ast::ExprKind::UnOp(op, expr) => {
                let val = self.visit_expr(expr)?;
                let result = match val.get_tag() {
                    Tag::Number => Value::number(match op {
                        UnOpKind::Neg => -val.get_number(),
                        UnOpKind::Not => unimplemented!(),
                    }),
                    Tag::Nil => Value::nil(),
                };
                Ok(result)
            }
            ast::ExprKind::NumberLiteral(x) => Ok(Value::number(*x)),
            _ => unimplemented!(),
        }
    }
}
