use crate::ast;
use crate::ast::Visitor;
use crate::ctx::UniqueString;
use crate::sem::SemInfo;
use crate::vm::Value;

pub struct Interpreter<'ast> {
    sem: &'ast SemInfo,
    vars: Vec<Value>,
}

impl<'ast> Interpreter<'ast> {
    pub fn run(file: &'ast ast::File, sem: &'ast SemInfo) -> Value {
        let mut int = Interpreter {
            sem,
            vars: vec![Value::nil(); sem.vars.len()],
        };
        match int.visit_file(file) {
            Ok(v) => v,
            Err(v) => v,
        }
    }

    fn var_idx(&self, name: &UniqueString) -> usize {
        self.sem.find_var(name).unwrap()
    }
}

type InterpResult = std::result::Result<Value, Value>;

impl<'ast> ast::Visitor<'ast> for Interpreter<'ast> {
    type Output = InterpResult;
    fn visit_file(&mut self, file: &'ast ast::File) -> InterpResult {
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

    fn visit_decl(&mut self, decl: &'ast ast::Decl) -> InterpResult {
        match &decl.kind {
            ast::DeclKind::Stmt(s) => self.visit_stmt(&s),
            ast::DeclKind::Var(name, expr) => {
                let idx = self.var_idx(name);
                if let Some(expr) = expr {
                    let init = self.visit_expr(expr)?;
                    self.vars[idx] = init;
                }
                Ok(Value::nil())
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &'ast ast::Stmt) -> InterpResult {
        match &stmt.kind {
            ast::StmtKind::Expr(e) => self.visit_expr(&e),
            ast::StmtKind::Return(e) => Err(self.visit_expr(&e)?),
            ast::StmtKind::Print(e) => {
                println!("{}", self.visit_expr(e)?);
                Ok(Value::nil())
            }
        }
    }

    fn visit_expr(&mut self, expr: &'ast ast::Expr) -> InterpResult {
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
                    Tag::Bool => match op {
                        UnOpKind::Neg => unimplemented!(),
                        UnOpKind::Not => Value::bool(!val.get_bool()),
                    },
                };
                Ok(result)
            }
            ast::ExprKind::NumberLiteral(x) => Ok(Value::number(*x)),
            ast::ExprKind::Ident(name) => {
                let idx = self.var_idx(name);
                Ok(self.vars[idx].clone())
            }
            _ => unimplemented!(),
        }
    }
}
