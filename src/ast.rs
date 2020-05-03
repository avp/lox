use codespan::{FileId, Span};
use crate::ctx::UniqueString;

pub type P<T> = Box<T>;

pub trait ASTNode<'ast> {
    fn visit_children<T>(&'ast self, v: &mut dyn Visitor<'ast, Output = T>);
}

#[derive(Debug)]
pub struct File {
    pub decls: Vec<P<Decl>>,
    pub id: FileId,
    pub span: Span,
}

impl<'ast> ASTNode<'ast> for File {
    fn visit_children<T>(&'ast self, v: &mut dyn Visitor<'ast, Output = T>) {
        for decl in &self.decls {
            v.visit_decl(decl);
        }
    }
}

#[derive(Debug)]
pub enum DeclKind {
    Var(UniqueString, Option<P<Expr>>),
    Stmt(P<Stmt>),
}

#[derive(Debug)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}

impl<'ast> ASTNode<'ast> for Decl {
    fn visit_children<T>(&'ast self, v: &mut dyn Visitor<'ast, Output = T>) {
        match &self.kind {
            DeclKind::Stmt(stmt) => {
                v.visit_stmt(stmt);
            }
            DeclKind::Var(_, Some(expr)) => {
                v.visit_expr(expr);
            }
            _ => {}
        };
    }
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(P<Expr>),
    Return(P<Expr>),
    Print(P<Expr>),
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl<'ast> ASTNode<'ast> for Stmt {
    fn visit_children<T>(&'ast self, v: &mut dyn Visitor<'ast, Output = T>) {
        match &self.kind {
            StmtKind::Expr(expr) => {
                v.visit_expr(expr);
            }
            StmtKind::Return(expr) => {
                v.visit_expr(expr);
            }
            StmtKind::Print(expr) => {
                v.visit_expr(expr);
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum UnOpKind {
    Neg,
    Not,
}

#[derive(Debug)]
pub enum ExprKind {
    Assign(P<Expr>, P<Expr>),
    BinOp(BinOpKind, P<Expr>, P<Expr>),
    UnOp(UnOpKind, P<Expr>),
    NumberLiteral(f64),
    StringLiteral(UniqueString),
    BoolLiteral(bool),
    Ident(UniqueString),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl<'ast> ASTNode<'ast> for Expr {
    fn visit_children<T>(&'ast self, v: &mut dyn Visitor<'ast, Output = T>) {
        match &self.kind {
            ExprKind::Assign(e1, e2) => {
                v.visit_expr(e1);
                v.visit_expr(e2);
            }
            ExprKind::BinOp(_, e1, e2) => {
                v.visit_expr(e1);
                v.visit_expr(e2);
            }
            ExprKind::UnOp(_, e) => {
                v.visit_expr(e);
            }
            ExprKind::NumberLiteral(_) => {}
            ExprKind::StringLiteral(_) => {}
            ExprKind::BoolLiteral(_) => {}
            ExprKind::Ident(_) => {}
        }
    }
}

pub trait Visitor<'ast> {
    type Output;
    fn visit_file(&mut self, file: &'ast File) -> Self::Output;
    fn visit_decl(&mut self, decl: &'ast Decl) -> Self::Output;
    fn visit_stmt(&mut self, stmt: &'ast Stmt) -> Self::Output;
    fn visit_expr(&mut self, expr: &'ast Expr) -> Self::Output;
}
