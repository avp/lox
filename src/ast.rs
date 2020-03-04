use codespan::Span;

pub type P<T> = Box<T>;

#[derive(Debug)]
pub struct File {
    pub decls: Vec<P<Decl>>,
    pub span: Span,
}

#[derive(Debug)]
pub enum DeclKind {
    Stmt(P<Stmt>),
}

#[derive(Debug)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
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
    BinOp(BinOpKind, P<Expr>, P<Expr>),
    UnOp(UnOpKind, P<Expr>),
    NumberLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    Ident(String),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

pub trait Visitor<T> {
    fn visit_file(&mut self, file: &File) -> T;
    fn visit_decl(&mut self, decl: &Decl) -> T;
    fn visit_stmt(&mut self, stmt: &Stmt) -> T;
    fn visit_expr(&mut self, expr: &Expr) -> T;
}
