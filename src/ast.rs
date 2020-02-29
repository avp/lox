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
    Print(P<Expr>),
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
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
