#[derive(Debug)]
pub struct File {
    pub decls: Vec<Box<Decl>>,
}

#[derive(Debug)]
pub enum DeclKind {
    Stmt(Box<Stmt>),
}

#[derive(Debug)]
pub struct Decl {
    pub kind: DeclKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(Box<Expr>),
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
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
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    UnOp(UnOpKind, Box<Expr>),
    NumberLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    Ident(String),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}
