struct File {
    decls: LinkedList<Box<Decl>>,
}

enum DeclKind {
    Stmt,
}

struct Decl {
    kind: DeclKind,
}

enum StmtKind {
    Expr,
}

struct Stmt {
    kind: StmtKind,
}

enum ExprKind {
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    NumberLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
}

struct Expr {
    kind: ExprKind,
}
