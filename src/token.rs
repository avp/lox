#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Comment,

    Ident,
    StringLiteral(String),
    NumberLiteral(f64),

    LParen,
    RParen,

    LBrace,
    RBrace,

    Comma,
    Dot,
    Minus,
    Plus,
    Star,
    Slash,
    Semi,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl Token {
    pub fn new(kind: TokenKind, len: usize) -> Token {
        Token { kind, len }
    }
}
