#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
    pub string: Option<String>,
    pub number: Option<f64>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenKind {
    Ident,
    StringLiteral,
    NumberLiteral,

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

    Empty,
    Eof,
}

impl Token {
    pub fn new(kind: TokenKind, len: usize) -> Token {
        Token {
            kind,
            len,
            string: None,
            number: None,
        }
    }

    pub fn number(number: f64, len: usize) -> Token {
        Token {
            kind: TokenKind::NumberLiteral,
            len,
            string: None,
            number: Some(number),
        }
    }

    pub fn string(string: String, len: usize) -> Token {
        Token {
            kind: TokenKind::StringLiteral,
            len,
            string: Some(string),
            number: None,
        }
    }

    pub fn ident(string: String, len: usize) -> Token {
        Token {
            kind: TokenKind::Ident,
            len,
            string: Some(string),
            number: None,
        }
    }
}
