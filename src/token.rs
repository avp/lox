use codespan::Span;

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub string: Option<String>,
    pub number: Option<f64>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenKind {
    Ident,
    ResWord(ResWord),

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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ResWord {
    True,
    False,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token {
            kind,
            span,
            string: None,
            number: None,
        }
    }

    pub fn number(number: f64, span: Span) -> Token {
        Token {
            kind: TokenKind::NumberLiteral,
            span,
            string: None,
            number: Some(number),
        }
    }

    pub fn string(string: String, span: Span) -> Token {
        Token {
            kind: TokenKind::StringLiteral,
            span,
            string: Some(string),
            number: None,
        }
    }

    pub fn ident(string: String, span: Span) -> Token {
        Token {
            kind: TokenKind::Ident,
            span,
            string: Some(string),
            number: None,
        }
    }

    pub fn res_word(string: &str, span: Span) -> Option<Token> {
        Some(Token {
            kind: match string {
                "true" => TokenKind::ResWord(ResWord::True),
                "false" => TokenKind::ResWord(ResWord::False),
                _ => return None,
            },
            span,
            string: None,
            number: None,
        })
    }

    pub fn is_res_word(&self) -> bool {
        match self.kind {
            TokenKind::ResWord(_) => true,
            _ => false,
        }
    }
}
