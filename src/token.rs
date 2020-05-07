use crate::ctx::UniqueString;

use codespan::Span;

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub string: Option<UniqueString>,
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

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Ident => "identifier",
                Self::ResWord(_) => "reserved word",

                Self::StringLiteral => "string literal",
                Self::NumberLiteral => "number",

                Self::LParen => "(",
                Self::RParen => ")",
                Self::LBrace => "{",
                Self::RBrace => "}",
                Self::Comma => ",",
                Self::Dot => ".",
                Self::Minus => "-",
                Self::Plus => "+",
                Self::Star => "*",
                Self::Slash => "/",
                Self::Semi => ";",

                Self::Bang => "!",
                Self::BangEqual => "!=",
                Self::Equal => "=",
                Self::EqualEqual => "==",
                Self::Greater => ">",
                Self::GreaterEqual => ">=",
                Self::Less => "<",
                Self::LessEqual => "<=",

                Self::Empty => "<empty>",
                Self::Eof => "<eof>",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ResWord {
    True,
    False,
    Return,
    Print,
    Var,
    For,
    While,
    If,
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

    pub fn string(string: UniqueString, span: Span) -> Token {
        Token {
            kind: TokenKind::StringLiteral,
            span,
            string: Some(string),
            number: None,
        }
    }

    pub fn ident(string: UniqueString, span: Span) -> Token {
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
                "return" => TokenKind::ResWord(ResWord::Return),
                "print" => TokenKind::ResWord(ResWord::Print),
                "var" => TokenKind::ResWord(ResWord::Var),
                "for" => TokenKind::ResWord(ResWord::For),
                "while" => TokenKind::ResWord(ResWord::While),
                "if" => TokenKind::ResWord(ResWord::If),
                _ => return None,
            },
            span,
            string: None,
            number: None,
        })
    }

    pub fn get_string(&self) -> UniqueString {
        self.string.clone().unwrap()
    }
}
