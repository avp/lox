use crate::token::{Token, TokenKind};

use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
pub struct Lexer<'src> {
    it: Peekable<Chars<'src>>,
    pub token: Token,
}

impl<'src> Lexer<'src> {
    pub fn new(text: &'src str) -> Lexer {
        Lexer {
            it: text.chars().peekable(),
            token: Token::new(TokenKind::Empty, 0),
        }
    }

    fn next(&mut self) -> char {
        match self.it.next() {
            Some(c) => c,
            None => '\0',
        }
    }

    fn peek(&mut self) -> char {
        match self.it.peek() {
            Some(&c) => c,
            None => '\0',
        }
    }

    fn scan_number(&mut self) -> Token {
        assert!(self.peek().is_digit(10));
        let mut len = 0;
        let mut num = self.next().to_digit(10).unwrap() as f64;
        loop {
            let c = self.peek();
            if c.is_digit(10) {
                len += 1;
                num *= 10f64;
                num += c.to_digit(10).unwrap() as f64;
            } else {
                return Token::number(num, len);
            }
            self.next();
        }
    }

    fn scan_ident(&mut self) -> Token {
        assert!(start_ident(self.peek()));
        let mut ident = String::new();
        let mut len = 0usize;
        loop {
            let c = self.peek();
            if !mid_ident(c) {
                return Token::ident(ident, len);
            }
            self.next();
            len += 1;
            ident.push(c);
        }
    }

    fn scan_str(&mut self) -> Token {
        assert_eq!(self.peek(), '"');
        let mut string = String::new();
        self.next();
        let mut len = 1usize;
        loop {
            let c = self.peek();
            if c == '"' {
                self.next();
                len += 1;
                return Token::string(string, len);
            }
            self.next();
            len += 1;
            match c {
                '\\' => match self.peek() {
                    '"' => {
                        string.push('"');
                        self.next();
                        len += 1;
                    }
                    '\\' => {
                        string.push('\\');
                        self.next();
                        len += 1;
                    }
                    'n' => {
                        string.push('\n');
                        self.next();
                        len += 1;
                    }
                    _ => {}
                },
                _ => {
                    string.push(c);
                }
            };
        }
    }

    pub fn advance(&mut self) {
        macro_rules! token_1 {
            ($kind:ident) => {{
                self.next();
                self.token = Token::new(TokenKind::$kind, 1);
            }};
        }

        macro_rules! token_1_2 {
            ($kind1:ident, $next:expr, $kind2:ident) => {{
                self.next();
                self.token = if self.peek() == $next {
                    self.next();
                    Token::new(TokenKind::$kind2, 2)
                } else {
                    Token::new(TokenKind::$kind1, 1)
                }
            }};
        }

        loop {
            let c = self.peek();
            if c == '\0' {
                self.token = Token::new(TokenKind::Eof, 0);
                return;
            }
            if c.is_whitespace() {
                self.next();
                continue;
            }
            if start_ident(c) {
                self.token = self.scan_ident();
                return;
            }
            if c == '/' {
                self.next();
                if self.peek() == '/' {
                    while self.peek() != '\n' {
                        self.next();
                    }
                    continue;
                }
                return token_1!(Slash);
            }
            return match c {
                '0'..='9' => {
                    self.token = self.scan_number();
                }
                '"' => {
                    self.token = self.scan_str();
                }
                '(' => token_1!(LParen),
                ')' => token_1!(RParen),
                '{' => token_1!(LBrace),
                '}' => token_1!(RBrace),
                ',' => token_1!(Comma),
                '.' => token_1!(Dot),
                '-' => token_1!(Minus),
                '+' => token_1!(Plus),
                '*' => token_1!(Star),
                '/' => token_1!(Slash),
                ';' => token_1!(Semi),
                '<' => token_1_2!(Less, '=', LessEqual),
                '>' => token_1_2!(Greater, '=', GreaterEqual),
                '!' => token_1_2!(Bang, '=', BangEqual),
                '=' => token_1_2!(Equal, '=', EqualEqual),
                _ => unimplemented!(),
            };
        }
    }
}

fn start_ident(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '$' || c == '_'
}

fn mid_ident(c: char) -> bool {
    start_ident(c) || c.is_digit(10)
}

#[cfg(test)]
mod tests {
    use super::TokenKind::*;
    use super::*;

    fn advance<'src, 'b>(lexer: &'src mut Lexer) -> &'src Token {
        lexer.advance();
        &lexer.token
    }

    #[test]
    fn smoke() {
        let src: &'static str = "1 { } ( ) > >= < <=";
        let mut lex = Lexer::new(src);
        assert_eq!(NumberLiteral, advance(&mut lex).kind);
        assert_eq!(Some(1f64), lex.token.number);
        assert_eq!(LBrace, advance(&mut lex).kind);
        assert_eq!(RBrace, advance(&mut lex).kind);
        assert_eq!(LParen, advance(&mut lex).kind);
        assert_eq!(RParen, advance(&mut lex).kind);
        assert_eq!(Greater, advance(&mut lex).kind);
        assert_eq!(GreaterEqual, advance(&mut lex).kind);
        assert_eq!(Less, advance(&mut lex).kind);
        assert_eq!(LessEqual, advance(&mut lex).kind);
    }
}
