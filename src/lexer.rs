use crate::token::{Token, TokenKind};

use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
struct Lexer<'src> {
    it: Peekable<Chars<'src>>,
    done: bool,
}

impl<'src> Lexer<'src> {
    fn new(text: &'src str) -> Lexer {
        Lexer {
            it: text.chars().peekable(),
            done: false,
        }
    }

    fn next(&mut self) -> Option<char> {
        self.it.next()
    }

    fn peek(&mut self) -> Option<char> {
        match self.it.peek() {
            Some(&c) => Some(c.clone()),
            None => None,
        }
    }

    fn scan_number(&mut self) -> Token {
        assert!(self.peek().unwrap().is_digit(10));
        let mut len = 0;
        let mut num = self.next().unwrap().to_digit(10).unwrap() as f64;
        loop {
            match self.peek() {
                None => return Token::new(TokenKind::NumberLiteral(num), len),
                Some(c) => {
                    if c.is_digit(10) {
                        len += 1;
                        num *= 10f64;
                        num += c.to_digit(10).unwrap() as f64;
                    } else {
                        return Token::new(TokenKind::NumberLiteral(num), len);
                    }
                }
            }
            self.next();
        }
    }

    fn advance(&mut self) -> Option<Token> {
        macro_rules! token1 {
            ($kind:ident) => {{
                self.next();
                Some(Token::new(TokenKind::$kind, 1))
            }};
        }

        loop {
            let c_opt: Option<char> = self.peek();
            match c_opt {
                None => {
                    self.done = true;
                    return None;
                }
                Some(c) => {
                    if c.is_whitespace() {
                        self.next();
                        continue;
                    }
                    return match c {
                        '0'..='9' => Some(self.scan_number()),
                        '(' => token1!(LParen),
                        ')' => token1!(RParen),
                        '{' => token1!(LBrace),
                        '}' => token1!(RBrace),
                        _ => unimplemented!(),
                    };
                }
            };
        }
    }
}

pub fn tokenize<'str>(text: &str) -> impl Iterator<Item = Token> + '_ {
    let mut lexer: Lexer = Lexer::new(text);
    std::iter::from_fn(move || {
        if lexer.done {
            None
        } else {
            lexer.advance()
        }
    })
}

#[cfg(test)]
mod tests {
    use super::tokenize;
    use super::TokenKind::*;
    #[test]
    fn num() {
        let src: &'static str = "1 { } ( )";
        let mut it = tokenize(src);
        assert_eq!(NumberLiteral(1f64), it.next().unwrap().kind);
        assert_eq!(LBrace, it.next().unwrap().kind);
        assert_eq!(RBrace, it.next().unwrap().kind);
        assert_eq!(LParen, it.next().unwrap().kind);
        assert_eq!(RParen, it.next().unwrap().kind);
    }
}
