use crate::token::{Token, TokenKind};

use codespan::{ByteIndex, Span};
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug)]
pub struct Lexer<'src> {
    it: Peekable<CharIndices<'src>>,
    pub token: Token,
}

impl<'src> Lexer<'src> {
    pub fn new(text: &'src str) -> Lexer {
        Lexer {
            it: text.char_indices().peekable(),
            token: Token::new(TokenKind::Empty, Span::initial()),
        }
    }

    fn next(&mut self) -> (ByteIndex, char) {
        match self.it.next() {
            Some((pos, c)) => (ByteIndex::from(pos as u32), c),
            None => (ByteIndex::from(0u32), '\0'),
        }
    }

    fn peek(&mut self) -> char {
        match self.it.peek() {
            Some(&(_, c)) => c,
            None => '\0',
        }
    }

    fn scan_number(&mut self) -> Token {
        assert!(self.peek().is_digit(10));
        let (start, c) = self.next();
        let mut end = start;
        let mut num = c.to_digit(10).unwrap() as f64;
        loop {
            let c = self.peek();
            if c.is_digit(10) {
                num *= 10f64;
                num += c.to_digit(10).unwrap() as f64;
            } else {
                return Token::number(num, Span::new(start, end));
            }
            end = self.next().0;
        }
    }

    fn scan_ident(&mut self) -> Token {
        assert!(start_ident(self.peek()));
        let mut ident = String::new();
        let (start, c) = self.next();
        ident.push(c);
        let mut end = start;
        loop {
            let c = self.peek();
            if !mid_ident(c) {
                let span = Span::new(start, end);
                return match Token::res_word(&ident, span) {
                    None => Token::ident(ident, span),
                    Some(t) => t,
                };
            }
            end = self.next().0;
            ident.push(c);
        }
    }

    fn scan_str(&mut self) -> Token {
        assert_eq!(self.peek(), '"');
        let mut string = String::new();
        let (start, _) = self.next();
        loop {
            let c = self.peek();
            if c == '"' {
                let (end, _) = self.next();
                return Token::string(string, Span::new(start, end));
            }
            self.next();
            match c {
                '\\' => match self.peek() {
                    '"' => {
                        string.push('"');
                        self.next();
                    }
                    '\\' => {
                        string.push('\\');
                        self.next();
                    }
                    'n' => {
                        string.push('\n');
                        self.next();
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
            ($k:ident) => {{
                let (start, _) = self.next();
                self.token = Token::new(TokenKind::$k, Span::new(start, start));
            }};
        }

        macro_rules! token_1_2 {
            ($kind1:ident, $next:expr, $kind2:ident) => {{
                let (start, _) = self.next();
                self.token = if self.peek() == $next {
                    let (end, _) = self.next();
                    Token::new(TokenKind::$kind2, Span::new(start, end))
                } else {
                    Token::new(TokenKind::$kind1, Span::new(start, start))
                }
            }};
        }

        loop {
            let c = self.peek();
            if c == '\0' {
                self.token = Token::new(TokenKind::Eof, Span::initial());
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
    use super::*;
    use crate::token::ResWord::*;
    use crate::token::TokenKind::*;

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

    #[test]
    fn res_word() {
        let src: &'static str = "true false";
        let mut lex = Lexer::new(src);
        assert_eq!(ResWord(True), advance(&mut lex).kind);
        assert_eq!(ResWord(False), advance(&mut lex).kind);
    }
}
