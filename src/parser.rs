use codespan::Span;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

pub struct Parser<'src> {
    lexer: Lexer<'src>,
}

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Fail)]
pub enum ParseError {
    #[fail(display = "Unexpected token: {:?}", expected)]
    Unexpected { expected: TokenKind },
}

impl<'src> Parser<'src> {
    pub fn parse(text: &'src str) -> Result<File> {
        let mut parser = Parser {
            lexer: Lexer::new(text),
        };
        parser.lexer.advance();
        parser.parse_file()
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.lexer.token.kind == kind
    }

    fn need(&self, kind: TokenKind) -> Result<()> {
        if self.lexer.token.kind == kind {
            Ok(())
        } else {
            Err(ParseError::Unexpected { expected: kind })
        }
    }

    fn eat(&mut self, kind: TokenKind) -> Result<()> {
        if self.lexer.token.kind == kind {
            self.lexer.advance();
            Ok(())
        } else {
            Err(ParseError::Unexpected { expected: kind })
        }
    }

    fn parse_file(&mut self) -> Result<File> {
        let mut decls = vec![];
        while !self.check(TokenKind::Eof) {
            decls.push(Box::new(self.parse_decl()?));
        }
        let span = match decls.first() {
            Some(d) => d.span.merge(decls.last().unwrap().span),
            None => Span::initial(),
        };
        Ok(File { decls, span })
    }

    fn parse_decl(&mut self) -> Result<Decl> {
        let stmt = self.parse_stmt()?;
        let span = stmt.span;
        Ok(Decl {
            kind: DeclKind::Stmt(Box::new(stmt)),
            span,
        })
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        let expr: Box<Expr> = self.parse_expr()?;
        let span = expr.span;
        Ok(Stmt {
            kind: StmtKind::Expr(expr),
            span,
        })
    }

    fn parse_expr(&mut self) -> Result<Box<Expr>> {
        Ok(self.parse_binary()?)
    }

    fn parse_binary(&mut self) -> Result<Box<Expr>> {
        let mut top: Box<Expr> = self.parse_unary()?;

        fn get_prec(kind: &TokenKind) -> u32 {
            match kind {
                TokenKind::Plus | TokenKind::Minus => 1,
                TokenKind::Star | TokenKind::Slash => 2,
                _ => 0,
            }
        }

        fn new_node(
            left: Box<Expr>,
            op: &TokenKind,
            right: Box<Expr>,
        ) -> Box<Expr> {
            let span = left.span.merge(right.span);
            Box::new(Expr {
                kind: ExprKind::BinOp(
                    match op {
                        TokenKind::Plus => BinOpKind::Add,
                        TokenKind::Minus => BinOpKind::Sub,
                        TokenKind::Star => BinOpKind::Mul,
                        TokenKind::Slash => BinOpKind::Div,
                        _ => unreachable!(
                            "Invalid token in binop stack: {:?}",
                            op
                        ),
                    },
                    left,
                    right,
                ),
                span,
            })
        }

        let mut stack: Vec<(Box<Expr>, TokenKind)> = vec![];

        loop {
            let prec = get_prec(&self.lexer.token.kind);
            if prec == 0 {
                break;
            }
            while let Some((_, tok)) = stack.last() {
                if prec > get_prec(&tok) {
                    break;
                }
                let (expr, tok) = stack.pop().unwrap();
                top = new_node(expr, &tok, top);
            }

            let tok = self.lexer.token.kind;
            self.lexer.advance();
            let mut right = self.parse_unary()?;
            std::mem::swap(&mut top, &mut right);
            stack.push((right, tok));
        }

        while let Some((expr, tok)) = stack.pop() {
            top = new_node(expr, &tok, top);
        }

        Ok(top)
    }

    fn parse_unary(&mut self) -> Result<Box<Expr>> {
        let start = self.lexer.token.span;
        match self.lexer.token.kind {
            TokenKind::Plus => {
                self.lexer.advance();
                self.parse_expr()
            }
            TokenKind::Minus => {
                self.lexer.advance();
                let expr = self.parse_expr()?;
                let span = start.merge(expr.span);
                Ok(Box::new(Expr {
                    kind: ExprKind::UnOp(UnOpKind::Neg, expr),
                    span,
                }))
            }
            TokenKind::Bang => {
                self.lexer.advance();
                let expr = self.parse_expr()?;
                let span = start.merge(expr.span);
                Ok(Box::new(Expr {
                    kind: ExprKind::UnOp(UnOpKind::Not, expr),
                    span,
                }))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Box<Expr>> {
        let start = self.lexer.token.span;
        let kind = match self.lexer.token.kind {
            TokenKind::Ident => {
                ExprKind::Ident(self.lexer.token.string.clone().unwrap())
            }
            TokenKind::NumberLiteral => {
                ExprKind::NumberLiteral(self.lexer.token.number.unwrap())
            }
            TokenKind::StringLiteral => ExprKind::StringLiteral(
                self.lexer.token.string.clone().unwrap(),
            ),
            TokenKind::LParen => {
                self.lexer.advance();
                let mut nested = self.parse_expr()?;
                nested.span = start.merge(self.lexer.token.span);
                self.eat(TokenKind::RParen)?;
                return Ok(nested);
            }
            _ => unreachable!("Invalid primary: {:?}", self.lexer.token),
        };
        self.lexer.advance();
        Ok(Box::new(Expr {
            kind,
            span: start,
        }))
    }
}
