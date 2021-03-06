use codespan::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::ast::*;
use crate::ctx::*;
use crate::lexer::*;
use crate::token::*;

pub struct Parser<'ctx, 'src> {
    lexer: Lexer<'ctx, 'src>,
    file_id: codespan::FileId,
}

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Fail)]
#[fail(display = "Compilation failed")]
pub struct ParseError(Diagnostic);

impl ParseError {
    pub fn emit<T: AsRef<str>>(&self, files: &codespan::Files<T>) {
        use codespan_reporting::term;
        let d = &self.0;
        let writer = term::termcolor::StandardStream::stderr(
            term::termcolor::ColorChoice::Auto,
        );
        let config = term::Config::default();
        term::emit(&mut writer.lock(), &config, files, &d).unwrap();
    }
}

impl<'ctx, 'src> Parser<'ctx, 'src> {
    pub fn parse(
        ctx: &'ctx mut Ctx,
        files: &codespan::Files<&'src str>,
        file_id: codespan::FileId,
    ) -> Result<P<Function>> {
        let mut parser = Parser {
            lexer: Lexer::new(ctx, files.source(file_id)),
            file_id,
        };
        parser.lexer.advance();
        parser.parse_func()
    }

    fn unexpected(&self) -> ParseError {
        ParseError(Diagnostic::new_error(
            format!("Unexpected '{}'", self.lexer.token.kind),
            Label::new(self.file_id, self.lexer.token.span, ""),
        ))
    }

    fn expected(&self, kind: TokenKind) -> ParseError {
        ParseError(Diagnostic::new_error(
            format!(
                "Expected '{}' but found '{}'",
                kind, self.lexer.token.kind
            ),
            Label::new(self.file_id, self.lexer.token.span, ""),
        ))
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.lexer.token.kind == kind
    }

    fn check_eat(&mut self, kind: TokenKind) -> bool {
        if self.lexer.token.kind == kind {
            self.lexer.advance();
            true
        } else {
            false
        }
    }

    fn need(&mut self, kind: TokenKind) -> Result<()> {
        if self.lexer.token.kind == kind {
            Ok(())
        } else {
            Err(self.expected(kind))
        }
    }

    fn eat(&mut self, kind: TokenKind) -> Result<()> {
        if self.lexer.token.kind == kind {
            self.lexer.advance();
            Ok(())
        } else {
            Err(self.expected(kind))
        }
    }

    fn parse_func(&mut self) -> Result<P<Function>> {
        let mut decls = vec![];
        while !self.check(TokenKind::Eof) {
            decls.push(self.parse_decl()?);
        }
        let span = match decls.first() {
            Some(d) => d.span.merge(decls.last().unwrap().span),
            None => Span::initial(),
        };
        Ok(P::new(Function {
            params: vec![],
            body: P::new(Block { decls, span }),
            span,
        }))
    }

    fn parse_block(&mut self) -> Result<P<Block>> {
        assert!(self.check(TokenKind::LBrace));
        let start = self.lexer.token.span;
        self.eat(TokenKind::LBrace)?;
        let mut decls = vec![];
        while !self.check(TokenKind::RBrace) {
            decls.push(self.parse_decl()?);
        }
        let span = start.merge(self.lexer.token.span);
        self.eat(TokenKind::RBrace)?;
        Ok(P::new(Block { decls, span }))
    }

    fn parse_decl(&mut self) -> Result<P<Decl>> {
        let start = self.lexer.token.span;
        if self.check_eat(TokenKind::ResWord(ResWord::Var)) {
            self.need(TokenKind::Ident)?;
            let ident = self.lexer.token.get_string();
            let mut span = start.merge(self.lexer.token.span);
            self.lexer.advance();
            let expr = if self.check_eat(TokenKind::Equal) {
                let expr: P<Expr> = self.parse_expr()?;
                span = start.merge(expr.span);
                Some(expr)
            } else {
                None
            };
            self.eat(TokenKind::Semi)?;
            Ok(P::new(Decl {
                kind: DeclKind::Var(ident, expr),
                span,
            }))
        } else {
            let stmt = self.parse_stmt()?;
            let span = stmt.span;
            Ok(P::new(Decl {
                kind: DeclKind::Stmt(stmt),
                span,
            }))
        }
    }

    fn parse_stmt(&mut self) -> Result<P<Stmt>> {
        let start = self.lexer.token.span;
        if self.check_eat(TokenKind::ResWord(ResWord::Print)) {
            let expr: P<Expr> = self.parse_expr()?;
            let span = start.merge(expr.span);
            self.eat(TokenKind::Semi)?;
            Ok(P::new(Stmt {
                kind: StmtKind::Print(expr),
                span,
            }))
        } else if self.check_eat(TokenKind::ResWord(ResWord::While)) {
            self.eat(TokenKind::LParen)?;
            let expr = self.parse_expr()?;
            self.eat(TokenKind::RParen)?;
            self.need(TokenKind::LBrace)?;
            let body = self.parse_block()?;
            let span = start.merge(body.span);
            Ok(P::new(Stmt {
                kind: StmtKind::While(expr, body),
                span,
            }))
        } else if self.check_eat(TokenKind::ResWord(ResWord::Return)) {
            let expr: P<Expr> = self.parse_expr()?;
            let span = start.merge(expr.span);
            self.eat(TokenKind::Semi)?;
            Ok(P::new(Stmt {
                kind: StmtKind::Return(expr),
                span,
            }))
        } else {
            let expr: P<Expr> = self.parse_expr()?;
            let span = expr.span;
            self.eat(TokenKind::Semi)?;
            Ok(P::new(Stmt {
                kind: StmtKind::Expr(expr),
                span,
            }))
        }
    }

    fn parse_expr(&mut self) -> Result<P<Expr>> {
        let left = self.parse_binary()?;
        if self.check_eat(TokenKind::Equal) {
            let right = self.parse_expr()?;
            let span = left.span.merge(right.span);
            Ok(P::new(Expr {
                kind: ExprKind::Assign(left, right),
                span,
            }))
        } else {
            Ok(left)
        }
    }

    fn parse_binary(&mut self) -> Result<P<Expr>> {
        let mut top: P<Expr> = self.parse_unary()?;

        fn get_prec(kind: &TokenKind) -> u32 {
            match kind {
                TokenKind::Plus | TokenKind::Minus => 1,
                TokenKind::Star | TokenKind::Slash => 2,
                TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::Less
                | TokenKind::LessEqual => 4,
                TokenKind::EqualEqual | TokenKind::BangEqual => 4,
                _ => 0,
            }
        }

        fn new_node(left: P<Expr>, op: &TokenKind, right: P<Expr>) -> P<Expr> {
            let span = left.span.merge(right.span);
            P::new(Expr {
                kind: ExprKind::BinOp(
                    match op {
                        TokenKind::Plus => BinOpKind::Add,
                        TokenKind::Minus => BinOpKind::Sub,
                        TokenKind::Star => BinOpKind::Mul,
                        TokenKind::Slash => BinOpKind::Div,
                        TokenKind::EqualEqual => BinOpKind::Equal,
                        TokenKind::BangEqual => BinOpKind::NotEqual,
                        TokenKind::Greater => BinOpKind::Greater,
                        TokenKind::GreaterEqual => BinOpKind::GreaterEqual,
                        TokenKind::Less => BinOpKind::Less,
                        TokenKind::LessEqual => BinOpKind::LessEqual,
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

        let mut stack: Vec<(P<Expr>, TokenKind)> = vec![];

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

    fn parse_unary(&mut self) -> Result<P<Expr>> {
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
                Ok(P::new(Expr {
                    kind: ExprKind::UnOp(UnOpKind::Neg, expr),
                    span,
                }))
            }
            TokenKind::Bang => {
                self.lexer.advance();
                let expr = self.parse_expr()?;
                let span = start.merge(expr.span);
                Ok(P::new(Expr {
                    kind: ExprKind::UnOp(UnOpKind::Not, expr),
                    span,
                }))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<P<Expr>> {
        let start = self.lexer.token.span;
        let kind = match self.lexer.token.kind {
            TokenKind::Ident => {
                ExprKind::Ident(self.lexer.token.string.clone().unwrap())
            }
            TokenKind::ResWord(ResWord::True) => ExprKind::BoolLiteral(true),
            TokenKind::ResWord(ResWord::False) => ExprKind::BoolLiteral(false),
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
            _ => return Err(self.unexpected()),
        };
        self.lexer.advance();
        Ok(P::new(Expr { kind, span: start }))
    }
}
