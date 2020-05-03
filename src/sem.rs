use crate::ast::ASTNode;
use crate::ast::*;
use crate::ctx::UniqueString;

use codespan_reporting::diagnostic::{Diagnostic, Label};

pub struct SemInfo {
    pub vars: Vec<UniqueString>,
}

impl SemInfo {
    pub fn find_var(&self, name: &UniqueString) -> Option<usize> {
        self.vars.iter().position(|n| n == name)
    }
}

pub struct SemanticValidator<'ast> {
    file: Option<&'ast File>,
    sem: SemInfo,
    errors: Vec<SemError>,
}

pub type Result<T> = std::result::Result<T, Vec<SemError>>;

#[derive(Debug, Fail)]
#[fail(display = "Compilation failed")]
pub struct SemError(Diagnostic);

impl SemError {
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

impl<'ast> SemanticValidator<'ast> {
    pub fn run(file: &'ast File) -> Result<SemInfo> {
        let mut sem = SemanticValidator {
            file: None,
            sem: SemInfo { vars: vec![] },
            errors: vec![],
        };
        sem.visit_file(file);
        if sem.errors.len() > 0 {
            return Err(sem.errors);
        }
        Ok(sem.sem)
    }
}

impl<'ast> Visitor<'ast> for SemanticValidator<'ast> {
    type Output = ();

    fn visit_file(&mut self, file: &'ast File) {
        self.file = Some(file);
        file.visit_children(self);
    }

    fn visit_decl(&mut self, decl: &'ast Decl) {
        match &decl.kind {
            DeclKind::Var(name, _) => {
                self.sem.vars.push(name.clone());
            }
            _ => {}
        };
        decl.visit_children(self);
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        stmt.visit_children(self);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        match &expr.kind {
            ExprKind::Ident(name) => {
                if !self.sem.find_var(name).is_some() {
                    self.errors.push(SemError(Diagnostic::new_error(
                        format!("Undeclared variable '{}'", name),
                        Label::new(self.file.unwrap().id, expr.span, ""),
                    )))
                }
            }
            _ => {}
        }
        expr.visit_children(self);
    }
}
