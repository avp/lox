use crate::ast::*;
use crate::ctx::UniqueString;

use codespan_reporting::diagnostic::{Diagnostic, Label};

/// Store semantic information about the program.
pub struct SemInfo {
    /// Variable names as a list.
    /// The index in the list can be found via `find_var`.
    pub vars: Vec<UniqueString>,

    /// Strings in the program as a list.
    /// The index in the list can be found via `find_string`.
    pub strings: Vec<UniqueString>,
}

impl SemInfo {
    pub fn find_var(&self, name: &UniqueString) -> Option<usize> {
        // TODO: Don't use linear scan for variables, that's ridiculous.
        self.vars.iter().position(|n| n == name)
    }

    pub fn find_string(&self, string: &UniqueString) -> Option<usize> {
        // TODO: Don't use linear scan for strings, that's also ridiculous.
        self.vars.iter().position(|n| n == string)
    }

    pub fn add_string(&mut self, string: UniqueString) -> usize {
        match self.find_string(&string) {
            Some(idx) => idx,
            None => {
                let res = self.strings.len();
                self.strings.push(string);
                res
            }
        }
    }
}

pub struct SemanticValidator<'ast> {
    file: Option<&'ast Function>,
    file_id: codespan::FileId,
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
    pub fn run(
        file: &'ast Function,
        file_id: codespan::FileId,
    ) -> Result<SemInfo> {
        let mut sem = SemanticValidator {
            file: None,
            file_id,
            sem: SemInfo {
                vars: vec![],
                strings: vec![],
            },
            errors: vec![],
        };
        sem.visit_func(file);
        if !sem.errors.is_empty() {
            return Err(sem.errors);
        }
        Ok(sem.sem)
    }
}

impl<'ast> Visitor<'ast> for SemanticValidator<'ast> {
    type Output = ();

    fn visit_func(&mut self, file: &'ast Function) {
        self.file = Some(file);
        file.visit_children(self);
    }

    fn visit_block(&mut self, block: &'ast Block) {
        block.visit_children(self);
    }

    fn visit_decl(&mut self, decl: &'ast Decl) {
        if let DeclKind::Var(name, _) = &decl.kind {
            self.sem.vars.push(name.clone());
        }
        decl.visit_children(self);
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        stmt.visit_children(self);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let ExprKind::Ident(name) = &expr.kind {
            if self.sem.find_var(name).is_none() {
                self.errors.push(SemError(Diagnostic::new_error(
                    format!("Undeclared variable '{}'", name),
                    Label::new(self.file_id, expr.span, ""),
                )))
            }
        }
        if let ExprKind::StringLiteral(s) = &expr.kind {
            self.sem.add_string(s.clone());
        }
        expr.visit_children(self);
    }
}
