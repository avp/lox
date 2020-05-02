use crate::ast::*;
use crate::ctx::UniqueString;

pub struct SemInfo {
    pub vars: Vec<UniqueString>,
}

pub struct SemanticValidator<'ast> {
    file: Option<&'ast File>,
    sem: SemInfo,
}

impl<'ast> SemanticValidator<'ast> {
    pub fn run(file: &'ast File) -> SemInfo {
        let mut sem = SemanticValidator {
            file: None,
            sem: SemInfo { vars: vec![] },
        };
        sem.visit_file(file);
        sem.sem
    }
}

impl<'ast> Visitor<'ast, ()> for SemanticValidator<'ast> {
    fn visit_file(&mut self, file: &'ast File) {
        self.file = Some(file);
        for decl in &file.decls {
            self.visit_decl(decl.as_ref());
        }
    }

    fn visit_decl(&mut self, decl: &Decl) {
        match &decl.kind {
            DeclKind::Var(name, _) => self.sem.vars.push(name.clone()),
            _ => {}
        }
    }

    fn visit_stmt(&mut self, _: &Stmt) {}

    fn visit_expr(&mut self, _: &Expr) {}
}
