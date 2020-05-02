use crate::ast::*;

pub struct SemanticValidator<'ast> {
    file: Option<&'ast mut File>,
}

impl<'ast> SemanticValidator<'ast> {
    pub fn run(file: &'ast mut File) {
        let mut sem = SemanticValidator { file: None };
        sem.visit_file(file);
    }
}

impl<'ast> MutVisitor<'ast, ()> for SemanticValidator<'ast> {
    fn visit_file(&mut self, file: &'ast mut File) {
        self.file = Some(file);
    }

    fn visit_decl(&mut self, decl: &'ast mut Decl) {
        match &decl.kind {
            DeclKind::Var(name, _) => self.file
                .as_mut()
                .unwrap()
                .locals
                .push(name.clone()),
            _ => {}
        }
    }

    fn visit_stmt(&mut self, _: &'ast mut Stmt) {}

    fn visit_expr(&mut self, _: &'ast mut Expr) {}
}
