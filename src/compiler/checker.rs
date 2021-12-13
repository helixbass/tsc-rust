use crate::{create_diagnostic_collection, for_each, Diagnostic, Node, SourceFile, TypeChecker};

pub fn create_type_checker(produce_diagnostics: bool) -> TypeChecker {
    TypeChecker {
        diagnostics: create_diagnostic_collection(),
    }
}

impl TypeChecker {
    fn check_source_element(&self, node: &Node) {}

    fn check_source_file(&self, source_file: &SourceFile) {
        self.check_source_file_worker(source_file)
    }

    fn check_source_file_worker(&self, node: &SourceFile) {
        if true {
            for_each(&node.statements, |statement, _index| {
                self.check_source_element(statement);
                Option::<()>::None
            });
        }
    }

    pub fn get_diagnostics(&self, source_file: &SourceFile) -> Vec<Box<dyn Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    fn get_diagnostics_worker(&self, source_file: &SourceFile) -> Vec<Box<dyn Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self.diagnostics.get_diagnostics(&source_file.file_name);

        semantic_diagnostics
            .into_iter()
            .map(|diagnostic| {
                let boxed: Box<dyn Diagnostic> = Box::new(diagnostic);
                boxed
            })
            .collect()
    }
}
