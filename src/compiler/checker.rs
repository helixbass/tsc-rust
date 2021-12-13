use crate::{create_diagnostic_collection, Diagnostic, SourceFile, TypeChecker};

pub fn create_type_checker(produce_diagnostics: bool) -> TypeChecker {
    TypeChecker {
        diagnostics: create_diagnostic_collection(),
    }
}

impl TypeChecker {
    fn check_source_file(&self, source_file: &SourceFile) {}

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
