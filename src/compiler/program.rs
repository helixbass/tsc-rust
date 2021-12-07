use crate::{CreateProgramOptions, Diagnostic, Program, SourceFile};

struct ProgramConcrete {}

impl ProgramConcrete {
    pub fn new() -> Self {
        ProgramConcrete {}
    }

    fn get_diagnostics_helper(
        &self,
        get_diagnostics: fn(&ProgramConcrete, &SourceFile) -> Vec<Diagnostic>,
    ) -> Vec<Diagnostic> {
        self.get_source_files()
            .iter()
            .flat_map(|source_file| get_diagnostics(self, source_file))
            .collect()
    }

    fn get_semantic_diagnostics_for_file(&self, source_file: &SourceFile) -> Vec<Diagnostic> {
        vec![]
    }
}

impl Program for ProgramConcrete {
    fn get_semantic_diagnostics(&self) -> Vec<Diagnostic> {
        self.get_diagnostics_helper(ProgramConcrete::get_semantic_diagnostics_for_file)
    }

    fn get_source_files(&self) -> Vec<SourceFile> {
        vec![]
    }
}

pub fn create_program(root_names_or_options: CreateProgramOptions) -> impl Program {
    ProgramConcrete::new()
}
