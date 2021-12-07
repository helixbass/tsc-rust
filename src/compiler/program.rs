use crate::{CreateProgramOptions, Diagnostic, Program};

struct ProgramConcrete {}

impl ProgramConcrete {
    pub fn new() -> Self {
        ProgramConcrete {}
    }
}

impl Program for ProgramConcrete {
    fn get_semantic_diagnostics(&self) -> Vec<Diagnostic> {
        vec![]
    }
}

pub fn create_program(root_names_or_options: CreateProgramOptions) -> impl Program {
    ProgramConcrete::new()
}
