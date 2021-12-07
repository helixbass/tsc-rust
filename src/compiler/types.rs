pub struct SourceFile {}

pub trait Program {
    fn get_source_files(&self) -> Vec<SourceFile>;
    fn get_semantic_diagnostics(&self) -> Vec<Diagnostic>;
}

pub enum ExitStatus {
    Success,
    DiagnosticsPresent_OutputsGenerated,
}

pub struct ParsedCommandLine {
    pub file_names: Vec<String>,
}

pub struct CreateProgramOptions<'config> {
    pub root_names: &'config [String],
}

pub struct Diagnostic {}
