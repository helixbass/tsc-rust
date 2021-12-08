pub struct Path(String);

impl Path {
    pub fn new(string: String) -> Self {
        Self(string)
    }
}

pub struct SourceFile {}

pub trait Program {
    fn get_source_files(&self) -> &[SourceFile];
    fn get_semantic_diagnostics(&self) -> Vec<Diagnostic>;
}

pub enum StructureIsReused {
    Not,
    Completely,
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

#[non_exhaustive]
pub struct CharacterCodes;
impl CharacterCodes {
    pub const slash: char = '/';
}

pub trait CompilerHost {
    fn get_current_directory(&self) -> String;
    fn get_canonical_file_name(&self, file_name: &str) -> String;
}

pub struct Diagnostic {}
