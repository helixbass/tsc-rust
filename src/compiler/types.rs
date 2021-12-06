pub struct ParsedCommandLine {
    pub file_names: Vec<String>,
}

pub struct CreateProgramOptions<'config> {
    pub root_names: &'config [String],
}

pub struct Program {}

pub enum ExitStatus {
    Success,
}
