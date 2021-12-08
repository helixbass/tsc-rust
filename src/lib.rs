#[macro_use]
extern crate lazy_static;

mod compiler;
mod execute_command_line;

pub use compiler::command_line_parser::parse_command_line;
pub use compiler::core::{for_each, last_or_undefined};
pub use compiler::path::{normalize_path, to_path};
pub use compiler::program::create_program;
pub use compiler::sys::{get_sys, System};
pub use compiler::types::{
    CharacterCodes, CompilerHost, CreateProgramOptions, Diagnostic, ExitStatus, ParsedCommandLine,
    Path, Program, SourceFile, StructureIsReused,
};
pub use compiler::watch::emit_files_and_report_errors_and_get_exit_status;
pub use execute_command_line::execute_command_line::execute_command_line;
