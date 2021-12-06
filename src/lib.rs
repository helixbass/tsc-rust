#[macro_use]
extern crate lazy_static;

mod compiler;
mod execute_command_line;

pub use compiler::command_line_parser::parse_command_line;
pub use compiler::program::create_program;
pub use compiler::sys::{System, SYS};
pub use compiler::types::{CreateProgramOptions, ExitStatus, ParsedCommandLine, Program};
pub use compiler::watch::emit_files_and_report_errors_and_get_exit_status;
pub use execute_command_line::execute_command_line::execute_command_line;
