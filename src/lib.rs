#[macro_use]
extern crate lazy_static;

mod compiler;
mod execute_command_line;

pub use compiler::command_line_parser::parse_command_line;
pub use compiler::sys::{System, SYS};
pub use compiler::types::ParsedCommandLine;
pub use execute_command_line::execute_command_line::execute_command_line;
