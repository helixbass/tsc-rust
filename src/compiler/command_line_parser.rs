use std::rc::Rc;

use crate::{CompilerOptions, ParsedCommandLine};

pub fn parse_command_line(command_line: &[String]) -> ParsedCommandLine {
    parse_command_line_worker(command_line)
}

fn parse_command_line_worker(command_line: &[String]) -> ParsedCommandLine {
    ParsedCommandLine {
        options: Rc::new(CompilerOptions {
            target: None,
            module: None,
        }),
        file_names: command_line.to_vec(),
    }
}
