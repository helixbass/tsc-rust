use crate::{parse_command_line, ParsedCommandLine, System};

pub fn execute_command_line(system: &System, command_line_args: &[String]) {
    let command_line = parse_command_line(command_line_args);
    execute_command_line_worker(system, command_line)
}

pub fn execute_command_line_worker(system: &System, command_line: ParsedCommandLine) {}
