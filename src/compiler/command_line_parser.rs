use crate::ParsedCommandLine;

pub fn parse_command_line(command_line: &[String]) -> ParsedCommandLine {
    parse_command_line_worker(command_line)
}

fn parse_command_line_worker(command_line: &[String]) -> ParsedCommandLine {
    ParsedCommandLine {
        file_names: command_line.to_vec(),
    }
}
