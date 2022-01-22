use std::collections::HashMap;
use std::rc::Rc;

use crate::{CommandLineOption, CompilerOptions, ParsedCommandLine};

thread_local! {
    pub(crate) static module_resolution_option_declarations: Vec<CommandLineOption> =
        option_declarations.with(|option_declarations_| {
            option_declarations_
                .iter()
                .filter(|option| matches!(option.affects_module_resolution(), Some(true)))
                .collect()
        });
}

pub(crate) struct OptionsNameMap {
    pub options_name_map: HashMap<String, CommandLineOption>,
    pub short_option_names: HashMap<String, String>,
}

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
