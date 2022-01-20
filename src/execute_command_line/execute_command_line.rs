use crate::{
    create_program, emit_files_and_report_errors_and_get_exit_status, parse_command_line,
    CreateProgramOptions, ParsedCommandLine, System,
};

pub fn execute_command_line(system: &dyn System, command_line_args: &[String]) {
    let command_line = parse_command_line(command_line_args);
    execute_command_line_worker(system, command_line)
}

fn execute_command_line_worker(sys: &dyn System, command_line: ParsedCommandLine) {
    perform_compilation(sys, command_line)
}

fn perform_compilation(_sys: &dyn System, config: ParsedCommandLine) {
    let program_options = CreateProgramOptions {
        root_names: &config.file_names,
        options: config.options,
    };
    let program = create_program(program_options);
    let _exit_status = emit_files_and_report_errors_and_get_exit_status(program);
}
