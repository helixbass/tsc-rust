use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    create_diagnostic_reporter, create_program, emit_files_and_report_errors_and_get_exit_status,
    file_extension_is, file_extension_is_one_of, for_each, get_line_starts, parse_command_line,
    supported_js_extensions_flat, supported_ts_extensions_flat, BuildOptions, CompilerOptions,
    CreateProgramOptions, DiagnosticReporter, EmitAndSemanticDiagnosticsBuilderProgram, Extension,
    Node, ParsedCommandLine, Program, System, TypeCheckerHost,
};

struct Statistic {
    pub name: String,
    pub value: String,
}

fn count_lines(program: &Program) -> HashMap<&'static str, usize> {
    let mut counts = get_counts_map();
    for_each(program.get_source_files(), |file, _| {
        let key = get_count_key(program, file);
        let line_count = get_line_starts(file.as_source_file()).len();
        counts.insert(key, *counts.get(key).unwrap() + line_count);
        Option::<()>::None
    });
    counts
}

fn count_nodes(program: &Program) -> HashMap<&'static str, usize> {
    let mut counts = get_counts_map();
    for_each(program.get_source_files(), |file, _| {
        let key = get_count_key(program, file);
        let file_as_source_file = file.as_source_file();
        let line_count = get_line_starts(file_as_source_file).len();
        counts.insert(
            key,
            *counts.get(key).unwrap() + file_as_source_file.node_count(),
        );
        Option::<()>::None
    });
    counts
}

fn get_counts_map() -> HashMap<&'static str, usize> {
    let mut counts = HashMap::new();
    counts.insert("Library", 0);
    counts.insert("Definitions", 0);
    counts.insert("TypeScript", 0);
    counts.insert("JavaScript", 0);
    counts.insert("JSON", 0);
    counts.insert("Other", 0);
    counts
}

fn get_count_key(program: &Program, file: &Node /*SourceFile*/) -> &'static str {
    let file_as_source_file = file.as_source_file();
    if program.is_source_file_default_library(file) {
        return "Library";
    } else if file_as_source_file.is_declaration_file() {
        return "Definitions";
    }

    let path = file_as_source_file.path();
    if file_extension_is_one_of(
        &path,
        &supported_ts_extensions_flat
            .iter()
            .map(|extension| extension.to_str())
            .collect::<Vec<_>>(),
    ) {
        "TypeScript"
    } else if file_extension_is_one_of(
        &path,
        &supported_js_extensions_flat
            .iter()
            .map(|extension| extension.to_str())
            .collect::<Vec<_>>(),
    ) {
        "JavaScript"
    } else if file_extension_is(&path, Extension::Json.to_str()) {
        "JSON"
    } else {
        "Other"
    }
}

fn update_report_diagnostic(
    sys: &dyn System,
    existing: Rc<dyn DiagnosticReporter>,
    options: CompilerOptionsOrBuildOptions,
) -> Rc<dyn DiagnosticReporter> {
    if should_be_pretty(sys, options) {
        create_diagnostic_reporter(sys, Some(true))
    } else {
        existing
    }
}

enum CompilerOptionsOrBuildOptions {
    CompilerOptions(Rc<CompilerOptions>),
    BuildOptions(Rc<BuildOptions>),
}

impl CompilerOptionsOrBuildOptions {
    pub fn pretty(&self) -> Option<bool> {
        match self {
            Self::CompilerOptions(options) => options.pretty,
            Self::BuildOptions(options) => options.pretty,
        }
    }
}

fn default_is_pretty(sys: &dyn System) -> bool {
    matches!(sys.write_output_is_tty(), Some(true))
        && sys.get_environment_variable("NO_COLOR").is_empty()
}

fn should_be_pretty(sys: &dyn System, options: CompilerOptionsOrBuildOptions) -> bool {
    if
    /* !options || */
    options.pretty().is_none() {
        return default_is_pretty(sys);
    }
    options.pretty().unwrap()
}

pub fn execute_command_line<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    system: &dyn System,
    mut cb: TCallback,
    command_line_args: &[String],
) {
    let command_line = parse_command_line(command_line_args);
    execute_command_line_worker(system, command_line)
}

pub enum ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine {
    Program(Rc<Program>),
    EmitAndSemanticDiagnosticsBuilderProgram(Rc<dyn EmitAndSemanticDiagnosticsBuilderProgram>),
    ParsedCommandLine(Rc<ParsedCommandLine>),
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
