use std::collections::HashMap;
use std::rc::Rc;

use super::execute_command_line_worker;
use crate::{
    create_program, emit_files_and_report_errors_and_get_exit_status, parse_build_command,
    parse_command_line, BuildOptions, CharacterCodes, CompilerOptions, CreateProgramOptions,
    Diagnostic, DiagnosticReporter, EmitAndSemanticDiagnosticsBuilderProgram,
    ExtendedConfigCacheEntry, ParsedBuildCommand, ParsedCommandLine, Program, System, WatchOptions,
};

pub fn is_build(command_line_args: &[String]) -> bool {
    if !command_line_args.is_empty()
        && matches!(
            command_line_args[0].chars().next(),
            Some(CharacterCodes::minus)
        )
    {
        let first_option: String = {
            let chars: Vec<char> = command_line_args[0].chars().collect();
            chars[(if matches!(chars.get(1), Some(&CharacterCodes::minus)) {
                2
            } else {
                1
            })..]
                .iter()
                .collect::<String>()
                .to_lowercase()
        };
        return matches!(&*first_option, "build" | "b");
    }
    false
}

pub fn execute_command_line<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    system: &dyn System,
    mut cb: TCallback,
    command_line_args: &[String],
) {
    if is_build(command_line_args) {
        let ParsedBuildCommand {
            build_options,
            watch_options,
            projects,
            errors,
        } = parse_build_command(&command_line_args[1..]);
        if let Some(build_options_generate_cpu_profile) =
            build_options.generate_cpu_profile.as_ref()
        {
            system.enable_cpu_profiler(build_options_generate_cpu_profile, &mut || {
                perform_build(
                    system,
                    &mut cb,
                    &build_options,
                    watch_options.as_ref(),
                    &projects,
                    &errors,
                )
            });
            return; // TODO: the Typescript version doesn't actually return here but seems like it should?
        } else {
            perform_build(
                system,
                &mut cb,
                &build_options,
                watch_options.as_ref(),
                &projects,
                &errors,
            );
            return;
        }
    }

    let mut command_line =
        parse_command_line(command_line_args, Some(|path: &str| system.read_file(path)));
    if let Some(command_line_options_generate_cpu_profile) =
        command_line.options.generate_cpu_profile.clone()
    {
        system.enable_cpu_profiler(&command_line_options_generate_cpu_profile, &mut || {
            execute_command_line_worker(system, &mut cb, &mut command_line)
        })
    } else {
        execute_command_line_worker(system, &mut cb, &mut command_line)
    }
}

pub enum ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine {
    Program(Rc<Program>),
    EmitAndSemanticDiagnosticsBuilderProgram(Rc<dyn EmitAndSemanticDiagnosticsBuilderProgram>),
    ParsedCommandLine(Rc<ParsedCommandLine>),
}

pub(super) fn report_watch_mode_without_sys_support(
    sys: &dyn System,
    report_diagnostic: &dyn DiagnosticReporter,
) -> bool {
    unimplemented!()
}

pub(super) fn perform_build<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    sys: &dyn System,
    cb: &mut TCallback,
    build_options: &BuildOptions,
    watch_options: Option<&WatchOptions>,
    projects: &[String],
    errors: &[Rc<Diagnostic>],
) {
    unimplemented!()
}

pub(super) fn perform_compilation<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    sys: &dyn System,
    mut cb: TCallback,
    report_diagnostic: Rc<dyn DiagnosticReporter>,
    config: &ParsedCommandLine,
) {
    let program_options = CreateProgramOptions {
        root_names: &config.file_names,
        options: config.options.clone(),
    };
    let program = create_program(program_options);
    let _exit_status = emit_files_and_report_errors_and_get_exit_status(program);
}

pub(super) fn perform_incremental_compilation<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    sys: &dyn System,
    mut cb: TCallback,
    report_diagnostic: Rc<dyn DiagnosticReporter>,
    config: &ParsedCommandLine,
) {
    unimplemented!()
}

pub(super) fn create_watch_of_config_file<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    system: &dyn System,
    mut cb: TCallback,
    report_diagnostic: Rc<dyn DiagnosticReporter>,
    config_parse_result: Rc<ParsedCommandLine>,
    options_to_extend: Rc<CompilerOptions>,
    watch_options_to_extend: Option<Rc<WatchOptions>>,
    extended_config_cache: HashMap<String, ExtendedConfigCacheEntry>,
) {
    unimplemented!()
}

pub(super) fn create_watch_of_files_and_compiler_options<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    system: &dyn System,
    mut cb: TCallback,
    report_diagnostic: Rc<dyn DiagnosticReporter>,
    root_files: &[String],
    options: Rc<CompilerOptions>,
    watch_options: Option<Rc<WatchOptions>>,
) {
    unimplemented!()
}

pub(super) fn write_config_file(
    sys: &dyn System,
    report_diagnostic: &dyn DiagnosticReporter,
    options: &CompilerOptions,
    file_names: &[String],
) {
    unimplemented!()
}
