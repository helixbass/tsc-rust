use std::collections::HashMap;
use std::rc::Rc;

use super::{
    execute_command_line_worker, print_build_help, print_version, should_be_pretty,
    update_report_diagnostic, CompilerOptionsOrBuildOptions,
};
use crate::{
    build_opts, create_builder_status_reporter, create_compiler_diagnostic,
    create_diagnostic_reporter, create_program, create_solution_builder,
    create_solution_builder_host, create_solution_builder_with_watch,
    create_solution_builder_with_watch_host, dump_tracing_legend,
    emit_files_and_report_errors_and_get_exit_status, parse_build_command, parse_command_line,
    validate_locale_and_set_language, BuildOptions, BuilderProgram, CharacterCodes,
    CompilerOptions, CreateProgram, CreateProgramOptions, CustomTransformers, Diagnostic,
    DiagnosticReporter, Diagnostics, EmitAndSemanticDiagnosticsBuilderProgram, ExitStatus,
    ExtendedConfigCacheEntry, ParsedBuildCommand, ParsedCommandLine, Program,
    ReportEmitErrorSummary, SemanticDiagnosticsBuilderProgram, SolutionBuilderHostBase, System,
    WatchOptions, WatchStatusReporter,
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
        let build_options = Rc::new(build_options);
        if let Some(build_options_generate_cpu_profile) =
            build_options.generate_cpu_profile.as_ref()
        {
            system.enable_cpu_profiler(build_options_generate_cpu_profile, &mut || {
                perform_build(
                    system,
                    &mut cb,
                    build_options.clone(),
                    watch_options.as_ref(),
                    &projects,
                    errors.clone(),
                )
            });
            return; // TODO: the Typescript version doesn't actually return here but seems like it should?
        } else {
            perform_build(
                system,
                &mut cb,
                build_options.clone(),
                watch_options.as_ref(),
                &projects,
                errors,
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
    if !sys.is_watch_file_supported() || !sys.is_watch_directory_supported() {
        report_diagnostic.call(Rc::new(
            create_compiler_diagnostic(
                &Diagnostics::The_current_host_does_not_support_the_0_option,
                Some(vec!["--watch".to_owned()]),
            )
            .into(),
        ));
        sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
    }
    false
}

pub(super) fn perform_build<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    sys: &dyn System,
    cb: &mut TCallback,
    build_options: Rc<BuildOptions>,
    watch_options: Option<&WatchOptions>,
    projects: &[String],
    mut errors: Vec<Rc<Diagnostic>>,
) {
    let report_diagnostic = update_report_diagnostic(
        sys,
        create_diagnostic_reporter(sys, None),
        build_options.clone().into(),
    );

    if let Some(build_options_locale) = build_options.locale.as_ref() {
        if !build_options_locale.is_empty() {
            validate_locale_and_set_language(build_options_locale, sys, Some(&mut errors));
        }
    }

    if !errors.is_empty() {
        errors
            .into_iter()
            .for_each(|error| report_diagnostic.call(error));
        sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
    }

    if matches!(build_options.help, Some(true)) {
        print_version(sys);
        build_opts.with(|build_opts_| {
            print_build_help(sys, &build_opts_);
        });
        sys.exit(Some(ExitStatus::Success));
    }

    if projects.is_empty() {
        print_version(sys);
        build_opts.with(|build_opts_| {
            print_build_help(sys, &build_opts_);
        });
        sys.exit(Some(ExitStatus::Success));
    }

    if !sys.is_get_modified_time_supported()
        || !sys.is_set_modified_time_supported()
        || matches!(build_options.clean, Some(true)) && !sys.is_delete_file_supported()
    {
        report_diagnostic.call(Rc::new(
            create_compiler_diagnostic(
                &Diagnostics::The_current_host_does_not_support_the_0_option,
                Some(vec!["--build".to_owned()]),
            )
            .into(),
        ));
        sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
    }

    if matches!(build_options.watch, Some(true)) {
        if report_watch_mode_without_sys_support(sys, &*report_diagnostic) {
            return;
        }
        let mut build_host = create_solution_builder_with_watch_host(
            Some(sys),
            Option::<CreateProgramDummy>::None,
            Some(report_diagnostic.clone()),
            Some(create_builder_status_reporter(
                sys,
                Some(should_be_pretty(sys, build_options.clone().into())),
            )),
            Some(create_watch_status_reporter(
                sys,
                build_options.clone().into(),
            )),
        );
        update_solution_builder_host(sys, cb, &mut build_host);
        let builder = create_solution_builder_with_watch(
            &build_host,
            projects,
            &build_options,
            watch_options,
        );
        builder.build(
            None,
            None,
            None,
            Option::<fn(&str) -> CustomTransformers>::None,
        );
        return /*builder*/;
    }

    let mut build_host = create_solution_builder_host(
        Some(sys),
        Option::<CreateProgramDummy>::None,
        Some(report_diagnostic.clone()),
        Some(create_builder_status_reporter(
            sys,
            Some(should_be_pretty(sys, build_options.clone().into())),
        )),
        create_report_error_summary(sys, build_options.clone().into()),
    );
    update_solution_builder_host(sys, cb, &mut build_host);
    let builder = create_solution_builder(&build_host, projects, &build_options);
    let exit_status = if matches!(build_options.clean, Some(true)) {
        builder.clean(None)
    } else {
        builder.build(
            None,
            None,
            None,
            Option::<fn(&str) -> CustomTransformers>::None,
        )
    };
    dump_tracing_legend();
    sys.exit(Some(exit_status));
}

struct BuilderProgramDummy {}

impl BuilderProgram for BuilderProgramDummy {}

impl SemanticDiagnosticsBuilderProgram for BuilderProgramDummy {}

impl EmitAndSemanticDiagnosticsBuilderProgram for BuilderProgramDummy {}

struct CreateProgramDummy {}

impl CreateProgram<BuilderProgramDummy> for CreateProgramDummy {}

pub(super) fn create_report_error_summary(
    sys: &dyn System,
    options: CompilerOptionsOrBuildOptions,
) -> Option<ReportEmitErrorSummary> {
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

pub(super) fn update_solution_builder_host<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
    TEmitAndSemanticDiagnosticsBuilderProgram: EmitAndSemanticDiagnosticsBuilderProgram,
    TBuildHost: SolutionBuilderHostBase<TEmitAndSemanticDiagnosticsBuilderProgram>,
>(
    sys: &dyn System,
    mut cb: TCallback,
    build_host: &mut TBuildHost,
) {
    unimplemented!()
}

pub(super) fn create_watch_status_reporter(
    sys: &dyn System,
    options: CompilerOptionsOrBuildOptions,
) -> WatchStatusReporter {
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
