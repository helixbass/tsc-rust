use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{
    execute_command_line_worker, print_build_help, print_version, should_be_pretty,
    update_report_diagnostic, CompilerOptionsOrBuildOptions, Statistic,
};
use crate::{
    build_opts, change_compiler_host_like_to_use_cache, create_builder_status_reporter,
    create_compiler_diagnostic, create_compiler_host_worker, create_diagnostic_reporter,
    create_get_canonical_file_name, create_incremental_compiler_host, create_program,
    create_solution_builder, create_solution_builder_host, create_solution_builder_with_watch,
    create_solution_builder_with_watch_host, create_watch_compiler_host_of_config_file,
    create_watch_program, create_watch_status_reporter as create_watch_status_reporter_,
    dump_tracing_legend, emit_files_and_report_errors_and_get_exit_status,
    get_config_file_parsing_diagnostics, get_error_summary_text, get_sys, parse_build_command,
    parse_command_line, perform_incremental_compilation as perform_incremental_compilation_,
    start_tracing, to_path, validate_locale_and_set_language, BuildOptions, BuilderProgram,
    CharacterCodes, CompilerHost, CompilerOptions, CreateProgram, CreateProgramOptions,
    CreateWatchCompilerHostOfConfigFileInput, CustomTransformers, Diagnostic, DiagnosticReporter,
    Diagnostics, EmitAndSemanticDiagnosticsBuilderProgram, ExitStatus, ExtendedConfigCacheEntry,
    IncrementalCompilationOptions, Node, ParsedBuildCommand, ParsedCommandLine, Program,
    ProgramHost, ReportEmitErrorSummary, ScriptReferenceHost, ScriptTarget,
    SemanticDiagnosticsBuilderProgram, SolutionBuilderHostBase, System, WatchCompilerHost,
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
    system: Rc<dyn System>,
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
                    system.clone(),
                    &mut cb,
                    build_options.clone(),
                    watch_options.as_deref(),
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
                watch_options.as_deref(),
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
            execute_command_line_worker(system.clone(), &mut cb, &mut command_line)
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

impl From<Rc<Program>> for ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine {
    fn from(value: Rc<Program>) -> Self {
        Self::Program(value)
    }
}

impl From<Rc<dyn EmitAndSemanticDiagnosticsBuilderProgram>>
    for ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine
{
    fn from(value: Rc<dyn EmitAndSemanticDiagnosticsBuilderProgram>) -> Self {
        Self::EmitAndSemanticDiagnosticsBuilderProgram(value)
    }
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
    sys: Rc<dyn System>,
    cb: &mut TCallback,
    build_options: Rc<BuildOptions>,
    watch_options: Option<&WatchOptions>,
    projects: &[String],
    mut errors: Vec<Rc<Diagnostic>>,
) {
    let report_diagnostic = update_report_diagnostic(
        sys.clone(),
        create_diagnostic_reporter(sys.clone(), None),
        build_options.clone().into(),
    );

    if let Some(build_options_locale) = build_options.locale.as_ref() {
        if !build_options_locale.is_empty() {
            validate_locale_and_set_language(build_options_locale, &*sys, Some(&mut errors));
        }
    }

    if !errors.is_empty() {
        errors
            .into_iter()
            .for_each(|error| report_diagnostic.call(error));
        sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
    }

    if matches!(build_options.help, Some(true)) {
        print_version(&*sys);
        build_opts.with(|build_opts_| {
            print_build_help(&*sys, &build_opts_);
        });
        sys.exit(Some(ExitStatus::Success));
    }

    if projects.is_empty() {
        print_version(&*sys);
        build_opts.with(|build_opts_| {
            print_build_help(&*sys, &build_opts_);
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
        if report_watch_mode_without_sys_support(&*sys, &*report_diagnostic) {
            return;
        }
        let mut build_host = create_solution_builder_with_watch_host(
            Some(&*sys),
            Option::<CreateProgramDummy>::None,
            Some(report_diagnostic.clone()),
            Some(create_builder_status_reporter(
                &*sys,
                Some(should_be_pretty(&*sys, build_options.clone().into())),
            )),
            Some(create_watch_status_reporter(
                sys.clone(),
                build_options.clone().into(),
            )),
        );
        update_solution_builder_host(&*sys, cb, &mut build_host);
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
        Some(&*sys),
        Option::<CreateProgramDummy>::None,
        Some(report_diagnostic.clone()),
        Some(create_builder_status_reporter(
            &*sys,
            Some(should_be_pretty(&*sys, build_options.clone().into())),
        )),
        create_report_error_summary(sys.clone(), build_options.clone().into()),
    );
    update_solution_builder_host(&*sys, cb, &mut build_host);
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

impl BuilderProgram for BuilderProgramDummy {
    fn get_program(&self) -> Rc<Program> {
        unimplemented!()
    }

    fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        unimplemented!()
    }

    fn get_source_files(&self) -> &[Rc<Node /*SourceFile*/>] {
        unimplemented!()
    }
}

impl SemanticDiagnosticsBuilderProgram for BuilderProgramDummy {}

impl EmitAndSemanticDiagnosticsBuilderProgram for BuilderProgramDummy {}

struct CreateProgramDummy {}

impl CreateProgram<BuilderProgramDummy> for CreateProgramDummy {}

pub(super) fn create_report_error_summary(
    sys: Rc<dyn System>,
    options: CompilerOptionsOrBuildOptions,
) -> Option<Rc<dyn ReportEmitErrorSummary>> {
    if should_be_pretty(&*sys, options) {
        Some(Rc::new(ReportEmitErrorSummaryConcrete::new(sys)))
    } else {
        None
    }
}

pub(super) struct ReportEmitErrorSummaryConcrete {
    sys: Rc<dyn System>,
}

impl ReportEmitErrorSummaryConcrete {
    pub fn new(sys: Rc<dyn System>) -> Self {
        Self { sys }
    }
}

impl ReportEmitErrorSummary for ReportEmitErrorSummaryConcrete {
    fn call(&self, error_count: usize) {
        self.sys
            .write(&get_error_summary_text(error_count, self.sys.new_line()));
    }
}

pub(super) fn perform_compilation<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    sys: Rc<dyn System>,
    mut cb: TCallback,
    report_diagnostic: Rc<dyn DiagnosticReporter>,
    config: &ParsedCommandLine,
) {
    let file_names = &config.file_names;
    let options = config.options.clone();
    let project_references = &config.project_references;
    let host = create_compiler_host_worker(&options, None, Some(sys.clone()));
    let current_directory = host.get_current_directory();
    let get_canonical_file_name =
        create_get_canonical_file_name(host.use_case_sensitive_file_names());
    change_compiler_host_like_to_use_cache(
        &host,
        |file_name| to_path(file_name, Some(&current_directory), get_canonical_file_name),
        Option::<
            fn(&str, ScriptTarget, Option<&mut dyn FnMut(&str)>, Option<bool>) -> Option<Rc<Node>>,
        >::None,
    );

    enable_statistics_and_tracing(&*sys, &options, false);

    let program_options = CreateProgramOptions {
        root_names: config.file_names.clone(),
        options: options.clone(),
        project_references: project_references.clone(),
        host: Some(Rc::new(host)),
        config_file_parsing_diagnostics: Some(get_config_file_parsing_diagnostics(config)),
        old_program: None,
    };
    let program = create_program(program_options);
    let exit_status = emit_files_and_report_errors_and_get_exit_status(
        program.clone(),
        report_diagnostic,
        Some(|s: &str| sys.write(&format!("{}{}", s, sys.new_line()))),
        create_report_error_summary(sys.clone(), options.into()),
        None,
        None,
        None,
        None,
    );
    report_statistics(&*sys, &program);
    cb(program.into());
    sys.exit(Some(exit_status));
}

pub(super) fn perform_incremental_compilation<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    sys: Rc<dyn System>,
    mut cb: TCallback,
    report_diagnostic: Rc<dyn DiagnosticReporter>,
    config: &ParsedCommandLine,
) {
    let options = config.options.clone();
    let file_names = &config.file_names;
    let project_references = &config.project_references;
    enable_statistics_and_tracing(&*sys, &options, false);
    let host = Rc::new(create_incremental_compiler_host(
        &options,
        Some(sys.clone()),
    ));
    let exit_status = perform_incremental_compilation_(IncrementalCompilationOptions {
        host: Some(host),
        system: Some(&*sys),
        root_names: file_names,
        options: &options.clone(),
        config_file_parsing_diagnostics: Some(&get_config_file_parsing_diagnostics(config)),
        project_references: project_references.as_deref(),
        report_diagnostic: Some(report_diagnostic),
        report_error_summary: create_report_error_summary(sys.clone(), options.into()),
        after_program_emit_and_diagnostics: Some(&|builder_program: Rc<
            dyn EmitAndSemanticDiagnosticsBuilderProgram,
        >| {
            report_statistics(&*sys, &builder_program.get_program());
            cb(builder_program.into());
        }),
    });
    sys.exit(Some(exit_status));
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
    update_create_program(sys, build_host.as_program_host());
    // TODO: how to model this?
    // buildHost.afterProgramEmitAndDiagnostics = program => {
    //     reportStatistics(sys, program.getProgram());
    //     cb(program);
    // }
    // buildHost.afterEmitBundle = cb;
}

pub(super) fn update_create_program<TBuilderProgram: BuilderProgram>(
    sys: &dyn System,
    host: &dyn ProgramHost<TBuilderProgram>,
) {
    // TODO: how to model this?
    // const compileUsingBuilder = host.createProgram;
    // host.createProgram = (rootNames, options, host, oldProgram, configFileParsingDiagnostics, projectReferences) => {
    //     Debug.assert(rootNames !== undefined || (options === undefined && !!oldProgram));
    //     if (options !== undefined) {
    //         enableStatisticsAndTracing(sys, options, /*isBuildMode*/ true);
    //     }
    //     return compileUsingBuilder(rootNames, options, host, oldProgram, configFileParsingDiagnostics, projectReferences);
    // };
}

pub(super) fn update_watch_compilation_host<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
    TEmitAndSemanticDiagnosticsBuilderProgram: EmitAndSemanticDiagnosticsBuilderProgram,
    TWatchCompilerHost: WatchCompilerHost<TEmitAndSemanticDiagnosticsBuilderProgram>,
>(
    sys: &dyn System,
    mut cb: TCallback,
    watch_compiler_host: &mut TWatchCompilerHost,
) {
    update_create_program(sys, watch_compiler_host.as_program_host());
    // TODO: how to model this?
    // const emitFilesUsingBuilder = watchCompilerHost.afterProgramCreate!;
    // watchCompilerHost.afterProgramCreate = builderProgram => {
    //     emitFilesUsingBuilder(builderProgram);
    //     reportStatistics(sys, builderProgram.getProgram());
    //     cb(builderProgram);
    // }
}

pub(super) fn create_watch_status_reporter(
    sys: Rc<dyn System>,
    options: CompilerOptionsOrBuildOptions,
) -> Rc<dyn WatchStatusReporter> {
    Rc::new(create_watch_status_reporter_(
        sys.clone(),
        Some(should_be_pretty(&*sys, options)),
    ))
}

pub(super) fn create_watch_of_config_file<
    TCallback: FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
>(
    system: Rc<dyn System>,
    mut cb: TCallback,
    report_diagnostic: Rc<dyn DiagnosticReporter>,
    config_parse_result: Rc<ParsedCommandLine>,
    options_to_extend: Rc<CompilerOptions>,
    watch_options_to_extend: Option<Rc<WatchOptions>>,
    extended_config_cache: HashMap<String, ExtendedConfigCacheEntry>,
) {
    let mut watch_compiler_host =
        create_watch_compiler_host_of_config_file(CreateWatchCompilerHostOfConfigFileInput {
            config_file_name: config_parse_result
                .options
                .config_file_path
                .as_ref()
                .unwrap(),
            options_to_extend: Some(&options_to_extend),
            watch_options_to_extend,
            system: &*system,
            report_diagnostic: Some(&*report_diagnostic),
            report_watch_status: Some(create_watch_status_reporter(
                system.clone(),
                config_parse_result.options.clone().into(),
            )),
            create_program: Option::<&dyn CreateProgram<BuilderProgramDummy>>::None,
            extra_file_extensions: None,
        });
    update_watch_compilation_host(&*system, cb, &mut watch_compiler_host);
    // TODO: how to model this?
    // watchCompilerHost.configFileParsingResult = configParseResult;
    // watchCompilerHost.extendedConfigCache = extendedConfigCache;
    create_watch_program(&watch_compiler_host);
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

pub(super) fn can_report_diagnostics(
    system: &dyn System,
    compiler_options: &CompilerOptions,
) -> bool {
    ptr::eq(system, &*get_sys())
        && (matches!(compiler_options.diagnostics, Some(true))
            || matches!(compiler_options.extended_diagnostics, Some(true)))
}

pub(super) fn can_trace(system: &dyn System, compiler_options: &CompilerOptions) -> bool {
    ptr::eq(system, &*get_sys())
        && matches!(compiler_options.generate_trace.as_ref(), Some(generate_trace) if !generate_trace.is_empty())
}

pub(super) fn enable_statistics_and_tracing(
    system: &dyn System,
    compiler_options: &CompilerOptions,
    is_build_mode: bool,
) {
    if can_report_diagnostics(system, compiler_options) {
        // performance.enable(system);
    }

    if can_trace(system, compiler_options) {
        start_tracing(
            if is_build_mode { "build" } else { "project" },
            compiler_options.generate_trace.as_ref().unwrap(),
            compiler_options.config_file_path.as_deref(),
        );
    }
}

pub(super) fn report_statistics(sys: &dyn System, program: &Program) {
    let compiler_options = program.get_compiler_options();

    if can_trace(sys, &compiler_options) {
        // tracing?.stopTracing();
    }

    let mut statistics: Vec<Statistic> = vec![];
    if can_report_diagnostics(sys, &compiler_options) {
        unimplemented!()
    }
}

pub(super) fn write_config_file(
    sys: &dyn System,
    report_diagnostic: &dyn DiagnosticReporter,
    options: &CompilerOptions,
    file_names: &[String],
) {
    unimplemented!()
}
