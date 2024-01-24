use std::{collections::HashMap, io, ptr, rc::Rc};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

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
    IncrementalCompilationOptions, Node, ParsedBuildCommand, ParsedCommandLine, Path, Program,
    ProgramHost, ReportEmitErrorSummary, SemanticDiagnosticsBuilderProgram,
    SolutionBuilderHostBase, System, ToPath, WatchCompilerHost, WatchOptions, WatchStatusReporter,
    HasArena, InArena, AllArenas,
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

pub fn execute_command_line(
    system: Id<Box<dyn System>>,
    mut cb: impl FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
    command_line_args: &[String],
    arena: &impl HasArena,
) -> io::Result<()> {
    if is_build(command_line_args) {
        let ParsedBuildCommand {
            build_options,
            watch_options,
            projects,
            errors,
        } = parse_build_command(&command_line_args[1..], arena);
        let build_options = Rc::new(build_options);
        if let Some(build_options_generate_cpu_profile) =
            build_options.generate_cpu_profile.as_ref()
        {
            system.ref_(arena).enable_cpu_profiler(build_options_generate_cpu_profile, &mut || {
                perform_build(
                    system.clone(),
                    &mut cb,
                    build_options.clone(),
                    watch_options.as_deref(),
                    &projects,
                    errors.clone(),
                    arena,
                )
            })?;
            return Ok(()); // TODO: the Typescript version doesn't actually return here but seems like it should?
        } else {
            perform_build(
                system,
                &mut cb,
                build_options.clone(),
                watch_options.as_deref(),
                &projects,
                errors,
                arena,
            )?;
            return Ok(());
        }
    }

    let mut command_line =
        parse_command_line(command_line_args, Some(|path: &str| system.ref_(arena).read_file(path)), arena);
    if let Some(command_line_options_generate_cpu_profile) =
        command_line.options.generate_cpu_profile.clone()
    {
        system.ref_(arena).enable_cpu_profiler(&command_line_options_generate_cpu_profile, &mut || {
            execute_command_line_worker(system.clone(), &mut cb, &mut command_line, arena)
        })
    } else {
        execute_command_line_worker(system, &mut cb, &mut command_line, arena)
    }
}

pub enum ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine {
    Program(Gc<Box<Program>>),
    EmitAndSemanticDiagnosticsBuilderProgram(Rc<dyn EmitAndSemanticDiagnosticsBuilderProgram>),
    ParsedCommandLine(Rc<ParsedCommandLine>),
}

impl From<Gc<Box<Program>>>
    for ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine
{
    fn from(value: Gc<Box<Program>>) -> Self {
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
) -> io::Result<bool> {
    if !sys.is_watch_file_supported() || !sys.is_watch_directory_supported() {
        report_diagnostic.call(Gc::new(
            create_compiler_diagnostic(
                &Diagnostics::The_current_host_does_not_support_the_0_option,
                Some(vec!["--watch".to_owned()]),
            )
            .into(),
        ))?;
        sys.exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
    }
    Ok(false)
}

pub(super) fn perform_build(
    sys: Id<Box<dyn System>>,
    cb: &mut impl FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
    build_options: Rc<BuildOptions>,
    watch_options: Option<&WatchOptions>,
    projects: &[String],
    mut errors: Vec<Gc<Diagnostic>>,
    arena: &impl HasArena,
) -> io::Result<()> {
    let report_diagnostic = update_report_diagnostic(
        sys.clone(),
        create_diagnostic_reporter(sys.clone(), None, arena),
        build_options.clone().into(),
        arena,
    );

    if let Some(build_options_locale) = build_options.locale.as_ref() {
        if !build_options_locale.is_empty() {
            validate_locale_and_set_language(build_options_locale, &**sys.ref_(arena), Some(&mut errors));
        }
    }

    if !errors.is_empty() {
        for error in errors {
            report_diagnostic.call(error)?;
        }
        sys.ref_(arena).exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
    }

    if matches!(build_options.help, Some(true)) {
        print_version(&**sys.ref_(arena));
        build_opts.with(|build_opts_| {
            print_build_help(&**sys.ref_(arena), &build_opts_);
        });
        sys.ref_(arena).exit(Some(ExitStatus::Success));
    }

    if projects.is_empty() {
        print_version(&**sys.ref_(arena));
        build_opts.with(|build_opts_| {
            print_build_help(&**sys.ref_(arena), &build_opts_);
        });
        sys.ref_(arena).exit(Some(ExitStatus::Success));
    }

    if !sys.ref_(arena).is_get_modified_time_supported()
        || !sys.ref_(arena).is_set_modified_time_supported()
        || matches!(build_options.clean, Some(true)) && !sys.ref_(arena).is_delete_file_supported()
    {
        report_diagnostic.call(Gc::new(
            create_compiler_diagnostic(
                &Diagnostics::The_current_host_does_not_support_the_0_option,
                Some(vec!["--build".to_owned()]),
            )
            .into(),
        ))?;
        sys.ref_(arena).exit(Some(ExitStatus::DiagnosticsPresent_OutputsSkipped));
    }

    if matches!(build_options.watch, Some(true)) {
        if report_watch_mode_without_sys_support(&**sys.ref_(arena), &**report_diagnostic)? {
            return Ok(());
        }
        let mut build_host = create_solution_builder_with_watch_host(
            Some(&**sys.ref_(arena)),
            Option::<CreateProgramDummy>::None,
            Some(report_diagnostic.clone()),
            Some(create_builder_status_reporter(
                &**sys.ref_(arena),
                Some(should_be_pretty(&**sys.ref_(arena), build_options.clone().into())),
            )),
            Some(create_watch_status_reporter(
                sys.clone(),
                build_options.clone().into(),
                arena,
            )),
        );
        update_solution_builder_host(&**sys.ref_(arena), cb, &mut build_host);
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
        return Ok(())/*builder*/;
    }

    let mut build_host = create_solution_builder_host(
        Some(&**sys.ref_(arena)),
        Option::<CreateProgramDummy>::None,
        Some(report_diagnostic.clone()),
        Some(create_builder_status_reporter(
            &**sys.ref_(arena),
            Some(should_be_pretty(&**sys.ref_(arena), build_options.clone().into())),
        )),
        create_report_error_summary(sys.clone(), build_options.clone().into(), arena),
    );
    update_solution_builder_host(&**sys.ref_(arena), cb, &mut build_host);
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
    sys.ref_(arena).exit(Some(exit_status));
}

#[derive(Trace, Finalize)]
struct BuilderProgramDummy {}

impl BuilderProgram for BuilderProgramDummy {
    fn get_program(&self) -> Gc<Box<Program>> {
        unimplemented!()
    }

    fn get_compiler_options(&self) -> Gc<CompilerOptions> {
        unimplemented!()
    }

    fn get_source_files(&self) -> &[Id<Node /*SourceFile*/>] {
        unimplemented!()
    }
}

impl SemanticDiagnosticsBuilderProgram for BuilderProgramDummy {}

impl EmitAndSemanticDiagnosticsBuilderProgram for BuilderProgramDummy {}

struct CreateProgramDummy {}

impl CreateProgram<BuilderProgramDummy> for CreateProgramDummy {}

pub(super) fn create_report_error_summary(
    sys: Id<Box<dyn System>>,
    options: CompilerOptionsOrBuildOptions,
    arena: &impl HasArena,
) -> Option<Rc<dyn ReportEmitErrorSummary>> {
    if should_be_pretty(&**sys.ref_(arena), options) {
        Some(Rc::new(ReportEmitErrorSummaryConcrete::new(sys)))
    } else {
        None
    }
}

pub(super) struct ReportEmitErrorSummaryConcrete {
    sys: Id<Box<dyn System>>,
}

impl ReportEmitErrorSummaryConcrete {
    pub fn new(sys: Id<Box<dyn System>>) -> Self {
        Self { sys }
    }
}

impl ReportEmitErrorSummary for ReportEmitErrorSummaryConcrete {
    fn call(&self, error_count: usize) {
        self.sys
            .ref_(self).write(&get_error_summary_text(error_count, self.sys.ref_(self).new_line()));
    }
}

impl HasArena for ReportEmitErrorSummaryConcrete {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub(super) fn perform_compilation(
    sys: Id<Box<dyn System>>,
    mut cb: impl FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
    report_diagnostic: Gc<Box<dyn DiagnosticReporter>>,
    config: &ParsedCommandLine,
    arena: &impl HasArena,
) -> io::Result<()> {
    let file_names = &config.file_names;
    let options = config.options.clone();
    let project_references = &config.project_references;
    let host: Gc<Box<dyn CompilerHost>> = Gc::new(Box::new(create_compiler_host_worker(
        options.clone(),
        None,
        Some(sys.clone()),
        arena,
    )));
    let current_directory = CompilerHost::get_current_directory(&**host)?;
    let get_canonical_file_name =
        create_get_canonical_file_name(CompilerHost::use_case_sensitive_file_names(&**host));
    change_compiler_host_like_to_use_cache(
        host.clone(),
        Gc::new(Box::new(PerformCompilationToPath::new(
            current_directory,
            get_canonical_file_name,
        ))),
        None,
    );

    enable_statistics_and_tracing(sys, &options, false, arena);

    let program_options = CreateProgramOptions {
        root_names: file_names.clone(),
        options: options.clone(),
        project_references: project_references.clone(),
        host: Some(host),
        config_file_parsing_diagnostics: Some(get_config_file_parsing_diagnostics(config, arena)),
        old_program: None,
    };
    let program = create_program(program_options)?;
    let exit_status = emit_files_and_report_errors_and_get_exit_status(
        program.clone(),
        report_diagnostic,
        Some(|s: &str| sys.ref_(arena).write(&format!("{}{}", s, sys.ref_(arena).new_line()))),
        create_report_error_summary(sys.clone(), options.into(), arena),
        None,
        None,
        None,
        None,
        arena,
    )?;
    report_statistics(sys, &program, arena);
    cb(program.into());
    sys.ref_(arena).exit(Some(exit_status));
}

#[derive(Trace, Finalize)]
struct PerformCompilationToPath {
    current_directory: String,
    // TODO: this wasn't compiling but seems like it should've been if fn implementations of Trace
    // (included in `gc`) were working as expected?
    #[unsafe_ignore_trace]
    get_canonical_file_name: fn(&str) -> String,
}

impl PerformCompilationToPath {
    pub fn new(current_directory: String, get_canonical_file_name: fn(&str) -> String) -> Self {
        Self {
            current_directory,
            get_canonical_file_name,
        }
    }
}

impl ToPath for PerformCompilationToPath {
    fn call(&self, file_name: &str) -> Path {
        to_path(file_name, Some(&self.current_directory), |file_name| {
            (self.get_canonical_file_name)(file_name)
        })
    }
}

pub(super) fn perform_incremental_compilation(
    sys: Id<Box<dyn System>>,
    mut cb: impl FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
    report_diagnostic: Gc<Box<dyn DiagnosticReporter>>,
    config: &ParsedCommandLine,
    arena: &impl HasArena,
) {
    let options = config.options.clone();
    let file_names = &config.file_names;
    let project_references = &config.project_references;
    enable_statistics_and_tracing(sys, &options, false, arena);
    let host: Gc<Box<dyn CompilerHost>> = Gc::new(Box::new(create_incremental_compiler_host(
        options.clone(),
        Some(sys.clone()),
        arena,
    )));
    let exit_status = perform_incremental_compilation_(IncrementalCompilationOptions {
        host: Some(host),
        system: Some(&**sys.ref_(arena)),
        root_names: file_names,
        options: &options.clone(),
        config_file_parsing_diagnostics: Some(&get_config_file_parsing_diagnostics(config, arena)),
        project_references: project_references.as_deref(),
        report_diagnostic: Some(report_diagnostic),
        report_error_summary: create_report_error_summary(sys.clone(), options.into(), arena),
        after_program_emit_and_diagnostics: Some(&|builder_program: Rc<
            dyn EmitAndSemanticDiagnosticsBuilderProgram,
        >| {
            report_statistics(sys, &builder_program.get_program(), arena);
            cb(builder_program.into());
        }),
    });
    sys.ref_(arena).exit(Some(exit_status));
}

pub(super) fn update_solution_builder_host<
    TEmitAndSemanticDiagnosticsBuilderProgram: EmitAndSemanticDiagnosticsBuilderProgram,
>(
    sys: &dyn System,
    _cb: impl FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
    build_host: &mut impl SolutionBuilderHostBase<TEmitAndSemanticDiagnosticsBuilderProgram>,
) {
    update_create_program(sys, build_host.as_program_host());
    // TODO: how to model this?
    // buildHost.afterProgramEmitAndDiagnostics = program => {
    //     reportStatistics(sys, program.getProgram());
    //     cb(program);
    // }
    // buildHost.afterEmitBundle = cb;
}

pub(super) fn update_create_program(
    _sys: &dyn System,
    _host: &dyn ProgramHost<impl BuilderProgram>,
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
    TEmitAndSemanticDiagnosticsBuilderProgram: EmitAndSemanticDiagnosticsBuilderProgram,
>(
    sys: &dyn System,
    _cb: impl FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
    watch_compiler_host: &mut impl WatchCompilerHost<TEmitAndSemanticDiagnosticsBuilderProgram>,
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
    sys: Id<Box<dyn System>>,
    options: CompilerOptionsOrBuildOptions,
    arena: &impl HasArena,
) -> Rc<dyn WatchStatusReporter> {
    Rc::new(create_watch_status_reporter_(
        sys.clone(),
        Some(should_be_pretty(&**sys.ref_(arena), options)),
    ))
}

pub(super) fn create_watch_of_config_file(
    system: Id<Box<dyn System>>,
    cb: impl FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
    report_diagnostic: Gc<Box<dyn DiagnosticReporter>>,
    config_parse_result: Rc<ParsedCommandLine>,
    options_to_extend: Gc<CompilerOptions>,
    watch_options_to_extend: Option<Rc<WatchOptions>>,
    _extended_config_cache: HashMap<String, ExtendedConfigCacheEntry>,
    arena: &impl HasArena,
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
            system: &**system.ref_(arena),
            report_diagnostic: Some(&**report_diagnostic),
            report_watch_status: Some(create_watch_status_reporter(
                system.clone(),
                config_parse_result.options.clone().into(),
                arena,
            )),
            create_program: Option::<&dyn CreateProgram<BuilderProgramDummy>>::None,
            extra_file_extensions: None,
        });
    update_watch_compilation_host(&**system.ref_(arena), cb, &mut watch_compiler_host);
    // TODO: how to model this?
    // watchCompilerHost.configFileParsingResult = configParseResult;
    // watchCompilerHost.extendedConfigCache = extendedConfigCache;
    create_watch_program(&watch_compiler_host);
}

pub(super) fn create_watch_of_files_and_compiler_options(
    _system: &dyn System,
    _cb: impl FnMut(ProgramOrEmitAndSemanticDiagnosticsBuilderProgramOrParsedCommandLine),
    _report_diagnostic: Gc<Box<dyn DiagnosticReporter>>,
    _root_files: &[String],
    _options: Gc<CompilerOptions>,
    _watch_options: Option<Rc<WatchOptions>>,
) {
    unimplemented!()
}

pub(super) fn can_report_diagnostics(
    system: Id<Box<dyn System>>,
    compiler_options: &CompilerOptions,
    arena: &impl HasArena,
) -> bool {
    system == get_sys(arena)
        && (matches!(compiler_options.diagnostics, Some(true))
            || matches!(compiler_options.extended_diagnostics, Some(true)))
}

pub(super) fn can_trace(system: Id<Box<dyn System>>, compiler_options: &CompilerOptions, arena: &impl HasArena) -> bool {
    system == get_sys(arena)
        && matches!(
            compiler_options.generate_trace.as_ref(),
            Some(generate_trace) if !generate_trace.is_empty()
        )
}

pub(super) fn enable_statistics_and_tracing(
    system: Id<Box<dyn System>>,
    compiler_options: &CompilerOptions,
    is_build_mode: bool,
    arena: &impl HasArena,
) {
    if can_report_diagnostics(system, compiler_options, arena) {
        // performance.enable(system);
    }

    if can_trace(system, compiler_options, arena) {
        start_tracing(
            if is_build_mode { "build" } else { "project" },
            compiler_options.generate_trace.as_ref().unwrap(),
            compiler_options.config_file_path.as_deref(),
        );
    }
}

pub(super) fn report_statistics(sys: &dyn System, program: &Program, arena: &impl HasArena) {
    let compiler_options = program.get_compiler_options();

    if can_trace(sys, &compiler_options, arena) {
        // tracing?.stopTracing();
    }

    let /*mut*/ _statistics: Vec<Statistic> = vec![];
    if can_report_diagnostics(sys, &compiler_options, arena) {
        unimplemented!()
    }
}

pub(super) fn write_config_file(
    _sys: &dyn System,
    _report_diagnostic: &dyn DiagnosticReporter,
    _options: &CompilerOptions,
    _file_names: &[String],
) {
    unimplemented!()
}
