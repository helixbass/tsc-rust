use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::rc::Rc;

use crate::{
    add_range, contains, create_get_canonical_file_name, flatten_diagnostic_message_text,
    format_color_and_reset, format_diagnostic, format_diagnostics_with_color_and_context, get_sys,
    sort_and_deduplicate_diagnostics, BuilderProgram, CancellationToken, CompilerHost,
    CompilerOptions, ConfigFileDiagnosticsReporter, CreateProgram, CustomTransformers, Diagnostic,
    DiagnosticRelatedInformationInterface, DiagnosticReporter, Diagnostics,
    EmitAndSemanticDiagnosticsBuilderProgram, ExitStatus, ExtendedConfigCacheEntry,
    FileExtensionInfo, ForegroundColorEscapeSequences, FormatDiagnosticsHost, ParsedCommandLine,
    Program, ProgramHost, ProjectReference, ReportEmitErrorSummary, SortedArray, System,
    WatchCompilerHost, WatchCompilerHostOfConfigFile, WatchHost, WatchOptions, WatchStatusReporter,
    WriteFileCallback,
};

thread_local! {
    static sys_format_diagnostics_host: Option<Rc<SysFormatDiagnosticsHost>> = /*sys ?*/ Some(Rc::new(SysFormatDiagnosticsHost::new(get_sys())));
}

struct SysFormatDiagnosticsHost {
    system: Rc<dyn System>,
    get_canonical_file_name: fn(&str) -> String,
}

impl SysFormatDiagnosticsHost {
    pub fn new(system: Rc<dyn System>) -> Self {
        let system_use_case_sensitive_file_names = system.use_case_sensitive_file_names();
        Self {
            system,
            get_canonical_file_name: create_get_canonical_file_name(
                system_use_case_sensitive_file_names,
            ),
        }
    }
}

impl FormatDiagnosticsHost for SysFormatDiagnosticsHost {
    fn get_current_directory(&self) -> String {
        self.system.get_current_directory()
    }

    fn get_new_line(&self) -> &str {
        self.system.new_line()
    }

    fn get_canonical_file_name(&self, file_name: &str) -> String {
        (self.get_canonical_file_name)(file_name)
    }
}

pub fn create_diagnostic_reporter(
    system: Rc<dyn System>,
    pretty: Option<bool>,
) -> Rc<dyn DiagnosticReporter> {
    let host: Rc<SysFormatDiagnosticsHost> =
        sys_format_diagnostics_host.with(|sys_format_diagnostics_host_| {
            if Rc::ptr_eq(&system, &get_sys())
            /*&& sysFormatDiagnosticsHost*/
            {
                sys_format_diagnostics_host_.clone().unwrap()
            } else {
                Rc::new(SysFormatDiagnosticsHost::new(system.clone()))
            }
        });
    Rc::new(DiagnosticReporterConcrete::new(host, pretty, system))
}

struct DiagnosticReporterConcrete {
    host: Rc<SysFormatDiagnosticsHost>,
    pretty: bool,
    diagnostics: RefCell<Vec<Rc<Diagnostic>>>,
    system: Rc<dyn System>,
}

impl DiagnosticReporterConcrete {
    pub fn new(
        host: Rc<SysFormatDiagnosticsHost>,
        pretty: Option<bool>,
        system: Rc<dyn System>,
    ) -> Self {
        Self {
            host,
            pretty: pretty.unwrap_or(false),
            diagnostics: RefCell::new(vec![]),
            system,
        }
    }
}

impl DiagnosticReporter for DiagnosticReporterConcrete {
    fn call(&self, diagnostic: Rc<Diagnostic>) {
        if !self.pretty {
            self.system
                .write(&format_diagnostic(&diagnostic, &*self.host));
            return;
        }

        let mut diagnostics = self.diagnostics.borrow_mut();
        diagnostics[0] = diagnostic;
        self.system.write(&format!(
            "{}{}",
            format_diagnostics_with_color_and_context(&diagnostics, &*self.host),
            self.host.get_new_line()
        ));
        diagnostics.pop();
    }
}

fn clear_screen_if_not_watching_for_file_changes(
    system: &dyn System,
    diagnostic: &Diagnostic,
    options: &CompilerOptions,
) -> bool {
    if system.is_clear_screen_implemented()
        && !matches!(options.preserve_watch_output, Some(true))
        && !matches!(options.extended_diagnostics, Some(true))
        && !matches!(options.diagnostics, Some(true))
        && contains(Some(&**screen_starting_message_codes), &diagnostic.code())
    {
        system.clear_screen();
        return true;
    }

    false
}

lazy_static! {
    pub static ref screen_starting_message_codes: Vec<u32> = vec![
        Diagnostics::Starting_compilation_in_watch_mode.code,
        Diagnostics::File_change_detected_Starting_incremental_compilation.code
    ];
}

fn get_plain_diagnostic_following_new_lines(diagnostic: &Diagnostic, new_line: &str) -> String {
    if contains(Some(&**screen_starting_message_codes), &diagnostic.code()) {
        format!("{}{}", new_line, new_line)
    } else {
        new_line.to_owned()
    }
}

pub fn get_locale_time_string(system: &dyn System) -> String {
    // TODO: how to mimic .toLocaleTimeString()?
    unimplemented!()
}

pub fn create_watch_status_reporter(
    system: Rc<dyn System>,
    pretty: Option<bool>,
) -> WatchStatusReporterConcrete {
    WatchStatusReporterConcrete::new(system, pretty)
}

pub struct WatchStatusReporterConcrete {
    system: Rc<dyn System>,
    pretty: bool,
}

impl WatchStatusReporterConcrete {
    pub fn new(system: Rc<dyn System>, pretty: Option<bool>) -> Self {
        Self {
            system,
            pretty: pretty.unwrap_or(false),
        }
    }
}

impl WatchStatusReporter for WatchStatusReporterConcrete {
    fn call(
        &self,
        diagnostic: Rc<Diagnostic>,
        new_line: &str,
        options: Rc<CompilerOptions>,
        _error_count: Option<usize>,
    ) {
        if self.pretty {
            clear_screen_if_not_watching_for_file_changes(&*self.system, &diagnostic, &options);
            let mut output = format!(
                "[{}] ",
                format_color_and_reset(
                    &get_locale_time_string(&*self.system),
                    ForegroundColorEscapeSequences::Grey
                )
            );
            output.push_str(&format!(
                "{}{}{}",
                flatten_diagnostic_message_text(
                    Some(diagnostic.message_text()),
                    self.system.new_line(),
                    None
                ),
                new_line,
                new_line
            ));
            self.system.write(&output);
        } else {
            let mut output = "".to_owned();

            if !clear_screen_if_not_watching_for_file_changes(&*self.system, &diagnostic, &options)
            {
                output.push_str(new_line);
            }

            output.push_str(&format!("{} - ", get_locale_time_string(&*self.system)));
            output.push_str(&format!(
                "{}{}",
                flatten_diagnostic_message_text(
                    Some(diagnostic.message_text()),
                    self.system.new_line(),
                    None
                ),
                get_plain_diagnostic_following_new_lines(&diagnostic, new_line)
            ));

            self.system.write(&output);
        }
    }
}

pub fn parse_config_file_with_system(
    config_file_name: &str,
    options_to_extend: &CompilerOptions,
    extended_config_cache: Option<&HashMap<String, ExtendedConfigCacheEntry>>,
    watch_options_to_extend: Option<Rc<WatchOptions>>,
    system: &dyn System,
    report_diagnostic: &dyn DiagnosticReporter,
) -> Option<ParsedCommandLine> {
    unimplemented!()
}

pub fn get_error_summary_text(error_count: usize, new_line: &str) -> String {
    unimplemented!()
}

struct EmitFilesAndReportErrorsReturn {
    diagnostics: SortedArray<Rc<Diagnostic>>,
}

fn emit_files_and_report_errors(program: Rc<Program>) -> EmitFilesAndReportErrorsReturn {
    let mut all_diagnostics: Vec<Rc<Diagnostic>> = vec![];
    let config_file_parsing_diagnostics_length = all_diagnostics.len();
    add_range(
        &mut all_diagnostics,
        Some(&program.get_syntactic_diagnostics()),
        None,
        None,
    );

    if all_diagnostics.len() == config_file_parsing_diagnostics_length {
        if true {
            add_range(
                &mut all_diagnostics,
                Some(&program.get_semantic_diagnostics()),
                None,
                None,
            );
        }
    }

    let diagnostics = sort_and_deduplicate_diagnostics(&all_diagnostics);

    EmitFilesAndReportErrorsReturn { diagnostics }
}

pub fn emit_files_and_report_errors_and_get_exit_status<TWrite: FnMut(&str)>(
    program: Rc<Program>,
    report_diagnostic: Rc<dyn DiagnosticReporter>,
    write: Option<TWrite>,
    report_summary: Option<Rc<dyn ReportEmitErrorSummary>>,
    write_file: Option<&dyn WriteFileCallback>,
    cancellation_token: Option<Rc<dyn CancellationToken>>,
    enit_only_dts_files: Option<bool>,
    custom_transformers: Option<CustomTransformers>,
) -> ExitStatus {
    let EmitFilesAndReportErrorsReturn { diagnostics } = emit_files_and_report_errors(program);
    println!("diagnostics: {:#?}", diagnostics);
    if !diagnostics.is_empty() {
        return ExitStatus::DiagnosticsPresent_OutputsGenerated;
    }
    ExitStatus::Success
}

pub struct CreateWatchCompilerHostOfConfigFileInput<
    'a,
    TBuilderProgram: BuilderProgram,
    // TCreateProgram: CreateProgram<TBuilderProgram>,
> {
    pub system: &'a dyn System,
    pub create_program: Option<&'a dyn CreateProgram<TBuilderProgram>>,
    pub report_diagnostic: Option<&'a dyn DiagnosticReporter>,
    pub report_watch_status: Option<Rc<dyn WatchStatusReporter>>,
    pub config_file_name: &'a str,
    pub options_to_extend: Option<&'a CompilerOptions>,
    pub watch_options_to_extend: Option<Rc<WatchOptions>>,
    pub extra_file_extensions: Option<&'a [FileExtensionInfo]>,
}

pub fn create_watch_compiler_host_of_config_file<'a, TBuilderProgram: BuilderProgram>(
    _: CreateWatchCompilerHostOfConfigFileInput<'a, TBuilderProgram>,
) -> impl WatchCompilerHostOfConfigFile<TBuilderProgram> {
    WatchCompilerHostOfConfigFileConcrete {
        phantom: PhantomData,
    }
}

pub struct WatchCompilerHostOfConfigFileConcrete<TBuilderProgram: BuilderProgram> {
    phantom: PhantomData<TBuilderProgram>,
}

impl<TBuilderProgram: BuilderProgram> WatchCompilerHostOfConfigFile<TBuilderProgram>
    for WatchCompilerHostOfConfigFileConcrete<TBuilderProgram>
{
}

impl<TBuilderProgram: BuilderProgram> WatchCompilerHost<TBuilderProgram>
    for WatchCompilerHostOfConfigFileConcrete<TBuilderProgram>
{
    fn as_program_host(&self) -> &dyn ProgramHost<TBuilderProgram> {
        self
    }
}

impl<TBuilderProgram: BuilderProgram> ProgramHost<TBuilderProgram>
    for WatchCompilerHostOfConfigFileConcrete<TBuilderProgram>
{
}

impl<TBuilderProgram: BuilderProgram> WatchHost
    for WatchCompilerHostOfConfigFileConcrete<TBuilderProgram>
{
}

impl<TBuilderProgram: BuilderProgram> ConfigFileDiagnosticsReporter
    for WatchCompilerHostOfConfigFileConcrete<TBuilderProgram>
{
    fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Rc<Diagnostic>) {
        unimplemented!()
    }
}

pub struct IncrementalCompilationOptions<'a> {
    pub root_names: &'a [String],
    pub options: &'a CompilerOptions,
    pub config_file_parsing_diagnostics: Option<&'a [Rc<Diagnostic>]>,
    pub project_references: Option<&'a [Rc<ProjectReference>]>,
    pub host: Option<Rc<dyn CompilerHost>>,
    pub report_diagnostic: Option<Rc<dyn DiagnosticReporter>>,
    pub report_error_summary: Option<Rc<dyn ReportEmitErrorSummary>>,
    pub after_program_emit_and_diagnostics:
        Option<&'a dyn FnMut(Rc<dyn EmitAndSemanticDiagnosticsBuilderProgram>)>,
    pub system: Option<&'a dyn System>,
}

pub fn perform_incremental_compilation(input: IncrementalCompilationOptions) -> ExitStatus {
    unimplemented!()
}
