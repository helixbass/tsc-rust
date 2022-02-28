use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    add_range, sort_and_deduplicate_diagnostics, CancellationToken, CompilerHost, CompilerOptions,
    CustomTransformers, Diagnostic, DiagnosticReporter, EmitAndSemanticDiagnosticsBuilderProgram,
    ExitStatus, ExtendedConfigCacheEntry, ParsedCommandLine, Program, ProjectReference,
    ReportEmitErrorSummary, SortedArray, System, WatchOptions, WriteFileCallback,
};

pub fn create_diagnostic_reporter(
    sys: &dyn System,
    pretty: Option<bool>,
) -> Rc<dyn DiagnosticReporter> {
    Rc::new(DiagnosticReporterConcrete::new())
}

struct DiagnosticReporterConcrete {}

impl DiagnosticReporterConcrete {
    pub fn new() -> Self {
        Self {}
    }
}

impl DiagnosticReporter for DiagnosticReporterConcrete {
    fn call(&self, diagnostic: Rc<Diagnostic>) {
        unimplemented!()
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
    write_file: Option<WriteFileCallback>,
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
