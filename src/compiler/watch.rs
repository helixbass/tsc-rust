use std::rc::Rc;

use crate::{
    add_range, sort_and_deduplicate_diagnostics, Diagnostic, ExitStatus, Program, SortedArray,
};

struct EmitFilesAndReportErrorsReturn {
    diagnostics: SortedArray<Rc<Diagnostic>>,
}

fn emit_files_and_report_errors<TProgram: Program>(
    mut program: TProgram,
) -> EmitFilesAndReportErrorsReturn {
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

pub fn emit_files_and_report_errors_and_get_exit_status<TProgram: Program>(
    program: TProgram,
) -> ExitStatus {
    let EmitFilesAndReportErrorsReturn { diagnostics } = emit_files_and_report_errors(program);
    println!("diagnostics: {:#?}", diagnostics);
    if !diagnostics.is_empty() {
        return ExitStatus::DiagnosticsPresent_OutputsGenerated;
    }
    ExitStatus::Success
}
