use std::rc::Rc;

use crate::{Diagnostic, ExitStatus, Program};

struct EmitFilesAndReportErrorsReturn {
    diagnostics: Vec<Rc<Diagnostic>>,
}

fn emit_files_and_report_errors<TProgram: Program>(
    mut program: TProgram,
) -> EmitFilesAndReportErrorsReturn {
    let diagnostics = program.get_semantic_diagnostics();

    EmitFilesAndReportErrorsReturn { diagnostics }
}

pub fn emit_files_and_report_errors_and_get_exit_status<TProgram: Program>(
    program: TProgram,
) -> ExitStatus {
    let EmitFilesAndReportErrorsReturn { diagnostics } = emit_files_and_report_errors(program);
    if !diagnostics.is_empty() {
        return ExitStatus::DiagnosticsPresent_OutputsGenerated;
    }
    ExitStatus::Success
}
