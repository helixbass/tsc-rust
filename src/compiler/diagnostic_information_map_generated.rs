use crate::{DiagnosticCategory, DiagnosticMessage};

const fn diag(
    code: u32,
    category: DiagnosticCategory,
    key: &'static str,
    message: &'static str,
) -> DiagnosticMessage {
    DiagnosticMessage {
        code,
        category,
        key,
        message,
    }
}

#[non_exhaustive]
pub struct Diagnostics;
#[allow(non_upper_case_globals)]
impl Diagnostics {
    pub const _0_expected: DiagnosticMessage = diag(
        1005,
        DiagnosticCategory::Error,
        "_0_expected_1005",
        "'{0}' expected.",
    );
}
