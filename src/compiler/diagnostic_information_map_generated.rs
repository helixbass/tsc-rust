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
    pub const Identifier_expected: DiagnosticMessage = diag(
        1003,
        DiagnosticCategory::Error,
        "Identifier_expected_1003",
        "Identifier expected.",
    );
    pub const _0_expected: DiagnosticMessage = diag(
        1005,
        DiagnosticCategory::Error,
        "_0_expected_1005",
        "'{0}' expected.",
    );
    pub const Expression_expected: DiagnosticMessage = diag(
        1109,
        DiagnosticCategory::Error,
        "Expression_expected_1109",
        "Expression expected.",
    );
}
