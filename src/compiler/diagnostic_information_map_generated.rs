use crate::{DiagnosticCategory, DiagnosticMessage};

#[non_exhaustive]
pub struct Diagnostics;
#[allow(non_upper_case_globals)]
impl Diagnostics {
    pub const _0_expected: DiagnosticMessage = DiagnosticMessage {
        code: 1005,
        category: DiagnosticCategory::Error,
        key: "_0_expected_1005",
        message: "'{0}' expected.",
    };
}
