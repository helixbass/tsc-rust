use crate::{DiagnosticCategory, DiagnosticMessage};

#[non_exhaustive]
pub struct Diagnostics;
impl Diagnostics {
    pub const _0_expected: DiagnosticMessage = DiagnosticMessage {
        code: 1005,
        category: DiagnosticCategory::Error,
        key: "_0_expected_1005".to_string(),
        message: "'{0}' expected.".to_string(),
    };
}
