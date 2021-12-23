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
    pub const Type_0_is_not_assignable_to_type_1: DiagnosticMessage = diag(
        2322,
        DiagnosticCategory::Error,
        "Type_0_is_not_assignable_to_type_1_2322",
        "Type '{0}' is not assignable to type '{1}'.",
    );
    pub const An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type:
        DiagnosticMessage = diag(
        2356,
        DiagnosticCategory::Error,
        "An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type_2356",
        "An arithmetic operand must be of type 'any', 'number', 'bigint' or an enum type.",
    );
}
