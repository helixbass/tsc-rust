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
    pub const Type_expected: DiagnosticMessage = diag(
        1110,
        DiagnosticCategory::Error,
        "Type_expected_1110",
        "Type expected.",
    );
    pub const Cannot_find_name_0: DiagnosticMessage = diag(
        2304,
        DiagnosticCategory::Error,
        "Cannot_find_name_0_2304",
        "Cannot find name '{0}'.",
    );
    pub const Cannot_find_global_type_0: DiagnosticMessage = diag(
        2318,
        DiagnosticCategory::Error,
        "Cannot_find_global_type_0_2318",
        "Cannot find global type '{0}'.",
    );
    pub const Type_0_is_not_assignable_to_type_1: DiagnosticMessage = diag(
        2322,
        DiagnosticCategory::Error,
        "Type_0_is_not_assignable_to_type_1_2322",
        "Type '{0}' is not assignable to type '{1}'.",
    );
    pub const Types_of_property_0_are_incompatible: DiagnosticMessage = diag(
        2326,
        DiagnosticCategory::Error,
        "Types_of_property_0_are_incompatible_2326",
        "Types of property '{0}' are incompatible.",
    );
    pub const Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1:
        DiagnosticMessage = diag(
        2353,
        DiagnosticCategory::Error,
        "Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1_2353",
        "Object literal may only specify known properties, and '{0}' does not exist in type '{1}'.",
    );
    pub const An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type:
        DiagnosticMessage = diag(
        2356,
        DiagnosticCategory::Error,
        "An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type_2356",
        "An arithmetic operand must be of type 'any', 'number', 'bigint' or an enum type.",
    );
    pub const Private_identifiers_are_not_allowed_outside_class_bodies: DiagnosticMessage = diag(
        18016,
        DiagnosticCategory::Error,
        "Private_identifiers_are_not_allowed_outside_class_bodies_18016",
        "Private identifiers are not allowed outside class bodies.",
    );
}
