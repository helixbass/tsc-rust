use gc::{Finalize, Gc, Trace};

use crate::{
    is_binding_element, is_call_signature_declaration, is_construct_signature_declaration,
    is_constructor_declaration, is_expression_with_type_arguments, is_function_declaration,
    is_get_accessor, is_import_equals_declaration, is_index_signature_declaration,
    is_jsdoc_type_alias, is_method_declaration, is_method_signature, is_parameter,
    is_property_access_expression, is_property_declaration, is_property_signature, is_set_accessor,
    is_type_alias_declaration, is_type_parameter_declaration, is_variable_declaration,
    DiagnosticMessage, Node, SymbolAccessibilityResult,
};

pub trait GetSymbolAccessibilityDiagnosticInterface: Trace + Finalize {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>>;
}

pub type GetSymbolAccessibilityDiagnostic = Gc<Box<dyn GetSymbolAccessibilityDiagnosticInterface>>;

#[derive(Trace, Finalize)]
pub struct SymbolAccessibilityDiagnostic {
    pub error_node: Gc<Node>,
    pub diagnostic_message: &'static DiagnosticMessage,
    pub type_name: Option<Gc<Node /*DeclarationName | QualifiedName*/>>,
}

pub fn can_produce_diagnostics(node: &Node) -> bool {
    is_variable_declaration(node)
        || is_property_declaration(node)
        || is_property_signature(node)
        || is_binding_element(node)
        || is_set_accessor(node)
        || is_get_accessor(node)
        || is_construct_signature_declaration(node)
        || is_call_signature_declaration(node)
        || is_method_declaration(node)
        || is_method_signature(node)
        || is_function_declaration(node)
        || is_parameter(node)
        || is_type_parameter_declaration(node)
        || is_expression_with_type_arguments(node)
        || is_import_equals_declaration(node)
        || is_type_alias_declaration(node)
        || is_constructor_declaration(node)
        || is_index_signature_declaration(node)
        || is_property_access_expression(node)
        || is_jsdoc_type_alias(node)
}

pub fn create_get_symbol_accessibility_diagnostic_for_node(
    node: &Node, /*DeclarationDiagnosticProducing*/
) -> GetSymbolAccessibilityDiagnostic {
    unimplemented!()
}
