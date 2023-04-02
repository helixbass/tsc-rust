use std::{borrow::Borrow, ptr};

use gc::Gc;

use super::{declaration_emit_node_builder_flags, is_processed_component, TransformDeclarations};
use crate::{
    can_produce_diagnostics, create_get_symbol_accessibility_diagnostic_for_node,
    get_comment_range, get_external_module_import_equals_declaration_expression,
    get_external_module_name_from_declaration, get_line_and_character_of_position,
    get_original_node_id, get_parse_tree_node, get_set_accessor_value_parameter,
    get_this_parameter, has_dynamic_name, has_effective_modifier, has_jsdoc_nodes,
    is_binding_pattern, is_class_declaration, is_declaration, is_entity_name,
    is_entity_name_expression, is_external_module, is_external_module_indicator,
    is_function_declaration, is_function_like, is_index_signature_declaration,
    is_interface_declaration, is_late_visibility_painted_statement, is_literal_import_type_node,
    is_mapped_type_node, is_method_declaration, is_method_signature, is_module_declaration,
    is_omitted_expression, is_private_identifier, is_semicolon_class_element,
    is_set_accessor_declaration, is_source_file, is_string_literal_like, is_tuple_type_node,
    is_type_alias_declaration, is_type_node, is_type_query_node, length, map_defined, maybe_map,
    needs_scope_marker, set_comment_range_rc, set_emit_flags, set_original_node, some,
    visit_each_child, visit_node, visit_nodes, with_synthetic_factory, Debug_, EmitFlags,
    FunctionLikeDeclarationInterface, GetSymbolAccessibilityDiagnostic, HasQuestionTokenInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, ModifierFlags,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, NonEmpty, ReadonlyTextRange,
    SignatureDeclarationInterface, SymbolInterface, SyntaxKind, VisitResult,
};

impl TransformDeclarations {
    pub(super) fn visit_declaration_subtree_cleanup(
        &self,
        input: &Node,
        can_produce_diagnostic: bool,
        previous_enclosing_declaration: Option<&Gc<Node>>,
        old_diag: &GetSymbolAccessibilityDiagnostic,
        should_enter_suppress_new_diagnostics_context_context: bool,
        old_within_object_literal_type: Option<bool>,
        return_value: Option<&Node>,
    ) -> VisitResult {
        if return_value.is_some() && can_produce_diagnostic && has_dynamic_name(input) {
            self.check_name(input);
        }
        if self.is_enclosing_declaration(input) {
            self.set_enclosing_declaration(previous_enclosing_declaration.cloned());
        }
        if can_produce_diagnostic && self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(old_diag.clone());
        }
        if should_enter_suppress_new_diagnostics_context_context {
            self.set_suppress_new_diagnostic_contexts(old_within_object_literal_type);
        }
        if matches!(
            return_value,
            Some(return_value) if ptr::eq(return_value, input)
        ) {
            return return_value.map(Node::node_wrapper).map(Into::into);
        }
        return_value
            .map(|return_value| {
                set_original_node(
                    self.preserve_js_doc(return_value, input),
                    Some(input.node_wrapper()),
                )
            })
            .map(Into::into)
    }

    pub(super) fn is_private_method_type_parameter(
        &self,
        node: &Node, /*TypeParameterDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn visit_declaration_statements(&self, input: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn transform_top_level_declaration(
        &self,
        input: &Node, /*LateVisibilityPaintedStatement*/
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }
}
