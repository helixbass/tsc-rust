use std::ptr;

use gc::Gc;

use super::TransformDeclarations;
use crate::{
    has_dynamic_name, set_original_node, GetSymbolAccessibilityDiagnostic, Node, NodeInterface,
    VisitResult,
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
