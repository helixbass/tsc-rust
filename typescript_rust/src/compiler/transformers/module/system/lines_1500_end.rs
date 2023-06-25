use gc::Gc;

use super::TransformSystemModule;
use crate::{get_node_id, GetOrInsertDefault, Node, VisitResult};

impl TransformSystemModule {
    pub(super) fn visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, false)
    }

    pub(super) fn discarded_value_visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, true)
    }

    pub(super) fn visit_expression_statement(
        &self,
        _node: &Node, /*ExpressionStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_parenthesized_expression(
        &self,
        _node: &Node, /*ExpressionStatement*/
        _value_is_discarded: bool,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_partially_emitted_expression(
        &self,
        _node: &Node, /*PartiallyEmittedExpression*/
        _value_is_discarded: bool,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_import_call_expression(
        &self,
        _node: &Node, /*ImportCall*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_destructuring_assignment(
        &self,
        _node: &Node, /*DestructuringAssignment*/
        _value_is_discarded: bool,
    ) -> VisitResult /*<Expression>*/ {
        unimplemented!()
    }

    pub(super) fn visit_prefix_or_postfix_unary_expression(
        &self,
        _node: &Node, /*PrefixUnaryExpression | PostfixUnaryExpression*/
        _value_is_discarded: bool,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn modifier_visitor(
        &self,
        _node: &Node, /*FunctionDeclaration*/
    ) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn prevent_substitution(&self, node: Gc<Node>) -> Gc<Node> {
        self.maybe_no_substitution_mut()
            .get_or_insert_default_()
            .insert(get_node_id(&node), true);
        node
    }
}
