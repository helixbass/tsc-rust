use super::TransformClassFields;
use crate::{Node, VisitResult};

impl TransformClassFields {
    pub(super) fn visit_element_access_expression(
        &self,
        _node: &Node, /*ElementAccessExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_pre_or_postfix_unary_expression(
        &self,
        _node: &Node, /*PrefixUnaryExpression | PostfixUnaryExpression*/
        _value_is_discarded: bool,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_for_statement(&self, _node: &Node /*ForStatement*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_expression_statement(
        &self,
        _node: &Node, /*ExpressionStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_call_expression(
        &self,
        _node: &Node, /*CallExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_tagged_template_expression(
        &self,
        _node: &Node, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_binary_expression(
        &self,
        _node: &Node, /*BinaryExpression*/
        _value_is_discarded: bool,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_class_like(
        &self,
        _node: &Node, /*ClassLikeDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_expression_with_type_arguments(
        &self,
        _node: &Node, /*ExpressionWithTypeArguments*/
    ) -> VisitResult {
        unimplemented!()
    }
}
