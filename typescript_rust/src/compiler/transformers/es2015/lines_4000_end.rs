use std::borrow::Borrow;

use gc::Gc;

use super::TransformES2015;
use crate::{Node, VisitResult};

impl TransformES2015 {
    pub(super) fn visit_spread_element(&self, _node: &Node /*SpreadElement*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_template_literal(
        &self,
        _node: &Node, /*LiteralExpression*/
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }

    pub(super) fn visit_string_literal(&self, _node: &Node /*StringLiteral*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_numeric_literal(
        &self,
        _node: &Node, /*NumericLiteral*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_tagged_template_expression(
        &self,
        _node: &Node, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_template_expression(
        &self,
        _node: &Node, /*TemplateExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_super_keyword(
        &self,
        _is_expression_of_call: bool,
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }

    pub(super) fn visit_meta_property(&self, _node: &Node /*MetaProperty*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn enable_substitutions_for_block_scoped_bindings(&self) {
        unimplemented!()
    }

    pub(super) fn enable_substitutions_for_captured_this(&self) {
        unimplemented!()
    }

    pub(super) fn get_class_member_prefix(
        &self,
        _node: &Node,   /*ClassExpression | ClassDeclaration*/
        _member: &Node, /*ClassElement*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn has_synthesized_default_super_call(
        &self,
        _constructor: Option<impl Borrow<Node /*ConstructorDeclaration*/>>,
        _has_extends_clause: bool,
    ) -> bool {
        unimplemented!()
    }
}
