use std::borrow::Borrow;

use gc::Gc;

use super::TransformGenerators;
use crate::{Node, NodeArray, VisitResult};

impl TransformGenerators {
    pub(super) fn transform_and_emit_for_in_statement(&self, _node: &Node /*ForInStatement*/) {
        unimplemented!()
    }

    pub(super) fn visit_for_in_statement(
        &self,
        _node: &Node, /*ForInStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_continue_statement(
        &self,
        _node: &Node, /*ContinueStatement*/
    ) {
        unimplemented!()
    }

    pub(super) fn visit_continue_statement(
        &self,
        _node: &Node, /*ContinueStatement*/
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_break_statement(&self, _node: &Node /*BreakStatement*/) {
        unimplemented!()
    }

    pub(super) fn visit_break_statement(
        &self,
        _node: &Node, /*BreakStatement*/
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_return_statement(
        &self,
        _node: &Node, /*ReturnStatement*/
    ) {
        unimplemented!()
    }

    pub(super) fn visit_return_statement(
        &self,
        _node: &Node, /*ReturnStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_with_statement(&self, _node: &Node /*WithStatement*/) {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_switch_statement(
        &self,
        _node: &Node, /*SwitchStatement*/
    ) {
        unimplemented!()
    }

    pub(super) fn visit_switch_statement(
        &self,
        _node: &Node, /*SwitchStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_labeled_statement(
        &self,
        _node: &Node, /*LabeledStatement*/
    ) {
        unimplemented!()
    }

    pub(super) fn visit_labeled_statement(
        &self,
        _node: &Node, /*LabeledStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_throw_statement(&self, _node: &Node /*ThrowStatement*/) {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_try_statement(&self, _node: &Node /*TryStatement*/) {
        unimplemented!()
    }

    pub(super) fn contains_yield(&self, _node: Option<impl Borrow<Node>>) -> bool {
        unimplemented!()
    }

    pub(super) fn count_initial_nodes_without_yield(
        &self,
        _nodes: &NodeArray, /*<Node>*/
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn cache_expression(
        &self,
        _node: &Node, /*Expression*/
    ) -> Gc<Node /*Identifier*/> {
        unimplemented!()
    }

    pub(super) fn declare_local(&self, _name: Option<&str>) -> Gc<Node /*Identifier*/> {
        unimplemented!()
    }
}
