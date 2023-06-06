use gc::Gc;

use super::TransformGenerators;
use crate::{Node, VisitResult};

impl TransformGenerators {
    pub(super) fn visit_for_in_statement(
        &self,
        _node: &Node, /*ForInStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_continue_statement(
        &self,
        _node: &Node, /*ContinueStatement*/
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub(super) fn visit_break_statement(
        &self,
        _node: &Node, /*BreakStatement*/
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub(super) fn visit_return_statement(
        &self,
        _node: &Node, /*ReturnStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_switch_statement(
        &self,
        _node: &Node, /*SwitchStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_labeled_statement(
        &self,
        _node: &Node, /*LabeledStatement*/
    ) -> VisitResult {
        unimplemented!()
    }
}
