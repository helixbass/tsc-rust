use gc::Gc;

use super::{Label, TransformGenerators};
use crate::{Node, ReadonlyTextRange};

impl TransformGenerators {
    pub(super) fn define_label(&self) -> Label {
        unimplemented!()
    }

    pub(super) fn mark_label(&self, _label: Label) {
        unimplemented!()
    }

    pub(super) fn begin_with_block(&self, _expression: &Node /*Identifier*/) {
        unimplemented!()
    }

    pub(super) fn end_with_block(&self) {
        unimplemented!()
    }

    pub(super) fn begin_script_loop_block(&self) {
        unimplemented!()
    }

    pub(super) fn begin_loop_block(&self, _continue_label: Label) -> Label {
        unimplemented!()
    }

    pub(super) fn end_loop_block(&self) {
        unimplemented!()
    }

    pub(super) fn find_break_target(&self, _label_text: Option<&str>) -> Label {
        unimplemented!()
    }

    pub(super) fn find_continue_target(&self, _label_text: Option<&str>) -> Label {
        unimplemented!()
    }

    pub(super) fn create_inline_break(
        &self,
        _label: Label,
        _location: Option<&impl ReadonlyTextRange>,
    ) -> Gc<Node /*ReturnStatement*/> {
        unimplemented!()
    }

    pub(super) fn create_inline_return(
        &self,
        _expression: Option<Gc<Node>>,
        _location: Option<&impl ReadonlyTextRange>,
    ) -> Gc<Node /*ReturnStatement*/> {
        unimplemented!()
    }

    pub(super) fn create_generator_resume(
        &self,
        _location: Option<&impl ReadonlyTextRange>,
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }
}
