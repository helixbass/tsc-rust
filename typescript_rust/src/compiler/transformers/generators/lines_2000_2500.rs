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

    pub(super) fn begin_script_loop_block(&self) {
        unimplemented!()
    }

    pub(super) fn begin_loop_block(&self, _continue_label: Label) -> Label {
        unimplemented!()
    }

    pub(super) fn end_loop_block(&self) {
        unimplemented!()
    }

    pub(super) fn create_generator_resume(
        &self,
        _location: Option<&impl ReadonlyTextRange>,
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }
}
