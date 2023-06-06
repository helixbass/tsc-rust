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

    pub(super) fn create_generator_resume(
        &self,
        _location: Option<&impl ReadonlyTextRange>,
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }
}
