use std::borrow::Borrow;

use super::TransformGenerators;
use crate::{Node, ReadonlyTextRange};

impl TransformGenerators {
    pub(super) fn write_return(
        &self,
        _expression: Option<impl Borrow<Node>>,
        _operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        unimplemented!()
    }
}
