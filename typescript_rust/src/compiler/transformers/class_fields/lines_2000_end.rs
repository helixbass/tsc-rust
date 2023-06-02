use gc::Gc;

use super::TransformClassFields;
use crate::{Node, VisitResult};

impl TransformClassFields {
    pub(super) fn visit_assignment_pattern(
        &self,
        _node: &Node, /*AssignmentPattern*/
    ) -> VisitResult {
        unimplemented!()
    }
}

pub(super) fn create_private_static_field_initializer(
    _variable_name: Gc<Node /*Identifier*/>,
    _initializer: Option<Gc<Node /*Expression*/>>,
) -> Gc<Node> {
    unimplemented!()
}

pub(super) fn create_private_instance_field_initializer(
    _receiver: Gc<Node /*LeftHandSideExpression*/>,
    _initializer: Option<Gc<Node /*Expression*/>>,
    _weak_map_name: Gc<Node /*Identifier*/>,
) -> Gc<Node> {
    unimplemented!()
}

pub(super) fn create_private_instance_method_initializer(
    _receiver: Gc<Node /*LeftHandSideExpression*/>,
    _weak_set_name: Gc<Node /*Identifier*/>,
) -> Gc<Node> {
    unimplemented!()
}
