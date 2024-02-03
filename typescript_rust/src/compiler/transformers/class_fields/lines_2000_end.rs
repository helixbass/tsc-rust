use id_arena::Id;

use super::TransformClassFields;
use crate::{
    get_factory, is_array_literal_expression, is_expression, is_object_literal_element_like,
    visit_nodes, HasArena, InArena, Node, NodeArray, VisitResult,
};

impl TransformClassFields {
    pub(super) fn visit_assignment_pattern(
        &self,
        node: Id<Node>, /*AssignmentPattern*/
    ) -> VisitResult {
        if is_array_literal_expression(&node.ref_(self)) {
            let node_ref = node.ref_(self);
            let node_as_array_literal_expression = node_ref.as_array_literal_expression();
            Some(
                self.factory
                    .ref_(self)
                    .update_array_literal_expression(
                        node,
                        visit_nodes(
                            node_as_array_literal_expression.elements,
                            Some(|node: Id<Node>| self.visit_array_assignment_target(node)),
                            Some(|node| is_expression(node, self)),
                            None,
                            None,
                            self,
                        ),
                    )
                    .into(),
            )
        } else {
            let node_ref = node.ref_(self);
            let node_as_object_literal_expression = node_ref.as_object_literal_expression();
            Some(
                self.factory
                    .ref_(self)
                    .update_object_literal_expression(
                        node,
                        visit_nodes(
                            node_as_object_literal_expression.properties,
                            Some(|node: Id<Node>| self.visit_object_assignment_target(node)),
                            Some(|node: Id<Node>| is_object_literal_element_like(&node.ref_(self))),
                            None,
                            None,
                            self,
                        ),
                    )
                    .into(),
            )
        }
    }
}

pub(super) fn create_private_static_field_initializer(
    variable_name: Id<Node /*Identifier*/>,
    initializer: Option<Id<Node /*Expression*/>>,
    arena: &impl HasArena,
) -> Id<Node> {
    get_factory(arena).create_assignment(
        variable_name,
        get_factory(arena).create_object_literal_expression(
            Some(vec![get_factory(arena).create_property_assignment(
                "value",
                initializer.unwrap_or_else(|| get_factory(arena).create_void_zero()),
            )]),
            None,
        ),
    )
}

pub(super) fn create_private_instance_field_initializer(
    receiver: Id<Node /*LeftHandSideExpression*/>,
    initializer: Option<Id<Node /*Expression*/>>,
    weak_map_name: Id<Node /*Identifier*/>,
    arena: &impl HasArena,
) -> Id<Node> {
    get_factory(arena).create_call_expression(
        get_factory(arena).create_property_access_expression(weak_map_name, "set"),
        Option::<Id<NodeArray>>::None,
        Some(vec![
            receiver,
            initializer.unwrap_or_else(|| get_factory(arena).create_void_zero()),
        ]),
    )
}

pub(super) fn create_private_instance_method_initializer(
    receiver: Id<Node /*LeftHandSideExpression*/>,
    weak_set_name: Id<Node /*Identifier*/>,
    arena: &impl HasArena,
) -> Id<Node> {
    get_factory(arena).create_call_expression(
        get_factory(arena).create_property_access_expression(weak_set_name, "add"),
        Option::<Id<NodeArray>>::None,
        Some(vec![receiver]),
    )
}

pub(super) fn is_reserved_private_name(node: &Node /*PrivateIdentifier*/) -> bool {
    node.as_private_identifier().escaped_text == "#constructor"
}
