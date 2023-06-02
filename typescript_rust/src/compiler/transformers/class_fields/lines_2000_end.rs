use gc::Gc;

use super::TransformClassFields;
use crate::{
    get_factory, is_array_literal_expression, is_expression, is_object_literal_element_like,
    visit_nodes, Node, NodeArray, NodeInterface, VisitResult,
};

impl TransformClassFields {
    pub(super) fn visit_assignment_pattern(
        &self,
        node: &Node, /*AssignmentPattern*/
    ) -> VisitResult {
        if is_array_literal_expression(node) {
            let node_as_array_literal_expression = node.as_array_literal_expression();
            Some(
                self.factory
                    .update_array_literal_expression(
                        node,
                        visit_nodes(
                            &node_as_array_literal_expression.elements,
                            Some(|node: &Node| self.visit_array_assignment_target(node)),
                            Some(is_expression),
                            None,
                            None,
                        ),
                    )
                    .into(),
            )
        } else {
            let node_as_object_literal_expression = node.as_object_literal_expression();
            Some(
                self.factory
                    .update_object_literal_expression(
                        node,
                        visit_nodes(
                            &node_as_object_literal_expression.properties,
                            Some(|node: &Node| self.visit_object_assignment_target(node)),
                            Some(is_object_literal_element_like),
                            None,
                            None,
                        ),
                    )
                    .into(),
            )
        }
    }
}

pub(super) fn create_private_static_field_initializer(
    variable_name: Gc<Node /*Identifier*/>,
    initializer: Option<Gc<Node /*Expression*/>>,
) -> Gc<Node> {
    get_factory()
        .create_assignment(
            variable_name,
            get_factory()
                .create_object_literal_expression(
                    Some(vec![get_factory()
                        .create_property_assignment(
                            "value",
                            initializer.unwrap_or_else(|| get_factory().create_void_zero()),
                        )
                        .wrap()]),
                    None,
                )
                .wrap(),
        )
        .wrap()
}

pub(super) fn create_private_instance_field_initializer(
    receiver: Gc<Node /*LeftHandSideExpression*/>,
    initializer: Option<Gc<Node /*Expression*/>>,
    weak_map_name: Gc<Node /*Identifier*/>,
) -> Gc<Node> {
    get_factory()
        .create_call_expression(
            get_factory()
                .create_property_access_expression(weak_map_name, "set")
                .wrap(),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                receiver,
                initializer.unwrap_or_else(|| get_factory().create_void_zero()),
            ]),
        )
        .wrap()
}

pub(super) fn create_private_instance_method_initializer(
    receiver: Gc<Node /*LeftHandSideExpression*/>,
    weak_set_name: Gc<Node /*Identifier*/>,
) -> Gc<Node> {
    get_factory()
        .create_call_expression(
            get_factory()
                .create_property_access_expression(weak_set_name, "add")
                .wrap(),
            Option::<Gc<NodeArray>>::None,
            Some(vec![receiver]),
        )
        .wrap()
}

pub(super) fn is_reserved_private_name(node: &Node /*PrivateIdentifier*/) -> bool {
    node.as_private_identifier().escaped_text == "#constructor"
}
