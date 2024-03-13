use id_arena::Id;

use crate::{
    cast, get_starts_on_new_line, impl_has_arena, is_array_binding_pattern,
    is_array_literal_expression, is_binding_element, is_binding_pattern, is_block, is_expression,
    is_identifier, is_object_binding_pattern, is_object_literal_element_like,
    is_object_literal_expression, map, released, set_original_node, set_starts_on_new_line,
    AllArenas, Debug_, FunctionLikeDeclarationInterface, HasArena, HasInitializerInterface,
    HasTypeInterface, HasTypeParametersInterface, InArena, NamedDeclarationInterface, Node,
    NodeConverters, NodeExt, NodeFactory, NodeInterface, SignatureDeclarationInterface, SyntaxKind,
};

pub fn create_node_converters(
    factory: Id<NodeFactory>,
    arena: &impl HasArena,
) -> NodeConvertersConcrete {
    NodeConvertersConcrete::new(factory, arena)
}

pub struct NodeConvertersConcrete {
    arena: *const AllArenas,
    factory: Id<NodeFactory>,
}

impl NodeConvertersConcrete {
    pub fn new(factory: Id<NodeFactory>, arena: &impl HasArena) -> Self {
        Self {
            factory,
            arena: arena.arena(),
        }
    }
}

impl NodeConverters for NodeConvertersConcrete {
    fn convert_to_function_block(
        &self,
        node: Id<Node>, /*ConciseBody*/
        multi_line: Option<bool>,
    ) -> Id<Node /*Block*/> {
        if is_block(&node.ref_(self)) {
            return node;
        }
        let return_statement = self
            .factory
            .ref_(self)
            .create_return_statement_raw(Some(node));
        let return_statement = return_statement
            .alloc(self.arena())
            .set_text_range(Some(&*node.ref_(self)), self);
        let body = self
            .factory
            .ref_(self)
            .create_block_raw(vec![return_statement], multi_line);
        let body = body
            .alloc(self.arena())
            .set_text_range(Some(&*node.ref_(self)), self);
        body
    }

    fn convert_to_function_expression(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> Id<Node /*FunctionExpression*/> {
        let node_ref = node.ref_(self);
        let node_as_function_declaration = node_ref.as_function_declaration();
        if node_as_function_declaration.maybe_body().is_none() {
            Debug_.fail(Some("Cannot convert a FunctionDeclaration without a body"));
        }
        let updated = self.factory.ref_(self).create_function_expression(
            node_as_function_declaration
                .maybe_modifiers()
                .as_ref()
                .map(Clone::clone),
            node_as_function_declaration.maybe_asterisk_token(),
            node_as_function_declaration.maybe_name(),
            node_as_function_declaration
                .maybe_type_parameters()
                .as_ref()
                .map(Clone::clone),
            Some(node_as_function_declaration.parameters().clone()),
            node_as_function_declaration.maybe_type(),
            node_as_function_declaration.maybe_body().unwrap(),
        );
        set_original_node(updated, Some(node), self);
        let updated = updated.set_text_range(Some(&*node.ref_(self)), self);
        if get_starts_on_new_line(node, self) == Some(true) {
            set_starts_on_new_line(updated, true, self);
        }
        updated
    }

    fn convert_to_array_assignment_element(
        &self,
        element: Id<Node>, /*ArrayBindingOrAssignmentElement*/
    ) -> Id<Node /*Expression*/> {
        if is_binding_element(&element.ref_(self)) {
            if element
                .ref_(self)
                .as_binding_element()
                .dot_dot_dot_token
                .is_some()
            {
                Debug_.assert_node(
                    Some(element.ref_(self).as_binding_element().name()),
                    Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
                    None,
                );
                return self
                    .factory
                    .ref_(self)
                    .create_spread_element_raw(released!(element
                        .ref_(self)
                        .as_binding_element()
                        .name()))
                    .alloc(self.arena())
                    .set_text_range(Some(&*element.ref_(self)), self)
                    .set_original_node(Some(element), self);
            }
            let expression = self.convert_to_assignment_element_target(released!(element
                .ref_(self)
                .as_binding_element()
                .name()));
            return match released!(element.ref_(self).as_binding_element().maybe_initializer()) {
                Some(element_initializer) => {
                    return self
                        .factory
                        .ref_(self)
                        .create_assignment_raw(expression, element_initializer)
                        .alloc(self.arena())
                        .set_text_range(Some(&*element.ref_(self)), self)
                        .set_original_node(Some(element), self);
                }
                None => expression,
            };
        }
        cast(Some(element), |&element| is_expression(element, self))
    }

    fn convert_to_object_assignment_element(
        &self,
        element: Id<Node>, /*ObjectBindingOrAssignmentElement*/
    ) -> Id<Node /*ObjectLiteralElementLike*/> {
        if is_binding_element(&element.ref_(self)) {
            if element
                .ref_(self)
                .as_binding_element()
                .dot_dot_dot_token
                .is_some()
            {
                Debug_.assert_node(
                    Some(element.ref_(self).as_binding_element().name()),
                    Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
                    None,
                );
                return self
                    .factory
                    .ref_(self)
                    .create_spread_assignment_raw(released!(element
                        .ref_(self)
                        .as_binding_element()
                        .name()))
                    .alloc(self.arena())
                    .set_text_range(Some(&*element.ref_(self)), self)
                    .set_original_node(Some(element), self);
            }
            if let Some(element_property_name) =
                released!(element.ref_(self).as_binding_element().property_name)
            {
                let expression = self.convert_to_assignment_element_target(released!(element
                    .ref_(self)
                    .as_binding_element()
                    .name()));
                return self
                    .factory
                    .ref_(self)
                    .create_property_assignment_raw(
                        element_property_name.clone(),
                        match released!(element.ref_(self).as_binding_element().maybe_initializer())
                        {
                            Some(initializer) => self
                                .factory
                                .ref_(self)
                                .create_assignment(expression, initializer),
                            None => expression,
                        },
                    )
                    .alloc(self.arena())
                    .set_text_range(Some(&*element.ref_(self)), self)
                    .set_original_node(Some(element), self);
            }
            Debug_.assert_node(
                Some(element.ref_(self).as_binding_element().name()),
                Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
                None,
            );
            return self
                .factory
                .ref_(self)
                .create_shorthand_property_assignment_raw(
                    released!(element.ref_(self).as_binding_element().name()),
                    released!(element.ref_(self).as_binding_element().maybe_initializer()),
                )
                .alloc(self.arena())
                .set_text_range(Some(&*element.ref_(self)), self)
                .set_original_node(Some(element), self);
        }
        cast(Some(element), |element| {
            is_object_literal_element_like(&element.ref_(self))
        })
    }

    fn convert_to_assignment_pattern(
        &self,
        node: Id<Node>, /*BindingOrAssignmentPattern*/
    ) -> Id<Node /*AssignmentPattern*/> {
        match released!(node.ref_(self).kind()) {
            SyntaxKind::ArrayBindingPattern | SyntaxKind::ArrayLiteralExpression => {
                self.convert_to_array_assignment_pattern(node)
            }
            SyntaxKind::ObjectBindingPattern | SyntaxKind::ObjectLiteralExpression => {
                self.convert_to_object_assignment_pattern(node)
            }
            _ => panic!("Unexpected kind"),
        }
    }

    fn convert_to_object_assignment_pattern(
        &self,
        node: Id<Node>, /*ObjectBindingOrAssignmentPattern*/
    ) -> Id<Node /*ObjectLiteralExpression*/> {
        if is_object_binding_pattern(&node.ref_(self)) {
            return self
                .factory
                .ref_(self)
                .create_object_literal_expression_raw(
                    Some(map(
                        &*released!(node
                            .ref_(self)
                            .as_object_binding_pattern()
                            .elements
                            .ref_(self)
                            .clone()),
                        |&element, _| self.convert_to_object_assignment_element(element),
                    )),
                    None,
                )
                .alloc(self.arena())
                .set_text_range(Some(&*node.ref_(self)), self)
                .set_original_node(Some(node), self);
        }
        cast(Some(node), |node| {
            is_object_literal_expression(&node.ref_(self))
        })
    }

    fn convert_to_array_assignment_pattern(
        &self,
        node: Id<Node>, /*ArrayBindingOrAssignmentPattern*/
    ) -> Id<Node /*ArrayLiteralExpression*/> {
        if is_array_binding_pattern(&node.ref_(self)) {
            let node_ref = node.ref_(self);
            let node_as_array_binding_pattern = node_ref.as_array_binding_pattern();
            return self
                .factory
                .ref_(self)
                .create_array_literal_expression_raw(
                    Some(map(
                        &*released!(node_as_array_binding_pattern.elements.ref_(self).clone()),
                        |&element, _| self.convert_to_array_assignment_element(element),
                    )),
                    None,
                )
                .alloc(self.arena())
                .set_text_range(Some(&*node.ref_(self)), self)
                .set_original_node(Some(node), self);
        }
        cast(Some(node), |node| {
            is_array_literal_expression(&node.ref_(self))
        })
    }

    fn convert_to_assignment_element_target(
        &self,
        node: Id<Node>, /*BindingOrAssignmentElementTarget*/
    ) -> Id<Node /*Expression*/> {
        if is_binding_pattern(Some(&node.ref_(self))) {
            return self.convert_to_assignment_pattern(node);
        }
        cast(Some(node), |&node| is_expression(node, self))
    }
}

impl_has_arena!(NodeConvertersConcrete);

pub fn null_node_converters() -> NullNodeConverters {
    NullNodeConverters::new()
}

pub struct NullNodeConverters;

impl NullNodeConverters {
    pub fn new() -> Self {
        Self
    }
}

impl NodeConverters for NullNodeConverters {
    fn convert_to_function_block(
        &self,
        _node: Id<Node>, /*ConciseBody*/
        _multi_line: Option<bool>,
    ) -> Id<Node /*Block*/> {
        unimplemented!()
    }

    fn convert_to_function_expression(
        &self,
        _node: Id<Node>, /*FunctionDeclaration*/
    ) -> Id<Node /*FunctionExpression*/> {
        unimplemented!()
    }

    fn convert_to_array_assignment_element(
        &self,
        _element: Id<Node>, /*ArrayBindingOrAssignmentElement*/
    ) -> Id<Node /*Expression*/> {
        unimplemented!()
    }

    fn convert_to_object_assignment_element(
        &self,
        _element: Id<Node>, /*ObjectBindingOrAssignmentElement*/
    ) -> Id<Node /*ObjectLiteralElementLike*/> {
        unimplemented!()
    }

    fn convert_to_assignment_pattern(
        &self,
        _node: Id<Node>, /*BindingOrAssignmentPattern*/
    ) -> Id<Node /*AssignmentPattern*/> {
        unimplemented!()
    }

    fn convert_to_object_assignment_pattern(
        &self,
        _node: Id<Node>, /*ObjectBindingOrAssignmentPattern*/
    ) -> Id<Node /*ObjectLiteralExpression*/> {
        unimplemented!()
    }

    fn convert_to_array_assignment_pattern(
        &self,
        _node: Id<Node>, /*ArrayBindingOrAssignmentPattern*/
    ) -> Id<Node /*ArrayLiteralExpression*/> {
        unimplemented!()
    }

    fn convert_to_assignment_element_target(
        &self,
        _node: Id<Node>, /*BindingOrAssignmentElementTarget*/
    ) -> Id<Node /*Expression*/> {
        unimplemented!()
    }
}
