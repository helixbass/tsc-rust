use std::marker::PhantomData;

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use crate::{
    cast, get_starts_on_new_line, is_array_binding_pattern, is_array_literal_expression,
    is_binding_element, is_binding_pattern, is_block, is_expression, is_identifier,
    is_object_binding_pattern, is_object_literal_element_like, is_object_literal_expression, map,
    set_original_node, set_starts_on_new_line, set_text_range, BaseNodeFactory, Debug_,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasTypeInterface,
    HasTypeParametersInterface, NamedDeclarationInterface, Node, NodeConverters, NodeFactory,
    NodeInterface, SignatureDeclarationInterface, SyntaxKind,
    HasArena, AllArenas, InArena, NodeExt,
};

pub fn create_node_converters<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
    factory: Gc<NodeFactory<TBaseNodeFactory>>,
) -> NodeConvertersConcrete<TBaseNodeFactory> {
    NodeConvertersConcrete::new(factory)
}

#[derive(Trace, Finalize)]
pub struct NodeConvertersConcrete<TBaseNodeFactory: BaseNodeFactory + 'static + Trace + Finalize> {
    factory: Gc<NodeFactory<TBaseNodeFactory>>,
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>
    NodeConvertersConcrete<TBaseNodeFactory>
{
    pub fn new(factory: Gc<NodeFactory<TBaseNodeFactory>>) -> Self {
        Self { factory }
    }
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>
    NodeConverters<TBaseNodeFactory> for NodeConvertersConcrete<TBaseNodeFactory>
{
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
            .create_return_statement_raw(Some(node));
        let return_statement = return_statement.alloc(self.arena()).set_text_range(Some(&*node.ref_(self)), self);
        let body = self
            .factory
            .create_block_raw(vec![return_statement], multi_line);
        let body = body.alloc(self.arena()).set_text_range(Some(&*node.ref_(self)), self);
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
        let updated = self.factory.create_function_expression(
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
        if get_starts_on_new_line(&node.ref_(self)) == Some(true) {
            set_starts_on_new_line(updated, true, self);
        }
        updated
    }

    fn convert_to_array_assignment_element(
        &self,
        element: Id<Node>, /*ArrayBindingOrAssignmentElement*/
    ) -> Id<Node /*Expression*/> {
        if is_binding_element(&element.ref_(self)) {
            let element_ref = element.ref_(self);
            let element_as_binding_element = element_ref.as_binding_element();
            if element_as_binding_element.dot_dot_dot_token.is_some() {
                Debug_.assert_node(
                    Some(element_as_binding_element.name()),
                    Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
                    None,
                );
                return self
                    .factory
                    .create_spread_element_raw(element_as_binding_element.name())
                    .alloc(self.arena())
                    .set_text_range(Some(&*element.ref_(self)), self)
                    .set_original_node(Some(element), self);
            }
            let expression =
                self.convert_to_assignment_element_target(element_as_binding_element.name());
            return match element_as_binding_element.maybe_initializer() {
                Some(element_initializer) => {
                    return self
                        .factory
                        .create_assignment_raw(expression, element_initializer)
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
            let element_ref = element.ref_(self);
            let element_as_binding_element = element_ref.as_binding_element();
            if element_as_binding_element.dot_dot_dot_token.is_some() {
                Debug_.assert_node(
                    Some(element_as_binding_element.name()),
                    Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
                    None,
                );
                return self
                    .factory
                    .create_spread_assignment_raw(element_as_binding_element.name())
                    .alloc(self.arena())
                    .set_text_range(Some(&*element.ref_(self)),self)
                    .set_original_node(Some(element), self);
            }
            if let Some(element_property_name) = element_as_binding_element.property_name {
                let expression =
                    self.convert_to_assignment_element_target(element_as_binding_element.name());
                return self.factory.create_property_assignment_raw(
                    element_property_name.clone(),
                    match element_as_binding_element.maybe_initializer() {
                        Some(initializer) => {
                            self.factory.create_assignment(expression, initializer)
                        }
                        None => expression,
                    },
                )
                    .alloc(self.arena())
                    .set_text_range(Some(&*element.ref_(self)), self)
                    .set_original_node(Some(element), self);
            }
            Debug_.assert_node(
                Some(element_as_binding_element.name()),
                Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
                None,
            );
            return self.factory.create_shorthand_property_assignment_raw(
                element_as_binding_element.name(),
                element_as_binding_element.maybe_initializer(),
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
        match node.ref_(self).kind() {
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
            let node_ref = node.ref_(self);
            let node_as_object_binding_pattern = node_ref.as_object_binding_pattern();
            return self.factory.create_object_literal_expression_raw(
                Some(map(
                    &node_as_object_binding_pattern.elements,
                    |element, _| self.convert_to_object_assignment_element(element),
                )),
                None,
            )
                .alloc(self.arena())
                .set_text_range(Some(&*node.ref_(self)), self)
                .set_original_node(Some(node), self);
        }
        cast(Some(node), |node| is_object_literal_expression(&node.ref_(self)))
    }

    fn convert_to_array_assignment_pattern(
        &self,
        node: Id<Node>, /*ArrayBindingOrAssignmentPattern*/
    ) -> Id<Node /*ArrayLiteralExpression*/> {
        if is_array_binding_pattern(&node.ref_(self)) {
            let node_ref = node.ref_(self);
            let node_as_array_binding_pattern = node_ref.as_array_binding_pattern();
            return self.factory.create_array_literal_expression_raw(
                Some(map(
                    &node_as_array_binding_pattern.elements,
                    |element, _| self.convert_to_array_assignment_element(element),
                )),
                None,
            )
                .alloc(self.arena())
                .set_text_range(Some(&*node.ref_(self)), self)
                .set_original_node(Some(node), self);
        }
        cast(Some(node), |node| is_array_literal_expression(&node.ref_(self)))
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

impl<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>
    HasArena for NodeConvertersConcrete<TBaseNodeFactory>
{
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub fn null_node_converters<TBaseNodeFactory: BaseNodeFactory>(
) -> NullNodeConverters<TBaseNodeFactory> {
    NullNodeConverters::<TBaseNodeFactory>::new()
}

#[derive(Trace, Finalize)]
pub struct NullNodeConverters<TBaseNodeFactory: BaseNodeFactory> {
    #[unsafe_ignore_trace]
    _base_node_factory: PhantomData<TBaseNodeFactory>,
}

impl<TBaseNodeFactory: BaseNodeFactory> NullNodeConverters<TBaseNodeFactory> {
    pub fn new() -> Self {
        Self {
            _base_node_factory: PhantomData,
        }
    }
}

impl<TBaseNodeFactory: BaseNodeFactory> NodeConverters<TBaseNodeFactory>
    for NullNodeConverters<TBaseNodeFactory>
{
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
