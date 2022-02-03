use std::marker::PhantomData;
use std::rc::Rc;

use crate::{
    cast, get_starts_on_new_line, is_array_binding_pattern, is_array_literal_expression,
    is_binding_element, is_binding_pattern, is_block, is_expression, is_identifier,
    is_object_binding_pattern, is_object_literal_element_like, is_object_literal_expression, map,
    set_original_node, set_starts_on_new_line, set_text_range, BaseNodeFactory, Debug_,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasTypeInterface,
    HasTypeParametersInterface, NamedDeclarationInterface, Node, NodeConverters, NodeFactory,
    NodeInterface, SignatureDeclarationInterface, SyntaxKind,
};

pub fn create_node_converters<TBaseNodeFactory: 'static + BaseNodeFactory>(
    factory: Rc<NodeFactory<TBaseNodeFactory>>,
) -> NodeConvertersConcrete<TBaseNodeFactory> {
    NodeConvertersConcrete::new(factory)
}

pub struct NodeConvertersConcrete<TBaseNodeFactory: BaseNodeFactory> {
    factory: Rc<NodeFactory<TBaseNodeFactory>>,
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeConvertersConcrete<TBaseNodeFactory> {
    pub fn new(factory: Rc<NodeFactory<TBaseNodeFactory>>) -> Self {
        Self { factory }
    }
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeConverters<TBaseNodeFactory>
    for NodeConvertersConcrete<TBaseNodeFactory>
{
    fn convert_to_function_block(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ConciseBody*/
        multi_line: Option<bool>,
    ) -> Rc<Node /*Block*/> {
        if is_block(node) {
            return node.node_wrapper();
        }
        let return_statement = self
            .factory
            .create_return_statement(base_factory, Some(node.node_wrapper()));
        let return_statement =
            set_text_range(&*Into::<Rc<Node>>::into(return_statement), Some(node)).node_wrapper();
        let body = self
            .factory
            .create_block(base_factory, vec![return_statement], multi_line);
        let body = set_text_range(&*Into::<Rc<Node>>::into(body), Some(node)).node_wrapper();
        body
    }

    fn convert_to_function_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*FunctionDeclaration*/
    ) -> Rc<Node /*FunctionExpression*/> {
        let node_as_function_declaration = node.as_function_declaration();
        if node_as_function_declaration.maybe_body().is_none() {
            Debug_.fail(Some("Cannot convert a FunctionDeclaration without a body"));
        }
        let updated: Rc<Node> = self
            .factory
            .create_function_expression(
                base_factory,
                node_as_function_declaration
                    .maybe_modifiers()
                    .map(Clone::clone),
                node_as_function_declaration.maybe_asterisk_token(),
                node_as_function_declaration.maybe_name(),
                node_as_function_declaration
                    .maybe_type_parameters()
                    .map(Clone::clone),
                node_as_function_declaration.parameters().clone(),
                node_as_function_declaration.maybe_type(),
                node_as_function_declaration.maybe_body().unwrap(),
            )
            .into();
        set_original_node(updated.clone(), Some(node.node_wrapper()));
        let updated = set_text_range(&*updated, Some(node)).node_wrapper();
        if get_starts_on_new_line(node) {
            set_starts_on_new_line(&updated, true);
        }
        updated
    }

    fn convert_to_array_assignment_element(
        &self,
        base_factory: &TBaseNodeFactory,
        element: &Node, /*ArrayBindingOrAssignmentElement*/
    ) -> Rc<Node /*Expression*/> {
        if is_binding_element(element) {
            let element_as_binding_element = element.as_binding_element();
            if element_as_binding_element.dot_dot_dot_token.is_some() {
                Debug_.assert_node(
                    Some(element_as_binding_element.name()),
                    Some(is_identifier),
                    None,
                );
                let ret = self
                    .factory
                    .create_spread_element(base_factory, element_as_binding_element.name());
                let ret =
                    set_text_range(&*Into::<Rc<Node>>::into(ret), Some(element)).node_wrapper();
                set_original_node(ret.clone(), Some(element.node_wrapper()));
                return ret;
            }
            let expression = self.convert_to_assignment_element_target(
                base_factory,
                &element_as_binding_element.name(),
            );
            return match element_as_binding_element.maybe_initializer() {
                Some(element_initializer) => {
                    let ret = self.factory.create_assignment(
                        base_factory,
                        expression,
                        element_initializer,
                    );
                    let ret =
                        set_text_range(&*Into::<Rc<Node>>::into(ret), Some(element)).node_wrapper();
                    set_original_node(ret.clone(), Some(element.node_wrapper()));
                    return ret;
                }
                None => expression,
            };
        }
        cast(Some(element), |element| is_expression(element)).node_wrapper()
    }

    fn convert_to_object_assignment_element(
        &self,
        base_factory: &TBaseNodeFactory,
        element: &Node, /*ObjectBindingOrAssignmentElement*/
    ) -> Rc<Node /*ObjectLiteralElementLike*/> {
        if is_binding_element(element) {
            let element_as_binding_element = element.as_binding_element();
            if element_as_binding_element.dot_dot_dot_token.is_some() {
                Debug_.assert_node(
                    Some(element_as_binding_element.name()),
                    Some(is_identifier),
                    None,
                );
                let ret = self
                    .factory
                    .create_spread_assignment(base_factory, element_as_binding_element.name());
                let ret =
                    set_text_range(&*Into::<Rc<Node>>::into(ret), Some(element)).node_wrapper();
                set_original_node(ret.clone(), Some(element.node_wrapper()));
                return ret;
            }
            if let Some(element_property_name) = element_as_binding_element.property_name.as_ref() {
                let expression = self.convert_to_assignment_element_target(
                    base_factory,
                    &element_as_binding_element.name(),
                );
                let ret = self.factory.create_property_assignment(
                    base_factory,
                    element_property_name.clone(),
                    match element_as_binding_element.maybe_initializer() {
                        Some(initializer) => self
                            .factory
                            .create_assignment(base_factory, expression, initializer)
                            .into(),
                        None => expression,
                    },
                );
                let ret =
                    set_text_range(&*Into::<Rc<Node>>::into(ret), Some(element)).node_wrapper();
                set_original_node(ret.clone(), Some(element.node_wrapper()));
                return ret;
            }
            Debug_.assert_node(
                Some(element_as_binding_element.name()),
                Some(is_identifier),
                None,
            );
            let ret = self.factory.create_shorthand_property_assignment(
                base_factory,
                element_as_binding_element.name(),
                element_as_binding_element.maybe_initializer(),
            );
            let ret = set_text_range(&*Into::<Rc<Node>>::into(ret), Some(element)).node_wrapper();
            set_original_node(ret.clone(), Some(element.node_wrapper()));
            return ret;
        }
        cast(Some(element), |element| {
            is_object_literal_element_like(element)
        })
        .node_wrapper()
    }

    fn convert_to_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*BindingOrAssignmentPattern*/
    ) -> Rc<Node /*AssignmentPattern*/> {
        match node.kind() {
            SyntaxKind::ArrayBindingPattern | SyntaxKind::ArrayLiteralExpression => {
                self.convert_to_array_assignment_pattern(base_factory, node)
            }
            SyntaxKind::ObjectBindingPattern | SyntaxKind::ObjectLiteralExpression => {
                self.convert_to_object_assignment_pattern(base_factory, node)
            }
            _ => panic!("Unexpected kind"),
        }
    }

    fn convert_to_object_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ObjectBindingOrAssignmentPattern*/
    ) -> Rc<Node /*ObjectLiteralExpression*/> {
        if is_object_binding_pattern(node) {
            let node_as_object_binding_pattern = node.as_object_binding_pattern();
            let ret = self.factory.create_object_literal_expression(
                base_factory,
                map(
                    Some(&node_as_object_binding_pattern.elements),
                    |element, _| self.convert_to_object_assignment_element(base_factory, element),
                ),
                None,
            );
            let ret = set_text_range(&*Into::<Rc<Node>>::into(ret), Some(node)).node_wrapper();
            set_original_node(ret.clone(), Some(node.node_wrapper()));
            return ret;
        }
        cast(Some(node), |node| is_object_literal_expression(node)).node_wrapper()
    }

    fn convert_to_array_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ArrayBindingOrAssignmentPattern*/
    ) -> Rc<Node /*ArrayLiteralExpression*/> {
        if is_array_binding_pattern(node) {
            let node_as_array_binding_pattern = node.as_array_binding_pattern();
            let ret = self.factory.create_array_literal_expression(
                base_factory,
                map(
                    Some(&node_as_array_binding_pattern.elements),
                    |element, _| self.convert_to_array_assignment_element(base_factory, element),
                ),
                None,
            );
            let ret = set_text_range(&*Into::<Rc<Node>>::into(ret), Some(node)).node_wrapper();
            set_original_node(ret.clone(), Some(node.node_wrapper()));
            return ret;
        }
        cast(Some(node), |node| is_array_literal_expression(node)).node_wrapper()
    }

    fn convert_to_assignment_element_target(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*BindingOrAssignmentElementTarget*/
    ) -> Rc<Node /*Expression*/> {
        if is_binding_pattern(Some(node)) {
            return self.convert_to_assignment_pattern(base_factory, node);
        }
        cast(Some(node), |node| is_expression(node)).node_wrapper()
    }
}

pub fn null_node_converters<TBaseNodeFactory: BaseNodeFactory>(
) -> NullNodeConverters<TBaseNodeFactory> {
    NullNodeConverters::<TBaseNodeFactory>::new()
}

pub struct NullNodeConverters<TBaseNodeFactory: BaseNodeFactory> {
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
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ConciseBody*/
        multi_line: Option<bool>,
    ) -> Rc<Node /*Block*/> {
        unimplemented!()
    }

    fn convert_to_function_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*FunctionDeclaration*/
    ) -> Rc<Node /*FunctionExpression*/> {
        unimplemented!()
    }

    fn convert_to_array_assignment_element(
        &self,
        base_factory: &TBaseNodeFactory,
        element: &Node, /*ArrayBindingOrAssignmentElement*/
    ) -> Rc<Node /*Expression*/> {
        unimplemented!()
    }

    fn convert_to_object_assignment_element(
        &self,
        base_factory: &TBaseNodeFactory,
        element: &Node, /*ObjectBindingOrAssignmentElement*/
    ) -> Rc<Node /*ObjectLiteralElementLike*/> {
        unimplemented!()
    }

    fn convert_to_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*BindingOrAssignmentPattern*/
    ) -> Rc<Node /*AssignmentPattern*/> {
        unimplemented!()
    }

    fn convert_to_object_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ObjectBindingOrAssignmentPattern*/
    ) -> Rc<Node /*ObjectLiteralExpression*/> {
        unimplemented!()
    }

    fn convert_to_array_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ArrayBindingOrAssignmentPattern*/
    ) -> Rc<Node /*ArrayLiteralExpression*/> {
        unimplemented!()
    }

    fn convert_to_assignment_element_target(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*BindingOrAssignmentElementTarget*/
    ) -> Rc<Node /*Expression*/> {
        unimplemented!()
    }
}
