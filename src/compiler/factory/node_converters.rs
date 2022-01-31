use std::marker::PhantomData;
use std::rc::Rc;

use crate::{BaseNodeFactory, Node, NodeConverters, NodeFactory};

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

    fn convert_to_object_assigment_element(
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

    fn convert_to_object_assigment_element(
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
