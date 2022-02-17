#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::RefCell;
use std::rc::Rc;

use super::{CompilerOptions, Node, NodeArray, NodeArrayOrVec, SyntaxKind};
use crate::{BaseNodeFactory, NodeFactoryFlags};

bitflags! {
    pub struct OuterExpressionKinds: u32 {
        const None = 0;
        const Parentheses = 1 << 0;
        const TypeAssertions = 1 << 1;
        const NonNullAssertions = 1 << 2;
        const PartiallyEmittedExpressions = 1 << 3;

        const Assertions = Self::TypeAssertions.bits | Self::NonNullAssertions.bits;
        const All = Self::Parentheses.bits | Self::Assertions.bits | Self::PartiallyEmittedExpressions.bits;

        const ExcludeJSDocTypeAssertion = 1 << 4;
    }
}

pub trait ParenthesizerRules<TBaseNodeFactory: BaseNodeFactory> {
    // fn get_parenthesize_left_side_of_binary_for_operator(&self, binary_operator: SyntaxKind) ->
    // fn get_parenthesize_right_side_of_binary_for_operator(&self, binary_operator: SyntaxKind) ->
    fn parenthesize_left_side_of_binary(
        &self,
        base_factory: &TBaseNodeFactory,
        binary_operator: SyntaxKind,
        left_side: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_right_side_of_binary(
        &self,
        base_factory: &TBaseNodeFactory,
        binary_operator: SyntaxKind,
        left_side: Option<Rc<Node /*Expression*/>>,
        right_side: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_computed_property_name(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_condition_of_conditional_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        condition: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_branch_of_conditional_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        branch: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_export_default(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_new(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_left_side_of_access(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_operand_of_postfix_unary(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: &Node, /*Expression*/
    ) -> Rc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_operand_of_prefix_unary(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: &Node, /*Expression*/
    ) -> Rc<Node /*UnaryExpression*/>;
    fn parenthesize_expressions_of_comma_delimited_list(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: NodeArrayOrVec, /*<Expression>*/
    ) -> NodeArray /*<Expression>*/;
    fn parenthesize_expression_for_disallowed_comma(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_expression_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_concise_body_of_arrow_function(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression | ConciseBody*/
    ) -> Rc<Node /*Expression | ConciseBody*/>;
    fn parenthesize_member_of_conditional_type(
        &self,
        base_factory: &TBaseNodeFactory,
        member: &Node, /*TypeNode*/
    ) -> Rc<Node /*TypeNode*/>;
    fn parenthesize_member_of_element_type(
        &self,
        base_factory: &TBaseNodeFactory,
        member: &Node, /*TypeNode*/
    ) -> Rc<Node /*TypeNode*/>;
    fn parenthesize_element_type_of_array_type(
        &self,
        base_factory: &TBaseNodeFactory,
        member: &Node, /*TypeNode*/
    ) -> Rc<Node /*TypeNode*/>;
    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        base_factory: &TBaseNodeFactory,
        members: NodeArrayOrVec, /*<TypeNode>*/
    ) -> NodeArray /*<TypeNode>*/;
    fn parenthesize_type_arguments(
        &self,
        base_factory: &TBaseNodeFactory,
        type_parameters: Option<NodeArrayOrVec /*<TypeNode>*/>,
    ) -> Option<NodeArray /*<TypeNode>*/>;
}

pub trait NodeConverters<TBaseNodeFactory: BaseNodeFactory> {
    fn convert_to_function_block(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ConciseBody*/
        multi_line: Option<bool>,
    ) -> Rc<Node /*Block*/>;
    fn convert_to_function_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*FunctionDeclaration*/
    ) -> Rc<Node /*FunctionExpression*/>;
    fn convert_to_array_assignment_element(
        &self,
        base_factory: &TBaseNodeFactory,
        element: &Node, /*ArrayBindingOrAssignmentElement*/
    ) -> Rc<Node /*Expression*/>;
    fn convert_to_object_assignment_element(
        &self,
        base_factory: &TBaseNodeFactory,
        element: &Node, /*ObjectBindingOrAssignmentElement*/
    ) -> Rc<Node /*ObjectLiteralElementLike*/>;
    fn convert_to_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*BindingOrAssignmentPattern*/
    ) -> Rc<Node /*AssignmentPattern*/>;
    fn convert_to_object_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ObjectBindingOrAssignmentPattern*/
    ) -> Rc<Node /*ObjectLiteralExpression*/>;
    fn convert_to_array_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ArrayBindingOrAssignmentPattern*/
    ) -> Rc<Node /*ArrayLiteralExpression*/>;
    fn convert_to_assignment_element_target(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*BindingOrAssignmentElementTarget*/
    ) -> Rc<Node /*Expression*/>;
}

pub struct NodeFactory<TBaseNodeFactory> {
    pub flags: NodeFactoryFlags,
    pub parenthesizer_rules: RefCell<Option<Box<dyn ParenthesizerRules<TBaseNodeFactory>>>>,
    pub converters: RefCell<Option<Box<dyn NodeConverters<TBaseNodeFactory>>>>,
}

bitflags! {
    pub struct LexicalEnvironmentFlags: u32 {
        const None = 0;
        const InParameters = 1 << 0;
        const VariablesHoistedInParameters = 1 << 1;
    }
}

pub trait CoreTransformationContext<TBaseNodeFactory: BaseNodeFactory> {
    fn factory(&self) -> &NodeFactory<TBaseNodeFactory>;

    fn get_compiler_options(&self) -> &CompilerOptions;

    fn start_lexical_environment(&self);

    fn set_lexical_environment_flags(&self, flags: LexicalEnvironmentFlags, value: bool);
    fn get_lexical_environment_flags(&self) -> LexicalEnvironmentFlags;

    fn suspend_lexical_environment(&self);

    fn resume_lexical_environment(&self);

    fn end_lexical_environment(&self) -> Option<Vec<Rc<Node /*Statement*/>>>;

    fn hoist_function_declaration(&self, node: &Node /*FunctionDeclaration*/);

    fn start_block_scope(&self);

    fn end_block_scope(&self) -> Option<Vec<Rc<Node /*Statement*/>>>;

    fn add_block_scoped_variable(&self, node: &Node /*Identifier*/);

    fn add_initialization_statement(&self, node: &Node /*Statement*/);
}

pub struct TransformationContext {}

pub type TransformerFactory = Rc<dyn FnMut(TransformationContext) -> Transformer>;

pub type Transformer = Rc<dyn FnMut(&Node) -> Rc<Node>>;
