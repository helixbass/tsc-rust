use std::rc::Rc;

use crate::{Node, NodeArray, NodeFactory, ParenthesizerRules, SyntaxKind};

pub fn create_parenthesizer_rules(factory: Rc<NodeFactory>) -> ParenthesizerRulesConcrete {}

pub struct ParenthesizerRulesConcrete {}

impl ParenthesizerRulesConcrete {}

impl ParenthesizerRules for ParenthesizerRulesConcrete {}

pub struct NullParenthesizerRules {}

impl NullParenthesizerRules {
    pub fn new() -> Self {
        Self {}
    }
}

impl ParenthesizerRules for NullParenthesizerRules {
    fn parenthesize_left_side_of_binary_for_operator(
        &self,
        _: SyntaxKind,
        left_side: Rc<Node>,
    ) -> Rc<Node> {
        left_side
    }

    fn parenthesize_right_side_of_binary_for_operator(
        &self,
        _: SyntaxKind,
        right_side: Rc<Node>,
    ) -> Rc<Node> {
        right_side
    }

    fn parenthesize_left_side_of_binary(&self, _: SyntaxKind, left_side: Rc<Node>) -> Rc<Node> {
        left_side
    }

    fn parenthesize_right_side_of_binary(
        &self,
        _: SyntaxKind,
        _left_side: Option<Rc<Node>>,
        right_side: Rc<Node>,
    ) -> Rc<Node> {
        right_side
    }

    fn parenthesize_expression_of_computed_property_name(&self, expression: Rc<Node>) -> Rc<Node> {
        expression
    }

    fn parenthesize_condition_of_conditional_expression(&self, condition: Rc<Node>) -> Rc<Node> {
        condition
    }

    fn parenthesize_branch_of_conditional_expression(&self, branch: Rc<Node>) -> Rc<Node> {
        branch
    }

    fn parenthesize_expression_of_export_default(&self, expression: Rc<Node>) -> Rc<Node> {
        expression
    }

    fn parenthesize_expression_of_new(&self, expression: Rc<Node>) -> Rc<Node> {
        expression
    }

    fn parenthesize_left_side_of_access(&self, expression: Rc<Node>) -> Rc<Node> {
        expression
    }

    fn parenthesize_operand_of_postfix_unary(&self, operand: Rc<Node>) -> Rc<Node> {
        operand
    }

    fn parenthesize_operand_of_prefix_unary(&self, operand: Rc<Node>) -> Rc<Node> {
        operand
    }

    fn parenthesize_expressions_of_comma_delimited_list(&self, elements: NodeArray) -> NodeArray {
        elements
    }

    fn parenthesize_expression_for_disallowed_comma(&self, expression: Rc<Node>) -> Rc<Node> {
        expression
    }

    fn parenthesize_expression_of_expression_statement(&self, expression: Rc<Node>) -> Rc<Node> {
        expression
    }

    fn parenthesize_concise_body_of_arrow_function(&self, expression: Rc<Node>) -> Rc<Node> {
        expression
    }

    fn parenthesize_member_of_conditional_type(&self, member: Rc<Node>) -> Rc<Node> {
        member
    }

    fn parenthesize_member_of_element_type(&self, member: Rc<Node>) -> Rc<Node> {
        member
    }

    fn parenthesize_element_type_of_array_type(&self, member: Rc<Node>) -> Rc<Node> {
        member
    }

    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        members: NodeArray,
    ) -> NodeArray {
        members
    }

    fn parenthesize_type_arguments(&self, type_parameters: Option<NodeArray>) -> Option<NodeArray> {
        type_parameters
    }
}
