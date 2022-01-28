use std::borrow::Borrow;
use std::marker::PhantomData;
use std::rc::Rc;

use crate::{
    compare_values, get_expression_associativity, get_expression_precedence,
    get_leftmost_expression, get_operator_associativity, get_operator_precedence,
    is_binary_expression, is_comma_sequence, is_left_hand_side_expression, is_literal_kind,
    set_text_range, skip_partially_emitted_expressions, Associativity, BaseNodeFactory, Comparison,
    Node, NodeArray, NodeFactory, NodeInterface, OperatorPrecedence, ParenthesizerRules,
    SyntaxKind,
};

pub fn create_parenthesizer_rules<TBaseNodeFactory: 'static + BaseNodeFactory>(
    factory: Rc<NodeFactory<TBaseNodeFactory>>,
) -> ParenthesizerRulesConcrete<TBaseNodeFactory> {
    ParenthesizerRulesConcrete::new(factory)
}

pub struct ParenthesizerRulesConcrete<TBaseNodeFactory: BaseNodeFactory> {
    factory: Rc<NodeFactory<TBaseNodeFactory>>,
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory> ParenthesizerRulesConcrete<TBaseNodeFactory> {
    pub fn new(factory: Rc<NodeFactory<TBaseNodeFactory>>) -> Self {
        Self { factory }
    }

    fn binary_operand_needs_parentheses<TLeftOperand: Borrow<Node>>(
        &self,
        binary_operator: SyntaxKind,
        operand: &Node, /*Expression*/
        is_left_side_of_binary: bool,
        left_operand: Option<TLeftOperand>,
    ) -> bool {
        let binary_operator_precedence =
            get_operator_precedence(SyntaxKind::BinaryExpression, binary_operator, None);
        let binary_operator_associativity =
            get_operator_associativity(SyntaxKind::BinaryExpression, binary_operator, None);
        let emitted_operand = skip_partially_emitted_expressions(operand);
        if !is_left_side_of_binary
            && operand.kind() == SyntaxKind::ArrowFunction
            && binary_operator_precedence > OperatorPrecedence::Assignment
        {
            return true;
        }
        let operand_precedence = get_expression_precedence(&emitted_operand);
        match compare_values(Some(operand_precedence), Some(binary_operator_precedence)) {
            Comparison::LessThan => {
                if !is_left_side_of_binary
                    && binary_operator_associativity == Associativity::Right
                    && operand.kind() == SyntaxKind::YieldExpression
                {
                    return false;
                }

                true
            }
            Comparison::GreaterThan => false,
            Comparison::EqualTo => {
                if is_left_side_of_binary {
                    binary_operator_associativity == Associativity::Right
                } else {
                    if is_binary_expression(&*emitted_operand)
                        && emitted_operand.as_binary_expression().operator_token.kind()
                            == binary_operator
                    {
                        if self.operator_has_associative_property(binary_operator) {
                            return false;
                        }

                        if binary_operator == SyntaxKind::PlusToken {
                            let left_kind = match left_operand {
                                Some(left_operand) => {
                                    let left_operand = left_operand.borrow();
                                    self.get_literal_kind_of_binary_plus_operand(left_operand)
                                }
                                None => SyntaxKind::Unknown,
                            };
                            if is_literal_kind(left_kind)
                                && left_kind
                                    == self
                                        .get_literal_kind_of_binary_plus_operand(&emitted_operand)
                            {
                                return false;
                            }
                        }
                    }

                    let operand_associativity = get_expression_associativity(&emitted_operand);
                    operand_associativity == Associativity::Left
                }
            }
        }
    }

    fn operator_has_associative_property(&self, binary_operator: SyntaxKind) -> bool {
        matches!(
            binary_operator,
            SyntaxKind::AsteriskToken
                | SyntaxKind::BarToken
                | SyntaxKind::AmpersandToken
                | SyntaxKind::CaretToken
        )
    }

    fn get_literal_kind_of_binary_plus_operand(
        &self,
        node: &Node, /*Expression*/
    ) -> SyntaxKind {
        let node = skip_partially_emitted_expressions(node);

        if is_literal_kind(node.kind()) {
            return node.kind();
        }

        if node.kind() == SyntaxKind::BinaryExpression
            && node.as_binary_expression().operator_token.kind() == SyntaxKind::PlusToken
        {
            let node_as_binary_expression = node.as_binary_expression();
            if node_as_binary_expression
                .maybe_cached_literal_kind()
                .is_some()
            {
                return node_as_binary_expression
                    .maybe_cached_literal_kind()
                    .unwrap();
            }

            let left_kind =
                self.get_literal_kind_of_binary_plus_operand(&node_as_binary_expression.left);
            let literal_kind = if is_literal_kind(left_kind)
                && left_kind
                    == self
                        .get_literal_kind_of_binary_plus_operand(&node_as_binary_expression.right)
            {
                left_kind
            } else {
                SyntaxKind::Unknown
            };

            node_as_binary_expression.set_cached_literal_kind(literal_kind);
            return literal_kind;
        }

        SyntaxKind::Unknown
    }

    fn parenthesize_binary_operand<TLeftOperand: Borrow<Node>>(
        &self,
        base_node_factory: &TBaseNodeFactory,
        binary_operator: SyntaxKind,
        operand: &Node, /*Expression*/
        is_left_side_of_binary: bool,
        left_operand: Option<TLeftOperand /*Expression*/>,
    ) -> Rc<Node /*Expression*/> {
        let skipped = skip_partially_emitted_expressions(operand);

        if skipped.kind() == SyntaxKind::ParenthesizedExpression {
            return operand.node_wrapper();
        }

        if self.binary_operand_needs_parentheses(
            binary_operator,
            operand,
            is_left_side_of_binary,
            left_operand,
        ) {
            self.factory
                .create_parenthesized_expression(base_node_factory, operand.node_wrapper())
                .into()
        } else {
            operand.node_wrapper()
        }
    }
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory> ParenthesizerRules<TBaseNodeFactory>
    for ParenthesizerRulesConcrete<TBaseNodeFactory>
{
    fn parenthesize_left_side_of_binary(
        &self,
        base_node_factory: &TBaseNodeFactory,
        binary_operator: SyntaxKind,
        left_side: &Node,
    ) -> Rc<Node> {
        self.parenthesize_binary_operand(
            base_node_factory,
            binary_operator,
            left_side,
            true,
            Option::<&Node>::None,
        )
    }

    fn parenthesize_right_side_of_binary(
        &self,
        base_node_factory: &TBaseNodeFactory,
        binary_operator: SyntaxKind,
        left_side: Option<Rc<Node>>,
        right_side: &Node,
    ) -> Rc<Node> {
        self.parenthesize_binary_operand(
            base_node_factory,
            binary_operator,
            right_side,
            false,
            left_side,
        )
    }

    fn parenthesize_expression_of_computed_property_name(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        if is_comma_sequence(expression) {
            self.factory
                .create_parenthesized_expression(base_node_factory, expression.node_wrapper())
                .into()
        } else {
            expression.node_wrapper()
        }
    }

    fn parenthesize_condition_of_conditional_expression(
        &self,
        base_node_factory: &TBaseNodeFactory,
        condition: &Node,
    ) -> Rc<Node> {
        let conditional_precedence = get_operator_precedence(
            SyntaxKind::ConditionalExpression,
            SyntaxKind::QuestionToken,
            None,
        );
        let emitted_condition = skip_partially_emitted_expressions(condition);
        let condition_precedence = get_expression_precedence(&emitted_condition);
        if compare_values(Some(condition_precedence), Some(conditional_precedence))
            != Comparison::GreaterThan
        {
            return self
                .factory
                .create_parenthesized_expression(base_node_factory, condition.node_wrapper())
                .into();
        }
        condition.node_wrapper()
    }

    fn parenthesize_branch_of_conditional_expression(
        &self,
        base_node_factory: &TBaseNodeFactory,
        branch: &Node,
    ) -> Rc<Node> {
        let emitted_expression = skip_partially_emitted_expressions(branch);
        if is_comma_sequence(&emitted_expression) {
            self.factory
                .create_parenthesized_expression(base_node_factory, branch.node_wrapper())
                .into()
        } else {
            branch.node_wrapper()
        }
    }

    fn parenthesize_expression_of_export_default(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        let check = skip_partially_emitted_expressions(expression);
        let mut needs_parens = is_comma_sequence(&check);
        if !needs_parens {
            match get_leftmost_expression(&check, false).kind() {
                SyntaxKind::ClassExpression | SyntaxKind::FunctionExpression => {
                    needs_parens = true;
                }
                _ => (),
            }
        }
        if needs_parens {
            self.factory
                .create_parenthesized_expression(base_node_factory, expression.node_wrapper())
                .into()
        } else {
            expression.node_wrapper()
        }
    }

    fn parenthesize_expression_of_new(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        let leftmost_expr = get_leftmost_expression(expression, true);
        match leftmost_expr.kind() {
            SyntaxKind::CallExpression => {
                return self
                    .factory
                    .create_parenthesized_expression(base_node_factory, expression.node_wrapper())
                    .into();
            }

            SyntaxKind::NewExpression => {
                return if leftmost_expr.as_new_expression().arguments.is_none() {
                    self.factory
                        .create_parenthesized_expression(
                            base_node_factory,
                            expression.node_wrapper(),
                        )
                        .into()
                } else {
                    expression.node_wrapper()
                };
            }
            _ => (),
        }

        self.parenthesize_left_side_of_access(base_node_factory, expression)
    }

    fn parenthesize_left_side_of_access(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        let emitted_expression = skip_partially_emitted_expressions(expression);
        if is_left_hand_side_expression(&emitted_expression)
            && (emitted_expression.kind() != SyntaxKind::NewExpression
                || emitted_expression.as_new_expression().arguments.is_some())
        {
            return expression.node_wrapper();
        }

        set_text_range(
            &*Into::<Rc<Node>>::into(
                self.factory
                    .create_parenthesized_expression(base_node_factory, expression.node_wrapper()),
            ),
            Some(expression),
        )
        .node_wrapper()
    }

    fn parenthesize_operand_of_postfix_unary(
        &self,
        base_node_factory: &TBaseNodeFactory,
        operand: &Node,
    ) -> Rc<Node> {
        operand.node_wrapper()
    }

    fn parenthesize_operand_of_prefix_unary(
        &self,
        base_node_factory: &TBaseNodeFactory,
        operand: &Node,
    ) -> Rc<Node> {
        operand.node_wrapper()
    }

    fn parenthesize_expressions_of_comma_delimited_list(
        &self,
        base_node_factory: &TBaseNodeFactory,
        elements: NodeArray,
    ) -> NodeArray {
        elements
    }

    fn parenthesize_expression_for_disallowed_comma(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_expression_of_expression_statement(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_concise_body_of_arrow_function(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_member_of_conditional_type(
        &self,
        base_node_factory: &TBaseNodeFactory,
        member: &Node,
    ) -> Rc<Node> {
        member.node_wrapper()
    }

    fn parenthesize_member_of_element_type(
        &self,
        base_node_factory: &TBaseNodeFactory,
        member: &Node,
    ) -> Rc<Node> {
        member.node_wrapper()
    }

    fn parenthesize_element_type_of_array_type(
        &self,
        base_node_factory: &TBaseNodeFactory,
        member: &Node,
    ) -> Rc<Node> {
        member.node_wrapper()
    }

    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        base_node_factory: &TBaseNodeFactory,
        members: NodeArray,
    ) -> NodeArray {
        members
    }

    fn parenthesize_type_arguments(
        &self,
        base_node_factory: &TBaseNodeFactory,
        type_parameters: Option<NodeArray>,
    ) -> Option<NodeArray> {
        type_parameters
    }
}

pub fn null_parenthesizer_rules<TBaseNodeFactory: BaseNodeFactory>(
) -> NullParenthesizerRules<TBaseNodeFactory> {
    NullParenthesizerRules::<TBaseNodeFactory>::new()
}

pub struct NullParenthesizerRules<TBaseNodeFactory: BaseNodeFactory> {
    _base_node_factory: PhantomData<TBaseNodeFactory>,
}

impl<TBaseNodeFactory: BaseNodeFactory> NullParenthesizerRules<TBaseNodeFactory> {
    pub fn new() -> Self {
        Self {
            _base_node_factory: PhantomData,
        }
    }
}

impl<TBaseNodeFactory: BaseNodeFactory> ParenthesizerRules<TBaseNodeFactory>
    for NullParenthesizerRules<TBaseNodeFactory>
{
    fn parenthesize_left_side_of_binary(
        &self,
        base_node_factory: &TBaseNodeFactory,
        _: SyntaxKind,
        left_side: &Node,
    ) -> Rc<Node> {
        left_side.node_wrapper()
    }

    fn parenthesize_right_side_of_binary(
        &self,
        base_node_factory: &TBaseNodeFactory,
        _: SyntaxKind,
        _left_side: Option<Rc<Node>>,
        right_side: &Node,
    ) -> Rc<Node> {
        right_side.node_wrapper()
    }

    fn parenthesize_expression_of_computed_property_name(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_condition_of_conditional_expression(
        &self,
        base_node_factory: &TBaseNodeFactory,
        condition: &Node,
    ) -> Rc<Node> {
        condition.node_wrapper()
    }

    fn parenthesize_branch_of_conditional_expression(
        &self,
        base_node_factory: &TBaseNodeFactory,
        branch: &Node,
    ) -> Rc<Node> {
        branch.node_wrapper()
    }

    fn parenthesize_expression_of_export_default(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_expression_of_new(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_left_side_of_access(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_operand_of_postfix_unary(
        &self,
        base_node_factory: &TBaseNodeFactory,
        operand: &Node,
    ) -> Rc<Node> {
        operand.node_wrapper()
    }

    fn parenthesize_operand_of_prefix_unary(
        &self,
        base_node_factory: &TBaseNodeFactory,
        operand: &Node,
    ) -> Rc<Node> {
        operand.node_wrapper()
    }

    fn parenthesize_expressions_of_comma_delimited_list(
        &self,
        base_node_factory: &TBaseNodeFactory,
        elements: NodeArray,
    ) -> NodeArray {
        elements
    }

    fn parenthesize_expression_for_disallowed_comma(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_expression_of_expression_statement(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_concise_body_of_arrow_function(
        &self,
        base_node_factory: &TBaseNodeFactory,
        expression: &Node,
    ) -> Rc<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_member_of_conditional_type(
        &self,
        base_node_factory: &TBaseNodeFactory,
        member: &Node,
    ) -> Rc<Node> {
        member.node_wrapper()
    }

    fn parenthesize_member_of_element_type(
        &self,
        base_node_factory: &TBaseNodeFactory,
        member: &Node,
    ) -> Rc<Node> {
        member.node_wrapper()
    }

    fn parenthesize_element_type_of_array_type(
        &self,
        base_node_factory: &TBaseNodeFactory,
        member: &Node,
    ) -> Rc<Node> {
        member.node_wrapper()
    }

    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        base_node_factory: &TBaseNodeFactory,
        members: NodeArray,
    ) -> NodeArray {
        members
    }

    fn parenthesize_type_arguments(
        &self,
        base_node_factory: &TBaseNodeFactory,
        type_parameters: Option<NodeArray>,
    ) -> Option<NodeArray> {
        type_parameters
    }
}
