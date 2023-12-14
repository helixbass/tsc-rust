use std::{borrow::Borrow, marker::PhantomData};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use crate::{
    compare_values, get_expression_associativity, get_expression_precedence,
    get_leftmost_expression, get_operator_associativity, get_operator_precedence,
    is_binary_expression, is_block, is_call_expression, is_comma_sequence,
    is_function_or_constructor_type_node, is_left_hand_side_expression, is_literal_kind,
    is_unary_expression, maybe_same_map, same_map, set_text_range, set_text_range_rc_node,
    skip_partially_emitted_expressions, some, Associativity, BaseNodeFactory, Comparison,
    HasTypeArgumentsInterface, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeInterface,
    OperatorPrecedence, OuterExpressionKinds, ParenthesizerRules, SyntaxKind,
};

pub fn create_parenthesizer_rules<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: Gc<NodeFactory<TBaseNodeFactory>>,
) -> ParenthesizerRulesConcrete<TBaseNodeFactory> {
    ParenthesizerRulesConcrete::new(factory)
}

#[derive(Trace, Finalize)]
pub struct ParenthesizerRulesConcrete<
    TBaseNodeFactory: BaseNodeFactory + 'static + Trace + Finalize,
> {
    factory: Gc<NodeFactory<TBaseNodeFactory>>,
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>
    ParenthesizerRulesConcrete<TBaseNodeFactory>
{
    pub fn new(factory: Gc<NodeFactory<TBaseNodeFactory>>) -> Self {
        Self { factory }
    }

    fn binary_operand_needs_parentheses<TLeftOperand: Borrow<Node>>(
        &self,
        binary_operator: SyntaxKind,
        operand: Id<Node>, /*Expression*/
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
        node: Id<Node>, /*Expression*/
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
        binary_operator: SyntaxKind,
        operand: Id<Node>, /*Expression*/
        is_left_side_of_binary: bool,
        left_operand: Option<TLeftOperand /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
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
                .create_parenthesized_expression(operand.node_wrapper())
        } else {
            operand.node_wrapper()
        }
    }

    fn parenthesize_ordinal_type_argument(
        &self,
        node: Id<Node>, /*TypeNode*/
        i: usize,
    ) -> Id<Node /*TypeNode*/> {
        if i == 0
            && is_function_or_constructor_type_node(node)
            && node
                .as_has_type_parameters()
                .maybe_type_parameters()
                .is_some()
        {
            self.factory.create_parenthesized_type(node.node_wrapper())
        } else {
            node.node_wrapper()
        }
    }
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>
    ParenthesizerRules<TBaseNodeFactory> for ParenthesizerRulesConcrete<TBaseNodeFactory>
{
    fn parenthesize_left_side_of_binary(
        &self,
        binary_operator: SyntaxKind,
        left_side: Id<Node>,
    ) -> Id<Node> {
        self.parenthesize_binary_operand(binary_operator, left_side, true, Option::<Id<Node>>::None)
    }

    fn parenthesize_right_side_of_binary(
        &self,
        binary_operator: SyntaxKind,
        left_side: Option<Id<Node>>,
        right_side: Id<Node>,
    ) -> Id<Node> {
        self.parenthesize_binary_operand(binary_operator, right_side, false, left_side)
    }

    fn parenthesize_expression_of_computed_property_name(&self, expression: Id<Node>) -> Id<Node> {
        if is_comma_sequence(expression) {
            self.factory
                .create_parenthesized_expression(expression.node_wrapper())
        } else {
            expression.node_wrapper()
        }
    }

    fn parenthesize_condition_of_conditional_expression(&self, condition: Id<Node>) -> Id<Node> {
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
                .create_parenthesized_expression(condition.node_wrapper());
        }
        condition.node_wrapper()
    }

    fn parenthesize_branch_of_conditional_expression(&self, branch: Id<Node>) -> Id<Node> {
        let emitted_expression = skip_partially_emitted_expressions(branch);
        if is_comma_sequence(&emitted_expression) {
            self.factory
                .create_parenthesized_expression(branch.node_wrapper())
        } else {
            branch.node_wrapper()
        }
    }

    fn parenthesize_expression_of_export_default(&self, expression: Id<Node>) -> Id<Node> {
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
                .create_parenthesized_expression(expression.node_wrapper())
        } else {
            expression.node_wrapper()
        }
    }

    fn parenthesize_expression_of_new(&self, expression: Id<Node>) -> Id<Node> {
        let leftmost_expr = get_leftmost_expression(expression, true);
        match leftmost_expr.kind() {
            SyntaxKind::CallExpression => {
                return self
                    .factory
                    .create_parenthesized_expression(expression.node_wrapper());
            }

            SyntaxKind::NewExpression => {
                return if leftmost_expr.as_new_expression().arguments.is_none() {
                    self.factory
                        .create_parenthesized_expression(expression.node_wrapper())
                } else {
                    expression.node_wrapper()
                };
            }
            _ => (),
        }

        self.parenthesize_left_side_of_access(expression)
    }

    fn parenthesize_left_side_of_access(&self, expression: Id<Node>) -> Id<Node> {
        let emitted_expression = skip_partially_emitted_expressions(expression);
        if is_left_hand_side_expression(&emitted_expression)
            && (emitted_expression.kind() != SyntaxKind::NewExpression
                || emitted_expression.as_new_expression().arguments.is_some())
        {
            return expression.node_wrapper();
        }

        set_text_range(
            &*self
                .factory
                .create_parenthesized_expression(expression.node_wrapper()),
            Some(expression),
        )
        .node_wrapper()
    }

    fn parenthesize_operand_of_postfix_unary(&self, operand: Id<Node>) -> Id<Node> {
        if is_left_hand_side_expression(operand) {
            operand.node_wrapper()
        } else {
            set_text_range(
                &*self
                    .factory
                    .create_parenthesized_expression(operand.node_wrapper()),
                Some(operand),
            )
            .node_wrapper()
        }
    }

    fn parenthesize_operand_of_prefix_unary(&self, operand: Id<Node>) -> Id<Node> {
        if is_unary_expression(operand) {
            operand.node_wrapper()
        } else {
            set_text_range(
                &*self
                    .factory
                    .create_parenthesized_expression(operand.node_wrapper()),
                Some(operand),
            )
            .node_wrapper()
        }
    }

    fn parenthesize_expressions_of_comma_delimited_list(
        &self,
        elements: NodeArrayOrVec,
    ) -> Gc<NodeArray> {
        let result = same_map(&elements, |element, _| {
            self.parenthesize_expression_for_disallowed_comma(element)
        });
        let node_array = self.factory.create_node_array(
            Some(result),
            match &elements {
                NodeArrayOrVec::NodeArray(elements) => Some(elements.has_trailing_comma),
                NodeArrayOrVec::Vec(_) => None,
            },
        );
        // set_text_range(
        //     &mut node_array,
        //     match elements {
        //         NodeArrayOrVec::NodeArray(elements) => Some(&elements),
        //         NodeArrayOrVec::Vec(_) => None,
        //     },
        // );
        match elements {
            NodeArrayOrVec::NodeArray(elements) => {
                set_text_range(&*node_array, Some(&*elements));
            }
            NodeArrayOrVec::Vec(_) => (),
        }
        node_array
    }

    fn parenthesize_expression_for_disallowed_comma(&self, expression: Id<Node>) -> Id<Node> {
        let emitted_expression = skip_partially_emitted_expressions(expression);
        let expression_precedence = get_expression_precedence(&emitted_expression);
        let comma_precedence =
            get_operator_precedence(SyntaxKind::BinaryExpression, SyntaxKind::CommaToken, None);
        if expression_precedence > comma_precedence {
            expression.node_wrapper()
        } else {
            set_text_range(
                &*self
                    .factory
                    .create_parenthesized_expression(expression.node_wrapper()),
                Some(expression),
            )
            .node_wrapper()
        }
    }

    fn parenthesize_expression_of_expression_statement(&self, expression: Id<Node>) -> Id<Node> {
        let emitted_expression = skip_partially_emitted_expressions(expression);
        if is_call_expression(&emitted_expression) {
            let emitted_expression_as_call_expression = emitted_expression.as_call_expression();
            let callee = &emitted_expression_as_call_expression.expression;
            let kind = skip_partially_emitted_expressions(callee).kind();
            if matches!(
                kind,
                SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
            ) {
                let updated = self.factory.update_call_expression(
                    &emitted_expression,
                    set_text_range_rc_node(
                        self.factory
                            .create_parenthesized_expression(callee.node_wrapper()),
                        Some(&**callee),
                    ),
                    emitted_expression_as_call_expression.maybe_type_arguments(),
                    emitted_expression_as_call_expression.arguments.clone(),
                );
                return self.factory.restore_outer_expressions(
                    Some(expression),
                    &updated,
                    Some(OuterExpressionKinds::PartiallyEmittedExpressions),
                );
            }
        }

        let leftmost_expression_kind = get_leftmost_expression(&emitted_expression, false).kind();
        if matches!(
            leftmost_expression_kind,
            SyntaxKind::ObjectLiteralExpression | SyntaxKind::FunctionExpression
        ) {
            return set_text_range(
                &*self
                    .factory
                    .create_parenthesized_expression(expression.node_wrapper()),
                Some(expression),
            )
            .node_wrapper();
        }

        expression.node_wrapper()
    }

    fn parenthesize_concise_body_of_arrow_function(&self, body: Id<Node>) -> Id<Node> {
        if !is_block(body)
            && (is_comma_sequence(body)
                || get_leftmost_expression(body, false).kind()
                    == SyntaxKind::ObjectLiteralExpression)
        {
            return set_text_range(
                &*self
                    .factory
                    .create_parenthesized_expression(body.node_wrapper()),
                Some(body),
            )
            .node_wrapper();
        }

        body.node_wrapper()
    }

    fn parenthesize_member_of_conditional_type(&self, member: Id<Node>) -> Id<Node> {
        if member.kind() == SyntaxKind::ConditionalType {
            self.factory
                .create_parenthesized_type(member.node_wrapper())
        } else {
            member.node_wrapper()
        }
    }

    fn parenthesize_member_of_element_type(&self, member: Id<Node>) -> Id<Node> {
        match member.kind() {
            SyntaxKind::UnionType
            | SyntaxKind::IntersectionType
            | SyntaxKind::FunctionType
            | SyntaxKind::ConstructorType => self
                .factory
                .create_parenthesized_type(member.node_wrapper()),
            _ => self.parenthesize_member_of_conditional_type(member),
        }
    }

    fn parenthesize_element_type_of_array_type(&self, member: Id<Node>) -> Id<Node> {
        match member.kind() {
            SyntaxKind::TypeQuery | SyntaxKind::TypeOperator | SyntaxKind::InferType => self
                .factory
                .create_parenthesized_type(member.node_wrapper()),
            _ => self.parenthesize_member_of_element_type(member),
        }
    }

    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        members: NodeArrayOrVec,
    ) -> Gc<NodeArray> {
        let members = match members {
            NodeArrayOrVec::NodeArray(members) => members.to_vec(),
            NodeArrayOrVec::Vec(members) => members,
        };
        self.factory.create_node_array(
            Some(same_map(&members, |member, _| {
                self.parenthesize_member_of_element_type(member)
            })),
            None,
        )
    }

    fn parenthesize_type_arguments(
        &self,
        type_arguments: Option<NodeArrayOrVec>,
    ) -> Option<Gc<NodeArray>> {
        if some(
            type_arguments.as_deref(),
            Option::<fn(&Id<Node>) -> bool>::None,
        ) {
            return Some(self.factory.create_node_array(
                maybe_same_map(type_arguments.as_deref(), |type_arguments, index| {
                    self.parenthesize_ordinal_type_argument(type_arguments, index)
                }),
                None,
            ));
        }
        None
    }
}

pub fn null_parenthesizer_rules<TBaseNodeFactory: BaseNodeFactory>(
) -> NullParenthesizerRules<TBaseNodeFactory> {
    NullParenthesizerRules::<TBaseNodeFactory>::new()
}

#[derive(Trace, Finalize)]
pub struct NullParenthesizerRules<TBaseNodeFactory: BaseNodeFactory> {
    #[unsafe_ignore_trace]
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
    fn parenthesize_left_side_of_binary(&self, _: SyntaxKind, left_side: Id<Node>) -> Id<Node> {
        left_side.node_wrapper()
    }

    fn parenthesize_right_side_of_binary(
        &self,
        _: SyntaxKind,
        _left_side: Option<Id<Node>>,
        right_side: Id<Node>,
    ) -> Id<Node> {
        right_side.node_wrapper()
    }

    fn parenthesize_expression_of_computed_property_name(&self, expression: Id<Node>) -> Id<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_condition_of_conditional_expression(&self, condition: Id<Node>) -> Id<Node> {
        condition.node_wrapper()
    }

    fn parenthesize_branch_of_conditional_expression(&self, branch: Id<Node>) -> Id<Node> {
        branch.node_wrapper()
    }

    fn parenthesize_expression_of_export_default(&self, expression: Id<Node>) -> Id<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_expression_of_new(&self, expression: Id<Node>) -> Id<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_left_side_of_access(&self, expression: Id<Node>) -> Id<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_operand_of_postfix_unary(&self, operand: Id<Node>) -> Id<Node> {
        operand.node_wrapper()
    }

    fn parenthesize_operand_of_prefix_unary(&self, operand: Id<Node>) -> Id<Node> {
        operand.node_wrapper()
    }

    fn parenthesize_expressions_of_comma_delimited_list(
        &self,
        elements: NodeArrayOrVec,
    ) -> Gc<NodeArray> {
        match elements {
            NodeArrayOrVec::NodeArray(elements) => elements,
            NodeArrayOrVec::Vec(_) => {
                panic!("Expected NodeArray")
            }
        }
    }

    fn parenthesize_expression_for_disallowed_comma(&self, expression: Id<Node>) -> Id<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_expression_of_expression_statement(&self, expression: Id<Node>) -> Id<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_concise_body_of_arrow_function(&self, expression: Id<Node>) -> Id<Node> {
        expression.node_wrapper()
    }

    fn parenthesize_member_of_conditional_type(&self, member: Id<Node>) -> Id<Node> {
        member.node_wrapper()
    }

    fn parenthesize_member_of_element_type(&self, member: Id<Node>) -> Id<Node> {
        member.node_wrapper()
    }

    fn parenthesize_element_type_of_array_type(&self, member: Id<Node>) -> Id<Node> {
        member.node_wrapper()
    }

    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        members: NodeArrayOrVec,
    ) -> Gc<NodeArray> {
        match members {
            NodeArrayOrVec::NodeArray(members) => members,
            NodeArrayOrVec::Vec(_) => {
                panic!("Expected NodeArray")
            }
        }
    }

    fn parenthesize_type_arguments(
        &self,
        type_parameters: Option<NodeArrayOrVec>,
    ) -> Option<Gc<NodeArray>> {
        type_parameters.map(|type_parameters| match type_parameters {
            NodeArrayOrVec::NodeArray(type_parameters) => type_parameters,
            NodeArrayOrVec::Vec(_) => {
                panic!("Expected NodeArray")
            }
        })
    }
}
