use id_arena::Id;

use crate::{
    compare_values, get_expression_associativity, get_expression_precedence,
    get_leftmost_expression, get_operator_associativity, get_operator_precedence, impl_has_arena,
    is_binary_expression, is_block, is_call_expression, is_comma_sequence,
    is_function_or_constructor_type_node, is_left_hand_side_expression, is_literal_kind,
    is_unary_expression, maybe_same_map, released, same_map, set_text_range,
    set_text_range_id_node, skip_partially_emitted_expressions, some, AllArenas, Associativity,
    Comparison, HasArena, HasTypeArgumentsInterface, InArena, Node, NodeArray, NodeArrayOrVec,
    NodeExt, NodeFactory, NodeInterface, OperatorPrecedence, OuterExpressionKinds,
    ParenthesizerRules, SyntaxKind,
};

pub fn create_parenthesizer_rules(
    factory: Id<NodeFactory>,
    arena: &impl HasArena,
) -> ParenthesizerRulesConcrete {
    ParenthesizerRulesConcrete::new(factory, arena)
}

pub struct ParenthesizerRulesConcrete {
    arena: *const AllArenas,
    factory: Id<NodeFactory>,
}

impl ParenthesizerRulesConcrete {
    pub fn new(factory: Id<NodeFactory>, arena: &impl HasArena) -> Self {
        Self {
            factory,
            arena: arena.arena(),
        }
    }

    fn binary_operand_needs_parentheses(
        &self,
        binary_operator: SyntaxKind,
        operand: Id<Node>, /*Expression*/
        is_left_side_of_binary: bool,
        left_operand: Option<Id<Node>>,
    ) -> bool {
        let binary_operator_precedence =
            get_operator_precedence(SyntaxKind::BinaryExpression, binary_operator, None);
        let binary_operator_associativity =
            get_operator_associativity(SyntaxKind::BinaryExpression, binary_operator, None);
        let emitted_operand = skip_partially_emitted_expressions(operand, self);
        if !is_left_side_of_binary
            && operand.ref_(self).kind() == SyntaxKind::ArrowFunction
            && binary_operator_precedence > OperatorPrecedence::Assignment
        {
            return true;
        }
        let operand_precedence = get_expression_precedence(emitted_operand, self);
        match compare_values(Some(operand_precedence), Some(binary_operator_precedence)) {
            Comparison::LessThan => {
                if !is_left_side_of_binary
                    && binary_operator_associativity == Associativity::Right
                    && operand.ref_(self).kind() == SyntaxKind::YieldExpression
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
                    if is_binary_expression(&emitted_operand.ref_(self))
                        && emitted_operand
                            .ref_(self)
                            .as_binary_expression()
                            .operator_token
                            .ref_(self)
                            .kind()
                            == binary_operator
                    {
                        if self.operator_has_associative_property(binary_operator) {
                            return false;
                        }

                        if binary_operator == SyntaxKind::PlusToken {
                            let left_kind = match left_operand {
                                Some(left_operand) => {
                                    self.get_literal_kind_of_binary_plus_operand(left_operand)
                                }
                                None => SyntaxKind::Unknown,
                            };
                            if is_literal_kind(left_kind)
                                && left_kind
                                    == self.get_literal_kind_of_binary_plus_operand(emitted_operand)
                            {
                                return false;
                            }
                        }
                    }

                    let operand_associativity = get_expression_associativity(emitted_operand, self);
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
        let node = skip_partially_emitted_expressions(node, self);

        if is_literal_kind(node.ref_(self).kind()) {
            return node.ref_(self).kind();
        }

        if node.ref_(self).kind() == SyntaxKind::BinaryExpression
            && node
                .ref_(self)
                .as_binary_expression()
                .operator_token
                .ref_(self)
                .kind()
                == SyntaxKind::PlusToken
        {
            let node_ref = node.ref_(self);
            let node_as_binary_expression = node_ref.as_binary_expression();
            if node_as_binary_expression
                .maybe_cached_literal_kind()
                .is_some()
            {
                return node_as_binary_expression
                    .maybe_cached_literal_kind()
                    .unwrap();
            }

            let left_kind =
                self.get_literal_kind_of_binary_plus_operand(node_as_binary_expression.left);
            let literal_kind = if is_literal_kind(left_kind)
                && left_kind
                    == self.get_literal_kind_of_binary_plus_operand(node_as_binary_expression.right)
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

    fn parenthesize_binary_operand(
        &self,
        binary_operator: SyntaxKind,
        operand: Id<Node>, /*Expression*/
        is_left_side_of_binary: bool,
        left_operand: Option<Id<Node> /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
        let skipped = skip_partially_emitted_expressions(operand, self);

        if skipped.ref_(self).kind() == SyntaxKind::ParenthesizedExpression {
            return operand;
        }

        if self.binary_operand_needs_parentheses(
            binary_operator,
            operand,
            is_left_side_of_binary,
            left_operand,
        ) {
            self.factory
                .ref_(self)
                .create_parenthesized_expression(operand)
        } else {
            operand
        }
    }

    fn parenthesize_ordinal_type_argument(
        &self,
        node: Id<Node>, /*TypeNode*/
        i: usize,
    ) -> Id<Node /*TypeNode*/> {
        if i == 0
            && is_function_or_constructor_type_node(&node.ref_(self))
            && node
                .ref_(self)
                .as_has_type_parameters()
                .maybe_type_parameters()
                .is_some()
        {
            self.factory.ref_(self).create_parenthesized_type(node)
        } else {
            node
        }
    }
}

impl ParenthesizerRules for ParenthesizerRulesConcrete {
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
        if is_comma_sequence(expression, self) {
            self.factory
                .ref_(self)
                .create_parenthesized_expression(expression)
        } else {
            expression
        }
    }

    fn parenthesize_condition_of_conditional_expression(&self, condition: Id<Node>) -> Id<Node> {
        let conditional_precedence = get_operator_precedence(
            SyntaxKind::ConditionalExpression,
            SyntaxKind::QuestionToken,
            None,
        );
        let emitted_condition = skip_partially_emitted_expressions(condition, self);
        let condition_precedence = get_expression_precedence(emitted_condition, self);
        if compare_values(Some(condition_precedence), Some(conditional_precedence))
            != Comparison::GreaterThan
        {
            return self
                .factory
                .ref_(self)
                .create_parenthesized_expression(condition);
        }
        condition
    }

    fn parenthesize_branch_of_conditional_expression(&self, branch: Id<Node>) -> Id<Node> {
        let emitted_expression = skip_partially_emitted_expressions(branch, self);
        if is_comma_sequence(emitted_expression, self) {
            self.factory
                .ref_(self)
                .create_parenthesized_expression(branch)
        } else {
            branch
        }
    }

    fn parenthesize_expression_of_export_default(&self, expression: Id<Node>) -> Id<Node> {
        let check = skip_partially_emitted_expressions(expression, self);
        let mut needs_parens = is_comma_sequence(check, self);
        if !needs_parens {
            match get_leftmost_expression(check, false, self)
                .ref_(self)
                .kind()
            {
                SyntaxKind::ClassExpression | SyntaxKind::FunctionExpression => {
                    needs_parens = true;
                }
                _ => (),
            }
        }
        if needs_parens {
            self.factory
                .ref_(self)
                .create_parenthesized_expression(expression)
        } else {
            expression
        }
    }

    fn parenthesize_expression_of_new(&self, expression: Id<Node>) -> Id<Node> {
        let leftmost_expr = get_leftmost_expression(expression, true, self);
        match leftmost_expr.ref_(self).kind() {
            SyntaxKind::CallExpression => {
                return self
                    .factory
                    .ref_(self)
                    .create_parenthesized_expression(expression);
            }

            SyntaxKind::NewExpression => {
                return if leftmost_expr
                    .ref_(self)
                    .as_new_expression()
                    .arguments
                    .is_none()
                {
                    self.factory
                        .ref_(self)
                        .create_parenthesized_expression(expression)
                } else {
                    expression
                };
            }
            _ => (),
        }

        self.parenthesize_left_side_of_access(expression)
    }

    fn parenthesize_left_side_of_access(&self, expression: Id<Node>) -> Id<Node> {
        let emitted_expression = skip_partially_emitted_expressions(expression, self);
        if is_left_hand_side_expression(emitted_expression, self)
            && (emitted_expression.ref_(self).kind() != SyntaxKind::NewExpression
                || emitted_expression
                    .ref_(self)
                    .as_new_expression()
                    .arguments
                    .is_some())
        {
            return expression;
        }

        self.factory
            .ref_(self)
            .create_parenthesized_expression(expression)
            .set_text_range(Some(&*expression.ref_(self)), self)
    }

    fn parenthesize_operand_of_postfix_unary(&self, operand: Id<Node>) -> Id<Node> {
        if is_left_hand_side_expression(operand, self) {
            operand
        } else {
            self.factory
                .ref_(self)
                .create_parenthesized_expression(operand)
                .set_text_range(Some(&*operand.ref_(self)), self)
        }
    }

    fn parenthesize_operand_of_prefix_unary(&self, operand: Id<Node>) -> Id<Node> {
        if is_unary_expression(operand, self) {
            operand
        } else {
            self.factory
                .ref_(self)
                .create_parenthesized_expression(operand)
                .set_text_range(Some(&*operand.ref_(self)), self)
        }
    }

    fn parenthesize_expressions_of_comma_delimited_list(
        &self,
        elements: NodeArrayOrVec,
    ) -> Id<NodeArray> {
        let result = same_map(&elements.to_vec(self), |&element, _| {
            self.parenthesize_expression_for_disallowed_comma(element)
        });
        let node_array = self.factory.ref_(self).create_node_array(
            Some(result),
            match &elements {
                NodeArrayOrVec::NodeArray(elements) => Some(elements.ref_(self).has_trailing_comma),
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
                set_text_range(&*node_array.ref_(self), Some(&*elements.ref_(self)));
            }
            NodeArrayOrVec::Vec(_) => (),
        }
        node_array
    }

    fn parenthesize_expression_for_disallowed_comma(&self, expression: Id<Node>) -> Id<Node> {
        let emitted_expression = skip_partially_emitted_expressions(expression, self);
        let expression_precedence = get_expression_precedence(emitted_expression, self);
        let comma_precedence =
            get_operator_precedence(SyntaxKind::BinaryExpression, SyntaxKind::CommaToken, None);
        if expression_precedence > comma_precedence {
            expression
        } else {
            self.factory
                .ref_(self)
                .create_parenthesized_expression(expression)
                .set_text_range(Some(&*expression.ref_(self)), self)
        }
    }

    fn parenthesize_expression_of_expression_statement(&self, expression: Id<Node>) -> Id<Node> {
        let emitted_expression = skip_partially_emitted_expressions(expression, self);
        if is_call_expression(&emitted_expression.ref_(self)) {
            let callee = emitted_expression
                .ref_(self)
                .as_call_expression()
                .expression;
            let kind = skip_partially_emitted_expressions(callee, self)
                .ref_(self)
                .kind();
            if matches!(
                kind,
                SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
            ) {
                let updated = self.factory.ref_(self).update_call_expression(
                    emitted_expression,
                    set_text_range_id_node(
                        self.factory
                            .ref_(self)
                            .create_parenthesized_expression(callee),
                        Some(&*callee.ref_(self)),
                        self,
                    ),
                    emitted_expression
                        .ref_(self)
                        .as_call_expression()
                        .maybe_type_arguments(),
                    emitted_expression
                        .ref_(self)
                        .as_call_expression()
                        .arguments
                        .clone(),
                );
                return self.factory.ref_(self).restore_outer_expressions(
                    Some(expression),
                    updated,
                    Some(OuterExpressionKinds::PartiallyEmittedExpressions),
                );
            }
        }

        let leftmost_expression_kind = get_leftmost_expression(emitted_expression, false, self)
            .ref_(self)
            .kind();
        if matches!(
            leftmost_expression_kind,
            SyntaxKind::ObjectLiteralExpression | SyntaxKind::FunctionExpression
        ) {
            return self
                .factory
                .ref_(self)
                .create_parenthesized_expression(expression)
                .set_text_range(Some(&*expression.ref_(self)), self);
        }

        expression
    }

    fn parenthesize_concise_body_of_arrow_function(&self, body: Id<Node>) -> Id<Node> {
        if !is_block(&body.ref_(self))
            && (is_comma_sequence(body, self)
                || get_leftmost_expression(body, false, self).ref_(self).kind()
                    == SyntaxKind::ObjectLiteralExpression)
        {
            return self
                .factory
                .ref_(self)
                .create_parenthesized_expression(body)
                .set_text_range(Some(&*body.ref_(self)), self);
        }

        body
    }

    fn parenthesize_member_of_conditional_type(&self, member: Id<Node>) -> Id<Node> {
        if member.ref_(self).kind() == SyntaxKind::ConditionalType {
            self.factory.ref_(self).create_parenthesized_type(member)
        } else {
            member
        }
    }

    fn parenthesize_member_of_element_type(&self, member: Id<Node>) -> Id<Node> {
        match released!(member.ref_(self).kind()) {
            SyntaxKind::UnionType
            | SyntaxKind::IntersectionType
            | SyntaxKind::FunctionType
            | SyntaxKind::ConstructorType => {
                self.factory.ref_(self).create_parenthesized_type(member)
            }
            _ => self.parenthesize_member_of_conditional_type(member),
        }
    }

    fn parenthesize_element_type_of_array_type(&self, member: Id<Node>) -> Id<Node> {
        match member.ref_(self).kind() {
            SyntaxKind::TypeQuery | SyntaxKind::TypeOperator | SyntaxKind::InferType => {
                self.factory.ref_(self).create_parenthesized_type(member)
            }
            _ => self.parenthesize_member_of_element_type(member),
        }
    }

    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        members: NodeArrayOrVec,
    ) -> Id<NodeArray> {
        let members = match members {
            NodeArrayOrVec::NodeArray(members) => members.ref_(self).to_vec(),
            NodeArrayOrVec::Vec(members) => members,
        };
        self.factory.ref_(self).create_node_array(
            Some(same_map(&members, |&member, _| {
                self.parenthesize_member_of_element_type(member)
            })),
            None,
        )
    }

    fn parenthesize_type_arguments(
        &self,
        type_arguments: Option<NodeArrayOrVec>,
    ) -> Option<Id<NodeArray>> {
        if some(
            type_arguments
                .as_ref()
                .map(|type_arguments| type_arguments.to_vec(self))
                .as_ref(),
            Option::<fn(&Id<Node>) -> bool>::None,
        ) {
            return Some(
                self.factory.ref_(self).create_node_array(
                    maybe_same_map(
                        type_arguments
                            .as_ref()
                            .map(|type_arguments| type_arguments.to_vec(self))
                            .as_deref(),
                        |&type_arguments, index| {
                            self.parenthesize_ordinal_type_argument(type_arguments, index)
                        },
                    ),
                    None,
                ),
            );
        }
        None
    }
}

impl_has_arena!(ParenthesizerRulesConcrete);

pub fn null_parenthesizer_rules() -> NullParenthesizerRules {
    NullParenthesizerRules::new()
}

pub struct NullParenthesizerRules;

impl NullParenthesizerRules {
    pub fn new() -> Self {
        Self
    }
}

impl ParenthesizerRules for NullParenthesizerRules {
    fn parenthesize_left_side_of_binary(&self, _: SyntaxKind, left_side: Id<Node>) -> Id<Node> {
        left_side
    }

    fn parenthesize_right_side_of_binary(
        &self,
        _: SyntaxKind,
        _left_side: Option<Id<Node>>,
        right_side: Id<Node>,
    ) -> Id<Node> {
        right_side
    }

    fn parenthesize_expression_of_computed_property_name(&self, expression: Id<Node>) -> Id<Node> {
        expression
    }

    fn parenthesize_condition_of_conditional_expression(&self, condition: Id<Node>) -> Id<Node> {
        condition
    }

    fn parenthesize_branch_of_conditional_expression(&self, branch: Id<Node>) -> Id<Node> {
        branch
    }

    fn parenthesize_expression_of_export_default(&self, expression: Id<Node>) -> Id<Node> {
        expression
    }

    fn parenthesize_expression_of_new(&self, expression: Id<Node>) -> Id<Node> {
        expression
    }

    fn parenthesize_left_side_of_access(&self, expression: Id<Node>) -> Id<Node> {
        expression
    }

    fn parenthesize_operand_of_postfix_unary(&self, operand: Id<Node>) -> Id<Node> {
        operand
    }

    fn parenthesize_operand_of_prefix_unary(&self, operand: Id<Node>) -> Id<Node> {
        operand
    }

    fn parenthesize_expressions_of_comma_delimited_list(
        &self,
        elements: NodeArrayOrVec,
    ) -> Id<NodeArray> {
        match elements {
            NodeArrayOrVec::NodeArray(elements) => elements,
            NodeArrayOrVec::Vec(_) => {
                panic!("Expected NodeArray")
            }
        }
    }

    fn parenthesize_expression_for_disallowed_comma(&self, expression: Id<Node>) -> Id<Node> {
        expression
    }

    fn parenthesize_expression_of_expression_statement(&self, expression: Id<Node>) -> Id<Node> {
        expression
    }

    fn parenthesize_concise_body_of_arrow_function(&self, expression: Id<Node>) -> Id<Node> {
        expression
    }

    fn parenthesize_member_of_conditional_type(&self, member: Id<Node>) -> Id<Node> {
        member
    }

    fn parenthesize_member_of_element_type(&self, member: Id<Node>) -> Id<Node> {
        member
    }

    fn parenthesize_element_type_of_array_type(&self, member: Id<Node>) -> Id<Node> {
        member
    }

    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        members: NodeArrayOrVec,
    ) -> Id<NodeArray> {
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
    ) -> Option<Id<NodeArray>> {
        type_parameters.map(|type_parameters| match type_parameters {
            NodeArrayOrVec::NodeArray(type_parameters) => type_parameters,
            NodeArrayOrVec::Vec(_) => {
                panic!("Expected NodeArray")
            }
        })
    }
}
