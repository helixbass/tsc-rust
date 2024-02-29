use std::{cell::RefCell, convert::TryInto, io, rc::Rc};

use id_arena::Id;

use super::{CheckMode, IterationUse, TypeFacts};
use crate::{
    create_binary_expression_trampoline, get_assigned_expando_initializer,
    get_assignment_declaration_kind, get_object_flags, get_source_file_of_node, impl_has_arena,
    is_assignment_operator, is_binary_expression, is_if_statement, is_in_js_file,
    is_private_identifier_property_access_expression, is_spread_assignment, push_or_replace,
    released, skip_parentheses, skip_trivia, text_span_contains_position, token_to_string,
    walk_up_parenthesized_expressions, AccessFlags, AllArenas, AssignmentDeclarationKind,
    BinaryExpressionStateMachine, BinaryExpressionTrampoline, Debug_,
    DiagnosticRelatedInformationInterface, Diagnostics, ExternalEmitHelpers, HasArena, InArena,
    LeftOrRight, NamedDeclarationInterface, Node, NodeArray, NodeInterface, Number, ObjectFlags,
    OptionTry, ReadonlyTextRange, ScriptTarget, SourceFileLike, Symbol, SyntaxKind, TextSpan, Type,
    TypeChecker, TypeFlags, TypeInterface, UnionReduction,
};

impl TypeChecker {
    pub(super) fn check_object_literal_assignment(
        &self,
        node: Id<Node>, /*ObjectLiteralExpression*/
        source_type: Id<Type>,
        right_is_this: Option<bool>,
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let properties = node_ref.as_object_literal_expression().properties;
        if self.strict_null_checks && properties.ref_(self).is_empty() {
            return self.check_non_null_type(source_type, node);
        }
        for i in 0..properties.ref_(self).len() {
            self.check_object_literal_destructuring_property_assignment(
                node,
                source_type,
                i,
                Some(properties),
                right_is_this,
            )?;
        }
        Ok(source_type)
    }

    pub(super) fn check_object_literal_destructuring_property_assignment(
        &self,
        node: Id<Node>, /*ObjectLiteralExpression*/
        object_literal_type: Id<Type>,
        property_index: usize,
        all_properties: Option<Id<NodeArray> /*<ObjectLiteralElementLike>*/>,
        right_is_this: Option<bool>,
    ) -> io::Result<Option<Id<Type>>> {
        let right_is_this = right_is_this.unwrap_or(false);
        let node_ref = node.ref_(self);
        let properties = node_ref.as_object_literal_expression().properties;
        let property = properties.ref_(self)[property_index];
        Ok(match property.ref_(self).kind() {
            SyntaxKind::PropertyAssignment | SyntaxKind::ShorthandPropertyAssignment => {
                let name = property.ref_(self).as_named_declaration().name();
                let expr_type = self.get_literal_type_from_property_name(name)?;
                if self.is_type_usable_as_property_name(expr_type) {
                    let text = self.get_property_name_from_type(expr_type);
                    let prop = self.get_property_of_type_(object_literal_type, &text, None)?;
                    if let Some(prop) = prop {
                        self.mark_property_as_referenced(prop, Some(property), right_is_this);
                        self.check_property_accessibility(
                            property,
                            false,
                            true,
                            object_literal_type,
                            prop,
                            None,
                        )?;
                    }
                }
                let element_type = self.get_indexed_access_type(
                    object_literal_type,
                    expr_type,
                    Some(AccessFlags::ExpressionPosition),
                    Some(name),
                    Option::<Id<Symbol>>::None,
                    None,
                )?;
                let type_ = self.get_flow_type_of_destructuring(property, element_type)?;
                Some(self.check_destructuring_assignment(
                    if property.ref_(self).kind() == SyntaxKind::ShorthandPropertyAssignment {
                        property
                    } else {
                        property
                            .ref_(self)
                            .as_has_initializer()
                            .maybe_initializer()
                            .unwrap()
                    },
                    type_,
                    None,
                    None,
                )?)
            }
            SyntaxKind::SpreadAssignment => {
                if property_index < properties.ref_(self).len() - 1 {
                    self.error(
                        Some(property),
                        &Diagnostics::A_rest_element_must_be_last_in_a_destructuring_pattern,
                        None,
                    );
                    None
                } else {
                    if self.language_version < ScriptTarget::ESNext {
                        self.check_external_emit_helpers(property, ExternalEmitHelpers::Rest)?;
                    }
                    let mut non_rest_names: Vec<Id<Node /*PropertyName*/>> = vec![];
                    if let Some(all_properties) = all_properties {
                        for other_property in &*all_properties.ref_(self) {
                            if !is_spread_assignment(&other_property.ref_(self)) {
                                non_rest_names
                                    .push(other_property.ref_(self).as_named_declaration().name());
                            }
                        }
                    }
                    let type_ = self.get_rest_type(
                        object_literal_type,
                        &non_rest_names,
                        object_literal_type.ref_(self).maybe_symbol(),
                    )?;
                    self.check_grammar_for_disallowed_trailing_comma(
                        all_properties,
                        Some(&Diagnostics::A_rest_parameter_or_binding_pattern_may_not_have_a_trailing_comma)
                    );
                    Some(self.check_destructuring_assignment(
                        property.ref_(self).as_has_expression().expression(),
                        type_,
                        None,
                        None,
                    )?)
                }
            }
            _ => {
                self.error(
                    Some(property),
                    &Diagnostics::Property_assignment_expected,
                    None,
                );
                None
            }
        })
    }

    pub(super) fn check_array_literal_assignment(
        &self,
        node: Id<Node>, /*ArrayLiteralExpression*/
        source_type: Id<Type>,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_array_literal_expression = node_ref.as_array_literal_expression();
        let elements = &node_as_array_literal_expression.elements;
        if self.language_version < ScriptTarget::ES2015
            && self.compiler_options.ref_(self).downlevel_iteration == Some(true)
        {
            self.check_external_emit_helpers(node, ExternalEmitHelpers::Read)?;
        }
        let possibly_out_of_bounds_type = self.check_iterated_type_or_element_type(
            IterationUse::Destructuring | IterationUse::PossiblyOutOfBounds,
            source_type,
            self.undefined_type(),
            Some(node)
        )? /*|| errorType*/;
        let mut in_bounds_type =
            if self.compiler_options.ref_(self).no_unchecked_indexed_access == Some(true) {
                None
            } else {
                Some(possibly_out_of_bounds_type.clone())
            };
        for i in 0..elements.ref_(self).len() {
            let mut type_ = possibly_out_of_bounds_type.clone();
            if node_as_array_literal_expression.elements.ref_(self)[i]
                .ref_(self)
                .kind()
                == SyntaxKind::SpreadElement
            {
                in_bounds_type = Some(in_bounds_type.try_unwrap_or_else(|| {
                    self.check_iterated_type_or_element_type(
                        IterationUse::Destructuring,
                        source_type,
                        self.undefined_type(),
                        Some(node),
                    ) /*|| errorType*/
                })?);
                type_ = in_bounds_type.clone().unwrap();
            }
            self.check_array_literal_destructuring_element_assignment(
                node,
                source_type,
                i,
                type_,
                check_mode,
            )?;
        }
        Ok(source_type)
    }

    pub(super) fn check_array_literal_destructuring_element_assignment(
        &self,
        node: Id<Node>, /*ArrayLiteralExpression*/
        source_type: Id<Type>,
        element_index: usize,
        element_type: Id<Type>,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Option<Id<Type>>> {
        let node_ref = node.ref_(self);
        let node_as_array_literal_expression = node_ref.as_array_literal_expression();
        let elements = node_as_array_literal_expression.elements;
        let element = elements.ref_(self)[element_index];
        if element.ref_(self).kind() != SyntaxKind::OmittedExpression {
            if element.ref_(self).kind() != SyntaxKind::SpreadElement {
                let index_type = self.get_number_literal_type(Number::new(element_index as f64));
                if self.is_array_like_type(source_type)? {
                    let access_flags = AccessFlags::ExpressionPosition
                        | if self.has_default_value(element) {
                            AccessFlags::NoTupleBoundsCheck
                        } else {
                            AccessFlags::None
                        };
                    let element_type = self
                        .get_indexed_access_type_or_undefined(
                            source_type,
                            index_type,
                            Some(access_flags),
                            Some(self.create_synthetic_expression(
                                element,
                                index_type,
                                None,
                                Option::<Id<Node>>::None,
                            )),
                            Option::<Id<Symbol>>::None,
                            None,
                        )?
                        .unwrap_or_else(|| self.error_type());
                    let assigned_type = if self.has_default_value(element) {
                        self.get_type_with_facts(element_type, TypeFacts::NEUndefined)?
                    } else {
                        element_type.clone()
                    };
                    let type_ = self.get_flow_type_of_destructuring(element, assigned_type)?;
                    return Ok(Some(self.check_destructuring_assignment(
                        element, type_, check_mode, None,
                    )?));
                }
                return Ok(Some(self.check_destructuring_assignment(
                    element,
                    element_type,
                    check_mode,
                    None,
                )?));
            }
            if element_index < elements.ref_(self).len() - 1 {
                self.error(
                    Some(element),
                    &Diagnostics::A_rest_element_must_be_last_in_a_destructuring_pattern,
                    None,
                );
            } else {
                let rest_expression = element.ref_(self).as_spread_element().expression;
                if rest_expression.ref_(self).kind() == SyntaxKind::BinaryExpression
                    && rest_expression
                        .ref_(self)
                        .as_binary_expression()
                        .operator_token
                        .ref_(self)
                        .kind()
                        == SyntaxKind::EqualsToken
                {
                    self.error(
                        Some(
                            rest_expression
                                .ref_(self)
                                .as_binary_expression()
                                .operator_token,
                        ),
                        &Diagnostics::A_rest_element_cannot_have_an_initializer,
                        None,
                    );
                } else {
                    self.check_grammar_for_disallowed_trailing_comma(
                        Some(node_as_array_literal_expression.elements),
                        Some(&Diagnostics::A_rest_parameter_or_binding_pattern_may_not_have_a_trailing_comma)
                    );
                    let type_ = if self
                        .every_type(source_type, |type_: Id<Type>| self.is_tuple_type(type_))
                    {
                        self.try_map_type(
                            source_type,
                            &mut |t: Id<Type>| -> io::Result<_> {
                                Ok(Some(self.slice_tuple_type(t, element_index, None)?))
                            },
                            None,
                        )?
                        .unwrap()
                    } else {
                        self.create_array_type(element_type, None)
                    };
                    return Ok(Some(self.check_destructuring_assignment(
                        rest_expression,
                        type_,
                        check_mode,
                        None,
                    )?));
                }
            }
        }
        Ok(None)
    }

    pub(super) fn check_destructuring_assignment(
        &self,
        expr_or_assignment: Id<Node>, /*Expression | ShorthandPropertyAssignment*/
        mut source_type: Id<Type>,
        check_mode: Option<CheckMode>,
        right_is_this: Option<bool>,
    ) -> io::Result<Id<Type>> {
        let mut target: Id<Node>;
        if expr_or_assignment.ref_(self).kind() == SyntaxKind::ShorthandPropertyAssignment {
            let expr_or_assignment_ref = expr_or_assignment.ref_(self);
            let prop = expr_or_assignment_ref.as_shorthand_property_assignment();
            if let Some(prop_object_assignment_initializer) = prop.object_assignment_initializer {
                if self.strict_null_checks
                    && !self
                        .get_falsy_flags(self.check_expression(
                            prop_object_assignment_initializer,
                            None,
                            None,
                        )?)
                        .intersects(TypeFlags::Undefined)
                {
                    source_type = self.get_type_with_facts(source_type, TypeFacts::NEUndefined)?;
                }
                self.check_binary_like_expression(
                    prop.name(),
                    prop.equals_token.unwrap(),
                    prop_object_assignment_initializer,
                    check_mode,
                    Option::<Id<Node>>::None,
                )?;
            }
            target = expr_or_assignment
                .ref_(self)
                .as_shorthand_property_assignment()
                .name();
        } else {
            target = expr_or_assignment;
        }

        if target.ref_(self).kind() == SyntaxKind::BinaryExpression
            && target
                .ref_(self)
                .as_binary_expression()
                .operator_token
                .ref_(self)
                .kind()
                == SyntaxKind::EqualsToken
        {
            self.check_binary_expression()
                .ref_(self)
                .call(target, check_mode)?;
            target = target.ref_(self).as_binary_expression().left;
        }
        if target.ref_(self).kind() == SyntaxKind::ObjectLiteralExpression {
            return self.check_object_literal_assignment(target, source_type, right_is_this);
        }
        if target.ref_(self).kind() == SyntaxKind::ArrayLiteralExpression {
            return self.check_array_literal_assignment(target, source_type, check_mode);
        }
        self.check_reference_assignment(target, source_type, check_mode)
    }

    pub(super) fn check_reference_assignment(
        &self,
        target: Id<Node>, /*Expression*/
        source_type: Id<Type>,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let target_type = self.check_expression(target, check_mode, None)?;
        let error = if target.ref_(self).parent().ref_(self).kind() == SyntaxKind::SpreadAssignment
        {
            &*Diagnostics::The_target_of_an_object_rest_assignment_must_be_a_variable_or_a_property_access
        } else {
            &*Diagnostics::The_left_hand_side_of_an_assignment_expression_must_be_a_variable_or_a_property_access
        };
        let optional_error = if target.ref_(self).parent().ref_(self).kind()
            == SyntaxKind::SpreadAssignment
        {
            &*Diagnostics::The_target_of_an_object_rest_assignment_may_not_be_an_optional_property_access
        } else {
            &*Diagnostics::The_left_hand_side_of_an_assignment_expression_may_not_be_an_optional_property_access
        };
        if self.check_reference_expression(target, error, optional_error) {
            self.check_type_assignable_to_and_optionally_elaborate(
                source_type,
                target_type,
                Some(target),
                Some(target),
                None,
                None,
            )?;
        }
        if is_private_identifier_property_access_expression(target, self) {
            self.check_external_emit_helpers(
                target.ref_(self).parent(),
                ExternalEmitHelpers::ClassPrivateFieldSet,
            )?;
        }
        Ok(source_type)
    }

    pub(super) fn is_side_effect_free(&self, node: Id<Node>) -> bool {
        let node = skip_parentheses(node, None, self);
        match node.ref_(self).kind() {
            SyntaxKind::Identifier
            | SyntaxKind::StringLiteral
            | SyntaxKind::RegularExpressionLiteral
            | SyntaxKind::TaggedTemplateExpression
            | SyntaxKind::TemplateExpression
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::NullKeyword
            | SyntaxKind::UndefinedKeyword
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ClassExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::ArrayLiteralExpression
            | SyntaxKind::ObjectLiteralExpression
            | SyntaxKind::TypeOfExpression
            | SyntaxKind::NonNullExpression
            | SyntaxKind::JsxSelfClosingElement
            | SyntaxKind::JsxElement => true,

            SyntaxKind::ConditionalExpression => {
                let node_ref = node.ref_(self);
                let node_as_conditional_expression = node_ref.as_conditional_expression();
                self.is_side_effect_free(node_as_conditional_expression.when_true)
                    && self.is_side_effect_free(node_as_conditional_expression.when_false)
            }

            SyntaxKind::BinaryExpression => {
                let node_ref = node.ref_(self);
                let node_as_binary_expression = node_ref.as_binary_expression();
                if is_assignment_operator(
                    node_as_binary_expression.operator_token.ref_(self).kind(),
                ) {
                    return false;
                }
                self.is_side_effect_free(node_as_binary_expression.left)
                    && self.is_side_effect_free(node_as_binary_expression.right)
            }

            SyntaxKind::PrefixUnaryExpression | SyntaxKind::PostfixUnaryExpression => {
                matches!(
                    node.ref_(self).as_unary_expression().operator(),
                    SyntaxKind::ExclamationToken
                        | SyntaxKind::PlusToken
                        | SyntaxKind::MinusToken
                        | SyntaxKind::TildeToken
                )
            }

            _ => false,
        }
    }

    pub(super) fn is_type_equality_comparable_to(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<bool> {
        Ok(target.ref_(self).flags().intersects(TypeFlags::Nullable)
            || self.is_type_comparable_to(source, target)?)
    }

    pub(super) fn create_check_binary_expression(&self) -> CheckBinaryExpression {
        let trampoline = create_binary_expression_trampoline(
            CheckBinaryExpressionStateMachine::new(self.arena_id(), self),
        );
        CheckBinaryExpression::new(trampoline)
    }

    pub(super) fn check_grammar_nullish_coalesce_with_logical_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        let left = node_as_binary_expression.left;
        let operator_token = node_as_binary_expression.operator_token;
        let right = node_as_binary_expression.right;
        if operator_token.ref_(self).kind() == SyntaxKind::QuestionQuestionToken {
            if is_binary_expression(&left.ref_(self))
                && matches!(
                    left.ref_(self)
                        .as_binary_expression()
                        .operator_token
                        .ref_(self)
                        .kind(),
                    SyntaxKind::BarBarToken | SyntaxKind::AmpersandAmpersandToken
                )
            {
                self.grammar_error_on_node(
                    left,
                    &Diagnostics::_0_and_1_operations_cannot_be_mixed_without_parentheses,
                    Some(vec![
                        token_to_string(
                            left.ref_(self)
                                .as_binary_expression()
                                .operator_token
                                .ref_(self)
                                .kind(),
                        )
                        .unwrap()
                        .to_owned(),
                        token_to_string(operator_token.ref_(self).kind())
                            .unwrap()
                            .to_owned(),
                    ]),
                );
            }
            if is_binary_expression(&right.ref_(self))
                && matches!(
                    right
                        .ref_(self)
                        .as_binary_expression()
                        .operator_token
                        .ref_(self)
                        .kind(),
                    SyntaxKind::BarBarToken | SyntaxKind::AmpersandAmpersandToken
                )
            {
                self.grammar_error_on_node(
                    right,
                    &Diagnostics::_0_and_1_operations_cannot_be_mixed_without_parentheses,
                    Some(vec![
                        token_to_string(
                            right
                                .ref_(self)
                                .as_binary_expression()
                                .operator_token
                                .ref_(self)
                                .kind(),
                        )
                        .unwrap()
                        .to_owned(),
                        token_to_string(operator_token.ref_(self).kind())
                            .unwrap()
                            .to_owned(),
                    ]),
                );
            }
        }
    }

    pub(super) fn check_binary_like_expression(
        &self,
        left: Id<Node>, /*Expression*/
        operator_token: Id<Node>,
        right: Id<Node>, /*Expression*/
        check_mode: Option<CheckMode>,
        error_node: Option<Id<Node>>,
    ) -> io::Result<Id<Type>> {
        let operator = operator_token.ref_(self).kind();
        if operator == SyntaxKind::EqualsToken
            && matches!(
                left.ref_(self).kind(),
                SyntaxKind::ObjectLiteralExpression | SyntaxKind::ArrayLiteralExpression
            )
        {
            return self.check_destructuring_assignment(
                left,
                self.check_expression(right, check_mode, None)?,
                check_mode,
                Some(right.ref_(self).kind() == SyntaxKind::ThisKeyword),
            );
        }
        let left_type: Id<Type>;
        if matches!(
            operator,
            SyntaxKind::AmpersandAmpersandToken
                | SyntaxKind::BarBarToken
                | SyntaxKind::QuestionQuestionToken
        ) {
            left_type = self.check_truthiness_expression(left, check_mode)?;
        } else {
            left_type = self.check_expression(left, check_mode, None)?;
        }

        let right_type = self.check_expression(right, check_mode, None)?;
        self.check_binary_like_expression_worker(
            left,
            operator_token,
            right,
            left_type,
            right_type,
            error_node,
        )
    }

    pub(super) fn check_binary_like_expression_worker(
        &self,
        left: Id<Node>, /*Expression*/
        operator_token: Id<Node>,
        right: Id<Node>, /*Expression*/
        mut left_type: Id<Type>,
        mut right_type: Id<Type>,
        error_node: Option<Id<Node>>,
    ) -> io::Result<Id<Type>> {
        let operator = operator_token.ref_(self).kind();
        Ok(match operator {
            SyntaxKind::AsteriskToken
            | SyntaxKind::AsteriskAsteriskToken
            | SyntaxKind::AsteriskEqualsToken
            | SyntaxKind::AsteriskAsteriskEqualsToken
            | SyntaxKind::SlashToken
            | SyntaxKind::SlashEqualsToken
            | SyntaxKind::PercentToken
            | SyntaxKind::PercentEqualsToken
            | SyntaxKind::MinusToken
            | SyntaxKind::MinusEqualsToken
            | SyntaxKind::LessThanLessThanToken
            | SyntaxKind::LessThanLessThanEqualsToken
            | SyntaxKind::GreaterThanGreaterThanToken
            | SyntaxKind::GreaterThanGreaterThanEqualsToken
            | SyntaxKind::GreaterThanGreaterThanGreaterThanToken
            | SyntaxKind::GreaterThanGreaterThanGreaterThanEqualsToken
            | SyntaxKind::BarToken
            | SyntaxKind::BarEqualsToken
            | SyntaxKind::CaretToken
            | SyntaxKind::CaretEqualsToken
            | SyntaxKind::AmpersandToken
            | SyntaxKind::AmpersandEqualsToken => {
                if left_type == self.silent_never_type() || right_type == self.silent_never_type() {
                    return Ok(self.silent_never_type());
                }

                let left_type = self.check_non_null_type(left_type, left)?;
                let right_type = self.check_non_null_type(right_type, right)?;

                let mut suggested_operator: Option<SyntaxKind> = None;
                if left_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::BooleanLike)
                    && right_type
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::BooleanLike)
                    && {
                        suggested_operator =
                            self.get_suggested_boolean_operator(operator_token.ref_(self).kind());
                        suggested_operator.is_some()
                    }
                {
                    self.error(
                        Some(
                            error_node.unwrap_or(operator_token)
                        ),
                        &Diagnostics::The_0_operator_is_not_allowed_for_boolean_types_Consider_using_1_instead,
                        Some(vec![
                            token_to_string(operator_token.ref_(self).kind()).unwrap().to_owned(),
                            token_to_string(suggested_operator.unwrap()).unwrap().to_owned(),
                        ])
                    );
                    self.number_type()
                } else {
                    let left_ok = self.check_arithmetic_operand_type(
                        left,
                        left_type,
                        &Diagnostics::The_left_hand_side_of_an_arithmetic_operation_must_be_of_type_any_number_bigint_or_an_enum_type,
                        Some(true)
                    )?;
                    let right_ok = self.check_arithmetic_operand_type(
                        right,
                        right_type,
                        &Diagnostics::The_right_hand_side_of_an_arithmetic_operation_must_be_of_type_any_number_bigint_or_an_enum_type,
                        Some(true)
                    )?;
                    let result_type: Id<Type>;
                    if self.is_type_assignable_to_kind(left_type, TypeFlags::AnyOrUnknown, None)?
                        && self.is_type_assignable_to_kind(
                            right_type,
                            TypeFlags::AnyOrUnknown,
                            None,
                        )?
                        || !(self.maybe_type_of_kind(left_type, TypeFlags::BigIntLike)
                            || self.maybe_type_of_kind(right_type, TypeFlags::BigIntLike))
                    {
                        result_type = self.number_type();
                    } else if self.both_are_big_int_like(left_type, right_type)? {
                        match operator {
                            SyntaxKind::GreaterThanGreaterThanGreaterThanToken
                            | SyntaxKind::GreaterThanGreaterThanGreaterThanEqualsToken => {
                                self.report_operator_error(
                                    error_node,
                                    operator_token,
                                    left_type,
                                    right_type,
                                    Option::<fn(Id<Type>, Id<Type>) -> io::Result<bool>>::None,
                                )?;
                            }
                            SyntaxKind::AsteriskAsteriskToken
                            | SyntaxKind::AsteriskAsteriskEqualsToken => {
                                if self.language_version < ScriptTarget::ES2016 {
                                    self.error(
                                        error_node,
                                        &Diagnostics::Exponentiation_cannot_be_performed_on_bigint_values_unless_the_target_option_is_set_to_es2016_or_later,
                                        None,
                                    );
                                }
                            }
                            _ => (),
                        }
                        result_type = self.bigint_type();
                    } else {
                        self.report_operator_error(
                            error_node,
                            operator_token,
                            left_type,
                            right_type,
                            Some(|type1: Id<Type>, type2: Id<Type>| {
                                self.both_are_big_int_like(type1, type2)
                            }),
                        )?;
                        result_type = self.error_type();
                    }
                    if left_ok && right_ok {
                        self.check_assignment_operator(
                            operator,
                            left,
                            left_type,
                            right,
                            result_type,
                        )?;
                    }
                    result_type
                }
            }
            SyntaxKind::PlusToken | SyntaxKind::PlusEqualsToken => {
                if left_type == self.silent_never_type() || right_type == self.silent_never_type() {
                    return Ok(self.silent_never_type());
                }

                if !self.is_type_assignable_to_kind(left_type, TypeFlags::StringLike, None)?
                    && !self.is_type_assignable_to_kind(right_type, TypeFlags::StringLike, None)?
                {
                    left_type = self.check_non_null_type(left_type, left)?;
                    right_type = self.check_non_null_type(right_type, left)?;
                }

                let mut result_type: Option<Id<Type>> = None;
                if self.is_type_assignable_to_kind(left_type, TypeFlags::NumberLike, Some(true))?
                    && self.is_type_assignable_to_kind(
                        right_type,
                        TypeFlags::NumberLike,
                        Some(true),
                    )?
                {
                    result_type = Some(self.number_type());
                } else if self.is_type_assignable_to_kind(
                    left_type,
                    TypeFlags::BigIntLike,
                    Some(true),
                )? && self.is_type_assignable_to_kind(
                    right_type,
                    TypeFlags::BigIntLike,
                    Some(true),
                )? {
                    result_type = Some(self.bigint_type());
                } else if self.is_type_assignable_to_kind(
                    left_type,
                    TypeFlags::StringLike,
                    Some(true),
                )? || self.is_type_assignable_to_kind(
                    right_type,
                    TypeFlags::StringLike,
                    Some(true),
                )? {
                    result_type = Some(self.string_type());
                } else if self.is_type_any(Some(left_type)) || self.is_type_any(Some(right_type)) {
                    result_type = Some(
                        if self.is_error_type(left_type) || self.is_error_type(right_type) {
                            self.error_type()
                        } else {
                            self.any_type()
                        },
                    );
                }

                if let Some(result_type) = result_type {
                    if !self.check_for_disallowed_es_symbol_operand(
                        left_type, right_type, left, right, operator,
                    ) {
                        return Ok(result_type.clone());
                    }
                }

                if result_type.is_none() {
                    let close_enough_kind = TypeFlags::NumberLike
                        | TypeFlags::BigIntLike
                        | TypeFlags::StringLike
                        | TypeFlags::AnyOrUnknown;
                    self.report_operator_error(
                        error_node,
                        operator_token,
                        left_type,
                        right_type,
                        Some(|left: Id<Type>, right: Id<Type>| {
                            Ok(
                                self.is_type_assignable_to_kind(left, close_enough_kind, None)?
                                    && self.is_type_assignable_to_kind(
                                        right,
                                        close_enough_kind,
                                        None,
                                    )?,
                            )
                        }),
                    )?;
                    return Ok(self.any_type());
                }
                let result_type = result_type.unwrap();

                if operator == SyntaxKind::PlusEqualsToken {
                    self.check_assignment_operator(operator, left, left_type, right, result_type)?;
                }
                result_type
            }
            SyntaxKind::LessThanToken
            | SyntaxKind::GreaterThanToken
            | SyntaxKind::LessThanEqualsToken
            | SyntaxKind::GreaterThanEqualsToken => {
                if self.check_for_disallowed_es_symbol_operand(
                    left_type, right_type, left, right, operator,
                ) {
                    let left_type = self.get_base_type_of_literal_type(
                        self.check_non_null_type(left_type, left)?,
                    )?;
                    let right_type = self.get_base_type_of_literal_type(
                        self.check_non_null_type(right_type, right)?,
                    )?;
                    self.report_operator_error_unless(
                        left_type,
                        right_type,
                        operator_token,
                        error_node,
                        |left: Id<Type>, right: Id<Type>| {
                            Ok(self.is_type_comparable_to(left, right)?
                                || self.is_type_comparable_to(right, left)?
                                || self
                                    .is_type_assignable_to(left, self.number_or_big_int_type())?
                                    && self.is_type_assignable_to(
                                        right,
                                        self.number_or_big_int_type(),
                                    )?)
                        },
                    )?;
                }
                self.boolean_type()
            }
            SyntaxKind::EqualsEqualsToken
            | SyntaxKind::ExclamationEqualsToken
            | SyntaxKind::EqualsEqualsEqualsToken
            | SyntaxKind::ExclamationEqualsEqualsToken => {
                self.report_operator_error_unless(
                    left_type,
                    right_type,
                    operator_token,
                    error_node,
                    |left: Id<Type>, right: Id<Type>| {
                        Ok(self.is_type_equality_comparable_to(left, right)?
                            || self.is_type_equality_comparable_to(right, left)?)
                    },
                )?;
                self.boolean_type()
            }

            SyntaxKind::InstanceOfKeyword => {
                self.check_instance_of_expression(left, right, left_type, right_type)?
            }
            SyntaxKind::InKeyword => {
                self.check_in_expression(left, right, left_type, right_type)?
            }
            SyntaxKind::AmpersandAmpersandToken | SyntaxKind::AmpersandAmpersandEqualsToken => {
                let result_type = if self
                    .get_type_facts(left_type, None)?
                    .intersects(TypeFacts::Truthy)
                {
                    self.get_union_type(
                        &[
                            self.extract_definitely_falsy_types(if self.strict_null_checks {
                                left_type
                            } else {
                                self.get_base_type_of_literal_type(right_type)?
                            }),
                            right_type,
                        ],
                        None,
                        Option::<Id<Symbol>>::None,
                        None,
                        None,
                    )?
                } else {
                    left_type
                };
                if operator == SyntaxKind::AmpersandAmpersandEqualsToken {
                    self.check_assignment_operator(operator, left, left_type, right, right_type)?;
                }
                result_type
            }
            SyntaxKind::BarBarToken | SyntaxKind::BarBarEqualsToken => {
                let result_type = if self
                    .get_type_facts(left_type, None)?
                    .intersects(TypeFacts::Falsy)
                {
                    self.get_union_type(
                        &[self.remove_definitely_falsy_types(left_type), right_type],
                        Some(UnionReduction::Subtype),
                        Option::<Id<Symbol>>::None,
                        None,
                        None,
                    )?
                } else {
                    left_type
                };
                if operator == SyntaxKind::BarBarEqualsToken {
                    self.check_assignment_operator(operator, left, left_type, right, right_type)?;
                }
                result_type
            }
            SyntaxKind::QuestionQuestionToken | SyntaxKind::QuestionQuestionEqualsToken => {
                let result_type = if self
                    .get_type_facts(left_type, None)?
                    .intersects(TypeFacts::EQUndefinedOrNull)
                {
                    self.get_union_type(
                        &[self.get_non_nullable_type(left_type)?, right_type],
                        Some(UnionReduction::Subtype),
                        Option::<Id<Symbol>>::None,
                        None,
                        None,
                    )?
                } else {
                    left_type
                };
                if operator == SyntaxKind::QuestionQuestionEqualsToken {
                    self.check_assignment_operator(operator, left, left_type, right, right_type)?;
                }
                result_type
            }
            SyntaxKind::EqualsToken => {
                let decl_kind = if is_binary_expression(&left.ref_(self).parent().ref_(self)) {
                    get_assignment_declaration_kind(left.ref_(self).parent(), self)
                } else {
                    AssignmentDeclarationKind::None
                };
                self.check_assignment_declaration(decl_kind, right_type)?;
                if self.is_assignment_declaration(left, right, decl_kind)? {
                    if !right_type.ref_(self).flags().intersects(TypeFlags::Object)
                        || !matches!(
                            decl_kind,
                            AssignmentDeclarationKind::ModuleExports
                                | AssignmentDeclarationKind::Prototype
                        ) && !self.is_empty_object_type(right_type)?
                            && !self.is_function_object_type(right_type)?
                            && !get_object_flags(&right_type.ref_(self))
                                .intersects(ObjectFlags::Class)
                    {
                        self.check_assignment_operator(
                            operator, left, left_type, right, right_type,
                        )?;
                    }
                    left_type
                } else {
                    self.check_assignment_operator(operator, left, left_type, right, right_type)?;
                    self.get_regular_type_of_object_literal(right_type)?
                }
            }
            SyntaxKind::CommaToken => {
                if self.compiler_options.ref_(self).allow_unreachable_code != Some(true)
                    && self.is_side_effect_free(left)
                    && !self.is_eval_node(right)
                {
                    let sf = get_source_file_of_node(left, self);
                    let sf_ref = sf.ref_(self);
                    let sf_as_source_file = sf_ref.as_source_file();
                    let source_text = sf_as_source_file.text_as_chars();
                    let start = skip_trivia(&source_text, left.ref_(self).pos(), None, None, None);
                    let is_in_diag_2657 = sf_as_source_file
                        .parse_diagnostics()
                        .ref_(self)
                        .iter()
                        .any(|diag| {
                            if diag.ref_(self).code()
                                != Diagnostics::JSX_expressions_must_have_one_parent_element.code
                            {
                                return false;
                            }
                            text_span_contains_position(
                                &TextSpan {
                                    start: diag.ref_(self).start(),
                                    length: diag.ref_(self).length(),
                                },
                                start,
                            )
                        });
                    if !is_in_diag_2657 {
                        self.error(
                            Some(left),
                            &Diagnostics::Left_side_of_comma_operator_is_unused_and_has_no_side_effects,
                            None,
                        );
                    }
                }
                right_type
            }

            _ => Debug_.fail(None),
        })
    }
}

#[derive(Debug)]
pub struct CheckBinaryExpression {
    trampoline: BinaryExpressionTrampoline<CheckBinaryExpressionStateMachine>,
}

impl CheckBinaryExpression {
    pub fn new(trampoline: BinaryExpressionTrampoline<CheckBinaryExpressionStateMachine>) -> Self {
        Self { trampoline }
    }

    pub fn call(
        &self,
        node: Id<Node>, /*BinaryExpression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let result = self.trampoline.call(node, check_mode)?;
        Debug_.assert_is_defined(&result, None);
        Ok(result.unwrap())
    }
}

pub struct WorkArea {
    pub check_mode: Option<CheckMode>,
    pub skip: bool,
    pub stack_index: isize,
    pub type_stack: Vec<Option<Id<Type>>>,
}

#[derive(Debug)]
pub struct CheckBinaryExpressionStateMachine {
    arena: *const AllArenas,
    type_checker: Id<TypeChecker>,
}

impl CheckBinaryExpressionStateMachine {
    pub fn new(type_checker: Id<TypeChecker>, arena: &impl HasArena) -> Self {
        Self {
            type_checker,
            arena: arena.arena(),
        }
    }

    pub fn maybe_check_expression(
        &self,
        state: Rc<RefCell<WorkArea>>,
        node: Id<Node>, /*Expression*/
    ) -> io::Result<Option<Id<Node /*BinaryExpression*/>>> {
        if is_binary_expression(&node.ref_(self)) {
            return Ok(Some(node));
        }
        let type_ = self.type_checker.ref_(self).check_expression(
            node,
            (*state).borrow().check_mode,
            None,
        )?;
        self.set_last_result(&mut state.borrow_mut(), Some(type_));
        Ok(None)
    }

    pub fn get_left_type(&self, state: Rc<RefCell<WorkArea>>) -> Option<Id<Type>> {
        let state = (*state).borrow();
        state
            .type_stack
            .get(TryInto::<usize>::try_into(state.stack_index).unwrap())
            .cloned()
            .flatten()
    }

    pub fn set_left_type(&self, state: &mut WorkArea, type_: Option<Id<Type>>) {
        push_or_replace(
            &mut state.type_stack,
            TryInto::<usize>::try_into(state.stack_index).unwrap(),
            type_,
        );
    }

    pub fn get_last_result(&self, state: Rc<RefCell<WorkArea>>) -> Option<Id<Type>> {
        let state = (*state).borrow();
        state
            .type_stack
            .get(TryInto::<usize>::try_into(state.stack_index).unwrap() + 1)
            .cloned()
            .flatten()
    }

    pub fn set_last_result(&self, state: &mut WorkArea, type_: Option<Id<Type>>) {
        push_or_replace(
            &mut state.type_stack,
            TryInto::<usize>::try_into(state.stack_index).unwrap() + 1,
            type_,
        );
    }
}

impl BinaryExpressionStateMachine for CheckBinaryExpressionStateMachine {
    type TResult = Option<Id<Type>>;
    type TOuterState = Option<CheckMode>;
    type TState = Rc<RefCell<WorkArea>>;

    fn on_enter(
        &self,
        node: Id<Node>, /*BinaryExpression*/
        mut state: Option<Rc<RefCell<WorkArea>>>,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Rc<RefCell<WorkArea>>> {
        if let Some(state) = state.as_ref() {
            let mut state = state.borrow_mut();
            state.stack_index += 1;
            state.skip = false;
            self.set_left_type(&mut state, None);
            self.set_last_result(&mut state, None);
        } else {
            state = Some(Rc::new(RefCell::new(WorkArea {
                check_mode,
                skip: false,
                stack_index: 0,
                type_stack: vec![None, None],
            })));
        }
        let state = state.unwrap();

        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        if is_in_js_file(Some(&node.ref_(self)))
            && get_assigned_expando_initializer(Some(node), self).is_some()
        {
            {
                let mut state = state.borrow_mut();
                state.skip = true;
                self.set_last_result(
                    &mut state,
                    Some(self.type_checker.ref_(self).check_expression(
                        node_as_binary_expression.right,
                        check_mode,
                        None,
                    )?),
                );
            }
            return Ok(state);
        }

        self.type_checker
            .ref_(self)
            .check_grammar_nullish_coalesce_with_logical_expression(node);

        let operator = node_as_binary_expression.operator_token.ref_(self).kind();
        if operator == SyntaxKind::EqualsToken
            && matches!(
                node_as_binary_expression.left.ref_(self).kind(),
                SyntaxKind::ObjectLiteralExpression | SyntaxKind::ArrayLiteralExpression
            )
        {
            {
                let mut state = state.borrow_mut();
                state.skip = true;
                self.set_last_result(
                    &mut state,
                    Some(
                        self.type_checker
                            .ref_(self)
                            .check_destructuring_assignment(
                                node_as_binary_expression.left,
                                self.type_checker.ref_(self).check_expression(
                                    node_as_binary_expression.right,
                                    check_mode,
                                    None,
                                )?,
                                check_mode,
                                Some(
                                    node_as_binary_expression.right.ref_(self).kind()
                                        == SyntaxKind::ThisKeyword,
                                ),
                            )?,
                    ),
                );
            }
            return Ok(state);
        }

        Ok(state)
    }

    fn on_left(
        &self,
        left: Id<Node>, /*Expression*/
        state: Rc<RefCell<WorkArea>>,
        _node: Id<Node>, /*BinaryExpression*/
    ) -> io::Result<Option<Id<Node /*BinaryExpression*/>>> {
        if !(*state).borrow().skip {
            return self.maybe_check_expression(state, left);
        }
        Ok(None)
    }

    fn on_operator(
        &self,
        operator_token: Id<Node>, /*BinaryOperatorToken*/
        state: Rc<RefCell<WorkArea>>,
        node: Id<Node>, /*BinaryExpression*/
    ) -> io::Result<()> {
        if !(*state).borrow().skip {
            let left_type = self.get_last_result(state.clone());
            Debug_.assert_is_defined(&left_type, None);
            let left_type = left_type.unwrap();
            self.set_left_type(&mut state.borrow_mut(), Some(left_type));
            self.set_last_result(&mut state.borrow_mut(), None);
            let operator = operator_token.ref_(self).kind();
            if matches!(
                operator,
                SyntaxKind::AmpersandAmpersandToken
                    | SyntaxKind::BarBarToken
                    | SyntaxKind::QuestionQuestionToken
            ) {
                let node_ref = node.ref_(self);
                let node_as_binary_expression = node_ref.as_binary_expression();
                if operator == SyntaxKind::AmpersandAmpersandToken {
                    let parent =
                        walk_up_parenthesized_expressions(node.ref_(self).parent(), self).unwrap();
                    self.type_checker
                        .ref_(self)
                        .check_testing_known_truthy_callable_or_awaitable_type(
                            node_as_binary_expression.left,
                            left_type,
                            if is_if_statement(&parent.ref_(self)) {
                                Some(parent.ref_(self).as_if_statement().then_statement)
                            } else {
                                None
                            },
                        )?;
                }
                self.type_checker
                    .ref_(self)
                    .check_truthiness_of_type(left_type, node_as_binary_expression.left);
            }
        }

        Ok(())
    }

    fn on_right(
        &self,
        right: Id<Node>, /*Expression*/
        state: Rc<RefCell<WorkArea>>,
        _node: Id<Node>, /*BinaryExpression*/
    ) -> io::Result<Option<Id<Node /*BinaryExpression*/>>> {
        if !(*state).borrow().skip {
            return self.maybe_check_expression(state, right);
        }
        Ok(None)
    }

    fn on_exit(
        &self,
        node: Id<Node>, /*BinaryExpression*/
        state: Rc<RefCell<WorkArea>>,
    ) -> io::Result<Option<Id<Type>>> {
        let result: Option<Id<Type>>;
        if (*state).borrow().skip {
            result = self.get_last_result(state.clone());
        } else {
            let left_type = self.get_left_type(state.clone());
            Debug_.assert_is_defined(&left_type, None);
            let left_type = left_type.unwrap();

            let right_type = self.get_last_result(state.clone());
            Debug_.assert_is_defined(&right_type, None);
            let right_type = right_type.unwrap();

            result = Some(
                self.type_checker
                    .ref_(self)
                    .check_binary_like_expression_worker(
                        released!(node.ref_(self).as_binary_expression().left),
                        released!(node.ref_(self).as_binary_expression().operator_token),
                        released!(node.ref_(self).as_binary_expression().right),
                        left_type,
                        right_type,
                        Some(node),
                    )?,
            );
        }

        {
            let mut state = state.borrow_mut();
            state.skip = false;
            self.set_left_type(&mut state, None);
            self.set_last_result(&mut state, None);
            state.stack_index -= 1;
        }
        Ok(result)
    }

    fn fold_state(
        &self,
        state: Rc<RefCell<WorkArea>>,
        result: Option<Id<Type>>,
        _side: LeftOrRight,
    ) -> Rc<RefCell<WorkArea>> {
        self.set_last_result(&mut state.borrow_mut(), result);
        state
    }

    fn implements_on_left(&self) -> bool {
        true
    }

    fn implements_on_operator(&self) -> bool {
        true
    }

    fn implements_on_right(&self) -> bool {
        true
    }

    fn implements_fold_state(&self) -> bool {
        true
    }
}

impl_has_arena!(CheckBinaryExpressionStateMachine);
