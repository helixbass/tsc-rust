use gc::{Finalize, Gc, Trace};
use std::cell::RefCell;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;
use std::{borrow::Borrow, io};

use super::{CheckMode, IterationUse, TypeFacts};
use crate::{
    create_binary_expression_trampoline, get_assigned_expando_initializer,
    get_assignment_declaration_kind, get_object_flags, get_source_file_of_node,
    is_assignment_operator, is_binary_expression, is_if_statement, is_in_js_file,
    is_private_identifier_property_access_expression, is_spread_assignment, push_or_replace,
    skip_parentheses, skip_trivia, text_span_contains_position, token_to_string,
    walk_up_parenthesized_expressions, AccessFlags, AssignmentDeclarationKind,
    BinaryExpressionStateMachine, BinaryExpressionTrampoline, Debug_,
    DiagnosticRelatedInformationInterface, Diagnostics, ExternalEmitHelpers, LeftOrRight,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, Number, ObjectFlags, OptionTry,
    ReadonlyTextRange, ScriptTarget, SourceFileLike, Symbol, SyntaxKind, TextSpan, Type,
    TypeChecker, TypeFlags, TypeInterface, UnionReduction,
};

impl TypeChecker {
    pub(super) fn check_object_literal_assignment(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        source_type: &Type,
        right_is_this: Option<bool>,
    ) -> io::Result<Gc<Type>> {
        let properties = &node.as_object_literal_expression().properties;
        if self.strict_null_checks && properties.is_empty() {
            return self.check_non_null_type(source_type, node);
        }
        for i in 0..properties.len() {
            self.check_object_literal_destructuring_property_assignment(
                node,
                source_type,
                i,
                Some(properties),
                right_is_this,
            )?;
        }
        Ok(source_type.type_wrapper())
    }

    pub(super) fn check_object_literal_destructuring_property_assignment(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        object_literal_type: &Type,
        property_index: usize,
        all_properties: Option<&NodeArray /*<ObjectLiteralElementLike>*/>,
        right_is_this: Option<bool>,
    ) -> io::Result<Option<Gc<Type>>> {
        let right_is_this = right_is_this.unwrap_or(false);
        let properties = &node.as_object_literal_expression().properties;
        let property = &properties[property_index];
        Ok(match property.kind() {
            SyntaxKind::PropertyAssignment | SyntaxKind::ShorthandPropertyAssignment => {
                let name = property.as_named_declaration().name();
                let expr_type = self.get_literal_type_from_property_name(&name)?;
                if self.is_type_usable_as_property_name(&expr_type) {
                    let text = self.get_property_name_from_type(&expr_type);
                    let prop = self.get_property_of_type_(object_literal_type, &text, None)?;
                    if let Some(prop) = prop.as_ref() {
                        self.mark_property_as_referenced(prop, Some(&**property), right_is_this);
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
                    &expr_type,
                    Some(AccessFlags::ExpressionPosition),
                    Some(&*name),
                    Option::<&Symbol>::None,
                    None,
                )?;
                let type_ = self.get_flow_type_of_destructuring(property, &element_type)?;
                Some(self.check_destructuring_assignment(
                    &*if property.kind() == SyntaxKind::ShorthandPropertyAssignment {
                        property.clone()
                    } else {
                        property.as_has_initializer().maybe_initializer().unwrap()
                    },
                    &type_,
                    None,
                    None,
                )?)
            }
            SyntaxKind::SpreadAssignment => {
                if property_index < properties.len() - 1 {
                    self.error(
                        Some(&**property),
                        &Diagnostics::A_rest_element_must_be_last_in_a_destructuring_pattern,
                        None,
                    );
                    None
                } else {
                    if self.language_version < ScriptTarget::ESNext {
                        self.check_external_emit_helpers(property, ExternalEmitHelpers::Rest)?;
                    }
                    let mut non_rest_names: Vec<Gc<Node /*PropertyName*/>> = vec![];
                    if let Some(all_properties) = all_properties {
                        for other_property in all_properties {
                            if !is_spread_assignment(other_property) {
                                non_rest_names.push(other_property.as_named_declaration().name());
                            }
                        }
                    }
                    let type_ = self.get_rest_type(
                        object_literal_type,
                        &non_rest_names,
                        object_literal_type.maybe_symbol(),
                    )?;
                    self.check_grammar_for_disallowed_trailing_comma(
                        all_properties,
                        Some(&Diagnostics::A_rest_parameter_or_binding_pattern_may_not_have_a_trailing_comma)
                    );
                    Some(self.check_destructuring_assignment(
                        &property.as_has_expression().expression(),
                        &type_,
                        None,
                        None,
                    )?)
                }
            }
            _ => {
                self.error(
                    Some(&**property),
                    &Diagnostics::Property_assignment_expected,
                    None,
                );
                None
            }
        })
    }

    pub(super) fn check_array_literal_assignment(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        source_type: &Type,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Gc<Type>> {
        let node_as_array_literal_expression = node.as_array_literal_expression();
        let elements = &node_as_array_literal_expression.elements;
        if self.language_version < ScriptTarget::ES2015
            && self.compiler_options.downlevel_iteration == Some(true)
        {
            self.check_external_emit_helpers(node, ExternalEmitHelpers::Read)?;
        }
        let possibly_out_of_bounds_type = self.check_iterated_type_or_element_type(
            IterationUse::Destructuring | IterationUse::PossiblyOutOfBounds,
            source_type,
            &self.undefined_type(),
            Some(node)
        )? /*|| errorType*/;
        let mut in_bounds_type = if self.compiler_options.no_unchecked_indexed_access == Some(true)
        {
            None
        } else {
            Some(possibly_out_of_bounds_type.clone())
        };
        for i in 0..elements.len() {
            let mut type_ = possibly_out_of_bounds_type.clone();
            if node_as_array_literal_expression.elements[i].kind() == SyntaxKind::SpreadElement {
                in_bounds_type = Some(in_bounds_type.try_unwrap_or_else(|| {
                    self.check_iterated_type_or_element_type(
                        IterationUse::Destructuring,
                        source_type,
                        &self.undefined_type(),
                        Some(node),
                    ) /*|| errorType*/
                })?);
                type_ = in_bounds_type.clone().unwrap();
            }
            self.check_array_literal_destructuring_element_assignment(
                node,
                source_type,
                i,
                &type_,
                check_mode,
            )?;
        }
        Ok(source_type.type_wrapper())
    }

    pub(super) fn check_array_literal_destructuring_element_assignment(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        source_type: &Type,
        element_index: usize,
        element_type: &Type,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Option<Gc<Type>>> {
        let node_as_array_literal_expression = node.as_array_literal_expression();
        let elements = &node_as_array_literal_expression.elements;
        let element = &elements[element_index];
        if element.kind() != SyntaxKind::OmittedExpression {
            if element.kind() != SyntaxKind::SpreadElement {
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
                            &index_type,
                            Some(access_flags),
                            Some(self.create_synthetic_expression(
                                element,
                                &index_type,
                                None,
                                Option::<&Node>::None,
                            )),
                            Option::<&Symbol>::None,
                            None,
                        )?
                        .unwrap_or_else(|| self.error_type());
                    let assigned_type = if self.has_default_value(element) {
                        self.get_type_with_facts(&element_type, TypeFacts::NEUndefined)?
                    } else {
                        element_type.clone()
                    };
                    let type_ = self.get_flow_type_of_destructuring(element, &assigned_type)?;
                    return Ok(Some(self.check_destructuring_assignment(
                        element, &type_, check_mode, None,
                    )?));
                }
                return Ok(Some(self.check_destructuring_assignment(
                    element,
                    element_type,
                    check_mode,
                    None,
                )?));
            }
            if element_index < elements.len() - 1 {
                self.error(
                    Some(&**element),
                    &Diagnostics::A_rest_element_must_be_last_in_a_destructuring_pattern,
                    None,
                );
            } else {
                let rest_expression = &element.as_spread_element().expression;
                if rest_expression.kind() == SyntaxKind::BinaryExpression
                    && rest_expression.as_binary_expression().operator_token.kind()
                        == SyntaxKind::EqualsToken
                {
                    self.error(
                        Some(&*rest_expression.as_binary_expression().operator_token),
                        &Diagnostics::A_rest_element_cannot_have_an_initializer,
                        None,
                    );
                } else {
                    self.check_grammar_for_disallowed_trailing_comma(
                        Some(&node_as_array_literal_expression.elements),
                        Some(&Diagnostics::A_rest_parameter_or_binding_pattern_may_not_have_a_trailing_comma)
                    );
                    let type_ =
                        if self.every_type(source_type, |type_: &Type| self.is_tuple_type(type_)) {
                            self.try_map_type(
                                source_type,
                                &mut |t: &Type| -> io::Result<_> {
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
                        &type_,
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
        expr_or_assignment: &Node, /*Expression | ShorthandPropertyAssignment*/
        source_type: &Type,
        check_mode: Option<CheckMode>,
        right_is_this: Option<bool>,
    ) -> io::Result<Gc<Type>> {
        let mut target: Gc<Node>;
        let mut source_type = source_type.type_wrapper();
        if expr_or_assignment.kind() == SyntaxKind::ShorthandPropertyAssignment {
            let prop = expr_or_assignment.as_shorthand_property_assignment();
            if let Some(prop_object_assignment_initializer) =
                prop.object_assignment_initializer.as_ref()
            {
                if self.strict_null_checks
                    && !self
                        .get_falsy_flags(&*self.check_expression(
                            prop_object_assignment_initializer,
                            None,
                            None,
                        )?)
                        .intersects(TypeFlags::Undefined)
                {
                    source_type = self.get_type_with_facts(&source_type, TypeFacts::NEUndefined)?;
                }
                self.check_binary_like_expression(
                    &prop.name(),
                    prop.equals_token.as_ref().unwrap(),
                    prop_object_assignment_initializer,
                    check_mode,
                    Option::<&Node>::None,
                )?;
            }
            target = expr_or_assignment.as_shorthand_property_assignment().name();
        } else {
            target = expr_or_assignment.node_wrapper();
        }

        if target.kind() == SyntaxKind::BinaryExpression
            && target.as_binary_expression().operator_token.kind() == SyntaxKind::EqualsToken
        {
            self.check_binary_expression().call(&target, check_mode)?;
            target = target.as_binary_expression().left.clone();
        }
        if target.kind() == SyntaxKind::ObjectLiteralExpression {
            return self.check_object_literal_assignment(&target, &source_type, right_is_this);
        }
        if target.kind() == SyntaxKind::ArrayLiteralExpression {
            return self.check_array_literal_assignment(&target, &source_type, check_mode);
        }
        self.check_reference_assignment(&target, &source_type, check_mode)
    }

    pub(super) fn check_reference_assignment(
        &self,
        target: &Node, /*Expression*/
        source_type: &Type,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Gc<Type>> {
        let target_type = self.check_expression(target, check_mode, None)?;
        let error = if target.parent().kind() == SyntaxKind::SpreadAssignment {
            &*Diagnostics::The_target_of_an_object_rest_assignment_must_be_a_variable_or_a_property_access
        } else {
            &*Diagnostics::The_left_hand_side_of_an_assignment_expression_must_be_a_variable_or_a_property_access
        };
        let optional_error = if target.parent().kind() == SyntaxKind::SpreadAssignment {
            &*Diagnostics::The_target_of_an_object_rest_assignment_may_not_be_an_optional_property_access
        } else {
            &*Diagnostics::The_left_hand_side_of_an_assignment_expression_may_not_be_an_optional_property_access
        };
        if self.check_reference_expression(target, error, optional_error) {
            self.check_type_assignable_to_and_optionally_elaborate(
                source_type,
                &target_type,
                Some(target),
                Some(target),
                None,
                None,
            )?;
        }
        if is_private_identifier_property_access_expression(target) {
            self.check_external_emit_helpers(
                &target.parent(),
                ExternalEmitHelpers::ClassPrivateFieldSet,
            )?;
        }
        Ok(source_type.type_wrapper())
    }

    pub(super) fn is_side_effect_free(&self, node: &Node) -> bool {
        let node = skip_parentheses(node, None);
        match node.kind() {
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
                let node_as_conditional_expression = node.as_conditional_expression();
                self.is_side_effect_free(&node_as_conditional_expression.when_true)
                    && self.is_side_effect_free(&node_as_conditional_expression.when_false)
            }

            SyntaxKind::BinaryExpression => {
                let node_as_binary_expression = node.as_binary_expression();
                if is_assignment_operator(node_as_binary_expression.operator_token.kind()) {
                    return false;
                }
                self.is_side_effect_free(&node_as_binary_expression.left)
                    && self.is_side_effect_free(&node_as_binary_expression.right)
            }

            SyntaxKind::PrefixUnaryExpression | SyntaxKind::PostfixUnaryExpression => {
                matches!(
                    node.as_unary_expression().operator(),
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
        source: &Type,
        target: &Type,
    ) -> io::Result<bool> {
        Ok(target.flags().intersects(TypeFlags::Nullable)
            || self.is_type_comparable_to(source, target)?)
    }

    pub(super) fn create_check_binary_expression(&self) -> CheckBinaryExpression {
        let trampoline = create_binary_expression_trampoline(
            CheckBinaryExpressionStateMachine::new(self.rc_wrapper()),
        );
        CheckBinaryExpression::new(trampoline)
    }

    pub(super) fn check_grammar_nullish_coalesce_with_logical_expression(
        &self,
        node: &Node, /*BinaryExpression*/
    ) {
        let node_as_binary_expression = node.as_binary_expression();
        let left = &node_as_binary_expression.left;
        let operator_token = &node_as_binary_expression.operator_token;
        let right = &node_as_binary_expression.right;
        if operator_token.kind() == SyntaxKind::QuestionQuestionToken {
            if is_binary_expression(left)
                && matches!(
                    left.as_binary_expression().operator_token.kind(),
                    SyntaxKind::BarBarToken | SyntaxKind::AmpersandAmpersandToken
                )
            {
                self.grammar_error_on_node(
                    left,
                    &Diagnostics::_0_and_1_operations_cannot_be_mixed_without_parentheses,
                    Some(vec![
                        token_to_string(left.as_binary_expression().operator_token.kind())
                            .unwrap()
                            .to_owned(),
                        token_to_string(operator_token.kind()).unwrap().to_owned(),
                    ]),
                );
            }
            if is_binary_expression(right)
                && matches!(
                    right.as_binary_expression().operator_token.kind(),
                    SyntaxKind::BarBarToken | SyntaxKind::AmpersandAmpersandToken
                )
            {
                self.grammar_error_on_node(
                    right,
                    &Diagnostics::_0_and_1_operations_cannot_be_mixed_without_parentheses,
                    Some(vec![
                        token_to_string(right.as_binary_expression().operator_token.kind())
                            .unwrap()
                            .to_owned(),
                        token_to_string(operator_token.kind()).unwrap().to_owned(),
                    ]),
                );
            }
        }
    }

    pub(super) fn check_binary_like_expression(
        &self,
        left: &Node, /*Expression*/
        operator_token: &Node,
        right: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
        error_node: Option<impl Borrow<Node>>,
    ) -> io::Result<Gc<Type>> {
        let operator = operator_token.kind();
        if operator == SyntaxKind::EqualsToken
            && matches!(
                left.kind(),
                SyntaxKind::ObjectLiteralExpression | SyntaxKind::ArrayLiteralExpression
            )
        {
            return self.check_destructuring_assignment(
                left,
                &*self.check_expression(right, check_mode, None)?,
                check_mode,
                Some(right.kind() == SyntaxKind::ThisKeyword),
            );
        }
        let left_type: Gc<Type>;
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
            &left_type,
            &right_type,
            error_node,
        )
    }

    pub(super) fn check_binary_like_expression_worker(
        &self,
        left: &Node, /*Expression*/
        operator_token: &Node,
        right: &Node, /*Expression*/
        left_type: &Type,
        right_type: &Type,
        error_node: Option<impl Borrow<Node>>,
    ) -> io::Result<Gc<Type>> {
        let operator = operator_token.kind();
        let error_node = error_node.map(|error_node| error_node.borrow().node_wrapper());
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
                if ptr::eq(left_type, &*self.silent_never_type())
                    || ptr::eq(right_type, &*self.silent_never_type())
                {
                    return Ok(self.silent_never_type());
                }

                let left_type = self.check_non_null_type(left_type, left)?;
                let right_type = self.check_non_null_type(right_type, right)?;

                let mut suggested_operator: Option<SyntaxKind> = None;
                if left_type.flags().intersects(TypeFlags::BooleanLike)
                    && right_type.flags().intersects(TypeFlags::BooleanLike)
                    && {
                        suggested_operator =
                            self.get_suggested_boolean_operator(operator_token.kind());
                        suggested_operator.is_some()
                    }
                {
                    self.error(
                        Some(
                            &*error_node.clone().unwrap_or_else(|| operator_token.node_wrapper())
                        ),
                        &Diagnostics::The_0_operator_is_not_allowed_for_boolean_types_Consider_using_1_instead,
                        Some(vec![
                            token_to_string(operator_token.kind()).unwrap().to_owned(),
                            token_to_string(suggested_operator.unwrap()).unwrap().to_owned(),
                        ])
                    );
                    self.number_type()
                } else {
                    let left_ok = self.check_arithmetic_operand_type(
                        left,
                        &left_type,
                        &Diagnostics::The_left_hand_side_of_an_arithmetic_operation_must_be_of_type_any_number_bigint_or_an_enum_type,
                        Some(true)
                    )?;
                    let right_ok = self.check_arithmetic_operand_type(
                        right,
                        &right_type,
                        &Diagnostics::The_right_hand_side_of_an_arithmetic_operation_must_be_of_type_any_number_bigint_or_an_enum_type,
                        Some(true)
                    )?;
                    let result_type: Gc<Type>;
                    if self.is_type_assignable_to_kind(&left_type, TypeFlags::AnyOrUnknown, None)?
                        && self.is_type_assignable_to_kind(
                            &right_type,
                            TypeFlags::AnyOrUnknown,
                            None,
                        )?
                        || !(self.maybe_type_of_kind(&left_type, TypeFlags::BigIntLike)
                            || self.maybe_type_of_kind(&right_type, TypeFlags::BigIntLike))
                    {
                        result_type = self.number_type();
                    } else if self.both_are_big_int_like(&left_type, &right_type)? {
                        match operator {
                            SyntaxKind::GreaterThanGreaterThanGreaterThanToken
                            | SyntaxKind::GreaterThanGreaterThanGreaterThanEqualsToken => {
                                self.report_operator_error(
                                    error_node.as_deref(),
                                    operator_token,
                                    &left_type,
                                    &right_type,
                                    Option::<fn(&Type, &Type) -> io::Result<bool>>::None,
                                )?;
                            }
                            SyntaxKind::AsteriskAsteriskToken
                            | SyntaxKind::AsteriskAsteriskEqualsToken => {
                                if self.language_version < ScriptTarget::ES2016 {
                                    self.error(
                                        error_node.as_deref(),
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
                            error_node.as_deref(),
                            operator_token,
                            &left_type,
                            &right_type,
                            Some(|type1: &Type, type2: &Type| {
                                self.both_are_big_int_like(type1, type2)
                            }),
                        )?;
                        result_type = self.error_type();
                    }
                    if left_ok && right_ok {
                        self.check_assignment_operator(
                            operator,
                            left,
                            &left_type,
                            right,
                            &result_type,
                        )?;
                    }
                    result_type
                }
            }
            SyntaxKind::PlusToken | SyntaxKind::PlusEqualsToken => {
                if ptr::eq(left_type, &*self.silent_never_type())
                    || ptr::eq(right_type, &*self.silent_never_type())
                {
                    return Ok(self.silent_never_type());
                }

                let mut left_type = left_type.type_wrapper();
                let mut right_type = right_type.type_wrapper();
                if !self.is_type_assignable_to_kind(&left_type, TypeFlags::StringLike, None)?
                    && !self.is_type_assignable_to_kind(&right_type, TypeFlags::StringLike, None)?
                {
                    left_type = self.check_non_null_type(&left_type, left)?;
                    right_type = self.check_non_null_type(&right_type, left)?;
                }

                let mut result_type: Option<Gc<Type>> = None;
                if self.is_type_assignable_to_kind(&left_type, TypeFlags::NumberLike, Some(true))?
                    && self.is_type_assignable_to_kind(
                        &right_type,
                        TypeFlags::NumberLike,
                        Some(true),
                    )?
                {
                    result_type = Some(self.number_type());
                } else if self.is_type_assignable_to_kind(
                    &left_type,
                    TypeFlags::BigIntLike,
                    Some(true),
                )? && self.is_type_assignable_to_kind(
                    &right_type,
                    TypeFlags::BigIntLike,
                    Some(true),
                )? {
                    result_type = Some(self.bigint_type());
                } else if self.is_type_assignable_to_kind(
                    &left_type,
                    TypeFlags::StringLike,
                    Some(true),
                )? || self.is_type_assignable_to_kind(
                    &right_type,
                    TypeFlags::StringLike,
                    Some(true),
                )? {
                    result_type = Some(self.string_type());
                } else if self.is_type_any(Some(&*left_type))
                    || self.is_type_any(Some(&*right_type))
                {
                    result_type = Some(
                        if self.is_error_type(&left_type) || self.is_error_type(&right_type) {
                            self.error_type()
                        } else {
                            self.any_type()
                        },
                    );
                }

                if let Some(result_type) = result_type.as_ref() {
                    if !self.check_for_disallowed_es_symbol_operand(
                        &left_type,
                        &right_type,
                        left,
                        right,
                        operator,
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
                        error_node.as_deref(),
                        operator_token,
                        &left_type,
                        &right_type,
                        Some(|left: &Type, right: &Type| {
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
                    self.check_assignment_operator(
                        operator,
                        left,
                        &left_type,
                        right,
                        &result_type,
                    )?;
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
                        &*self.check_non_null_type(left_type, left)?,
                    )?;
                    let right_type = self.get_base_type_of_literal_type(
                        &*self.check_non_null_type(right_type, right)?,
                    )?;
                    self.report_operator_error_unless(
                        &left_type,
                        &right_type,
                        operator_token,
                        error_node.as_deref(),
                        |left: &Type, right: &Type| {
                            Ok(self.is_type_comparable_to(left, right)?
                                || self.is_type_comparable_to(right, left)?
                                || self
                                    .is_type_assignable_to(left, &self.number_or_big_int_type())?
                                    && self.is_type_assignable_to(
                                        right,
                                        &self.number_or_big_int_type(),
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
                    error_node.as_deref(),
                    |left: &Type, right: &Type| {
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
                            self.extract_definitely_falsy_types(&*if self.strict_null_checks {
                                left_type.type_wrapper()
                            } else {
                                self.get_base_type_of_literal_type(right_type)?
                            }),
                            right_type.type_wrapper(),
                        ],
                        None,
                        Option::<&Symbol>::None,
                        None,
                        Option::<&Type>::None,
                    )?
                } else {
                    left_type.type_wrapper()
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
                        &[
                            self.remove_definitely_falsy_types(left_type),
                            right_type.type_wrapper(),
                        ],
                        Some(UnionReduction::Subtype),
                        Option::<&Symbol>::None,
                        None,
                        Option::<&Type>::None,
                    )?
                } else {
                    left_type.type_wrapper()
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
                        &[
                            self.get_non_nullable_type(left_type)?,
                            right_type.type_wrapper(),
                        ],
                        Some(UnionReduction::Subtype),
                        Option::<&Symbol>::None,
                        None,
                        Option::<&Type>::None,
                    )?
                } else {
                    left_type.type_wrapper()
                };
                if operator == SyntaxKind::QuestionQuestionEqualsToken {
                    self.check_assignment_operator(operator, left, left_type, right, right_type)?;
                }
                result_type
            }
            SyntaxKind::EqualsToken => {
                let decl_kind = if is_binary_expression(&left.parent()) {
                    get_assignment_declaration_kind(&left.parent())
                } else {
                    AssignmentDeclarationKind::None
                };
                self.check_assignment_declaration(decl_kind, right_type)?;
                if self.is_assignment_declaration(left, right, decl_kind)? {
                    if !right_type.flags().intersects(TypeFlags::Object)
                        || !matches!(
                            decl_kind,
                            AssignmentDeclarationKind::ModuleExports
                                | AssignmentDeclarationKind::Prototype
                        ) && !self.is_empty_object_type(right_type)?
                            && !self.is_function_object_type(right_type)?
                            && !get_object_flags(right_type).intersects(ObjectFlags::Class)
                    {
                        self.check_assignment_operator(
                            operator, left, left_type, right, right_type,
                        )?;
                    }
                    left_type.type_wrapper()
                } else {
                    self.check_assignment_operator(operator, left, left_type, right, right_type)?;
                    self.get_regular_type_of_object_literal(right_type)?
                }
            }
            SyntaxKind::CommaToken => {
                if self.compiler_options.allow_unreachable_code != Some(true)
                    && self.is_side_effect_free(left)
                    && !self.is_eval_node(right)
                {
                    let sf = get_source_file_of_node(left);
                    let sf_as_source_file = sf.as_source_file();
                    let source_text = sf_as_source_file.text_as_chars();
                    let start = skip_trivia(&source_text, left.pos(), None, None, None);
                    let is_in_diag_2657 = (*sf_as_source_file.parse_diagnostics())
                        .borrow()
                        .iter()
                        .any(|diag| {
                            if diag.code()
                                != Diagnostics::JSX_expressions_must_have_one_parent_element.code
                            {
                                return false;
                            }
                            text_span_contains_position(
                                &TextSpan {
                                    start: diag.start(),
                                    length: diag.length(),
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
                right_type.type_wrapper()
            }

            _ => Debug_.fail(None),
        })
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct CheckBinaryExpression {
    trampoline: BinaryExpressionTrampoline<CheckBinaryExpressionStateMachine>,
}

impl CheckBinaryExpression {
    pub fn new(trampoline: BinaryExpressionTrampoline<CheckBinaryExpressionStateMachine>) -> Self {
        Self { trampoline }
    }

    pub fn call(
        &self,
        node: &Node, /*BinaryExpression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Gc<Type>> {
        let result = self.trampoline.call(node, check_mode)?;
        Debug_.assert_is_defined(&result, None);
        Ok(result.unwrap())
    }
}

pub struct WorkArea {
    pub check_mode: Option<CheckMode>,
    pub skip: bool,
    pub stack_index: isize,
    pub type_stack: Vec<Option<Gc<Type>>>,
}

#[derive(Debug, Trace, Finalize)]
pub struct CheckBinaryExpressionStateMachine {
    type_checker: Gc<TypeChecker>,
}

impl CheckBinaryExpressionStateMachine {
    pub fn new(type_checker: Gc<TypeChecker>) -> Self {
        Self { type_checker }
    }

    pub fn maybe_check_expression(
        &self,
        state: Rc<RefCell<WorkArea>>,
        node: &Node, /*Expression*/
    ) -> io::Result<Option<Gc<Node /*BinaryExpression*/>>> {
        if is_binary_expression(node) {
            return Ok(Some(node.node_wrapper()));
        }
        let type_ = self
            .type_checker
            .check_expression(node, (*state).borrow().check_mode, None)?;
        self.set_last_result(&mut state.borrow_mut(), Some(type_));
        Ok(None)
    }

    pub fn get_left_type(&self, state: Rc<RefCell<WorkArea>>) -> Option<Gc<Type>> {
        let state = (*state).borrow();
        state
            .type_stack
            .get(TryInto::<usize>::try_into(state.stack_index).unwrap())
            .cloned()
            .flatten()
    }

    pub fn set_left_type<TType: Borrow<Type>>(&self, state: &mut WorkArea, type_: Option<TType>) {
        push_or_replace(
            &mut state.type_stack,
            TryInto::<usize>::try_into(state.stack_index).unwrap(),
            type_.map(|type_| type_.borrow().type_wrapper()),
        );
    }

    pub fn get_last_result(&self, state: Rc<RefCell<WorkArea>>) -> Option<Gc<Type>> {
        let state = (*state).borrow();
        state
            .type_stack
            .get(TryInto::<usize>::try_into(state.stack_index).unwrap() + 1)
            .cloned()
            .flatten()
    }

    pub fn set_last_result<TType: Borrow<Type>>(&self, state: &mut WorkArea, type_: Option<TType>) {
        push_or_replace(
            &mut state.type_stack,
            TryInto::<usize>::try_into(state.stack_index).unwrap() + 1,
            type_.map(|type_| type_.borrow().type_wrapper()),
        );
    }
}

impl BinaryExpressionStateMachine for CheckBinaryExpressionStateMachine {
    type TResult = Option<Gc<Type>>;
    type TOuterState = Option<CheckMode>;
    type TState = Rc<RefCell<WorkArea>>;

    fn on_enter(
        &self,
        node: &Node, /*BinaryExpression*/
        mut state: Option<Rc<RefCell<WorkArea>>>,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Rc<RefCell<WorkArea>>> {
        if let Some(state) = state.as_ref() {
            let mut state = state.borrow_mut();
            state.stack_index += 1;
            state.skip = false;
            self.set_left_type(&mut state, Option::<&Type>::None);
            self.set_last_result(&mut state, Option::<&Type>::None);
        } else {
            state = Some(Rc::new(RefCell::new(WorkArea {
                check_mode,
                skip: false,
                stack_index: 0,
                type_stack: vec![None, None],
            })));
        }
        let state = state.unwrap();

        let node_as_binary_expression = node.as_binary_expression();
        if is_in_js_file(Some(node)) && get_assigned_expando_initializer(Some(node)).is_some() {
            {
                let mut state = state.borrow_mut();
                state.skip = true;
                self.set_last_result(
                    &mut state,
                    Some(self.type_checker.check_expression(
                        &node_as_binary_expression.right,
                        check_mode,
                        None,
                    )?),
                );
            }
            return Ok(state);
        }

        self.type_checker
            .check_grammar_nullish_coalesce_with_logical_expression(node);

        let operator = node_as_binary_expression.operator_token.kind();
        if operator == SyntaxKind::EqualsToken
            && matches!(
                node_as_binary_expression.left.kind(),
                SyntaxKind::ObjectLiteralExpression | SyntaxKind::ArrayLiteralExpression
            )
        {
            {
                let mut state = state.borrow_mut();
                state.skip = true;
                self.set_last_result(
                    &mut state,
                    Some(self.type_checker.check_destructuring_assignment(
                        &node_as_binary_expression.left,
                        &*self.type_checker.check_expression(
                            &node_as_binary_expression.right,
                            check_mode,
                            None,
                        )?,
                        check_mode,
                        Some(node_as_binary_expression.right.kind() == SyntaxKind::ThisKeyword),
                    )?),
                );
            }
            return Ok(state);
        }

        Ok(state)
    }

    fn on_left(
        &self,
        left: &Node, /*Expression*/
        state: Rc<RefCell<WorkArea>>,
        _node: &Node, /*BinaryExpression*/
    ) -> io::Result<Option<Gc<Node /*BinaryExpression*/>>> {
        if !(*state).borrow().skip {
            return self.maybe_check_expression(state, left);
        }
        Ok(None)
    }

    fn on_operator(
        &self,
        operator_token: &Node, /*BinaryOperatorToken*/
        state: Rc<RefCell<WorkArea>>,
        node: &Node, /*BinaryExpression*/
    ) -> io::Result<()> {
        if !(*state).borrow().skip {
            let left_type = self.get_last_result(state.clone());
            Debug_.assert_is_defined(&left_type, None);
            let left_type = left_type.unwrap();
            self.set_left_type(&mut state.borrow_mut(), Some(&*left_type));
            self.set_last_result(&mut state.borrow_mut(), Option::<&Type>::None);
            let operator = operator_token.kind();
            if matches!(
                operator,
                SyntaxKind::AmpersandAmpersandToken
                    | SyntaxKind::BarBarToken
                    | SyntaxKind::QuestionQuestionToken
            ) {
                let node_as_binary_expression = node.as_binary_expression();
                if operator == SyntaxKind::AmpersandAmpersandToken {
                    let parent = walk_up_parenthesized_expressions(&node.parent()).unwrap();
                    self.type_checker
                        .check_testing_known_truthy_callable_or_awaitable_type(
                            &node_as_binary_expression.left,
                            &left_type,
                            if is_if_statement(&parent) {
                                Some(&*parent.as_if_statement().then_statement)
                            } else {
                                None
                            },
                        )?;
                }
                self.type_checker
                    .check_truthiness_of_type(&left_type, &node_as_binary_expression.left);
            }
        }

        Ok(())
    }

    fn on_right(
        &self,
        right: &Node, /*Expression*/
        state: Rc<RefCell<WorkArea>>,
        _node: &Node, /*BinaryExpression*/
    ) -> io::Result<Option<Gc<Node /*BinaryExpression*/>>> {
        if !(*state).borrow().skip {
            return self.maybe_check_expression(state, right);
        }
        Ok(None)
    }

    fn on_exit(
        &self,
        node: &Node, /*BinaryExpression*/
        state: Rc<RefCell<WorkArea>>,
    ) -> io::Result<Option<Gc<Type>>> {
        let result: Option<Gc<Type>>;
        if (*state).borrow().skip {
            result = self.get_last_result(state.clone());
        } else {
            let left_type = self.get_left_type(state.clone());
            Debug_.assert_is_defined(&left_type, None);
            let left_type = left_type.unwrap();

            let right_type = self.get_last_result(state.clone());
            Debug_.assert_is_defined(&right_type, None);
            let right_type = right_type.unwrap();

            let node_as_binary_expression = node.as_binary_expression();
            result = Some(self.type_checker.check_binary_like_expression_worker(
                &node_as_binary_expression.left,
                &node_as_binary_expression.operator_token,
                &node_as_binary_expression.right,
                &left_type,
                &right_type,
                Some(node),
            )?);
        }

        {
            let mut state = state.borrow_mut();
            state.skip = false;
            self.set_left_type(&mut state, Option::<&Type>::None);
            self.set_last_result(&mut state, Option::<&Type>::None);
            state.stack_index -= 1;
        }
        Ok(result)
    }

    fn fold_state(
        &self,
        state: Rc<RefCell<WorkArea>>,
        result: Option<Gc<Type>>,
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
