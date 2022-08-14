#![allow(non_upper_case_globals)]

use std::ptr;
use std::rc::Rc;

use super::{CheckMode, GetFlowTypeOfReference, TypeFacts};
use crate::{
    are_option_rcs_equal, escape_leading_underscores, find, is_access_expression,
    is_binary_expression, is_call_chain, is_expression_of_optional_chain_root, is_identifier,
    is_property_access_expression, is_string_literal_like, is_variable_declaration, map,
    HasInitializerInterface, HasTypeInterface, Signature, SignatureKind, TypePredicate,
    TypePredicateKind, UnionOrIntersectionTypeInterface, __String, get_object_flags, Node,
    NodeInterface, ObjectFlags, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface,
};

impl GetFlowTypeOfReference {
    pub(super) fn narrow_type_by_constructor(
        &self,
        type_: &Type,
        operator: SyntaxKind,
        identifier: &Node, /*Expression*/
        assume_true: bool,
    ) -> Rc<Type> {
        if if assume_true {
            !matches!(
                operator,
                SyntaxKind::EqualsEqualsToken | SyntaxKind::EqualsEqualsEqualsToken
            )
        } else {
            !matches!(
                operator,
                SyntaxKind::ExclamationEqualsToken | SyntaxKind::ExclamationEqualsEqualsToken
            )
        } {
            return type_.type_wrapper();
        }

        let identifier_type = self.type_checker.get_type_of_expression(identifier);
        if !self.type_checker.is_function_type(&identifier_type)
            && !self.type_checker.is_constructor_type(&identifier_type)
        {
            return type_.type_wrapper();
        }

        let prototype_property = self.type_checker.get_property_of_type_(
            &identifier_type,
            &__String::new("prototype".to_owned()),
            None,
        );
        if prototype_property.is_none() {
            return type_.type_wrapper();
        }
        let prototype_property = prototype_property.unwrap();

        let prototype_type = self.type_checker.get_type_of_symbol(&prototype_property);
        let candidate = if !self.type_checker.is_type_any(Some(&*prototype_type)) {
            Some(prototype_type)
        } else {
            None
        };
        if candidate.is_none() {
            return type_.type_wrapper();
        }
        let candidate = candidate.unwrap();
        if Rc::ptr_eq(&candidate, &self.type_checker.global_object_type())
            || Rc::ptr_eq(&candidate, &self.type_checker.global_function_type())
        {
            return type_.type_wrapper();
        }

        if self.type_checker.is_type_any(Some(type_)) {
            return candidate;
        }

        let is_constructed_by = |source: &Type, target: &Type| {
            if source.flags().intersects(TypeFlags::Object)
                && get_object_flags(source).intersects(ObjectFlags::Class)
                || target.flags().intersects(TypeFlags::Object)
                    && get_object_flags(target).intersects(ObjectFlags::Class)
            {
                return are_option_rcs_equal(
                    source.maybe_symbol().as_ref(),
                    target.maybe_symbol().as_ref(),
                );
            }

            self.type_checker.is_type_subtype_of(source, target)
        };

        self.type_checker
            .filter_type(type_, |t: &Type| is_constructed_by(t, &candidate))
    }

    pub(super) fn narrow_type_by_instanceof(
        &self,
        type_: &Type,
        expr: &Node, /*BinaryExpression*/
        assume_true: bool,
    ) -> Rc<Type> {
        let expr_as_binary_expression = expr.as_binary_expression();
        let left = self
            .type_checker
            .get_reference_candidate(&expr_as_binary_expression.left);
        if !self
            .type_checker
            .is_matching_reference(&self.reference, &left)
        {
            if assume_true
                && self.type_checker.strict_null_checks
                && self
                    .type_checker
                    .optional_chain_contains_reference(&left, &self.reference)
            {
                return self
                    .type_checker
                    .get_type_with_facts(type_, TypeFacts::NEUndefinedOrNull);
            }
            return type_.type_wrapper();
        }

        let right_type = self
            .type_checker
            .get_type_of_expression(&expr_as_binary_expression.right);
        if !self
            .type_checker
            .is_type_derived_from(&right_type, &self.type_checker.global_function_type())
        {
            return type_.type_wrapper();
        }

        let mut target_type: Option<Rc<Type>> = None;
        let prototype_property = self.type_checker.get_property_of_type_(
            &right_type,
            &__String::new("prototype".to_owned()),
            None,
        );
        if let Some(prototype_property) = prototype_property.as_ref() {
            let prototype_property_type = self.type_checker.get_type_of_symbol(prototype_property);
            if !self
                .type_checker
                .is_type_any(Some(&*prototype_property_type))
            {
                target_type = Some(prototype_property_type);
            }
        }

        if self.type_checker.is_type_any(Some(type_))
            && matches!(
                target_type.as_ref(),
                Some(target_type) if Rc::ptr_eq(
                    target_type,
                    &self.type_checker.global_object_type()
                ) || Rc::ptr_eq(
                    target_type,
                    &self.type_checker.global_function_type()
                )
            )
        {
            return type_.type_wrapper();
        }

        if target_type.is_none() {
            let construct_signatures = self
                .type_checker
                .get_signatures_of_type(&right_type, SignatureKind::Construct);
            target_type = Some(if !construct_signatures.is_empty() {
                self.type_checker.get_union_type(
                    map(&construct_signatures, |signature: &Rc<Signature>, _| {
                        self.type_checker.get_return_type_of_signature(
                            self.type_checker.get_erased_signature(signature.clone()),
                        )
                    }),
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                )
            } else {
                self.type_checker.empty_object_type()
            });
        }
        let target_type = target_type.unwrap();

        if !assume_true && right_type.flags().intersects(TypeFlags::Union) {
            let non_constructor_type_in_union =
                find(&right_type.as_union_type().types(), |t: &Rc<Type>, _| {
                    !self.type_checker.is_constructor_type(t)
                })
                .cloned();
            if non_constructor_type_in_union.is_none() {
                return type_.type_wrapper();
            }
        }

        self.get_narrowed_type(
            type_,
            &target_type,
            assume_true,
            |source: &Type, target: &Type| self.type_checker.is_type_derived_from(source, target),
        )
    }

    pub(super) fn get_narrowed_type<TIsRelated: FnMut(&Type, &Type) -> bool>(
        &self,
        type_: &Type,
        candidate: &Type,
        assume_true: bool,
        mut is_related: TIsRelated,
    ) -> Rc<Type> {
        if !assume_true {
            return self
                .type_checker
                .filter_type(type_, |t: &Type| !is_related(t, candidate));
        }
        if type_.flags().intersects(TypeFlags::Union) {
            let assignable_type = self
                .type_checker
                .filter_type(type_, |t: &Type| is_related(t, candidate));
            if !assignable_type.flags().intersects(TypeFlags::Never) {
                return assignable_type;
            }
        }

        if self.type_checker.is_type_subtype_of(candidate, type_) {
            candidate.type_wrapper()
        } else if self.type_checker.is_type_assignable_to(type_, candidate) {
            type_.type_wrapper()
        } else if self.type_checker.is_type_assignable_to(candidate, type_) {
            candidate.type_wrapper()
        } else {
            self.type_checker.get_intersection_type(
                &vec![type_.type_wrapper(), candidate.type_wrapper()],
                Option::<&Symbol>::None,
                None,
            )
        }
    }

    pub(super) fn narrow_type_by_call_expression(
        &self,
        type_: &Type,
        call_expression: &Node, /*CallExpression*/
        assume_true: bool,
    ) -> Rc<Type> {
        if self
            .type_checker
            .has_matching_argument(call_expression, &self.reference)
        {
            let signature = if assume_true || !is_call_chain(call_expression) {
                self.type_checker.get_effects_signature(call_expression)
            } else {
                None
            };
            let predicate = signature
                .as_ref()
                .and_then(|signature| self.type_checker.get_type_predicate_of_signature(signature));
            if let Some(predicate) = predicate.as_ref().filter(|predicate| {
                matches!(
                    predicate.kind,
                    TypePredicateKind::This | TypePredicateKind::Identifier
                )
            }) {
                return self.narrow_type_by_type_predicate(
                    type_,
                    predicate,
                    call_expression,
                    assume_true,
                );
            }
        }
        let call_expression_as_call_expression = call_expression.as_call_expression();
        if self.type_checker.contains_missing_type(type_)
            && is_access_expression(&self.reference)
            && is_property_access_expression(&call_expression_as_call_expression.expression)
        {
            let call_access = &call_expression_as_call_expression.expression;
            let call_access_as_property_access_expression =
                call_access.as_property_access_expression();
            if self.type_checker.is_matching_reference(
                &self.reference.as_has_expression().expression(),
                &self
                    .type_checker
                    .get_reference_candidate(&call_access_as_property_access_expression.expression),
            ) && is_identifier(&call_access_as_property_access_expression.name)
                && call_access_as_property_access_expression
                    .name
                    .as_identifier()
                    .escaped_text
                    .eq_str("hasOwnProperty")
                && call_expression_as_call_expression.arguments.len() == 1
            {
                let argument = &call_expression_as_call_expression.arguments[0];
                if is_string_literal_like(argument)
                    && matches!(
                        self.type_checker.get_accessed_property_name(&self.reference).as_ref(),
                        Some(accessed_property_name) if accessed_property_name == &escape_leading_underscores(&argument.as_literal_like_node().text())
                    )
                {
                    return self.type_checker.get_type_with_facts(
                        type_,
                        if assume_true {
                            TypeFacts::NEUndefined
                        } else {
                            TypeFacts::EQUndefined
                        },
                    );
                }
            }
        }
        type_.type_wrapper()
    }

    pub(super) fn narrow_type_by_type_predicate(
        &self,
        type_: &Type,
        predicate: &TypePredicate,
        call_expression: &Node, /*CallExpression*/
        assume_true: bool,
    ) -> Rc<Type> {
        let mut type_ = type_.type_wrapper();
        if let Some(predicate_type) = predicate.type_.as_ref().filter(|predicate_type| {
            !(self.type_checker.is_type_any(Some(&*type_))
                && (Rc::ptr_eq(predicate_type, &self.type_checker.global_object_type())
                    || Rc::ptr_eq(predicate_type, &self.type_checker.global_function_type())))
        }) {
            let predicate_argument = self
                .type_checker
                .get_type_predicate_argument(predicate, call_expression);
            if let Some(predicate_argument) = predicate_argument.as_ref() {
                if self
                    .type_checker
                    .is_matching_reference(&self.reference, predicate_argument)
                {
                    return self.get_narrowed_type(
                        &type_,
                        predicate_type,
                        assume_true,
                        |type1: &Type, type2: &Type| {
                            self.type_checker.is_type_subtype_of(type1, type2)
                        },
                    );
                }
                if self.type_checker.strict_null_checks
                    && assume_true
                    && self
                        .type_checker
                        .optional_chain_contains_reference(predicate_argument, &self.reference)
                    && !self
                        .type_checker
                        .get_type_facts(predicate_type, None)
                        .intersects(TypeFacts::EQUndefined)
                {
                    type_ = self
                        .type_checker
                        .get_type_with_facts(&type_, TypeFacts::NEUndefinedOrNull);
                }
                let access = self.get_discriminant_property_access(predicate_argument, &type_);
                if let Some(access) = access.as_ref() {
                    return self.narrow_type_by_discriminant(&type_, access, |t: &Type| {
                        self.get_narrowed_type(
                            t,
                            predicate_type,
                            assume_true,
                            |type1: &Type, type2: &Type| {
                                self.type_checker.is_type_subtype_of(type1, type2)
                            },
                        )
                    });
                }
            }
        }
        type_.type_wrapper()
    }

    pub(super) fn narrow_type(
        &self,
        type_: &Type,
        expr: &Node, /*Expression*/
        assume_true: bool,
    ) -> Rc<Type> {
        if is_expression_of_optional_chain_root(expr) || {
            let expr_parent = expr.parent();
            is_binary_expression(&expr_parent) && {
                let expr_parent_as_binary_expression = expr_parent.as_binary_expression();
                expr_parent_as_binary_expression.operator_token.kind()
                    == SyntaxKind::QuestionQuestionToken
                    && ptr::eq(&*expr_parent_as_binary_expression.left, expr)
            }
        } {
            return self.narrow_type_by_optionality(type_, expr, assume_true);
        }
        match expr.kind() {
            SyntaxKind::Identifier => {
                if !self
                    .type_checker
                    .is_matching_reference(&self.reference, expr)
                    && self.type_checker.inline_level() < 5
                {
                    let symbol = self.type_checker.get_resolved_symbol(expr);
                    if self.type_checker.is_const_variable(&symbol) {
                        let declaration = symbol.maybe_value_declaration();
                        if let Some(declaration) = declaration.as_ref().filter(|declaration| {
                            is_variable_declaration(declaration) && {
                                let declaration_as_variable_declaration =
                                    declaration.as_variable_declaration();
                                declaration_as_variable_declaration.maybe_type().is_none()
                                    && declaration_as_variable_declaration
                                        .maybe_initializer()
                                        .is_some()
                                    && self.type_checker.is_constant_reference(&self.reference)
                            }
                        }) {
                            self.type_checker
                                .set_inline_level(self.type_checker.inline_level() + 1);
                            let result = self.narrow_type(
                                type_,
                                &declaration
                                    .as_has_initializer()
                                    .maybe_initializer()
                                    .unwrap(),
                                assume_true,
                            );
                            self.type_checker
                                .set_inline_level(self.type_checker.inline_level() - 1);
                            return result;
                        }
                    }
                }
                return self.narrow_type_by_truthiness(type_, expr, assume_true);
            }
            SyntaxKind::ThisKeyword
            | SyntaxKind::SuperKeyword
            | SyntaxKind::PropertyAccessExpression
            | SyntaxKind::ElementAccessExpression => {
                return self.narrow_type_by_truthiness(type_, expr, assume_true);
            }
            SyntaxKind::CallExpression => {
                return self.narrow_type_by_call_expression(type_, expr, assume_true);
            }
            SyntaxKind::ParenthesizedExpression | SyntaxKind::NonNullExpression => {
                return self.narrow_type(
                    type_,
                    &expr.as_has_expression().expression(),
                    assume_true,
                );
            }
            SyntaxKind::BinaryExpression => {
                return self.narrow_type_by_binary_expression(type_, expr, assume_true);
            }
            SyntaxKind::PrefixUnaryExpression => {
                let expr_as_prefix_unary_expression = expr.as_prefix_unary_expression();
                if expr_as_prefix_unary_expression.operator == SyntaxKind::ExclamationToken {
                    return self.narrow_type(
                        type_,
                        &expr_as_prefix_unary_expression.operand,
                        !assume_true,
                    );
                }
            }
            _ => (),
        }
        type_.type_wrapper()
    }

    pub(super) fn narrow_type_by_optionality(
        &self,
        type_: &Type,
        expr: &Node, /*Expression*/
        assume_present: bool,
    ) -> Rc<Type> {
        if self
            .type_checker
            .is_matching_reference(&self.reference, expr)
        {
            return self.type_checker.get_type_with_facts(
                type_,
                if assume_present {
                    TypeFacts::NEUndefinedOrNull
                } else {
                    TypeFacts::EQUndefinedOrNull
                },
            );
        }
        let access = self.get_discriminant_property_access(expr, type_);
        if let Some(access) = access.as_ref() {
            return self.narrow_type_by_discriminant(type_, access, |t: &Type| {
                self.type_checker.get_type_with_facts(
                    t,
                    if assume_present {
                        TypeFacts::NEUndefinedOrNull
                    } else {
                        TypeFacts::EQUndefinedOrNull
                    },
                )
            });
        }
        type_.type_wrapper()
    }
}

impl TypeChecker {
    pub(super) fn get_type_of_symbol_at_location_(
        &self,
        symbol: &Symbol,
        location: &Node,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_symbol_assigned(&self, symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn is_const_variable(&self, symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn get_narrowable_type_for_reference(
        &self,
        type_: &Type,
        reference: &Node,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_identifier(
        &self,
        node: &Node, /*Identifier*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let symbol = self.get_resolved_symbol(node);
        if Rc::ptr_eq(&symbol, &self.unknown_symbol()) {
            return self.error_type();
        }

        let local_or_export_symbol = self
            .get_export_symbol_of_value_symbol_if_exported(Some(symbol))
            .unwrap();

        let type_ = self.get_type_of_symbol(&*local_or_export_symbol);

        type_
    }
}
