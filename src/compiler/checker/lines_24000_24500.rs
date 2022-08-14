#![allow(non_upper_case_globals)]

use std::ptr;
use std::rc::Rc;

use super::{GetFlowTypeOfReference, TypeFacts};
use crate::{
    escape_leading_underscores, every, has_static_modifier, is_private_identifier,
    is_string_literal_like, Debug_, SymbolFlags, SymbolInterface, SyntaxKind, __String,
    are_rc_slices_equal, is_access_expression, is_optional_chain, map, same_map, Node,
    NodeInterface, Symbol, Type, TypeFlags, TypeInterface, TypePredicate,
    UnionOrIntersectionTypeInterface, UnionReduction,
};

impl GetFlowTypeOfReference {
    pub(super) fn get_union_or_evolving_array_type(
        &self,
        types: &[Rc<Type>],
        subtype_reduction: UnionReduction,
    ) -> Rc<Type> {
        if self.type_checker.is_evolving_array_type_list(types) {
            return self
                .type_checker
                .get_evolving_array_type(&self.type_checker.get_union_type(
                    map(types, |type_: &Rc<Type>, _| {
                        self.type_checker
                            .get_element_type_of_evolving_array_type(type_)
                    }),
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                ));
        }
        let result = self.type_checker.get_union_type(
            same_map(types, |type_: &Rc<Type>, _| {
                self.type_checker.finalize_evolving_array_type(type_)
            }),
            None,
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        );
        if !Rc::ptr_eq(&result, &self.declared_type)
            && (result.flags() & self.declared_type.flags()).intersects(TypeFlags::Union)
            && are_rc_slices_equal(
                result.as_union_type().types(),
                self.declared_type.as_union_type().types(),
            )
        {
            return self.declared_type.clone();
        }
        result
    }

    pub(super) fn get_discriminant_property_access(
        &self,
        expr: &Node, /*Expression*/
        computed_type: &Type,
    ) -> Option<Rc<Node>> {
        let mut access: Option<Rc<Node>> = None;
        let mut name: Option<__String> = None;
        let type_ = if self.declared_type.flags().intersects(TypeFlags::Union) {
            &*self.declared_type
        } else {
            computed_type
        };
        if type_.flags().intersects(TypeFlags::Union)
            && {
                access = self.type_checker.get_property_access(expr);
                access.is_some()
            }
            && {
                name = self
                    .type_checker
                    .get_accessed_property_name(access.as_ref().unwrap());
                name.is_some()
            }
            && self.type_checker.is_matching_reference(&self.reference, &*{
                let access = access.as_ref().unwrap();
                if is_access_expression(access) {
                    access.as_has_expression().expression()
                } else {
                    access
                        .parent()
                        .parent()
                        .as_has_initializer()
                        .maybe_initializer()
                        .unwrap()
                }
            })
            && self
                .type_checker
                .is_discriminant_property(Some(&*type_), name.as_ref().unwrap())
        {
            access
        } else {
            None
        }
    }

    pub(super) fn narrow_type_by_discriminant<TNarrowType: FnMut(&Type) -> Rc<Type>>(
        &self,
        type_: &Type,
        access: &Node, /*AccessExpression | BindingElement*/
        mut narrow_type: TNarrowType,
    ) -> Rc<Type> {
        let prop_name = self.type_checker.get_accessed_property_name(access);
        if prop_name.is_none() {
            return type_.type_wrapper();
        }
        let prop_name = prop_name.unwrap();
        let remove_nullable = self.type_checker.strict_null_checks
            && is_optional_chain(access)
            && self
                .type_checker
                .maybe_type_of_kind(type_, TypeFlags::Nullable);
        let prop_type = self.type_checker.get_type_of_property_of_type_(
            &*if remove_nullable {
                self.type_checker
                    .get_type_with_facts(type_, TypeFacts::NEUndefinedOrNull)
            } else {
                type_.type_wrapper()
            },
            &prop_name,
        );
        if prop_type.is_none() {
            return type_.type_wrapper();
        }
        let mut prop_type = prop_type.unwrap();
        prop_type = if remove_nullable {
            self.type_checker.get_optional_type_(&prop_type, None)
        } else {
            prop_type
        };
        let narrowed_prop_type = narrow_type(&prop_type);
        self.type_checker.filter_type(type_, |t: &Type| {
            let discriminant_type = self
                .type_checker
                .get_type_of_property_or_index_signature(t, &prop_name);
            !narrowed_prop_type.flags().intersects(TypeFlags::Never)
                && self
                    .type_checker
                    .is_type_comparable_to(&narrowed_prop_type, &discriminant_type)
        })
    }

    pub(super) fn narrow_type_by_discriminant_property(
        &self,
        type_: &Type,
        access: &Node, /*AccessExpression | BindingElement*/
        operator: SyntaxKind,
        value: &Node, /*Expression*/
        assume_true: bool,
    ) -> Rc<Type> {
        if matches!(
            operator,
            SyntaxKind::EqualsEqualsEqualsToken | SyntaxKind::ExclamationEqualsEqualsToken
        ) && type_.flags().intersects(TypeFlags::Union)
        {
            let key_property_name = self.type_checker.get_key_property_name(type_);
            if let Some(key_property_name) =
                key_property_name.as_ref().filter(|key_property_name| {
                    matches!(
                        self.type_checker.get_accessed_property_name(access).as_ref(),
                        Some(accessed_property_name) if *key_property_name == accessed_property_name
                    )
                })
            {
                let candidate = self.type_checker.get_constituent_type_for_key_type(
                    type_,
                    &self.type_checker.get_type_of_expression(value),
                );
                if let Some(candidate) = candidate.as_ref() {
                    return if operator
                        == if assume_true {
                            SyntaxKind::EqualsEqualsEqualsToken
                        } else {
                            SyntaxKind::ExclamationEqualsEqualsToken
                        } {
                        candidate.clone()
                    } else if self.type_checker.is_unit_type(
                        &self
                            .type_checker
                            .get_type_of_property_of_type_(candidate, key_property_name)
                            .unwrap_or_else(|| self.type_checker.unknown_type()),
                    ) {
                        self.type_checker.remove_type(type_, candidate)
                    } else {
                        type_.type_wrapper()
                    };
                }
            }
        }
        self.narrow_type_by_discriminant(type_, access, |t: &Type| {
            self.narrow_type_by_equality(t, operator, value, assume_true)
        })
    }

    pub(super) fn narrow_type_by_switch_on_discriminant_property(
        &self,
        type_: &Type,
        access: &Node,           /*AccessExpression | BindingElement*/
        switch_statement: &Node, /*SwitchStatement*/
        clause_start: usize,
        clause_end: usize,
    ) -> Rc<Type> {
        if clause_start < clause_end
            && type_.flags().intersects(TypeFlags::Union)
            && self.type_checker.get_key_property_name(type_)
                == self.type_checker.get_accessed_property_name(access)
        {
            let clause_types = self.type_checker.get_switch_clause_types(switch_statement);
            let clause_types = &clause_types[clause_start..clause_end];
            let candidate = self.type_checker.get_union_type(
                map(clause_types, |t: &Rc<Type>, _| {
                    self.type_checker
                        .get_constituent_type_for_key_type(type_, t)
                        .unwrap_or_else(|| self.type_checker.unknown_type())
                }),
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            );
            if !Rc::ptr_eq(&candidate, &self.type_checker.unknown_type()) {
                return candidate;
            }
        }
        self.narrow_type_by_discriminant(type_, access, |t: &Type| {
            self.narrow_type_by_switch_on_discriminant(
                t,
                switch_statement,
                clause_start,
                clause_end,
            )
        })
    }

    pub(super) fn narrow_type_by_truthiness(
        &self,
        type_: &Type,
        expr: &Node, /*Expression*/
        assume_true: bool,
    ) -> Rc<Type> {
        if self
            .type_checker
            .is_matching_reference(&self.reference, expr)
        {
            return if type_.flags().intersects(TypeFlags::Unknown) && assume_true {
                self.type_checker.non_null_unknown_type()
            } else {
                self.type_checker.get_type_with_facts(
                    type_,
                    if assume_true {
                        TypeFacts::Truthy
                    } else {
                        TypeFacts::Falsy
                    },
                )
            };
        }
        let mut type_ = type_.type_wrapper();
        if self.type_checker.strict_null_checks
            && assume_true
            && self
                .type_checker
                .optional_chain_contains_reference(expr, &self.reference)
        {
            type_ = self
                .type_checker
                .get_type_with_facts(&type_, TypeFacts::NEUndefinedOrNull);
        }
        let access = self.get_discriminant_property_access(expr, &type_);
        if let Some(access) = access.as_ref() {
            return self.narrow_type_by_discriminant(&type_, access, |t: &Type| {
                self.type_checker.get_type_with_facts(
                    t,
                    if assume_true {
                        TypeFacts::Truthy
                    } else {
                        TypeFacts::Falsy
                    },
                )
            });
        }
        type_
    }

    pub(super) fn is_type_presence_possible(
        &self,
        type_: &Type,
        prop_name: &__String,
        assume_true: bool,
    ) -> bool {
        let prop = self
            .type_checker
            .get_property_of_type_(type_, prop_name, None);
        if let Some(prop) = prop.as_ref() {
            return if prop.flags().intersects(SymbolFlags::Optional) {
                true
            } else {
                assume_true
            };
        }
        if self
            .type_checker
            .get_applicable_index_info_for_name(type_, prop_name)
            .is_some()
        {
            true
        } else {
            !assume_true
        }
    }

    pub(super) fn narrow_by_in_keyword(
        &self,
        type_: &Type,
        name: &__String,
        assume_true: bool,
    ) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union)
            || type_.flags().intersects(TypeFlags::Object) && !ptr::eq(&*self.declared_type, type_)
            || self.type_checker.is_this_type_parameter(type_)
            || type_.flags().intersects(TypeFlags::Intersection)
                && every(type_.as_intersection_type().types(), |t: &Rc<Type>, _| {
                    !matches!(
                        t.maybe_symbol().as_ref(),
                        Some(t_symbol) if Rc::ptr_eq(
                            t_symbol,
                            &self.type_checker.global_this_symbol()
                        )
                    )
                })
        {
            return self.type_checker.filter_type(type_, |t: &Type| {
                self.is_type_presence_possible(t, name, assume_true)
            });
        }
        type_.type_wrapper()
    }

    pub(super) fn narrow_type_by_binary_expression(
        &self,
        type_: &Type,
        expr: &Node, /*BinaryExpression*/
        assume_true: bool,
    ) -> Rc<Type> {
        let expr_as_binary_expression = expr.as_binary_expression();
        let mut type_ = type_.type_wrapper();
        match expr_as_binary_expression.operator_token.kind() {
            SyntaxKind::EqualsToken
            | SyntaxKind::BarBarEqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken => {
                return self.narrow_type_by_truthiness(
                    &self.narrow_type(&type_, &expr_as_binary_expression.right, assume_true),
                    &expr_as_binary_expression.left,
                    assume_true,
                );
            }
            SyntaxKind::EqualsEqualsToken
            | SyntaxKind::ExclamationEqualsToken
            | SyntaxKind::EqualsEqualsEqualsToken
            | SyntaxKind::ExclamationEqualsEqualsToken => {
                let operator = expr_as_binary_expression.operator_token.kind();
                let left = self
                    .type_checker
                    .get_reference_candidate(&expr_as_binary_expression.left);
                let right = self
                    .type_checker
                    .get_reference_candidate(&expr_as_binary_expression.right);
                if left.kind() == SyntaxKind::TypeOfExpression && is_string_literal_like(&right) {
                    return self.narrow_type_by_typeof(
                        &type_,
                        &left,
                        operator,
                        &right,
                        assume_true,
                    );
                }
                if right.kind() == SyntaxKind::TypeOfExpression && is_string_literal_like(&left) {
                    return self.narrow_type_by_typeof(
                        &type_,
                        &right,
                        operator,
                        &left,
                        assume_true,
                    );
                }
                if self
                    .type_checker
                    .is_matching_reference(&self.reference, &left)
                {
                    return self.narrow_type_by_equality(&type_, operator, &right, assume_true);
                }
                if self
                    .type_checker
                    .is_matching_reference(&self.reference, &right)
                {
                    return self.narrow_type_by_equality(&type_, operator, &left, assume_true);
                }
                if self.type_checker.strict_null_checks {
                    if self
                        .type_checker
                        .optional_chain_contains_reference(&left, &self.reference)
                    {
                        type_ = self.narrow_type_by_optional_chain_containment(
                            &type_,
                            operator,
                            &right,
                            assume_true,
                        );
                    } else if self
                        .type_checker
                        .optional_chain_contains_reference(&right, &self.reference)
                    {
                        type_ = self.narrow_type_by_optional_chain_containment(
                            &type_,
                            operator,
                            &left,
                            assume_true,
                        );
                    }
                }
                let left_access = self.get_discriminant_property_access(&left, &type_);
                if let Some(left_access) = left_access.as_ref() {
                    return self.narrow_type_by_discriminant_property(
                        &type_,
                        left_access,
                        operator,
                        &right,
                        assume_true,
                    );
                }
                let right_access = self.get_discriminant_property_access(&right, &type_);
                if let Some(right_access) = right_access.as_ref() {
                    return self.narrow_type_by_discriminant_property(
                        &type_,
                        right_access,
                        operator,
                        &left,
                        assume_true,
                    );
                }
                if self.is_matching_constructor_reference(&left) {
                    return self.narrow_type_by_constructor(&type_, operator, &right, assume_true);
                }
                if self.is_matching_constructor_reference(&right) {
                    return self.narrow_type_by_constructor(&type_, operator, &left, assume_true);
                }
            }
            SyntaxKind::InstanceOfKeyword => {
                return self.narrow_type_by_instanceof(&type_, expr, assume_true);
            }
            SyntaxKind::InKeyword => {
                if is_private_identifier(&expr_as_binary_expression.left) {
                    return self.narrow_type_by_private_identifier_in_in_expression(
                        &type_,
                        expr,
                        assume_true,
                    );
                }
                let target = self
                    .type_checker
                    .get_reference_candidate(&expr_as_binary_expression.right);
                let left_type = self
                    .type_checker
                    .get_type_of_node(&expr_as_binary_expression.left);
                if left_type.flags().intersects(TypeFlags::StringLiteral) {
                    let name =
                        escape_leading_underscores(&left_type.as_string_literal_type().value);
                    if self.type_checker.contains_missing_type(&type_)
                        && is_access_expression(&self.reference)
                        && self.type_checker.is_matching_reference(
                            &self.reference.as_has_expression().expression(),
                            &target,
                        )
                        && matches!(
                            self.type_checker.get_accessed_property_name(
                                &self.reference
                            ).as_ref(),
                            Some(accessed_property_name) if accessed_property_name == &name
                        )
                    {
                        return self.type_checker.get_type_with_facts(
                            &type_,
                            if assume_true {
                                TypeFacts::NEUndefined
                            } else {
                                TypeFacts::EQUndefined
                            },
                        );
                    }
                    if self
                        .type_checker
                        .is_matching_reference(&self.reference, &target)
                    {
                        return self.narrow_by_in_keyword(&type_, &name, assume_true);
                    }
                }
            }
            SyntaxKind::CommaToken => {
                return self.narrow_type(&type_, &expr_as_binary_expression.right, assume_true);
            }
            SyntaxKind::AmpersandAmpersandToken => {
                return if assume_true {
                    self.narrow_type(
                        &self.narrow_type(&type_, &expr_as_binary_expression.left, true),
                        &expr_as_binary_expression.right,
                        true,
                    )
                } else {
                    self.type_checker.get_union_type(
                        vec![
                            self.narrow_type(&type_, &expr_as_binary_expression.left, false),
                            self.narrow_type(&type_, &expr_as_binary_expression.right, false),
                        ],
                        None,
                        Option::<&Symbol>::None,
                        None,
                        Option::<&Type>::None,
                    )
                };
            }
            SyntaxKind::BarBarToken => {
                return if assume_true {
                    self.type_checker.get_union_type(
                        vec![
                            self.narrow_type(&type_, &expr_as_binary_expression.left, true),
                            self.narrow_type(&type_, &expr_as_binary_expression.right, true),
                        ],
                        None,
                        Option::<&Symbol>::None,
                        None,
                        Option::<&Type>::None,
                    )
                } else {
                    self.narrow_type(
                        &self.narrow_type(&type_, &expr_as_binary_expression.left, false),
                        &expr_as_binary_expression.right,
                        false,
                    )
                };
            }
            _ => (),
        }
        type_
    }

    pub(super) fn narrow_type_by_private_identifier_in_in_expression(
        &self,
        type_: &Type,
        expr: &Node, /*BinaryExpression*/
        assume_true: bool,
    ) -> Rc<Type> {
        let expr_as_binary_expression = expr.as_binary_expression();
        let target = self
            .type_checker
            .get_reference_candidate(&expr_as_binary_expression.right);
        if !self
            .type_checker
            .is_matching_reference(&self.reference, &target)
        {
            return type_.type_wrapper();
        }

        Debug_.assert_node(
            Some(&*expr_as_binary_expression.left),
            Some(is_private_identifier),
            None,
        );
        let symbol = self
            .type_checker
            .get_symbol_for_private_identifier_expression(&expr_as_binary_expression.left);
        if symbol.is_none() {
            return type_.type_wrapper();
        }
        let symbol = symbol.unwrap();
        let class_symbol = symbol.maybe_parent().unwrap();
        let target_type = if has_static_modifier(Debug_.check_defined::<&Rc<Node>>(
            symbol.maybe_value_declaration().as_ref(),
            Some("should always have a declaration"),
        )) {
            self.type_checker.get_type_of_symbol(&class_symbol)
        } else {
            self.type_checker.get_declared_type_of_symbol(&class_symbol)
        };
        self.get_narrowed_type(
            type_,
            &target_type,
            assume_true,
            |source: &Type, target: &Type| self.type_checker.is_type_derived_from(source, target),
        )
    }

    pub(super) fn narrow_type_by_optional_chain_containment(
        &self,
        type_: &Type,
        operator: SyntaxKind,
        value: &Node, /*Expression*/
        assume_true: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn narrow_type_by_equality(
        &self,
        type_: &Type,
        operator: SyntaxKind,
        value: &Node, /*Expression*/
        assume_true: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn narrow_type_by_typeof(
        &self,
        type_: &Type,
        type_of_expr: &Node, /*TypeOfExpression*/
        operator: SyntaxKind,
        literal: &Node, /*LiteralExpression*/
        assume_true: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn narrow_type_by_switch_optional_chain_containment<
        TClauseCheck: FnMut(&Type) -> bool,
    >(
        &self,
        type_: &Type,
        switch_statement: &Node, /*SwitchStatement*/
        clause_start: usize,
        clause_end: usize,
        clause_check: TClauseCheck,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn narrow_type_by_switch_on_discriminant(
        &self,
        type_: &Type,
        switch_statement: &Node, /*SwitchStatement*/
        clause_start: usize,
        clause_end: usize,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn narrow_by_switch_on_type_of(
        &self,
        type_: &Type,
        switch_statement: &Node, /*SwitchStatement*/
        clause_start: usize,
        clause_end: usize,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_matching_constructor_reference(
        &self,
        expr: &Node, /*Expression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn narrow_type_by_constructor(
        &self,
        type_: &Type,
        operator: SyntaxKind,
        identifier: &Node, /*Expression*/
        assume_true: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn narrow_type_by_instanceof(
        &self,
        type_: &Type,
        expr: &Node, /*BinaryExpression*/
        assume_true: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_narrowed_type<TIsRelated: FnMut(&Type, &Type) -> bool>(
        &self,
        type_: &Type,
        candidate: &Type,
        assume_true: bool,
        is_related: TIsRelated,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn narrow_type_by_type_predicate(
        &self,
        type_: &Type,
        predicate: &TypePredicate,
        call_expression: &Node, /*CallExpression*/
        assume_true: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn narrow_type(
        &self,
        type_: &Type,
        expr: &Node, /*Expression*/
        assume_true: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }
}
