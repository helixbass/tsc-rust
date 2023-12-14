use std::{io, ptr};

use gc::Gc;
use id_arena::Id;

use super::{typeof_eq_facts, typeof_ne_facts, GetFlowTypeOfReference, TypeFacts};
use crate::{
    are_gc_slices_equal, contains_gc, escape_leading_underscores, every, find_index,
    has_static_modifier, id_text, is_element_access_expression, is_private_identifier,
    is_property_access_expression, is_string_literal_like, Debug_, SymbolFlags, SymbolInterface,
    SyntaxKind, __String, contains, is_access_expression, is_optional_chain, map, try_map,
    HasArena, Node, NodeInterface, OptionTry, Symbol, Type, TypeFlags, TypeInterface,
    UnionOrIntersectionTypeInterface, UnionReduction,
};

impl GetFlowTypeOfReference {
    pub(super) fn get_union_or_evolving_array_type(
        &self,
        types: &[Id<Type>],
        subtype_reduction: UnionReduction,
    ) -> io::Result<Id<Type>> {
        if self.type_checker.is_evolving_array_type_list(types) {
            return Ok(self.type_checker.get_evolving_array_type(
                self.type_checker.get_union_type(
                    &map(types, |&type_: &Id<Type>, _| {
                        self.type_checker
                            .get_element_type_of_evolving_array_type(type_)
                    }),
                    None,
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?,
            ));
        }
        let result = self.type_checker.get_union_type(
            &try_map(types, |&type_: &Id<Type>, _| {
                self.type_checker.finalize_evolving_array_type(type_)
            })?,
            Some(subtype_reduction),
            Option::<Id<Symbol>>::None,
            None,
            None,
        )?;
        if result != self.declared_type
            && (self.type_checker.type_(result).flags()
                & self.type_checker.type_(self.declared_type).flags())
            .intersects(TypeFlags::Union)
            && self.type_checker.type_(result).as_union_type().types()
                == self
                    .type_checker
                    .type_(self.declared_type)
                    .as_union_type()
                    .types()
        {
            return Ok(self.declared_type.clone());
        }
        Ok(result)
    }

    pub(super) fn get_discriminant_property_access(
        &self,
        expr: &Node, /*Expression*/
        computed_type: Id<Type>,
    ) -> io::Result<Option<Gc<Node>>> {
        let mut access: Option<Gc<Node>> = None;
        let mut name: Option<__String> = None;
        let type_ = if self
            .type_checker
            .type_(self.declared_type)
            .flags()
            .intersects(TypeFlags::Union)
        {
            self.declared_type
        } else {
            computed_type
        };
        Ok(
            if self
                .type_checker
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Union)
                && {
                    access = self.type_checker.get_property_access(expr)?;
                    access.is_some()
                }
                && {
                    name = self
                        .type_checker
                        .get_accessed_property_name(access.as_ref().unwrap())?;
                    name.is_some()
                }
                && self
                    .type_checker
                    .is_matching_reference(&self.reference, &*{
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
                    })?
                && self
                    .type_checker
                    .is_discriminant_property(Some(type_), name.as_ref().unwrap())?
            {
                access
            } else {
                None
            },
        )
    }

    #[allow(dead_code)]
    pub(super) fn narrow_type_by_discriminant(
        &self,
        type_: Id<Type>,
        access: &Node, /*AccessExpression | BindingElement*/
        mut narrow_type: impl FnMut(Id<Type>) -> Id<Type>,
    ) -> Id<Type> {
        self.try_narrow_type_by_discriminant(type_, access, |type_: Id<Type>| {
            Ok(narrow_type(type_))
        })
        .unwrap()
    }

    pub(super) fn try_narrow_type_by_discriminant(
        &self,
        type_: Id<Type>,
        access: &Node, /*AccessExpression | BindingElement*/
        mut narrow_type: impl FnMut(Id<Type>) -> io::Result<Id<Type>>,
    ) -> io::Result<Id<Type>> {
        let prop_name = self.type_checker.get_accessed_property_name(access)?;
        if prop_name.is_none() {
            return Ok(type_);
        }
        let prop_name = prop_name.unwrap();
        let remove_nullable = self.type_checker.strict_null_checks
            && is_optional_chain(access)
            && self
                .type_checker
                .maybe_type_of_kind(type_, TypeFlags::Nullable);
        let prop_type = self.type_checker.get_type_of_property_of_type_(
            if remove_nullable {
                self.type_checker
                    .get_type_with_facts(type_, TypeFacts::NEUndefinedOrNull)?
            } else {
                type_
            },
            &prop_name,
        )?;
        if prop_type.is_none() {
            return Ok(type_);
        }
        let mut prop_type = prop_type.unwrap();
        prop_type = if remove_nullable {
            self.type_checker.get_optional_type_(prop_type, None)?
        } else {
            prop_type
        };
        let narrowed_prop_type = narrow_type(prop_type)?;
        self.type_checker.try_filter_type(type_, |t: Id<Type>| {
            let discriminant_type = self
                .type_checker
                .get_type_of_property_or_index_signature(t, &prop_name)?;
            Ok(!self
                .type_checker
                .type_(narrowed_prop_type)
                .flags()
                .intersects(TypeFlags::Never)
                && self
                    .type_checker
                    .is_type_comparable_to(narrowed_prop_type, discriminant_type)?)
        })
    }

    pub(super) fn narrow_type_by_discriminant_property(
        &self,
        type_: Id<Type>,
        access: &Node, /*AccessExpression | BindingElement*/
        operator: SyntaxKind,
        value: &Node, /*Expression*/
        assume_true: bool,
    ) -> io::Result<Id<Type>> {
        if matches!(
            operator,
            SyntaxKind::EqualsEqualsEqualsToken | SyntaxKind::ExclamationEqualsEqualsToken
        ) && self
            .type_checker
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Union)
        {
            let key_property_name = self.type_checker.get_key_property_name(type_)?;
            if let Some(key_property_name) = key_property_name.as_ref().try_filter(
                |key_property_name| -> io::Result<_> {
                    Ok(matches!(
                        self.type_checker.get_accessed_property_name(access)?.as_ref(),
                        Some(accessed_property_name) if *key_property_name == accessed_property_name
                    ))
                },
            )? {
                let candidate = self.type_checker.get_constituent_type_for_key_type(
                    type_,
                    self.type_checker.get_type_of_expression(value)?,
                );
                if let Some(candidate) = candidate {
                    return Ok(
                        if operator
                            == if assume_true {
                                SyntaxKind::EqualsEqualsEqualsToken
                            } else {
                                SyntaxKind::ExclamationEqualsEqualsToken
                            }
                        {
                            candidate.clone()
                        } else if self.type_checker.is_unit_type(
                            self.type_checker
                                .get_type_of_property_of_type_(candidate, key_property_name)?
                                .unwrap_or_else(|| self.type_checker.unknown_type()),
                        ) {
                            self.type_checker.remove_type(type_, candidate)
                        } else {
                            type_
                        },
                    );
                }
            }
        }
        self.try_narrow_type_by_discriminant(type_, access, |t: Id<Type>| {
            self.narrow_type_by_equality(t, operator, value, assume_true)
        })
    }

    pub(super) fn narrow_type_by_switch_on_discriminant_property(
        &self,
        type_: Id<Type>,
        access: &Node,           /*AccessExpression | BindingElement*/
        switch_statement: &Node, /*SwitchStatement*/
        clause_start: usize,
        clause_end: usize,
    ) -> io::Result<Id<Type>> {
        if clause_start < clause_end
            && self
                .type_checker
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Union)
            && self.type_checker.get_key_property_name(type_)?
                == self.type_checker.get_accessed_property_name(access)?
        {
            let clause_types = self
                .type_checker
                .get_switch_clause_types(switch_statement)?;
            let clause_types = &clause_types[clause_start..clause_end];
            let candidate = self.type_checker.get_union_type(
                &map(clause_types, |&t: &Id<Type>, _| {
                    self.type_checker
                        .get_constituent_type_for_key_type(type_, t)
                        .unwrap_or_else(|| self.type_checker.unknown_type())
                }),
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?;
            if candidate != self.type_checker.unknown_type() {
                return Ok(candidate);
            }
        }
        self.try_narrow_type_by_discriminant(type_, access, |t: Id<Type>| {
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
        mut type_: Id<Type>,
        expr: &Node, /*Expression*/
        assume_true: bool,
    ) -> io::Result<Id<Type>> {
        if self
            .type_checker
            .is_matching_reference(&self.reference, expr)?
        {
            return Ok(
                if self
                    .type_checker
                    .type_(type_)
                    .flags()
                    .intersects(TypeFlags::Unknown)
                    && assume_true
                {
                    self.type_checker.non_null_unknown_type()
                } else {
                    self.type_checker.get_type_with_facts(
                        type_,
                        if assume_true {
                            TypeFacts::Truthy
                        } else {
                            TypeFacts::Falsy
                        },
                    )?
                },
            );
        }
        if self.type_checker.strict_null_checks
            && assume_true
            && self
                .type_checker
                .optional_chain_contains_reference(expr, &self.reference)?
        {
            type_ = self
                .type_checker
                .get_type_with_facts(type_, TypeFacts::NEUndefinedOrNull)?;
        }
        let access = self.get_discriminant_property_access(expr, type_)?;
        if let Some(access) = access.as_ref() {
            return self.try_narrow_type_by_discriminant(type_, access, |t: Id<Type>| {
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
        Ok(type_)
    }

    pub(super) fn is_type_presence_possible(
        &self,
        type_: Id<Type>,
        prop_name: &str, /*__String*/
        assume_true: bool,
    ) -> io::Result<bool> {
        let prop = self
            .type_checker
            .get_property_of_type_(type_, prop_name, None)?;
        if let Some(prop) = prop {
            return Ok(
                if self
                    .type_checker
                    .symbol(prop)
                    .flags()
                    .intersects(SymbolFlags::Optional)
                {
                    true
                } else {
                    assume_true
                },
            );
        }
        Ok(
            if self
                .type_checker
                .get_applicable_index_info_for_name(type_, prop_name)?
                .is_some()
            {
                true
            } else {
                !assume_true
            },
        )
    }

    pub(super) fn narrow_by_in_keyword(
        &self,
        type_: Id<Type>,
        name: &str, /*__String*/
        assume_true: bool,
    ) -> io::Result<Id<Type>> {
        if self
            .type_checker
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Union)
            || self
                .type_checker
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Object)
                && self.declared_type != type_
            || self.type_checker.is_this_type_parameter(type_)
            || self
                .type_checker
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Intersection)
                && every(
                    self.type_checker
                        .type_(type_)
                        .as_intersection_type()
                        .types(),
                    |&t: &Id<Type>, _| {
                        !matches!(
                            self.type_checker.type_(t).maybe_symbol(),
                            Some(t_symbol) if
                                t_symbol ==
                                self.type_checker.global_this_symbol()
                        )
                    },
                )
        {
            return self.type_checker.try_filter_type(type_, |t: Id<Type>| {
                self.is_type_presence_possible(t, name, assume_true)
            });
        }
        Ok(type_)
    }

    pub(super) fn narrow_type_by_binary_expression(
        &self,
        mut type_: Id<Type>,
        expr: &Node, /*BinaryExpression*/
        assume_true: bool,
    ) -> io::Result<Id<Type>> {
        let expr_as_binary_expression = expr.as_binary_expression();
        match expr_as_binary_expression.operator_token.kind() {
            SyntaxKind::EqualsToken
            | SyntaxKind::BarBarEqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken => {
                return self.narrow_type_by_truthiness(
                    self.narrow_type(type_, &expr_as_binary_expression.right, assume_true)?,
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
                    return self.narrow_type_by_typeof(type_, &left, operator, &right, assume_true);
                }
                if right.kind() == SyntaxKind::TypeOfExpression && is_string_literal_like(&left) {
                    return self.narrow_type_by_typeof(type_, &right, operator, &left, assume_true);
                }
                if self
                    .type_checker
                    .is_matching_reference(&self.reference, &left)?
                {
                    return self.narrow_type_by_equality(type_, operator, &right, assume_true);
                }
                if self
                    .type_checker
                    .is_matching_reference(&self.reference, &right)?
                {
                    return self.narrow_type_by_equality(type_, operator, &left, assume_true);
                }
                if self.type_checker.strict_null_checks {
                    if self
                        .type_checker
                        .optional_chain_contains_reference(&left, &self.reference)?
                    {
                        type_ = self.narrow_type_by_optional_chain_containment(
                            type_,
                            operator,
                            &right,
                            assume_true,
                        )?;
                    } else if self
                        .type_checker
                        .optional_chain_contains_reference(&right, &self.reference)?
                    {
                        type_ = self.narrow_type_by_optional_chain_containment(
                            type_,
                            operator,
                            &left,
                            assume_true,
                        )?;
                    }
                }
                let left_access = self.get_discriminant_property_access(&left, type_)?;
                if let Some(left_access) = left_access.as_ref() {
                    return self.narrow_type_by_discriminant_property(
                        type_,
                        left_access,
                        operator,
                        &right,
                        assume_true,
                    );
                }
                let right_access = self.get_discriminant_property_access(&right, type_)?;
                if let Some(right_access) = right_access.as_ref() {
                    return self.narrow_type_by_discriminant_property(
                        type_,
                        right_access,
                        operator,
                        &left,
                        assume_true,
                    );
                }
                if self.is_matching_constructor_reference(&left)? {
                    return self.narrow_type_by_constructor(type_, operator, &right, assume_true);
                }
                if self.is_matching_constructor_reference(&right)? {
                    return self.narrow_type_by_constructor(type_, operator, &left, assume_true);
                }
            }
            SyntaxKind::InstanceOfKeyword => {
                return self.narrow_type_by_instanceof(type_, expr, assume_true);
            }
            SyntaxKind::InKeyword => {
                if is_private_identifier(&expr_as_binary_expression.left) {
                    return self.narrow_type_by_private_identifier_in_in_expression(
                        type_,
                        expr,
                        assume_true,
                    );
                }
                let target = self
                    .type_checker
                    .get_reference_candidate(&expr_as_binary_expression.right);
                let left_type = self
                    .type_checker
                    .get_type_of_node(&expr_as_binary_expression.left)?;
                if self
                    .type_checker
                    .type_(left_type)
                    .flags()
                    .intersects(TypeFlags::StringLiteral)
                {
                    let left_type_ref = self.type_checker.type_(left_type);
                    let name =
                        escape_leading_underscores(&left_type_ref.as_string_literal_type().value);
                    if self.type_checker.contains_missing_type(type_)
                        && is_access_expression(&self.reference)
                        && self.type_checker.is_matching_reference(
                            &self.reference.as_has_expression().expression(),
                            &target,
                        )?
                        && matches!(
                            self.type_checker.get_accessed_property_name(
                                &self.reference
                            )?.as_ref(),
                            Some(accessed_property_name) if accessed_property_name == &name
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
                    if self
                        .type_checker
                        .is_matching_reference(&self.reference, &target)?
                    {
                        return self.narrow_by_in_keyword(type_, &name, assume_true);
                    }
                }
            }
            SyntaxKind::CommaToken => {
                return self.narrow_type(type_, &expr_as_binary_expression.right, assume_true);
            }
            SyntaxKind::AmpersandAmpersandToken => {
                return Ok(if assume_true {
                    self.narrow_type(
                        self.narrow_type(type_, &expr_as_binary_expression.left, true)?,
                        &expr_as_binary_expression.right,
                        true,
                    )?
                } else {
                    self.type_checker.get_union_type(
                        &[
                            self.narrow_type(type_, &expr_as_binary_expression.left, false)?,
                            self.narrow_type(type_, &expr_as_binary_expression.right, false)?,
                        ],
                        None,
                        Option::<Id<Symbol>>::None,
                        None,
                        None,
                    )?
                });
            }
            SyntaxKind::BarBarToken => {
                return Ok(if assume_true {
                    self.type_checker.get_union_type(
                        &[
                            self.narrow_type(type_, &expr_as_binary_expression.left, true)?,
                            self.narrow_type(type_, &expr_as_binary_expression.right, true)?,
                        ],
                        None,
                        Option::<Id<Symbol>>::None,
                        None,
                        None,
                    )?
                } else {
                    self.narrow_type(
                        self.narrow_type(type_, &expr_as_binary_expression.left, false)?,
                        &expr_as_binary_expression.right,
                        false,
                    )?
                });
            }
            _ => (),
        }
        Ok(type_)
    }

    pub(super) fn narrow_type_by_private_identifier_in_in_expression(
        &self,
        type_: Id<Type>,
        expr: &Node, /*BinaryExpression*/
        assume_true: bool,
    ) -> io::Result<Id<Type>> {
        let expr_as_binary_expression = expr.as_binary_expression();
        let target = self
            .type_checker
            .get_reference_candidate(&expr_as_binary_expression.right);
        if !self
            .type_checker
            .is_matching_reference(&self.reference, &target)?
        {
            return Ok(type_);
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
            return Ok(type_);
        }
        let symbol = symbol.unwrap();
        let class_symbol = self.type_checker.symbol(symbol).maybe_parent().unwrap();
        let target_type = if has_static_modifier(
            Debug_.check_defined::<&Gc<Node>>(
                self.type_checker
                    .symbol(symbol)
                    .maybe_value_declaration()
                    .as_ref(),
                Some("should always have a declaration"),
            ),
        ) {
            self.type_checker.get_type_of_symbol(class_symbol)?
        } else {
            self.type_checker
                .get_declared_type_of_symbol(class_symbol)?
        };
        self.get_narrowed_type(
            type_,
            target_type,
            assume_true,
            |source: Id<Type>, target: Id<Type>| {
                self.type_checker.is_type_derived_from(source, target)
            },
        )
    }

    pub(super) fn narrow_type_by_optional_chain_containment(
        &self,
        type_: Id<Type>,
        operator: SyntaxKind,
        value: &Node, /*Expression*/
        assume_true: bool,
    ) -> io::Result<Id<Type>> {
        let equals_operator = matches!(
            operator,
            SyntaxKind::EqualsEqualsToken | SyntaxKind::EqualsEqualsEqualsToken
        );
        let nullable_flags = if matches!(
            operator,
            SyntaxKind::EqualsEqualsToken | SyntaxKind::ExclamationEqualsToken
        ) {
            TypeFlags::Nullable
        } else {
            TypeFlags::Undefined
        };
        let value_type = self.type_checker.get_type_of_expression(value)?;
        let remove_nullable = equals_operator != assume_true
            && self.type_checker.every_type(value_type, |t: Id<Type>| {
                self.type_checker
                    .type_(t)
                    .flags()
                    .intersects(nullable_flags)
            })
            || equals_operator == assume_true
                && self.type_checker.every_type(value_type, |t: Id<Type>| {
                    !self
                        .type_checker
                        .type_(t)
                        .flags()
                        .intersects(TypeFlags::AnyOrUnknown | nullable_flags)
                });
        Ok(if remove_nullable {
            self.type_checker
                .get_type_with_facts(type_, TypeFacts::NEUndefinedOrNull)?
        } else {
            type_
        })
    }

    pub(super) fn narrow_type_by_equality(
        &self,
        type_: Id<Type>,
        operator: SyntaxKind,
        value: &Node, /*Expression*/
        mut assume_true: bool,
    ) -> io::Result<Id<Type>> {
        if self
            .type_checker
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Any)
        {
            return Ok(type_);
        }
        if matches!(
            operator,
            SyntaxKind::ExclamationEqualsToken | SyntaxKind::ExclamationEqualsEqualsToken
        ) {
            assume_true = !assume_true;
        }
        let value_type = self.type_checker.get_type_of_expression(value)?;
        if assume_true
            && self
                .type_checker
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Unknown)
            && matches!(
                operator,
                SyntaxKind::EqualsEqualsToken | SyntaxKind::ExclamationEqualsToken
            )
            && self
                .type_checker
                .type_(value_type)
                .flags()
                .intersects(TypeFlags::Null)
        {
            return self.type_checker.get_union_type(
                &[
                    self.type_checker.null_type(),
                    self.type_checker.undefined_type(),
                ],
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            );
        }
        if self
            .type_checker
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Unknown)
            && assume_true
            && matches!(
                operator,
                SyntaxKind::EqualsEqualsEqualsToken | SyntaxKind::ExclamationEqualsEqualsToken
            )
        {
            if self
                .type_checker
                .type_(value_type)
                .flags()
                .intersects(TypeFlags::Primitive | TypeFlags::NonPrimitive)
            {
                return Ok(value_type);
            }
            if self
                .type_checker
                .type_(value_type)
                .flags()
                .intersects(TypeFlags::Object)
            {
                return Ok(self.type_checker.non_primitive_type());
            }
            return Ok(type_);
        }
        if self
            .type_checker
            .type_(value_type)
            .flags()
            .intersects(TypeFlags::Nullable)
        {
            if !self.type_checker.strict_null_checks {
                return Ok(type_);
            }
            let double_equals = matches!(
                operator,
                SyntaxKind::EqualsEqualsToken | SyntaxKind::ExclamationEqualsToken
            );
            let facts = if double_equals {
                if assume_true {
                    TypeFacts::EQUndefinedOrNull
                } else {
                    TypeFacts::NEUndefinedOrNull
                }
            } else if self
                .type_checker
                .type_(value_type)
                .flags()
                .intersects(TypeFlags::Null)
            {
                if assume_true {
                    TypeFacts::EQNull
                } else {
                    TypeFacts::NENull
                }
            } else {
                if assume_true {
                    TypeFacts::EQUndefined
                } else {
                    TypeFacts::NEUndefined
                }
            };
            return Ok(
                if self
                    .type_checker
                    .type_(type_)
                    .flags()
                    .intersects(TypeFlags::Unknown)
                    && facts.intersects(TypeFacts::NENull | TypeFacts::NEUndefinedOrNull)
                {
                    self.type_checker.non_null_unknown_type()
                } else {
                    self.type_checker.get_type_with_facts(type_, facts)?
                },
            );
        }
        if assume_true {
            let filter_fn = |t: Id<Type>| {
                Ok(if operator == SyntaxKind::EqualsEqualsToken {
                    self.type_checker.are_types_comparable(t, value_type)?
                        || self
                            .type_checker
                            .is_coercible_under_double_equals(t, value_type)
                } else {
                    self.type_checker.are_types_comparable(t, value_type)?
                })
            };
            return Ok(self.type_checker.replace_primitives_with_literals(
                self.type_checker.try_filter_type(type_, filter_fn)?,
                value_type,
            ));
        }
        if self.type_checker.is_unit_type(value_type) {
            return self.type_checker.try_filter_type(type_, |t: Id<Type>| {
                Ok(!(self.type_checker.is_unit_like_type(t)
                    && self.type_checker.are_types_comparable(t, value_type)?))
            });
        }
        Ok(type_)
    }

    pub(super) fn narrow_type_by_typeof(
        &self,
        type_: Id<Type>,
        type_of_expr: &Node, /*TypeOfExpression*/
        operator: SyntaxKind,
        literal: &Node, /*LiteralExpression*/
        mut assume_true: bool,
    ) -> io::Result<Id<Type>> {
        if matches!(
            operator,
            SyntaxKind::ExclamationEqualsToken | SyntaxKind::ExclamationEqualsEqualsToken
        ) {
            assume_true = !assume_true;
        }
        let type_of_expr_as_type_of_expression = type_of_expr.as_type_of_expression();
        let target = self
            .type_checker
            .get_reference_candidate(&type_of_expr_as_type_of_expression.expression);
        let literal_as_literal_like_node = literal.as_literal_like_node();
        if !self
            .type_checker
            .is_matching_reference(&self.reference, &target)?
        {
            if self.type_checker.strict_null_checks
                && self
                    .type_checker
                    .optional_chain_contains_reference(&target, &self.reference)?
                && assume_true == (&**literal_as_literal_like_node.text() != "undefined")
            {
                return self
                    .type_checker
                    .get_type_with_facts(type_, TypeFacts::NEUndefinedOrNull);
            }
            return Ok(type_);
        }
        if self
            .type_checker
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Any)
            && &**literal_as_literal_like_node.text() == "function"
        {
            return Ok(type_);
        }
        if assume_true
            && self
                .type_checker
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Unknown)
            && &**literal_as_literal_like_node.text() == "object"
        {
            return Ok(if type_ == self.type_checker.non_null_unknown_type() {
                self.type_checker.non_primitive_type()
            } else {
                self.type_checker.get_union_type(
                    &[
                        self.type_checker.non_primitive_type(),
                        self.type_checker.null_type(),
                    ],
                    None,
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?
            });
        }
        let facts = if assume_true {
            typeof_eq_facts
                .get(&&**literal_as_literal_like_node.text())
                .copied()
                .unwrap_or(TypeFacts::TypeofEQHostObject)
        } else {
            typeof_ne_facts
                .get(&&**literal_as_literal_like_node.text())
                .copied()
                .unwrap_or(TypeFacts::TypeofNEHostObject)
        };
        let implied_type =
            self.get_implied_type_from_typeof_guard(type_, &literal_as_literal_like_node.text())?;
        self.type_checker.get_type_with_facts(
            if let Some(implied_type) = implied_type.filter(|_implied_type| assume_true) {
                let mut callback_returning_non_optional =
                    self.narrow_union_member_by_typeof(implied_type);
                self.type_checker
                    .try_map_type(
                        type_,
                        &mut move |candidate: Id<Type>| {
                            Ok(Some(callback_returning_non_optional(candidate)?))
                        },
                        None,
                    )?
                    .unwrap()
            } else {
                type_
            },
            facts,
        )
    }

    pub(super) fn narrow_type_by_switch_optional_chain_containment(
        &self,
        type_: Id<Type>,
        switch_statement: &Node, /*SwitchStatement*/
        clause_start: usize,
        clause_end: usize,
        mut clause_check: impl FnMut(Id<Type>) -> bool,
    ) -> io::Result<Id<Type>> {
        let every_clause_checks = clause_start != clause_end
            && every(
                &self
                    .type_checker
                    .get_switch_clause_types(switch_statement)?[clause_start..clause_end],
                |&clause_type: &Id<Type>, _| clause_check(clause_type),
            );
        Ok(if every_clause_checks {
            self.type_checker
                .get_type_with_facts(type_, TypeFacts::NEUndefinedOrNull)?
        } else {
            type_
        })
    }

    pub(super) fn narrow_type_by_switch_on_discriminant(
        &self,
        type_: Id<Type>,
        switch_statement: &Node, /*SwitchStatement*/
        clause_start: usize,
        clause_end: usize,
    ) -> io::Result<Id<Type>> {
        let switch_types = self
            .type_checker
            .get_switch_clause_types(switch_statement)?;
        if switch_types.is_empty() {
            return Ok(type_);
        }
        let clause_types = &switch_types[clause_start..clause_end];
        let has_default_clause = clause_start == clause_end
            || contains(Some(clause_types), &self.type_checker.never_type());
        if self
            .type_checker
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Unknown)
            && !has_default_clause
        {
            let mut ground_clause_types: Option<Vec<Id<Type>>> = None;
            for i in 0..clause_types.len() {
                let t = clause_types[i];
                if self
                    .type_checker
                    .type_(t)
                    .flags()
                    .intersects(TypeFlags::Primitive | TypeFlags::NonPrimitive)
                {
                    if let Some(ground_clause_types) = ground_clause_types.as_mut() {
                        ground_clause_types.push(t.clone());
                    }
                } else if self
                    .type_checker
                    .type_(t)
                    .flags()
                    .intersects(TypeFlags::Object)
                {
                    if ground_clause_types.is_none() {
                        ground_clause_types = Some(clause_types[0..i].to_owned());
                    }
                    ground_clause_types
                        .as_mut()
                        .unwrap()
                        .push(self.type_checker.non_primitive_type());
                } else {
                    return Ok(type_);
                }
            }
            return self.type_checker.get_union_type(
                &match ground_clause_types {
                    None => clause_types.to_owned(),
                    Some(ground_clause_types) => ground_clause_types,
                },
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            );
        }
        let discriminant_type = self.type_checker.get_union_type(
            clause_types,
            None,
            Option::<Id<Symbol>>::None,
            None,
            None,
        )?;
        let case_type = if self
            .type_checker
            .type_(discriminant_type)
            .flags()
            .intersects(TypeFlags::Never)
        {
            self.type_checker.never_type()
        } else {
            self.type_checker.replace_primitives_with_literals(
                self.type_checker.try_filter_type(type_, |t: Id<Type>| {
                    self.type_checker.are_types_comparable(discriminant_type, t)
                })?,
                discriminant_type,
            )
        };
        if !has_default_clause {
            return Ok(case_type);
        }
        let default_type = self.type_checker.filter_type(type_, |t: Id<Type>| {
            !(self.type_checker.is_unit_like_type(t)
                && contains(
                    Some(&switch_types),
                    &self
                        .type_checker
                        .get_regular_type_of_literal_type(self.type_checker.extract_unit_type(t)),
                ))
        });
        Ok(
            if self
                .type_checker
                .type_(case_type)
                .flags()
                .intersects(TypeFlags::Never)
            {
                default_type
            } else {
                self.type_checker.get_union_type(
                    &[case_type, default_type],
                    None,
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?
            },
        )
    }

    pub(super) fn get_implied_type_from_typeof_guard(
        &self,
        type_: Id<Type>,
        text: &str,
    ) -> io::Result<Option<Id<Type>>> {
        Ok(match text {
            "function" => {
                if self
                    .type_checker
                    .type_(type_)
                    .flags()
                    .intersects(TypeFlags::Any)
                {
                    Some(type_)
                } else {
                    Some(self.type_checker.global_function_type())
                }
            }
            "object" => {
                if self
                    .type_checker
                    .type_(type_)
                    .flags()
                    .intersects(TypeFlags::Unknown)
                {
                    Some(self.type_checker.get_union_type(
                        &[
                            self.type_checker.non_primitive_type(),
                            self.type_checker.null_type(),
                        ],
                        None,
                        Option::<Id<Symbol>>::None,
                        None,
                        None,
                    )?)
                } else {
                    Some(type_)
                }
            }
            _ => self.type_checker.typeof_types_by_name().get(&text).cloned(),
        })
    }

    pub(super) fn narrow_union_member_by_typeof(
        &self,
        candidate: Id<Type>,
    ) -> impl FnMut(Id<Type>) -> io::Result<Id<Type>> + 'static {
        let type_checker = self.type_checker.clone();
        move |type_: Id<Type>| {
            if type_checker.is_type_subtype_of(type_, candidate)? {
                return Ok(type_);
            }
            if type_checker.is_type_subtype_of(candidate, type_)? {
                return Ok(candidate.clone());
            }
            if type_checker
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Instantiable)
            {
                let constraint = type_checker
                    .get_base_constraint_of_type(type_)?
                    .unwrap_or_else(|| type_checker.any_type());
                if type_checker.is_type_subtype_of(candidate, constraint)? {
                    return type_checker.get_intersection_type(
                        &vec![type_, candidate.clone()],
                        Option::<Id<Symbol>>::None,
                        None,
                    );
                }
            }
            Ok(type_)
        }
    }

    pub(super) fn narrow_by_switch_on_type_of(
        &self,
        type_: Id<Type>,
        switch_statement: &Node, /*SwitchStatement*/
        clause_start: usize,
        clause_end: usize,
    ) -> io::Result<Id<Type>> {
        let switch_witnesses = self
            .type_checker
            .get_switch_clause_type_of_witnesses(switch_statement, true);
        if switch_witnesses.is_empty() {
            return Ok(type_);
        }
        let default_case_location = find_index(
            &switch_witnesses,
            |elem: &Option<String>, _| elem.is_none(),
            None,
        );
        let has_default_clause = clause_start == clause_end
            || matches!(
                default_case_location,
                Some(default_case_location) if default_case_location >= clause_start && default_case_location < clause_end
            );
        let clause_witnesses: Vec<String>;
        let switch_facts: TypeFacts;
        if let Some(default_case_location) = default_case_location {
            let witnesses = switch_witnesses
                .iter()
                .filter_map(|witness| witness.clone())
                .collect::<Vec<_>>();
            let fixed_clause_start = if default_case_location < clause_start {
                clause_start - 1
            } else {
                clause_start
            };
            let fixed_clause_end = if default_case_location < clause_end {
                clause_end - 1
            } else {
                clause_end
            };
            clause_witnesses = witnesses[fixed_clause_start..fixed_clause_end].to_owned();
            switch_facts = self.type_checker.get_facts_from_typeof_switch(
                fixed_clause_start,
                fixed_clause_end,
                &witnesses,
                has_default_clause,
            );
        } else {
            clause_witnesses = switch_witnesses[clause_start..clause_end]
                .into_iter()
                .map(|witness| witness.clone().unwrap())
                .collect::<Vec<_>>();
            switch_facts = self.type_checker.get_facts_from_typeof_switch(
                clause_start,
                clause_end,
                &switch_witnesses
                    .iter()
                    .map(|witness| witness.clone().unwrap())
                    .collect::<Vec<_>>(),
                has_default_clause,
            );
        }
        if has_default_clause {
            return self.type_checker.try_filter_type(type_, |t: Id<Type>| {
                Ok(self.type_checker.get_type_facts(t, None)? & switch_facts == switch_facts)
            });
        }
        let implied_type = self.type_checker.get_type_with_facts(
            self.type_checker.get_union_type(
                &clause_witnesses
                    .iter()
                    .map(|text| -> io::Result<_> {
                        Ok(self
                            .get_implied_type_from_typeof_guard(type_, text)?
                            .unwrap_or_else(|| type_))
                    })
                    .collect::<Result<Vec<_>, _>>()?,
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?,
            switch_facts,
        )?;
        let mut callback_returning_non_optional = self.narrow_union_member_by_typeof(implied_type);
        self.type_checker.get_type_with_facts(
            self.type_checker
                .try_map_type(
                    type_,
                    &mut move |candidate: Id<Type>| {
                        Ok(Some(callback_returning_non_optional(candidate)?))
                    },
                    None,
                )?
                .unwrap(),
            switch_facts,
        )
    }

    pub(super) fn is_matching_constructor_reference(
        &self,
        expr: &Node, /*Expression*/
    ) -> io::Result<bool> {
        Ok((is_property_access_expression(expr)
            && id_text(&expr.as_property_access_expression().name) == "constructor"
            || is_element_access_expression(expr) && {
                let expr_as_element_access_expression = expr.as_element_access_expression();
                is_string_literal_like(&expr_as_element_access_expression.argument_expression)
                    && &**expr_as_element_access_expression
                        .argument_expression
                        .as_literal_like_node()
                        .text()
                        == "constructor"
            })
            && self
                .type_checker
                .is_matching_reference(&self.reference, &expr.as_has_expression().expression())?)
    }
}
