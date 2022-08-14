#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{GetFlowTypeOfReference, TypeFacts};
use crate::{
    SyntaxKind, __String, are_rc_slices_equal, is_access_expression, is_optional_chain, map,
    same_map, Node, NodeInterface, Symbol, Type, TypeFlags, TypeInterface, TypePredicate,
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

    pub(super) fn narrow_type_by_switch_on_type_of(
        &self,
        type_: &Type,
        switch_statement: &Node, /*SwitchStatement*/
        clause_start: usize,
        clause_end: usize,
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
