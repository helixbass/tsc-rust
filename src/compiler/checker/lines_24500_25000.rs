#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{GetFlowTypeOfReference, TypeFacts};
use crate::{
    are_option_rcs_equal, find, map, Node, Signature, SignatureKind, Symbol, SyntaxKind, Type,
    TypePredicate, UnionOrIntersectionTypeInterface, __String, get_object_flags, ObjectFlags,
    TypeFlags, TypeInterface,
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
