#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::GetFlowTypeOfReference;
use crate::{
    are_option_rcs_equal, Node, SyntaxKind, Type, TypePredicate, __String, get_object_flags,
    ObjectFlags, TypeFlags, TypeInterface,
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
