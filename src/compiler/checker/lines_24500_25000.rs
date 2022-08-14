#![allow(non_upper_case_globals)]

use std::ptr;
use std::rc::Rc;

use super::{typeof_eq_facts, typeof_ne_facts, GetFlowTypeOfReference, TypeFacts};
use crate::{
    contains_rc, escape_leading_underscores, every, find_index, has_static_modifier, id_text,
    is_element_access_expression, is_private_identifier, is_property_access_expression,
    is_string_literal_like, Debug_, SymbolFlags, SymbolInterface, SyntaxKind, __String,
    are_rc_slices_equal, is_access_expression, is_optional_chain, map, same_map, Node,
    NodeInterface, Symbol, Type, TypeFlags, TypeInterface, TypePredicate,
    UnionOrIntersectionTypeInterface, UnionReduction,
};

impl GetFlowTypeOfReference {
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
