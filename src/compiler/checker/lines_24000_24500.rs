#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::GetFlowTypeOfReference;
use crate::{Node, Type, TypePredicate, UnionReduction};

impl GetFlowTypeOfReference {
    pub(super) fn get_union_or_evolving_array_type(
        &self,
        types: &[Rc<Type>],
        subtype_reduction: UnionReduction,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_discriminant_property_access(
        &self,
        expr: &Node, /*Expression*/
        computed_type: &Type,
    ) -> Option<Rc<Node>> {
        unimplemented!()
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
