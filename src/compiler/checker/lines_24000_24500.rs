#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::{Cell, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

use super::{GetFlowTypeOfReference, TypeFacts};
use crate::{
    contains_rc, get_assignment_target_kind, get_declared_expando_initializer, get_object_flags,
    is_in_js_file, is_parameter_or_catch_clause_variable, is_var_const, is_variable_declaration,
    maybe_every, push_if_unique_rc, skip_parentheses, AssignmentKind, FlowFlags, FlowNode,
    FlowNodeBase, FlowType, Node, NodeInterface, ObjectFlags, Symbol, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface, TypePredicate, TypePredicateKind, UnionReduction,
};

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
