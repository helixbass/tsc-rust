#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

use super::TypeFacts;
use crate::{
    get_object_flags, is_parameter_or_catch_clause_variable, maybe_every, FlowFlags, FlowNode,
    FlowNodeBase, FlowType, Node, NodeInterface, ObjectFlags, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn is_post_super_flow_node(
        &self,
        mut flow: Rc<FlowNode>,
        mut no_cache_check: bool,
    ) -> bool {
        loop {
            let flags = flow.flags();
            if flags.intersects(FlowFlags::Shared) {
                if !no_cache_check {
                    let id = self.get_flow_node_id(&flow);
                    let post_super = self.flow_node_post_super().get(&id).copied();
                    return match post_super {
                        Some(post_super) => post_super,
                        None => {
                            let ret = self.is_post_super_flow_node(flow.clone(), true);
                            self.flow_node_post_super().insert(id, ret);
                            return ret;
                        }
                    };
                }
                no_cache_check = false;
            }
            if flags.intersects(
                FlowFlags::Assignment
                    | FlowFlags::Condition
                    | FlowFlags::ArrayMutation
                    | FlowFlags::SwitchClause,
            ) {
                flow = flow.as_has_antecedent().antecedent();
            } else if flags.intersects(FlowFlags::Call) {
                let flow_as_flow_call = flow.as_flow_call();
                if flow_as_flow_call
                    .node
                    .as_call_expression()
                    .expression
                    .kind()
                    == SyntaxKind::SuperKeyword
                {
                    return true;
                }
                flow = flow_as_flow_call.antecedent.clone();
            } else if flags.intersects(FlowFlags::BranchLabel) {
                return maybe_every(
                    flow.as_flow_label().maybe_antecedents().as_deref(),
                    |f: &Rc<FlowNode>, _| self.is_post_super_flow_node(f.clone(), false),
                );
            } else if flags.intersects(FlowFlags::LoopLabel) {
                let new_flow =
                    flow.as_flow_label().maybe_antecedents().as_ref().unwrap()[0].clone();
                flow = new_flow;
            } else if flags.intersects(FlowFlags::ReduceLabel) {
                let flow_as_flow_reduce_label = flow.as_flow_reduce_label();
                let target_as_flow_label = flow_as_flow_reduce_label.target.as_flow_label();
                let save_antecedents = target_as_flow_label.maybe_antecedents().clone();
                *target_as_flow_label.maybe_antecedents() =
                    Some(flow_as_flow_reduce_label.antecedents.clone());
                let result = self
                    .is_post_super_flow_node(flow_as_flow_reduce_label.antecedent.clone(), false);
                *target_as_flow_label.maybe_antecedents() = save_antecedents;
                return result;
            } else {
                return flags.intersects(FlowFlags::Unreachable);
            }
        }
    }

    pub(super) fn is_constant_reference(&self, node: &Node) -> bool {
        match node.kind() {
            SyntaxKind::Identifier => {
                let symbol = self.get_resolved_symbol(node);
                return self.is_const_variable(&symbol)
                    || is_parameter_or_catch_clause_variable(&symbol)
                        && !self.is_symbol_assigned(&symbol);
            }
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                return self.is_constant_reference(&node.as_has_expression().expression())
                    && self.is_readonly_symbol(
                        &(*self.get_node_links(node))
                            .borrow()
                            .resolved_symbol
                            .clone()
                            .unwrap_or_else(|| self.unknown_symbol()),
                    );
            }
            _ => (),
        }
        false
    }

    pub(super) fn get_flow_type_of_reference<
        TInitialType: Borrow<Type>,
        TFlowContainer: Borrow<Node>,
    >(
        &self,
        reference: &Node,
        declared_type: &Type,
        initial_type: Option<TInitialType>,
        flow_container: Option<TFlowContainer>,
    ) -> Rc<Type> {
        let initial_type = initial_type.map_or_else(
            || declared_type.type_wrapper(),
            |initial_type| initial_type.borrow().type_wrapper(),
        );
        GetFlowTypeOfReference::new(
            self.rc_wrapper(),
            reference.node_wrapper(),
            declared_type.type_wrapper(),
            initial_type,
            flow_container.map(|flow_container| flow_container.borrow().node_wrapper()),
        )
        .call()
    }
}

pub(super) struct GetFlowTypeOfReference {
    pub type_checker: Rc<TypeChecker>,
    pub reference: Rc<Node>,
    pub declared_type: Rc<Type>,
    pub initial_type: Rc<Type>,
    pub flow_container: Option<Rc<Node>>,
    key: RefCell<Option<String>>,
    is_key_set: Cell<bool>,
    flow_depth: Cell<usize>,
}

impl GetFlowTypeOfReference {
    pub(super) fn new(
        type_checker: Rc<TypeChecker>,
        reference: Rc<Node>,
        declared_type: Rc<Type>,
        initial_type: Rc<Type>,
        flow_container: Option<Rc<Node>>,
    ) -> Self {
        Self {
            type_checker,
            reference,
            declared_type,
            initial_type,
            flow_container,
            key: RefCell::new(None),
            is_key_set: Cell::new(false),
            flow_depth: Cell::new(0),
        }
    }

    pub(super) fn call(&self) -> Rc<Type> {
        if self.type_checker.flow_analysis_disabled() {
            return self.type_checker.error_type();
        }
        if self.reference.maybe_flow_node().is_none() {
            return self.declared_type.clone();
        }
        self.type_checker
            .set_flow_invocation_count(self.type_checker.flow_invocation_count() + 1);
        let shared_flow_start = self.type_checker.shared_flow_count();
        let evolved_type = self.type_checker.get_type_from_flow_type(
            &self.get_type_at_flow_node(self.reference.maybe_flow_node().as_ref().unwrap()),
        );
        self.type_checker.set_shared_flow_count(shared_flow_start);
        let result_type = if get_object_flags(&evolved_type).intersects(ObjectFlags::EvolvingArray)
            && self
                .type_checker
                .is_evolving_array_operation_target(&self.reference)
        {
            self.type_checker.auto_array_type()
        } else {
            self.type_checker
                .finalize_evolving_array_type(&evolved_type)
        };
        if Rc::ptr_eq(&result_type, &self.type_checker.unreachable_never_type())
            || matches!(
                self.reference.maybe_parent().as_ref(),
                Some(reference_parent) if reference_parent.kind() == SyntaxKind::NonNullExpression
            ) && !result_type.flags().intersects(TypeFlags::Never)
                && self
                    .type_checker
                    .get_type_with_facts(&result_type, TypeFacts::NEUndefinedOrNull)
                    .flags()
                    .intersects(TypeFlags::Never)
        {
            return self.declared_type.clone();
        }
        if Rc::ptr_eq(&result_type, &self.type_checker.non_null_unknown_type()) {
            self.type_checker.unknown_type()
        } else {
            result_type
        }
    }

    pub(super) fn get_type_at_flow_node(&self, flow: &FlowNode) -> FlowType {
        unimplemented!()
    }
}
