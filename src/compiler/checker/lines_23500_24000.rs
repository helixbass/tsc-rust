#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use crate::{
    is_parameter_or_catch_clause_variable, maybe_every, FlowFlags, FlowNode, FlowNodeBase, Node,
    NodeInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
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
        unimplemented!()
    }
}
