#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::{Cell, RefCell, RefMut};
use std::rc::Rc;

use super::TypeFacts;
use crate::{
    get_assignment_target_kind, get_declared_expando_initializer, get_object_flags, is_in_js_file,
    is_parameter_or_catch_clause_variable, is_var_const, is_variable_declaration, maybe_every,
    skip_parentheses, AssignmentKind, FlowFlags, FlowNode, FlowNodeBase, FlowType, Node,
    NodeInterface, ObjectFlags, Symbol, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    TypePredicate, TypePredicateKind,
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
    shared_flow_start: Cell<Option<usize>>,
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
            shared_flow_start: Cell::new(None),
        }
    }

    pub(super) fn maybe_key(&self) -> RefMut<Option<String>> {
        self.key.borrow_mut()
    }

    pub(super) fn is_key_set(&self) -> bool {
        self.is_key_set.get()
    }

    pub(super) fn set_is_key_set(&self, is_key_set: bool) {
        self.is_key_set.set(is_key_set);
    }

    pub(super) fn flow_depth(&self) -> usize {
        self.flow_depth.get()
    }

    pub(super) fn set_flow_depth(&self, flow_depth: usize) {
        self.flow_depth.set(flow_depth);
    }

    pub(super) fn shared_flow_start(&self) -> usize {
        self.shared_flow_start.get().unwrap()
    }

    pub(super) fn set_shared_flow_start(&self, shared_flow_start: usize) {
        self.shared_flow_start.set(Some(shared_flow_start));
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
        self.set_shared_flow_start(self.type_checker.shared_flow_count());
        let evolved_type = self.type_checker.get_type_from_flow_type(
            &self.get_type_at_flow_node(self.reference.maybe_flow_node().clone().unwrap()),
        );
        self.type_checker
            .set_shared_flow_count(self.shared_flow_start());
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

    pub(super) fn get_or_set_cache_key(&self) -> Option<String> {
        if self.is_key_set() {
            return self.maybe_key().clone();
        }
        self.set_is_key_set(true);
        let ret = self.type_checker.get_flow_cache_key(
            &self.reference,
            &self.declared_type,
            &self.initial_type,
            self.flow_container.as_deref(),
        );
        *self.maybe_key() = ret.clone();
        ret
    }

    pub(super) fn get_type_at_flow_node(&self, mut flow: Rc<FlowNode>) -> FlowType {
        if self.flow_depth() == 2000 {
            // tracing?.instant(tracing.Phase.CheckTypes, "getTypeAtFlowNode_DepthLimit", { flowId: flow.id });
            self.type_checker.set_flow_analysis_disabled(true);
            self.type_checker.report_flow_control_error(&self.reference);
            return self.type_checker.error_type().into();
        }
        self.set_flow_depth(self.flow_depth() + 1);
        let mut shared_flow: Option<Rc<FlowNode>> = None;
        loop {
            let flags = flow.flags();
            if flags.intersects(FlowFlags::Shared) {
                for i in self.shared_flow_start()..self.type_checker.shared_flow_count() {
                    if matches!(
                        self.type_checker.shared_flow_nodes().get(&i),
                        Some(shared_flow_node) if Rc::ptr_eq(
                            shared_flow_node,
                            &flow
                        )
                    ) {
                        self.set_flow_depth(self.flow_depth() - 1);
                        return self
                            .type_checker
                            .shared_flow_types()
                            .get(&i)
                            .map(Clone::clone)
                            .unwrap();
                    }
                }
                shared_flow = Some(flow.clone());
            }
            let mut type_: Option<FlowType> = None;
            if flags.intersects(FlowFlags::Assignment) {
                type_ = self.get_type_at_flow_assignment(flow.clone());
                if type_.is_none() {
                    flow = flow.as_flow_assignment().antecedent.clone();
                    continue;
                }
            } else if flags.intersects(FlowFlags::Call) {
                type_ = self.get_type_at_flow_call(flow.clone());
                if type_.is_none() {
                    flow = flow.as_flow_call().antecedent.clone();
                    continue;
                }
            } else if flags.intersects(FlowFlags::Condition) {
                type_ = Some(self.get_type_at_flow_condition(flow.clone()));
            } else if flags.intersects(FlowFlags::SwitchClause) {
                type_ = Some(self.get_type_at_switch_clause(flow.clone()));
            } else if flags.intersects(FlowFlags::Label) {
                let flow_as_flow_label = flow.as_flow_label();
                if flow_as_flow_label
                    .maybe_antecedents()
                    .as_ref()
                    .unwrap()
                    .len()
                    == 1
                {
                    let new_flow =
                        flow_as_flow_label.maybe_antecedents().as_ref().unwrap()[0].clone();
                    flow = new_flow;
                    continue;
                }
                type_ = Some(if flags.intersects(FlowFlags::BranchLabel) {
                    self.get_type_at_flow_branch_label(flow.clone())
                } else {
                    self.get_type_at_flow_loop_label(flow.clone())
                });
            } else if flags.intersects(FlowFlags::ArrayMutation) {
                type_ = self.get_type_at_flow_array_mutation(flow.clone());
                if type_.is_none() {
                    flow = flow.as_flow_array_mutation().antecedent.clone();
                    continue;
                }
            } else if flags.intersects(FlowFlags::ReduceLabel) {
                let flow_as_flow_reduce_label = flow.as_flow_reduce_label();
                let target = &flow_as_flow_reduce_label.target.as_flow_label();
                let save_antecedents = target.maybe_antecedents().clone();
                *target.maybe_antecedents() = Some(flow_as_flow_reduce_label.antecedents.clone());
                type_ =
                    Some(self.get_type_at_flow_node(flow_as_flow_reduce_label.antecedent.clone()));
                *target.maybe_antecedents() = save_antecedents;
            } else if flags.intersects(FlowFlags::Start) {
                let container = flow.as_flow_start().maybe_node();
                if let Some(container) = container.as_ref().filter(|container| {
                    !matches!(
                        self.flow_container.as_ref(),
                        Some(flow_container) if Rc::ptr_eq(
                            container,
                            flow_container
                        )
                    ) && !matches!(
                        self.reference.kind(),
                        SyntaxKind::PropertyAccessExpression
                            | SyntaxKind::ElementAccessExpression
                            | SyntaxKind::ThisKeyword
                    )
                }) {
                    flow = container.maybe_flow_node().clone().unwrap();
                    continue;
                }
                type_ = Some(self.initial_type.clone().into());
            } else {
                type_ = Some(
                    self.type_checker
                        .convert_auto_to_any(&self.declared_type)
                        .into(),
                );
            }
            if let Some(shared_flow) = shared_flow.as_ref() {
                self.type_checker
                    .shared_flow_nodes()
                    .insert(self.type_checker.shared_flow_count(), shared_flow.clone());
                self.type_checker.shared_flow_types().insert(
                    self.type_checker.shared_flow_count(),
                    type_.clone().unwrap(),
                );
                self.type_checker
                    .set_shared_flow_count(self.type_checker.shared_flow_count() + 1);
            }
            self.set_flow_depth(self.flow_depth() - 1);
            return type_.unwrap();
        }
    }

    pub(super) fn get_initial_or_assigned_type(
        &self,
        flow: &FlowNode, /*FlowAssignment*/
    ) -> Rc<Type> {
        let node = &flow.as_flow_assignment().node;
        self.type_checker.get_narrowable_type_for_reference(
            &*if matches!(
                node.kind(),
                SyntaxKind::VariableDeclaration | SyntaxKind::BindingElement
            ) {
                self.type_checker.get_initial_type(node)
            } else {
                self.type_checker.get_assigned_type(node)
            },
            &self.reference,
            None,
        )
    }

    pub(super) fn get_type_at_flow_assignment(
        &self,
        flow: Rc<FlowNode /*FlowAssignment*/>,
    ) -> Option<FlowType> {
        let flow_as_flow_assignment = flow.as_flow_assignment();
        let node = &flow_as_flow_assignment.node;
        if self
            .type_checker
            .is_matching_reference(&self.reference, node)
        {
            if !self.type_checker.is_reachable_flow_node(flow.clone()) {
                return Some(self.type_checker.unreachable_never_type().into());
            }
            if get_assignment_target_kind(node) == AssignmentKind::Compound {
                let flow_type =
                    self.get_type_at_flow_node(flow_as_flow_assignment.antecedent.clone());
                return Some(self.type_checker.create_flow_type(
                    &self.type_checker.get_base_type_of_literal_type(
                        &self.type_checker.get_type_from_flow_type(&flow_type),
                    ),
                    self.type_checker.is_incomplete(&flow_type),
                ));
            }
            if Rc::ptr_eq(&self.declared_type, &self.type_checker.auto_type())
                || Rc::ptr_eq(&self.declared_type, &self.type_checker.auto_array_type())
            {
                if self.type_checker.is_empty_array_assignment(node) {
                    return Some(
                        self.type_checker
                            .get_evolving_array_type(&self.type_checker.never_type())
                            .into(),
                    );
                }
                let assigned_type = self
                    .type_checker
                    .get_widened_literal_type(&self.get_initial_or_assigned_type(&flow));
                return if self
                    .type_checker
                    .is_type_assignable_to(&assigned_type, &self.declared_type)
                {
                    Some(assigned_type.into())
                } else {
                    Some(self.type_checker.any_array_type().into())
                };
            }
            if self.declared_type.flags().intersects(TypeFlags::Union) {
                return Some(
                    self.type_checker
                        .get_assignment_reduced_type(
                            &self.declared_type,
                            &self.get_initial_or_assigned_type(&flow),
                        )
                        .into(),
                );
            }
            return Some(self.declared_type.clone().into());
        }
        if self
            .type_checker
            .contains_matching_reference(&self.reference, node)
        {
            if !self.type_checker.is_reachable_flow_node(flow.clone()) {
                return Some(self.type_checker.unreachable_never_type().into());
            }
            if is_variable_declaration(node) && (is_in_js_file(Some(&**node)) || is_var_const(node))
            {
                let init = get_declared_expando_initializer(node);
                if matches!(
                    init.as_ref(),
                    Some(init) if matches!(
                        init.kind(),
                        SyntaxKind::FunctionExpression |
                        SyntaxKind::ArrowFunction
                    )
                ) {
                    return Some(
                        self.get_type_at_flow_node(flow_as_flow_assignment.antecedent.clone()),
                    );
                }
            }
            return Some(self.declared_type.clone().into());
        }
        if is_variable_declaration(node)
            && node.parent().parent().kind() == SyntaxKind::ForInStatement
            && self.type_checker.is_matching_reference(
                &self.reference,
                &node.parent().parent().as_for_in_statement().expression,
            )
        {
            return Some(
                self.type_checker
                    .get_non_nullable_type_if_needed(&self.type_checker.get_type_from_flow_type(
                        &self.get_type_at_flow_node(flow_as_flow_assignment.antecedent.clone()),
                    ))
                    .into(),
            );
        }
        return None;
    }

    pub(super) fn narrow_type_by_assertion(
        &self,
        type_: &Type,
        expr: &Node, /*Expression*/
    ) -> Rc<Type> {
        let node = skip_parentheses(expr, Some(true));
        if node.kind() == SyntaxKind::FalseKeyword {
            return self.type_checker.unreachable_never_type();
        }
        if node.kind() == SyntaxKind::BinaryExpression {
            let node_as_binary_expression = node.as_binary_expression();
            if node_as_binary_expression.operator_token.kind()
                == SyntaxKind::AmpersandAmpersandToken
            {
                return self.narrow_type_by_assertion(
                    &self.narrow_type_by_assertion(type_, &node_as_binary_expression.left),
                    &node_as_binary_expression.right,
                );
            }
            if node_as_binary_expression.operator_token.kind() == SyntaxKind::BarBarToken {
                return self.type_checker.get_union_type(
                    vec![
                        self.narrow_type_by_assertion(type_, &node_as_binary_expression.left),
                        self.narrow_type_by_assertion(type_, &node_as_binary_expression.right),
                    ],
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                );
            }
        }
        self.narrow_type(type_, &node, true)
    }

    pub(super) fn get_type_at_flow_call(
        &self,
        flow: Rc<FlowNode /*FlowCall*/>,
    ) -> Option<FlowType> {
        let flow_as_flow_call = flow.as_flow_call();
        let signature = self
            .type_checker
            .get_effects_signature(&flow_as_flow_call.node);
        if let Some(signature) = signature.as_ref() {
            let predicate = self.type_checker.get_type_predicate_of_signature(signature);
            if let Some(predicate) = predicate.as_ref().filter(|predicate| {
                matches!(
                    predicate.kind,
                    TypePredicateKind::AssertsThis | TypePredicateKind::AssertsIdentifier
                )
            }) {
                let flow_type = self.get_type_at_flow_node(flow_as_flow_call.antecedent.clone());
                let type_ = self.type_checker.finalize_evolving_array_type(
                    &self.type_checker.get_type_from_flow_type(&flow_type),
                );
                let narrowed_type = if predicate.type_.is_some() {
                    self.narrow_type_by_type_predicate(
                        &type_,
                        predicate,
                        &flow_as_flow_call.node,
                        true,
                    )
                } else if predicate.kind == TypePredicateKind::AssertsIdentifier
                    && matches!(
                        predicate.parameter_index,
                        Some(predicate_parameter_index) if predicate_parameter_index < flow_as_flow_call.node.as_call_expression().arguments.len()
                    )
                {
                    self.narrow_type_by_assertion(
                        &type_,
                        &flow_as_flow_call.node.as_call_expression().arguments
                            [predicate.parameter_index.unwrap()],
                    )
                } else {
                    type_.clone()
                };
                return if Rc::ptr_eq(&narrowed_type, &type_) {
                    Some(flow_type)
                } else {
                    Some(self.type_checker.create_flow_type(
                        &narrowed_type,
                        self.type_checker.is_incomplete(&flow_type),
                    ))
                };
            }
            if self
                .type_checker
                .get_return_type_of_signature(signature.clone())
                .flags()
                .intersects(TypeFlags::Never)
            {
                return Some(self.type_checker.unreachable_never_type().into());
            }
        }
        None
    }

    pub(super) fn get_type_at_flow_array_mutation(
        &self,
        flow: Rc<FlowNode /*FlowArrayMutation*/>,
    ) -> Option<FlowType> {
        unimplemented!()
    }

    pub(super) fn get_type_at_flow_condition(
        &self,
        flow: Rc<FlowNode /*FlowCondition*/>,
    ) -> FlowType {
        unimplemented!()
    }

    pub(super) fn get_type_at_switch_clause(
        &self,
        flow: Rc<FlowNode /*FlowSwitchClause*/>,
    ) -> FlowType {
        unimplemented!()
    }

    pub(super) fn get_type_at_flow_branch_label(
        &self,
        flow: Rc<FlowNode /*FlowLabel*/>,
    ) -> FlowType {
        unimplemented!()
    }

    pub(super) fn get_type_at_flow_loop_label(&self, flow: Rc<FlowNode /*FlowLabel*/>) -> FlowType {
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
