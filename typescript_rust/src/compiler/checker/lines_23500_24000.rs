use std::{
    cell::{Cell, RefCell, RefMut},
    collections::HashMap,
    io,
};

use id_arena::Id;

use super::TypeFacts;
use crate::{
    contains, get_assignment_target_kind, get_declared_expando_initializer, get_object_flags,
    impl_has_arena, is_in_js_file, is_parameter_or_catch_clause_variable, is_var_const,
    is_variable_declaration, maybe_every, push_if_unique_eq, released, skip_parentheses, AllArenas,
    AssignmentKind, FlowFlags, FlowNode, FlowNodeBase, FlowType, HasArena, InArena, Node,
    NodeInterface, ObjectFlags, Symbol, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    TypePredicateKind, UnionReduction,
};

impl TypeChecker {
    pub(super) fn is_post_super_flow_node(
        &self,
        mut flow: Id<FlowNode>,
        mut no_cache_check: bool,
    ) -> bool {
        loop {
            let flags = flow.ref_(self).flags();
            if flags.intersects(FlowFlags::Shared) {
                if !no_cache_check {
                    let id = self.get_flow_node_id(&flow.ref_(self));
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
                flow = flow.ref_(self).as_has_antecedent().antecedent();
            } else if flags.intersects(FlowFlags::Call) {
                let flow_ref = flow.ref_(self);
                let flow_as_flow_call = flow_ref.as_flow_call();
                if flow_as_flow_call
                    .node
                    .ref_(self)
                    .as_call_expression()
                    .expression
                    .ref_(self)
                    .kind()
                    == SyntaxKind::SuperKeyword
                {
                    return true;
                }
                flow = flow_as_flow_call.antecedent.clone();
            } else if flags.intersects(FlowFlags::BranchLabel) {
                return maybe_every(
                    flow.ref_(self)
                        .as_flow_label()
                        .maybe_antecedents()
                        .as_deref(),
                    |f: &Id<FlowNode>, _| self.is_post_super_flow_node(f.clone(), false),
                );
            } else if flags.intersects(FlowFlags::LoopLabel) {
                let new_flow = flow
                    .ref_(self)
                    .as_flow_label()
                    .maybe_antecedents()
                    .as_ref()
                    .unwrap()[0]
                    .clone();
                flow = new_flow;
            } else if flags.intersects(FlowFlags::ReduceLabel) {
                let flow_ref = flow.ref_(self);
                let flow_as_flow_reduce_label = flow_ref.as_flow_reduce_label();
                let flow_target_ref = flow_as_flow_reduce_label.target.ref_(self);
                let target_as_flow_label = flow_target_ref.as_flow_label();
                let save_antecedents = target_as_flow_label.maybe_antecedents().clone();
                *target_as_flow_label.maybe_antecedents_mut() =
                    Some(flow_as_flow_reduce_label.antecedents.clone());
                let result = self
                    .is_post_super_flow_node(flow_as_flow_reduce_label.antecedent.clone(), false);
                *target_as_flow_label.maybe_antecedents_mut() = save_antecedents;
                return result;
            } else {
                return flags.intersects(FlowFlags::Unreachable);
            }
        }
    }

    pub(super) fn is_constant_reference(&self, node: Id<Node>) -> io::Result<bool> {
        match node.ref_(self).kind() {
            SyntaxKind::Identifier => {
                let symbol = self.get_resolved_symbol(node)?;
                return Ok(self.is_const_variable(symbol)
                    || is_parameter_or_catch_clause_variable(symbol, self)
                        && !self.is_symbol_assigned(symbol)?);
            }
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                return Ok(self
                    .is_constant_reference(node.ref_(self).as_has_expression().expression())?
                    && self.is_readonly_symbol(
                        self.get_node_links(node)
                            .ref_(self)
                            .resolved_symbol
                            .clone()
                            .unwrap_or_else(|| self.unknown_symbol()),
                    )?);
            }
            _ => (),
        }
        Ok(false)
    }

    pub(super) fn get_flow_type_of_reference(
        &self,
        reference: Id<Node>,
        declared_type: Id<Type>,
        initial_type: Option<Id<Type>>,
        flow_container: Option<Id<Node>>,
    ) -> io::Result<Id<Type>> {
        let initial_type = initial_type.unwrap_or(declared_type);
        GetFlowTypeOfReference::new(
            self.arena_id(),
            reference,
            declared_type,
            initial_type,
            flow_container,
            self,
        )
        .call()
    }
}

pub(super) struct GetFlowTypeOfReference {
    arena: *const AllArenas,
    pub type_checker: Id<TypeChecker>,
    pub reference: Id<Node>,
    pub declared_type: Id<Type>,
    pub initial_type: Id<Type>,
    pub flow_container: Option<Id<Node>>,
    key: RefCell<Option<String>>,
    is_key_set: Cell<bool>,
    flow_depth: Cell<usize>,
    shared_flow_start: Cell<Option<usize>>,
}

impl GetFlowTypeOfReference {
    pub(super) fn new(
        type_checker: Id<TypeChecker>,
        reference: Id<Node>,
        declared_type: Id<Type>,
        initial_type: Id<Type>,
        flow_container: Option<Id<Node>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
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

    pub(super) fn call(&self) -> io::Result<Id<Type>> {
        if self.type_checker.ref_(self).flow_analysis_disabled() {
            return Ok(self.type_checker.ref_(self).error_type());
        }
        if self.reference.ref_(self).maybe_flow_node().is_none() {
            return Ok(self.declared_type.clone());
        }
        self.type_checker
            .ref_(self)
            .set_flow_invocation_count(self.type_checker.ref_(self).flow_invocation_count() + 1);
        self.set_shared_flow_start(self.type_checker.ref_(self).shared_flow_count());
        let evolved_type =
            self.type_checker
                .ref_(self)
                .get_type_from_flow_type(&self.get_type_at_flow_node(
                    released!(self.reference.ref_(self).maybe_flow_node()).unwrap(),
                )?);
        self.type_checker
            .ref_(self)
            .set_shared_flow_count(self.shared_flow_start());
        let result_type = if get_object_flags(&evolved_type.ref_(self))
            .intersects(ObjectFlags::EvolvingArray)
            && self
                .type_checker
                .ref_(self)
                .is_evolving_array_operation_target(self.reference)?
        {
            self.type_checker.ref_(self).auto_array_type()
        } else {
            self.type_checker
                .ref_(self)
                .finalize_evolving_array_type(evolved_type)?
        };
        if result_type == self.type_checker.ref_(self).unreachable_never_type()
            || matches!(
                self.reference.ref_(self).maybe_parent(),
                Some(reference_parent) if reference_parent.ref_(self).kind() == SyntaxKind::NonNullExpression
            ) && !result_type.ref_(self).flags().intersects(TypeFlags::Never)
                && self
                    .type_checker
                    .ref_(self)
                    .get_type_with_facts(result_type, TypeFacts::NEUndefinedOrNull)?
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::Never)
        {
            return Ok(self.declared_type.clone());
        }
        Ok(
            if result_type == self.type_checker.ref_(self).non_null_unknown_type() {
                self.type_checker.ref_(self).unknown_type()
            } else {
                result_type
            },
        )
    }

    pub(super) fn get_or_set_cache_key(&self) -> io::Result<Option<String>> {
        if self.is_key_set() {
            return Ok(self.maybe_key().clone());
        }
        self.set_is_key_set(true);
        let ret = self.type_checker.ref_(self).get_flow_cache_key(
            self.reference,
            self.declared_type,
            self.initial_type,
            self.flow_container,
        )?;
        *self.maybe_key() = ret.clone();
        Ok(ret)
    }

    pub(super) fn get_type_at_flow_node(&self, mut flow: Id<FlowNode>) -> io::Result<FlowType> {
        if self.flow_depth() == 2000 {
            // tracing?.instant(tracing.Phase.CheckTypes, "getTypeAtFlowNode_DepthLimit", { flowId: flow.id });
            self.type_checker
                .ref_(self)
                .set_flow_analysis_disabled(true);
            self.type_checker
                .ref_(self)
                .report_flow_control_error(self.reference);
            return Ok(self.type_checker.ref_(self).error_type().into());
        }
        self.set_flow_depth(self.flow_depth() + 1);
        let mut shared_flow: Option<Id<FlowNode>> = None;
        loop {
            let flags = flow.ref_(self).flags();
            if flags.intersects(FlowFlags::Shared) {
                for i in self.shared_flow_start()..self.type_checker.ref_(self).shared_flow_count()
                {
                    if self
                        .type_checker
                        .ref_(self)
                        .shared_flow_nodes()
                        .get(&i)
                        .copied()
                        == Some(flow)
                    {
                        self.set_flow_depth(self.flow_depth() - 1);
                        return Ok(self
                            .type_checker
                            .ref_(self)
                            .shared_flow_types()
                            .get(&i)
                            .cloned()
                            .unwrap());
                    }
                }
                shared_flow = Some(flow.clone());
            }
            let type_: Option<FlowType>;
            if flags.intersects(FlowFlags::Assignment) {
                type_ = self.get_type_at_flow_assignment(flow.clone())?;
                if type_.is_none() {
                    flow = flow.ref_(self).as_flow_assignment().antecedent.clone();
                    continue;
                }
            } else if flags.intersects(FlowFlags::Call) {
                type_ = self.get_type_at_flow_call(flow.clone())?;
                if type_.is_none() {
                    flow = flow.ref_(self).as_flow_call().antecedent.clone();
                    continue;
                }
            } else if flags.intersects(FlowFlags::Condition) {
                type_ = Some(self.get_type_at_flow_condition(flow.clone())?);
            } else if flags.intersects(FlowFlags::SwitchClause) {
                type_ = Some(self.get_type_at_switch_clause(flow.clone())?);
            } else if flags.intersects(FlowFlags::Label) {
                let flow_ref = flow.ref_(self);
                let flow_as_flow_label = flow_ref.as_flow_label();
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
                    self.get_type_at_flow_branch_label(flow.clone())?
                } else {
                    self.get_type_at_flow_loop_label(flow.clone())?
                });
            } else if flags.intersects(FlowFlags::ArrayMutation) {
                type_ = self.get_type_at_flow_array_mutation(flow.clone())?;
                if type_.is_none() {
                    flow = flow.ref_(self).as_flow_array_mutation().antecedent.clone();
                    continue;
                }
            } else if flags.intersects(FlowFlags::ReduceLabel) {
                let flow_ref = flow.ref_(self);
                let flow_as_flow_reduce_label = flow_ref.as_flow_reduce_label();
                let flow_target_ref = flow_as_flow_reduce_label.target.ref_(self);
                let target = flow_target_ref.as_flow_label();
                let save_antecedents = target.maybe_antecedents().clone();
                *target.maybe_antecedents_mut() =
                    Some(flow_as_flow_reduce_label.antecedents.clone());
                type_ =
                    Some(self.get_type_at_flow_node(flow_as_flow_reduce_label.antecedent.clone())?);
                *target.maybe_antecedents_mut() = save_antecedents;
            } else if flags.intersects(FlowFlags::Start) {
                let container = flow.ref_(self).as_flow_start().maybe_node();
                if let Some(container) = container.filter(|&container| {
                    self.flow_container != Some(container)
                        && !matches!(
                            self.reference.ref_(self).kind(),
                            SyntaxKind::PropertyAccessExpression
                                | SyntaxKind::ElementAccessExpression
                                | SyntaxKind::ThisKeyword
                        )
                }) {
                    flow = container.ref_(self).maybe_flow_node().unwrap();
                    continue;
                }
                type_ = Some(self.initial_type.clone().into());
            } else {
                type_ = Some(
                    self.type_checker
                        .ref_(self)
                        .convert_auto_to_any(self.declared_type)
                        .into(),
                );
            }
            if let Some(shared_flow) = shared_flow.as_ref() {
                self.type_checker.ref_(self).shared_flow_nodes_mut().insert(
                    self.type_checker.ref_(self).shared_flow_count(),
                    shared_flow.clone(),
                );
                self.type_checker.ref_(self).shared_flow_types_mut().insert(
                    self.type_checker.ref_(self).shared_flow_count(),
                    type_.clone().unwrap(),
                );
                self.type_checker
                    .ref_(self)
                    .set_shared_flow_count(self.type_checker.ref_(self).shared_flow_count() + 1);
            }
            self.set_flow_depth(self.flow_depth() - 1);
            return Ok(type_.unwrap());
        }
    }

    pub(super) fn get_initial_or_assigned_type(
        &self,
        flow: &FlowNode, /*FlowAssignment*/
    ) -> io::Result<Id<Type>> {
        let node = flow.as_flow_assignment().node;
        self.type_checker
            .ref_(self)
            .get_narrowable_type_for_reference(
                if matches!(
                    node.ref_(self).kind(),
                    SyntaxKind::VariableDeclaration | SyntaxKind::BindingElement
                ) {
                    self.type_checker.ref_(self).get_initial_type(node)?
                } else {
                    self.type_checker.ref_(self).get_assigned_type(node)?
                },
                self.reference,
                None,
            )
    }

    pub(super) fn get_type_at_flow_assignment(
        &self,
        flow: Id<FlowNode /*FlowAssignment*/>,
    ) -> io::Result<Option<FlowType>> {
        let flow_ref = flow.ref_(self);
        let flow_as_flow_assignment = flow_ref.as_flow_assignment();
        let node = flow_as_flow_assignment.node;
        if self
            .type_checker
            .ref_(self)
            .is_matching_reference(self.reference, node)?
        {
            if !self
                .type_checker
                .ref_(self)
                .is_reachable_flow_node(flow.clone())?
            {
                return Ok(Some(
                    self.type_checker.ref_(self).unreachable_never_type().into(),
                ));
            }
            if get_assignment_target_kind(node, self) == AssignmentKind::Compound {
                let flow_type =
                    self.get_type_at_flow_node(flow_as_flow_assignment.antecedent.clone())?;
                return Ok(Some(
                    self.type_checker.ref_(self).create_flow_type(
                        self.type_checker.ref_(self).get_base_type_of_literal_type(
                            self.type_checker
                                .ref_(self)
                                .get_type_from_flow_type(&flow_type),
                        )?,
                        self.type_checker.ref_(self).is_incomplete(&flow_type),
                    ),
                ));
            }
            if self.declared_type == self.type_checker.ref_(self).auto_type()
                || self.declared_type == self.type_checker.ref_(self).auto_array_type()
            {
                if self.type_checker.ref_(self).is_empty_array_assignment(node) {
                    return Ok(Some(
                        self.type_checker
                            .ref_(self)
                            .get_evolving_array_type(self.type_checker.ref_(self).never_type())
                            .into(),
                    ));
                }
                let assigned_type = self.type_checker.ref_(self).get_widened_literal_type(
                    self.get_initial_or_assigned_type(&flow.ref_(self))?,
                )?;
                return Ok(
                    if self
                        .type_checker
                        .ref_(self)
                        .is_type_assignable_to(assigned_type, self.declared_type)?
                    {
                        Some(assigned_type.into())
                    } else {
                        Some(self.type_checker.ref_(self).any_array_type().into())
                    },
                );
            }
            if self
                .declared_type
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Union)
            {
                return Ok(Some(
                    self.type_checker
                        .ref_(self)
                        .get_assignment_reduced_type(
                            self.declared_type,
                            self.get_initial_or_assigned_type(&flow.ref_(self))?,
                        )?
                        .into(),
                ));
            }
            return Ok(Some(self.declared_type.clone().into()));
        }
        if self
            .type_checker
            .ref_(self)
            .contains_matching_reference(self.reference, node)?
        {
            if !self
                .type_checker
                .ref_(self)
                .is_reachable_flow_node(flow.clone())?
            {
                return Ok(Some(
                    self.type_checker.ref_(self).unreachable_never_type().into(),
                ));
            }
            if is_variable_declaration(&node.ref_(self))
                && (is_in_js_file(Some(&node.ref_(self))) || is_var_const(node, self))
            {
                let init = get_declared_expando_initializer(node, self);
                if matches!(
                    init,
                    Some(init) if matches!(
                        init.ref_(self).kind(),
                        SyntaxKind::FunctionExpression |
                        SyntaxKind::ArrowFunction
                    )
                ) {
                    return Ok(Some(self.get_type_at_flow_node(
                        flow_as_flow_assignment.antecedent.clone(),
                    )?));
                }
            }
            return Ok(Some(self.declared_type.clone().into()));
        }
        if is_variable_declaration(&node.ref_(self))
            && node
                .ref_(self)
                .parent()
                .ref_(self)
                .parent()
                .ref_(self)
                .kind()
                == SyntaxKind::ForInStatement
            && self.type_checker.ref_(self).is_matching_reference(
                self.reference,
                node.ref_(self)
                    .parent()
                    .ref_(self)
                    .parent()
                    .ref_(self)
                    .as_for_in_statement()
                    .expression,
            )?
        {
            return Ok(Some(
                self.type_checker
                    .ref_(self)
                    .get_non_nullable_type_if_needed(
                        self.type_checker.ref_(self).get_type_from_flow_type(
                            &self.get_type_at_flow_node(
                                flow_as_flow_assignment.antecedent.clone(),
                            )?,
                        ),
                    )?
                    .into(),
            ));
        }
        Ok(None)
    }

    pub(super) fn narrow_type_by_assertion(
        &self,
        type_: Id<Type>,
        expr: Id<Node>, /*Expression*/
    ) -> io::Result<Id<Type>> {
        let node = skip_parentheses(expr, Some(true), self);
        if node.ref_(self).kind() == SyntaxKind::FalseKeyword {
            return Ok(self.type_checker.ref_(self).unreachable_never_type());
        }
        if node.ref_(self).kind() == SyntaxKind::BinaryExpression {
            let node_ref = node.ref_(self);
            let node_as_binary_expression = node_ref.as_binary_expression();
            if node_as_binary_expression.operator_token.ref_(self).kind()
                == SyntaxKind::AmpersandAmpersandToken
            {
                return self.narrow_type_by_assertion(
                    self.narrow_type_by_assertion(type_, node_as_binary_expression.left)?,
                    node_as_binary_expression.right,
                );
            }
            if node_as_binary_expression.operator_token.ref_(self).kind() == SyntaxKind::BarBarToken
            {
                return self.type_checker.ref_(self).get_union_type(
                    &[
                        self.narrow_type_by_assertion(type_, node_as_binary_expression.left)?,
                        self.narrow_type_by_assertion(type_, node_as_binary_expression.right)?,
                    ],
                    None,
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                );
            }
        }
        self.narrow_type(type_, node, true)
    }

    pub(super) fn get_type_at_flow_call(
        &self,
        flow: Id<FlowNode /*FlowCall*/>,
    ) -> io::Result<Option<FlowType>> {
        let flow_ref = flow.ref_(self);
        let flow_as_flow_call = flow_ref.as_flow_call();
        let signature = self
            .type_checker
            .ref_(self)
            .get_effects_signature(flow_as_flow_call.node)?;
        if let Some(signature) = signature {
            let predicate = self
                .type_checker
                .ref_(self)
                .get_type_predicate_of_signature(signature)?;
            if let Some(predicate) = predicate.filter(|predicate| {
                matches!(
                    predicate.ref_(self).kind,
                    TypePredicateKind::AssertsThis | TypePredicateKind::AssertsIdentifier
                )
            }) {
                let flow_type = self.get_type_at_flow_node(flow_as_flow_call.antecedent.clone())?;
                let type_ = self.type_checker.ref_(self).finalize_evolving_array_type(
                    self.type_checker
                        .ref_(self)
                        .get_type_from_flow_type(&flow_type),
                )?;
                let narrowed_type = if predicate.ref_(self).type_.is_some() {
                    self.narrow_type_by_type_predicate(
                        type_,
                        predicate,
                        flow_as_flow_call.node,
                        true,
                    )?
                } else if predicate.ref_(self).kind == TypePredicateKind::AssertsIdentifier
                    && matches!(
                        predicate.ref_(self).parameter_index,
                        Some(predicate_parameter_index) if predicate_parameter_index < flow_as_flow_call.node.ref_(self).as_call_expression().arguments.ref_(self).len()
                    )
                {
                    self.narrow_type_by_assertion(
                        type_,
                        flow_as_flow_call
                            .node
                            .ref_(self)
                            .as_call_expression()
                            .arguments
                            .ref_(self)[predicate.ref_(self).parameter_index.unwrap()],
                    )?
                } else {
                    type_.clone()
                };
                return Ok(if narrowed_type == type_ {
                    Some(flow_type)
                } else {
                    Some(self.type_checker.ref_(self).create_flow_type(
                        narrowed_type,
                        self.type_checker.ref_(self).is_incomplete(&flow_type),
                    ))
                });
            }
            if self
                .type_checker
                .ref_(self)
                .get_return_type_of_signature(signature.clone())?
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Never)
            {
                return Ok(Some(
                    self.type_checker.ref_(self).unreachable_never_type().into(),
                ));
            }
        }
        Ok(None)
    }

    pub(super) fn get_type_at_flow_array_mutation(
        &self,
        flow: Id<FlowNode /*FlowArrayMutation*/>,
    ) -> io::Result<Option<FlowType>> {
        if self.declared_type == self.type_checker.ref_(self).auto_type()
            || self.declared_type == self.type_checker.ref_(self).auto_array_type()
        {
            let flow_ref = flow.ref_(self);
            let flow_as_flow_array_mutation = flow_ref.as_flow_array_mutation();
            let node = &flow_as_flow_array_mutation.node;
            let expr = if node.ref_(self).kind() == SyntaxKind::CallExpression {
                node.ref_(self)
                    .as_call_expression()
                    .expression
                    .ref_(self)
                    .as_property_access_expression()
                    .expression
            } else {
                node.ref_(self)
                    .as_binary_expression()
                    .left
                    .ref_(self)
                    .as_element_access_expression()
                    .expression
            };
            if self.type_checker.ref_(self).is_matching_reference(
                self.reference,
                self.type_checker.ref_(self).get_reference_candidate(expr),
            )? {
                let flow_type =
                    self.get_type_at_flow_node(flow_as_flow_array_mutation.antecedent.clone())?;
                let type_ = self
                    .type_checker
                    .ref_(self)
                    .get_type_from_flow_type(&flow_type);
                if get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::EvolvingArray) {
                    let mut evolved_type = type_.clone();
                    if node.ref_(self).kind() == SyntaxKind::CallExpression {
                        for &arg in &*node.ref_(self).as_call_expression().arguments.ref_(self) {
                            evolved_type = self
                                .type_checker
                                .ref_(self)
                                .add_evolving_array_element_type(evolved_type, arg)?;
                        }
                    } else {
                        let node_ref = node.ref_(self);
                        let node_as_binary_expression = node_ref.as_binary_expression();
                        let index_type = self
                            .type_checker
                            .ref_(self)
                            .get_context_free_type_of_expression(
                                node_as_binary_expression
                                    .left
                                    .ref_(self)
                                    .as_element_access_expression()
                                    .argument_expression,
                            )?;
                        if self.type_checker.ref_(self).is_type_assignable_to_kind(
                            index_type,
                            TypeFlags::NumberLike,
                            None,
                        )? {
                            evolved_type = self
                                .type_checker
                                .ref_(self)
                                .add_evolving_array_element_type(
                                    evolved_type,
                                    node_as_binary_expression.right,
                                )?;
                        }
                    }
                    return Ok(if evolved_type == type_ {
                        Some(flow_type)
                    } else {
                        Some(self.type_checker.ref_(self).create_flow_type(
                            evolved_type,
                            self.type_checker.ref_(self).is_incomplete(&flow_type),
                        ))
                    });
                }
                return Ok(Some(flow_type));
            }
        }
        Ok(None)
    }

    pub(super) fn get_type_at_flow_condition(
        &self,
        flow: Id<FlowNode /*FlowCondition*/>,
    ) -> io::Result<FlowType> {
        let flow_ref = flow.ref_(self);
        let flow_as_flow_condition = flow_ref.as_flow_condition();
        let flow_type = self.get_type_at_flow_node(flow_as_flow_condition.antecedent.clone())?;
        let type_ = self
            .type_checker
            .ref_(self)
            .get_type_from_flow_type(&flow_type);
        if type_.ref_(self).flags().intersects(TypeFlags::Never) {
            return Ok(flow_type);
        }
        let assume_true = flow.ref_(self).flags().intersects(FlowFlags::TrueCondition);
        let non_evolving_type = self
            .type_checker
            .ref_(self)
            .finalize_evolving_array_type(type_)?;
        let narrowed_type =
            self.narrow_type(non_evolving_type, flow_as_flow_condition.node, assume_true)?;
        if narrowed_type == non_evolving_type {
            return Ok(flow_type);
        }
        Ok(self.type_checker.ref_(self).create_flow_type(
            narrowed_type,
            self.type_checker.ref_(self).is_incomplete(&flow_type),
        ))
    }

    pub(super) fn get_type_at_switch_clause(
        &self,
        flow: Id<FlowNode /*FlowSwitchClause*/>,
    ) -> io::Result<FlowType> {
        let flow_ref = flow.ref_(self);
        let flow_as_flow_switch_clause = flow_ref.as_flow_switch_clause();
        let expr = flow_as_flow_switch_clause
            .switch_statement
            .ref_(self)
            .as_switch_statement()
            .expression;
        let flow_type =
            self.get_type_at_flow_node(flow_as_flow_switch_clause.antecedent.clone())?;
        let mut type_ = self
            .type_checker
            .ref_(self)
            .get_type_from_flow_type(&flow_type);
        if self
            .type_checker
            .ref_(self)
            .is_matching_reference(self.reference, expr)?
        {
            type_ = self.narrow_type_by_switch_on_discriminant(
                type_,
                flow_as_flow_switch_clause.switch_statement,
                flow_as_flow_switch_clause.clause_start,
                flow_as_flow_switch_clause.clause_end,
            )?;
        } else if expr.ref_(self).kind() == SyntaxKind::TypeOfExpression
            && self.type_checker.ref_(self).is_matching_reference(
                self.reference,
                expr.ref_(self).as_type_of_expression().expression,
            )?
        {
            type_ = self.narrow_by_switch_on_type_of(
                type_,
                flow_as_flow_switch_clause.switch_statement,
                flow_as_flow_switch_clause.clause_start,
                flow_as_flow_switch_clause.clause_end,
            )?;
        } else {
            if self.type_checker.ref_(self).strict_null_checks {
                if self
                    .type_checker
                    .ref_(self)
                    .optional_chain_contains_reference(expr, self.reference)?
                {
                    type_ = self.narrow_type_by_switch_optional_chain_containment(
                        type_,
                        flow_as_flow_switch_clause.switch_statement,
                        flow_as_flow_switch_clause.clause_start,
                        flow_as_flow_switch_clause.clause_end,
                        |t: Id<Type>| {
                            !t.ref_(self)
                                .flags()
                                .intersects(TypeFlags::Undefined | TypeFlags::Never)
                        },
                    )?;
                } else if expr.ref_(self).kind() == SyntaxKind::TypeOfExpression
                    && self
                        .type_checker
                        .ref_(self)
                        .optional_chain_contains_reference(
                            expr.ref_(self).as_type_of_expression().expression,
                            self.reference,
                        )?
                {
                    type_ = self.narrow_type_by_switch_optional_chain_containment(
                        type_,
                        flow_as_flow_switch_clause.switch_statement,
                        flow_as_flow_switch_clause.clause_start,
                        flow_as_flow_switch_clause.clause_end,
                        |t: Id<Type>| {
                            !t.ref_(self).flags().intersects(TypeFlags::Never)
                                || t.ref_(self).flags().intersects(TypeFlags::StringLiteral)
                                    && t.ref_(self).as_string_literal_type().value == "undefined"
                        },
                    )?;
                }
            }
            let access = self.get_discriminant_property_access(expr, type_)?;
            if let Some(access) = access {
                type_ = self.narrow_type_by_switch_on_discriminant_property(
                    type_,
                    access,
                    flow_as_flow_switch_clause.switch_statement,
                    flow_as_flow_switch_clause.clause_start,
                    flow_as_flow_switch_clause.clause_end,
                )?;
            }
        }
        Ok(self.type_checker.ref_(self).create_flow_type(
            type_,
            self.type_checker.ref_(self).is_incomplete(&flow_type),
        ))
    }

    pub(super) fn get_type_at_flow_branch_label(
        &self,
        flow: Id<FlowNode /*FlowLabel*/>,
    ) -> io::Result<FlowType> {
        let mut antecedent_types: Vec<Id<Type>> = vec![];
        let mut subtype_reduction = false;
        let mut seen_incomplete = false;
        let mut bypass_flow: Option<Id<FlowNode /*FlowSwitchClause*/>> = None;
        let flow_ref = flow.ref_(self);
        let flow_as_flow_label = flow_ref.as_flow_label();
        for antecedent in flow_as_flow_label.maybe_antecedents().as_ref().unwrap() {
            if bypass_flow.is_none()
                && antecedent
                    .ref_(self)
                    .flags()
                    .intersects(FlowFlags::SwitchClause)
                && {
                    let antecedent_ref = antecedent.ref_(self);
                    let antecendent_as_flow_switch_clause = antecedent_ref.as_flow_switch_clause();
                    antecendent_as_flow_switch_clause.clause_start
                        == antecendent_as_flow_switch_clause.clause_end
                }
            {
                bypass_flow = Some(antecedent.clone());
                continue;
            }
            let flow_type = self.get_type_at_flow_node(antecedent.clone())?;
            let type_ = self
                .type_checker
                .ref_(self)
                .get_type_from_flow_type(&flow_type);
            if type_ == self.declared_type && self.declared_type == self.initial_type {
                return Ok(type_.into());
            }
            push_if_unique_eq(&mut antecedent_types, &type_);
            if !self
                .type_checker
                .ref_(self)
                .is_type_subset_of(type_, self.declared_type)?
            {
                subtype_reduction = true;
            }
            if self.type_checker.ref_(self).is_incomplete(&flow_type) {
                seen_incomplete = true;
            }
        }
        if let Some(bypass_flow) = bypass_flow.as_ref() {
            let flow_type = self.get_type_at_flow_node(bypass_flow.clone())?;
            let type_ = self
                .type_checker
                .ref_(self)
                .get_type_from_flow_type(&flow_type);
            if !contains(Some(&antecedent_types), &type_)
                && !self
                    .type_checker
                    .ref_(self)
                    .is_exhaustive_switch_statement(
                        bypass_flow
                            .ref_(self)
                            .as_flow_switch_clause()
                            .switch_statement,
                    )?
            {
                if type_ == self.declared_type && self.declared_type == self.initial_type {
                    return Ok(type_.into());
                }
                antecedent_types.push(type_.clone());
                if !self
                    .type_checker
                    .ref_(self)
                    .is_type_subset_of(type_, self.declared_type)?
                {
                    subtype_reduction = true;
                }
                if self.type_checker.ref_(self).is_incomplete(&flow_type) {
                    seen_incomplete = true;
                }
            }
        }
        Ok(self.type_checker.ref_(self).create_flow_type(
            self.get_union_or_evolving_array_type(
                &antecedent_types,
                if subtype_reduction {
                    UnionReduction::Subtype
                } else {
                    UnionReduction::Literal
                },
            )?,
            seen_incomplete,
        ))
    }

    pub(super) fn get_type_at_flow_loop_label(
        &self,
        flow: Id<FlowNode /*FlowLabel*/>,
    ) -> io::Result<FlowType> {
        let id = self
            .type_checker
            .ref_(self)
            .get_flow_node_id(&flow.ref_(self));
        let cache = {
            let value = self
                .type_checker
                .ref_(self)
                .flow_loop_caches()
                .get(&id)
                .cloned();
            value
        }
        .unwrap_or_else(|| {
            let flow_loop_cache = self.alloc_flow_loop_cache(HashMap::new());
            self.type_checker
                .ref_(self)
                .flow_loop_caches()
                .insert(id, flow_loop_cache.clone());
            flow_loop_cache
        });
        let key = self.get_or_set_cache_key()?;
        if key.is_none() {
            return Ok(self.declared_type.clone().into());
        }
        let key = key.unwrap();
        let cached = cache.ref_(self).get(&key).cloned();
        if let Some(cached) = cached {
            return Ok(cached.into());
        }
        for i in self.type_checker.ref_(self).flow_loop_start()
            ..self.type_checker.ref_(self).flow_loop_count()
        {
            if self
                .type_checker
                .ref_(self)
                .flow_loop_nodes()
                .get(&i)
                .copied()
                == Some(flow)
                && matches!(
                    self.type_checker.ref_(self).flow_loop_keys().get(&i),
                    Some(flow_loop_key) if flow_loop_key == &key
                )
                && !self
                    .type_checker
                    .ref_(self)
                    .flow_loop_types()
                    .get(&i)
                    .unwrap()
                    .is_empty()
            {
                return Ok(self.type_checker.ref_(self).create_flow_type(
                    self.get_union_or_evolving_array_type(
                        &self
                            .type_checker
                            .ref_(self)
                            .flow_loop_types()
                            .get(&i)
                            .cloned()
                            .unwrap(),
                        UnionReduction::Literal,
                    )?,
                    true,
                ));
            }
        }
        let mut antecedent_types: Vec<Id<Type>> = vec![];
        let mut subtype_reduction = false;
        let mut first_antecedent_type: Option<FlowType> = None;
        let flow_ref = flow.ref_(self);
        let flow_as_flow_label = flow_ref.as_flow_label();
        for antecedent in flow_as_flow_label.maybe_antecedents().clone().unwrap() {
            let flow_type: FlowType;
            match first_antecedent_type.as_ref() {
                None => {
                    first_antecedent_type = Some(self.get_type_at_flow_node(antecedent.clone())?);
                    flow_type = first_antecedent_type.clone().unwrap();
                }
                Some(_) => {
                    self.type_checker
                        .ref_(self)
                        .flow_loop_nodes()
                        .insert(self.type_checker.ref_(self).flow_loop_count(), flow.clone());
                    self.type_checker
                        .ref_(self)
                        .flow_loop_keys()
                        .insert(self.type_checker.ref_(self).flow_loop_count(), key.clone());
                    self.type_checker.ref_(self).flow_loop_types().insert(
                        self.type_checker.ref_(self).flow_loop_count(),
                        // TODO: does this need to be a "live" reference to antecedent_types (vs a copy) in order to correctly mimic the Typescript version?
                        antecedent_types.clone(),
                    );
                    self.type_checker
                        .ref_(self)
                        .set_flow_loop_count(self.type_checker.ref_(self).flow_loop_count() + 1);
                    let save_flow_type_cache =
                        self.type_checker.ref_(self).maybe_flow_type_cache().take();
                    flow_type = self.get_type_at_flow_node(antecedent.clone())?;
                    *self.type_checker.ref_(self).maybe_flow_type_cache() = save_flow_type_cache;
                    self.type_checker
                        .ref_(self)
                        .set_flow_loop_count(self.type_checker.ref_(self).flow_loop_count() - 1);
                    let cached = cache.ref_(self).get(&key).cloned();
                    if let Some(cached) = cached {
                        return Ok(cached.into());
                    }
                }
            }
            let type_ = self
                .type_checker
                .ref_(self)
                .get_type_from_flow_type(&flow_type);
            push_if_unique_eq(&mut antecedent_types, &type_);
            if !self
                .type_checker
                .ref_(self)
                .is_type_subset_of(type_, self.declared_type)?
            {
                subtype_reduction = true;
            }
            if type_ == self.declared_type {
                break;
            }
        }
        let result = self.get_union_or_evolving_array_type(
            &antecedent_types,
            if subtype_reduction {
                UnionReduction::Subtype
            } else {
                UnionReduction::Literal
            },
        )?;
        if self
            .type_checker
            .ref_(self)
            .is_incomplete(first_antecedent_type.as_ref().unwrap())
        {
            return Ok(self.type_checker.ref_(self).create_flow_type(result, true));
        }
        cache.ref_mut(self).insert(key, result.clone());
        Ok(result.into())
    }
}

impl_has_arena!(GetFlowTypeOfReference);
