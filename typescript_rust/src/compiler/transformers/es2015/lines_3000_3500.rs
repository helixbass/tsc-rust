use std::io;

use gc::{Gc, GcCell};
use indexmap::IndexMap;

use super::{
    ConvertedLoopState, CopyDirection, HierarchyFacts, Jump, LoopOutParameter,
    LoopOutParameterFlags, TransformES2015,
};
use crate::{
    EmitFlags, Node, NodeArray, NodeExt, NodeInterface, SyntaxKind, TransformFlags, _d, add_range,
    create_member_access_for_property_name, get_all_accessor_declarations, id_text,
    insert_statements_after_standard_prologue, is_binding_pattern, is_block, is_expression,
    is_for_statement, is_omitted_expression, is_property_name, is_statement, map, return_if_none,
    set_emit_flags, set_original_node, start_on_new_line, try_visit_node, Debug_,
    HasInitializerInterface, Matches, NamedDeclarationInterface, NodeCheckFlags,
};

impl TransformES2015 {
    pub(super) fn create_out_variable(&self, p: &LoopOutParameter) -> Gc<Node> {
        self.factory
            .create_variable_declaration(
                Some(p.original_name.clone()),
                None,
                None,
                Some(p.out_param_name.clone()),
            )
            .wrap()
    }

    pub(super) fn create_function_for_initializer_of_for_statement(
        &self,
        node: &Node, /*ForStatementWithConvertibleInitializer*/
        current_state: Gc<GcCell<ConvertedLoopState>>,
    ) -> io::Result<IterationStatementPartFunction<Gc<Node /*VariableDeclarationList*/>>> {
        let node_as_for_statement = node.as_for_statement();
        let function_name = self.factory.create_unique_name("_loop_init", None);

        let node_initializer = node_as_for_statement.initializer.as_ref().unwrap();
        let contains_yield = node_initializer
            .transform_flags()
            .intersects(TransformFlags::ContainsYield);
        let mut emit_flags = EmitFlags::None;
        let current_state = (*current_state).borrow();
        if current_state.contains_lexical_this == Some(true) {
            emit_flags |= EmitFlags::CapturesThis;
        }
        if contains_yield
            && self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::AsyncFunctionBody)
        {
            emit_flags |= EmitFlags::AsyncFunctionBody;
        }

        let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
        statements.push(
            self.factory
                .create_variable_statement(Option::<Gc<NodeArray>>::None, node_initializer.clone())
                .wrap(),
        );
        self.copy_out_parameters(
            &current_state.loop_out_parameters,
            LoopOutParameterFlags::Initializer,
            CopyDirection::ToOutParameter,
            &mut statements,
        );

        let function_declaration = self
            .factory
            .create_variable_statement(
                Option::<Gc<NodeArray>>::None,
                self.factory
                    .create_variable_declaration_list(
                        vec![self
                            .factory
                            .create_variable_declaration(
                                Some(function_name.clone()),
                                None,
                                None,
                                Some(
                                    self.factory
                                        .create_function_expression(
                                            Option::<Gc<NodeArray>>::None,
                                            contains_yield.then(|| {
                                                self.factory
                                                    .create_token(SyntaxKind::AsteriskToken)
                                                    .wrap()
                                            }),
                                            Option::<Gc<Node>>::None,
                                            Option::<Gc<NodeArray>>::None,
                                            Option::<Gc<NodeArray>>::None,
                                            None,
                                            try_visit_node(
                                                &self
                                                    .factory
                                                    .create_block(statements, Some(true))
                                                    .wrap(),
                                                Some(|node: &Node| self.visitor(node)),
                                                Some(is_block),
                                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                            )?,
                                        )
                                        .wrap()
                                        .set_emit_flags(emit_flags),
                                ),
                            )
                            .wrap()],
                        None,
                    )
                    .wrap()
                    .set_emit_flags(EmitFlags::NoHoisting),
            )
            .wrap();

        let part = self
            .factory
            .create_variable_declaration_list(
                map(
                    &current_state.loop_out_parameters,
                    |loop_out_parameter: &LoopOutParameter, _| {
                        self.create_out_variable(loop_out_parameter)
                    },
                ),
                None,
            )
            .wrap();

        Ok(IterationStatementPartFunction {
            function_name,
            contains_yield,
            function_declaration,
            part,
        })
    }

    pub(super) fn create_function_for_body_of_iteration_statement(
        &self,
        node: &Node, /*IterationStatement*/
        current_state: Gc<GcCell<ConvertedLoopState>>,
        outer_state: Option<Gc<GcCell<ConvertedLoopState>>>,
    ) -> io::Result<IterationStatementPartFunction<Vec<Gc<Node /*Statement*/>>>> {
        let function_name = self.factory.create_unique_name("_loop", None);
        self.context.start_lexical_environment();
        let statement = try_visit_node(
            &node.as_has_statement().statement(),
            Some(|node: &Node| self.visitor(node)),
            Some(is_statement),
            Some(&|nodes: &[Gc<Node>]| self.factory.lift_to_block(nodes)),
        )?;
        let lexical_environment = self.context.end_lexical_environment();

        let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
        if self.should_convert_condition_of_for_statement(node)
            || self.should_convert_incrementor_of_for_statement(node)
        {
            current_state.borrow_mut().condition_variable =
                Some(self.factory.create_unique_name("inc", None));
            let node_as_for_statement = node.as_for_statement();
            if let Some(node_incrementor) = node_as_for_statement.incrementor.as_ref() {
                statements.push(
                    self.factory
                        .create_if_statement(
                            (*current_state)
                                .borrow()
                                .condition_variable
                                .clone()
                                .unwrap(),
                            self.factory
                                .create_expression_statement(try_visit_node(
                                    node_incrementor,
                                    Some(|node: &Node| self.visitor(node)),
                                    Some(is_expression),
                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                )?)
                                .wrap(),
                            Some(
                                self.factory
                                    .create_expression_statement(
                                        self.factory
                                            .create_assignment(
                                                (*current_state)
                                                    .borrow()
                                                    .condition_variable
                                                    .clone()
                                                    .unwrap(),
                                                self.factory.create_true().wrap(),
                                            )
                                            .wrap(),
                                    )
                                    .wrap(),
                            ),
                        )
                        .wrap(),
                );
            } else {
                statements.push(
                    self.factory
                        .create_if_statement(
                            self.factory
                                .create_logical_not(
                                    (*current_state)
                                        .borrow()
                                        .condition_variable
                                        .clone()
                                        .unwrap(),
                                )
                                .wrap(),
                            self.factory
                                .create_expression_statement(
                                    self.factory
                                        .create_assignment(
                                            (*current_state)
                                                .borrow()
                                                .condition_variable
                                                .clone()
                                                .unwrap(),
                                            self.factory.create_true().wrap(),
                                        )
                                        .wrap(),
                                )
                                .wrap(),
                            None,
                        )
                        .wrap(),
                );
            }

            if self.should_convert_condition_of_for_statement(node) {
                statements.push(
                    self.factory
                        .create_if_statement(
                            self.factory
                                .create_prefix_unary_expression(
                                    SyntaxKind::ExclamationToken,
                                    try_visit_node(
                                        node_as_for_statement.condition.as_deref().unwrap(),
                                        Some(|node: &Node| self.visitor(node)),
                                        Some(is_expression),
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    )?,
                                )
                                .wrap(),
                            try_visit_node(
                                &self
                                    .factory
                                    .create_break_statement(Option::<Gc<Node>>::None)
                                    .wrap(),
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_statement),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            )?,
                            None,
                        )
                        .wrap(),
                );
            }
        }

        if is_block(&statement) {
            add_range(
                &mut statements,
                Some(&statement.as_block().statements),
                None,
                None,
            );
        } else {
            statements.push(statement.clone());
        }

        self.copy_out_parameters(
            &(*current_state).borrow().loop_out_parameters,
            LoopOutParameterFlags::Body,
            CopyDirection::ToOutParameter,
            &mut statements,
        );
        insert_statements_after_standard_prologue(&mut statements, lexical_environment.as_deref());

        let loop_body = self.factory.create_block(statements, Some(true)).wrap();
        if is_block(&statement) {
            set_original_node(&*loop_body, Some(statement.clone()));
        }

        let contains_yield = node
            .as_has_statement()
            .statement()
            .transform_flags()
            .intersects(TransformFlags::ContainsYield);

        let mut emit_flags = EmitFlags::ReuseTempVariableScope;
        if (*current_state).borrow().contains_lexical_this == Some(true) {
            emit_flags |= EmitFlags::CapturesThis;
        }
        if contains_yield
            && self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::AsyncFunctionBody)
        {
            emit_flags |= EmitFlags::AsyncFunctionBody;
        }

        let function_declaration = self
            .factory
            .create_variable_statement(
                Option::<Gc<NodeArray>>::None,
                self.factory
                    .create_variable_declaration_list(
                        vec![self
                            .factory
                            .create_variable_declaration(
                                Some(function_name.clone()),
                                None,
                                None,
                                Some(
                                    self.factory
                                        .create_function_expression(
                                            Option::<Gc<NodeArray>>::None,
                                            contains_yield.then(|| {
                                                self.factory
                                                    .create_token(SyntaxKind::AsteriskToken)
                                                    .wrap()
                                            }),
                                            Option::<Gc<Node>>::None,
                                            Option::<Gc<NodeArray>>::None,
                                            Some((*current_state).borrow().loop_parameters.clone()),
                                            None,
                                            loop_body,
                                        )
                                        .wrap()
                                        .set_emit_flags(emit_flags),
                                ),
                            )
                            .wrap()],
                        None,
                    )
                    .wrap()
                    .set_emit_flags(EmitFlags::NoHoisting),
            )
            .wrap();

        let part = self.generate_call_to_converted_loop(
            &function_name,
            current_state,
            outer_state,
            contains_yield,
        );

        Ok(IterationStatementPartFunction {
            function_name,
            contains_yield,
            function_declaration,
            part,
        })
    }

    pub(super) fn copy_out_parameter(
        &self,
        out_param: &LoopOutParameter,
        copy_direction: CopyDirection,
    ) -> Gc<Node /*BinaryExpression*/> {
        let source = if copy_direction == CopyDirection::ToOriginal {
            out_param.out_param_name.clone()
        } else {
            out_param.original_name.clone()
        };
        let target = if copy_direction == CopyDirection::ToOriginal {
            out_param.original_name.clone()
        } else {
            out_param.out_param_name.clone()
        };
        self.factory
            .create_binary_expression(target, SyntaxKind::EqualsToken, source)
            .wrap()
    }

    pub(super) fn copy_out_parameters(
        &self,
        out_params: &[LoopOutParameter],
        part_flags: LoopOutParameterFlags,
        copy_direction: CopyDirection,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
    ) {
        for out_param in out_params {
            if out_param.flags.intersects(part_flags) {
                statements.push(
                    self.factory
                        .create_expression_statement(
                            self.copy_out_parameter(out_param, copy_direction),
                        )
                        .wrap(),
                );
            }
        }
    }

    pub(super) fn generate_call_to_converted_loop_initializer(
        &self,
        init_function_expression_name: &Node, /*Identifier*/
        contains_yield: bool,
    ) -> Gc<Node /*Statement*/> {
        let call = self
            .factory
            .create_call_expression(
                init_function_expression_name.node_wrapper(),
                Option::<Gc<NodeArray>>::None,
                Some(vec![]),
            )
            .wrap();
        let call_result = if contains_yield {
            self.factory
                .create_yield_expression(
                    Some(self.factory.create_token(SyntaxKind::AsteriskToken).wrap()),
                    Some(set_emit_flags(call, EmitFlags::Iterator)),
                )
                .wrap()
        } else {
            call
        };
        self.factory.create_expression_statement(call_result).wrap()
    }

    pub(super) fn generate_call_to_converted_loop(
        &self,
        loop_function_expression_name: &Node, /*Identifier*/
        state: Gc<GcCell<ConvertedLoopState>>,
        outer_state: Option<Gc<GcCell<ConvertedLoopState>>>,
        contains_yield: bool,
    ) -> Vec<Gc<Node /*Statement*/>> {
        let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
        let state = (*state).borrow();
        let is_simple_loop = !state
            .non_local_jumps
            .unwrap_or_default()
            .intersects(!Jump::Continue)
            && state.labeled_non_local_breaks.is_none()
            && state.labeled_non_local_continues.is_none();

        let call = self
            .factory
            .create_call_expression(
                loop_function_expression_name.node_wrapper(),
                Option::<Gc<NodeArray>>::None,
                Some(map(&state.loop_parameters, |p: &Gc<Node>, _| {
                    p.as_parameter_declaration().name()
                })),
            )
            .wrap();
        let call_result = if contains_yield {
            self.factory
                .create_yield_expression(
                    Some(self.factory.create_token(SyntaxKind::AsteriskToken).wrap()),
                    Some(set_emit_flags(call, EmitFlags::Iterator)),
                )
                .wrap()
        } else {
            call
        };
        if is_simple_loop {
            statements.push(
                self.factory
                    .create_expression_statement(call_result.clone())
                    .wrap(),
            );
            self.copy_out_parameters(
                &state.loop_out_parameters,
                LoopOutParameterFlags::Body,
                CopyDirection::ToOriginal,
                &mut statements,
            );
        } else {
            let loop_result_name = self.factory.create_unique_name("state", None);
            let state_variable = self
                .factory
                .create_variable_statement(
                    Option::<Gc<NodeArray>>::None,
                    self.factory
                        .create_variable_declaration_list(
                            vec![self
                                .factory
                                .create_variable_declaration(
                                    Some(loop_result_name.clone()),
                                    None,
                                    None,
                                    Some(call_result),
                                )
                                .wrap()],
                            None,
                        )
                        .wrap(),
                )
                .wrap();
            statements.push(state_variable);
            self.copy_out_parameters(
                &state.loop_out_parameters,
                LoopOutParameterFlags::Body,
                CopyDirection::ToOriginal,
                &mut statements,
            );

            if state
                .non_local_jumps
                .unwrap_or_default()
                .intersects(Jump::Return)
            {
                let return_statement: Gc<Node /*ReturnStatement*/>;
                if let Some(outer_state) = outer_state.as_ref() {
                    *outer_state
                        .borrow_mut()
                        .non_local_jumps
                        .get_or_insert_with(|| _d()) |= Jump::Return;
                    return_statement = self
                        .factory
                        .create_return_statement(Some(loop_result_name.clone()))
                        .wrap();
                } else {
                    return_statement = self
                        .factory
                        .create_return_statement(Some(
                            self.factory
                                .create_property_access_expression(
                                    loop_result_name.clone(),
                                    "value",
                                )
                                .wrap(),
                        ))
                        .wrap();
                }
                statements.push(
                    self.factory
                        .create_if_statement(
                            self.factory
                                .create_type_check(loop_result_name.clone(), "object"),
                            return_statement,
                            None,
                        )
                        .wrap(),
                );
            }

            if state
                .non_local_jumps
                .unwrap_or_default()
                .intersects(Jump::Break)
            {
                statements.push(
                    self.factory
                        .create_if_statement(
                            self.factory
                                .create_strict_equality(
                                    loop_result_name.clone(),
                                    self.factory
                                        .create_string_literal("break".to_owned(), None, None)
                                        .wrap(),
                                )
                                .wrap(),
                            self.factory
                                .create_break_statement(Option::<Gc<Node>>::None)
                                .wrap(),
                            None,
                        )
                        .wrap(),
                );
            }

            if state.labeled_non_local_breaks.is_some()
                || state.labeled_non_local_continues.is_some()
            {
                let mut case_clauses: Vec<Gc<Node /*CaseClause*/>> = _d();
                self.process_labeled_jumps(
                    state.labeled_non_local_breaks.as_ref(),
                    true,
                    &loop_result_name,
                    outer_state.clone(),
                    &mut case_clauses,
                );
                self.process_labeled_jumps(
                    state.labeled_non_local_continues.as_ref(),
                    false,
                    &loop_result_name,
                    outer_state.clone(),
                    &mut case_clauses,
                );
                statements.push(
                    self.factory
                        .create_switch_statement(
                            loop_result_name,
                            self.factory.create_case_block(case_clauses).wrap(),
                        )
                        .wrap(),
                );
            }
        }
        statements
    }

    pub(super) fn set_labeled_jump(
        &self,
        state: &mut ConvertedLoopState,
        is_break: bool,
        label_text: String,
        label_marker: String,
    ) {
        if is_break {
            state
                .labeled_non_local_breaks
                .get_or_insert_with(|| _d())
                .insert(label_text, label_marker);
        } else {
            state
                .labeled_non_local_continues
                .get_or_insert_with(|| _d())
                .insert(label_text, label_marker);
        }
    }

    pub(super) fn process_labeled_jumps(
        &self,
        table: Option<&IndexMap<String, String>>,
        is_break: bool,
        loop_result_name: &Node, /*Identifier*/
        outer_loop: Option<Gc<GcCell<ConvertedLoopState>>>,
        case_clauses: &mut Vec<Gc<Node /*CaseClause*/>>,
    ) {
        let table = return_if_none!(table);
        for (label_text, label_marker) in table {
            let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
            if match outer_loop.as_ref() {
                None => true,
                Some(outer_loop) => {
                    (**outer_loop)
                        .borrow()
                        .labels
                        .as_ref()
                        .matches(|outer_loop_labels| {
                            outer_loop_labels.get(label_text).copied() == Some(true)
                        })
                }
            } {
                let label = self
                    .factory
                    .create_identifier(label_text, Option::<Gc<NodeArray>>::None, None)
                    .wrap();
                statements.push(if is_break {
                    self.factory.create_break_statement(Some(label)).wrap()
                } else {
                    self.factory.create_continue_statement(Some(label)).wrap()
                });
            } else {
                self.set_labeled_jump(
                    &mut outer_loop.as_ref().unwrap().borrow_mut(),
                    is_break,
                    label_text.clone(),
                    label_marker.clone(),
                );
                statements.push(
                    self.factory
                        .create_return_statement(Some(loop_result_name.node_wrapper()))
                        .wrap(),
                );
            }
            case_clauses.push(
                self.factory
                    .create_case_clause(
                        self.factory
                            .create_string_literal(label_marker.clone(), None, None)
                            .wrap(),
                        statements,
                    )
                    .wrap(),
            );
        }
    }

    pub(super) fn process_loop_variable_declaration(
        &self,
        container: &Node, /*IterationStatement*/
        decl: &Node,      /*VariableDeclaration | BindingElement*/
        loop_parameters: &mut Vec<Gc<Node /*ParameterDeclaration*/>>,
        loop_out_parameters: &mut Vec<LoopOutParameter>,
        has_captured_bindings_in_for_initializer: bool,
    ) -> io::Result<()> {
        let ref name = decl.as_named_declaration().name();
        if is_binding_pattern(Some(&**name)) {
            for element in &name.as_has_elements().elements() {
                if !is_omitted_expression(element) {
                    self.process_loop_variable_declaration(
                        container,
                        element,
                        loop_parameters,
                        loop_out_parameters,
                        has_captured_bindings_in_for_initializer,
                    )?;
                }
            }
        } else {
            loop_parameters.push(
                self.factory
                    .create_parameter_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        None,
                        Some(name.clone()),
                        None,
                        None,
                        None,
                    )
                    .wrap(),
            );
            let check_flags = self.resolver.get_node_check_flags(decl);
            if check_flags.intersects(NodeCheckFlags::NeedsLoopOutParameter)
                || has_captured_bindings_in_for_initializer
            {
                let out_param_name = self
                    .factory
                    .create_unique_name(&format!("out_{}", id_text(name)), None);
                let mut flags = LoopOutParameterFlags::None;
                if check_flags.intersects(NodeCheckFlags::NeedsLoopOutParameter) {
                    flags |= LoopOutParameterFlags::Body;
                }
                if is_for_statement(container)
                    && container
                        .as_for_statement()
                        .initializer
                        .as_ref()
                        .try_matches(|container_initializer| {
                            self.resolver
                                .is_binding_captured_by_node(container_initializer, decl)
                        })?
                {
                    flags |= LoopOutParameterFlags::Initializer;
                }
                loop_out_parameters.push(LoopOutParameter {
                    flags,
                    original_name: name.clone(),
                    out_param_name,
                });
            }
        }

        Ok(())
    }

    pub(super) fn add_object_literal_members(
        &self,
        expressions: &mut Vec<Gc<Node /*Expression*/>>,
        node: &Node,     /*ObjectLiteralExpression*/
        receiver: &Node, /*Identifier*/
        start: usize,
    ) -> io::Result<()> {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        let properties = &node_as_object_literal_expression.properties;
        for property in properties.iter().skip(start) {
            match property.kind() {
                SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                    let accessors = get_all_accessor_declarations(
                        &node_as_object_literal_expression.properties,
                        property,
                    );
                    if Gc::ptr_eq(property, &accessors.first_accessor) {
                        expressions.push(self.transform_accessors_to_expression(
                            receiver,
                            &accessors,
                            node,
                            node_as_object_literal_expression.multi_line == Some(true),
                        )?);
                    }
                }
                SyntaxKind::MethodDeclaration => {
                    expressions.push(
                        self.transform_object_literal_method_declaration_to_expression(
                            property,
                            receiver,
                            node,
                            node_as_object_literal_expression.multi_line,
                        )?,
                    );
                }
                SyntaxKind::PropertyAssignment => {
                    expressions.push(self.transform_property_assignment_to_expression(
                        property,
                        receiver,
                        node_as_object_literal_expression.multi_line,
                    )?);
                }
                SyntaxKind::ShorthandPropertyAssignment => {
                    expressions.push(self.transform_shorthand_property_assignment_to_expression(
                        property,
                        receiver,
                        node_as_object_literal_expression.multi_line,
                    )?);
                }
                _ => Debug_.fail_bad_syntax_kind(node, None),
            }
        }

        Ok(())
    }

    pub(super) fn transform_property_assignment_to_expression(
        &self,
        property: &Node, /*PropertyAssignment*/
        receiver: &Node, /*Expression*/
        starts_on_new_line: Option<bool>,
    ) -> io::Result<Gc<Node>> {
        let property_as_property_assignment = property.as_property_assignment();
        let expression = self
            .factory
            .create_assignment(
                create_member_access_for_property_name(
                    &self.factory,
                    receiver,
                    &*try_visit_node(
                        &property_as_property_assignment.name(),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_property_name),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                    Option::<&Node>::None,
                ),
                try_visit_node(
                    &property_as_property_assignment.maybe_initializer().unwrap(),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )?,
            )
            .wrap()
            .set_text_range(Some(property));
        if starts_on_new_line == Some(true) {
            start_on_new_line(&*expression);
        }
        Ok(expression)
    }

    pub(super) fn transform_shorthand_property_assignment_to_expression(
        &self,
        property: &Node, /*ShorthandPropertyAssignment*/
        receiver: &Node, /*Expression*/
        starts_on_new_line: Option<bool>,
    ) -> io::Result<Gc<Node>> {
        let property_as_shorthand_property_assignment = property.as_shorthand_property_assignment();
        let expression = self
            .factory
            .create_assignment(
                create_member_access_for_property_name(
                    &self.factory,
                    receiver,
                    &*try_visit_node(
                        &property_as_shorthand_property_assignment.name(),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_property_name),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                    Option::<&Node>::None,
                ),
                self.factory
                    .clone_node(&property_as_shorthand_property_assignment.name()),
            )
            .wrap()
            .set_text_range(Some(property));
        if starts_on_new_line == Some(true) {
            start_on_new_line(&*expression);
        }
        Ok(expression)
    }
}

pub(super) struct IterationStatementPartFunction<TPart> {
    pub function_name: Gc<Node /*Identifier*/>,
    pub function_declaration: Gc<Node /*Statement*/>,
    pub contains_yield: bool,
    pub part: TPart,
}
