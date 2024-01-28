use std::io;

use gc::{Gc, GcCell};
use id_arena::Id;
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
    GetOrInsertDefault, HasInitializerInterface, Matches, NamedDeclarationInterface,
    NodeCheckFlags,
    InArena,
    CoreTransformationContext,
};

impl TransformES2015 {
    pub(super) fn create_out_variable(&self, p: &LoopOutParameter) -> Id<Node> {
        self.factory.ref_(self).create_variable_declaration(
            Some(p.original_name.clone()),
            None,
            None,
            Some(p.out_param_name.clone()),
        )
    }

    pub(super) fn create_function_for_initializer_of_for_statement(
        &self,
        node: Id<Node>, /*ForStatementWithConvertibleInitializer*/
        current_state: Id<ConvertedLoopState>,
    ) -> io::Result<IterationStatementPartFunction<Id<Node /*VariableDeclarationList*/>>> {
        let node_ref = node.ref_(self);
        let node_as_for_statement = node_ref.as_for_statement();
        let function_name = self.factory.ref_(self).create_unique_name("_loop_init", None);

        let node_initializer = node_as_for_statement.initializer.unwrap();
        let contains_yield = node_initializer
            .ref_(self).transform_flags()
            .intersects(TransformFlags::ContainsYield);
        let mut emit_flags = EmitFlags::None;
        let current_state = current_state.ref_(self);
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

        let mut statements: Vec<Id<Node /*Statement*/>> = _d();
        statements.push(
            self.factory
                .ref_(self).create_variable_statement(Option::<Id<NodeArray>>::None, node_initializer.clone()),
        );
        self.copy_out_parameters(
            &current_state.loop_out_parameters,
            LoopOutParameterFlags::Initializer,
            CopyDirection::ToOutParameter,
            &mut statements,
        );

        let function_declaration = self.factory.ref_(self).create_variable_statement(
            Option::<Id<NodeArray>>::None,
            self.factory
                .ref_(self).create_variable_declaration_list(
                    vec![self.factory.ref_(self).create_variable_declaration(
                        Some(function_name.clone()),
                        None,
                        None,
                        Some(
                            self.factory
                                .ref_(self).create_function_expression(
                                    Option::<Id<NodeArray>>::None,
                                    contains_yield.then(|| {
                                        self.factory.ref_(self).create_token(SyntaxKind::AsteriskToken)
                                    }),
                                    Option::<Id<Node>>::None,
                                    Option::<Id<NodeArray>>::None,
                                    Option::<Id<NodeArray>>::None,
                                    None,
                                    try_visit_node(
                                        self.factory.ref_(self).create_block(statements, Some(true)),
                                        Some(|node: Id<Node>| self.visitor(node)),
                                        Some(|node: Id<Node>| is_block(&node.ref_(self))),
                                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                    )?,
                                )
                                .set_emit_flags(emit_flags, self),
                        ),
                    )],
                    None,
                )
                .set_emit_flags(EmitFlags::NoHoisting, self),
        );

        let part = self.factory.ref_(self).create_variable_declaration_list(
            map(
                &current_state.loop_out_parameters,
                |loop_out_parameter: &LoopOutParameter, _| {
                    self.create_out_variable(loop_out_parameter)
                },
            ),
            None,
        );

        Ok(IterationStatementPartFunction {
            function_name,
            contains_yield,
            function_declaration,
            part,
        })
    }

    pub(super) fn create_function_for_body_of_iteration_statement(
        &self,
        node: Id<Node>, /*IterationStatement*/
        current_state: Id<ConvertedLoopState>,
        outer_state: Option<Id<ConvertedLoopState>>,
    ) -> io::Result<IterationStatementPartFunction<Vec<Id<Node /*Statement*/>>>> {
        let function_name = self.factory.ref_(self).create_unique_name("_loop", None);
        self.context.ref_(self).start_lexical_environment();
        let statement = try_visit_node(
            node.ref_(self).as_has_statement().statement(),
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_statement(node, self)),
            Some(&|nodes: &[Id<Node>]| self.factory.ref_(self).lift_to_block(nodes)),
        )?;
        let lexical_environment = self.context.ref_(self).end_lexical_environment();

        let mut statements: Vec<Id<Node /*Statement*/>> = _d();
        if self.should_convert_condition_of_for_statement(node)
            || self.should_convert_incrementor_of_for_statement(node)
        {
            current_state.ref_mut(self).condition_variable =
                Some(self.factory.ref_(self).create_unique_name("inc", None));
            let node_ref = node.ref_(self);
            let node_as_for_statement = node_ref.as_for_statement();
            if let Some(node_incrementor) = node_as_for_statement.incrementor {
                statements.push(
                    self.factory.ref_(self).create_if_statement(
                        current_state
                            .ref_(self)
                            .condition_variable
                            .clone()
                            .unwrap(),
                        self.factory.ref_(self).create_expression_statement(try_visit_node(
                            node_incrementor,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_expression(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )?),
                        Some(
                            self.factory.ref_(self).create_expression_statement(
                                self.factory.ref_(self).create_assignment(
                                    current_state
                                        .ref_(self)
                                        .condition_variable
                                        .clone()
                                        .unwrap(),
                                    self.factory.ref_(self).create_true(),
                                ),
                            ),
                        ),
                    ),
                );
            } else {
                statements.push(
                    self.factory.ref_(self).create_if_statement(
                        self.factory.ref_(self).create_logical_not(
                            current_state
                                .ref_(self)
                                .condition_variable
                                .clone()
                                .unwrap(),
                        ),
                        self.factory.ref_(self).create_expression_statement(
                            self.factory.ref_(self).create_assignment(
                                current_state
                                    .ref_(self)
                                    .condition_variable
                                    .clone()
                                    .unwrap(),
                                self.factory.ref_(self).create_true(),
                            ),
                        ),
                        None,
                    ),
                );
            }

            if self.should_convert_condition_of_for_statement(node) {
                statements.push(
                    self.factory.ref_(self).create_if_statement(
                        self.factory.ref_(self).create_prefix_unary_expression(
                            SyntaxKind::ExclamationToken,
                            try_visit_node(
                                node_as_for_statement.condition.unwrap(),
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            )?,
                        ),
                        try_visit_node(
                            self
                                .factory
                                .ref_(self).create_break_statement(Option::<Id<Node>>::None),
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_statement(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )?,
                        None,
                    ),
                );
            }
        }

        if is_block(&statement.ref_(self)) {
            add_range(
                &mut statements,
                Some(&statement.ref_(self).as_block().statements.ref_(self)),
                None,
                None,
            );
        } else {
            statements.push(statement);
        }

        self.copy_out_parameters(
            &current_state.ref_(self).loop_out_parameters,
            LoopOutParameterFlags::Body,
            CopyDirection::ToOutParameter,
            &mut statements,
        );
        insert_statements_after_standard_prologue(&mut statements, lexical_environment.as_deref(), self);

        let loop_body = self.factory.ref_(self).create_block(statements, Some(true));
        if is_block(&statement.ref_(self)) {
            set_original_node(loop_body, Some(statement), self);
        }

        let contains_yield = node
            .ref_(self).as_has_statement()
            .statement()
            .ref_(self).transform_flags()
            .intersects(TransformFlags::ContainsYield);

        let mut emit_flags = EmitFlags::ReuseTempVariableScope;
        if current_state.ref_(self).contains_lexical_this == Some(true) {
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

        let function_declaration = self.factory.ref_(self).create_variable_statement(
            Option::<Id<NodeArray>>::None,
            self.factory
                .ref_(self).create_variable_declaration_list(
                    vec![self.factory.ref_(self).create_variable_declaration(
                        Some(function_name.clone()),
                        None,
                        None,
                        Some(
                            self.factory
                                .ref_(self).create_function_expression(
                                    Option::<Id<NodeArray>>::None,
                                    contains_yield.then(|| {
                                        self.factory.ref_(self).create_token(SyntaxKind::AsteriskToken)
                                    }),
                                    Option::<Id<Node>>::None,
                                    Option::<Id<NodeArray>>::None,
                                    Some(current_state.ref_(self).loop_parameters.clone()),
                                    None,
                                    loop_body,
                                )
                                .set_emit_flags(emit_flags, self),
                        ),
                    )],
                    None,
                )
                .set_emit_flags(EmitFlags::NoHoisting, self),
        );

        let part = self.generate_call_to_converted_loop(
            function_name,
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
    ) -> Id<Node /*BinaryExpression*/> {
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
            .ref_(self).create_binary_expression(target, SyntaxKind::EqualsToken, source)
    }

    pub(super) fn copy_out_parameters(
        &self,
        out_params: &[LoopOutParameter],
        part_flags: LoopOutParameterFlags,
        copy_direction: CopyDirection,
        statements: &mut Vec<Id<Node /*Statement*/>>,
    ) {
        for out_param in out_params {
            if out_param.flags.intersects(part_flags) {
                statements.push(self.factory.ref_(self).create_expression_statement(
                    self.copy_out_parameter(out_param, copy_direction),
                ));
            }
        }
    }

    pub(super) fn generate_call_to_converted_loop_initializer(
        &self,
        init_function_expression_name: Id<Node>, /*Identifier*/
        contains_yield: bool,
    ) -> Id<Node /*Statement*/> {
        let call = self.factory.ref_(self).create_call_expression(
            init_function_expression_name,
            Option::<Id<NodeArray>>::None,
            Some(vec![]),
        );
        let call_result = if contains_yield {
            self.factory.ref_(self).create_yield_expression(
                Some(self.factory.ref_(self).create_token(SyntaxKind::AsteriskToken)),
                Some(set_emit_flags(call, EmitFlags::Iterator, self)),
            )
        } else {
            call
        };
        self.factory.ref_(self).create_expression_statement(call_result)
    }

    pub(super) fn generate_call_to_converted_loop(
        &self,
        loop_function_expression_name: Id<Node>, /*Identifier*/
        state: Id<ConvertedLoopState>,
        outer_state: Option<Id<ConvertedLoopState>>,
        contains_yield: bool,
    ) -> Vec<Id<Node /*Statement*/>> {
        let mut statements: Vec<Id<Node /*Statement*/>> = _d();
        let state = state.ref_(self);
        let is_simple_loop = !state
            .non_local_jumps
            .unwrap_or_default()
            .intersects(!Jump::Continue)
            && state.labeled_non_local_breaks.is_none()
            && state.labeled_non_local_continues.is_none();

        let call = self.factory.ref_(self).create_call_expression(
            loop_function_expression_name,
            Option::<Id<NodeArray>>::None,
            Some(map(&state.loop_parameters, |p: &Id<Node>, _| {
                p.ref_(self).as_parameter_declaration().name()
            })),
        );
        let call_result = if contains_yield {
            self.factory.ref_(self).create_yield_expression(
                Some(self.factory.ref_(self).create_token(SyntaxKind::AsteriskToken)),
                Some(set_emit_flags(call, EmitFlags::Iterator, self)),
            )
        } else {
            call
        };
        if is_simple_loop {
            statements.push(self.factory.ref_(self).create_expression_statement(call_result));
            self.copy_out_parameters(
                &state.loop_out_parameters,
                LoopOutParameterFlags::Body,
                CopyDirection::ToOriginal,
                &mut statements,
            );
        } else {
            let loop_result_name = self.factory.ref_(self).create_unique_name("state", None);
            let state_variable = self.factory.ref_(self).create_variable_statement(
                Option::<Id<NodeArray>>::None,
                self.factory.ref_(self).create_variable_declaration_list(
                    vec![self.factory.ref_(self).create_variable_declaration(
                        Some(loop_result_name.clone()),
                        None,
                        None,
                        Some(call_result),
                    )],
                    None,
                ),
            );
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
                let return_statement: Id<Node /*ReturnStatement*/>;
                if let Some(outer_state) = outer_state.as_ref() {
                    *outer_state
                        .ref_mut(self)
                        .non_local_jumps
                        .get_or_insert_default_() |= Jump::Return;
                    return_statement = self
                        .factory
                        .ref_(self).create_return_statement(Some(loop_result_name.clone()));
                } else {
                    return_statement = self.factory.ref_(self).create_return_statement(Some(
                        self.factory
                            .ref_(self).create_property_access_expression(loop_result_name.clone(), "value"),
                    ));
                }
                statements.push(
                    self.factory.ref_(self).create_if_statement(
                        self.factory
                            .ref_(self).create_type_check(loop_result_name.clone(), "object"),
                        return_statement,
                        None,
                    ),
                );
            }

            if state
                .non_local_jumps
                .unwrap_or_default()
                .intersects(Jump::Break)
            {
                statements.push(
                    self.factory.ref_(self).create_if_statement(
                        self.factory.ref_(self).create_strict_equality(
                            loop_result_name.clone(),
                            self.factory
                                .ref_(self).create_string_literal("break".to_owned(), None, None),
                        ),
                        self.factory
                            .ref_(self).create_break_statement(Option::<Id<Node>>::None),
                        None,
                    ),
                );
            }

            if state.labeled_non_local_breaks.is_some()
                || state.labeled_non_local_continues.is_some()
            {
                let mut case_clauses: Vec<Id<Node /*CaseClause*/>> = _d();
                self.process_labeled_jumps(
                    state.labeled_non_local_breaks.as_ref(),
                    true,
                    loop_result_name,
                    outer_state.clone(),
                    &mut case_clauses,
                );
                self.process_labeled_jumps(
                    state.labeled_non_local_continues.as_ref(),
                    false,
                    loop_result_name,
                    outer_state,
                    &mut case_clauses,
                );
                statements.push(self.factory.ref_(self).create_switch_statement(
                    loop_result_name,
                    self.factory.ref_(self).create_case_block(case_clauses),
                ));
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
                .get_or_insert_default_()
                .insert(label_text, label_marker);
        } else {
            state
                .labeled_non_local_continues
                .get_or_insert_default_()
                .insert(label_text, label_marker);
        }
    }

    pub(super) fn process_labeled_jumps(
        &self,
        table: Option<&IndexMap<String, String>>,
        is_break: bool,
        loop_result_name: Id<Node>, /*Identifier*/
        outer_loop: Option<Id<ConvertedLoopState>>,
        case_clauses: &mut Vec<Id<Node /*CaseClause*/>>,
    ) {
        let table = return_if_none!(table);
        for (label_text, label_marker) in table {
            let mut statements: Vec<Id<Node /*Statement*/>> = _d();
            if match outer_loop {
                None => true,
                Some(outer_loop) => {
                    outer_loop
                        .ref_(self)
                        .labels
                        .as_ref()
                        .matches(|outer_loop_labels| {
                            outer_loop_labels.get(label_text).copied() == Some(true)
                        })
                }
            } {
                let label = self.factory.ref_(self).create_identifier(label_text);
                statements.push(if is_break {
                    self.factory.ref_(self).create_break_statement(Some(label))
                } else {
                    self.factory.ref_(self).create_continue_statement(Some(label))
                });
            } else {
                self.set_labeled_jump(
                    &mut outer_loop.as_ref().unwrap().ref_mut(self),
                    is_break,
                    label_text.clone(),
                    label_marker.clone(),
                );
                statements.push(
                    self.factory
                        .ref_(self).create_return_statement(Some(loop_result_name)),
                );
            }
            case_clauses.push(
                self.factory.ref_(self).create_case_clause(
                    self.factory
                        .ref_(self).create_string_literal(label_marker.clone(), None, None),
                    statements,
                ),
            );
        }
    }

    pub(super) fn process_loop_variable_declaration(
        &self,
        container: Id<Node>, /*IterationStatement*/
        decl: Id<Node>,      /*VariableDeclaration | BindingElement*/
        loop_parameters: &mut Vec<Id<Node /*ParameterDeclaration*/>>,
        loop_out_parameters: &mut Vec<LoopOutParameter>,
        has_captured_bindings_in_for_initializer: bool,
    ) -> io::Result<()> {
        let name = decl.ref_(self).as_named_declaration().name();
        if is_binding_pattern(Some(&*name.ref_(self))) {
            for &element in &*name.ref_(self).as_has_elements().elements().ref_(self) {
                if !is_omitted_expression(&element.ref_(self)) {
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
            loop_parameters.push(self.factory.ref_(self).create_parameter_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                None,
                Some(name.clone()),
                None,
                None,
                None,
            ));
            let check_flags = self.resolver.ref_(self).get_node_check_flags(decl);
            if check_flags.intersects(NodeCheckFlags::NeedsLoopOutParameter)
                || has_captured_bindings_in_for_initializer
            {
                let out_param_name = self
                    .factory
                    .ref_(self).create_unique_name(&format!("out_{}", id_text(&name.ref_(self))), None);
                let mut flags = LoopOutParameterFlags::None;
                if check_flags.intersects(NodeCheckFlags::NeedsLoopOutParameter) {
                    flags |= LoopOutParameterFlags::Body;
                }
                if is_for_statement(&container.ref_(self))
                    && container
                        .ref_(self).as_for_statement()
                        .initializer
                        .try_matches(|container_initializer| {
                            self.resolver
                                .ref_(self).is_binding_captured_by_node(container_initializer, decl)
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
        expressions: &mut Vec<Id<Node /*Expression*/>>,
        node: Id<Node>,     /*ObjectLiteralExpression*/
        receiver: Id<Node>, /*Identifier*/
        start: usize,
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_object_literal_expression = node_ref.as_object_literal_expression();
        let properties = &node_as_object_literal_expression.properties;
        for &property in properties.ref_(self).iter().skip(start) {
            match property.ref_(self).kind() {
                SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                    let accessors = get_all_accessor_declarations(
                        &node_as_object_literal_expression.properties.ref_(self),
                        property,
                        self,
                    );
                    if property == accessors.first_accessor {
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
                _ => Debug_.fail_bad_syntax_kind(&node.ref_(self), None),
            }
        }

        Ok(())
    }

    pub(super) fn transform_property_assignment_to_expression(
        &self,
        property: Id<Node>, /*PropertyAssignment*/
        receiver: Id<Node>, /*Expression*/
        starts_on_new_line: Option<bool>,
    ) -> io::Result<Id<Node>> {
        let property_ref = property.ref_(self);
        let property_as_property_assignment = property_ref.as_property_assignment();
        let expression = self
            .factory
            .ref_(self).create_assignment(
                create_member_access_for_property_name(
                    &self.factory.ref_(self),
                    receiver,
                    try_visit_node(
                        property_as_property_assignment.name(),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node: Id<Node>| is_property_name(&node.ref_(self))),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    Option::<&Node>::None,
                ),
                try_visit_node(
                    property_as_property_assignment.maybe_initializer().unwrap(),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?,
            )
            .set_text_range(Some(&*property.ref_(self)), self);
        if starts_on_new_line == Some(true) {
            start_on_new_line(expression, self);
        }
        Ok(expression)
    }

    pub(super) fn transform_shorthand_property_assignment_to_expression(
        &self,
        property: Id<Node>, /*ShorthandPropertyAssignment*/
        receiver: Id<Node>, /*Expression*/
        starts_on_new_line: Option<bool>,
    ) -> io::Result<Id<Node>> {
        let property_ref = property.ref_(self);
        let property_as_shorthand_property_assignment = property_ref.as_shorthand_property_assignment();
        let expression = self
            .factory
            .ref_(self).create_assignment(
                create_member_access_for_property_name(
                    &self.factory.ref_(self),
                    receiver,
                    try_visit_node(
                        property_as_shorthand_property_assignment.name(),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node: Id<Node>| is_property_name(&node.ref_(self))),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    Option::<&Node>::None,
                ),
                self.factory
                    .ref_(self).clone_node(property_as_shorthand_property_assignment.name()),
            )
            .set_text_range(Some(&*property.ref_(self)), self);
        if starts_on_new_line == Some(true) {
            start_on_new_line(expression, self);
        }
        Ok(expression)
    }
}

pub(super) struct IterationStatementPartFunction<TPart> {
    pub function_name: Id<Node /*Identifier*/>,
    pub function_declaration: Id<Node /*Statement*/>,
    pub contains_yield: bool,
    pub part: TPart,
}
