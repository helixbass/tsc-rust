use std::{borrow::Borrow, io};

use id_arena::Id;

use super::{
    ConvertedLoopState, ConvertedLoopStateBuilder, HierarchyFacts, IterationStatementPartFunction,
    Jump, LoopOutParameter, TransformES2015,
};
use crate::{
    VisitResult, _d, get_combined_node_flags, is_expression, is_for_initializer, is_for_statement,
    is_identifier, is_object_literal_element_like, is_omitted_expression, is_statement,
    start_on_new_line, try_maybe_visit_node, try_maybe_visit_nodes, try_visit_each_child,
    try_visit_node, BoolExt, Debug_, EmitFlags, GetOrInsertDefault, Matches,
    NamedDeclarationInterface, Node, NodeArray, NodeCheckFlags, NodeExt, NodeFlags, NodeInterface,
    OptionTry, SyntaxKind, TransformFlags,
    HasArena, InArena,
    CoreTransformationContext,
};

impl TransformES2015 {
    pub(super) fn convert_for_of_statement_for_iterable(
        &self,
        node: Id<Node>, /*ForOfStatement*/
        outermost_labeled_statement: Option<Id<Node> /*LabeledStatement*/>,
        converted_loop_body_statements: Option<&[Id<Node /*Statement*/>]>,
        ancestor_facts: Option<HierarchyFacts>,
    ) -> io::Result<Id<Node /*Statement*/>> {
        let node_ref = node.ref_(self);
        let node_as_for_of_statement = node_ref.as_for_of_statement();
        let expression = try_visit_node(
            node_as_for_of_statement.expression,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        let iterator = if is_identifier(&expression.ref_(self)) {
            self.factory
                .ref_(self).get_generated_name_for_node(Some(expression), None)
        } else {
            self.factory
                .ref_(self).create_temp_variable(Option::<fn(Id<Node>)>::None, None)
        };
        let result = if is_identifier(&expression.ref_(self)) {
            self.factory
                .ref_(self).get_generated_name_for_node(Some(iterator), None)
        } else {
            self.factory
                .ref_(self).create_temp_variable(Option::<fn(Id<Node>)>::None, None)
        };
        let error_record = self.factory.ref_(self).create_unique_name("e", None);
        let catch_variable = self
            .factory
            .ref_(self).get_generated_name_for_node(Some(error_record), None);
        let return_method = self
            .factory
            .ref_(self).create_temp_variable(Option::<fn(Id<Node>)>::None, None);
        let values = self
            .emit_helpers()
            .create_values_helper(expression)
            .set_text_range(Some(&*node_as_for_of_statement.expression.ref_(self)), self);
        let next = self.factory.ref_(self).create_call_expression(
            self.factory
                .ref_(self).create_property_access_expression(iterator.clone(), "next"),
            Option::<Id<NodeArray>>::None,
            Some(vec![]),
        );

        self.context.ref_(self).hoist_variable_declaration(error_record);
        self.context.ref_(self).hoist_variable_declaration(return_method);

        let initializer = if ancestor_facts
            .unwrap_or_default()
            .intersects(HierarchyFacts::IterationContainer)
        {
            self.factory.ref_(self).inline_expressions(&[
                self.factory
                    .ref_(self).create_assignment(error_record.clone(), self.factory.ref_(self).create_void_zero()),
                values,
            ])
        } else {
            values
        };

        let for_statement = self
            .factory
            .ref_(self).create_for_statement(
                Some(
                    self.factory
                        .ref_(self).create_variable_declaration_list(
                            vec![
                                self.factory
                                    .ref_(self).create_variable_declaration(
                                        Some(iterator.clone()),
                                        None,
                                        None,
                                        Some(initializer),
                                    )
                                    .set_text_range(Some(&*node_as_for_of_statement.expression.ref_(self)), self),
                                self.factory.ref_(self).create_variable_declaration(
                                    Some(result.clone()),
                                    None,
                                    None,
                                    Some(next.clone()),
                                ),
                            ],
                            None,
                        )
                        .set_text_range(Some(&*node_as_for_of_statement.expression.ref_(self)), self)
                        .set_emit_flags(EmitFlags::NoHoisting, self),
                ),
                Some(
                    self.factory.ref_(self).create_logical_not(
                        self.factory
                            .ref_(self).create_property_access_expression(result.clone(), "done"),
                    ),
                ),
                Some(self.factory.ref_(self).create_assignment(result.clone(), next)),
                self.convert_for_of_statement_head(
                    node,
                    self
                        .factory
                        .ref_(self).create_property_access_expression(result, "value"),
                    converted_loop_body_statements,
                )?,
            )
            .set_text_range(Some(&*node.ref_(self)), self)
            .set_emit_flags(EmitFlags::NoTokenTrailingSourceMaps, self);

        Ok(self.factory.ref_(self).create_try_statement(
            self.factory.ref_(self).create_block(
                vec![self.factory.ref_(self).restore_enclosing_label(
                    for_statement,
                    outermost_labeled_statement,
                    self.maybe_converted_loop_state().map(|_| {
                        |node: Id<Node>| {
                            self.reset_label(node);
                        }
                    }),
                )],
                None,
            ),
            Some(
                self.factory.ref_(self).create_catch_clause(
                    Some(self.factory.ref_(self).create_variable_declaration(
                        Some(catch_variable.clone()),
                        None,
                        None,
                        None,
                    )),
                    self.factory
                        .ref_(self).create_block(
                            vec![self.factory.ref_(self).create_expression_statement(
                                self.factory.ref_(self).create_assignment(
                                    error_record.clone(),
                                    self.factory.ref_(self).create_object_literal_expression(
                                        Some(vec![self
                                            .factory
                                            .ref_(self).create_property_assignment("error", catch_variable)]),
                                        None,
                                    ),
                                ),
                            )],
                            None,
                        )
                        .set_emit_flags(EmitFlags::SingleLine, self),
                ),
            ),
            Some(self.factory.ref_(self).create_block(
                vec![self.factory.ref_(self).create_try_statement(
                        self.factory.ref_(self).create_block(
                            vec![self
                                .factory
                                .ref_(self).create_if_statement(
                                    self.factory.ref_(self).create_logical_and(
                                        self.factory.ref_(self).create_logical_and(
                                            result.clone(),
                                            self.factory.ref_(self).create_logical_not(
                                                self.factory.ref_(self).create_property_access_expression(
                                                    result, "done",
                                                ),
                                            ),
                                        ),
                                        self.factory.ref_(self).create_assignment(
                                            return_method.clone(),
                                            self.factory.ref_(self).create_property_access_expression(
                                                iterator.clone(),
                                                "return",
                                            ),
                                        ),
                                    ),
                                    self.factory.ref_(self).create_expression_statement(
                                        self.factory.ref_(self).create_function_call_call(
                                            return_method,
                                            iterator,
                                            vec![],
                                        ),
                                    ),
                                    None,
                                )
                                .set_emit_flags(EmitFlags::SingleLine, self)],
                            None,
                        ),
                        None,
                        Some(
                            self.factory
                                .ref_(self).create_block(
                                    vec![self
                                        .factory
                                        .ref_(self).create_if_statement(
                                            error_record.clone(),
                                            self.factory.ref_(self).create_throw_statement(
                                                self.factory.ref_(self).create_property_access_expression(
                                                    error_record,
                                                    "error",
                                                ),
                                            ),
                                            None,
                                        )
                                        .set_emit_flags(EmitFlags::SingleLine, self)],
                                    None,
                                )
                                .set_emit_flags(EmitFlags::SingleLine, self),
                        ),
                    )],
                None,
            )),
        ))
    }

    pub(super) fn visit_object_literal_expression(
        &self,
        node: Id<Node>, /*ObjectLiteralExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_ref = node.ref_(self);
        let node_as_object_literal_expression = node_ref.as_object_literal_expression();
        let properties = node_as_object_literal_expression.properties;

        let mut num_initial_properties: Option<usize> = _d();
        let mut has_computed = false;
        for (i, property) in properties.ref_(self).iter().enumerate() {
            if property
                .ref_(self).transform_flags()
                .intersects(TransformFlags::ContainsYield)
                && self
                    .maybe_hierarchy_facts()
                    .unwrap_or_default()
                    .intersects(HierarchyFacts::AsyncFunctionBody)
                || {
                    has_computed = Debug_
                        .check_defined(property.ref_(self).as_named_declaration().maybe_name(), None)
                        .ref_(self).kind()
                        == SyntaxKind::ComputedPropertyName;
                    has_computed
                }
            {
                num_initial_properties = Some(i);
                break;
            }
        }

        if num_initial_properties.is_none() {
            return try_visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            );
        }
        let num_initial_properties = num_initial_properties.unwrap();

        let temp = self.factory.ref_(self).create_temp_variable(
            Some(|node: Id<Node>| {
                self.context.ref_(self).hoist_variable_declaration(node);
            }),
            None,
        );

        let mut expressions: Vec<Id<Node /*Expression*/>> = _d();
        let assignment = self.factory.ref_(self).create_assignment(
            temp.clone(),
            self.factory
                .ref_(self).create_object_literal_expression(
                    try_maybe_visit_nodes(
                        Some(properties),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node: Id<Node>| is_object_literal_element_like(&node.ref_(self))),
                        Some(0),
                        Some(num_initial_properties),
                        self,
                    )?,
                    node_as_object_literal_expression.multi_line,
                )
                .set_emit_flags(if has_computed {
                    EmitFlags::Indented
                } else {
                    EmitFlags::None
                }, self),
        );

        if node_as_object_literal_expression.multi_line == Some(true) {
            start_on_new_line(assignment, self);
        }

        expressions.push(assignment);

        self.add_object_literal_members(&mut expressions, node, temp, num_initial_properties)?;

        expressions.push(
            if node_as_object_literal_expression.multi_line == Some(true) {
                self.factory
                    .ref_(self).clone_node(temp)
                    .set_text_range(Some(&*temp.ref_(self)), self)
                    .and_set_parent(temp.ref_(self).maybe_parent(), self)
                    .start_on_new_line(self)
            } else {
                temp
            },
        );
        Ok(self.factory.ref_(self).inline_expressions(&expressions))
    }

    pub(super) fn should_convert_part_of_iteration_statement(&self, node: Id<Node>) -> bool {
        self.resolver
            .ref_(self).get_node_check_flags(node)
            .intersects(NodeCheckFlags::ContainsCapturedBlockScopedBinding)
    }

    pub(super) fn should_convert_initializer_of_for_statement(
        &self,
        node: Id<Node>, /*IterationStatement*/
    ) -> bool {
        is_for_statement(&node.ref_(self))
            && node
                .ref_(self).as_for_statement()
                .initializer
                .matches(|node_initializer| {
                    self.should_convert_part_of_iteration_statement(node_initializer)
                })
    }

    pub(super) fn should_convert_condition_of_for_statement(
        &self,
        node: Id<Node>, /*IterationStatement*/
    ) -> bool {
        is_for_statement(&node.ref_(self))
            && node
                .ref_(self).as_for_statement()
                .condition
                .matches(|node_condition| {
                    self.should_convert_part_of_iteration_statement(node_condition)
                })
    }

    pub(super) fn should_convert_incrementor_of_for_statement(
        &self,
        node: Id<Node>, /*IterationStatement*/
    ) -> bool {
        is_for_statement(&node.ref_(self))
            && node
                .ref_(self).as_for_statement()
                .incrementor
                .matches(|node_incrementor| {
                    self.should_convert_part_of_iteration_statement(node_incrementor)
                })
    }

    pub(super) fn should_convert_iteration_statement(
        &self,
        node: Id<Node>, /*IterationStatement*/
    ) -> bool {
        self.should_convert_body_of_iteration_statement(node)
            || self.should_convert_initializer_of_for_statement(node)
    }

    pub(super) fn should_convert_body_of_iteration_statement(
        &self,
        node: Id<Node>, /*IterationStatement*/
    ) -> bool {
        self.resolver
            .ref_(self).get_node_check_flags(node)
            .intersects(NodeCheckFlags::LoopWithCapturedBlockScopedBinding)
    }

    pub(super) fn hoist_variable_declaration_declared_in_converted_loop(
        &self,
        state: &mut ConvertedLoopState,
        node: Id<Node>, /*VariableDeclaration*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_variable_declaration = node_ref.as_variable_declaration();
        state.hoisted_local_variables.get_or_insert_default_();

        self.hoist_variable_declaration_declared_in_converted_loop_visit(
            state,
            node_as_variable_declaration.name(),
        );
    }

    #[allow(clippy::only_used_in_recursion)]
    pub(super) fn hoist_variable_declaration_declared_in_converted_loop_visit(
        &self,
        state: &mut ConvertedLoopState,
        node: Id<Node>, /*Identifier | BindingPattern*/
    ) {
        if node.ref_(self).kind() == SyntaxKind::Identifier {
            state
                .hoisted_local_variables
                .as_mut()
                .unwrap()
                .push(node);
        } else {
            for element in &*node.ref_(self).as_has_elements().elements().ref_(self) {
                if !is_omitted_expression(&element.ref_(self)) {
                    self.hoist_variable_declaration_declared_in_converted_loop_visit(
                        state,
                        element.ref_(self).as_binding_element().name(),
                    );
                }
            }
        }
    }

    #[allow(dead_code)]
    pub(super) fn convert_iteration_statement_body_if_necessary(
        &self,
        node: Id<Node>, /*IterationStatement*/
        outermost_labeled_statement: Option<Id<Node /*LabeledStatement*/>>,
        ancestor_facts: Option<HierarchyFacts>,
        convert: Option<
            impl FnMut(
                Id<Node>,         /*IterationStatement*/
                Option<Id<Node>>, /*LabeledStatement*/
                Option<&[Id<Node /*Statement*/>]>,
                Option<HierarchyFacts>,
            ) -> Id<Node /*Statement*/>, /*LoopConverter*/
        >,
    ) -> VisitResult /*<Statement>*/ {
        self.try_convert_iteration_statement_body_if_necessary(
            node,
            outermost_labeled_statement,
            ancestor_facts,
            convert.map(|mut convert| {
                move |a: Id<Node>,
                      b: Option<Id<Node>>,
                      c: Option<&[Id<Node>]>,
                      d: Option<HierarchyFacts>| Ok(convert(a, b, c, d))
            }),
        )
        .unwrap()
    }

    pub(super) fn try_convert_iteration_statement_body_if_necessary(
        &self,
        node: Id<Node>, /*IterationStatement*/
        outermost_labeled_statement: Option<Id<Node /*LabeledStatement*/>>,
        ancestor_facts: Option<HierarchyFacts>,
        convert: Option<
            impl FnMut(
                Id<Node>,         /*IterationStatement*/
                Option<Id<Node>>, /*LabeledStatement*/
                Option<&[Id<Node /*Statement*/>]>,
                Option<HierarchyFacts>,
            ) -> io::Result<Id<Node /*Statement*/>>, /*LoopConverter*/
        >,
    ) -> io::Result<VisitResult /*<Statement>*/> {
        if !self.should_convert_iteration_statement(node) {
            let mut save_allowed_non_labeled_jumps: Option<Jump> = _d();
            if let Some(converted_loop_state) = self.maybe_converted_loop_state() {
                let mut converted_loop_state = converted_loop_state.ref_mut(self);
                save_allowed_non_labeled_jumps = converted_loop_state.allowed_non_labeled_jumps;
                converted_loop_state.allowed_non_labeled_jumps = Some(Jump::Break | Jump::Continue);
            }

            let result = convert.try_map_or_else(
                || {
                    Ok(self.factory.ref_(self).restore_enclosing_label(
                        if is_for_statement(&node.ref_(self)) {
                            self.visit_each_child_of_for_statement(node)?
                        } else {
                            try_visit_each_child(
                                node,
                                |node: Id<Node>| self.visitor(node),
                                &*self.context.ref_(self),
                                self,
                            )?
                        },
                        outermost_labeled_statement,
                        self.maybe_converted_loop_state().map(|_| {
                            |node: Id<Node>| {
                                self.reset_label(node);
                            }
                        }),
                    ))
                },
                |mut convert| {
                    convert(
                        node,
                        outermost_labeled_statement,
                        None,
                        ancestor_facts,
                    )
                },
            )?;

            if let Some(converted_loop_state) = self.maybe_converted_loop_state() {
                converted_loop_state.ref_mut(self).allowed_non_labeled_jumps =
                    save_allowed_non_labeled_jumps;
            }
            return Ok(Some(result.into()));
        }

        let current_state = self.create_converted_loop_state(node)?;
        let mut statements: Vec<Id<Node /*Statement*/>> = _d();

        let outer_converted_loop_state = self.maybe_converted_loop_state();
        self.set_converted_loop_state(Some(current_state.clone()));

        let initializer_function = self
            .should_convert_initializer_of_for_statement(node)
            .try_then(|| {
                self.create_function_for_initializer_of_for_statement(node, current_state.clone())
            })?;
        let body_function = self
            .should_convert_body_of_iteration_statement(node)
            .try_then(|| {
                self.create_function_for_body_of_iteration_statement(
                    node,
                    current_state.clone(),
                    outer_converted_loop_state.clone(),
                )
            })?;

        self.set_converted_loop_state(outer_converted_loop_state.clone());

        if let Some(initializer_function) = initializer_function.as_ref() {
            statements.push(initializer_function.function_declaration.clone());
        }
        if let Some(body_function) = body_function.as_ref() {
            statements.push(body_function.function_declaration.clone());
        }

        self.add_extra_declarations_for_converted_loop(
            &mut statements,
            &current_state.ref_(self),
            outer_converted_loop_state,
        );

        if let Some(initializer_function) = initializer_function.as_ref() {
            statements.push(self.generate_call_to_converted_loop_initializer(
                initializer_function.function_name,
                initializer_function.contains_yield,
            ));
        }

        let loop_: Id<Node /*Statement*/>;
        if let Some(body_function) = body_function.as_ref() {
            if let Some(mut convert) = convert {
                loop_ = convert(
                    node,
                    outermost_labeled_statement,
                    Some(&body_function.part),
                    ancestor_facts,
                )?;
            } else {
                let clone = self.convert_iteration_statement_core(
                    node,
                    initializer_function.as_ref(),
                    self
                        .factory
                        .ref_(self).create_block(body_function.part.clone(), Some(true)),
                )?;
                loop_ = self.factory.ref_(self).restore_enclosing_label(
                    clone,
                    outermost_labeled_statement,
                    self.maybe_converted_loop_state().map(|_| {
                        |node: Id<Node>| {
                            self.reset_label(node);
                        }
                    }),
                );
            }
        } else {
            let clone = self.convert_iteration_statement_core(
                node,
                initializer_function.as_ref(),
                try_visit_node(
                    node.ref_(self).as_has_statement().statement(),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_statement(node, self)),
                    Some(&|nodes: &[Id<Node>]| self.factory.ref_(self).lift_to_block(nodes)),
                )?,
            )?;
            loop_ = self.factory.ref_(self).restore_enclosing_label(
                clone,
                outermost_labeled_statement,
                self.maybe_converted_loop_state().map(|_| {
                    |node: Id<Node>| {
                        self.reset_label(node);
                    }
                }),
            );
        }

        statements.push(loop_);
        Ok(Some(statements.into()))
    }

    pub(super) fn convert_iteration_statement_core(
        &self,
        node: Id<Node>, /*IterationStatement*/
        initializer_function: Option<
            &IterationStatementPartFunction<Id<Node /*VariableDeclarationList*/>>,
        >,
        converted_loop_body: Id<Node>, /*Statement*/
    ) -> io::Result<Id<Node>> {
        Ok(match node.ref_(self).kind() {
            SyntaxKind::ForStatement => {
                self.convert_for_statement(node, initializer_function, converted_loop_body)?
            }
            SyntaxKind::ForInStatement => {
                self.convert_for_in_statement(node, converted_loop_body)?
            }
            SyntaxKind::ForOfStatement => {
                self.convert_for_of_statement(node, converted_loop_body)?
            }
            SyntaxKind::DoStatement => self.convert_do_statement(node, converted_loop_body)?,
            SyntaxKind::WhileStatement => {
                self.convert_while_statement(node, converted_loop_body)?
            }
            _ => Debug_.fail_bad_syntax_kind(&node.ref_(self), Some("IterationStatement expected")),
        })
    }

    pub(super) fn convert_for_statement(
        &self,
        node: Id<Node>, /*ForStatement*/
        initializer_function: Option<
            &IterationStatementPartFunction<Id<Node /*VariableDeclarationList*/>>,
        >,
        converted_loop_body: Id<Node>, /*Statement*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_for_statement = node_ref.as_for_statement();
        let should_convert_condition =
            node_as_for_statement
                .condition
                .matches(|node_condition| {
                    self.should_convert_part_of_iteration_statement(node_condition)
                });
        let should_convert_incrementor = should_convert_condition
            || node_as_for_statement
                .incrementor
                .matches(|node_incrementor| {
                    self.should_convert_part_of_iteration_statement(node_incrementor)
                });
        Ok(self.factory.ref_(self).update_for_statement(
            node,
            try_maybe_visit_node(
                if let Some(initializer_function) = initializer_function.as_ref() {
                    Some(initializer_function.part.clone())
                } else {
                    node_as_for_statement.initializer.clone()
                },
                Some(|node: Id<Node>| self.visitor_with_unused_expression_result(node)),
                Some(|node| is_for_initializer(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_maybe_visit_node(
                if should_convert_condition {
                    None
                } else {
                    node_as_for_statement.condition
                },
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_maybe_visit_node(
                if should_convert_incrementor {
                    None
                } else {
                    node_as_for_statement.incrementor
                },
                Some(|node: Id<Node>| self.visitor_with_unused_expression_result(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            converted_loop_body,
        ))
    }

    pub(super) fn convert_for_of_statement(
        &self,
        node: Id<Node>,                /*ForOfStatement*/
        converted_loop_body: Id<Node>, /*Statement*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_for_of_statement = node_ref.as_for_of_statement();
        Ok(self.factory.ref_(self).update_for_of_statement(
            node,
            None,
            try_visit_node(
                node_as_for_of_statement.initializer,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_for_initializer(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_visit_node(
                node_as_for_of_statement.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            converted_loop_body,
        ))
    }

    pub(super) fn convert_for_in_statement(
        &self,
        node: Id<Node>,                /*ForInStatement*/
        converted_loop_body: Id<Node>, /*Statement*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_for_in_statement = node_ref.as_for_in_statement();
        Ok(self.factory.ref_(self).update_for_in_statement(
            node,
            try_visit_node(
                node_as_for_in_statement.initializer,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_for_initializer(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_visit_node(
                node_as_for_in_statement.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            converted_loop_body,
        ))
    }

    pub(super) fn convert_do_statement(
        &self,
        node: Id<Node>,                /*DoStatement*/
        converted_loop_body: Id<Node>, /*Statement*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_do_statement = node_ref.as_do_statement();
        Ok(self.factory.ref_(self).update_do_statement(
            node,
            converted_loop_body,
            try_visit_node(
                node_as_do_statement.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
        ))
    }

    pub(super) fn convert_while_statement(
        &self,
        node: Id<Node>,                /*WhileStatement*/
        converted_loop_body: Id<Node>, /*Statement*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_while_statement = node_ref.as_while_statement();
        Ok(self.factory.ref_(self).update_while_statement(
            node,
            try_visit_node(
                node_as_while_statement.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            converted_loop_body,
        ))
    }

    pub(super) fn create_converted_loop_state(
        &self,
        node: Id<Node>, /*IterationStatement*/
    ) -> io::Result<Id<ConvertedLoopState>> {
        let mut loop_initializer: Option<Id<Node /*VariableDeclarationList*/>> = _d();
        match node.ref_(self).kind() {
            SyntaxKind::ForStatement | SyntaxKind::ForInStatement | SyntaxKind::ForOfStatement => {
                let initializer = node.ref_(self).as_has_initializer().maybe_initializer();
                if initializer.matches(|initializer| {
                    initializer.ref_(self).kind() == SyntaxKind::VariableDeclarationList
                }) {
                    loop_initializer = initializer;
                }
            }
            _ => (),
        }

        let mut loop_parameters: Vec<Id<Node /*ParameterDeclaration*/>> = _d();
        let mut loop_out_parameters: Vec<LoopOutParameter> = _d();
        if let Some(loop_initializer) = loop_initializer.filter(|&loop_initializer| {
            get_combined_node_flags(loop_initializer, self).intersects(NodeFlags::BlockScoped)
        }) {
            let has_captured_bindings_in_for_initializer =
                self.should_convert_initializer_of_for_statement(node);
            for &decl in &*loop_initializer.ref_(self).as_variable_declaration_list().declarations.ref_(self) {
                self.process_loop_variable_declaration(
                    node,
                    decl,
                    &mut loop_parameters,
                    &mut loop_out_parameters,
                    has_captured_bindings_in_for_initializer,
                )?;
            }
        }

        let mut current_state = ConvertedLoopStateBuilder::default()
            .loop_parameters(loop_parameters)
            .loop_out_parameters(loop_out_parameters)
            .build()
            .unwrap();
        if let Some(converted_loop_state) = self.maybe_converted_loop_state() {
            let converted_loop_state = converted_loop_state.ref_(self);
            if let Some(converted_loop_state_arguments_name) =
                converted_loop_state.arguments_name.as_ref()
            {
                current_state.arguments_name = Some(converted_loop_state_arguments_name.clone());
            }
            if let Some(converted_loop_state_this_name) = converted_loop_state.this_name.as_ref() {
                current_state.this_name = Some(converted_loop_state_this_name.clone());
            }
            if let Some(converted_loop_state_hoisted_local_variables) =
                converted_loop_state.hoisted_local_variables.as_ref()
            {
                current_state.hoisted_local_variables =
                    Some(converted_loop_state_hoisted_local_variables.clone());
            }
        }
        Ok(self.alloc_converted_loop_state(current_state))
    }

    pub(super) fn add_extra_declarations_for_converted_loop(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        state: &ConvertedLoopState,
        outer_state: Option<Id<ConvertedLoopState>>,
    ) {
        let mut extra_variable_declarations: Option<Vec<Id<Node /*VariableDeclaration*/>>> = _d();
        if let Some(state_arguments_name) = state.arguments_name.as_ref() {
            if let Some(outer_state) = outer_state {
                outer_state.ref_mut(self).arguments_name = Some(state_arguments_name.clone());
            } else {
                extra_variable_declarations.get_or_insert_default_().push(
                    self.factory.ref_(self).create_variable_declaration(
                        Some(state_arguments_name.clone()),
                        None,
                        None,
                        Some(self.factory.ref_(self).create_identifier("arguments")),
                    ),
                );
            }
        }

        if let Some(state_this_name) = state.this_name {
            if let Some(outer_state) = outer_state {
                outer_state.ref_mut(self).this_name = Some(state_this_name);
            } else {
                extra_variable_declarations.get_or_insert_default_().push(
                    self.factory.ref_(self).create_variable_declaration(
                        Some(state_this_name.clone()),
                        None,
                        None,
                        Some(self.factory.ref_(self).create_identifier("this")),
                    ),
                );
            }
        }

        if let Some(state_hoisted_local_variables) = state.hoisted_local_variables.as_ref() {
            if let Some(outer_state) = outer_state.as_ref() {
                outer_state.ref_mut(self).hoisted_local_variables =
                    Some(state_hoisted_local_variables.clone());
            } else {
                let extra_variable_declarations =
                    extra_variable_declarations.get_or_insert_default_();
                for identifier in state_hoisted_local_variables {
                    extra_variable_declarations.push(self.factory.ref_(self).create_variable_declaration(
                        Some(identifier.clone()),
                        None,
                        None,
                        None,
                    ));
                }
            }
        }

        if !state.loop_out_parameters.is_empty() {
            let extra_variable_declarations = extra_variable_declarations.get_or_insert_default_();
            for out_param in &state.loop_out_parameters {
                extra_variable_declarations.push(self.factory.ref_(self).create_variable_declaration(
                    Some(out_param.out_param_name.clone()),
                    None,
                    None,
                    None,
                ));
            }
        }

        if let Some(state_condition_variable) = state.condition_variable.as_ref() {
            extra_variable_declarations.get_or_insert_default_().push(
                self.factory.ref_(self).create_variable_declaration(
                    Some(state_condition_variable.clone()),
                    None,
                    None,
                    Some(self.factory.ref_(self).create_false()),
                ),
            );
        }

        if let Some(extra_variable_declarations) = extra_variable_declarations {
            statements.push(
                self.factory.ref_(self).create_variable_statement(
                    Option::<Id<NodeArray>>::None,
                    self.factory
                        .ref_(self).create_variable_declaration_list(extra_variable_declarations, None),
                ),
            );
        }
    }
}
