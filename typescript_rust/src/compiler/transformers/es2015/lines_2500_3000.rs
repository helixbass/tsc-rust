use std::{borrow::Borrow, io};

use gc::{Gc, GcCell};

use super::{
    ConvertedLoopState, ConvertedLoopStateBuilder, HierarchyFacts, IterationStatementPartFunction,
    Jump, LoopOutParameter, TransformES2015,
};
use crate::{
    VisitResult, _d, get_combined_node_flags, is_expression, is_for_initializer, is_for_statement,
    is_identifier, is_object_literal_element_like, is_omitted_expression, is_statement,
    start_on_new_line, try_maybe_visit_node, try_maybe_visit_nodes, try_visit_each_child,
    try_visit_node, BoolExt, Debug_, EmitFlags, Matches, NamedDeclarationInterface, Node,
    NodeArray, NodeCheckFlags, NodeExt, NodeFlags, NodeInterface, NodeWrappered, OptionTry,
    SyntaxKind, TransformFlags,
};

impl TransformES2015 {
    pub(super) fn convert_for_of_statement_for_iterable(
        &self,
        node: &Node, /*ForOfStatement*/
        outermost_labeled_statement: Option<&Node /*LabeledStatement*/>,
        converted_loop_body_statements: Option<&[Gc<Node /*Statement*/>]>,
        ancestor_facts: Option<HierarchyFacts>,
    ) -> io::Result<Gc<Node /*Statement*/>> {
        let node_as_for_of_statement = node.as_for_of_statement();
        let ref expression = try_visit_node(
            &node_as_for_of_statement.expression,
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?;
        let iterator = if is_identifier(expression) {
            self.factory
                .get_generated_name_for_node(Some(&**expression), None)
        } else {
            self.factory
                .create_temp_variable(Option::<fn(&Node)>::None, None)
        };
        let result = if is_identifier(expression) {
            self.factory
                .get_generated_name_for_node(Some(&*iterator), None)
        } else {
            self.factory
                .create_temp_variable(Option::<fn(&Node)>::None, None)
        };
        let error_record = self.factory.create_unique_name("e", None);
        let catch_variable = self
            .factory
            .get_generated_name_for_node(Some(&*error_record), None);
        let return_method = self
            .factory
            .create_temp_variable(Option::<fn(&Node)>::None, None);
        let values = self
            .emit_helpers()
            .create_values_helper(expression.clone())
            .set_text_range(Some(&*node_as_for_of_statement.expression));
        let next = self
            .factory
            .create_call_expression(
                self.factory
                    .create_property_access_expression(iterator.clone(), "next")
                    .wrap(),
                Option::<Gc<NodeArray>>::None,
                Some(vec![]),
            )
            .wrap();

        self.context.hoist_variable_declaration(&error_record);
        self.context.hoist_variable_declaration(&return_method);

        let initializer = if ancestor_facts
            .unwrap_or_default()
            .intersects(HierarchyFacts::IterationContainer)
        {
            self.factory.inline_expressions(&[
                self.factory
                    .create_assignment(error_record.clone(), self.factory.create_void_zero())
                    .wrap(),
                values.clone(),
            ])
        } else {
            values.clone()
        };

        let for_statement = self
            .factory
            .create_for_statement(
                Some(
                    self.factory
                        .create_variable_declaration_list(
                            vec![
                                self.factory
                                    .create_variable_declaration(
                                        Some(iterator.clone()),
                                        None,
                                        None,
                                        Some(initializer),
                                    )
                                    .wrap()
                                    .set_text_range(Some(&*node_as_for_of_statement.expression)),
                                self.factory
                                    .create_variable_declaration(
                                        Some(result.clone()),
                                        None,
                                        None,
                                        Some(next.clone()),
                                    )
                                    .wrap(),
                            ],
                            None,
                        )
                        .wrap()
                        .set_text_range(Some(&*node_as_for_of_statement.expression))
                        .set_emit_flags(EmitFlags::NoHoisting),
                ),
                Some(
                    self.factory
                        .create_logical_not(
                            self.factory
                                .create_property_access_expression(result.clone(), "done")
                                .wrap(),
                        )
                        .wrap(),
                ),
                Some(self.factory.create_assignment(result.clone(), next).wrap()),
                self.convert_for_of_statement_head(
                    node,
                    &self
                        .factory
                        .create_property_access_expression(result.clone(), "value")
                        .wrap(),
                    converted_loop_body_statements,
                )?,
            )
            .wrap()
            .set_text_range(Some(node))
            .set_emit_flags(EmitFlags::NoTokenTrailingSourceMaps);

        Ok(self
            .factory
            .create_try_statement(
                self.factory
                    .create_block(
                        vec![self.factory.restore_enclosing_label(
                            &for_statement,
                            outermost_labeled_statement,
                            self.maybe_converted_loop_state().map(|_| {
                                |node: &Node| {
                                    self.reset_label(node);
                                }
                            }),
                        )],
                        None,
                    )
                    .wrap(),
                Some(
                    self.factory
                        .create_catch_clause(
                            Some(
                                self.factory
                                    .create_variable_declaration(
                                        Some(catch_variable.clone()),
                                        None,
                                        None,
                                        None,
                                    )
                                    .wrap(),
                            ),
                            self.factory
                                .create_block(
                                    vec![self
                                        .factory
                                        .create_expression_statement(
                                            self.factory
                                                .create_assignment(
                                                    error_record.clone(),
                                                    self.factory
                                                        .create_object_literal_expression(
                                                            Some(vec![self
                                                                .factory
                                                                .create_property_assignment(
                                                                    "error",
                                                                    catch_variable,
                                                                )
                                                                .wrap()]),
                                                            None,
                                                        )
                                                        .wrap(),
                                                )
                                                .wrap(),
                                        )
                                        .wrap()],
                                    None,
                                )
                                .wrap()
                                .set_emit_flags(EmitFlags::SingleLine),
                        )
                        .wrap(),
                ),
                Some(
                    self.factory
                        .create_block(
                            vec![self
                                .factory
                                .create_try_statement(
                                    self.factory
                                        .create_block(
                                            vec![
                                                self.factory.create_if_statement(
                                                    self.factory.create_logical_and(
                                                        self.factory.create_logical_and(
                                                            result.clone(),
                                                            self.factory.create_logical_not(
                                                                self.factory.create_property_access_expression(
                                                                    result,
                                                                    "done"
                                                                ).wrap()
                                                            ).wrap()
                                                        ).wrap(),
                                                        self.factory.create_assignment(
                                                            return_method.clone(),
                                                            self.factory.create_property_access_expression(
                                                                iterator.clone(),
                                                                "return"
                                                            ).wrap()
                                                        ).wrap()
                                                    ).wrap(),
                                                    self.factory.create_expression_statement(
                                                        self.factory.create_function_call_call(
                                                            return_method,
                                                            iterator,
                                                            vec![]
                                                        )
                                                    ).wrap(),
                                                    None,
                                                ).wrap()
                                                    .set_emit_flags(EmitFlags::SingleLine)
                                            ],
                                            None,
                                        )
                                        .wrap(),
                                    None,
                                    Some(
                                        self.factory
                                            .create_block(
                                                vec![
                                                    self.factory.create_if_statement(
                                                        error_record.clone(),
                                                        self.factory.create_throw_statement(
                                                            self.factory.create_property_access_expression(
                                                                error_record,
                                                                "error"
                                                            ).wrap()
                                                        ).wrap(),
                                                        None,
                                                    ).wrap()
                                                        .set_emit_flags(EmitFlags::SingleLine)
                                                ],
                                                None,
                                            )
                                            .wrap()
                                            .set_emit_flags(EmitFlags::SingleLine),
                                    ),
                                )
                                .wrap()],
                            None,
                        )
                        .wrap(),
                ),
            )
            .wrap())
    }

    pub(super) fn visit_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        let properties = &node_as_object_literal_expression.properties;

        let mut num_initial_properties: Option<usize> = _d();
        let mut has_computed = false;
        for (i, property) in properties.iter().enumerate() {
            if property
                .transform_flags()
                .intersects(TransformFlags::ContainsYield)
                && self
                    .maybe_hierarchy_facts()
                    .unwrap_or_default()
                    .intersects(HierarchyFacts::AsyncFunctionBody)
                || {
                    has_computed = Debug_
                        .check_defined(property.as_named_declaration().maybe_name(), None)
                        .kind()
                        == SyntaxKind::ComputedPropertyName;
                    has_computed
                }
            {
                num_initial_properties = Some(i);
                break;
            }
        }

        if num_initial_properties.is_none() {
            return try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context);
        }
        let num_initial_properties = num_initial_properties.unwrap();

        let temp = self.factory.create_temp_variable(
            Some(|node: &Node| {
                self.context.hoist_variable_declaration(node);
            }),
            None,
        );

        let mut expressions: Vec<Gc<Node /*Expression*/>> = _d();
        let assignment = self
            .factory
            .create_assignment(
                temp.clone(),
                self.factory
                    .create_object_literal_expression(
                        try_maybe_visit_nodes(
                            Some(properties),
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_object_literal_element_like),
                            Some(0),
                            Some(num_initial_properties),
                        )?,
                        node_as_object_literal_expression.multi_line,
                    )
                    .wrap()
                    .set_emit_flags(if has_computed {
                        EmitFlags::Indented
                    } else {
                        EmitFlags::None
                    }),
            )
            .wrap();

        if node_as_object_literal_expression.multi_line == Some(true) {
            start_on_new_line(&*assignment);
        }

        expressions.push(assignment);

        self.add_object_literal_members(&mut expressions, node, &temp, num_initial_properties)?;

        expressions.push(
            if node_as_object_literal_expression.multi_line == Some(true) {
                self.factory
                    .clone_node(&temp)
                    .set_text_range(Some(&*temp))
                    .and_set_parent(temp.maybe_parent())
                    .start_on_new_line()
            } else {
                temp
            },
        );
        Ok(self.factory.inline_expressions(&expressions))
    }

    pub(super) fn should_convert_part_of_iteration_statement(&self, node: &Node) -> bool {
        self.resolver
            .get_node_check_flags(node)
            .intersects(NodeCheckFlags::ContainsCapturedBlockScopedBinding)
    }

    pub(super) fn should_convert_initializer_of_for_statement(
        &self,
        node: &Node, /*IterationStatement*/
    ) -> bool {
        is_for_statement(node)
            && node
                .as_for_statement()
                .initializer
                .as_ref()
                .matches(|node_initializer| {
                    self.should_convert_part_of_iteration_statement(node_initializer)
                })
    }

    pub(super) fn should_convert_condition_of_for_statement(
        &self,
        node: &Node, /*IterationStatement*/
    ) -> bool {
        is_for_statement(node)
            && node
                .as_for_statement()
                .condition
                .as_ref()
                .matches(|node_condition| {
                    self.should_convert_part_of_iteration_statement(node_condition)
                })
    }

    pub(super) fn should_convert_incrementor_of_for_statement(
        &self,
        node: &Node, /*IterationStatement*/
    ) -> bool {
        is_for_statement(node)
            && node
                .as_for_statement()
                .incrementor
                .as_ref()
                .matches(|node_incrementor| {
                    self.should_convert_part_of_iteration_statement(node_incrementor)
                })
    }

    pub(super) fn should_convert_iteration_statement(
        &self,
        node: &Node, /*IterationStatement*/
    ) -> bool {
        self.should_convert_body_of_iteration_statement(node)
            || self.should_convert_initializer_of_for_statement(node)
    }

    pub(super) fn should_convert_body_of_iteration_statement(
        &self,
        node: &Node, /*IterationStatement*/
    ) -> bool {
        self.resolver
            .get_node_check_flags(node)
            .intersects(NodeCheckFlags::LoopWithCapturedBlockScopedBinding)
    }

    pub(super) fn hoist_variable_declaration_declared_in_converted_loop(
        &self,
        state: &mut ConvertedLoopState,
        node: &Node, /*VariableDeclaration*/
    ) {
        let node_as_variable_declaration = node.as_variable_declaration();
        state
            .hoisted_local_variables
            .get_or_insert_with(|| Default::default());

        self.hoist_variable_declaration_declared_in_converted_loop_visit(
            state,
            &node_as_variable_declaration.name(),
        );
    }

    pub(super) fn hoist_variable_declaration_declared_in_converted_loop_visit(
        &self,
        state: &mut ConvertedLoopState,
        node: &Node, /*Identifier | BindingPattern*/
    ) {
        if node.kind() == SyntaxKind::Identifier {
            state
                .hoisted_local_variables
                .as_mut()
                .unwrap()
                .push(node.node_wrapper());
        } else {
            for element in &node.as_has_elements().elements() {
                if !is_omitted_expression(element) {
                    self.hoist_variable_declaration_declared_in_converted_loop_visit(
                        state,
                        &element.as_binding_element().name(),
                    );
                }
            }
        }
    }

    #[allow(dead_code)]
    pub(super) fn convert_iteration_statement_body_if_necessary(
        &self,
        node: &Node, /*IterationStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
        ancestor_facts: Option<HierarchyFacts>,
        convert: Option<
            impl FnMut(
                &Node,         /*IterationStatement*/
                Option<&Node>, /*LabeledStatement*/
                Option<&[Gc<Node /*Statement*/>]>,
                Option<HierarchyFacts>,
            ) -> Gc<Node /*Statement*/>, /*LoopConverter*/
        >,
    ) -> VisitResult /*<Statement>*/ {
        self.try_convert_iteration_statement_body_if_necessary(
            node,
            outermost_labeled_statement,
            ancestor_facts,
            convert.map(|mut convert| {
                move |a: &Node,
                      b: Option<&Node>,
                      c: Option<&[Gc<Node>]>,
                      d: Option<HierarchyFacts>| Ok(convert(a, b, c, d))
            }),
        )
        .unwrap()
    }

    pub(super) fn try_convert_iteration_statement_body_if_necessary(
        &self,
        node: &Node, /*IterationStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
        ancestor_facts: Option<HierarchyFacts>,
        convert: Option<
            impl FnMut(
                &Node,         /*IterationStatement*/
                Option<&Node>, /*LabeledStatement*/
                Option<&[Gc<Node /*Statement*/>]>,
                Option<HierarchyFacts>,
            ) -> io::Result<Gc<Node /*Statement*/>>, /*LoopConverter*/
        >,
    ) -> io::Result<VisitResult /*<Statement>*/> {
        if !self.should_convert_iteration_statement(node) {
            let mut save_allowed_non_labeled_jumps: Option<Jump> = _d();
            if let Some(converted_loop_state) = self.maybe_converted_loop_state() {
                let mut converted_loop_state = converted_loop_state.borrow_mut();
                save_allowed_non_labeled_jumps = converted_loop_state.allowed_non_labeled_jumps;
                converted_loop_state.allowed_non_labeled_jumps = Some(Jump::Break | Jump::Continue);
            }

            let outermost_labeled_statement = outermost_labeled_statement.node_wrappered();
            let result = convert.try_map_or_else(
                || {
                    Ok(self.factory.restore_enclosing_label(
                        &*if is_for_statement(node) {
                            self.visit_each_child_of_for_statement(node)?
                        } else {
                            try_visit_each_child(
                                node,
                                |node: &Node| self.visitor(node),
                                &**self.context,
                            )?
                        },
                        outermost_labeled_statement.as_deref(),
                        self.maybe_converted_loop_state().map(|_| {
                            |node: &Node| {
                                self.reset_label(node);
                            }
                        }),
                    ))
                },
                |mut convert| {
                    convert(
                        node,
                        outermost_labeled_statement.as_deref(),
                        None,
                        ancestor_facts,
                    )
                },
            )?;

            if let Some(converted_loop_state) = self.maybe_converted_loop_state() {
                converted_loop_state.borrow_mut().allowed_non_labeled_jumps =
                    save_allowed_non_labeled_jumps;
            }
            return Ok(Some(result.into()));
        }

        let current_state = self.create_converted_loop_state(node)?;
        let mut statements: Vec<Gc<Node /*Statement*/>> = _d();

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
            &(*current_state).borrow(),
            outer_converted_loop_state.clone(),
        );

        if let Some(initializer_function) = initializer_function.as_ref() {
            statements.push(self.generate_call_to_converted_loop_initializer(
                &initializer_function.function_name,
                initializer_function.contains_yield,
            ));
        }

        let loop_: Gc<Node /*Statement*/>;
        if let Some(body_function) = body_function.as_ref() {
            if let Some(mut convert) = convert {
                loop_ = convert(
                    node,
                    outermost_labeled_statement.node_wrappered().as_deref(),
                    Some(&body_function.part),
                    ancestor_facts,
                )?;
            } else {
                let clone = self.convert_iteration_statement_core(
                    node,
                    initializer_function.as_ref(),
                    &self
                        .factory
                        .create_block(body_function.part.clone(), Some(true))
                        .wrap(),
                )?;
                loop_ = self.factory.restore_enclosing_label(
                    &clone,
                    outermost_labeled_statement,
                    self.maybe_converted_loop_state().map(|_| {
                        |node: &Node| {
                            self.reset_label(node);
                        }
                    }),
                );
            }
        } else {
            let clone = self.convert_iteration_statement_core(
                node,
                initializer_function.as_ref(),
                &*try_visit_node(
                    &node.as_has_statement().statement(),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_statement),
                    Some(&|nodes: &[Gc<Node>]| self.factory.lift_to_block(nodes)),
                )?,
            )?;
            loop_ = self.factory.restore_enclosing_label(
                &clone,
                outermost_labeled_statement,
                self.maybe_converted_loop_state().map(|_| {
                    |node: &Node| {
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
        node: &Node, /*IterationStatement*/
        initializer_function: Option<
            &IterationStatementPartFunction<Gc<Node /*VariableDeclarationList*/>>,
        >,
        converted_loop_body: &Node, /*Statement*/
    ) -> io::Result<Gc<Node>> {
        Ok(match node.kind() {
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
            _ => Debug_.fail_bad_syntax_kind(node, Some("IterationStatement expected")),
        })
    }

    pub(super) fn convert_for_statement(
        &self,
        node: &Node, /*ForStatement*/
        initializer_function: Option<
            &IterationStatementPartFunction<Gc<Node /*VariableDeclarationList*/>>,
        >,
        converted_loop_body: &Node, /*Statement*/
    ) -> io::Result<Gc<Node>> {
        let node_as_for_statement = node.as_for_statement();
        let should_convert_condition =
            node_as_for_statement
                .condition
                .as_ref()
                .matches(|node_condition| {
                    self.should_convert_part_of_iteration_statement(node_condition)
                });
        let should_convert_incrementor = should_convert_condition
            || node_as_for_statement
                .condition
                .as_ref()
                .matches(|node_incrementor| {
                    self.should_convert_part_of_iteration_statement(node_incrementor)
                });
        Ok(self.factory.update_for_statement(
            node,
            try_maybe_visit_node(
                if let Some(initializer_function) = initializer_function.as_ref() {
                    Some(initializer_function.part.clone())
                } else {
                    node_as_for_statement.initializer.clone()
                },
                Some(|node: &Node| self.visitor_with_unused_expression_result(node)),
                Some(is_for_initializer),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            try_maybe_visit_node(
                if should_convert_condition {
                    None
                } else {
                    node_as_for_statement.condition.as_deref()
                },
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            try_maybe_visit_node(
                if should_convert_incrementor {
                    None
                } else {
                    node_as_for_statement.incrementor.as_deref()
                },
                Some(|node: &Node| self.visitor_with_unused_expression_result(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            converted_loop_body.node_wrapper(),
        ))
    }

    pub(super) fn convert_for_of_statement(
        &self,
        node: &Node,                /*ForOfStatement*/
        converted_loop_body: &Node, /*Statement*/
    ) -> io::Result<Gc<Node>> {
        let node_as_for_of_statement = node.as_for_of_statement();
        Ok(self.factory.update_for_of_statement(
            node,
            None,
            try_visit_node(
                &node_as_for_of_statement.initializer,
                Some(|node: &Node| self.visitor(node)),
                Some(is_for_initializer),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            try_visit_node(
                &node_as_for_of_statement.expression,
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            converted_loop_body.node_wrapper(),
        ))
    }

    pub(super) fn convert_for_in_statement(
        &self,
        node: &Node,                /*ForInStatement*/
        converted_loop_body: &Node, /*Statement*/
    ) -> io::Result<Gc<Node>> {
        let node_as_for_in_statement = node.as_for_in_statement();
        Ok(self.factory.update_for_in_statement(
            node,
            try_visit_node(
                &node_as_for_in_statement.initializer,
                Some(|node: &Node| self.visitor(node)),
                Some(is_for_initializer),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            try_visit_node(
                &node_as_for_in_statement.expression,
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            converted_loop_body.node_wrapper(),
        ))
    }

    pub(super) fn convert_do_statement(
        &self,
        node: &Node,                /*DoStatement*/
        converted_loop_body: &Node, /*Statement*/
    ) -> io::Result<Gc<Node>> {
        let node_as_do_statement = node.as_do_statement();
        Ok(self.factory.update_do_statement(
            node,
            converted_loop_body.node_wrapper(),
            try_visit_node(
                &node_as_do_statement.expression,
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
        ))
    }

    pub(super) fn convert_while_statement(
        &self,
        node: &Node,                /*WhileStatement*/
        converted_loop_body: &Node, /*Statement*/
    ) -> io::Result<Gc<Node>> {
        let node_as_while_statement = node.as_while_statement();
        Ok(self.factory.update_while_statement(
            node,
            try_visit_node(
                &node_as_while_statement.expression,
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            converted_loop_body.node_wrapper(),
        ))
    }

    pub(super) fn create_converted_loop_state(
        &self,
        node: &Node, /*IterationStatement*/
    ) -> io::Result<Gc<GcCell<ConvertedLoopState>>> {
        let mut loop_initializer: Option<Gc<Node /*VariableDeclarationList*/>> = _d();
        match node.kind() {
            SyntaxKind::ForStatement | SyntaxKind::ForInStatement | SyntaxKind::ForOfStatement => {
                let initializer = node.as_has_initializer().maybe_initializer();
                if initializer.as_ref().matches(|initializer| {
                    initializer.kind() == SyntaxKind::VariableDeclarationList
                }) {
                    loop_initializer = initializer;
                }
            }
            _ => (),
        }

        let mut loop_parameters: Vec<Gc<Node /*ParameterDeclaration*/>> = _d();
        let mut loop_out_parameters: Vec<LoopOutParameter> = _d();
        if let Some(loop_initializer) = loop_initializer.filter(|loop_initializer| {
            get_combined_node_flags(loop_initializer).intersects(NodeFlags::BlockScoped)
        }) {
            let has_captured_bindings_in_for_initializer =
                self.should_convert_initializer_of_for_statement(node);
            for decl in &loop_initializer.as_variable_declaration_list().declarations {
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
            let converted_loop_state = (*converted_loop_state).borrow();
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
        Ok(Gc::new(GcCell::new(current_state)))
    }

    pub(super) fn add_extra_declarations_for_converted_loop(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        state: &ConvertedLoopState,
        outer_state: Option<Gc<GcCell<ConvertedLoopState>>>,
    ) {
        let mut extra_variable_declarations: Option<Vec<Gc<Node /*VariableDeclaration*/>>> = _d();
        if let Some(state_arguments_name) = state.arguments_name.as_ref() {
            if let Some(outer_state) = outer_state.as_ref() {
                outer_state.borrow_mut().arguments_name = Some(state_arguments_name.clone());
            } else {
                extra_variable_declarations
                    .get_or_insert_with(|| _d())
                    .push(
                        self.factory
                            .create_variable_declaration(
                                Some(state_arguments_name.clone()),
                                None,
                                None,
                                Some(self.factory.create_identifier("arguments")),
                            )
                            .wrap(),
                    );
            }
        }

        if let Some(state_this_name) = state.this_name.as_ref() {
            if let Some(outer_state) = outer_state.as_ref() {
                outer_state.borrow_mut().this_name = Some(state_this_name.clone());
            } else {
                extra_variable_declarations
                    .get_or_insert_with(|| _d())
                    .push(
                        self.factory
                            .create_variable_declaration(
                                Some(state_this_name.clone()),
                                None,
                                None,
                                Some(self.factory.create_identifier("this")),
                            )
                            .wrap(),
                    );
            }
        }

        if let Some(state_hoisted_local_variables) = state.hoisted_local_variables.as_ref() {
            if let Some(outer_state) = outer_state.as_ref() {
                outer_state.borrow_mut().hoisted_local_variables =
                    Some(state_hoisted_local_variables.clone());
            } else {
                let extra_variable_declarations =
                    extra_variable_declarations.get_or_insert_with(|| _d());
                for identifier in state_hoisted_local_variables {
                    extra_variable_declarations.push(
                        self.factory
                            .create_variable_declaration(Some(identifier.clone()), None, None, None)
                            .wrap(),
                    );
                }
            }
        }

        if !state.loop_out_parameters.is_empty() {
            let extra_variable_declarations =
                extra_variable_declarations.get_or_insert_with(|| _d());
            for out_param in &state.loop_out_parameters {
                extra_variable_declarations.push(
                    self.factory
                        .create_variable_declaration(
                            Some(out_param.out_param_name.clone()),
                            None,
                            None,
                            None,
                        )
                        .wrap(),
                );
            }
        }

        if let Some(state_condition_variable) = state.condition_variable.as_ref() {
            extra_variable_declarations
                .get_or_insert_with(|| _d())
                .push(
                    self.factory
                        .create_variable_declaration(
                            Some(state_condition_variable.clone()),
                            None,
                            None,
                            Some(self.factory.create_false().wrap()),
                        )
                        .wrap(),
                );
        }

        if let Some(extra_variable_declarations) = extra_variable_declarations {
            statements.push(
                self.factory
                    .create_variable_statement(
                        Option::<Gc<NodeArray>>::None,
                        self.factory
                            .create_variable_declaration_list(extra_variable_declarations, None)
                            .wrap(),
                    )
                    .wrap(),
            );
        }
    }
}
