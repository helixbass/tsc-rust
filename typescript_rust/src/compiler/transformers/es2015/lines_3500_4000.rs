use std::{io, rc::Rc};

use gc::Gc;

use super::{HierarchyFacts, SpreadSegment, SpreadSegmentKind, TransformES2015};
use crate::{
    cast_present, create_member_access_for_property_name, filter, first, get_emit_flags,
    is_arrow_function, is_assignment_expression, is_binary_expression, is_binding_pattern,
    is_block, is_call_expression, is_computed_property_name, is_expression, is_function_expression,
    is_property_name, is_spread_element, is_statement, is_super_property, is_variable_statement,
    move_range_pos, skip_outer_expressions, some, start_on_new_line, try_cast,
    try_flatten_destructuring_binding, try_visit_each_child, try_visit_node, try_visit_nodes,
    try_visit_parameter_list, Debug_, EmitFlags, FlattenLevel, FunctionLikeDeclarationInterface,
    HasInitializerInterface, Matches, NamedDeclarationInterface, Node, NodeArray, NodeExt,
    NodeInterface, ReadonlyTextRangeConcrete, SyntaxKind, VecExt, VisitResult, _d, add_range,
    element_at, flatten, is_call_to_helper, is_expression_statement, is_identifier,
    is_packed_array_literal, is_return_statement, set_emit_flags, set_original_node, try_span_map,
    AsDoubleDeref, CallBinding, GeneratedIdentifierFlags, SignatureDeclarationInterface,
    TransformFlags,
};

impl TransformES2015 {
    pub(super) fn transform_object_literal_method_declaration_to_expression(
        &self,
        method: &Node,   /*MethodDeclaration*/
        receiver: &Node, /*Expression*/
        container: &Node,
        starts_on_new_line: Option<bool>,
    ) -> io::Result<Gc<Node>> {
        let method_as_method_declaration = method.as_method_declaration();
        let expression = self
            .factory
            .create_assignment(
                create_member_access_for_property_name(
                    &self.factory,
                    receiver,
                    &*try_visit_node(
                        &method_as_method_declaration.name(),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_property_name),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                    Option::<&Node>::None,
                ),
                self.transform_function_like_to_expression(
                    method,
                    Some(method),
                    Option::<&Node>::None,
                    Some(container),
                )?,
            )
            .set_text_range(Some(method));
        if starts_on_new_line == Some(true) {
            start_on_new_line(&*expression);
        }
        Ok(expression)
    }

    pub(super) fn visit_catch_clause(
        &self,
        node: &Node, /*CatchClause*/
    ) -> io::Result<Gc<Node /*CatchClause*/>> {
        let node_as_catch_clause = node.as_catch_clause();
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::BlockScopeExcludes,
            HierarchyFacts::BlockScopeIncludes,
        );
        let updated: Gc<Node /*CatchClause*/>;
        Debug_.assert(
            node_as_catch_clause.variable_declaration.is_some(),
            Some("Catch clause variable should always be present when downleveling ES2015."),
        );
        let node_variable_declaration = node_as_catch_clause.variable_declaration.as_ref().unwrap();
        if is_binding_pattern(
            node_variable_declaration
                .as_variable_declaration()
                .maybe_name(),
        ) {
            let temp = self
                .factory
                .create_temp_variable(Option::<fn(&Node)>::None, None);
            let new_variable_declaration = self
                .factory
                .create_variable_declaration(Some(temp.clone()), None, None, None)
                .set_text_range(Some(&**node_variable_declaration));
            let vars = try_flatten_destructuring_binding(
                node_variable_declaration,
                |node: &Node| self.visitor(node),
                &**self.context,
                FlattenLevel::All,
                Some(&*temp),
                None,
                None,
            )?;
            let list = self
                .factory
                .create_variable_declaration_list(vars, None)
                .set_text_range(Some(&**node_variable_declaration));
            let destructure = self
                .factory
                .create_variable_statement(Option::<Gc<NodeArray>>::None, list);
            updated = self.factory.update_catch_clause(
                node,
                Some(new_variable_declaration),
                self.add_statement_to_start_of_block(&node_as_catch_clause.block, destructure)?,
            );
        } else {
            updated =
                try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?;
        }

        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn add_statement_to_start_of_block(
        &self,
        block: &Node, /*Block*/
        statement: Gc<Node /*Statement*/>,
    ) -> io::Result<Gc<Node /*Block*/>> {
        let block_as_block = block.as_block();
        let transformed_statements = try_visit_nodes(
            &block_as_block.statements,
            Some(|node: &Node| self.visitor(node)),
            Some(is_statement),
            None,
            None,
        )?;
        Ok(self.factory.update_block(
            block,
            vec![statement].and_extend(transformed_statements.owned_iter()),
        ))
    }

    pub(super) fn visit_method_declaration(
        &self,
        node: &Node, /*MethodDeclaration*/
    ) -> io::Result<Gc<Node /*ObjectLiteralElementLike*/>> {
        let node_as_method_declaration = node.as_method_declaration();
        Debug_.assert(
            !is_computed_property_name(&node_as_method_declaration.name()),
            None,
        );
        let function_expression = self
            .transform_function_like_to_expression(
                node,
                Some(&ReadonlyTextRangeConcrete::from(move_range_pos(node, -1))),
                Option::<&Node>::None,
                Option::<&Node>::None,
            )?
            .set_additional_emit_flags(EmitFlags::NoLeadingComments);
        Ok(self
            .factory
            .create_property_assignment(node_as_method_declaration.name(), function_expression)
            .set_text_range(Some(node)))
    }

    pub(super) fn visit_accessor_declaration(
        &self,
        node: &Node, /*AccessorDeclaration*/
    ) -> io::Result<Gc<Node /*AccessorDeclaration*/>> {
        let ref node_name = node.as_named_declaration().name();
        Debug_.assert(!is_computed_property_name(node_name), None);
        let saved_converted_loop_state = self.maybe_converted_loop_state();
        self.set_converted_loop_state(None);
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::FunctionExcludes,
            HierarchyFacts::FunctionIncludes,
        );
        let updated: Gc<Node /*AccessorDeclaration*/>;
        let parameters = try_visit_parameter_list(
            Some(&node.as_signature_declaration().parameters()),
            |node: &Node| self.visitor(node),
            &**self.context,
        )?
        .unwrap();
        let body = self.transform_function_body(node)?;
        if node.kind() == SyntaxKind::GetAccessor {
            updated = self.factory.update_get_accessor_declaration(
                node,
                node.maybe_decorators(),
                node.maybe_modifiers(),
                node_name.clone(),
                parameters,
                node.as_has_type().maybe_type(),
                Some(body),
            );
        } else {
            updated = self.factory.update_set_accessor_declaration(
                node,
                node.maybe_decorators(),
                node.maybe_modifiers(),
                node_name.clone(),
                parameters,
                Some(body),
            );
        }
        self.exit_subtree(
            ancestor_facts,
            HierarchyFacts::FunctionSubtreeExcludes,
            HierarchyFacts::None,
        );
        self.set_converted_loop_state(saved_converted_loop_state);
        Ok(updated)
    }

    pub(super) fn visit_shorthand_property_assignment(
        &self,
        node: &Node, /*ShorthandPropertyAssignment*/
    ) -> io::Result<Gc<Node /*ObjectLiteralElementLike*/>> {
        let node_as_shorthand_property_assignment = node.as_shorthand_property_assignment();
        Ok(self
            .factory
            .create_property_assignment(
                node_as_shorthand_property_assignment.name(),
                self.visit_identifier(
                    &self
                        .factory
                        .clone_node(&node_as_shorthand_property_assignment.name()),
                )?,
            )
            .set_text_range(Some(node)))
    }

    pub(super) fn visit_computed_property_name(
        &self,
        node: &Node, /*ComputedPropertyName*/
    ) -> io::Result<VisitResult> {
        Ok(Some(
            try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?.into(),
        ))
    }

    pub(super) fn visit_yield_expression(
        &self,
        node: &Node, /*YieldExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_array_literal_expression(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let node_as_array_literal_expression = node.as_array_literal_expression();
        if some(
            Some(&node_as_array_literal_expression.elements),
            Some(|element: &Gc<Node>| is_spread_element(element)),
        ) {
            return self.transform_and_spread_elements(
                &node_as_array_literal_expression.elements,
                false,
                node_as_array_literal_expression.multi_line == Some(true),
                node_as_array_literal_expression.elements.has_trailing_comma,
            );
        }
        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_call_expression(
        &self,
        node: &Node, /*CallExpression*/
    ) -> io::Result<VisitResult> {
        let node_as_call_expression = node.as_call_expression();
        if get_emit_flags(node).intersects(EmitFlags::TypeScriptClassWrapper) {
            return self.visit_type_script_class_wrapper(node);
        }

        let ref expression = skip_outer_expressions(&node_as_call_expression.expression, None);
        if expression.kind() == SyntaxKind::SuperKeyword
            || is_super_property(expression)
            || some(
                Some(&node_as_call_expression.arguments),
                Some(|argument: &Gc<Node>| is_spread_element(argument)),
            )
        {
            return Ok(Some(
                self.visit_call_expression_with_potential_captured_this_assignment(node, true)?
                    .into(),
            ));
        }

        Ok(Some(
            self.factory
                .update_call_expression(
                    node,
                    try_visit_node(
                        &node_as_call_expression.expression,
                        Some(|node: &Node| self.call_expression_visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                    Option::<Gc<NodeArray>>::None,
                    try_visit_nodes(
                        &node_as_call_expression.arguments,
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_expression),
                        None,
                        None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_type_script_class_wrapper(
        &self,
        node: &Node, /*CallExpression*/
    ) -> io::Result<VisitResult> {
        let node_as_call_expression = node.as_call_expression();
        let body = cast_present(
            cast_present(
                skip_outer_expressions(&node_as_call_expression.expression, None),
                |node: &Gc<Node>| is_arrow_function(node),
            )
            .as_arrow_function()
            .maybe_body()
            .unwrap(),
            |node: &Gc<Node>| is_block(node),
        );
        let body_as_block = body.as_block();

        let saved_converted_loop_state = self.maybe_converted_loop_state();
        self.set_converted_loop_state(None);
        let body_statements = try_visit_nodes(
            &body_as_block.statements,
            Some(|node: &Node| self.class_wrapper_statement_visitor(node)),
            Some(is_statement),
            None,
            None,
        )?;
        self.set_converted_loop_state(saved_converted_loop_state);

        let class_statements = filter(&body_statements, |statement: &Gc<Node>| {
            self.is_variable_statement_with_initializer(statement)
        });
        let remaining_statements = filter(&body_statements, |stmt: &Gc<Node>| {
            !self.is_variable_statement_with_initializer(stmt)
        });
        let var_statement =
            cast_present(first(&class_statements).clone(), |statement: &Gc<Node>| {
                is_variable_statement(statement)
            });
        let var_statement_as_variable_statement = var_statement.as_variable_statement();

        let variable = &var_statement_as_variable_statement
            .declaration_list
            .as_variable_declaration_list()
            .declarations[0];
        let variable_as_variable_declaration = variable.as_variable_declaration();
        let ref initializer = skip_outer_expressions(
            &variable_as_variable_declaration
                .maybe_initializer()
                .unwrap(),
            None,
        );

        let mut alias_assignment = try_cast(initializer.clone(), |initializer: &Gc<Node>| {
            is_assignment_expression(initializer, None)
        });
        if alias_assignment.is_none()
            && is_binary_expression(initializer)
            && initializer.as_binary_expression().operator_token.kind() == SyntaxKind::CommaToken
        {
            alias_assignment = try_cast(
                initializer.as_binary_expression().left.clone(),
                |initializer_left: &Gc<Node>| is_assignment_expression(initializer_left, None),
            );
        }

        let call = cast_present(
            alias_assignment.as_ref().map_or_else(
                || initializer.clone(),
                |alias_assignment| {
                    skip_outer_expressions(&alias_assignment.as_binary_expression().right, None)
                },
            ),
            |node: &Gc<Node>| is_call_expression(node),
        );
        let call_as_call_expression = call.as_call_expression();
        let func = cast_present(
            skip_outer_expressions(&call_as_call_expression.expression, None),
            |node: &Gc<Node>| is_function_expression(node),
        );
        let func_as_function_expression = func.as_function_expression();

        let func_statements = func_as_function_expression
            .maybe_body()
            .unwrap()
            .as_block()
            .statements
            .clone();
        let mut class_body_start = 0;
        let mut class_body_end = -1;

        let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
        if let Some(alias_assignment) = alias_assignment.as_ref() {
            let extends_call = try_cast(
                func_statements.get(0).cloned(),
                |node: &Option<Gc<Node>>| {
                    node.as_ref().matches(|node| is_expression_statement(node))
                },
            )
            .flatten();
            if let Some(extends_call) = extends_call {
                statements.push(extends_call);
                class_body_start += 1;
            }

            statements.push(func_statements[class_body_start].clone());
            class_body_start += 1;

            statements.push(self.factory.create_expression_statement(
                self.factory.create_assignment(
                    alias_assignment.as_binary_expression().left.clone(),
                    cast_present(
                        variable_as_variable_declaration.name(),
                        |node: &Gc<Node>| is_identifier(node),
                    ),
                ),
            ));
        }

        while !is_return_statement(element_at(&func_statements, class_body_end).unwrap()) {
            class_body_end -= 1;
        }

        add_range(
            &mut statements,
            Some(&func_statements),
            Some(isize::try_from(class_body_start).unwrap()),
            Some(class_body_end),
        );

        if class_body_end < -1 {
            add_range(
                &mut statements,
                Some(&func_statements),
                Some(class_body_end + 1),
                None,
            );
        }

        add_range(&mut statements, Some(&remaining_statements), None, None);

        add_range(&mut statements, Some(&class_statements), Some(1), None);

        Ok(Some(
            self.factory
                .restore_outer_expressions(
                    Some(&*node_as_call_expression.expression),
                    &self.factory.restore_outer_expressions(
                        variable_as_variable_declaration.maybe_initializer(),
                        &self.factory.restore_outer_expressions(
                            alias_assignment.as_ref().map(|alias_assignment| {
                                alias_assignment.as_binary_expression().right.clone()
                            }),
                            &self.factory.update_call_expression(
                                &call,
                                self.factory.restore_outer_expressions(
                                    Some(&*call_as_call_expression.expression),
                                    &self.factory.update_function_expression(
                                        &func,
                                        Option::<Gc<NodeArray>>::None,
                                        None,
                                        None,
                                        Option::<Gc<NodeArray>>::None,
                                        func_as_function_expression.parameters(),
                                        None,
                                        self.factory.update_block(
                                            &func_as_function_expression.maybe_body().unwrap(),
                                            statements,
                                        ),
                                    ),
                                    None,
                                ),
                                Option::<Gc<NodeArray>>::None,
                                call_as_call_expression.arguments.clone(),
                            ),
                            None,
                        ),
                        None,
                    ),
                    None,
                )
                .into(),
        ))
    }

    pub(super) fn is_variable_statement_with_initializer(
        &self,
        stmt: &Node, /*Statement*/
    ) -> bool {
        is_variable_statement(stmt)
            && first(
                &stmt
                    .as_variable_statement()
                    .declaration_list
                    .as_variable_declaration_list()
                    .declarations,
            )
            .as_variable_declaration()
            .maybe_initializer()
            .is_some()
    }

    pub(super) fn visit_immediate_super_call_in_body(
        &self,
        node: &Node, /*CallExpression*/
    ) -> io::Result<Gc<Node>> {
        self.visit_call_expression_with_potential_captured_this_assignment(node, false)
    }

    pub(super) fn visit_call_expression_with_potential_captured_this_assignment(
        &self,
        node: &Node, /*CallExpression*/
        assign_to_captured_this: bool,
    ) -> io::Result<Gc<Node /*CallExpression | BinaryExpression*/>> {
        let node_as_call_expression = node.as_call_expression();
        if node
            .transform_flags()
            .intersects(TransformFlags::ContainsRestOrSpread)
            || node_as_call_expression.expression.kind() == SyntaxKind::SuperKeyword
            || is_super_property(&skip_outer_expressions(
                &node_as_call_expression.expression,
                None,
            ))
        {
            let CallBinding { target, this_arg } = self.factory.create_call_binding(
                &node_as_call_expression.expression,
                |node: &Node| {
                    self.context.hoist_variable_declaration(node);
                },
                None,
                None,
            );
            if node_as_call_expression.expression.kind() == SyntaxKind::SuperKeyword {
                set_emit_flags(&*this_arg, EmitFlags::NoSubstitution);
            }

            let mut resulting_call: Gc<Node /*CallExpression | BinaryExpression*/>;
            if node
                .transform_flags()
                .intersects(TransformFlags::ContainsRestOrSpread)
            {
                resulting_call = self.factory.create_function_apply_call(
                    try_visit_node(
                        &target,
                        Some(|node: &Node| self.call_expression_visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                    if node_as_call_expression.expression.kind() == SyntaxKind::SuperKeyword {
                        this_arg
                    } else {
                        try_visit_node(
                            &this_arg,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_expression),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        )?
                    },
                    self.transform_and_spread_elements(
                        &node_as_call_expression.arguments,
                        true,
                        false,
                        false,
                    )?,
                );
            } else {
                resulting_call = self
                    .factory
                    .create_function_call_call(
                        try_visit_node(
                            &target,
                            Some(|node: &Node| self.call_expression_visitor(node)),
                            Some(is_expression),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        )?,
                        if node_as_call_expression.expression.kind() == SyntaxKind::SuperKeyword {
                            this_arg
                        } else {
                            try_visit_node(
                                &this_arg,
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_expression),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            )?
                        },
                        try_visit_nodes(
                            &node_as_call_expression.arguments,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_expression),
                            None,
                            None,
                        )?,
                    )
                    .set_text_range(Some(node))
            }

            if node_as_call_expression.expression.kind() == SyntaxKind::SuperKeyword {
                let initializer = self
                    .factory
                    .create_logical_or(resulting_call, self.create_actual_this());
                resulting_call = if assign_to_captured_this {
                    self.factory.create_assignment(
                        self.factory.create_unique_name(
                            "_this",
                            Some(
                                GeneratedIdentifierFlags::Optimistic
                                    | GeneratedIdentifierFlags::FileLevel,
                            ),
                        ),
                        initializer,
                    )
                } else {
                    initializer
                };
            }
            return Ok(set_original_node(resulting_call, Some(node.node_wrapper())));
        }

        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_new_expression(
        &self,
        node: &Node, /*NewExpression*/
    ) -> io::Result<Gc<Node /*LeftHandSideExpression*/>> {
        let node_as_new_expression = node.as_new_expression();
        if some(
            node_as_new_expression.arguments.as_double_deref(),
            Some(|node: &Gc<Node>| is_spread_element(node)),
        ) {
            let CallBinding { target, this_arg } = self.factory.create_call_binding(
                &self.factory.create_property_access_expression(
                    node_as_new_expression.expression.clone(),
                    "bind",
                ),
                |node: &Node| {
                    self.context.hoist_variable_declaration(node);
                },
                None,
                None,
            );
            return Ok(self.factory.create_new_expression(
                self.factory.create_function_apply_call(
                    try_visit_node(
                        &target,
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                    this_arg,
                    self.transform_and_spread_elements(
                        &self.factory.create_node_array(
                            Some(
                                vec![self.factory.create_void_zero()].and_extend(
                                    node_as_new_expression
                                        .arguments
                                        .clone()
                                        .unwrap()
                                        .owned_iter(),
                                ),
                            ),
                            None,
                        ),
                        true,
                        false,
                        false,
                    )?,
                ),
                Option::<Gc<NodeArray>>::None,
                Some(vec![]),
            ));
        }
        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn transform_and_spread_elements(
        &self,
        elements: &NodeArray, /*<Expression>*/
        is_argument_list: bool,
        multi_line: bool,
        has_trailing_comma: bool,
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let num_elements = elements.len();
        let segments = flatten(&try_span_map(
            elements,
            |node: &Gc<Node>, _| -> Rc<dyn PartitionSpread> {
                if is_spread_element(&node) {
                    Rc::new(PartitionSpreadVisitSpanOfSpreads::new(self.rc_wrapper()))
                } else {
                    Rc::new(PartitionSpreadVisitSpanOfNonSpreads::new(self.rc_wrapper()))
                }
            },
            |partition: &[Gc<Node>], visit_partition, _start, end| {
                visit_partition.call(
                    partition,
                    multi_line,
                    has_trailing_comma && end == num_elements,
                )
            },
        )?);

        if segments.len() == 1 {
            let first_segment = &segments[0];
            if is_argument_list && self.compiler_options.downlevel_iteration != Some(true)
                || is_packed_array_literal(&first_segment.expression)
                || is_call_to_helper(&first_segment.expression, "___spreadArray")
            {
                return Ok(first_segment.expression.clone());
            }
        }

        let helpers = self.emit_helpers();
        let starts_with_spread = segments[0].kind != SpreadSegmentKind::None;
        let mut expression: Gc<Node /*Expression*/> = if starts_with_spread {
            self.factory
                .create_array_literal_expression(Option::<Gc<NodeArray>>::None, None)
        } else {
            segments[0].expression.clone()
        };
        for segment in segments.iter().skip(if starts_with_spread { 0 } else { 1 }) {
            expression = helpers.create_spread_array_helper(
                expression,
                segment.expression.clone(),
                segment.kind == SpreadSegmentKind::UnpackedSpread && !is_argument_list,
            );
        }
        Ok(expression)
    }
}

pub(super) trait PartitionSpread {
    fn call(
        &self,
        chunk: &[Gc<Node>],
        multi_line: bool,
        has_trailing_comma: bool,
    ) -> io::Result<Vec<SpreadSegment>>;

    fn eq_key(&self) -> &'static str;
}

impl Eq for dyn PartitionSpread {}

impl PartialEq for dyn PartitionSpread {
    fn eq(&self, other: &Self) -> bool {
        self.eq_key() == other.eq_key()
    }
}

pub(super) struct PartitionSpreadVisitSpanOfSpreads {
    transform_es2015: Gc<Box<TransformES2015>>,
}

impl PartitionSpreadVisitSpanOfSpreads {
    pub(super) fn new(transform_es2015: Gc<Box<TransformES2015>>) -> Self {
        Self { transform_es2015 }
    }
}

impl PartitionSpread for PartitionSpreadVisitSpanOfSpreads {
    fn call(
        &self,
        chunk: &[Gc<Node>],
        _multi_line: bool,
        _has_trailing_comma: bool,
    ) -> io::Result<Vec<SpreadSegment>> {
        self.transform_es2015.visit_span_of_spreads(chunk)
    }

    fn eq_key(&self) -> &'static str {
        "PartitionSpreadVisitSpanOfSpreads"
    }
}

pub(super) struct PartitionSpreadVisitSpanOfNonSpreads {
    transform_es2015: Gc<Box<TransformES2015>>,
}

impl PartitionSpreadVisitSpanOfNonSpreads {
    pub(super) fn new(transform_es2015: Gc<Box<TransformES2015>>) -> Self {
        Self { transform_es2015 }
    }
}

impl PartitionSpread for PartitionSpreadVisitSpanOfNonSpreads {
    fn call(
        &self,
        chunk: &[Gc<Node>],
        multi_line: bool,
        has_trailing_comma: bool,
    ) -> io::Result<Vec<SpreadSegment>> {
        Ok(vec![self.transform_es2015.visit_span_of_non_spreads(
            chunk.to_owned(),
            multi_line,
            has_trailing_comma,
        )?])
    }

    fn eq_key(&self) -> &'static str {
        "PartitionSpreadVisitSpanOfNonSpreads"
    }
}
