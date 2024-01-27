use std::{io, rc::Rc};

use gc::Gc;
use id_arena::Id;

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
    TransformFlags, Transformer,
    InArena, OptionInArena, downcast_transformer_ref,
    CoreTransformationContext, HasArena, AllArenas,
};

impl TransformES2015 {
    pub(super) fn transform_object_literal_method_declaration_to_expression(
        &self,
        method: Id<Node>,   /*MethodDeclaration*/
        receiver: Id<Node>, /*Expression*/
        container: Id<Node>,
        starts_on_new_line: Option<bool>,
    ) -> io::Result<Id<Node>> {
        let method_ref = method.ref_(self);
        let method_as_method_declaration = method_ref.as_method_declaration();
        let expression = self
            .factory
            .ref_(self).create_assignment(
                create_member_access_for_property_name(
                    &self.factory.ref_(self),
                    receiver,
                    try_visit_node(
                        method_as_method_declaration.name(),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node: Id<Node>| is_property_name(&node.ref_(self))),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    Option::<&Node>::None,
                ),
                self.transform_function_like_to_expression(
                    method,
                    Some(&*method.ref_(self)),
                    Option::<Id<Node>>::None,
                    Some(container),
                )?,
            )
            .set_text_range(Some(&*method.ref_(self)), self);
        if starts_on_new_line == Some(true) {
            start_on_new_line(expression, self);
        }
        Ok(expression)
    }

    pub(super) fn visit_catch_clause(
        &self,
        node: Id<Node>, /*CatchClause*/
    ) -> io::Result<Id<Node /*CatchClause*/>> {
        let node_ref = node.ref_(self);
        let node_as_catch_clause = node_ref.as_catch_clause();
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::BlockScopeExcludes,
            HierarchyFacts::BlockScopeIncludes,
        );
        #[allow(clippy::needless_late_init)]
        let updated: Id<Node /*CatchClause*/>;
        Debug_.assert(
            node_as_catch_clause.variable_declaration.is_some(),
            Some("Catch clause variable should always be present when downleveling ES2015."),
        );
        let node_variable_declaration = node_as_catch_clause.variable_declaration.unwrap();
        if is_binding_pattern(
            node_variable_declaration
                .ref_(self).as_variable_declaration()
                .maybe_name().refed(self).as_deref(),
        ) {
            let temp = self
                .factory
                .ref_(self).create_temp_variable(Option::<fn(Id<Node>)>::None, None);
            let new_variable_declaration = self
                .factory
                .ref_(self).create_variable_declaration(Some(temp.clone()), None, None, None)
                .set_text_range(Some(&*node_variable_declaration.ref_(self)), self);
            let vars = try_flatten_destructuring_binding(
                node_variable_declaration,
                |node: Id<Node>| self.visitor(node),
                self.context.clone(),
                FlattenLevel::All,
                Some(temp),
                None,
                None,
                self,
            )?;
            let list = self
                .factory
                .ref_(self).create_variable_declaration_list(vars, None)
                .set_text_range(Some(&*node_variable_declaration.ref_(self)), self);
            let destructure = self
                .factory
                .ref_(self).create_variable_statement(Option::<Id<NodeArray>>::None, list);
            updated = self.factory.ref_(self).update_catch_clause(
                node,
                Some(new_variable_declaration),
                self.add_statement_to_start_of_block(node_as_catch_clause.block, destructure)?,
            );
        } else {
            updated =
                try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self)?;
        }

        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn add_statement_to_start_of_block(
        &self,
        block: Id<Node>, /*Block*/
        statement: Id<Node /*Statement*/>,
    ) -> io::Result<Id<Node /*Block*/>> {
        let block_ref = block.ref_(self);
        let block_as_block = block_ref.as_block();
        let transformed_statements = try_visit_nodes(
            &block_as_block.statements,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_statement(node, self)),
            None,
            None,
            self,
        )?;
        Ok(self.factory.ref_(self).update_block(
            block,
            vec![statement].and_extend(transformed_statements.owned_iter()),
        ))
    }

    pub(super) fn visit_method_declaration(
        &self,
        node: Id<Node>, /*MethodDeclaration*/
    ) -> io::Result<Id<Node /*ObjectLiteralElementLike*/>> {
        let node_ref = node.ref_(self);
        let node_as_method_declaration = node_ref.as_method_declaration();
        Debug_.assert(
            !is_computed_property_name(&node_as_method_declaration.name().ref_(self)),
            None,
        );
        let function_expression = self
            .transform_function_like_to_expression(
                node,
                Some(&ReadonlyTextRangeConcrete::from(move_range_pos(&*node.ref_(self), -1))),
                Option::<Id<Node>>::None,
                Option::<Id<Node>>::None,
            )?
            .set_additional_emit_flags(EmitFlags::NoLeadingComments, self);
        Ok(self
            .factory
            .ref_(self).create_property_assignment(node_as_method_declaration.name(), function_expression)
            .set_text_range(Some(&*node.ref_(self)), self))
    }

    pub(super) fn visit_accessor_declaration(
        &self,
        node: Id<Node>, /*AccessorDeclaration*/
    ) -> io::Result<Id<Node /*AccessorDeclaration*/>> {
        let node_name = node.ref_(self).as_named_declaration().name();
        Debug_.assert(!is_computed_property_name(&node_name.ref_(self)), None);
        let saved_converted_loop_state = self.maybe_converted_loop_state();
        self.set_converted_loop_state(None);
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::FunctionExcludes,
            HierarchyFacts::FunctionIncludes,
        );
        let updated: Id<Node /*AccessorDeclaration*/>;
        let parameters = try_visit_parameter_list(
            Some(&node.ref_(self).as_signature_declaration().parameters()),
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )?
        .unwrap();
        let body = self.transform_function_body(node)?;
        if node.ref_(self).kind() == SyntaxKind::GetAccessor {
            updated = self.factory.ref_(self).update_get_accessor_declaration(
                node,
                node.ref_(self).maybe_decorators(),
                node.ref_(self).maybe_modifiers(),
                node_name,
                parameters,
                node.ref_(self).as_has_type().maybe_type(),
                Some(body),
            );
        } else {
            updated = self.factory.ref_(self).update_set_accessor_declaration(
                node,
                node.ref_(self).maybe_decorators(),
                node.ref_(self).maybe_modifiers(),
                node_name,
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
        node: Id<Node>, /*ShorthandPropertyAssignment*/
    ) -> io::Result<Id<Node /*ObjectLiteralElementLike*/>> {
        let node_ref = node.ref_(self);
        let node_as_shorthand_property_assignment = node_ref.as_shorthand_property_assignment();
        Ok(self
            .factory
            .ref_(self).create_property_assignment(
                node_as_shorthand_property_assignment.name(),
                self.visit_identifier(
                    self
                        .factory
                        .ref_(self).clone_node(node_as_shorthand_property_assignment.name()),
                )?,
            )
            .set_text_range(Some(&*node.ref_(self)), self))
    }

    pub(super) fn visit_computed_property_name(
        &self,
        node: Id<Node>, /*ComputedPropertyName*/
    ) -> io::Result<VisitResult> {
        Ok(Some(
            try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self)?
                .into(),
        ))
    }

    pub(super) fn visit_yield_expression(
        &self,
        node: Id<Node>, /*YieldExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self)
    }

    pub(super) fn visit_array_literal_expression(
        &self,
        node: Id<Node>, /*ArrayLiteralExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_ref = node.ref_(self);
        let node_as_array_literal_expression = node_ref.as_array_literal_expression();
        if some(
            Some(&node_as_array_literal_expression.elements),
            Some(|element: &Id<Node>| is_spread_element(&element.ref_(self))),
        ) {
            return self.transform_and_spread_elements(
                &node_as_array_literal_expression.elements,
                false,
                node_as_array_literal_expression.multi_line == Some(true),
                node_as_array_literal_expression.elements.has_trailing_comma,
            );
        }
        try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self)
    }

    pub(super) fn visit_call_expression(
        &self,
        node: Id<Node>, /*CallExpression*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        if get_emit_flags(&node.ref_(self)).intersects(EmitFlags::TypeScriptClassWrapper) {
            return self.visit_type_script_class_wrapper(node);
        }

        let expression = skip_outer_expressions(node_as_call_expression.expression, None, self);
        if expression.ref_(self).kind() == SyntaxKind::SuperKeyword
            || is_super_property(expression, self)
            || some(
                Some(&node_as_call_expression.arguments),
                Some(|argument: &Id<Node>| is_spread_element(&argument.ref_(self))),
            )
        {
            return Ok(Some(
                self.visit_call_expression_with_potential_captured_this_assignment(node, true)?
                    .into(),
            ));
        }

        Ok(Some(
            self.factory
                .ref_(self).update_call_expression(
                    node,
                    try_visit_node(
                        node_as_call_expression.expression,
                        Some(|node: Id<Node>| self.call_expression_visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    Option::<Id<NodeArray>>::None,
                    try_visit_nodes(
                        &node_as_call_expression.arguments,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        None,
                        None,
                        self,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_type_script_class_wrapper(
        &self,
        node: Id<Node>, /*CallExpression*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        let body = cast_present(
            cast_present(
                skip_outer_expressions(node_as_call_expression.expression, None, self),
                |node: &Id<Node>| is_arrow_function(&node.ref_(self)),
            )
            .ref_(self).as_arrow_function()
            .maybe_body()
            .unwrap(),
            |node: &Id<Node>| is_block(&node.ref_(self)),
        );
        let body_ref = body.ref_(self);
        let body_as_block = body_ref.as_block();

        let saved_converted_loop_state = self.maybe_converted_loop_state();
        self.set_converted_loop_state(None);
        let body_statements = try_visit_nodes(
            &body_as_block.statements,
            Some(|node: Id<Node>| self.class_wrapper_statement_visitor(node)),
            Some(|node| is_statement(node, self)),
            None,
            None,
            self,
        )?;
        self.set_converted_loop_state(saved_converted_loop_state);

        let class_statements = filter(&body_statements, |&statement: &Id<Node>| {
            self.is_variable_statement_with_initializer(statement)
        });
        let remaining_statements = filter(&body_statements, |&stmt: &Id<Node>| {
            !self.is_variable_statement_with_initializer(stmt)
        });
        let var_statement =
            cast_present(first(&class_statements).clone(), |statement: &Id<Node>| {
                is_variable_statement(&statement.ref_(self))
            });
        let var_statement_ref = var_statement.ref_(self);
        let var_statement_as_variable_statement = var_statement_ref.as_variable_statement();

        let variable = var_statement_as_variable_statement
            .declaration_list
            .ref_(self).as_variable_declaration_list()
            .declarations[0];
        let variable_ref = variable.ref_(self);
        let variable_as_variable_declaration = variable_ref.as_variable_declaration();
        let initializer = skip_outer_expressions(
            variable_as_variable_declaration
                .maybe_initializer()
                .unwrap(),
            None,
            self,
        );

        let mut alias_assignment = try_cast(initializer, |&initializer: &Id<Node>| {
            is_assignment_expression(initializer, None, self)
        });
        if alias_assignment.is_none()
            && is_binary_expression(&initializer.ref_(self))
            && initializer.ref_(self).as_binary_expression().operator_token.ref_(self).kind() == SyntaxKind::CommaToken
        {
            alias_assignment = try_cast(
                initializer.ref_(self).as_binary_expression().left,
                |&initializer_left: &Id<Node>| is_assignment_expression(initializer_left, None, self),
            );
        }

        let call = cast_present(
            alias_assignment.map_or_else(
                || initializer,
                |alias_assignment| {
                    skip_outer_expressions(alias_assignment.ref_(self).as_binary_expression().right, None, self)
                },
            ),
            |node: &Id<Node>| is_call_expression(&node.ref_(self)),
        );
        let call_ref = call.ref_(self);
        let call_as_call_expression = call_ref.as_call_expression();
        let func = cast_present(
            skip_outer_expressions(call_as_call_expression.expression, None, self),
            |node: &Id<Node>| is_function_expression(&node.ref_(self)),
        );
        let func_ref = func.ref_(self);
        let func_as_function_expression = func_ref.as_function_expression();

        let func_statements = func_as_function_expression
            .maybe_body()
            .unwrap()
            .ref_(self).as_block()
            .statements
            .clone();
        let mut class_body_start = 0;
        let mut class_body_end = -1;

        let mut statements: Vec<Id<Node /*Statement*/>> = _d();
        if let Some(alias_assignment) = alias_assignment {
            let extends_call = try_cast(
                func_statements.get(0).cloned(),
                |node: &Option<Id<Node>>| {
                    node.matches(|node| is_expression_statement(&node.ref_(self)))
                },
            )
            .flatten();
            if let Some(extends_call) = extends_call {
                statements.push(extends_call);
                class_body_start += 1;
            }

            statements.push(func_statements[class_body_start].clone());
            class_body_start += 1;

            statements.push(self.factory.ref_(self).create_expression_statement(
                self.factory.ref_(self).create_assignment(
                    alias_assignment.ref_(self).as_binary_expression().left,
                    cast_present(
                        variable_as_variable_declaration.name(),
                        |node: &Id<Node>| is_identifier(&node.ref_(self)),
                    ),
                ),
            ));
        }

        while !is_return_statement(&element_at(&func_statements, class_body_end).unwrap().ref_(self)) {
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
                .ref_(self).restore_outer_expressions(
                    Some(node_as_call_expression.expression),
                    self.factory.ref_(self).restore_outer_expressions(
                        variable_as_variable_declaration.maybe_initializer(),
                        self.factory.ref_(self).restore_outer_expressions(
                            alias_assignment.map(|alias_assignment| {
                                alias_assignment.ref_(self).as_binary_expression().right
                            }),
                            self.factory.ref_(self).update_call_expression(
                                call,
                                self.factory.ref_(self).restore_outer_expressions(
                                    Some(call_as_call_expression.expression),
                                    self.factory.ref_(self).update_function_expression(
                                        func,
                                        Option::<Id<NodeArray>>::None,
                                        None,
                                        None,
                                        Option::<Id<NodeArray>>::None,
                                        func_as_function_expression.parameters(),
                                        None,
                                        self.factory.ref_(self).update_block(
                                            func_as_function_expression.maybe_body().unwrap(),
                                            statements,
                                        ),
                                    ),
                                    None,
                                ),
                                Option::<Id<NodeArray>>::None,
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
        stmt: Id<Node>, /*Statement*/
    ) -> bool {
        is_variable_statement(&stmt.ref_(self))
            && first(
                &stmt
                    .ref_(self).as_variable_statement()
                    .declaration_list
                    .ref_(self).as_variable_declaration_list()
                    .declarations,
            )
            .ref_(self).as_variable_declaration()
            .maybe_initializer()
            .is_some()
    }

    pub(super) fn visit_immediate_super_call_in_body(
        &self,
        node: Id<Node>, /*CallExpression*/
    ) -> io::Result<Id<Node>> {
        self.visit_call_expression_with_potential_captured_this_assignment(node, false)
    }

    pub(super) fn visit_call_expression_with_potential_captured_this_assignment(
        &self,
        node: Id<Node>, /*CallExpression*/
        assign_to_captured_this: bool,
    ) -> io::Result<Id<Node /*CallExpression | BinaryExpression*/>> {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        if node
            .ref_(self).transform_flags()
            .intersects(TransformFlags::ContainsRestOrSpread)
            || node_as_call_expression.expression.ref_(self).kind() == SyntaxKind::SuperKeyword
            || is_super_property(skip_outer_expressions(
                node_as_call_expression.expression,
                None,
                self,
            ), self)
        {
            let CallBinding { target, this_arg } = self.factory.ref_(self).create_call_binding(
                node_as_call_expression.expression,
                |node: Id<Node>| {
                    self.context.ref_(self).hoist_variable_declaration(node);
                },
                None,
                None,
            );
            if node_as_call_expression.expression.ref_(self).kind() == SyntaxKind::SuperKeyword {
                set_emit_flags(this_arg, EmitFlags::NoSubstitution, self);
            }

            let mut resulting_call: Id<Node /*CallExpression | BinaryExpression*/>;
            if node
                .ref_(self).transform_flags()
                .intersects(TransformFlags::ContainsRestOrSpread)
            {
                resulting_call = self.factory.ref_(self).create_function_apply_call(
                    try_visit_node(
                        target,
                        Some(|node: Id<Node>| self.call_expression_visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    if node_as_call_expression.expression.ref_(self).kind() == SyntaxKind::SuperKeyword {
                        this_arg
                    } else {
                        try_visit_node(
                            this_arg,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_expression(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
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
                    .ref_(self).create_function_call_call(
                        try_visit_node(
                            target,
                            Some(|node: Id<Node>| self.call_expression_visitor(node)),
                            Some(|node| is_expression(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )?,
                        if node_as_call_expression.expression.ref_(self).kind() == SyntaxKind::SuperKeyword {
                            this_arg
                        } else {
                            try_visit_node(
                                this_arg,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            )?
                        },
                        try_visit_nodes(
                            &node_as_call_expression.arguments,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_expression(node, self)),
                            None,
                            None,
                            self,
                        )?,
                    )
                    .set_text_range(Some(&*node.ref_(self)), self)
            }

            if node_as_call_expression.expression.ref_(self).kind() == SyntaxKind::SuperKeyword {
                let initializer = self
                    .factory
                    .ref_(self).create_logical_or(resulting_call, self.create_actual_this());
                resulting_call = if assign_to_captured_this {
                    self.factory.ref_(self).create_assignment(
                        self.factory.ref_(self).create_unique_name(
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
            return Ok(set_original_node(resulting_call, Some(node), self));
        }

        try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self)
    }

    pub(super) fn visit_new_expression(
        &self,
        node: Id<Node>, /*NewExpression*/
    ) -> io::Result<Id<Node /*LeftHandSideExpression*/>> {
        let node_ref = node.ref_(self);
        let node_as_new_expression = node_ref.as_new_expression();
        if some(
            node_as_new_expression.arguments.as_double_deref(),
            Some(|node: &Id<Node>| is_spread_element(&node.ref_(self))),
        ) {
            let CallBinding { target, this_arg } = self.factory.ref_(self).create_call_binding(
                self.factory.ref_(self).create_property_access_expression(
                    node_as_new_expression.expression.clone(),
                    "bind",
                ),
                |node: Id<Node>| {
                    self.context.ref_(self).hoist_variable_declaration(node);
                },
                None,
                None,
            );
            return Ok(self.factory.ref_(self).create_new_expression(
                self.factory.ref_(self).create_function_apply_call(
                    try_visit_node(
                        target,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    this_arg,
                    self.transform_and_spread_elements(
                        &self.factory.ref_(self).create_node_array(
                            Some(
                                vec![self.factory.ref_(self).create_void_zero()].and_extend(
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
                Option::<Id<NodeArray>>::None,
                Some(vec![]),
            ));
        }
        try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self)
    }

    pub(super) fn transform_and_spread_elements(
        &self,
        elements: Id<NodeArray>, /*<Expression>*/
        is_argument_list: bool,
        multi_line: bool,
        has_trailing_comma: bool,
    ) -> io::Result<Id<Node /*Expression*/>> {
        let num_elements = elements.len();
        let segments = flatten(&try_span_map(
            elements,
            |node: &Id<Node>, _| -> Rc<dyn PartitionSpread> {
                if is_spread_element(&node.ref_(self)) {
                    Rc::new(PartitionSpreadVisitSpanOfSpreads::new(self.arena_id()))
                } else {
                    Rc::new(PartitionSpreadVisitSpanOfNonSpreads::new(self.arena_id()))
                }
            },
            |partition: &[Id<Node>], visit_partition, _start, end| {
                visit_partition.call(
                    partition,
                    multi_line,
                    has_trailing_comma && end == num_elements,
                )
            },
        )?);

        if segments.len() == 1 {
            let first_segment = &segments[0];
            if is_argument_list && self.compiler_options.ref_(self).downlevel_iteration != Some(true)
                || is_packed_array_literal(first_segment.expression)
                || is_call_to_helper(first_segment.expression, "___spreadArray", self)
            {
                return Ok(first_segment.expression);
            }
        }

        let helpers = self.emit_helpers();
        let starts_with_spread = segments[0].kind != SpreadSegmentKind::None;
        let mut expression: Id<Node /*Expression*/> = if starts_with_spread {
            self.factory
                .ref_(self).create_array_literal_expression(Option::<Id<NodeArray>>::None, None)
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
        chunk: &[Id<Node>],
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
    transform_es2015: Transformer,
}

impl PartitionSpreadVisitSpanOfSpreads {
    pub(super) fn new(transform_es2015: Transformer) -> Self {
        Self { transform_es2015 }
    }

    pub(super) fn transform_es2015(&self) -> debug_cell::Ref<'_, TransformES2015> {
        downcast_transformer_ref(self.transform_es2015, self)
    }
}

impl PartitionSpread for PartitionSpreadVisitSpanOfSpreads {
    fn call(
        &self,
        chunk: &[Id<Node>],
        _multi_line: bool,
        _has_trailing_comma: bool,
    ) -> io::Result<Vec<SpreadSegment>> {
        self.transform_es2015().visit_span_of_spreads(chunk)
    }

    fn eq_key(&self) -> &'static str {
        "PartitionSpreadVisitSpanOfSpreads"
    }
}

impl HasArena for PartitionSpreadVisitSpanOfSpreads {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub(super) struct PartitionSpreadVisitSpanOfNonSpreads {
    transform_es2015: Transformer,
}

impl PartitionSpreadVisitSpanOfNonSpreads {
    pub(super) fn new(transform_es2015: Transformer) -> Self {
        Self { transform_es2015 }
    }

    pub(super) fn transform_es2015(&self) -> debug_cell::Ref<'_, TransformES2015> {
        downcast_transformer_ref(self.transform_es2015, self)
    }
}

impl PartitionSpread for PartitionSpreadVisitSpanOfNonSpreads {
    fn call(
        &self,
        chunk: &[Id<Node>],
        multi_line: bool,
        has_trailing_comma: bool,
    ) -> io::Result<Vec<SpreadSegment>> {
        Ok(vec![self.transform_es2015().visit_span_of_non_spreads(
            chunk.to_owned(),
            multi_line,
            has_trailing_comma,
        )?])
    }

    fn eq_key(&self) -> &'static str {
        "PartitionSpreadVisitSpanOfNonSpreads"
    }
}

impl HasArena for PartitionSpreadVisitSpanOfNonSpreads {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}
