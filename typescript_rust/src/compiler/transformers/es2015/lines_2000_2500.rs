use std::{borrow::Borrow, cmp, io};

use gc::Gc;
use id_arena::Id;

use super::{HierarchyFacts, TransformES2015};
use crate::{
    add_range, concatenate, create_range, first_or_undefined, get_emit_flags,
    has_syntactic_modifier, id_text, is_binding_pattern, is_block, is_destructuring_assignment,
    is_expression, is_for_initializer, is_identifier, is_iteration_statement, is_statement,
    is_variable_declaration_list, last, move_range_end, move_range_pos, set_emit_flags,
    set_source_map_range, set_text_range_end, try_flat_map, try_flatten_destructuring_assignment,
    try_flatten_destructuring_binding, try_maybe_visit_node, try_visit_each_child, try_visit_node,
    unwrap_innermost_statement_of_label, EmitFlags, FlattenLevel, GetOrInsertDefault,
    HasInitializerInterface, Matches, ModifierFlags, NamedDeclarationInterface, Node, NodeArray,
    NodeArrayExt, NodeCheckFlags, NodeExt, NodeFlags, NodeInterface, Number, ReadonlyTextRange,
    ReadonlyTextRangeConcrete, SourceMapRange, SyntaxKind, TransformFlags, VisitResult,
};

impl TransformES2015 {
    pub(super) fn visit_block(
        &self,
        node: Id<Node>, /*Block*/
        is_function_body: bool,
    ) -> io::Result<Id<Node /*Block*/>> {
        if is_function_body {
            return try_visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &**self.context,
            );
        }
        let ancestor_facts = if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::IterationStatement)
        {
            self.enter_subtree(
                HierarchyFacts::IterationStatementBlockExcludes,
                HierarchyFacts::IterationStatementBlockIncludes,
            )
        } else {
            self.enter_subtree(HierarchyFacts::BlockExcludes, HierarchyFacts::BlockIncludes)
        };
        let updated =
            try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context)?;
        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn visit_expression_statement(
        &self,
        node: Id<Node>, /*ExpressionStatement*/
    ) -> io::Result<Id<Node /*Statement*/>> {
        try_visit_each_child(
            node,
            |node: Id<Node>| self.visitor_with_unused_expression_result(node),
            &**self.context,
        )
    }

    pub(super) fn visit_parenthesized_expression(
        &self,
        node: Id<Node>, /*ParenthesizedExpression*/
        expression_result_is_unused: bool,
    ) -> io::Result<Id<Node /*ParenthesizedExpression*/>> {
        try_visit_each_child(
            node,
            |node: Id<Node>| {
                Ok(if expression_result_is_unused {
                    self.visitor_with_unused_expression_result(node)?
                } else {
                    self.visitor(node)?
                })
            },
            &**self.context,
        )
    }

    pub(super) fn visit_binary_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
        expression_result_is_unused: bool,
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_as_binary_expression = node.as_binary_expression();
        if is_destructuring_assignment(node, self) {
            return try_flatten_destructuring_assignment(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                self.context.clone(),
                FlattenLevel::All,
                Some(!expression_result_is_unused),
                Option::<
                    fn(Id<Node>, Id<Node>, Option<&dyn ReadonlyTextRange>) -> io::Result<Id<Node>>,
                >::None,
            );
        }
        if node_as_binary_expression.operator_token.kind() == SyntaxKind::CommaToken {
            return Ok(self.factory.update_binary_expression(
                node,
                try_visit_node(
                    &node_as_binary_expression.left,
                    Some(|node: Id<Node>| self.visitor_with_unused_expression_result(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?,
                node_as_binary_expression.operator_token.clone(),
                try_visit_node(
                    &node_as_binary_expression.right,
                    Some(|node: Id<Node>| {
                        Ok(if expression_result_is_unused {
                            self.visitor_with_unused_expression_result(node)?
                        } else {
                            self.visitor(node)?
                        })
                    }),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?,
            ));
        }
        try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_comma_list_expression(
        &self,
        node: Id<Node>, /*CommaListExpression*/
        expression_result_is_unused: bool,
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_as_comma_list_expression = node.as_comma_list_expression();
        if expression_result_is_unused {
            return try_visit_each_child(
                node,
                |node: Id<Node>| self.visitor_with_unused_expression_result(node),
                &**self.context,
            );
        }
        let mut result: Option<Vec<Id<Node /*Expression*/>>> = Default::default();
        for (i, element) in node_as_comma_list_expression.elements.iter().enumerate() {
            let visited = try_visit_node(
                element,
                Some(|node: Id<Node>| {
                    Ok(if i < node_as_comma_list_expression.elements.len() - 1 {
                        self.visitor_with_unused_expression_result(node)?
                    } else {
                        self.visitor(node)?
                    })
                }),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?;
            if result.is_some() || !Gc::ptr_eq(&visited, element) {
                result
                    .get_or_insert_with(|| node_as_comma_list_expression.elements[0..i].to_owned())
                    .push(visited);
            }
        }
        let elements = result.map_or_else(
            || node_as_comma_list_expression.elements.clone(),
            |result| {
                self.factory
                    .create_node_array(Some(result), None)
                    .set_text_range(Some(&*node_as_comma_list_expression.elements))
            },
        );
        Ok(self.factory.update_comma_list_expression(node, elements))
    }

    pub(super) fn is_variable_statement_of_type_script_class_wrapper(
        &self,
        node: Id<Node>, /*VariableStatement*/
    ) -> bool {
        let node_as_variable_statement = node.as_variable_statement();
        let node_declaration_list_as_variable_declaration_list = node_as_variable_statement
            .declaration_list
            .as_variable_declaration_list();
        node_declaration_list_as_variable_declaration_list
            .declarations
            .len()
            == 1
            && node_declaration_list_as_variable_declaration_list.declarations[0]
                .as_variable_declaration()
                .maybe_initializer()
                .matches(|ref node_declaration_list_declarations_0_initializer| {
                    get_emit_flags(node_declaration_list_declarations_0_initializer)
                        .intersects(EmitFlags::TypeScriptClassWrapper)
                })
    }

    pub(super) fn visit_variable_statement(
        &self,
        node: Id<Node>, /*VariableStatement*/
    ) -> io::Result<Option<Id<Node /*Statement*/>>> {
        let node_as_variable_statement = node.as_variable_statement();
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::None,
            if has_syntactic_modifier(node, ModifierFlags::Export, self) {
                HierarchyFacts::ExportedVariableStatement
            } else {
                HierarchyFacts::None
            },
        );
        let updated: Option<Id<Node /*Statement*/>>;
        if let Some(converted_loop_state) = self.maybe_converted_loop_state().filter(|_| {
            !node_as_variable_statement
                .declaration_list
                .flags()
                .intersects(NodeFlags::BlockScoped)
                && !self.is_variable_statement_of_type_script_class_wrapper(node)
        }) {
            let mut assignments: Option<Vec<Id<Node /*Expression*/>>> = Default::default();
            for decl in &node_as_variable_statement
                .declaration_list
                .as_variable_declaration_list()
                .declarations
            {
                let decl_as_variable_declaration = decl.as_variable_declaration();
                self.hoist_variable_declaration_declared_in_converted_loop(
                    &mut converted_loop_state.borrow_mut(),
                    decl,
                );
                if let Some(decl_initializer) = decl_as_variable_declaration.maybe_initializer() {
                    let assignment: Id<Node /*Expression*/>;
                    if is_binding_pattern(decl_as_variable_declaration.maybe_name()) {
                        assignment = try_flatten_destructuring_assignment(
                            decl,
                            Some(|node: Id<Node>| self.visitor(node)),
                            self.context.clone(),
                            FlattenLevel::All,
                            None,
                            Option::<
                                fn(
                                    Id<Node>,
                                    Id<Node>,
                                    Option<&dyn ReadonlyTextRange>,
                                ) -> io::Result<Id<Node>>,
                            >::None,
                        )?;
                    } else {
                        assignment = self
                            .factory
                            .create_binary_expression(
                                decl_as_variable_declaration.name(),
                                SyntaxKind::EqualsToken,
                                try_visit_node(
                                    &decl_initializer,
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Some(|node| is_expression(node, self)),
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                )?,
                            )
                            .set_text_range(Some(&**decl));
                    }

                    assignments.get_or_insert_default_().push(assignment);
                }
            }
            if let Some(assignments) = assignments {
                updated = Some(
                    self.factory
                        .create_expression_statement(self.factory.inline_expressions(&assignments))
                        .set_text_range(Some(node)),
                );
            } else {
                updated = None;
            }
        } else {
            updated = Some(try_visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &**self.context,
            )?);
        }

        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn visit_variable_declaration_list(
        &self,
        node: Id<Node>, /*VariableDeclarationList*/
    ) -> io::Result<Id<Node /*VariableDeclarationList*/>> {
        let node_as_variable_declaration_list = node.as_variable_declaration_list();
        if node.flags().intersects(NodeFlags::BlockScoped)
            || node
                .transform_flags()
                .intersects(TransformFlags::ContainsBindingPattern)
        {
            if node.flags().intersects(NodeFlags::BlockScoped) {
                self.enable_substitutions_for_block_scoped_bindings();
            }

            let declarations: Vec<Id<Node>> = try_flat_map(
                Some(&node_as_variable_declaration_list.declarations),
                |declaration: &Id<Node>, _| -> io::Result<_> {
                    Ok(if node.flags().intersects(NodeFlags::Let) {
                        self.visit_variable_declaration_in_let_declaration_list(declaration)?
                    } else {
                        self.visit_variable_declaration(declaration)?
                    }
                    .map(Into::into)
                    .unwrap_or_default())
                },
            )?;

            let declaration_list = self
                .factory
                .create_variable_declaration_list(declarations.clone(), None)
                .set_original_node(Some(node.node_wrapper()))
                .set_text_range(Some(node))
                .set_comment_range(node);

            if node
                .transform_flags()
                .intersects(TransformFlags::ContainsBindingPattern)
                && (is_binding_pattern(
                    node_as_variable_declaration_list.declarations[0]
                        .as_variable_declaration()
                        .maybe_name(),
                ) || is_binding_pattern(
                    last(&node_as_variable_declaration_list.declarations)
                        .as_variable_declaration()
                        .maybe_name(),
                ))
            {
                set_source_map_range(
                    &*declaration_list,
                    Some(self.get_range_union(&declarations)),
                );
            }

            return Ok(declaration_list);
        }
        try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context)
    }

    pub(super) fn get_range_union(
        &self,
        declarations: &[Id<Node>],
    ) -> Gc<SourceMapRange /*TextRange*/> {
        let mut pos = -1;
        let mut end = -1;
        for node in declarations {
            pos = if pos == -1 {
                node.pos()
            } else if node.pos() == -1 {
                pos
            } else {
                cmp::min(pos, node.pos())
            };
            end = cmp::max(end, node.end());
        }
        (&create_range(pos, Some(end))).into()
    }

    pub(super) fn should_emit_explicit_initializer_for_let_declaration(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
    ) -> io::Result<bool> {
        let flags = self.resolver.get_node_check_flags(node);
        let is_captured_in_function = flags.intersects(NodeCheckFlags::CapturedBlockScopedBinding);
        let is_declared_in_loop = flags.intersects(NodeCheckFlags::BlockScopedBindingInLoop);
        let emitted_as_top_level = self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::TopLevel)
            || (is_captured_in_function
                && is_declared_in_loop
                && self
                    .maybe_hierarchy_facts()
                    .unwrap_or_default()
                    .intersects(HierarchyFacts::IterationStatementBlock));

        let emit_explicit_initializer = !emitted_as_top_level
            && !self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::ForInOrForOfStatement)
            && (!self.resolver.is_declaration_with_colliding_name(node)?
                || is_declared_in_loop
                    && !is_captured_in_function
                    && !self.maybe_hierarchy_facts().unwrap_or_default().intersects(
                        HierarchyFacts::ForStatement | HierarchyFacts::ForInOrForOfStatement,
                    ));

        Ok(emit_explicit_initializer)
    }

    pub(super) fn visit_variable_declaration_in_let_declaration_list(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
    ) -> io::Result<VisitResult> {
        let node_as_variable_declaration = node.as_variable_declaration();
        let name = node_as_variable_declaration.maybe_name();
        if is_binding_pattern(name) {
            return self.visit_variable_declaration(node);
        }

        if node_as_variable_declaration.maybe_initializer().is_none()
            && self.should_emit_explicit_initializer_for_let_declaration(node)?
        {
            return Ok(Some(
                self.factory
                    .update_variable_declaration(
                        node,
                        node_as_variable_declaration.maybe_name(),
                        None,
                        None,
                        Some(self.factory.create_void_zero()),
                    )
                    .into(),
            ));
        }

        Ok(Some(
            try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context)?
                .into(),
        ))
    }

    pub(super) fn visit_variable_declaration(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
    ) -> io::Result<VisitResult> /*<VariableDeclaration>*/ {
        let node_as_variable_declaration = node.as_variable_declaration();
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::ExportedVariableStatement,
            HierarchyFacts::None,
        );
        let updated: VisitResult/*<VariableDeclaration>*/;
        if is_binding_pattern(node_as_variable_declaration.maybe_name()) {
            updated = Some(
                try_flatten_destructuring_binding(
                    node,
                    |node: Id<Node>| self.visitor(node),
                    self.context.clone(),
                    FlattenLevel::All,
                    Option::<Id<Node>>::None,
                    Some(
                        !ancestor_facts
                            .unwrap_or_default()
                            .intersects(HierarchyFacts::ExportedVariableStatement),
                    ),
                    None,
                )?
                .into(),
            );
        } else {
            updated = Some(
                try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context)?
                    .into(),
            );
        }

        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn record_label(&self, node: Id<Node> /*LabeledStatement*/) {
        let node_as_labeled_statement = node.as_labeled_statement();
        self.converted_loop_state()
            .borrow_mut()
            .labels
            .as_mut()
            .unwrap()
            .insert(id_text(&node_as_labeled_statement.label).to_owned(), true);
    }

    pub(super) fn reset_label(&self, node: Id<Node> /*LabeledStatement*/) {
        let node_as_labeled_statement = node.as_labeled_statement();
        self.converted_loop_state()
            .borrow_mut()
            .labels
            .as_mut()
            .unwrap()
            .insert(id_text(&node_as_labeled_statement.label).to_owned(), false);
    }

    pub(super) fn visit_labeled_statement(
        &self,
        node: Id<Node>, /*LabeledStatement*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        if let Some(converted_loop_state) = self.maybe_converted_loop_state() {
            converted_loop_state
                .borrow_mut()
                .labels
                .get_or_insert_default_();
        }
        let statement = unwrap_innermost_statement_of_label(
            node,
            self.maybe_converted_loop_state().map(|_| {
                |node: Id<Node>| {
                    self.record_label(node);
                }
            }),
            self,
        );
        Ok(if is_iteration_statement(statement, false, self) {
            self.visit_iteration_statement(statement, node)?
        } else {
            Some(
                self.factory
                    .restore_enclosing_label(
                        &*try_visit_node(
                            statement,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(is_statement),
                            Some(|nodes: &[Id<Node>]| self.factory.lift_to_block(nodes)),
                        )?,
                        Some(node),
                        if self.maybe_converted_loop_state().is_some() {
                            Some(|node: Id<Node>| {
                                self.reset_label(node);
                            })
                        } else {
                            None
                        },
                    )
                    .into(),
            )
        })
    }

    pub(super) fn visit_iteration_statement(
        &self,
        node: Id<Node>,                        /*IterationStatement*/
        outermost_labeled_statement: Id<Node>, /*LabeledStatement*/
    ) -> io::Result<VisitResult> {
        Ok(match node.kind() {
            SyntaxKind::DoStatement | SyntaxKind::WhileStatement => {
                self.visit_do_or_while_statement(node, Some(outermost_labeled_statement))
            }
            SyntaxKind::ForStatement => {
                self.visit_for_statement(node, Some(outermost_labeled_statement))
            }
            SyntaxKind::ForInStatement => {
                self.visit_for_in_statement(node, Some(outermost_labeled_statement))
            }
            SyntaxKind::ForOfStatement => {
                self.visit_for_of_statement(node, Some(outermost_labeled_statement))?
            }
            _ => unreachable!(),
        })
    }

    pub(super) fn visit_iteration_statement_with_facts(
        &self,
        exclude_facts: HierarchyFacts,
        include_facts: HierarchyFacts,
        node: Id<Node>, /*IterationStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
        convert: Option<
            impl FnMut(
                Id<Node>,         /*IterationStatement*/
                Option<Id<Node>>, /*LabeledStatement*/
                Option<&[Id<Node /*Statement*/>]>,
                Option<HierarchyFacts>,
            ) -> Id<Node /*Statement*/>, /*LoopConverter*/
        >,
    ) -> VisitResult {
        self.try_visit_iteration_statement_with_facts(
            exclude_facts,
            include_facts,
            node,
            outermost_labeled_statement,
            convert.map(|mut convert| {
                move |a: Id<Node>,
                      b: Option<Id<Node>>,
                      c: Option<&[Id<Node>]>,
                      d: Option<HierarchyFacts>| Ok(convert(a, b, c, d))
            }),
        )
        .unwrap()
    }

    pub(super) fn try_visit_iteration_statement_with_facts(
        &self,
        exclude_facts: HierarchyFacts,
        include_facts: HierarchyFacts,
        node: Id<Node>, /*IterationStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
        convert: Option<
            impl FnMut(
                Id<Node>,         /*IterationStatement*/
                Option<Id<Node>>, /*LabeledStatement*/
                Option<&[Id<Node /*Statement*/>]>,
                Option<HierarchyFacts>,
            ) -> io::Result<Id<Node /*Statement*/>>, /*LoopConverter*/
        >,
    ) -> io::Result<VisitResult> {
        let ancestor_facts = self.enter_subtree(exclude_facts, include_facts);
        let updated = self.try_convert_iteration_statement_body_if_necessary(
            node,
            outermost_labeled_statement,
            ancestor_facts,
            convert,
        )?;
        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn visit_do_or_while_statement(
        &self,
        node: Id<Node>, /*DoStatement | WhileStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        self.visit_iteration_statement_with_facts(
            HierarchyFacts::DoOrWhileStatementExcludes,
            HierarchyFacts::DoOrWhileStatementIncludes,
            node,
            outermost_labeled_statement,
            Option::<
                fn(
                    Id<Node>,
                    Option<Id<Node>>,
                    Option<&[Id<Node>]>,
                    Option<HierarchyFacts>,
                ) -> Id<Node>,
            >::None,
        )
    }

    pub(super) fn visit_for_statement(
        &self,
        node: Id<Node>, /*ForStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        self.visit_iteration_statement_with_facts(
            HierarchyFacts::ForStatementExcludes,
            HierarchyFacts::ForStatementIncludes,
            node,
            outermost_labeled_statement,
            Option::<
                fn(
                    Id<Node>,
                    Option<Id<Node>>,
                    Option<&[Id<Node>]>,
                    Option<HierarchyFacts>,
                ) -> Id<Node>,
            >::None,
        )
    }

    pub(super) fn visit_each_child_of_for_statement(
        &self,
        node: Id<Node>, /*ForStatement*/
    ) -> io::Result<Id<Node>> {
        let node_as_for_statement = node.as_for_statement();
        Ok(self.factory.update_for_statement(
            node,
            try_maybe_visit_node(
                node_as_for_statement.initializer.as_deref(),
                Some(|node: Id<Node>| self.visitor_with_unused_expression_result(node)),
                Some(|node| is_for_initializer(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_maybe_visit_node(
                node_as_for_statement.condition.as_deref(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_maybe_visit_node(
                node_as_for_statement.incrementor.as_deref(),
                Some(|node: Id<Node>| self.visitor_with_unused_expression_result(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_visit_node(
                &node_as_for_statement.statement,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_statement),
                Some(|nodes: &[Id<Node>]| self.factory.lift_to_block(nodes)),
            )?,
        ))
    }

    pub(super) fn visit_for_in_statement(
        &self,
        node: Id<Node>, /*ForInStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        self.visit_iteration_statement_with_facts(
            HierarchyFacts::ForInOrForOfStatementExcludes,
            HierarchyFacts::ForInOrForOfStatementIncludes,
            node,
            outermost_labeled_statement,
            Option::<
                fn(
                    Id<Node>,
                    Option<Id<Node>>,
                    Option<&[Id<Node>]>,
                    Option<HierarchyFacts>,
                ) -> Id<Node>,
            >::None,
        )
    }

    pub(super) fn visit_for_of_statement(
        &self,
        node: Id<Node>, /*ForOfStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        self.try_visit_iteration_statement_with_facts(
            HierarchyFacts::ForInOrForOfStatementExcludes,
            HierarchyFacts::ForInOrForOfStatementIncludes,
            node,
            outermost_labeled_statement,
            Some(
                |node: Id<Node>,
                 outermost_labeled_statement: Option<Id<Node>>,
                 converted_loop_body_statements: Option<&[Id<Node>]>,
                 ancestor_facts: Option<HierarchyFacts>| {
                    Ok(if self.compiler_options.downlevel_iteration == Some(true) {
                        self.convert_for_of_statement_for_iterable(
                            node,
                            outermost_labeled_statement,
                            converted_loop_body_statements,
                            ancestor_facts,
                        )?
                    } else {
                        self.convert_for_of_statement_for_array(
                            node,
                            outermost_labeled_statement,
                            converted_loop_body_statements,
                        )?
                    })
                },
            ),
        )
    }

    pub(super) fn convert_for_of_statement_head(
        &self,
        node: Id<Node>,        /*ForOfStatement*/
        bound_value: Id<Node>, /*Expression*/
        converted_loop_body_statements: Option<&[Id<Node /*Statement*/>]>,
    ) -> io::Result<Id<Node>> {
        let node_as_for_of_statement = node.as_for_of_statement();
        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
        let initializer = &node_as_for_of_statement.initializer;
        if is_variable_declaration_list(initializer) {
            if node_as_for_of_statement
                .initializer
                .flags()
                .intersects(NodeFlags::BlockScoped)
            {
                self.enable_substitutions_for_block_scoped_bindings();
            }

            let first_original_declaration =
                first_or_undefined(&initializer.as_variable_declaration_list().declarations);
            if let Some(first_original_declaration) =
                first_original_declaration.filter(|first_original_declaration| {
                    is_binding_pattern(
                        first_original_declaration
                            .as_variable_declaration()
                            .maybe_name(),
                    )
                })
            {
                let declarations = try_flatten_destructuring_binding(
                    first_original_declaration,
                    |node: Id<Node>| self.visitor(node),
                    self.context.clone(),
                    FlattenLevel::All,
                    Some(bound_value),
                    None,
                    None,
                )?;

                let declaration_list = self
                    .factory
                    .create_variable_declaration_list(declarations.clone(), None)
                    .set_text_range(Some(&*node_as_for_of_statement.initializer))
                    .set_source_map_range(Some(
                        (&create_range(declarations[0].pos(), Some(last(&declarations).end())))
                            .into(),
                    ));

                statements.push(
                    self.factory
                        .create_variable_statement(Option::<Gc<NodeArray>>::None, declaration_list),
                );
            } else {
                statements.push(
                    self.factory
                        .create_variable_statement(
                            Option::<Gc<NodeArray>>::None,
                            self.factory
                                .create_variable_declaration_list(
                                    vec![self.factory.create_variable_declaration(
                                        if let Some(first_original_declaration) =
                                            first_original_declaration
                                        {
                                            first_original_declaration
                                                .as_variable_declaration()
                                                .maybe_name()
                                        } else {
                                            Some(self.factory.create_temp_variable(
                                                Option::<fn(Id<Node>)>::None,
                                                None,
                                            ))
                                        },
                                        None,
                                        None,
                                        Some(bound_value.node_wrapper()),
                                    )],
                                    None,
                                )
                                .set_text_range(Some(&ReadonlyTextRangeConcrete::from(
                                    move_range_pos(&**initializer, -1),
                                )))
                                .set_original_node(Some(initializer.clone())),
                        )
                        .set_text_range(Some(&ReadonlyTextRangeConcrete::from(move_range_end(
                            &**initializer,
                            -1,
                        )))),
                );
            }
        } else {
            let ref assignment = self
                .factory
                .create_assignment(initializer.clone(), bound_value.node_wrapper());
            if is_destructuring_assignment(assignment, self) {
                statements.push(
                    self.factory.create_expression_statement(
                        self.visit_binary_expression(assignment, true)?,
                    ),
                );
            } else {
                set_text_range_end(&**assignment, initializer.end());
                statements.push(
                    self.factory
                        .create_expression_statement(try_visit_node(
                            assignment,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_expression(node, self)),
                            Some(|nodes: &[Id<Node>]| self.factory.lift_to_block(nodes)),
                        )?)
                        .set_text_range(Some(&ReadonlyTextRangeConcrete::from(move_range_end(
                            &**initializer,
                            -1,
                        )))),
                );
            }
        }

        Ok(
            if let Some(converted_loop_body_statements) = converted_loop_body_statements {
                self.create_synthetic_block_for_converted_statements({
                    add_range(
                        &mut statements,
                        Some(converted_loop_body_statements),
                        None,
                        None,
                    );
                    statements
                })
            } else {
                let statement = try_visit_node(
                    &node_as_for_of_statement.statement,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_statement),
                    Some(|nodes: &[Id<Node>]| self.factory.lift_to_block(nodes)),
                )?;
                if is_block(&statement) {
                    let statement_as_block = statement.as_block();
                    self.factory.update_block(
                        &statement,
                        self.factory
                            .create_node_array(
                                Some(concatenate(
                                    statements,
                                    statement_as_block.statements.to_vec(),
                                )),
                                None,
                            )
                            .set_text_range(Some(&*statement_as_block.statements)),
                    )
                } else {
                    statements.push(statement);
                    self.create_synthetic_block_for_converted_statements(statements)
                }
            },
        )
    }

    pub(super) fn create_synthetic_block_for_converted_statements(
        &self,
        statements: Vec<Id<Node /*Statement*/>>,
    ) -> Id<Node> {
        self.factory
            .create_block(
                self.factory.create_node_array(Some(statements), None),
                Some(true),
            )
            .set_emit_flags(EmitFlags::NoSourceMap | EmitFlags::NoTokenSourceMaps)
    }

    pub(super) fn convert_for_of_statement_for_array(
        &self,
        node: Id<Node>, /*ForOfStatement*/
        outermost_labeled_statement: Option<Id<Node> /*LabeledStatement*/>,
        converted_loop_body_statements: Option<&[Id<Node /*Statement*/>]>,
    ) -> io::Result<Id<Node /*Statement*/>> {
        let node_as_for_of_statement = node.as_for_of_statement();
        let ref expression = try_visit_node(
            &node_as_for_of_statement.expression,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;

        let counter = self.factory.create_loop_variable(None);
        let rhs_reference = if is_identifier(expression) {
            self.factory
                .get_generated_name_for_node(Some(&**expression), None)
        } else {
            self.factory
                .create_temp_variable(Option::<fn(Id<Node>)>::None, None)
        };

        set_emit_flags(
            &**expression,
            EmitFlags::NoSourceMap | get_emit_flags(expression),
        );

        let ref for_statement = self
            .factory
            .create_for_statement(
                Some(
                    self.factory
                        .create_variable_declaration_list(
                            vec![
                                self.factory
                                    .create_variable_declaration(
                                        Some(counter.clone()),
                                        None,
                                        None,
                                        Some(
                                            self.factory
                                                .create_numeric_literal(Number::new(0.0), None),
                                        ),
                                    )
                                    .set_text_range(Some(&ReadonlyTextRangeConcrete::from(
                                        move_range_pos(&*node_as_for_of_statement.expression, -1),
                                    ))),
                                self.factory
                                    .create_variable_declaration(
                                        Some(rhs_reference.clone()),
                                        None,
                                        None,
                                        Some(expression.clone()),
                                    )
                                    .set_text_range(Some(&*node_as_for_of_statement.expression)),
                            ],
                            None,
                        )
                        .set_text_range(Some(&*node_as_for_of_statement.expression))
                        .set_emit_flags(EmitFlags::NoHoisting),
                ),
                Some(
                    self.factory
                        .create_less_than(
                            counter.clone(),
                            self.factory
                                .create_property_access_expression(rhs_reference.clone(), "length"),
                        )
                        .set_text_range(Some(&*node_as_for_of_statement.expression)),
                ),
                Some(
                    self.factory
                        .create_postfix_increment(counter.clone())
                        .set_text_range(Some(&*node_as_for_of_statement.expression)),
                ),
                self.convert_for_of_statement_head(
                    node,
                    &self
                        .factory
                        .create_element_access_expression(rhs_reference, counter),
                    converted_loop_body_statements,
                )?,
            )
            .set_text_range(Some(node))
            .set_emit_flags(EmitFlags::NoTokenTrailingSourceMaps)
            // TODO: this appears to be duplicate wrt above, upstream?
            .set_text_range(Some(node));

        Ok(self.factory.restore_enclosing_label(
            for_statement,
            outermost_labeled_statement,
            self.maybe_converted_loop_state().map(|_| {
                |node: Id<Node>| {
                    self.reset_label(node);
                }
            }),
        ))
    }
}
