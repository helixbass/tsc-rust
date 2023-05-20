use std::{borrow::Borrow, cmp, io};

use gc::Gc;

use super::{HierarchyFacts, TransformES2015};
use crate::{
    add_range, concatenate, create_range, first_or_undefined, flat_map, get_emit_flags,
    has_syntactic_modifier, id_text, is_binding_pattern, is_block, is_destructuring_assignment,
    is_expression, is_for_initializer, is_identifier, is_iteration_statement, is_statement,
    is_variable_declaration_list, last, move_range_end, move_range_pos, set_emit_flags,
    set_source_map_range, set_text_range_end, try_flat_map, try_flatten_destructuring_assignment,
    try_flatten_destructuring_binding, try_visit_each_child, try_visit_node,
    unwrap_innermost_statement_of_label, EmitFlags, FlattenLevel, HasInitializerInterface, Matches,
    ModifierFlags, NamedDeclarationInterface, Node, NodeArray, NodeArrayExt, NodeCheckFlags,
    NodeExt, NodeFlags, NodeInterface, Number, ReadonlyTextRange, ReadonlyTextRangeConcrete,
    SourceMapRange, SyntaxKind, TransformFlags, VisitResult,
};

impl TransformES2015 {
    pub(super) fn visit_block(
        &self,
        node: &Node, /*Block*/
        is_function_body: bool,
    ) -> io::Result<Gc<Node /*Block*/>> {
        if is_function_body {
            return try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context);
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
            try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?;
        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn visit_expression_statement(
        &self,
        node: &Node, /*ExpressionStatement*/
    ) -> io::Result<Gc<Node /*Statement*/>> {
        try_visit_each_child(
            node,
            |node: &Node| self.visitor_with_unused_expression_result(node),
            &**self.context,
        )
    }

    pub(super) fn visit_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
        expression_result_is_unused: bool,
    ) -> io::Result<Gc<Node /*ParenthesizedExpression*/>> {
        try_visit_each_child(
            node,
            |node: &Node| {
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
        node: &Node, /*BinaryExpression*/
        expression_result_is_unused: bool,
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let node_as_binary_expression = node.as_binary_expression();
        if is_destructuring_assignment(node) {
            return try_flatten_destructuring_assignment(
                node,
                Some(|node: &Node| self.visitor(node)),
                &**self.context,
                FlattenLevel::All,
                Some(!expression_result_is_unused),
                Option::<fn(&Node, &Node, Option<&dyn ReadonlyTextRange>) -> io::Result<Gc<Node>>>::None,
            );
        }
        if node_as_binary_expression.operator_token.kind() == SyntaxKind::CommaToken {
            return Ok(self.factory.update_binary_expression(
                node,
                try_visit_node(
                    Some(&*node_as_binary_expression.left),
                    Some(|node: &Node| self.visitor_with_unused_expression_result(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )?
                .unwrap(),
                node_as_binary_expression.operator_token.clone(),
                try_visit_node(
                    Some(&*node_as_binary_expression.right),
                    Some(|node: &Node| {
                        Ok(if expression_result_is_unused {
                            self.visitor_with_unused_expression_result(node)?
                        } else {
                            self.visitor(node)?
                        })
                    }),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )?
                .unwrap(),
            ));
        }
        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_comma_list_expression(
        &self,
        node: &Node, /*CommaListExpression*/
        expression_result_is_unused: bool,
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let node_as_comma_list_expression = node.as_comma_list_expression();
        if expression_result_is_unused {
            return try_visit_each_child(
                node,
                |node: &Node| self.visitor_with_unused_expression_result(node),
                &**self.context,
            );
        }
        let mut result: Option<Vec<Gc<Node /*Expression*/>>> = Default::default();
        for (i, element) in node_as_comma_list_expression.elements.iter().enumerate() {
            let visited = try_visit_node(
                Some(&**element),
                Some(|node: &Node| {
                    Ok(if i < node_as_comma_list_expression.elements.len() - 1 {
                        self.visitor_with_unused_expression_result(node)?
                    } else {
                        self.visitor(node)?
                    })
                }),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?
            .unwrap();
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
        node: &Node, /*VariableStatement*/
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
        node: &Node, /*VariableStatement*/
    ) -> io::Result<Option<Gc<Node /*Statement*/>>> {
        let node_as_variable_statement = node.as_variable_statement();
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::None,
            if has_syntactic_modifier(node, ModifierFlags::Export) {
                HierarchyFacts::ExportedVariableStatement
            } else {
                HierarchyFacts::None
            },
        );
        let updated: Option<Gc<Node /*Statement*/>>;
        if let Some(converted_loop_state) = self.maybe_converted_loop_state().filter(|_| {
            !node_as_variable_statement
                .declaration_list
                .flags()
                .intersects(NodeFlags::BlockScoped)
                && !self.is_variable_statement_of_type_script_class_wrapper(node)
        }) {
            let mut assignments: Option<Vec<Gc<Node /*Expression*/>>> = Default::default();
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
                    let assignment: Gc<Node /*Expression*/>;
                    if is_binding_pattern(decl_as_variable_declaration.maybe_name()) {
                        assignment = try_flatten_destructuring_assignment(
                            decl,
                            Some(|node: &Node| self.visitor(node)),
                            &**self.context,
                            FlattenLevel::All,
                            None,
                            Option::<
                                fn(
                                    &Node,
                                    &Node,
                                    Option<&dyn ReadonlyTextRange>,
                                ) -> io::Result<Gc<Node>>,
                            >::None,
                        )?;
                    } else {
                        assignment = self
                            .factory
                            .create_binary_expression(
                                decl_as_variable_declaration.name(),
                                SyntaxKind::EqualsToken,
                                try_visit_node(
                                    Some(decl_initializer),
                                    Some(|node: &Node| self.visitor(node)),
                                    Some(is_expression),
                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                )?
                                .unwrap(),
                            )
                            .wrap()
                            .set_text_range(Some(&**decl));
                    }

                    assignments
                        .get_or_insert_with(|| Default::default())
                        .push(assignment);
                }
            }
            if let Some(assignments) = assignments {
                updated = Some(
                    self.factory
                        .create_expression_statement(self.factory.inline_expressions(&assignments))
                        .wrap()
                        .set_text_range(Some(node)),
                );
            } else {
                updated = None;
            }
        } else {
            updated = Some(try_visit_each_child(
                node,
                |node: &Node| self.visitor(node),
                &**self.context,
            )?);
        }

        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn visit_variable_declaration_list(
        &self,
        node: &Node, /*VariableDeclarationList*/
    ) -> io::Result<Gc<Node /*VariableDeclarationList*/>> {
        let node_as_variable_declaration_list = node.as_variable_declaration_list();
        if node.flags().intersects(NodeFlags::BlockScoped)
            || node
                .transform_flags()
                .intersects(TransformFlags::ContainsBindingPattern)
        {
            if node.flags().intersects(NodeFlags::BlockScoped) {
                self.enable_substitutions_for_block_scoped_bindings();
            }

            let declarations: Vec<Gc<Node>> = try_flat_map(
                Some(&node_as_variable_declaration_list.declarations),
                |declaration: &Gc<Node>, _| -> io::Result<_> {
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
                .wrap()
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
        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn get_range_union(
        &self,
        declarations: &[Gc<Node>],
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
        node: &Node, /*VariableDeclaration*/
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
        node: &Node, /*VariableDeclaration*/
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
            try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?.into(),
        ))
    }

    pub(super) fn visit_variable_declaration(
        &self,
        node: &Node, /*VariableDeclaration*/
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
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    FlattenLevel::All,
                    Option::<&Node>::None,
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
                try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?
                    .into(),
            );
        }

        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn record_label(&self, node: &Node /*LabeledStatement*/) {
        let node_as_labeled_statement = node.as_labeled_statement();
        self.converted_loop_state()
            .borrow_mut()
            .labels
            .as_mut()
            .unwrap()
            .insert(id_text(&node_as_labeled_statement.label).to_owned(), true);
    }

    pub(super) fn reset_label(&self, node: &Node /*LabeledStatement*/) {
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
        node: &Node, /*LabeledStatement*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        self.maybe_converted_loop_state()
            .map(|converted_loop_state| {
                converted_loop_state
                    .borrow_mut()
                    .labels
                    .get_or_insert_with(|| Default::default());
            });
        let ref statement = unwrap_innermost_statement_of_label(
            node,
            self.maybe_converted_loop_state().map(|_| {
                |node: &Node| {
                    self.record_label(node);
                }
            }),
        );
        Ok(if is_iteration_statement(statement, false) {
            self.visit_iteration_statement(statement, node)?
        } else {
            Some(
                self.factory
                    .restore_enclosing_label(
                        &try_visit_node(
                            Some(&**statement),
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_statement),
                            Some(|nodes: &[Gc<Node>]| self.factory.lift_to_block(nodes)),
                        )?
                        .unwrap(),
                        Some(node),
                        if self.maybe_converted_loop_state().is_some() {
                            Some(|node: &Node| {
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
        node: &Node,                        /*IterationStatement*/
        outermost_labeled_statement: &Node, /*LabeledStatement*/
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
        node: &Node, /*IterationStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
        convert: Option<
            impl FnMut(
                &Node,         /*IterationStatement*/
                Option<&Node>, /*LabeledStatement*/
                Option<&[Gc<Node /*Statement*/>]>,
                Option<HierarchyFacts>,
            ) -> Gc<Node /*Statement*/>, /*LoopConverter*/
        >,
    ) -> VisitResult {
        self.try_visit_iteration_statement_with_facts(
            exclude_facts,
            include_facts,
            node,
            outermost_labeled_statement,
            convert.map(|mut convert| {
                move |a: &Node,
                      b: Option<&Node>,
                      c: Option<&[Gc<Node>]>,
                      d: Option<HierarchyFacts>| Ok(convert(a, b, c, d))
            }),
        )
        .unwrap()
    }

    pub(super) fn try_visit_iteration_statement_with_facts(
        &self,
        exclude_facts: HierarchyFacts,
        include_facts: HierarchyFacts,
        node: &Node, /*IterationStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
        convert: Option<
            impl FnMut(
                &Node,         /*IterationStatement*/
                Option<&Node>, /*LabeledStatement*/
                Option<&[Gc<Node /*Statement*/>]>,
                Option<HierarchyFacts>,
            ) -> io::Result<Gc<Node /*Statement*/>>, /*LoopConverter*/
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
        node: &Node, /*DoStatement | WhileStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        self.visit_iteration_statement_with_facts(
            HierarchyFacts::DoOrWhileStatementExcludes,
            HierarchyFacts::DoOrWhileStatementIncludes,
            node,
            outermost_labeled_statement,
            Option::<
                fn(&Node, Option<&Node>, Option<&[Gc<Node>]>, Option<HierarchyFacts>) -> Gc<Node>,
            >::None,
        )
    }

    pub(super) fn visit_for_statement(
        &self,
        node: &Node, /*ForStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        self.visit_iteration_statement_with_facts(
            HierarchyFacts::ForStatementExcludes,
            HierarchyFacts::ForStatementIncludes,
            node,
            outermost_labeled_statement,
            Option::<
                fn(&Node, Option<&Node>, Option<&[Gc<Node>]>, Option<HierarchyFacts>) -> Gc<Node>,
            >::None,
        )
    }

    pub(super) fn visit_each_child_of_for_statement(
        &self,
        node: &Node, /*ForStatement*/
    ) -> io::Result<Gc<Node>> {
        let node_as_for_statement = node.as_for_statement();
        Ok(self.factory.update_for_statement(
            node,
            try_visit_node(
                node_as_for_statement.initializer.as_deref(),
                Some(|node: &Node| self.visitor_with_unused_expression_result(node)),
                Some(is_for_initializer),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            try_visit_node(
                node_as_for_statement.condition.as_deref(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            try_visit_node(
                node_as_for_statement.incrementor.as_deref(),
                Some(|node: &Node| self.visitor_with_unused_expression_result(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            try_visit_node(
                Some(&*node_as_for_statement.statement),
                Some(|node: &Node| self.visitor(node)),
                Some(is_statement),
                Some(|nodes: &[Gc<Node>]| self.factory.lift_to_block(nodes)),
            )?
            .unwrap(),
        ))
    }

    pub(super) fn visit_for_in_statement(
        &self,
        node: &Node, /*ForInStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        self.visit_iteration_statement_with_facts(
            HierarchyFacts::ForInOrForOfStatementExcludes,
            HierarchyFacts::ForInOrForOfStatementIncludes,
            node,
            outermost_labeled_statement,
            Option::<
                fn(&Node, Option<&Node>, Option<&[Gc<Node>]>, Option<HierarchyFacts>) -> Gc<Node>,
            >::None,
        )
    }

    pub(super) fn visit_for_of_statement(
        &self,
        node: &Node, /*ForOfStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        self.try_visit_iteration_statement_with_facts(
            HierarchyFacts::ForInOrForOfStatementExcludes,
            HierarchyFacts::ForInOrForOfStatementIncludes,
            node,
            outermost_labeled_statement,
            Some(
                |node: &Node,
                 outermost_labeled_statement: Option<&Node>,
                 converted_loop_body_statements: Option<&[Gc<Node>]>,
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
        node: &Node,        /*ForOfStatement*/
        bound_value: &Node, /*Expression*/
        converted_loop_body_statements: Option<&[Gc<Node /*Statement*/>]>,
    ) -> io::Result<Gc<Node>> {
        let node_as_for_of_statement = node.as_for_of_statement();
        let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();
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
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    FlattenLevel::All,
                    Some(bound_value),
                    None,
                    None,
                )?;

                let declaration_list = self
                    .factory
                    .create_variable_declaration_list(declarations.clone(), None)
                    .wrap()
                    .set_text_range(Some(&*node_as_for_of_statement.initializer))
                    .set_source_map_range(Some(
                        (&create_range(declarations[0].pos(), Some(last(&declarations).end())))
                            .into(),
                    ));

                statements.push(
                    self.factory
                        .create_variable_statement(Option::<Gc<NodeArray>>::None, declaration_list)
                        .wrap(),
                );
            } else {
                statements.push(
                    self.factory
                        .create_variable_statement(
                            Option::<Gc<NodeArray>>::None,
                            self.factory
                                .create_variable_declaration_list(
                                    vec![self
                                        .factory
                                        .create_variable_declaration(
                                            if let Some(first_original_declaration) =
                                                first_original_declaration
                                            {
                                                first_original_declaration
                                                    .as_variable_declaration()
                                                    .maybe_name()
                                            } else {
                                                Some(self.factory.create_temp_variable(
                                                    Option::<fn(&Node)>::None,
                                                    None,
                                                ))
                                            },
                                            None,
                                            None,
                                            Some(bound_value.node_wrapper()),
                                        )
                                        .wrap()],
                                    None,
                                )
                                .wrap()
                                .set_text_range(Some(&ReadonlyTextRangeConcrete::from(
                                    move_range_pos(&**initializer, -1),
                                )))
                                .set_original_node(Some(initializer.clone())),
                        )
                        .wrap()
                        .set_text_range(Some(&ReadonlyTextRangeConcrete::from(move_range_end(
                            &**initializer,
                            -1,
                        )))),
                );
            }
        } else {
            let ref assignment = self
                .factory
                .create_assignment(initializer.clone(), bound_value.node_wrapper())
                .wrap();
            if is_destructuring_assignment(assignment) {
                statements.push(
                    self.factory
                        .create_expression_statement(
                            self.visit_binary_expression(assignment, true)?,
                        )
                        .wrap(),
                );
            } else {
                set_text_range_end(&**assignment, initializer.end());
                statements.push(
                    self.factory
                        .create_expression_statement(
                            try_visit_node(
                                Some(&**assignment),
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_expression),
                                Some(|nodes: &[Gc<Node>]| self.factory.lift_to_block(nodes)),
                            )?
                            .unwrap(),
                        )
                        .wrap()
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
                    Some(&*node_as_for_of_statement.statement),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_statement),
                    Some(|nodes: &[Gc<Node>]| self.factory.lift_to_block(nodes)),
                )?
                .unwrap();
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
        statements: Vec<Gc<Node /*Statement*/>>,
    ) -> Gc<Node> {
        self.factory
            .create_block(
                self.factory.create_node_array(Some(statements), None),
                Some(true),
            )
            .wrap()
            .set_emit_flags(EmitFlags::NoSourceMap | EmitFlags::NoTokenSourceMaps)
    }

    pub(super) fn convert_for_of_statement_for_array(
        &self,
        node: &Node, /*ForOfStatement*/
        outermost_labeled_statement: Option<&Node /*LabeledStatement*/>,
        converted_loop_body_statements: Option<&[Gc<Node /*Statement*/>]>,
    ) -> io::Result<Gc<Node /*Statement*/>> {
        let node_as_for_of_statement = node.as_for_of_statement();
        let ref expression = try_visit_node(
            Some(&*node_as_for_of_statement.expression),
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?
        .unwrap();

        let counter = self.factory.create_loop_variable(None);
        let rhs_reference = if is_identifier(expression) {
            self.factory
                .get_generated_name_for_node(Some(&**expression), None)
        } else {
            self.factory
                .create_temp_variable(Option::<fn(&Node)>::None, None)
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
                                                .create_numeric_literal(Number::new(0.0), None)
                                                .wrap(),
                                        ),
                                    )
                                    .wrap()
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
                                    .wrap()
                                    .set_text_range(Some(&*node_as_for_of_statement.expression)),
                            ],
                            None,
                        )
                        .wrap()
                        .set_text_range(Some(&*node_as_for_of_statement.expression))
                        .set_emit_flags(EmitFlags::NoHoisting),
                ),
                Some(
                    self.factory
                        .create_less_than(
                            counter.clone(),
                            self.factory
                                .create_property_access_expression(rhs_reference.clone(), "length")
                                .wrap(),
                        )
                        .wrap()
                        .set_text_range(Some(&*node_as_for_of_statement.expression)),
                ),
                Some(
                    self.factory
                        .create_postfix_increment(counter.clone())
                        .wrap()
                        .set_text_range(Some(&*node_as_for_of_statement.expression)),
                ),
                self.convert_for_of_statement_head(
                    node,
                    &self
                        .factory
                        .create_element_access_expression(rhs_reference, counter)
                        .wrap(),
                    converted_loop_body_statements,
                )?,
            )
            .wrap()
            .set_text_range(Some(node))
            .set_emit_flags(EmitFlags::NoTokenTrailingSourceMaps)
            // TODO: this appears to be duplicate wrt above, upstream?
            .set_text_range(Some(node));

        Ok(self.factory.restore_enclosing_label(
            for_statement,
            outermost_labeled_statement,
            self.maybe_converted_loop_state().map(|_| {
                |node: &Node| {
                    self.reset_label(node);
                }
            }),
        ))
    }
}
