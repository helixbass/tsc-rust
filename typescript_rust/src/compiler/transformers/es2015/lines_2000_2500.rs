use std::{borrow::Borrow, cmp, io};

use gc::Gc;

use super::{HierarchyFacts, TransformES2015};
use crate::{
    create_range, flat_map, get_emit_flags, has_syntactic_modifier, is_binding_pattern,
    is_destructuring_assignment, is_expression, last, set_source_map_range,
    try_flatten_destructuring_assignment, try_visit_each_child, try_visit_node, EmitFlags,
    FlattenLevel, HasInitializerInterface, Matches, ModifierFlags, NamedDeclarationInterface, Node,
    NodeArrayExt, NodeCheckFlags, NodeExt, NodeFlags, NodeInterface, ReadonlyTextRange,
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
        if self.maybe_converted_loop_state().is_some()
            && !node_as_variable_statement
                .declaration_list
                .flags()
                .intersects(NodeFlags::BlockScoped)
            && !self.is_variable_statement_of_type_script_class_wrapper(node)
        {
            let mut assignments: Option<Vec<Gc<Node /*Expression*/>>> = Default::default();
            for decl in &node_as_variable_statement
                .declaration_list
                .as_variable_declaration_list()
                .declarations
            {
                let decl_as_variable_declaration = decl.as_variable_declaration();
                self.hoist_variable_declaration_declared_in_converted_loop(
                    &mut self.converted_loop_state_mut(),
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

            let declarations: Vec<Gc<Node>> = flat_map(
                Some(&node_as_variable_declaration_list.declarations),
                |declaration: &Gc<Node>, _| {
                    if node.flags().intersects(NodeFlags::Let) {
                        self.visit_variable_declaration_in_let_declaration_list(declaration)
                    } else {
                        self.visit_variable_declaration(declaration)
                    }
                    .map(Into::into)
                    .unwrap_or_default()
                },
            );

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
        _node: &Node, /*VariableDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_variable_declaration(
        &self,
        _node: &Node, /*VariableDeclaration*/
    ) -> VisitResult /*<VariableDeclaration>*/ {
        unimplemented!()
    }

    pub(super) fn visit_labeled_statement(
        &self,
        _node: &Node, /*LabeledStatement*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_do_or_while_statement(
        &self,
        _node: &Node, /*DoStatement | WhileStatement*/
        _outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_for_statement(
        &self,
        _node: &Node, /*ForStatement*/
        _outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_for_in_statement(
        &self,
        _node: &Node, /*ForInStatement*/
        _outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_for_of_statement(
        &self,
        _node: &Node, /*ForOfStatement*/
        _outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }
}
