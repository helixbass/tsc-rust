use std::io;

use id_arena::Id;

use super::TransformSystemModule;
use crate::{
    get_emit_flags, get_text_of_identifier_or_literal, has_syntactic_modifier, id_text,
    is_binding_pattern, is_expression, is_for_initializer, is_generated_identifier, is_identifier,
    is_omitted_expression, is_variable_declaration_list, set_emit_flags, EmitFlags,
    GetOrInsertDefault, HasInitializerInterface, ModifierFlags, Node, NodeArray, NodeExt,
    NodeInterface, SyntaxKind, VisitResult, _d, is_block, is_case_block, is_case_or_default_clause,
    is_destructuring_assignment, is_import_call, is_statement, released, try_maybe_visit_node,
    try_visit_each_child, try_visit_iteration_body, try_visit_node, try_visit_nodes, InArena,
    TransformFlags,
};

impl TransformSystemModule {
    pub(super) fn append_exports_of_binding_element(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        decl: Id<Node>, /*VariableDeclaration | BindingElement*/
        export_self: bool,
    ) /*: Statement[] | undefined*/
    {
        if self.module_info().ref_(self).export_equals.is_some() {
            return /*statements*/;
        }

        let decl_name = decl.ref_(self).as_named_declaration().name();
        if is_binding_pattern(Some(&*decl_name.ref_(self))) {
            for &element in &*decl_name.ref_(self).as_has_elements().elements().ref_(self) {
                if !is_omitted_expression(&element.ref_(self)) {
                    self.append_exports_of_binding_element(statements, element, export_self);
                }
            }
        } else if !is_generated_identifier(&decl_name.ref_(self)) {
            let mut exclude_name = None;
            if export_self {
                self.append_export_statement(
                    statements,
                    decl_name,
                    self.factory.ref_(self).get_local_name(decl, None, None),
                    None,
                );
                exclude_name = Some(id_text(&decl_name.ref_(self)).to_owned());
            }

            self.append_exports_of_declaration(statements, decl, exclude_name.as_deref());
        }

        // return statements;
    }

    pub(super) fn append_exports_of_hoisted_declaration(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        decl: Id<Node>, /*ClassDeclaration | FunctionDeclaration*/
    ) /*: Statement[] | undefined*/
    {
        if self.module_info().ref_(self).export_equals.is_some() {
            return /*statements*/;
        }

        let mut exclude_name = None;
        if has_syntactic_modifier(decl, ModifierFlags::Export, self) {
            let export_name = if has_syntactic_modifier(decl, ModifierFlags::Default, self) {
                self.factory
                    .ref_(self)
                    .create_string_literal("default".to_owned(), None, None)
            } else {
                decl.ref_(self).as_named_declaration().name()
            };
            self.append_export_statement(
                statements,
                export_name,
                self.factory.ref_(self).get_local_name(decl, None, None),
                None,
            );
            exclude_name =
                Some(get_text_of_identifier_or_literal(&export_name.ref_(self)).into_owned());
        }

        if decl
            .ref_(self)
            .as_named_declaration()
            .maybe_name()
            .is_some()
        {
            self.append_exports_of_declaration(statements, decl, exclude_name.as_deref());
        }

        // return statements;
    }

    pub(super) fn append_exports_of_declaration(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        decl: Id<Node>, /*Declaration*/
        exclude_name: Option<&str>,
    ) /*: Statement[] | undefined*/
    {
        if self.module_info().ref_(self).export_equals.is_some() {
            return /*statements*/;
        }

        let name = self
            .factory
            .ref_(self)
            .get_declaration_name(Some(decl), None, None);
        let module_info = self.module_info();
        let module_info_ref = module_info.ref_(self);
        let export_specifiers = module_info_ref
            .export_specifiers
            .get(id_text(&name.ref_(self)));
        if let Some(export_specifiers) = export_specifiers {
            for export_specifier in export_specifiers {
                let export_specifier_ref = export_specifier.ref_(self);
                let export_specifier_as_export_specifier =
                    export_specifier_ref.as_export_specifier();
                if Some(
                    &*export_specifier_as_export_specifier
                        .name
                        .ref_(self)
                        .as_identifier()
                        .escaped_text,
                ) != exclude_name
                {
                    self.append_export_statement(
                        statements,
                        export_specifier_as_export_specifier.name,
                        name,
                        None,
                    );
                }
            }
        }
        // return statements;
    }

    pub(super) fn append_export_statement(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        export_name: Id<Node>, /*Identifier | StringLiteral*/
        expression: Id<Node>,  /*Expression*/
        allow_comments: Option<bool>,
    ) /*: Statement[] | undefined*/
    {
        statements
            .get_or_insert_default_()
            .push(self.create_export_statement(export_name, expression, allow_comments));
        // return statements;
    }

    pub(super) fn create_export_statement(
        &self,
        name: Id<Node>,  /*Identifier | StringLiteral*/
        value: Id<Node>, /*Expression*/
        allow_comments: Option<bool>,
    ) -> Id<Node> {
        let statement = self
            .factory
            .ref_(self)
            .create_expression_statement(self.create_export_expression(name, value))
            .start_on_new_line(self);
        if allow_comments != Some(true) {
            set_emit_flags(statement, EmitFlags::NoComments, self);
        }

        statement
    }

    pub(super) fn create_export_expression(
        &self,
        name: Id<Node>,  /*Identifier | StringLiteral*/
        value: Id<Node>, /*Expression*/
    ) -> Id<Node> {
        let export_name = if is_identifier(&name.ref_(self)) {
            self.factory
                .ref_(self)
                .create_string_literal_from_node(name)
        } else {
            name
        };
        set_emit_flags(
            value,
            get_emit_flags(value, self) | EmitFlags::NoComments,
            self,
        );
        self.factory
            .ref_(self)
            .create_call_expression(
                self.export_function(),
                Option::<Id<NodeArray>>::None,
                Some(vec![export_name, value]),
            )
            .set_comment_range(&*value.ref_(self), self)
    }

    pub(super) fn top_level_nested_visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/
    {
        Ok(match released!(node.ref_(self).kind()) {
            SyntaxKind::VariableStatement => self.visit_variable_statement(node)?,
            SyntaxKind::FunctionDeclaration => self.visit_function_declaration(node)?,
            SyntaxKind::ClassDeclaration => self.visit_class_declaration(node)?,
            SyntaxKind::ForStatement => self.visit_for_statement(node, true)?,
            SyntaxKind::ForInStatement => self.visit_for_in_statement(node)?,
            SyntaxKind::ForOfStatement => self.visit_for_of_statement(node)?,
            SyntaxKind::DoStatement => self.visit_do_statement(node)?,
            SyntaxKind::WhileStatement => self.visit_while_statement(node)?,
            SyntaxKind::LabeledStatement => self.visit_labeled_statement(node)?,
            SyntaxKind::WithStatement => self.visit_with_statement(node)?,
            SyntaxKind::SwitchStatement => self.visit_switch_statement(node)?,
            SyntaxKind::CaseBlock => Some(self.visit_case_block(node)?.into()),
            SyntaxKind::CaseClause => self.visit_case_clause(node)?,
            SyntaxKind::DefaultClause => self.visit_default_clause(node)?,
            SyntaxKind::TryStatement => self.visit_try_statement(node)?,
            SyntaxKind::CatchClause => Some(self.visit_catch_clause(node)?.into()),
            SyntaxKind::Block => Some(self.visit_block(node)?.into()),
            SyntaxKind::MergeDeclarationMarker => self.visit_merge_declaration_marker(node),
            SyntaxKind::EndOfDeclarationMarker => self.visit_end_of_declaration_marker(node),
            _ => self.visitor(node)?,
        })
    }

    pub(super) fn visit_for_statement(
        &self,
        node: Id<Node>, /*ForStatement*/
        is_top_level: bool,
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_for_statement = node_ref.as_for_statement();
        let saved_enclosing_block_scoped_container = self.maybe_enclosing_block_scoped_container();
        self.set_enclosing_block_scoped_container(Some(node));

        let node = self.factory.ref_(self).update_for_statement(
            node,
            try_maybe_visit_node(
                node_as_for_statement.initializer,
                Some(|node: Id<Node>| {
                    Ok(if is_top_level {
                        Some(self.visit_for_initializer(node)?.into())
                    } else {
                        self.discarded_value_visitor(node)?
                    })
                }),
                Some(|node| is_for_initializer(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_maybe_visit_node(
                node_as_for_statement.condition,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_maybe_visit_node(
                node_as_for_statement.incrementor,
                Some(|node: Id<Node>| self.discarded_value_visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_visit_iteration_body(
                node_as_for_statement.statement,
                |node: Id<Node>| {
                    if is_top_level {
                        self.top_level_nested_visitor(node)
                    } else {
                        self.visitor(node)
                    }
                },
                &*self.context.ref_(self),
                self,
            )?,
        );

        self.set_enclosing_block_scoped_container(saved_enclosing_block_scoped_container);
        Ok(Some(node.into()))
    }

    pub(super) fn visit_for_in_statement(
        &self,
        node: Id<Node>, /*ForInStatement*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let saved_enclosing_block_scoped_container = self.maybe_enclosing_block_scoped_container();
        self.set_enclosing_block_scoped_container(Some(node));

        let node = self.factory.ref_(self).update_for_in_statement(
            node,
            self.visit_for_initializer(released!(
                node.ref_(self).as_for_in_statement().initializer
            ))?,
            try_visit_node(
                released!(node.ref_(self).as_for_in_statement().expression),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_visit_iteration_body(
                released!(node.ref_(self).as_for_in_statement().statement),
                |node: Id<Node>| self.top_level_nested_visitor(node),
                &*self.context.ref_(self),
                self,
            )?,
        );

        self.set_enclosing_block_scoped_container(saved_enclosing_block_scoped_container);
        Ok(Some(node.into()))
    }

    pub(super) fn visit_for_of_statement(
        &self,
        node: Id<Node>, /*ForOfStatement*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_for_of_statement = node_ref.as_for_of_statement();
        let saved_enclosing_block_scoped_container = self.maybe_enclosing_block_scoped_container();
        self.set_enclosing_block_scoped_container(Some(node));

        let node = self.factory.ref_(self).update_for_of_statement(
            node,
            node_as_for_of_statement.await_modifier.clone(),
            self.visit_for_initializer(node_as_for_of_statement.initializer)?,
            try_visit_node(
                node_as_for_of_statement.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_visit_iteration_body(
                node_as_for_of_statement.statement,
                |node: Id<Node>| self.top_level_nested_visitor(node),
                &*self.context.ref_(self),
                self,
            )?,
        );

        self.set_enclosing_block_scoped_container(saved_enclosing_block_scoped_container);
        Ok(Some(node.into()))
    }

    pub(super) fn should_hoist_for_initializer(
        &self,
        node: Id<Node>, /*ForInitializer*/
    ) -> bool {
        is_variable_declaration_list(&node.ref_(self))
            && self.should_hoist_variable_declaration_list(node)
    }

    pub(super) fn visit_for_initializer(
        &self,
        node: Id<Node>, /*ForInitializer*/
    ) -> io::Result<Id<Node /*ForInitializer*/>> {
        Ok(if self.should_hoist_for_initializer(node) {
            let mut expressions: Option<Vec<Id<Node /*Expression*/>>> = _d();
            for &variable in
                &*released!(node.ref_(self).as_variable_declaration_list().declarations).ref_(self)
            {
                expressions
                    .get_or_insert_default_()
                    .push(self.transform_initialized_variable(variable, false)?);
                if variable
                    .ref_(self)
                    .as_variable_declaration()
                    .maybe_initializer()
                    .is_none()
                {
                    self.hoist_binding_element(variable);
                }
            }

            expressions.map_or_else(
                || self.factory.ref_(self).create_omitted_expression(),
                |expressions| self.factory.ref_(self).inline_expressions(&expressions),
            )
        } else {
            try_visit_node(
                node,
                Some(|node: Id<Node>| self.discarded_value_visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?
        })
    }

    pub(super) fn visit_do_statement(
        &self,
        node: Id<Node>, /*DoStatement*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_do_statement = node_ref.as_do_statement();
        Ok(Some(
            self.factory
                .ref_(self)
                .update_do_statement(
                    node,
                    try_visit_iteration_body(
                        node_as_do_statement.statement,
                        |node: Id<Node>| self.top_level_nested_visitor(node),
                        &*self.context.ref_(self),
                        self,
                    )?,
                    try_visit_node(
                        node_as_do_statement.expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_while_statement(
        &self,
        node: Id<Node>, /*WhileStatement*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_while_statement = node_ref.as_while_statement();
        Ok(Some(
            self.factory
                .ref_(self)
                .update_while_statement(
                    node,
                    try_visit_node(
                        node_as_while_statement.expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    try_visit_iteration_body(
                        node_as_while_statement.statement,
                        |node: Id<Node>| self.top_level_nested_visitor(node),
                        &*self.context.ref_(self),
                        self,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_labeled_statement(
        &self,
        node: Id<Node>, /*LabeledStatement*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_labeled_statement = node_ref.as_labeled_statement();
        Ok(Some(
            self.factory
                .ref_(self)
                .update_labeled_statement(
                    node,
                    node_as_labeled_statement.label.clone(),
                    try_visit_node(
                        node_as_labeled_statement.statement,
                        Some(|node: Id<Node>| self.top_level_nested_visitor(node)),
                        Some(|node| is_statement(node, self)),
                        Some(|nodes: &[Id<Node>]| self.factory.ref_(self).lift_to_block(nodes)),
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_with_statement(
        &self,
        node: Id<Node>, /*WithStatement*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_with_statement = node_ref.as_with_statement();
        Ok(Some(
            self.factory
                .ref_(self)
                .update_with_statement(
                    node,
                    try_visit_node(
                        node_as_with_statement.expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    try_visit_node(
                        node_as_with_statement.statement,
                        Some(|node: Id<Node>| self.top_level_nested_visitor(node)),
                        Some(|node| is_statement(node, self)),
                        Some(|nodes: &[Id<Node>]| self.factory.ref_(self).lift_to_block(nodes)),
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_switch_statement(
        &self,
        node: Id<Node>, /*SwitchStatement*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_switch_statement = node_ref.as_switch_statement();
        Ok(Some(
            self.factory
                .ref_(self)
                .update_switch_statement(
                    node,
                    try_visit_node(
                        node_as_switch_statement.expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    try_visit_node(
                        node_as_switch_statement.case_block,
                        Some(|node: Id<Node>| self.top_level_nested_visitor(node)),
                        Some(|node: Id<Node>| is_case_block(&node.ref_(self))),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_case_block(
        &self,
        node: Id<Node>, /*CaseBlock*/
    ) -> io::Result<Id<Node /*CaseBlock*/>> {
        let node_ref = node.ref_(self);
        let node_as_case_block = node_ref.as_case_block();
        let saved_enclosing_block_scoped_container = self.maybe_enclosing_block_scoped_container();
        self.set_enclosing_block_scoped_container(Some(node));

        let node = self.factory.ref_(self).update_case_block(
            node,
            try_visit_nodes(
                node_as_case_block.clauses,
                Some(|node: Id<Node>| self.top_level_nested_visitor(node)),
                Some(|node: Id<Node>| is_case_or_default_clause(&node.ref_(self))),
                None,
                None,
                self,
            )?,
        );

        self.set_enclosing_block_scoped_container(saved_enclosing_block_scoped_container);
        Ok(node)
    }

    pub(super) fn visit_case_clause(
        &self,
        node: Id<Node>, /*CaseClause*/
    ) -> io::Result<VisitResult> /*<CaseOrDefaultClause>*/ {
        let node_ref = node.ref_(self);
        let node_as_case_clause = node_ref.as_case_clause();
        Ok(Some(
            self.factory
                .ref_(self)
                .update_case_clause(
                    node,
                    try_visit_node(
                        node_as_case_clause.expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    try_visit_nodes(
                        node_as_case_clause.statements,
                        Some(|node: Id<Node>| self.top_level_nested_visitor(node)),
                        Some(|node| is_statement(node, self)),
                        None,
                        None,
                        self,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_default_clause(
        &self,
        node: Id<Node>, /*DefaultClause*/
    ) -> io::Result<VisitResult> /*<CaseOrDefaultClause>*/ {
        Ok(Some(
            try_visit_each_child(
                node,
                |node: Id<Node>| self.top_level_nested_visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .into(),
        ))
    }

    pub(super) fn visit_try_statement(
        &self,
        node: Id<Node>, /*TryStatement*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        Ok(Some(
            try_visit_each_child(
                node,
                |node: Id<Node>| self.top_level_nested_visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .into(),
        ))
    }

    pub(super) fn visit_catch_clause(
        &self,
        node: Id<Node>, /*CatchClause*/
    ) -> io::Result<Id<Node /*CatchClause*/>> {
        let node_ref = node.ref_(self);
        let node_as_catch_clause = node_ref.as_catch_clause();
        let saved_enclosing_block_scoped_container = self.maybe_enclosing_block_scoped_container();
        self.set_enclosing_block_scoped_container(Some(node));

        let node = self.factory.ref_(self).update_catch_clause(
            node,
            node_as_catch_clause.variable_declaration.clone(),
            try_visit_node(
                node_as_catch_clause.block,
                Some(|node: Id<Node>| self.top_level_nested_visitor(node)),
                Some(|node: Id<Node>| is_block(&node.ref_(self))),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
        );

        self.set_enclosing_block_scoped_container(saved_enclosing_block_scoped_container);
        Ok(node)
    }

    pub(super) fn visit_block(
        &self,
        node: Id<Node>, /*Block*/
    ) -> io::Result<Id<Node /*Block*/>> {
        let saved_enclosing_block_scoped_container = self.maybe_enclosing_block_scoped_container();
        self.set_enclosing_block_scoped_container(Some(node));

        let node = try_visit_each_child(
            node,
            |node: Id<Node>| self.top_level_nested_visitor(node),
            &*self.context.ref_(self),
            self,
        )?;

        self.set_enclosing_block_scoped_container(saved_enclosing_block_scoped_container);
        Ok(node)
    }

    pub(super) fn visitor_worker(
        &self,
        node: Id<Node>,
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> /*<Node>*/ {
        if !node.ref_(self).transform_flags().intersects(
            TransformFlags::ContainsDestructuringAssignment
                | TransformFlags::ContainsDynamicImport
                | TransformFlags::ContainsUpdateExpressionForIdentifier,
        ) {
            return Ok(Some(node.into()));
        }
        match node.ref_(self).kind() {
            SyntaxKind::ForStatement => return self.visit_for_statement(node, false),
            SyntaxKind::ExpressionStatement => return self.visit_expression_statement(node),
            SyntaxKind::ParenthesizedExpression => {
                return self.visit_parenthesized_expression(node, value_is_discarded)
            }
            SyntaxKind::PartiallyEmittedExpression => {
                return self.visit_partially_emitted_expression(node, value_is_discarded)
            }
            SyntaxKind::BinaryExpression => {
                if is_destructuring_assignment(node, self) {
                    return self.visit_destructuring_assignment(node, value_is_discarded);
                }
            }
            SyntaxKind::CallExpression => {
                if is_import_call(node, self) {
                    return Ok(Some(self.visit_import_call_expression(node)?.into()));
                }
            }
            SyntaxKind::PrefixUnaryExpression | SyntaxKind::PostfixUnaryExpression => {
                return self.visit_prefix_or_postfix_unary_expression(node, value_is_discarded)
            }
            _ => (),
        }
        Ok(Some(
            try_visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .into(),
        ))
    }
}
