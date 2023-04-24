use gc::Gc;

use super::{
    ParenthesizeConciseBodyOfArrowFunctionCurrentParenthesizerRule,
    ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule,
    ParenthesizeExpressionOfExportDefaultCurrentParenthesizerRule,
    ParenthesizeRightSideOfBinaryCurrentParenthesizerRule,
};
use crate::{
    for_each, get_comment_range, get_emit_flags, is_block, is_let, is_module_declaration,
    is_var_const, node_is_synthesized, range_is_on_single_line, EmitFlags, HasInitializerInterface,
    HasTypeInterface, HasTypeParametersInterface, InterfaceOrClassLikeDeclarationInterface,
    ListFormat, NamedDeclarationInterface, Node, NodeFlags, NodeInterface, Printer,
    ReadonlyTextRange, SyntaxKind, TextRange,
};

impl Printer {
    pub(super) fn emit_switch_statement(&self, node: &Node /*SwitchStatement*/) {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::SwitchKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_token_with_comment(
            SyntaxKind::OpenParenToken,
            open_paren_pos,
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        let node_as_switch_statement = node.as_switch_statement();
        self.emit_expression(Some(&*node_as_switch_statement.expression), None);
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node_as_switch_statement.expression.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.write_space();
        self.emit(Some(&*node_as_switch_statement.case_block), None);
    }

    pub(super) fn emit_labeled_statement(&self, node: &Node /*LabeledStatement*/) {
        let node_as_labeled_statement = node.as_labeled_statement();
        self.emit(Some(&*node_as_labeled_statement.label), None);
        self.emit_token_with_comment(
            SyntaxKind::ColonToken,
            node_as_labeled_statement.label.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.write_space();
        self.emit(Some(&*node_as_labeled_statement.statement), None);
    }

    pub(super) fn emit_throw_statement(&self, node: &Node /*ThrowStatement*/) {
        self.emit_token_with_comment(
            SyntaxKind::ThrowKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit_expression_with_leading_space(Some(&*node.as_throw_statement().expression), None);
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_try_statement(&self, node: &Node /*TryStatement*/) {
        self.emit_token_with_comment(
            SyntaxKind::TryKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_as_try_statement = node.as_try_statement();
        self.emit(Some(&*node_as_try_statement.try_block), None);
        if let Some(node_catch_clause) = node_as_try_statement.catch_clause.as_ref() {
            self.write_line_or_space(node, &node_as_try_statement.try_block, node_catch_clause);
            self.emit(Some(&**node_catch_clause), None);
        }
        if let Some(node_finally_block) = node_as_try_statement.finally_block.as_ref() {
            self.write_line_or_space(
                node,
                node_as_try_statement
                    .catch_clause
                    .as_ref()
                    .unwrap_or(&node_as_try_statement.try_block),
                node_finally_block,
            );
            self.emit_token_with_comment(
                SyntaxKind::FinallyKeyword,
                node_as_try_statement
                    .catch_clause
                    .as_ref()
                    .unwrap_or(&node_as_try_statement.try_block)
                    .end(),
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
            self.emit(Some(&**node_finally_block), None);
        }
    }

    pub(super) fn emit_debugger_statement(&self, node: &Node /*DebuggerStatement*/) {
        self.write_token(
            SyntaxKind::DebuggerKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            None,
        );
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_variable_declaration(&self, node: &Node /*VariableDeclaration*/) {
        let node_as_variable_declaration = node.as_variable_declaration();
        self.emit(node_as_variable_declaration.maybe_name().as_deref(), None);
        self.emit(
            node_as_variable_declaration.exclamation_token.as_deref(),
            None,
        );
        self.emit_type_annotation(node_as_variable_declaration.maybe_type());
        self.emit_initializer(
            node_as_variable_declaration.maybe_initializer(),
            node_as_variable_declaration.maybe_type().map_or_else(
                || node_as_variable_declaration.name().end(),
                |node_type| node_type.end(),
            ),
            node,
            Some(Gc::new(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
    }

    pub(super) fn emit_variable_declaration_list(
        &self,
        node: &Node, /*VariableDeclarationList*/
    ) {
        self.write_keyword(if is_let(node) {
            "let"
        } else if is_var_const(node) {
            "const"
        } else {
            "var"
        });
        self.write_space();
        self.emit_list(
            Some(node),
            Some(&node.as_variable_declaration_list().declarations),
            ListFormat::VariableDeclarationList,
            None,
            None,
            None,
        );
    }

    pub(super) fn emit_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        self.emit_function_declaration_or_expression(node);
    }

    pub(super) fn emit_function_declaration_or_expression(
        &self,
        node: &Node, /*FunctionDeclaration | FunctionExpression*/
    ) {
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.write_keyword("function");
        let node_as_function_like_declaration = node.as_function_like_declaration();
        self.emit(
            node_as_function_like_declaration
                .maybe_asterisk_token()
                .as_deref(),
            None,
        );
        self.write_space();
        self.emit_identifier_name(node_as_function_like_declaration.maybe_name().as_deref());
        self.emit_signature_and_body(node, |node: &Node| self.emit_signature_head(node));
    }

    pub(super) fn emit_signature_and_body<TEmitSignatureHead: FnMut(&Node)>(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
        mut emit_signature_head: TEmitSignatureHead,
    ) {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        let body = node_as_function_like_declaration.maybe_body();
        if let Some(body) = body.as_ref() {
            if is_block(body) {
                let indented_flag = get_emit_flags(node).intersects(EmitFlags::Indented);
                if indented_flag {
                    self.increase_indent();
                }

                self.push_name_generation_scope(Some(node));
                for_each(
                    &node_as_function_like_declaration.parameters(),
                    |parameter: &Gc<Node>, _| -> Option<()> {
                        self.generate_names(Some(&**parameter));
                        None
                    },
                );
                self.generate_names(node_as_function_like_declaration.maybe_body().as_deref());

                emit_signature_head(node);
                self.emit_block_function_body(body);
                self.pop_name_generation_scope(Some(node));

                if indented_flag {
                    self.decrease_indent();
                }
            } else {
                emit_signature_head(node);
                self.write_space();
                self.emit_expression(
                    Some(&**body),
                    Some(Gc::new(Box::new(
                        ParenthesizeConciseBodyOfArrowFunctionCurrentParenthesizerRule::new(
                            self.parenthesizer(),
                        ),
                    ))),
                );
            }
        } else {
            emit_signature_head(node);
            self.write_trailing_semicolon();
        }
    }

    pub(super) fn emit_signature_head(
        &self,
        node: &Node, /*FunctionDeclaration | FunctionExpression | MethodDeclaration | AccessorDeclaration | ConstructorDeclaration*/
    ) {
        let node_as_signature_declaration = node.as_signature_declaration();
        self.emit_type_parameters(
            node,
            node_as_signature_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        self.emit_parameters(node, &node_as_signature_declaration.parameters());
        self.emit_type_annotation(node_as_signature_declaration.maybe_type().as_deref());
    }

    pub(super) fn should_emit_block_function_body_on_single_line(
        &self,
        body: &Node, /*Block*/
    ) -> bool {
        if get_emit_flags(body).intersects(EmitFlags::SingleLine) {
            return true;
        }

        let body_as_block = body.as_block();
        if body_as_block.multi_line == Some(true) {
            return false;
        }

        if !node_is_synthesized(body) && !range_is_on_single_line(body, &self.current_source_file())
        {
            return false;
        }

        if self.get_leading_line_terminator_count(
            Some(body),
            &body_as_block.statements,
            ListFormat::PreserveLines,
        ) > 0
            || self.get_closing_line_terminator_count(
                Some(body),
                body_as_block.statements.clone().into(),
                ListFormat::PreserveLines,
            ) > 0
        {
            return false;
        }

        let mut previous_statement: Option<Gc<Node /*Statement*/>> = None;
        for statement in &body_as_block.statements {
            if self.get_separating_line_terminator_count(
                previous_statement.as_deref(),
                statement,
                ListFormat::PreserveLines,
            ) > 0
            {
                return false;
            }

            previous_statement = Some(statement.node_wrapper());
        }

        true
    }

    pub(super) fn emit_block_function_body(&self, body: &Node /*Block*/) {
        self.on_before_emit_node(Some(body));
        self.write_space();
        self.write_punctuation("{");
        self.increase_indent();

        let body_as_block = body.as_block();
        let should_emit_block_function_body_on_single_line =
            self.should_emit_block_function_body_on_single_line(body);
        // if (emitBodyWithDetachedComments) {
        self.emit_body_with_detached_comments(body, &*body_as_block.statements, |node: &Node| {
            if should_emit_block_function_body_on_single_line {
                self.emit_block_function_body_on_single_line(node);
            } else {
                self.emit_block_function_body_worker(node, None);
            }
        });
        // }
        // else {
        //     emitBlockFunctionBody(body);
        // }

        self.decrease_indent();
        self.write_token(
            SyntaxKind::CloseBraceToken,
            body_as_block.statements.end(),
            |text: &str| self.write_punctuation(text),
            Some(body),
        );
        self.on_after_emit_node(Some(body));
    }

    pub(super) fn emit_block_function_body_on_single_line(&self, body: &Node /*Block*/) {
        self.emit_block_function_body_worker(body, Some(true));
    }

    pub(super) fn emit_block_function_body_worker(
        &self,
        body: &Node, /*Block*/
        emit_block_function_body_on_single_line: Option<bool>,
    ) {
        let body_as_block = body.as_block();
        let statement_offset =
            self.emit_prologue_directives(&body_as_block.statements, None, &mut None, None);
        let pos = self.writer().get_text_pos();
        self.emit_helpers(body);
        if statement_offset == 0
            && pos == self.writer().get_text_pos()
            && emit_block_function_body_on_single_line == Some(true)
        {
            self.decrease_indent();
            self.emit_list(
                Some(body),
                Some(&body_as_block.statements),
                ListFormat::SingleLineFunctionBodyStatements,
                None,
                None,
                None,
            );
            self.increase_indent();
        } else {
            self.emit_list(
                Some(body),
                Some(&body_as_block.statements),
                ListFormat::MultiLineFunctionBodyStatements,
                None,
                Some(statement_offset),
                None,
            );
        }
    }

    pub(super) fn emit_class_declaration(&self, node: &Node /*ClassDeclaration*/) {
        self.emit_class_declaration_or_expression(node);
    }

    pub(super) fn emit_class_declaration_or_expression(
        &self,
        node: &Node, /*ClassDeclaration | ClassExpression*/
    ) {
        let node_as_class_like_declaration = node.as_class_like_declaration();
        for_each(
            &node_as_class_like_declaration.members(),
            |member: &Gc<Node>, _| -> Option<()> {
                self.generate_member_names(Some(&**member));
                None
            },
        );

        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.write_keyword("class");
        if let Some(node_name) = node_as_class_like_declaration.maybe_name().as_ref() {
            self.write_space();
            self.emit_identifier_name(Some(&**node_name));
        }

        let indented_flag = get_emit_flags(node).intersects(EmitFlags::Indented);
        if indented_flag {
            self.increase_indent();
        }

        self.emit_type_parameters(
            node,
            node_as_class_like_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        self.emit_list(
            Some(node),
            node_as_class_like_declaration
                .maybe_heritage_clauses()
                .as_deref(),
            ListFormat::ClassHeritageClauses,
            None,
            None,
            None,
        );

        self.write_space();
        self.write_punctuation("{");
        self.emit_list(
            Some(node),
            Some(&node_as_class_like_declaration.members()),
            ListFormat::ClassMembers,
            None,
            None,
            None,
        );
        self.write_punctuation("}");

        if indented_flag {
            self.decrease_indent();
        }
    }

    pub(super) fn emit_interface_declaration(&self, node: &Node /*InterfaceDeclaration*/) {
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.write_keyword("interface");
        self.write_space();
        let node_as_interface_declaration = node.as_interface_declaration();
        self.emit(node_as_interface_declaration.maybe_name().as_deref(), None);
        self.emit_type_parameters(
            node,
            node_as_interface_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        self.emit_list(
            Some(node),
            node_as_interface_declaration
                .maybe_heritage_clauses()
                .as_deref(),
            ListFormat::HeritageClauses,
            None,
            None,
            None,
        );
        self.write_space();
        self.write_punctuation("{");
        self.emit_list(
            Some(node),
            Some(&node_as_interface_declaration.members),
            ListFormat::InterfaceMembers,
            None,
            None,
            None,
        );
        self.write_punctuation("}");
    }

    pub(super) fn emit_type_alias_declaration(&self, node: &Node /*TypeAliasDeclaration*/) {
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.write_keyword("type");
        self.write_space();
        let node_as_type_alias_declaration = node.as_type_alias_declaration();
        self.emit(node_as_type_alias_declaration.maybe_name().as_deref(), None);
        self.emit_type_parameters(
            node,
            node_as_type_alias_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        self.write_space();
        self.write_punctuation("=");
        self.write_space();
        self.emit(node_as_type_alias_declaration.maybe_type().as_deref(), None);
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_enum_declaration(&self, node: &Node /*EnumDeclaration*/) {
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.write_keyword("enum");
        self.write_space();
        let node_as_enum_declaration = node.as_enum_declaration();
        self.emit(node_as_enum_declaration.maybe_name().as_deref(), None);

        self.write_space();
        self.write_punctuation("{");
        self.emit_list(
            Some(node),
            Some(&node_as_enum_declaration.members),
            ListFormat::EnumMembers,
            None,
            None,
            None,
        );
        self.write_punctuation("}");
    }

    pub(super) fn emit_module_declaration(&self, node: &Node /*ModuleDeclaration*/) {
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        if (!node.flags()).intersects(NodeFlags::GlobalAugmentation) {
            self.write_keyword(if node.flags().intersects(NodeFlags::Namespace) {
                "namespace"
            } else {
                "module"
            });
            self.write_space();
        }
        let node_as_module_declaration = node.as_module_declaration();
        self.emit(node_as_module_declaration.maybe_name().as_deref(), None);

        let mut body = node_as_module_declaration.body.clone();
        if body.is_none() {
            return self.write_trailing_semicolon();
        }
        while let Some(body_present) = body.as_ref().filter(|body| is_module_declaration(body)) {
            self.write_punctuation(".");
            let body_as_module_declaration = body_present.as_module_declaration();
            self.emit(body_as_module_declaration.maybe_name().as_deref(), None);
            body = body_as_module_declaration.body.clone();
        }

        self.write_space();
        self.emit(body.as_deref(), None);
    }

    pub(super) fn emit_module_block(&self, node: &Node /*ModuleBlock*/) {
        self.push_name_generation_scope(Some(node));
        for_each(
            &node.as_module_block().statements,
            |statement: &Gc<Node>, _| -> Option<()> {
                self.generate_names(Some(&**statement));
                None
            },
        );
        self.emit_block_statements(node, self.is_empty_block(node));
        self.pop_name_generation_scope(Some(node));
    }

    pub(super) fn emit_case_block(&self, node: &Node /*CaseBlock*/) {
        self.emit_token_with_comment(
            SyntaxKind::OpenBraceToken,
            node.pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        let node_as_case_block = node.as_case_block();
        self.emit_list(
            Some(node),
            Some(&node_as_case_block.clauses),
            ListFormat::CaseBlockClauses,
            None,
            None,
            None,
        );
        self.emit_token_with_comment(
            SyntaxKind::CloseBraceToken,
            node_as_case_block.clauses.end(),
            |text: &str| self.write_punctuation(text),
            node,
            Some(true),
        );
    }

    pub(super) fn emit_import_equals_declaration(
        &self,
        node: &Node, /*ImportEqualsDeclaration*/
    ) {
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.emit_token_with_comment(
            SyntaxKind::ImportKeyword,
            node.maybe_modifiers()
                .as_ref()
                .map_or_else(|| node.pos(), |node_modifiers| node_modifiers.end()),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_as_import_equals_declaration = node.as_import_equals_declaration();
        if node_as_import_equals_declaration.is_type_only {
            self.emit_token_with_comment(
                SyntaxKind::TypeKeyword,
                node.pos(),
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
        }
        self.emit(
            node_as_import_equals_declaration.maybe_name().as_deref(),
            None,
        );
        self.write_space();
        self.emit_token_with_comment(
            SyntaxKind::EqualsToken,
            node_as_import_equals_declaration.name().end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.write_space();
        self.emit_module_reference(&node_as_import_equals_declaration.module_reference);
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_module_reference(&self, node: &Node /*ModuleReference*/) {
        if node.kind() == SyntaxKind::Identifier {
            self.emit_expression(Some(node), None);
        } else {
            self.emit(Some(node), None);
        }
    }

    pub(super) fn emit_import_declaration(&self, node: &Node /*ImportDeclaration*/) {
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.emit_token_with_comment(
            SyntaxKind::ImportKeyword,
            node.maybe_modifiers()
                .as_ref()
                .map_or_else(|| node.pos(), |node_modifiers| node_modifiers.end()),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_as_import_declaration = node.as_import_declaration();
        if let Some(node_import_clause) = node_as_import_declaration.import_clause.as_ref() {
            self.emit(Some(&**node_import_clause), None);
            self.write_space();
            self.emit_token_with_comment(
                SyntaxKind::FromKeyword,
                node_import_clause.end(),
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
        }
        self.emit_expression(Some(&*node_as_import_declaration.module_specifier), None);
        if let Some(node_assert_clause) = node_as_import_declaration.assert_clause.as_ref() {
            self.emit_with_leading_space(Some(&**node_assert_clause));
        }
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_import_clause(&self, node: &Node /*ImportClause*/) {
        let node_as_import_clause = node.as_import_clause();
        if node_as_import_clause.is_type_only {
            self.emit_token_with_comment(
                SyntaxKind::TypeKeyword,
                node.pos(),
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
        }
        self.emit(node_as_import_clause.maybe_name().as_deref(), None);
        if let Some(node_name) = node_as_import_clause.maybe_name().as_ref() {
            if node_as_import_clause.named_bindings.is_some() {
                self.emit_token_with_comment(
                    SyntaxKind::CommaToken,
                    node_name.end(),
                    |text: &str| self.write_punctuation(text),
                    node,
                    None,
                );
                self.write_space();
            }
        }
        self.emit(node_as_import_clause.named_bindings.as_deref(), None);
    }

    pub(super) fn emit_namespace_import(&self, node: &Node /*NamespaceImport*/) {
        let as_pos = self.emit_token_with_comment(
            SyntaxKind::AsteriskToken,
            node.pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.write_space();
        self.emit_token_with_comment(
            SyntaxKind::AsKeyword,
            as_pos,
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit(node.as_namespace_import().maybe_name().as_deref(), None);
    }

    pub(super) fn emit_named_imports(&self, node: &Node /*NamedImports*/) {
        self.emit_named_imports_or_exports(node);
    }

    pub(super) fn emit_import_specifier(&self, node: &Node /*ImportSpecifier*/) {
        self.emit_import_or_export_specifier(node);
    }

    pub(super) fn emit_export_assignment(&self, node: &Node /*ExportAssignment*/) {
        let next_pos = self.emit_token_with_comment(
            SyntaxKind::ExportKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_as_export_assignment = node.as_export_assignment();
        if node_as_export_assignment.is_export_equals == Some(true) {
            self.emit_token_with_comment(
                SyntaxKind::EqualsToken,
                next_pos,
                |text: &str| self.write_operator(text),
                node,
                None,
            );
        } else {
            self.emit_token_with_comment(
                SyntaxKind::DefaultKeyword,
                next_pos,
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
        }
        self.write_space();
        self.emit_expression(
            Some(&*node_as_export_assignment.expression),
            if node_as_export_assignment.is_export_equals == Some(true) {
                Some(Gc::new(Box::new(
                    ParenthesizeRightSideOfBinaryCurrentParenthesizerRule::new(
                        self.parenthesizer(),
                    ),
                )))
            } else {
                Some(Gc::new(Box::new(
                    ParenthesizeExpressionOfExportDefaultCurrentParenthesizerRule::new(
                        self.parenthesizer(),
                    ),
                )))
            },
        );
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_export_declaration(&self, node: &Node /*ExportDeclaration*/) {
        let mut next_pos = self.emit_token_with_comment(
            SyntaxKind::ExportKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_as_export_declaration = node.as_export_declaration();
        if node_as_export_declaration.is_type_only {
            next_pos = self.emit_token_with_comment(
                SyntaxKind::TypeKeyword,
                next_pos,
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
        }
        if let Some(node_export_clause) = node_as_export_declaration.export_clause.as_ref() {
            self.emit(Some(&**node_export_clause), None);
        } else {
            next_pos = self.emit_token_with_comment(
                SyntaxKind::AsteriskToken,
                next_pos,
                |text: &str| self.write_punctuation(text),
                node,
                None,
            );
        }
        if let Some(node_module_specifier) = node_as_export_declaration.module_specifier.as_ref() {
            self.write_space();
            let from_pos = node_as_export_declaration
                .export_clause
                .as_ref()
                .map_or(next_pos, |node_export_clause| node_export_clause.end());
            self.emit_token_with_comment(
                SyntaxKind::FromKeyword,
                from_pos,
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
            self.emit_expression(Some(&**node_module_specifier), None);
        }
        if let Some(node_assert_clause) = node_as_export_declaration.assert_clause.as_ref() {
            self.emit_with_leading_space(Some(&**node_assert_clause));
        }
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_assert_clause(&self, node: &Node /*AssertClause*/) {
        self.emit_token_with_comment(
            SyntaxKind::AssertKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let elements = &node.as_assert_clause().elements;
        self.emit_list(
            Some(node),
            Some(elements),
            ListFormat::ImportClauseEntries,
            None,
            None,
            None,
        );
    }

    pub(super) fn emit_assert_entry(&self, node: &Node /*AssertEntry*/) {
        let node_as_assert_entry = node.as_assert_entry();
        self.emit(Some(&*node_as_assert_entry.name), None);
        self.write_punctuation(":");
        self.write_space();

        let value = &node_as_assert_entry.value;
        if !get_emit_flags(value).intersects(EmitFlags::NoLeadingComments) {
            let comment_range = get_comment_range(value);
            self.emit_trailing_comments_of_position(comment_range.pos(), None, None);
        }
        self.emit(Some(&**value), None);
    }

    pub(super) fn emit_namespace_export_declaration(
        &self,
        node: &Node, /*NamespaceExportDeclaration*/
    ) {
        let mut next_pos = self.emit_token_with_comment(
            SyntaxKind::ExportKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        next_pos = self.emit_token_with_comment(
            SyntaxKind::AsKeyword,
            next_pos,
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        /*nextPos =*/
        self.emit_token_with_comment(
            SyntaxKind::NamespaceKeyword,
            next_pos,
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit(
            node.as_namespace_export_declaration()
                .maybe_name()
                .as_deref(),
            None,
        );
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_namespace_export(&self, node: &Node /*NamespaceExport*/) {
        let as_pos = self.emit_token_with_comment(
            SyntaxKind::AsteriskToken,
            node.pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.write_space();
        self.emit_token_with_comment(
            SyntaxKind::AsKeyword,
            as_pos,
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit(Some(&*node.as_namespace_export().name), None);
    }

    pub(super) fn emit_named_exports(&self, node: &Node /*NamedExports*/) {
        self.emit_named_imports_or_exports(node);
    }

    pub(super) fn emit_export_specifier(&self, node: &Node /*ExportSpecifier*/) {
        self.emit_import_or_export_specifier(node);
    }

    pub(super) fn emit_named_imports_or_exports(&self, node: &Node /*NamedImportsOrExports*/) {
        self.write_punctuation("{");
        self.emit_list(
            Some(node),
            Some(&node.as_has_elements().elements()),
            ListFormat::NamedImportsOrExportsElements,
            None,
            None,
            None,
        );
        self.write_punctuation("}");
    }

    pub(super) fn emit_import_or_export_specifier(
        &self,
        node: &Node, /*ImportOrExportSpecifier*/
    ) {
        if node.as_has_is_type_only().is_type_only() {
            self.write_keyword("type");
            self.write_space();
        }
        if let Some(node_property_name) = node.as_has_property_name().maybe_property_name().as_ref()
        {
            self.emit(Some(&**node_property_name), None);
            self.write_space();
            self.emit_token_with_comment(
                SyntaxKind::AsKeyword,
                node_property_name.end(),
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
        }

        self.emit(node.as_named_declaration().maybe_name().as_deref(), None);
    }

    pub(super) fn emit_external_module_reference(
        &self,
        node: &Node, /*ExternalModuleReference*/
    ) {
        self.write_keyword("require");
        self.write_punctuation("(");
        self.emit_expression(Some(&*node.as_external_module_reference().expression), None);
        self.write_punctuation(")");
    }
}
