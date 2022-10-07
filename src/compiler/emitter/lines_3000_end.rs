use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::brackets;
use crate::{
    for_each, get_emit_flags, get_literal_text, id_text, is_block, is_identifier, is_let,
    is_module_declaration, is_var_const, node_is_synthesized, range_is_on_single_line,
    token_to_string, with_synthetic_factory, EmitFlags, EmitHint, GetLiteralTextFlags,
    HasInitializerInterface, HasTypeInterface, HasTypeParametersInterface,
    InterfaceOrClassLikeDeclarationInterface, ListFormat, NamedDeclarationInterface, Node,
    NodeArray, NodeFlags, NodeInterface, Printer, ReadonlyTextRange, SourceFilePrologueInfo,
    SourceMapSource, Symbol, SyntaxKind,
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
            SyntaxKind::DebuggerStatement,
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
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer
                            .parenthesize_expression_for_disallowed_comma(synthetic_factory, node)
                    })
                }
            })),
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
        self.emit_decorators(node, node.maybe_decorators().as_ref());
        self.emit_modifiers(node, node.maybe_modifiers().as_ref());
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
                    node_as_function_like_declaration.parameters(),
                    |parameter: &Rc<Node>, _| -> Option<()> {
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
                    Some(Rc::new({
                        let parenthesizer = self.parenthesizer();
                        move |node: &Node| {
                            with_synthetic_factory(|synthetic_factory| {
                                parenthesizer.parenthesize_concise_body_of_arrow_function(
                                    synthetic_factory,
                                    node,
                                )
                            })
                        }
                    })),
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
                .as_ref(),
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
                &body_as_block.statements,
                ListFormat::PreserveLines,
            ) > 0
        {
            return false;
        }

        let mut previous_statement: Option<Rc<Node /*Statement*/>> = None;
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
        self.emit_body_with_detached_comments(body, &body_as_block.statements, |node: &Node| {
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
            self.emit_prologue_directives(&body_as_block.statements, None, None, None);
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
            node_as_class_like_declaration.members(),
            |member: &Rc<Node>, _| -> Option<()> {
                self.generate_member_names(Some(&**member));
                None
            },
        );

        self.emit_decorators(node, node.maybe_decorators().as_ref());
        self.emit_modifiers(node, node.maybe_modifiers().as_ref());
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
                .as_ref(),
        );
        self.emit_list(
            Some(node),
            node_as_class_like_declaration.maybe_heritage_clauses(),
            ListFormat::ClassHeritageClauses,
            None,
            None,
            None,
        );

        self.write_space();
        self.write_punctuation("{");
        self.emit_list(
            Some(node),
            Some(node_as_class_like_declaration.members()),
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
        self.emit_decorators(node, node.maybe_decorators().as_ref());
        self.emit_modifiers(node, node.maybe_modifiers().as_ref());
        self.write_keyword("interface");
        self.write_space();
        let node_as_interface_declaration = node.as_interface_declaration();
        self.emit(node_as_interface_declaration.maybe_name().as_deref(), None);
        self.emit_type_parameters(
            node,
            node_as_interface_declaration
                .maybe_type_parameters()
                .as_ref(),
        );
        self.emit_list(
            Some(node),
            node_as_interface_declaration.maybe_heritage_clauses(),
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
        self.emit_decorators(node, node.maybe_decorators().as_ref());
        self.emit_modifiers(node, node.maybe_modifiers().as_ref());
        self.write_keyword("type");
        self.write_space();
        let node_as_type_alias_declaration = node.as_type_alias_declaration();
        self.emit(node_as_type_alias_declaration.maybe_name().as_deref(), None);
        self.emit_type_parameters(
            node,
            node_as_type_alias_declaration
                .maybe_type_parameters()
                .as_ref(),
        );
        self.write_space();
        self.write_punctuation("=");
        self.write_space();
        self.emit(node_as_type_alias_declaration.maybe_type().as_deref(), None);
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_enum_declaration(&self, node: &Node /*EnumDeclaration*/) {
        self.emit_modifiers(node, node.maybe_modifiers().as_ref());
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
        self.emit_modifiers(node, node.maybe_modifiers().as_ref());
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
            |statement: &Rc<Node>, _| -> Option<()> {
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
        self.emit_modifiers(node, node.maybe_modifiers().as_ref());
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
        self.emit_modifiers(node, node.maybe_modifiers().as_ref());
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
                Some(Rc::new({
                    let parenthesizer = self.parenthesizer();
                    move |node: &Node| {
                        with_synthetic_factory(|synthetic_factory| {
                            parenthesizer.parenthesize_right_side_of_binary(
                                synthetic_factory,
                                SyntaxKind::EqualsToken,
                                None,
                                node,
                            )
                        })
                    }
                }))
            } else {
                Some(Rc::new({
                    let parenthesizer = self.parenthesizer();
                    move |node: &Node| {
                        with_synthetic_factory(|synthetic_factory| {
                            parenthesizer
                                .parenthesize_expression_of_export_default(synthetic_factory, node)
                        })
                    }
                }))
            },
        );
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_export_declaration(&self, node: &Node /*ExportDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn emit_assert_clause(&self, node: &Node /*AssertClause*/) {
        unimplemented!()
    }

    pub(super) fn emit_assert_entry(&self, node: &Node /*AssertEntry*/) {
        unimplemented!()
    }

    pub(super) fn emit_namespace_export_declaration(
        &self,
        node: &Node, /*NamespaceExportDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_namespace_export(&self, node: &Node /*NamespaceExport*/) {
        unimplemented!()
    }

    pub(super) fn emit_named_exports(&self, node: &Node /*NamedExports*/) {
        unimplemented!()
    }

    pub(super) fn emit_export_specifier(&self, node: &Node /*ExportSpecifier*/) {
        unimplemented!()
    }

    pub(super) fn emit_named_imports_or_exports(&self, node: &Node /*NamedImportsOrExports*/) {
        unimplemented!()
    }

    pub(super) fn emit_import_or_export_specifier(
        &self,
        node: &Node, /*ImportOrExportSpecifier*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_external_module_reference(
        &self,
        node: &Node, /*ExternalModuleReference*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_jsx_element(&self, node: &Node /*JsxElement*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsx_self_closing_element(&self, node: &Node /*JsxSelfClosingElement*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsx_fragment(&self, node: &Node /*JsxFragment*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsx_opening_element_or_fragment(
        &self,
        node: &Node, /*JsxOpeningElement | JsxOpeningFragment*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_jsx_text(&self, node: &Node /*JsxText*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsx_closing_element_or_fragment(
        &self,
        node: &Node, /*JsxClosingElement | JsxClosingFragment*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_jsx_attributes(&self, node: &Node /*JsxAttributes*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsx_attribute(&self, node: &Node /*JsxAttribute*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsx_spread_attribute(&self, node: &Node /*JsxSpreadAttribute*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsx_expression(&self, node: &Node /*JsxExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_case_clause(&self, node: &Node /*CaseClause*/) {
        unimplemented!()
    }

    pub(super) fn emit_default_clause(&self, node: &Node /*DefaultClause*/) {
        unimplemented!()
    }

    pub(super) fn emit_heritage_clause(&self, node: &Node /*HeritageClause*/) {
        unimplemented!()
    }

    pub(super) fn emit_catch_clause(&self, node: &Node /*CatchClause*/) {
        unimplemented!()
    }

    pub(super) fn emit_property_assignment(&self, node: &Node /*PropertyAssignment*/) {
        unimplemented!()
    }

    pub(super) fn emit_shorthand_property_assignment(
        &self,
        node: &Node, /*ShorthandPropertyAssignment*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_spread_assignment(&self, node: &Node /*SpreadAssignment*/) {
        unimplemented!()
    }

    pub(super) fn emit_enum_member(&self, node: &Node /*EnumMember*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc(&self, node: &Node /*JSDoc*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_simple_typed_tag(
        &self,
        node: &Node, /*JSDocTypeTag | JSDocThisTag | JSDocEnumTag | JSDocReturnTag*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_see_tag(&self, node: &Node /*JSDocSeeTag*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_name_reference(&self, node: &Node /*JSDocNameReference*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_heritage_tag(
        &self,
        node: &Node, /*JSDocImplementsTag | JSDocAugmentsTag*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_template_tag(&self, node: &Node /*JSDocTemplateTag*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_typedef_tag(&self, node: &Node /*JSDocTypedefTag*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_callback_tag(&self, node: &Node /*JSDocCallbackTag*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_simple_tag(&self, node: &Node /*JSDocTag*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_type_literal(&self, node: &Node /*JSDocTypeLiteral*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_signature(&self, node: &Node /*JSDocSignature*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_property_like_tag(&self, node: &Node /*JSDocPropertyLikeTag*/) {
        unimplemented!()
    }

    pub(super) fn emit_jsdoc_type_expression(&self, node: &Node /*JSDocTypeExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_source_file(&self, node: &Node /*SourceFile*/) {
        unimplemented!()
    }

    pub(super) fn emit_synthetic_triple_slash_references_if_needed(
        &self,
        node: &Node, /*Bundle*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_partially_emitted_expression(
        &self,
        node: &Node, /*PartiallyEmittedExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_comma_list(&self, node: &Node /*CommaListExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_prologue_directives(
        &self,
        statements: &[Rc<Node>],
        source_file: Option<&Node /*SourceFile*/>,
        seen_prologue_directives: Option<&mut HashSet<String>>,
        record_bundle_file_section: Option<bool /*true*/>,
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn emit_prologue_directives_if_needed(
        &self,
        source_file_or_bundle: &Node, /*Bundle | SourceFile*/
    ) {
        unimplemented!()
    }

    pub(super) fn get_prologue_directives_from_bundled_source_files(
        &self,
        bundle: &Node, /*Bundle*/
    ) -> Option<Vec<SourceFilePrologueInfo>> {
        unimplemented!()
    }

    pub(super) fn emit_shebang_if_needed(
        &self,
        source_file_or_bundle: &Node, /*Bundle | SourceFile | UnparsedSource*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn emit_node_with_writer(&self, node: Option<&Node>, writer: fn(&Printer, &str)) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let saved_write = self.write.get();
        self.write.set(writer);
        self.emit(Some(node), None);
        self.write.set(saved_write);
    }

    pub(super) fn emit_modifiers(&self, node: &Node, modifiers: Option<&NodeArray /*<Modifier>*/>) {
        unimplemented!()
    }

    pub(super) fn emit_type_annotation<TNode: Borrow<Node>>(
        &self,
        node: Option<TNode /*TypeNode*/>,
    ) {
        if let Some(node) = node {
            let node = node.borrow();
            self.write_punctuation(":");
            self.write_space();
            self.emit(Some(node), None);
        }
    }

    pub(super) fn emit_initializer<TNode: Borrow<Node>>(
        &self,
        node: Option<TNode /*Expression*/>,
        equal_comment_start_pos: isize,
        container: &Node,
        parenthesizer_rule: Option<Rc<dyn Fn(&Node) -> Rc<Node>>>,
    ) {
        unimplemented!()
    }

    pub(super) fn emit_with_leading_space(&self, node: Option<&Node>) {
        unimplemented!()
    }

    pub(super) fn emit_expression_with_leading_space(
        &self,
        node: Option<&Node>,
        parenthesizer_rule: Option<Rc<dyn Fn(&Node) -> Rc<Node>>>,
    ) {
        unimplemented!()
    }

    pub(super) fn emit_with_trailing_space(&self, node: Option<&Node>) {
        unimplemented!()
    }

    pub(super) fn emit_embedded_statement(&self, parent: &Node, node: &Node /*Statement*/) {
        unimplemented!()
    }

    pub(super) fn emit_decorators(
        &self,
        parent_node: &Node,
        decorators: Option<&NodeArray /*<Decorator>*/>,
    ) {
        unimplemented!()
    }

    pub(super) fn emit_type_arguments(
        &self,
        parent_node: &Node,
        type_arguments: Option<&NodeArray /*<TypeNode>*/>,
    ) {
        self.emit_list(
            Some(parent_node),
            type_arguments,
            ListFormat::TypeArguments,
            // TODO: this is wrong, should be parenthesizer.parenthesizeMemberOfElementType
            None,
            None,
            None,
        );
    }

    pub(super) fn emit_type_parameters(
        &self,
        parent_node: &Node, /*SignatureDeclaration | InterfaceDeclaration | TypeAliasDeclaration | ClassDeclaration | ClassExpression*/
        type_parameters: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
    ) {
        unimplemented!()
    }

    pub(super) fn emit_parameters(
        &self,
        parent_node: &Node,
        parameters: &NodeArray, /*<ParameterDeclaration>*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_parameters_for_arrow(
        &self,
        parent_node: &Node,     /*FunctionTypeNode | ArrowFunction*/
        parameters: &NodeArray, /*<ParameterDeclaration>*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_parameters_for_index_signature(
        &self,
        parent_node: &Node,
        parameters: &NodeArray, /*<ParameterDeclaration>*/
    ) {
        unimplemented!()
    }

    pub(super) fn write_delimiter(&self, format: ListFormat) {
        match format & ListFormat::DelimitersMask {
            ListFormat::None => (),
            ListFormat::CommaDelimited => {
                self.write_punctuation(",");
            }
            ListFormat::BarDelimited => {
                self.write_space();
                self.write_punctuation("|");
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn emit_list<TNode: Borrow<Node>>(
        &self,
        parent_node: Option<TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
        parenthesizer_rule: Option<Rc<dyn Fn(&Node) -> Rc<Node>>>,
        start: Option<usize>,
        count: Option<usize>,
    ) {
        self.emit_node_list(
            Printer::emit,
            parent_node,
            children,
            format,
            parenthesizer_rule,
            start,
            count,
        );
    }

    pub(super) fn emit_expression_list<TNode: Borrow<Node>>(
        &self,
        parent_node: Option<TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
        parenthesizer_rule: Option<Rc<dyn Fn(&Node) -> Rc<Node>>>,
        start: Option<usize>,
        count: Option<usize>,
    ) {
        self.emit_node_list(
            Printer::emit_expression,
            parent_node,
            children,
            format,
            parenthesizer_rule,
            start,
            count,
        );
    }

    pub(super) fn emit_node_list<TNode: Borrow<Node>>(
        &self,
        emit: fn(&Printer, Option<&Node>, Option<Rc<dyn Fn(&Node) -> Rc<Node>>>),
        parent_node: Option<TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
        parenthesizer_rule: Option<Rc<dyn Fn(&Node) -> Rc<Node>>>,
        start: Option<usize>,
        count: Option<usize>,
    ) {
        let start = start.unwrap_or(0);
        let count = count.unwrap_or_else(|| {
            if let Some(children) = children {
                children.len() - start
            } else {
                0
            }
        });
        let is_undefined = children.is_none();
        if is_undefined && format.intersects(ListFormat::OptionalIfUndefined) {
            return;
        }

        let is_empty = match children {
            None => true,
            Some(children) => start >= children.len(),
        } || count == 0;
        if is_empty && format.intersects(ListFormat::OptionalIfEmpty) {
            // TODO
            return;
        }

        if format.intersects(ListFormat::BracketsMask) {
            self.write_punctuation(get_opening_bracket(format));
            if is_empty {
                if let Some(children) = children {
                    self.emit_trailing_comments_of_position(children.pos(), Some(true), None);
                }
            }
        }

        // TODO

        let children = children.unwrap();
        if is_empty {
            unimplemented!()
        } else {
            if false {
                unimplemented!()
            } else if format.intersects(ListFormat::SpaceBetweenBraces) {
                self.write_space();
            }

            let mut previous_sibling: Option<Rc<Node>> = None;
            let children_iter = children.iter();
            for child in children_iter.skip(start) {
                if false {
                    unimplemented!()
                } else if let Some(previous_sibling) = previous_sibling.as_ref() {
                    self.write_delimiter(format);
                }

                if false {
                    unimplemented!()
                } else if previous_sibling.is_some()
                    && format.intersects(ListFormat::SpaceBetweenSiblings)
                {
                    self.write_space();
                }

                emit(
                    self,
                    Some(&**child),
                    None, // TODO: this is wrong
                );

                previous_sibling = Some(child.clone());
            }

            if false {
                unimplemented!()
            } else if format.intersects(ListFormat::SpaceAfterList | ListFormat::SpaceBetweenBraces)
            {
                self.write_space();
            }
        }
    }

    pub(super) fn write_base(&self, s: &str) {
        self.writer().write(s);
    }

    pub(super) fn write_literal(&self, s: &str) {
        unimplemented!()
    }

    pub(super) fn write_string_literal(&self, s: &str) {
        self.writer().write_string_literal(s);
    }

    pub(super) fn write_symbol(&self, s: &str, sym: &Symbol) {
        self.writer().write_symbol(s, sym);
    }

    pub(super) fn write_punctuation(&self, s: &str) {
        self.writer().write_punctuation(s);
    }

    pub(super) fn write_trailing_semicolon(&self) {
        self.writer().write_trailing_semicolon(";");
    }

    pub(super) fn write_keyword(&self, s: &str) {
        self.writer().write_keyword(s);
    }

    pub(super) fn write_operator(&self, s: &str) {
        unimplemented!()
    }

    pub(super) fn write_parameter(&self, s: &str) {
        unimplemented!()
    }

    pub(super) fn write_space(&self) {
        self.writer().write_space(" ");
    }

    pub(super) fn write_property(&self, s: &str) {
        self.writer().write_property(s);
    }

    pub(super) fn non_escaping_write(&self, s: &str) {
        unimplemented!()
    }

    pub(super) fn write_line(&self, count: Option<usize>) {
        let count = count.unwrap_or(1);
        unimplemented!()
    }

    pub(super) fn increase_indent(&self) {
        self.writer().increase_indent();
    }

    pub(super) fn decrease_indent(&self) {
        self.writer().decrease_indent();
    }

    pub(super) fn write_token<TWriter: FnMut(&str)>(
        &self,
        token: SyntaxKind,
        pos: isize,
        mut writer: TWriter,
        context_node: Option<&Node>,
    ) -> Option<isize> {
        unimplemented!()
    }

    pub(super) fn write_token_node(&self, node: &Node, writer: fn(&Printer, &str)) {
        writer(self, token_to_string(node.kind()).unwrap());
    }

    pub(super) fn write_token_text<TWriter: FnMut(&str)>(
        &self,
        token: SyntaxKind,
        mut writer: TWriter,
        pos: Option<isize>,
    ) -> Option<isize> {
        let token_string = token_to_string(token).unwrap();
        writer(token_string);
        pos.map(|pos| {
            if pos < 0 {
                pos
            } else {
                pos + TryInto::<isize>::try_into(token_string.len()).unwrap()
            }
        })
    }

    pub(super) fn write_line_or_space(
        &self,
        parent_node: &Node,
        prev_child_node: &Node,
        next_child_node: &Node,
    ) {
        unimplemented!()
    }

    pub(super) fn write_lines(&self, text: &str) {
        unimplemented!()
    }

    pub(super) fn write_lines_and_indent(
        &self,
        line_count: usize,
        write_space_if_not_indenting: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn decrease_indent_if(&self, value1: bool, value2: Option<bool>) {
        unimplemented!()
    }

    pub(super) fn get_leading_line_terminator_count(
        &self,
        parent_node: Option<&Node>,
        children: &[Rc<Node>],
        format: ListFormat,
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn get_separating_line_terminator_count(
        &self,
        previous_node: Option<&Node>,
        next_node: &Node,
        format: ListFormat,
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn get_closing_line_terminator_count(
        &self,
        parent_node: Option<&Node>,
        children: &[Rc<Node>],
        format: ListFormat,
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn write_line_separators_and_indent_before(
        &self,
        node: &Node,
        parent: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn write_line_separators_after(&self, node: &Node, parent: &Node) {
        unimplemented!()
    }

    pub(super) fn get_lines_between_nodes(
        &self,
        parent: &Node,
        node1: &Node,
        node2: &Node,
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn is_empty_block(&self, block: &Node /*BlockLike*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_text_of_node(&self, node: &Node, include_trivia: Option<bool>) -> String {
        if false {
            unimplemented!()
        } else if (is_identifier(node) || false) && true {
            return id_text(node);
        }

        unimplemented!()
    }

    pub(super) fn get_literal_text_of_node(
        &self,
        node: &Node,
        never_ascii_escape: Option<bool>,
        jsx_attribute_escape: bool,
    ) -> Cow<'static, str> {
        let flags = GetLiteralTextFlags::None;

        get_literal_text(node, self.maybe_current_source_file(), flags)
    }

    pub(super) fn push_name_generation_scope(&self, node: Option<&Node>) {
        unimplemented!()
    }

    pub(super) fn pop_name_generation_scope(&self, node: Option<&Node>) {
        unimplemented!()
    }

    pub(super) fn generate_names(&self, node: Option<&Node>) {
        unimplemented!()
    }

    pub(super) fn generate_member_names(&self, node: Option<&Node>) {
        unimplemented!()
    }

    pub(super) fn generate_name_if_needed(&self, name: Option<&Node /*DeclarationName*/>) {
        unimplemented!()
    }

    pub(super) fn make_file_level_optimistic_unique_name(&self, name: &str) -> String {
        unimplemented!()
    }

    pub(super) fn pipeline_emit_with_comments(&self, hint: EmitHint, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_comments_before_node(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_comments_after_node(
        &self,
        node: &Node,
        saved_container_pos: isize,
        saved_container_end: isize,
        saved_declaration_list_container_end: isize,
    ) {
        unimplemented!()
    }

    pub(super) fn emit_body_with_detached_comments<
        TDetachedRange: ReadonlyTextRange,
        TEmitCallback: FnMut(&Node),
    >(
        &self,
        node: &Node,
        detached_range: &TDetachedRange,
        emit_callback: TEmitCallback,
    ) {
        // unimplemented!()
    }

    pub(super) fn emit_leading_comments_of_position(&self, pos: isize) {
        // unimplemented!()
    }

    pub(super) fn emit_trailing_comments_of_position(
        &self,
        pos: isize,
        prefix_space: Option<bool>,
        force_no_newline: Option<bool>,
    ) {
        // unimplemented!()
    }

    pub(super) fn pipeline_emit_with_source_maps(&self, hint: EmitHint, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_source_maps_before_node(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_source_maps_after_node(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn set_source_map_source(&self, source: SourceMapSource) {
        unimplemented!()
    }
}

pub(super) fn create_brackets_map() -> HashMap<ListFormat, (&'static str, &'static str)> {
    HashMap::from_iter(IntoIterator::into_iter([
        (ListFormat::Braces, ("{", "}")),
        (ListFormat::Parenthesis, ("(", ")")),
        (ListFormat::AngleBrackets, ("<", ">")),
        (ListFormat::SquareBrackets, ("[", "]")),
    ]))
}

pub(super) fn get_opening_bracket(format: ListFormat) -> &'static str {
    brackets
        .get(&(format & ListFormat::BracketsMask))
        .unwrap()
        .0
}

pub(super) fn get_closing_bracket(format: ListFormat) -> &'static str {
    brackets
        .get(&(format & ListFormat::BracketsMask))
        .unwrap()
        .1
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TempFlags {
    Auto = 0x00000000,
    CountMask = 0x0FFFFFFF,
    _I = 0x10000000,
}
