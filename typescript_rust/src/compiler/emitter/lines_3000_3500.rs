use std::io;

use id_arena::Id;

use super::{
    ParenthesizeConciseBodyOfArrowFunctionCurrentParenthesizerRule,
    ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule,
    ParenthesizeExpressionOfExportDefaultCurrentParenthesizerRule,
    ParenthesizeRightSideOfBinaryCurrentParenthesizerRule,
};
use crate::{
    for_each, get_comment_range, get_emit_flags, is_block, is_let, is_module_declaration,
    is_var_const, node_is_synthesized, range_is_on_single_line, released, EmitFlags, HasArena,
    HasInitializerInterface, HasTypeInterface, HasTypeParametersInterface, InArena,
    InterfaceOrClassLikeDeclarationInterface, ListFormat, NamedDeclarationInterface, Node,
    NodeFlags, NodeInterface, Printer, ReadonlyTextRange, ReadonlyTextRangeConcrete, SyntaxKind,
    TextRange,
};

impl Printer {
    pub(super) fn emit_switch_statement(
        &self,
        node: Id<Node>, /*SwitchStatement*/
    ) -> io::Result<()> {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::SwitchKeyword,
            node.ref_(self).pos(),
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
        self.emit_expression(
            Some(released!(node.ref_(self).as_switch_statement().expression)),
            None,
        )?;
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node.ref_(self)
                .as_switch_statement()
                .expression
                .ref_(self)
                .end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.write_space();
        self.emit(
            Some(released!(node.ref_(self).as_switch_statement().case_block)),
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_labeled_statement(
        &self,
        node: Id<Node>, /*LabeledStatement*/
    ) -> io::Result<()> {
        self.emit(Some(node.ref_(self).as_labeled_statement().label), None)?;
        self.emit_token_with_comment(
            SyntaxKind::ColonToken,
            node.ref_(self)
                .as_labeled_statement()
                .label
                .ref_(self)
                .end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.write_space();
        self.emit(Some(node.ref_(self).as_labeled_statement().statement), None)?;

        Ok(())
    }

    pub(super) fn emit_throw_statement(
        &self,
        node: Id<Node>, /*ThrowStatement*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::ThrowKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit_expression_with_leading_space(
            released!(Some(node.ref_(self).as_throw_statement().expression)),
            None,
        )?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_try_statement(
        &self,
        node: Id<Node>, /*TryStatement*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::TryKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit(
            Some(released!(node.ref_(self).as_try_statement().try_block)),
            None,
        )?;
        if let Some(node_catch_clause) = released!(node.ref_(self).as_try_statement().catch_clause)
        {
            self.write_line_or_space(
                node,
                node.ref_(self).as_try_statement().try_block,
                node_catch_clause,
            );
            self.emit(Some(node_catch_clause), None)?;
        }
        Ok(
            if let Some(node_finally_block) =
                released!(node.ref_(self).as_try_statement().finally_block)
            {
                self.write_line_or_space(
                    node,
                    node.ref_(self)
                        .as_try_statement()
                        .catch_clause
                        .unwrap_or(node.ref_(self).as_try_statement().try_block),
                    node_finally_block,
                );
                self.emit_token_with_comment(
                    SyntaxKind::FinallyKeyword,
                    node.ref_(self)
                        .as_try_statement()
                        .catch_clause
                        .unwrap_or(node.ref_(self).as_try_statement().try_block)
                        .ref_(self)
                        .end(),
                    |text: &str| self.write_keyword(text),
                    node,
                    None,
                );
                self.write_space();
                self.emit(Some(node_finally_block), None)?;
            },
        )
    }

    pub(super) fn emit_debugger_statement(&self, node: Id<Node> /*DebuggerStatement*/) {
        self.write_token(
            SyntaxKind::DebuggerKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            None,
        );
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_variable_declaration(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
    ) -> io::Result<()> {
        self.emit(
            released!(node.ref_(self).as_variable_declaration().maybe_name()),
            None,
        )?;
        self.emit(
            node.ref_(self).as_variable_declaration().exclamation_token,
            None,
        )?;
        self.emit_type_annotation(node.ref_(self).as_variable_declaration().maybe_type())?;
        self.emit_initializer(
            released!(node
                .ref_(self)
                .as_variable_declaration()
                .maybe_initializer()),
            released!(node
                .ref_(self)
                .as_variable_declaration()
                .maybe_type()
                .map_or_else(
                    || {
                        node.ref_(self)
                            .as_variable_declaration()
                            .name()
                            .ref_(self)
                            .end()
                    },
                    |node_type| node_type.ref_(self).end(),
                )),
            node,
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_variable_declaration_list(
        &self,
        node: Id<Node>, /*VariableDeclarationList*/
    ) -> io::Result<()> {
        self.write_keyword(if is_let(node, self) {
            "let"
        } else if is_var_const(node, self) {
            "const"
        } else {
            "var"
        });
        self.write_space();
        self.emit_list(
            Some(node),
            released!(Some(
                node.ref_(self).as_variable_declaration_list().declarations
            )),
            ListFormat::VariableDeclarationList,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_function_declaration(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> io::Result<()> {
        self.emit_function_declaration_or_expression(node)?;

        Ok(())
    }

    pub(super) fn emit_function_declaration_or_expression(
        &self,
        node: Id<Node>, /*FunctionDeclaration | FunctionExpression*/
    ) -> io::Result<()> {
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.write_keyword("function");
        self.emit(
            node.ref_(self)
                .as_function_like_declaration()
                .maybe_asterisk_token(),
            None,
        )?;
        self.write_space();
        self.emit_identifier_name(node.ref_(self).as_function_like_declaration().maybe_name())?;
        self.emit_signature_and_body(node, |node: Id<Node>| self.emit_signature_head(node))?;

        Ok(())
    }

    pub(super) fn emit_signature_and_body(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration*/
        mut emit_signature_head: impl FnMut(Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        let body = node.ref_(self).as_function_like_declaration().maybe_body();
        Ok(if let Some(body) = body {
            if is_block(&body.ref_(self)) {
                let indented_flag = get_emit_flags(node, self).intersects(EmitFlags::Indented);
                if indented_flag {
                    self.increase_indent();
                }

                self.push_name_generation_scope(Some(node));
                for_each(
                    &*node
                        .ref_(self)
                        .as_function_like_declaration()
                        .parameters()
                        .ref_(self),
                    |&parameter: &Id<Node>, _| -> Option<()> {
                        self.generate_names(Some(parameter));
                        None
                    },
                );
                self.generate_names(node.ref_(self).as_function_like_declaration().maybe_body());

                emit_signature_head(node)?;
                self.emit_block_function_body(body)?;
                self.pop_name_generation_scope(Some(node));

                if indented_flag {
                    self.decrease_indent();
                }
            } else {
                emit_signature_head(node)?;
                self.write_space();
                self.emit_expression(
                    Some(body),
                    Some(self.alloc_current_parenthesizer_rule(Box::new(
                        ParenthesizeConciseBodyOfArrowFunctionCurrentParenthesizerRule::new(
                            self.parenthesizer(),
                            self,
                        ),
                    ))),
                )?;
            }
        } else {
            emit_signature_head(node)?;
            self.write_trailing_semicolon();
        })
    }

    pub(super) fn emit_signature_head(
        &self,
        node: Id<Node>, /*FunctionDeclaration | FunctionExpression | MethodDeclaration | AccessorDeclaration | ConstructorDeclaration*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_signature_declaration = node_ref.as_signature_declaration();
        self.emit_type_parameters(node, node_as_signature_declaration.maybe_type_parameters())?;
        self.emit_parameters(node, node_as_signature_declaration.parameters())?;
        self.emit_type_annotation(node_as_signature_declaration.maybe_type())?;

        Ok(())
    }

    pub(super) fn should_emit_block_function_body_on_single_line(
        &self,
        body: Id<Node>, /*Block*/
    ) -> bool {
        if get_emit_flags(body, self).intersects(EmitFlags::SingleLine) {
            return true;
        }

        let body_ref = body.ref_(self);
        let body_as_block = body_ref.as_block();
        if body_as_block.multi_line == Some(true) {
            return false;
        }

        if !node_is_synthesized(&*body.ref_(self))
            && !range_is_on_single_line(&*body.ref_(self), &self.current_source_file().ref_(self))
        {
            return false;
        }

        if self.get_leading_line_terminator_count(
            Some(body),
            &body_as_block.statements.ref_(self),
            ListFormat::PreserveLines,
        ) > 0
            || self.get_closing_line_terminator_count(
                Some(body),
                (&*body_as_block.statements.ref_(self)).into(),
                ListFormat::PreserveLines,
            ) > 0
        {
            return false;
        }

        let mut previous_statement: Option<Id<Node /*Statement*/>> = None;
        for &statement in &*body_as_block.statements.ref_(self) {
            if self.get_separating_line_terminator_count(
                previous_statement,
                statement,
                ListFormat::PreserveLines,
            ) > 0
            {
                return false;
            }

            previous_statement = Some(statement);
        }

        true
    }

    pub(super) fn emit_block_function_body(&self, body: Id<Node> /*Block*/) -> io::Result<()> {
        self.on_before_emit_node(Some(body));
        self.write_space();
        self.write_punctuation("{");
        self.increase_indent();

        let should_emit_block_function_body_on_single_line =
            self.should_emit_block_function_body_on_single_line(body);
        // if (emitBodyWithDetachedComments) {
        self.try_emit_body_with_detached_comments(
            body,
            &released!(ReadonlyTextRangeConcrete::from(
                &*body.ref_(self).as_block().statements.ref_(self)
            )),
            |node: Id<Node>| {
                Ok(if should_emit_block_function_body_on_single_line {
                    self.emit_block_function_body_on_single_line(node)?;
                } else {
                    self.emit_block_function_body_worker(node, None)?;
                })
            },
        )?;
        // }
        // else {
        //     emitBlockFunctionBody(body);
        // }

        self.decrease_indent();
        self.write_token(
            SyntaxKind::CloseBraceToken,
            body.ref_(self).as_block().statements.ref_(self).end(),
            |text: &str| self.write_punctuation(text),
            Some(body),
        );
        self.on_after_emit_node(Some(body));

        Ok(())
    }

    pub(super) fn emit_block_function_body_on_single_line(
        &self,
        body: Id<Node>, /*Block*/
    ) -> io::Result<()> {
        self.emit_block_function_body_worker(body, Some(true))?;

        Ok(())
    }

    pub(super) fn emit_block_function_body_worker(
        &self,
        body: Id<Node>, /*Block*/
        emit_block_function_body_on_single_line: Option<bool>,
    ) -> io::Result<()> {
        let statement_offset = self.emit_prologue_directives(
            &body.ref_(self).as_block().statements.ref_(self),
            None,
            &mut None,
            None,
        )?;
        let pos = self.writer().get_text_pos();
        self.emit_helpers(body);
        Ok(
            if statement_offset == 0
                && pos == self.writer().get_text_pos()
                && emit_block_function_body_on_single_line == Some(true)
            {
                self.decrease_indent();
                self.emit_list(
                    Some(body),
                    released!(Some(body.ref_(self).as_block().statements)),
                    ListFormat::SingleLineFunctionBodyStatements,
                    None,
                    None,
                    None,
                )?;
                self.increase_indent();
            } else {
                self.emit_list(
                    Some(body),
                    released!(Some(body.ref_(self).as_block().statements)),
                    ListFormat::MultiLineFunctionBodyStatements,
                    None,
                    Some(statement_offset),
                    None,
                )?;
            },
        )
    }

    pub(super) fn emit_class_declaration(
        &self,
        node: Id<Node>, /*ClassDeclaration*/
    ) -> io::Result<()> {
        self.emit_class_declaration_or_expression(node)?;

        Ok(())
    }

    pub(super) fn emit_class_declaration_or_expression(
        &self,
        node: Id<Node>, /*ClassDeclaration | ClassExpression*/
    ) -> io::Result<()> {
        for_each(
            &*node
                .ref_(self)
                .as_class_like_declaration()
                .members()
                .ref_(self),
            |&member: &Id<Node>, _| -> Option<()> {
                self.generate_member_names(Some(member));
                None
            },
        );

        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.write_keyword("class");
        if let Some(node_name) = node.ref_(self).as_class_like_declaration().maybe_name() {
            self.write_space();
            self.emit_identifier_name(Some(node_name))?;
        }

        let indented_flag = get_emit_flags(node, self).intersects(EmitFlags::Indented);
        if indented_flag {
            self.increase_indent();
        }

        self.emit_type_parameters(
            node,
            node.ref_(self)
                .as_class_like_declaration()
                .maybe_type_parameters(),
        )?;
        self.emit_list(
            Some(node),
            released!(node
                .ref_(self)
                .as_class_like_declaration()
                .maybe_heritage_clauses()),
            ListFormat::ClassHeritageClauses,
            None,
            None,
            None,
        )?;

        self.write_space();
        self.write_punctuation("{");
        self.emit_list(
            Some(node),
            released!(Some(node.ref_(self).as_class_like_declaration().members())),
            ListFormat::ClassMembers,
            None,
            None,
            None,
        )?;
        self.write_punctuation("}");

        Ok(if indented_flag {
            self.decrease_indent();
        })
    }

    pub(super) fn emit_interface_declaration(
        &self,
        node: Id<Node>, /*InterfaceDeclaration*/
    ) -> io::Result<()> {
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.write_keyword("interface");
        self.write_space();
        self.emit(
            node.ref_(self).as_interface_declaration().maybe_name(),
            None,
        )?;
        self.emit_type_parameters(
            node,
            node.ref_(self)
                .as_interface_declaration()
                .maybe_type_parameters(),
        )?;
        self.emit_list(
            Some(node),
            node.ref_(self)
                .as_interface_declaration()
                .maybe_heritage_clauses(),
            ListFormat::HeritageClauses,
            None,
            None,
            None,
        )?;
        self.write_space();
        self.write_punctuation("{");
        self.emit_list(
            Some(node),
            Some(node.ref_(self).as_interface_declaration().members),
            ListFormat::InterfaceMembers,
            None,
            None,
            None,
        )?;
        self.write_punctuation("}");

        Ok(())
    }

    pub(super) fn emit_type_alias_declaration(
        &self,
        node: Id<Node>, /*TypeAliasDeclaration*/
    ) -> io::Result<()> {
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.write_keyword("type");
        self.write_space();
        let node_ref = node.ref_(self);
        let node_as_type_alias_declaration = node_ref.as_type_alias_declaration();
        self.emit(node_as_type_alias_declaration.maybe_name(), None)?;
        self.emit_type_parameters(node, node_as_type_alias_declaration.maybe_type_parameters())?;
        self.write_space();
        self.write_punctuation("=");
        self.write_space();
        self.emit(node_as_type_alias_declaration.maybe_type(), None)?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_enum_declaration(
        &self,
        node: Id<Node>, /*EnumDeclaration*/
    ) -> io::Result<()> {
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.write_keyword("enum");
        self.write_space();
        let node_ref = node.ref_(self);
        let node_as_enum_declaration = node_ref.as_enum_declaration();
        self.emit(node_as_enum_declaration.maybe_name(), None)?;

        self.write_space();
        self.write_punctuation("{");
        self.emit_list(
            Some(node),
            Some(node_as_enum_declaration.members),
            ListFormat::EnumMembers,
            None,
            None,
            None,
        )?;
        self.write_punctuation("}");

        Ok(())
    }

    pub(super) fn emit_module_declaration(
        &self,
        node: Id<Node>, /*ModuleDeclaration*/
    ) -> io::Result<()> {
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        if (!node.ref_(self).flags()).intersects(NodeFlags::GlobalAugmentation) {
            self.write_keyword(
                if node.ref_(self).flags().intersects(NodeFlags::Namespace) {
                    "namespace"
                } else {
                    "module"
                },
            );
            self.write_space();
        }
        self.emit(node.ref_(self).as_module_declaration().maybe_name(), None)?;

        let mut body = node.ref_(self).as_module_declaration().body;
        if body.is_none() {
            return Ok(self.write_trailing_semicolon());
        }
        while let Some(body_present) = body.filter(|body| is_module_declaration(&body.ref_(self))) {
            self.write_punctuation(".");
            let body_present_ref = body_present.ref_(self);
            let body_as_module_declaration = body_present_ref.as_module_declaration();
            self.emit(body_as_module_declaration.maybe_name(), None)?;
            body = body_as_module_declaration.body;
        }

        self.write_space();
        self.emit(body, None)?;

        Ok(())
    }

    pub(super) fn emit_module_block(&self, node: Id<Node> /*ModuleBlock*/) -> io::Result<()> {
        self.push_name_generation_scope(Some(node));
        for_each(
            &*node.ref_(self).as_module_block().statements.ref_(self),
            |&statement: &Id<Node>, _| -> Option<()> {
                self.generate_names(Some(statement));
                None
            },
        );
        self.emit_block_statements(node, self.is_empty_block(node))?;
        self.pop_name_generation_scope(Some(node));

        Ok(())
    }

    pub(super) fn emit_case_block(&self, node: Id<Node> /*CaseBlock*/) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::OpenBraceToken,
            node.ref_(self).pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_list(
            Some(node),
            released!(Some(node.ref_(self).as_case_block().clauses)),
            ListFormat::CaseBlockClauses,
            None,
            None,
            None,
        )?;
        self.emit_token_with_comment(
            SyntaxKind::CloseBraceToken,
            node.ref_(self).as_case_block().clauses.ref_(self).end(),
            |text: &str| self.write_punctuation(text),
            node,
            Some(true),
        );

        Ok(())
    }

    pub(super) fn emit_import_equals_declaration(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration*/
    ) -> io::Result<()> {
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.emit_token_with_comment(
            SyntaxKind::ImportKeyword,
            node.ref_(self).maybe_modifiers().as_ref().map_or_else(
                || node.ref_(self).pos(),
                |node_modifiers| node_modifiers.ref_(self).end(),
            ),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_ref = node.ref_(self);
        let node_as_import_equals_declaration = node_ref.as_import_equals_declaration();
        if node_as_import_equals_declaration.is_type_only {
            self.emit_token_with_comment(
                SyntaxKind::TypeKeyword,
                node.ref_(self).pos(),
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
        }
        self.emit(node_as_import_equals_declaration.maybe_name(), None)?;
        self.write_space();
        self.emit_token_with_comment(
            SyntaxKind::EqualsToken,
            node_as_import_equals_declaration.name().ref_(self).end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.write_space();
        self.emit_module_reference(node_as_import_equals_declaration.module_reference)?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_module_reference(
        &self,
        node: Id<Node>, /*ModuleReference*/
    ) -> io::Result<()> {
        Ok(if node.ref_(self).kind() == SyntaxKind::Identifier {
            self.emit_expression(Some(node), None)?;
        } else {
            self.emit(Some(node), None)?;
        })
    }

    pub(super) fn emit_import_declaration(
        &self,
        node: Id<Node>, /*ImportDeclaration*/
    ) -> io::Result<()> {
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.emit_token_with_comment(
            SyntaxKind::ImportKeyword,
            node.ref_(self).maybe_modifiers().as_ref().map_or_else(
                || node.ref_(self).pos(),
                |node_modifiers| node_modifiers.ref_(self).end(),
            ),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_ref = node.ref_(self);
        let node_as_import_declaration = node_ref.as_import_declaration();
        if let Some(node_import_clause) = node_as_import_declaration.import_clause {
            self.emit(Some(node_import_clause), None)?;
            self.write_space();
            self.emit_token_with_comment(
                SyntaxKind::FromKeyword,
                node_import_clause.ref_(self).end(),
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
        }
        self.emit_expression(Some(node_as_import_declaration.module_specifier), None)?;
        if let Some(node_assert_clause) = node_as_import_declaration.assert_clause {
            self.emit_with_leading_space(Some(node_assert_clause))?;
        }
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_import_clause(
        &self,
        node: Id<Node>, /*ImportClause*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_import_clause = node_ref.as_import_clause();
        if node_as_import_clause.is_type_only {
            self.emit_token_with_comment(
                SyntaxKind::TypeKeyword,
                node.ref_(self).pos(),
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
        }
        self.emit(node_as_import_clause.maybe_name(), None)?;
        if let Some(node_name) = node_as_import_clause.maybe_name() {
            if node_as_import_clause.named_bindings.is_some() {
                self.emit_token_with_comment(
                    SyntaxKind::CommaToken,
                    node_name.ref_(self).end(),
                    |text: &str| self.write_punctuation(text),
                    node,
                    None,
                );
                self.write_space();
            }
        }
        self.emit(node_as_import_clause.named_bindings, None)?;

        Ok(())
    }

    pub(super) fn emit_namespace_import(
        &self,
        node: Id<Node>, /*NamespaceImport*/
    ) -> io::Result<()> {
        let as_pos = self.emit_token_with_comment(
            SyntaxKind::AsteriskToken,
            node.ref_(self).pos(),
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
        self.emit(node.ref_(self).as_namespace_import().maybe_name(), None)?;

        Ok(())
    }

    pub(super) fn emit_named_imports(
        &self,
        node: Id<Node>, /*NamedImports*/
    ) -> io::Result<()> {
        self.emit_named_imports_or_exports(node)?;

        Ok(())
    }

    pub(super) fn emit_import_specifier(
        &self,
        node: Id<Node>, /*ImportSpecifier*/
    ) -> io::Result<()> {
        self.emit_import_or_export_specifier(node)?;

        Ok(())
    }

    pub(super) fn emit_export_assignment(
        &self,
        node: Id<Node>, /*ExportAssignment*/
    ) -> io::Result<()> {
        let next_pos = self.emit_token_with_comment(
            SyntaxKind::ExportKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_ref = node.ref_(self);
        let node_as_export_assignment = node_ref.as_export_assignment();
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
            Some(node_as_export_assignment.expression),
            if node_as_export_assignment.is_export_equals == Some(true) {
                Some(self.alloc_current_parenthesizer_rule(Box::new(
                    ParenthesizeRightSideOfBinaryCurrentParenthesizerRule::new(
                        self.parenthesizer(),
                        self,
                    ),
                )))
            } else {
                Some(self.alloc_current_parenthesizer_rule(Box::new(
                    ParenthesizeExpressionOfExportDefaultCurrentParenthesizerRule::new(
                        self.parenthesizer(),
                        self,
                    ),
                )))
            },
        )?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_export_declaration(
        &self,
        node: Id<Node>, /*ExportDeclaration*/
    ) -> io::Result<()> {
        let mut next_pos = self.emit_token_with_comment(
            SyntaxKind::ExportKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_ref = node.ref_(self);
        let node_as_export_declaration = node_ref.as_export_declaration();
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
        if let Some(node_export_clause) = node_as_export_declaration.export_clause {
            self.emit(Some(node_export_clause), None)?;
        } else {
            next_pos = self.emit_token_with_comment(
                SyntaxKind::AsteriskToken,
                next_pos,
                |text: &str| self.write_punctuation(text),
                node,
                None,
            );
        }
        if let Some(node_module_specifier) = node_as_export_declaration.module_specifier {
            self.write_space();
            let from_pos = node_as_export_declaration
                .export_clause
                .map_or(next_pos, |node_export_clause| {
                    node_export_clause.ref_(self).end()
                });
            self.emit_token_with_comment(
                SyntaxKind::FromKeyword,
                from_pos,
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
            self.emit_expression(Some(node_module_specifier), None)?;
        }
        if let Some(node_assert_clause) = node_as_export_declaration.assert_clause {
            self.emit_with_leading_space(Some(node_assert_clause))?;
        }
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_assert_clause(
        &self,
        node: Id<Node>, /*AssertClause*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::AssertKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_ref = node.ref_(self);
        let elements = node_ref.as_assert_clause().elements;
        self.emit_list(
            Some(node),
            Some(elements),
            ListFormat::ImportClauseEntries,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_assert_entry(&self, node: Id<Node> /*AssertEntry*/) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_assert_entry = node_ref.as_assert_entry();
        self.emit(Some(node_as_assert_entry.name), None)?;
        self.write_punctuation(":");
        self.write_space();

        let value = node_as_assert_entry.value;
        if !get_emit_flags(value, self).intersects(EmitFlags::NoLeadingComments) {
            let comment_range = get_comment_range(value, self);
            self.emit_trailing_comments_of_position(comment_range.pos(), None, None);
        }
        self.emit(Some(value), None)?;

        Ok(())
    }

    pub(super) fn emit_namespace_export_declaration(
        &self,
        node: Id<Node>, /*NamespaceExportDeclaration*/
    ) -> io::Result<()> {
        let mut next_pos = self.emit_token_with_comment(
            SyntaxKind::ExportKeyword,
            node.ref_(self).pos(),
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
            node.ref_(self)
                .as_namespace_export_declaration()
                .maybe_name(),
            None,
        )?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_namespace_export(
        &self,
        node: Id<Node>, /*NamespaceExport*/
    ) -> io::Result<()> {
        let as_pos = self.emit_token_with_comment(
            SyntaxKind::AsteriskToken,
            node.ref_(self).pos(),
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
        self.emit(Some(node.ref_(self).as_namespace_export().name), None)?;

        Ok(())
    }

    pub(super) fn emit_named_exports(
        &self,
        node: Id<Node>, /*NamedExports*/
    ) -> io::Result<()> {
        self.emit_named_imports_or_exports(node)?;

        Ok(())
    }

    pub(super) fn emit_export_specifier(
        &self,
        node: Id<Node>, /*ExportSpecifier*/
    ) -> io::Result<()> {
        self.emit_import_or_export_specifier(node)?;

        Ok(())
    }

    pub(super) fn emit_named_imports_or_exports(
        &self,
        node: Id<Node>, /*NamedImportsOrExports*/
    ) -> io::Result<()> {
        self.write_punctuation("{");
        self.emit_list(
            Some(node),
            Some(node.ref_(self).as_has_elements().elements()),
            ListFormat::NamedImportsOrExportsElements,
            None,
            None,
            None,
        )?;
        self.write_punctuation("}");

        Ok(())
    }

    pub(super) fn emit_import_or_export_specifier(
        &self,
        node: Id<Node>, /*ImportOrExportSpecifier*/
    ) -> io::Result<()> {
        if node.ref_(self).as_has_is_type_only().is_type_only() {
            self.write_keyword("type");
            self.write_space();
        }
        if let Some(node_property_name) =
            node.ref_(self).as_has_property_name().maybe_property_name()
        {
            self.emit(Some(node_property_name), None)?;
            self.write_space();
            self.emit_token_with_comment(
                SyntaxKind::AsKeyword,
                node_property_name.ref_(self).end(),
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            self.write_space();
        }

        self.emit(node.ref_(self).as_named_declaration().maybe_name(), None)?;

        Ok(())
    }

    pub(super) fn emit_external_module_reference(
        &self,
        node: Id<Node>, /*ExternalModuleReference*/
    ) -> io::Result<()> {
        self.write_keyword("require");
        self.write_punctuation("(");
        self.emit_expression(
            Some(node.ref_(self).as_external_module_reference().expression),
            None,
        )?;
        self.write_punctuation(")");

        Ok(())
    }
}
