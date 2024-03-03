use std::{cell::RefCell, io, rc::Rc};

use id_arena::Id;

use super::PipelinePhase;
use crate::{
    cast, create_binary_expression_trampoline, get_emit_flags, get_parse_tree_node, impl_has_arena,
    is_binary_expression, is_block, is_expression, is_json_source_file, node_is_synthesized,
    positions_are_on_same_line, released, skip_trivia, AllArenas, BinaryExpressionStateMachine,
    BinaryExpressionTrampoline, CurrentParenthesizerRule, Debug_, EmitFlags, EmitHint, HasArena,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, InArena, LeftOrRight,
    ListFormat, NamedDeclarationInterface, Node, NodeInterface, ParenthesizerRules, Printer,
    ReadonlyTextRange, SignatureDeclarationInterface, SourceFileLike, SyntaxKind,
};

impl Printer {
    pub(super) fn emit_call_expression(
        &self,
        node: Id<Node>, /*CallExpression*/
    ) -> io::Result<()> {
        let indirect_call = get_emit_flags(node, self).intersects(EmitFlags::IndirectCall);
        if indirect_call {
            self.write_punctuation("(");
            self.write_literal("0");
            self.write_punctuation(",");
            self.write_space();
        }
        self.emit_expression(
            released!(Some(node.ref_(self).as_call_expression().expression)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        if indirect_call {
            self.write_punctuation(")");
        }
        self.emit(
            node.ref_(self).as_call_expression().question_dot_token,
            None,
        )?;
        self.emit_type_arguments(
            node,
            node.ref_(self).as_call_expression().maybe_type_arguments(),
        )?;
        self.emit_expression_list(
            Some(node),
            released!(Some(node.ref_(self).as_call_expression().arguments)),
            ListFormat::CallExpressionArguments,
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_new_expression(
        &self,
        node: Id<Node>, /*NewExpression*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::NewKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            released!(Some(node.ref_(self).as_new_expression().expression)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionOfNewCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.emit_type_arguments(
            node,
            node.ref_(self).as_new_expression().maybe_type_arguments(),
        )?;
        self.emit_expression_list(
            Some(node),
            released!(node.ref_(self).as_new_expression().arguments),
            ListFormat::NewExpressionArguments,
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_tagged_template_expression(
        &self,
        node: Id<Node>, /*TaggedTemplateExpression*/
    ) -> io::Result<()> {
        let indirect_call = get_emit_flags(node, self).intersects(EmitFlags::IndirectCall);
        if indirect_call {
            self.write_punctuation("(");
            self.write_literal("0");
            self.write_punctuation(",");
            self.write_space();
        }
        self.emit_expression(
            released!(Some(node.ref_(self).as_tagged_template_expression().tag)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        if indirect_call {
            self.write_punctuation(")");
        }
        self.emit_type_arguments(
            node,
            node.ref_(self)
                .as_tagged_template_expression()
                .maybe_type_arguments(),
        )?;
        self.write_space();
        self.emit_expression(
            Some(node.ref_(self).as_tagged_template_expression().template),
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_type_assertion_expression(
        &self,
        node: Id<Node>, /*TypeAssertion*/
    ) -> io::Result<()> {
        self.write_punctuation("<");
        let node_ref = node.ref_(self);
        let node_as_type_assertion = node_ref.as_type_assertion();
        self.emit(Some(node_as_type_assertion.type_), None)?;
        self.write_punctuation(">");
        self.emit_expression(
            Some(node_as_type_assertion.expression),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_parenthesized_expression(
        &self,
        node: Id<Node>, /*ParenthesizedExpression*/
    ) -> io::Result<()> {
        let _open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::OpenParenToken,
            node.ref_(self).pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        let indented = self.write_line_separators_and_indent_before(
            node.ref_(self).as_parenthesized_expression().expression,
            node,
        );
        self.emit_expression(
            Some(released!(
                node.ref_(self).as_parenthesized_expression().expression
            )),
            None,
        )?;
        self.write_line_separators_after(
            node.ref_(self).as_parenthesized_expression().expression,
            node,
        );
        self.decrease_indent_if(indented, None);
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            /*node.expression ?*/
            node.ref_(self)
                .as_parenthesized_expression()
                .expression
                .ref_(self)
                .end(), /*: openParenPos*/
            |str_| self.write_punctuation(str_),
            node,
            None,
        );

        Ok(())
    }

    pub(super) fn emit_function_expression(
        &self,
        node: Id<Node>, /*FunctionExpression*/
    ) -> io::Result<()> {
        self.generate_name_if_needed(node.ref_(self).as_function_expression().maybe_name());
        self.emit_function_declaration_or_expression(node)?;

        Ok(())
    }

    pub(super) fn emit_arrow_function(
        &self,
        node: Id<Node>, /*ArrowFunction*/
    ) -> io::Result<()> {
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.emit_signature_and_body(node, |node: Id<Node>| self.emit_arrow_function_head(node))?;

        Ok(())
    }

    pub(super) fn emit_arrow_function_head(
        &self,
        node: Id<Node>, /*ArrowFunction*/
    ) -> io::Result<()> {
        self.emit_type_parameters(
            node,
            node.ref_(self).as_arrow_function().maybe_type_parameters(),
        )?;
        self.emit_parameters_for_arrow(
            node,
            released!(node.ref_(self).as_arrow_function().parameters()),
        )?;
        self.emit_type_annotation(node.ref_(self).as_arrow_function().maybe_type())?;
        self.write_space();
        self.emit(
            Some(
                node.ref_(self)
                    .as_arrow_function()
                    .equals_greater_than_token,
            ),
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_delete_expression(
        &self,
        node: Id<Node>, /*DeleteExpression*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::DeleteKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            released!(Some(node.ref_(self).as_delete_expression().expression)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_type_of_expression(
        &self,
        node: Id<Node>, /*TypeOfExpression*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::TypeOfKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            released!(Some(node.ref_(self).as_type_of_expression().expression)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_void_expression(
        &self,
        node: Id<Node>, /*VoidExpression*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::VoidKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            released!(Some(node.ref_(self).as_void_expression().expression)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_await_expression(
        &self,
        node: Id<Node>, /*AwaitExpression*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::AwaitKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            released!(Some(node.ref_(self).as_await_expression().expression)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_prefix_unary_expression(
        &self,
        node: Id<Node>, /*PrefixUnaryExpression*/
    ) -> io::Result<()> {
        self.write_token_text(
            node.ref_(self).as_prefix_unary_expression().operator,
            |text: &str| self.write_operator(text),
            None,
        );
        if self.should_emit_whitespace_before_operand(node) {
            self.write_space();
        }
        self.emit_expression(
            released!(Some(node.ref_(self).as_prefix_unary_expression().operand)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn should_emit_whitespace_before_operand(
        &self,
        node: Id<Node>, /*PrefixUnaryExpression*/
    ) -> bool {
        let node_ref = node.ref_(self);
        let node_as_prefix_unary_expression = node_ref.as_prefix_unary_expression();
        let operand = node_as_prefix_unary_expression.operand;
        operand.ref_(self).kind() == SyntaxKind::PrefixUnaryExpression
            && (node_as_prefix_unary_expression.operator == SyntaxKind::PlusToken
                && matches!(
                    operand.ref_(self).as_prefix_unary_expression().operator,
                    SyntaxKind::PlusToken | SyntaxKind::PlusPlusToken
                )
                || node_as_prefix_unary_expression.operator == SyntaxKind::MinusToken
                    && matches!(
                        operand.ref_(self).as_prefix_unary_expression().operator,
                        SyntaxKind::MinusToken | SyntaxKind::MinusMinusToken
                    ))
    }

    pub(super) fn emit_postfix_unary_expression(
        &self,
        node: Id<Node>, /*PostfixUnaryExpression*/
    ) -> io::Result<()> {
        self.emit_expression(
            released!(Some(node.ref_(self).as_postfix_unary_expression().operand)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeOperandOfPostfixUnaryCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.write_token_text(
            node.ref_(self).as_postfix_unary_expression().operator,
            |text: &str| self.write_operator(text),
            None,
        );

        Ok(())
    }

    pub(super) fn create_emit_binary_expression(&self) -> EmitBinaryExpression {
        let trampoline = create_binary_expression_trampoline(
            EmitBinaryExpressionStateMachine::new(self.arena_id(), self),
        );
        EmitBinaryExpression::new(trampoline)
    }

    pub(super) fn emit_conditional_expression(
        &self,
        node: Id<Node>, /*ConditionalExpression*/
    ) -> io::Result<()> {
        let lines_before_question = self.get_lines_between_nodes(
            node,
            node.ref_(self).as_conditional_expression().condition,
            node.ref_(self).as_conditional_expression().question_token,
        );
        let lines_after_question = self.get_lines_between_nodes(
            node,
            node.ref_(self).as_conditional_expression().question_token,
            node.ref_(self).as_conditional_expression().when_true,
        );
        let lines_before_colon = self.get_lines_between_nodes(
            node,
            node.ref_(self).as_conditional_expression().when_true,
            node.ref_(self).as_conditional_expression().colon_token,
        );
        let lines_after_colon = self.get_lines_between_nodes(
            node,
            node.ref_(self).as_conditional_expression().colon_token,
            node.ref_(self).as_conditional_expression().when_false,
        );

        self.emit_expression(
            released!(Some(node.ref_(self).as_conditional_expression().condition)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeConditionOfConditionalExpressionCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.write_lines_and_indent(lines_before_question, true);
        self.emit(
            Some(node.ref_(self).as_conditional_expression().question_token),
            None,
        )?;
        self.write_lines_and_indent(lines_after_question, true);
        self.emit_expression(
            released!(Some(node.ref_(self).as_conditional_expression().when_true)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeBranchOfConditionalExpressionCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.decrease_indent_if(lines_before_question != 0, Some(lines_after_question != 0));

        self.write_lines_and_indent(lines_before_colon, true);
        self.emit(
            Some(node.ref_(self).as_conditional_expression().colon_token),
            None,
        )?;
        self.write_lines_and_indent(lines_after_colon, true);
        self.emit_expression(
            released!(Some(node.ref_(self).as_conditional_expression().when_false)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeBranchOfConditionalExpressionCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.decrease_indent_if(lines_before_colon != 0, Some(lines_after_colon != 0));

        Ok(())
    }

    pub(super) fn emit_template_expression(
        &self,
        node: Id<Node>, /*TemplateExpression*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_template_expression = node_ref.as_template_expression();
        self.emit(Some(node_as_template_expression.head), None)?;
        self.emit_list(
            Some(node),
            Some(node_as_template_expression.template_spans),
            ListFormat::TemplateExpressionSpans,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_yield_expression(
        &self,
        node: Id<Node>, /*YieldExpression*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::YieldKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit(node.ref_(self).as_yield_expression().asterisk_token, None)?;
        self.emit_expression_with_leading_space(
            released!(node.ref_(self).as_yield_expression().expression),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_spread_element(
        &self,
        node: Id<Node>, /*SpreadElement*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::DotDotDotToken,
            node.ref_(self).pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_expression(
            released!(Some(node.ref_(self).as_spread_element().expression)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_class_expression(
        &self,
        node: Id<Node>, /*ClassExpression*/
    ) -> io::Result<()> {
        self.generate_name_if_needed(node.ref_(self).as_class_expression().maybe_name());
        self.emit_class_declaration_or_expression(node)?;

        Ok(())
    }

    pub(super) fn emit_expression_with_type_arguments(
        &self,
        node: Id<Node>, /*ExpressionWithTypeArguments*/
    ) -> io::Result<()> {
        self.emit_expression(
            Some(released!(
                node.ref_(self)
                    .as_expression_with_type_arguments()
                    .expression
            )),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.emit_type_arguments(
            node,
            node.ref_(self)
                .as_expression_with_type_arguments()
                .maybe_type_arguments(),
        )?;

        Ok(())
    }

    pub(super) fn emit_as_expression(
        &self,
        node: Id<Node>, /*AsExpression*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_as_expression = node_ref.as_as_expression();
        self.emit_expression(Some(node_as_as_expression.expression), None)?;
        Ok(
            if let Some(node_type) = node_as_as_expression.maybe_type() {
                self.write_space();
                self.write_keyword("as");
                self.write_space();
                self.emit(Some(node_type), None)?;
            },
        )
    }

    pub(super) fn emit_non_null_expression(
        &self,
        node: Id<Node>, /*NonNullExpression*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_non_null_expression = node_ref.as_non_null_expression();
        self.emit_expression(
            Some(node_as_non_null_expression.expression),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.write_operator("!");

        Ok(())
    }

    pub(super) fn emit_meta_property(
        &self,
        node: Id<Node>, /*MetaProperty*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_meta_property = node_ref.as_meta_property();
        self.write_token(
            node_as_meta_property.keyword_token,
            node.ref_(self).pos(),
            |text: &str| self.write_punctuation(text),
            None,
        );
        self.write_punctuation(".");
        self.emit(Some(node_as_meta_property.name), None)?;

        Ok(())
    }

    pub(super) fn emit_template_span(
        &self,
        node: Id<Node>, /*TemplateSpan*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_template_span = node_ref.as_template_span();
        self.emit_expression(Some(node_as_template_span.expression), None)?;
        self.emit(Some(node_as_template_span.literal), None)?;

        Ok(())
    }

    pub(super) fn emit_block(&self, node: Id<Node> /*Block*/) -> io::Result<()> {
        self.emit_block_statements(
            node,
            node.ref_(self).as_block().multi_line != Some(true) && self.is_empty_block(node),
        )?;

        Ok(())
    }

    pub(super) fn emit_block_statements(
        &self,
        node: Id<Node>, /*BlockLlke*/
        force_single_line: bool,
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::OpenBraceToken,
            node.ref_(self).pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        let format =
            if force_single_line || get_emit_flags(node, self).intersects(EmitFlags::SingleLine) {
                ListFormat::SingleLineBlockStatements
            } else {
                ListFormat::MultiLineBlockStatements
            };
        self.emit_list(
            Some(node),
            released!(Some(node.ref_(self).as_has_statements().statements())),
            format,
            None,
            None,
            None,
        )?;
        self.emit_token_with_comment(
            SyntaxKind::CloseBraceToken,
            node.ref_(self)
                .as_has_statements()
                .statements()
                .ref_(self)
                .end(),
            |text: &str| self.write_punctuation(text),
            node,
            Some(format.intersects(ListFormat::MultiLine)),
        );

        Ok(())
    }

    pub(super) fn emit_variable_statement(
        &self,
        node: Id<Node>, /*VariableStatement*/
    ) -> io::Result<()> {
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.emit(
            released!(Some(
                node.ref_(self).as_variable_statement().declaration_list
            )),
            None,
        )?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_empty_statement(&self, is_embedded_statement: bool) {
        if is_embedded_statement {
            self.write_punctuation(";");
        } else {
            self.write_trailing_semicolon();
        }
    }

    pub(super) fn emit_expression_statement(
        &self,
        node: Id<Node>, /*ExpressionStatement*/
    ) -> io::Result<()> {
        self.emit_expression(
            released!(Some(node.ref_(self).as_expression_statement().expression)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionOfExpressionStatementCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        Ok(
            if !is_json_source_file(&self.current_source_file().ref_(self))
                || node_is_synthesized(
                    &*node
                        .ref_(self)
                        .as_expression_statement()
                        .expression
                        .ref_(self),
                )
            {
                self.write_trailing_semicolon();
            },
        )
    }

    pub(super) fn emit_if_statement(&self, node: Id<Node> /*IfStatement*/) -> io::Result<()> {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::IfKeyword,
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
            Some(released!(node.ref_(self).as_if_statement().expression)),
            None,
        )?;
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node.ref_(self)
                .as_if_statement()
                .expression
                .ref_(self)
                .end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_embedded_statement(
            node,
            released!(node.ref_(self).as_if_statement().then_statement),
        )?;
        Ok(
            if let Some(node_else_statement) =
                released!(node.ref_(self).as_if_statement().else_statement)
            {
                self.write_line_or_space(
                    node,
                    node.ref_(self).as_if_statement().then_statement,
                    node_else_statement,
                );
                self.emit_token_with_comment(
                    SyntaxKind::ElseKeyword,
                    node.ref_(self)
                        .as_if_statement()
                        .then_statement
                        .ref_(self)
                        .end(),
                    |text: &str| self.write_keyword(text),
                    node,
                    None,
                );
                if node_else_statement.ref_(self).kind() == SyntaxKind::IfStatement {
                    self.write_space();
                    self.emit(Some(node_else_statement), None)?;
                } else {
                    self.emit_embedded_statement(node, node_else_statement)?;
                }
            },
        )
    }

    pub(super) fn emit_while_clause(
        &self,
        node: Id<Node>, /*WhileStatement | DoStatement*/
        start_pos: isize,
    ) -> io::Result<()> {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::WhileKeyword,
            start_pos,
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
            released!(node.ref_(self).as_has_expression().maybe_expression()),
            None,
        )?;
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node.ref_(self)
                .as_has_expression()
                .expression()
                .ref_(self)
                .end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );

        Ok(())
    }

    pub(super) fn emit_do_statement(&self, node: Id<Node> /*DoStatement*/) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::DoKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit_embedded_statement(node, released!(node.ref_(self).as_do_statement().statement))?;
        if is_block(&node.ref_(self).as_do_statement().statement.ref_(self))
            && self.maybe_preserve_source_newlines() != Some(true)
        {
            self.write_space();
        } else {
            self.write_line_or_space(
                node,
                node.ref_(self).as_do_statement().statement,
                node.ref_(self).as_do_statement().expression,
            );
        }

        self.emit_while_clause(
            node,
            node.ref_(self).as_do_statement().statement.ref_(self).end(),
        )?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_while_statement(
        &self,
        node: Id<Node>, /*WhileStatement*/
    ) -> io::Result<()> {
        self.emit_while_clause(node, released!(node.ref_(self).pos()))?;
        self.emit_embedded_statement(
            node,
            released!(node.ref_(self).as_while_statement().statement),
        )?;

        Ok(())
    }

    pub(super) fn emit_for_statement(
        &self,
        node: Id<Node>, /*ForStatement*/
    ) -> io::Result<()> {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::ForKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let mut pos = self.emit_token_with_comment(
            SyntaxKind::OpenParenToken,
            open_paren_pos,
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_for_binding(released!(node.ref_(self).as_for_statement().initializer))?;
        pos = self.emit_token_with_comment(
            SyntaxKind::SemicolonToken,
            node.ref_(self)
                .as_for_statement()
                .initializer
                .as_ref()
                .map_or(pos, |node_initializer| node_initializer.ref_(self).end()),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_expression_with_leading_space(
            released!(node.ref_(self).as_for_statement().condition),
            None,
        )?;
        pos = self.emit_token_with_comment(
            SyntaxKind::SemicolonToken,
            node.ref_(self)
                .as_for_statement()
                .condition
                .as_ref()
                .map_or(pos, |node_condition| node_condition.ref_(self).end()),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_expression_with_leading_space(
            released!(node.ref_(self).as_for_statement().incrementor),
            None,
        )?;
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node.ref_(self)
                .as_for_statement()
                .incrementor
                .map_or(pos, |node_incrementor| node_incrementor.ref_(self).end()),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_embedded_statement(
            node,
            released!(node.ref_(self).as_for_statement().statement),
        )?;

        Ok(())
    }

    pub(super) fn emit_for_in_statement(
        &self,
        node: Id<Node>, /*ForInStatement*/
    ) -> io::Result<()> {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::ForKeyword,
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
        self.emit_for_binding(Some(released!(
            node.ref_(self).as_for_in_statement().initializer
        )))?;
        self.write_space();
        self.emit_token_with_comment(
            SyntaxKind::InKeyword,
            node.ref_(self)
                .as_for_in_statement()
                .initializer
                .ref_(self)
                .end(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            Some(released!(node.ref_(self).as_for_in_statement().expression)),
            None,
        )?;
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node.ref_(self)
                .as_for_in_statement()
                .expression
                .ref_(self)
                .end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_embedded_statement(
            node,
            released!(node.ref_(self).as_for_in_statement().statement),
        )?;

        Ok(())
    }

    pub(super) fn emit_for_of_statement(
        &self,
        node: Id<Node>, /*ForOfStatement*/
    ) -> io::Result<()> {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::ForKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_with_trailing_space(node.ref_(self).as_for_of_statement().await_modifier)?;
        self.emit_token_with_comment(
            SyntaxKind::OpenParenToken,
            open_paren_pos,
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_for_binding(Some(released!(
            node.ref_(self).as_for_of_statement().initializer
        )))?;
        self.write_space();
        self.emit_token_with_comment(
            SyntaxKind::OfKeyword,
            node.ref_(self)
                .as_for_of_statement()
                .initializer
                .ref_(self)
                .end(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            Some(released!(node.ref_(self).as_for_of_statement().expression)),
            None,
        )?;
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node.ref_(self)
                .as_for_of_statement()
                .expression
                .ref_(self)
                .end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_embedded_statement(
            node,
            released!(node.ref_(self).as_for_of_statement().statement),
        )?;

        Ok(())
    }

    pub(super) fn emit_for_binding(
        &self,
        node: Option<Id<Node> /*VariableDeclarationList | Expression*/>,
    ) -> io::Result<()> {
        Ok(if let Some(node) = node {
            if node.ref_(self).kind() == SyntaxKind::VariableDeclarationList {
                self.emit(Some(node), None)?;
            } else {
                self.emit_expression(Some(node), None)?;
            }
        })
    }

    pub(super) fn emit_continue_statement(
        &self,
        node: Id<Node>, /*ContinueStatement*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::ContinueKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit_with_leading_space(node.ref_(self).as_continue_statement().label)?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_break_statement(
        &self,
        node: Id<Node>, /*BreakStatement*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::BreakKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit_with_leading_space(node.ref_(self).as_break_statement().label)?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_token_with_comment(
        &self,
        token: SyntaxKind,
        mut pos: isize,
        writer: impl FnMut(&str),
        context_node: Id<Node>,
        indent_leading: Option<bool>,
    ) -> isize {
        let node = get_parse_tree_node(
            Some(context_node),
            Option::<fn(Id<Node>) -> bool>::None,
            self,
        );
        let is_similar_node = matches!(
            node,
            Some(node) if node.ref_(self).kind() == context_node.ref_(self).kind()
        );
        let start_pos = pos;
        if is_similar_node {
            if let Some(current_source_file) = self.maybe_current_source_file() {
                pos = skip_trivia(
                    &current_source_file
                        .ref_(self)
                        .as_source_file()
                        .text_as_chars(),
                    pos,
                    None,
                    None,
                    None,
                );
            }
        }
        if is_similar_node && context_node.ref_(self).pos() != start_pos {
            let needs_indent = indent_leading == Some(true)
                && matches!(
                    self.maybe_current_source_file(),
                    Some(current_source_file) if !positions_are_on_same_line(
                        start_pos,
                        pos,
                        &current_source_file.ref_(self),
                    )
                );
            if needs_indent {
                self.increase_indent();
            }
            self.emit_leading_comments_of_position(start_pos);
            if needs_indent {
                self.decrease_indent();
            }
        }
        pos = self.write_token_text(token, writer, Some(pos)).unwrap();
        if is_similar_node && context_node.ref_(self).end() != pos {
            let is_jsx_expr_context = context_node.ref_(self).kind() == SyntaxKind::JsxExpression;
            self.emit_trailing_comments_of_position(
                pos,
                Some(!is_jsx_expr_context),
                Some(is_jsx_expr_context),
            );
        }
        pos
    }

    pub(super) fn emit_return_statement(
        &self,
        node: Id<Node>, /*ReturnStatement*/
    ) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::ReturnKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit_expression_with_leading_space(
            released!(node.ref_(self).as_return_statement().expression),
            None,
        )?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_with_statement(
        &self,
        node: Id<Node>, /*WithStatement*/
    ) -> io::Result<()> {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::WithKeyword,
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
        self.emit_expression(Some(node.ref_(self).as_with_statement().expression), None)?;
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node.ref_(self)
                .as_with_statement()
                .expression
                .ref_(self)
                .end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_embedded_statement(node, node.ref_(self).as_with_statement().statement)?;

        Ok(())
    }
}

pub struct EmitBinaryExpression {
    trampoline: BinaryExpressionTrampoline<EmitBinaryExpressionStateMachine>,
}

impl EmitBinaryExpression {
    pub fn new(trampoline: BinaryExpressionTrampoline<EmitBinaryExpressionStateMachine>) -> Self {
        Self { trampoline }
    }

    pub fn call(&self, node: Id<Node> /*BinaryExpression*/) -> io::Result<()> {
        self.trampoline.call(node, ())?;

        Ok(())
    }
}

pub struct WorkArea {
    pub stack_index: isize,
    pub preserve_source_newlines_stack: Vec<Option<bool>>,
    pub container_pos_stack: Vec<isize>,
    pub container_end_stack: Vec<isize>,
    pub declaration_list_container_end_stack: Vec<isize>,
    pub should_emit_comments_stack: Vec<bool>,
    pub should_emit_source_maps_stack: Vec<bool>,
}

pub struct EmitBinaryExpressionStateMachine {
    arena: *const AllArenas,
    printer: Id<Printer>,
}

impl EmitBinaryExpressionStateMachine {
    pub fn new(printer: Id<Printer>, arena: &impl HasArena) -> Self {
        Self {
            printer,
            arena: arena.arena(),
        }
    }

    fn maybe_emit_expression(
        &self,
        mut next: Id<Node>, /*Expression*/
        parent: Id<Node>,   /*BinaryExpression*/
        side: LeftOrRight,
    ) -> io::Result<Option<Id<Node>>> {
        let parenthesizer_rule: Id<Box<dyn CurrentParenthesizerRule>> = self
            .alloc_current_parenthesizer_rule(Box::new(
                MaybeEmitExpressionCurrentParenthesizerRule::new(
                    side,
                    self.printer.ref_(self).parenthesizer(),
                    parent
                        .ref_(self)
                        .as_binary_expression()
                        .operator_token
                        .ref_(self)
                        .kind(),
                    self,
                ),
            ));

        let mut pipeline_phase = self.printer.ref_(self).get_pipeline_phase(
            PipelinePhase::Notification,
            EmitHint::Expression,
            next,
        )?;
        // per https://users.rust-lang.org/t/compare-function-pointers-for-equality/52339/3
        if pipeline_phase as usize == Printer::pipeline_emit_with_substitution as usize {
            Debug_.assert_is_defined(&self.printer.ref_(self).maybe_last_substitution(), None);
            next = parenthesizer_rule.ref_(self).call(cast(
                self.printer.ref_(self).maybe_last_substitution(),
                |&node: &Id<Node>| is_expression(node, self),
            ));
            pipeline_phase = self.printer.ref_(self).get_next_pipeline_phase(
                PipelinePhase::Substitution,
                EmitHint::Expression,
                next,
            )?;
            self.printer.ref_(self).set_last_substitution(None);
        }

        if pipeline_phase as usize == Printer::pipeline_emit_with_comments as usize
            || pipeline_phase as usize == Printer::pipeline_emit_with_source_maps as usize
            || pipeline_phase as usize == Printer::pipeline_emit_with_hint as usize
        {
            if is_binary_expression(&next.ref_(self)) {
                return Ok(Some(next));
            }
        }

        self.printer
            .ref_(self)
            .set_current_parenthesizer_rule(Some(parenthesizer_rule));
        pipeline_phase(&self.printer.ref_(self), EmitHint::Expression, next)?;
        Ok(None)
    }
}

impl BinaryExpressionStateMachine for EmitBinaryExpressionStateMachine {
    type TResult = ();
    type TOuterState = ();
    type TState = Rc<RefCell<WorkArea>>;

    fn on_enter(
        &self,
        node: Id<Node>, /*BinaryExpression*/
        mut state: Option<Rc<RefCell<WorkArea>>>,
        _: (),
    ) -> io::Result<Rc<RefCell<WorkArea>>> {
        if let Some(state) = state.as_ref() {
            let mut state = state.borrow_mut();
            state.stack_index += 1;
            state
                .preserve_source_newlines_stack
                .push(self.printer.ref_(self).maybe_preserve_source_newlines());
            state
                .container_pos_stack
                .push(self.printer.ref_(self).container_pos());
            state
                .container_end_stack
                .push(self.printer.ref_(self).container_end());
            state
                .declaration_list_container_end_stack
                .push(self.printer.ref_(self).declaration_list_container_end());
            let emit_comments = self.printer.ref_(self).should_emit_comments(node);
            state.should_emit_comments_stack.push(emit_comments);
            let emit_source_maps = self.printer.ref_(self).should_emit_source_maps(node);
            state.should_emit_source_maps_stack.push(emit_source_maps);
            self.printer.ref_(self).on_before_emit_node(Some(node));
            if emit_comments {
                self.printer.ref_(self).emit_comments_before_node(node);
            }
            if emit_source_maps {
                self.printer.ref_(self).emit_source_maps_before_node(node);
            }
            self.printer.ref_(self).before_emit_node(node);
        } else {
            state = Some(Rc::new(RefCell::new(WorkArea {
                stack_index: 0,
                preserve_source_newlines_stack: vec![None],
                container_pos_stack: vec![-1],
                container_end_stack: vec![-1],
                declaration_list_container_end_stack: vec![-1],
                should_emit_comments_stack: vec![false],
                should_emit_source_maps_stack: vec![false],
            })));
        }
        let state = state.unwrap();
        Ok(state)
    }

    fn on_left(
        &self,
        next: Id<Node>, /*Expression*/
        _work_area: Rc<RefCell<WorkArea>>,
        parent: Id<Node>, /*BinaryExpression*/
    ) -> io::Result<Option<Id<Node /*BinaryExpression*/>>> {
        self.maybe_emit_expression(next, parent, LeftOrRight::Left)
    }

    fn on_operator(
        &self,
        operator_token: Id<Node>, /*BinaryOperatorToken*/
        _state: Rc<RefCell<WorkArea>>,
        node: Id<Node>, /*BinaryExpression*/
    ) -> io::Result<()> {
        let is_comma_operator = operator_token.ref_(self).kind() != SyntaxKind::CommaToken;
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        let lines_before_operator = self.printer.ref_(self).get_lines_between_nodes(
            node,
            node_as_binary_expression.left,
            operator_token,
        );
        let lines_after_operator = self.printer.ref_(self).get_lines_between_nodes(
            node,
            operator_token,
            node_as_binary_expression.right,
        );
        self.printer
            .ref_(self)
            .write_lines_and_indent(lines_before_operator, is_comma_operator);
        self.printer
            .ref_(self)
            .emit_leading_comments_of_position(operator_token.ref_(self).pos());
        self.printer.ref_(self).write_token_node(
            operator_token,
            if operator_token.ref_(self).kind() == SyntaxKind::InKeyword {
                Printer::write_keyword
            } else {
                Printer::write_operator
            },
        );
        self.printer.ref_(self).emit_trailing_comments_of_position(
            operator_token.ref_(self).end(),
            Some(true),
            None,
        );
        self.printer
            .ref_(self)
            .write_lines_and_indent(lines_after_operator, true);

        Ok(())
    }

    fn on_right(
        &self,
        next: Id<Node>, /*Expression*/
        _state: Rc<RefCell<WorkArea>>,
        parent: Id<Node>, /*BinaryExpression*/
    ) -> io::Result<Option<Id<Node /*BinaryExpression*/>>> {
        self.maybe_emit_expression(next, parent, LeftOrRight::Right)
    }

    fn on_exit(
        &self,
        node: Id<Node>, /*BinaryExpression*/
        state: Rc<RefCell<WorkArea>>,
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        let lines_before_operator = self.printer.ref_(self).get_lines_between_nodes(
            node,
            node_as_binary_expression.left,
            node_as_binary_expression.operator_token,
        );
        let lines_after_operator = self.printer.ref_(self).get_lines_between_nodes(
            node,
            node_as_binary_expression.operator_token,
            node_as_binary_expression.right,
        );
        self.printer
            .ref_(self)
            .decrease_indent_if(lines_before_operator != 0, Some(lines_after_operator != 0));
        {
            let mut state = state.borrow_mut();
            if state.stack_index > 0 {
                let saved_preserve_source_newlines =
                    state.preserve_source_newlines_stack.pop().unwrap();
                let saved_container_pos = state.container_pos_stack.pop().unwrap();
                let saved_container_end = state.container_end_stack.pop().unwrap();
                let saved_declaration_list_container_end =
                    state.declaration_list_container_end_stack.pop().unwrap();
                let should_emit_comments = state.should_emit_comments_stack.pop().unwrap();
                let should_emit_source_maps = state.should_emit_source_maps_stack.pop().unwrap();
                self.printer
                    .ref_(self)
                    .after_emit_node(saved_preserve_source_newlines);
                if should_emit_source_maps {
                    self.printer.ref_(self).emit_source_maps_after_node(node);
                }
                if should_emit_comments {
                    self.printer.ref_(self).emit_comments_after_node(
                        node,
                        saved_container_pos,
                        saved_container_end,
                        saved_declaration_list_container_end,
                    );
                }
                self.printer.ref_(self).on_after_emit_node(Some(node));
                state.stack_index -= 1;
            }
        }
        Ok(())
    }

    fn implements_on_left(&self) -> bool {
        true
    }

    fn implements_on_operator(&self) -> bool {
        true
    }

    fn implements_on_right(&self) -> bool {
        true
    }

    fn implements_fold_state(&self) -> bool {
        false
    }
}

impl_has_arena!(EmitBinaryExpressionStateMachine);

struct MaybeEmitExpressionCurrentParenthesizerRule {
    side: LeftOrRight,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
    parent_operator_token_kind: SyntaxKind,
    arena: *const AllArenas,
}

impl MaybeEmitExpressionCurrentParenthesizerRule {
    pub fn new(
        side: LeftOrRight,
        parenthesizer: Id<Box<dyn ParenthesizerRules>>,
        parent_operator_token_kind: SyntaxKind,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            side,
            parenthesizer,
            parent_operator_token_kind,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for MaybeEmitExpressionCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        if self.side == LeftOrRight::Left {
            self.parenthesizer
                .ref_(self)
                .parenthesize_left_side_of_binary(self.parent_operator_token_kind, node)
        } else {
            self.parenthesizer
                .ref_(self)
                .parenthesize_right_side_of_binary(self.parent_operator_token_kind, None, node)
        }
    }
}

impl_has_arena!(MaybeEmitExpressionCurrentParenthesizerRule);

pub(super) struct ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule {
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
    arena: *const AllArenas,
}

impl ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_operand_of_prefix_unary(node)
    }
}

impl_has_arena!(ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule);

pub(super) struct ParenthesizeOperandOfPostfixUnaryCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeOperandOfPostfixUnaryCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for ParenthesizeOperandOfPostfixUnaryCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_operand_of_postfix_unary(node)
    }
}

impl_has_arena!(ParenthesizeOperandOfPostfixUnaryCurrentParenthesizerRule);

pub(super) struct ParenthesizeLeftSideOfAccessCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeLeftSideOfAccessCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for ParenthesizeLeftSideOfAccessCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_left_side_of_access(node)
    }
}

impl_has_arena!(ParenthesizeLeftSideOfAccessCurrentParenthesizerRule);

pub(super) struct ParenthesizeExpressionOfNewCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeExpressionOfNewCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for ParenthesizeExpressionOfNewCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_expression_of_new(node)
    }
}

impl_has_arena!(ParenthesizeExpressionOfNewCurrentParenthesizerRule);

pub(super) struct ParenthesizeMemberOfElementTypeCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeMemberOfElementTypeCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for ParenthesizeMemberOfElementTypeCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_member_of_element_type(node)
    }
}

impl_has_arena!(ParenthesizeMemberOfElementTypeCurrentParenthesizerRule);

pub(super) struct ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_member_of_conditional_type(node)
    }
}

impl_has_arena!(ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule);

pub(super) struct ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_element_type_of_array_type(node)
    }
}

impl_has_arena!(ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule);

pub(super) struct ParenthesizeExpressionOfComputedPropertyNameCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeExpressionOfComputedPropertyNameCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule
    for ParenthesizeExpressionOfComputedPropertyNameCurrentParenthesizerRule
{
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_expression_of_computed_property_name(node)
    }
}

impl_has_arena!(ParenthesizeExpressionOfComputedPropertyNameCurrentParenthesizerRule);

pub(super) struct ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_expression_for_disallowed_comma(node)
    }
}

impl_has_arena!(ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule);

pub(super) struct ParenthesizeExpressionOfExportDefaultCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeExpressionOfExportDefaultCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for ParenthesizeExpressionOfExportDefaultCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_expression_of_export_default(node)
    }
}

impl_has_arena!(ParenthesizeExpressionOfExportDefaultCurrentParenthesizerRule);

pub(super) struct ParenthesizeRightSideOfBinaryCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeRightSideOfBinaryCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for ParenthesizeRightSideOfBinaryCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_right_side_of_binary(SyntaxKind::EqualsToken, None, node)
    }
}

impl_has_arena!(ParenthesizeRightSideOfBinaryCurrentParenthesizerRule);

pub(super) struct ParenthesizeConciseBodyOfArrowFunctionCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeConciseBodyOfArrowFunctionCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule for ParenthesizeConciseBodyOfArrowFunctionCurrentParenthesizerRule {
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_concise_body_of_arrow_function(node)
    }
}

impl_has_arena!(ParenthesizeConciseBodyOfArrowFunctionCurrentParenthesizerRule);

pub(super) struct ParenthesizeExpressionOfExpressionStatementCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeExpressionOfExpressionStatementCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule
    for ParenthesizeExpressionOfExpressionStatementCurrentParenthesizerRule
{
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_expression_of_expression_statement(node)
    }
}

impl_has_arena!(ParenthesizeExpressionOfExpressionStatementCurrentParenthesizerRule);

pub(super) struct ParenthesizeBranchOfConditionalExpressionCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeBranchOfConditionalExpressionCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule
    for ParenthesizeBranchOfConditionalExpressionCurrentParenthesizerRule
{
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_branch_of_conditional_expression(node)
    }
}

impl_has_arena!(ParenthesizeBranchOfConditionalExpressionCurrentParenthesizerRule);

pub(super) struct ParenthesizeConditionOfConditionalExpressionCurrentParenthesizerRule {
    arena: *const AllArenas,
    parenthesizer: Id<Box<dyn ParenthesizerRules>>,
}

impl ParenthesizeConditionOfConditionalExpressionCurrentParenthesizerRule {
    pub fn new(parenthesizer: Id<Box<dyn ParenthesizerRules>>, arena: &impl HasArena) -> Self {
        Self {
            parenthesizer,
            arena: arena.arena(),
        }
    }
}

impl CurrentParenthesizerRule
    for ParenthesizeConditionOfConditionalExpressionCurrentParenthesizerRule
{
    fn call(&self, node: Id<Node>) -> Id<Node> {
        self.parenthesizer
            .ref_(self)
            .parenthesize_condition_of_conditional_expression(node)
    }
}

impl_has_arena!(ParenthesizeConditionOfConditionalExpressionCurrentParenthesizerRule);
