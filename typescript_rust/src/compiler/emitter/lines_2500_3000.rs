use gc::{Finalize, Gc, Trace};
use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::Rc;

use super::PipelinePhase;
use crate::{
    cast, create_binary_expression_trampoline, get_emit_flags, get_parse_tree_node,
    is_binary_expression, is_block, is_expression, is_json_source_file, node_is_synthesized,
    positions_are_on_same_line, skip_trivia, with_synthetic_factory, BaseNodeFactorySynthetic,
    BinaryExpressionStateMachine, BinaryExpressionTrampoline, CurrentParenthesizerRule, Debug_,
    EmitFlags, EmitHint, HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface,
    LeftOrRight, ListFormat, NamedDeclarationInterface, Node, NodeInterface, ParenthesizerRules,
    Printer, ReadonlyTextRange, SignatureDeclarationInterface, SourceFileLike, SyntaxKind,
};

impl Printer {
    pub(super) fn emit_call_expression(&self, node: &Node /*CallExpression*/) {
        let indirect_call = get_emit_flags(node).intersects(EmitFlags::IndirectCall);
        if indirect_call {
            self.write_punctuation("(");
            self.write_literal("0");
            self.write_punctuation(",");
            self.write_space();
        }
        let node_as_call_expression = node.as_call_expression();
        self.emit_expression(
            Some(&*node_as_call_expression.expression),
            Some(Gc::new(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
        if indirect_call {
            self.write_punctuation(")");
        }
        self.emit(node_as_call_expression.question_dot_token.as_deref(), None);
        self.emit_type_arguments(
            node,
            node_as_call_expression.maybe_type_arguments().as_ref(),
        );
        self.emit_expression_list(
            Some(node),
            Some(&node_as_call_expression.arguments),
            ListFormat::CallExpressionArguments,
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer
                            .parenthesize_expression_for_disallowed_comma(synthetic_factory, node)
                    })
                }
            })),
            None,
            None,
        );
    }

    pub(super) fn emit_new_expression(&self, node: &Node /*NewExpression*/) {
        self.emit_token_with_comment(
            SyntaxKind::NewKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_as_new_expression = node.as_new_expression();
        self.emit_expression(
            Some(&*node_as_new_expression.expression),
            Some(Gc::new(Box::new(
                ParenthesizeExpressionOfNewCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
        self.emit_type_arguments(node, node_as_new_expression.maybe_type_arguments().as_ref());
        self.emit_expression_list(
            Some(node),
            node_as_new_expression.arguments.as_ref(),
            ListFormat::NewExpressionArguments,
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer
                            .parenthesize_expression_for_disallowed_comma(synthetic_factory, node)
                    })
                }
            })),
            None,
            None,
        );
    }

    pub(super) fn emit_tagged_template_expression(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) {
        let indirect_call = get_emit_flags(node).intersects(EmitFlags::IndirectCall);
        if indirect_call {
            self.write_punctuation("(");
            self.write_literal("0");
            self.write_punctuation(",");
            self.write_space();
        }
        let node_as_tagged_template_expression = node.as_tagged_template_expression();
        self.emit_expression(
            Some(&*node_as_tagged_template_expression.tag),
            Some(Gc::new(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
        if indirect_call {
            self.write_punctuation(")");
        }
        self.emit_type_arguments(
            node,
            node_as_tagged_template_expression
                .maybe_type_arguments()
                .as_ref(),
        );
        self.write_space();
        self.emit_expression(Some(&*node_as_tagged_template_expression.template), None);
    }

    pub(super) fn emit_type_assertion_expression(&self, node: &Node /*TypeAssertion*/) {
        self.write_punctuation("<");
        let node_as_type_assertion = node.as_type_assertion();
        self.emit(Some(&*node_as_type_assertion.type_), None);
        self.write_punctuation(">");
        self.emit_expression(
            Some(&*node_as_type_assertion.expression),
            Some(Gc::new(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
    }

    pub(super) fn emit_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
    ) {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::OpenParenToken,
            node.pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        let node_as_parenthesized_expression = node.as_parenthesized_expression();
        let indented = self.write_line_separators_and_indent_before(
            &node_as_parenthesized_expression.expression,
            node,
        );
        self.emit_expression(Some(&*node_as_parenthesized_expression.expression), None);
        self.write_line_separators_after(&node_as_parenthesized_expression.expression, node);
        self.decrease_indent_if(indented, None);
    }

    pub(super) fn emit_function_expression(&self, node: &Node /*FunctionExpression*/) {
        self.generate_name_if_needed(node.as_function_expression().maybe_name().as_deref());
        self.emit_function_declaration_or_expression(node);
    }

    pub(super) fn emit_arrow_function(&self, node: &Node /*ArrowFunction*/) {
        self.emit_decorators(node, node.maybe_decorators().as_ref());
        self.emit_modifiers(node, node.maybe_modifiers().as_ref());
        self.emit_signature_and_body(node, |node: &Node| self.emit_arrow_function_head(node));
    }

    pub(super) fn emit_arrow_function_head(&self, node: &Node /*ArrowFunction*/) {
        let node_as_arrow_function = node.as_arrow_function();
        self.emit_type_parameters(
            node,
            node_as_arrow_function.maybe_type_parameters().as_ref(),
        );
        self.emit_parameters_for_arrow(node, &node_as_arrow_function.parameters());
        self.emit_type_annotation(node_as_arrow_function.maybe_type());
        self.write_space();
        self.emit(
            Some(&*node_as_arrow_function.equals_greater_than_token),
            None,
        );
    }

    pub(super) fn emit_delete_expression(&self, node: &Node /*DeleteExpression*/) {
        self.emit_token_with_comment(
            SyntaxKind::DeleteKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            Some(&*node.as_delete_expression().expression),
            Some(Gc::new(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
    }

    pub(super) fn emit_type_of_expression(&self, node: &Node /*TypeOfExpression*/) {
        self.emit_token_with_comment(
            SyntaxKind::TypeOfKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            Some(&*node.as_type_of_expression().expression),
            Some(Gc::new(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
    }

    pub(super) fn emit_void_expression(&self, node: &Node /*VoidExpression*/) {
        self.emit_token_with_comment(
            SyntaxKind::VoidKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            Some(&*node.as_void_expression().expression),
            Some(Gc::new(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
    }

    pub(super) fn emit_await_expression(&self, node: &Node /*AwaitExpression*/) {
        self.emit_token_with_comment(
            SyntaxKind::AwaitKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            Some(&*node.as_await_expression().expression),
            Some(Gc::new(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
    }

    pub(super) fn emit_prefix_unary_expression(&self, node: &Node /*PrefixUnaryExpression*/) {
        let node_as_prefix_unary_expression = node.as_prefix_unary_expression();
        self.write_token_text(
            node_as_prefix_unary_expression.operator,
            |text: &str| self.write_operator(text),
            None,
        );
        if self.should_emit_whitespace_before_operand(node) {
            self.write_space();
        }
        self.emit_expression(
            Some(&*node_as_prefix_unary_expression.operand),
            Some(Gc::new(Box::new(
                ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
    }

    pub(super) fn should_emit_whitespace_before_operand(
        &self,
        node: &Node, /*PrefixUnaryExpression*/
    ) -> bool {
        let node_as_prefix_unary_expression = node.as_prefix_unary_expression();
        let operand = &node_as_prefix_unary_expression.operand;
        operand.kind() == SyntaxKind::PrefixUnaryExpression
            && (node_as_prefix_unary_expression.operator == SyntaxKind::PlusToken
                && matches!(
                    operand.as_prefix_unary_expression().operator,
                    SyntaxKind::PlusToken | SyntaxKind::PlusPlusToken
                )
                || node_as_prefix_unary_expression.operator == SyntaxKind::MinusToken
                    && matches!(
                        operand.as_prefix_unary_expression().operator,
                        SyntaxKind::MinusToken | SyntaxKind::MinusMinusToken
                    ))
    }

    pub(super) fn emit_postfix_unary_expression(
        &self,
        node: &Node, /*PostfixUnaryExpression*/
    ) {
        let node_as_postfix_unary_expression = node.as_postfix_unary_expression();
        self.emit_expression(
            Some(&*node_as_postfix_unary_expression.operand),
            Some(Gc::new(Box::new(
                ParenthesizeOperandOfPostfixUnaryCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
        self.write_token_text(
            node_as_postfix_unary_expression.operator,
            |text: &str| self.write_operator(text),
            None,
        );
    }

    pub(super) fn create_emit_binary_expression(&self) -> EmitBinaryExpression {
        let trampoline = create_binary_expression_trampoline(
            EmitBinaryExpressionStateMachine::new(self.rc_wrapper()),
        );
        EmitBinaryExpression::new(trampoline)
    }

    pub(super) fn emit_conditional_expression(&self, node: &Node /*ConditionalExpression*/) {
        let node_as_conditional_expression = node.as_conditional_expression();
        let lines_before_question = self.get_lines_between_nodes(
            node,
            &node_as_conditional_expression.condition,
            &node_as_conditional_expression.question_token,
        );
        let lines_after_question = self.get_lines_between_nodes(
            node,
            &node_as_conditional_expression.question_token,
            &node_as_conditional_expression.when_true,
        );
        let lines_before_colon = self.get_lines_between_nodes(
            node,
            &node_as_conditional_expression.when_true,
            &node_as_conditional_expression.colon_token,
        );
        let lines_after_colon = self.get_lines_between_nodes(
            node,
            &node_as_conditional_expression.colon_token,
            &node_as_conditional_expression.when_false,
        );

        self.emit_expression(
            Some(&*node_as_conditional_expression.condition),
            Some(Gc::new(Box::new(
                ParenthesizeConditionOfConditionalExpressionCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
        self.write_lines_and_indent(lines_before_question, true);
        self.emit(Some(&*node_as_conditional_expression.question_token), None);
        self.write_lines_and_indent(lines_after_question, true);
        self.emit_expression(
            Some(&*node_as_conditional_expression.when_true),
            Some(Gc::new(Box::new(
                ParenthesizeBranchOfConditionalExpressionCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
        self.decrease_indent_if(lines_before_question != 0, Some(lines_after_question != 0));

        self.write_lines_and_indent(lines_before_colon, true);
        self.emit(Some(&*node_as_conditional_expression.colon_token), None);
        self.write_lines_and_indent(lines_after_colon, true);
        self.emit_expression(
            Some(&*node_as_conditional_expression.when_false),
            Some(Gc::new(Box::new(
                ParenthesizeBranchOfConditionalExpressionCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
        self.decrease_indent_if(lines_before_colon != 0, Some(lines_after_colon != 0));
    }

    pub(super) fn emit_template_expression(&self, node: &Node /*TemplateExpression*/) {
        let node_as_template_expression = node.as_template_expression();
        self.emit(Some(&*node_as_template_expression.head), None);
        self.emit_list(
            Some(node),
            Some(&node_as_template_expression.template_spans),
            ListFormat::TemplateExpressionSpans,
            None,
            None,
            None,
        );
    }

    pub(super) fn emit_yield_expression(&self, node: &Node /*YieldExpression*/) {
        self.emit_token_with_comment(
            SyntaxKind::YieldKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        let node_as_yield_expression = node.as_yield_expression();
        self.emit(node_as_yield_expression.asterisk_token.as_deref(), None);
        self.emit_expression_with_leading_space(
            node_as_yield_expression.expression.as_deref(),
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

    pub(super) fn emit_spread_element(&self, node: &Node /*SpreadElement*/) {
        self.emit_token_with_comment(
            SyntaxKind::DotDotDotToken,
            node.pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_expression(
            Some(&*node.as_spread_element().expression),
            Some(Gc::new(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
    }

    pub(super) fn emit_class_expression(&self, node: &Node /*ClassExpression*/) {
        let node_as_class_expression = node.as_class_expression();
        self.generate_name_if_needed(node_as_class_expression.maybe_name().as_deref());
        self.emit_class_declaration_or_expression(node);
    }

    pub(super) fn emit_expression_with_type_arguments(
        &self,
        node: &Node, /*ExpressionWithTypeArguments*/
    ) {
        let node_as_expression_with_type_arguments = node.as_expression_with_type_arguments();
        self.emit_expression(
            Some(&*node_as_expression_with_type_arguments.expression),
            Some(Gc::new(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
        self.emit_type_arguments(
            node,
            node_as_expression_with_type_arguments
                .maybe_type_arguments()
                .as_ref(),
        );
    }

    pub(super) fn emit_as_expression(&self, node: &Node /*AsExpression*/) {
        let node_as_as_expression = node.as_as_expression();
        self.emit_expression(Some(&*node_as_as_expression.expression), None);
        if let Some(node_type) = node_as_as_expression.maybe_type().as_ref() {
            self.write_space();
            self.write_keyword("as");
            self.write_space();
            self.emit(Some(&**node_type), None);
        }
    }

    pub(super) fn emit_non_null_expression(&self, node: &Node /*NonNullExpression*/) {
        let node_as_non_null_expression = node.as_non_null_expression();
        self.emit_expression(
            Some(&*node_as_non_null_expression.expression),
            Some(Gc::new(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
        self.write_operator("!");
    }

    pub(super) fn emit_meta_property(&self, node: &Node /*MetaProperty*/) {
        let node_as_meta_property = node.as_meta_property();
        self.write_token(
            node_as_meta_property.keyword_token,
            node.pos(),
            |text: &str| self.write_punctuation(text),
            None,
        );
        self.write_punctuation(".");
        self.emit(Some(&*node_as_meta_property.name), None);
    }

    pub(super) fn emit_template_span(&self, node: &Node /*TemplateSpan*/) {
        let node_as_template_span = node.as_template_span();
        self.emit_expression(Some(&*node_as_template_span.expression), None);
        self.emit(Some(&*node_as_template_span.literal), None);
    }

    pub(super) fn emit_block(&self, node: &Node /*Block*/) {
        self.emit_block_statements(
            node,
            node.as_block().multi_line != Some(true) && self.is_empty_block(node),
        );
    }

    pub(super) fn emit_block_statements(
        &self,
        node: &Node, /*BlockLlke*/
        force_single_line: bool,
    ) {
        self.emit_token_with_comment(
            SyntaxKind::OpenBraceToken,
            node.pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        let format = if force_single_line || get_emit_flags(node).intersects(EmitFlags::SingleLine)
        {
            ListFormat::SingleLineBlockStatements
        } else {
            ListFormat::MultiLineBlockStatements
        };
        self.emit_list(
            Some(node),
            Some(node.as_has_statements().statements()),
            format,
            None,
            None,
            None,
        );
        self.emit_token_with_comment(
            SyntaxKind::CloseBraceToken,
            node.as_has_statements().statements().end(),
            |text: &str| self.write_punctuation(text),
            node,
            Some(format.intersects(ListFormat::MultiLine)),
        );
    }

    pub(super) fn emit_variable_statement(&self, node: &Node /*VariableStatement*/) {
        self.emit_modifiers(node, node.maybe_modifiers().as_ref());
        self.emit(Some(&*node.as_variable_statement().declaration_list), None);
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_empty_statement(&self, is_embedded_statement: bool) {
        if is_embedded_statement {
            self.write_punctuation(";");
        } else {
            self.write_trailing_semicolon();
        }
    }

    pub(super) fn emit_expression_statement(&self, node: &Node /*ExpressionStatement*/) {
        let node_as_expression_statement = node.as_expression_statement();
        self.emit_expression(
            Some(&*node_as_expression_statement.expression),
            Some(Gc::new(Box::new(
                ParenthesizeExpressionOfExpressionStatementCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
        if !is_json_source_file(&self.current_source_file())
            || node_is_synthesized(&*node_as_expression_statement.expression)
        {
            self.write_trailing_semicolon();
        }
    }

    pub(super) fn emit_if_statement(&self, node: &Node /*IfStatement*/) {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::IfKeyword,
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
        let node_as_if_statement = node.as_if_statement();
        self.emit_expression(Some(&*node_as_if_statement.expression), None);
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node_as_if_statement.expression.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_embedded_statement(node, &node_as_if_statement.then_statement);
        if let Some(node_else_statement) = node_as_if_statement.else_statement.as_ref() {
            self.write_line_or_space(
                node,
                &node_as_if_statement.then_statement,
                node_else_statement,
            );
            self.emit_token_with_comment(
                SyntaxKind::ElseKeyword,
                node_as_if_statement.then_statement.end(),
                |text: &str| self.write_keyword(text),
                node,
                None,
            );
            if node_else_statement.kind() == SyntaxKind::IfStatement {
                self.write_space();
                self.emit(Some(&**node_else_statement), None);
            } else {
                self.emit_embedded_statement(node, node_else_statement);
            }
        }
    }

    pub(super) fn emit_while_clause(
        &self,
        node: &Node, /*WhileStatement | DoStatement*/
        start_pos: isize,
    ) {
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
        self.emit_expression(node.as_has_expression().maybe_expression().as_deref(), None);
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node.as_has_expression().expression().end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
    }

    pub(super) fn emit_do_statement(&self, node: &Node /*DoStatement*/) {
        self.emit_token_with_comment(
            SyntaxKind::DoKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        let node_as_do_statement = node.as_do_statement();
        self.emit_embedded_statement(node, &node_as_do_statement.statement);
        if is_block(&node_as_do_statement.statement)
            && self.maybe_preserve_source_newlines() != Some(true)
        {
            self.write_space();
        } else {
            self.write_line_or_space(
                node,
                &node_as_do_statement.statement,
                &node_as_do_statement.expression,
            );
        }

        self.emit_while_clause(node, node_as_do_statement.statement.end());
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_while_statement(&self, node: &Node /*WhileStatement*/) {
        self.emit_while_clause(node, node.pos());
        self.emit_embedded_statement(node, &node.as_while_statement().statement);
    }

    pub(super) fn emit_for_statement(&self, node: &Node /*ForStatement*/) {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::ForKeyword,
            node.pos(),
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
        let node_as_for_statement = node.as_for_statement();
        self.emit_for_binding(node_as_for_statement.initializer.as_deref());
        pos = self.emit_token_with_comment(
            SyntaxKind::SemicolonToken,
            node_as_for_statement
                .initializer
                .as_ref()
                .map_or(pos, |node_initializer| node_initializer.end()),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_expression_with_leading_space(node_as_for_statement.condition.as_deref(), None);
        pos = self.emit_token_with_comment(
            SyntaxKind::SemicolonToken,
            node_as_for_statement
                .condition
                .as_ref()
                .map_or(pos, |node_condition| node_condition.end()),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_expression_with_leading_space(node_as_for_statement.incrementor.as_deref(), None);
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node_as_for_statement
                .incrementor
                .as_ref()
                .map_or(pos, |node_incrementor| node_incrementor.end()),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_embedded_statement(node, &node_as_for_statement.statement);
    }

    pub(super) fn emit_for_in_statement(&self, node: &Node /*ForInStatement*/) {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::ForKeyword,
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
        let node_as_for_in_statement = node.as_for_in_statement();
        self.emit_for_binding(Some(&*node_as_for_in_statement.initializer));
        self.write_space();
        self.emit_token_with_comment(
            SyntaxKind::InKeyword,
            node_as_for_in_statement.initializer.end(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(Some(&*node_as_for_in_statement.expression), None);
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node_as_for_in_statement.expression.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_embedded_statement(node, &node_as_for_in_statement.statement);
    }

    pub(super) fn emit_for_of_statement(&self, node: &Node /*ForOfStatement*/) {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::ForKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_as_for_of_statement = node.as_for_of_statement();
        self.emit_with_trailing_space(node_as_for_of_statement.await_modifier.as_deref());
        self.emit_token_with_comment(
            SyntaxKind::OpenParenToken,
            open_paren_pos,
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_for_binding(Some(&*node_as_for_of_statement.initializer));
        self.write_space();
        self.emit_token_with_comment(
            SyntaxKind::OfKeyword,
            node_as_for_of_statement.initializer.end(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(Some(&*node_as_for_of_statement.expression), None);
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node_as_for_of_statement.expression.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_embedded_statement(node, &node_as_for_of_statement.statement);
    }

    pub(super) fn emit_for_binding(
        &self,
        node: Option<&Node /*VariableDeclarationList | Expression*/>,
    ) {
        if let Some(node) = node {
            if node.kind() == SyntaxKind::VariableDeclarationList {
                self.emit(Some(node), None);
            } else {
                self.emit_expression(Some(node), None);
            }
        }
    }

    pub(super) fn emit_continue_statement(&self, node: &Node /*ContinueStatement*/) {
        self.emit_token_with_comment(
            SyntaxKind::ContinueKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit_with_leading_space(node.as_continue_statement().label.as_deref());
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_break_statement(&self, node: &Node /*BreakStatement*/) {
        self.emit_token_with_comment(
            SyntaxKind::BreakKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit_with_leading_space(node.as_break_statement().label.as_deref());
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_token_with_comment<TWriter: FnMut(&str)>(
        &self,
        token: SyntaxKind,
        mut pos: isize,
        writer: TWriter,
        context_node: &Node,
        indent_leading: Option<bool>,
    ) -> isize {
        let node = get_parse_tree_node(Some(context_node), Option::<fn(&Node) -> bool>::None);
        let is_similar_node = matches!(
            node.as_ref(),
            Some(node) if node.kind() == context_node.kind()
        );
        let start_pos = pos;
        if is_similar_node {
            if let Some(current_source_file) = self.maybe_current_source_file().as_ref() {
                pos = skip_trivia(
                    &current_source_file.as_source_file().text_as_chars(),
                    pos,
                    None,
                    None,
                    None,
                );
            }
        }
        if is_similar_node && context_node.pos() != start_pos {
            let needs_indent = indent_leading == Some(true)
                && matches!(
                    self.maybe_current_source_file().as_ref(),
                    Some(current_source_file) if !positions_are_on_same_line(
                        start_pos.try_into().unwrap(),
                        pos.try_into().unwrap(),
                        current_source_file,
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
        if is_similar_node && context_node.end() != pos {
            let is_jsx_expr_context = context_node.kind() == SyntaxKind::JsxExpression;
            self.emit_trailing_comments_of_position(
                pos,
                Some(!is_jsx_expr_context),
                Some(is_jsx_expr_context),
            );
        }
        pos
    }

    pub(super) fn emit_return_statement(&self, node: &Node /*ReturnStatement*/) {
        self.emit_token_with_comment(
            SyntaxKind::ReturnKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit_expression_with_leading_space(
            node.as_return_statement().expression.as_deref(),
            None,
        );
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_with_statement(&self, node: &Node /*WithStatement*/) {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::WithKeyword,
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
        let node_as_with_statement = node.as_with_statement();
        self.emit_expression(Some(&*node_as_with_statement.expression), None);
        self.emit_token_with_comment(
            SyntaxKind::CloseParenToken,
            node_as_with_statement.expression.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_embedded_statement(node, &node_as_with_statement.statement);
    }
}

#[derive(Trace, Finalize)]
pub struct EmitBinaryExpression {
    trampoline: BinaryExpressionTrampoline<EmitBinaryExpressionStateMachine>,
}

impl EmitBinaryExpression {
    pub fn new(trampoline: BinaryExpressionTrampoline<EmitBinaryExpressionStateMachine>) -> Self {
        Self { trampoline }
    }

    pub fn call(&self, node: &Node /*BinaryExpression*/) {
        self.trampoline.call(node, ());
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

#[derive(Trace, Finalize)]
pub struct EmitBinaryExpressionStateMachine {
    printer: Gc<Printer>,
}

impl EmitBinaryExpressionStateMachine {
    pub fn new(printer: Gc<Printer>) -> Self {
        Self { printer }
    }

    fn maybe_emit_expression(
        &self,
        next: &Node,   /*Expression*/
        parent: &Node, /*BinaryExpression*/
        side: LeftOrRight,
    ) -> Option<Gc<Node>> {
        let parenthesizer_rule: Gc<Box<dyn CurrentParenthesizerRule>> =
            Gc::new(Box::new(MaybeEmitExpressionCurrentParenthesizerRule::new(
                side,
                self.printer.parenthesizer(),
                parent.as_binary_expression().operator_token.kind(),
            )));

        let mut pipeline_phase = self.printer.get_pipeline_phase(
            PipelinePhase::Notification,
            EmitHint::Expression,
            next,
        );
        let mut next = next.node_wrapper();
        // per https://users.rust-lang.org/t/compare-function-pointers-for-equality/52339/3
        if pipeline_phase as usize == Printer::pipeline_emit_with_substitution as usize {
            Debug_.assert_is_defined(&self.printer.maybe_last_substitution(), None);
            next = parenthesizer_rule.call(&*cast(
                self.printer.maybe_last_substitution(),
                |node: &Gc<Node>| is_expression(node),
            ));
            pipeline_phase = self.printer.get_next_pipeline_phase(
                PipelinePhase::Substitution,
                EmitHint::Expression,
                &next,
            );
            self.printer.set_last_substitution(None);
        }

        if pipeline_phase as usize == Printer::pipeline_emit_with_comments as usize
            || pipeline_phase as usize == Printer::pipeline_emit_with_source_maps as usize
            || pipeline_phase as usize == Printer::pipeline_emit_with_hint as usize
        {
            if is_binary_expression(&next) {
                return Some(next);
            }
        }

        self.printer
            .set_current_parenthesizer_rule(Some(parenthesizer_rule));
        pipeline_phase(&self.printer, EmitHint::Expression, &next);
        None
    }
}

impl BinaryExpressionStateMachine for EmitBinaryExpressionStateMachine {
    type TResult = ();
    type TOuterState = ();
    type TState = Rc<RefCell<WorkArea>>;

    fn on_enter(
        &self,
        node: &Node, /*BinaryExpression*/
        mut state: Option<Rc<RefCell<WorkArea>>>,
        _: (),
    ) -> Rc<RefCell<WorkArea>> {
        if let Some(state) = state.as_ref() {
            let mut state = state.borrow_mut();
            state.stack_index += 1;
            state
                .preserve_source_newlines_stack
                .push(self.printer.maybe_preserve_source_newlines());
            state.container_pos_stack.push(self.printer.container_pos());
            state.container_end_stack.push(self.printer.container_end());
            state
                .declaration_list_container_end_stack
                .push(self.printer.declaration_list_container_end());
            let emit_comments = self.printer.should_emit_comments(node);
            state.should_emit_comments_stack.push(emit_comments);
            let emit_source_maps = self.printer.should_emit_source_maps(node);
            state.should_emit_source_maps_stack.push(emit_source_maps);
            self.printer.on_before_emit_node(Some(node));
            if emit_comments {
                self.printer.emit_comments_before_node(node);
            }
            if emit_source_maps {
                self.printer.emit_source_maps_before_node(node);
            }
            self.printer.before_emit_node(node);
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
        state
    }

    fn on_left(
        &self,
        next: &Node, /*Expression*/
        _work_area: Rc<RefCell<WorkArea>>,
        parent: &Node, /*BinaryExpression*/
    ) -> Option<Gc<Node /*BinaryExpression*/>> {
        self.maybe_emit_expression(next, parent, LeftOrRight::Left)
    }

    fn on_operator(
        &self,
        operator_token: &Node, /*BinaryOperatorToken*/
        _state: Rc<RefCell<WorkArea>>,
        node: &Node, /*BinaryExpression*/
    ) {
        let is_comma_operator = operator_token.kind() != SyntaxKind::CommaToken;
        let node_as_binary_expression = node.as_binary_expression();
        let lines_before_operator = self.printer.get_lines_between_nodes(
            node,
            &node_as_binary_expression.left,
            operator_token,
        );
        let lines_after_operator = self.printer.get_lines_between_nodes(
            node,
            operator_token,
            &node_as_binary_expression.right,
        );
        self.printer
            .write_lines_and_indent(lines_before_operator, is_comma_operator);
        self.printer
            .emit_leading_comments_of_position(operator_token.pos());
        self.printer.write_token_node(
            operator_token,
            if operator_token.kind() == SyntaxKind::InKeyword {
                Printer::write_keyword
            } else {
                Printer::write_operator
            },
        );
        self.printer
            .emit_trailing_comments_of_position(operator_token.end(), Some(true), None);
        self.printer
            .write_lines_and_indent(lines_after_operator, true);
    }

    fn on_right(
        &self,
        next: &Node, /*Expression*/
        _state: Rc<RefCell<WorkArea>>,
        parent: &Node, /*BinaryExpression*/
    ) -> Option<Gc<Node /*BinaryExpression*/>> {
        self.maybe_emit_expression(next, parent, LeftOrRight::Right)
    }

    fn on_exit(&self, node: &Node /*BinaryExpression*/, state: Rc<RefCell<WorkArea>>) -> () {
        let node_as_binary_expression = node.as_binary_expression();
        let lines_before_operator = self.printer.get_lines_between_nodes(
            node,
            &node_as_binary_expression.left,
            &node_as_binary_expression.operator_token,
        );
        let lines_after_operator = self.printer.get_lines_between_nodes(
            node,
            &node_as_binary_expression.operator_token,
            &node_as_binary_expression.right,
        );
        self.printer
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
                self.printer.after_emit_node(saved_preserve_source_newlines);
                if should_emit_source_maps {
                    self.printer.emit_source_maps_after_node(node);
                }
                if should_emit_comments {
                    self.printer.emit_comments_after_node(
                        node,
                        saved_container_pos,
                        saved_container_end,
                        saved_declaration_list_container_end,
                    );
                }
                self.printer.on_after_emit_node(Some(node));
                state.stack_index -= 1;
            }
        }
        ()
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

#[derive(Trace, Finalize)]
struct MaybeEmitExpressionCurrentParenthesizerRule {
    #[unsafe_ignore_trace]
    side: LeftOrRight,
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
    #[unsafe_ignore_trace]
    parent_operator_token_kind: SyntaxKind,
}

impl MaybeEmitExpressionCurrentParenthesizerRule {
    pub fn new(
        side: LeftOrRight,
        parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
        parent_operator_token_kind: SyntaxKind,
    ) -> Self {
        Self {
            side,
            parenthesizer,
            parent_operator_token_kind,
        }
    }
}

impl CurrentParenthesizerRule for MaybeEmitExpressionCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        if self.side == LeftOrRight::Left {
            with_synthetic_factory(|synthetic_factory| {
                self.parenthesizer.parenthesize_left_side_of_binary(
                    synthetic_factory,
                    self.parent_operator_token_kind,
                    node,
                )
            })
        } else {
            with_synthetic_factory(|synthetic_factory| {
                self.parenthesizer.parenthesize_right_side_of_binary(
                    synthetic_factory,
                    self.parent_operator_token_kind,
                    None,
                    node,
                )
            })
        }
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule for ParenthesizeOperandOfPrefixUnaryCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_operand_of_prefix_unary(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeOperandOfPostfixUnaryCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeOperandOfPostfixUnaryCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule for ParenthesizeOperandOfPostfixUnaryCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_operand_of_postfix_unary(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeLeftSideOfAccessCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeLeftSideOfAccessCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule for ParenthesizeLeftSideOfAccessCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_left_side_of_access(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeExpressionOfNewCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeExpressionOfNewCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule for ParenthesizeExpressionOfNewCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_expression_of_new(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeMemberOfElementTypeCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeMemberOfElementTypeCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule for ParenthesizeMemberOfElementTypeCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_member_of_element_type(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule for ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_member_of_conditional_type(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule for ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_element_type_of_array_type(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeExpressionOfComputedPropertyNameCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeExpressionOfComputedPropertyNameCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule
    for ParenthesizeExpressionOfComputedPropertyNameCurrentParenthesizerRule
{
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_expression_of_computed_property_name(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule for ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_expression_for_disallowed_comma(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeExpressionOfExportDefaultCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeExpressionOfExportDefaultCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule for ParenthesizeExpressionOfExportDefaultCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_expression_of_export_default(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeRightSideOfBinaryCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeRightSideOfBinaryCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule for ParenthesizeRightSideOfBinaryCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer.parenthesize_right_side_of_binary(
                synthetic_factory,
                SyntaxKind::EqualsToken,
                None,
                node,
            )
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeConciseBodyOfArrowFunctionCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeConciseBodyOfArrowFunctionCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule for ParenthesizeConciseBodyOfArrowFunctionCurrentParenthesizerRule {
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_concise_body_of_arrow_function(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeExpressionOfExpressionStatementCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeExpressionOfExpressionStatementCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule
    for ParenthesizeExpressionOfExpressionStatementCurrentParenthesizerRule
{
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_expression_of_expression_statement(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeBranchOfConditionalExpressionCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeBranchOfConditionalExpressionCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule
    for ParenthesizeBranchOfConditionalExpressionCurrentParenthesizerRule
{
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_branch_of_conditional_expression(synthetic_factory, node)
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ParenthesizeConditionOfConditionalExpressionCurrentParenthesizerRule {
    parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
}

impl ParenthesizeConditionOfConditionalExpressionCurrentParenthesizerRule {
    pub fn new(parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>) -> Self {
        Self { parenthesizer }
    }
}

impl CurrentParenthesizerRule
    for ParenthesizeConditionOfConditionalExpressionCurrentParenthesizerRule
{
    fn call(&self, node: &Node) -> Gc<Node> {
        with_synthetic_factory(|synthetic_factory| {
            self.parenthesizer
                .parenthesize_condition_of_conditional_expression(synthetic_factory, node)
        })
    }
}
