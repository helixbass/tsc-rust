use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::{brackets, PipelinePhase};
use crate::{
    cast, create_binary_expression_trampoline, get_emit_flags, get_literal_text,
    get_parse_tree_node, id_text, is_binary_expression, is_expression, is_identifier,
    positions_are_on_same_line, skip_trivia, token_to_string, with_synthetic_factory,
    BinaryExpressionStateMachine, BinaryExpressionTrampoline, Debug_, EmitFlags, EmitHint,
    GetLiteralTextFlags, HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface,
    LeftOrRight, ListFormat, NamedDeclarationInterface, Node, NodeArray, NodeInterface, Printer,
    ReadonlyTextRange, SignatureDeclarationInterface, SourceFileLike, SourceFilePrologueInfo,
    SourceMapSource, Symbol, SyntaxKind,
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
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_left_side_of_access(synthetic_factory, node)
                    })
                }
            })),
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
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_expression_of_new(synthetic_factory, node)
                    })
                }
            })),
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
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_left_side_of_access(synthetic_factory, node)
                    })
                }
            })),
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
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_operand_of_prefix_unary(synthetic_factory, node)
                    })
                }
            })),
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
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_operand_of_prefix_unary(synthetic_factory, node)
                    })
                }
            })),
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
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_operand_of_prefix_unary(synthetic_factory, node)
                    })
                }
            })),
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
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_operand_of_prefix_unary(synthetic_factory, node)
                    })
                }
            })),
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
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_operand_of_prefix_unary(synthetic_factory, node)
                    })
                }
            })),
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
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_operand_of_prefix_unary(synthetic_factory, node)
                    })
                }
            })),
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
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_operand_of_postfix_unary(synthetic_factory, node)
                    })
                }
            })),
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
        unimplemented!()
    }

    pub(super) fn emit_template_expression(&self, node: &Node /*TemplateExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_yield_expression(&self, node: &Node /*YieldExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_spread_element(&self, node: &Node /*SpreadElement*/) {
        unimplemented!()
    }

    pub(super) fn emit_class_expression(&self, node: &Node /*ClassExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_expression_with_type_arguments(
        &self,
        node: &Node, /*ExpressionWithTypeArguments*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_as_expression(&self, node: &Node /*AsExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_non_null_expression(&self, node: &Node /*NonNullExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_meta_property(&self, node: &Node /*MetaProperty*/) {
        unimplemented!()
    }

    pub(super) fn emit_template_span(&self, node: &Node /*TemplateSpan*/) {
        unimplemented!()
    }

    pub(super) fn emit_block(&self, node: &Node /*Block*/) {
        unimplemented!()
    }

    pub(super) fn emit_variable_statement(&self, node: &Node /*VariableStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_empty_statement(&self, is_embedded_statement: bool) {
        unimplemented!()
    }

    pub(super) fn emit_expression_statement(&self, node: &Node /*ExpressionStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_if_statement(&self, node: &Node /*IfStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_do_statement(&self, node: &Node /*DoStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_while_statement(&self, node: &Node /*WhileStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_for_statement(&self, node: &Node /*ForStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_for_in_statement(&self, node: &Node /*ForInStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_for_of_statement(&self, node: &Node /*ForOfStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_continue_statement(&self, node: &Node /*ContinueStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_break_statement(&self, node: &Node /*BreakStatement*/) {
        unimplemented!()
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
        unimplemented!()
    }

    pub(super) fn emit_with_statement(&self, node: &Node /*WithStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_switch_statement(&self, node: &Node /*SwitchStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_labeled_statement(&self, node: &Node /*LabeledStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_throw_statement(&self, node: &Node /*ThrowStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_try_statement(&self, node: &Node /*TryStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_debugger_statement(&self, node: &Node /*DebuggerStatement*/) {
        unimplemented!()
    }

    pub(super) fn emit_variable_declaration(&self, node: &Node /*VariableDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn emit_variable_declaration_list(
        &self,
        node: &Node, /*VariableDeclarationList*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn emit_function_declaration_or_expression(
        &self,
        node: &Node, /*FunctionDeclaration | FunctionExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_signature_and_body<TEmitSignatureHead: FnMut(&Node)>(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
        mut emit_signature_head: TEmitSignatureHead,
    ) {
        unimplemented!()
    }

    pub(super) fn emit_signature_head(
        &self,
        node: &Node, /*FunctionDeclaration | FunctionExpression | MethodDeclaration | AccessorDeclaration | ConstructorDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_block_function_body(&self, body: &Node /*Block*/) {
        unimplemented!()
    }

    pub(super) fn emit_class_declaration(&self, node: &Node /*ClassDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn emit_interface_declaration(&self, node: &Node /*InterfaceDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn emit_type_alias_declaration(&self, node: &Node /*TypeAliasDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn emit_enum_declaration(&self, node: &Node /*EnumDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn emit_module_declaration(&self, node: &Node /*ModuleDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn emit_module_block(&self, node: &Node /*ModuleBlock*/) {
        unimplemented!()
    }

    pub(super) fn emit_case_block(&self, node: &Node /*CaseBlock*/) {
        unimplemented!()
    }

    pub(super) fn emit_import_equals_declaration(
        &self,
        node: &Node, /*ImportEqualsDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_import_declaration(&self, node: &Node /*ImportDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn emit_import_clause(&self, node: &Node /*ImportClause*/) {
        unimplemented!()
    }

    pub(super) fn emit_namespace_import(&self, node: &Node /*NamespaceImport*/) {
        unimplemented!()
    }

    pub(super) fn emit_named_imports(&self, node: &Node /*NamedImports*/) {
        unimplemented!()
    }

    pub(super) fn emit_import_specifier(&self, node: &Node /*ImportSpecifier*/) {
        unimplemented!()
    }

    pub(super) fn emit_export_assignment(&self, node: &Node /*ExportAssignment*/) {
        unimplemented!()
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

pub struct EmitBinaryExpressionStateMachine {
    printer: Rc<Printer>,
}

impl EmitBinaryExpressionStateMachine {
    pub fn new(printer: Rc<Printer>) -> Self {
        Self { printer }
    }

    fn maybe_emit_expression(
        &self,
        next: &Node,   /*Expression*/
        parent: &Node, /*BinaryExpression*/
        side: LeftOrRight,
    ) -> Option<Rc<Node>> {
        let parenthesizer_rule: Rc<dyn Fn(&Node) -> Rc<Node>> = if side == LeftOrRight::Left {
            Rc::new({
                let parenthesizer = self.printer.parenthesizer();
                let parent_operator_token_kind =
                    parent.as_binary_expression().operator_token.kind();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_left_side_of_binary(
                            synthetic_factory,
                            parent_operator_token_kind,
                            node,
                        )
                    })
                }
            })
        } else {
            Rc::new({
                let parenthesizer = self.printer.parenthesizer();
                let parent_operator_token_kind =
                    parent.as_binary_expression().operator_token.kind();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_right_side_of_binary(
                            synthetic_factory,
                            parent_operator_token_kind,
                            None,
                            node,
                        )
                    })
                }
            })
        };

        let mut pipeline_phase = self.printer.get_pipeline_phase(
            PipelinePhase::Notification,
            EmitHint::Expression,
            next,
        );
        let mut next = next.node_wrapper();
        // per https://users.rust-lang.org/t/compare-function-pointers-for-equality/52339/3
        if pipeline_phase as usize == Printer::pipeline_emit_with_substitution as usize {
            Debug_.assert_is_defined(&self.printer.maybe_last_substitution(), None);
            next = parenthesizer_rule(&*cast(
                self.printer.maybe_last_substitution(),
                |node: &Rc<Node>| is_expression(node),
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
    ) -> Option<Rc<Node /*BinaryExpression*/>> {
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
    ) -> Option<Rc<Node /*BinaryExpression*/>> {
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
