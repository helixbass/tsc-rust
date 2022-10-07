use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::brackets;
use crate::{
    for_each, get_constant_value, get_emit_flags, get_literal_text, get_parse_tree_node, id_text,
    is_access_expression, is_finite, is_identifier, is_json_source_file, is_numeric_literal,
    positions_are_on_same_line, set_text_range_pos_end, skip_partially_emitted_expressions,
    skip_trivia, string_contains, token_to_string, with_synthetic_factory,
    with_synthetic_factory_and_factory, EmitFlags, EmitHint, FunctionLikeDeclarationInterface,
    GetLiteralTextFlags, HasInitializerInterface, HasQuestionTokenInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, ListFormat,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, Printer, ReadonlyTextRange,
    ScriptTarget, SignatureDeclarationInterface, SourceFileLike, SourceFilePrologueInfo,
    SourceMapSource, StringOrNumber, Symbol, SyntaxKind, TokenFlags,
};

impl Printer {
    pub(super) fn emit_call_expression(&self, node: &Node /*CallExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_new_expression(&self, node: &Node /*NewExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_tagged_template_expression(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_type_assertion_expression(&self, node: &Node /*TypeAssertion*/) {
        unimplemented!()
    }

    pub(super) fn emit_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn emit_function_expression(&self, node: &Node /*FunctionExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_arrow_function(&self, node: &Node /*ArrowFunction*/) {
        unimplemented!()
    }

    pub(super) fn emit_delete_expression(&self, node: &Node /*DeleteExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_type_of_expression(&self, node: &Node /*TypeOfExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_void_expression(&self, node: &Node /*VoidExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_await_expression(&self, node: &Node /*AwaitExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_prefix_unary_expression(&self, node: &Node /*PrefixUnaryExpression*/) {
        unimplemented!()
    }

    pub(super) fn emit_postfix_unary_expression(
        &self,
        node: &Node, /*PostfixUnaryExpression*/
    ) {
        unimplemented!()
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

    pub(super) fn decrease_indent_if(&self, value1: bool, value2: bool) {
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

    pub(super) fn make_file_level_optimistic_unique_name(&self, name: &str) -> String {
        unimplemented!()
    }

    pub(super) fn pipeline_emit_with_comments(&self, hint: EmitHint, node: &Node) {
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
