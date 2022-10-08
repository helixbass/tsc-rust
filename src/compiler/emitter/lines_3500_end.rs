use regex::Regex;
use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::brackets;
use crate::{
    for_each_leading_comment_range, for_each_trailing_comment_range, get_comment_range,
    get_emit_flags, get_line_and_character_of_position, get_literal_text,
    get_text_of_jsdoc_comment, id_text, is_identifier, is_jsx_closing_element,
    is_jsx_opening_element, is_prologue_directive, is_unparsed_source, node_is_synthesized,
    range_start_positions_are_on_same_line, token_to_string, with_factory, with_synthetic_factory,
    EmitFlags, EmitHint, FileReference, GetLiteralTextFlags, HasInitializerInterface,
    HasTypeArgumentsInterface, HasTypeParametersInterface, JSDocTagInterface,
    JSDocTypeLikeTagInterface, ListFormat, LiteralLikeNodeInterface, NamedDeclarationInterface,
    Node, NodeArray, NodeInterface, Printer, ReadonlyTextRange, SourceFileLike,
    SourceFilePrologueInfo, SourceMapSource, StrOrNodeArrayRef, Symbol, SyntaxKind, TextRange,
};

impl Printer {
    pub(super) fn emit_jsx_element(&self, node: &Node /*JsxElement*/) {
        let node_as_jsx_element = node.as_jsx_element();
        self.emit(Some(&*node_as_jsx_element.opening_element), None);
        self.emit_list(
            Some(node),
            Some(&node_as_jsx_element.children),
            ListFormat::JsxElementOrFragmentChildren,
            None,
            None,
            None,
        );
        self.emit(Some(&*node_as_jsx_element.closing_element), None);
    }

    pub(super) fn emit_jsx_self_closing_element(&self, node: &Node /*JsxSelfClosingElement*/) {
        self.write_punctuation("<");
        let node_as_jsx_self_closing_element = node.as_jsx_self_closing_element();
        self.emit_jsx_tag_name(&node_as_jsx_self_closing_element.tag_name);
        self.emit_type_arguments(
            node,
            node_as_jsx_self_closing_element
                .maybe_type_arguments()
                .as_ref(),
        );
        self.write_space();
        self.emit(Some(&*node_as_jsx_self_closing_element.attributes), None);
        self.write_punctuation("/>");
    }

    pub(super) fn emit_jsx_fragment(&self, node: &Node /*JsxFragment*/) {
        let node_as_jsx_fragment = node.as_jsx_fragment();
        self.emit(Some(&*node_as_jsx_fragment.opening_fragment), None);
        self.emit_list(
            Some(node),
            Some(&node_as_jsx_fragment.children),
            ListFormat::JsxElementOrFragmentChildren,
            None,
            None,
            None,
        );
        self.emit(Some(&*node_as_jsx_fragment.closing_fragment), None);
    }

    pub(super) fn emit_jsx_opening_element_or_fragment(
        &self,
        node: &Node, /*JsxOpeningElement | JsxOpeningFragment*/
    ) {
        self.write_punctuation("<");

        if is_jsx_opening_element(node) {
            let node_as_jsx_opening_element = node.as_jsx_opening_element();
            let indented = self.write_line_separators_and_indent_before(
                &node_as_jsx_opening_element.tag_name,
                node,
            );
            self.emit_jsx_tag_name(&node_as_jsx_opening_element.tag_name);
            self.emit_type_arguments(
                node,
                node_as_jsx_opening_element.maybe_type_arguments().as_ref(),
            );
            if
            /*node.attributes.properties &&*/
            !node_as_jsx_opening_element
                .attributes
                .as_jsx_attributes()
                .properties
                .is_empty()
            {
                self.write_space();
            }
            self.emit(Some(&*node_as_jsx_opening_element.attributes), None);
            self.write_line_separators_after(&node_as_jsx_opening_element.attributes, node);
            self.decrease_indent_if(indented, None);
        }

        self.write_punctuation(">");
    }

    pub(super) fn emit_jsx_text(&self, node: &Node /*JsxText*/) {
        self.writer().write_literal(&node.as_jsx_text().text());
    }

    pub(super) fn emit_jsx_closing_element_or_fragment(
        &self,
        node: &Node, /*JsxClosingElement | JsxClosingFragment*/
    ) {
        self.write_punctuation("</");
        if is_jsx_closing_element(node) {
            self.emit_jsx_tag_name(&node.as_jsx_closing_element().tag_name);
        }
        self.write_punctuation(">");
    }

    pub(super) fn emit_jsx_attributes(&self, node: &Node /*JsxAttributes*/) {
        self.emit_list(
            Some(node),
            Some(&node.as_jsx_attributes().properties),
            ListFormat::JsxElementAttributes,
            None,
            None,
            None,
        );
    }

    pub(super) fn emit_jsx_attribute(&self, node: &Node /*JsxAttribute*/) {
        let node_as_jsx_attribute = node.as_jsx_attribute();
        self.emit(Some(&*node_as_jsx_attribute.name), None);
        self.emit_node_with_prefix(
            "=",
            |text: &str| self.write_punctuation(text),
            node_as_jsx_attribute.initializer.as_deref(),
            |node: &Node| self.emit_jsx_attribute_value(node),
        );
    }

    pub(super) fn emit_jsx_spread_attribute(&self, node: &Node /*JsxSpreadAttribute*/) {
        self.write_punctuation("{...");
        self.emit_expression(Some(&*node.as_jsx_spread_attribute().expression), None);
        self.write_punctuation("}");
    }

    pub(super) fn has_trailing_comments_at_position(&self, pos: isize) -> bool {
        let mut result = false;

        let current_source_file = self.maybe_current_source_file();
        let default_text: Option<Vec<char>> = if current_source_file.is_some() {
            None
        } else {
            Some(vec![])
        };
        for_each_trailing_comment_range(
            current_source_file
                .as_ref()
                .map(|current_source_file| current_source_file.as_source_file().text_as_chars())
                .as_deref()
                .unwrap_or_else(|| default_text.as_ref().unwrap()),
            (pos + 1).try_into().unwrap(),
            |_, _, _, _, _| -> () {
                result = true;
                ()
            },
            &(),
        );
        result
    }

    pub(super) fn has_leading_comments_at_position(&self, pos: isize) -> bool {
        let mut result = false;

        let current_source_file = self.maybe_current_source_file();
        let default_text: Option<Vec<char>> = if current_source_file.is_some() {
            None
        } else {
            Some(vec![])
        };
        for_each_leading_comment_range(
            current_source_file
                .as_ref()
                .map(|current_source_file| current_source_file.as_source_file().text_as_chars())
                .as_deref()
                .unwrap_or_else(|| default_text.as_ref().unwrap()),
            (pos + 1).try_into().unwrap(),
            |_, _, _, _, _| -> () {
                result = true;
                ()
            },
            &(),
        );
        result
    }

    pub(super) fn has_comments_at_position(&self, pos: isize) -> bool {
        self.has_trailing_comments_at_position(pos) || self.has_leading_comments_at_position(pos)
    }

    pub(super) fn emit_jsx_expression(&self, node: &Node /*JsxExpression*/) {
        let node_as_jsx_expression = node.as_jsx_expression();
        if node_as_jsx_expression.expression.is_some()
            || !self.comments_disabled()
                && !node_is_synthesized(node)
                && self.has_comments_at_position(node.pos())
        {
            let is_multiline = matches!(
                self.maybe_current_source_file().as_ref(),
                Some(current_source_file) if !node_is_synthesized(node) &&
                    get_line_and_character_of_position(
                        current_source_file.as_source_file(),
                        node.pos().try_into().unwrap(),
                    ).line !=
                    get_line_and_character_of_position(
                        current_source_file.as_source_file(),
                        node.end().try_into().unwrap(),
                    ).line
            );
            if is_multiline {
                self.writer().increase_indent();
            }
            let end = self.emit_token_with_comment(
                SyntaxKind::OpenBraceToken,
                node.pos(),
                |text: &str| self.write_punctuation(text),
                node,
                None,
            );
            self.emit(node_as_jsx_expression.dot_dot_dot_token.as_deref(), None);
            self.emit_expression(node_as_jsx_expression.expression.as_deref(), None);
            self.emit_token_with_comment(
                SyntaxKind::CloseBraceToken,
                node_as_jsx_expression
                    .expression
                    .as_ref()
                    .map_or(end, |node_expression| node_expression.end()),
                |text: &str| self.write_punctuation(text),
                node,
                None,
            );
            if is_multiline {
                self.writer().decrease_indent();
            }
        }
    }

    pub(super) fn emit_jsx_tag_name(&self, node: &Node /*JsxTagNameExpression*/) {
        if node.kind() == SyntaxKind::Identifier {
            self.emit_expression(Some(node), None);
        } else {
            self.emit(Some(node), None);
        }
    }

    pub(super) fn emit_case_clause(&self, node: &Node /*CaseClause*/) {
        self.emit_token_with_comment(
            SyntaxKind::CaseKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_as_case_clause = node.as_case_clause();
        self.emit_expression(
            Some(&*node_as_case_clause.expression),
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

        self.emit_case_or_default_clause_rest(
            node,
            &node_as_case_clause.statements,
            node_as_case_clause.expression.end(),
        );
    }

    pub(super) fn emit_default_clause(&self, node: &Node /*DefaultClause*/) {
        let pos = self.emit_token_with_comment(
            SyntaxKind::DefaultKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit_case_or_default_clause_rest(node, &node.as_default_clause().statements, pos);
    }

    pub(super) fn emit_case_or_default_clause_rest(
        &self,
        parent_node: &Node,
        statements: &NodeArray, /*<Statement>*/
        colon_pos: isize,
    ) {
        let emit_as_single_statement = statements.len() == 1
            && (node_is_synthesized(parent_node)
                || node_is_synthesized(&*statements[0])
                || range_start_positions_are_on_same_line(
                    parent_node,
                    &*statements[0],
                    &self.current_source_file(),
                ));

        let mut format = ListFormat::CaseOrDefaultClauseStatements;
        if emit_as_single_statement {
            self.write_token(
                SyntaxKind::ColonToken,
                colon_pos,
                |text: &str| self.write_punctuation(text),
                Some(parent_node),
            );
            self.write_space();
            format &= !(ListFormat::MultiLine | ListFormat::Indented);
        } else {
            self.emit_token_with_comment(
                SyntaxKind::ColonToken,
                colon_pos,
                |text: &str| self.write_punctuation(text),
                parent_node,
                None,
            );
        }
        self.emit_list(
            Some(parent_node),
            Some(statements),
            format,
            None,
            None,
            None,
        );
    }

    pub(super) fn emit_heritage_clause(&self, node: &Node /*HeritageClause*/) {
        self.write_space();
        let node_as_heritage_clause = node.as_heritage_clause();
        self.write_token_text(
            node_as_heritage_clause.token,
            |text: &str| self.write_keyword(text),
            None,
        );
        self.write_space();
        self.emit_list(
            Some(node),
            Some(&node_as_heritage_clause.types),
            ListFormat::HeritageClauseTypes,
            None,
            None,
            None,
        );
    }

    pub(super) fn emit_catch_clause(&self, node: &Node /*CatchClause*/) {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::CatchKeyword,
            node.pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_as_catch_clause = node.as_catch_clause();
        if let Some(node_variable_declaration) = node_as_catch_clause.variable_declaration.as_ref()
        {
            self.emit_token_with_comment(
                SyntaxKind::OpenParenToken,
                open_paren_pos,
                |text: &str| self.write_punctuation(text),
                node,
                None,
            );
            self.emit(Some(&**node_variable_declaration), None);
            self.emit_token_with_comment(
                SyntaxKind::CloseParenToken,
                node_variable_declaration.end(),
                |text: &str| self.write_punctuation(text),
                node,
                None,
            );
            self.write_space();
        }
        self.emit(Some(&*node_as_catch_clause.block), None);
    }

    pub(super) fn emit_property_assignment(&self, node: &Node /*PropertyAssignment*/) {
        let node_as_property_assignment = node.as_property_assignment();
        self.emit(node_as_property_assignment.maybe_name().as_deref(), None);
        self.write_punctuation(":");
        self.write_space();
        let ref initializer = node_as_property_assignment.maybe_initializer().unwrap();
        if !get_emit_flags(initializer).intersects(EmitFlags::NoLeadingComments) {
            let comment_range = get_comment_range(initializer);
            self.emit_trailing_comments_of_position(comment_range.pos(), None, None);
        }
        self.emit_expression(
            Some(&**initializer),
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

    pub(super) fn emit_shorthand_property_assignment(
        &self,
        node: &Node, /*ShorthandPropertyAssignment*/
    ) {
        let node_as_shorthand_property_assignment = node.as_shorthand_property_assignment();
        self.emit(
            node_as_shorthand_property_assignment
                .maybe_name()
                .as_deref(),
            None,
        );
        if let Some(node_object_assignment_initializer) = node_as_shorthand_property_assignment
            .object_assignment_initializer
            .as_ref()
        {
            self.write_space();
            self.write_punctuation("=");
            self.write_space();
            self.emit_expression(
                Some(&**node_object_assignment_initializer),
                Some(Rc::new({
                    let parenthesizer = self.parenthesizer();
                    move |node: &Node| {
                        with_synthetic_factory(|synthetic_factory| {
                            parenthesizer.parenthesize_expression_for_disallowed_comma(
                                synthetic_factory,
                                node,
                            )
                        })
                    }
                })),
            );
        }
    }

    pub(super) fn emit_spread_assignment(&self, node: &Node /*SpreadAssignment*/) {
        let node_as_spread_assignment = node.as_spread_assignment();
        // if (node.expression) {
        self.emit_token_with_comment(
            SyntaxKind::DotDotDotToken,
            node.pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_expression(
            Some(&*node_as_spread_assignment.expression),
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
        // }
    }

    pub(super) fn emit_enum_member(&self, node: &Node /*EnumMember*/) {
        let node_as_enum_member = node.as_enum_member();
        self.emit(node_as_enum_member.maybe_name().as_deref(), None);
        self.emit_initializer(
            node_as_enum_member.initializer.as_deref(),
            node_as_enum_member.name.end(),
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

    pub(super) fn emit_jsdoc(&self, node: &Node /*JSDoc*/) {
        self.write("/**");
        let node_as_jsdoc = node.as_jsdoc();
        if let Some(node_comment) = node_as_jsdoc.comment.as_ref() {
            let text = get_text_of_jsdoc_comment(Some(node_comment));
            if let Some(text) = text.filter(|text| !text.is_empty()) {
                lazy_static! {
                    static ref lines_regex: Regex = Regex::new(r"\r\n?|\n").unwrap();
                }
                let lines = lines_regex.split(&text);
                for line in lines {
                    self.write_line(None);
                    self.write_space();
                    self.write_punctuation("*");
                    self.write_space();
                    self.write(line);
                }
            }
        }
        if let Some(node_tags) = node_as_jsdoc.tags.as_ref() {
            if node_tags.len() == 1
                && node_tags[0].kind() == SyntaxKind::JSDocTypeTag
                && node_as_jsdoc.comment.is_none()
            {
                self.write_space();
                self.emit(Some(&*node_tags[0]), None);
            } else {
                self.emit_list(
                    Some(node),
                    Some(node_tags),
                    ListFormat::JSDocComment,
                    None,
                    None,
                    None,
                );
            }
        }
        self.write_space();
        self.write("*/");
    }

    pub(super) fn emit_jsdoc_simple_typed_tag(
        &self,
        tag: &Node, /*JSDocTypeTag | JSDocThisTag | JSDocEnumTag | JSDocReturnTag*/
    ) {
        let tag_as_base_jsdoc_type_like_tag = tag.as_base_jsdoc_type_like_tag();
        self.emit_jsdoc_tag_name(&tag_as_base_jsdoc_type_like_tag.tag_name());
        self.emit_jsdoc_type_expression(
            tag_as_base_jsdoc_type_like_tag
                .maybe_type_expression()
                .as_deref(),
        );
        self.emit_jsdoc_comment(
            tag_as_base_jsdoc_type_like_tag
                .maybe_comment()
                .map(Into::into),
        );
    }

    pub(super) fn emit_jsdoc_see_tag(&self, tag: &Node /*JSDocSeeTag*/) {
        let tag_as_jsdoc_see_tag = tag.as_jsdoc_see_tag();
        self.emit_jsdoc_tag_name(&tag_as_jsdoc_see_tag.tag_name());
        self.emit(tag_as_jsdoc_see_tag.name.as_deref(), None);
        self.emit_jsdoc_comment(tag_as_jsdoc_see_tag.maybe_comment().map(Into::into));
    }

    pub(super) fn emit_jsdoc_name_reference(&self, node: &Node /*JSDocNameReference*/) {
        self.write_space();
        self.write_punctuation("{");
        self.emit(Some(&*node.as_jsdoc_name_reference().name), None);
        self.write_punctuation("}");
    }

    pub(super) fn emit_jsdoc_heritage_tag(
        &self,
        tag: &Node, /*JSDocImplementsTag | JSDocAugmentsTag*/
    ) {
        let tag_as_jsdoc_heritage_tag = tag.as_jsdoc_heritage_tag();
        self.emit_jsdoc_tag_name(&tag_as_jsdoc_heritage_tag.tag_name());
        self.write_space();
        self.write_punctuation("{");
        self.emit(Some(&*tag_as_jsdoc_heritage_tag.class()), None);
        self.write_punctuation("}");
        self.emit_jsdoc_comment(tag_as_jsdoc_heritage_tag.maybe_comment().map(Into::into));
    }

    pub(super) fn emit_jsdoc_template_tag(&self, tag: &Node /*JSDocTemplateTag*/) {
        let tag_as_jsdoc_template_tag = tag.as_jsdoc_template_tag();
        self.emit_jsdoc_tag_name(&tag_as_jsdoc_template_tag.tag_name());
        self.emit_jsdoc_type_expression(tag_as_jsdoc_template_tag.constraint.as_deref());
        self.write_space();
        self.emit_list(
            Some(tag),
            Some(&tag_as_jsdoc_template_tag.type_parameters),
            ListFormat::CommaListElements,
            None,
            None,
            None,
        );
        self.emit_jsdoc_comment(tag_as_jsdoc_template_tag.maybe_comment().map(Into::into));
    }

    pub(super) fn emit_jsdoc_typedef_tag(&self, tag: &Node /*JSDocTypedefTag*/) {
        let tag_as_jsdoc_typedef_tag = tag.as_jsdoc_typedef_tag();
        self.emit_jsdoc_tag_name(&tag_as_jsdoc_typedef_tag.tag_name());
        if let Some(tag_type_expression) = tag_as_jsdoc_typedef_tag.type_expression.as_ref() {
            if tag_type_expression.kind() == SyntaxKind::JSDocTypeExpression {
                self.emit_jsdoc_type_expression(Some(&**tag_type_expression));
            } else {
                self.write_space();
                self.write_punctuation("{");
                self.write("Object");
                if tag_type_expression.as_jsdoc_type_literal().is_array_type {
                    self.write_punctuation("[");
                    self.write_punctuation("]");
                }
                self.write_punctuation("}");
            }
        }
        if let Some(tag_full_name) = tag_as_jsdoc_typedef_tag.full_name.as_ref() {
            self.write_space();
            self.emit(Some(&**tag_full_name), None);
        }
        self.emit_jsdoc_comment(tag_as_jsdoc_typedef_tag.maybe_comment().map(Into::into));
        if let Some(tag_type_expression) =
            tag_as_jsdoc_typedef_tag
                .type_expression
                .as_ref()
                .filter(|tag_type_expression| {
                    tag_type_expression.kind() == SyntaxKind::JSDocTypeLiteral
                })
        {
            self.emit_jsdoc_type_literal(tag_type_expression);
        }
    }

    pub(super) fn emit_jsdoc_callback_tag(&self, tag: &Node /*JSDocCallbackTag*/) {
        let tag_as_jsdoc_callback_tag = tag.as_jsdoc_callback_tag();
        self.emit_jsdoc_tag_name(&tag_as_jsdoc_callback_tag.tag_name());
        if let Some(tag_name) = tag_as_jsdoc_callback_tag.name.as_ref() {
            self.write_space();
            self.emit(Some(&**tag_name), None);
        }
        self.emit_jsdoc_comment(tag_as_jsdoc_callback_tag.maybe_comment().map(Into::into));
        self.emit_jsdoc_signature(&tag_as_jsdoc_callback_tag.type_expression);
    }

    pub(super) fn emit_jsdoc_simple_tag(&self, tag: &Node /*JSDocTag*/) {
        let tag_as_jsdoc_tag = tag.as_jsdoc_tag();
        self.emit_jsdoc_tag_name(&tag_as_jsdoc_tag.tag_name());
        self.emit_jsdoc_comment(tag_as_jsdoc_tag.maybe_comment().map(Into::into));
    }

    pub(super) fn emit_jsdoc_type_literal(&self, lit: &Node /*JSDocTypeLiteral*/) {
        self.emit_list(
            Some(lit),
            Some(&with_factory(|factory| {
                factory.create_node_array(
                    lit.as_jsdoc_type_literal().js_doc_property_tags.clone(),
                    None,
                )
            })),
            ListFormat::JSDocComment,
            None,
            None,
            None,
        );
    }

    pub(super) fn emit_jsdoc_signature(&self, sig: &Node /*JSDocSignature*/) {
        let sig_as_jsdoc_signature = sig.as_jsdoc_signature();
        if let Some(sig_type_parameters) = sig_as_jsdoc_signature.maybe_type_parameters().as_ref() {
            self.emit_list(
                Some(sig),
                Some(&with_factory(|factory| {
                    factory.create_node_array(Some(sig_type_parameters.clone()), None)
                })),
                ListFormat::JSDocComment,
                None,
                None,
                None,
            );
        }
        // if (sig.parameters) {
        self.emit_list(
            Some(sig),
            Some(&with_factory(|factory| {
                factory.create_node_array(Some(sig_as_jsdoc_signature.parameters.clone()), None)
            })),
            ListFormat::JSDocComment,
            None,
            None,
            None,
        );
        // }
        if let Some(sig_type) = sig_as_jsdoc_signature.type_.as_ref() {
            self.write_line(None);
            self.write_space();
            self.write_punctuation("*");
            self.write_space();
            self.emit(Some(&**sig_type), None);
        }
    }

    pub(super) fn emit_jsdoc_property_like_tag(&self, param: &Node /*JSDocPropertyLikeTag*/) {
        let param_as_jsdoc_property_like_tag = param.as_jsdoc_property_like_tag();
        self.emit_jsdoc_tag_name(&param_as_jsdoc_property_like_tag.tag_name());
        self.emit_jsdoc_type_expression(
            param_as_jsdoc_property_like_tag.type_expression.as_deref(),
        );
        self.write_space();
        if param_as_jsdoc_property_like_tag.is_bracketed {
            self.write_punctuation("[");
        }
        self.emit(Some(&*param_as_jsdoc_property_like_tag.name), None);
        if param_as_jsdoc_property_like_tag.is_bracketed {
            self.write_punctuation("]");
        }
        self.emit_jsdoc_comment(
            param_as_jsdoc_property_like_tag
                .maybe_comment()
                .map(Into::into),
        );
    }

    pub(super) fn emit_jsdoc_tag_name(&self, tag_name: &Node /*Identifier*/) {
        self.write_punctuation("@");
        self.emit(Some(tag_name), None);
    }

    pub(super) fn emit_jsdoc_comment(&self, comment: Option<StrOrNodeArrayRef>) {
        let text = get_text_of_jsdoc_comment(comment);
        if let Some(text) = text.filter(|text| !text.is_empty()) {
            self.write_space();
            self.write(&text);
        }
    }

    pub(super) fn emit_jsdoc_type_expression(
        &self,
        type_expression: Option<&Node /*JSDocTypeExpression*/>,
    ) {
        if let Some(type_expression) = type_expression {
            self.write_space();
            self.write_punctuation("{");
            self.emit(
                Some(&*type_expression.as_jsdoc_type_expression().type_),
                None,
            );
            self.write_punctuation("}");
        }
    }

    pub(super) fn emit_source_file(&self, node: &Node /*SourceFile*/) {
        self.write_line(None);
        let statements = &node.as_source_file().statements;
        // if (emitBodyWithDetachedComments) {
        let should_emit_detached_comment = statements.is_empty()
            || !is_prologue_directive(&statements[0])
            || node_is_synthesized(&*statements[0]);
        if should_emit_detached_comment {
            self.emit_body_with_detached_comments(node, statements, |node: &Node| {
                self.emit_source_file_worker(node)
            });
            return;
        }
        // }
        self.emit_source_file_worker(node);
    }

    pub(super) fn emit_synthetic_triple_slash_references_if_needed(
        &self,
        node: &Node, /*Bundle*/
    ) {
        let node_as_bundle = node.as_bundle();
        let default_references: Vec<FileReference> = vec![];
        self.emit_triple_slash_directives(
            node_as_bundle.has_no_default_lib == Some(true),
            node_as_bundle
                .synthetic_file_references
                .as_ref()
                .unwrap_or(&default_references),
            node_as_bundle
                .synthetic_type_references
                .as_ref()
                .unwrap_or(&default_references),
            node_as_bundle
                .synthetic_lib_references
                .as_ref()
                .unwrap_or(&default_references),
        );
        for prepend in &node_as_bundle.prepends {
            if is_unparsed_source(prepend) {
                if let Some(prepend_synthetic_references) =
                    prepend.as_unparsed_source().synthetic_references.as_ref()
                {
                    for ref_ in prepend_synthetic_references {
                        self.emit(Some(&**ref_), None);
                        self.write_line(None);
                    }
                }
            }
        }
    }

    pub(super) fn emit_triple_slash_directives_if_needed(&self, node: &Node /*SourceFile*/) {
        let node_as_source_file = node.as_source_file();
        if node_as_source_file.is_declaration_file() {
            self.emit_triple_slash_directives(
                node_as_source_file.has_no_default_lib(),
                &node_as_source_file.referenced_files(),
                &node_as_source_file.type_reference_directives(),
                &node_as_source_file.lib_reference_directives(),
            );
        }
    }

    pub(super) fn emit_triple_slash_directives(
        &self,
        has_no_default_lib: bool,
        files: &[FileReference],
        types: &[FileReference],
        libs: &[FileReference],
    ) {
        unimplemented!()
    }

    pub(super) fn emit_source_file_worker(&self, node: &Node /*SourceFile*/) {
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

    pub(super) fn emit_node_with_prefix<TPrefixWriter: FnMut(&str), TEmit: FnMut(&Node)>(
        &self,
        prefix: &str,
        prefix_writer: TPrefixWriter,
        node: Option<&Node>,
        emit: TEmit,
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
