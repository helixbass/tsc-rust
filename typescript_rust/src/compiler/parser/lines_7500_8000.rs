#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use gc::Gc;
use std::borrow::Cow;
use std::cell::{RefCell, RefMut};
use std::convert::{TryFrom, TryInto};

use super::ParserType;
use crate::{
    attach_file_to_diagnostics, for_each, for_each_child_returns, is_export_assignment,
    is_export_declaration, is_external_module_reference, is_import_declaration,
    is_import_equals_declaration, is_jsdoc_like_text, is_meta_property,
    last_index_of_returns_isize, set_parent, some, BaseNode, BaseNodeFactory, Debug_, Diagnostic,
    HasStatementsInterface, JSDoc, LanguageVariant, Node, NodeArray, NodeFlags, NodeInterface,
    ScriptKind, ScriptTarget, SourceTextAsChars, StringOrNodeArray, SyntaxKind,
};

impl ParserType {
    pub(super) fn parse_export_assignment(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Gc<Node /*ExportAssignment*/> {
        let saved_await_context = self.in_await_context();
        self.set_await_context(true);
        let mut is_export_equals: Option<bool> = None;
        if self.parse_optional(SyntaxKind::EqualsToken) {
            is_export_equals = Some(true);
        } else {
            self.parse_expected(SyntaxKind::DefaultKeyword, None, None);
        }
        let expression = self.parse_assignment_expression_or_higher();
        self.parse_semicolon();
        self.set_await_context(saved_await_context);
        let node = self.factory().create_export_assignment(
            self,
            decorators,
            modifiers,
            is_export_equals,
            expression,
        );
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn set_external_module_indicator(&self, source_file: &Node /*SourceFile*/) {
        let source_file_as_source_file = source_file.as_source_file();
        source_file_as_source_file.set_external_module_indicator(
            for_each(&source_file_as_source_file.statements(), |statement, _| {
                self.is_an_external_module_indicator_node(statement)
            })
            .or_else(|| self.get_import_meta_if_necessary(source_file)),
        );
    }

    pub(super) fn is_an_external_module_indicator_node(&self, node: &Node) -> Option<Gc<Node>> {
        if self.has_modifier_of_kind(node, SyntaxKind::ExportKeyword)
            || is_import_equals_declaration(node)
                && is_external_module_reference(
                    &node.as_import_equals_declaration().module_reference,
                )
            || is_import_declaration(node)
            || is_export_assignment(node)
            || is_export_declaration(node)
        {
            Some(node.node_wrapper())
        } else {
            None
        }
    }

    pub(super) fn get_import_meta_if_necessary(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Option<Gc<Node>> {
        if source_file
            .flags()
            .intersects(NodeFlags::PossiblyContainsImportMeta)
        {
            self.walk_tree_for_external_module_indicators(source_file)
        } else {
            None
        }
    }

    pub(super) fn walk_tree_for_external_module_indicators(&self, node: &Node) -> Option<Gc<Node>> {
        if self.is_import_meta(node) {
            Some(node.node_wrapper())
        } else {
            for_each_child_returns(
                node,
                |child| self.walk_tree_for_external_module_indicators(child),
                Option::<fn(&NodeArray) -> Option<Gc<Node>>>::None,
            )
        }
    }

    pub(super) fn has_modifier_of_kind(&self, node: &Node, kind: SyntaxKind) -> bool {
        let modifiers = node.maybe_modifiers();
        let modifiers: Option<&[Gc<Node>]> = modifiers.as_ref().map(|node_array| {
            let slice_ref: &[Gc<Node>] = node_array;
            slice_ref
        });
        some(modifiers, Some(|m: &Gc<Node>| m.kind() == kind))
    }

    pub(super) fn is_import_meta(&self, node: &Node) -> bool {
        if !is_meta_property(node) {
            return false;
        }
        let node_as_meta_property = node.as_meta_property();
        node_as_meta_property.keyword_token == SyntaxKind::ImportKeyword
            && node_as_meta_property.name.as_identifier().escaped_text == "meta"
    }

    pub fn JSDocParser_parse_jsdoc_type_expression_for_tests(
        &self,
        content: String,
        start: Option<usize>,
        length: Option<usize>,
    ) -> Option<ParsedJSDocTypeExpression> {
        self.initialize_state(
            "file.js",
            content.clone(),
            ScriptTarget::Latest,
            None,
            ScriptKind::JS,
        );
        self.scanner_mut().set_text(
            Some(content.chars().collect()),
            Some(content),
            start,
            length,
        );
        self.set_current_token(
            self.scanner()
                .scan(Some(&|message, length| self.scan_error(message, length))),
        );
        let js_doc_type_expression = self.JSDocParser_parse_jsdoc_type_expression(None);

        let source_file = self.create_source_file(
            "file.js",
            ScriptTarget::Latest,
            ScriptKind::JS,
            false,
            vec![],
            self.factory()
                .create_token(self, SyntaxKind::EndOfFileToken)
                .wrap(),
            NodeFlags::None,
        );
        let diagnostics = attach_file_to_diagnostics(&*self.parse_diagnostics(), &source_file);
        {
            let maybe_js_doc_diagnostics = self.maybe_js_doc_diagnostics();
            if let Some(js_doc_diagnostics) = &*maybe_js_doc_diagnostics {
                source_file
                    .as_source_file()
                    .set_js_doc_diagnostics(attach_file_to_diagnostics(
                        js_doc_diagnostics,
                        &source_file,
                    ));
            }
        }

        self.clear_state();

        /*jsDocTypeExpression ?*/
        Some(ParsedJSDocTypeExpression {
            js_doc_type_expression,
            diagnostics,
        })
    }

    pub fn JSDocParser_parse_jsdoc_type_expression(
        &self,
        may_omit_braces: Option<bool>,
    ) -> Gc<Node /*JSDocTypeExpression*/> {
        let may_omit_braces = may_omit_braces.unwrap_or(false);
        let pos = self.get_node_pos();
        let has_brace = if may_omit_braces {
            self.parse_optional(SyntaxKind::OpenBraceToken)
        } else {
            self.parse_expected(SyntaxKind::OpenBraceToken, None, None)
        };
        let type_ = self.do_inside_of_context(NodeFlags::JSDoc, || self.parse_jsdoc_type());
        if !may_omit_braces || has_brace {
            self.parse_expected_jsdoc(SyntaxKind::CloseBraceToken);
        }

        let result = self
            .factory()
            .create_jsdoc_type_expression(self, type_)
            .wrap();
        self.fixup_parent_references(&result);
        self.finish_node_ref(&*result, pos, None);
        result
    }

    pub fn JSDocParser_parse_jsdoc_name_reference(&self) -> Gc<Node /*JSDocNameReference*/> {
        let pos = self.get_node_pos();
        let has_brace = self.parse_optional(SyntaxKind::OpenBraceToken);
        let p2 = self.get_node_pos();
        let mut entity_name: Gc<Node /*EntityName | JSDocMemberName*/> =
            self.parse_entity_name(false, None).wrap();
        while self.token() == SyntaxKind::PrivateIdentifier {
            self.re_scan_hash_token();
            self.next_token_jsdoc();
            entity_name = self
                .finish_node(
                    self.factory().create_jsdoc_member_name(
                        self,
                        entity_name,
                        self.parse_identifier(None, None).wrap(),
                    ),
                    p2,
                    None,
                )
                .wrap();
        }
        if has_brace {
            self.parse_expected_jsdoc(SyntaxKind::CloseBraceToken);
        }

        let result = self
            .factory()
            .create_jsdoc_name_reference(self, entity_name)
            .wrap();
        self.fixup_parent_references(&result);
        self.finish_node_ref(&*result, pos, None);
        result
    }

    pub fn JSDocParser_parse_isolated_jsdoc_comment(
        &self,
        content: String,
        start: Option<usize>,
        length: Option<usize>,
    ) -> Option<ParsedIsolatedJSDocComment> {
        self.initialize_state(
            "",
            content.clone(),
            ScriptTarget::Latest,
            None,
            ScriptKind::JS,
        );
        let js_doc: Option<Gc<Node>> = self.do_inside_of_context(NodeFlags::JSDoc, || {
            self.JSDocParser_parse_jsdoc_comment_worker(start, length)
                .map(NodeInterface::wrap)
        });

        let source_file = self.create_source_file(
            "",
            ScriptTarget::Latest,
            ScriptKind::JS,
            false,
            vec![],
            self.factory()
                .create_token(self, SyntaxKind::EndOfFileToken)
                .wrap(),
            NodeFlags::None,
        );
        source_file.as_source_file().set_text(content);
        source_file
            .as_source_file()
            .set_language_variant(LanguageVariant::Standard);
        let diagnostics = attach_file_to_diagnostics(&*self.parse_diagnostics(), &source_file);
        self.clear_state();

        js_doc.map(|js_doc| ParsedIsolatedJSDocComment {
            js_doc,
            diagnostics,
        })
    }

    pub fn JSDocParser_parse_jsdoc_comment(
        &self,
        parent: &Node,
        start: usize,
        length: usize,
    ) -> Option<Gc<Node /*JSDoc*/>> {
        let save_token = self.current_token();
        let save_parse_diagnostics_length = self.parse_diagnostics().len();
        let save_parse_error_before_next_finished_node =
            self.parse_error_before_next_finished_node();

        let comment: Option<Gc<Node>> = self.do_inside_of_context(NodeFlags::JSDoc, || {
            self.JSDocParser_parse_jsdoc_comment_worker(Some(start), Some(length))
                .map(NodeInterface::wrap)
        });
        if let Some(comment) = comment.as_ref() {
            set_parent(comment, Some(parent.node_wrapper()));
        }

        if self.context_flags().intersects(NodeFlags::JavaScriptFile) {
            let mut js_doc_diagnostics = self.maybe_js_doc_diagnostics();
            if js_doc_diagnostics.is_none() {
                *js_doc_diagnostics = Some(vec![]);
            }
            js_doc_diagnostics
                .as_mut()
                .unwrap()
                .append(&mut self.parse_diagnostics().clone());
        }
        self.set_current_token(save_token);
        self.parse_diagnostics()
            .truncate(save_parse_diagnostics_length);
        self.set_parse_error_before_next_finished_node(save_parse_error_before_next_finished_node);
        comment
    }

    pub fn JSDocParser_parse_jsdoc_comment_worker(
        &self,
        start: Option<usize>,
        length: Option<usize>,
    ) -> Option<JSDoc> {
        let start = start.unwrap_or(0);
        let content = self.source_text();
        let content_as_chars = self.source_text_as_chars();
        let end = match length {
            None => content_as_chars.len(),
            Some(length) => start + length,
        };
        let length = end - start;

        // Debug_.assert(start >= 0, None);
        Debug_.assert(start <= end, None);
        Debug_.assert(end <= content_as_chars.len(), None);

        if !is_jsdoc_like_text(&content_as_chars, start) {
            return None;
        }
        ParseJSDocCommentWorker::new(self, start, end, length, &content, &content_as_chars).call()
    }
}

pub(super) struct ParseJSDocCommentWorker<'parser> {
    pub(super) parser: &'parser ParserType,
    pub(super) start: usize,
    pub(super) end: usize,
    pub(super) length: usize,
    pub(super) content_str: &'parser str,
    pub(super) content: &'parser SourceTextAsChars,
    pub(super) tags: Option<Vec<Gc<Node /*JSDocTag*/>>>,
    pub(super) tags_pos: Option<isize>,
    pub(super) tags_end: Option<isize>,
    pub(super) link_end: Option<usize>,
    pub(super) comments_pos: Option<isize>,
    pub(super) comments: RefCell<Vec<String>>,
    pub(super) parts: Vec<Gc<Node /*JSDocComment*/>>,
}

impl<'parser> ParseJSDocCommentWorker<'parser> {
    pub(super) fn new(
        parser: &'parser ParserType,
        start: usize,
        end: usize,
        length: usize,
        content_str: &'parser str,
        content: &'parser SourceTextAsChars,
    ) -> Self {
        Self {
            parser,
            start,
            end,
            length,
            content_str,
            content,
            tags: None,
            tags_pos: None,
            tags_end: None,
            link_end: None,
            comments_pos: None,
            comments: RefCell::new(vec![]),
            parts: vec![],
        }
    }

    pub(super) fn tags(&mut self) -> &mut Vec<Gc<Node>> {
        self.tags.as_mut().unwrap()
    }

    pub(super) fn tags_pos(&self) -> isize {
        self.tags_pos.unwrap()
    }

    pub(super) fn tags_end(&self) -> isize {
        self.tags_end.unwrap()
    }

    pub(super) fn link_end(&self) -> usize {
        self.link_end.unwrap()
    }

    pub(super) fn comments(&self) -> RefMut<Vec<String>> {
        self.comments.borrow_mut()
    }

    pub(super) fn call(&mut self) -> Option<JSDoc> {
        Some(self.parser
            .scanner()
            .scan_range(self.start + 3, self.length - 5, || {
                let mut state = JSDocState::SawAsterisk;
                let mut margin: Option<usize> = None;
                let mut indent: usize = self.start
                    - usize::try_from(last_index_of_returns_isize(self.content, &'\n', |a, b| a == b, Some(self.start)) + 1).unwrap()
                    + 4;

                self.parser.next_token_jsdoc();
                while self.parse_optional_jsdoc(SyntaxKind::WhitespaceTrivia) {}
                if self.parse_optional_jsdoc(SyntaxKind::NewLineTrivia) {
                    state = JSDocState::BeginningOfLine;
                    indent = 0;
                }
                loop {
                    match self.parser.token() {
                        SyntaxKind::AtToken => {
                            if matches!(
                                state,
                                JSDocState::BeginningOfLine | JSDocState::SawAsterisk
                            ) {
                                self.remove_trailing_whitespace(&mut self.comments());
                                if self.comments_pos.is_none() {
                                    self.comments_pos = Some(self.parser.get_node_pos());
                                }
                                let tag = self.parse_tag(indent).wrap();
                                self.add_tag(Some(tag));
                                state = JSDocState::BeginningOfLine;
                                margin = None;
                            } else {
                                let margin_and_indent = self.push_comment(
                                    margin,
                                    indent,
                                    self.parser.scanner().get_token_text(),
                                );
                                margin = margin_and_indent.0;
                                indent = margin_and_indent.1;
                            }
                        }
                        SyntaxKind::NewLineTrivia => {
                            self.comments().push(self.parser.scanner().get_token_text());
                            state = JSDocState::BeginningOfLine;
                            indent = 0;
                        }
                        SyntaxKind::AsteriskToken => {
                            let asterisk = self.parser.scanner().get_token_text();
                            if matches!(state, JSDocState::SawAsterisk | JSDocState::SavingComments)
                            {
                                state = JSDocState::SavingComments;
                                let margin_and_indent =
                                    self.push_comment(margin, indent, asterisk);
                                margin = margin_and_indent.0;
                                indent = margin_and_indent.1;
                            } else {
                                state = JSDocState::SawAsterisk;
                                indent += asterisk.chars().count();
                            }
                        }
                        SyntaxKind::WhitespaceTrivia => {
                            let whitespace = self.parser.scanner().get_token_text();
                            let whitespace_chars: Vec<char> = whitespace.chars().collect();
                            if state == JSDocState::SavingComments {
                                self.comments().push(whitespace);
                            } else if let Some(margin) = margin {
                                if indent + whitespace_chars.len() > margin {
                                    self.comments()
                                        .push(whitespace_chars[(
                                            // this looks like `margin - indent` can be negative so
                                            // mimic the behavior of .slice() with a negative
                                            // argument (and avoid usize underflow)
                                            if indent > margin {
                                                if indent > whitespace_chars.len() + margin {
                                                    0
                                                } else {
                                                    whitespace_chars.len() + margin - indent
                                                }
                                            } else {
                                                margin - indent
                                            }
                                        )..].into_iter().collect());
                                }
                            }
                            indent += whitespace_chars.len();
                        }
                        SyntaxKind::EndOfFileToken => {
                            break;
                        }
                        SyntaxKind::OpenBraceToken => {
                            state = JSDocState::SavingComments;
                            let comment_end = self.parser.scanner().get_start_pos();
                            let link_start = self.parser.scanner().get_text_pos() - 1;
                            let link = self.parse_jsdoc_link(link_start);
                            if let Some(link) = link {
                                if match self.link_end {
                                    Some(link_end) if link_end == 0 => true,
                                    None => true,
                                    _ => false,
                                } {
                                    self.remove_leading_newlines(&mut self.comments());
                                }
                                let part=
                                    self.parser.finish_node(
                                        self.parser.factory().create_jsdoc_text(
                                            self.parser,
                                            self.comments().join(""),
                                        ),
                                        self.link_end.unwrap_or(self.start).try_into().unwrap(),
                                        Some(comment_end.try_into().unwrap()),
                                    ).wrap();
                                self.parts.push(part);
                                self.parts.push(link.wrap());
                                *self.comments() = vec![];
                                self.link_end = Some(self.parser.scanner().get_text_pos());
                            }
                        }
                        _ => {
                            state = JSDocState::SavingComments;
                            let margin_and_indent = self.push_comment(
                                margin,
                                indent,
                                self.parser.scanner().get_token_text(),
                            );
                            margin = margin_and_indent.0;
                            indent = margin_and_indent.1;
                        }
                    }
                    self.parser.next_token_jsdoc();
                }
                self.remove_trailing_whitespace(&mut self.comments());
                if !self.parts.is_empty() && !self.comments().is_empty() {
                    let part=
                        self.parser.finish_node(
                            self.parser.factory().create_jsdoc_text(self.parser, self.comments().join("")),
                            self.link_end.unwrap_or(self.start).try_into().unwrap(),
                            self.comments_pos,
                        ).wrap();
                    self.parts.push(part);
                }
                if !self.parts.is_empty() && self.tags.is_some() {
                    Debug_.assert_is_defined(&self.comments_pos, Some("having parsed tags implies that the end of the comments span should be set"));
                }
                let tags_array = self.tags.as_ref().map(
                    |tags| self.parser.create_node_array(tags.clone(), self.tags_pos.unwrap(), self.tags_end, None));
                self.parser.finish_node(
                    self.parser.factory().create_jsdoc_comment(self.parser, if !self.parts.is_empty() {
                        Some(Into::<StringOrNodeArray>::into(self.parser.create_node_array(self.parts.clone(), self.start.try_into().unwrap(), self.comments_pos, None)))
                    } else if !self.comments().is_empty() {
                        Some(Into::<StringOrNodeArray>::into(self.comments().join("")))
                    } else {
                        None
                    }, tags_array),
                    self.start.try_into().unwrap(),
                    Some(self.end.try_into().unwrap()),
                )
            }))
    }

    pub(super) fn push_comment(
        &self,
        mut margin: Option<usize>,
        mut indent: usize,
        text: String,
    ) -> (Option<usize>, usize) {
        if margin.is_none() {
            margin = Some(indent);
        }
        let text_len = text.chars().count();
        self.comments().push(text);
        indent += text_len;
        (margin, indent)
    }

    pub(super) fn remove_leading_newlines(&self, comments: &mut Vec<String>) {
        while !comments.is_empty() && matches!(&*comments[0], "\n" | "\r") {
            comments.remove(0);
        }
    }

    pub(super) fn remove_trailing_whitespace(&self, comments: &mut Vec<String>) {
        while !comments.is_empty() && comments[comments.len() - 1].trim() == "" {
            comments.pop().unwrap();
        }
    }

    pub(super) fn is_next_nonwhitespace_token_end_of_file(&self) -> bool {
        loop {
            self.parser.next_token_jsdoc();
            if self.parser.token() == SyntaxKind::EndOfFileToken {
                return true;
            }
            if !matches!(
                self.parser.token(),
                SyntaxKind::WhitespaceTrivia | SyntaxKind::NewLineTrivia
            ) {
                return false;
            }
        }
    }

    pub(super) fn skip_whitespace(&self) {
        if matches!(
            self.parser.token(),
            SyntaxKind::WhitespaceTrivia | SyntaxKind::NewLineTrivia
        ) {
            if self
                .parser
                .look_ahead_bool(|| self.is_next_nonwhitespace_token_end_of_file())
            {
                return;
            }
        }
        while matches!(
            self.parser.token(),
            SyntaxKind::WhitespaceTrivia | SyntaxKind::NewLineTrivia
        ) {
            self.parser.next_token_jsdoc();
        }
    }

    pub(super) fn skip_whitespace_or_asterisk(&self) -> Cow<'static, str> {
        if matches!(
            self.parser.token(),
            SyntaxKind::WhitespaceTrivia | SyntaxKind::NewLineTrivia
        ) {
            if self
                .parser
                .look_ahead_bool(|| self.is_next_nonwhitespace_token_end_of_file())
            {
                return "".into();
            }
        }

        let mut preceding_line_break = self.parser.scanner().has_preceding_line_break();
        let mut seen_line_break = false;
        let mut indent_text = "".to_owned();
        while preceding_line_break && self.parser.token() == SyntaxKind::AsteriskToken
            || matches!(
                self.parser.token(),
                SyntaxKind::WhitespaceTrivia | SyntaxKind::NewLineTrivia
            )
        {
            indent_text.push_str(&self.parser.scanner().get_token_text());
            if self.parser.token() == SyntaxKind::NewLineTrivia {
                preceding_line_break = true;
                seen_line_break = true;
                indent_text = "".to_owned();
            } else if self.parser.token() == SyntaxKind::AsteriskToken {
                preceding_line_break = false;
            }
            self.parser.next_token_jsdoc();
        }
        if seen_line_break {
            indent_text.into()
        } else {
            "".into()
        }
    }

    pub(super) fn parse_tag(&self, margin: usize) -> Node {
        Debug_.assert(self.parser.token() == SyntaxKind::AtToken, None);
        let start = self.parser.scanner().get_token_pos();
        self.parser.next_token_jsdoc();

        let tag_name: Gc<Node> = self.parse_jsdoc_identifier_name(None).wrap();
        let indent_text = self.skip_whitespace_or_asterisk();

        let mut tag: Option<Node> = None;
        match &*tag_name.as_identifier().escaped_text {
            "author" => {
                tag = Some(
                    self.parse_author_tag(start, tag_name, margin, &indent_text)
                        .into(),
                );
            }
            "implements" => {
                tag = Some(
                    self.parse_implements_tag(start, tag_name, margin, &indent_text)
                        .into(),
                );
            }
            "augments" | "extends" => {
                tag = Some(
                    self.parse_augments_tag(start, tag_name, margin, &indent_text)
                        .into(),
                );
            }
            "class" | "constructor" => {
                tag = Some(
                    self.parse_simple_tag(
                        start,
                        |tag_name, comment| {
                            self.parser.factory().create_jsdoc_class_tag(
                                self.parser,
                                tag_name,
                                comment,
                            )
                        },
                        tag_name,
                        margin,
                        &indent_text,
                    )
                    .into(),
                );
            }
            "public" => {
                tag = Some(
                    self.parse_simple_tag(
                        start,
                        |tag_name, comment| {
                            self.parser.factory().create_jsdoc_public_tag(
                                self.parser,
                                tag_name,
                                comment,
                            )
                        },
                        tag_name,
                        margin,
                        &indent_text,
                    )
                    .into(),
                );
            }
            "private" => {
                tag = Some(
                    self.parse_simple_tag(
                        start,
                        |tag_name, comment| {
                            self.parser.factory().create_jsdoc_private_tag(
                                self.parser,
                                tag_name,
                                comment,
                            )
                        },
                        tag_name,
                        margin,
                        &indent_text,
                    )
                    .into(),
                );
            }
            "protected" => {
                tag = Some(
                    self.parse_simple_tag(
                        start,
                        |tag_name, comment| {
                            self.parser.factory().create_jsdoc_protected_tag(
                                self.parser,
                                tag_name,
                                comment,
                            )
                        },
                        tag_name,
                        margin,
                        &indent_text,
                    )
                    .into(),
                );
            }
            "readonly" => {
                tag = Some(
                    self.parse_simple_tag(
                        start,
                        |tag_name, comment| {
                            self.parser.factory().create_jsdoc_readonly_tag(
                                self.parser,
                                tag_name,
                                comment,
                            )
                        },
                        tag_name,
                        margin,
                        &indent_text,
                    )
                    .into(),
                );
            }
            "override" => {
                tag = Some(
                    self.parse_simple_tag(
                        start,
                        |tag_name, comment| {
                            self.parser.factory().create_jsdoc_override_tag(
                                self.parser,
                                tag_name,
                                comment,
                            )
                        },
                        tag_name,
                        margin,
                        &indent_text,
                    )
                    .into(),
                );
            }
            "deprecated" => {
                self.parser.set_has_deprecated_tag(true);
                tag = Some(
                    self.parse_simple_tag(
                        start,
                        |tag_name, comment| {
                            self.parser.factory().create_jsdoc_deprecated_tag(
                                self.parser,
                                tag_name,
                                comment,
                            )
                        },
                        tag_name,
                        margin,
                        &indent_text,
                    )
                    .into(),
                );
            }
            "this" => {
                tag = Some(
                    self.parse_this_tag(start, tag_name, margin, &indent_text)
                        .into(),
                );
            }
            "enum" => {
                tag = Some(
                    self.parse_enum_tag(start, tag_name, margin, &indent_text)
                        .into(),
                );
            }
            "arg" | "argument" | "param" => {
                tag = Some(
                    self.parse_parameter_or_property_tag(
                        start,
                        tag_name,
                        PropertyLikeParse::Parameter,
                        margin,
                    )
                    .into(),
                );
            }
            "return" | "returns" => {
                tag = Some(
                    self.parse_return_tag(start, tag_name, margin, &indent_text)
                        .into(),
                );
            }
            "template" => {
                tag = Some(
                    self.parse_template_tag(start, tag_name, margin, &indent_text)
                        .into(),
                );
            }
            "type" => {
                tag = Some(
                    self.parse_type_tag(start, tag_name, Some(margin), Some(&indent_text))
                        .into(),
                );
            }
            "typedef" => {
                tag = Some(
                    self.parse_typedef_tag(start, tag_name, margin, &indent_text)
                        .into(),
                );
            }
            "callback" => {
                tag = Some(
                    self.parse_callback_tag(start, tag_name, margin, &indent_text)
                        .into(),
                );
            }
            "see" => {
                tag = Some(
                    self.parse_see_tag(start, tag_name, Some(margin), Some(&indent_text))
                        .into(),
                );
            }
            _ => {
                tag = Some(
                    self.parse_unknown_tag(start, tag_name, margin, &indent_text)
                        .into(),
                );
            }
        }
        tag.unwrap()
    }

    pub(super) fn parse_trailing_tag_comments(
        &self,
        pos: usize,
        end: usize,
        mut margin: usize,
        indent_text: &str,
    ) -> Option<StringOrNodeArray> {
        if indent_text.is_empty() {
            margin += end - pos;
        }
        self.parse_tag_comments(margin, Some(indent_text.chars().skip(margin).collect()))
    }

    pub(super) fn parse_tag_comments(
        &self,
        mut indent: usize,
        initial_margin: Option<String>,
    ) -> Option<StringOrNodeArray> {
        let comments_pos = self.parser.get_node_pos();
        let mut comments: Vec<String> = vec![];
        let mut parts: Vec<Gc<Node /*JSDocComment*/>> = vec![];
        let mut link_end: Option<usize> = None;
        let mut state = JSDocState::BeginningOfLine;
        let mut previous_whitespace = false;
        let mut margin: Option<usize> = None;
        if let Some(initial_margin) = initial_margin {
            if !initial_margin.is_empty() {
                let margin_and_indent = self.push_comment(margin, indent, initial_margin);
                margin = margin_and_indent.0;
                indent = margin_and_indent.1;
            }
            state = JSDocState::SawAsterisk;
        }
        let mut tok = self.parser.token();
        loop {
            match tok {
                SyntaxKind::NewLineTrivia => {
                    state = JSDocState::BeginningOfLine;
                    comments.push(self.parser.scanner().get_token_text());
                    indent = 0;
                }
                SyntaxKind::AtToken => {
                    if state == JSDocState::SavingBackticks
                        || state == JSDocState::SavingComments
                            && (!previous_whitespace
                                || self
                                    .parser
                                    .look_ahead_bool(|| self.is_next_jsdoc_token_whitespace()))
                    {
                        comments.push(self.parser.scanner().get_token_text());
                    } else {
                        self.parser
                            .scanner()
                            .set_text_pos(self.parser.scanner().get_text_pos() - 1);
                        break;
                    }
                }
                SyntaxKind::EndOfFileToken => {
                    break;
                }
                SyntaxKind::WhitespaceTrivia => {
                    if matches!(
                        state,
                        JSDocState::SavingComments | JSDocState::SavingBackticks
                    ) {
                        let margin_and_indent = self.push_comment(
                            margin,
                            indent,
                            self.parser.scanner().get_token_text(),
                        );
                        margin = margin_and_indent.0;
                        indent = margin_and_indent.1;
                    } else {
                        let whitespace = self.parser.scanner().get_token_text();
                        let whitespace_chars: Vec<char> = whitespace.chars().collect();
                        if let Some(margin) = margin {
                            if indent + whitespace_chars.len() > margin {
                                comments.push(
                                    whitespace_chars[margin - indent..].into_iter().collect(),
                                );
                            }
                        }
                        indent += whitespace_chars.len();
                    }
                }
                SyntaxKind::OpenBraceToken => {
                    state = JSDocState::SavingComments;
                    let comment_end = self.parser.scanner().get_start_pos();
                    let link_start = self.parser.scanner().get_text_pos() - 1;
                    let link: Option<Gc<Node>> = self.parse_jsdoc_link(link_start).map(Node::wrap);
                    if let Some(link) = link {
                        parts.push(
                            self.parser
                                .finish_node(
                                    self.parser
                                        .factory()
                                        .create_jsdoc_text(self.parser, comments.join("")),
                                    link_end
                                        .map(|link_end| link_end.try_into().unwrap())
                                        .unwrap_or(comments_pos),
                                    Some(comment_end.try_into().unwrap()),
                                )
                                .wrap(),
                        );
                        parts.push(link);
                        comments = vec![];
                        link_end = Some(self.parser.scanner().get_text_pos());
                    } else {
                        let margin_and_indent = self.push_comment(
                            margin,
                            indent,
                            self.parser.scanner().get_token_text(),
                        );
                        margin = margin_and_indent.0;
                        indent = margin_and_indent.1;
                    }
                }
                SyntaxKind::BacktickToken => {
                    if state == JSDocState::SavingBackticks {
                        state = JSDocState::SavingComments;
                    } else {
                        state = JSDocState::SavingBackticks;
                    }
                    let margin_and_indent =
                        self.push_comment(margin, indent, self.parser.scanner().get_token_text());
                    margin = margin_and_indent.0;
                    indent = margin_and_indent.1;
                }
                SyntaxKind::AsteriskToken => {
                    if state == JSDocState::BeginningOfLine {
                        state = JSDocState::SawAsterisk;
                        indent += 1;
                    } else {
                        if state != JSDocState::SavingBackticks {
                            state = JSDocState::SavingComments;
                        }
                        let margin_and_indent = self.push_comment(
                            margin,
                            indent,
                            self.parser.scanner().get_token_text(),
                        );
                        margin = margin_and_indent.0;
                        indent = margin_and_indent.1;
                    }
                }
                _ => {
                    if state != JSDocState::SavingBackticks {
                        state = JSDocState::SavingComments;
                    }
                    let margin_and_indent =
                        self.push_comment(margin, indent, self.parser.scanner().get_token_text());
                    margin = margin_and_indent.0;
                    indent = margin_and_indent.1;
                }
            }
            previous_whitespace = self.parser.token() == SyntaxKind::WhitespaceTrivia;
            tok = self.parser.next_token_jsdoc();
        }

        self.remove_leading_newlines(&mut comments);
        self.remove_trailing_whitespace(&mut comments);
        if !parts.is_empty() {
            if !comments.is_empty() {
                parts.push(
                    self.parser
                        .finish_node(
                            self.parser
                                .factory()
                                .create_jsdoc_text(self.parser, comments.join("")),
                            link_end
                                .map(|link_end| link_end.try_into().unwrap())
                                .unwrap_or(comments_pos),
                            None,
                        )
                        .wrap(),
                );
            }
            Some(
                self.parser
                    .create_node_array(
                        parts,
                        comments_pos,
                        Some(self.parser.scanner().get_text_pos().try_into().unwrap()),
                        None,
                    )
                    .into(),
            )
        } else if !comments.is_empty() {
            Some(comments.join("").into())
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum JSDocState {
    BeginningOfLine,
    SawAsterisk,
    SavingComments,
    SavingBackticks,
}

bitflags! {
    pub(super) struct PropertyLikeParse: u32 {
        const None = 0;
        const Property = 1 << 0;
        const Parameter = 1 << 1;
        const CallbackParameter = 1 << 2;
    }
}

pub struct ParsedJSDocTypeExpression {
    pub js_doc_type_expression: Gc<Node /*JSDocTypeExpression*/>,
    pub diagnostics: Vec<Gc<Diagnostic>>,
}

pub struct ParsedIsolatedJSDocComment {
    pub js_doc: Gc<Node /*JSDoc*/>,
    pub diagnostics: Vec<Gc<Diagnostic>>,
}

impl BaseNodeFactory for ParserType {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.SourceFileConstructor()(kind, 0, 0))
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.IdentifierConstructor()(kind, 0, 0))
    }

    fn create_base_private_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.PrivateIdentifierConstructor()(kind, 0, 0))
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.TokenConstructor()(kind, 0, 0))
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.NodeConstructor()(kind, 0, 0))
    }
}

// lazy_static! {
//     static ref ParserMut: Mutex<ParserType> = Mutex::new(ParserType::new());
// }

#[allow(non_snake_case)]
pub(super) fn Parser() -> Gc<ParserType> {
    ParserType::new()
}
// fn Parser() -> MutexGuard<'static, ParserType> {
//     ParserMut.lock().unwrap()
// }

bitflags! {
    pub struct ParsingContext: u32 {
        const None = 0;
        const SourceElements = 1 << 0;
        const BlockStatements = 1 << 1;
        const SwitchClauses = 1 << 2;
        const SwitchClauseStatements = 1 << 3;
        const TypeMembers = 1 << 4;
        const ClassMembers = 1 << 5;
        const EnumMembers = 1 << 6;
        const HeritageClauseElement = 1 << 7;
        const VariableDeclarations = 1 << 8;
        const ObjectBindingElements = 1 << 9;
        const ArrayBindingElements = 1 << 10;
        const ArgumentExpressions = 1 << 11;
        const ObjectLiteralMembers = 1 << 12;
        const JsxAttributes = 1 << 13;
        const JsxChildren = 1 << 14;
        const ArrayLiteralMembers = 1 << 15;
        const Parameters = 1 << 16;
        const JSDocParameters = 1 << 17;
        const RestProperties = 1 << 18;
        const TypeParameters = 1 << 19;
        const TypeArguments = 1 << 20;
        const TupleElementTypes = 1 << 21;
        const HeritageClauses = 1 << 22;
        const ImportOrExportSpecifiers = 1 << 23;
        const AssertEntries = 1 << 24;
        const Count = 1 << 25;
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum Tristate {
    False,
    True,
    Unknown,
}
