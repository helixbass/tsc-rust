#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{RefCell, RefMut};
use std::convert::{TryFrom, TryInto};
use std::rc::Rc;

use super::ParserType;
use crate::{
    attach_file_to_diagnostics, for_each, for_each_child_returns, is_export_assignment,
    is_export_declaration, is_external_module_reference, is_import_declaration,
    is_import_equals_declaration, is_jsdoc_like_text, is_meta_property, last_index_of, set_parent,
    some, BaseJSDocTag, BaseNode, BaseNodeFactory, Debug_, Diagnostic, JSDoc, LanguageVariant,
    Node, NodeArray, NodeFlags, NodeInterface, ScriptKind, ScriptTarget, SourceTextAsChars,
    StringOrNodeArray, SyntaxKind,
};

impl ParserType {
    pub(super) fn parse_export_assignment(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> Rc<Node /*ExportAssignment*/> {
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
        let node = self.factory.create_export_assignment(
            self,
            decorators,
            modifiers,
            is_export_equals,
            expression,
        );
        self.with_jsdoc(self.finish_node(node, pos, None).into(), has_jsdoc)
    }

    pub(super) fn set_external_module_indicator(&self, source_file: &Node /*SourceFile*/) {
        let source_file_as_source_file = source_file.as_source_file();
        source_file_as_source_file.set_external_module_indicator(
            for_each(&source_file_as_source_file.statements, |statement, _| {
                self.is_an_external_module_indicator_node(statement)
            })
            .or_else(|| self.get_import_meta_if_necessary(source_file)),
        );
    }

    pub(super) fn is_an_external_module_indicator_node(&self, node: &Node) -> Option<Rc<Node>> {
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
    ) -> Option<Rc<Node>> {
        if source_file
            .flags()
            .intersects(NodeFlags::PossiblyContainsImportMeta)
        {
            self.walk_tree_for_external_module_indicators(source_file)
        } else {
            None
        }
    }

    pub(super) fn walk_tree_for_external_module_indicators(&self, node: &Node) -> Option<Rc<Node>> {
        if self.is_import_meta(node) {
            Some(node.node_wrapper())
        } else {
            for_each_child_returns(
                node,
                |child| self.walk_tree_for_external_module_indicators(child),
                Option::<fn(&NodeArray) -> Option<Rc<Node>>>::None,
            )
        }
    }

    pub(super) fn has_modifier_of_kind(&self, node: &Node, kind: SyntaxKind) -> bool {
        let modifiers = node.maybe_modifiers();
        let modifiers: Option<&[Rc<Node>]> = modifiers.as_ref().map(|node_array| {
            let slice_ref: &[Rc<Node>] = node_array;
            slice_ref
        });
        some(modifiers, Some(|m: &Rc<Node>| m.kind() == kind))
    }

    pub(super) fn is_import_meta(&self, node: &Node) -> bool {
        if !is_meta_property(node) {
            return false;
        }
        let node_as_meta_property = node.as_meta_property();
        node_as_meta_property.keyword_token == SyntaxKind::ImportKeyword
            && node_as_meta_property
                .name
                .as_identifier()
                .escaped_text
                .eq_str("meta")
    }

    pub fn JSDocParser_parse_jsdoc_type_expression_for_tests(
        &mut self,
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
            self.factory
                .create_token(self, SyntaxKind::EndOfFileToken)
                .into(),
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
    ) -> Rc<Node /*JSDocTypeExpression*/> {
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

        let result = Into::<Rc<Node>>::into(self.factory.create_jsdoc_type_expression(self, type_));
        self.fixup_parent_references(&result);
        self.finish_node_ref(&*result, pos, None);
        result
    }

    pub fn JSDocParser_parse_jsdoc_name_reference(&self) -> Rc<Node /*JSDocNameReference*/> {
        let pos = self.get_node_pos();
        let has_brace = self.parse_optional(SyntaxKind::OpenBraceToken);
        let p2 = self.get_node_pos();
        let mut entity_name: Rc<Node /*EntityName | JSDocMemberName*/> =
            self.parse_entity_name(false, None).wrap();
        while self.token() == SyntaxKind::PrivateIdentifier {
            self.re_scan_hash_token();
            self.next_token_jsdoc();
            entity_name = self
                .finish_node(
                    self.factory.create_jsdoc_member_name(
                        self,
                        entity_name,
                        self.parse_identifier(None, None).wrap(),
                    ),
                    p2,
                    None,
                )
                .into();
        }
        if has_brace {
            self.parse_expected_jsdoc(SyntaxKind::CloseBraceToken);
        }

        let result =
            Into::<Rc<Node>>::into(self.factory.create_jsdoc_name_reference(self, entity_name));
        self.fixup_parent_references(&result);
        self.finish_node_ref(&*result, pos, None);
        result
    }

    pub fn JSDocParser_parse_isolated_jsdoc_comment(
        &mut self,
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
        let js_doc: Option<Rc<Node>> = self.do_inside_of_context(NodeFlags::JSDoc, || {
            self.JSDocParser_parse_jsdoc_comment_worker(start, length)
                .map(Into::into)
        });

        let source_file = self.create_source_file(
            "",
            ScriptTarget::Latest,
            ScriptKind::JS,
            false,
            vec![],
            self.factory
                .create_token(self, SyntaxKind::EndOfFileToken)
                .into(),
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
    ) -> Option<Rc<Node /*JSDoc*/>> {
        let save_token = self.current_token();
        let save_parse_diagnostics_length = self.parse_diagnostics().len();
        let save_parse_error_before_next_finished_node =
            self.parse_error_before_next_finished_node();

        let comment: Option<Rc<Node>> = self.do_inside_of_context(NodeFlags::JSDoc, || {
            self.JSDocParser_parse_jsdoc_comment_worker(Some(start), Some(length))
                .map(Into::into)
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

        if !is_jsdoc_like_text(content_as_chars, start) {
            return None;
        }
        ParseJSDocCommentWorker::new(self, start, end, length, content, content_as_chars).call()
    }
}

pub(super) struct ParseJSDocCommentWorker<'parser> {
    pub(super) parser: &'parser ParserType,
    pub(super) start: usize,
    pub(super) end: usize,
    pub(super) length: usize,
    pub(super) content_str: &'parser str,
    pub(super) content: &'parser SourceTextAsChars,
    pub(super) tags: Option<Vec<Rc<Node /*JSDocTag*/>>>,
    pub(super) tags_pos: Option<usize>,
    pub(super) tags_end: Option<usize>,
    pub(super) link_end: Option<usize>,
    pub(super) comments_pos: Option<isize>,
    pub(super) comments: RefCell<Vec<String>>,
    pub(super) parts: Vec<Rc<Node /*JSDocComment*/>>,
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

    pub(super) fn tags(&mut self) -> &mut Vec<Rc<Node>> {
        self.tags.as_mut().unwrap()
    }

    pub(super) fn tags_pos(&self) -> usize {
        self.tags_pos.unwrap()
    }

    pub(super) fn tags_end(&self) -> usize {
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
            .scanner_mut()
            .scan_range(self.start + 3, self.length - 5, || {
                let mut state = JSDocState::SawAsterisk;
                let mut margin: Option<usize> = None;
                let mut indent: usize = self.start
                    - usize::try_from(last_index_of(self.content, '\n', self.start) + 1).unwrap()
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
                                    self.add_tag(Some(self.parse_tag(indent).into()));
                                    state = JSDocState::BeginningOfLine;
                                    margin = None;
                                }
                            } else {
                                let margin_and_indent = self.push_comment_call(
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
                                    self.push_comment_call(margin, indent, asterisk);
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
                                        .push(whitespace_chars[margin - indent..].into_iter().collect());
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
                                let part: Rc<Node> =
                                    self.parser.finish_node(
                                        self.parser.factory.create_jsdoc_text(
                                            self.parser,
                                            self.comments().join(""),
                                        ),
                                        self.link_end.unwrap_or(self.start).try_into().unwrap(),
                                        Some(comment_end.try_into().unwrap()),
                                    ).into();
                                self.parts.push(part);
                                self.parts.push(link.wrap());
                                *self.comments() = vec![];
                                self.link_end = Some(self.parser.scanner().get_text_pos());
                            }
                        }
                        _ => {
                            state = JSDocState::SavingComments;
                            let margin_and_indent = self.push_comment_call(
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
                    let part: Rc<Node> =
                        self.parser.finish_node(
                            self.parser.factory.create_jsdoc_text(self.parser, self.comments().join("")),
                            self.link_end.unwrap_or(self.start).try_into().unwrap(),
                            self.comments_pos,
                        ).into();
                    self.parts.push(part);
                }
                if !self.parts.is_empty() && self.tags.is_some() {
                    Debug_.assert_is_defined(&self.comments_pos, Some("having parsed tags implies that the end of the comments span should be set"));
                }
                let tags_array = self.tags.as_ref().map(
                    |tags| self.parser.create_node_array(tags.clone(), self.tags_pos.unwrap().try_into().unwrap(), self.tags_end.map(|tags_end| tags_end.try_into().unwrap()), None));
                self.parser.finish_node(
                    self.parser.factory.create_jsdoc_comment(self.parser, if !self.parts.is_empty() {
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

    pub(super) fn push_comment_call(
        &mut self,
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
        unimplemented!()
    }

    pub(super) fn remove_trailing_whitespace(&self, comments: &mut Vec<String>) {
        unimplemented!()
    }

    pub(super) fn parse_tag(&self, margin: usize) -> BaseJSDocTag {
        unimplemented!()
    }

    pub(super) fn parse_jsdoc_link(&self, start: usize) -> Option<Node> {
        unimplemented!()
    }

    pub(super) fn add_tag(&self, tag: Option<Rc<Node /*JSDocTag*/>>) {
        unimplemented!()
    }

    pub(super) fn parse_optional_jsdoc(&self, t: SyntaxKind /*JSDocSyntaxKind*/) -> bool {
        unimplemented!()
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
    pub js_doc_type_expression: Rc<Node /*JSDocTypeExpression*/>,
    pub diagnostics: Vec<Rc<Diagnostic>>,
}

pub struct ParsedIsolatedJSDocComment {
    pub js_doc: Rc<Node /*JSDoc*/>,
    pub diagnostics: Vec<Rc<Diagnostic>>,
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
pub(super) fn Parser() -> ParserType {
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
