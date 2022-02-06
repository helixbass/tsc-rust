#![allow(non_upper_case_globals)]

use std::cell::{Cell, Ref, RefCell, RefMut};
use std::convert::TryInto;
use std::rc::Rc;

use super::{Parser, ParsingContext};
use crate::{
    create_node_factory, create_scanner, ensure_script_kind, normalize_path, object_allocator,
    BaseNode, Diagnostic, DiagnosticMessage, Identifier, IncrementalParserSyntaxCursor, Node,
    NodeFactory, NodeFactoryFlags, NodeFlags, Scanner, ScriptKind, ScriptTarget, SyntaxKind,
    TemplateLiteralLikeNode,
};
use local_macros::ast_type;

pub fn create_source_file(
    file_name: &str,
    source_text: String,
    language_version: ScriptTarget,
    set_parent_nodes: Option<bool>,
    script_kind: Option<ScriptKind>,
) -> Rc<Node /*SourceFile*/> {
    let set_parent_nodes = set_parent_nodes.unwrap_or(false);
    // tracing?.push(tracing.Phase.Parse, "createSourceFile", { path: fileName }, /*separateBeginAndEnd*/ true);
    // performance.mark("beforeParse");
    let result: Rc<Node /*SourceFile*/>;

    // perfLogger.logStartParseSourceFile(fileName);
    if language_version == ScriptTarget::JSON {
        result = Parser().parse_source_file(
            file_name,
            source_text,
            language_version,
            None,
            Some(set_parent_nodes),
            Some(ScriptKind::JSON),
        );
    } else {
        result = Parser().parse_source_file(
            file_name,
            source_text,
            language_version,
            None,
            Some(set_parent_nodes),
            script_kind,
        );
    }
    // perfLogger.logStopParseSourceFile();

    // performance.mark("afterParse");
    // performance.measure("Parse", "beforeParse", "afterParse");
    // tracing?.pop();
    result
}

pub fn parse_isolated_entity_name(
    text: String,
    language_version: ScriptTarget,
) -> Option<Rc<Node /*EntityName*/>> {
    Parser().parse_isolated_entity_name(text, language_version)
}

pub fn parse_json_text(file_name: &str, source_text: String) -> Rc<Node /*JsonSourceFile*/> {
    Parser().parse_json_text(file_name, source_text, None, None, None)
}

#[ast_type(impl_from = false)]
pub enum MissingNode {
    Identifier(Identifier),
    TemplateLiteralLikeNode(TemplateLiteralLikeNode),
}

#[allow(non_snake_case)]
pub struct ParserType {
    pub(super) scanner: RefCell<Scanner>,
    pub(super) NodeConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    pub(super) IdentifierConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    pub(super) PrivateIdentifierConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    pub(super) TokenConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    pub(super) SourceFileConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    pub(super) factory: Rc<NodeFactory<ParserType>>,
    pub(super) file_name: Option<String>,
    pub(super) source_text: Option<String>,
    pub(super) parse_diagnostics:
        Option<RefCell<Vec<Rc<Diagnostic /*DiagnosticWithDetachedLocation*/>>>>,
    pub(super) current_token: RefCell<Option<SyntaxKind>>,
    pub(super) parsing_context: Cell<Option<ParsingContext>>,
    pub(super) context_flags: Cell<Option<NodeFlags>>,
    pub(super) top_level: Cell<bool>,
    pub(super) parse_error_before_next_finished_node: Cell<bool>,
}

impl ParserType {
    pub(super) fn new() -> Self {
        ParserType {
            scanner: RefCell::new(create_scanner(
                ScriptTarget::Latest,
                true,
                None,
                None,
                None,
                None,
                None,
            )),
            NodeConstructor: None,
            IdentifierConstructor: None,
            PrivateIdentifierConstructor: None,
            TokenConstructor: None,
            SourceFileConstructor: None,
            factory: create_node_factory(
                NodeFactoryFlags::NoParenthesizerRules
                    | NodeFactoryFlags::NoNodeConverters
                    | NodeFactoryFlags::NoOriginalNode,
            ),
            file_name: None,
            source_text: None,
            parse_diagnostics: None,
            current_token: RefCell::new(None),
            parsing_context: Cell::new(None),
            context_flags: Cell::new(None),
            top_level: Cell::new(true),
            parse_error_before_next_finished_node: Cell::new(false),
        }
    }

    pub(super) fn scanner(&self) -> Ref<Scanner> {
        self.scanner.borrow()
    }

    pub(super) fn scanner_mut(&self) -> RefMut<Scanner> {
        self.scanner.borrow_mut()
    }

    #[allow(non_snake_case)]
    pub(super) fn NodeConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.NodeConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_NodeConstructor(
        &mut self,
        NodeConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.NodeConstructor = Some(NodeConstructor);
    }

    #[allow(non_snake_case)]
    pub(super) fn IdentifierConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.IdentifierConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_IdentifierConstructor(
        &mut self,
        IdentifierConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.IdentifierConstructor = Some(IdentifierConstructor);
    }

    #[allow(non_snake_case)]
    pub(super) fn PrivateIdentifierConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.PrivateIdentifierConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_PrivateIdentifierConstructor(
        &mut self,
        PrivateIdentifierConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.PrivateIdentifierConstructor = Some(PrivateIdentifierConstructor);
    }

    #[allow(non_snake_case)]
    pub(super) fn TokenConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.TokenConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_TokenConstructor(
        &mut self,
        TokenConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.TokenConstructor = Some(TokenConstructor);
    }

    #[allow(non_snake_case)]
    pub(super) fn SourceFileConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.SourceFileConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_SourceFileConstructor(
        &mut self,
        SourceFileConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.SourceFileConstructor = Some(SourceFileConstructor);
    }

    pub(super) fn file_name(&self) -> &str {
        self.file_name.as_ref().unwrap()
    }

    pub(super) fn set_file_name(&mut self, file_name: String) {
        self.file_name = Some(file_name);
    }

    pub(super) fn source_text(&self) -> &str {
        self.source_text.as_ref().unwrap()
    }

    pub(super) fn set_source_text(&mut self, source_text: String) {
        self.source_text = Some(source_text);
    }

    pub(super) fn parse_diagnostics(&self) -> RefMut<Vec<Rc<Diagnostic>>> {
        self.parse_diagnostics.as_ref().unwrap().borrow_mut()
    }

    pub(super) fn set_parse_diagnostics(&mut self, parse_diagnostics: Vec<Rc<Diagnostic>>) {
        self.parse_diagnostics = Some(RefCell::new(parse_diagnostics));
    }

    pub(super) fn current_token(&self) -> SyntaxKind {
        self.current_token.borrow().unwrap()
    }

    pub(super) fn set_current_token(&self, token: SyntaxKind) {
        *self.current_token.borrow_mut() = Some(token);
    }

    pub(super) fn parsing_context(&self) -> ParsingContext {
        self.parsing_context.get().unwrap()
    }

    pub(super) fn set_parsing_context(&self, parsing_context: ParsingContext) {
        self.parsing_context.set(Some(parsing_context));
    }

    pub(super) fn context_flags(&self) -> NodeFlags {
        self.context_flags.get().unwrap()
    }

    pub(super) fn set_context_flags(&self, context_flags: NodeFlags) {
        self.context_flags.set(Some(context_flags));
    }

    pub(super) fn top_level(&self) -> bool {
        self.top_level.get()
    }

    pub(super) fn set_top_level(&self, top_level: bool) {
        self.top_level.set(top_level);
    }

    pub(super) fn parse_error_before_next_finished_node(&self) -> bool {
        self.parse_error_before_next_finished_node.get()
    }

    pub(super) fn set_parse_error_before_next_finished_node(&self, value: bool) {
        self.parse_error_before_next_finished_node.set(value);
    }

    pub(super) fn scan_error(&self, message: &DiagnosticMessage, length: usize) {
        self.parse_error_at_position(
            self.scanner().get_text_pos().try_into().unwrap(),
            length.try_into().unwrap(),
            message,
            None,
        );
    }

    pub fn parse_source_file(
        &mut self,
        file_name: &str,
        source_text: String,
        language_version: ScriptTarget,
        syntax_cursor: Option<IncrementalParserSyntaxCursor>,
        set_parent_nodes: Option<bool>,
        script_kind: Option<ScriptKind>,
    ) -> Rc<Node /*SourceFile*/> {
        let script_kind = ensure_script_kind(file_name, script_kind);
        if script_kind == ScriptKind::JSON {
            unimplemented!()
        }

        self.initialize_state(
            file_name,
            source_text,
            language_version,
            syntax_cursor,
            script_kind,
        );
        self.parse_source_file_worker()
    }

    pub fn parse_isolated_entity_name(
        &mut self,
        content: String,
        language_version: ScriptTarget,
    ) -> Option<Rc<Node /*EntityName*/>> {
        self.initialize_state("", content, language_version, None, ScriptKind::JS);
        unimplemented!()
    }

    pub fn parse_json_text(
        &mut self,
        file_name: &str,
        source_text: String,
        language_version: Option<ScriptTarget>,
        syntax_cursor: Option<IncrementalParserSyntaxCursor>,
        set_parent_nodes: Option<bool>,
    ) -> Rc<Node /*JsonSourceFile*/> {
        let language_version = language_version.unwrap_or(ScriptTarget::ES2015);
        self.initialize_state(
            file_name,
            source_text,
            language_version,
            syntax_cursor,
            ScriptKind::JSON,
        );
        unimplemented!()
    }

    pub(super) fn initialize_state(
        &mut self,
        _file_name: &str,
        _source_text: String,
        _language_version: ScriptTarget,
        _syntax_cursor: Option<IncrementalParserSyntaxCursor>,
        _script_kind: ScriptKind,
    ) {
        self.set_NodeConstructor(object_allocator.get_node_constructor());
        self.set_IdentifierConstructor(object_allocator.get_identifier_constructor());
        self.set_PrivateIdentifierConstructor(
            object_allocator.get_private_identifier_constructor(),
        );
        self.set_TokenConstructor(object_allocator.get_token_constructor());
        self.set_SourceFileConstructor(object_allocator.get_source_file_constructor());

        self.set_file_name(normalize_path(_file_name));
        self.set_source_text(_source_text.clone());

        self.set_parse_diagnostics(vec![]);
        self.set_parsing_context(ParsingContext::None);
        self.set_top_level(true);

        self.set_context_flags(NodeFlags::None);
        self.set_parse_error_before_next_finished_node(false);

        let mut scanner = self.scanner_mut();
        scanner.set_text(
            Some(_source_text.chars().collect()),
            Some(_source_text.to_string()),
            None,
            None,
        );
        // scanner.set_on_error(Some(Box::new(move |message, length| {
        //     self.scan_error(message, length)
        // })));
    }
}
