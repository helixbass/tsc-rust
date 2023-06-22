use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    io,
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use super::{Parser, ParsingContext};
use crate::{
    attach_file_to_diagnostics, convert_to_object_worker, create_node_factory, create_scanner,
    ensure_script_kind, for_each_child, gc_cell_ref_unwrapped, get_language_variant, is_logging,
    normalize_path, object_allocator, ref_unwrapped, BaseNode, Debug_, Diagnostic, Diagnostics,
    HasStatementsInterface, IncrementalParser, IncrementalParserSyntaxCursor,
    JsonConversionNotifierDummy, LanguageVariant, Node, NodeArray, NodeFactory, NodeFactoryFlags,
    NodeFlags, NodeInterface, ParsedIsolatedJSDocComment, ParsedJSDocTypeExpression,
    ReadonlyPragmaMap, Scanner, ScriptKind, ScriptTarget, SourceTextAsChars, SyntaxKind,
    TextChangeRange,
};

pub enum ForEachChildRecursivelyCallbackReturn<TValue> {
    Skip,
    Value(TValue),
}

impl<TValue> From<TValue> for ForEachChildRecursivelyCallbackReturn<TValue> {
    fn from(value: TValue) -> Self {
        Self::Value(value)
    }
}

pub fn for_each_child_recursively<
    TValue,
    TCBNode: FnMut(&Node, &Node) -> Option<ForEachChildRecursivelyCallbackReturn<TValue>>,
    TCBNodes: FnMut(&NodeArray, &Node) -> Option<ForEachChildRecursivelyCallbackReturn<TValue>>,
>(
    root_node: &Node,
    mut cb_node: TCBNode,
    mut cb_nodes: Option<TCBNodes>,
) -> Option<TValue> {
    let mut queue: Vec<RcNodeOrNodeArray> = gather_possible_children(root_node);
    let mut parents: Vec<Gc<Node>> = vec![];
    while parents.len() < queue.len() {
        parents.push(root_node.node_wrapper());
    }
    while !queue.is_empty() {
        let current = queue.pop().unwrap();
        let parent = parents.pop().unwrap();
        match current {
            RcNodeOrNodeArray::NodeArray(current) => {
                if let Some(cb_nodes) = cb_nodes.as_mut() {
                    let res = cb_nodes(&current, &parent);
                    if let Some(res) = res {
                        match res {
                            ForEachChildRecursivelyCallbackReturn::Skip => {
                                continue;
                            }
                            ForEachChildRecursivelyCallbackReturn::Value(res) => {
                                return Some(res);
                            }
                        }
                    }
                }
                for current_child in current.to_vec().iter().rev() {
                    queue.push(current_child.clone().into());
                    parents.push(parent.clone());
                }
            }
            RcNodeOrNodeArray::RcNode(current) => {
                let res = cb_node(&current, &parent);
                if let Some(res) = res {
                    match res {
                        ForEachChildRecursivelyCallbackReturn::Skip => {
                            continue;
                        }
                        ForEachChildRecursivelyCallbackReturn::Value(res) => {
                            return Some(res);
                        }
                    }
                }
                if current.kind() >= SyntaxKind::FirstNode {
                    for child in gather_possible_children(&current) {
                        queue.push(child);
                        parents.push(current.clone());
                    }
                }
            }
        }
    }
    None
}

pub fn for_each_child_recursively_bool(
    root_node: &Node,
    mut cb_node: impl FnMut(&Node, &Node) -> bool,
    cb_nodes: Option<impl FnMut(&NodeArray, &Node) -> bool>,
) -> bool {
    try_for_each_child_recursively_bool(
        root_node,
        |a: &Node, b: &Node| -> Result<_, ()> { Ok(cb_node(a, b)) },
        cb_nodes.map(|mut cb_nodes| {
            move |a: &NodeArray, b: &Node| -> Result<_, ()> { Ok(cb_nodes(a, b)) }
        }),
    )
    .unwrap()
}

pub fn try_for_each_child_recursively_bool<TError>(
    root_node: &Node,
    mut cb_node: impl FnMut(&Node, &Node) -> Result<bool, TError>,
    mut cb_nodes: Option<impl FnMut(&NodeArray, &Node) -> Result<bool, TError>>,
) -> Result<bool, TError> {
    let mut queue: Vec<RcNodeOrNodeArray> = gather_possible_children(root_node);
    let mut parents: Vec<Gc<Node>> = vec![];
    while parents.len() < queue.len() {
        parents.push(root_node.node_wrapper());
    }
    while !queue.is_empty() {
        let current = queue.pop().unwrap();
        let parent = parents.pop().unwrap();
        match current {
            RcNodeOrNodeArray::NodeArray(current) => {
                if let Some(cb_nodes) = cb_nodes.as_mut() {
                    let res = cb_nodes(&current, &parent)?;
                    if res {
                        return Ok(true);
                    }
                }
                for current_child in current.to_vec().iter().rev() {
                    queue.push(current_child.clone().into());
                    parents.push(parent.clone());
                }
            }
            RcNodeOrNodeArray::RcNode(current) => {
                let res = cb_node(&current, &parent)?;
                if res {
                    return Ok(true);
                }
                if current.kind() >= SyntaxKind::FirstNode {
                    for child in gather_possible_children(&current) {
                        queue.push(child);
                        parents.push(current.clone());
                    }
                }
            }
        }
    }
    Ok(false)
}

enum RcNodeOrNodeArray {
    RcNode(Gc<Node>),
    NodeArray(Gc<NodeArray>),
}

impl From<Gc<Node>> for RcNodeOrNodeArray {
    fn from(value: Gc<Node>) -> Self {
        Self::RcNode(value)
    }
}

impl From<Gc<NodeArray>> for RcNodeOrNodeArray {
    fn from(value: Gc<NodeArray>) -> Self {
        Self::NodeArray(value)
    }
}

fn gather_possible_children(node: &Node) -> Vec<RcNodeOrNodeArray> {
    let children: RefCell<Vec<RcNodeOrNodeArray>> = RefCell::new(vec![]);
    for_each_child(
        node,
        |child| {
            children.borrow_mut().insert(0, child.node_wrapper().into());
        },
        Some(|node_array: &NodeArray| {
            children
                .borrow_mut()
                .insert(0, node_array.rc_wrapper().into());
        }),
    );
    children.into_inner()
}

pub fn create_source_file(
    file_name: &str,
    source_text: String,
    language_version: ScriptTarget,
    set_parent_nodes: Option<bool>,
    script_kind: Option<ScriptKind>,
) -> io::Result<Gc<Node /*SourceFile*/>> {
    let set_parent_nodes = set_parent_nodes.unwrap_or(false);
    // tracing?.push(tracing.Phase.Parse, "createSourceFile", { path: fileName }, /*separateBeginAndEnd*/ true);
    // performance.mark("beforeParse");
    let result: Gc<Node /*SourceFile*/>;

    // perfLogger.logStartParseSourceFile(fileName);
    if language_version == ScriptTarget::JSON {
        result = Parser().parse_source_file(
            file_name,
            source_text,
            language_version,
            None,
            Some(set_parent_nodes),
            Some(ScriptKind::JSON),
        )?;
    } else {
        result = Parser().parse_source_file(
            file_name,
            source_text,
            language_version,
            None,
            Some(set_parent_nodes),
            script_kind,
        )?;
    }
    // perfLogger.logStopParseSourceFile();

    // performance.mark("afterParse");
    // performance.measure("Parse", "beforeParse", "afterParse");
    // tracing?.pop();
    Ok(result)
}

pub fn parse_isolated_entity_name(
    text: String,
    language_version: ScriptTarget,
) -> Option<Gc<Node /*EntityName*/>> {
    Parser().parse_isolated_entity_name(text, language_version)
}

pub fn parse_json_text(file_name: &str, source_text: String) -> Gc<Node /*JsonSourceFile*/> {
    Parser().parse_json_text(file_name, source_text, None, None, None)
}

pub fn is_external_module(file: &Node /*SourceFile*/) -> bool {
    file.as_source_file()
        .maybe_external_module_indicator()
        .is_some()
}

pub fn update_source_file(
    source_file: &Node, /*SourceFile*/
    new_text: String,
    text_change_range: TextChangeRange,
    aggressive_checks: Option<bool>,
) -> Gc<Node /*SourceFile*/> {
    let aggressive_checks = aggressive_checks.unwrap_or(false);
    let new_source_file = IncrementalParser().update_source_file(
        source_file,
        new_text,
        text_change_range,
        aggressive_checks,
    );
    new_source_file.set_flags(
        new_source_file.flags() | (source_file.flags() & NodeFlags::PermanentlySetIncrementalFlags),
    );
    new_source_file
}

#[allow(dead_code)]
pub(crate) fn parse_isolated_jsdoc_comment(
    content: String,
    start: Option<usize>,
    length: Option<usize>,
) -> Option<ParsedIsolatedJSDocComment> {
    let result = Parser().JSDocParser_parse_isolated_jsdoc_comment(content, start, length);
    if let Some(result) = result.as_ref()
    /*&& result.jsDoc*/
    {
        Parser().fixup_parent_references(&result.js_doc);
    }

    result
}

pub fn parse_jsdoc_type_expression_for_tests(
    content: String,
    start: Option<usize>,
    length: Option<usize>,
) -> Option<ParsedJSDocTypeExpression> {
    Parser().JSDocParser_parse_jsdoc_type_expression_for_tests(content, start, length)
}

#[allow(non_snake_case)]
#[derive(Trace, Finalize)]
pub struct ParserType {
    #[unsafe_ignore_trace]
    pub(super) scanner: RefCell<Scanner>,
    #[unsafe_ignore_trace]
    pub(super) disallow_in_and_decorator_context: NodeFlags,
    #[unsafe_ignore_trace]
    pub(super) NodeConstructor: Cell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    #[unsafe_ignore_trace]
    pub(super) IdentifierConstructor: Cell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    #[unsafe_ignore_trace]
    pub(super) PrivateIdentifierConstructor: Cell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    #[unsafe_ignore_trace]
    pub(super) TokenConstructor: Cell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    #[unsafe_ignore_trace]
    pub(super) SourceFileConstructor: Cell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    pub(super) factory: GcCell<Option<Gc<NodeFactory<ParserType>>>>,
    #[unsafe_ignore_trace]
    pub(super) file_name: RefCell<Option<String>>,
    #[unsafe_ignore_trace]
    pub(super) source_flags: Cell<Option<NodeFlags>>,
    #[unsafe_ignore_trace]
    pub(super) source_text: RefCell<Option<String>>,
    #[unsafe_ignore_trace]
    pub(super) source_text_as_chars: RefCell<Option<SourceTextAsChars>>,
    #[unsafe_ignore_trace]
    pub(super) language_version: Cell<Option<ScriptTarget>>,
    #[unsafe_ignore_trace]
    pub(super) script_kind: Cell<Option<ScriptKind>>,
    #[unsafe_ignore_trace]
    pub(super) language_variant: Cell<Option<LanguageVariant>>,
    pub(super) parse_diagnostics:
        GcCell<Option<Vec<Gc<Diagnostic /*DiagnosticWithDetachedLocation*/>>>>,
    pub(super) js_doc_diagnostics:
        GcCell<Option<Vec<Gc<Diagnostic /*DiagnosticWithDetachedLocation*/>>>>,
    pub(super) syntax_cursor: GcCell<Option<IncrementalParserSyntaxCursor>>,
    #[unsafe_ignore_trace]
    pub(super) current_token: RefCell<Option<SyntaxKind>>,
    #[unsafe_ignore_trace]
    pub(super) node_count: Cell<Option<usize>>,
    #[unsafe_ignore_trace]
    pub(super) identifiers: RefCell<Option<Rc<RefCell<HashMap<String, String>>>>>,
    #[unsafe_ignore_trace]
    pub(super) private_identifiers: RefCell<Option<Rc<RefCell<HashMap<String, String>>>>>,
    #[unsafe_ignore_trace]
    pub(super) identifier_count: Cell<Option<usize>>,
    #[unsafe_ignore_trace]
    pub(super) parsing_context: Cell<Option<ParsingContext>>,
    #[unsafe_ignore_trace]
    pub(super) not_parenthesized_arrow: RefCell<Option<HashSet<usize>>>,
    #[unsafe_ignore_trace]
    pub(super) context_flags: Cell<Option<NodeFlags>>,
    #[unsafe_ignore_trace]
    pub(super) top_level: Cell<bool>,
    #[unsafe_ignore_trace]
    pub(super) parse_error_before_next_finished_node: Cell<bool>,
    #[unsafe_ignore_trace]
    pub(super) has_deprecated_tag: Cell<bool>,
}

impl ParserType {
    pub(super) fn new() -> Gc<Self> {
        let ret = Gc::new(ParserType {
            scanner: RefCell::new(create_scanner(
                ScriptTarget::Latest,
                true,
                None,
                None,
                None,
                None,
                None,
            )),
            disallow_in_and_decorator_context: NodeFlags::DisallowInContext
                | NodeFlags::DecoratorContext,
            NodeConstructor: Default::default(),
            IdentifierConstructor: Default::default(),
            PrivateIdentifierConstructor: Default::default(),
            TokenConstructor: Default::default(),
            SourceFileConstructor: Default::default(),
            factory: Default::default(),
            file_name: Default::default(),
            source_flags: Default::default(),
            source_text: Default::default(),
            source_text_as_chars: Default::default(),
            language_version: Default::default(),
            script_kind: Default::default(),
            language_variant: Default::default(),
            parse_diagnostics: Default::default(),
            js_doc_diagnostics: Default::default(),
            syntax_cursor: Default::default(),
            current_token: Default::default(),
            node_count: Default::default(),
            identifiers: Default::default(),
            private_identifiers: Default::default(),
            identifier_count: Default::default(),
            parsing_context: Default::default(),
            not_parenthesized_arrow: Default::default(),
            context_flags: Default::default(),
            top_level: Cell::new(true),
            parse_error_before_next_finished_node: Default::default(),
            has_deprecated_tag: Default::default(),
        });
        *ret.factory.borrow_mut() = Some(create_node_factory(
            NodeFactoryFlags::NoParenthesizerRules
                | NodeFactoryFlags::NoNodeConverters
                | NodeFactoryFlags::NoOriginalNode,
            ret.clone(),
        ));
        ret
    }

    pub(super) fn factory(&self) -> GcCellRef<Gc<NodeFactory<ParserType>>> {
        gc_cell_ref_unwrapped(&self.factory)
    }

    pub(super) fn scanner(&self) -> Ref<Scanner> {
        self.scanner.borrow()
    }

    pub(super) fn scanner_mut(&self) -> RefMut<Scanner> {
        self.scanner.borrow_mut()
    }

    #[allow(non_snake_case)]
    pub(super) fn NodeConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.NodeConstructor.get().unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_NodeConstructor(
        &self,
        NodeConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.NodeConstructor.set(Some(NodeConstructor));
    }

    #[allow(non_snake_case)]
    pub(super) fn IdentifierConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.IdentifierConstructor.get().unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_IdentifierConstructor(
        &self,
        IdentifierConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.IdentifierConstructor.set(Some(IdentifierConstructor));
    }

    #[allow(non_snake_case)]
    pub(super) fn PrivateIdentifierConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.PrivateIdentifierConstructor.get().unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_PrivateIdentifierConstructor(
        &self,
        PrivateIdentifierConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.PrivateIdentifierConstructor
            .set(Some(PrivateIdentifierConstructor));
    }

    #[allow(non_snake_case)]
    pub(super) fn TokenConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.TokenConstructor.get().unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_TokenConstructor(
        &self,
        TokenConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.TokenConstructor.set(Some(TokenConstructor));
    }

    #[allow(non_snake_case)]
    pub(super) fn SourceFileConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.SourceFileConstructor.get().unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_SourceFileConstructor(
        &self,
        SourceFileConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.SourceFileConstructor.set(Some(SourceFileConstructor));
    }

    pub(super) fn file_name(&self) -> Ref<String> {
        ref_unwrapped(&self.file_name)
    }

    pub(super) fn set_file_name(&self, file_name: String) {
        *self.file_name.borrow_mut() = Some(file_name);
    }

    pub(super) fn source_flags(&self) -> NodeFlags {
        self.source_flags.get().unwrap()
    }

    pub(super) fn set_source_flags(&self, source_flags: NodeFlags) {
        self.source_flags.set(Some(source_flags));
    }

    pub(super) fn source_text(&self) -> Ref<String> {
        ref_unwrapped(&self.source_text)
    }

    pub(super) fn set_source_text(&self, source_text: Option<String>) {
        *self.source_text_as_chars.borrow_mut() = source_text
            .as_ref()
            .map(|source_text| source_text.chars().collect());
        *self.source_text.borrow_mut() = source_text;
    }

    pub(super) fn source_text_as_chars(&self) -> Ref<SourceTextAsChars> {
        ref_unwrapped(&self.source_text_as_chars)
    }

    pub(super) fn language_version(&self) -> ScriptTarget {
        self.language_version.get().unwrap()
    }

    pub(super) fn set_language_version(&self, language_version: Option<ScriptTarget>) {
        self.language_version.set(language_version);
    }

    pub(super) fn script_kind(&self) -> ScriptKind {
        self.script_kind.get().unwrap()
    }

    pub(super) fn set_script_kind(&self, script_kind: Option<ScriptKind>) {
        self.script_kind.set(script_kind);
    }

    pub(super) fn language_variant(&self) -> LanguageVariant {
        self.language_variant.get().unwrap()
    }

    pub(super) fn set_language_variant(&self, language_variant: Option<LanguageVariant>) {
        self.language_variant.set(language_variant);
    }

    pub(super) fn parse_diagnostics(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Diagnostic>>>, Vec<Gc<Diagnostic>>> {
        GcCellRefMut::map(self.parse_diagnostics.borrow_mut(), |option| {
            option.as_mut().unwrap()
        })
    }

    pub(super) fn set_parse_diagnostics(&self, parse_diagnostics: Option<Vec<Gc<Diagnostic>>>) {
        *self.parse_diagnostics.borrow_mut() = parse_diagnostics;
    }

    pub(super) fn maybe_js_doc_diagnostics(&self) -> GcCellRefMut<Option<Vec<Gc<Diagnostic>>>> {
        self.js_doc_diagnostics.borrow_mut()
    }

    pub(super) fn set_js_doc_diagnostics(&self, js_doc_diagnostics: Option<Vec<Gc<Diagnostic>>>) {
        *self.js_doc_diagnostics.borrow_mut() = js_doc_diagnostics;
    }

    pub(super) fn maybe_syntax_cursor(&self) -> GcCellRef<Option<IncrementalParserSyntaxCursor>> {
        self.syntax_cursor.borrow()
    }

    pub(super) fn syntax_cursor(&self) -> GcCellRef<IncrementalParserSyntaxCursor> {
        GcCellRef::map(self.syntax_cursor.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub(super) fn take_syntax_cursor(&self) -> Option<IncrementalParserSyntaxCursor> {
        self.syntax_cursor.borrow_mut().take()
    }

    pub(super) fn set_syntax_cursor(&self, syntax_cursor: Option<IncrementalParserSyntaxCursor>) {
        *self.syntax_cursor.borrow_mut() = syntax_cursor;
    }

    pub(super) fn maybe_current_token(&self) -> Option<SyntaxKind> {
        *self.current_token.borrow()
    }

    pub(super) fn current_token(&self) -> SyntaxKind {
        self.current_token.borrow().unwrap()
    }

    pub(super) fn set_current_token(&self, token: SyntaxKind) {
        *self.current_token.borrow_mut() = Some(token);
    }

    pub(super) fn node_count(&self) -> usize {
        self.node_count.get().unwrap()
    }

    pub(super) fn set_node_count(&self, node_count: usize) {
        self.node_count.set(Some(node_count));
    }

    pub(super) fn increment_node_count(&self) {
        self.node_count.set(Some(self.node_count() + 1));
    }

    pub(super) fn identifiers_rc(&self) -> Rc<RefCell<HashMap<String, String>>> {
        self.identifiers.borrow().clone().unwrap()
    }

    pub(super) fn identifiers(&self) -> Ref<Rc<RefCell<HashMap<String, String>>>> {
        Ref::map(self.identifiers.borrow(), |option| option.as_ref().unwrap())
    }

    pub(super) fn set_identifiers(
        &self,
        identifiers: Option<Rc<RefCell<HashMap<String, String>>>>,
    ) {
        *self.identifiers.borrow_mut() = identifiers;
    }

    pub(super) fn private_identifiers(&self) -> Ref<Rc<RefCell<HashMap<String, String>>>> {
        Ref::map(self.private_identifiers.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub(super) fn set_private_identifiers(
        &self,
        private_identifiers: Option<Rc<RefCell<HashMap<String, String>>>>,
    ) {
        *self.private_identifiers.borrow_mut() = private_identifiers;
    }

    pub(super) fn identifier_count(&self) -> usize {
        self.identifier_count.get().unwrap()
    }

    pub(super) fn set_identifier_count(&self, identifier_count: usize) {
        self.identifier_count.set(Some(identifier_count));
    }

    pub(super) fn increment_identifier_count(&self) {
        self.identifier_count.set(Some(self.identifier_count() + 1));
    }

    pub(super) fn parsing_context(&self) -> ParsingContext {
        self.parsing_context.get().unwrap()
    }

    pub(super) fn set_parsing_context(&self, parsing_context: ParsingContext) {
        self.parsing_context.set(Some(parsing_context));
    }

    pub(super) fn maybe_not_parenthesized_arrow(&self) -> RefMut<Option<HashSet<usize>>> {
        self.not_parenthesized_arrow.borrow_mut()
    }

    pub(super) fn set_not_parenthesized_arrow(
        &self,
        not_parenthesized_arrow: Option<HashSet<usize>>,
    ) {
        *self.not_parenthesized_arrow.borrow_mut() = not_parenthesized_arrow;
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

    pub(super) fn has_deprecated_tag(&self) -> bool {
        self.has_deprecated_tag.get()
    }

    pub(super) fn set_has_deprecated_tag(&self, value: bool) {
        self.has_deprecated_tag.set(value);
    }

    pub(super) fn count_node(&self, node: BaseNode) -> BaseNode {
        self.increment_node_count();
        node
    }

    pub fn parse_source_file(
        &self,
        file_name: &str,
        source_text: String,
        language_version: ScriptTarget,
        syntax_cursor: Option<IncrementalParserSyntaxCursor>,
        set_parent_nodes: Option<bool>,
        script_kind: Option<ScriptKind>,
    ) -> io::Result<Gc<Node /*SourceFile*/>> {
        if is_logging {
            println!("parsing source file: {}", file_name,);
        }
        let set_parent_nodes = set_parent_nodes.unwrap_or(false);
        let script_kind = ensure_script_kind(file_name, script_kind);
        if script_kind == ScriptKind::JSON {
            let result = self.parse_json_text(
                file_name,
                source_text,
                Some(language_version),
                syntax_cursor,
                Some(set_parent_nodes),
            );
            let result_as_source_file = result.as_source_file();
            convert_to_object_worker(
                &result,
                result_as_source_file
                    .statements()
                    .get(0)
                    .map(|statement| statement.as_expression_statement().expression.clone()),
                result_as_source_file.parse_diagnostics(),
                false,
                None,
                Option::<&JsonConversionNotifierDummy>::None,
            )?;
            result_as_source_file.set_referenced_files(Default::default());
            result_as_source_file.set_type_reference_directives(Default::default());
            result_as_source_file.set_lib_reference_directives(Default::default());
            result_as_source_file.set_amd_dependencies(vec![]);
            result_as_source_file.set_has_no_default_lib(false);
            result_as_source_file.set_pragmas(ReadonlyPragmaMap::new());
            return Ok(result);
        }

        self.initialize_state(
            file_name,
            source_text,
            language_version,
            syntax_cursor,
            script_kind,
        );

        let result = self.parse_source_file_worker(language_version, set_parent_nodes, script_kind);

        self.clear_state();

        Ok(result)
    }

    pub fn parse_isolated_entity_name(
        &self,
        content: String,
        language_version: ScriptTarget,
    ) -> Option<Gc<Node /*EntityName*/>> {
        self.initialize_state("", content, language_version, None, ScriptKind::JS);
        self.next_token();
        let entity_name = self.parse_entity_name(true, None);
        let is_invalid =
            self.token() == SyntaxKind::EndOfFileToken && self.parse_diagnostics().is_empty();
        self.clear_state();
        if is_invalid {
            Some(entity_name.wrap())
        } else {
            None
        }
    }

    pub fn parse_json_text(
        &self,
        file_name: &str,
        source_text: String,
        language_version: Option<ScriptTarget>,
        syntax_cursor: Option<IncrementalParserSyntaxCursor>,
        set_parent_nodes: Option<bool>,
    ) -> Gc<Node /*JsonSourceFile*/> {
        let language_version = language_version.unwrap_or(ScriptTarget::ES2015);
        let set_parent_nodes = set_parent_nodes.unwrap_or(false);
        self.initialize_state(
            file_name,
            source_text,
            language_version,
            syntax_cursor,
            ScriptKind::JSON,
        );
        self.set_source_flags(self.context_flags());

        self.next_token();
        let pos = self.get_node_pos();
        let statements: Gc<NodeArray>;
        let end_of_file_token: Gc<Node>;
        if self.token() == SyntaxKind::EndOfFileToken {
            statements = self.create_node_array(vec![], pos, Some(pos), None);
            end_of_file_token = self.parse_token_node().wrap();
        } else {
            let mut expressions: Option<Vec<Gc<Node>>> = None;
            while self.token() != SyntaxKind::EndOfFileToken {
                let expression: Gc<Node>;
                match self.token() {
                    SyntaxKind::OpenBracketToken => {
                        expression = self.parse_array_literal_expression().wrap();
                    }
                    SyntaxKind::TrueKeyword
                    | SyntaxKind::FalseKeyword
                    | SyntaxKind::NullKeyword => {
                        expression = self.parse_token_node().wrap();
                    }
                    SyntaxKind::MinusToken => {
                        if self.look_ahead_bool(|| {
                            self.next_token() == SyntaxKind::NumericLiteral
                                && self.next_token() != SyntaxKind::ColonToken
                        }) {
                            expression = self.parse_prefix_unary_expression().wrap();
                        } else {
                            expression = self.parse_object_literal_expression().wrap();
                        }
                    }
                    SyntaxKind::NumericLiteral | SyntaxKind::StringLiteral => {
                        if self.look_ahead_bool(|| self.next_token() != SyntaxKind::ColonToken) {
                            expression = self.parse_literal_node().wrap();
                        } else {
                            expression = self.parse_object_literal_expression().wrap();
                        }
                    }
                    _ => {
                        expression = self.parse_object_literal_expression().wrap();
                    }
                }

                match expressions {
                    Some(_) => {
                        expressions.as_mut().unwrap().push(expression);
                    }
                    None => {
                        expressions = Some(vec![expression]);
                        if self.token() != SyntaxKind::EndOfFileToken {
                            self.parse_error_at_current_token(&Diagnostics::Unexpected_token, None);
                        }
                    }
                }
            }

            let expression: Gc<Node> = match expressions {
                Some(expressions) if expressions.len() > 1 => self
                    .finish_node(
                        self.factory()
                            .create_array_literal_expression_raw(Some(expressions), None),
                        pos,
                        None,
                    )
                    .wrap(),
                _ => Debug_.check_defined(expressions, None)[0].clone(),
            };
            let statement = self.factory().create_expression_statement_raw(expression);
            let statement = self.finish_node(statement, pos, None);
            statements = self.create_node_array(vec![statement.wrap()], pos, None, None);
            end_of_file_token = self
                .parse_expected_token(
                    SyntaxKind::EndOfFileToken,
                    Some(&Diagnostics::Unexpected_token),
                    None,
                )
                .wrap();
        }

        let source_file = self.create_source_file(
            file_name,
            ScriptTarget::ES2015,
            ScriptKind::JSON,
            false,
            statements,
            end_of_file_token,
            self.source_flags(),
        );

        if set_parent_nodes {
            self.fixup_parent_references(&source_file);
        }

        let source_file_as_source_file = source_file.as_source_file();
        source_file_as_source_file.set_node_count(self.node_count());
        source_file_as_source_file.set_identifier_count(self.identifier_count());
        source_file_as_source_file.set_identifiers(self.identifiers_rc());
        source_file_as_source_file.set_parse_diagnostics(Gc::new(GcCell::new(
            attach_file_to_diagnostics(&*self.parse_diagnostics(), &source_file),
        )));
        {
            let maybe_js_doc_diagnostics = self.maybe_js_doc_diagnostics();
            if let Some(js_doc_diagnostics) = &*maybe_js_doc_diagnostics {
                source_file_as_source_file.set_js_doc_diagnostics(attach_file_to_diagnostics(
                    js_doc_diagnostics,
                    &source_file,
                ));
            }
        }

        let result = source_file;
        self.clear_state();
        result
    }

    pub(super) fn initialize_state(
        &self,
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
        self.set_source_text(Some(_source_text.clone()));
        self.set_language_version(Some(_language_version));
        self.set_syntax_cursor(_syntax_cursor);
        self.set_script_kind(Some(_script_kind));
        self.set_language_variant(Some(get_language_variant(_script_kind)));

        self.set_parse_diagnostics(Some(vec![]));
        self.set_parsing_context(ParsingContext::None);
        self.set_identifiers(Some(Rc::new(RefCell::new(HashMap::new()))));
        self.set_private_identifiers(Some(Rc::new(RefCell::new(HashMap::new()))));
        self.set_identifier_count(0);
        self.set_node_count(0);
        self.set_source_flags(NodeFlags::None);
        self.set_top_level(true);

        match self.script_kind() {
            ScriptKind::JS | ScriptKind::JSX => {
                self.set_context_flags(NodeFlags::JavaScriptFile);
            }
            ScriptKind::JSON => {
                self.set_context_flags(NodeFlags::JavaScriptFile | NodeFlags::JsonFile);
            }
            _ => {
                self.set_context_flags(NodeFlags::None);
            }
        }
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
        scanner.set_script_target(self.language_version());
        scanner.set_language_variant(self.language_variant());
    }
}
