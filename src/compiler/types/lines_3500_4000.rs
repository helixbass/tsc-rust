#![allow(non_upper_case_globals)]

use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

use super::{
    BaseNode, BaseTextRange, BuildInfo, CompilerOptions, Diagnostic, EmitHelper, FileReference,
    FlowNode, LanguageVariant, Node, NodeArray, Path, PatternAmbientModule, ReadonlyPragmaMap,
    ResolvedModuleFull, ResolvedTypeReferenceDirective, ScriptKind, ScriptTarget, Symbol,
    SymbolTable, TypeChecker,
};
use crate::{ConfigFileSpecs, ModeAwareCache, PackageId, PragmaContext, __String};
use local_macros::ast_type;

pub type SourceTextAsChars = Vec<char>;

pub fn str_to_source_text_as_chars(str_: &str) -> SourceTextAsChars {
    str_.chars().collect()
}

pub fn text_len(text: &SourceTextAsChars) -> usize {
    text.len()
}

pub fn maybe_text_char_at_index(text: &SourceTextAsChars, index: usize) -> Option<char> {
    text.get(index).map(|ch| *ch)
}

pub fn text_char_at_index(text: &SourceTextAsChars, index: usize) -> char {
    maybe_text_char_at_index(text, index).unwrap()
}

pub fn text_substring(text: &SourceTextAsChars, start: usize, end: usize) -> String {
    text[start..end].into_iter().collect()
}

pub fn text_str_num_chars(text: &str, start: usize, end: usize) -> usize {
    text[start..end].chars().count()
}

#[derive(Debug)]
pub struct AmdDependency {
    path: String,
    name: Option<String>,
}

pub trait SourceFileLike {
    fn text(&self) -> Ref<String>;
    fn text_as_chars(&self) -> Ref<SourceTextAsChars>;
    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>>;
    fn line_map(&self) -> Ref<Vec<usize>>;
    fn maybe_get_position_of_line_and_character(
        &self,
        line: usize,
        character: usize,
        allow_edits: Option<bool>,
    ) -> Option<usize>;
}

#[derive(Debug)]
pub struct RedirectInfo {
    pub redirect_target: Rc<Node /*SourceFile*/>,
    pub undirected: Rc<Node /*SourceFile*/>,
}

pub trait HasStatementsInterface {
    fn statements(&self) -> &[Rc<Node>];
}

#[derive(Debug)]
#[ast_type]
pub struct SourceFile {
    _node: BaseNode,
    _symbols_without_a_symbol_table_strong_references: RefCell<Vec<Rc<Symbol>>>,
    pub statements: NodeArray,
    pub end_of_file_token: Rc<Node /*Token<SyntaxFile.EndOfFileToken>*/>,

    file_name: RefCell<String>,
    path: RefCell<Option<Path>>,
    text: RefCell<String>,
    text_as_chars: RefCell<SourceTextAsChars>,
    resolved_path: RefCell<Option<Path>>,
    original_file_name: RefCell<Option<String>>,

    redirect_info: RefCell<Option<RedirectInfo>>,

    amd_dependencies: RefCell<Option<Vec<AmdDependency>>>,
    module_name: RefCell<Option<String>>,
    referenced_files: RefCell<Option<Vec<FileReference>>>,
    type_reference_directives: RefCell<Option<Vec<FileReference>>>,
    lib_reference_directives: RefCell<Option<Vec<FileReference>>>,
    language_variant: Cell<LanguageVariant>,
    is_declaration_file: Cell<bool>,

    has_no_default_lib: Cell<bool>,

    language_version: Cell<ScriptTarget>,

    script_kind: Cell<ScriptKind>,

    external_module_indicator: RefCell<Option<Rc<Node>>>,
    common_js_module_indicator: RefCell<Option<Rc<Node>>>,
    js_global_augmentations: RefCell<Option<Rc<RefCell<SymbolTable>>>>,

    identifiers: RefCell<Option<Rc<RefCell<HashMap<String, String>>>>>,
    node_count: Cell<Option<usize>>,
    identifier_count: Cell<Option<usize>>,
    symbol_count: Cell<Option<usize>>,

    parse_diagnostics: RefCell<Option<Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>>>,

    bind_diagnostics: RefCell<Option<Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>>>,
    bind_suggestion_diagnostics: RefCell<Option<Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>>>,

    js_doc_diagnostics: RefCell<Option<Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>>>,

    line_map: RefCell<Option<Vec<usize>>>,
    classifiable_names: RefCell<Option<Rc<RefCell<HashSet<__String>>>>>,
    comment_directives: RefCell<Option<Vec<CommentDirective>>>,
    resolved_modules: RefCell<Option<ModeAwareCache<Rc<ResolvedModuleFull /*| undefined*/>>>>,
    resolved_type_reference_directive_names:
        RefCell<Option<ModeAwareCache<Rc<ResolvedTypeReferenceDirective /*| undefined*/>>>>,
    pattern_ambient_modules: RefCell<Option<Vec<PatternAmbientModule>>>,
    pragmas: RefCell<Option<ReadonlyPragmaMap>>,

    end_flow_node: RefCell<Option<Rc<FlowNode>>>,

    // TsConfigSourceFile
    extended_source_files: RefCell<Option<Vec<String>>>,
    config_file_specs: RefCell<Option<Rc<ConfigFileSpecs>>>,
}

impl SourceFile {
    pub fn new(
        base_node: BaseNode,
        statements: NodeArray,
        end_of_file_token: Rc<Node>,
        file_name: String,
        text: String,
        language_version: ScriptTarget,
        language_variant: LanguageVariant,
        script_kind: ScriptKind,
        is_declaration_file: bool,
        has_no_default_lib: bool,
    ) -> Self {
        let text_as_chars = text.chars().collect();
        Self {
            _node: base_node,
            _symbols_without_a_symbol_table_strong_references: RefCell::new(vec![]),
            statements,
            end_of_file_token,
            file_name: RefCell::new(file_name),
            path: RefCell::new(None),
            text: RefCell::new(text),
            text_as_chars: RefCell::new(text_as_chars),
            resolved_path: RefCell::new(None),
            original_file_name: RefCell::new(None),
            redirect_info: RefCell::new(None),
            amd_dependencies: RefCell::new(None),
            module_name: RefCell::new(None),
            referenced_files: RefCell::new(None),
            type_reference_directives: RefCell::new(None),
            lib_reference_directives: RefCell::new(None),
            identifiers: RefCell::new(None),
            node_count: Cell::new(None),
            identifier_count: Cell::new(None),
            symbol_count: Cell::new(None),
            parse_diagnostics: RefCell::new(None),
            bind_diagnostics: RefCell::new(None),
            bind_suggestion_diagnostics: RefCell::new(None),
            js_doc_diagnostics: RefCell::new(None),
            line_map: RefCell::new(None),
            classifiable_names: RefCell::new(None),
            language_version: Cell::new(language_version),
            language_variant: Cell::new(language_variant),
            script_kind: Cell::new(script_kind),
            external_module_indicator: RefCell::new(None),
            common_js_module_indicator: RefCell::new(None),
            js_global_augmentations: RefCell::new(None),
            is_declaration_file: Cell::new(is_declaration_file),
            has_no_default_lib: Cell::new(has_no_default_lib),
            comment_directives: RefCell::new(None),
            resolved_modules: RefCell::new(None),
            resolved_type_reference_directive_names: RefCell::new(None),
            pattern_ambient_modules: RefCell::new(None),
            pragmas: RefCell::new(None),
            end_flow_node: RefCell::new(None),
            extended_source_files: RefCell::new(None),
            config_file_specs: RefCell::new(None),
        }
    }

    pub fn file_name(&self) -> Ref<String> {
        self.file_name.borrow()
    }

    pub fn set_file_name(&self, file_name: String) {
        *self.file_name.borrow_mut() = file_name;
    }

    pub fn maybe_path(&self) -> Ref<Option<Path>> {
        self.path.borrow()
    }

    pub fn path(&self) -> Ref<Path> {
        Ref::map(self.path.borrow(), |option| option.as_ref().unwrap())
    }

    pub fn set_path(&self, path: Path) {
        *self.path.borrow_mut() = Some(path);
    }

    pub fn set_text(&self, text: String) {
        *self.text_as_chars.borrow_mut() = text.chars().collect();
        *self.text.borrow_mut() = text;
    }

    pub fn maybe_resolved_path(&self) -> Ref<Option<Path>> {
        self.resolved_path.borrow()
    }

    pub fn set_resolved_path(&self, resolved_path: Option<Path>) {
        *self.resolved_path.borrow_mut() = resolved_path;
    }

    pub fn maybe_original_file_name(&self) -> Ref<Option<String>> {
        self.original_file_name.borrow()
    }

    pub fn original_file_name(&self) -> Ref<String> {
        Ref::map(self.original_file_name.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_original_file_name(&self, original_file_name: Option<String>) {
        *self.original_file_name.borrow_mut() = original_file_name;
    }

    pub fn maybe_redirect_info(&self) -> RefMut<Option<RedirectInfo>> {
        self.redirect_info.borrow_mut()
    }

    pub fn has_no_default_lib(&self) -> bool {
        self.has_no_default_lib.get()
    }

    pub fn set_has_no_default_lib(&self, has_no_default_lib: bool) {
        self.has_no_default_lib.set(has_no_default_lib);
    }

    pub fn language_version(&self) -> ScriptTarget {
        self.language_version.get()
    }

    pub fn set_language_version(&self, language_version: ScriptTarget) {
        self.language_version.set(language_version);
    }

    pub fn script_kind(&self) -> ScriptKind {
        self.script_kind.get()
    }

    pub fn set_script_kind(&self, script_kind: ScriptKind) {
        self.script_kind.set(script_kind);
    }

    pub fn amd_dependencies(&self) -> Ref<Vec<AmdDependency>> {
        Ref::map(self.amd_dependencies.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_amd_dependencies(&self, amd_dependencies: Vec<AmdDependency>) {
        *self.amd_dependencies.borrow_mut() = Some(amd_dependencies);
    }

    pub fn maybe_module_name(&self) -> RefMut<Option<String>> {
        self.module_name.borrow_mut()
    }

    pub fn referenced_files(&self) -> Ref<Vec<FileReference>> {
        Ref::map(self.referenced_files.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_referenced_files(&self, referenced_files: Vec<FileReference>) {
        *self.referenced_files.borrow_mut() = Some(referenced_files);
    }

    pub fn type_reference_directives(&self) -> Ref<Vec<FileReference>> {
        Ref::map(self.type_reference_directives.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_type_reference_directives(&self, type_reference_directives: Vec<FileReference>) {
        *self.type_reference_directives.borrow_mut() = Some(type_reference_directives);
    }

    pub fn lib_reference_directives(&self) -> Ref<Vec<FileReference>> {
        Ref::map(self.lib_reference_directives.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_lib_reference_directives(&self, lib_reference_directives: Vec<FileReference>) {
        *self.lib_reference_directives.borrow_mut() = Some(lib_reference_directives);
    }

    pub fn language_variant(&self) -> LanguageVariant {
        self.language_variant.get()
    }

    pub fn set_language_variant(&self, language_variant: LanguageVariant) {
        self.language_variant.set(language_variant);
    }

    pub fn is_declaration_file(&self) -> bool {
        self.is_declaration_file.get()
    }

    pub fn set_is_declaration_file(&self, is_declaration_file: bool) {
        self.is_declaration_file.set(is_declaration_file);
    }

    pub(crate) fn maybe_external_module_indicator(&self) -> Option<Rc<Node>> {
        self.external_module_indicator.borrow().clone()
    }

    pub(crate) fn set_external_module_indicator(
        &self,
        external_module_indicator: Option<Rc<Node>>,
    ) {
        *self.external_module_indicator.borrow_mut() = external_module_indicator;
    }

    pub(crate) fn maybe_common_js_module_indicator(&self) -> Option<Rc<Node>> {
        self.common_js_module_indicator.borrow().clone()
    }

    pub(crate) fn maybe_common_js_module_indicator_mut(&self) -> RefMut<Option<Rc<Node>>> {
        self.common_js_module_indicator.borrow_mut()
    }

    pub(crate) fn set_common_js_module_indicator(
        &self,
        common_js_module_indicator: Option<Rc<Node>>,
    ) {
        *self.common_js_module_indicator.borrow_mut() = common_js_module_indicator;
    }

    pub(crate) fn maybe_js_global_augmentations(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>> {
        self.js_global_augmentations.borrow_mut()
    }

    pub fn identifiers(&self) -> Rc<RefCell<HashMap<String, String>>> {
        self.identifiers.borrow().clone().unwrap()
    }

    pub fn set_identifiers(&self, identifiers: Rc<RefCell<HashMap<String, String>>>) {
        *self.identifiers.borrow_mut() = Some(identifiers);
    }

    pub fn node_count(&self) -> usize {
        self.node_count.get().unwrap()
    }

    pub fn set_node_count(&self, node_count: usize) {
        self.node_count.set(Some(node_count))
    }

    pub fn identifier_count(&self) -> usize {
        self.identifier_count.get().unwrap()
    }

    pub fn set_identifier_count(&self, identifier_count: usize) {
        self.identifier_count.set(Some(identifier_count))
    }

    pub fn symbol_count(&self) -> usize {
        self.symbol_count.get().unwrap()
    }

    pub fn set_symbol_count(&self, symbol_count: usize) {
        self.symbol_count.set(Some(symbol_count))
    }

    pub fn parse_diagnostics(&self) -> RefMut<Vec<Rc<Diagnostic>>> {
        RefMut::map(self.parse_diagnostics.borrow_mut(), |option| {
            option.as_mut().unwrap()
        })
    }

    pub fn set_parse_diagnostics(&self, parse_diagnostics: Vec<Rc<Diagnostic>>) {
        *self.parse_diagnostics.borrow_mut() = Some(parse_diagnostics);
    }

    pub fn bind_diagnostics(&self) -> RefMut<Vec<Rc<Diagnostic>>> {
        RefMut::map(self.bind_diagnostics.borrow_mut(), |option| {
            option.as_mut().unwrap()
        })
    }

    pub fn set_bind_diagnostics(&self, bind_diagnostics: Option<Vec<Rc<Diagnostic>>>) {
        *self.bind_diagnostics.borrow_mut() = bind_diagnostics;
    }

    pub fn maybe_bind_suggestion_diagnostics(&self) -> RefMut<Option<Vec<Rc<Diagnostic>>>> {
        self.bind_suggestion_diagnostics.borrow_mut()
    }

    pub fn set_bind_suggestion_diagnostics(
        &self,
        bind_suggestion_diagnostics: Option<Vec<Rc<Diagnostic>>>,
    ) {
        *self.bind_suggestion_diagnostics.borrow_mut() = bind_suggestion_diagnostics;
    }

    pub fn maybe_js_doc_diagnostics(&self) -> RefMut<Option<Vec<Rc<Diagnostic>>>> {
        self.js_doc_diagnostics.borrow_mut()
    }

    pub fn set_js_doc_diagnostics(&self, js_doc_diagnostics: Vec<Rc<Diagnostic>>) {
        *self.js_doc_diagnostics.borrow_mut() = Some(js_doc_diagnostics);
    }

    pub fn set_classifiable_names(
        &self,
        classifiable_names: Option<Rc<RefCell<HashSet<__String>>>>,
    ) {
        *self.classifiable_names.borrow_mut() = classifiable_names;
    }

    pub fn maybe_comment_directives(&self) -> Ref<Option<Vec<CommentDirective>>> {
        self.comment_directives.borrow()
    }

    pub fn set_comment_directives(&self, comment_directives: Option<Vec<CommentDirective>>) {
        *self.comment_directives.borrow_mut() = comment_directives;
    }

    pub fn maybe_resolved_modules(&self) -> RefMut<Option<ModeAwareCache<Rc<ResolvedModuleFull>>>> {
        self.resolved_modules.borrow_mut()
    }

    pub fn maybe_resolved_type_reference_directive_names(
        &self,
    ) -> RefMut<Option<ModeAwareCache<Rc<ResolvedTypeReferenceDirective>>>> {
        self.resolved_type_reference_directive_names.borrow_mut()
    }

    pub fn pattern_ambient_modules_mut(&self) -> RefMut<Option<Vec<PatternAmbientModule>>> {
        self.pattern_ambient_modules.borrow_mut()
    }

    pub fn pragmas(&self) -> Ref<ReadonlyPragmaMap> {
        Ref::map(self.pragmas.borrow(), |option| option.as_ref().unwrap())
    }

    pub fn set_pragmas(&self, pragmas: ReadonlyPragmaMap) {
        *self.pragmas.borrow_mut() = Some(pragmas);
    }

    pub fn set_end_flow_node(&self, end_flow_node: Option<Rc<FlowNode>>) {
        *self.end_flow_node.borrow_mut() = end_flow_node;
    }

    pub fn maybe_extended_source_files(&self) -> RefMut<Option<Vec<String>>> {
        self.extended_source_files.borrow_mut()
    }

    pub fn maybe_config_file_specs(&self) -> Ref<Option<Rc<ConfigFileSpecs>>> {
        self.config_file_specs.borrow()
    }

    pub fn set_config_file_specs(&self, config_file_specs: Option<Rc<ConfigFileSpecs>>) {
        *self.config_file_specs.borrow_mut() = config_file_specs;
    }

    pub fn keep_strong_reference_to_symbol(&self, symbol: Rc<Symbol>) {
        self._symbols_without_a_symbol_table_strong_references
            .borrow_mut()
            .push(symbol);
    }
}

impl SourceFileLike for SourceFile {
    fn text(&self) -> Ref<String> {
        self.text.borrow()
    }

    fn text_as_chars(&self) -> Ref<SourceTextAsChars> {
        self.text_as_chars.borrow()
    }

    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>> {
        self.line_map.borrow_mut()
    }

    fn line_map(&self) -> Ref<Vec<usize>> {
        Ref::map(self.line_map.borrow(), |line_map| {
            line_map.as_ref().unwrap()
        })
    }

    fn maybe_get_position_of_line_and_character(
        &self,
        line: usize,
        character: usize,
        allow_edits: Option<bool>,
    ) -> Option<usize> {
        None
    }
}

impl PragmaContext for SourceFile {}

impl HasStatementsInterface for SourceFile {
    fn statements(&self) -> &[Rc<Node>] {
        &self.statements
    }
}

#[derive(Clone, Debug)]
pub struct CommentDirective {
    pub range: BaseTextRange,
    pub type_: CommentDirectiveType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CommentDirectiveType {
    ExpectError,
    Ignore,
}

pub(crate) type ExportedModulesFromDeclarationEmit = Vec<Rc<Symbol>>;

#[derive(Debug)]
#[ast_type]
pub struct Bundle {
    _node: BaseNode,
    pub prepends: Vec<Rc<Node /*InputFiles | UnparsedSource*/>>,
    pub source_files: Vec<Rc<Node /*SourceFile*/>>,
    pub(crate) synthetic_file_references: Option<Vec<FileReference>>,
    pub(crate) synthetic_type_references: Option<Vec<FileReference>>,
    pub(crate) synthetic_lib_references: Option<Vec<FileReference>>,
    pub(crate) has_no_default_lib: Option<bool>,
}

impl Bundle {
    pub fn new(base_node: BaseNode, prepends: Vec<Rc<Node>>, source_files: Vec<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            prepends,
            source_files,
            synthetic_file_references: None,
            synthetic_type_references: None,
            synthetic_lib_references: None,
            has_no_default_lib: None,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct InputFiles {
    _node: BaseNode,
    pub javascript_path: Option<String>,
    pub javascript_text: String,
    pub javascript_map_path: Option<String>,
    pub javascript_map_text: Option<String>,
    pub declaration_path: Option<String>,
    pub declaration_text: String,
    pub declaration_map_path: Option<String>,
    pub declaration_map_text: Option<String>,
    pub(crate) build_info_path: Option<String>,
    pub(crate) build_info: Option<BuildInfo>,
    pub(crate) old_file_of_current_emit: Option<bool>,
}

impl InputFiles {
    pub fn new(base_node: BaseNode, javascript_text: String, declaration_text: String) -> Self {
        Self {
            _node: base_node,
            javascript_text,
            declaration_text,
            javascript_path: None,
            javascript_map_path: None,
            javascript_map_text: None,
            declaration_path: None,
            declaration_map_path: None,
            declaration_map_text: None,
            build_info_path: None,
            build_info: None,
            old_file_of_current_emit: None,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct UnparsedSource {
    _node: BaseNode,
    pub file_name: String,
    pub text: String,
    pub prologues: Vec<Rc<Node /*UnparsedPrologue*/>>,
    pub helpers: Option<Vec<Rc<EmitHelper /*UnscopedEmitHelper*/>>>,

    pub referenced_files: Vec<FileReference>,
    pub type_reference_directives: Option<Vec<String>>,
    pub lib_reference_directives: Vec<FileReference>,
    pub has_no_default_lib: Option<bool>,

    pub source_map_path: Option<String>,
    pub source_map_text: Option<String>,
    pub synthetic_references: Option<Vec<Rc<Node /*UnparsedSyntheticReference*/>>>,
    pub texts: Vec<Rc<Node /*UnparsedSourceText*/>>,
    pub(crate) old_file_of_current_emit: Option<bool>,
}

// TODO: implement SourceFileLike for UnparsedSource
impl UnparsedSource {
    pub fn new(
        base_node: BaseNode,
        prologues: Vec<Rc<Node>>,
        synthetic_references: Option<Vec<Rc<Node>>>,
        texts: Vec<Rc<Node>>,
        file_name: String,
        text: String,
        referenced_files: Vec<FileReference>,
        lib_reference_directives: Vec<FileReference>,
    ) -> Self {
        Self {
            _node: base_node,
            prologues,
            synthetic_references,
            texts,
            file_name,
            text,
            referenced_files,
            lib_reference_directives,
            helpers: None,
            type_reference_directives: None,
            has_no_default_lib: None,
            source_map_path: None,
            source_map_text: None,
            old_file_of_current_emit: None,
        }
    }
}

pub trait UnparsedSectionInterface {
    fn maybe_data(&self) -> Option<&str>;
}

#[derive(Debug)]
#[ast_type(impl_from = false)]
pub struct BaseUnparsedNode {
    _node: BaseNode,
    data: Option<String>,
}

impl BaseUnparsedNode {
    pub fn new(base_node: BaseNode, data: Option<String>) -> Self {
        Self {
            _node: base_node,
            data,
        }
    }
}

impl UnparsedSectionInterface for BaseUnparsedNode {
    fn maybe_data(&self) -> Option<&str> {
        self.data.as_deref()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "UnparsedSectionInterface")]
pub struct UnparsedPrologue {
    _unparsed_node: BaseUnparsedNode,
}

impl UnparsedPrologue {
    pub fn new(base_unparsed_node: BaseUnparsedNode) -> Self {
        Self {
            _unparsed_node: base_unparsed_node,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "UnparsedSectionInterface")]
pub struct UnparsedPrepend {
    _unparsed_node: BaseUnparsedNode,
    texts: Vec<Rc<Node /*UnparsedTextLike*/>>,
}

impl UnparsedPrepend {
    pub fn new(base_unparsed_node: BaseUnparsedNode, texts: Vec<Rc<Node>>) -> Self {
        Self {
            _unparsed_node: base_unparsed_node,
            texts,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "UnparsedSectionInterface")]
pub struct UnparsedTextLike {
    _unparsed_node: BaseUnparsedNode,
}

impl UnparsedTextLike {
    pub fn new(base_unparsed_node: BaseUnparsedNode) -> Self {
        Self {
            _unparsed_node: base_unparsed_node,
        }
    }
}

pub trait ScriptReferenceHost {
    fn get_compiler_options(&self) -> Rc<CompilerOptions>;
    fn get_source_file(&self, file_name: &str) -> Option<Rc<Node /*SourceFile*/>>;
    fn get_source_file_by_path(&self, path: &Path) -> Option<Rc<Node /*SourceFile*/>>;
    fn get_current_directory(&self) -> String;
}

pub trait ParseConfigHost {
    fn use_case_sensitive_file_names(&self) -> bool;

    fn read_directory(
        &self,
        root_dir: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> Vec<String>;

    fn file_exists(&self, path: &str) -> bool;

    fn read_file(&self, path: &str) -> Option<String>;
    fn trace(&self, s: &str) {}
}

pub trait WriteFileCallback {
    fn call(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&dyn FnMut(String)>,
        source_files: Option<&[Rc<Node /*SourceFile*/>]>,
    );
}

pub trait CancellationToken {
    fn is_cancellation_requested(&self) -> bool;

    fn throw_if_cancellation_requested(&self);
}

pub trait CancellationTokenDebuggable: CancellationToken + fmt::Debug {}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FileIncludeKind {
    RootFile,
    SourceFromProjectReference,
    OutputFromProjectReference,
    Import,
    ReferenceFile,
    TypeReferenceDirective,
    LibFile,
    LibReferenceDirective,
    AutomaticTypeDirectiveFile,
}

#[derive(Clone, Debug)]
pub struct RootFile {
    pub kind: FileIncludeKind, /*FileIncludeKind.RootFile*/
    pub index: usize,
}

#[derive(Clone, Debug)]
pub struct LibFile {
    pub kind: FileIncludeKind, /*FileIncludeKind.LibFile*/
    pub index: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct ProjectReferenceFile {
    pub kind: FileIncludeKind, /*ProjectReferenceFileKind*/
    pub index: usize,
}

#[derive(Clone, Debug)]
pub struct ReferencedFile {
    pub kind: FileIncludeKind, /*ReferencedFileKind*/
    pub file: Path,
    pub index: usize,
}

#[derive(Clone, Debug)]
pub struct AutomaticTypeDirectiveFile {
    pub kind: FileIncludeKind, /*FileIncludeKind.AutomaticTypeDirectiveFile*/
    pub type_reference: String,
    pub package_id: Option<PackageId>,
}

#[derive(Clone, Debug)]
pub enum FileIncludeReason {
    RootFile(RootFile),
    LibFile(LibFile),
    ProjectReferenceFile(ProjectReferenceFile),
    ReferencedFile(ReferencedFile),
    AutomaticTypeDirectiveFile(AutomaticTypeDirectiveFile),
}

#[derive(Debug)]
pub struct Program {
    pub(crate) _rc_wrapper: RefCell<Option<Rc<Program>>>,
    pub(crate) options: Rc<CompilerOptions>,
    pub(crate) files: Vec<Rc</*SourceFile*/ Node>>,
    pub(crate) current_directory: String,
    pub(crate) diagnostics_producing_type_checker: RefCell<Option<Rc<TypeChecker>>>,
}
