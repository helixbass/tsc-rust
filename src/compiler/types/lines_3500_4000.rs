#![allow(non_upper_case_globals)]

use std::cell::{Cell, Ref, RefCell, RefMut};
use std::rc::Rc;

use super::{
    BaseNode, BaseTextRange, BuildInfo, Diagnostic, EmitHelper, FileReference, LanguageVariant,
    Node, NodeArray, Path, ReadonlyPragmaMap, ScriptKind, ScriptTarget, Symbol, TypeCheckerHost,
};
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

    amd_dependencies: RefCell<Option<Vec<AmdDependency>>>,
    referenced_files: RefCell<Option<Vec<FileReference>>>,
    type_reference_directives: RefCell<Option<Vec<FileReference>>>,
    lib_reference_directives: RefCell<Option<Vec<FileReference>>>,
    language_variant: LanguageVariant,
    is_declaration_file: bool,

    has_no_default_lib: Cell<bool>,

    language_version: ScriptTarget,

    pub(crate) script_kind: ScriptKind,

    external_module_indicator: RefCell<Option<Rc<Node>>>,

    parse_diagnostics: RefCell<Option<Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>>>,

    line_map: RefCell<Option<Vec<usize>>>,
    pragmas: RefCell<Option<ReadonlyPragmaMap>>,
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
            amd_dependencies: RefCell::new(None),
            referenced_files: RefCell::new(None),
            type_reference_directives: RefCell::new(None),
            lib_reference_directives: RefCell::new(None),
            parse_diagnostics: RefCell::new(None),
            line_map: RefCell::new(None),
            language_version,
            language_variant,
            script_kind,
            external_module_indicator: RefCell::new(None),
            is_declaration_file,
            has_no_default_lib: Cell::new(has_no_default_lib),
            pragmas: RefCell::new(None),
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

    pub fn set_path(&self, path: Path) {
        *self.path.borrow_mut() = Some(path);
    }

    pub fn set_text(&self, text: String) {
        *self.text_as_chars.borrow_mut() = text.chars().collect();
        *self.text.borrow_mut() = text;
    }

    pub fn has_no_default_lib(&self) -> bool {
        self.has_no_default_lib.get()
    }

    pub fn set_has_no_default_lib(&self, has_no_default_lib: bool) {
        self.has_no_default_lib.set(has_no_default_lib);
    }

    pub fn amd_dependencies(&self) -> Ref<Vec<AmdDependency>> {
        Ref::map(self.amd_dependencies.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_amd_dependencies(&self, amd_dependencies: Vec<AmdDependency>) {
        *self.amd_dependencies.borrow_mut() = Some(amd_dependencies);
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

    pub(crate) fn maybe_external_module_indicator(&self) -> Option<Rc<Node>> {
        self.external_module_indicator.borrow().clone()
    }

    pub(crate) fn set_external_module_indicator(
        &self,
        external_module_indicator: Option<Rc<Node>>,
    ) {
        *self.external_module_indicator.borrow_mut() = external_module_indicator;
    }

    pub fn parse_diagnostics(&self) -> RefMut<Vec<Rc<Diagnostic>>> {
        RefMut::map(self.parse_diagnostics.borrow_mut(), |option| {
            option.as_mut().unwrap()
        })
    }

    pub fn set_parse_diagnostics(&self, parse_diagnostics: Vec<Rc<Diagnostic>>) {
        *self.parse_diagnostics.borrow_mut() = Some(parse_diagnostics);
    }

    pub fn pragmas(&self) -> Ref<ReadonlyPragmaMap> {
        Ref::map(self.pragmas.borrow(), |option| option.as_ref().unwrap())
    }

    pub fn set_pragmas(&self, pragmas: ReadonlyPragmaMap) {
        *self.pragmas.borrow_mut() = Some(pragmas);
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

pub trait Program: TypeCheckerHost {
    fn get_syntactic_diagnostics(&mut self) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>;
    fn get_semantic_diagnostics(&mut self) -> Vec<Rc<Diagnostic>>;
}
