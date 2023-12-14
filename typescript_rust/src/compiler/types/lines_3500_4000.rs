use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    fmt, io,
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use id_arena::Id;
use local_macros::{ast_type, enum_unwrapped};

use super::{
    BaseNode, BaseTextRange, BuildInfo, CompilerOptions, Diagnostic, EmitHelper, FileReference,
    FlowNode, LanguageVariant, Node, NodeArray, Path, PatternAmbientModule, ReadonlyPragmaMap,
    ResolvedModuleFull, ResolvedTypeReferenceDirective, ScriptKind, ScriptTarget, Symbol,
    SymbolTable, TypeChecker,
};
use crate::{
    get_build_info, ActualResolveModuleNamesWorker, ActualResolveTypeReferenceDirectiveNamesWorker,
    BundleFileSection, CheckJsDirective, CompilerHost, ConfigFileSpecs, CreateProgramOptions,
    Debug_, DiagnosticCache, DiagnosticCollection, DiagnosticMessage, Extension, FilesByNameValue,
    ModeAwareCache, ModuleKind, ModuleResolutionCache, ModuleResolutionHost,
    ModuleResolutionHostOverrider, MultiMap, PackageId, ParseConfigFileHost, PragmaContext,
    ProjectReference, RawSourceMap, ReadFileCallback, RedirectTargetsMap, ResolvedProjectReference,
    SourceOfProjectReferenceRedirect, StructureIsReused, SymlinkCache, Type,
    TypeCheckerHostDebuggable, TypeFlags, TypeInterface, TypeReferenceDirectiveResolutionCache,
    __String, get_line_and_character_of_position, AllArenas, HasArena, LineAndCharacter,
    ProgramBuildInfo,
};

#[derive(Clone, Debug, Trace, Finalize)]
pub enum FlowType {
    Type(Id<Type>),
    IncompleteType(IncompleteType),
}

impl FlowType {
    pub fn flags(&self, arena: &AllArenas) -> TypeFlags {
        match self {
            Self::Type(value) => arena.type_(*value).flags(),
            Self::IncompleteType(value) => value.flags,
        }
    }

    pub fn as_incomplete_type(&self) -> &IncompleteType {
        enum_unwrapped!(self, [FlowType, IncompleteType])
    }

    pub fn as_type(&self) -> &Id<Type> {
        enum_unwrapped!(self, [FlowType, Type])
    }
}

impl From<Id<Type>> for FlowType {
    fn from(value: Id<Type>) -> Self {
        Self::Type(value)
    }
}

impl From<IncompleteType> for FlowType {
    fn from(value: IncompleteType) -> Self {
        Self::IncompleteType(value)
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct IncompleteType {
    #[unsafe_ignore_trace]
    pub flags: TypeFlags,
    pub type_: Id<Type>,
}

impl IncompleteType {
    pub fn new(flags: TypeFlags, type_: Id<Type>) -> Self {
        Self { flags, type_ }
    }
}

pub type SourceTextAsChars = Vec<char>;

pub fn str_to_source_text_as_chars(str_: &str) -> SourceTextAsChars {
    str_.chars().collect()
}

pub fn text_len(text: &SourceTextAsChars) -> usize {
    text.len()
}

pub fn maybe_text_char_at_index(text: &SourceTextAsChars, index: usize) -> Option<char> {
    text.get(index).copied()
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

#[derive(Clone, Debug)]
pub struct AmdDependency {
    pub path: String,
    pub name: Option<String>,
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

#[derive(Clone, Debug, Trace, Finalize)]
pub struct RedirectInfo {
    pub redirect_target: Id<Node /*SourceFile*/>,
    pub unredirected: Id<Node /*SourceFile*/>,
}

pub trait HasStatementsInterface {
    fn statements(&self) -> Gc<NodeArray>;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct SourceFile {
    _node: BaseNode,
    contents: Box<SourceFileContents>,
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct SourceFileContents {
    statements: Gc<NodeArray>,
    end_of_file_token: Id<Node /*Token<SyntaxFile.EndOfFileToken>*/>,

    #[unsafe_ignore_trace]
    file_name: RefCell<String>,
    #[unsafe_ignore_trace]
    path: RefCell<Option<Path>>,
    #[unsafe_ignore_trace]
    text: RefCell<String>,
    #[unsafe_ignore_trace]
    text_as_chars: RefCell<SourceTextAsChars>,
    #[unsafe_ignore_trace]
    resolved_path: RefCell<Option<Path>>,
    #[unsafe_ignore_trace]
    original_file_name: RefCell<Option<String>>,

    redirect_info: GcCell<Option<RedirectInfo>>,

    #[unsafe_ignore_trace]
    amd_dependencies: RefCell<Option<Vec<AmdDependency>>>,
    #[unsafe_ignore_trace]
    module_name: RefCell<Option<String>>,
    #[unsafe_ignore_trace]
    referenced_files: RefCell<Option<Rc<RefCell<Vec<FileReference>>>>>,
    #[unsafe_ignore_trace]
    type_reference_directives: RefCell<Option<Rc<RefCell<Vec<FileReference>>>>>,
    #[unsafe_ignore_trace]
    lib_reference_directives: RefCell<Option<Rc<RefCell<Vec<FileReference>>>>>,
    #[unsafe_ignore_trace]
    language_variant: Cell<LanguageVariant>,
    #[unsafe_ignore_trace]
    is_declaration_file: Cell<bool>,

    renamed_dependencies: GcCell<Option<HashMap<String, String>>>,

    #[unsafe_ignore_trace]
    has_no_default_lib: Cell<bool>,

    #[unsafe_ignore_trace]
    language_version: Cell<ScriptTarget>,

    #[unsafe_ignore_trace]
    implied_node_format: Cell<Option<ModuleKind>>,

    #[unsafe_ignore_trace]
    script_kind: Cell<ScriptKind>,

    external_module_indicator: GcCell<Option<Id<Node>>>,
    common_js_module_indicator: GcCell<Option<Id<Node>>>,
    js_global_augmentations: GcCell<Option<Gc<GcCell<SymbolTable>>>>,

    #[unsafe_ignore_trace]
    identifiers: RefCell<Option<Rc<RefCell<HashMap<String, String>>>>>,
    #[unsafe_ignore_trace]
    node_count: Cell<Option<usize>>,
    #[unsafe_ignore_trace]
    identifier_count: Cell<Option<usize>>,
    #[unsafe_ignore_trace]
    symbol_count: Cell<Option<usize>>,

    parse_diagnostics: GcCell<Option<Gc<GcCell<Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>>>>>>,

    bind_diagnostics: GcCell<Option<Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>>>>,
    bind_suggestion_diagnostics: GcCell<Option<Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>>>>,

    js_doc_diagnostics: GcCell<Option<Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>>>>,

    additional_syntactic_diagnostics:
        GcCell<Option<Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>>>>,

    #[unsafe_ignore_trace]
    line_map: RefCell<Option<Vec<usize>>>,
    #[unsafe_ignore_trace]
    classifiable_names: RefCell<Option<Rc<RefCell<HashSet<__String>>>>>,
    #[unsafe_ignore_trace]
    comment_directives: RefCell<Option<Vec<Rc<CommentDirective>>>>,
    resolved_modules:
        GcCell<Option<ModeAwareCache<Option<Gc<ResolvedModuleFull /*| undefined*/>>>>>,
    resolved_type_reference_directive_names:
        GcCell<Option<ModeAwareCache<Option<Gc<ResolvedTypeReferenceDirective>>>>>,
    imports: GcCell<Option<Vec<Id<Node /*StringLiteralLike*/>>>>,
    module_augmentations: GcCell<Option<Vec<Id<Node /*StringLiteral | Identifier*/>>>>,
    pattern_ambient_modules: GcCell<Option<Vec<Gc<PatternAmbientModule>>>>,
    #[unsafe_ignore_trace]
    ambient_module_names: RefCell<Option<Vec<String>>>,
    #[unsafe_ignore_trace]
    check_js_directive: RefCell<Option<CheckJsDirective>>,
    #[unsafe_ignore_trace]
    pragmas: RefCell<Option<ReadonlyPragmaMap>>,
    #[unsafe_ignore_trace]
    local_jsx_namespace: RefCell<Option<__String>>,
    #[unsafe_ignore_trace]
    local_jsx_fragment_namespace: RefCell<Option<__String>>,
    local_jsx_factory: GcCell<Option<Id<Node>>>,
    local_jsx_fragment_factory: GcCell<Option<Id<Node>>>,

    exported_modules_from_declaration_emit: GcCell<Option<ExportedModulesFromDeclarationEmit>>,
    end_flow_node: GcCell<Option<Gc<FlowNode>>>,

    // TsConfigSourceFile
    #[unsafe_ignore_trace]
    extended_source_files: RefCell<Option<Vec<String>>>,
    #[unsafe_ignore_trace]
    config_file_specs: RefCell<Option<Rc<ConfigFileSpecs>>>,
}

impl SourceFile {
    pub fn new(
        base_node: BaseNode,
        statements: Gc<NodeArray>,
        end_of_file_token: Id<Node>,
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
            contents: Box::new(SourceFileContents {
                statements,
                end_of_file_token,
                file_name: RefCell::new(file_name),
                path: Default::default(),
                text: RefCell::new(text),
                text_as_chars: RefCell::new(text_as_chars),
                resolved_path: Default::default(),
                original_file_name: Default::default(),
                redirect_info: Default::default(),
                amd_dependencies: Default::default(),
                module_name: Default::default(),
                referenced_files: Default::default(),
                type_reference_directives: Default::default(),
                lib_reference_directives: Default::default(),
                identifiers: Default::default(),
                node_count: Default::default(),
                identifier_count: Default::default(),
                symbol_count: Default::default(),
                parse_diagnostics: Default::default(),
                bind_diagnostics: Default::default(),
                bind_suggestion_diagnostics: Default::default(),
                js_doc_diagnostics: Default::default(),
                additional_syntactic_diagnostics: Default::default(),
                line_map: Default::default(),
                classifiable_names: Default::default(),
                language_version: Cell::new(language_version),
                implied_node_format: Default::default(),
                language_variant: Cell::new(language_variant),
                script_kind: Cell::new(script_kind),
                external_module_indicator: Default::default(),
                common_js_module_indicator: Default::default(),
                js_global_augmentations: Default::default(),
                is_declaration_file: Cell::new(is_declaration_file),
                has_no_default_lib: Cell::new(has_no_default_lib),
                renamed_dependencies: Default::default(),
                comment_directives: Default::default(),
                resolved_modules: Default::default(),
                resolved_type_reference_directive_names: Default::default(),
                imports: Default::default(),
                module_augmentations: Default::default(),
                pattern_ambient_modules: Default::default(),
                ambient_module_names: Default::default(),
                pragmas: Default::default(),
                check_js_directive: Default::default(),
                local_jsx_namespace: Default::default(),
                local_jsx_fragment_namespace: Default::default(),
                local_jsx_factory: Default::default(),
                local_jsx_fragment_factory: Default::default(),
                exported_modules_from_declaration_emit: Default::default(),
                end_flow_node: Default::default(),
                extended_source_files: Default::default(),
                config_file_specs: Default::default(),
            }),
        }
    }

    pub fn set_statements(&mut self, statements: Gc<NodeArray>) {
        self.contents.statements = statements;
    }

    pub fn end_of_file_token(&self) -> Id<Node> {
        self.contents.end_of_file_token.clone()
    }

    pub fn set_end_of_file_token(&mut self, end_of_file_token: Id<Node>) {
        self.contents.end_of_file_token = end_of_file_token;
    }

    pub fn file_name(&self) -> Ref<String> {
        self.contents.file_name.borrow()
    }

    pub fn set_file_name(&self, file_name: String) {
        *self.contents.file_name.borrow_mut() = file_name;
    }

    pub fn maybe_path(&self) -> Ref<Option<Path>> {
        self.contents.path.borrow()
    }

    pub fn path(&self) -> Ref<Path> {
        Ref::map(self.contents.path.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_path(&self, path: Path) {
        *self.contents.path.borrow_mut() = Some(path);
    }

    pub fn set_text(&self, text: String) {
        *self.contents.text_as_chars.borrow_mut() = text.chars().collect();
        *self.contents.text.borrow_mut() = text;
    }

    pub fn maybe_resolved_path(&self) -> Ref<Option<Path>> {
        self.contents.resolved_path.borrow()
    }

    pub fn set_resolved_path(&self, resolved_path: Option<Path>) {
        *self.contents.resolved_path.borrow_mut() = resolved_path;
    }

    pub fn maybe_original_file_name(&self) -> Ref<Option<String>> {
        self.contents.original_file_name.borrow()
    }

    pub fn original_file_name(&self) -> Ref<String> {
        Ref::map(self.contents.original_file_name.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_original_file_name(&self, original_file_name: Option<String>) {
        *self.contents.original_file_name.borrow_mut() = original_file_name;
    }

    pub fn maybe_redirect_info(&self) -> GcCellRef<Option<RedirectInfo>> {
        self.contents.redirect_info.borrow()
    }

    pub fn maybe_redirect_info_mut(&self) -> GcCellRefMut<Option<RedirectInfo>> {
        self.contents.redirect_info.borrow_mut()
    }

    pub fn maybe_renamed_dependencies(&self) -> GcCellRef<Option<HashMap<String, String>>> {
        self.contents.renamed_dependencies.borrow()
    }

    pub fn has_no_default_lib(&self) -> bool {
        self.contents.has_no_default_lib.get()
    }

    pub fn set_has_no_default_lib(&self, has_no_default_lib: bool) {
        self.contents.has_no_default_lib.set(has_no_default_lib);
    }

    pub fn language_version(&self) -> ScriptTarget {
        self.contents.language_version.get()
    }

    pub fn set_language_version(&self, language_version: ScriptTarget) {
        self.contents.language_version.set(language_version);
    }

    pub fn script_kind(&self) -> ScriptKind {
        self.contents.script_kind.get()
    }

    pub fn set_script_kind(&self, script_kind: ScriptKind) {
        self.contents.script_kind.set(script_kind);
    }

    pub fn maybe_amd_dependencies(&self) -> Ref<Option<Vec<AmdDependency>>> {
        self.contents.amd_dependencies.borrow()
    }

    pub fn amd_dependencies(&self) -> Ref<Vec<AmdDependency>> {
        Ref::map(self.contents.amd_dependencies.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_amd_dependencies(&self, amd_dependencies: Vec<AmdDependency>) {
        *self.contents.amd_dependencies.borrow_mut() = Some(amd_dependencies);
    }

    pub fn maybe_module_name(&self) -> RefMut<Option<String>> {
        self.contents.module_name.borrow_mut()
    }

    pub fn maybe_referenced_files(&self) -> Option<Rc<RefCell<Vec<FileReference>>>> {
        self.contents.referenced_files.borrow().clone()
    }

    pub fn referenced_files(&self) -> Rc<RefCell<Vec<FileReference>>> {
        self.contents.referenced_files.borrow().clone().unwrap()
    }

    pub fn set_referenced_files(&self, referenced_files: Rc<RefCell<Vec<FileReference>>>) {
        *self.contents.referenced_files.borrow_mut() = Some(referenced_files);
    }

    pub fn maybe_type_reference_directives(&self) -> Option<Rc<RefCell<Vec<FileReference>>>> {
        self.contents.type_reference_directives.borrow().clone()
    }

    pub fn type_reference_directives(&self) -> Rc<RefCell<Vec<FileReference>>> {
        self.contents
            .type_reference_directives
            .borrow()
            .clone()
            .unwrap()
    }

    pub fn set_type_reference_directives(
        &self,
        type_reference_directives: Rc<RefCell<Vec<FileReference>>>,
    ) {
        *self.contents.type_reference_directives.borrow_mut() = Some(type_reference_directives);
    }

    pub fn maybe_lib_reference_directives(&self) -> Option<Rc<RefCell<Vec<FileReference>>>> {
        self.contents.lib_reference_directives.borrow().clone()
    }

    pub fn lib_reference_directives(&self) -> Rc<RefCell<Vec<FileReference>>> {
        self.contents
            .lib_reference_directives
            .borrow()
            .clone()
            .unwrap()
    }

    pub fn set_lib_reference_directives(
        &self,
        lib_reference_directives: Rc<RefCell<Vec<FileReference>>>,
    ) {
        *self.contents.lib_reference_directives.borrow_mut() = Some(lib_reference_directives);
    }

    pub fn language_variant(&self) -> LanguageVariant {
        self.contents.language_variant.get()
    }

    pub fn set_language_variant(&self, language_variant: LanguageVariant) {
        self.contents.language_variant.set(language_variant);
    }

    pub fn is_declaration_file(&self) -> bool {
        self.contents.is_declaration_file.get()
    }

    pub fn set_is_declaration_file(&self, is_declaration_file: bool) {
        self.contents.is_declaration_file.set(is_declaration_file);
    }

    pub(crate) fn maybe_implied_node_format(&self) -> Option<ModuleKind> {
        self.contents.implied_node_format.get()
    }

    pub(crate) fn set_implied_node_format(&self, implied_node_format: Option<ModuleKind>) {
        self.contents.implied_node_format.set(implied_node_format);
    }

    pub(crate) fn maybe_external_module_indicator(&self) -> Option<Id<Node>> {
        self.contents.external_module_indicator.borrow().clone()
    }

    pub(crate) fn set_external_module_indicator(
        &self,
        external_module_indicator: Option<Id<Node>>,
    ) {
        *self.contents.external_module_indicator.borrow_mut() = external_module_indicator;
    }

    pub(crate) fn maybe_common_js_module_indicator(&self) -> Option<Id<Node>> {
        self.contents.common_js_module_indicator.borrow().clone()
    }

    pub(crate) fn maybe_common_js_module_indicator_mut(&self) -> GcCellRefMut<Option<Id<Node>>> {
        self.contents.common_js_module_indicator.borrow_mut()
    }

    pub(crate) fn maybe_js_global_augmentations(
        &self,
    ) -> GcCellRefMut<Option<Gc<GcCell<SymbolTable>>>> {
        self.contents.js_global_augmentations.borrow_mut()
    }

    pub fn identifiers(&self) -> Rc<RefCell<HashMap<String, String>>> {
        self.contents.identifiers.borrow().clone().unwrap()
    }

    pub fn set_identifiers(&self, identifiers: Rc<RefCell<HashMap<String, String>>>) {
        *self.contents.identifiers.borrow_mut() = Some(identifiers);
    }

    pub fn node_count(&self) -> usize {
        self.contents.node_count.get().unwrap()
    }

    pub fn set_node_count(&self, node_count: usize) {
        self.contents.node_count.set(Some(node_count))
    }

    pub fn identifier_count(&self) -> usize {
        self.contents.identifier_count.get().unwrap()
    }

    pub fn set_identifier_count(&self, identifier_count: usize) {
        self.contents.identifier_count.set(Some(identifier_count))
    }

    pub fn symbol_count(&self) -> usize {
        self.contents.symbol_count.get().unwrap()
    }

    pub fn set_symbol_count(&self, symbol_count: usize) {
        self.contents.symbol_count.set(Some(symbol_count))
    }

    pub fn parse_diagnostics(&self) -> Gc<GcCell<Vec<Gc<Diagnostic>>>> {
        self.contents.parse_diagnostics.borrow().clone().unwrap()
    }

    pub fn set_parse_diagnostics(&self, parse_diagnostics: Gc<GcCell<Vec<Gc<Diagnostic>>>>) {
        *self.contents.parse_diagnostics.borrow_mut() = Some(parse_diagnostics);
    }

    pub fn maybe_bind_diagnostics(&self) -> GcCellRef<Option<Vec<Gc<Diagnostic>>>> {
        self.contents.bind_diagnostics.borrow()
    }

    pub fn bind_diagnostics(&self) -> GcCellRef<Vec<Gc<Diagnostic>>> {
        GcCellRef::map(self.contents.bind_diagnostics.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn bind_diagnostics_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Diagnostic>>>, Vec<Gc<Diagnostic>>> {
        GcCellRefMut::map(self.contents.bind_diagnostics.borrow_mut(), |option| {
            option.as_mut().unwrap()
        })
    }

    pub fn set_bind_diagnostics(&self, bind_diagnostics: Option<Vec<Gc<Diagnostic>>>) {
        *self.contents.bind_diagnostics.borrow_mut() = bind_diagnostics;
    }

    pub fn maybe_bind_suggestion_diagnostics(&self) -> GcCellRefMut<Option<Vec<Gc<Diagnostic>>>> {
        self.contents.bind_suggestion_diagnostics.borrow_mut()
    }

    pub fn set_bind_suggestion_diagnostics(
        &self,
        bind_suggestion_diagnostics: Option<Vec<Gc<Diagnostic>>>,
    ) {
        *self.contents.bind_suggestion_diagnostics.borrow_mut() = bind_suggestion_diagnostics;
    }

    pub fn maybe_js_doc_diagnostics(&self) -> GcCellRefMut<Option<Vec<Gc<Diagnostic>>>> {
        self.contents.js_doc_diagnostics.borrow_mut()
    }

    pub fn set_js_doc_diagnostics(&self, js_doc_diagnostics: Vec<Gc<Diagnostic>>) {
        *self.contents.js_doc_diagnostics.borrow_mut() = Some(js_doc_diagnostics);
    }

    pub fn maybe_additional_syntactic_diagnostics_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Diagnostic>>>> {
        self.contents.additional_syntactic_diagnostics.borrow_mut()
    }

    pub fn set_classifiable_names(
        &self,
        classifiable_names: Option<Rc<RefCell<HashSet<__String>>>>,
    ) {
        *self.contents.classifiable_names.borrow_mut() = classifiable_names;
    }

    pub fn maybe_comment_directives(&self) -> Ref<Option<Vec<Rc<CommentDirective>>>> {
        self.contents.comment_directives.borrow()
    }

    pub fn comment_directives(&self) -> Ref<Vec<Rc<CommentDirective>>> {
        Ref::map(
            self.contents.comment_directives.borrow(),
            |comment_directives| comment_directives.as_ref().unwrap(),
        )
    }

    pub fn set_comment_directives(&self, comment_directives: Option<Vec<Rc<CommentDirective>>>) {
        *self.contents.comment_directives.borrow_mut() = comment_directives;
    }

    pub fn maybe_resolved_modules(
        &self,
    ) -> GcCellRefMut<Option<ModeAwareCache<Option<Gc<ResolvedModuleFull>>>>> {
        self.contents.resolved_modules.borrow_mut()
    }

    pub fn maybe_resolved_type_reference_directive_names(
        &self,
    ) -> GcCellRefMut<Option<ModeAwareCache<Option<Gc<ResolvedTypeReferenceDirective>>>>> {
        self.contents
            .resolved_type_reference_directive_names
            .borrow_mut()
    }

    pub fn maybe_imports(&self) -> GcCellRef<Option<Vec<Id<Node>>>> {
        self.contents.imports.borrow()
    }

    pub fn imports(&self) -> GcCellRef<Vec<Id<Node>>> {
        GcCellRef::map(self.contents.imports.borrow(), |imports| {
            imports.as_ref().unwrap()
        })
    }

    pub fn maybe_imports_mut(&self) -> GcCellRefMut<Option<Vec<Id<Node>>>> {
        self.contents.imports.borrow_mut()
    }

    pub fn maybe_module_augmentations(&self) -> GcCellRef<Option<Vec<Id<Node>>>> {
        self.contents.module_augmentations.borrow()
    }

    pub fn module_augmentations(&self) -> GcCellRef<Vec<Id<Node>>> {
        GcCellRef::map(
            self.contents.module_augmentations.borrow(),
            |module_augmentations| module_augmentations.as_ref().unwrap(),
        )
    }

    pub fn maybe_module_augmentations_mut(&self) -> GcCellRefMut<Option<Vec<Id<Node>>>> {
        self.contents.module_augmentations.borrow_mut()
    }

    pub fn maybe_pattern_ambient_modules(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<PatternAmbientModule>>>> {
        self.contents.pattern_ambient_modules.borrow_mut()
    }

    pub fn maybe_ambient_module_names(&self) -> RefMut<Option<Vec<String>>> {
        self.contents.ambient_module_names.borrow_mut()
    }

    pub fn maybe_check_js_directive(&self) -> Ref<Option<CheckJsDirective>> {
        self.contents.check_js_directive.borrow()
    }

    pub fn maybe_check_js_directive_mut(&self) -> RefMut<Option<CheckJsDirective>> {
        self.contents.check_js_directive.borrow_mut()
    }

    pub fn pragmas(&self) -> Ref<ReadonlyPragmaMap> {
        Ref::map(self.contents.pragmas.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_pragmas(&self, pragmas: ReadonlyPragmaMap) {
        *self.contents.pragmas.borrow_mut() = Some(pragmas);
    }

    pub fn maybe_local_jsx_namespace(&self) -> RefMut<Option<__String>> {
        self.contents.local_jsx_namespace.borrow_mut()
    }

    pub fn maybe_local_jsx_fragment_namespace(&self) -> RefMut<Option<__String>> {
        self.contents.local_jsx_fragment_namespace.borrow_mut()
    }

    pub fn maybe_local_jsx_factory(&self) -> GcCellRefMut<Option<Id<Node>>> {
        self.contents.local_jsx_factory.borrow_mut()
    }

    pub fn maybe_local_jsx_fragment_factory(&self) -> GcCellRefMut<Option<Id<Node>>> {
        self.contents.local_jsx_fragment_factory.borrow_mut()
    }

    pub fn set_end_flow_node(&self, end_flow_node: Option<Gc<FlowNode>>) {
        *self.contents.end_flow_node.borrow_mut() = end_flow_node;
    }

    pub fn maybe_extended_source_files(&self) -> RefMut<Option<Vec<String>>> {
        self.contents.extended_source_files.borrow_mut()
    }

    pub fn maybe_config_file_specs(&self) -> Ref<Option<Rc<ConfigFileSpecs>>> {
        self.contents.config_file_specs.borrow()
    }

    pub fn set_config_file_specs(&self, config_file_specs: Option<Rc<ConfigFileSpecs>>) {
        *self.contents.config_file_specs.borrow_mut() = config_file_specs;
    }

    pub fn maybe_exported_modules_from_declaration_emit(
        &self,
    ) -> GcCellRefMut<Option<Vec<Id<Symbol>>>> {
        self.contents
            .exported_modules_from_declaration_emit
            .borrow_mut()
    }

    pub fn maybe_end_flow_node(&self) -> GcCellRefMut<Option<Gc<FlowNode>>> {
        self.contents.end_flow_node.borrow_mut()
    }
}

impl SourceFileLike for SourceFile {
    fn text(&self) -> Ref<String> {
        self.contents.text.borrow()
    }

    fn text_as_chars(&self) -> Ref<SourceTextAsChars> {
        self.contents.text_as_chars.borrow()
    }

    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>> {
        self.contents.line_map.borrow_mut()
    }

    fn line_map(&self) -> Ref<Vec<usize>> {
        Ref::map(self.contents.line_map.borrow(), |line_map| {
            line_map.as_ref().unwrap()
        })
    }

    fn maybe_get_position_of_line_and_character(
        &self,
        _line: usize,
        _character: usize,
        _allow_edits: Option<bool>,
    ) -> Option<usize> {
        None
    }
}

impl PragmaContext for SourceFile {
    fn language_version(&self) -> ScriptTarget {
        self.language_version()
    }

    fn maybe_pragmas(&self) -> RefMut<Option<ReadonlyPragmaMap>> {
        self.contents.pragmas.borrow_mut()
    }

    fn maybe_check_js_directive(&self) -> RefMut<Option<CheckJsDirective>> {
        self.contents.check_js_directive.borrow_mut()
    }

    fn maybe_referenced_files_mut(&self) -> RefMut<Option<Rc<RefCell<Vec<FileReference>>>>> {
        self.contents.referenced_files.borrow_mut()
    }

    fn maybe_type_reference_directives_mut(
        &self,
    ) -> RefMut<Option<Rc<RefCell<Vec<FileReference>>>>> {
        self.contents.type_reference_directives.borrow_mut()
    }

    fn maybe_lib_reference_directives_mut(
        &self,
    ) -> RefMut<Option<Rc<RefCell<Vec<FileReference>>>>> {
        self.contents.lib_reference_directives.borrow_mut()
    }

    fn maybe_amd_dependencies(&self) -> RefMut<Option<Vec<AmdDependency>>> {
        self.contents.amd_dependencies.borrow_mut()
    }

    fn maybe_has_no_default_lib(&self) -> Option<bool> {
        Some(self.contents.has_no_default_lib.get())
    }

    fn set_has_no_default_lib(&self, has_no_default_lib: bool) {
        self.contents.has_no_default_lib.set(has_no_default_lib);
    }

    fn maybe_module_name(&self) -> RefMut<Option<String>> {
        self.contents.module_name.borrow_mut()
    }
}

impl HasStatementsInterface for SourceFile {
    fn statements(&self) -> Gc<NodeArray> {
        self.contents.statements.clone()
    }
}

impl HasFileNameInterface for SourceFile {
    fn file_name(&self) -> String {
        self.file_name().clone()
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

pub(crate) type ExportedModulesFromDeclarationEmit = Vec<Id<Symbol>>;

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct Bundle {
    _node: BaseNode,
    pub prepends: Vec<Id<Node /*InputFiles | UnparsedSource*/>>,
    pub source_files: Vec<Option<Id<Node /*SourceFile*/>>>,
    #[unsafe_ignore_trace]
    pub(crate) synthetic_file_references: Option<Vec<FileReference>>,
    #[unsafe_ignore_trace]
    pub(crate) synthetic_type_references: Option<Vec<FileReference>>,
    #[unsafe_ignore_trace]
    pub(crate) synthetic_lib_references: Option<Vec<FileReference>>,
    pub(crate) has_no_default_lib: Option<bool>,
}

impl Bundle {
    pub fn new(
        base_node: BaseNode,
        prepends: Vec<Id<Node>>,
        source_files: Vec<Option<Id<Node>>>,
    ) -> Self {
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

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct InputFiles {
    _node: BaseNode,
    fallback_javascript_text: String,
    fallback_declaration_text: String,
    initialized_state: Gc<InputFilesInitializedState>,
    pub javascript_path: Option<String>,
    pub javascript_map_path: Option<String>,
    pub declaration_path: Option<String>,
    pub declaration_map_path: Option<String>,
    pub(crate) build_info_path: Option<String>,
    pub(crate) old_file_of_current_emit: Option<bool>,
}

impl InputFiles {
    pub fn new(
        base_node: BaseNode,
        fallback_javascript_text: String,
        fallback_declaration_text: String,
    ) -> Self {
        Self {
            _node: base_node,
            fallback_javascript_text,
            fallback_declaration_text,
            initialized_state: Default::default(),
            javascript_path: None,
            javascript_map_path: None,
            declaration_path: None,
            declaration_map_path: None,
            build_info_path: None,
            old_file_of_current_emit: None,
        }
    }

    pub fn initialize_with_read_file_callback(
        &mut self,
        read_file_callback: Gc<Box<dyn ReadFileCallback>>,
        declaration_text_or_javascript_path: String,
        javascript_map_path: Option<String>,
        javascript_map_text_or_declaration_path: Option<String>,
        declaration_map_path: Option<String>,
        declaration_map_text_or_build_info_path: Option<String>,
    ) {
        let javascript_map_text_or_declaration_path =
            Debug_.check_defined(javascript_map_text_or_declaration_path, None);
        self.initialized_state =
            Gc::new(InputFilesInitializedState::InitializedWithReadFileCallback(
                InputFilesInitializedWithReadFileCallback::new(
                    read_file_callback,
                    declaration_text_or_javascript_path.clone(),
                    javascript_map_path.clone(),
                    javascript_map_text_or_declaration_path.clone(),
                    declaration_map_path.clone(),
                    declaration_map_text_or_build_info_path.clone(),
                ),
            ));
        self.javascript_path = Some(declaration_text_or_javascript_path);
        self.javascript_map_path = javascript_map_path;
        self.declaration_path = Some(javascript_map_text_or_declaration_path);
        self.declaration_map_path = declaration_map_path;
        self.build_info_path = declaration_map_text_or_build_info_path;
    }

    pub fn initialize_with_string(
        &mut self,
        javascript_text_or_read_file_text: String,
        javascript_map_path: Option<String>,
        javascript_map_text_or_declaration_path: Option<String>,
        declaration_text_or_javascript_path: String,
        declaration_map_path: Option<String>,
        declaration_map_text_or_build_info_path: Option<String>,
        javascript_path: Option<String>,
        declaration_path: Option<String>,
        build_info_path: Option<String>,
        build_info: Option<Gc<BuildInfo>>,
        old_file_of_current_emit: Option<bool>,
    ) {
        self.initialized_state = Gc::new(InputFilesInitializedState::InitializedWithString(
            InputFilesInitializedWithString::new(
                javascript_text_or_read_file_text,
                javascript_map_text_or_declaration_path,
                declaration_text_or_javascript_path,
                declaration_map_text_or_build_info_path,
                build_info,
            ),
        ));
        self.javascript_map_path = javascript_map_path;
        self.declaration_map_path = declaration_map_path;
        self.javascript_path = javascript_path;
        self.declaration_path = declaration_path;
        self.build_info_path = build_info_path;
        self.old_file_of_current_emit = old_file_of_current_emit;
    }

    pub fn javascript_text(&self) -> String {
        match &*self.initialized_state {
            InputFilesInitializedState::Uninitialized => self.fallback_javascript_text.clone(),
            InputFilesInitializedState::InitializedWithReadFileCallback(
                initialized_with_read_file_callback,
            ) => initialized_with_read_file_callback.defined_text_getter(
                &initialized_with_read_file_callback.declaration_text_or_javascript_path,
            ),
            InputFilesInitializedState::InitializedWithString(initialized_with_string) => {
                initialized_with_string.javascript_text.clone()
            }
        }
    }

    pub fn javascript_map_text(&self) -> Option<String> {
        match &*self.initialized_state {
            InputFilesInitializedState::Uninitialized => None,
            InputFilesInitializedState::InitializedWithReadFileCallback(
                initialized_with_read_file_callback,
            ) => initialized_with_read_file_callback.text_getter(
                initialized_with_read_file_callback
                    .javascript_map_path
                    .as_deref(),
            ),
            InputFilesInitializedState::InitializedWithString(initialized_with_string) => {
                initialized_with_string.javascript_map_text.clone()
            }
        }
    }

    pub fn declaration_text(&self) -> String {
        match &*self.initialized_state {
            InputFilesInitializedState::Uninitialized => self.fallback_declaration_text.clone(),
            InputFilesInitializedState::InitializedWithReadFileCallback(
                initialized_with_read_file_callback,
            ) => initialized_with_read_file_callback.defined_text_getter(
                &initialized_with_read_file_callback.javascript_map_text_or_declaration_path,
            ),
            InputFilesInitializedState::InitializedWithString(initialized_with_string) => {
                initialized_with_string.declaration_text.clone()
            }
        }
    }

    pub fn declaration_map_text(&self) -> Option<String> {
        match &*self.initialized_state {
            InputFilesInitializedState::Uninitialized => None,
            InputFilesInitializedState::InitializedWithReadFileCallback(
                initialized_with_read_file_callback,
            ) => initialized_with_read_file_callback.text_getter(
                initialized_with_read_file_callback
                    .declaration_map_path
                    .as_deref(),
            ),
            InputFilesInitializedState::InitializedWithString(initialized_with_string) => {
                initialized_with_string.declaration_map_text.clone()
            }
        }
    }

    pub fn build_info(&self) -> Option<Gc<BuildInfo>> {
        match &*self.initialized_state {
            InputFilesInitializedState::Uninitialized => None,
            InputFilesInitializedState::InitializedWithReadFileCallback(
                initialized_with_read_file_callback,
            ) => initialized_with_read_file_callback.get_and_cache_build_info(|| {
                initialized_with_read_file_callback.text_getter(
                    initialized_with_read_file_callback
                        .declaration_map_text_or_build_info_path
                        .as_deref(),
                )
            }),
            InputFilesInitializedState::InitializedWithString(initialized_with_string) => {
                initialized_with_string.build_info.clone()
            }
        }
    }
}

pub trait HasOldFileOfCurrentEmitInterface {
    fn maybe_old_file_of_current_emit(&self) -> Option<bool>;
}

impl HasOldFileOfCurrentEmitInterface for InputFiles {
    fn maybe_old_file_of_current_emit(&self) -> Option<bool> {
        self.old_file_of_current_emit
    }
}

#[derive(Debug, Trace, Finalize)]
enum InputFilesInitializedState {
    Uninitialized,
    InitializedWithReadFileCallback(InputFilesInitializedWithReadFileCallback),
    InitializedWithString(InputFilesInitializedWithString),
}

impl Default for InputFilesInitializedState {
    fn default() -> Self {
        Self::Uninitialized
    }
}

#[derive(Debug, Trace, Finalize)]
struct InputFilesInitializedWithReadFileCallback {
    read_file_callback: Gc<Box<dyn ReadFileCallback>>,
    #[unsafe_ignore_trace]
    cache: RefCell<HashMap<String, Option<String>>>,
    declaration_text_or_javascript_path: String,
    javascript_map_path: Option<String>,
    javascript_map_text_or_declaration_path: String,
    declaration_map_path: Option<String>,
    declaration_map_text_or_build_info_path: Option<String>,
    #[unsafe_ignore_trace]
    build_info: GcCell<Option<Option<Gc<BuildInfo>>>>,
}

impl InputFilesInitializedWithReadFileCallback {
    pub fn new(
        read_file_callback: Gc<Box<dyn ReadFileCallback>>,
        declaration_text_or_javascript_path: String,
        javascript_map_path: Option<String>,
        javascript_map_text_or_declaration_path: String,
        declaration_map_path: Option<String>,
        declaration_map_text_or_build_info_path: Option<String>,
    ) -> Self {
        Self {
            read_file_callback,
            cache: Default::default(),
            declaration_text_or_javascript_path,
            javascript_map_path,
            javascript_map_text_or_declaration_path,
            declaration_map_path,
            declaration_map_text_or_build_info_path,
            build_info: Default::default(),
        }
    }

    fn cache_mut(&self) -> RefMut<HashMap<String, Option<String>>> {
        self.cache.borrow_mut()
    }

    fn text_getter(&self, path: Option<&str>) -> Option<String> {
        let path = path?;
        let mut cache = self.cache_mut();
        let mut value = cache.get(path).cloned();
        if value.is_none() {
            value = self.read_file_callback.call(path).map(Some);
            cache.insert(
                path.to_owned(),
                match value.clone() {
                    Some(Some(value)) => Some(value),
                    None => None,
                    _ => unreachable!(),
                },
            );
        }
        value.flatten()
    }

    fn defined_text_getter(&self, path: &str) -> String {
        let result = self.text_getter(Some(path));
        result.unwrap_or_else(|| format!("/* Input file {path} was missing */\r\n"))
    }

    fn get_and_cache_build_info(
        &self,
        mut get_text: impl FnMut() -> Option<String>,
    ) -> Option<Gc<BuildInfo>> {
        let mut build_info = self.build_info.borrow_mut();
        if build_info.is_none() {
            let result = get_text();
            *build_info = Some(result.map(|result| get_build_info(&result)));
        }
        build_info.clone().unwrap()
    }
}

#[derive(Debug, Trace, Finalize)]
struct InputFilesInitializedWithString {
    javascript_text: String,
    javascript_map_text: Option<String>,
    declaration_text: String,
    declaration_map_text: Option<String>,
    build_info: Option<Gc<BuildInfo>>,
}

impl InputFilesInitializedWithString {
    pub fn new(
        javascript_text: String,
        javascript_map_text: Option<String>,
        declaration_text: String,
        declaration_map_text: Option<String>,
        build_info: Option<Gc<BuildInfo>>,
    ) -> Self {
        Self {
            javascript_text,
            javascript_map_text,
            declaration_text,
            declaration_map_text,
            build_info,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct UnparsedSource {
    _node: BaseNode,
    pub file_name: String,
    #[unsafe_ignore_trace]
    text: RefCell<String>,
    #[unsafe_ignore_trace]
    text_as_chars: RefCell<SourceTextAsChars>,
    pub prologues: Vec<Id<Node /*UnparsedPrologue*/>>,
    pub helpers: Option<Vec<Gc<EmitHelper /*UnscopedEmitHelper*/>>>,

    #[unsafe_ignore_trace]
    pub referenced_files: Vec<FileReference>,
    #[unsafe_ignore_trace]
    pub type_reference_directives: Option<Vec<String>>,
    #[unsafe_ignore_trace]
    pub lib_reference_directives: Vec<FileReference>,
    pub has_no_default_lib: Option<bool>,

    pub source_map_path: Option<String>,
    pub source_map_text: Option<String>,
    pub synthetic_references: Option<Vec<Id<Node /*UnparsedSyntheticReference*/>>>,
    pub texts: Vec<Id<Node /*UnparsedSourceText*/>>,
    pub(crate) old_file_of_current_emit: Option<bool>,
    #[unsafe_ignore_trace]
    pub(crate) parsed_source_map: RefCell<Option<Option<Rc<RawSourceMap>>>>,
}

impl UnparsedSource {
    pub fn new(
        base_node: BaseNode,
        prologues: Vec<Id<Node>>,
        synthetic_references: Option<Vec<Id<Node>>>,
        texts: Vec<Id<Node>>,
        file_name: String,
        text: String,
        referenced_files: Vec<FileReference>,
        lib_reference_directives: Vec<FileReference>,
    ) -> Self {
        let text_as_chars = text.chars().collect::<Vec<_>>();
        Self {
            _node: base_node,
            prologues,
            synthetic_references,
            texts,
            file_name,
            text: RefCell::new(text),
            text_as_chars: RefCell::new(text_as_chars),
            referenced_files,
            lib_reference_directives,
            helpers: None,
            type_reference_directives: None,
            has_no_default_lib: None,
            source_map_path: None,
            source_map_text: None,
            old_file_of_current_emit: None,
            parsed_source_map: RefCell::new(None),
        }
    }

    pub fn get_line_and_character_of_position(&self, pos: usize) -> LineAndCharacter {
        get_line_and_character_of_position(self, pos)
    }
}

impl HasTextsInterface for UnparsedSource {
    fn texts(&self) -> &[Id<Node>] {
        &self.texts
    }
}

impl HasOldFileOfCurrentEmitInterface for UnparsedSource {
    fn maybe_old_file_of_current_emit(&self) -> Option<bool> {
        self.old_file_of_current_emit
    }
}

impl SourceFileLike for UnparsedSource {
    fn text(&self) -> Ref<String> {
        self.text.borrow()
    }

    fn text_as_chars(&self) -> Ref<SourceTextAsChars> {
        self.text_as_chars.borrow()
    }

    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>> {
        unimplemented!()
    }

    fn line_map(&self) -> Ref<Vec<usize>> {
        unimplemented!()
    }

    fn maybe_get_position_of_line_and_character(
        &self,
        _line: usize,
        _character: usize,
        _allow_edits: Option<bool>,
    ) -> Option<usize> {
        unimplemented!()
    }
}

pub trait HasFileNameInterface {
    fn file_name(&self) -> String;
}

impl HasFileNameInterface for UnparsedSource {
    fn file_name(&self) -> String {
        self.file_name.clone()
    }
}

pub trait UnparsedSectionInterface {
    fn maybe_data(&self) -> Option<&str>;
}

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "UnparsedSectionInterface")]
pub struct UnparsedPrepend {
    _unparsed_node: BaseUnparsedNode,
    texts: Vec<Id<Node /*UnparsedTextLike*/>>,
}

impl UnparsedPrepend {
    pub fn new(base_unparsed_node: BaseUnparsedNode, texts: Vec<Id<Node>>) -> Self {
        Self {
            _unparsed_node: base_unparsed_node,
            texts,
        }
    }
}

pub trait HasTextsInterface {
    fn texts(&self) -> &[Id<Node>];
}

impl HasTextsInterface for UnparsedPrepend {
    fn texts(&self) -> &[Id<Node>] {
        &self.texts
    }
}

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "UnparsedSectionInterface")]
pub struct UnparsedSyntheticReference {
    _unparsed_node: BaseUnparsedNode,
    pub section: Gc<BundleFileSection>,
}

impl UnparsedSyntheticReference {
    pub fn new(base_unparsed_node: BaseUnparsedNode, section: Gc<BundleFileSection>) -> Self {
        Self {
            _unparsed_node: base_unparsed_node,
            section,
        }
    }
}

pub trait ScriptReferenceHost {
    fn get_compiler_options(&self) -> Gc<CompilerOptions>;
    fn get_source_file(&self, file_name: &str) -> Option<Id<Node /*SourceFile*/>>;
    fn get_source_file_by_path(&self, path: &Path) -> Option<Id<Node /*SourceFile*/>>;
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
    ) -> io::Result<Vec<String>>;

    fn file_exists(&self, path: &str) -> bool;

    fn read_file(&self, path: &str) -> io::Result<Option<String>>;
    fn trace(&self, _s: &str) {}
    fn is_trace_supported(&self) -> bool;
    fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost;
}

pub trait WriteFileCallback: Trace + Finalize {
    fn call(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        source_files: Option<&[Id<Node /*SourceFile*/>]>,
    ) -> io::Result<()>;
}

pub trait CancellationToken: Trace + Finalize {
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

#[derive(Clone, Debug, Eq, PartialEq, Trace, Finalize)]
pub struct RootFile {
    #[unsafe_ignore_trace]
    pub kind: FileIncludeKind, /*FileIncludeKind.RootFile*/
    pub index: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Trace, Finalize)]
pub struct LibFile {
    #[unsafe_ignore_trace]
    pub kind: FileIncludeKind, /*FileIncludeKind.LibFile*/
    pub index: Option<usize>,
}

#[derive(Clone, Debug, Eq, PartialEq, Trace, Finalize)]
pub struct ProjectReferenceFile {
    #[unsafe_ignore_trace]
    pub kind: FileIncludeKind, /*ProjectReferenceFileKind*/
    pub index: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Trace, Finalize)]
pub struct ReferencedFile {
    #[unsafe_ignore_trace]
    pub kind: FileIncludeKind, /*ReferencedFileKind*/
    #[unsafe_ignore_trace]
    pub file: Path,
    pub index: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Trace, Finalize)]
pub struct AutomaticTypeDirectiveFile {
    #[unsafe_ignore_trace]
    pub kind: FileIncludeKind, /*FileIncludeKind.AutomaticTypeDirectiveFile*/
    pub type_reference: String,
    #[unsafe_ignore_trace]
    pub package_id: Option<PackageId>,
}

#[derive(Clone, Debug, Eq, PartialEq, Trace, Finalize)]
pub enum FileIncludeReason {
    RootFile(RootFile),
    LibFile(LibFile),
    ProjectReferenceFile(ProjectReferenceFile),
    ReferencedFile(ReferencedFile),
    AutomaticTypeDirectiveFile(AutomaticTypeDirectiveFile),
}

impl FileIncludeReason {
    pub fn kind(&self) -> FileIncludeKind {
        match self {
            Self::RootFile(value) => value.kind,
            Self::LibFile(value) => value.kind,
            Self::ProjectReferenceFile(value) => value.kind,
            Self::ReferencedFile(value) => value.kind,
            Self::AutomaticTypeDirectiveFile(value) => value.kind,
        }
    }

    pub fn as_root_file(&self) -> &RootFile {
        enum_unwrapped!(self, [FileIncludeReason, RootFile])
    }

    pub fn as_referenced_file(&self) -> &ReferencedFile {
        enum_unwrapped!(self, [FileIncludeReason, ReferencedFile])
    }

    pub fn as_project_reference_file(&self) -> &ProjectReferenceFile {
        enum_unwrapped!(self, [FileIncludeReason, ProjectReferenceFile])
    }

    pub fn as_automatic_type_directive_file(&self) -> &AutomaticTypeDirectiveFile {
        enum_unwrapped!(self, [FileIncludeReason, AutomaticTypeDirectiveFile])
    }

    pub fn as_lib_file(&self) -> &LibFile {
        enum_unwrapped!(self, [FileIncludeReason, LibFile])
    }
}

#[derive(Copy, Clone, Debug)]
pub enum FilePreprocessingDiagnosticsKind {
    FilePreprocessingReferencedDiagnostic,
    FilePreprocessingFileExplainingDiagnostic,
}

#[derive(Trace, Finalize)]
pub struct FilePreprocessingReferencedDiagnostic {
    #[unsafe_ignore_trace]
    pub kind: FilePreprocessingDiagnosticsKind, /*FilePreprocessingDiagnosticsKind.FilePreprocessingReferencedDiagnostic*/
    pub reason: ReferencedFile,
    pub diagnostic: &'static DiagnosticMessage,
    pub args: Option<Vec<String>>,
}

#[derive(Trace, Finalize)]
pub struct FilePreprocessingFileExplainingDiagnostic {
    #[unsafe_ignore_trace]
    pub kind: FilePreprocessingDiagnosticsKind, /*FilePreprocessingDiagnosticsKind.FilePreprocessingFileExplainingDiagnostic*/
    #[unsafe_ignore_trace]
    pub file: Option<Path>,
    pub file_processing_reason: Gc<FileIncludeReason>,
    pub diagnostic: &'static DiagnosticMessage,
    pub args: Option<Vec<String>>,
}

#[derive(Trace, Finalize)]
pub enum FilePreprocessingDiagnostics {
    FilePreprocessingReferencedDiagnostic(FilePreprocessingReferencedDiagnostic),
    FilePreprocessingFileExplainingDiagnostic(FilePreprocessingFileExplainingDiagnostic),
}

impl FilePreprocessingDiagnostics {
    pub fn kind(&self) -> FilePreprocessingDiagnosticsKind {
        match self {
            Self::FilePreprocessingReferencedDiagnostic(value) => value.kind,
            Self::FilePreprocessingFileExplainingDiagnostic(value) => value.kind,
        }
    }

    pub fn as_referenced_diagnostic(&self) -> &FilePreprocessingReferencedDiagnostic {
        enum_unwrapped!(
            self,
            [
                FilePreprocessingDiagnostics,
                FilePreprocessingReferencedDiagnostic
            ]
        )
    }

    pub fn as_file_explaining_diagnostic(&self) -> &FilePreprocessingFileExplainingDiagnostic {
        enum_unwrapped!(
            self,
            [
                FilePreprocessingDiagnostics,
                FilePreprocessingFileExplainingDiagnostic
            ]
        )
    }
}

#[derive(Trace, Finalize)]
pub struct Program {
    pub(crate) _rc_wrapper: GcCell<Option<Gc<Box<Program>>>>,
    pub(crate) _dyn_type_checker_host_debuggable_wrapper:
        GcCell<Option<Gc<Box<dyn TypeCheckerHostDebuggable>>>>,
    pub(crate) create_program_options: GcCell<Option<CreateProgramOptions>>,
    #[unsafe_ignore_trace]
    pub(crate) root_names: RefCell<Option<Vec<String>>>,
    pub(crate) options: Gc<CompilerOptions>,
    pub(crate) config_file_parsing_diagnostics: GcCell<Option<Vec<Gc<Diagnostic>>>>,
    #[unsafe_ignore_trace]
    pub(crate) project_references: RefCell<Option<Vec<Rc<ProjectReference>>>>,
    pub(crate) processing_default_lib_files: GcCell<Option<Vec<Id</*SourceFile*/ Node>>>>,
    pub(crate) processing_other_files: GcCell<Option<Vec<Id</*SourceFile*/ Node>>>>,
    pub(crate) files: GcCell<Option<Vec<Id</*SourceFile*/ Node>>>>,
    pub(crate) symlinks: GcCell<Option<Gc<SymlinkCache>>>,
    #[unsafe_ignore_trace]
    pub(crate) common_source_directory: RefCell<Option<String>>,
    pub(crate) diagnostics_producing_type_checker: GcCell<Option<Gc<TypeChecker>>>,
    pub(crate) no_diagnostics_type_checker: GcCell<Option<Gc<TypeChecker>>>,
    #[allow(dead_code)]
    #[unsafe_ignore_trace]
    pub(crate) classifiable_names: RefCell<Option<HashSet<__String>>>,
    #[unsafe_ignore_trace]
    pub(crate) ambient_module_name_to_unmodified_file_name: RefCell<HashMap<String, String>>,
    pub(crate) file_reasons: GcCell<Gc<GcCell<MultiMap<Path, Gc<FileIncludeReason>>>>>,
    pub(crate) cached_bind_and_check_diagnostics_for_file: GcCell<DiagnosticCache>,
    pub(crate) cached_declaration_diagnostics_for_file: GcCell<DiagnosticCache>,

    pub(crate) resolved_type_reference_directives:
        GcCell<Gc<GcCell<HashMap<String, Option<Gc<ResolvedTypeReferenceDirective>>>>>>,
    pub(crate) file_processing_diagnostics: GcCell<Option<Vec<Gc<FilePreprocessingDiagnostics>>>>,

    pub(crate) max_node_module_js_depth: usize,
    #[unsafe_ignore_trace]
    pub(crate) current_node_modules_depth: Cell<usize>,

    #[unsafe_ignore_trace]
    pub(crate) modules_with_elided_imports: RefCell<HashMap<String, bool>>,

    #[unsafe_ignore_trace]
    pub(crate) source_files_found_searching_node_modules: RefCell<HashMap<String, bool>>,

    pub(crate) old_program: GcCell<Option<Gc<Box<Program>>>>,
    pub(crate) host: GcCell<Option<Gc<Box<dyn CompilerHost>>>>,
    pub(crate) config_parsing_host: GcCell<Option<Gc<Box<dyn ParseConfigFileHost>>>>,

    #[unsafe_ignore_trace]
    pub(crate) skip_default_lib: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    pub(crate) get_default_library_file_name_memoized: RefCell<Option<String>>,
    #[unsafe_ignore_trace]
    pub(crate) default_library_path: RefCell<Option<String>>,
    pub(crate) program_diagnostics: GcCell<Option<DiagnosticCollection>>,
    #[unsafe_ignore_trace]
    pub(crate) current_directory: RefCell<Option<String>>,
    #[unsafe_ignore_trace]
    pub(crate) supported_extensions: RefCell<Option<Vec<Vec<Extension>>>>,
    #[unsafe_ignore_trace]
    pub(crate) supported_extensions_with_json_if_resolve_json_module:
        RefCell<Option<Vec<Vec<Extension>>>>,

    #[unsafe_ignore_trace]
    pub(crate) has_emit_blocking_diagnostics: RefCell<Option<HashMap<Path, bool>>>,
    pub(crate) _compiler_options_object_literal_syntax:
        GcCell<Option<Option<Id<Node /*ObjectLiteralExpression*/>>>>,
    pub(crate) module_resolution_cache: GcCell<Option<Gc<ModuleResolutionCache>>>,
    pub(crate) type_reference_directive_resolution_cache:
        GcCell<Option<Gc<TypeReferenceDirectiveResolutionCache>>>,
    pub(crate) actual_resolve_module_names_worker:
        GcCell<Option<Gc<Box<dyn ActualResolveModuleNamesWorker>>>>,
    pub(crate) actual_resolve_type_reference_directive_names_worker:
        GcCell<Option<Gc<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>>>>,

    pub(crate) package_id_to_source_file: GcCell<Option<HashMap<String, Id<Node /*SourceFile*/>>>>,
    #[unsafe_ignore_trace]
    pub(crate) source_file_to_package_name: RefCell<Option<HashMap<Path, String>>>,
    #[unsafe_ignore_trace]
    pub(crate) redirect_targets_map: RefCell<Rc<RefCell<RedirectTargetsMap>>>,
    #[unsafe_ignore_trace]
    pub(crate) uses_uri_style_node_core_modules: Cell<Option<bool>>,

    pub(crate) files_by_name: GcCell<Option<HashMap<String, FilesByNameValue>>>,
    #[unsafe_ignore_trace]
    pub(crate) missing_file_paths: RefCell<Option<Vec<Path>>>,
    pub(crate) files_by_name_ignore_case: GcCell<Option<HashMap<String, Id<Node /*SourceFile*/>>>>,

    pub(crate) resolved_project_references:
        GcCell<Option<Vec<Option<Gc<ResolvedProjectReference>>>>>,
    pub(crate) project_reference_redirects:
        GcCell<Option<HashMap<Path, Option<Gc<ResolvedProjectReference>>>>>,
    #[unsafe_ignore_trace]
    pub(crate) map_from_file_to_project_reference_redirects: RefCell<Option<HashMap<Path, Path>>>,
    #[unsafe_ignore_trace]
    pub(crate) map_from_to_project_reference_redirect_source:
        RefCell<Option<HashMap<Path, SourceOfProjectReferenceRedirect>>>,
    #[unsafe_ignore_trace]
    pub(crate) use_source_of_project_reference_redirect: Cell<Option<bool>>,

    pub(crate) file_exists_rc: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,
    pub(crate) directory_exists_rc: GcCell<Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>>,

    #[unsafe_ignore_trace]
    pub(crate) should_create_new_source_file: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    pub(crate) structure_is_reused: Cell<Option<StructureIsReused>>,

    pub(crate) get_program_build_info: GcCell<Option<Gc<Box<dyn GetProgramBuildInfo>>>>,
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Program").finish()
    }
}

pub trait GetProgramBuildInfo: Trace + Finalize {
    fn call(&self) -> Option<Gc<ProgramBuildInfo>>;
}
