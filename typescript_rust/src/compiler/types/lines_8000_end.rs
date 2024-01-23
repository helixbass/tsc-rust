use std::{
    cell::{Cell, RefCell},
    collections::{HashMap, HashSet},
    io,
    iter::FromIterator,
    rc::Rc,
};

use bitflags::bitflags;
use derive_builder::Builder;
use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;
use local_macros::{ast_type, enum_unwrapped};
use serde::Serialize;

use super::{BaseNode, CommentDirective, Diagnostic, Node, Symbol, SymbolFlags, SymbolWriter};
use crate::{
    AllArenas, BaseNodeFactorySynthetic, CommentRange, EmitBinaryExpression, EmitHint,
    FileIncludeReason, LineAndCharacter, ModuleKind, MultiMap, NewLineKind, NodeArray, NodeId,
    ParenthesizerRules, Path, ProgramBuildInfo, RedirectTargetsMap, ScriptTarget, SortedArray,
    SourceMapSource, SymlinkCache, SyntaxKind, TempFlags, TextRange,
};

#[derive(Trace, Finalize)]
pub struct Printer {
    #[unsafe_ignore_trace]
    pub arena: *const AllArenas,
    pub _rc_wrapper: GcCell<Option<Gc<Printer>>>,
    pub printer_options: PrinterOptions,
    pub handlers: Gc<Box<dyn PrintHandlers>>,
    pub extended_diagnostics: bool,
    pub new_line: String,
    #[unsafe_ignore_trace]
    pub module_kind: ModuleKind,
    pub current_source_file: GcCell<Option<Id<Node /*SourceFile*/>>>,
    #[unsafe_ignore_trace]
    pub bundled_helpers: RefCell<HashMap<String, bool>>,
    #[unsafe_ignore_trace]
    pub node_id_to_generated_name: RefCell<HashMap<NodeId, String>>,
    #[unsafe_ignore_trace]
    pub auto_generated_id_to_generated_name: RefCell<HashMap<usize, String>>,
    #[unsafe_ignore_trace]
    pub generated_names: RefCell<HashSet<String>>,
    #[unsafe_ignore_trace]
    pub temp_flags_stack: RefCell<Vec<TempFlags>>,
    #[unsafe_ignore_trace]
    pub temp_flags: Cell<TempFlags>,
    #[unsafe_ignore_trace]
    pub reserved_names_stack: RefCell<Vec<Option<Rc<RefCell<HashSet<String>>>>>>,
    #[unsafe_ignore_trace]
    pub reserved_names: RefCell<Option<Rc<RefCell<HashSet<String>>>>>,
    #[unsafe_ignore_trace]
    pub preserve_source_newlines: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    pub next_list_element_pos: Cell<Option<isize>>,

    pub writer: GcCell<Option<Gc<Box<dyn EmitTextWriter>>>>,
    pub own_writer: GcCell<Option<Gc<Box<dyn EmitTextWriter>>>>,
    #[unsafe_ignore_trace]
    pub write: Cell<fn(&Printer, &str)>,
    #[unsafe_ignore_trace]
    pub is_own_file_emit: Cell<bool>,
    pub bundle_file_info: GcCell<Option<Gc<GcCell<BundleFileInfo>>>>,
    pub relative_to_build_info: Option<Gc<Box<dyn RelativeToBuildInfo>>>,
    pub record_internal_section: Option<bool>,
    #[unsafe_ignore_trace]
    pub source_file_text_pos: Cell<usize>,
    #[unsafe_ignore_trace]
    pub source_file_text_kind: Cell<BundleFileSectionKind>,

    #[unsafe_ignore_trace]
    pub source_maps_disabled: Cell<bool>,
    pub source_map_generator: GcCell<Option<Gc<Box<dyn SourceMapGenerator>>>>,
    pub source_map_source: GcCell<Option<Gc<SourceMapSource>>>,
    #[unsafe_ignore_trace]
    pub source_map_source_index: Cell<isize>,
    pub most_recently_added_source_map_source: GcCell<Option<Gc<SourceMapSource>>>,
    #[unsafe_ignore_trace]
    pub most_recently_added_source_map_source_index: Cell<isize>,
    #[unsafe_ignore_trace]
    pub container_pos: Cell<isize>,
    #[unsafe_ignore_trace]
    pub container_end: Cell<isize>,
    #[unsafe_ignore_trace]
    pub declaration_list_container_end: Cell<isize>,
    #[unsafe_ignore_trace]
    pub current_line_map: RefCell<Option<Vec<usize>>>,
    #[unsafe_ignore_trace]
    pub detached_comments_info: RefCell<Option<Vec<DetachedCommentInfo>>>,
    #[unsafe_ignore_trace]
    pub has_written_comment: Cell<bool>,
    #[unsafe_ignore_trace]
    pub comments_disabled: Cell<bool>,
    pub last_substitution: GcCell<Option<Id<Node>>>,
    pub current_parenthesizer_rule: GcCell<Option<Gc<Box<dyn CurrentParenthesizerRule>>>>,
    pub parenthesizer: Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>>,
    pub emit_binary_expression: GcCell<Option<Gc<EmitBinaryExpression>>>,
}

pub trait CurrentParenthesizerRule: Trace + Finalize {
    fn call(&self, node: Id<Node>) -> Id<Node>;
}

#[derive(Copy, Clone)]
pub struct DetachedCommentInfo {
    pub node_pos: isize,
    pub detached_comment_end_pos: isize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize)]
pub enum BundleFileSectionKind {
    #[serde(rename = "prologue")]
    Prologue,
    #[serde(rename = "emitHelpers")]
    EmitHelpers,
    #[serde(rename = "no-default-lib")]
    NoDefaultLib,
    #[serde(rename = "reference")]
    Reference,
    #[serde(rename = "type")]
    Type,
    #[serde(rename = "lib")]
    Lib,
    #[serde(rename = "prepend")]
    Prepend,
    #[serde(rename = "text")]
    Text,
    #[serde(rename = "internal")]
    Internal,
}

impl BundleFileSectionKind {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Prologue => "prologue",
            Self::EmitHelpers => "emitHelpers",
            Self::NoDefaultLib => "no-default-lib",
            Self::Reference => "reference",
            Self::Type => "type",
            Self::Lib => "lib",
            Self::Prepend => "prepend",
            Self::Text => "text",
            Self::Internal => "internal",
        }
    }
}

#[derive(Clone, Debug, Serialize, Trace, Finalize)]
pub struct BundleFileSectionBase {
    #[unsafe_ignore_trace]
    pos: Cell<isize>,
    #[unsafe_ignore_trace]
    end: Cell<isize>,
    #[unsafe_ignore_trace]
    kind: BundleFileSectionKind,
    data: Option<String>,
}

impl BundleFileSectionBase {
    pub fn new(pos: isize, end: isize, kind: BundleFileSectionKind, data: Option<String>) -> Self {
        Self {
            pos: Cell::new(pos),
            end: Cell::new(end),
            kind,
            data,
        }
    }
}

impl TextRange for BundleFileSectionBase {
    fn pos(&self) -> isize {
        self.pos.get()
    }

    fn set_pos(&self, pos: isize) {
        self.pos.set(pos);
    }

    fn end(&self) -> isize {
        self.end.get()
    }

    fn set_end(&self, end: isize) {
        self.end.set(end);
    }
}

pub trait BundleFileSectionInterface: TextRange {
    fn kind(&self) -> BundleFileSectionKind;
    fn maybe_data(&self) -> Option<&String>;
}

impl BundleFileSectionInterface for BundleFileSectionBase {
    fn kind(&self) -> BundleFileSectionKind {
        self.kind
    }

    fn maybe_data(&self) -> Option<&String> {
        self.data.as_ref()
    }
}

#[derive(Clone, Debug, Serialize, Trace, Finalize)]
pub struct BundleFilePrologue {
    #[serde(flatten)]
    _bundle_file_section_base: BundleFileSectionBase,
}

impl BundleFilePrologue {
    pub fn data(&self) -> &String {
        self.maybe_data().unwrap()
    }
}

impl TextRange for BundleFilePrologue {
    fn pos(&self) -> isize {
        self._bundle_file_section_base.pos()
    }

    fn set_pos(&self, pos: isize) {
        self._bundle_file_section_base.set_pos(pos)
    }

    fn end(&self) -> isize {
        self._bundle_file_section_base.end()
    }

    fn set_end(&self, end: isize) {
        self._bundle_file_section_base.set_end(end)
    }
}

impl BundleFileSectionInterface for BundleFilePrologue {
    fn kind(&self) -> BundleFileSectionKind {
        self._bundle_file_section_base.kind()
    }

    fn maybe_data(&self) -> Option<&String> {
        self._bundle_file_section_base.maybe_data()
    }
}

#[derive(Clone, Debug, Serialize, Trace, Finalize)]
pub struct BundleFileEmitHelpers {
    #[serde(flatten)]
    _bundle_file_section_base: BundleFileSectionBase,
}

impl BundleFileEmitHelpers {
    pub fn data(&self) -> &String {
        self.maybe_data().unwrap()
    }
}

impl TextRange for BundleFileEmitHelpers {
    fn pos(&self) -> isize {
        self._bundle_file_section_base.pos()
    }

    fn set_pos(&self, pos: isize) {
        self._bundle_file_section_base.set_pos(pos)
    }

    fn end(&self) -> isize {
        self._bundle_file_section_base.end()
    }

    fn set_end(&self, end: isize) {
        self._bundle_file_section_base.set_end(end)
    }
}

impl BundleFileSectionInterface for BundleFileEmitHelpers {
    fn kind(&self) -> BundleFileSectionKind {
        self._bundle_file_section_base.kind()
    }

    fn maybe_data(&self) -> Option<&String> {
        self._bundle_file_section_base.maybe_data()
    }
}

#[derive(Clone, Debug, Serialize, Trace, Finalize)]
pub struct BundleFileHasNoDefaultLib {
    #[serde(flatten)]
    _bundle_file_section_base: BundleFileSectionBase,
}

impl TextRange for BundleFileHasNoDefaultLib {
    fn pos(&self) -> isize {
        self._bundle_file_section_base.pos()
    }

    fn set_pos(&self, pos: isize) {
        self._bundle_file_section_base.set_pos(pos)
    }

    fn end(&self) -> isize {
        self._bundle_file_section_base.end()
    }

    fn set_end(&self, end: isize) {
        self._bundle_file_section_base.set_end(end)
    }
}

impl BundleFileSectionInterface for BundleFileHasNoDefaultLib {
    fn kind(&self) -> BundleFileSectionKind {
        self._bundle_file_section_base.kind()
    }

    fn maybe_data(&self) -> Option<&String> {
        self._bundle_file_section_base.maybe_data()
    }
}

#[derive(Clone, Debug, Serialize, Trace, Finalize)]
pub struct BundleFileReference {
    #[serde(flatten)]
    _bundle_file_section_base: BundleFileSectionBase,
}

impl BundleFileReference {
    pub fn data(&self) -> &String {
        self.maybe_data().unwrap()
    }
}

impl TextRange for BundleFileReference {
    fn pos(&self) -> isize {
        self._bundle_file_section_base.pos()
    }

    fn set_pos(&self, pos: isize) {
        self._bundle_file_section_base.set_pos(pos)
    }

    fn end(&self) -> isize {
        self._bundle_file_section_base.end()
    }

    fn set_end(&self, end: isize) {
        self._bundle_file_section_base.set_end(end)
    }
}

impl BundleFileSectionInterface for BundleFileReference {
    fn kind(&self) -> BundleFileSectionKind {
        self._bundle_file_section_base.kind()
    }

    fn maybe_data(&self) -> Option<&String> {
        self._bundle_file_section_base.maybe_data()
    }
}

#[derive(Clone, Debug, Serialize, Trace, Finalize)]
pub struct BundleFilePrepend {
    #[serde(flatten)]
    _bundle_file_section_base: BundleFileSectionBase,
    pub texts: Vec<Gc<BundleFileSection /*BundleFileTextLike*/>>,
}

impl BundleFilePrepend {
    pub fn data(&self) -> &String {
        self.maybe_data().unwrap()
    }
}

impl TextRange for BundleFilePrepend {
    fn pos(&self) -> isize {
        self._bundle_file_section_base.pos()
    }

    fn set_pos(&self, pos: isize) {
        self._bundle_file_section_base.set_pos(pos)
    }

    fn end(&self) -> isize {
        self._bundle_file_section_base.end()
    }

    fn set_end(&self, end: isize) {
        self._bundle_file_section_base.set_end(end)
    }
}

impl BundleFileSectionInterface for BundleFilePrepend {
    fn kind(&self) -> BundleFileSectionKind {
        self._bundle_file_section_base.kind()
    }

    fn maybe_data(&self) -> Option<&String> {
        self._bundle_file_section_base.maybe_data()
    }
}

#[derive(Clone, Debug, Serialize, Trace, Finalize)]
pub struct BundleFileTextLike {
    #[serde(flatten)]
    _bundle_file_section_base: BundleFileSectionBase,
}

impl TextRange for BundleFileTextLike {
    fn pos(&self) -> isize {
        self._bundle_file_section_base.pos()
    }

    fn set_pos(&self, pos: isize) {
        self._bundle_file_section_base.set_pos(pos)
    }

    fn end(&self) -> isize {
        self._bundle_file_section_base.end()
    }

    fn set_end(&self, end: isize) {
        self._bundle_file_section_base.set_end(end)
    }
}

impl BundleFileSectionInterface for BundleFileTextLike {
    fn kind(&self) -> BundleFileSectionKind {
        self._bundle_file_section_base.kind()
    }

    fn maybe_data(&self) -> Option<&String> {
        self._bundle_file_section_base.maybe_data()
    }
}

#[derive(Clone, Debug, Serialize, Trace, Finalize)]
#[serde(untagged)]
pub enum BundleFileSection {
    BundleFilePrologue(BundleFilePrologue),
    BundleFileEmitHelpers(BundleFileEmitHelpers),
    BundleFileHasNoDefaultLib(BundleFileHasNoDefaultLib),
    BundleFileReference(BundleFileReference),
    BundleFilePrepend(BundleFilePrepend),
    BundleFileTextLike(BundleFileTextLike),
}

impl BundleFileSection {
    pub fn new_prologue(data: String, pos: isize, end: isize) -> Self {
        Self::BundleFilePrologue(BundleFilePrologue {
            _bundle_file_section_base: BundleFileSectionBase::new(
                pos,
                end,
                BundleFileSectionKind::Prologue,
                Some(data),
            ),
        })
    }

    pub fn new_emit_helpers(data: String, pos: isize, end: isize) -> Self {
        Self::BundleFileEmitHelpers(BundleFileEmitHelpers {
            _bundle_file_section_base: BundleFileSectionBase::new(
                pos,
                end,
                BundleFileSectionKind::EmitHelpers,
                Some(data),
            ),
        })
    }

    pub fn new_has_no_default_lib(data: Option<String>, pos: isize, end: isize) -> Self {
        Self::BundleFileEmitHelpers(BundleFileEmitHelpers {
            _bundle_file_section_base: BundleFileSectionBase::new(
                pos,
                end,
                BundleFileSectionKind::NoDefaultLib,
                data,
            ),
        })
    }

    pub fn new_reference(
        kind: BundleFileSectionKind,
        data: String,
        pos: isize,
        end: isize,
    ) -> Self {
        Self::BundleFileReference(BundleFileReference {
            _bundle_file_section_base: BundleFileSectionBase::new(pos, end, kind, Some(data)),
        })
    }

    pub fn new_prepend(
        data: String,
        texts: Vec<Gc<BundleFileSection>>,
        pos: isize,
        end: isize,
    ) -> Self {
        Self::BundleFilePrepend(BundleFilePrepend {
            _bundle_file_section_base: BundleFileSectionBase::new(
                pos,
                end,
                BundleFileSectionKind::Prepend,
                Some(data),
            ),
            texts,
        })
    }

    pub fn new_text_like(
        kind: BundleFileSectionKind,
        data: Option<String>,
        pos: isize,
        end: isize,
    ) -> Self {
        Self::BundleFileTextLike(BundleFileTextLike {
            _bundle_file_section_base: BundleFileSectionBase::new(pos, end, kind, data),
        })
    }
}

impl TextRange for BundleFileSection {
    fn pos(&self) -> isize {
        match self {
            Self::BundleFilePrologue(value) => value.pos(),
            Self::BundleFileEmitHelpers(value) => value.pos(),
            Self::BundleFileHasNoDefaultLib(value) => value.pos(),
            Self::BundleFileReference(value) => value.pos(),
            Self::BundleFilePrepend(value) => value.pos(),
            Self::BundleFileTextLike(value) => value.pos(),
        }
    }

    fn set_pos(&self, pos: isize) {
        match self {
            Self::BundleFilePrologue(value) => value.set_pos(pos),
            Self::BundleFileEmitHelpers(value) => value.set_pos(pos),
            Self::BundleFileHasNoDefaultLib(value) => value.set_pos(pos),
            Self::BundleFileReference(value) => value.set_pos(pos),
            Self::BundleFilePrepend(value) => value.set_pos(pos),
            Self::BundleFileTextLike(value) => value.set_pos(pos),
        }
    }

    fn end(&self) -> isize {
        match self {
            Self::BundleFilePrologue(value) => value.end(),
            Self::BundleFileEmitHelpers(value) => value.end(),
            Self::BundleFileHasNoDefaultLib(value) => value.end(),
            Self::BundleFileReference(value) => value.end(),
            Self::BundleFilePrepend(value) => value.end(),
            Self::BundleFileTextLike(value) => value.end(),
        }
    }

    fn set_end(&self, end: isize) {
        match self {
            Self::BundleFilePrologue(value) => value.set_end(end),
            Self::BundleFileEmitHelpers(value) => value.set_end(end),
            Self::BundleFileHasNoDefaultLib(value) => value.set_end(end),
            Self::BundleFileReference(value) => value.set_end(end),
            Self::BundleFilePrepend(value) => value.set_end(end),
            Self::BundleFileTextLike(value) => value.set_end(end),
        }
    }
}

impl BundleFileSectionInterface for BundleFileSection {
    fn kind(&self) -> BundleFileSectionKind {
        match self {
            Self::BundleFilePrologue(value) => value.kind(),
            Self::BundleFileEmitHelpers(value) => value.kind(),
            Self::BundleFileHasNoDefaultLib(value) => value.kind(),
            Self::BundleFileReference(value) => value.kind(),
            Self::BundleFilePrepend(value) => value.kind(),
            Self::BundleFileTextLike(value) => value.kind(),
        }
    }

    fn maybe_data(&self) -> Option<&String> {
        match self {
            Self::BundleFilePrologue(value) => value.maybe_data(),
            Self::BundleFileEmitHelpers(value) => value.maybe_data(),
            Self::BundleFileHasNoDefaultLib(value) => value.maybe_data(),
            Self::BundleFileReference(value) => value.maybe_data(),
            Self::BundleFilePrepend(value) => value.maybe_data(),
            Self::BundleFileTextLike(value) => value.maybe_data(),
        }
    }
}

#[derive(Serialize, Trace, Finalize)]
pub struct SourceFilePrologueDirectiveExpression {
    pub pos: isize,
    pub end: isize,
    pub text: String,
}

#[derive(Serialize, Trace, Finalize)]
pub struct SourceFilePrologueDirective {
    pub pos: isize,
    pub end: isize,
    pub expression: SourceFilePrologueDirectiveExpression,
}

#[derive(Serialize, Trace, Finalize)]
pub struct SourceFilePrologueInfo {
    pub file: usize,
    pub text: String,
    pub directives: Vec<SourceFilePrologueDirective>,
}

#[derive(Default, Serialize, Trace, Finalize)]
pub struct SourceFileInfo {
    pub helpers: Option<Vec<String>>,
    pub prologues: Option<Vec<SourceFilePrologueInfo>>,
}

#[derive(Serialize, Trace, Finalize)]
pub struct BundleFileInfo {
    pub sections: Vec<Gc<BundleFileSection>>,
    pub sources: Option<SourceFileInfo>,
}

#[derive(Serialize, Trace, Finalize)]
pub struct BundleBuildInfo {
    pub js: Option<Gc<GcCell<BundleFileInfo>>>,
    pub dts: Option<BundleFileInfo>,
    pub common_source_directory: String,
    pub source_files: Vec<String>,
}

#[derive(Serialize, Trace, Finalize)]
pub struct BuildInfo {
    pub bundle: Option<Gc<GcCell<BundleBuildInfo>>>,
    pub program: Option<Gc<ProgramBuildInfo>>,
    pub version: String,
}

impl std::fmt::Debug for BuildInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuildInfo")
            // .field("bundle", &self.bundle)
            // .field("program", &self.program)
            .field("version", &self.version)
            .finish()
    }
}

pub trait PrintHandlers: Trace + Finalize {
    fn has_global_name(&self, _name: &str) -> Option<bool> {
        None
    }
    fn on_emit_node(
        &self,
        _hint: EmitHint,
        _node: Id<Node>,
        _emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        Ok(())
    }
    fn is_on_emit_node_supported(&self) -> bool;
    fn is_emit_notification_enabled(&self, _node: Id<Node>) -> Option<bool> {
        None
    }
    fn substitute_node(&self, _hint: EmitHint, _node: Id<Node>) -> io::Result<Option<Id<Node>>> {
        Ok(None)
    }
    fn is_substitute_node_supported(&self) -> bool;
    fn on_emit_source_map_of_node(
        &self,
        _hint: EmitHint,
        _node: Id<Node>,
        _emit_callback: &dyn Fn(EmitHint, Id<Node>),
    ) {
    }
    fn on_emit_source_map_of_token(
        &self,
        _node: Option<Id<Node>>,
        _token: SyntaxKind,
        _writer: &dyn Fn(&str),
        _pos: usize,
        _emit_callback: &dyn Fn(SyntaxKind, &dyn Fn(&str), usize) -> usize,
    ) -> Option<usize> {
        None
    }
    fn on_emit_source_map_of_position(&self, _pos: usize) {}
    fn on_set_source_file(&self, _node: Id<Node> /*SourceFile*/) {}
    fn on_before_emit_node(&self, _node: Option<Id<Node>>) {}
    fn on_after_emit_node(&self, _node: Option<Id<Node>>) {}
    fn on_before_emit_node_array(&self, _nodes: Option<&NodeArray>) {}
    fn on_after_emit_node_array(&self, _nodes: Option<&NodeArray>) {}
    fn on_before_emit_token(&self, _node: Option<Id<Node>>) {}
    fn on_after_emit_token(&self, _node: Option<Id<Node>>) {}
}

mod _PrinterOptionsDeriveTraceScope {
    use local_macros::Trace;

    use super::*;

    #[derive(Builder, Default, Trace, Finalize)]
    #[builder(default)]
    pub struct PrinterOptions {
        pub remove_comments: Option<bool>,
        #[unsafe_ignore_trace]
        pub new_line: Option<NewLineKind>,
        pub omit_trailing_semicolon: Option<bool>,
        pub no_emit_helpers: Option<bool>,
        #[unsafe_ignore_trace]
        pub(crate) module: Option<ModuleKind>,
        #[unsafe_ignore_trace]
        pub(crate) target: Option<ScriptTarget>,
        pub(crate) source_map: Option<bool>,
        pub(crate) inline_source_map: Option<bool>,
        pub(crate) inline_sources: Option<bool>,
        pub(crate) extended_diagnostics: Option<bool>,
        pub(crate) only_print_js_doc_style: Option<bool>,
        pub(crate) never_ascii_escape: Option<bool>,
        pub(crate) write_bundle_file_info: Option<bool>,
        pub(crate) record_internal_section: Option<bool>,
        pub(crate) strip_internal: Option<bool>,
        pub(crate) preserve_source_newlines: Option<bool>,
        pub(crate) terminate_unterminated_literals: Option<bool>,
        pub(crate) relative_to_build_info: Option<Gc<Box<dyn RelativeToBuildInfo>>>,
    }
}
pub use _PrinterOptionsDeriveTraceScope::{PrinterOptions, PrinterOptionsBuilder};

pub trait RelativeToBuildInfo: Trace + Finalize {
    fn call(&self, file_name: &str) -> String;
}

#[derive(Debug, Serialize)]
pub struct RawSourceMap {
    pub version: u32, /*3*/
    pub file: String,
    pub source_root: Option<String>,
    pub sources: Vec<String>,
    pub sources_content: Option<Vec<Option<String>>>,
    pub mappings: String,
    pub names: Option<Vec<String>>,
}

pub trait SourceMapGenerator: Trace + Finalize {
    fn get_sources(&self) -> Vec<String>;
    fn add_source(&self, file_name: &str) -> usize;
    fn set_source_content(&self, source_index: usize, content: Option<String>);
    fn add_name(&self, name: &str) -> usize;
    fn add_mapping(
        &self,
        generated_line: usize,
        generated_character: usize,
        source_index: usize,
        source_line: usize,
        source_character: usize,
        name_index: Option<usize>,
    );
    fn append_source_map(
        &self,
        generated_line: usize,
        generated_character: usize,
        source_map: &RawSourceMap,
        source_map_path: &str,
        start: Option<LineAndCharacter>,
        end: Option<LineAndCharacter>,
    );
    fn to_json(&self) -> RawSourceMap;
    fn to_string(&self) -> String;
}

pub trait EmitTextWriter: SymbolWriter + Trace + Finalize {
    fn write(&self, s: &str);
    fn write_trailing_semicolon(&self, text: &str);
    fn write_comment(&self, text: &str);
    fn get_text(&self) -> String;
    fn raw_write(&self, text: &str);
    fn write_literal(&self, text: &str);
    fn get_text_pos(&self) -> usize;
    fn get_line(&self) -> usize;
    fn get_column(&self) -> usize;
    fn get_indent(&self) -> usize;
    fn is_at_start_of_line(&self) -> bool;
    fn has_trailing_comment(&self) -> bool;
    fn has_trailing_whitespace(&self) -> bool;
    fn get_text_pos_with_write_line(&self) -> Option<usize> {
        None
    }
    fn non_escaping_write(&self, _text: &str) {}
    fn is_non_escaping_write_supported(&self) -> bool;
}

pub trait ModuleSpecifierResolutionHost {
    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        None
    }
    fn file_exists(&self, path: &str) -> bool;
    fn get_current_directory(&self) -> String;
    fn directory_exists(&self, _path: &str) -> Option<bool> {
        None
    }
    fn read_file(&self, _path: &str) -> Option<io::Result<Option<String>>> {
        None
    }
    fn is_read_file_supported(&self) -> bool;
    fn realpath(&self, _path: &str) -> Option<String> {
        None
    }
    fn get_symlink_cache(&self) -> Option<Gc<SymlinkCache>> {
        None
    }
    fn get_module_specifier_cache(&self) -> Option<Rc<dyn ModuleSpecifierCache>> {
        None
    }
    fn get_global_typings_cache_location(&self) -> Option<String> {
        None
    }
    fn is_get_nearest_ancestor_directory_with_package_json_supported(&self) -> bool;
    fn get_nearest_ancestor_directory_with_package_json(
        &self,
        _file_name: &str,
        _root_dir: Option<&str>,
    ) -> Option<String> {
        None
    }

    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>>;
    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String>;
    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool;
    fn get_file_include_reasons(&self) -> Gc<GcCell<MultiMap<Path, Gc<FileIncludeReason>>>>;
}

#[derive(Clone)]
pub struct ModulePath {
    pub path: String,
    pub is_in_node_modules: bool,
    pub is_redirect: bool,
}

pub struct ResolvedModuleSpecifierInfo {
    pub module_paths: Option<Vec<ModulePath>>,
    pub module_specifiers: Option<Vec<String>>,
    pub is_auto_importable: Option<bool>,
}

pub trait ModuleSpecifierCache {
    fn get(
        &self,
        from_file_name: &Path,
        to_file_name: &Path,
        preferences: &UserPreferences,
    ) -> Option<Rc<ResolvedModuleSpecifierInfo>>;
    fn set(
        &self,
        from_file_name: &Path,
        to_file_name: &Path,
        preferences: &UserPreferences,
        module_paths: &[ModulePath],
        module_specifiers: &[String],
    );
    fn set_module_paths(
        &self,
        from_file_name: &Path,
        to_file_name: &Path,
        preferences: &UserPreferences,
        module_paths: &[ModulePath],
    );
}

pub trait SymbolTracker: Trace + Finalize {
    fn track_symbol(
        &self,
        _symbol: Id<Symbol>,
        _enclosing_declaration: Option<Id<Node>>,
        _meaning: SymbolFlags,
    ) -> Option<io::Result<bool>> {
        None
    }
    fn is_track_symbol_supported(&self) -> bool;
    fn disable_track_symbol(&self);
    fn reenable_track_symbol(&self);
    fn report_inaccessible_this_error(&self) {}
    fn is_report_inaccessible_this_error_supported(&self) -> bool;
    fn report_private_in_base_of_class_expression(&self, _property_name: &str) {}
    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool;
    fn report_inaccessible_unique_symbol_error(&self) {}
    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool;
    fn report_cyclic_structure_error(&self) {}
    fn is_report_cyclic_structure_error_supported(&self) -> bool;
    fn report_likely_unsafe_import_required_error(&self, _specifier: &str) {}
    fn is_report_likely_unsafe_import_required_error_supported(&self) -> bool;
    fn report_truncation_error(&self) {}
    fn module_resolver_host(
        &self,
    ) -> Option<&dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory> {
        None
    }
    fn is_module_resolver_host_supported(&self) -> bool;
    fn track_referenced_ambient_module(
        &self,
        _decl: Id<Node>, /*ModuleDeclaration*/
        _symbol: Id<Symbol>,
    ) -> io::Result<()> {
        Ok(())
    }
    fn is_track_referenced_ambient_module_supported(&self) -> bool;
    fn track_external_module_symbol_of_import_type_node(&self, _symbol: Id<Symbol>) {}
    fn report_nonlocal_augmentation(
        &self,
        _containing_file: Id<Node>, /*SourceFile*/
        _parent_symbol: Id<Symbol>,
        _augmenting_symbol: Id<Symbol>,
    ) {
    }
    fn is_report_nonlocal_augmentation_supported(&self) -> bool;
    fn report_non_serializable_property(&self, _property_name: &str) {}
    fn is_report_non_serializable_property_supported(&self) -> bool;
}

pub trait ModuleSpecifierResolutionHostAndGetCommonSourceDirectory:
    ModuleSpecifierResolutionHost + Trace + Finalize
{
    fn get_common_source_directory(&self) -> String;
    fn as_dyn_module_specifier_resolution_host(&self) -> &dyn ModuleSpecifierResolutionHost;
}

#[derive(Copy, Clone, Debug)]
pub struct TextSpan {
    pub start: isize,
    pub length: isize,
}

#[derive(Copy, Clone, Debug)]
pub struct TextChangeRange {
    pub span: TextSpan,
    pub new_length: isize,
}

#[derive(Debug, Trace, Finalize)]
pub struct DiagnosticCollection {
    #[unsafe_ignore_trace]
    pub arena: *const AllArenas,
    pub non_file_diagnostics: SortedArray<Gc<Diagnostic>>,
    pub files_with_diagnostics: SortedArray<String>,
    pub file_diagnostics: HashMap<String, SortedArray<Gc<Diagnostic /*DiagnosticWithLocation*/>>>,
    #[unsafe_ignore_trace]
    pub has_read_non_file_diagnostics: Cell<bool>,
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct SyntaxList {
    _node: BaseNode,
    pub _children: Vec<Id<Node>>,
}

bitflags! {
    pub struct ListFormat: u32 {
        const None = 0;

        const SingleLine = 0;
        const MultiLine = 1 << 0;
        const PreserveLines = 1 << 1;
        const LinesMask = Self::SingleLine.bits | Self::MultiLine.bits | Self::PreserveLines.bits;

        const NotDelimited = 0;
        const BarDelimited = 1 << 2;
        const AmpersandDelimited = 1 << 3;
        const CommaDelimited = 1 << 4;
        const AsteriskDelimited = 1 << 5;
        const DelimitersMask = Self::BarDelimited.bits | Self::AmpersandDelimited.bits | Self::CommaDelimited.bits | Self::AsteriskDelimited.bits;

        const AllowTrailingComma = 1 << 6;

        const Indented = 1 << 7;
        const SpaceBetweenBraces = 1 << 8;
        const SpaceBetweenSiblings = 1 << 9;

        const Braces = 1 << 10;
        const Parenthesis = 1 << 11;
        const AngleBrackets = 1 << 12;
        const SquareBrackets = 1 << 13;
        const BracketsMask = Self::Braces.bits | Self::Parenthesis.bits | Self::AngleBrackets.bits | Self::SquareBrackets.bits;

        const OptionalIfUndefined = 1 << 14;
        const OptionalIfEmpty = 1 << 15;
        const Optional = Self::OptionalIfUndefined.bits | Self::OptionalIfEmpty.bits;

        const PreferNewLine = 1 << 16;
        const NoTrailingNewLine = 1 << 17;
        const NoInterveningComments = 1 << 18;

        const NoSpaceIfEmpty = 1 << 19;
        const SingleElement = 1 << 20;
        const SpaceAfterList = 1 << 21;

        const Modifiers = Self::SingleLine.bits | Self::SpaceBetweenSiblings.bits | Self::NoInterveningComments.bits;
        const HeritageClauses = Self::SingleLine.bits | Self::SpaceBetweenSiblings.bits;
        const SingleLineTypeLiteralMembers = Self::SingleLine.bits | Self::SpaceBetweenBraces.bits | Self::SpaceBetweenSiblings.bits;
        const MultiLineTypeLiteralMembers = Self::MultiLine.bits | Self::Indented.bits | Self::OptionalIfEmpty.bits;

        const SingleLineTupleTypeElements = Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits;
        const MultiLineTupleTypeElements = Self::CommaDelimited.bits | Self::Indented.bits | Self::SpaceBetweenSiblings.bits | Self::MultiLine.bits;
        const UnionTypeConstituents = Self::BarDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits;
        const IntersectionTypeConstituents = Self::AmpersandDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits;
        const ObjectBindingPatternElements = Self::SingleLine.bits | Self::AllowTrailingComma.bits | Self::SpaceBetweenBraces.bits | Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::NoSpaceIfEmpty.bits;
        const ArrayBindingPatternElements = Self::SingleLine.bits | Self::AllowTrailingComma.bits | Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::NoSpaceIfEmpty.bits;
        const ObjectLiteralExpressionProperties = Self::PreserveLines.bits | Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SpaceBetweenBraces.bits | Self::Indented.bits | Self::Braces.bits | Self::NoSpaceIfEmpty.bits;
        const ImportClauseEntries = Self::PreserveLines.bits | Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SpaceBetweenBraces.bits | Self::Indented.bits | Self::Braces.bits | Self::NoSpaceIfEmpty.bits;
        const ArrayLiteralExpressionElements = Self::PreserveLines.bits | Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::AllowTrailingComma.bits | Self::Indented.bits | Self::SquareBrackets.bits;
        const CommaListElements = Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits;
        const CallExpressionArguments = Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits | Self::Parenthesis.bits;
        const NewExpressionArguments = Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits | Self::Parenthesis.bits | Self::OptionalIfUndefined.bits;
        const TemplateExpressionSpans = Self::SingleLine.bits | Self::NoInterveningComments.bits;
        const SingleLineBlockStatements = Self::SpaceBetweenBraces.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits;
        const MultiLineBlockStatements = Self::Indented.bits | Self::MultiLine.bits;
        const VariableDeclarationList = Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits;
        const SingleLineFunctionBodyStatements = Self::SingleLine.bits | Self::SpaceBetweenSiblings.bits | Self::SpaceBetweenBraces.bits;
        const MultiLineFunctionBodyStatements = Self::MultiLine.bits;
        const ClassHeritageClauses = Self::SingleLine.bits;
        const ClassMembers = Self::Indented.bits | Self::MultiLine.bits;
        const InterfaceMembers = Self::Indented.bits | Self::MultiLine.bits;
        const EnumMembers = Self::CommaDelimited.bits | Self::Indented.bits | Self::MultiLine.bits;
        const CaseBlockClauses = Self::Indented.bits | Self::MultiLine.bits;
        const NamedImportsOrExportsElements = Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::AllowTrailingComma.bits | Self::SingleLine.bits | Self::SpaceBetweenBraces.bits | Self::NoSpaceIfEmpty.bits;
        const JsxElementOrFragmentChildren = Self::SingleLine.bits | Self::NoInterveningComments.bits;
        const JsxElementAttributes = Self::SingleLine.bits | Self::SpaceBetweenSiblings.bits | Self::NoInterveningComments.bits;
        const CaseOrDefaultClauseStatements = Self::Indented.bits | Self::MultiLine.bits | Self::NoTrailingNewLine.bits | Self::OptionalIfEmpty.bits;
        const HeritageClauseTypes = Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits;

        const Decorators = Self::MultiLine.bits | Self::Optional.bits | Self::SpaceAfterList.bits;
        const TypeArguments = Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits | Self::AngleBrackets.bits | Self::Optional.bits;
        const TypeParameters = Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits | Self::AngleBrackets.bits | Self::Optional.bits;
        const Parameters = Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits | Self::Parenthesis.bits;
        const IndexSignatureParameters = Self::CommaDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits | Self::Indented.bits | Self::SquareBrackets.bits;
        const JSDocComment = Self::MultiLine.bits | Self::AsteriskDelimited.bits;
    }
}

bitflags! {
    pub struct PragmaKindFlags: u32 {
        const None = 0;
        const TripleSlashXML = 1 << 0;
        const SingleLine = 1 << 1;
        const MultiLine = 1 << 2;
        const All = Self::TripleSlashXML.bits | Self::SingleLine.bits | Self::MultiLine.bits;
        const Default = Self::All.bits;
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum PragmaName {
    Reference,
    AmdDependency,
    AmdModule,
    TsCheck,
    TsNocheck,
    Jsx,
    Jsxfrag,
    Jsximportsource,
    Jsxruntime,
}

// impl PragmaName {
//     pub fn to_str(&self) -> &'static str {
//         match self {
//             Self::Reference => "reference",
//             Self::AmdDependency => "amd-dependency",
//             Self::AmdModule => "amd-module",
//             Self::TsCheck => "ts-check",
//             Self::TsNocheck => "ts-nocheck",
//             Self::Jsx => "jsx",
//             Self::Jsxfrag => "jsxfrag",
//             Self::Jsximportsource => "jsximportsource",
//             Self::Jsxruntime => "jsxruntime",
//         }
//     }
// }

pub fn to_pragma_name(name: &str) -> Option<PragmaName> {
    match name {
        "reference" => Some(PragmaName::Reference),
        "amd-dependency" => Some(PragmaName::AmdDependency),
        "amd-module" => Some(PragmaName::AmdModule),
        "ts-check" => Some(PragmaName::TsCheck),
        "ts-nocheck" => Some(PragmaName::TsNocheck),
        "jsx" => Some(PragmaName::Jsx),
        "jsxfrag" => Some(PragmaName::Jsxfrag),
        "jsximportsource" => Some(PragmaName::Jsximportsource),
        "jsxruntime" => Some(PragmaName::Jsxruntime),
        _ => None,
    }
}

pub struct PragmaSpec {
    pub kind: PragmaKindFlags,
    pub args: Option<Vec<PragmaArgumentSpec>>,
}

pub struct PragmaArgumentSpec {
    pub name: PragmaArgumentName,
    pub optional: bool,
    pub capture_span: bool,
}

lazy_static! {
    pub static ref comment_pragmas: HashMap<PragmaName, PragmaSpec> =
        HashMap::from_iter(IntoIterator::into_iter([
            (
                PragmaName::Reference,
                PragmaSpec {
                    args: Some(vec![
                        PragmaArgumentSpec {
                            name: PragmaArgumentName::Types,
                            optional: true,
                            capture_span: true,
                        },
                        PragmaArgumentSpec {
                            name: PragmaArgumentName::Lib,
                            optional: true,
                            capture_span: true,
                        },
                        PragmaArgumentSpec {
                            name: PragmaArgumentName::Path,
                            optional: true,
                            capture_span: true,
                        },
                        PragmaArgumentSpec {
                            name: PragmaArgumentName::NoDefaultLib,
                            optional: true,
                            capture_span: false,
                        },
                    ]),
                    kind: PragmaKindFlags::TripleSlashXML,
                }
            ),
            (
                PragmaName::AmdDependency,
                PragmaSpec {
                    args: Some(vec![
                        PragmaArgumentSpec {
                            name: PragmaArgumentName::Path,
                            optional: false,
                            capture_span: false,
                        },
                        PragmaArgumentSpec {
                            name: PragmaArgumentName::Name,
                            optional: true,
                            capture_span: false,
                        },
                    ]),
                    kind: PragmaKindFlags::TripleSlashXML,
                }
            ),
            (
                PragmaName::AmdModule,
                PragmaSpec {
                    args: Some(vec![PragmaArgumentSpec {
                        name: PragmaArgumentName::Name,
                        optional: false,
                        capture_span: false,
                    },]),
                    kind: PragmaKindFlags::TripleSlashXML,
                }
            ),
            (
                PragmaName::TsCheck,
                PragmaSpec {
                    args: None,
                    kind: PragmaKindFlags::SingleLine,
                }
            ),
            (
                PragmaName::TsNocheck,
                PragmaSpec {
                    args: None,
                    kind: PragmaKindFlags::SingleLine,
                }
            ),
            (
                PragmaName::Jsx,
                PragmaSpec {
                    args: Some(vec![PragmaArgumentSpec {
                        name: PragmaArgumentName::Factory,
                        optional: false,
                        capture_span: false,
                    },]),
                    kind: PragmaKindFlags::MultiLine,
                }
            ),
            (
                PragmaName::Jsxfrag,
                PragmaSpec {
                    args: Some(vec![PragmaArgumentSpec {
                        name: PragmaArgumentName::Factory,
                        optional: false,
                        capture_span: false,
                    },]),
                    kind: PragmaKindFlags::MultiLine,
                }
            ),
            (
                PragmaName::Jsximportsource,
                PragmaSpec {
                    args: Some(vec![PragmaArgumentSpec {
                        name: PragmaArgumentName::Factory,
                        optional: false,
                        capture_span: false,
                    },]),
                    kind: PragmaKindFlags::MultiLine,
                }
            ),
            (
                PragmaName::Jsxruntime,
                PragmaSpec {
                    args: Some(vec![PragmaArgumentSpec {
                        name: PragmaArgumentName::Factory,
                        optional: false,
                        capture_span: false,
                    },]),
                    kind: PragmaKindFlags::MultiLine,
                }
            ),
        ]));
}

pub fn get_pragma_spec(name: PragmaName) -> &'static PragmaSpec {
    comment_pragmas.get(&name).unwrap()
}

pub type PragmaArguments = HashMap<PragmaArgumentName, PragmaArgument>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum PragmaArgumentName {
    Types,
    Lib,
    Path,
    NoDefaultLib,
    Name,
    Factory,
}

impl PragmaArgumentName {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Types => "types",
            Self::Lib => "lib",
            Self::Path => "path",
            Self::NoDefaultLib => "no-default-lib",
            Self::Name => "name",
            Self::Factory => "factory",
        }
    }
}

#[derive(Debug)]
pub enum PragmaArgument {
    WithoutCapturedSpan(String),
    WithCapturedSpan(PragmaArgumentWithCapturedSpan),
}

impl PragmaArgument {
    pub fn as_without_captured_span(&self) -> &String {
        enum_unwrapped!(self, [PragmaArgument, WithoutCapturedSpan])
    }

    pub fn as_with_captured_span(&self) -> &PragmaArgumentWithCapturedSpan {
        enum_unwrapped!(self, [PragmaArgument, WithCapturedSpan])
    }
}

#[derive(Debug)]
pub struct PragmaArgumentWithCapturedSpan {
    pub value: String,
    pub pos: usize,
    pub end: usize,
}

#[derive(Debug)]
pub struct PragmaValue {
    pub arguments: PragmaArguments,
    pub range: CommentRange,
}

pub struct PragmaPseudoMapEntry {
    pub name: PragmaName,
    pub args: Rc<PragmaValue>,
}

pub type ReadonlyPragmaMap = HashMap<PragmaName, Vec<Rc<PragmaValue>>>;

pub struct CommentDirectivesMap {
    pub directives_by_line: HashMap<String, Rc<CommentDirective>>,
    pub used_lines: HashMap<String, bool>,
}

impl CommentDirectivesMap {
    pub fn new(
        directives_by_line: HashMap<String, Rc<CommentDirective>>,
        used_lines: HashMap<String, bool>,
    ) -> Self {
        Self {
            directives_by_line,
            used_lines,
        }
    }
}

#[derive(Builder, Default)]
#[builder(default)]
pub struct UserPreferences {
    pub disable_suggestions: Option<bool>,
    pub quote_preference: Option<String /*"auto" | "double" | "single"*/>,
    pub include_completions_for_module_exports: Option<bool>,
    pub include_completions_for_import_statements: Option<bool>,
    pub include_completions_with_snippet_text: Option<bool>,
    pub include_automatic_optional_chain_completions: Option<bool>,
    pub include_completions_with_insert_text: Option<bool>,
    pub include_completions_with_class_member_snippets: Option<bool>,
    pub allow_incomplete_completions: Option<bool>,
    pub import_module_specifier_preference:
        Option<String /*"shortest" | "project-relative" | "relative" | "non-relative"*/>,
    pub import_module_specifier_ending: Option<String /*"auto" | "minimal" | "index" | "js"*/>,
    pub allow_text_changes_in_new_files: Option<bool>,
    pub provide_prefix_and_suffix_text_for_rename: Option<bool>,
    pub include_package_json_auto_imports: Option<String /*"auto" | "on" | "off"*/>,
    pub provide_refactor_not_applicable_reason: Option<bool>,
    pub jsx_attribute_completion_style: Option<String /*"auto" | "braces" | "none"*/>,
}

#[derive(Clone, Debug)]
pub struct PseudoBigInt {
    pub negative: bool,
    pub base_10_value: String,
}

impl PseudoBigInt {
    pub fn new(negative: bool, base_10_value: String) -> Self {
        Self {
            negative,
            base_10_value,
        }
    }
}

impl PartialEq for PseudoBigInt {
    fn eq(&self, other: &PseudoBigInt) -> bool {
        // TODO: is this right?
        self.negative == other.negative && self.base_10_value == other.base_10_value
    }
}

impl Eq for PseudoBigInt {}
