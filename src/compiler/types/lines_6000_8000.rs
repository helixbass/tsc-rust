#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use super::{
    BaseTextRange, DiagnosticMessage, ModuleResolutionKind, Node, NodeArray, NodeArrayOrVec,
    SyntaxKind, SynthesizedComment, TextRange,
};
use crate::{BaseNodeFactory, MapLike, NodeFactoryFlags, OptionsNameMap};
use local_macros::{command_line_option_type, enum_unwrapped};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PluginImport {
    pub name: String,
}

#[derive(Debug)]
pub enum CompilerOptionsValue {
    Bool(Option<bool>),
    String(Option<String>),
    SourceFile(Option<Rc<Node /*SourceFile*/>>),
    ImportsNotUsedAsValues(Option<ImportsNotUsedAsValues>),
    JsxEmit(Option<JsxEmit>),
    VecString(Option<Vec<String>>),
    Usize(Option<usize>),
    ModuleKind(Option<ModuleKind>),
    ModuleResolutionKind(Option<ModuleResolutionKind>),
    NewLineKind(Option<NewLineKind>),
    MapLikeVecString(Option<MapLike<Vec<String>>>),
    VecPluginImport(Option<Vec<PluginImport>>),
    ScriptTarget(Option<ScriptTarget>),
}

impl CompilerOptionsValue {
    pub fn as_option_bool(&self) -> Option<bool> {
        enum_unwrapped!(self, [CompilerOptionsValue, Bool]).clone()
    }
}

impl PartialEq for CompilerOptionsValue {
    fn eq(&self, other: &CompilerOptionsValue) -> bool {
        match (self, other) {
            (CompilerOptionsValue::Bool(self_value), CompilerOptionsValue::Bool(other_value)) => {
                self_value == other_value
            }
            (
                CompilerOptionsValue::String(self_value),
                CompilerOptionsValue::String(other_value),
            ) => self_value == other_value,
            (
                CompilerOptionsValue::SourceFile(self_value),
                CompilerOptionsValue::SourceFile(other_value),
            ) => {
                self_value.is_some()
                    && other_value.is_some()
                    && Rc::ptr_eq(self_value.as_ref().unwrap(), other_value.as_ref().unwrap())
                    || self_value.is_none() && other_value.is_none()
            }
            (
                CompilerOptionsValue::ImportsNotUsedAsValues(self_value),
                CompilerOptionsValue::ImportsNotUsedAsValues(other_value),
            ) => self_value == other_value,
            (
                CompilerOptionsValue::JsxEmit(self_value),
                CompilerOptionsValue::JsxEmit(other_value),
            ) => self_value == other_value,
            (
                CompilerOptionsValue::VecString(self_value),
                CompilerOptionsValue::VecString(other_value),
            ) => self_value == other_value,
            (CompilerOptionsValue::Usize(self_value), CompilerOptionsValue::Usize(other_value)) => {
                self_value == other_value
            }
            (
                CompilerOptionsValue::ModuleKind(self_value),
                CompilerOptionsValue::ModuleKind(other_value),
            ) => self_value == other_value,
            (
                CompilerOptionsValue::ModuleResolutionKind(self_value),
                CompilerOptionsValue::ModuleResolutionKind(other_value),
            ) => self_value == other_value,
            (
                CompilerOptionsValue::NewLineKind(self_value),
                CompilerOptionsValue::NewLineKind(other_value),
            ) => self_value == other_value,
            (
                CompilerOptionsValue::MapLikeVecString(self_value),
                CompilerOptionsValue::MapLikeVecString(other_value),
            ) => self_value == other_value,
            (
                CompilerOptionsValue::VecPluginImport(self_value),
                CompilerOptionsValue::VecPluginImport(other_value),
            ) => self_value == other_value,
            (
                CompilerOptionsValue::ScriptTarget(self_value),
                CompilerOptionsValue::ScriptTarget(other_value),
            ) => self_value == other_value,
            _ => false,
        }
    }
}

impl Eq for CompilerOptionsValue {}

#[derive(Debug)]
pub struct CompilerOptions {
    pub(crate) all: Option<bool>,
    pub allow_js: Option<bool>,
    pub allow_non_ts_extensions: Option<bool>,
    pub allow_synthetic_default_imports: Option<bool>,
    pub allow_umd_global_access: Option<bool>,
    pub allow_unreachable_code: Option<bool>,
    pub allow_unused_labels: Option<bool>,
    pub always_strict: Option<bool>,
    pub base_url: Option<String>,
    pub(crate) build: Option<bool>,
    pub charset: Option<String>,
    pub check_js: Option<bool>,
    pub(crate) config_file_path: Option<String>,
    pub(crate) config_file: Option<Rc<Node /*TsConfigSourceFile*/>>,
    pub declaration: Option<bool>,
    pub declaration_map: Option<bool>,
    pub emit_declaration_only: Option<bool>,
    pub declaration_dir: Option<String>,
    pub diagnostics: Option<bool>,
    pub extended_diagnostics: Option<bool>,
    pub disable_size_limit: Option<bool>,
    pub disable_source_of_project_reference_redirect: Option<bool>,
    pub disable_solution_searching: Option<bool>,
    pub disable_referenced_project_load: Option<bool>,
    pub downlevel_iteration: Option<bool>,
    pub emit_bom: Option<bool>,
    pub emit_decorator_metadata: Option<bool>,
    pub exact_optional_property_types: Option<bool>,
    pub experimental_decorators: Option<bool>,
    pub force_consistent_casing_in_file_names: Option<bool>,
    pub(crate) generate_cpu_profile: Option<String>,
    pub(crate) generate_trace: Option<String>,
    pub(crate) help: Option<bool>,
    pub import_helpers: Option<bool>,
    pub imports_not_used_as_values: Option<ImportsNotUsedAsValues>,
    pub init: Option<bool>,
    pub inline_source_map: Option<bool>,
    pub inline_sources: Option<bool>,
    pub isolated_modules: Option<bool>,
    pub jsx: Option<JsxEmit>,
    pub keyof_strings_only: Option<bool>,
    pub lib: Option<Vec<String>>,
    pub(crate) list_emitted_files: Option<bool>,
    pub(crate) list_files: Option<bool>,
    pub(crate) explain_files: Option<bool>,
    pub(crate) list_files_only: Option<bool>,
    pub locale: Option<String>,
    pub map_root: Option<String>,
    pub max_node_module_js_depth: Option<usize>,
    pub module: Option<ModuleKind>,
    pub module_resolution: Option<ModuleResolutionKind>,
    pub new_line: Option<NewLineKind>,
    pub no_emit: Option<bool>,
    pub(crate) no_emit_for_js_files: Option<bool>,
    pub no_emit_helpers: Option<bool>,
    pub no_emit_on_error: Option<bool>,
    pub no_error_truncation: Option<bool>,
    pub no_fallthrough_cases_in_switch: Option<bool>,
    pub no_implicit_any: Option<bool>,
    pub no_implicit_returns: Option<bool>,
    pub no_implicit_this: Option<bool>,
    pub no_strict_generic_checks: Option<bool>,
    pub no_unused_locals: Option<bool>,
    pub no_unused_parameters: Option<bool>,
    pub no_implicit_use_strict: Option<bool>,
    pub no_property_access_from_index_signature: Option<bool>,
    pub assume_changes_only_affect_direct_dependencies: Option<bool>,
    pub no_lib: Option<bool>,
    pub no_resolve: Option<bool>,
    pub no_unchecked_indexed_access: Option<bool>,
    pub out: Option<String>,
    pub out_dir: Option<String>,
    pub out_file: Option<String>,
    pub paths: Option<MapLike<Vec<String>>>,
    pub(crate) paths_base_path: Option<String>,
    pub plugins: Option<Vec<PluginImport>>,
    pub preserve_const_enums: Option<bool>,
    pub no_implicit_override: Option<bool>,
    pub preserve_symlinks: Option<bool>,
    pub preserve_value_imports: Option<bool>,
    pub(crate) preserve_watch_output: Option<bool>,
    pub project: Option<String>,
    pub(crate) pretty: Option<bool>,
    pub react_namespace: Option<String>,
    pub jsx_factory: Option<String>,
    pub jsx_fragment_factory: Option<String>,
    pub jsx_import_source: Option<String>,
    pub composite: Option<bool>,
    pub incremental: Option<bool>,
    pub ts_build_info_file: Option<String>,
    pub remove_comments: Option<bool>,
    pub root_dir: Option<String>,
    pub root_dirs: Option<Vec<String>>,
    pub skip_lib_check: Option<bool>,
    pub skip_default_lib_check: Option<bool>,
    pub source_map: Option<bool>,
    pub source_root: Option<String>,
    pub strict: Option<bool>,
    pub strict_function_types: Option<bool>,
    pub strict_bind_call_apply: Option<bool>,
    pub strict_null_checks: Option<bool>,
    pub strict_property_initialization: Option<bool>,
    pub strip_internal: Option<bool>,
    pub suppress_excess_property_errors: Option<bool>,
    pub suppress_implicit_any_index_errors: Option<bool>,
    pub(crate) suppress_output_path_check: Option<bool>,
    pub target: Option<ScriptTarget>,
    pub trace_resolution: Option<bool>,
    pub use_unknown_in_catch_variables: Option<bool>,
    pub resolve_json_module: Option<bool>,
    pub types: Option<Vec<String>>,
    pub type_roots: Option<Vec<String>>,
    pub(crate) version: Option<bool>,
    pub(crate) watch: Option<bool>,
    pub es_module_interop: Option<bool>,
    pub(crate) show_config: Option<bool>,
    pub use_define_for_class_fields: Option<bool>,
    // [option: string]: CompilerOptionsValue | TsConfigSourceFile | undefined;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ModuleKind {
    None = 0,
    CommonJS = 1,
    AMD = 2,
    UMD = 3,

    System = 4,
    ES2015 = 5,
    ES2020 = 6,
    ES2022 = 7,
    ESNext = 99,

    Node12 = 100,
    NodeNext = 199,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum JsxEmit {
    None = 0,
    Preserve = 1,
    React = 2,
    ReactNative = 3,
    ReactJSX = 4,
    ReactJSXDev = 5,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ImportsNotUsedAsValues {
    Remove,
    Preserve,
    Error,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NewLineKind {
    CarriageReturnLineFeed = 0,
    LineFeed = 1,
}

#[derive(Debug)]
pub struct LineAndCharacter {
    pub line: usize,
    pub character: usize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ScriptKind {
    Unknown = 0,
    JS = 1,
    JSX = 2,
    TS = 3,
    TSX = 4,
    External = 5,
    JSON = 6,
    Deferred = 7,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum ScriptTarget {
    ES3 = 0,
    ES5 = 1,
    ES2015 = 2,
    ES2016 = 3,
    ES2017 = 4,
    ES2018 = 5,
    ES2019 = 6,
    ES2020 = 7,
    ES2021 = 8,
    ESNext = 99,
    JSON = 100,
}

impl ScriptTarget {
    pub const Latest: ScriptTarget = ScriptTarget::ESNext;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LanguageVariant {
    Standard,
    JSX,
}

#[derive(Debug)]
pub struct ParsedCommandLine {
    pub options: Rc<CompilerOptions>,
    pub file_names: Vec<String>,
}

pub struct CreateProgramOptions<'config> {
    pub root_names: &'config [String],
    pub options: Rc<CompilerOptions>,
}

#[derive(Debug)]
pub enum CommandLineOptionMapTypeValue {
    StaticStr(&'static str),
    String(String),
    ScriptTarget(ScriptTarget),
    ModuleKind(ModuleKind),
    JsxEmit(JsxEmit),
    ImportsNotUsedAsValues(ImportsNotUsedAsValues),
    ModuleResolutionKind(ModuleResolutionKind),
    NewLineKind(NewLineKind),
}

#[derive(Debug)]
pub enum CommandLineOptionType {
    String,
    Number,
    Boolean,
    Object,
    List,
    Map(HashMap<&'static str, CommandLineOptionMapTypeValue /*number | string*/>),
}

pub enum StringOrDiagnosticMessage {
    String(String),
    DiagnosticMessage(DiagnosticMessage),
}

pub trait CommandLineOptionInterface {
    fn command_line_option_wrapper(&self) -> Rc<CommandLineOption>;
    fn set_command_line_option_wrapper(&self, wrapper: Rc<CommandLineOption>);
    fn name(&self) -> &str;
    fn type_(&self) -> &CommandLineOptionType;
    fn is_file_path(&self) -> bool;
    fn maybe_short_name(&self) -> Option<&str>;
    fn maybe_description(&self) -> Option<&DiagnosticMessage>;
    fn maybe_default_value_description(&self) -> Option<&StringOrDiagnosticMessage>;
    fn maybe_param_type(&self) -> Option<&DiagnosticMessage>;
    fn is_tsconfig_only(&self) -> bool;
    fn is_command_line_only(&self) -> bool;
    fn show_in_simplified_help_view(&self) -> bool;
    fn maybe_category(&self) -> Option<&DiagnosticMessage>;
    fn strict_flag(&self) -> bool;
    fn affects_source_file(&self) -> bool;
    fn affects_module_resolution(&self) -> bool;
    fn affects_bind_diagnostics(&self) -> bool;
    fn affects_semantic_diagnostics(&self) -> bool;
    fn affects_emit(&self) -> bool;
    fn affects_program_structure(&self) -> bool;
    fn transpile_option_value(&self) -> bool;
}

pub struct CommandLineOptionBase {
    pub _command_line_option_wrapper: RefCell<Option<Weak<CommandLineOption>>>,
    pub name: String,
    pub type_: CommandLineOptionType,
    pub is_file_path: Option<bool>,
    pub short_name: Option<String>,
    pub description: Option<DiagnosticMessage>,
    pub default_value_description: Option<StringOrDiagnosticMessage>,
    pub param_type: Option<DiagnosticMessage>,
    pub is_tsconfig_only: Option<bool>,
    pub is_command_line_only: Option<bool>,
    pub show_in_simplified_help_view: Option<bool>,
    pub category: Option<DiagnosticMessage>,
    pub strict_flag: Option<bool>,
    pub affects_source_file: Option<bool>,
    pub affects_module_resolution: Option<bool>,
    pub affects_bind_diagnostics: Option<bool>,
    pub affects_semantic_diagnostics: Option<bool>,
    pub affects_emit: Option<bool>,
    pub affects_program_structure: Option<bool>,
    pub transpile_option_value: Option<bool>,
    // extra_validation: Option<Box<dyn Fn(CompilerOptionsValue) -> (DiagnosticMessage, Vec<String>)>>,
}

impl CommandLineOptionInterface for CommandLineOptionBase {
    fn command_line_option_wrapper(&self) -> Rc<CommandLineOption> {
        self._command_line_option_wrapper
            .borrow()
            .as_ref()
            .unwrap()
            .upgrade()
            .unwrap()
    }

    fn set_command_line_option_wrapper(&self, wrapper: Rc<CommandLineOption>) {
        *self._command_line_option_wrapper.borrow_mut() = Some(Rc::downgrade(&wrapper));
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn type_(&self) -> &CommandLineOptionType {
        &self.type_
    }

    fn is_file_path(&self) -> bool {
        self.is_file_path.unwrap_or(false)
    }

    fn maybe_short_name(&self) -> Option<&str> {
        self.short_name.as_deref()
    }

    fn maybe_description(&self) -> Option<&DiagnosticMessage> {
        self.description.as_ref()
    }

    fn maybe_default_value_description(&self) -> Option<&StringOrDiagnosticMessage> {
        self.default_value_description.as_ref()
    }

    fn maybe_param_type(&self) -> Option<&DiagnosticMessage> {
        self.param_type.as_ref()
    }

    fn is_tsconfig_only(&self) -> bool {
        self.is_tsconfig_only.unwrap_or(false)
    }

    fn is_command_line_only(&self) -> bool {
        self.is_command_line_only.unwrap_or(false)
    }

    fn show_in_simplified_help_view(&self) -> bool {
        self.show_in_simplified_help_view.unwrap_or(false)
    }

    fn maybe_category(&self) -> Option<&DiagnosticMessage> {
        self.category.as_ref()
    }

    fn strict_flag(&self) -> bool {
        self.strict_flag.unwrap_or(false)
    }

    fn affects_source_file(&self) -> bool {
        self.affects_source_file.unwrap_or(false)
    }

    fn affects_module_resolution(&self) -> bool {
        self.affects_module_resolution.unwrap_or(false)
    }

    fn affects_bind_diagnostics(&self) -> bool {
        self.affects_bind_diagnostics.unwrap_or(false)
    }

    fn affects_semantic_diagnostics(&self) -> bool {
        self.affects_semantic_diagnostics.unwrap_or(false)
    }

    fn affects_emit(&self) -> bool {
        self.affects_emit.unwrap_or(false)
    }

    fn affects_program_structure(&self) -> bool {
        self.affects_program_structure.unwrap_or(false)
    }

    fn transpile_option_value(&self) -> bool {
        self.transpile_option_value.unwrap_or(false)
    }
}

#[command_line_option_type]
pub struct CommandLineOptionOfStringType {
    _command_line_option_base: CommandLineOptionBase,
}

impl CommandLineOptionOfStringType {
    pub(crate) fn new(command_line_option_base: CommandLineOptionBase) -> Self {
        Self {
            _command_line_option_base: command_line_option_base,
        }
    }
}

#[command_line_option_type]
pub struct CommandLineOptionOfNumberType {
    _command_line_option_base: CommandLineOptionBase,
}

impl CommandLineOptionOfNumberType {
    pub(crate) fn new(command_line_option_base: CommandLineOptionBase) -> Self {
        Self {
            _command_line_option_base: command_line_option_base,
        }
    }
}

#[command_line_option_type]
pub struct CommandLineOptionOfBooleanType {
    _command_line_option_base: CommandLineOptionBase,
}

impl CommandLineOptionOfBooleanType {
    pub(crate) fn new(command_line_option_base: CommandLineOptionBase) -> Self {
        Self {
            _command_line_option_base: command_line_option_base,
        }
    }
}

#[command_line_option_type]
pub struct CommandLineOptionOfCustomType {
    _command_line_option_base: CommandLineOptionBase,
}

impl CommandLineOptionOfCustomType {
    pub(crate) fn new(command_line_option_base: CommandLineOptionBase) -> Self {
        Self {
            _command_line_option_base: command_line_option_base,
        }
    }
}

pub trait AlternateModeDiagnostics {
    fn diagnostic(&self) -> &DiagnosticMessage;
    fn get_options_name_map(&self) -> OptionsNameMap;
}

pub trait DidYouMeanOptionsDiagnostics {
    fn maybe_alternate_mode(&self) -> Option<Box<dyn AlternateModeDiagnostics>>;
    fn option_declarations(&self) -> &[CommandLineOption];
    fn unknown_option_diagnostic(&self) -> &DiagnosticMessage;
    fn unknown_did_you_mean_diagnostic(&self) -> &DiagnosticMessage;
}

#[command_line_option_type]
pub struct TsConfigOnlyOption {
    _command_line_option_base: CommandLineOptionBase,
    pub element_options: Option<HashMap<String, CommandLineOption>>,
    pub extra_key_diagnostics: Option<Box<dyn DidYouMeanOptionsDiagnostics>>,
}

impl TsConfigOnlyOption {
    pub(crate) fn new(
        command_line_option_base: CommandLineOptionBase,
        element_options: Option<HashMap<String, CommandLineOption>>,
        extra_key_diagnostics: Option<Box<dyn DidYouMeanOptionsDiagnostics>>,
    ) -> Self {
        Self {
            _command_line_option_base: command_line_option_base,
            element_options,
            extra_key_diagnostics,
        }
    }
}

#[command_line_option_type]
pub struct CommandLineOptionOfListType {
    _command_line_option_base: CommandLineOptionBase,
    pub element: Rc<CommandLineOption>,
}

impl CommandLineOptionOfListType {
    pub(crate) fn new(
        command_line_option_base: CommandLineOptionBase,
        element: Rc<CommandLineOption>,
    ) -> Self {
        Self {
            _command_line_option_base: command_line_option_base,
            element,
        }
    }
}

#[command_line_option_type(impl_from = false)]
pub enum CommandLineOption {
    CommandLineOptionOfCustomType(CommandLineOptionOfCustomType),
    CommandLineOptionOfStringType(CommandLineOptionOfStringType),
    CommandLineOptionOfNumberType(CommandLineOptionOfNumberType),
    CommandLineOptionOfBooleanType(CommandLineOptionOfBooleanType),
    TsConfigOnlyOption(TsConfigOnlyOption),
    CommandLineOptionOfListType(CommandLineOptionOfListType),
}

#[non_exhaustive]
pub struct CharacterCodes;
#[allow(non_upper_case_globals)]
impl CharacterCodes {
    pub const null_character: char = '\u{0000}';
    pub const max_ascii_character: char = '\u{007f}';

    pub const line_feed: char = '\n';
    pub const carriage_return: char = '\r';
    pub const line_separator: char = '\u{2028}';
    pub const paragraph_separator: char = '\u{2029}';
    pub const next_line: char = '\u{0085}';

    pub const space: char = ' ';
    pub const non_breaking_space: char = '\u{00a0}';
    pub const en_quad: char = '\u{2000}';
    pub const em_quad: char = '\u{2001}';
    pub const en_space: char = '\u{2002}';
    pub const em_space: char = '\u{2003}';
    pub const three_per_em_space: char = '\u{2004}';
    pub const four_per_em_space: char = '\u{2005}';
    pub const six_per_em_space: char = '\u{2006}';
    pub const figure_space: char = '\u{2007}';
    pub const punctuation_space: char = '\u{2008}';
    pub const thin_space: char = '\u{2009}';
    pub const hair_space: char = '\u{200a}';
    pub const zero_width_space: char = '\u{200b}';
    pub const narrow_no_break_space: char = '\u{202f}';
    pub const ideographic_space: char = '\u{3000}';
    pub const mathematical_space: char = '\u{205f}';
    pub const ogham: char = '\u{1680}';

    pub const underscore: char = '_';
    pub const dollar_sign: char = '$';

    pub const _0: char = '0';
    pub const _1: char = '1';
    pub const _2: char = '2';
    pub const _3: char = '3';
    pub const _4: char = '4';
    pub const _5: char = '5';
    pub const _6: char = '6';
    pub const _7: char = '7';
    pub const _8: char = '8';
    pub const _9: char = '9';

    pub const a: char = 'a';
    pub const b: char = 'b';
    pub const c: char = 'c';
    pub const d: char = 'd';
    pub const e: char = 'e';
    pub const f: char = 'f';
    pub const g: char = 'g';
    pub const h: char = 'h';
    pub const i: char = 'i';
    pub const j: char = 'j';
    pub const k: char = 'k';
    pub const l: char = 'l';
    pub const m: char = 'm';
    pub const n: char = 'n';
    pub const o: char = 'o';
    pub const p: char = 'p';
    pub const q: char = 'q';
    pub const r: char = 'r';
    pub const s: char = 's';
    pub const t: char = 't';
    pub const u: char = 'u';
    pub const v: char = 'v';
    pub const w: char = 'w';
    pub const x: char = 'x';
    pub const y: char = 'y';
    pub const z: char = 'z';

    pub const A: char = 'A';
    pub const B: char = 'B';
    pub const C: char = 'C';
    pub const D: char = 'D';
    pub const E: char = 'E';
    pub const F: char = 'F';
    pub const G: char = 'G';
    pub const H: char = 'H';
    pub const I: char = 'I';
    pub const J: char = 'J';
    pub const K: char = 'K';
    pub const L: char = 'L';
    pub const M: char = 'M';
    pub const N: char = 'N';
    pub const O: char = 'O';
    pub const P: char = 'P';
    pub const Q: char = 'Q';
    pub const R: char = 'R';
    pub const S: char = 'S';
    pub const T: char = 'T';
    pub const U: char = 'U';
    pub const V: char = 'V';
    pub const W: char = 'W';
    pub const X: char = 'X';
    pub const Y: char = 'Y';
    pub const Z: char = 'Z';

    pub const ampersand: char = '&';
    pub const asterisk: char = '*';
    pub const at: char = '@';
    pub const backslash: char = '\\';
    pub const backtick: char = '`';
    pub const bar: char = '|';
    pub const caret: char = '^';
    pub const close_brace: char = '}';
    pub const close_bracket: char = ']';
    pub const close_paren: char = ')';
    pub const colon: char = ':';
    pub const comma: char = ',';
    pub const dot: char = '.';
    pub const double_quote: char = '"';
    pub const equals: char = '=';
    pub const exclamation: char = '!';
    pub const greater_than: char = '>';
    pub const hash: char = '#';
    pub const less_than: char = '<';
    pub const minus: char = '-';
    pub const open_brace: char = '{';
    pub const open_bracket: char = '[';
    pub const open_paren: char = '(';
    pub const percent: char = '%';
    pub const plus: char = '+';
    pub const question: char = '?';
    pub const semicolon: char = ';';
    pub const single_quote: char = '\'';
    pub const slash: char = '/';
    pub const tilde: char = '~';

    pub const backspace: char = '\u{0008}';
    pub const form_feed: char = '\u{000c}';
    pub const byte_order_mark: char = '\u{feff}';
    pub const tab: char = '\t';
    pub const vertical_tab: char = '\u{000b}';
}

pub trait ModuleResolutionHost {
    fn read_file(&self, file_name: &str) -> Option<String>;
}

pub trait CompilerHost: ModuleResolutionHost {
    fn get_source_file(&self, file_name: &str) -> Option<Rc<Node /*SourceFile*/>>;
    fn get_current_directory(&self) -> String;
    fn get_canonical_file_name(&self, file_name: &str) -> String;
}

bitflags! {
    pub struct TransformFlags: u32 {
        const None = 0;

        const ContainsTypeScript = 1 << 0;

        const ContainsJsx = 1 << 1;
        const ContainsESNext = 1 << 2;
        const ContainsES2021 = 1 << 3;
        const ContainsES2020 = 1 << 4;
        const ContainsES2019 = 1 << 5;
        const ContainsES2018 = 1 << 6;
        const ContainsES2017 = 1 << 7;
        const ContainsES2016 = 1 << 8;
        const ContainsES2015 = 1 << 9;
        const ContainsGenerator = 1 << 10;
        const ContainsDestructuringAssignment = 1 << 11;

        const ContainsTypeScriptClassSyntax = 1 << 12;
        const ContainsLexicalThis = 1 << 13;
        const ContainsRestOrSpread = 1 << 14;
        const ContainsObjectRestOrSpread = 1 << 15;
        const ContainsComputedPropertyName = 1 << 16;
        const ContainsBlockScopedBinding = 1 << 17;
        const ContainsBindingPattern = 1 << 18;
        const ContainsYield = 1 << 19;
        const ContainsAwait = 1 << 20;
        const ContainsHoistedDeclarationOrCompletion = 1 << 21;
        const ContainsDynamicImport = 1 << 22;
        const ContainsClassFields = 1 << 23;
        const ContainsPossibleTopLevelAwait = 1 << 24;
        const ContainsLexicalSuper = 1 << 25;
        const ContainsUpdateExpressionForIdentifier = 1 << 26;
        const HasComputedFlags = 1 << 29;

        const AssertTypeScript = Self::ContainsTypeScript.bits;
        const AssertJsx = Self::ContainsJsx.bits;
        const AssertESNext = Self::ContainsESNext.bits;
        const AssertES2021 = Self::ContainsES2021.bits;
        const AssertES2020 = Self::ContainsES2020.bits;
        const AssertES2019 = Self::ContainsES2019.bits;
        const AssertES2018 = Self::ContainsES2018.bits;
        const AssertES2017 = Self::ContainsES2017.bits;
        const AssertES2016 = Self::ContainsES2016.bits;
        const AssertES2015 = Self::ContainsES2015.bits;
        const AssertGenerator = Self::ContainsGenerator.bits;
        const AssertDestructuringAssignment = Self::ContainsDestructuringAssignment.bits;

        const OuterExpressionExcludes = Self::HasComputedFlags.bits;
        const PropertyAccessExcludes = Self::OuterExpressionExcludes.bits;
        const NodeExcludes = Self::PropertyAccessExcludes.bits;
        const ArrowFunctionExcludes = Self::NodeExcludes.bits | Self::ContainsTypeScriptClassSyntax.bits | Self::ContainsBlockScopedBinding.bits | Self::ContainsYield.bits | Self::ContainsAwait.bits | Self::ContainsHoistedDeclarationOrCompletion.bits | Self::ContainsBindingPattern.bits |  Self::ContainsObjectRestOrSpread.bits |  Self::ContainsPossibleTopLevelAwait.bits;
        const FunctionExcludes = Self::NodeExcludes.bits | Self::ContainsTypeScriptClassSyntax.bits | Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits | Self::ContainsBlockScopedBinding.bits | Self::ContainsYield.bits | Self::ContainsAwait.bits |  Self::ContainsHoistedDeclarationOrCompletion.bits |  Self::ContainsBindingPattern.bits |  Self::ContainsObjectRestOrSpread.bits |  Self::ContainsPossibleTopLevelAwait.bits;
        const ConstructorExcludes = Self::NodeExcludes.bits | Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits | Self::ContainsBlockScopedBinding.bits | Self::ContainsYield.bits | Self::ContainsAwait.bits |  Self::ContainsHoistedDeclarationOrCompletion.bits |  Self::ContainsBindingPattern.bits |  Self::ContainsObjectRestOrSpread.bits |  Self::ContainsPossibleTopLevelAwait.bits;
        const MethodOrAccessorExcludes = Self::NodeExcludes.bits | Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits | Self::ContainsBlockScopedBinding.bits | Self::ContainsYield.bits | Self::ContainsAwait.bits |  Self::ContainsHoistedDeclarationOrCompletion.bits |  Self::ContainsBindingPattern.bits |  Self::ContainsObjectRestOrSpread.bits;
        const PropertyExcludes = Self::NodeExcludes.bits | Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits;
        const ClassExcludes = Self::NodeExcludes.bits | Self::ContainsTypeScriptClassSyntax.bits | Self::ContainsComputedPropertyName.bits;
        const ModuleExcludes = Self::NodeExcludes.bits | Self::ContainsTypeScriptClassSyntax.bits | Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits | Self::ContainsBlockScopedBinding.bits | Self::ContainsHoistedDeclarationOrCompletion.bits | Self::ContainsPossibleTopLevelAwait.bits;
        const TypeExcludes = !Self::ContainsTypeScript.bits;
        const ObjectLiteralExcludes = Self::NodeExcludes.bits | Self::ContainsTypeScriptClassSyntax.bits | Self::ContainsComputedPropertyName.bits | Self::ContainsObjectRestOrSpread.bits;
        const ArrayLiteralOrCallOrNewExcludes = Self::NodeExcludes.bits | Self::ContainsRestOrSpread.bits;
        const VariableDeclarationListExcludes = Self::NodeExcludes.bits | Self::ContainsBindingPattern.bits | Self::ContainsObjectRestOrSpread.bits;
        const ParameterExcludes = Self::NodeExcludes.bits;
        const CatchClauseExcludes = Self::NodeExcludes.bits | Self::ContainsObjectRestOrSpread.bits;
        const BindingPatternExcludes = Self::NodeExcludes.bits | Self::ContainsRestOrSpread.bits;
        const ContainsLexicalThisOrSuper = Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits;

        const PropertyNamePropagatingFlags = Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits;
    }
}

#[derive(Debug)]
pub struct SourceMapRange {
    pos: Cell<isize>,
    end: Cell<isize>,
    source: Option<SourceMapSource>,
}

impl SourceMapRange {
    pub fn new(pos: isize, end: isize, source: Option<SourceMapSource>) -> Self {
        Self {
            pos: Cell::new(pos),
            end: Cell::new(end),
            source,
        }
    }
}

impl TextRange for SourceMapRange {
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

#[derive(Debug)]
pub struct SourceMapSource {
    pub file_name: String,
    pub text: String,
    pub(crate) line_map: Vec<usize>,
    pub skip_trivia: Option<fn(usize) -> usize>,
}

#[derive(Clone, Debug)]
pub enum StringOrUsize {
    String(String),
    Usize(usize),
}

#[derive(Debug)]
pub struct EmitNode {
    pub annotated_nodes: Option<Vec<Rc<Node>>>,
    pub flags: Option<EmitFlags>,
    pub leading_comments: Option<Vec<Rc<SynthesizedComment>>>,
    pub trailing_comments: Option<Vec<Rc<SynthesizedComment>>>,
    pub comment_range: Option<BaseTextRange>,
    pub source_map_range: Option<Rc<SourceMapRange>>,
    pub token_source_map_ranges: Option<HashMap<SyntaxKind, Option<Rc<SourceMapRange>>>>,
    pub constant_value: Option<StringOrUsize>,
    pub external_helpers_module_name: Option<Rc<Node /*Identifier*/>>,
    pub external_helpers: Option<bool>,
    pub helpers: Option<Vec<Rc<EmitHelper>>>,
    pub starts_on_new_line: Option<bool>,
    pub snippet_element: Option<SnippetElement>,
}

impl Default for EmitNode {
    fn default() -> Self {
        Self {
            annotated_nodes: None,
            flags: None,
            leading_comments: None,
            trailing_comments: None,
            comment_range: None,
            source_map_range: None,
            token_source_map_ranges: None,
            constant_value: None,
            external_helpers_module_name: None,
            external_helpers: None,
            helpers: None,
            starts_on_new_line: None,
            snippet_element: None,
        }
    }
}

#[derive(Debug)]
pub enum SnippetElement {
    TabStop(TabStop),
    Placeholder(Placeholder),
}

#[derive(Debug)]
pub struct TabStop {
    pub kind: SnippetKind,
    pub order: usize,
}

#[derive(Debug)]
pub struct Placeholder {
    pub kind: SnippetKind,
    pub order: usize,
}

#[derive(Debug)]
pub enum SnippetKind {
    TabStop,
    Placeholder,
    Choice,
    Variable,
}

pub trait EmitHelperBase {
    fn name(&self) -> &str;
    fn scoped(&self) -> bool;
    fn text(&self) -> &str; // TODO: support callback value?
    fn priority(&self) -> Option<usize>;
    fn dependencies(&self) -> Option<&[Rc<EmitHelper>]>;
}

#[derive(Debug)]
pub struct ScopedEmitHelper {
    name: String,
    scoped: bool, /*true*/
    text: String,
    priority: Option<usize>,
    dependencies: Option<Vec<Rc<EmitHelper>>>,
}

impl EmitHelperBase for ScopedEmitHelper {
    fn name(&self) -> &str {
        &self.name
    }

    fn scoped(&self) -> bool {
        self.scoped
    }

    fn text(&self) -> &str {
        &self.text
    }

    fn priority(&self) -> Option<usize> {
        self.priority.clone()
    }

    fn dependencies(&self) -> Option<&[Rc<EmitHelper>]> {
        self.dependencies.as_deref()
    }
}

#[derive(Debug)]
pub struct UnscopedEmitHelper {
    name: String,
    scoped: bool, /*false*/
    text: String,
    priority: Option<usize>,
    dependencies: Option<Vec<Rc<EmitHelper>>>,
    pub(crate) import_name: Option<String>,
}

impl EmitHelperBase for UnscopedEmitHelper {
    fn name(&self) -> &str {
        &self.name
    }

    fn scoped(&self) -> bool {
        self.scoped
    }

    fn text(&self) -> &str {
        &self.text
    }

    fn priority(&self) -> Option<usize> {
        self.priority.clone()
    }

    fn dependencies(&self) -> Option<&[Rc<EmitHelper>]> {
        self.dependencies.as_deref()
    }
}

#[derive(Debug)]
pub enum EmitHelper {
    ScopedEmitHelper(ScopedEmitHelper),
    UnscopedEmitHelper(UnscopedEmitHelper),
}

bitflags! {
    pub struct EmitFlags: u32 {
        const None = 0;
        const SingleLine = 1 << 0;
        const AdviseOnEmitNode = 1 << 1;
        const NoSubstitution = 1 << 2;
        const CapturesThis = 1 << 3;
        const NoLeadingSourceMap = 1 << 4;
        const NoTrailingSourceMap = 1 << 5;
        const NoSourceMap = Self::NoLeadingSourceMap.bits | Self::NoTrailingSourceMap.bits;
        const NoNestedSourceMaps = 1 << 6;
        const NoTokenLeadingSourceMaps = 1 << 7;
        const NoTokenTrailingSourceMaps = 1 << 8;
        const NoTokenSourceMaps = Self::NoTokenLeadingSourceMaps.bits | Self::NoTokenTrailingSourceMaps.bits;
        const NoLeadingComments = 1 << 9;
        const NoTrailingComments = 1 << 10;
        const NoComments = Self::NoLeadingComments.bits | Self::NoTrailingComments.bits;
        const NoNestedComments = 1 << 11;
        const HelperName = 1 << 12;
        const ExportName = 1 << 13;
        const LocalName = 1 << 14;
        const InternalName = 1 << 15;
        const Indented = 1 << 16;
        const NoIndentation = 1 << 17;
        const AsyncFunctionBody = 1 << 18;
        const ReuseTempVariableScope = 1 << 19;
        const CustomPrologue = 1 << 20;
        const NoHoisting = 1 << 21;
        const HasEndOfDeclarationMarker = 1 << 22;
        const Iterator = 1 << 23;
        const NoAsciiEscaping = 1 << 24;
        const TypeScriptClassWrapper = 1 << 25;
        const NeverApplyImportHelper = 1 << 26;
        const IgnoreSourceNewlines = 1 << 27;
        const Immutable = 1 << 28;
        const IndirectCall = 1 << 29;
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EmitHint {
    Expression,
    Unspecified,
}

bitflags! {
    pub struct OuterExpressionKinds: u32 {
        const None = 0;
        const Parentheses = 1 << 0;
        const TypeAssertions = 1 << 1;
        const NonNullAssertions = 1 << 2;
        const PartiallyEmittedExpressions = 1 << 3;

        const Assertions = Self::TypeAssertions.bits | Self::NonNullAssertions.bits;
        const All = Self::Parentheses.bits | Self::Assertions.bits | Self::PartiallyEmittedExpressions.bits;

        const ExcludeJSDocTypeAssertion = 1 << 4;
    }
}

pub trait ParenthesizerRules<TBaseNodeFactory: BaseNodeFactory> {
    // fn get_parenthesize_left_side_of_binary_for_operator(&self, binary_operator: SyntaxKind) ->
    // fn get_parenthesize_right_side_of_binary_for_operator(&self, binary_operator: SyntaxKind) ->
    fn parenthesize_left_side_of_binary(
        &self,
        base_factory: &TBaseNodeFactory,
        binary_operator: SyntaxKind,
        left_side: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_right_side_of_binary(
        &self,
        base_factory: &TBaseNodeFactory,
        binary_operator: SyntaxKind,
        left_side: Option<Rc<Node /*Expression*/>>,
        right_side: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_computed_property_name(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_condition_of_conditional_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        condition: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_branch_of_conditional_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        branch: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_export_default(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_new(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_left_side_of_access(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_operand_of_postfix_unary(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: &Node, /*Expression*/
    ) -> Rc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_operand_of_prefix_unary(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: &Node, /*Expression*/
    ) -> Rc<Node /*UnaryExpression*/>;
    fn parenthesize_expressions_of_comma_delimited_list(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: NodeArray, /*<Expression>*/
    ) -> NodeArray /*<Expression>*/;
    fn parenthesize_expression_for_disallowed_comma(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_expression_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression*/
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_concise_body_of_arrow_function(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: &Node, /*Expression | ConciseBody*/
    ) -> Rc<Node /*Expression | ConciseBody*/>;
    fn parenthesize_member_of_conditional_type(
        &self,
        base_factory: &TBaseNodeFactory,
        member: &Node, /*TypeNode*/
    ) -> Rc<Node /*TypeNode*/>;
    fn parenthesize_member_of_element_type(
        &self,
        base_factory: &TBaseNodeFactory,
        member: &Node, /*TypeNode*/
    ) -> Rc<Node /*TypeNode*/>;
    fn parenthesize_element_type_of_array_type(
        &self,
        base_factory: &TBaseNodeFactory,
        member: &Node, /*TypeNode*/
    ) -> Rc<Node /*TypeNode*/>;
    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        base_factory: &TBaseNodeFactory,
        members: NodeArrayOrVec, /*<TypeNode>*/
    ) -> NodeArray /*<TypeNode>*/;
    fn parenthesize_type_arguments(
        &self,
        base_factory: &TBaseNodeFactory,
        type_parameters: Option<NodeArrayOrVec /*<TypeNode>*/>,
    ) -> Option<NodeArray /*<TypeNode>*/>;
}

pub trait NodeConverters<TBaseNodeFactory: BaseNodeFactory> {
    fn convert_to_function_block(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ConciseBody*/
        multi_line: Option<bool>,
    ) -> Rc<Node /*Block*/>;
    fn convert_to_function_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*FunctionDeclaration*/
    ) -> Rc<Node /*FunctionExpression*/>;
    fn convert_to_array_assignment_element(
        &self,
        base_factory: &TBaseNodeFactory,
        element: &Node, /*ArrayBindingOrAssignmentElement*/
    ) -> Rc<Node /*Expression*/>;
    fn convert_to_object_assignment_element(
        &self,
        base_factory: &TBaseNodeFactory,
        element: &Node, /*ObjectBindingOrAssignmentElement*/
    ) -> Rc<Node /*ObjectLiteralElementLike*/>;
    fn convert_to_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*BindingOrAssignmentPattern*/
    ) -> Rc<Node /*AssignmentPattern*/>;
    fn convert_to_object_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ObjectBindingOrAssignmentPattern*/
    ) -> Rc<Node /*ObjectLiteralExpression*/>;
    fn convert_to_array_assignment_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ArrayBindingOrAssignmentPattern*/
    ) -> Rc<Node /*ArrayLiteralExpression*/>;
    fn convert_to_assignment_element_target(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*BindingOrAssignmentElementTarget*/
    ) -> Rc<Node /*Expression*/>;
}

pub struct NodeFactory<TBaseNodeFactory> {
    pub flags: NodeFactoryFlags,
    pub parenthesizer_rules: RefCell<Option<Box<dyn ParenthesizerRules<TBaseNodeFactory>>>>,
    pub converters: RefCell<Option<Box<dyn NodeConverters<TBaseNodeFactory>>>>,
}
