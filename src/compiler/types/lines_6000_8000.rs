#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::rc::Rc;

use super::{ModuleResolutionKind, Node, NodeArray, SourceFile, SyntaxKind};
use crate::{MapLike, NodeFactoryFlags};

#[derive(Debug)]
pub struct PluginImport {
    pub name: String,
}

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
    pub(crate) config_file: Option<Rc<SourceFile /*TsConfigSourceFile*/>>,
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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
    fn get_source_file(&self, file_name: &str) -> Option<Rc<SourceFile>>;
    fn get_current_directory(&self) -> String;
    fn get_canonical_file_name(&self, file_name: &str) -> String;
}

bitflags! {
    pub struct EmitFlags: u32 {
        const None = 0;
        const NoAsciiEscaping = 1 << 24;
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EmitHint {
    Expression,
    Unspecified,
}

pub trait ParenthesizerRules {
    // fn get_parenthesize_left_side_of_binary_for_operator(&self, binary_operator: SyntaxKind) ->
    // fn get_parenthesize_right_side_of_binary_for_operator(&self, binary_operator: SyntaxKind) ->
    fn parenthesize_left_side_of_binary(
        &self,
        binary_operator: SyntaxKind,
        left_side: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_right_side_of_binary(
        &self,
        binary_operator: SyntaxKind,
        left_side: Option<Rc<Node /*Expression*/>>,
        right_side: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_computed_property_name(
        &self,
        expression: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_condition_of_conditional_expression(
        &self,
        condition: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_branch_of_conditional_expression(
        &self,
        branch: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_export_default(
        &self,
        expression: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_new(
        &self,
        expression: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_left_side_of_access(
        &self,
        expression: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_operand_of_postfix_unary(
        &self,
        operand: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_operand_of_prefix_unary(
        &self,
        operand: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*UnaryExpression*/>;
    fn parenthesize_expressions_of_comma_delimited_list(
        &self,
        elements: NodeArray, /*<Expression>*/
    ) -> NodeArray /*<Expression>*/;
    fn parenthesize_expression_for_disallowed_comma(
        &self,
        expression: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_expression_of_expression_statement(
        &self,
        expression: Rc<Node /*Expression*/>,
    ) -> Rc<Node /*Expression*/>;
    fn parenthesize_concise_body_of_arrow_function(
        &self,
        expression: Rc<Node /*Expression | ConciseBody*/>,
    ) -> Rc<Node /*Expression | ConciseBody*/>;
    fn parenthesize_member_of_conditional_type(
        &self,
        member: Rc<Node /*TypeNode*/>,
    ) -> Rc<Node /*TypeNode*/>;
    fn parenthesize_member_of_element_type(
        &self,
        member: Rc<Node /*TypeNode*/>,
    ) -> Rc<Node /*TypeNode*/>;
    fn parenthesize_element_type_of_array_type(
        &self,
        member: Rc<Node /*TypeNode*/>,
    ) -> Rc<Node /*TypeNode*/>;
    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        members: NodeArray, /*<TypeNode>*/
    ) -> NodeArray /*<TypeNode>*/;
    fn parenthesize_type_arguments(
        &self,
        type_parameters: Option<NodeArray /*<TypeNode>*/>,
    ) -> Option<NodeArray /*<TypeNode>*/>;
}

pub struct NodeFactory {
    pub flags: NodeFactoryFlags,
}
