#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use derive_builder::Builder;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::iter::FromIterator;
use std::rc::Rc;

use super::{BaseNode, CommentDirective, Diagnostic, Node, Symbol, SymbolFlags, SymbolWriter};
use crate::{
    CommentRange, FileIncludeReason, ModuleKind, MultiMap, NewLineKind, Path, RedirectTargetsMap,
    ScriptTarget, SortedArray, SymlinkCache,
};
use local_macros::{ast_type, enum_unwrapped};

pub struct Printer {
    pub current_source_file: Option<Rc<Node /*SourceFile*/>>,
    pub writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    pub write: fn(&Printer, &str),
}

pub(crate) type BuildInfo = ();

pub trait PrintHandlers {
    fn has_global_name(&self, name: &str) -> Option<bool>;
}

#[derive(Builder, Default)]
#[builder(default)]
pub struct PrinterOptions {
    pub remove_comments: Option<bool>,
    pub new_line: Option<NewLineKind>,
    pub omit_trailing_semicolon: Option<bool>,
    pub no_emit_helpers: Option<bool>,
    pub(crate) module: Option<ModuleKind>,
    pub(crate) target: Option<ScriptTarget>,
    pub(crate) source_map: Option<bool>,
    pub(crate) inline_source_map: Option<bool>,
    pub(crate) inline_sources: Option<bool>,
    pub(crate) extended_diagnostics: Option<bool>,
    pub(crate) only_print_js_doc_style: Option<bool>,
    pub(crate) never_ascii_escape: Option<bool>,
    pub(crate) write_bundle_info_file: Option<bool>,
    pub(crate) record_internal_section: Option<bool>,
    pub(crate) strip_internal: Option<bool>,
    pub(crate) preserve_source_newlines: Option<bool>,
    pub(crate) terminate_unterminated_literals: Option<bool>,
    // pub(crate) relative_to_build_info: Option<fn(&str) -> String>,
}

pub(crate) struct RawSourceMap {
    pub version: u32, /*3*/
    pub file: String,
    pub source_root: Option<String>,
    pub sources: Vec<String>,
    pub sources_content: Option<Vec<Option<String>>>,
    pub mappings: String,
    pub names: Option<Vec<String>>,
}

pub trait EmitTextWriter: SymbolWriter {
    fn write(&mut self, s: &str);
    fn write_trailing_semicolon(&mut self, text: &str);
    fn write_comment(&mut self, text: &str);
    fn get_text(&self) -> String;
    fn raw_write(&mut self, text: &str);
    fn write_literal(&mut self, text: &str);
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
    fn non_escaping_write(&mut self, text: &str) {}
}

pub trait ModuleSpecifierResolutionHost {
    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        None
    }
    fn file_exists(&self, path: &str) -> bool;
    fn get_current_directory(&self) -> String;
    fn directory_exists(&self, path: &str) -> Option<bool> {
        None
    }
    fn read_file(&self, path: &str) -> Option<io::Result<String>> {
        None
    }
    fn is_read_file_supported(&self) -> bool;
    fn realpath(&self, path: &str) -> Option<String> {
        None
    }
    fn get_symlink_cache(&self) -> Option<Rc<SymlinkCache>> {
        None
    }
    fn get_module_specifier_cache(&self) -> Option<Rc<dyn ModuleSpecifierCache>> {
        None
    }
    fn get_global_typings_cache_location(&self) -> Option<String> {
        None
    }
    fn get_nearest_ancestor_directory_with_package_json(
        &self,
        file_name: &str,
        root_dir: Option<&str>,
    ) -> Option<String> {
        None
    }

    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>>;
    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String>;
    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool;
    fn get_file_include_reasons(&self) -> Rc<RefCell<MultiMap<Path, FileIncludeReason>>>;
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
}

pub trait SymbolTracker {
    fn track_symbol(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<Rc<Node>>,
        meaning: SymbolFlags,
    ) -> Option<bool> {
        None
    }
    fn is_track_symbol_supported(&self) -> bool;
    fn report_inaccessible_this_error(&self) {}
    fn is_report_inaccessible_this_error_supported(&self) -> bool;
    fn report_private_in_base_of_class_expression(&self, property_name: &str) {}
    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool;
    fn report_inaccessible_unique_symbol_error(&self) {}
    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool;
    fn report_cyclic_structure_error(&self) {}
    fn is_report_cyclic_structure_error_supported(&self) -> bool;
    fn report_likely_unsafe_import_required_error(&self, specifier: &str) {}
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
        decl: &Node, /*ModuleDeclaration*/
        symbol: &Symbol,
    ) {
    }
    fn is_track_referenced_ambient_module_supported(&self) -> bool;
    fn track_external_module_symbol_of_import_type_node(&self, symbol: &Symbol) {}
    fn report_nonlocal_augmentation(
        &self,
        containing_file: &Node, /*SourceFile*/
        parent_symbol: &Symbol,
        augmenting_symbol: &Symbol,
    ) {
    }
    fn is_report_nonlocal_augmentation_supported(&self) -> bool;
    fn report_non_serializable_property(&self, property_name: &str) {}
    fn is_report_non_serializable_property_supported(&self) -> bool;
}

pub trait ModuleSpecifierResolutionHostAndGetCommonSourceDirectory:
    ModuleSpecifierResolutionHost
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

#[derive(Debug)]
pub struct DiagnosticCollection {
    pub non_file_diagnostics: SortedArray<Rc<Diagnostic>>,
    pub files_with_diagnostics: SortedArray<String>,
    pub file_diagnostics: HashMap<String, SortedArray<Rc<Diagnostic /*DiagnosticWithLocation*/>>>,
    pub has_read_non_file_diagnostics: bool,
}

#[derive(Debug)]
#[ast_type]
pub struct SyntaxList {
    _node: BaseNode,
    pub _children: Vec<Rc<Node>>,
}

bitflags! {
    pub struct ListFormat: u32 {
        const None = 0;

        const SingleLine = 0;

        const BarDelimited = 1 << 2;
        const AmpersandDelimited = 1 << 3;
        const CommaDelimited = 1 << 4;
        const AsteriskDelimited = 1 << 5;
        const DelimitersMask = Self::BarDelimited.bits | Self::AmpersandDelimited.bits | Self::CommaDelimited.bits | Self::AsteriskDelimited.bits;

        const SpaceBetweenBraces = 1 << 8;
        const SpaceBetweenSiblings = 1 << 9;

        const NoSpaceIfEmpty = 1 << 19;
        const SpaceAfterList = 1 << 21;

        const SingleLineTypeLiteralMembers = Self::SingleLine.bits | Self::SpaceBetweenBraces.bits | Self::SpaceBetweenSiblings.bits;

        const UnionTypeConstituents = Self::BarDelimited.bits | Self::SpaceBetweenSiblings.bits | Self::SingleLine.bits;
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
