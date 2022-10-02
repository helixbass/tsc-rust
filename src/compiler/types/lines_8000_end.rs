#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use derive_builder::Builder;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
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

#[derive(Debug)]
pub struct PragmaArgumentTypeFactory {
    pub factory: String,
}

#[derive(Debug)]
pub enum PragmaArgumentType {
    PragmaArgumentTypeFactory(PragmaArgumentTypeFactory),
}

impl PragmaArgumentType {
    pub fn as_pragma_argument_type_factory(&self) -> &PragmaArgumentTypeFactory {
        enum_unwrapped!(self, [PragmaArgumentType, PragmaArgumentTypeFactory])
    }

    pub fn factory(&self) -> String {
        match self {
            Self::PragmaArgumentTypeFactory(value) => value.factory.clone(),
        }
    }
}

#[derive(Debug)]
pub struct PragmaPseudoMapValue {
    pub arguments: PragmaArgumentType,
    pub range: CommentRange,
}

pub type ReadonlyPragmaMap = HashMap<String, Vec<Rc<PragmaPseudoMapValue>>>;

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
