#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use derive_builder::Builder;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use super::{DiagnosticMessage, ModuleResolutionKind, Node};
use crate::{
    hash_map_to_compiler_options, CompilerHost, Diagnostic, MapLike, Number, OptionsNameMap,
    Program, StringOrPattern,
};
use local_macros::{command_line_option_type, enum_unwrapped};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PluginImport {
    pub name: String,
}

#[derive(Debug)]
pub struct ProjectReference {
    pub path: String,
    pub original_path: Option<String>,
    pub prepend: Option<bool>,
    pub circular: Option<bool>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum WatchFileKind {
    FixedPollingInterval,
    PriorityPollingInterval,
    DynamicPriorityPolling,
    FixedChunkSizePolling,
    UseFsEvents,
    UseFsEventsOnParentDirectory,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum WatchDirectoryKind {
    UseFsEvents,
    FixedPollingInterval,
    DynamicPriorityPolling,
    FixedChunkSizePolling,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PollingWatchKind {
    FixedInterval,
    PriorityInterval,
    DynamicPriority,
    FixedChunkSize,
}

#[derive(Clone, Debug)]
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
    Number(Option<Number>),
    WatchFileKind(Option<WatchFileKind>),
    WatchDirectoryKind(Option<WatchDirectoryKind>),
    PollingWatchKind(Option<PollingWatchKind>),
}

impl CompilerOptionsValue {
    pub fn is_some(&self) -> bool {
        match self {
            Self::Bool(value) => value.is_some(),
            Self::String(value) => value.is_some(),
            Self::SourceFile(value) => value.is_some(),
            Self::ImportsNotUsedAsValues(value) => value.is_some(),
            Self::JsxEmit(value) => value.is_some(),
            Self::VecString(value) => value.is_some(),
            Self::Usize(value) => value.is_some(),
            Self::ModuleKind(value) => value.is_some(),
            Self::ModuleResolutionKind(value) => value.is_some(),
            Self::NewLineKind(value) => value.is_some(),
            Self::MapLikeVecString(value) => value.is_some(),
            Self::VecPluginImport(value) => value.is_some(),
            Self::ScriptTarget(value) => value.is_some(),
            Self::Number(value) => value.is_some(),
            Self::WatchFileKind(value) => value.is_some(),
            Self::WatchDirectoryKind(value) => value.is_some(),
            Self::PollingWatchKind(value) => value.is_some(),
        }
    }

    pub fn as_none(&self) -> Self {
        match self {
            Self::Bool(_) => Self::Bool(None),
            Self::String(_) => Self::String(None),
            Self::SourceFile(_) => Self::SourceFile(None),
            Self::ImportsNotUsedAsValues(_) => Self::ImportsNotUsedAsValues(None),
            Self::JsxEmit(_) => Self::JsxEmit(None),
            Self::VecString(_) => Self::VecString(None),
            Self::Usize(_) => Self::Usize(None),
            Self::ModuleKind(_) => Self::ModuleKind(None),
            Self::ModuleResolutionKind(_) => Self::ModuleResolutionKind(None),
            Self::NewLineKind(_) => Self::NewLineKind(None),
            Self::MapLikeVecString(_) => Self::MapLikeVecString(None),
            Self::VecPluginImport(_) => Self::VecPluginImport(None),
            Self::ScriptTarget(_) => Self::ScriptTarget(None),
            Self::Number(_) => Self::Number(None),
            Self::WatchFileKind(_) => Self::WatchFileKind(None),
            Self::WatchDirectoryKind(_) => Self::WatchDirectoryKind(None),
            Self::PollingWatchKind(_) => Self::PollingWatchKind(None),
        }
    }

    pub fn or_empty_vec(self) -> Self {
        match self {
            Self::VecString(option) => Self::VecString(option.or_else(|| Some(vec![]))),
            Self::VecPluginImport(option) => Self::VecPluginImport(option.or_else(|| Some(vec![]))),
            _ => panic!("Expected vec option"),
        }
    }

    pub fn as_option_bool(&self) -> Option<bool> {
        enum_unwrapped!(self, [CompilerOptionsValue, Bool]).clone()
    }

    pub fn into_option_bool(self) -> Option<bool> {
        enum_unwrapped!(self, [CompilerOptionsValue, Bool])
    }

    pub fn into_option_vec_string(self) -> Option<Vec<String>> {
        enum_unwrapped!(self, [CompilerOptionsValue, VecString])
    }

    pub fn into_option_watch_file_kind(self) -> Option<WatchFileKind> {
        enum_unwrapped!(self, [CompilerOptionsValue, WatchFileKind])
    }

    pub fn into_option_watch_directory_kind(self) -> Option<WatchDirectoryKind> {
        enum_unwrapped!(self, [CompilerOptionsValue, WatchDirectoryKind])
    }

    pub fn into_option_polling_watch_kind(self) -> Option<PollingWatchKind> {
        enum_unwrapped!(self, [CompilerOptionsValue, PollingWatchKind])
    }

    pub fn into_option_string(self) -> Option<String> {
        enum_unwrapped!(self, [CompilerOptionsValue, String])
    }

    pub fn into_option_vec_plugin_import(self) -> Option<Vec<PluginImport>> {
        enum_unwrapped!(self, [CompilerOptionsValue, VecPluginImport])
    }

    pub fn into_option_new_line_kind(self) -> Option<NewLineKind> {
        enum_unwrapped!(self, [CompilerOptionsValue, NewLineKind])
    }

    pub fn into_option_module_kind(self) -> Option<ModuleKind> {
        enum_unwrapped!(self, [CompilerOptionsValue, ModuleKind])
    }

    pub fn into_option_module_resolution_kind(self) -> Option<ModuleResolutionKind> {
        enum_unwrapped!(self, [CompilerOptionsValue, ModuleResolutionKind])
    }

    pub fn into_option_usize(self) -> Option<usize> {
        enum_unwrapped!(self, [CompilerOptionsValue, Usize])
    }

    pub fn into_option_rc_node(self) -> Option<Rc<Node>> {
        enum_unwrapped!(self, [CompilerOptionsValue, SourceFile])
    }

    pub fn into_option_imports_not_used_as_values(self) -> Option<ImportsNotUsedAsValues> {
        enum_unwrapped!(self, [CompilerOptionsValue, ImportsNotUsedAsValues])
    }

    pub fn into_option_map_like_vec_string(self) -> Option<MapLike<Vec<String>>> {
        enum_unwrapped!(self, [CompilerOptionsValue, MapLikeVecString])
    }

    pub fn into_option_script_target(self) -> Option<ScriptTarget> {
        enum_unwrapped!(self, [CompilerOptionsValue, ScriptTarget])
    }

    pub fn into_option_jsx_emit(self) -> Option<JsxEmit> {
        enum_unwrapped!(self, [CompilerOptionsValue, JsxEmit])
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
            (
                CompilerOptionsValue::Number(self_value),
                CompilerOptionsValue::Number(other_value),
            ) => self_value == other_value,
            (
                CompilerOptionsValue::WatchFileKind(self_value),
                CompilerOptionsValue::WatchFileKind(other_value),
            ) => self_value == other_value,
            (
                CompilerOptionsValue::WatchDirectoryKind(self_value),
                CompilerOptionsValue::WatchDirectoryKind(other_value),
            ) => self_value == other_value,
            (
                CompilerOptionsValue::PollingWatchKind(self_value),
                CompilerOptionsValue::PollingWatchKind(other_value),
            ) => self_value == other_value,
            _ => false,
        }
    }
}

impl Eq for CompilerOptionsValue {}

#[derive(Builder, Debug, Default)]
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
    #[builder(setter(skip))]
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

pub fn extend_compiler_options(a: &CompilerOptions, b: &CompilerOptions) -> CompilerOptions {
    CompilerOptions {
        all: a.all.or(b.all),
        allow_js: a.allow_js.or(b.allow_js),
        allow_non_ts_extensions: a.allow_non_ts_extensions.or(b.allow_non_ts_extensions),
        allow_synthetic_default_imports: a
            .allow_synthetic_default_imports
            .or(b.allow_synthetic_default_imports),
        allow_umd_global_access: a.allow_umd_global_access.or(b.allow_umd_global_access),
        allow_unreachable_code: a.allow_unreachable_code.or(b.allow_unreachable_code),
        allow_unused_labels: a.allow_unused_labels.or(b.allow_unused_labels),
        always_strict: a.always_strict.or(b.always_strict),
        base_url: a
            .base_url
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.base_url.as_ref().map(Clone::clone)),
        build: a.build.or(b.build),
        charset: a
            .charset
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.charset.as_ref().map(Clone::clone)),
        check_js: a.check_js.or(b.check_js),
        config_file_path: a
            .config_file_path
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.config_file_path.as_ref().map(Clone::clone)),
        config_file: a
            .config_file
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.config_file.as_ref().map(Clone::clone)),
        declaration: a.declaration.or(b.declaration),
        declaration_map: a.declaration_map.or(b.declaration_map),
        emit_declaration_only: a.emit_declaration_only.or(b.emit_declaration_only),
        declaration_dir: a
            .declaration_dir
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.declaration_dir.as_ref().map(Clone::clone)),
        diagnostics: a.diagnostics.or(b.diagnostics),
        extended_diagnostics: a.extended_diagnostics.or(b.extended_diagnostics),
        disable_size_limit: a.disable_size_limit.or(b.disable_size_limit),
        disable_source_of_project_reference_redirect: a
            .disable_source_of_project_reference_redirect
            .or(b.disable_source_of_project_reference_redirect),
        disable_solution_searching: a
            .disable_solution_searching
            .or(b.disable_solution_searching),
        disable_referenced_project_load: a
            .disable_referenced_project_load
            .or(b.disable_referenced_project_load),
        downlevel_iteration: a.downlevel_iteration.or(b.downlevel_iteration),
        emit_bom: a.emit_bom.or(b.emit_bom),
        emit_decorator_metadata: a.emit_decorator_metadata.or(b.emit_decorator_metadata),
        exact_optional_property_types: a
            .exact_optional_property_types
            .or(b.exact_optional_property_types),
        experimental_decorators: a.experimental_decorators.or(b.experimental_decorators),
        force_consistent_casing_in_file_names: a
            .force_consistent_casing_in_file_names
            .or(b.force_consistent_casing_in_file_names),
        generate_cpu_profile: a
            .generate_cpu_profile
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.generate_cpu_profile.as_ref().map(Clone::clone)),
        generate_trace: a
            .generate_trace
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.generate_trace.as_ref().map(Clone::clone)),
        help: a.help.or(b.help),
        import_helpers: a.import_helpers.or(b.import_helpers),
        imports_not_used_as_values: a
            .imports_not_used_as_values
            .or(b.imports_not_used_as_values),
        init: a.init.or(b.init),
        inline_source_map: a.inline_source_map.or(b.inline_source_map),
        inline_sources: a.inline_sources.or(b.inline_sources),
        isolated_modules: a.isolated_modules.or(b.isolated_modules),
        jsx: a.jsx.or(b.jsx),
        keyof_strings_only: a.keyof_strings_only.or(b.keyof_strings_only),
        lib: a
            .lib
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.lib.as_ref().map(Clone::clone)),
        list_emitted_files: a.list_emitted_files.or(b.list_emitted_files),
        list_files: a.list_files.or(b.list_files),
        explain_files: a.explain_files.or(b.explain_files),
        list_files_only: a.list_files_only.or(b.list_files_only),
        locale: a
            .locale
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.locale.as_ref().map(Clone::clone)),
        map_root: a
            .map_root
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.map_root.as_ref().map(Clone::clone)),
        max_node_module_js_depth: a.max_node_module_js_depth.or(b.max_node_module_js_depth),
        module: a.module.or(b.module),
        module_resolution: a.module_resolution.or(b.module_resolution),
        new_line: a.new_line.or(b.new_line),
        no_emit: a.no_emit.or(b.no_emit),
        no_emit_for_js_files: a.no_emit_for_js_files.or(b.no_emit_for_js_files),
        no_emit_helpers: a.no_emit_helpers.or(b.no_emit_helpers),
        no_emit_on_error: a.no_emit_on_error.or(b.no_emit_on_error),
        no_error_truncation: a.no_error_truncation.or(b.no_error_truncation),
        no_fallthrough_cases_in_switch: a
            .no_fallthrough_cases_in_switch
            .or(b.no_fallthrough_cases_in_switch),
        no_implicit_any: a.no_implicit_any.or(b.no_implicit_any),
        no_implicit_returns: a.no_implicit_returns.or(b.no_implicit_returns),
        no_implicit_this: a.no_implicit_this.or(b.no_implicit_this),
        no_strict_generic_checks: a.no_strict_generic_checks.or(b.no_strict_generic_checks),
        no_unused_locals: a.no_unused_locals.or(b.no_unused_locals),
        no_unused_parameters: a.no_unused_parameters.or(b.no_unused_parameters),
        no_implicit_use_strict: a.no_implicit_use_strict.or(b.no_implicit_use_strict),
        no_property_access_from_index_signature: a
            .no_property_access_from_index_signature
            .or(b.no_property_access_from_index_signature),
        assume_changes_only_affect_direct_dependencies: a
            .assume_changes_only_affect_direct_dependencies
            .or(b.assume_changes_only_affect_direct_dependencies),
        no_lib: a.no_lib.or(b.no_lib),
        no_resolve: a.no_resolve.or(b.no_resolve),
        no_unchecked_indexed_access: a
            .no_unchecked_indexed_access
            .or(b.no_unchecked_indexed_access),
        out: a
            .out
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.out.as_ref().map(Clone::clone)),
        out_dir: a
            .out_dir
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.out_dir.as_ref().map(Clone::clone)),
        out_file: a
            .out_file
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.out_file.as_ref().map(Clone::clone)),
        paths: a
            .paths
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.paths.as_ref().map(Clone::clone)),
        paths_base_path: a
            .paths_base_path
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.paths_base_path.as_ref().map(Clone::clone)),
        plugins: a
            .plugins
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.plugins.as_ref().map(Clone::clone)),
        preserve_const_enums: a.preserve_const_enums.or(b.preserve_const_enums),
        no_implicit_override: a.no_implicit_override.or(b.no_implicit_override),
        preserve_symlinks: a.preserve_symlinks.or(b.preserve_symlinks),
        preserve_value_imports: a.preserve_value_imports.or(b.preserve_value_imports),
        preserve_watch_output: a.preserve_watch_output.or(b.preserve_watch_output),
        project: a
            .project
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.project.as_ref().map(Clone::clone)),
        pretty: a.pretty.or(b.pretty),
        react_namespace: a
            .react_namespace
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.react_namespace.as_ref().map(Clone::clone)),
        jsx_factory: a
            .jsx_factory
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.jsx_factory.as_ref().map(Clone::clone)),
        jsx_fragment_factory: a
            .jsx_fragment_factory
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.jsx_fragment_factory.as_ref().map(Clone::clone)),
        jsx_import_source: a
            .jsx_import_source
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.jsx_import_source.as_ref().map(Clone::clone)),
        composite: a.composite.or(b.composite),
        incremental: a.incremental.or(b.incremental),
        ts_build_info_file: a
            .ts_build_info_file
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.ts_build_info_file.as_ref().map(Clone::clone)),
        remove_comments: a.remove_comments.or(b.remove_comments),
        root_dir: a
            .root_dir
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.root_dir.as_ref().map(Clone::clone)),
        root_dirs: a
            .root_dirs
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.root_dirs.as_ref().map(Clone::clone)),
        skip_lib_check: a.skip_lib_check.or(b.skip_lib_check),
        skip_default_lib_check: a.skip_default_lib_check.or(b.skip_default_lib_check),
        source_map: a.source_map.or(b.source_map),
        source_root: a
            .source_root
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.source_root.as_ref().map(Clone::clone)),
        strict: a.strict.or(b.strict),
        strict_function_types: a.strict_function_types.or(b.strict_function_types),
        strict_bind_call_apply: a.strict_bind_call_apply.or(b.strict_bind_call_apply),
        strict_null_checks: a.strict_null_checks.or(b.strict_null_checks),
        strict_property_initialization: a
            .strict_property_initialization
            .or(b.strict_property_initialization),
        strip_internal: a.strip_internal.or(b.strip_internal),
        suppress_excess_property_errors: a
            .suppress_excess_property_errors
            .or(b.suppress_excess_property_errors),
        suppress_implicit_any_index_errors: a
            .suppress_implicit_any_index_errors
            .or(b.suppress_implicit_any_index_errors),
        suppress_output_path_check: a
            .suppress_output_path_check
            .or(b.suppress_output_path_check),
        target: a.target.or(b.target),
        trace_resolution: a.trace_resolution.or(b.trace_resolution),
        use_unknown_in_catch_variables: a
            .use_unknown_in_catch_variables
            .or(b.use_unknown_in_catch_variables),
        resolve_json_module: a.resolve_json_module.or(b.resolve_json_module),
        types: a
            .types
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.types.as_ref().map(Clone::clone)),
        type_roots: a
            .type_roots
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.type_roots.as_ref().map(Clone::clone)),
        version: a.version.or(b.version),
        watch: a.watch.or(b.watch),
        es_module_interop: a.es_module_interop.or(b.es_module_interop),
        show_config: a.show_config.or(b.show_config),
        use_define_for_class_fields: a
            .use_define_for_class_fields
            .or(b.use_define_for_class_fields),
    }
}

pub fn maybe_extend_compiler_options(
    a: Option<&CompilerOptions>,
    b: Option<&CompilerOptions>,
) -> CompilerOptions {
    match (a, b) {
        (Some(a), Some(b)) => extend_compiler_options(a, b),
        _ => {
            let default: CompilerOptions = Default::default();
            extend_compiler_options(a.unwrap_or(&default), b.unwrap_or(&default))
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct WatchOptions {
    pub watch_file: Option<WatchFileKind>,
    pub watch_directory: Option<WatchDirectoryKind>,
    pub fallback_polling: Option<PollingWatchKind>,
    pub synchronous_watch_directory: Option<bool>,
    pub exclude_directories: Option<Vec<String>>,
    pub exclude_files: Option<Vec<String>>,
    // [option: string]: CompilerOptionsValue | undefined;
}

pub fn extend_watch_options(a: &WatchOptions, b: &WatchOptions) -> WatchOptions {
    WatchOptions {
        watch_file: a.watch_file.or(b.watch_file),
        watch_directory: a.watch_directory.or(b.watch_directory),
        fallback_polling: a.fallback_polling.or(b.fallback_polling),
        synchronous_watch_directory: a
            .synchronous_watch_directory
            .or(b.synchronous_watch_directory),
        exclude_directories: a
            .exclude_directories
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.exclude_directories.as_ref().map(Clone::clone)),
        exclude_files: a
            .exclude_files
            .as_ref()
            .map(Clone::clone)
            .or_else(|| b.exclude_files.as_ref().map(Clone::clone)),
    }
}

#[derive(Clone, Debug)]
pub struct TypeAcquisition {
    pub enable_auto_discovery: Option<bool>,
    pub enable: Option<bool>,
    pub include: Option<Vec<String>>,
    pub exclude: Option<Vec<String>>,
    pub disable_filename_based_type_acquisition: Option<bool>,
    // [option: string]: CompilerOptionsValue | undefined;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
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
pub struct ParsedCommandLineWithBaseOptions {
    pub options: HashMap<String, CompilerOptionsValue>,
    pub type_acquisition: Option<Rc<TypeAcquisition>>,
    pub file_names: Vec<String>,
    pub project_references: Option<Vec<Rc<ProjectReference>>>,
    pub watch_options: Option<Rc<WatchOptions>>,
    pub raw: Option<serde_json::Value>,
    pub errors: Vec<Rc<Diagnostic>>,
    pub wildcard_directories: Option<HashMap<String, WatchDirectoryFlags>>,
    pub compile_on_save: Option<bool>,
}

impl ParsedCommandLineWithBaseOptions {
    pub fn into_parsed_command_line(self) -> ParsedCommandLine {
        ParsedCommandLine {
            options: Rc::new(hash_map_to_compiler_options(&self.options)),
            type_acquisition: self.type_acquisition,
            file_names: self.file_names,
            project_references: self.project_references,
            watch_options: self.watch_options,
            raw: self.raw,
            errors: self.errors,
            wildcard_directories: self.wildcard_directories,
            compile_on_save: self.compile_on_save,
        }
    }
}

#[derive(Debug)]
pub struct ParsedCommandLine {
    pub options: Rc<CompilerOptions>,
    pub type_acquisition: Option<Rc<TypeAcquisition>>,
    pub file_names: Vec<String>,
    pub project_references: Option<Vec<Rc<ProjectReference>>>,
    pub watch_options: Option<Rc<WatchOptions>>,
    pub raw: Option<serde_json::Value>,
    pub errors: Vec<Rc<Diagnostic>>,
    pub wildcard_directories: Option<HashMap<String, WatchDirectoryFlags>>,
    pub compile_on_save: Option<bool>,
}

bitflags! {
    pub struct WatchDirectoryFlags: u32 {
        const None = 0;
        const Recursive = 1 << 0;
    }
}

#[derive(Debug)]
pub struct ConfigFileSpecs {
    pub files_specs: Option<Vec<String>>,
    pub include_specs: Option<Vec<String>>,
    pub exclude_specs: Option<Vec<String>>,
    pub validated_files_spec: Option<Vec<String>>,
    pub validated_include_specs: Option<Vec<String>>,
    pub validated_exclude_specs: Option<Vec<String>>,
    pub path_patterns: Option<Vec<StringOrPattern>>,
}

pub struct CreateProgramOptions {
    pub root_names: Vec<String>,
    pub options: Rc<CompilerOptions>,
    pub project_references: Option<Vec<Rc<ProjectReference>>>,
    pub host: Option<Rc<dyn CompilerHost>>,
    pub old_program: Option<Rc<Program>>,
    pub config_file_parsing_diagnostics: Option<Vec<Rc<Diagnostic>>>,
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
    WatchFileKind(WatchFileKind),
    WatchDirectoryKind(WatchDirectoryKind),
    PollingWatchKind(PollingWatchKind),
}

impl CommandLineOptionMapTypeValue {
    pub fn as_compiler_options_value(&self) -> CompilerOptionsValue {
        match self {
            Self::StaticStr(value) => CompilerOptionsValue::String(Some((*value).to_owned())),
            Self::String(value) => CompilerOptionsValue::String(Some(value.clone())),
            Self::ScriptTarget(value) => CompilerOptionsValue::ScriptTarget(Some(*value)),
            Self::ModuleKind(value) => CompilerOptionsValue::ModuleKind(Some(*value)),
            Self::JsxEmit(value) => CompilerOptionsValue::JsxEmit(Some(*value)),
            Self::ImportsNotUsedAsValues(value) => {
                CompilerOptionsValue::ImportsNotUsedAsValues(Some(*value))
            }
            Self::ModuleResolutionKind(value) => {
                CompilerOptionsValue::ModuleResolutionKind(Some(*value))
            }
            Self::NewLineKind(value) => CompilerOptionsValue::NewLineKind(Some(*value)),
            Self::WatchFileKind(value) => CompilerOptionsValue::WatchFileKind(Some(*value)),
            Self::WatchDirectoryKind(value) => {
                CompilerOptionsValue::WatchDirectoryKind(Some(*value))
            }
            Self::PollingWatchKind(value) => CompilerOptionsValue::PollingWatchKind(Some(*value)),
        }
    }
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

impl CommandLineOptionType {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::String => "string",
            Self::Number => "number",
            Self::Boolean => "boolean",
            Self::Object => "object",
            Self::List => "list",
            Self::Map(_) => "map",
        }
    }

    pub fn as_map(&self) -> &HashMap<&'static str, CommandLineOptionMapTypeValue> {
        enum_unwrapped!(self, [CommandLineOptionType, Map])
    }
}

#[derive(Eq, PartialEq)]
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
    // TODO: rename maybe_transpile_option_value() for consistency?
    fn transpile_option_value(&self) -> Option<Option<bool>>;
    fn maybe_extra_validation(
        &self,
    ) -> Option<
        fn(Option<&serde_json::Value>) -> Option<(&'static DiagnosticMessage, Option<Vec<String>>)>,
    >;
    fn maybe_extra_validation_compiler_options_value(
        &self,
    ) -> Option<
        fn(&CompilerOptionsValue) -> Option<(&'static DiagnosticMessage, Option<Vec<String>>)>,
    >;
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
    pub transpile_option_value: Option<Option<bool>>,
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

    fn transpile_option_value(&self) -> Option<Option<bool>> {
        self.transpile_option_value
    }

    fn maybe_extra_validation(
        &self,
    ) -> Option<
        fn(Option<&serde_json::Value>) -> Option<(&'static DiagnosticMessage, Option<Vec<String>>)>,
    > {
        None
    }

    fn maybe_extra_validation_compiler_options_value(
        &self,
    ) -> Option<
        fn(&CompilerOptionsValue) -> Option<(&'static DiagnosticMessage, Option<Vec<String>>)>,
    > {
        None
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

pub struct AlternateModeDiagnostics {
    pub diagnostic: &'static DiagnosticMessage,
    pub get_options_name_map: fn() -> Rc<OptionsNameMap>,
}

pub trait DidYouMeanOptionsDiagnostics {
    fn maybe_alternate_mode(&self) -> Option<Rc<AlternateModeDiagnostics>>;
    fn option_declarations(&self) -> Vec<Rc<CommandLineOption>>;
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

impl CommandLineOption {
    pub fn as_command_line_option_of_list_type(&self) -> &CommandLineOptionOfListType {
        enum_unwrapped!(self, [CommandLineOption, CommandLineOptionOfListType])
    }

    pub fn as_ts_config_only_option(&self) -> &TsConfigOnlyOption {
        enum_unwrapped!(self, [CommandLineOption, TsConfigOnlyOption])
    }

    pub fn to_compiler_options_value_none(&self) -> CompilerOptionsValue {
        match self {
            Self::CommandLineOptionOfCustomType(_) => self
                .type_()
                .as_map()
                .values()
                .next()
                .unwrap()
                .as_compiler_options_value()
                .as_none(),
            Self::CommandLineOptionOfStringType(_) => CompilerOptionsValue::String(None),
            Self::CommandLineOptionOfNumberType(_) => CompilerOptionsValue::Usize(None),
            Self::CommandLineOptionOfBooleanType(_) => CompilerOptionsValue::Bool(None),
            Self::TsConfigOnlyOption(_) => CompilerOptionsValue::MapLikeVecString(None),
            Self::CommandLineOptionOfListType(list_type) => match list_type.element.type_() {
                CommandLineOptionType::String => CompilerOptionsValue::VecString(None),
                CommandLineOptionType::Object => CompilerOptionsValue::VecPluginImport(None),
                _ => panic!("Unexpected element type"),
            },
        }
    }

    pub fn to_compiler_options_value(&self, value: &serde_json::Value) -> CompilerOptionsValue {
        unimplemented!()
    }
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
