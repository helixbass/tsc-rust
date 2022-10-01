use regex::Regex;
use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::cmp;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::io;
use std::rc::Rc;
use std::time;
use std::time::SystemTime;

use super::filter_semantic_diagnostics;
use crate::{
    add_emit_flags, append, change_extension, clone, combine_paths, compare_paths, concatenate,
    contains_path, convert_to_relative_path, create_diagnostic_collection,
    create_diagnostic_for_node_in_source_file, create_get_canonical_file_name,
    create_module_resolution_cache, create_multi_map, create_source_file, create_symlink_cache,
    create_type_checker, create_type_reference_directive_resolution_cache,
    diagnostic_category_name, extension_from_path, external_helpers_module_name_text,
    file_extension_is, file_extension_is_one_of, flatten, for_each,
    for_each_ancestor_directory_str, for_each_bool, for_each_child_returns, generate_djb2_hash,
    get_allow_js_compiler_option, get_automatic_type_directive_names,
    get_common_source_directory_of_config, get_default_lib_file_name, get_directory_path,
    get_emit_module_resolution_kind, get_emit_script_target, get_external_module_name,
    get_jsx_implicit_import_base, get_jsx_runtime_import, get_line_and_character_of_position,
    get_new_line_character, get_normalized_absolute_path,
    get_normalized_absolute_path_without_root, get_normalized_path_components,
    get_output_declaration_file_name, get_package_scope_for_path, get_path_from_path_components,
    get_property_assignment, get_spelling_suggestion, get_strict_option_value,
    get_supported_extensions, get_supported_extensions_with_json_if_resolve_json_module, get_sys,
    get_text_of_identifier_or_literal, has_extension, has_js_file_extension, has_jsdoc_nodes,
    has_syntactic_modifier, is_ambient_module, is_any_import_or_re_export,
    is_declaration_file_name, is_external_module, is_external_module_name_relative, is_import_call,
    is_import_equals_declaration, is_in_js_file, is_literal_import_type_node,
    is_module_declaration, is_require_call, is_rooted_disk_path, is_source_file_js,
    is_string_literal, is_string_literal_like, is_watch_set, lib_map, libs, map_defined,
    maybe_for_each, maybe_map, missing_file_modified_time, node_modules_path_part, normalize_path,
    options_have_changes, out_file, package_id_to_string, remove_file_extension, remove_prefix,
    remove_suffix, resolution_extension_is_ts_or_json, resolve_config_file_project_name,
    resolve_module_name, resolve_type_reference_directive, set_parent, set_parent_recursive,
    set_resolved_module, set_resolved_type_reference_directive,
    source_file_affecting_compiler_options, stable_sort, starts_with, string_contains,
    supported_js_extensions_flat, to_file_name_lower_case, to_path as to_path_helper,
    walk_up_parenthesized_expressions, with_synthetic_factory_and_factory,
    write_file_ensuring_directories, AutomaticTypeDirectiveFile, CancellationTokenDebuggable,
    Comparison, CompilerHost, CompilerOptions, CompilerOptionsBuilder,
    ConfigFileDiagnosticsReporter, CreateProgramOptions, CustomTransformers, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticMessageText,
    DiagnosticRelatedInformationInterface, Diagnostics, DirectoryStructureHost, EmitFlags,
    EmitResult, Extension, FileIncludeKind, FileIncludeReason, FilePreprocessingDiagnostics,
    FilePreprocessingDiagnosticsKind, FilePreprocessingFileExplainingDiagnostic,
    FilePreprocessingReferencedDiagnostic, FileReference, LibFile, LineAndCharacter,
    LiteralLikeNodeInterface, ModifierFlags, ModuleKind, ModuleResolutionCache,
    ModuleResolutionHost, ModuleResolutionHostOverrider, ModuleResolutionKind,
    ModuleSpecifierResolutionHost, MultiMap, NamedDeclarationInterface, Node, NodeArray, NodeFlags,
    NodeInterface, PackageId, PackageJsonInfoCache, ParseConfigFileHost, ParseConfigHost,
    ParsedCommandLine, Path, Program, ProjectReference, ReadonlyTextRange, RedirectTargetsMap,
    ReferencedFile, ResolvedConfigFileName, ResolvedModuleFull, ResolvedProjectReference,
    ResolvedTypeReferenceDirective, RootFile, ScriptReferenceHost, ScriptTarget, SortedArray,
    SourceFile, SourceFileLike, SourceOfProjectReferenceRedirect, StringOrRcNode,
    StructureIsReused, SymlinkCache, SyntaxKind, System, TypeChecker, TypeCheckerHost,
    TypeCheckerHostDebuggable, TypeReferenceDirectiveResolutionCache, WriteFileCallback,
};

impl Program {
    pub(super) fn resolve_type_reference_directive_names_worker<
        TContainingFile: Into<StringOrRcNode>,
    >(
        &self,
        type_directive_names: &[String],
        containing_file: TContainingFile,
    ) -> Vec<Option<Rc<ResolvedTypeReferenceDirective>>> {
        if type_directive_names.is_empty() {
            return vec![];
        }
        let containing_file: StringOrRcNode = containing_file.into();
        let containing_file_name = match &containing_file {
            StringOrRcNode::RcNode(containing_file) => get_normalized_absolute_path(
                &containing_file.as_source_file().original_file_name(),
                Some(&self.current_directory()),
            ),
            StringOrRcNode::String(containing_file) => containing_file.clone(),
        };
        let redirected_reference = match &containing_file {
            StringOrRcNode::RcNode(containing_file) => {
                self.get_redirect_reference_for_resolution(containing_file)
            }
            StringOrRcNode::String(_) => None,
        };
        // tracing?.push(tracing.Phase.Program, "resolveTypeReferenceDirectiveNamesWorker", { containingFileName });
        // performance.mark("beforeResolveTypeReference");
        let result = self
            .actual_resolve_type_reference_directive_names_worker()
            .call(
                type_directive_names,
                &containing_file_name,
                redirected_reference.clone(),
            );
        // performance.mark("afterResolveTypeReference");
        // performance.measure("ResolveTypeReference", "beforeResolveTypeReference", "afterResolveTypeReference");
        // tracing?.pop();
        result
    }

    pub(super) fn get_redirect_reference_for_resolution(
        &self,
        file: &Node, /*SourceFile*/
    ) -> Option<Rc<ResolvedProjectReference>> {
        unimplemented!()
    }

    pub(super) fn compare_default_lib_files(
        &self,
        a: &Node, /*SourceFile*/
        b: &Node, /*SourceFile*/
    ) -> Comparison {
        unimplemented!()
    }

    pub(super) fn has_invalidated_resolution(&self, source_file: &Path) -> bool {
        self.host()
            .has_invalidated_resolution(source_file)
            .unwrap_or(false)
    }

    pub fn get_root_file_names(&self) -> &[String] {
        unimplemented!()
    }

    pub fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        self.options.clone()
    }

    pub fn use_case_sensitive_file_names(&self) -> bool {
        unimplemented!()
    }

    pub fn get_file_include_reasons(&self) -> MultiMap<Path, FileIncludeReason> {
        unimplemented!()
    }

    pub fn get_source_file_(&self, file_name: &str) -> Option<Rc<Node /*SourceFile*/>> {
        unimplemented!()
    }

    pub fn get_syntactic_diagnostics(
        &self,
        source_file: Option<&Node /*SourceFile*/>,
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>> {
        self.get_diagnostics_helper(
            Program::get_syntactic_diagnostics_for_file,
            cancellation_token,
        )
    }

    pub fn get_semantic_diagnostics(
        &self,
        source_file: Option<&Node /*SourceFile*/>,
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_helper(
            Program::get_semantic_diagnostics_for_file,
            cancellation_token,
        )
    }

    pub fn emit(
        &self,
        target_source_file: Option<&Node /*SourceFile*/>,
        write_file: Option<&dyn WriteFileCallback>,
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
        emit_only_dts_files: Option<bool>,
        custom_transformers: Option<CustomTransformers>,
        force_dts_emit: Option<bool>,
    ) -> EmitResult {
        EmitResult {
            emit_skipped: true,
            diagnostics: vec![],
            emitted_files: None,
            source_maps: None,
            exported_modules_from_declaration_emit: None,
        }
    }

    pub fn get_current_directory(&self) -> String {
        self.current_directory().clone()
    }

    pub fn to_path(&self, file_name: &str) -> Path {
        to_path_helper(file_name, Some(&self.current_directory()), |file_name| {
            self.get_canonical_file_name(file_name)
        })
    }

    pub fn to_path_rc(&self) -> Rc<dyn Fn(&str) -> Path> {
        let self_clone = self.rc_wrapper();
        Rc::new(move |file_name| self_clone.to_path(file_name))
    }

    pub(super) fn resolve_module_names_reusing_old_state(
        &self,
        module_names: &[String],
        file: &Node, /*SourceFile*/
    ) -> Vec<Rc<ResolvedModuleFull>> {
        unimplemented!()
    }

    pub fn try_reuse_structure_from_old_program(&self) -> StructureIsReused {
        if self.maybe_old_program().is_none() {
            return StructureIsReused::Not;
        }

        unimplemented!()
    }

    pub fn get_resolved_project_references(
        &self,
    ) -> Ref<Option<Vec<Option<Rc<ResolvedProjectReference>>>>> {
        self.resolved_project_references.borrow()
    }

    pub fn is_source_file_default_library(&self, file: &Node /*SourceFile*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_diagnostics_producing_type_checker(&self) -> Rc<TypeChecker> {
        // self.diagnostics_producing_type_checker
        //     .get_or_insert_with(|| create_type_checker(self, true))

        // if let Some(type_checker) = self.diagnostics_producing_type_checker.as_ref() {
        //     return type_checker;
        // } else {
        //     self.diagnostics_producing_type_checker = Some(create_type_checker(self, true));
        //     self.diagnostics_producing_type_checker.as_ref().unwrap()
        // }
        let mut diagnostics_producing_type_checker =
            self.diagnostics_producing_type_checker.borrow_mut();
        if diagnostics_producing_type_checker.is_none() {
            *diagnostics_producing_type_checker =
                Some(create_type_checker(self.rc_wrapper(), true));
        }
        diagnostics_producing_type_checker.as_ref().unwrap().clone()
    }

    pub(super) fn get_diagnostics_helper(
        &self,
        get_diagnostics: fn(
            &Program,
            &Node, /*SourceFile*/
            Option<Rc<dyn CancellationTokenDebuggable>>,
        ) -> Vec<Rc<Diagnostic>>,
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_source_files()
            .iter()
            .flat_map(|source_file| get_diagnostics(self, source_file, cancellation_token.clone()))
            .collect()
    }

    pub(super) fn get_program_diagnostics(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Diagnostic>> {
        vec![]
    }

    pub(super) fn run_with_cancellation_token<TReturn, TClosure: FnOnce() -> TReturn>(
        &self,
        func: TClosure,
    ) -> TReturn {
        func()
    }

    pub(super) fn get_syntactic_diagnostics_for_file(
        &self,
        source_file: &Node, /*SourceFile*/
        // TODO: getSyntacticDiagnosticsForFile() doesn't actually take this argument, should
        // refactor eg get_diagnostics_helper() to use closures instead?
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        source_file.as_source_file().parse_diagnostics().clone()
    }

    pub(super) fn get_semantic_diagnostics_for_file(
        &self,
        source_file: &Node, /*SourceFile*/
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        concatenate(
            filter_semantic_diagnostics(
                self.get_bind_and_check_diagnostics_for_file(source_file, cancellation_token),
            ),
            self.get_program_diagnostics(source_file),
        )
    }
}
