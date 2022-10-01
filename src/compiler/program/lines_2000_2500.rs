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
    pub(super) fn get_bind_and_check_diagnostics_for_file(
        &self,
        source_file: &Node, /*SourceFile*/
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_and_cache_diagnostics(
            source_file,
            cancellation_token,
            Program::get_bind_and_check_diagnostics_for_file_no_cache,
        )
    }

    pub(super) fn get_bind_and_check_diagnostics_for_file_no_cache(
        &self,
        source_file: &Node, /*SourceFile*/
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        // self.run_with_cancellation_token(|| {
        let type_checker = self.get_diagnostics_producing_type_checker();

        let include_bind_and_check_diagnostics = true;
        let check_diagnostics = if include_bind_and_check_diagnostics {
            type_checker.get_diagnostics(Some(source_file), cancellation_token)
        } else {
            vec![]
        };

        check_diagnostics
        // })
    }

    pub(super) fn get_and_cache_diagnostics(
        &self,
        source_file: &Node, /*SourceFile*/
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
        get_diagnostics: fn(
            &Program,
            &Node, /*SourceFile*/
            Option<Rc<dyn CancellationTokenDebuggable>>,
        ) -> Vec<Rc<Diagnostic>>,
    ) -> Vec<Rc<Diagnostic>> {
        let result = get_diagnostics(self, source_file, cancellation_token);
        result
    }

    pub fn get_options_diagnostics(
        &self,
        _cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> SortedArray<Rc<Diagnostic>> {
        SortedArray::new(vec![])
    }

    pub fn get_global_diagnostics(
        &self,
        _cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> SortedArray<Rc<Diagnostic>> {
        SortedArray::new(vec![])
    }

    pub fn get_config_file_parsing_diagnostics(&self) -> Vec<Rc<Diagnostic>> {
        vec![]
    }

    pub fn process_root_file(
        &self,
        file_name: &str,
        is_default_lib: bool,
        ignore_no_default_lib: bool,
        reason: &FileIncludeReason,
    ) {
        self.process_source_file(
            &normalize_path(file_name),
            is_default_lib,
            ignore_no_default_lib,
            None,
            reason,
        );
    }

    pub fn create_synthetic_import(&self, text: &str, file: &Node /*SourceFile*/) -> Rc<Node> {
        let external_helpers_module_reference: Rc<Node> =
            with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                factory
                    .create_string_literal(synthetic_factory, text.to_owned(), None, None)
                    .into()
            });
        let import_decl: Rc<Node> =
            with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                factory
                    .create_import_declaration(
                        synthetic_factory,
                        Option::<NodeArray>::None,
                        Option::<NodeArray>::None,
                        None,
                        external_helpers_module_reference.clone(),
                        None,
                    )
                    .into()
            });
        add_emit_flags(import_decl.clone(), EmitFlags::NeverApplyImportHelper);
        set_parent(&external_helpers_module_reference, Some(&*import_decl));
        set_parent(&import_decl, Some(file));
        external_helpers_module_reference
            .set_flags(external_helpers_module_reference.flags() & !NodeFlags::Synthesized);
        import_decl.set_flags(import_decl.flags() & !NodeFlags::Synthesized);
        external_helpers_module_reference
    }

    pub fn collect_external_module_references(&self, file: &Node /*SourceFile*/) {
        let file_as_source_file = file.as_source_file();
        if file_as_source_file.maybe_imports().is_some() {
            return;
        }

        let is_java_script_file = is_source_file_js(file);
        let is_external_module_file = is_external_module(file);

        let mut imports: Option<Vec<Rc<Node /*StringLiteralLike*/>>> = None;
        let mut module_augmentations: Option<Vec<Rc<Node /*StringLiteral | Identifier*/>>> = None;
        let mut ambient_modules: Option<Vec<String>> = None;

        if (self.options.isolated_modules == Some(true) || is_external_module_file)
            && !file_as_source_file.is_declaration_file()
        {
            if self.options.import_helpers == Some(true) {
                imports =
                    Some(vec![self.create_synthetic_import(
                        external_helpers_module_name_text,
                        file,
                    )]);
            }
            let jsx_import = get_jsx_runtime_import(
                get_jsx_implicit_import_base(&self.options, Some(file)).as_deref(),
                &self.options,
            );
            if let Some(jsx_import) = jsx_import
                .as_ref()
                .filter(|jsx_import| !jsx_import.is_empty())
            {
                imports
                    .get_or_insert_with(|| vec![])
                    .push(self.create_synthetic_import(jsx_import, file));
            }
        }

        for node in &file_as_source_file.statements {
            self.collect_module_references(
                &mut imports,
                file,
                is_external_module_file,
                &mut module_augmentations,
                &mut ambient_modules,
                node,
                false,
            );
        }
        if file
            .flags()
            .intersects(NodeFlags::PossiblyContainsDynamicImport)
            || is_java_script_file
        {
            self.collect_dynamic_import_or_require_calls(is_java_script_file, &mut imports, file);
        }

        *file_as_source_file.maybe_imports() = Some(imports.unwrap_or_else(|| vec![]));
        *file_as_source_file.maybe_module_augmentations() =
            Some(module_augmentations.unwrap_or_else(|| vec![]));
        *file_as_source_file.maybe_ambient_module_names() =
            Some(ambient_modules.unwrap_or_else(|| vec![]));
    }
}
