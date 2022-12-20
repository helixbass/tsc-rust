use gc::{Finalize, Gc, GcCell, Trace};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::io;
use std::iter::FromIterator;
use std::ptr;
use std::rc::Rc;

use super::{
    for_each_project_reference, get_mode_for_resolution_at_index, SourceFileImportsList, ToPath,
};
use crate::{
    chain_diagnostic_messages, chain_diagnostic_messages_multiple, compare_paths, contains_path,
    create_compiler_diagnostic, create_compiler_diagnostic_from_message_chain,
    create_diagnostic_for_node_in_source_file, create_file_diagnostic,
    create_file_diagnostic_from_message_chain, create_symlink_cache, explain_if_file_is_redirect,
    file_extension_is, file_extension_is_one_of, file_include_reason_to_diagnostics, find,
    get_allow_js_compiler_option, get_base_file_name, get_directory_path, get_emit_declarations,
    get_emit_module_kind, get_emit_module_resolution_kind, get_emit_script_target,
    get_error_span_for_node, get_property_assignment, get_referenced_file_location,
    get_root_length, get_spelling_suggestion, get_strict_option_value,
    get_ts_build_info_emit_output_file_path, get_ts_config_object_literal_expression,
    has_json_module_emit_enabled, has_zero_or_one_asterisk_character, inverse_jsx_option_map,
    is_declaration_file_name, is_external_module, is_identifier_text, is_in_js_file,
    is_incremental_compilation, is_object_literal_expression, is_option_str_empty,
    is_reference_file_location, is_referenced_file, is_source_file_js, lib_map, libs,
    maybe_for_each, out_file, parse_isolated_entity_name, path_is_absolute, path_is_relative,
    remove_file_extension, remove_prefix, remove_suffix, resolution_extension_is_ts_or_json,
    resolve_config_file_project_name, set_resolved_module, source_file_may_be_emitted,
    string_contains, supported_js_extensions_flat, to_file_name_lower_case, version, Comparison,
    CompilerHost, CompilerOptions, ConfigFileDiagnosticsReporter, Debug_, Diagnostic,
    DiagnosticInterface, DiagnosticMessage, DiagnosticMessageChain, DiagnosticRelatedInformation,
    Diagnostics, DirectoryStructureHost, Extension, FileIncludeKind, FileIncludeReason,
    FilePreprocessingDiagnostics, FilePreprocessingDiagnosticsKind,
    FilePreprocessingFileExplainingDiagnostic, FilePreprocessingReferencedDiagnostic,
    FileReference, GetCanonicalFileName, JsxEmit, ModuleKind, ModuleResolutionHost,
    ModuleResolutionHostOverrider, ModuleResolutionKind, NamedDeclarationInterface, Node,
    NodeFlags, NodeInterface, ParseConfigFileHost, ParseConfigHost, Path, Program,
    ProjectReference, ReferencedFile, ResolvedConfigFileName, ResolvedModuleFull,
    ResolvedProjectReference, ScriptKind, ScriptReferenceHost, ScriptTarget, SymlinkCache,
    SyntaxKind,
};

impl Program {
    pub fn process_lib_reference_directives(&self, file: &Node /*SourceFile*/) {
        let file_as_source_file = file.as_source_file();
        maybe_for_each(
            file_as_source_file
                .maybe_lib_reference_directives()
                .as_ref(),
            |lib_reference: &FileReference, index| -> Option<()> {
                let lib_name = to_file_name_lower_case(&lib_reference.file_name);
                let lib_file_name = lib_map.with(|lib_map_| lib_map_.get(&&*lib_name).copied());
                if let Some(lib_file_name) = lib_file_name {
                    self.process_root_file(
                        &self.path_for_lib_file(lib_file_name),
                        true,
                        true,
                        &FileIncludeReason::ReferencedFile(ReferencedFile {
                            kind: FileIncludeKind::LibReferenceDirective,
                            file: file_as_source_file.path().clone(),
                            index,
                        }),
                    );
                } else {
                    let unqualified_lib_name =
                        remove_suffix(remove_prefix(&lib_name, "lib."), ".d.ts");
                    let suggestion = libs.with(|libs_| {
                        get_spelling_suggestion(unqualified_lib_name, libs_, |lib| {
                            Some((*lib).to_owned())
                        })
                        .map(|suggestion| (*suggestion).to_owned())
                    });
                    let diagnostic = if suggestion.is_some() {
                        &*Diagnostics::Cannot_find_lib_definition_for_0_Did_you_mean_1
                    } else {
                        &*Diagnostics::Cannot_find_lib_definition_for_0
                    };
                    self.maybe_file_processing_diagnostics().get_or_insert_with(|| {
                        vec![]
                    }).push(
                        FilePreprocessingDiagnostics::FilePreprocessingReferencedDiagnostic(FilePreprocessingReferencedDiagnostic {
                            kind: FilePreprocessingDiagnosticsKind::FilePreprocessingReferencedDiagnostic,
                            reason: ReferencedFile {
                                kind: FileIncludeKind::LibReferenceDirective,
                                file: file_as_source_file.path().clone(),
                                index,
                            },
                            diagnostic,
                            args: if let Some(suggestion) = suggestion {
                                Some(vec![
                                    lib_name,
                                    suggestion,
                                ])
                            } else {
                                Some(vec![
                                    lib_name,
                                ])
                            }
                        })
                    );
                }
                None
            },
        );
    }

    pub fn get_canonical_file_name(&self, file_name: &str) -> String {
        self.host().get_canonical_file_name(file_name)
    }

    pub fn get_canonical_file_name_rc(&self) -> Gc<Box<dyn GetCanonicalFileName>> {
        let host = self.host();
        Gc::new(Box::new(CompilerHostGetCanonicalFileName::new(host)))
    }

    pub fn process_imported_modules(&self, file: &Node /*SourceFile*/) {
        self.collect_external_module_references(file);
        let file_as_source_file = file.as_source_file();
        if !file_as_source_file
            .maybe_imports()
            .as_ref()
            .unwrap()
            .is_empty()
            || !file_as_source_file
                .maybe_module_augmentations()
                .as_ref()
                .unwrap()
                .is_empty()
        {
            let module_names = get_module_names(file);
            let resolutions = self.resolve_module_names_reusing_old_state(&module_names, file);
            Debug_.assert(resolutions.len() == module_names.len(), None);
            let options_for_file = if self.use_source_of_project_reference_redirect() {
                self.get_redirect_reference_for_resolution(file)
                    .map(|value| value.command_line.options.clone())
            } else {
                None
            }
            .unwrap_or_else(|| self.options.clone());
            for index in 0..module_names.len() {
                let resolution = resolutions[index].as_ref();
                set_resolved_module(
                    file,
                    &module_names[index],
                    resolution.cloned(),
                    get_mode_for_resolution_at_index(file_as_source_file, index),
                );

                if resolution.is_none() {
                    continue;
                }
                let resolution = resolution.unwrap();

                let is_from_node_modules_search = resolution.is_external_library_import;
                let is_js_file = !resolution_extension_is_ts_or_json(resolution.extension());
                let is_js_file_from_node_modules =
                    is_from_node_modules_search == Some(true) && is_js_file;
                let resolved_file_name = &resolution.resolved_file_name;

                if is_from_node_modules_search == Some(true) {
                    self.set_current_node_modules_depth(self.current_node_modules_depth() + 1);
                }

                let elide_import = is_js_file_from_node_modules
                    && self.current_node_modules_depth() > self.max_node_module_js_depth;
                let should_add_file = !resolved_file_name.is_empty()
                    && get_resolution_diagnostic(&options_for_file, resolution).is_none()
                    && options_for_file.no_resolve != Some(true)
                    && index < file_as_source_file.maybe_imports().as_ref().unwrap().len()
                    && !elide_import
                    && !(is_js_file && !get_allow_js_compiler_option(&options_for_file))
                    && (is_in_js_file(Some(
                        &*file_as_source_file.maybe_imports().as_ref().unwrap()[index],
                    )) || !file_as_source_file.maybe_imports().as_ref().unwrap()[index]
                        .flags()
                        .intersects(NodeFlags::JSDoc));

                if elide_import {
                    self.modules_with_elided_imports()
                        .insert(file_as_source_file.path().to_string(), true);
                } else if should_add_file {
                    self.find_source_file(
                        resolved_file_name,
                        false,
                        false,
                        &FileIncludeReason::ReferencedFile(ReferencedFile {
                            kind: FileIncludeKind::Import,
                            file: file_as_source_file.path().clone(),
                            index,
                        }),
                        resolution.package_id.as_ref(),
                    );
                }

                if is_from_node_modules_search == Some(true) {
                    self.set_current_node_modules_depth(self.current_node_modules_depth() - 1);
                }
            }
        } else {
            *file_as_source_file.maybe_resolved_modules() = None;
        }
    }

    pub fn check_source_files_belong_to_path(
        &self,
        source_files: &[Gc<Node /*SourceFile*/>],
        root_directory: &str,
    ) -> bool {
        unimplemented!()
    }

    pub fn parse_project_reference_config_file(
        &self,
        ref_: &ProjectReference,
    ) -> Option<Gc<ResolvedProjectReference>> {
        if self.maybe_project_reference_redirects().is_none() {
            *self.maybe_project_reference_redirects() = Some(HashMap::new());
        }

        let ref_path = resolve_project_reference_path(ref_);
        let source_file_path = self.to_path(&ref_path);
        let from_cache = self
            .maybe_project_reference_redirects()
            .as_ref()
            .unwrap()
            .get(&source_file_path)
            .cloned();
        if let Some(from_cache) = from_cache {
            return from_cache;
        }
        unimplemented!()
    }

    pub fn verify_compiler_options(&self) {
        let is_nightly = string_contains(version, "dev");
        if !is_nightly {
            if get_emit_module_kind(&self.options) == ModuleKind::Node12 {
                self.create_option_value_diagnostic(
                    "module",
                    &Diagnostics::Compiler_option_0_of_value_1_is_unstable_Use_nightly_TypeScript_to_silence_this_error_Try_updating_with_npm_install_D_typescript_next,
                    Some(vec![
                        "module".to_owned(),
                        "node12".to_owned(),
                    ])
                );
            } else if get_emit_module_kind(&self.options) == ModuleKind::NodeNext {
                self.create_option_value_diagnostic(
                    "module",
                    &Diagnostics::Compiler_option_0_of_value_1_is_unstable_Use_nightly_TypeScript_to_silence_this_error_Try_updating_with_npm_install_D_typescript_next,
                    Some(vec![
                        "module".to_owned(),
                        "nodenext".to_owned(),
                    ])
                );
            } else if get_emit_module_resolution_kind(&self.options) == ModuleResolutionKind::Node12
            {
                self.create_option_value_diagnostic(
                    "moduleResolution",
                    &Diagnostics::Compiler_option_0_of_value_1_is_unstable_Use_nightly_TypeScript_to_silence_this_error_Try_updating_with_npm_install_D_typescript_next,
                    Some(vec![
                        "moduleResolution".to_owned(),
                        "node12".to_owned(),
                    ])
                );
            } else if get_emit_module_resolution_kind(&self.options)
                == ModuleResolutionKind::NodeNext
            {
                self.create_option_value_diagnostic(
                    "moduleResolution",
                    &Diagnostics::Compiler_option_0_of_value_1_is_unstable_Use_nightly_TypeScript_to_silence_this_error_Try_updating_with_npm_install_D_typescript_next,
                    Some(vec![
                        "moduleResolution".to_owned(),
                        "nodenext".to_owned(),
                    ])
                );
            }
        }
        if self.options.strict_property_initialization == Some(true)
            && !get_strict_option_value(&self.options, "strictNullChecks")
        {
            self.create_diagnostic_for_option_name(
                &Diagnostics::Option_0_cannot_be_specified_without_specifying_option_1,
                "strictPropertyInitialization",
                Some("strictNullChecks"),
                None,
            );
        }
        if self.options.exact_optional_property_types == Some(true)
            && !get_strict_option_value(&self.options, "strictNullChecks")
        {
            self.create_diagnostic_for_option_name(
                &Diagnostics::Option_0_cannot_be_specified_without_specifying_option_1,
                "exactOptionalPropertyTypes",
                Some("strictNullChecks"),
                None,
            );
        }

        if self.options.isolated_modules == Some(true) {
            if !is_option_str_empty(self.options.out.as_deref()) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_with_option_1,
                    "out",
                    Some("isolatedModules"),
                    None,
                );
            }

            if !is_option_str_empty(self.options.out_file.as_deref()) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_with_option_1,
                    "outFile",
                    Some("isolatedModules"),
                    None,
                );
            }
        }

        if self.options.inline_source_map == Some(true) {
            if self.options.source_map == Some(true) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_with_option_1,
                    "sourceMap",
                    Some("inlineSourceMap"),
                    None,
                );
            }

            if !is_option_str_empty(self.options.map_root.as_deref()) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_with_option_1,
                    "mapRoot",
                    Some("inlineSourceMap"),
                    None,
                );
            }
        }

        if self.options.composite == Some(true) {
            if self.options.declaration == Some(false) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Composite_projects_may_not_disable_declaration_emit,
                    "declaration",
                    None,
                    None,
                );
            }
            if self.options.incremental == Some(false) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Composite_projects_may_not_disable_incremental_compilation,
                    "declaration",
                    None,
                    None,
                );
            }
        }

        let output_file = out_file(&self.options);
        if !is_option_str_empty(self.options.ts_build_info_file.as_deref()) {
            if !is_incremental_compilation(&self.options) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_without_specifying_option_1_or_option_2,
                    "tsBuildInfoFile",
                    Some("incremental"),
                    Some("composite"),
                );
            }
        } else if self.options.incremental == Some(true)
            && is_option_str_empty(output_file)
            && is_option_str_empty(self.options.config_file_path.as_deref())
        {
            self.program_diagnostics_mut().add(
                Gc::new(
                    create_compiler_diagnostic(
                        &Diagnostics::Option_incremental_can_only_be_specified_using_tsconfig_emitting_to_single_file_or_when_option_tsBuildInfoFile_is_specified,
                        None,
                    ).into()
                )
            );
        }

        self.verify_project_references();

        if self.options.composite == Some(true) {
            let root_paths: HashSet<Path> = HashSet::from_iter(
                self.root_names()
                    .iter()
                    .map(|root_name| self.to_path(root_name)),
            );
            for file in &*self.files() {
                if source_file_may_be_emitted(file, self, None)
                    && !root_paths.contains(&*file.as_source_file().path())
                {
                    self.add_program_diagnostic_explaining_file(
                        file,
                        &Diagnostics::File_0_is_not_listed_within_the_file_list_of_project_1_Projects_must_list_all_files_or_use_an_include_pattern,
                        Some(vec![
                            file.as_source_file().file_name().clone(),
                            self.options.config_file_path.clone().unwrap_or_else(|| "".to_owned())
                        ])
                    );
                }
            }
        }

        if let Some(options_paths) = self.options.paths.as_ref() {
            for key in options_paths.keys() {
                // if (!hasProperty(options.paths, key)) {
                //     continue;
                // }
                if !has_zero_or_one_asterisk_character(key) {
                    self.create_diagnostic_for_option_paths(
                        true,
                        key,
                        &Diagnostics::Pattern_0_can_have_at_most_one_Asterisk_character,
                        Some(vec![key.clone()]),
                    );
                }
                // if (isArray(options.paths[key])) {
                let options_paths_value = options_paths.get(key).unwrap();
                let len = options_paths_value.len();
                if len == 0 {
                    self.create_diagnostic_for_option_paths(
                        false,
                        key,
                        &Diagnostics::Substitutions_for_pattern_0_shouldn_t_be_an_empty_array,
                        Some(vec![key.clone()]),
                    );
                }
                for i in 0..len {
                    let subst = &options_paths_value[i];
                    // TODO: this stuff is more in the realm of compiler option "type validation" than "compatible values"?
                    // const typeOfSubst = typeof subst;
                    // if (typeOfSubst === "string") {
                    if !has_zero_or_one_asterisk_character(subst) {
                        self.create_diagnostic_for_option_path_key_value(
                            key,
                            i,
                            &Diagnostics::Substitution_0_in_pattern_1_can_have_at_most_one_Asterisk_character,
                            Some(vec![
                                subst.clone(),
                                key.clone(),
                            ])
                        );
                    }
                    if is_option_str_empty(self.options.base_url.as_deref())
                        && !path_is_relative(subst)
                        && !path_is_absolute(subst)
                    {
                        self.create_diagnostic_for_option_path_key_value(
                            key,
                            i,
                            &Diagnostics::Non_relative_paths_are_not_allowed_when_baseUrl_is_not_set_Did_you_forget_a_leading_Slash,
                            None,
                        );
                    }
                    // } else {
                    //     createDiagnosticForOptionPathKeyValue(key, i, Diagnostics.Substitution_0_for_pattern_1_has_incorrect_type_expected_string_got_2, subst, key, typeOfSubst);
                    // }
                }
                // } else {
                //     createDiagnosticForOptionPaths(/*onKey*/ false, key, Diagnostics.Substitutions_for_pattern_0_should_be_an_array, key);
                // }
            }
        }

        if self.options.source_map != Some(true) && self.options.inline_source_map != Some(true) {
            if self.options.inline_sources == Some(true) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_can_only_be_used_when_either_option_inlineSourceMap_or_option_sourceMap_is_provided,
                    "inlineSources",
                    None, None,
                );
            }
            if !is_option_str_empty(self.options.source_root.as_deref()) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_can_only_be_used_when_either_option_inlineSourceMap_or_option_sourceMap_is_provided,
                    "sourceRoot",
                    None, None,
                );
            }
        }

        if !is_option_str_empty(self.options.out.as_deref())
            && !is_option_str_empty(self.options.out_file.as_deref())
        {
            self.create_diagnostic_for_option_name(
                &Diagnostics::Option_0_cannot_be_specified_with_option_1,
                "out",
                Some("outFile"),
                None,
            );
        }

        if !is_option_str_empty(self.options.map_root.as_deref())
            && !(self.options.source_map == Some(true)
                || self.options.declaration_map == Some(true))
        {
            self.create_diagnostic_for_option_name(
                &Diagnostics::Option_0_cannot_be_specified_without_specifying_option_1_or_option_2,
                "mapRoot",
                Some("sourceMap"),
                Some("declarationMap"),
            );
        }

        if !is_option_str_empty(self.options.declaration_dir.as_deref()) {
            if !get_emit_declarations(&self.options) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_without_specifying_option_1_or_option_2,
                    "declarationDir",
                    Some("declaration"),
                    Some("composite"),
                );
            }
            if !is_option_str_empty(output_file) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_without_specifying_option_1,
                    "declarationDir",
                    Some(if !is_option_str_empty(self.options.out.as_deref()) {
                        "out"
                    } else {
                        "outFile"
                    }),
                    None,
                );
            }
        }

        if self.options.declaration_map == Some(true) && !get_emit_declarations(&self.options) {
            self.create_diagnostic_for_option_name(
                &Diagnostics::Option_0_cannot_be_specified_without_specifying_option_1_or_option_2,
                "declarationMap",
                Some("declaration"),
                Some("composite"),
            );
        }

        if self.options.lib.is_some() && self.options.no_lib == Some(true) {
            self.create_diagnostic_for_option_name(
                &Diagnostics::Option_0_cannot_be_specified_with_option_1,
                "lib",
                Some("noLib"),
                None,
            );
        }

        if self.options.no_implicit_use_strict == Some(true)
            && get_strict_option_value(&self.options, "alwaysStrict")
        {
            self.create_diagnostic_for_option_name(
                &Diagnostics::Option_0_cannot_be_specified_with_option_1,
                "noImplicitUseStrict",
                Some("alwaysStrict"),
                None,
            );
        }

        let language_version = get_emit_script_target(&self.options);

        let first_non_ambient_external_module_source_file =
            find(&**self.files(), |f: &Gc<Node>, _| {
                is_external_module(f) && !f.as_source_file().is_declaration_file()
            })
            .cloned();
        if self.options.isolated_modules == Some(true) {
            if self.options.module == Some(ModuleKind::None)
                && language_version < ScriptTarget::ES2015
            {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_isolatedModules_can_only_be_used_when_either_option_module_is_provided_or_option_target_is_ES2015_or_higher,
                    "isolatedModules",
                    Some("target"),
                    None,
                );
            }

            if self.options.preserve_const_enums == Some(false) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_preserveConstEnums_cannot_be_disabled_when_isolatedModules_is_enabled,
                    "preserveConstEnums",
                    Some("isolatedModules"),
                    None,
                );
            }

            let first_non_external_module_source_file = find(&**self.files(), |f: &Gc<Node>, _| {
                !is_external_module(f)
                    && !is_source_file_js(f)
                    && !f.as_source_file().is_declaration_file()
                    && f.as_source_file().script_kind() != ScriptKind::JSON
            })
            .cloned();
            if let Some(first_non_external_module_source_file) =
                first_non_external_module_source_file.as_ref()
            {
                let span = get_error_span_for_node(
                    first_non_external_module_source_file,
                    first_non_external_module_source_file,
                );
                self.program_diagnostics_mut().add(
                    Gc::new(
                        create_file_diagnostic(
                            first_non_external_module_source_file,
                            span.start,
                            span.length,
                            &Diagnostics::_0_cannot_be_compiled_under_isolatedModules_because_it_is_considered_a_global_script_file_Add_an_import_export_or_an_empty_export_statement_to_make_it_a_module,
                            Some(vec![
                                get_base_file_name(
                                    &first_non_external_module_source_file.as_source_file().file_name(),
                                    None, None,
                                )
                            ])
                        ).into()
                    )
                );
            }
        } else if let Some(first_non_ambient_external_module_source_file) =
            first_non_ambient_external_module_source_file.as_ref()
        {
            if language_version < ScriptTarget::ES2015
                && self.options.module == Some(ModuleKind::None)
            {
                let span = get_error_span_for_node(
                    first_non_ambient_external_module_source_file,
                    first_non_ambient_external_module_source_file,
                );
                self.program_diagnostics_mut().add(
                    Gc::new(
                        create_file_diagnostic(
                            first_non_ambient_external_module_source_file,
                            span.start,
                            span.length,
                            &Diagnostics::Cannot_use_imports_exports_or_module_augmentations_when_module_is_none,
                            None,
                        ).into()
                    )
                );
            }
        }

        if !is_option_str_empty(output_file) && self.options.emit_declaration_only != Some(true) {
            if matches!(
                self.options.module,
                Some(options_module) if !matches!(
                    options_module,
                    ModuleKind::AMD | ModuleKind::System
                )
            ) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Only_amd_and_system_modules_are_supported_alongside_0,
                    if !is_option_str_empty(self.options.out.as_deref()) {
                        "out"
                    } else {
                        "outFile"
                    },
                    Some("module"),
                    None,
                );
            } else if self.options.module.is_none() {
                if let Some(first_non_ambient_external_module_source_file) =
                    first_non_ambient_external_module_source_file.as_ref()
                {
                    let span = get_error_span_for_node(
                        first_non_ambient_external_module_source_file,
                        &first_non_ambient_external_module_source_file
                            .as_source_file()
                            .maybe_external_module_indicator()
                            .unwrap(),
                    );
                    self.program_diagnostics_mut().add(
                        Gc::new(
                            create_file_diagnostic(
                                first_non_ambient_external_module_source_file,
                                span.start,
                                span.length,
                                &Diagnostics::Cannot_compile_modules_using_option_0_unless_the_module_flag_is_amd_or_system,
                                Some(vec![
                                    if !is_option_str_empty(self.options.out.as_deref()) {
                                        "out"
                                    } else {
                                        "outFile"
                                    }.to_owned(),
                                ])
                            ).into()
                        )
                    );
                }
            }
        }

        if self.options.resolve_json_module == Some(true) {
            if !matches!(
                get_emit_module_resolution_kind(&self.options),
                ModuleResolutionKind::NodeJs
                    | ModuleResolutionKind::Node12
                    | ModuleResolutionKind::NodeNext
            ) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_resolveJsonModule_cannot_be_specified_without_node_module_resolution_strategy,
                    "resolveJsonModule",
                    None, None,
                );
            } else if !has_json_module_emit_enabled(&self.options) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_resolveJsonModule_can_only_be_specified_when_module_code_generation_is_commonjs_amd_es2015_or_esNext,
                    "resolveJsonModule",
                    Some("module"),
                    None,
                );
            }
        }

        if !is_option_str_empty(self.options.out_dir.as_deref())
            || !is_option_str_empty(self.options.root_dir.as_deref())
            || !is_option_str_empty(self.options.source_root.as_deref())
            || !is_option_str_empty(self.options.map_root.as_deref())
        {
            let dir = self.get_common_source_directory();

            if !is_option_str_empty(self.options.out_dir.as_deref())
                && dir.is_empty()
                && self
                    .files()
                    .iter()
                    .any(|file| get_root_length(&file.as_source_file().file_name()) > 1)
            {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Cannot_find_the_common_subdirectory_path_for_the_input_files,
                    "outDir",
                    None,
                    None,
                );
            }
        }

        if self.options.use_define_for_class_fields == Some(true)
            && language_version == ScriptTarget::ES3
        {
            self.create_diagnostic_for_option_name(
                &Diagnostics::Option_0_cannot_be_specified_when_option_target_is_ES3,
                "useDefineForClassFields",
                None,
                None,
            );
        }

        if self.options.check_js == Some(true) && !get_allow_js_compiler_option(&self.options) {
            self.program_diagnostics_mut().add(Gc::new(
                create_compiler_diagnostic(
                    &Diagnostics::Option_0_cannot_be_specified_without_specifying_option_1,
                    Some(vec!["checkJs".to_owned(), "allowJs".to_owned()]),
                )
                .into(),
            ));
        }

        if self.options.emit_declaration_only == Some(true) {
            if !get_emit_declarations(&self.options) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_without_specifying_option_1_or_option_2,
                    "emitDeclarationOnly",
                    Some("declaration"),
                    Some("composite")
                );
            }

            if self.options.no_emit == Some(true) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_with_option_1,
                    "emitDeclarationOnly",
                    Some("noEmit"),
                    None,
                );
            }
        }

        if self.options.emit_decorator_metadata == Some(true)
            && self.options.experimental_decorators != Some(true)
        {
            self.create_diagnostic_for_option_name(
                &Diagnostics::Option_0_cannot_be_specified_without_specifying_option_1,
                "emitDecoratorMetadata",
                Some("experimentalDecorators"),
                None,
            );
        }

        if let Some(options_jsx_factory) = self
            .options
            .jsx_factory
            .as_ref()
            .filter(|options_jsx_factory| !options_jsx_factory.is_empty())
        {
            if !is_option_str_empty(self.options.react_namespace.as_deref()) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_with_option_1,
                    "reactNamespace",
                    Some("jsxFactory"),
                    None,
                );
            }
            if matches!(
                self.options.jsx,
                Some(JsxEmit::ReactJSX) | Some(JsxEmit::ReactJSXDev)
            ) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_when_option_jsx_is_1,
                    "jsxFactory",
                    inverse_jsx_option_map.with(|inverse_jsx_option_map_| {
                        inverse_jsx_option_map_
                            .get(&self.options.jsx.unwrap())
                            .copied()
                    }),
                    None,
                );
            }
            if parse_isolated_entity_name(options_jsx_factory.clone(), language_version).is_none() {
                self.create_option_value_diagnostic(
                    "jsxFactory",
                    &Diagnostics::Invalid_value_for_jsxFactory_0_is_not_a_valid_identifier_or_qualified_name,
                    Some(vec![
                        options_jsx_factory.clone()
                    ])
                );
            }
        } else if let Some(options_react_namespace) =
            self.options
                .react_namespace
                .as_ref()
                .filter(|options_react_namespace| {
                    !options_react_namespace.is_empty()
                        && !is_identifier_text(
                            options_react_namespace,
                            Some(language_version),
                            None,
                        )
                })
        {
            self.create_option_value_diagnostic(
                "reactNamespace",
                &Diagnostics::Invalid_value_for_reactNamespace_0_is_not_a_valid_identifier,
                Some(vec![options_react_namespace.clone()]),
            );
        }

        if let Some(options_jsx_fragment_factory) = self
            .options
            .jsx_fragment_factory
            .as_ref()
            .filter(|options_jsx_fragment_factory| !options_jsx_fragment_factory.is_empty())
        {
            if is_option_str_empty(self.options.jsx_factory.as_deref()) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_without_specifying_option_1,
                    "jsxFragmentFactory",
                    Some("jsxFactory"),
                    None,
                );
            }
            if matches!(
                self.options.jsx,
                Some(JsxEmit::ReactJSX) | Some(JsxEmit::ReactJSXDev)
            ) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_when_option_jsx_is_1,
                    "jsxFragmentFactory",
                    inverse_jsx_option_map.with(|inverse_jsx_option_map_| {
                        inverse_jsx_option_map_
                            .get(&self.options.jsx.unwrap())
                            .copied()
                    }),
                    None,
                );
            }
            if parse_isolated_entity_name(options_jsx_fragment_factory.clone(), language_version)
                .is_none()
            {
                self.create_option_value_diagnostic(
                    "jsxFragmentFactory",
                    &Diagnostics::Invalid_value_for_jsxFragmentFactory_0_is_not_a_valid_identifier_or_qualified_name,
                    Some(vec![
                        options_jsx_fragment_factory.clone()
                    ])
                );
            }
        }

        if !is_option_str_empty(self.options.react_namespace.as_deref()) {
            if matches!(
                self.options.jsx,
                Some(JsxEmit::ReactJSX) | Some(JsxEmit::ReactJSXDev)
            ) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_when_option_jsx_is_1,
                    "reactNamespace",
                    inverse_jsx_option_map.with(|inverse_jsx_option_map_| {
                        inverse_jsx_option_map_
                            .get(&self.options.jsx.unwrap())
                            .copied()
                    }),
                    None,
                );
            }
        }

        if !is_option_str_empty(self.options.jsx_import_source.as_deref()) {
            if self.options.jsx == Some(JsxEmit::React) {
                self.create_diagnostic_for_option_name(
                    &Diagnostics::Option_0_cannot_be_specified_when_option_jsx_is_1,
                    "jsxImportSource",
                    inverse_jsx_option_map.with(|inverse_jsx_option_map_| {
                        inverse_jsx_option_map_
                            .get(&self.options.jsx.unwrap())
                            .copied()
                    }),
                    None,
                );
            }
        }

        if self.options.preserve_value_imports == Some(true)
            && get_emit_module_kind(&self.options) < ModuleKind::ES2015
        {
            self.create_option_value_diagnostic(
                "importsNotUsedAsValues",
                &Diagnostics::Option_preserveValueImports_can_only_be_used_when_module_is_set_to_es2015_or_later,
                None,
            );
        }

        if self.options.no_emit != Some(true)
            && self.options.suppress_output_path_check != Some(true)
        {
            // TODO
            // unimplemented!()
            // let emit_host = self.get_emit_host();
        }
    }

    pub fn create_diagnostic_explaining_file<TFile: Borrow<Node>>(
        &self,
        file: Option<TFile>,
        mut file_processing_reason: Option<&FileIncludeReason>,
        diagnostic: &'static DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Gc<Diagnostic> {
        let mut file_include_reasons: Option<Vec<DiagnosticMessageChain>> = None;
        let mut related_info: Option<Vec<Rc<DiagnosticRelatedInformation>>> = None;
        let mut location_reason = if is_referenced_file(file_processing_reason) {
            file_processing_reason
        } else {
            None
        };
        let file = file.map(|file| file.borrow().node_wrapper());
        let file_reasons = self.file_reasons();
        if let Some(file) = file.as_ref() {
            if let Some(reasons) = file_reasons.get(&*file.as_source_file().path()) {
                for reason in reasons {
                    self.process_reason(
                        &mut file_include_reasons,
                        &mut location_reason,
                        &mut related_info,
                        &mut file_processing_reason,
                        reason,
                    );
                }
            }
        }
        if file_processing_reason.is_some() {
            let file_processing_reason_present = file_processing_reason.unwrap();
            self.process_reason(
                &mut file_include_reasons,
                &mut location_reason,
                &mut related_info,
                &mut file_processing_reason,
                file_processing_reason_present,
            );
        }
        if location_reason.is_some()
            && matches!(
                file_include_reasons.as_ref(),
                Some(file_include_reasons) if file_include_reasons.len() == 1
            )
        {
            file_include_reasons = None;
        }
        let location = location_reason.map(|location_reason| {
            get_referenced_file_location(
                |path: &Path| self.get_source_file_by_path(path),
                location_reason.as_referenced_file(),
            )
        });
        let file_include_reason_details = file_include_reasons.map(|file_include_reasons| {
            chain_diagnostic_messages_multiple(
                file_include_reasons,
                &Diagnostics::The_file_is_in_the_program_because_Colon,
                None,
            )
        });
        let redirect_info = file
            .as_ref()
            .and_then(|file| explain_if_file_is_redirect(file, Option::<fn(&str) -> String>::None));
        let chain = if let Some(mut redirect_info) = redirect_info {
            chain_diagnostic_messages_multiple(
                if let Some(file_include_reason_details) = file_include_reason_details {
                    redirect_info.insert(0, file_include_reason_details);
                    redirect_info
                } else {
                    redirect_info
                },
                diagnostic,
                args,
            )
        } else {
            chain_diagnostic_messages(file_include_reason_details, diagnostic, args)
        };
        if let Some(location) = location.filter(|location| is_reference_file_location(location)) {
            let location_as_reference_file_location = location.as_reference_file_location();
            Gc::new(
                create_file_diagnostic_from_message_chain(
                    &location_as_reference_file_location.file,
                    location_as_reference_file_location.pos.try_into().unwrap(),
                    (location_as_reference_file_location.end
                        - location_as_reference_file_location.pos)
                        .try_into()
                        .unwrap(),
                    chain,
                    related_info,
                )
                .into(),
            )
        } else {
            Gc::new(create_compiler_diagnostic_from_message_chain(chain, related_info).into())
        }
    }

    pub fn process_reason<'a>(
        &self,
        file_include_reasons: &mut Option<Vec<DiagnosticMessageChain>>,
        location_reason: &mut Option<&'a FileIncludeReason>,
        related_info: &mut Option<Vec<Rc<DiagnosticRelatedInformation>>>,
        file_processing_reason: &mut Option<&FileIncludeReason>,
        reason: &'a FileIncludeReason,
    ) {
        file_include_reasons.get_or_insert_with(|| vec![]).push(
            file_include_reason_to_diagnostics(self, reason, Option::<fn(&str) -> String>::None),
        );
        if location_reason.is_none() && is_referenced_file(Some(reason)) {
            *location_reason = Some(reason);
        } else if !matches!(
            *location_reason,
            Some(location_reason) if ptr::eq(
                location_reason,
                reason
            )
        ) {
            if let Some(related_information) =
                self.file_include_reason_to_related_information(reason)
            {
                related_info
                    .get_or_insert_with(|| vec![])
                    .push(related_information);
            }
        }
        if matches!(
            *file_processing_reason,
            Some(file_processing_reason) if ptr::eq(
                reason,
                file_processing_reason
            )
        ) {
            *file_processing_reason = None
        }
    }

    pub fn add_file_preprocessing_file_explaining_diagnostic<TFile: Borrow<Node>>(
        &self,
        file: Option<TFile>,
        file_processing_reason: &FileIncludeReason,
        diagnostic: &'static DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.maybe_file_processing_diagnostics().get_or_insert_with(|| vec![]).push(FilePreprocessingDiagnostics::FilePreprocessingFileExplainingDiagnostic(FilePreprocessingFileExplainingDiagnostic {
            kind: FilePreprocessingDiagnosticsKind::FilePreprocessingFileExplainingDiagnostic,
            file: file.map(|file| file.borrow().as_source_file().path().clone()),
            file_processing_reason: file_processing_reason.clone(),
            diagnostic,
            args,
        }))
    }

    pub fn add_program_diagnostic_explaining_file(
        &self,
        file: &Node, /*SourceFile*/
        diagnostic: &'static DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.program_diagnostics_mut()
            .add(self.create_diagnostic_explaining_file(Some(file), None, diagnostic, args));
    }

    pub fn file_include_reason_to_related_information(
        &self,
        reason: &FileIncludeReason,
    ) -> Option<Rc<DiagnosticRelatedInformation /*DiagnosticWithLocation*/>> {
        unimplemented!()
    }

    pub fn verify_project_references(&self) {
        let build_info_path = if self.options.suppress_output_path_check != Some(true) {
            get_ts_build_info_emit_output_file_path(&self.options)
        } else {
            None
        };
        for_each_project_reference(
            self.maybe_project_references().as_deref(),
            self.maybe_resolved_project_references().as_deref(),
            |resolved_ref: Option<Gc<ResolvedProjectReference>>,
             parent: Option<&ResolvedProjectReference>,
             index|
             -> Option<()> {
                let ref_ = if let Some(parent) = parent {
                    parent.command_line.project_references.clone().unwrap()
                } else {
                    self.maybe_project_references().clone().unwrap()
                }[index]
                    .clone();
                let parent_file = parent.map(|parent| parent.source_file.clone());
                if resolved_ref.is_none() {
                    self.create_diagnostic_for_reference(
                        parent_file.as_deref(),
                        index,
                        &Diagnostics::File_0_not_found,
                        Some(vec![ref_.path.clone()]),
                    );
                    return None;
                }
                let resolved_ref = resolved_ref.unwrap();
                let options = &resolved_ref.command_line.options;
                if options.composite != Some(true) || options.no_emit == Some(true) {
                    let inputs = if let Some(parent) = parent {
                        parent.command_line.file_names.clone()
                    } else {
                        self.root_names().clone()
                    };
                    if !inputs.is_empty() {
                        if options.composite != Some(true) {
                            self.create_diagnostic_for_reference(
                                parent_file.as_deref(),
                                index,
                                &Diagnostics::Referenced_project_0_must_have_setting_composite_Colon_true,
                                Some(vec![
                                    ref_.path.clone()
                                ])
                            );
                        }
                        if options.no_emit == Some(true) {
                            self.create_diagnostic_for_reference(
                                parent_file.as_deref(),
                                index,
                                &Diagnostics::Referenced_project_0_may_not_disable_emit,
                                Some(vec![ref_.path.clone()]),
                            );
                        }
                    }
                }
                if ref_.prepend == Some(true) {
                    let out = out_file(options);
                    if let Some(out) = out.filter(|out| !out.is_empty()) {
                        if !self.host().file_exists(out) {
                            self.create_diagnostic_for_reference(
                                parent_file.as_deref(),
                                index,
                                &Diagnostics::Output_file_0_from_project_1_does_not_exist,
                                Some(vec![out.to_owned(), ref_.path.clone()]),
                            );
                        }
                    } else {
                        self.create_diagnostic_for_reference(
                            parent_file.as_deref(),
                            index,
                            &Diagnostics::Cannot_prepend_project_0_because_it_does_not_have_outFile_set,
                            Some(vec![
                                ref_.path.clone()
                            ])
                        );
                    }
                }
                if parent.is_none() {
                    if let Some(build_info_path) =
                        build_info_path.as_ref().filter(|build_info_path| {
                            Some(*build_info_path)
                                == get_ts_build_info_emit_output_file_path(options).as_ref()
                        })
                    {
                        self.create_diagnostic_for_reference(
                            parent_file.as_deref(),
                            index,
                            &Diagnostics::Cannot_write_file_0_because_it_will_overwrite_tsbuildinfo_file_generated_by_referenced_project_1,
                            Some(vec![
                                build_info_path.clone(),
                                ref_.path.clone()
                            ])
                        );
                        self.has_emit_blocking_diagnostics()
                            .insert(self.to_path(build_info_path), true);
                    }
                }
                None
            },
            Option::<
                fn(
                    Option<&[Rc<ProjectReference>]>,
                    Option<&ResolvedProjectReference>,
                ) -> Option<()>,
            >::None,
        );
    }

    pub fn create_diagnostic_for_option_path_key_value(
        &self,
        key: &str,
        value_index: usize,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        unimplemented!()
    }

    pub fn create_diagnostic_for_option_paths(
        &self,
        on_key: bool,
        key: &str,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        unimplemented!()
    }

    pub fn create_diagnostic_for_option_name(
        &self,
        message: &DiagnosticMessage,
        option1: &str,
        option2: Option<&str>,
        option3: Option<&str>,
    ) {
        let mut args = vec![option1.to_owned()];
        if let Some(option2) = option2 {
            args.push(option2.to_owned());
        }
        if let Some(option3) = option3 {
            args.push(option3.to_owned());
        }
        self.create_diagnostic_for_option(true, option1, option2, message, Some(args));
    }

    pub fn create_option_value_diagnostic(
        &self,
        option1: &str,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.create_diagnostic_for_option(false, option1, None, message, args);
    }

    pub fn create_diagnostic_for_reference<TSourceFile: Borrow<Node>>(
        &self,
        source_file: Option<TSourceFile /*JsonSourceFile*/>,
        index: usize,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        unimplemented!()
    }

    pub fn create_diagnostic_for_option(
        &self,
        on_key: bool,
        option1: &str,
        option2: Option<&str>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        let compiler_options_object_literal_syntax =
            self.get_compiler_options_object_literal_syntax();
        let need_compiler_diagnostic = match compiler_options_object_literal_syntax.as_ref() {
            None => true,
            Some(compiler_options_object_literal_syntax) => !self
                .create_option_diagnostic_in_object_literal_syntax(
                    compiler_options_object_literal_syntax,
                    on_key,
                    option1,
                    option2,
                    message,
                    args.clone(),
                ),
        };

        if need_compiler_diagnostic {
            self.program_diagnostics_mut()
                .add(Gc::new(create_compiler_diagnostic(message, args).into()));
        }
    }

    pub fn get_compiler_options_object_literal_syntax(&self) -> Option<Gc<Node>> {
        if self
            .maybe_compiler_options_object_literal_syntax()
            .is_none()
        {
            self.set_compiler_options_object_literal_syntax(Some(None));
            let json_object_literal =
                get_ts_config_object_literal_expression(self.options.config_file.as_deref());
            if let Some(json_object_literal) = json_object_literal.as_ref() {
                for prop in get_property_assignment(json_object_literal, "compilerOptions", None) {
                    let prop_as_property_assignment = prop.as_property_assignment();
                    if is_object_literal_expression(&prop_as_property_assignment.initializer) {
                        self.set_compiler_options_object_literal_syntax(Some(Some(
                            prop_as_property_assignment.initializer.clone(),
                        )));
                        break;
                    }
                }
            }
        }
        self.maybe_compiler_options_object_literal_syntax()
            .flatten()
    }

    pub fn create_option_diagnostic_in_object_literal_syntax(
        &self,
        object_literal: &Node, /*ObjectLiteralExpression*/
        on_key: bool,
        key1: &str,
        key2: Option<&str>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> bool {
        let props = get_property_assignment(object_literal, key1, key2);
        for prop in &props {
            self.program_diagnostics_mut().add(Gc::new(
                create_diagnostic_for_node_in_source_file(
                    self.options.config_file.as_ref().unwrap(),
                    &*if on_key {
                        prop.as_property_assignment().name()
                    } else {
                        prop.as_property_assignment().initializer.clone()
                    },
                    message,
                    args.clone(),
                )
                .into(),
            ));
        }
        !props.is_empty()
    }

    pub fn block_emitting_of_file(&self, emit_file_name: &str, diag: Gc<Diagnostic>) {
        self.has_emit_blocking_diagnostics()
            .insert(self.to_path(emit_file_name), true);
        self.program_diagnostics_mut().add(diag);
    }

    pub fn is_emitted_file(&self, file: &str) -> bool {
        if self.options.no_emit == Some(true) {
            return false;
        }

        let file_path = self.to_path(file);
        if self.get_source_file_by_path(&file_path).is_some() {
            return false;
        }

        let out = out_file(&self.options);
        if let Some(out) = out {
            return self.is_same_file(&*file_path, out)
                || self.is_same_file(
                    &*file_path,
                    &format!("{}{}", remove_file_extension(out), Extension::Dts.to_str()),
                );
        }

        if matches!(
            self.options.declaration_dir.as_ref(),
            Some(options_declaration_dir) if contains_path(
                options_declaration_dir,
                &*file_path,
                Some(self.current_directory().clone()),
                Some(!CompilerHost::use_case_sensitive_file_names(&**self.host()))
            )
        ) {
            return true;
        }

        if let Some(options_out_dir) = self.options.out_dir.as_ref() {
            return contains_path(
                options_out_dir,
                &*file_path,
                Some(self.current_directory().clone()),
                Some(CompilerHost::use_case_sensitive_file_names(&**self.host())),
            );
        }

        if file_extension_is_one_of(&*file_path, &supported_js_extensions_flat)
            || file_extension_is(&*file_path, Extension::Dts.to_str())
        {
            let file_path_without_extension = remove_file_extension(&*file_path);
            return self
                .get_source_file_by_path(&Path::new(format!(
                    "{}{}",
                    file_path_without_extension,
                    Extension::Ts.to_str()
                )))
                .is_some()
                || self
                    .get_source_file_by_path(&Path::new(format!(
                        "{}{}",
                        file_path_without_extension,
                        Extension::Tsx.to_str()
                    )))
                    .is_some();
        }
        false
    }

    pub fn is_same_file(&self, file1: &str, file2: &str) -> bool {
        compare_paths(
            file1,
            file2,
            Some(self.current_directory().clone()),
            Some(!CompilerHost::use_case_sensitive_file_names(&**self.host())),
        ) == Comparison::EqualTo
    }

    pub fn get_symlink_cache(&self) -> Gc<SymlinkCache> {
        let host_symlink_cache = self.host().get_symlink_cache();
        if let Some(host_symlink_cache) = host_symlink_cache {
            return host_symlink_cache;
        }
        if self.symlinks().is_none() {
            let host = self.host();
            *self.symlinks() = Some(Gc::new(create_symlink_cache(
                &self.current_directory(),
                Rc::new(move |file_name: &str| host.get_canonical_file_name(file_name)),
            )));
        }
        let symlinks = self.symlinks().clone().unwrap();
        if
        /*files && resolvedTypeReferenceDirectives &&*/
        !symlinks.has_processed_resolutions() {
            symlinks.set_symlinks_from_resolutions(
                &self.files(),
                Some(&self.resolved_type_reference_directives()),
            );
        }
        symlinks
    }

    pub fn get_symlink_cache_rc(&self) -> Gc<Box<dyn GetSymlinkCache>> {
        Gc::new(Box::new(ProgramGetSymlinkCache::new(self.rc_wrapper())))
    }
}

#[derive(Trace, Finalize)]
pub struct ProgramGetSymlinkCache {
    program: Gc<Box<Program>>,
}

impl ProgramGetSymlinkCache {
    pub fn new(program: Gc<Box<Program>>) -> Self {
        Self { program }
    }
}

impl GetSymlinkCache for ProgramGetSymlinkCache {
    fn call(&self) -> Gc<SymlinkCache> {
        self.program.get_symlink_cache()
    }
}

pub(super) struct HostForUseSourceOfProjectReferenceRedirect {
    pub compiler_host: Gc<Box<dyn CompilerHost>>,
    pub get_symlink_cache: Gc<Box<dyn GetSymlinkCache>>,
    pub use_source_of_project_reference_redirect: bool,
    pub to_path: Gc<Box<dyn ToPath>>,
    pub get_resolved_project_references: Gc<Box<dyn GetResolvedProjectReferences>>,
    pub for_each_resolved_project_reference: Gc<Box<dyn ForEachResolvedProjectReference>>,
}

pub(super) fn update_host_for_use_source_of_project_reference_redirect(
    host: HostForUseSourceOfProjectReferenceRedirect,
) -> UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
    let overrider: Gc<Box<dyn ModuleResolutionHostOverrider>> = Gc::new(Box::new(
        UpdateHostForUseSourceOfProjectReferenceRedirectOverrider::new(
            host.compiler_host.clone(),
            host.get_symlink_cache.clone(),
            host.to_path.clone(),
            host.get_resolved_project_references.clone(),
            host.for_each_resolved_project_reference.clone(),
        ),
    ));

    if !host.use_source_of_project_reference_redirect {
        return UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
            on_program_create_complete: Rc::new(|| {}),
            directory_exists: None,
            file_exists: overrider,
        };
    }

    host.compiler_host
        .set_overriding_file_exists(Some(overrider.clone()));

    host.compiler_host
        .set_overriding_directory_exists(Some(overrider.clone()));

    host.compiler_host
        .set_overriding_get_directories(Some(overrider.clone()));

    host.compiler_host
        .set_overriding_realpath(Some(overrider.clone()));

    UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
        on_program_create_complete: {
            let host_compiler_host_clone = host.compiler_host.clone();
            Rc::new(move || {
                host_compiler_host_clone.set_overriding_file_exists(None);
                host_compiler_host_clone.set_overriding_directory_exists(None);
                host_compiler_host_clone.set_overriding_get_directories(None);
            })
        },
        directory_exists: Some(overrider.clone()),
        file_exists: overrider,
    }
}

pub(super) struct UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
    pub on_program_create_complete: Rc<dyn Fn()>,
    pub directory_exists: Option<Gc<Box<dyn ModuleResolutionHostOverrider>>>,
    pub file_exists: Gc<Box<dyn ModuleResolutionHostOverrider>>,
}

#[derive(Trace, Finalize)]
struct UpdateHostForUseSourceOfProjectReferenceRedirectOverrider {
    pub host_compiler_host: Gc<Box<dyn CompilerHost>>,
    pub host_get_symlink_cache: Gc<Box<dyn GetSymlinkCache>>,
    pub host_to_path: Gc<Box<dyn ToPath>>,
    pub host_get_resolved_project_references: Gc<Box<dyn GetResolvedProjectReferences>>,
    pub host_for_each_resolved_project_reference: Gc<Box<dyn ForEachResolvedProjectReference>>,
    #[unsafe_ignore_trace]
    set_of_declaration_directories: RefCell<Option<HashSet<Path>>>,
}

impl UpdateHostForUseSourceOfProjectReferenceRedirectOverrider {
    pub fn new(
        host_compiler_host: Gc<Box<dyn CompilerHost>>,
        host_get_symlink_cache: Gc<Box<dyn GetSymlinkCache>>,
        host_to_path: Gc<Box<dyn ToPath>>,
        host_get_resolved_project_references: Gc<Box<dyn GetResolvedProjectReferences>>,
        host_for_each_resolved_project_reference: Gc<Box<dyn ForEachResolvedProjectReference>>,
    ) -> Self {
        Self {
            host_compiler_host,
            host_get_symlink_cache,
            host_to_path,
            host_get_resolved_project_references,
            host_for_each_resolved_project_reference,
            set_of_declaration_directories: Default::default(),
        }
    }

    fn handle_directory_could_be_symlink(&self, directory: &str) {
        unimplemented!()
    }

    fn file_or_directory_exists_using_source(
        &self,
        file_or_directory: &str,
        is_file: bool,
    ) -> bool {
        unimplemented!()
    }
}

impl ModuleResolutionHostOverrider for UpdateHostForUseSourceOfProjectReferenceRedirectOverrider {
    fn file_exists(&self, file: &str) -> bool {
        if self.host_compiler_host.file_exists_non_overridden(file) {
            return true;
        }
        if self.host_get_resolved_project_references.call().is_none() {
            return false;
        }
        if !is_declaration_file_name(file) {
            return false;
        }

        self.file_or_directory_exists_using_source(file, true)
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        if !self.host_compiler_host.is_directory_exists_supported() {
            return None;
        }
        if self
            .host_compiler_host
            .directory_exists_non_overridden(path)
            == Some(true)
        {
            self.handle_directory_could_be_symlink(path);
            return Some(true);
        }

        if self.host_get_resolved_project_references.call().is_none() {
            return Some(false);
        }

        if self.set_of_declaration_directories.borrow().is_none() {
            let mut set_of_declaration_directories =
                self.set_of_declaration_directories.borrow_mut();
            *set_of_declaration_directories = Some(HashSet::new());
            let set_of_declaration_directories = set_of_declaration_directories.as_mut().unwrap();
            self.host_for_each_resolved_project_reference
                .call(&mut |ref_| {
                    let out = out_file(&ref_.command_line.options);
                    if let Some(out) = out {
                        set_of_declaration_directories
                            .insert(get_directory_path(&self.host_to_path.call(out)).into());
                    } else {
                        let declaration_dir = ref_
                            .command_line
                            .options
                            .declaration_dir
                            .as_ref()
                            .or_else(|| ref_.command_line.options.out_dir.as_ref());
                        if let Some(declaration_dir) = declaration_dir {
                            set_of_declaration_directories
                                .insert(self.host_to_path.call(declaration_dir));
                        }
                    }
                });
        }

        Some(self.file_or_directory_exists_using_source(path, false))
    }

    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        if !self.host_compiler_host.is_get_directories_supported() {
            return None;
        }
        if self.host_get_resolved_project_references.call().is_none()
            || self
                .host_compiler_host
                .directory_exists_non_overridden(path)
                == Some(true)
        {
            self.host_compiler_host.get_directories_non_overridden(path)
        } else {
            Some(vec![])
        }
    }

    fn realpath(&self, s: &str) -> Option<String> {
        if !self.host_compiler_host.is_realpath_supported() {
            return None;
        }
        self.host_get_symlink_cache
            .call()
            .get_symlinked_files()
            .as_ref()
            .and_then(|symlinked_files| symlinked_files.get(&self.host_to_path.call(s)).cloned())
            .or_else(|| self.host_compiler_host.realpath_non_overridden(s))
    }

    fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
        unreachable!()
    }

    fn write_file(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        source_files: Option<&[Gc<Node /*SourceFile*/>]>,
    ) {
        unreachable!()
    }

    fn create_directory(&self, directory: &str) {
        unreachable!()
    }
}

pub trait GetSymlinkCache: Trace + Finalize {
    fn call(&self) -> Gc<SymlinkCache>;
}

pub trait GetResolvedProjectReferences: Trace + Finalize {
    fn call(&self) -> Option<Vec<Option<Gc<ResolvedProjectReference>>>>;
}

pub trait ForEachResolvedProjectReference: Trace + Finalize {
    fn call(&self, callback: &mut dyn FnMut(Gc<ResolvedProjectReference>));
}

pub(super) fn filter_semantic_diagnostics(
    diagnostic: Vec<Gc<Diagnostic>>,
    option: &CompilerOptions,
) -> Vec<Gc<Diagnostic>> {
    diagnostic
        .into_iter()
        .filter(|d| match d.maybe_skipped_on().as_ref() {
            None => true,
            Some(d_skipped_on) => option.get_value(d_skipped_on).as_option_bool() != Some(true),
        })
        .collect()
}

pub trait CompilerHostLike: Trace + Finalize {
    fn use_case_sensitive_file_names(&self) -> bool;
    fn get_current_directory(&self) -> String;
    fn file_exists(&self, file_name: &str) -> bool;
    fn read_file(&self, file_name: &str) -> io::Result<Option<String>>;
    fn read_directory(
        &self,
        root_dir: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> Option<Vec<String>> {
        None
    }
    fn trace(&self, s: &str) {}
    fn is_trace_supported(&self) -> bool;
    fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Gc<Diagnostic>) {}

    // These exist to allow "forwarding" CompilerHost -> CompilerHostLike -> DirectoryStructureHost
    fn is_read_directory_implemented(&self) -> bool;
    fn realpath(&self, path: &str) -> Option<String>;
    fn create_directory(&self, path: &str);
    fn write_file(&self, path: &str, data: &str, write_byte_order_mark: Option<bool>);
    fn directory_exists(&self, path: &str) -> Option<bool>;
    fn get_directories(&self, path: &str) -> Option<Vec<String>>;
}

#[derive(Trace, Finalize)]
pub struct CompilerHostLikeRcDynCompilerHost {
    host: Gc<Box<dyn CompilerHost>>,
}

impl CompilerHostLikeRcDynCompilerHost {
    pub fn new(host: Gc<Box<dyn CompilerHost>>) -> Self {
        Self { host }
    }
}

impl CompilerHostLike for CompilerHostLikeRcDynCompilerHost {
    fn use_case_sensitive_file_names(&self) -> bool {
        CompilerHost::use_case_sensitive_file_names(&**self.host)
    }

    fn get_current_directory(&self) -> String {
        CompilerHost::get_current_directory(&**self.host)
    }

    fn file_exists(&self, file_name: &str) -> bool {
        self.host.file_exists(file_name)
    }

    fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
        self.host.read_file(file_name)
    }

    fn read_directory(
        &self,
        root_dir: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> Option<Vec<String>> {
        self.host
            .read_directory(root_dir, extensions, excludes, includes, depth)
    }

    fn trace(&self, s: &str) {
        ModuleResolutionHost::trace(&**self.host, s)
    }

    fn is_trace_supported(&self) -> bool {
        self.host.is_trace_supported()
    }

    // fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Gc<Diagnostic>) {}
    fn is_read_directory_implemented(&self) -> bool {
        self.host.is_read_directory_implemented()
    }

    fn realpath(&self, path: &str) -> Option<String> {
        self.host.realpath(path)
    }

    fn create_directory(&self, path: &str) {
        self.host.create_directory(path)
    }

    fn write_file(&self, path: &str, data: &str, write_byte_order_mark: Option<bool>) {
        self.host
            .write_file(path, data, write_byte_order_mark.unwrap(), None, None)
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        self.host.directory_exists(path)
    }

    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        self.host.get_directories(path)
    }
}

#[derive(Trace, Finalize)]
pub struct DirectoryStructureHostRcDynCompilerHostLike {
    host: Gc<Box<dyn CompilerHostLike>>,
}

impl DirectoryStructureHostRcDynCompilerHostLike {
    pub fn new(host: Gc<Box<dyn CompilerHostLike>>) -> Self {
        Self { host }
    }
}

impl DirectoryStructureHost for DirectoryStructureHostRcDynCompilerHostLike {
    fn file_exists(&self, path: &str) -> bool {
        self.host.file_exists(path)
    }

    fn read_file(&self, path: &str, encoding: Option<&str>) -> io::Result<Option<String>> {
        self.host.read_file(path)
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        self.host.directory_exists(path)
    }
    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        self.host.get_directories(path)
    }
    fn read_directory(
        &self,
        path: &str,
        extensions: &[&str],
        exclude: Option<&[String]>,
        include: Option<&[String]>,
        depth: Option<usize>,
    ) -> Option<Vec<String>> {
        self.host
            .read_directory(path, extensions, exclude, include.unwrap(), depth)
    }
    fn is_read_directory_implemented(&self) -> bool {
        self.host.is_read_directory_implemented()
    }

    fn realpath(&self, path: &str) -> Option<String> {
        self.host.realpath(path)
    }

    fn create_directory(&self, path: &str) {
        self.host.create_directory(path)
    }

    fn write_file(&self, path: &str, data: &str, write_byte_order_mark: Option<bool>) {
        self.host.write_file(path, data, write_byte_order_mark)
    }
}

pub(crate) fn parse_config_host_from_compiler_host_like(
    host: Gc<Box<dyn CompilerHostLike>>,
    directory_structure_host: Option<Gc<Box<dyn DirectoryStructureHost>>>,
) -> ParseConfigHostFromCompilerHostLike {
    let directory_structure_host = directory_structure_host.unwrap_or_else(|| {
        Gc::new(Box::new(DirectoryStructureHostRcDynCompilerHostLike::new(
            host.clone(),
        )))
    });
    ParseConfigHostFromCompilerHostLike::new(host, directory_structure_host)
}

#[derive(Trace, Finalize)]
pub struct ParseConfigHostFromCompilerHostLike {
    host: Gc<Box<dyn CompilerHostLike>>,
    directory_structure_host: Gc<Box<dyn DirectoryStructureHost>>,
}

impl ParseConfigHostFromCompilerHostLike {
    pub fn new(
        host: Gc<Box<dyn CompilerHostLike>>,
        directory_structure_host: Gc<Box<dyn DirectoryStructureHost>>,
    ) -> Self {
        Self {
            host,
            directory_structure_host,
        }
    }
}

impl ParseConfigFileHost for ParseConfigHostFromCompilerHostLike {
    fn get_current_directory(&self) -> String {
        self.host.get_current_directory()
    }
}

impl ParseConfigHost for ParseConfigHostFromCompilerHostLike {
    fn use_case_sensitive_file_names(&self) -> bool {
        self.host.use_case_sensitive_file_names()
    }

    fn read_directory(
        &self,
        root: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> Vec<String> {
        Debug_.assert(self.directory_structure_host.is_read_directory_implemented(), Some("'CompilerHost.readDirectory' must be implemented to correctly process 'projectReferences'"));
        self.directory_structure_host
            .read_directory(root, extensions, excludes, Some(includes), depth)
            .unwrap()
    }

    fn file_exists(&self, path: &str) -> bool {
        self.directory_structure_host.file_exists(path)
    }

    fn read_file(&self, path: &str) -> io::Result<Option<String>> {
        self.directory_structure_host.read_file(path, None)
    }

    fn trace(&self, s: &str) {
        CompilerHostLike::trace(&**self.host, s)
    }

    fn is_trace_supported(&self) -> bool {
        self.host.is_trace_supported()
    }

    fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
        self
    }
}

impl ConfigFileDiagnosticsReporter for ParseConfigHostFromCompilerHostLike {
    fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Gc<Diagnostic>) {
        self.host
            .on_un_recoverable_config_file_diagnostic(diagnostic)
    }
}

pub fn resolve_project_reference_path(ref_: &ProjectReference) -> ResolvedConfigFileName {
    let passed_in_ref = ref_;
    resolve_config_file_project_name(&passed_in_ref.path)
}

pub fn get_resolution_diagnostic(
    options: &CompilerOptions,
    resolved_module: &ResolvedModuleFull,
) -> Option<&'static DiagnosticMessage> {
    let extension = resolved_module.extension();
    match extension {
        Extension::Ts | Extension::Dts => None,
        Extension::Tsx => need_jsx(options),
        Extension::Jsx => need_jsx(options).or_else(|| need_allow_js(options)),
        Extension::Js => need_allow_js(options),
        Extension::Json => need_resolve_json_module(options),
        _ => None,
    }
}

fn need_jsx(options: &CompilerOptions) -> Option<&'static DiagnosticMessage> {
    if options.jsx.is_some() {
        None
    } else {
        Some(&Diagnostics::Module_0_was_resolved_to_1_but_jsx_is_not_set)
    }
}

fn need_allow_js(options: &CompilerOptions) -> Option<&'static DiagnosticMessage> {
    if get_allow_js_compiler_option(options) || !get_strict_option_value(options, "noImplicitAny") {
        None
    } else {
        Some(&Diagnostics::Could_not_find_a_declaration_file_for_module_0_1_implicitly_has_an_any_type)
    }
}

fn need_resolve_json_module(options: &CompilerOptions) -> Option<&'static DiagnosticMessage> {
    if matches!(options.resolve_json_module, Some(true)) {
        None
    } else {
        Some(&Diagnostics::Module_0_was_resolved_to_1_but_resolveJsonModule_is_not_used)
    }
}

fn get_module_names(file: &Node /*SourceFile*/) -> Vec<String> {
    let file_as_source_file = file.as_source_file();
    let imports = file_as_source_file.maybe_imports();
    let imports = imports.as_ref().unwrap();
    let module_augmentations = file_as_source_file.maybe_module_augmentations();
    let module_augmentations = module_augmentations.as_ref().unwrap();
    let mut res: Vec<String> = imports
        .into_iter()
        .map(|i| i.as_literal_like_node().text().clone())
        .collect();
    for aug in module_augmentations {
        if aug.kind() == SyntaxKind::StringLiteral {
            res.push(aug.as_literal_like_node().text().clone());
        }
    }
    res
}

pub(crate) fn get_module_name_string_literal_at<TFile: SourceFileImportsList>(
    file: &TFile,
    index: usize,
) -> Gc<Node /*StringLiteralLike*/> {
    unimplemented!()
}

#[derive(Trace, Finalize)]
struct CompilerHostGetCanonicalFileName {
    host: Gc<Box<dyn CompilerHost>>,
}

impl CompilerHostGetCanonicalFileName {
    pub fn new(host: Gc<Box<dyn CompilerHost>>) -> Self {
        Self { host }
    }
}

impl GetCanonicalFileName for CompilerHostGetCanonicalFileName {
    fn call(&self, file_name: &str) -> String {
        self.host.get_canonical_file_name(file_name)
    }
}
