use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    convert::TryInto,
    io,
    iter::FromIterator,
    rc::Rc,
};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use super::{
    for_each_project_reference, get_mode_for_resolution_at_index, SourceFileImportsList, ToPath,
};
use crate::{
    chain_diagnostic_messages, chain_diagnostic_messages_multiple, compare_paths, contains_path,
    create_compiler_diagnostic, create_compiler_diagnostic_from_message_chain,
    create_diagnostic_for_node_in_source_file, create_file_diagnostic,
    create_file_diagnostic_from_message_chain, create_input_files, create_symlink_cache,
    explain_if_file_is_redirect, file_extension_is, file_extension_is_one_of,
    file_include_reason_to_diagnostics, find, first_defined, for_each_emitted_file, for_each_entry,
    get_allow_js_compiler_option, get_base_file_name, get_directory_path, get_emit_declarations,
    get_emit_module_kind, get_emit_module_resolution_kind, get_emit_script_target,
    get_error_span_for_node, get_matched_file_spec, get_matched_include_spec,
    get_normalized_absolute_path, get_output_paths_for_bundle, get_property_assignment,
    get_referenced_file_location, get_root_length, get_spelling_suggestion,
    get_strict_option_value, get_ts_build_info_emit_output_file_path,
    get_ts_config_object_literal_expression, get_ts_config_prop_array,
    get_ts_config_prop_array_element_value, has_json_module_emit_enabled,
    has_zero_or_one_asterisk_character, inverse_jsx_option_map, is_array_literal_expression,
    is_declaration_file_name, is_external_module, is_identifier_text, is_in_js_file,
    is_incremental_compilation, is_object_literal_expression, is_option_str_empty,
    is_reference_file_location, is_referenced_file, is_source_file_js, lib_map, libs, out_file,
    parse_isolated_entity_name, parse_json_source_file_config_file_content, path_is_absolute,
    path_is_relative, remove_file_extension, remove_prefix, remove_suffix,
    resolution_extension_is_ts_or_json, resolve_config_file_project_name, set_resolved_module,
    source_file_may_be_emitted, string_contains, supported_js_extensions_flat,
    target_option_declaration, to_file_name_lower_case, try_maybe_for_each, version,
    CancellationTokenDebuggable, CommandLineOptionInterface, CommandLineOptionMapTypeValue,
    Comparison, CompilerHost, CompilerOptions, ConfigFileDiagnosticsReporter, Debug_, Diagnostic,
    DiagnosticInterface, DiagnosticMessage, DiagnosticMessageChain, DiagnosticRelatedInformation,
    Diagnostics, DirectoryStructureHost, EmitFileNames, EmitResult, Extension, FileIncludeKind,
    FileIncludeReason, FilePreprocessingDiagnostics, FilePreprocessingDiagnosticsKind,
    FilePreprocessingFileExplainingDiagnostic, FilePreprocessingReferencedDiagnostic,
    FileReference, GetCanonicalFileName, GetOrInsertDefault, HasArena, JsxEmit, ModuleKind,
    ModuleResolutionHost, ModuleResolutionHostOverrider, ModuleResolutionKind,
    NamedDeclarationInterface, Node, NodeFlags, NodeInterface, ParseConfigFileHost,
    ParseConfigHost, ParsedCommandLine, Path, PeekableExt, Program, ProjectReference,
    ReadFileCallback, ReferencedFile, ResolvedModuleFull, ResolvedProjectReference,
    ResolvedProjectReferenceBuilder, ScriptKind, ScriptReferenceHost, ScriptTarget, SymlinkCache,
    SyntaxKind, UnwrapOrEmpty, WriteFileCallback,
};

impl Program {
    pub fn process_lib_reference_directives(
        &self,
        file: Id<Node>, /*SourceFile*/
    ) -> io::Result<()> {
        let file_as_source_file = file.as_source_file();
        try_maybe_for_each(
            file_as_source_file
                .maybe_lib_reference_directives()
                .as_ref()
                .map(|file_lib_reference_directives| (**file_lib_reference_directives).borrow())
                .as_deref(),
            |lib_reference: &FileReference, index| -> io::Result<Option<()>> {
                let lib_name = to_file_name_lower_case(&lib_reference.file_name);
                let lib_file_name = lib_map.with(|lib_map_| lib_map_.get(&&*lib_name).copied());
                if let Some(lib_file_name) = lib_file_name {
                    self.process_root_file(
                        &self.path_for_lib_file(lib_file_name)?,
                        true,
                        true,
                        Gc::new(FileIncludeReason::ReferencedFile(ReferencedFile {
                            kind: FileIncludeKind::LibReferenceDirective,
                            file: file_as_source_file.path().clone(),
                            index,
                        })),
                    )?;
                } else {
                    let unqualified_lib_name =
                        remove_suffix(remove_prefix(&lib_name, "lib."), ".d.ts");
                    let suggestion = libs.with(|libs_| {
                        get_spelling_suggestion(unqualified_lib_name, libs_, |lib: &&str| {
                            Some((*lib).to_owned())
                        })
                        .map(|suggestion| (*suggestion).to_owned())
                    });
                    let diagnostic = if suggestion.is_some() {
                        &*Diagnostics::Cannot_find_lib_definition_for_0_Did_you_mean_1
                    } else {
                        &*Diagnostics::Cannot_find_lib_definition_for_0
                    };
                    self.maybe_file_processing_diagnostics().get_or_insert_default_().push(
                        Gc::new(FilePreprocessingDiagnostics::FilePreprocessingReferencedDiagnostic(FilePreprocessingReferencedDiagnostic {
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
                        }))
                    );
                }
                Ok(None)
            },
        )?;

        Ok(())
    }

    pub fn get_canonical_file_name(&self, file_name: &str) -> String {
        self.host().get_canonical_file_name(file_name)
    }

    pub fn get_canonical_file_name_rc(&self) -> Gc<Box<dyn GetCanonicalFileName>> {
        let host = self.host();
        Gc::new(Box::new(CompilerHostGetCanonicalFileName::new(host)))
    }

    pub fn process_imported_modules(&self, file: Id<Node> /*SourceFile*/) -> io::Result<()> {
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
            let resolutions = self.resolve_module_names_reusing_old_state(&module_names, file)?;
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
                        Gc::new(FileIncludeReason::ReferencedFile(ReferencedFile {
                            kind: FileIncludeKind::Import,
                            file: file_as_source_file.path().clone(),
                            index,
                        })),
                        resolution.package_id.as_ref(),
                    )?;
                }

                if is_from_node_modules_search == Some(true) {
                    self.set_current_node_modules_depth(self.current_node_modules_depth() - 1);
                }
            }
        } else {
            *file_as_source_file.maybe_resolved_modules() = None;
        }

        Ok(())
    }

    pub fn check_source_files_belong_to_path(
        &self,
        source_files: &[Id<Node /*SourceFile*/>],
        root_directory: &str,
    ) -> bool {
        let mut all_files_belong_to_path = true;
        let absolute_root_directory_path =
            self.host()
                .get_canonical_file_name(&get_normalized_absolute_path(
                    root_directory,
                    Some(&**self.current_directory()),
                ));
        for source_file in source_files {
            let source_file_as_source_file = source_file.as_source_file();
            if !source_file_as_source_file.is_declaration_file() {
                let absolute_source_file_path =
                    self.host()
                        .get_canonical_file_name(&get_normalized_absolute_path(
                            &source_file_as_source_file.file_name(),
                            Some(&**self.current_directory()),
                        ));
                if absolute_source_file_path.find(&absolute_root_directory_path) != Some(0) {
                    self.add_program_diagnostic_explaining_file(
                        source_file,
                        &Diagnostics::File_0_is_not_under_rootDir_1_rootDir_is_expected_to_contain_all_source_files,
                        Some(vec![
                            source_file_as_source_file.file_name().clone(),
                            root_directory.to_owned(),
                        ])
                    );
                    all_files_belong_to_path = false;
                }
            }
        }

        all_files_belong_to_path
    }

    pub fn parse_project_reference_config_file(
        &self,
        ref_: &ProjectReference,
    ) -> io::Result<Option<Gc<ResolvedProjectReference>>> {
        if self.maybe_project_reference_redirects_mut().is_none() {
            *self.maybe_project_reference_redirects_mut() = Some(HashMap::new());
        }

        let ref_path = resolve_project_reference_path(ref_);
        let source_file_path = self.to_path(&ref_path);
        let from_cache = self
            .maybe_project_reference_redirects_mut()
            .as_ref()
            .unwrap()
            .get(&source_file_path)
            .cloned();
        if let Some(from_cache) = from_cache {
            return Ok(from_cache);
        }

        let command_line: Option<ParsedCommandLine>;
        let source_file: Option<Id<Node /*JsonSourceFile*/>>;
        if self.host().is_get_parsed_command_line_supported() {
            command_line = self.host().get_parsed_command_line(&ref_path);
            if command_line.is_none() {
                self.add_file_to_files_by_name(Option::<Id<Node>>::None, &source_file_path, None);
                self.project_reference_redirects_mut()
                    .insert(source_file_path.clone(), None);
                return Ok(None);
            }
            let command_line = command_line.as_ref().unwrap();
            source_file =
                Some(Debug_.check_defined(command_line.options.config_file.clone(), None));
            let source_file = source_file.as_ref().unwrap();
            Debug_.assert(
                match source_file.as_source_file().maybe_path().as_ref() {
                    None => true,
                    Some(source_file_path_) => source_file_path_ == &source_file_path,
                },
                None,
            );
            self.add_file_to_files_by_name(Some(&**source_file), &source_file_path, None);
        } else {
            let base_path = get_normalized_absolute_path(
                &get_directory_path(&ref_path),
                Some(&CompilerHost::get_current_directory(&**self.host())?),
            );
            source_file = self
                .host()
                .get_source_file(&ref_path, ScriptTarget::JSON, None, None)?;
            self.add_file_to_files_by_name(source_file.as_deref(), &source_file_path, None);
            if source_file.is_none() {
                self.project_reference_redirects_mut()
                    .insert(source_file_path.clone(), None);
                return Ok(None);
            }
            let source_file = source_file.as_ref().unwrap();
            command_line = Some(parse_json_source_file_config_file_content(
                source_file,
                &**self.config_parsing_host(),
                &base_path,
                None,
                Some(&ref_path),
                None,
                None,
                None,
                None,
            )?);
        }
        let source_file = source_file.unwrap();
        let source_file_as_source_file = source_file.as_source_file();
        source_file_as_source_file.set_file_name(ref_path.clone());
        source_file_as_source_file.set_path(source_file_path.clone());
        source_file_as_source_file.set_resolved_path(Some(source_file_path.clone()));
        source_file_as_source_file.set_original_file_name(Some(ref_path));
        let command_line = Gc::new(command_line.unwrap());

        let resolved_ref = Gc::new(
            ResolvedProjectReferenceBuilder::default()
                .command_line(command_line.clone())
                .source_file(source_file)
                .build()
                .unwrap(),
        );
        self.project_reference_redirects_mut()
            .insert(source_file_path, Some(resolved_ref.clone()));
        if let Some(command_line_project_references) = command_line.project_references.as_ref() {
            resolved_ref.set_references(Some(
                command_line_project_references
                    .into_iter()
                    .map(|command_line_project_reference| {
                        self.parse_project_reference_config_file(command_line_project_reference)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ));
        }
        Ok(Some(resolved_ref))
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
            find(&**self.files(), |f: &Id<Node>, _| {
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

            let first_non_external_module_source_file = find(&**self.files(), |f: &Id<Node>, _| {
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
                            &Diagnostics::Cannot_use_imports_exports_or_module_augmentations_when_module_is_none,
                            None,
                        ).into()
                    )
                );
            }
        }

        if !is_option_str_empty(output_file) && self.options.emit_declaration_only != Some(true) {
            if matches!(
                self.options.module.filter(|options_module| *options_module != ModuleKind::None),
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
            let emit_host = self.get_emit_host(None);
            let mut emit_files_seen: HashSet<String> = Default::default();
            for_each_emitted_file(
                &**emit_host,
                |emit_file_names, _| {
                    if self.options.emit_declaration_only != Some(true) {
                        self.verify_emit_file_path(
                            emit_file_names.js_file_path.as_deref(),
                            &mut emit_files_seen,
                        );
                    }
                    self.verify_emit_file_path(
                        emit_file_names.declaration_file_path.as_deref(),
                        &mut emit_files_seen,
                    );
                },
                Option::<Id<Node>>::None,
                None,
                None,
                None,
                self,
            );
        }
    }

    pub(super) fn verify_emit_file_path(
        &self,
        emit_file_name: Option<&str>,
        emit_files_seen: &mut HashSet<String>,
    ) {
        if let Some(emit_file_name) =
            emit_file_name.filter(|emit_file_name| !emit_file_name.is_empty())
        {
            let emit_file_path = self.to_path(emit_file_name);
            if self.files_by_name().contains_key(&*emit_file_path) {
                let mut chain: Option<DiagnosticMessageChain> = None;
                if self
                    .options
                    .config_file_path
                    .as_ref()
                    .filter(|options_config_file_path| !options_config_file_path.is_empty())
                    .is_none()
                {
                    chain = Some(chain_diagnostic_messages(
                        None,
                        &Diagnostics::Adding_a_tsconfig_json_file_will_help_organize_projects_that_contain_both_TypeScript_and_JavaScript_files_Learn_more_at_https_Colon_Slash_Slashaka_ms_Slashtsconfig,
                        None,
                    ));
                }
                chain = Some(chain_diagnostic_messages(
                    chain,
                    &Diagnostics::Cannot_write_file_0_because_it_would_overwrite_input_file,
                    Some(vec![emit_file_name.to_owned()]),
                ));
                self.block_emitting_of_file(
                    emit_file_name,
                    Gc::new(
                        create_compiler_diagnostic_from_message_chain(chain.unwrap(), None).into(),
                    ),
                );
            }

            let emit_file_key = if !CompilerHost::use_case_sensitive_file_names(&**self.host()) {
                to_file_name_lower_case(&emit_file_path)
            } else {
                (&*emit_file_path).to_owned()
            };
            if emit_files_seen.contains(&emit_file_key) {
                self.block_emitting_of_file(
                    emit_file_name,
                    Gc::new(create_compiler_diagnostic(
                        &Diagnostics::Cannot_write_file_0_because_it_would_be_overwritten_by_multiple_input_files,
                        Some(vec![
                            emit_file_name.to_owned()
                        ])
                    ).into())
                );
            } else {
                emit_files_seen.insert(emit_file_key);
            }
        }
    }

    pub fn create_diagnostic_explaining_file(
        &self,
        file: Option<Id<Node>>,
        mut file_processing_reason: Option<Gc<FileIncludeReason>>,
        diagnostic: &'static DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Gc<Diagnostic> {
        let mut file_include_reasons: Option<Vec<DiagnosticMessageChain>> = None;
        let mut related_info: Option<Vec<Gc<DiagnosticRelatedInformation>>> = None;
        let mut location_reason = if is_referenced_file(file_processing_reason.as_deref()) {
            file_processing_reason.clone()
        } else {
            None
        };
        let file = file.map(|file| file.borrow().node_wrapper());
        let file_reasons = self.file_reasons();
        if let Some(file) = file.as_ref() {
            let file_reasons = (*file_reasons).borrow();
            if let Some(reasons) = file_reasons.get(&*file.as_source_file().path()) {
                for reason in reasons {
                    self.process_reason(
                        &mut file_include_reasons,
                        &mut location_reason,
                        &mut related_info,
                        &mut file_processing_reason,
                        reason.clone(),
                    );
                }
            }
        }
        if file_processing_reason.is_some() {
            let file_processing_reason_present = file_processing_reason.clone().unwrap();
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

    pub fn process_reason(
        &self,
        file_include_reasons: &mut Option<Vec<DiagnosticMessageChain>>,
        location_reason: &mut Option<Gc<FileIncludeReason>>,
        related_info: &mut Option<Vec<Gc<DiagnosticRelatedInformation>>>,
        file_processing_reason: &mut Option<Gc<FileIncludeReason>>,
        reason: Gc<FileIncludeReason>,
    ) {
        file_include_reasons
            .get_or_insert_default_()
            .push(file_include_reason_to_diagnostics(
                self,
                &reason,
                Option::<fn(&str) -> String>::None,
            ));
        if location_reason.is_none() && is_referenced_file(Some(&reason)) {
            *location_reason = Some(reason.clone());
        } else if !matches!(
            location_reason.clone(),
            Some(location_reason) if location_reason == reason
        ) {
            if let Some(related_information) =
                self.file_include_reason_to_related_information(&reason)
            {
                related_info
                    .get_or_insert_default_()
                    .push(related_information);
            }
        }
        if matches!(
            file_processing_reason.clone(),
            Some(file_processing_reason) if reason == file_processing_reason
        ) {
            *file_processing_reason = None
        }
    }

    pub fn add_file_preprocessing_file_explaining_diagnostic(
        &self,
        file: Option<Id<Node>>,
        file_processing_reason: Gc<FileIncludeReason>,
        diagnostic: &'static DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.maybe_file_processing_diagnostics()
            .get_or_insert_default_()
            .push(Gc::new(FilePreprocessingDiagnostics::FilePreprocessingFileExplainingDiagnostic(FilePreprocessingFileExplainingDiagnostic {
                kind: FilePreprocessingDiagnosticsKind::FilePreprocessingFileExplainingDiagnostic,
                file: file.map(|file| file.borrow().as_source_file().path().clone()),
                file_processing_reason: file_processing_reason.clone(),
                diagnostic,
                args,
            })))
    }

    pub fn add_program_diagnostic_explaining_file(
        &self,
        file: Id<Node>, /*SourceFile*/
        diagnostic: &'static DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.program_diagnostics_mut()
            .add(self.create_diagnostic_explaining_file(Some(file), None, diagnostic, args));
    }

    pub fn file_include_reason_to_related_information(
        &self,
        reason: &FileIncludeReason,
    ) -> Option<Gc<DiagnosticRelatedInformation /*DiagnosticWithLocation*/>> {
        if is_referenced_file(Some(reason)) {
            let reference_location = get_referenced_file_location(
                |path| self.get_source_file_by_path(path),
                reason.as_referenced_file(),
            );
            let message: &DiagnosticMessage;
            match reason.kind() {
                FileIncludeKind::Import => {
                    message = &*Diagnostics::File_is_included_via_import_here;
                }
                FileIncludeKind::ReferenceFile => {
                    message = &*Diagnostics::File_is_included_via_reference_here;
                }
                FileIncludeKind::TypeReferenceDirective => {
                    message = &*Diagnostics::File_is_included_via_type_library_reference_here;
                }
                FileIncludeKind::LibReferenceDirective => {
                    message = &*Diagnostics::File_is_included_via_library_reference_here;
                }
                _ => {
                    Debug_.assert_never(reason, None);
                }
            }
            return if is_reference_file_location(&reference_location) {
                let reference_location_as_reference_file_location =
                    reference_location.as_reference_file_location();
                Some(Gc::new(
                    create_file_diagnostic(
                        &reference_location_as_reference_file_location.file,
                        reference_location_as_reference_file_location.pos,
                        reference_location_as_reference_file_location.end
                            - reference_location_as_reference_file_location.pos,
                        message,
                        None,
                    )
                    .into(),
                ))
            } else {
                None
            };
        }

        let options_config_file = self.options.config_file.as_ref()?;
        let config_file_node: Option<Id<Node>>;
        let message: &DiagnosticMessage;
        match reason.kind() {
            FileIncludeKind::RootFile => {
                if options_config_file
                    .as_source_file()
                    .maybe_config_file_specs()
                    .is_none()
                {
                    return None;
                }
                let reason_as_root_file = reason.as_root_file();
                let file_name = get_normalized_absolute_path(
                    &self.root_names()[reason_as_root_file.index],
                    Some(&**self.current_directory()),
                );
                let matched_by_files = get_matched_file_spec(self, &file_name);
                if let Some(matched_by_files) =
                    matched_by_files.filter(|matched_by_files| !matched_by_files.is_empty())
                {
                    config_file_node = get_ts_config_prop_array_element_value(
                        Some(&**options_config_file),
                        "files",
                        &matched_by_files,
                    );
                    message = &*Diagnostics::File_is_matched_by_files_list_specified_here;
                } else {
                    let matched_by_include = get_matched_include_spec(self, &file_name)
                        .filter(|matched_by_include| !matched_by_include.is_empty())?;
                    config_file_node = get_ts_config_prop_array_element_value(
                        Some(&**options_config_file),
                        "include",
                        &matched_by_include,
                    );
                    message = &*Diagnostics::File_is_matched_by_include_pattern_specified_here;
                }
            }
            FileIncludeKind::SourceFromProjectReference
            | FileIncludeKind::OutputFromProjectReference => {
                let reason_as_project_reference_file = reason.as_project_reference_file();
                let referenced_resolved_ref = Debug_.check_defined(
                    self.maybe_resolved_project_references_mut()
                        .as_ref()
                        .and_then(|resolved_project_references| {
                            resolved_project_references
                                .get(reason_as_project_reference_file.index)
                                .cloned()
                                .flatten()
                        }),
                    None,
                );
                let reference_info = for_each_project_reference(
                    self.maybe_project_references().as_deref(),
                    self.maybe_resolved_project_references_mut().as_deref(),
                    |resolved_ref, parent, index| {
                        if matches!(
                            resolved_ref.as_ref(),
                            Some(resolved_ref) if Gc::ptr_eq(
                                resolved_ref,
                                &referenced_resolved_ref,
                            )
                        ) {
                            Some((
                                /*sourceFile:*/
                                parent.map_or_else(
                                    || options_config_file.clone(),
                                    |parent| parent.source_file.clone(),
                                ),
                                /*index:*/ index,
                            ))
                        } else {
                            None
                        }
                    },
                    Option::<
                        fn(Option<&[Rc<ProjectReference>]>, Option<&ResolvedProjectReference>) -> _,
                    >::None,
                )?;
                let (source_file, index) = reference_info;
                let references_syntax = first_defined(
                    get_ts_config_prop_array(Some(&*source_file), "references"),
                    |property: Id<Node>, _| {
                        property.as_has_initializer().maybe_initializer().filter(
                            |property_initializer| {
                                is_array_literal_expression(property_initializer)
                            },
                        )
                    },
                );
                return references_syntax
                    .filter(|references_syntax| {
                        references_syntax
                            .as_array_literal_expression()
                            .elements
                            .len()
                            > index
                    })
                    .map(|references_syntax| {
                        Gc::new(
                        create_diagnostic_for_node_in_source_file(
                            &source_file,
                            &references_syntax.as_array_literal_expression().elements[index],
                            if reason.kind() == FileIncludeKind::OutputFromProjectReference {
                                &*Diagnostics::File_is_output_from_referenced_project_specified_here
                            } else {
                                &*Diagnostics::File_is_source_from_referenced_project_specified_here
                            },
                            None,
                        ).into()
                    )
                    });
            }
            FileIncludeKind::AutomaticTypeDirectiveFile => {
                if self.options.types.is_none() {
                    return None;
                }
                config_file_node = self.get_options_syntax_by_array_element_value(
                    "types",
                    &reason.as_automatic_type_directive_file().type_reference,
                );
                message = &Diagnostics::File_is_entry_point_of_type_library_specified_here;
            }
            FileIncludeKind::LibFile => {
                let reason_as_lib_file = reason.as_lib_file();
                if let Some(reason_index) = reason_as_lib_file.index {
                    config_file_node = self.get_options_syntax_by_array_element_value(
                        "lib",
                        &self.options.lib.as_ref().unwrap()[reason_index],
                    );
                    message = &Diagnostics::File_is_library_specified_here;
                } else {
                    let target = target_option_declaration.with(|target_option_declaration_| {
                        for_each_entry(target_option_declaration_.type_().as_map(), |value, key| {
                            if value
                                == &CommandLineOptionMapTypeValue::ScriptTarget(
                                    get_emit_script_target(&self.options),
                                )
                            {
                                Some((*key).to_owned())
                            } else {
                                None
                            }
                        })
                    });
                    config_file_node = target
                        .as_ref()
                        .and_then(|target| self.get_options_syntax_by_value("target", target));
                    message = &Diagnostics::File_is_default_library_for_target_specified_here;
                }
            }
            _ => {
                Debug_.assert_never(reason, None);
            }
        }
        config_file_node.as_ref().map(|config_file_node| {
            Gc::new(
                create_diagnostic_for_node_in_source_file(
                    options_config_file,
                    config_file_node,
                    message,
                    None,
                )
                .into(),
            )
        })
    }

    pub fn verify_project_references(&self) {
        let build_info_path = if self.options.suppress_output_path_check != Some(true) {
            get_ts_build_info_emit_output_file_path(&self.options)
        } else {
            None
        };
        for_each_project_reference(
            self.maybe_project_references().as_deref(),
            self.maybe_resolved_project_references_mut().as_deref(),
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
                        self.has_emit_blocking_diagnostics_mut()
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
        let mut need_compiler_diagnostic = true;
        let paths_syntax = self.get_options_paths_syntax();
        for path_prop in paths_syntax {
            let ref path_prop_initializer =
                path_prop.as_has_initializer().maybe_initializer().unwrap();
            if is_object_literal_expression(path_prop_initializer) {
                for key_props in get_property_assignment(path_prop_initializer, key, None) {
                    let ref initializer =
                        key_props.as_has_initializer().maybe_initializer().unwrap();
                    if is_array_literal_expression(initializer) {
                        let initializer_as_array_literal_expression =
                            initializer.as_array_literal_expression();
                        if initializer_as_array_literal_expression.elements.len() > value_index {
                            self.program_diagnostics_mut().add(Gc::new(
                                create_diagnostic_for_node_in_source_file(
                                    self.options.config_file.as_ref().unwrap(),
                                    &initializer_as_array_literal_expression.elements[value_index],
                                    message,
                                    args.clone(),
                                )
                                .into(),
                            ));
                            need_compiler_diagnostic = false;
                        }
                    }
                }
            }
        }
        if need_compiler_diagnostic {
            self.program_diagnostics_mut()
                .add(Gc::new(create_compiler_diagnostic(message, args).into()));
        }
    }

    pub fn create_diagnostic_for_option_paths(
        &self,
        on_key: bool,
        key: &str,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        let mut need_compiler_diagnostic = true;
        let paths_syntax = self.get_options_paths_syntax();
        for path_prop in paths_syntax {
            let ref path_prop_initializer =
                path_prop.as_has_initializer().maybe_initializer().unwrap();
            if is_object_literal_expression(path_prop_initializer)
                && self.create_option_diagnostic_in_object_literal_syntax(
                    path_prop_initializer,
                    on_key,
                    key,
                    None,
                    message,
                    args.clone(),
                )
            {
                need_compiler_diagnostic = false;
            }
        }
        if need_compiler_diagnostic {
            self.program_diagnostics_mut()
                .add(Gc::new(create_compiler_diagnostic(message, args).into()));
        }
    }

    pub fn get_options_syntax_by_name<'name>(
        &self,
        name: &'name str,
    ) -> Option<impl Iterator<Item = Id<Node>> + 'name> {
        let compiler_options_object_literal_syntax =
            self.get_compiler_options_object_literal_syntax();
        compiler_options_object_literal_syntax.map(
            |compiler_options_object_literal_syntax| {
                get_property_assignment(compiler_options_object_literal_syntax, name, None, self)
            },
        )
    }

    pub fn get_options_paths_syntax(&self) -> impl Iterator<Item = Id<Node>> {
        self.get_options_syntax_by_name("paths").unwrap_or_empty()
    }

    pub fn get_options_syntax_by_value(&self, _name: &str, _value: &str) -> Option<Id<Node>> {
        unimplemented!()
    }

    pub fn get_options_syntax_by_array_element_value(
        &self,
        _name: &str,
        _value: &str,
    ) -> Option<Id<Node>> {
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

    pub fn create_diagnostic_for_reference(
        &self,
        source_file: Option<Id<Node> /*JsonSourceFile*/>,
        index: usize,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        let source_file = source_file.node_wrappered();
        let references_syntax = first_defined(
            get_ts_config_prop_array(
                source_file
                    .clone()
                    .or_else(|| self.options.config_file.clone()),
                "references",
            ),
            |ref property: Id<Node>, _| {
                let property_as_property_assignment = property.as_property_assignment();
                is_array_literal_expression(&property_as_property_assignment.initializer)
                    .then(|| property_as_property_assignment.initializer.clone())
            },
        );
        if let Some(references_syntax) = references_syntax.filter(|references_syntax| {
            references_syntax
                .as_array_literal_expression()
                .elements
                .len()
                > index
        }) {
            self.program_diagnostics_mut().add(
                create_diagnostic_for_node_in_source_file(
                    source_file
                        .as_ref()
                        .or_else(|| self.options.config_file.as_ref())
                        .unwrap(),
                    &references_syntax.as_array_literal_expression().elements[index],
                    message,
                    args,
                )
                .into(),
            );
        } else {
            self.program_diagnostics_mut()
                .add(create_compiler_diagnostic(message, args).into());
        }
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

    pub fn get_compiler_options_object_literal_syntax(&self) -> Option<Id<Node>> {
        if self
            .maybe_compiler_options_object_literal_syntax()
            .is_none()
        {
            self.set_compiler_options_object_literal_syntax(Some(None));
            let json_object_literal =
                get_ts_config_object_literal_expression(self.options.config_file, self);
            if let Some(json_object_literal) = json_object_literal {
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
        object_literal: Id<Node>, /*ObjectLiteralExpression*/
        on_key: bool,
        key1: &str,
        key2: Option<&str>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> bool {
        let props = get_property_assignment(object_literal, key1, key2);
        for prop in props.clone() {
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
        !props.peekable().is_empty_()
    }

    pub fn block_emitting_of_file(&self, emit_file_name: &str, diag: Gc<Diagnostic>) {
        self.has_emit_blocking_diagnostics_mut()
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
            *self.symlinks() = Some(Gc::new(create_symlink_cache(
                &self.current_directory(),
                Gc::new(Box::new(HostGetCanonicalFileName::new(self.host()))),
            )));
        }
        let symlinks = self.symlinks().clone().unwrap();
        if
        /*files && resolvedTypeReferenceDirectives &&*/
        !symlinks.has_processed_resolutions() {
            symlinks.set_symlinks_from_resolutions(
                &self.files(),
                Some(&(*self.resolved_type_reference_directives()).borrow()),
            );
        }
        symlinks
    }

    pub fn get_symlink_cache_rc(&self) -> Gc<Box<dyn GetSymlinkCache>> {
        Gc::new(Box::new(ProgramGetSymlinkCache::new(self.rc_wrapper())))
    }
}

#[derive(Trace, Finalize)]
pub struct HostGetCanonicalFileName {
    host: Gc<Box<dyn CompilerHost>>,
}

impl HostGetCanonicalFileName {
    pub fn new(host: Gc<Box<dyn CompilerHost>>) -> Self {
        Self { host }
    }
}

impl GetCanonicalFileName for HostGetCanonicalFileName {
    fn call(&self, file_name: &str) -> String {
        self.host.get_canonical_file_name(file_name)
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

    fn handle_directory_could_be_symlink(&self, _directory: &str) {
        unimplemented!()
    }

    fn file_or_directory_exists_using_source(
        &self,
        _file_or_directory: &str,
        _is_file: bool,
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

    fn read_file(&self, _file_name: &str) -> io::Result<Option<String>> {
        unreachable!()
    }

    fn write_file(
        &self,
        _file_name: &str,
        _data: &str,
        _write_byte_order_mark: bool,
        _on_error: Option<&mut dyn FnMut(&str)>,
        _source_files: Option<&[Id<Node /*SourceFile*/>]>,
    ) -> io::Result<()> {
        unreachable!()
    }

    fn create_directory(&self, _directory: &str) -> io::Result<()> {
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

pub(crate) fn emit_skipped_with_no_diagnostics() -> EmitResult {
    EmitResult {
        emit_skipped: true,
        diagnostics: Default::default(),
        emitted_files: Default::default(),
        source_maps: Default::default(),
        exported_modules_from_declaration_emit: Default::default(),
    }
}

pub(crate) fn handle_no_emit_options(
    program: Gc<Box<Program>>,
    source_file: Option<Id<Node> /*SourceFile*/>,
    write_file: Option<Gc<Box<dyn WriteFileCallback>>>,
    cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
) -> io::Result<Option<EmitResult>> {
    let options = program.get_compiler_options();
    if options.no_emit == Some(true) {
        program.get_semantic_diagnostics(source_file, cancellation_token.clone())?;
        return Ok(Some(
            if source_file.is_some()
                || out_file(&options)
                    .filter(|out_file| !out_file.is_empty())
                    .is_some()
            {
                emit_skipped_with_no_diagnostics()
            } else {
                program.emit_build_info(write_file, cancellation_token)?
            },
        ));
    }

    if options.no_emit_on_error != Some(true) {
        return Ok(None);
    }
    let mut diagnostics: Vec<Gc<Diagnostic>> = [
        program
            .get_options_diagnostics(cancellation_token.clone())
            .into(),
        program.get_syntactic_diagnostics(source_file, cancellation_token.clone()),
        program
            .get_global_diagnostics(cancellation_token.clone())?
            .into(),
        program.get_semantic_diagnostics(source_file, cancellation_token.clone())?,
    ]
    .concat();

    if diagnostics.is_empty() && get_emit_declarations(&program.get_compiler_options()) {
        diagnostics = program.get_declaration_diagnostics(None, cancellation_token.clone())?;
    }

    if diagnostics.is_empty() {
        return Ok(None);
    }
    let mut emitted_files: Option<Vec<String>> = None;
    if source_file.is_none()
        && out_file(&options)
            .filter(|out_file| !out_file.is_empty())
            .is_none()
    {
        let emit_result = program.emit_build_info(write_file, cancellation_token)?;
        let EmitResult {
            diagnostics: mut emit_result_diagnostics,
            emitted_files: emit_result_emitted_files,
            ..
        } = emit_result;
        // if (emitResult.diagnostics) {
        diagnostics.append(&mut emit_result_diagnostics);
        // }
        emitted_files = emit_result_emitted_files;
    }
    Ok(Some(EmitResult {
        emit_skipped: true,
        diagnostics,
        emitted_files,
        source_maps: Default::default(),
        exported_modules_from_declaration_emit: Default::default(),
    }))
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
    fn get_current_directory(&self) -> io::Result<String>;
    fn file_exists(&self, file_name: &str) -> bool;
    fn read_file(&self, file_name: &str) -> io::Result<Option<String>>;
    fn read_directory(
        &self,
        _root_dir: &str,
        _extensions: &[&str],
        _excludes: Option<&[String]>,
        _includes: &[String],
        _depth: Option<usize>,
    ) -> Option<io::Result<Vec<String>>> {
        None
    }
    fn trace(&self, _s: &str) {}
    fn is_trace_supported(&self) -> bool;
    fn on_un_recoverable_config_file_diagnostic(&self, _diagnostic: Gc<Diagnostic>) {}

    // These exist to allow "forwarding" CompilerHost -> CompilerHostLike -> DirectoryStructureHost
    fn is_read_directory_implemented(&self) -> bool;
    fn realpath(&self, path: &str) -> Option<String>;
    fn create_directory(&self, path: &str) -> io::Result<()>;
    fn write_file(
        &self,
        path: &str,
        data: &str,
        write_byte_order_mark: Option<bool>,
    ) -> io::Result<()>;
    fn directory_exists(&self, path: &str) -> Option<bool>;
    fn get_directories(&self, path: &str) -> Option<Vec<String>>;
}

#[derive(Trace, Finalize)]
pub struct CompilerHostLikeRcDynCompilerHost {
    host: Gc<Box<dyn CompilerHost>>,
}

impl CompilerHostLikeRcDynCompilerHost {
    pub fn new(host: Gc<Box<dyn CompilerHost>>) -> Self {
        host.into()
    }
}

impl CompilerHostLike for CompilerHostLikeRcDynCompilerHost {
    fn use_case_sensitive_file_names(&self) -> bool {
        CompilerHost::use_case_sensitive_file_names(&**self.host)
    }

    fn get_current_directory(&self) -> io::Result<String> {
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
    ) -> Option<io::Result<Vec<String>>> {
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

    fn create_directory(&self, path: &str) -> io::Result<()> {
        self.host.create_directory(path)
    }

    fn write_file(
        &self,
        path: &str,
        data: &str,
        write_byte_order_mark: Option<bool>,
    ) -> io::Result<()> {
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

impl From<Gc<Box<dyn CompilerHost>>> for CompilerHostLikeRcDynCompilerHost {
    fn from(host: Gc<Box<dyn CompilerHost>>) -> Self {
        Self { host }
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

    fn read_file(&self, path: &str, _encoding: Option<&str>) -> io::Result<Option<String>> {
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
    ) -> Option<io::Result<Vec<String>>> {
        self.host
            .read_directory(path, extensions, exclude, include.unwrap(), depth)
    }

    fn is_read_directory_implemented(&self) -> bool {
        self.host.is_read_directory_implemented()
    }

    fn realpath(&self, path: &str) -> Option<String> {
        self.host.realpath(path)
    }

    fn create_directory(&self, path: &str) -> io::Result<()> {
        self.host.create_directory(path)
    }

    fn write_file(
        &self,
        path: &str,
        data: &str,
        write_byte_order_mark: Option<bool>,
    ) -> io::Result<()> {
        self.host.write_file(path, data, write_byte_order_mark)
    }
}

pub fn parse_config_host_from_compiler_host_like(
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
    fn get_current_directory(&self) -> io::Result<String> {
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
    ) -> io::Result<Vec<String>> {
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

pub(crate) fn create_prepend_nodes(
    project_references: Option<&[Rc<ProjectReference>]>,
    mut get_command_line: impl FnMut(&ProjectReference, usize) -> Option<Gc<ParsedCommandLine>>,
    read_file: Gc<Box<dyn ReadFileCallback>>,
) -> Vec<Id<Node /*InputFiles*/>> {
    if project_references.is_none() {
        return vec![];
    }
    let project_references = project_references.unwrap();
    let mut nodes: Vec<Id<Node /*InputFiles*/>> = vec![];
    for (i, ref_) in project_references.into_iter().enumerate() {
        let resolved_ref_opts = get_command_line(ref_, i);
        if ref_.prepend == Some(true) {
            if let Some(resolved_ref_opts) = resolved_ref_opts
            /*&& resolvedRefOpts.options*/
            {
                let out = out_file(&resolved_ref_opts.options);
                if out.filter(|out| !out.is_empty()).is_none() {
                    continue;
                }

                let EmitFileNames {
                    js_file_path,
                    source_map_file_path,
                    declaration_file_path,
                    declaration_map_path,
                    build_info_path,
                } = get_output_paths_for_bundle(&resolved_ref_opts.options, true);
                let node = create_input_files(
                    read_file.clone(),
                    js_file_path.clone().unwrap(),
                    source_map_file_path.clone(),
                    declaration_file_path.clone(),
                    declaration_map_path.clone(),
                    build_info_path.clone(),
                    None,
                    None,
                    None,
                    None,
                    None,
                );
                nodes.push(node);
            }
        }
    }
    nodes
}

pub fn resolve_project_reference_path(ref_: &ProjectReference) -> String /*ResolvedConfigFileName*/
{
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

pub(super) fn need_jsx(options: &CompilerOptions) -> Option<&'static DiagnosticMessage> {
    if options.jsx.is_some() {
        None
    } else {
        Some(&Diagnostics::Module_0_was_resolved_to_1_but_jsx_is_not_set)
    }
}

pub(super) fn need_allow_js(options: &CompilerOptions) -> Option<&'static DiagnosticMessage> {
    if get_allow_js_compiler_option(options) || !get_strict_option_value(options, "noImplicitAny") {
        None
    } else {
        Some(&Diagnostics::Could_not_find_a_declaration_file_for_module_0_1_implicitly_has_an_any_type)
    }
}

pub(super) fn need_resolve_json_module(
    options: &CompilerOptions,
) -> Option<&'static DiagnosticMessage> {
    if matches!(options.resolve_json_module, Some(true)) {
        None
    } else {
        Some(&Diagnostics::Module_0_was_resolved_to_1_but_resolveJsonModule_is_not_used)
    }
}

pub(super) fn get_module_names(file: Id<Node> /*SourceFile*/) -> Vec<String> {
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

pub(crate) fn get_module_name_string_literal_at(
    file: &impl SourceFileImportsList,
    index: usize,
) -> Id<Node /*StringLiteralLike*/> {
    let imports = file.imports();
    let module_augmentations = file.module_augmentations();
    if index < imports.len() {
        return imports[index].clone();
    }
    let mut aug_index = imports.len();
    for aug in &*module_augmentations {
        if aug.kind() == SyntaxKind::StringLiteral {
            if index == aug_index {
                return aug.clone();
            }
            aug_index += 1;
        }
    }
    Debug_.fail(Some(
        "should never ask for module name at index higher than possible module name",
    ));
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
