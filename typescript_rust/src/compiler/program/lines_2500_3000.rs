use regex::Regex;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

use super::{
    for_each_resolved_project_reference, get_implied_node_format_for_file, is_referenced_file,
    resolve_tripleslash_reference, FilesByNameValue,
};
use crate::{
    append, change_extension, combine_paths, file_extension_is, flatten, for_each, for_each_bool,
    for_each_child_returns, get_common_source_directory_of_config, get_emit_script_target,
    get_external_module_name, get_normalized_absolute_path_without_root,
    get_output_declaration_file_name, get_text_of_identifier_or_literal, has_extension,
    has_js_file_extension, has_jsdoc_nodes, has_syntactic_modifier, is_ambient_module,
    is_any_import_or_re_export, is_declaration_file_name, is_external_module_name_relative,
    is_import_call, is_literal_import_type_node, is_module_declaration, is_require_call,
    is_string_literal, is_string_literal_like, maybe_for_each, maybe_map, node_modules_path_part,
    out_file, package_id_to_string, resolve_module_name, set_parent_recursive,
    set_resolved_type_reference_directive, starts_with, string_contains, to_file_name_lower_case,
    CompilerHost, CompilerOptionsBuilder, DiagnosticMessage, Diagnostics, Extension,
    FileIncludeKind, FileIncludeReason, FileReference, LiteralLikeNodeInterface, ModifierFlags,
    ModuleResolutionKind, Node, NodeArray, NodeInterface, PackageId, Path, Program,
    ReadonlyTextRange, ReferencedFile, ResolvedProjectReference, ResolvedTypeReferenceDirective,
    ScriptReferenceHost, SourceFileLike, SourceOfProjectReferenceRedirect, SyntaxKind,
};

impl Program {
    pub(super) fn collect_module_references(
        &self,
        imports: &mut Option<Vec<Rc<Node>>>,
        file: &Node,
        is_external_module_file: bool,
        module_augmentations: &mut Option<Vec<Rc<Node>>>,
        ambient_modules: &mut Option<Vec<String>>,
        node: &Node, /*Statement*/
        in_ambient_module: bool,
    ) {
        if is_any_import_or_re_export(node) {
            let module_name_expr = get_external_module_name(node);
            if let Some(module_name_expr) = module_name_expr.as_ref().filter(|module_name_expr| {
                is_string_literal(module_name_expr) && {
                    let module_name_text = module_name_expr.as_string_literal().text();
                    !module_name_text.is_empty()
                        && (!in_ambient_module
                            || !is_external_module_name_relative(&module_name_text))
                }
            }) {
                set_parent_recursive(Some(node), false);
                append(
                    imports.get_or_insert_with(|| vec![]),
                    Some(module_name_expr.clone()),
                );
                if !self.uses_uri_style_node_core_modules()
                    && self.current_node_modules_depth() == 0
                    && !file.as_source_file().is_declaration_file()
                {
                    self.set_uses_uri_style_node_core_modules(starts_with(
                        &module_name_expr.as_string_literal().text(),
                        "node:",
                    ));
                }
            }
        } else if is_module_declaration(node) {
            if is_ambient_module(node)
                && (in_ambient_module
                    || has_syntactic_modifier(node, ModifierFlags::Ambient)
                    || file.as_source_file().is_declaration_file())
            {
                let node_name = node.as_named_declaration().name();
                node_name.set_parent(node.node_wrapper());
                let name_text = get_text_of_identifier_or_literal(&node_name);
                if is_external_module_file
                    || (in_ambient_module && !is_external_module_name_relative(&name_text))
                {
                    module_augmentations
                        .get_or_insert_with(|| vec![])
                        .push(node_name);
                } else if !in_ambient_module {
                    if file.as_source_file().is_declaration_file() {
                        ambient_modules
                            .get_or_insert_with(|| vec![])
                            .push(name_text);
                    }
                    let body = node.as_module_declaration().body.as_ref();
                    if let Some(body) = body {
                        for statement in &body.as_module_block().statements {
                            self.collect_module_references(
                                imports,
                                file,
                                is_external_module_file,
                                module_augmentations,
                                ambient_modules,
                                statement,
                                true,
                            );
                        }
                    }
                }
            }
        }
    }

    pub(super) fn collect_dynamic_import_or_require_calls(
        &self,
        is_java_script_file: bool,
        imports: &mut Option<Vec<Rc<Node>>>,
        file: &Node, /*SourceFile*/
    ) {
        lazy_static! {
            static ref r: Regex = Regex::new(r"import|require").unwrap();
        }
        for match_ in r.find_iter(&file.as_source_file().text()) {
            let ref node = self.get_node_at_position(
                is_java_script_file,
                file,
                // TODO: I think this needs to use "char count" rather than "byte count" somehow?
                match_.start().try_into().unwrap(),
            );
            if is_java_script_file && is_require_call(node, true) {
                set_parent_recursive(Some(&**node), false);
                if let Some(node_arguments_0) = node.as_call_expression().arguments.get(0).cloned()
                {
                    append(
                        imports.get_or_insert_with(|| vec![]),
                        Some(node_arguments_0),
                    );
                }
            } else if is_import_call(node) && {
                let node_arguments = &node.as_call_expression().arguments;
                node_arguments.len() >= 1 && is_string_literal_like(&node_arguments[0])
            } {
                set_parent_recursive(Some(&**node), false);
                if let Some(node_arguments_0) = node.as_call_expression().arguments.get(0).cloned()
                {
                    append(
                        imports.get_or_insert_with(|| vec![]),
                        Some(node_arguments_0),
                    );
                }
            } else if is_literal_import_type_node(node) {
                set_parent_recursive(Some(&**node), false);
                append(
                    imports.get_or_insert_with(|| vec![]),
                    Some(
                        node.as_import_type_node()
                            .argument
                            .as_literal_type_node()
                            .literal
                            .clone(),
                    ),
                );
            }
        }
    }

    pub(super) fn get_node_at_position(
        &self,
        is_java_script_file: bool,
        source_file: &Node, /*SourceFile*/
        position: isize,
    ) -> Rc<Node> {
        let mut current = source_file.node_wrapper();
        loop {
            let child = if is_java_script_file && has_jsdoc_nodes(&current) {
                maybe_for_each(current.maybe_js_doc().as_ref(), |child: &Rc<Node>, _| {
                    self.get_containing_child(position, child)
                })
            } else {
                None
            }
            .or_else(|| {
                for_each_child_returns(
                    &current,
                    |child: &Node| self.get_containing_child(position, child),
                    Option::<fn(&NodeArray) -> Option<Rc<Node>>>::None,
                )
            });
            if child.is_none() {
                return current;
            }
            current = child.unwrap();
        }
    }

    pub(super) fn get_containing_child(&self, position: isize, child: &Node) -> Option<Rc<Node>> {
        if child.pos() <= position
            && (position < child.end()
                || position == child.end() && child.kind() == SyntaxKind::EndOfFileToken)
        {
            return Some(child.node_wrapper());
        }
        None
    }

    pub(super) fn get_source_file_from_reference_worker<
        TGetSourceFile: FnMut(&str) -> Option<Rc<Node>>,
        TFail: FnMut(&'static DiagnosticMessage, Option<Vec<String>>),
    >(
        &self,
        file_name: &str,
        mut get_source_file: TGetSourceFile,
        mut fail: Option<TFail>,
        reason: Option<&FileIncludeReason>,
    ) -> Option<Rc<Node>> {
        if has_extension(file_name) {
            let canonical_file_name = self.host().get_canonical_file_name(file_name);
            if self.options.allow_non_ts_extensions != Some(true)
                && !for_each_bool(
                    &flatten(&self.supported_extensions_with_json_if_resolve_json_module()),
                    |extension: &Extension, _| {
                        file_extension_is(&canonical_file_name, extension.to_str())
                    },
                )
            {
                if let Some(fail) = fail.as_mut() {
                    if has_js_file_extension(&canonical_file_name) {
                        fail(&Diagnostics::File_0_is_a_JavaScript_file_Did_you_mean_to_enable_the_allowJs_option, Some(vec![
                            file_name.to_owned(),
                        ]));
                    } else {
                        fail(&Diagnostics::File_0_has_an_unsupported_extension_The_only_supported_extensions_are_1, Some(vec![
                            file_name.to_owned(),
                            format!("'{}'", flatten(&self.supported_extensions()).iter().map(Extension::to_str).collect::<Vec<_>>().join("', '"))
                        ]));
                    }
                }
                return None;
            }

            let source_file = get_source_file(file_name);
            if let Some(fail) = fail.as_mut() {
                if source_file.is_none() {
                    let redirect = self.get_project_reference_redirect_(file_name);
                    if let Some(redirect) = redirect {
                        fail(
                            &Diagnostics::Output_file_0_has_not_been_built_from_source_file_1,
                            Some(vec![redirect, file_name.to_owned()]),
                        );
                    } else {
                        fail(
                            &Diagnostics::File_0_not_found,
                            Some(vec![file_name.to_owned()]),
                        );
                    }
                } else if is_referenced_file(reason)
                    && canonical_file_name
                        == self.host().get_canonical_file_name(
                            &self
                                .get_source_file_by_path(&reason.unwrap().as_referenced_file().file)
                                .unwrap()
                                .as_source_file()
                                .file_name(),
                        )
                {
                    fail(&Diagnostics::A_file_cannot_have_a_reference_to_itself, None);
                }
            }
            source_file
        } else {
            let source_file_no_extension = if self.options.allow_non_ts_extensions == Some(true) {
                get_source_file(file_name)
            } else {
                None
            };
            if source_file_no_extension.is_some() {
                return source_file_no_extension;
            }

            if let Some(fail) = fail.as_mut() {
                if self.options.allow_non_ts_extensions == Some(true) {
                    fail(
                        &Diagnostics::File_0_not_found,
                        Some(vec![file_name.to_owned()]),
                    );
                    return None;
                }
            }

            let source_file_with_added_extension = for_each(
                &self.supported_extensions()[0],
                |extension: &Extension, _| {
                    get_source_file(&format!("{}{}", file_name, extension.to_str()))
                },
            );
            if let Some(fail) = fail.as_mut() {
                if source_file_with_added_extension.is_none() {
                    fail(
                        &Diagnostics::Could_not_resolve_the_path_0_with_the_extensions_Colon_1,
                        Some(vec![
                            file_name.to_owned(),
                            format!(
                                "'{}'",
                                flatten(&self.supported_extensions())
                                    .iter()
                                    .map(Extension::to_str)
                                    .collect::<Vec<_>>()
                                    .join("', '")
                            ),
                        ]),
                    );
                }
            }
            source_file_with_added_extension
        }
    }

    pub fn process_source_file(
        &self,
        file_name: &str,
        is_default_lib: bool,
        ignore_no_default_lib: bool,
        package_id: Option<&PackageId>,
        reason: &FileIncludeReason,
    ) {
        self.get_source_file_from_reference_worker(
            file_name,
            |file_name| {
                self.find_source_file(
                    file_name,
                    is_default_lib,
                    ignore_no_default_lib,
                    reason,
                    package_id,
                )
            },
            Some(
                |diagnostic: &'static DiagnosticMessage, args: Option<Vec<String>>| {
                    self.add_file_preprocessing_file_explaining_diagnostic(
                        Option::<&Node>::None,
                        reason,
                        diagnostic,
                        args,
                    )
                },
            ),
            Some(reason),
        );
    }

    pub fn report_file_names_differ_only_in_casing_error(
        &self,
        file_name: &str,
        existing_file: &Node, /*SourceFile*/
        reason: &FileIncludeReason,
    ) {
        unimplemented!()
    }

    pub fn create_redirect_source_file(
        &self,
        redirect_target: &Node, /*SourceFile*/
        unredirected: &Node,    /*SourceFile*/
        file_name: &str,
        path: &Path,
        resolved_path: &Path,
        original_file_name: &str,
    ) -> Rc<Node /*SourceFile*/> {
        unimplemented!()
    }

    pub fn find_source_file(
        &self,
        file_name: &str,
        is_default_lib: bool,
        ignore_no_default_lib: bool,
        reason: &FileIncludeReason,
        package_id: Option<&PackageId>,
    ) -> Option<Rc<Node>> {
        // tracing?.push(tracing.Phase.Program, "findSourceFile", {
        //     fileName,
        //     isDefaultLib: isDefaultLib || undefined,
        //     fileIncludeKind: (FileIncludeKind as any)[reason.kind],
        // });
        let result = self.find_source_file_worker(
            file_name,
            is_default_lib,
            ignore_no_default_lib,
            reason,
            package_id,
        );
        // tracing?.pop();
        result
    }

    pub fn find_source_file_worker(
        &self,
        file_name: &str,
        is_default_lib: bool,
        ignore_no_default_lib: bool,
        reason: &FileIncludeReason,
        package_id: Option<&PackageId>,
    ) -> Option<Rc<Node>> {
        let path = self.to_path(file_name);
        if self.use_source_of_project_reference_redirect() {
            let mut source = self.get_source_of_project_reference_redirect(&path);
            if source.is_none()
                && self.host().is_realpath_supported()
                && self.options.preserve_symlinks == Some(true)
                && is_declaration_file_name(file_name)
                && string_contains(file_name, node_modules_path_part)
            {
                let real_path = self.to_path(&self.host().realpath(file_name).unwrap());
                if real_path != path {
                    source = self.get_source_of_project_reference_redirect(&real_path);
                }
            }
            if let Some(source) = source.as_ref() {
                let file = match source {
                    SourceOfProjectReferenceRedirect::String(source) => self.find_source_file(
                        source,
                        is_default_lib,
                        ignore_no_default_lib,
                        reason,
                        package_id,
                    ),
                    _ => None,
                };
                if let Some(file) = file.as_ref() {
                    self.add_file_to_files_by_name(Some(&**file), &path, None);
                    return Some(file.clone());
                }
            }
        }
        let original_file_name = file_name;
        let mut file_name = file_name.to_owned();
        if self.files_by_name().contains_key(&*path) {
            let file = self.files_by_name().get(&*path).unwrap().clone();
            let file = match file {
                FilesByNameValue::SourceFile(file) => Some(file),
                _ => None,
            };
            self.add_file_include_reason(file.as_deref(), reason);
            if let Some(file) = file.as_ref() {
                if self.options.force_consistent_casing_in_file_names == Some(true) {
                    let ref checked_name = file.as_source_file().file_name();
                    let is_redirect = self.to_path(checked_name) != self.to_path(&file_name);
                    if is_redirect {
                        file_name = self
                            .get_project_reference_redirect_(&file_name)
                            .unwrap_or(file_name);
                    }
                    let checked_absolute_path = get_normalized_absolute_path_without_root(
                        checked_name,
                        Some(&**self.current_directory()),
                    );
                    let input_absolute_path = get_normalized_absolute_path_without_root(
                        &file_name,
                        Some(&**self.current_directory()),
                    );
                    if checked_absolute_path != input_absolute_path {
                        self.report_file_names_differ_only_in_casing_error(
                            &file_name, file, reason,
                        );
                    }
                }
            }

            if let Some(file) = file.as_ref().filter(|file| {
                matches!(
                    self.source_files_found_searching_node_modules()
                        .get(&**file.as_source_file().path())
                        .cloned(),
                    Some(true)
                ) && self.current_node_modules_depth() == 0
            }) {
                self.source_files_found_searching_node_modules()
                    .insert(file.as_source_file().path().to_string(), false);
                if self.options.no_resolve != Some(true) {
                    self.process_referenced_files(file, is_default_lib);
                    self.process_type_reference_directives(file);
                }
                if self.options.no_lib != Some(true) {
                    self.process_lib_reference_directives(file);
                }

                self.modules_with_elided_imports()
                    .insert(file.as_source_file().path().to_string(), false);
                self.process_imported_modules(file);
            } else if let Some(file) = file.as_ref().filter(|file| {
                matches!(
                    self.modules_with_elided_imports()
                        .get(&**file.as_source_file().path())
                        .cloned(),
                    Some(true)
                )
            }) {
                if self.current_node_modules_depth() < self.max_node_module_js_depth {
                    self.modules_with_elided_imports()
                        .insert(file.as_source_file().path().to_string(), false);
                    self.process_imported_modules(file);
                }
            }

            return file;
        }

        let mut redirected_path: Option<Path> = None;
        if is_referenced_file(Some(reason)) && !self.use_source_of_project_reference_redirect() {
            let redirect_project = self.get_project_reference_redirect_project(&file_name);
            if let Some(redirect_project) = redirect_project.as_ref() {
                if matches!(
                    out_file(&redirect_project.command_line.options),
                    Some(out_file) if !out_file.is_empty()
                ) {
                    return None;
                }
                let redirect = self.get_project_reference_output_name(redirect_project, &file_name);
                file_name = redirect.clone();
                redirected_path = Some(self.to_path(&redirect));
            }
        }

        let file = self.host().get_source_file(
            &file_name,
            get_emit_script_target(&self.options),
            // TODO: this is wrong
            Some(&mut |host_error_message| {
                self.add_file_preprocessing_file_explaining_diagnostic(
                    Option::<&Node>::None,
                    reason,
                    &Diagnostics::Cannot_read_file_0_Colon_1,
                    Some(vec![file_name.clone(), host_error_message.to_owned()]),
                );
            }),
            Some(self.should_create_new_source_file()),
        );

        if let Some(package_id) = package_id {
            let package_id_key = package_id_to_string(package_id);
            let file_from_package_id = self
                .package_id_to_source_file()
                .get(&package_id_key)
                .cloned();
            if let Some(file_from_package_id) = file_from_package_id.as_ref() {
                let dup_file = self.create_redirect_source_file(
                    file_from_package_id,
                    file.as_ref().unwrap(),
                    &file_name,
                    &path,
                    &self.to_path(&file_name),
                    original_file_name,
                );
                self.redirect_targets_map().add(
                    file_from_package_id.as_source_file().path().clone(),
                    file_name.clone(),
                );
                self.add_file_to_files_by_name(Some(&*dup_file), &path, redirected_path.as_ref());
                self.add_file_include_reason(Some(&*dup_file), reason);
                self.source_file_to_package_name()
                    .insert(path.clone(), package_id.name.clone());
                self.processing_other_files
                    .borrow_mut()
                    .as_mut()
                    .unwrap()
                    .push(dup_file.clone());
                return Some(dup_file);
            } else if let Some(file) = file.as_ref() {
                self.package_id_to_source_file()
                    .insert(package_id_key, file.clone());
                self.source_file_to_package_name()
                    .insert(path.clone(), package_id.name.clone());
            }
        }
        self.add_file_to_files_by_name(file.as_deref(), &path, redirected_path.as_ref());

        if let Some(file) = file.as_ref() {
            self.source_files_found_searching_node_modules()
                .insert(path.to_string(), self.current_node_modules_depth() > 0);
            let file_as_source_file = file.as_source_file();
            file_as_source_file.set_file_name(file_name.clone());
            file_as_source_file.set_path(path.clone());
            file_as_source_file.set_resolved_path(Some(self.to_path(&file_name)));
            file_as_source_file.set_original_file_name(Some(original_file_name.to_owned()));
            file_as_source_file.set_implied_node_format(get_implied_node_format_for_file(
                file_as_source_file.maybe_resolved_path().as_ref().unwrap(),
                self.maybe_module_resolution_cache()
                    .as_ref()
                    .map(|module_resolution_cache| {
                        module_resolution_cache.get_package_json_info_cache()
                    })
                    .as_deref(),
                self.host().as_dyn_module_resolution_host(),
                &self.options,
            ));
            self.add_file_include_reason(Some(&**file), reason);

            if CompilerHost::use_case_sensitive_file_names(&*self.host()) {
                let path_lower_case = to_file_name_lower_case(&path);
                let existing_file = self
                    .files_by_name_ignore_case()
                    .get(&path_lower_case)
                    .cloned();
                if let Some(existing_file) = existing_file.as_ref() {
                    self.report_file_names_differ_only_in_casing_error(
                        &file_name,
                        existing_file,
                        reason,
                    );
                } else {
                    self.files_by_name_ignore_case()
                        .insert(path_lower_case, file.clone());
                }
            }

            self.set_skip_default_lib(Some(
                self.maybe_skip_default_lib() == Some(true)
                    || file_as_source_file.has_no_default_lib() && !ignore_no_default_lib,
            ));

            if self.options.no_resolve != Some(true) {
                self.process_referenced_files(file, is_default_lib);
                self.process_type_reference_directives(file);
            }
            if self.options.no_lib != Some(true) {
                self.process_lib_reference_directives(file);
            }

            self.process_imported_modules(file);

            if is_default_lib {
                self.processing_default_lib_files
                    .borrow_mut()
                    .as_mut()
                    .unwrap()
                    .push(file.clone());
            } else {
                self.processing_other_files
                    .borrow_mut()
                    .as_mut()
                    .unwrap()
                    .push(file.clone());
            }
        }
        file
    }

    pub(super) fn add_file_include_reason<TFile: Borrow<Node>>(
        &self,
        file: Option<TFile /*SourceFile*/>,
        reason: &FileIncludeReason,
    ) {
        if let Some(file) = file {
            let file = file.borrow();
            self.file_reasons()
                .add(file.as_source_file().path().clone(), reason.clone());
        }
    }

    pub(super) fn add_file_to_files_by_name<TFile: Borrow<Node>>(
        &self,
        file: Option<TFile /*SourceFile*/>,
        path: &Path,
        redirected_path: Option<&Path>,
    ) {
        let file = file.map(|file| file.borrow().node_wrapper());
        if let Some(redirected_path) = redirected_path {
            self.files_by_name().insert(
                redirected_path.to_string(),
                match file.as_ref() {
                    None => FilesByNameValue::Undefined,
                    Some(file) => FilesByNameValue::SourceFile(file.clone()),
                },
            );
            self.files_by_name().insert(
                path.to_string(),
                match file.as_ref() {
                    None => FilesByNameValue::False,
                    Some(file) => FilesByNameValue::SourceFile(file.clone()),
                },
            );
        } else {
            self.files_by_name().insert(
                path.to_string(),
                match file.as_ref() {
                    None => FilesByNameValue::Undefined,
                    Some(file) => FilesByNameValue::SourceFile(file.clone()),
                },
            );
        }
    }

    pub fn get_project_reference_redirect_(&self, file_name: &str) -> Option<String> {
        unimplemented!()
    }

    pub fn get_project_reference_redirect_project(
        &self,
        file_name: &str,
    ) -> Option<Rc<ResolvedProjectReference>> {
        if match self.maybe_resolved_project_references().as_ref() {
            None => true,
            Some(resolved_project_references) => resolved_project_references.is_empty(),
        } || file_extension_is(file_name, Extension::Dts.to_str())
            || file_extension_is(file_name, Extension::Json.to_str())
        {
            return None;
        }

        self.get_resolved_project_reference_to_redirect(file_name)
    }

    pub fn get_project_reference_output_name(
        &self,
        referenced_project: &ResolvedProjectReference,
        file_name: &str,
    ) -> String {
        unimplemented!()
    }

    pub fn get_resolved_project_reference_to_redirect(
        &self,
        file_name: &str,
    ) -> Option<Rc<ResolvedProjectReference>> {
        let mut map_from_file_to_project_reference_redirects =
            self.maybe_map_from_file_to_project_reference_redirects();
        let map_from_file_to_project_reference_redirects =
            map_from_file_to_project_reference_redirects.get_or_insert_with(|| {
                let mut map_from_file_to_project_reference_redirects = HashMap::new();
                self.for_each_resolved_project_reference(
                    |referenced_project: Rc<ResolvedProjectReference>| -> Option<()> {
                        let referenced_project_source_file_path =
                            referenced_project.source_file.as_source_file().path();
                        if &self.to_path(self.options.config_file_path.as_ref().unwrap())
                            != &*referenced_project_source_file_path
                        {
                            referenced_project
                                .command_line
                                .file_names
                                .iter()
                                .for_each(|f| {
                                    map_from_file_to_project_reference_redirects.insert(
                                        self.to_path(f),
                                        referenced_project_source_file_path.clone(),
                                    );
                                });
                        }
                        None
                    },
                );
                map_from_file_to_project_reference_redirects
            });

        let referenced_project_path =
            map_from_file_to_project_reference_redirects.get(&self.to_path(file_name));
        referenced_project_path.and_then(|referenced_project_path| {
            self.get_resolved_project_reference_by_path(referenced_project_path)
        })
    }

    pub fn for_each_resolved_project_reference<
        TReturn,
        TCallback: FnMut(Rc<ResolvedProjectReference>) -> Option<TReturn>,
    >(
        &self,
        mut cb: TCallback,
    ) -> Option<TReturn> {
        for_each_resolved_project_reference(
            self.maybe_resolved_project_references().as_deref(),
            |resolved_project_reference, _parent| cb(resolved_project_reference),
        )
    }

    pub fn for_each_resolved_project_reference_rc(
        &self,
    ) -> Rc<dyn Fn(&mut dyn FnMut(Rc<ResolvedProjectReference>))> {
        let self_clone = self.rc_wrapper();
        Rc::new(move |cb| {
            for_each_resolved_project_reference(
                self_clone.maybe_resolved_project_references().as_deref(),
                |resolved_project_reference, _parent| -> Option<()> {
                    cb(resolved_project_reference);
                    None
                },
            );
        })
    }

    pub(super) fn get_source_of_project_reference_redirect(
        &self,
        path: &Path,
    ) -> Option<SourceOfProjectReferenceRedirect> {
        if !is_declaration_file_name(path) {
            return None;
        }
        self.maybe_map_from_to_project_reference_redirect_source()
            .get_or_insert_with(|| {
                let mut map_from_to_project_reference_redirect_source = HashMap::new();
                self.for_each_resolved_project_reference(
                    |resolved_ref: Rc<ResolvedProjectReference>| -> Option<()> {
                        let out = out_file(&resolved_ref.command_line.options);
                        if let Some(out) = out {
                            let output_dts = change_extension(out, Extension::Dts.to_str());
                            map_from_to_project_reference_redirect_source.insert(
                                self.to_path(&output_dts),
                                SourceOfProjectReferenceRedirect::True,
                            );
                        } else {
                            let mut got_common_source_directory: Option<String> = None;
                            let mut get_common_source_directory = || {
                                if let Some(got_common_source_directory) =
                                    got_common_source_directory.as_ref()
                                {
                                    return got_common_source_directory.clone();
                                }
                                let got_common_source_directory =
                                    Some(get_common_source_directory_of_config(
                                        &resolved_ref.command_line,
                                        !CompilerHost::use_case_sensitive_file_names(&*self.host()),
                                    ));
                                got_common_source_directory.clone().unwrap()
                            };
                            for_each(
                                &resolved_ref.command_line.file_names,
                                |file_name: &String, _| -> Option<()> {
                                    if !file_extension_is(file_name, Extension::Dts.to_str())
                                        && !file_extension_is(file_name, Extension::Json.to_str())
                                    {
                                        let output_dts = get_output_declaration_file_name(
                                            file_name,
                                            &resolved_ref.command_line,
                                            !CompilerHost::use_case_sensitive_file_names(
                                                &*self.host(),
                                            ),
                                            Some(&mut get_common_source_directory),
                                        );
                                        map_from_to_project_reference_redirect_source.insert(
                                            self.to_path(&output_dts),
                                            file_name.clone().into(),
                                        );
                                    }
                                    None
                                },
                            );
                        }
                        None
                    },
                );
                map_from_to_project_reference_redirect_source
            })
            .get(path)
            .cloned()
    }

    pub(super) fn is_source_of_project_reference_redirect_(&self, file_name: &str) -> bool {
        self.use_source_of_project_reference_redirect()
            && self
                .get_resolved_project_reference_to_redirect(file_name)
                .is_some()
    }

    pub(super) fn get_resolved_project_reference_by_path(
        &self,
        project_reference_path: &Path,
    ) -> Option<Rc<ResolvedProjectReference>> {
        unimplemented!()
    }

    pub fn process_referenced_files(&self, file: &Node /*SourceFile*/, is_default_lib: bool) {
        let file_as_source_file = file.as_source_file();
        maybe_for_each(
            file_as_source_file.maybe_referenced_files().as_ref(),
            |ref_: &FileReference, index| -> Option<()> {
                self.process_source_file(
                    &resolve_tripleslash_reference(
                        &ref_.file_name,
                        &file_as_source_file.file_name(),
                    ),
                    is_default_lib,
                    false,
                    None,
                    &FileIncludeReason::ReferencedFile(ReferencedFile {
                        kind: FileIncludeKind::ReferenceFile,
                        file: file_as_source_file.path().clone(),
                        index,
                    }),
                );
                None
            },
        );
    }

    pub fn process_type_reference_directives(&self, file: &Node /*SourceFile*/) {
        let file_as_source_file = file.as_source_file();
        let file_type_reference_directives = file_as_source_file.maybe_type_reference_directives();
        let type_directives = maybe_map(
            file_type_reference_directives.as_ref(),
            |ref_: &FileReference, _| to_file_name_lower_case(&ref_.file_name),
        );
        if type_directives.is_none() {
            return;
        }
        let type_directives = type_directives.unwrap();
        let file_type_reference_directives = file_type_reference_directives.as_ref().unwrap();

        let resolutions = self
            .resolve_type_reference_directive_names_worker(&type_directives, file.node_wrapper());
        for index in 0..type_directives.len() {
            let ref_ = &file_type_reference_directives[index];
            let resolved_type_reference_directive = &resolutions[index];
            let file_name = to_file_name_lower_case(&ref_.file_name);
            set_resolved_type_reference_directive(
                file,
                &file_name,
                resolved_type_reference_directive.clone(),
            );
            self.process_type_reference_directive(
                &file_name,
                resolved_type_reference_directive.clone(),
                &FileIncludeReason::ReferencedFile(ReferencedFile {
                    kind: FileIncludeKind::TypeReferenceDirective,
                    file: file_as_source_file.path().clone(),
                    index,
                }),
            );
        }
    }

    pub fn process_type_reference_directive(
        &self,
        type_reference_directive: &str,
        resolved_type_reference_directive: Option<Rc<ResolvedTypeReferenceDirective>>,
        reason: &FileIncludeReason,
    ) {
        // tracing?.push(tracing.Phase.Program, "processTypeReferenceDirective", { directive: typeReferenceDirective, hasResolved: !!resolveModuleNamesReusingOldState, refKind: reason.kind, refPath: isReferencedFile(reason) ? reason.file : undefined });
        self.process_type_reference_directive_worker(
            type_reference_directive,
            resolved_type_reference_directive,
            reason,
        )
        // tracing?.pop();
    }

    pub fn process_type_reference_directive_worker(
        &self,
        type_reference_directive: &str,
        resolved_type_reference_directive: Option<Rc<ResolvedTypeReferenceDirective>>,
        reason: &FileIncludeReason,
    ) {
        let previous_resolution = self
            .resolved_type_reference_directives()
            .get(type_reference_directive)
            .cloned()
            .flatten();
        if matches!(
            previous_resolution.as_ref(),
            Some(previous_resolution) if previous_resolution.primary
        ) {
            return;
        }
        let mut save_resolution = true;
        if let Some(resolved_type_reference_directive) =
            resolved_type_reference_directive.as_deref()
        {
            if resolved_type_reference_directive.is_external_library_import == Some(true) {
                self.set_current_node_modules_depth(self.current_node_modules_depth() + 1);
            }

            if resolved_type_reference_directive.primary {
                self.process_source_file(
                    resolved_type_reference_directive
                        .resolved_file_name
                        .as_ref()
                        .unwrap(),
                    false,
                    false,
                    resolved_type_reference_directive.package_id.as_ref(),
                    reason,
                );
            } else {
                if let Some(previous_resolution) = previous_resolution.as_ref() {
                    if resolved_type_reference_directive.resolved_file_name
                        != previous_resolution.resolved_file_name
                    {
                        let other_file_text = self.host().read_file(
                            resolved_type_reference_directive
                                .resolved_file_name
                                .as_ref()
                                .unwrap(),
                        );
                        let existing_file = self
                            .get_source_file_(
                                previous_resolution.resolved_file_name.as_ref().unwrap(),
                            )
                            .unwrap();
                        if !matches!(
                            other_file_text.as_ref(),
                            Ok(other_file_text) if other_file_text == &*existing_file.as_source_file().text()
                        ) {
                            self.add_file_preprocessing_file_explaining_diagnostic(
                                Some(&*existing_file),
                                reason,
                                &Diagnostics::Conflicting_definitions_for_0_found_at_1_and_2_Consider_installing_a_specific_version_of_this_library_to_resolve_the_conflict,
                                Some(vec![
                                    type_reference_directive.to_owned(),
                                    resolved_type_reference_directive.resolved_file_name.clone().unwrap(),
                                    previous_resolution.resolved_file_name.clone().unwrap(),
                                ])
                            );
                        }
                    }
                    save_resolution = false;
                } else {
                    self.process_source_file(
                        resolved_type_reference_directive
                            .resolved_file_name
                            .as_ref()
                            .unwrap(),
                        false,
                        false,
                        resolved_type_reference_directive.package_id.as_ref(),
                        reason,
                    );
                }
            }

            if resolved_type_reference_directive.is_external_library_import == Some(true) {
                self.set_current_node_modules_depth(self.current_node_modules_depth() - 1);
            }
        } else {
            self.add_file_preprocessing_file_explaining_diagnostic(
                Option::<&Node>::None,
                reason,
                &Diagnostics::Cannot_find_type_definition_file_for_0,
                Some(vec![type_reference_directive.to_owned()]),
            );
        }

        if save_resolution {
            self.resolved_type_reference_directives().insert(
                type_reference_directive.to_owned(),
                resolved_type_reference_directive,
            );
        }
    }

    pub fn path_for_lib_file(&self, lib_file_name: &str) -> String {
        let components = lib_file_name
            .split(".")
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>();
        let mut path = components[1].clone();
        let mut i = 2;
        while let Some(components_i) = components
            .get(i)
            .filter(|components_i| &***components_i != "d")
        {
            path.push_str(&format!(
                "{}{}",
                if i == 2 { "/" } else { "-" },
                components_i
            ));
            i += 1;
        }
        let resolve_from = combine_paths(
            &self.current_directory(),
            &[Some(&*format!(
                "__lib_node_modules_lookup_{}__.ts",
                lib_file_name
            ))],
        );
        let local_override_module_result = resolve_module_name(
            &format!("@typescript/lib-{}", path),
            &resolve_from,
            Rc::new(
                CompilerOptionsBuilder::default()
                    .module_resolution(Some(ModuleResolutionKind::NodeJs))
                    .build()
                    .unwrap(),
            ),
            self.host().as_dyn_module_resolution_host(),
            self.maybe_module_resolution_cache().clone(),
            None,
            None,
        );
        if let Some(local_override_module_result_resolved_module) =
            local_override_module_result.resolved_module.as_ref()
        {
            return local_override_module_result_resolved_module
                .resolved_file_name
                .clone();
        }
        combine_paths(&self.default_library_path(), &[Some(lib_file_name)])
    }
}
