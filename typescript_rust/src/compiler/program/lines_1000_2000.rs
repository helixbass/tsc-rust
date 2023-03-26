use gc::{Finalize, Gc, GcCellRef, Trace};
use std::{cell::RefCell, io, ptr, rc::Rc};

use super::{create_prepend_nodes, filter_semantic_diagnostics, handle_no_emit_options, ToPath};
use crate::{
    compare_values, concatenate, contains, contains_path, create_type_checker, emit_files,
    file_extension_is_one_of, filter, get_base_file_name, get_common_source_directory,
    get_mode_for_resolution_at_index, get_normalized_absolute_path, get_resolved_module,
    get_transformers, is_trace_enabled, libs, map_defined, node_modules_path_part, out_file,
    package_id_to_string, remove_prefix, remove_suffix, skip_type_checking,
    source_file_may_be_emitted, string_contains, to_path as to_path_helper, trace,
    CancellationTokenDebuggable, Comparison, CompilerHost, CompilerOptions, CustomTransformers,
    Debug_, Diagnostic, Diagnostics, EmitHost, EmitResult, Extension, FileIncludeReason,
    FileReference, ModuleSpecifierResolutionHost, MultiMap, Node, Path, Program, ProgramBuildInfo,
    ReadFileCallback, RedirectTargetsMap, ResolvedModuleFull, ResolvedProjectReference,
    ResolvedTypeReferenceDirective, ScriptReferenceHost, SourceFileLike,
    SourceFileMayBeEmittedHost, SourceOfProjectReferenceRedirect, StringOrRcNode,
    StructureIsReused, SymlinkCache, TypeChecker, TypeCheckerHost, WriteFileCallback,
};

impl Program {
    pub(super) fn resolve_module_names_worker(
        &self,
        module_names: &[String],
        containing_file: &Node, /*SourceFile*/
        reused_names: Option<&[String]>,
    ) -> Vec<Option<Gc<ResolvedModuleFull>>> {
        if module_names.is_empty() {
            return vec![];
        }
        let containing_file_as_source_file = containing_file.as_source_file();
        let containing_file_name = get_normalized_absolute_path(
            &containing_file_as_source_file.original_file_name(),
            Some(&self.current_directory()),
        );
        let redirected_reference = self.get_redirect_reference_for_resolution(containing_file);
        // tracing?.push(tracing.Phase.Program, "resolveModuleNamesWorker", { containingFileName });
        // performance.mark("beforeResolveModule");
        let result = self.actual_resolve_module_names_worker().call(
            module_names,
            containing_file,
            &containing_file_name,
            reused_names,
            redirected_reference.clone(),
        );
        // performance.mark("afterResolveModule");
        // performance.measure("ResolveModule", "beforeResolveModule", "afterResolveModule");
        // tracing?.pop();
        result
    }

    pub(super) fn resolve_type_reference_directive_names_worker<
        TContainingFile: Into<StringOrRcNode>,
    >(
        &self,
        type_directive_names: &[String],
        containing_file: TContainingFile,
    ) -> Vec<Option<Gc<ResolvedTypeReferenceDirective>>> {
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
    ) -> Option<Gc<ResolvedProjectReference>> {
        let file_as_source_file = file.as_source_file();
        let redirect = self
            .get_resolved_project_reference_to_redirect(&file_as_source_file.original_file_name());
        if redirect.is_some()
            || !file_extension_is_one_of(
                &file_as_source_file.original_file_name(),
                &[Extension::Dts, Extension::Dcts, Extension::Dmts],
            )
        {
            return redirect;
        }

        let result_from_dts = self.get_redirect_reference_for_resolution_from_source_of_project(
            &file_as_source_file.path(),
        );
        if result_from_dts.is_some() {
            return result_from_dts;
        }

        if !self.host().is_realpath_supported()
            || self.options.preserve_symlinks != Some(true)
            || !string_contains(
                &file_as_source_file.original_file_name(),
                node_modules_path_part,
            )
        {
            return None;
        }
        let real_declaration_path = self.to_path(
            &self
                .host()
                .realpath(&file_as_source_file.original_file_name())
                .unwrap(),
        );
        if &real_declaration_path == &*file_as_source_file.path() {
            None
        } else {
            self.get_redirect_reference_for_resolution_from_source_of_project(
                &real_declaration_path,
            )
        }
    }

    pub(super) fn get_redirect_reference_for_resolution_from_source_of_project(
        &self,
        file_path: &Path,
    ) -> Option<Gc<ResolvedProjectReference>> {
        let source = self.get_source_of_project_reference_redirect(file_path);
        if let Some(SourceOfProjectReferenceRedirect::String(source)) = source.as_ref() {
            return self.get_resolved_project_reference_to_redirect(source);
        }
        if source.is_none() {
            return None;
        }
        self.for_each_resolved_project_reference(|resolved_ref: Gc<ResolvedProjectReference>| {
            let out = out_file(&resolved_ref.command_line.options)?;
            if out.is_empty() {
                return None;
            }
            if &self.to_path(out) == file_path {
                Some(resolved_ref)
            } else {
                None
            }
        })
    }

    pub(super) fn compare_default_lib_files(
        &self,
        a: &Node, /*SourceFile*/
        b: &Node, /*SourceFile*/
    ) -> Comparison {
        compare_values(
            Some(self.get_default_lib_file_priority(a)),
            Some(self.get_default_lib_file_priority(b)),
        )
    }

    pub(super) fn get_default_lib_file_priority(&self, a: &Node /*SourceFile*/) -> usize {
        let a_file_name = a.as_source_file().file_name();
        if contains_path(
            &self.default_library_path(),
            &a_file_name,
            Option::<String>::None,
            Some(false),
        ) {
            let basename = get_base_file_name(&a_file_name, None, None);
            if matches!(&*basename, "lib.d.ts" | "lib.es6.d.ts") {
                return 0;
            }
            let name = remove_suffix(remove_prefix(&basename, "lib."), ".d.ts");
            let index = libs.with(|libs_| libs_.into_iter().position(|lib| *lib == name));
            if let Some(index) = index {
                return index + 1;
            }
        }
        libs.with(|libs_| libs_.len() + 2)
    }

    pub(super) fn has_invalidated_resolution(&self, source_file: &Path) -> bool {
        self.host()
            .has_invalidated_resolution(source_file)
            .unwrap_or(false)
    }

    pub fn get_root_file_names(&self) -> &[String] {
        unimplemented!()
    }

    pub fn get_source_files(&self) -> GcCellRef<Vec<Gc<Node>>> {
        self.files()
    }

    pub fn get_compiler_options(&self) -> Gc<CompilerOptions> {
        self.options.clone()
    }

    pub fn use_case_sensitive_file_names(&self) -> bool {
        unimplemented!()
    }

    pub fn get_file_include_reasons(&self) -> Rc<RefCell<MultiMap<Path, FileIncludeReason>>> {
        self.file_reasons.clone()
    }

    pub fn get_source_file_(&self, file_name: &str) -> Option<Gc<Node /*SourceFile*/>> {
        self.get_source_file_by_path(&self.to_path(file_name))
    }

    pub fn get_syntactic_diagnostics(
        &self,
        source_file: Option<&Node /*SourceFile*/>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>> {
        self.get_diagnostics_helper(
            Program::get_syntactic_diagnostics_for_file,
            cancellation_token,
        )
    }

    pub fn get_semantic_diagnostics(
        &self,
        source_file: Option<&Node /*SourceFile*/>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> Vec<Gc<Diagnostic>> {
        self.get_diagnostics_helper(
            Program::get_semantic_diagnostics_for_file,
            cancellation_token,
        )
    }

    pub fn emit(
        &self,
        source_file: Option<&Node /*SourceFile*/>,
        write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
        emit_only_dts_files: Option<bool>,
        transformers: Option<&CustomTransformers>,
        force_dts_emit: Option<bool>,
    ) -> EmitResult {
        return super::emit_skipped_with_no_diagnostics();
        // tracing?.push(tracing.Phase.Emit, "emit", { path: sourceFile?.path }, /*separateBeginAndEnd*/ true);
        let result = self.run_with_cancellation_token(|| {
            self.emit_worker(
                source_file,
                write_file_callback,
                cancellation_token,
                emit_only_dts_files,
                transformers,
                force_dts_emit,
            )
        });
        // tracing?.pop();
        result
    }

    pub(super) fn is_emit_blocked(&self, emit_file_name: &str) -> bool {
        unimplemented!()
    }

    pub(super) fn emit_worker(
        &self,
        source_file: Option<&Node /*SourceFile*/>,
        write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
        emit_only_dts_files: Option<bool>,
        custom_transformers: Option<&CustomTransformers>,
        force_dts_emit: Option<bool>,
    ) -> EmitResult {
        if force_dts_emit != Some(true) {
            let result = handle_no_emit_options(
                self.rc_wrapper(),
                source_file,
                write_file_callback.clone(),
                cancellation_token.clone(),
            );
            if let Some(result) = result {
                return result;
            }
        }

        let emit_resolver = self
            .get_diagnostics_producing_type_checker()
            .get_emit_resolver(
                if matches!(
                    out_file(&self.options),
                    Some(out_file) if !out_file.is_empty()
                ) {
                    None
                } else {
                    source_file
                },
                cancellation_token,
            );

        // performance.mark("beforeEmit");

        let emit_result = emit_files(
            emit_resolver,
            self.get_emit_host(write_file_callback),
            source_file,
            get_transformers(&self.options, custom_transformers, emit_only_dts_files),
            emit_only_dts_files,
            Some(false),
            force_dts_emit,
        );

        // performance.mark("afterEmit");
        // performance.measure("Emit", "beforeEmit", "afterEmit");
        emit_result
    }

    pub fn get_current_directory(&self) -> String {
        self.current_directory().clone()
    }

    pub fn to_path(&self, file_name: &str) -> Path {
        to_path_helper(file_name, Some(&self.current_directory()), |file_name| {
            self.get_canonical_file_name(file_name)
        })
    }

    pub fn to_path_rc(&self) -> Gc<Box<dyn ToPath>> {
        Gc::new(Box::new(ProgramToPath::new(self.rc_wrapper())))
    }

    pub fn get_common_source_directory(&self) -> String {
        self.maybe_common_source_directory_mut()
            .get_or_insert_with(|| {
                let emitted_files = filter(&**self.files(), |file: &Gc<Node>| {
                    source_file_may_be_emitted(file, self, None)
                });
                get_common_source_directory(
                    &self.options,
                    || {
                        map_defined(Some(&emitted_files), |file: &Gc<Node>, _| {
                            let file_as_source_file = file.as_source_file();
                            if file_as_source_file.is_declaration_file() {
                                None
                            } else {
                                Some(file_as_source_file.file_name().clone())
                            }
                        })
                    },
                    &self.current_directory(),
                    |file_name: &str| self.get_canonical_file_name(file_name),
                    Some(|common_source_directory: &str| {
                        self.check_source_files_belong_to_path(
                            &emitted_files,
                            common_source_directory,
                        );
                    }),
                )
            })
            .clone()
    }

    pub(super) fn resolve_module_names_reusing_old_state(
        &self,
        module_names: &[String],
        file: &Node, /*SourceFile*/
    ) -> Vec<Option<Gc<ResolvedModuleFull>>> {
        let file_as_source_file = file.as_source_file();
        if self.structure_is_reused() == StructureIsReused::Not
            && file_as_source_file
                .maybe_ambient_module_names()
                .as_ref()
                .unwrap()
                .is_empty()
        {
            return self.resolve_module_names_worker(module_names, file, None);
        }

        let old_source_file = self
            .maybe_old_program()
            .as_ref()
            .and_then(|old_program| old_program.get_source_file_(&file_as_source_file.file_name()));
        if !matches!(
            old_source_file.as_deref(),
            Some(old_source_file) if ptr::eq(
                old_source_file,
                file,
            )
        ) {
            if let Some(file_resolved_modules) =
                file_as_source_file.maybe_resolved_modules().as_ref()
            {
                let mut result: Vec<Option<Gc<ResolvedModuleFull>>> = vec![];
                let mut i = 0;
                for module_name in module_names {
                    let resolved_module = file_resolved_modules
                        .get(
                            module_name,
                            get_mode_for_resolution_at_index(file_as_source_file, i),
                        )
                        .flatten();
                    i += 1;
                    result.push(resolved_module);
                }
                return result;
            }
        }
        let mut unknown_module_names: Option<Vec<String>> = None;
        let mut result: Option<Vec<Option<ResolveModuleNamesReusingOldStateResultItem>>> = None;
        let mut reused_names: Option<Vec<String>> = None;

        for i in 0..module_names.len() {
            let module_name = &module_names[i];
            if let Some(old_source_file) = old_source_file
                .as_ref()
                .filter(|old_source_file| ptr::eq(file, &***old_source_file))
            {
                let old_source_file_as_source_file = old_source_file.as_source_file();
                if !self.has_invalidated_resolution(&old_source_file_as_source_file.path()) {
                    let old_resolved_module = get_resolved_module(
                        Some(&**old_source_file),
                        module_name,
                        get_mode_for_resolution_at_index(old_source_file_as_source_file, i),
                    );
                    if let Some(old_resolved_module) = old_resolved_module.as_ref() {
                        if is_trace_enabled(
                            &self.options,
                            self.host().as_dyn_module_resolution_host(),
                        ) {
                            trace(
                                self.host().as_dyn_module_resolution_host(),
                                if old_resolved_module.package_id.is_some() {
                                    &*Diagnostics::Reusing_resolution_of_module_0_from_1_of_old_program_it_was_successfully_resolved_to_2_with_Package_ID_3
                                } else {
                                    &*Diagnostics::Reusing_resolution_of_module_0_from_1_of_old_program_it_was_successfully_resolved_to_2
                                },
                                if let Some(old_resolved_module_package_id) =
                                    old_resolved_module.package_id.as_ref()
                                {
                                    Some(vec![
                                        module_name.clone(),
                                        get_normalized_absolute_path(
                                            &file_as_source_file.original_file_name(),
                                            Some(&self.current_directory()),
                                        ),
                                        old_resolved_module.resolved_file_name.clone(),
                                        package_id_to_string(old_resolved_module_package_id),
                                    ])
                                } else {
                                    Some(vec![
                                        module_name.clone(),
                                        get_normalized_absolute_path(
                                            &file_as_source_file.original_file_name(),
                                            Some(&self.current_directory()),
                                        ),
                                        old_resolved_module.resolved_file_name.clone(),
                                    ])
                                },
                            );
                        }
                        result.get_or_insert_with(|| vec![None; module_names.len()])[i] = Some(
                            ResolveModuleNamesReusingOldStateResultItem::ResolvedModuleFull(
                                old_resolved_module.clone(),
                            ),
                        );
                        reused_names
                            .get_or_insert_with(|| vec![])
                            .push(module_name.clone());
                        continue;
                    }
                }
            }
            let mut resolves_to_ambient_module_in_non_modified_file = false;
            if contains(
                file_as_source_file.maybe_ambient_module_names().as_deref(),
                module_name,
            ) {
                resolves_to_ambient_module_in_non_modified_file = true;
                if is_trace_enabled(&self.options, self.host().as_dyn_module_resolution_host()) {
                    trace(
                        self.host().as_dyn_module_resolution_host(),
                        &Diagnostics::Module_0_was_resolved_as_locally_declared_ambient_module_in_file_1,
                        Some(vec![
                            module_name.clone(),
                            get_normalized_absolute_path(
                                &file_as_source_file.original_file_name(),
                                Some(&self.current_directory()),
                            )
                        ])
                    );
                }
            } else {
                resolves_to_ambient_module_in_non_modified_file = self
                    .module_name_resolves_to_ambient_module_in_non_modified_file(
                        old_source_file.as_deref(),
                        module_name,
                        i,
                    );
            }

            if resolves_to_ambient_module_in_non_modified_file {
                result.get_or_insert_with(|| vec![None; module_names.len()])[i] = Some(ResolveModuleNamesReusingOldStateResultItem::PredictedToResolveToAmbientModuleMarker);
            } else {
                unknown_module_names
                    .get_or_insert_with(|| vec![])
                    .push(module_name.clone());
            }
        }

        let resolutions = if let Some(unknown_module_names) = unknown_module_names
            .as_ref()
            .filter(|unknown_module_names| !unknown_module_names.is_empty())
        {
            self.resolve_module_names_worker(unknown_module_names, file, reused_names.as_deref())
        } else {
            vec![]
        };

        if result.is_none() {
            Debug_.assert(resolutions.len() == module_names.len(), None);
            return resolutions;
        }
        let result = result.unwrap();

        let mut j = 0;
        let result = result.into_iter().map(|value| {
            match value {
                Some(ResolveModuleNamesReusingOldStateResultItem::PredictedToResolveToAmbientModuleMarker) => None,
                Some(ResolveModuleNamesReusingOldStateResultItem::ResolvedModuleFull(value)) => Some(value),
                None => {
                    let value = resolutions[j].clone();
                    j += 1;
                    value
                }
            }
        }).collect::<Vec<_>>();
        Debug_.assert(j == resolutions.len(), None);

        result
    }

    pub(super) fn module_name_resolves_to_ambient_module_in_non_modified_file(
        &self,
        old_source_file: Option<&Node>,
        module_name: &str,
        index: usize,
    ) -> bool {
        if index
            >= old_source_file
                .and_then(|old_source_file| {
                    old_source_file
                        .as_source_file()
                        .maybe_imports()
                        .as_ref()
                        .map(|old_source_file_imports| old_source_file_imports.len())
                })
                .unwrap_or(0)
                + old_source_file
                    .and_then(|old_source_file| {
                        old_source_file
                            .as_source_file()
                            .maybe_module_augmentations()
                            .as_ref()
                            .map(|old_source_file_module_augmentations| {
                                old_source_file_module_augmentations.len()
                            })
                    })
                    .unwrap_or(0)
        {
            return false;
        }
        let resolution_to_file = get_resolved_module(
            old_source_file,
            module_name,
            old_source_file.and_then(|old_source_file| {
                get_mode_for_resolution_at_index(old_source_file.as_source_file(), index)
            }),
        );
        let resolved_file = resolution_to_file.as_ref().and_then(|resolution_to_file| {
            self.maybe_old_program()
                .unwrap()
                .get_source_file_(&resolution_to_file.resolved_file_name)
        });
        if resolution_to_file.is_some() && resolved_file.is_some() {
            return false;
        }

        let unmodified_file = self
            .ambient_module_name_to_unmodified_file_name()
            .get(module_name)
            .cloned();

        if unmodified_file.is_none() {
            return false;
        }
        let unmodified_file = unmodified_file.unwrap();

        if is_trace_enabled(&self.options, self.host().as_dyn_module_resolution_host()) {
            trace(
                self.host().as_dyn_module_resolution_host(),
                &Diagnostics::Module_0_was_resolved_as_ambient_module_declared_in_1_since_this_file_was_not_modified,
                Some(vec![
                    module_name.to_owned(),
                    unmodified_file,
                ])
            );
        }
        true
    }

    pub fn try_reuse_structure_from_old_program(&self) -> StructureIsReused {
        if self.maybe_old_program().is_none() {
            return StructureIsReused::Not;
        }

        unimplemented!()
    }

    pub(super) fn get_emit_host(
        &self,
        write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
    ) -> Gc<Box<dyn EmitHost>> {
        Gc::new(Box::new(ProgramEmitHost::new(
            self.rc_wrapper(),
            write_file_callback,
        )))
    }

    pub(super) fn emit_build_info(
        &self,
        write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> EmitResult {
        unimplemented!()
    }

    pub fn get_resolved_project_references(
        &self,
    ) -> GcCellRef<Option<Vec<Option<Gc<ResolvedProjectReference>>>>> {
        self.resolved_project_references.borrow()
    }

    pub(super) fn get_prepend_nodes(&self) -> Vec<Gc<Node /*InputFiles*/>> {
        create_prepend_nodes(
            self.maybe_project_references().as_deref(),
            |_ref, index| {
                self.maybe_resolved_project_references()
                    .as_ref()
                    .unwrap()
                    .get(index)
                    .cloned()
                    .flatten()
                    .map(|resolved_project_reference| {
                        resolved_project_reference.command_line.clone()
                    })
            },
            Gc::new(Box::new(GetPrependNodesReadFileCallback::new(
                self.rc_wrapper(),
            ))),
        )
    }

    pub fn is_source_file_from_external_library(&self, file: &Node /*SourceFile*/) -> bool {
        self.source_files_found_searching_node_modules()
            .get(&**file.as_source_file().path())
            .copied()
            == Some(true)
    }

    pub fn is_source_file_default_library(&self, file: &Node /*SourceFile*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_diagnostics_producing_type_checker(&self) -> Gc<TypeChecker> {
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
            *diagnostics_producing_type_checker = Some(create_type_checker(
                self.as_dyn_type_checker_host_debuggable(),
                true,
            ));
        }
        diagnostics_producing_type_checker.as_ref().unwrap().clone()
    }

    pub(super) fn get_diagnostics_helper(
        &self,
        get_diagnostics: fn(
            &Program,
            &Node, /*SourceFile*/
            Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
        ) -> Vec<Gc<Diagnostic>>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> Vec<Gc<Diagnostic>> {
        self.get_source_files()
            .iter()
            .flat_map(|source_file| get_diagnostics(self, source_file, cancellation_token.clone()))
            .collect()
    }

    pub(super) fn get_program_diagnostics(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Gc<Diagnostic>> {
        if skip_type_checking(source_file, &self.options, |file_name: &str| {
            self.is_source_of_project_reference_redirect_(file_name)
        }) {
            return vec![];
        }

        let source_file_as_source_file = source_file.as_source_file();
        let program_diagnostics_in_file = self
            .program_diagnostics()
            .get_diagnostics(Some(&source_file_as_source_file.file_name()));
        if !matches!(
            source_file_as_source_file.maybe_comment_directives().as_ref(),
            Some(source_file_comment_directives) if !source_file_comment_directives.is_empty()
        ) {
            return program_diagnostics_in_file;
        }

        self.get_diagnostics_with_preceding_directives(
            source_file,
            source_file_as_source_file
                .maybe_comment_directives()
                .as_ref()
                .unwrap(),
            &program_diagnostics_in_file,
        )
        .diagnostics
    }

    pub fn get_declaration_diagnostics(
        &self,
        source_file: Option<&Node /*SourceFile*/>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>> {
        // unimplemented!()
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
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> Vec<Gc<Diagnostic>> {
        (*source_file.as_source_file().parse_diagnostics())
            .borrow()
            .clone()
    }

    pub(super) fn get_semantic_diagnostics_for_file(
        &self,
        source_file: &Node, /*SourceFile*/
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> Vec<Gc<Diagnostic>> {
        concatenate(
            filter_semantic_diagnostics(
                self.get_bind_and_check_diagnostics_for_file(source_file, cancellation_token),
                &self.options,
            ),
            self.get_program_diagnostics(source_file),
        )
    }
}

#[derive(Debug, Trace, Finalize)]
struct GetPrependNodesReadFileCallback {
    program: Gc<Box<Program>>,
}

impl GetPrependNodesReadFileCallback {
    fn new(program: Gc<Box<Program>>) -> Self {
        Self { program }
    }
}

impl ReadFileCallback for GetPrependNodesReadFileCallback {
    fn call(&self, file_name: &str) -> Option<String> {
        let path = self.program.to_path(file_name);
        let source_file = self.program.get_source_file_by_path(&path);
        if let Some(source_file) = source_file {
            Some(source_file.as_source_file().text().clone())
        } else if self.program.files_by_name().contains_key(&*path) {
            None
        } else {
            self.program.host().read_file(&path).ok().flatten()
        }
    }
}

#[derive(Trace, Finalize)]
pub struct ProgramEmitHost {
    program: Gc<Box<Program>>,
    write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
}

impl ProgramEmitHost {
    pub fn new(
        program: Gc<Box<Program>>,
        write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
    ) -> Self {
        Self {
            program,
            write_file_callback,
        }
    }
}

impl EmitHost for ProgramEmitHost {
    fn get_prepend_nodes(&self) -> Vec<Gc<Node /*InputFiles | UnparsedSource*/>> {
        self.program.get_prepend_nodes()
    }

    fn get_canonical_file_name(&self, file_name: &str) -> String {
        self.program.get_canonical_file_name(file_name)
    }

    fn get_common_source_directory(&self) -> String {
        self.program.get_common_source_directory()
    }

    fn get_current_directory(&self) -> String {
        self.program.current_directory().clone()
    }

    fn get_new_line(&self) -> String {
        self.program.host().get_new_line()
    }

    fn get_source_files(&self) -> GcCellRef<Vec<Gc<Node /*SourceFile*/>>> {
        self.program.get_source_files()
    }

    fn get_lib_file_from_reference(&self, ref_: &FileReference) -> Option<Gc<Node /*SourceFile*/>> {
        self.program.get_lib_file_from_reference(ref_)
    }

    fn write_file(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        source_files: Option<&[Gc<Node /*SourceFile*/>]>,
    ) {
        if let Some(write_file_callback) = self.write_file_callback.clone() {
            write_file_callback.call(
                file_name,
                data,
                write_byte_order_mark,
                on_error,
                source_files,
            )
        } else {
            self.program.host().write_file(
                file_name,
                data,
                write_byte_order_mark,
                on_error,
                source_files,
            )
        }
    }

    fn is_emit_blocked(&self, emit_file_name: &str) -> bool {
        self.program.is_emit_blocked(emit_file_name)
    }

    fn use_case_sensitive_file_names(&self) -> bool {
        CompilerHost::use_case_sensitive_file_names(&**self.program.host())
    }

    fn get_program_build_info(&self) -> Option<ProgramBuildInfo> {
        // TODO: this looks like it's implemented as a dynamically-set property on Program in
        // createBuilderProgram()
        unimplemented!()
    }

    fn get_source_file_from_reference(
        &self,
        file: &Node, /*SourceFile | UnparsedSource*/
        ref_: &FileReference,
    ) -> Option<Gc<Node /*SourceFile*/>> {
        self.program.get_source_file_from_reference(file, ref_)
    }

    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>> {
        self.program.redirect_targets_map.clone()
    }

    fn as_source_file_may_be_emitted_host(&self) -> &dyn SourceFileMayBeEmittedHost {
        self
    }
}

impl ScriptReferenceHost for ProgramEmitHost {
    fn get_compiler_options(&self) -> Gc<CompilerOptions> {
        self.program.get_compiler_options()
    }

    fn get_source_file(&self, file_name: &str) -> Option<Gc<Node /*SourceFile*/>> {
        self.program.get_source_file_(file_name)
    }

    fn get_source_file_by_path(&self, path: &Path) -> Option<Gc<Node /*SourceFile*/>> {
        self.program.get_source_file_by_path(path)
    }

    fn get_current_directory(&self) -> String {
        self.program.current_directory().clone()
    }
}

impl ModuleSpecifierResolutionHost for ProgramEmitHost {
    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        Some(CompilerHost::use_case_sensitive_file_names(
            &**self.program.host(),
        ))
    }

    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String> {
        self.program.get_project_reference_redirect_(file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        self.program
            .is_source_of_project_reference_redirect_(file_name)
    }

    fn get_symlink_cache(&self) -> Option<Gc<SymlinkCache>> {
        Some(self.program.get_symlink_cache())
    }

    fn is_read_file_supported(&self) -> bool {
        true
    }

    fn read_file(&self, f: &str) -> Option<io::Result<Option<String>>> {
        Some(self.program.host().read_file(f))
    }

    fn file_exists(&self, f: &str) -> bool {
        let path = self.program.to_path(f);
        if self.program.get_source_file_by_path(&path).is_some() {
            return true;
        }
        if contains(self.program.maybe_missing_file_paths().as_deref(), &path) {
            return false;
        }
        self.program.host().file_exists(f)
    }

    fn get_current_directory(&self) -> String {
        self.program.current_directory().clone()
    }

    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>> {
        self.program.redirect_targets_map.clone()
    }

    fn get_file_include_reasons(&self) -> Rc<RefCell<MultiMap<Path, FileIncludeReason>>> {
        self.program.get_file_include_reasons()
    }
}

impl SourceFileMayBeEmittedHost for ProgramEmitHost {
    fn get_compiler_options(&self) -> Gc<CompilerOptions> {
        self.program.get_compiler_options()
    }

    fn is_source_file_from_external_library(&self, file: &Node /*SourceFile*/) -> bool {
        self.program.is_source_file_from_external_library(file)
    }

    fn get_resolved_project_reference_to_redirect(
        &self,
        file_name: &str,
    ) -> Option<Gc<ResolvedProjectReference>> {
        self.program
            .get_resolved_project_reference_to_redirect(file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        self.program
            .is_source_of_project_reference_redirect_(file_name)
    }
}

#[derive(Clone)]
pub(super) enum ResolveModuleNamesReusingOldStateResultItem {
    ResolvedModuleFull(Gc<ResolvedModuleFull>),
    PredictedToResolveToAmbientModuleMarker,
}

#[derive(Trace, Finalize)]
pub struct ProgramToPath {
    program: Gc<Box<Program>>,
}

impl ProgramToPath {
    pub fn new(program: Gc<Box<Program>>) -> Self {
        Self { program }
    }
}

impl ToPath for ProgramToPath {
    fn call(&self, file_name: &str) -> Path {
        self.program.to_path(file_name)
    }
}
