use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    io, ptr,
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell, GcCellRef, Trace};
use id_arena::Id;
use itertools::Itertools;

use super::{
    create_prepend_nodes, filter_semantic_diagnostics, get_module_names, handle_no_emit_options,
    try_for_each_project_reference_bool, ToPath,
};
use crate::{
    array_is_equal_to, changes_affect_module_resolution, changes_affecting_program_structure,
    compare_values, concatenate, contains, contains_gc, contains_path, create_type_checker,
    emit_files, file_extension_is_one_of, filter, get_base_file_name, get_common_source_directory,
    get_emit_script_target, get_mode_for_resolution_at_index, get_normalized_absolute_path,
    get_resolved_module, get_transformers, has_changes_in_resolutions, is_source_file_js,
    is_trace_enabled, libs, map_defined, module_resolution_is_equal_to, no_transformers,
    node_modules_path_part, not_implemented_resolver, out_file, package_id_to_string,
    project_reference_is_equal_to, ref_unwrapped, remove_prefix, remove_suffix, skip_type_checking,
    sort_and_deduplicate_diagnostics, source_file_may_be_emitted, static_arena, string_contains,
    to_file_name_lower_case, to_path as to_path_helper, trace, try_flat_map,
    type_directive_is_equal_to, zip_to_mode_aware_cache, AllArenas, AsDoubleDeref,
    CancellationTokenDebuggable, Comparison, CompilerHost, CompilerOptions, CustomTransformers,
    Debug_, Diagnostic, Diagnostics, EmitHost, EmitResult, Extension, FileIncludeReason,
    FileReference, FilesByNameValue, GetOrInsertDefault, ModuleSpecifierResolutionHost,
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, MultiMap, Node, NodeFlags,
    NodeInterface, NonEmpty, Path, Program, ProgramBuildInfo, ProjectReference, ReadFileCallback,
    RedirectTargetsMap, ResolveModuleNameResolutionHost, ResolvedModuleFull,
    ResolvedProjectReference, ResolvedTypeReferenceDirective, ScriptReferenceHost, SourceFileLike,
    SourceFileMayBeEmittedHost, SourceOfProjectReferenceRedirect, StringOrRcNode,
    StructureIsReused, SymlinkCache, TypeChecker, WriteFileCallback,
    HasArena, InArena, OptionInArena,
};

impl Program {
    pub(super) fn resolve_module_names_worker(
        &self,
        module_names: &[String],
        containing_file: Id<Node>, /*SourceFile*/
        reused_names: Option<&[String]>,
    ) -> io::Result<Vec<Option<Gc<ResolvedModuleFull>>>> {
        if module_names.is_empty() {
            return Ok(vec![]);
        }
        let containing_file_ref = containing_file.ref_(self);
        let containing_file_as_source_file = containing_file_ref.as_source_file();
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
        )?;
        // performance.mark("afterResolveModule");
        // performance.measure("ResolveModule", "beforeResolveModule", "afterResolveModule");
        // tracing?.pop();
        Ok(result)
    }

    pub(super) fn resolve_type_reference_directive_names_worker(
        &self,
        type_directive_names: &[String],
        containing_file: impl Into<StringOrRcNode>,
    ) -> io::Result<Vec<Option<Id<ResolvedTypeReferenceDirective>>>> {
        if type_directive_names.is_empty() {
            return Ok(vec![]);
        }
        let containing_file: StringOrRcNode = containing_file.into();
        let containing_file_name = match &containing_file {
            StringOrRcNode::RcNode(containing_file) => get_normalized_absolute_path(
                &containing_file.ref_(self).as_source_file().original_file_name(),
                Some(&self.current_directory()),
            ),
            StringOrRcNode::String(containing_file) => containing_file.clone(),
        };
        let redirected_reference = match &containing_file {
            StringOrRcNode::RcNode(containing_file) => {
                let containing_file = *containing_file;
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
            )?;
        // performance.mark("afterResolveTypeReference");
        // performance.measure("ResolveTypeReference", "beforeResolveTypeReference", "afterResolveTypeReference");
        // tracing?.pop();
        Ok(result)
    }

    pub(super) fn get_redirect_reference_for_resolution(
        &self,
        file: Id<Node>, /*SourceFile*/
    ) -> Option<Gc<ResolvedProjectReference>> {
        let file_ref = file.ref_(self);
        let file_as_source_file = file_ref.as_source_file();
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
            || self.options.ref_(self).preserve_symlinks != Some(true)
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
            let resolved_ref_command_line_options_ref = resolved_ref.command_line.options.ref_(self);
            let out = out_file(&resolved_ref_command_line_options_ref)?;
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
        a: Id<Node>, /*SourceFile*/
        b: Id<Node>, /*SourceFile*/
    ) -> Comparison {
        compare_values(
            Some(self.get_default_lib_file_priority(a)),
            Some(self.get_default_lib_file_priority(b)),
        )
    }

    pub(super) fn get_default_lib_file_priority(&self, a: Id<Node> /*SourceFile*/) -> usize {
        let a_ref = a.ref_(self);
        let a_file_name = a_ref.as_source_file().file_name();
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

    pub fn get_root_file_names(&self) -> Ref<Vec<String>> {
        Ref::map(self.root_names.borrow(), |root_names| {
            root_names.as_ref().unwrap()
        })
    }

    pub fn get_source_files(&self) -> GcCellRef<Vec<Id<Node>>> {
        self.files()
    }

    pub fn maybe_get_missing_file_paths(&self) -> Ref<Option<Vec<Path>>> {
        self.missing_file_paths.borrow()
    }

    pub fn get_missing_file_paths(&self) -> Ref<Vec<Path>> {
        ref_unwrapped(&self.missing_file_paths)
    }

    pub fn get_files_by_name_map(&self) -> GcCellRef<HashMap<String, FilesByNameValue>> {
        self.files_by_name()
    }

    pub fn get_compiler_options(&self) -> Id<CompilerOptions> {
        self.options.clone()
    }

    pub fn use_case_sensitive_file_names(&self) -> bool {
        CompilerHost::use_case_sensitive_file_names(&**self.host())
    }

    pub fn get_file_include_reasons(&self) -> Gc<GcCell<MultiMap<Path, Id<FileIncludeReason>>>> {
        self.file_reasons.borrow().clone()
    }

    pub fn get_source_file_(&self, file_name: &str) -> Option<Id<Node /*SourceFile*/>> {
        self.get_source_file_by_path(&self.to_path(file_name))
    }

    pub fn get_syntactic_diagnostics(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> Vec<Id<Diagnostic /*DiagnosticWithLocation*/>> {
        self.get_diagnostics_helper(
            source_file,
            |source_file, cancellation_token| {
                self.get_syntactic_diagnostics_for_file(source_file, cancellation_token)
            },
            cancellation_token,
        )
    }

    pub fn get_semantic_diagnostics(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<Vec<Id<Diagnostic>>> {
        self.try_get_diagnostics_helper(
            source_file,
            |source_file, cancellation_token| {
                self.get_semantic_diagnostics_for_file(source_file, cancellation_token)
            },
            cancellation_token,
        )
    }

    pub fn emit(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
        emit_only_dts_files: Option<bool>,
        transformers: Option<&CustomTransformers>,
        force_dts_emit: Option<bool>,
    ) -> io::Result<EmitResult> {
        // return super::emit_skipped_with_no_diagnostics();
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
        })?;
        // tracing?.pop();
        Ok(result)
    }

    pub(super) fn is_emit_blocked(&self, emit_file_name: &str) -> bool {
        self.has_emit_blocking_diagnostics()
            .contains_key(&self.to_path(emit_file_name))
    }

    pub(super) fn emit_worker(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
        emit_only_dts_files: Option<bool>,
        custom_transformers: Option<&CustomTransformers>,
        force_dts_emit: Option<bool>,
    ) -> io::Result<EmitResult> {
        if force_dts_emit != Some(true) {
            let result = handle_no_emit_options(
                self.arena_id(),
                source_file,
                write_file_callback.clone(),
                cancellation_token.clone(),
                self,
            )?;
            if let Some(result) = result {
                return Ok(result);
            }
        }

        let emit_resolver = self
            .get_diagnostics_producing_type_checker()?
            .get_emit_resolver(
                if matches!(
                    out_file(&self.options.ref_(self)),
                    Some(out_file) if !out_file.is_empty()
                ) {
                    None
                } else {
                    source_file
                },
                cancellation_token,
            )?;

        // performance.mark("beforeEmit");

        let emit_result = emit_files(
            emit_resolver,
            self.get_emit_host(write_file_callback),
            source_file,
            get_transformers(&self.options.ref_(self), custom_transformers, emit_only_dts_files, self),
            emit_only_dts_files,
            Some(false),
            force_dts_emit,
            self,
        )?;

        // performance.mark("afterEmit");
        // performance.measure("Emit", "beforeEmit", "afterEmit");
        Ok(emit_result)
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
        Gc::new(Box::new(ProgramToPath::new(self.arena_id())))
    }

    pub fn get_common_source_directory(&self) -> String {
        self.maybe_common_source_directory_mut()
            .get_or_insert_with(|| {
                let emitted_files = filter(&**self.files(), |&file: &Id<Node>| {
                    source_file_may_be_emitted(&file.ref_(self), self, None, self)
                });
                get_common_source_directory(
                    &self.options.ref_(self),
                    || {
                        map_defined(Some(&emitted_files), |file: &Id<Node>, _| {
                            let file_ref = file.ref_(self);
                            let file_as_source_file = file_ref.as_source_file();
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
        file: Id<Node>, /*SourceFile*/
    ) -> io::Result<Vec<Option<Gc<ResolvedModuleFull>>>> {
        let file_ref = file.ref_(self);
        let file_as_source_file = file_ref.as_source_file();
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
            .and_then(|old_program| old_program.ref_(self).get_source_file_(&file_as_source_file.file_name()));
        if old_source_file != Some(file) {
            if let Some(file_resolved_modules) =
                file_as_source_file.maybe_resolved_modules().as_ref()
            {
                let mut result: Vec<Option<Gc<ResolvedModuleFull>>> = vec![];
                let mut i = 0;
                for module_name in module_names {
                    let resolved_module = file_resolved_modules
                        .get(
                            module_name,
                            get_mode_for_resolution_at_index(file_as_source_file, i, self),
                        )
                        .flatten();
                    i += 1;
                    result.push(resolved_module);
                }
                return Ok(result);
            }
        }
        let mut unknown_module_names: Option<Vec<String>> = None;
        let mut result: Option<Vec<Option<ResolveModuleNamesReusingOldStateResultItem>>> = None;
        let mut reused_names: Option<Vec<String>> = None;

        for i in 0..module_names.len() {
            let module_name = &module_names[i];
            if let Some(old_source_file) = old_source_file
                .filter(|&old_source_file| file == old_source_file)
            {
                let old_source_file_ref = old_source_file.ref_(self);
                let old_source_file_as_source_file = old_source_file_ref.as_source_file();
                if !self.has_invalidated_resolution(&old_source_file_as_source_file.path()) {
                    let old_resolved_module = get_resolved_module(
                        Some(&old_source_file.ref_(self)),
                        module_name,
                        get_mode_for_resolution_at_index(old_source_file_as_source_file, i, self),
                    );
                    if let Some(old_resolved_module) = old_resolved_module.as_ref() {
                        if is_trace_enabled(
                            &self.options.ref_(self),
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
                            .get_or_insert_default_()
                            .push(module_name.clone());
                        continue;
                    }
                }
            }
            let resolves_to_ambient_module_in_non_modified_file: bool/* = false*/;
            if contains(
                file_as_source_file.maybe_ambient_module_names().as_deref(),
                module_name,
            ) {
                resolves_to_ambient_module_in_non_modified_file = true;
                if is_trace_enabled(&self.options.ref_(self), self.host().as_dyn_module_resolution_host()) {
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
                        old_source_file,
                        module_name,
                        i,
                    );
            }

            if resolves_to_ambient_module_in_non_modified_file {
                result.get_or_insert_with(|| vec![None; module_names.len()])[i] = Some(ResolveModuleNamesReusingOldStateResultItem::PredictedToResolveToAmbientModuleMarker);
            } else {
                unknown_module_names
                    .get_or_insert_default_()
                    .push(module_name.clone());
            }
        }

        let resolutions = if let Some(unknown_module_names) = unknown_module_names
            .as_ref()
            .filter(|unknown_module_names| !unknown_module_names.is_empty())
        {
            self.resolve_module_names_worker(unknown_module_names, file, reused_names.as_deref())?
        } else {
            vec![]
        };

        if result.is_none() {
            Debug_.assert(resolutions.len() == module_names.len(), None);
            return Ok(resolutions);
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

        Ok(result)
    }

    pub(super) fn module_name_resolves_to_ambient_module_in_non_modified_file(
        &self,
        old_source_file: Option<Id<Node>>,
        module_name: &str,
        index: usize,
    ) -> bool {
        if index
            >= old_source_file
                .and_then(|old_source_file| {
                    old_source_file
                        .ref_(self).as_source_file()
                        .maybe_imports()
                        .as_ref()
                        .map(|old_source_file_imports| old_source_file_imports.len())
                })
                .unwrap_or(0)
                + old_source_file
                    .and_then(|old_source_file| {
                        old_source_file
                            .ref_(self).as_source_file()
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
            old_source_file.refed(self).as_deref(),
            module_name,
            old_source_file.and_then(|old_source_file| {
                get_mode_for_resolution_at_index(old_source_file.ref_(self).as_source_file(), index, self)
            }),
        );
        let resolved_file = resolution_to_file.as_ref().and_then(|resolution_to_file| {
            self.maybe_old_program()
                .unwrap()
                .ref_(self).get_source_file_(&resolution_to_file.resolved_file_name)
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

        if is_trace_enabled(&self.options.ref_(self), self.host().as_dyn_module_resolution_host()) {
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

    pub fn can_reuse_project_references(&self) -> io::Result<bool> {
        Ok(!try_for_each_project_reference_bool(
            self.maybe_old_program()
                .as_ref()
                .unwrap()
                .ref_(self).get_project_references()
                .as_deref(),
            self.maybe_old_program()
                .as_ref()
                .unwrap()
                .ref_(self).get_resolved_project_references()
                .as_deref(),
            |old_resolved_ref: Option<Gc<ResolvedProjectReference>>,
             parent: Option<&ResolvedProjectReference>,
             index: usize| {
                let new_ref = parent
                    .map(|parent| {
                        parent.command_line.project_references.as_ref().unwrap()[index].clone()
                    })
                    .unwrap_or_else(|| {
                        self.maybe_project_references().as_ref().unwrap()[index].clone()
                    });
                let new_resolved_ref = self.parse_project_reference_config_file(&new_ref)?;
                io::Result::Ok(if let Some(old_resolved_ref) = old_resolved_ref {
                    match new_resolved_ref {
                        None => true,
                        Some(new_resolved_ref) => {
                            new_resolved_ref.source_file != old_resolved_ref.source_file
                             || old_resolved_ref.command_line.file_names
                                != new_resolved_ref.command_line.file_names
                        }
                    }
                } else {
                    new_resolved_ref.is_some()
                })
            },
            Some(
                |old_project_references: Option<&[Rc<ProjectReference>]>,
                 parent: Option<&ResolvedProjectReference>| {
                    let new_references = if let Some(parent) = parent {
                        self.get_resolved_project_reference_by_path(
                            &parent.source_file.ref_(self).as_source_file().path(),
                        )
                        .unwrap()
                        .command_line
                        .project_references
                        .clone()
                    } else {
                        self.maybe_project_references().clone()
                    };
                    Ok(!array_is_equal_to(
                        old_project_references,
                        new_references.as_deref(),
                        |a: &Rc<ProjectReference>, b: &Rc<ProjectReference>, _| {
                            project_reference_is_equal_to(a, b)
                        },
                    ))
                },
            ),
            self,
        )?)
    }

    pub fn try_reuse_structure_from_old_program(&self) -> io::Result<StructureIsReused> {
        let old_program = self.maybe_old_program();
        if old_program.is_none() {
            return Ok(StructureIsReused::Not);
        }
        let old_program = old_program.unwrap();

        let old_options = old_program.ref_(self).get_compiler_options();
        if changes_affect_module_resolution(&old_options.ref_(self), &self.options.ref_(self)) {
            return Ok(StructureIsReused::Not);
        }

        let old_program_ref = old_program.ref_(self);
        let old_root_names = old_program_ref.get_root_file_names();
        if &*old_root_names != &*self.root_names() {
            return Ok(StructureIsReused::Not);
        }

        if !self.can_reuse_project_references()? {
            return Ok(StructureIsReused::Not);
        }
        if let Some(project_references) = self.maybe_project_references().as_ref() {
            *self.resolved_project_references.borrow_mut() = Some(
                project_references
                    .into_iter()
                    .map(|project_reference| {
                        self.parse_project_reference_config_file(project_reference)
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            );
        }

        let mut new_source_files: Vec<Id<Node /*SourceFile*/>> = Default::default();
        struct ModifiedSourceFile {
            pub old_file: Id<Node /*SourceFile*/>,
            pub new_file: Id<Node /*SourceFile*/>,
        }
        let mut modified_source_files: Vec<ModifiedSourceFile> = Default::default();
        self.set_structure_is_reused(StructureIsReused::Completely);

        if old_program
            .ref_(self).get_missing_file_paths()
            .iter()
            .any(|missing_file_path| self.host().file_exists(missing_file_path))
        {
            return Ok(StructureIsReused::Not);
        }

        let old_program_ref = old_program.ref_(self);
        let old_source_files = old_program_ref.get_source_files();
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        enum SeenPackageName {
            Exists,
            Modified,
        }
        let mut seen_package_names: HashMap<String, SeenPackageName> = Default::default();

        for &old_source_file in &*old_source_files {
            let old_source_file_ref = old_source_file.ref_(self);
            let old_source_file_as_source_file = old_source_file_ref.as_source_file();
            let Some(mut new_source_file) = (if self.host().is_get_source_file_by_path_supported() {
                self.host().get_source_file_by_path(
                    &old_source_file_as_source_file.file_name(),
                    old_source_file_as_source_file
                        .maybe_resolved_path()
                        .as_ref()
                        .unwrap(),
                    get_emit_script_target(&self.options.ref_(self)),
                    None,
                    Some(self.should_create_new_source_file()),
                )
            } else {
                self.host().get_source_file(
                    &old_source_file_as_source_file.file_name(),
                    get_emit_script_target(&self.options.ref_(self)),
                    None,
                    Some(self.should_create_new_source_file()),
                )?
            }) else {
                return Ok(StructureIsReused::Not);
            };

            let new_source_file_ref = new_source_file.ref_(self);
            let mut new_source_file_as_source_file = new_source_file_ref.as_source_file();

            Debug_.assert(
                new_source_file_as_source_file
                    .maybe_redirect_info()
                    .is_none(),
                Some("Host should not return a redirect source file from `getSourceFile`"),
            );

            let file_changed: bool;
            if let Some(old_source_file_redirect_info) = old_source_file_as_source_file
                .maybe_redirect_info()
                .as_ref()
            {
                if new_source_file != old_source_file_redirect_info.unredirected {
                    return Ok(StructureIsReused::Not);
                }
                file_changed = false;
                new_source_file = old_source_file;
                new_source_file_as_source_file = old_source_file_ref.as_source_file();
            } else if (*old_program.ref_(self).redirect_targets_map())
                .borrow()
                .contains_key(&*old_source_file_as_source_file.path())
            {
                if new_source_file != old_source_file {
                    return Ok(StructureIsReused::Not);
                }
                file_changed = false;
            } else {
                file_changed = new_source_file != old_source_file;
            }

            let old_source_file_path = {
                let value = old_source_file_as_source_file.path().clone();
                value
            };
            new_source_file_as_source_file.set_path(old_source_file_path);
            let old_source_file_original_file_name = {
                let value = old_source_file_as_source_file
                    .maybe_original_file_name()
                    .clone();
                value
            };
            new_source_file_as_source_file
                .set_original_file_name(old_source_file_original_file_name);
            let old_source_file_resolved_path = {
                let value = old_source_file_as_source_file.maybe_resolved_path().clone();
                value
            };
            new_source_file_as_source_file.set_resolved_path(old_source_file_resolved_path);
            let old_source_file_file_name = {
                let value = old_source_file_as_source_file.file_name().clone();
                value
            };
            new_source_file_as_source_file.set_file_name(old_source_file_file_name);

            let package_name = old_program
                .ref_(self).source_file_to_package_name()
                .get(&*old_source_file_as_source_file.path())
                .cloned();
            if let Some(ref package_name) = package_name {
                let prev_kind = seen_package_names.get(package_name).copied();
                let new_kind = if file_changed {
                    SeenPackageName::Modified
                } else {
                    SeenPackageName::Exists
                };
                if prev_kind.is_some() && new_kind == SeenPackageName::Modified
                    || prev_kind == Some(SeenPackageName::Modified)
                {
                    return Ok(StructureIsReused::Not);
                }
                seen_package_names.insert(package_name.clone(), new_kind);
            }

            if file_changed {
                if !array_is_equal_to(
                    old_source_file_as_source_file
                        .maybe_lib_reference_directives()
                        .as_ref()
                        .map(|old_source_file_lib_reference_directives| {
                            (**old_source_file_lib_reference_directives).borrow()
                        })
                        .as_double_deref(),
                    new_source_file_as_source_file
                        .maybe_lib_reference_directives()
                        .as_ref()
                        .map(|new_source_file_lib_reference_directives| {
                            (**new_source_file_lib_reference_directives).borrow()
                        })
                        .as_double_deref(),
                    |a: &FileReference, b: &FileReference, _| self.file_reference_is_equal_to(a, b),
                ) {
                    self.set_structure_is_reused(StructureIsReused::SafeModules);
                }

                if old_source_file_as_source_file.has_no_default_lib()
                    != new_source_file_as_source_file.has_no_default_lib()
                {
                    self.set_structure_is_reused(StructureIsReused::SafeModules);
                }

                if !array_is_equal_to(
                    old_source_file_as_source_file
                        .maybe_referenced_files()
                        .as_ref()
                        .map(|old_source_file_referenced_files| {
                            (**old_source_file_referenced_files).borrow()
                        })
                        .as_double_deref(),
                    new_source_file_as_source_file
                        .maybe_referenced_files()
                        .as_ref()
                        .map(|new_source_file_referenced_files| {
                            (**new_source_file_referenced_files).borrow()
                        })
                        .as_double_deref(),
                    |a: &FileReference, b: &FileReference, _| self.file_reference_is_equal_to(a, b),
                ) {
                    self.set_structure_is_reused(StructureIsReused::SafeModules);
                }

                self.collect_external_module_references(new_source_file);

                if !array_is_equal_to(
                    old_source_file_as_source_file.maybe_imports().as_deref(),
                    new_source_file_as_source_file.maybe_imports().as_deref(),
                    |&a: &Id<Node>, &b: &Id<Node>, _| self.module_name_is_equal_to(a, b),
                ) {
                    self.set_structure_is_reused(StructureIsReused::SafeModules);
                }
                if !array_is_equal_to(
                    old_source_file_as_source_file
                        .maybe_module_augmentations()
                        .as_deref(),
                    new_source_file_as_source_file
                        .maybe_module_augmentations()
                        .as_deref(),
                    |&a: &Id<Node>, &b: &Id<Node>, _| self.module_name_is_equal_to(a, b),
                ) {
                    self.set_structure_is_reused(StructureIsReused::SafeModules);
                }
                if old_source_file.ref_(self).flags() & NodeFlags::PermanentlySetIncrementalFlags
                    != new_source_file.ref_(self).flags() & NodeFlags::PermanentlySetIncrementalFlags
                {
                    self.set_structure_is_reused(StructureIsReused::SafeModules);
                }

                if !array_is_equal_to(
                    old_source_file_as_source_file
                        .maybe_type_reference_directives()
                        .as_ref()
                        .map(|old_source_file_type_reference_directives| {
                            (**old_source_file_type_reference_directives).borrow()
                        })
                        .as_double_deref(),
                    new_source_file_as_source_file
                        .maybe_type_reference_directives()
                        .as_ref()
                        .map(|new_source_file_type_reference_directives| {
                            (**new_source_file_type_reference_directives).borrow()
                        })
                        .as_double_deref(),
                    |a: &FileReference, b: &FileReference, _| self.file_reference_is_equal_to(a, b),
                ) {
                    self.set_structure_is_reused(StructureIsReused::SafeModules);
                }

                modified_source_files.push(ModifiedSourceFile {
                    old_file: old_source_file.clone(),
                    new_file: new_source_file.clone(),
                });
            } else if self.has_invalidated_resolution(&old_source_file_as_source_file.path()) {
                self.set_structure_is_reused(StructureIsReused::SafeModules);

                modified_source_files.push(ModifiedSourceFile {
                    old_file: old_source_file.clone(),
                    new_file: new_source_file.clone(),
                });
            }

            new_source_files.push(new_source_file);
        }

        if self.structure_is_reused() != StructureIsReused::Completely {
            return Ok(self.structure_is_reused());
        }

        let modified_files = modified_source_files
            .iter()
            .map(|f| f.old_file.clone())
            .collect_vec();
        for &old_file in &*old_source_files {
            if !contains(Some(&modified_files), &old_file) {
                let old_file_ref = old_file.ref_(self);
                let old_file_as_source_file = old_file_ref.as_source_file();
                for module_name in old_file_as_source_file
                    .maybe_ambient_module_names()
                    .as_ref()
                    .unwrap()
                {
                    self.ambient_module_name_to_unmodified_file_name().insert(
                        module_name.clone(),
                        old_file_as_source_file.file_name().clone(),
                    );
                }
            }
        }

        for ModifiedSourceFile {
            old_file: old_source_file,
            new_file: new_source_file,
        } in modified_source_files
        {
            let module_names = get_module_names(new_source_file, self);
            let resolutions =
                self.resolve_module_names_reusing_old_state(&module_names, new_source_file)?;
            let old_source_file_ref = old_source_file.ref_(self);
            let old_source_file_as_source_file = old_source_file_ref.as_source_file();
            let new_source_file_ref = new_source_file.ref_(self);
            let new_source_file_as_source_file = new_source_file_ref.as_source_file();
            let resolutions_changed = has_changes_in_resolutions(
                &module_names,
                &resolutions,
                old_source_file_as_source_file
                    .maybe_resolved_modules()
                    .as_ref(),
                Some(old_source_file),
                |a: &Option<Gc<ResolvedModuleFull>>, b: &Option<Gc<ResolvedModuleFull>>| {
                    module_resolution_is_equal_to(a.as_ref().unwrap(), b.as_ref().unwrap())
                },
                self,
            );
            if resolutions_changed {
                self.set_structure_is_reused(StructureIsReused::SafeModules);
                *new_source_file_as_source_file.maybe_resolved_modules() = Some(
                    zip_to_mode_aware_cache(new_source_file, &module_names, &resolutions, self),
                );
            } else {
                *new_source_file_as_source_file.maybe_resolved_modules() =
                    old_source_file_as_source_file
                        .maybe_resolved_modules()
                        .clone();
            }
            let types_reference_directives = (*new_source_file_as_source_file
                .type_reference_directives())
            .borrow()
            .iter()
            .map(|ref_| to_file_name_lower_case(&ref_.file_name))
            .collect_vec();
            let type_reference_resolutions = self.resolve_type_reference_directive_names_worker(
                &types_reference_directives,
                new_source_file.clone(),
            )?;
            let type_reference_resolutions_changed = has_changes_in_resolutions(
                &types_reference_directives,
                &type_reference_resolutions,
                old_source_file_as_source_file
                    .maybe_resolved_type_reference_directive_names()
                    .as_ref(),
                Some(old_source_file),
                |a: &Option<Id<ResolvedTypeReferenceDirective>>,
                 b: &Option<Id<ResolvedTypeReferenceDirective>>| {
                    type_directive_is_equal_to(&a.unwrap().ref_(self), &b.unwrap().ref_(self))
                },
                self,
            );
            if type_reference_resolutions_changed {
                self.set_structure_is_reused(StructureIsReused::SafeModules);
                *new_source_file_as_source_file.maybe_resolved_type_reference_directive_names() =
                    Some(zip_to_mode_aware_cache(
                        new_source_file,
                        &types_reference_directives,
                        &type_reference_resolutions,
                        self,
                    ));
            } else {
                *new_source_file_as_source_file.maybe_resolved_type_reference_directive_names() =
                    old_source_file_as_source_file
                        .maybe_resolved_type_reference_directive_names()
                        .clone();
            }
        }

        if self.structure_is_reused() != StructureIsReused::Completely {
            return Ok(self.structure_is_reused());
        }

        if changes_affecting_program_structure(&old_options.ref_(self), &self.options.ref_(self))
            || self.host().has_changed_automatic_type_directive_names() == Some(true)
        {
            return Ok(StructureIsReused::SafeModules);
        }

        *self.maybe_missing_file_paths() = Some(old_program.ref_(self).get_missing_file_paths().clone());

        Debug_.assert(
            new_source_files.len() == old_program.ref_(self).get_source_files().len(),
            None,
        );
        for new_source_file in &new_source_files {
            self.files_by_name_mut().insert(
                (&**new_source_file.ref_(self).as_source_file().path()).to_owned(),
                new_source_file.clone().into(),
            );
        }
        let old_program_ref = old_program.ref_(self);
        let old_files_by_name_map = old_program_ref.get_files_by_name_map();
        for (path, old_file) in &*old_files_by_name_map {
            if !matches!(old_file, FilesByNameValue::SourceFile(_)) {
                self.files_by_name_mut()
                    .insert(path.clone(), old_file.clone());
                continue;
            }
            let old_file = old_file.as_source_file();
            let old_file_ref = old_file.ref_(self);
            let old_file_as_source_file = old_file_ref.as_source_file();
            if &**old_file_as_source_file.path() == &**path {
                if old_program.ref_(self).is_source_file_from_external_library(old_file) {
                    self.source_files_found_searching_node_modules_mut()
                        .insert((&**old_file_as_source_file.path()).to_owned(), true);
                }
                continue;
            }
            let value = {
                let value = self
                    .files_by_name()
                    .get(&**old_file_as_source_file.path())
                    .cloned();
                value
            };
            self.files_by_name_mut()
                .insert(path.clone(), value.unwrap_or(FilesByNameValue::Undefined));
        }

        self.set_files(Some(new_source_files));
        self.set_file_reasons(old_program.ref_(self).get_file_include_reasons());
        *self.maybe_file_processing_diagnostics() =
            old_program.ref_(self).maybe_file_processing_diagnostics().clone();
        self.set_resolved_type_reference_directives(
            old_program.ref_(self).resolved_type_reference_directives(),
        );

        *self.source_file_to_package_name.borrow_mut() =
            Some(old_program.ref_(self).source_file_to_package_name().clone());
        self.set_redirect_targets_map(old_program.ref_(self).redirect_targets_map());
        self.set_uses_uri_style_node_core_modules(old_program.ref_(self).uses_uri_style_node_core_modules());

        Ok(StructureIsReused::Completely)
    }

    pub(super) fn get_emit_host(
        &self,
        write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
    ) -> Id<Box<dyn EmitHost>> {
        self.alloc_emit_host(Box::new(ProgramEmitHost::new(
            self.arena_id(),
            write_file_callback,
        )))
    }

    pub(super) fn emit_build_info(
        &self,
        write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
        _cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<EmitResult> {
        Debug_.assert(out_file(&self.options.ref_(self)).non_empty().is_none(), None);
        // tracing?.push(tracing.Phase.Emit, "emitBuildInfo", {}, /*separateBeginAndEnd*/ true);
        // performance.mark("beforeEmit");
        let emit_result = emit_files(
            not_implemented_resolver(self),
            self.get_emit_host(write_file_callback),
            None,
            no_transformers(),
            Some(false),
            Some(true),
            None,
            self,
        )?;

        // performance.mark("afterEmit");
        // performance.measure("Emit", "beforeEmit", "afterEmit");
        // tracing?.pop();
        Ok(emit_result)
    }

    pub fn get_resolved_project_references(
        &self,
    ) -> GcCellRef<Option<Vec<Option<Gc<ResolvedProjectReference>>>>> {
        self.resolved_project_references.borrow()
    }

    pub fn get_project_references(&self) -> Ref<Option<Vec<Rc<ProjectReference>>>> {
        self.maybe_project_references()
    }

    pub(super) fn get_prepend_nodes(&self) -> Vec<Id<Node /*InputFiles*/>> {
        create_prepend_nodes(
            self.maybe_project_references().as_deref(),
            |_ref, index| {
                self.maybe_resolved_project_references_mut()
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
                self.arena_id(),
            ))),
            self,
        )
    }

    pub fn is_source_file_from_external_library(&self, file: Id<Node> /*SourceFile*/) -> bool {
        self.source_files_found_searching_node_modules()
            .get(&**file.ref_(self).as_source_file().path())
            .copied()
            == Some(true)
    }

    pub fn is_source_file_default_library(&self, _file: Id<Node> /*SourceFile*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_diagnostics_producing_type_checker(&self) -> io::Result<Gc<TypeChecker>> {
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
                &*static_arena(),
                self.arena_id(),
                true,
            )?);
        }
        Ok(diagnostics_producing_type_checker.as_ref().unwrap().clone())
    }

    pub(super) fn get_diagnostics_helper(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        mut get_diagnostics: impl FnMut(
            Id<Node>, /*SourceFile*/
            Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
        ) -> Vec<Id<Diagnostic>>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> Vec<Id<Diagnostic>> {
        self.try_get_diagnostics_helper(
            source_file,
            |a, b| Ok(get_diagnostics(a, b)),
            cancellation_token,
        )
        .unwrap()
    }

    pub(super) fn try_get_diagnostics_helper(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        mut get_diagnostics: impl FnMut(
            Id<Node>, /*SourceFile*/
            Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
        ) -> io::Result<Vec<Id<Diagnostic>>>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<Vec<Id<Diagnostic>>> {
        if let Some(source_file) = source_file {
            return get_diagnostics(source_file, cancellation_token);
        }
        Ok(sort_and_deduplicate_diagnostics(&try_flat_map(
            Some(&*self.get_source_files()),
            |&source_file: &Id<Node>, _| {
                // if (cancellationToken) {
                //     cancellationToken.throwIfCancellationRequested();
                // }
                get_diagnostics(source_file, cancellation_token.clone())
            },
        )?, self)
        .into())
    }

    pub(super) fn get_program_diagnostics(
        &self,
        source_file: Id<Node>, /*SourceFile*/
    ) -> Vec<Id<Diagnostic>> {
        if skip_type_checking(&source_file.ref_(self), &self.options.ref_(self), |file_name: &str| {
            self.is_source_of_project_reference_redirect_(file_name)
        }) {
            return vec![];
        }

        let source_file_ref = source_file.ref_(self);
        let source_file_as_source_file = source_file_ref.as_source_file();
        let program_diagnostics_in_file = self
            .program_diagnostics()
            .get_diagnostics(Some(&source_file_as_source_file.file_name()));
        if !matches!(
            source_file_as_source_file.maybe_comment_directives().as_ref(),
            Some(source_file_comment_directives) if !source_file_comment_directives.is_empty()
        ) {
            return program_diagnostics_in_file;
        }

        let ret = self.get_diagnostics_with_preceding_directives(
            source_file,
            source_file_as_source_file
                .maybe_comment_directives()
                .as_ref()
                .unwrap(),
            &program_diagnostics_in_file,
        )
        .diagnostics;
        ret
    }

    pub fn get_declaration_diagnostics(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<Vec<Id<Diagnostic /*DiagnosticWithLocation*/>>> {
        let options = self.get_compiler_options();
        Ok(
            if source_file.is_none() || out_file(&options.ref_(self)).is_non_empty() {
                self.get_declaration_diagnostics_worker(source_file, cancellation_token)?
            } else {
                self.try_get_diagnostics_helper(
                    source_file,
                    |source_file, cancellation_token| {
                        self.get_declaration_diagnostics_for_file(source_file, cancellation_token)
                    },
                    cancellation_token,
                )?
            },
        )
    }

    pub(super) fn run_with_cancellation_token<TReturn>(
        &self,
        func: impl FnOnce() -> TReturn,
    ) -> TReturn {
        func()
    }

    pub(super) fn get_syntactic_diagnostics_for_file(
        &self,
        source_file: Id<Node>, /*SourceFile*/
        // TODO: getSyntacticDiagnosticsForFile() doesn't actually take this argument, should
        // refactor eg get_diagnostics_helper() to use closures instead?
        _cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> Vec<Id<Diagnostic>> {
        let source_file_ref = source_file.ref_(self);
        let source_file_as_source_file = source_file_ref.as_source_file();
        if is_source_file_js(&source_file.ref_(self)) {
            let mut source_file_additional_syntactic_diagnostics =
                source_file_as_source_file.maybe_additional_syntactic_diagnostics_mut();
            let source_file_additional_syntactic_diagnostics =
                source_file_additional_syntactic_diagnostics
                    .get_or_insert_with(|| self.get_js_syntactic_diagnostics_for_file(source_file));
            return concatenate(
                source_file_additional_syntactic_diagnostics.clone(),
                (*source_file.ref_(self).as_source_file().parse_diagnostics())
                    .borrow()
                    .clone(),
            );
        }
        (*source_file.ref_(self).as_source_file().parse_diagnostics())
            .borrow()
            .clone()
    }

    pub(super) fn get_semantic_diagnostics_for_file(
        &self,
        source_file: Id<Node>, /*SourceFile*/
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<Vec<Id<Diagnostic>>> {
        Ok(concatenate(
            filter_semantic_diagnostics(
                self.get_bind_and_check_diagnostics_for_file(source_file, cancellation_token)?,
                &self.options.ref_(self),
                self,
            ),
            self.get_program_diagnostics(source_file),
        ))
    }
}

#[derive(Debug, Trace, Finalize)]
struct GetPrependNodesReadFileCallback {
    program: Id<Program>,
}

impl GetPrependNodesReadFileCallback {
    fn new(program: Id<Program>) -> Self {
        Self { program }
    }
}

impl ReadFileCallback for GetPrependNodesReadFileCallback {
    fn call(&self, file_name: &str) -> Option<String> {
        let path = self.program.ref_(self).to_path(file_name);
        let source_file = self.program.ref_(self).get_source_file_by_path(&path);
        if let Some(source_file) = source_file {
            Some(source_file.ref_(self).as_source_file().text().clone())
        } else if self.program.ref_(self).files_by_name().contains_key(&*path) {
            None
        } else {
            self.program.ref_(self).host().read_file(&path).ok().flatten()
        }
    }
}

impl HasArena for GetPrependNodesReadFileCallback {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
pub struct ProgramEmitHost {
    program: Id<Program>,
    write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
}

impl ProgramEmitHost {
    pub fn new(
        program: Id<Program>,
        write_file_callback: Option<Gc<Box<dyn WriteFileCallback>>>,
    ) -> Self {
        Self {
            program,
            write_file_callback,
        }
    }
}

impl EmitHost for ProgramEmitHost {
    fn get_prepend_nodes(&self) -> Vec<Id<Node /*InputFiles | UnparsedSource*/>> {
        self.program.ref_(self).get_prepend_nodes()
    }

    fn get_new_line(&self) -> String {
        self.program.ref_(self).host().get_new_line()
    }

    fn get_source_files(&self) -> Vec<Id<Node /*SourceFile*/>> {
        self.program.ref_(self).get_source_files().clone()
    }

    fn get_lib_file_from_reference(&self, ref_: &FileReference) -> Option<Id<Node /*SourceFile*/>> {
        self.program.ref_(self).get_lib_file_from_reference(ref_)
    }

    fn write_file(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        source_files: Option<&[Id<Node /*SourceFile*/>]>,
    ) -> io::Result<()> {
        if let Some(write_file_callback) = self.write_file_callback.clone() {
            write_file_callback.call(
                file_name,
                data,
                write_byte_order_mark,
                on_error,
                source_files,
            )?;
        } else {
            self.program.ref_(self).host().write_file(
                file_name,
                data,
                write_byte_order_mark,
                on_error,
                source_files,
            )?;
        }
        Ok(())
    }

    fn is_emit_blocked(&self, emit_file_name: &str) -> bool {
        self.program.ref_(self).is_emit_blocked(emit_file_name)
    }

    fn use_case_sensitive_file_names(&self) -> bool {
        CompilerHost::use_case_sensitive_file_names(&**self.program.ref_(self).host())
    }

    fn get_program_build_info(&self) -> Option<Gc<ProgramBuildInfo>> {
        self.program
            .ref_(self).maybe_get_program_build_info_rc()
            .and_then(|get_program_build_info| get_program_build_info.call())
    }

    fn get_source_file_from_reference(
        &self,
        file: Id<Node>, /*SourceFile | UnparsedSource*/
        ref_: &FileReference,
    ) -> io::Result<Option<Id<Node /*SourceFile*/>>> {
        self.program.ref_(self).get_source_file_from_reference(file, ref_)
    }

    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>> {
        self.program.ref_(self).redirect_targets_map()
    }

    fn as_source_file_may_be_emitted_host(&self) -> &dyn SourceFileMayBeEmittedHost {
        self
    }

    fn as_module_specifier_resolution_host_and_get_common_source_directory(
        &self,
    ) -> &(dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory + 'static) {
        self
    }
}

impl ScriptReferenceHost for ProgramEmitHost {
    fn get_compiler_options(&self) -> Id<CompilerOptions> {
        self.program.ref_(self).get_compiler_options()
    }

    fn get_source_file(&self, file_name: &str) -> Option<Id<Node /*SourceFile*/>> {
        self.program.ref_(self).get_source_file_(file_name)
    }

    fn get_source_file_by_path(&self, path: &Path) -> Option<Id<Node /*SourceFile*/>> {
        self.program.ref_(self).get_source_file_by_path(path)
    }

    fn get_current_directory(&self) -> String {
        self.program.ref_(self).current_directory().clone()
    }
}

impl ModuleSpecifierResolutionHostAndGetCommonSourceDirectory for ProgramEmitHost {
    fn get_common_source_directory(&self) -> String {
        self.program.ref_(self).get_common_source_directory()
    }

    fn as_dyn_module_specifier_resolution_host(&self) -> &dyn ModuleSpecifierResolutionHost {
        self
    }
}

impl ModuleSpecifierResolutionHost for ProgramEmitHost {
    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        Some(CompilerHost::use_case_sensitive_file_names(
            &**self.program.ref_(self).host(),
        ))
    }

    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String> {
        self.program.ref_(self).get_project_reference_redirect_(file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        self.program
            .ref_(self).is_source_of_project_reference_redirect_(file_name)
    }

    fn get_symlink_cache(&self) -> Option<Gc<SymlinkCache>> {
        Some(self.program.ref_(self).get_symlink_cache())
    }

    fn is_read_file_supported(&self) -> bool {
        true
    }

    fn read_file(&self, f: &str) -> Option<io::Result<Option<String>>> {
        Some(self.program.ref_(self).host().read_file(f))
    }

    fn file_exists(&self, f: &str) -> bool {
        let path = self.program.ref_(self).to_path(f);
        if self.program.ref_(self).get_source_file_by_path(&path).is_some() {
            return true;
        }
        if contains(self.program.ref_(self).maybe_missing_file_paths().as_deref(), &path) {
            return false;
        }
        self.program.ref_(self).host().file_exists(f)
    }

    fn get_current_directory(&self) -> String {
        self.program.ref_(self).current_directory().clone()
    }

    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>> {
        self.program.ref_(self).redirect_targets_map()
    }

    fn get_file_include_reasons(&self) -> Gc<GcCell<MultiMap<Path, Id<FileIncludeReason>>>> {
        self.program.ref_(self).get_file_include_reasons()
    }

    fn is_get_nearest_ancestor_directory_with_package_json_supported(&self) -> bool {
        false
    }
}

impl SourceFileMayBeEmittedHost for ProgramEmitHost {
    fn get_compiler_options(&self) -> Id<CompilerOptions> {
        self.program.ref_(self).get_compiler_options()
    }

    fn is_source_file_from_external_library(&self, file: Id<Node> /*SourceFile*/) -> bool {
        self.program.ref_(self).is_source_file_from_external_library(file)
    }

    fn get_resolved_project_reference_to_redirect(
        &self,
        file_name: &str,
    ) -> Option<Gc<ResolvedProjectReference>> {
        self.program
            .ref_(self).get_resolved_project_reference_to_redirect(file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        self.program
            .ref_(self).is_source_of_project_reference_redirect_(file_name)
    }
}

impl ResolveModuleNameResolutionHost for ProgramEmitHost {
    fn get_canonical_file_name(&self, file_name: &str) -> String {
        self.program.ref_(self).get_canonical_file_name(file_name)
    }

    fn get_common_source_directory(&self) -> String {
        self.program.ref_(self).get_common_source_directory()
    }

    fn get_current_directory(&self) -> String {
        self.program.ref_(self).current_directory().clone()
    }
}

impl HasArena for ProgramEmitHost {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
pub struct EmitHostWriteFileCallback {
    host: Id<Box<dyn EmitHost>>,
}

impl EmitHostWriteFileCallback {
    pub fn new(host: Id<Box<dyn EmitHost>>) -> Self {
        Self { host }
    }
}

impl WriteFileCallback for EmitHostWriteFileCallback {
    fn call(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        source_files: Option<&[Id<Node /*SourceFile*/>]>,
    ) -> io::Result<()> {
        self.host.ref_(self).write_file(
            file_name,
            data,
            write_byte_order_mark,
            on_error,
            source_files,
        )
    }
}

impl HasArena for EmitHostWriteFileCallback {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Clone)]
pub(super) enum ResolveModuleNamesReusingOldStateResultItem {
    ResolvedModuleFull(Gc<ResolvedModuleFull>),
    PredictedToResolveToAmbientModuleMarker,
}

#[derive(Trace, Finalize)]
pub struct ProgramToPath {
    program: Id<Program>,
}

impl ProgramToPath {
    pub fn new(program: Id<Program>) -> Self {
        Self { program }
    }
}

impl ToPath for ProgramToPath {
    fn call(&self, file_name: &str) -> Path {
        self.program.ref_(self).to_path(file_name)
    }
}

impl HasArena for ProgramToPath {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}
