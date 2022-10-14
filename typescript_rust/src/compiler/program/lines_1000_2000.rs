use std::cell::Ref;
use std::ptr;
use std::rc::Rc;

use super::filter_semantic_diagnostics;
use crate::{
    compare_values, concatenate, contains, contains_path, create_type_checker,
    file_extension_is_one_of, filter, get_base_file_name, get_common_source_directory,
    get_mode_for_resolution_at_index, get_normalized_absolute_path, get_resolved_module,
    is_trace_enabled, libs, map_defined, node_modules_path_part, out_file, package_id_to_string,
    remove_prefix, remove_suffix, skip_type_checking, source_file_may_be_emitted, string_contains,
    to_path as to_path_helper, trace, CancellationTokenDebuggable, Comparison, CompilerOptions,
    CustomTransformers, Debug_, Diagnostic, Diagnostics, EmitResult, Extension, FileIncludeReason,
    MultiMap, Node, Path, Program, ResolvedModuleFull, ResolvedProjectReference,
    ResolvedTypeReferenceDirective, ScriptReferenceHost, SourceOfProjectReferenceRedirect,
    StringOrRcNode, StructureIsReused, TypeChecker, TypeCheckerHost, WriteFileCallback,
};

impl Program {
    pub(super) fn resolve_module_names_worker(
        &self,
        module_names: &[String],
        containing_file: &Node, /*SourceFile*/
        reused_names: Option<&[String]>,
    ) -> Vec<Option<Rc<ResolvedModuleFull>>> {
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
    ) -> Option<Rc<ResolvedProjectReference>> {
        let source = self.get_source_of_project_reference_redirect(file_path);
        if let Some(SourceOfProjectReferenceRedirect::String(source)) = source.as_ref() {
            return self.get_resolved_project_reference_to_redirect(source);
        }
        if source.is_none() {
            return None;
        }
        self.for_each_resolved_project_reference(|resolved_ref: Rc<ResolvedProjectReference>| {
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
        self.get_source_file_by_path(&self.to_path(file_name))
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

    pub fn get_common_source_directory(&self) -> String {
        self.maybe_common_source_directory_mut()
            .get_or_insert_with(|| {
                let emitted_files = filter(&**self.files(), |file: &Rc<Node>| {
                    source_file_may_be_emitted(file, self, None)
                });
                get_common_source_directory(
                    &self.options,
                    || {
                        map_defined(Some(&emitted_files), |file: &Rc<Node>, _| {
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
    ) -> Vec<Option<Rc<ResolvedModuleFull>>> {
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
                let mut result: Vec<Option<Rc<ResolvedModuleFull>>> = vec![];
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

    pub fn get_resolved_project_references(
        &self,
    ) -> Ref<Option<Vec<Option<Rc<ResolvedProjectReference>>>>> {
        self.resolved_project_references.borrow()
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
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>> {
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
                &self.options,
            ),
            self.get_program_diagnostics(source_file),
        )
    }
}

#[derive(Clone)]
pub(super) enum ResolveModuleNamesReusingOldStateResultItem {
    ResolvedModuleFull(Rc<ResolvedModuleFull>),
    PredictedToResolveToAmbientModuleMarker,
}
