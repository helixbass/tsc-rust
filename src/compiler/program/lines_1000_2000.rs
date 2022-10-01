use std::cell::Ref;
use std::rc::Rc;

use super::filter_semantic_diagnostics;
use crate::{
    concatenate, create_type_checker, get_normalized_absolute_path, to_path as to_path_helper,
    CancellationTokenDebuggable, Comparison, CompilerOptions, CustomTransformers, Diagnostic,
    EmitResult, FileIncludeReason, MultiMap, Node, Path, Program, ResolvedModuleFull,
    ResolvedProjectReference, ResolvedTypeReferenceDirective, StringOrRcNode, StructureIsReused,
    TypeChecker, TypeCheckerHost, TypeReferenceDirectiveResolutionCache, WriteFileCallback,
};

impl Program {
    pub(super) fn resolve_module_names_worker(
        &self,
        module_names: &[String],
        containing_file: &Node, /*SourceFile*/
        reused_names: Option<&[String]>,
    ) -> Vec<Rc<ResolvedModuleFull>> {
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
        let result = self
            .actual_resolve_module_names_worker()
            .call(
                module_names,
                containing_file,
                &containing_file_name,
                reused_names,
                redirected_reference.as_deref(),
            )
            // TODO: it looked like actual_resolve_module_names_worker needs to "tell the truth"
            // and return Vec<Option<Rc<ResolvedModuleFull>>> so this should likely "bubble that
            // truth up" as the return type of .resolve_module_names_worker() here but for the
            // moment panicking if it finds a None
            .into_iter()
            .map(Option::unwrap)
            .collect();
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
