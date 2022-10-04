use std::rc::Rc;

use crate::{
    add_emit_flags, external_helpers_module_name_text, get_jsx_implicit_import_base,
    get_jsx_runtime_import, is_external_module, is_source_file_js, normalize_path, set_parent,
    with_synthetic_factory_and_factory, CancellationTokenDebuggable, Diagnostic, EmitFlags,
    FileIncludeReason, Node, NodeArray, NodeFlags, NodeInterface, Program, SortedArray,
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
        // unimplemented!()
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

        *file_as_source_file.maybe_imports_mut() = Some(imports.unwrap_or_else(|| vec![]));
        *file_as_source_file.maybe_module_augmentations() =
            Some(module_augmentations.unwrap_or_else(|| vec![]));
        *file_as_source_file.maybe_ambient_module_names() =
            Some(ambient_modules.unwrap_or_else(|| vec![]));
    }
}
