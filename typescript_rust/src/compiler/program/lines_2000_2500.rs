use regex::Regex;
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    add_emit_flags, compute_line_and_character_of_position, create_comment_directives_map,
    create_diagnostic_for_range, external_helpers_module_name_text, get_jsx_implicit_import_base,
    get_jsx_runtime_import, get_line_starts, is_check_js_enabled_for_file, is_external_module,
    is_source_file_js, normalize_path, set_parent, skip_type_checking,
    with_synthetic_factory_and_factory, CancellationTokenDebuggable, CommentDirective,
    CommentDirectivesMap, Debug_, Diagnostic, DiagnosticRelatedInformationInterface, Diagnostics,
    EmitFlags, FileIncludeReason, Node, NodeArray, NodeFlags, NodeInterface, Program, ScriptKind,
    SortedArray, SourceFileLike,
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
        if skip_type_checking(source_file, &self.options, |file_name: &str| {
            self.is_source_of_project_reference_redirect_(file_name)
        }) {
            return vec![];
        }

        let type_checker = self.get_diagnostics_producing_type_checker();

        let source_file_as_source_file = source_file.as_source_file();
        Debug_.assert(
            source_file_as_source_file
                .maybe_bind_diagnostics()
                .is_some(),
            None,
        );

        let is_check_js = is_check_js_enabled_for_file(source_file, &self.options);
        let is_ts_no_check = matches!(
            source_file_as_source_file.maybe_check_js_directive().as_ref(),
            Some(source_file_check_js_directive) if source_file_check_js_directive.enabled == false
        );
        let include_bind_and_check_diagnostics = !is_ts_no_check
            && (matches!(
                source_file_as_source_file.script_kind(),
                ScriptKind::TS | ScriptKind::TSX | ScriptKind::External | ScriptKind::Deferred
            ) || is_check_js);
        let bind_diagnostics = if include_bind_and_check_diagnostics {
            source_file_as_source_file.bind_diagnostics().clone()
        } else {
            vec![]
        };
        let check_diagnostics = if include_bind_and_check_diagnostics {
            type_checker.get_diagnostics(Some(source_file), cancellation_token)
        } else {
            vec![]
        };

        self.get_merged_bind_and_check_diagnostics(
            source_file,
            include_bind_and_check_diagnostics,
            &[
                Some(bind_diagnostics),
                Some(check_diagnostics),
                if is_check_js {
                    source_file_as_source_file
                        .maybe_js_doc_diagnostics()
                        .clone()
                } else {
                    None
                },
            ],
        )
        // })
    }

    pub(super) fn get_merged_bind_and_check_diagnostics(
        &self,
        source_file: &Node, /*SourceFile*/
        include_bind_and_check_diagnostics: bool,
        all_diagnostics: &[Option<Vec<Rc<Diagnostic>>>],
    ) -> Vec<Rc<Diagnostic>> {
        let flat_diagnostics = all_diagnostics
            .into_iter()
            .filter_map(|option| option.clone())
            .flatten()
            .collect::<Vec<_>>();
        let source_file_as_source_file = source_file.as_source_file();
        if !include_bind_and_check_diagnostics
            || !matches!(
                source_file_as_source_file.maybe_comment_directives().as_ref(),
                Some(source_file_comment_directives) if !source_file_comment_directives.is_empty()
            )
        {
            return flat_diagnostics;
        }

        let DiagnosticsWithPrecedingDirectives {
            mut diagnostics,
            directives,
        } = self.get_diagnostics_with_preceding_directives(
            source_file,
            source_file_as_source_file.comment_directives().as_ref(),
            &flat_diagnostics,
        );

        for error_expectation in directives.get_unused_expectations() {
            diagnostics.push(Rc::new(
                create_diagnostic_for_range(
                    source_file,
                    &error_expectation.range,
                    &Diagnostics::Unused_ts_expect_error_directive,
                )
                .into(),
            ));
        }

        diagnostics
    }

    pub(super) fn get_diagnostics_with_preceding_directives(
        &self,
        source_file: &Node, /*SourceFile*/
        comment_directives: &[Rc<CommentDirective>],
        flat_diagnostics: &[Rc<Diagnostic>],
    ) -> DiagnosticsWithPrecedingDirectives {
        let mut directives = create_comment_directives_map(source_file, comment_directives);
        let diagnostics: Vec<Rc<Diagnostic>> = flat_diagnostics
            .into_iter()
            .filter(|diagnostic| {
                self.mark_preceding_comment_directive_line(diagnostic, &mut directives)
                    .is_none()
            })
            .cloned()
            .collect();

        DiagnosticsWithPrecedingDirectives {
            diagnostics,
            directives,
        }
    }

    pub(super) fn mark_preceding_comment_directive_line(
        &self,
        diagnostic: &Diagnostic,
        directives: &mut CommentDirectivesMap,
    ) -> Option<usize> {
        let ref file = diagnostic.maybe_file()?;
        let start = diagnostic.start();

        let file_as_source_file = file.as_source_file();
        let ref line_starts = get_line_starts(file_as_source_file);
        let line =
            compute_line_and_character_of_position(line_starts, start.try_into().unwrap()).line;
        if line == 0 {
            return None;
        }
        let mut line = line - 1;
        let file_text_as_chars = file_as_source_file.text_as_chars();
        loop
        /*while (line >= 0)*/
        {
            if directives.mark_used(line) {
                return Some(line);
            }

            let line_text: String = if line == line_starts.len() - 1 {
                &file_text_as_chars[line_starts[line]..]
            } else {
                &file_text_as_chars[line_starts[line]..line_starts[line + 1]]
            }
            .into_iter()
            .collect();
            let line_text = line_text.trim();
            lazy_static! {
                static ref comment_regex: Regex = Regex::new(r"^(\s*)//(.*)$").unwrap();
            }
            if !line_text.is_empty() && !comment_regex.is_match(line_text) {
                return None;
            }

            if line == 0 {
                break;
            } else {
                line -= 1;
            }
        }

        None
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

pub struct DiagnosticsWithPrecedingDirectives {
    pub diagnostics: Vec<Rc<Diagnostic>>,
    pub directives: CommentDirectivesMap,
}
