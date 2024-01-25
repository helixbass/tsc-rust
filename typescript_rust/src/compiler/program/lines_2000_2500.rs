use std::{convert::TryInto, io, ptr, rc::Rc};

use gc::{Gc, GcCell};
use id_arena::Id;
use regex::Regex;

use super::DiagnosticCache;
use crate::{
    add_emit_flags, append, compute_line_and_character_of_position, concatenate,
    create_comment_directives_map, create_diagnostic_for_node_in_source_file,
    create_diagnostic_for_range, create_file_diagnostic, external_helpers_module_name_text,
    for_each_child_recursively, get_declaration_diagnostics, get_external_module_name, get_factory,
    get_jsx_implicit_import_base, get_jsx_runtime_import, get_line_starts,
    get_text_of_identifier_or_literal, has_syntactic_modifier, is_ambient_module,
    is_any_import_or_re_export, is_check_js_enabled_for_file, is_external_module,
    is_external_module_name_relative, is_import_call, is_literal_import_type_node,
    is_module_declaration, is_require_call, is_source_file_js, is_string_literal,
    is_string_literal_like, normalize_path, set_parent, set_parent_recursive, skip_type_checking,
    sort_and_deduplicate_diagnostics, starts_with, token_to_string, CancellationTokenDebuggable,
    CommentDirective, CommentDirectivesMap, Debug_, Diagnostic, DiagnosticMessage,
    DiagnosticRelatedInformationInterface, DiagnosticWithLocation, Diagnostics, EmitFlags,
    FileIncludeReason, FileReference, ForEachChildRecursivelyCallbackReturn, GetOrInsertDefault,
    HasStatementsInterface, LiteralLikeNodeInterface, ModifierFlags, Node, NodeArray, NodeFlags,
    NodeInterface, Program, ReadonlyTextRange, ResolvedProjectReference, ScriptKind, SortedArray,
    SourceFileLike, SyntaxKind,
    HasArena, InArena,
};

impl Program {
    pub(super) fn get_bind_and_check_diagnostics_for_file(
        &self,
        source_file: Id<Node>, /*SourceFile*/
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<Vec<Id<Diagnostic>>> {
        self.try_get_and_cache_diagnostics(
            Some(source_file),
            cancellation_token,
            &mut self.cached_bind_and_check_diagnostics_for_file_mut(),
            |source_file, cancellation_token| {
                self.get_bind_and_check_diagnostics_for_file_no_cache(
                    source_file.unwrap(),
                    cancellation_token,
                )
            },
        )
    }

    pub(super) fn get_bind_and_check_diagnostics_for_file_no_cache(
        &self,
        source_file: Id<Node>, /*SourceFile*/
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<Vec<Id<Diagnostic>>> {
        // self.run_with_cancellation_token(|| {
        if skip_type_checking(&source_file.ref_(self), &self.options.ref_(self), |file_name: &str| {
            self.is_source_of_project_reference_redirect_(file_name)
        }) {
            return Ok(vec![]);
        }

        let type_checker = self.get_diagnostics_producing_type_checker()?;

        let source_file_ref = source_file.ref_(self);
        let source_file_as_source_file = source_file_ref.as_source_file();
        Debug_.assert(
            source_file_as_source_file
                .maybe_bind_diagnostics()
                .is_some(),
            None,
        );

        let is_check_js = is_check_js_enabled_for_file(&source_file.ref_(self), &self.options.ref_(self));
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
            type_checker.get_diagnostics(Some(source_file), cancellation_token)?
        } else {
            vec![]
        };

        Ok(self.get_merged_bind_and_check_diagnostics(
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
        ))
        // })
    }

    pub(super) fn get_merged_bind_and_check_diagnostics(
        &self,
        source_file: Id<Node>, /*SourceFile*/
        include_bind_and_check_diagnostics: bool,
        all_diagnostics: &[Option<Vec<Id<Diagnostic>>>],
    ) -> Vec<Id<Diagnostic>> {
        let flat_diagnostics = all_diagnostics
            .into_iter()
            .filter_map(|option| option.clone())
            .flatten()
            .collect::<Vec<_>>();
        let source_file_ref = source_file.ref_(self);
        let source_file_as_source_file = source_file_ref.as_source_file();
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
            diagnostics.push(self.alloc_diagnostic(
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
        source_file: Id<Node>, /*SourceFile*/
        comment_directives: &[Rc<CommentDirective>],
        flat_diagnostics: &[Id<Diagnostic>],
    ) -> DiagnosticsWithPrecedingDirectives {
        let mut directives = create_comment_directives_map(&source_file.ref_(self), comment_directives);
        let diagnostics: Vec<Id<Diagnostic>> = flat_diagnostics
            .into_iter()
            .filter(|diagnostic| {
                self.mark_preceding_comment_directive_line(&diagnostic.ref_(self), &mut directives)
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

        let file_ref = file.ref_(self);
        let file_as_source_file = file_ref.as_source_file();
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

    pub(super) fn get_js_syntactic_diagnostics_for_file(
        &self,
        source_file: Id<Node>, /*SourceFile*/
    ) -> Vec<Id<Diagnostic /*DiagnosticWithLocation*/>> {
        self.run_with_cancellation_token(|| {
            let diagnostics: Gc<GcCell<Vec<Id<Diagnostic /*DiagnosticWithLocation*/>>>> =
                Default::default();
            self.get_js_syntactic_diagnostics_for_file_walk(
                diagnostics.clone(),
                source_file,
                source_file,
                source_file,
            );
            for_each_child_recursively(
                source_file,
                {
                    let diagnostics = diagnostics.clone();
                    move |node: Id<Node>, parent: Id<Node>| {
                        self.get_js_syntactic_diagnostics_for_file_walk(
                            diagnostics.clone(),
                            source_file,
                            node,
                            parent,
                        )
                    }
                },
                {
                    let diagnostics = diagnostics.clone();
                    Some(move |nodes: &NodeArray, parent: Id<Node>| {
                        self.get_js_syntactic_diagnostics_for_file_walk_array(
                            diagnostics.clone(),
                            source_file,
                            nodes,
                            parent,
                        )
                    })
                },
                self,
            );

            let ret = (*diagnostics).borrow().clone();
            ret
        })
    }

    pub(super) fn get_js_syntactic_diagnostics_for_file_walk(
        &self,
        diagnostics: Gc<GcCell<Vec<Id<Diagnostic>>>>,
        source_file: Id<Node>,
        node: Id<Node>,
        parent: Id<Node>,
    ) -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
        match parent.ref_(self).kind() {
            SyntaxKind::Parameter
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::MethodDeclaration => {
                if parent.ref_(self).as_has_question_token().maybe_question_token() == Some(node) {
                    diagnostics.borrow_mut().push(self.alloc_diagnostic(
                        self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                            source_file,
                            node,
                            &Diagnostics::The_0_modifier_can_only_be_used_in_TypeScript_files,
                            Some(vec!["?".to_owned()]),
                        )
                        .into(),
                    ));
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
                if parent.ref_(self).as_has_type().maybe_type() == Some(node) {
                    diagnostics.borrow_mut().push(self.alloc_diagnostic(
                        self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                            source_file,
                            node,
                            &Diagnostics::Type_annotations_can_only_be_used_in_TypeScript_files,
                            None,
                        )
                        .into(),
                    ));
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
            }
            SyntaxKind::MethodSignature
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::FunctionExpression
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::ArrowFunction
            | SyntaxKind::VariableDeclaration => {
                if parent.ref_(self).as_has_type().maybe_type() == Some(node) {
                    diagnostics.borrow_mut().push(self.alloc_diagnostic(
                        self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                            source_file,
                            node,
                            &Diagnostics::Type_annotations_can_only_be_used_in_TypeScript_files,
                            None,
                        )
                        .into(),
                    ));
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
            }
            _ => (),
        }

        match node.ref_(self).kind() {
            SyntaxKind::ImportClause => {
                if node.ref_(self).as_import_clause().is_type_only {
                    diagnostics.borrow_mut().push(self.alloc_diagnostic(
                        self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                            source_file,
                            parent,
                            &Diagnostics::_0_declarations_can_only_be_used_in_TypeScript_files,
                            Some(vec!["import type".to_owned()]),
                        )
                        .into(),
                    ));
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
            }
            SyntaxKind::ExportDeclaration => {
                if node.ref_(self).as_export_declaration().is_type_only {
                    diagnostics.borrow_mut().push(self.alloc_diagnostic(
                        self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                            source_file,
                            node,
                            &Diagnostics::_0_declarations_can_only_be_used_in_TypeScript_files,
                            Some(vec!["export type".to_owned()]),
                        )
                        .into(),
                    ));
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
            }
            SyntaxKind::ImportEqualsDeclaration => {
                diagnostics.borrow_mut().push(self.alloc_diagnostic(
                    self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                        source_file,
                        node,
                        &Diagnostics::import_can_only_be_used_in_TypeScript_files,
                        None,
                    )
                    .into(),
                ));
                return Some(ForEachChildRecursivelyCallbackReturn::Skip);
            }
            SyntaxKind::ExportAssignment => {
                if node.ref_(self).as_export_assignment().is_export_equals == Some(true) {
                    diagnostics.borrow_mut().push(self.alloc_diagnostic(
                        self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                            source_file,
                            node,
                            &Diagnostics::export_can_only_be_used_in_TypeScript_files,
                            None,
                        )
                        .into(),
                    ));
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
            }
            SyntaxKind::HeritageClause => {
                let node_ref = node.ref_(self);
                let heritage_clause = node_ref.as_heritage_clause();
                if heritage_clause.token == SyntaxKind::ImplementsKeyword {
                    diagnostics.borrow_mut().push(self.alloc_diagnostic(
                        self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                            source_file,
                            node,
                            &Diagnostics::implements_clauses_can_only_be_used_in_TypeScript_files,
                            None,
                        )
                        .into(),
                    ));
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
            }
            SyntaxKind::InterfaceDeclaration => {
                let interface_keyword = token_to_string(SyntaxKind::InterfaceKeyword);
                Debug_.assert_is_defined(&interface_keyword, None);
                let interface_keyword = interface_keyword.unwrap();
                diagnostics.borrow_mut().push(self.alloc_diagnostic(
                    self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                        source_file,
                        node,
                        &Diagnostics::_0_declarations_can_only_be_used_in_TypeScript_files,
                        Some(vec![interface_keyword.to_owned()]),
                    )
                    .into(),
                ));
                return Some(ForEachChildRecursivelyCallbackReturn::Skip);
            }
            SyntaxKind::ModuleDeclaration => {
                let module_keyword = if node.ref_(self).flags().intersects(NodeFlags::Namespace) {
                    token_to_string(SyntaxKind::NamespaceKeyword)
                } else {
                    token_to_string(SyntaxKind::ModuleKeyword)
                };
                Debug_.assert_is_defined(&module_keyword, None);
                let module_keyword = module_keyword.unwrap();
                diagnostics.borrow_mut().push(self.alloc_diagnostic(
                    self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                        source_file,
                        node,
                        &Diagnostics::_0_declarations_can_only_be_used_in_TypeScript_files,
                        Some(vec![module_keyword.to_owned()]),
                    )
                    .into(),
                ));
                return Some(ForEachChildRecursivelyCallbackReturn::Skip);
            }
            SyntaxKind::TypeAliasDeclaration => {
                diagnostics.borrow_mut().push(self.alloc_diagnostic(
                    self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                        source_file,
                        node,
                        &Diagnostics::Type_aliases_can_only_be_used_in_TypeScript_files,
                        None,
                    )
                    .into(),
                ));
                return Some(ForEachChildRecursivelyCallbackReturn::Skip);
            }
            SyntaxKind::EnumDeclaration => {
                let enum_keyword =
                    Debug_.check_defined(token_to_string(SyntaxKind::EnumKeyword), None);
                diagnostics.borrow_mut().push(self.alloc_diagnostic(
                    self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                        source_file,
                        node,
                        &Diagnostics::_0_declarations_can_only_be_used_in_TypeScript_files,
                        Some(vec![enum_keyword.to_owned()]),
                    )
                    .into(),
                ));
                return Some(ForEachChildRecursivelyCallbackReturn::Skip);
            }
            SyntaxKind::NonNullExpression => {
                diagnostics.borrow_mut().push(self.alloc_diagnostic(
                    self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                        source_file,
                        node,
                        &Diagnostics::Non_null_assertions_can_only_be_used_in_TypeScript_files,
                        None,
                    )
                    .into(),
                ));
                return Some(ForEachChildRecursivelyCallbackReturn::Skip);
            }
            SyntaxKind::AsExpression => {
                diagnostics.borrow_mut().push(
                    self.alloc_diagnostic(self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                            source_file,
                        node.ref_(self).as_as_expression().type_,
                        &Diagnostics::Type_assertion_expressions_can_only_be_used_in_TypeScript_files,
                        None,
                    ).into())
                );
                return Some(ForEachChildRecursivelyCallbackReturn::Skip);
            }
            SyntaxKind::TypeAssertionExpression => {
                Debug_.fail(None);
            }
            _ => (),
        }
        None
    }

    pub(super) fn get_js_syntactic_diagnostics_for_file_walk_array(
        &self,
        diagnostics: Gc<GcCell<Vec<Id<Diagnostic>>>>,
        source_file: Id<Node>,
        nodes: &NodeArray,
        parent: Id<Node>,
    ) -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
        if matches!(
            parent.ref_(self).maybe_decorators().as_deref(),
            Some(parent_decorators) if ptr::eq(parent_decorators, nodes)
        ) && self.options.ref_(self).experimental_decorators != Some(true)
        {
            diagnostics.borrow_mut().push(
                self.alloc_diagnostic(self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                    source_file,
                    parent,
                    &Diagnostics::Experimental_support_for_decorators_is_a_feature_that_is_subject_to_change_in_a_future_release_Set_the_experimentalDecorators_option_in_your_tsconfig_or_jsconfig_to_remove_this_warning,
                    None,
                ).into())
            );
        }

        match parent.ref_(self).kind() {
            SyntaxKind::ClassDeclaration
            | SyntaxKind::ClassExpression
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::FunctionExpression
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::ArrowFunction => {
                if matches!(
                    parent.ref_(self).as_has_type_parameters().maybe_type_parameters().as_deref(),
                    Some(parent_type_parameters) if ptr::eq(nodes, parent_type_parameters)
                ) {
                    diagnostics.borrow_mut().push(
                        self.alloc_diagnostic(self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node_array(
                            source_file,
                            nodes,
                            &Diagnostics::Type_parameter_declarations_can_only_be_used_in_TypeScript_files,
                            None,
                        ).into())
                    );
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }

                if let Some(parent_modifiers) = parent
                    .ref_(self).maybe_modifiers()
                    .as_deref()
                    .filter(|parent_modifiers| ptr::eq(nodes, *parent_modifiers))
                {
                    self.get_js_syntactic_diagnostics_for_file_check_modifiers(
                        diagnostics.clone(),
                        source_file,
                        parent_modifiers,
                        parent.ref_(self).kind() == SyntaxKind::VariableStatement,
                    );
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
            }
            SyntaxKind::VariableStatement => {
                if let Some(parent_modifiers) = parent
                    .ref_(self).maybe_modifiers()
                    .as_deref()
                    .filter(|parent_modifiers| ptr::eq(nodes, *parent_modifiers))
                {
                    self.get_js_syntactic_diagnostics_for_file_check_modifiers(
                        diagnostics.clone(),
                        source_file,
                        parent_modifiers,
                        parent.ref_(self).kind() == SyntaxKind::VariableStatement,
                    );
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
            }
            SyntaxKind::PropertyDeclaration => {
                if matches!(
                    parent.ref_(self).maybe_modifiers().as_deref(),
                    Some(parent_modifiers) if ptr::eq(nodes, parent_modifiers)
                ) {
                    for &modifier in nodes {
                        if modifier.ref_(self).kind() != SyntaxKind::StaticKeyword {
                            diagnostics.borrow_mut().push(
                                self.alloc_diagnostic(self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                                    source_file,
                                    modifier,
                                    &Diagnostics::The_0_modifier_can_only_be_used_in_TypeScript_files,
                                    Some(vec![token_to_string(modifier.ref_(self).kind()).unwrap().to_owned()])
                                ).into())
                            );
                        }
                    }
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
            }
            SyntaxKind::Parameter => {
                if matches!(
                    parent.ref_(self).maybe_modifiers().as_deref(),
                    Some(parent_modifiers) if ptr::eq(nodes, parent_modifiers)
                ) {
                    diagnostics.borrow_mut().push(
                        self.alloc_diagnostic(self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node_array(
                            source_file,
                            nodes,
                            &Diagnostics::Parameter_modifiers_can_only_be_used_in_TypeScript_files,
                            None,
                        ).into())
                    );
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
            }
            SyntaxKind::CallExpression
            | SyntaxKind::NewExpression
            | SyntaxKind::ExpressionWithTypeArguments
            | SyntaxKind::JsxSelfClosingElement
            | SyntaxKind::JsxOpeningElement
            | SyntaxKind::TaggedTemplateExpression => {
                if matches!(
                    parent.ref_(self).as_has_type_arguments().maybe_type_arguments().as_deref(),
                    Some(parent_type_arguments) if ptr::eq(nodes, parent_type_arguments)
                ) {
                    diagnostics.borrow_mut().push(
                        self.alloc_diagnostic(self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node_array(
                            source_file,
                            nodes,
                            &Diagnostics::Type_arguments_can_only_be_used_in_TypeScript_files,
                            None,
                        ).into())
                    );
                    return Some(ForEachChildRecursivelyCallbackReturn::Skip);
                }
            }
            _ => (),
        }
        None
    }

    pub(super) fn get_js_syntactic_diagnostics_for_file_check_modifiers(
        &self,
        diagnostics: Gc<GcCell<Vec<Id<Diagnostic>>>>,
        source_file: Id<Node>,
        modifiers: &NodeArray, /*<Modifier>*/
        is_const_valid: bool,
    ) {
        for &modifier in modifiers {
            match modifier.ref_(self).kind() {
                SyntaxKind::ConstKeyword => {
                    if is_const_valid {
                        continue;
                    }

                    diagnostics.borrow_mut().push(self.alloc_diagnostic(
                        self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                            source_file,
                            modifier,
                            &Diagnostics::The_0_modifier_can_only_be_used_in_TypeScript_files,
                            Some(vec![token_to_string(modifier.ref_(self).kind()).unwrap().to_owned()]),
                        )
                        .into(),
                    ));
                }
                SyntaxKind::PublicKeyword
                | SyntaxKind::PrivateKeyword
                | SyntaxKind::ProtectedKeyword
                | SyntaxKind::ReadonlyKeyword
                | SyntaxKind::DeclareKeyword
                | SyntaxKind::AbstractKeyword
                | SyntaxKind::OverrideKeyword => {
                    diagnostics.borrow_mut().push(self.alloc_diagnostic(
                        self.get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
                            source_file,
                            modifier,
                            &Diagnostics::The_0_modifier_can_only_be_used_in_TypeScript_files,
                            Some(vec![token_to_string(modifier.ref_(self).kind()).unwrap().to_owned()]),
                        )
                        .into(),
                    ));
                }
                SyntaxKind::StaticKeyword
                | SyntaxKind::ExportKeyword
                | SyntaxKind::DefaultKeyword => (),
                _ => (),
            }
        }
    }

    pub(super) fn get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node_array(
        &self,
        source_file: Id<Node>,
        nodes: &NodeArray,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> DiagnosticWithLocation {
        let start = nodes.pos();
        create_file_diagnostic(source_file, start, nodes.end() - start, message, args)
    }

    pub(super) fn get_js_syntactic_diagnostics_for_file_create_diagnostic_for_node(
        &self,
        source_file: Id<Node>,
        node: Id<Node>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> DiagnosticWithLocation {
        create_diagnostic_for_node_in_source_file(source_file, node, message, args, self)
    }

    pub(super) fn get_declaration_diagnostics_worker(
        &self,
        source_file: Option<Id<Node>>, /*SourceFile*/
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<Vec<Id<Diagnostic /*DiagnosticWithLocation*/>>> {
        self.try_get_and_cache_diagnostics(
            source_file,
            cancellation_token,
            &mut self.cached_declaration_diagnostics_for_file_mut(),
            |source_file, cancellation_token| {
                self.get_declaration_diagnostics_for_file_no_cache(source_file, cancellation_token)
            },
        )
    }

    pub(super) fn get_declaration_diagnostics_for_file_no_cache(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<Vec<Id<Diagnostic /*DiagnosticWithLocation*/>>> {
        self.run_with_cancellation_token(|| -> io::Result<_> {
            let resolver = self
                .get_diagnostics_producing_type_checker()?
                .get_emit_resolver(source_file, cancellation_token)?;
            Ok(get_declaration_diagnostics(
                // TODO: should this be eg Some(NoOpWriteFileCallback::new()) instead?
                self.get_emit_host(None),
                resolver,
                source_file,
                self,
            )?
            .unwrap_or_default())
        })
    }

    #[allow(dead_code)]
    pub(super) fn get_and_cache_diagnostics(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
        cache: &mut DiagnosticCache,
        mut get_diagnostics: impl FnMut(
            Option<Id<Node>>, /*SourceFile*/
            Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
        ) -> Vec<Id<Diagnostic>>,
    ) -> Vec<Id<Diagnostic>> {
        self.try_get_and_cache_diagnostics(source_file, cancellation_token, cache, |a, b| {
            Ok(get_diagnostics(a, b))
        })
        .unwrap()
    }

    pub(super) fn try_get_and_cache_diagnostics(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
        cache: &mut DiagnosticCache,
        mut get_diagnostics: impl FnMut(
            Option<Id<Node>>, /*SourceFile*/
            Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
        ) -> io::Result<Vec<Id<Diagnostic>>>,
    ) -> io::Result<Vec<Id<Diagnostic>>> {
        let cached_result = if let Some(source_file) = source_file {
            cache.per_file.as_ref().and_then(|cache_per_file| {
                cache_per_file
                    .get(&*source_file.ref_(self).as_source_file().path())
                    .cloned()
            })
        } else {
            cache.all_diagnostics.clone()
        };

        if let Some(cached_result) = cached_result {
            return Ok(cached_result);
        }
        let result = get_diagnostics(source_file, cancellation_token)?;
        if let Some(source_file) = source_file {
            cache
                .per_file
                .get_or_insert_default_()
                .insert(source_file.ref_(self).as_source_file().path().clone(), result.clone());
        } else {
            cache.all_diagnostics = Some(result.clone());
        }
        Ok(result)
    }

    pub fn get_declaration_diagnostics_for_file(
        &self,
        source_file: Id<Node>, /*SourceFile*/
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<Vec<Id<Diagnostic /*DiagnosticWithLocation*/>>> {
        Ok(if source_file.ref_(self).as_source_file().is_declaration_file() {
            vec![]
        } else {
            self.get_declaration_diagnostics_worker(Some(source_file), cancellation_token)?
        })
    }

    pub fn get_options_diagnostics(
        &self,
        _cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> SortedArray<Id<Diagnostic>> {
        sort_and_deduplicate_diagnostics(&concatenate(
            self.program_diagnostics().get_global_diagnostics(),
            self.get_options_diagnostics_of_config_file(),
        ), self)
    }

    pub fn get_options_diagnostics_of_config_file(&self) -> Vec<Id<Diagnostic>> {
        let options_ref = self.options.ref_(self);
        let Some(options_config_file) = options_ref.config_file.as_ref() else {
            return vec![];
        };
        let mut diagnostics = self
            .program_diagnostics()
            .get_diagnostics(Some(&**options_config_file.ref_(self).as_source_file().file_name()));
        self.for_each_resolved_project_reference(
            |resolved_ref: Gc<ResolvedProjectReference>| -> Option<()> {
                diagnostics.append(&mut self.program_diagnostics().get_diagnostics(Some(
                    &**resolved_ref.source_file.ref_(self).as_source_file().file_name(),
                )));
                None
            },
        );
        diagnostics
    }

    pub fn get_global_diagnostics(
        &self,
        _cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<SortedArray<Id<Diagnostic>>> {
        Ok(if !self.root_names().is_empty() {
            sort_and_deduplicate_diagnostics(
                &self
                    .get_diagnostics_producing_type_checker()?
                    .get_global_diagnostics(),
                self,
            )
        } else {
            vec![].into()
        })
    }

    pub fn get_config_file_parsing_diagnostics(&self) -> Vec<Id<Diagnostic>> {
        self.maybe_config_file_parsing_diagnostics()
            .clone()
            .unwrap_or_else(|| vec![])
    }

    pub fn process_root_file(
        &self,
        file_name: &str,
        is_default_lib: bool,
        ignore_no_default_lib: bool,
        reason: Id<FileIncludeReason>,
    ) -> io::Result<()> {
        self.process_source_file(
            &normalize_path(file_name),
            is_default_lib,
            ignore_no_default_lib,
            None,
            reason,
        )?;

        Ok(())
    }

    pub fn file_reference_is_equal_to(&self, a: &FileReference, b: &FileReference) -> bool {
        a.file_name == b.file_name
    }

    pub fn module_name_is_equal_to(
        &self,
        a: Id<Node>, /*StringLiteralLike | Identifier*/
        b: Id<Node>, /*StringLiteralLike | Identifier*/
    ) -> bool {
        if a.ref_(self).kind() == SyntaxKind::Identifier {
            b.ref_(self).kind() == SyntaxKind::Identifier
                && a.ref_(self).as_identifier().escaped_text == b.ref_(self).as_identifier().escaped_text
        } else {
            b.ref_(self).kind() == SyntaxKind::StringLiteral
                && &*a.ref_(self).as_literal_like_node().text() == &*b.ref_(self).as_literal_like_node().text()
        }
    }

    pub fn create_synthetic_import(
        &self,
        text: &str,
        file: Id<Node>, /*SourceFile*/
    ) -> Id<Node> {
        let external_helpers_module_reference =
            get_factory().create_string_literal(text.to_owned(), None, None);
        let import_decl = get_factory().create_import_declaration(
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            None,
            external_helpers_module_reference.clone(),
            None,
        );
        add_emit_flags(import_decl, EmitFlags::NeverApplyImportHelper, self);
        set_parent(&external_helpers_module_reference.ref_(self), Some(import_decl));
        set_parent(&import_decl.ref_(self), Some(file));
        external_helpers_module_reference
            .ref_(self).set_flags(external_helpers_module_reference.ref_(self).flags() & !NodeFlags::Synthesized);
        import_decl.ref_(self).set_flags(import_decl.ref_(self).flags() & !NodeFlags::Synthesized);
        external_helpers_module_reference
    }

    pub fn collect_external_module_references(&self, file: Id<Node> /*SourceFile*/) {
        let file_ref = file.ref_(self);
        let file_as_source_file = file_ref.as_source_file();
        if file_as_source_file.maybe_imports().is_some() {
            return;
        }

        let is_java_script_file = is_source_file_js(&file.ref_(self));
        let is_external_module_file = is_external_module(&file.ref_(self));

        let mut imports: Option<Vec<Id<Node /*StringLiteralLike*/>>> = None;
        let mut module_augmentations: Option<Vec<Id<Node /*StringLiteral | Identifier*/>>> = None;
        let mut ambient_modules: Option<Vec<String>> = None;

        if (self.options.ref_(self).isolated_modules == Some(true) || is_external_module_file)
            && !file_as_source_file.is_declaration_file()
        {
            if self.options.ref_(self).import_helpers == Some(true) {
                imports =
                    Some(vec![self.create_synthetic_import(
                        external_helpers_module_name_text,
                        file,
                    )]);
            }
            let jsx_import = get_jsx_runtime_import(
                get_jsx_implicit_import_base(&self.options.ref_(self), Some(&file.ref_(self))).as_deref(),
                &self.options.ref_(self),
            );
            if let Some(jsx_import) = jsx_import
                .as_ref()
                .filter(|jsx_import| !jsx_import.is_empty())
            {
                imports
                    .get_or_insert_default_()
                    .push(self.create_synthetic_import(jsx_import, file));
            }
        }

        for &node in &file_as_source_file.statements() {
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
            .ref_(self).flags()
            .intersects(NodeFlags::PossiblyContainsDynamicImport)
            || is_java_script_file
        {
            self.collect_dynamic_import_or_require_calls(is_java_script_file, &mut imports, file);
        }

        *file_as_source_file.maybe_imports_mut() = Some(imports.unwrap_or_else(|| vec![]));
        *file_as_source_file.maybe_module_augmentations_mut() =
            Some(module_augmentations.unwrap_or_default());
        *file_as_source_file.maybe_ambient_module_names() =
            Some(ambient_modules.unwrap_or_default());
    }

    pub(super) fn collect_module_references(
        &self,
        imports: &mut Option<Vec<Id<Node>>>,
        file: Id<Node>,
        is_external_module_file: bool,
        module_augmentations: &mut Option<Vec<Id<Node>>>,
        ambient_modules: &mut Option<Vec<String>>,
        node: Id<Node>, /*Statement*/
        in_ambient_module: bool,
    ) {
        if is_any_import_or_re_export(&node.ref_(self)) {
            let module_name_expr = get_external_module_name(node, self);
            if let Some(module_name_expr) = module_name_expr.filter(|module_name_expr| {
                is_string_literal(&module_name_expr.ref_(self)) && {
                    let module_name_expr_ref = module_name_expr.ref_(self);
                    let module_name_text = module_name_expr_ref.as_string_literal().text();
                    !module_name_text.is_empty()
                        && (!in_ambient_module
                            || !is_external_module_name_relative(&module_name_text))
                }
            }) {
                set_parent_recursive(Some(node), false, self);
                append(
                    imports.get_or_insert_default_(),
                    Some(module_name_expr.clone()),
                );
                if !self.uses_uri_style_node_core_modules()
                    && self.current_node_modules_depth() == 0
                    && !file.ref_(self).as_source_file().is_declaration_file()
                {
                    self.set_uses_uri_style_node_core_modules(starts_with(
                        &module_name_expr.ref_(self).as_string_literal().text(),
                        "node:",
                    ));
                }
            }
        } else if is_module_declaration(&node.ref_(self)) {
            if is_ambient_module(node, self)
                && (in_ambient_module
                    || has_syntactic_modifier(node, ModifierFlags::Ambient, self)
                    || file.ref_(self).as_source_file().is_declaration_file())
            {
                let node_name = node.ref_(self).as_named_declaration().name();
                node_name.ref_(self).set_parent(Some(node));
                let node_name_ref = node_name.ref_(self);
                let name_text = get_text_of_identifier_or_literal(&node_name_ref);
                if is_external_module_file
                    || (in_ambient_module && !is_external_module_name_relative(&name_text))
                {
                    module_augmentations
                        .get_or_insert_default_()
                        .push(node_name);
                } else if !in_ambient_module {
                    if file.ref_(self).as_source_file().is_declaration_file() {
                        ambient_modules
                            .get_or_insert_default_()
                            .push(name_text.into_owned());
                    }
                    let body = node.ref_(self).as_module_declaration().body;
                    if let Some(body) = body {
                        for &statement in &body.ref_(self).as_module_block().statements {
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
        imports: &mut Option<Vec<Id<Node>>>,
        file: Id<Node>, /*SourceFile*/
    ) {
        lazy_static! {
            static ref r: Regex = Regex::new(r"import|require").unwrap();
        }
        for match_ in r.find_iter(&file.ref_(self).as_source_file().text()) {
            let node = self.get_node_at_position(
                is_java_script_file,
                file,
                // TODO: I think this needs to use "char count" rather than "byte count" somehow?
                match_.end().try_into().unwrap(),
            );
            if is_java_script_file && is_require_call(node, true, self) {
                set_parent_recursive(Some(node), false, self);
                if let Some(node_arguments_0) = node.ref_(self).as_call_expression().arguments.get(0).cloned()
                {
                    append(imports.get_or_insert_default_(), Some(node_arguments_0));
                }
            } else if is_import_call(node, self) && {
                let node_ref = node.ref_(self);
                let node_arguments = &node_ref.as_call_expression().arguments;
                node_arguments.len() >= 1 && is_string_literal_like(&node_arguments[0].ref_(self))
            } {
                set_parent_recursive(Some(node), false, self);
                if let Some(node_arguments_0) = node.ref_(self).as_call_expression().arguments.get(0).cloned()
                {
                    append(imports.get_or_insert_default_(), Some(node_arguments_0));
                }
            } else if is_literal_import_type_node(node, self) {
                set_parent_recursive(Some(node), false, self);
                append(
                    imports.get_or_insert_default_(),
                    Some(
                        node.ref_(self).as_import_type_node()
                            .argument
                            .ref_(self).as_literal_type_node()
                            .literal
                            .clone(),
                    ),
                );
            }
        }
    }

    pub(super) fn get_containing_child(
        &self,
        position: isize,
        child: Id<Node>,
    ) -> Option<Id<Node>> {
        if child.ref_(self).pos() <= position
            && (position < child.ref_(self).end()
                || position == child.ref_(self).end() && child.ref_(self).kind() == SyntaxKind::EndOfFileToken)
        {
            return Some(child);
        }
        None
    }
}

pub struct DiagnosticsWithPrecedingDirectives {
    pub diagnostics: Vec<Id<Diagnostic>>,
    pub directives: CommentDirectivesMap,
}
