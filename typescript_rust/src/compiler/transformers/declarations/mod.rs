use std::{
    borrow::{Borrow, Cow},
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    mem, ptr,
};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use crate::{
    add_related_info_rc, are_option_gcs_equal, can_produce_diagnostics,
    compiler::scanner::skip_trivia, contains_comparer, create_diagnostic_for_node,
    create_empty_exports, create_get_symbol_accessibility_diagnostic_for_node,
    create_unparsed_source_file, declaration_name_to_string, filter, gc_cell_ref_mut_unwrapped,
    gc_cell_ref_unwrapped, get_directory_path, get_factory, get_leading_comment_ranges,
    get_leading_comment_ranges_of_node, get_name_of_declaration, get_original_node_id,
    get_output_paths_for, get_parse_tree_node, get_relative_path_to_directory_or_url,
    get_resolved_external_module_name, get_set_accessor_value_parameter, get_source_file_of_node,
    get_text_of_node, get_this_parameter, get_trailing_comment_ranges, has_effective_modifier,
    has_extension, is_any_import_syntax, is_binding_pattern, is_class_declaration,
    is_export_assignment, is_external_module, is_external_module_reference,
    is_external_or_common_js_module, is_function_declaration, is_function_like,
    is_import_declaration, is_import_equals_declaration, is_index_signature_declaration,
    is_interface_declaration, is_json_source_file, is_mapped_type_node, is_module_declaration,
    is_omitted_expression, is_set_accessor_declaration, is_source_file, is_source_file_js,
    is_source_file_not_json, is_string_literal, is_string_literal_like, is_type_alias_declaration,
    is_unparsed_source, last, map, map_defined, maybe_concatenate, maybe_filter, maybe_for_each,
    maybe_for_each_bool, maybe_map, module_specifiers, normalize_slashes,
    path_contains_node_modules, path_is_relative, push_if_unique_gc, set_text_range_node_array,
    some, starts_with, string_contains, to_file_name_lower_case, to_path, transform_nodes,
    visit_node, visit_nodes, with_synthetic_factory, AllAccessorDeclarations,
    BaseNodeFactorySynthetic, CommentRange, CompilerOptions, Debug_, Diagnostic, Diagnostics,
    EmitHost, EmitResolver, FileReference, FunctionLikeDeclarationInterface,
    GetSymbolAccessibilityDiagnostic, GetSymbolAccessibilityDiagnosticInterface,
    HasInitializerInterface, HasStatementsInterface, HasTypeInterface, LiteralLikeNodeInterface,
    ModifierFlags, ModuleSpecifierResolutionHostAndGetCommonSourceDirectory,
    NamedDeclarationInterface, Node, NodeArray, NodeBuilderFlags, NodeFactory, NodeId,
    NodeInterface, NonEmpty, ReadonlyTextRange, ScriptReferenceHost, SourceFileLike, Symbol,
    SymbolAccessibility, SymbolAccessibilityDiagnostic, SymbolAccessibilityResult, SymbolFlags,
    SymbolInterface, SymbolTracker, SyntaxKind, TextRange, TransformationContext,
    TransformationResult, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface, VisitResult,
};

pub mod diagnostics;

pub fn get_declaration_diagnostics(
    host: Gc<Box<dyn EmitHost>>,
    resolver: Gc<Box<dyn EmitResolver>>,
    file: Option<&Node /*SourceFile*/>,
) -> Option<Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>>> {
    let compiler_options = ScriptReferenceHost::get_compiler_options(&**host);
    let result = transform_nodes(
        Some(resolver),
        Some(host.clone()),
        get_factory(),
        compiler_options,
        &if let Some(file) = file {
            vec![file.node_wrapper()]
        } else {
            filter(&host.get_source_files(), |source_file: &Gc<Node>| {
                is_source_file_not_json(source_file)
            })
        },
        &[transform_declarations()],
        false,
    );
    result.diagnostics()
}

fn has_internal_annotation(
    range: &CommentRange,
    current_source_file: &Node, /*SourceFile*/
) -> bool {
    let current_source_file_text = current_source_file.as_source_file().text();
    let comment =
        &current_source_file_text[range.pos().try_into().unwrap()..range.end().try_into().unwrap()];
    string_contains(comment, "@internal")
}

pub fn is_internal_declaration(
    node: &Node,
    current_source_file: &Node, /*SourceFile*/
) -> bool {
    let parse_tree_node = get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None);
    if let Some(parse_tree_node) = parse_tree_node
        .as_ref()
        .filter(|parse_tree_node| parse_tree_node.kind() == SyntaxKind::Parameter)
    {
        let param_idx = parse_tree_node
            .parent()
            .as_signature_declaration()
            .parameters()
            .into_iter()
            .position(|parameter: &Gc<Node>| Gc::ptr_eq(parameter, parse_tree_node));
        let previous_sibling = if let Some(param_idx) = param_idx.filter(|param_idx| *param_idx > 0)
        {
            parse_tree_node
                .parent()
                .as_signature_declaration()
                .parameters()
                .get(param_idx - 1)
                .cloned()
        } else {
            None
        };
        let text = current_source_file.as_source_file().text_as_chars();
        let comment_ranges = if let Some(previous_sibling) = previous_sibling.as_ref() {
            maybe_concatenate(
                get_trailing_comment_ranges(
                    &text,
                    skip_trivia(
                        &text,
                        previous_sibling.end() + 1,
                        Some(false),
                        Some(true),
                        None,
                    ),
                ),
                get_leading_comment_ranges(&text, node.pos()),
            )
        } else {
            get_trailing_comment_ranges(
                &text,
                skip_trivia(&text, node.pos(), Some(false), Some(true), None),
            )
        };
        return matches!(
            comment_ranges,
            Some(comment_ranges) if !comment_ranges.is_empty() &&
                has_internal_annotation(
                    last(&comment_ranges),
                    current_source_file
                )
        );
    }
    let leading_comment_ranges = parse_tree_node.and_then(|ref parse_tree_node| {
        get_leading_comment_ranges_of_node(parse_tree_node, current_source_file)
    });
    maybe_for_each_bool(
        leading_comment_ranges.as_ref(),
        |range: &CommentRange, _| has_internal_annotation(range, current_source_file),
    )
}

fn declaration_emit_node_builder_flags() -> NodeBuilderFlags {
    NodeBuilderFlags::MultilineObjectLiterals
        | NodeBuilderFlags::WriteClassExpressionAsTypeLiteral
        | NodeBuilderFlags::UseTypeOfFunction
        | NodeBuilderFlags::UseStructuralFallback
        | NodeBuilderFlags::AllowEmptyTuple
        | NodeBuilderFlags::GenerateNamesForShadowedTypeParams
        | NodeBuilderFlags::NoTruncation
}

#[derive(Trace, Finalize)]
struct TransformDeclarations {
    _transformer_wrapper: GcCell<Option<Transformer>>,
    context: Gc<Box<dyn TransformationContext>>,
    get_symbol_accessibility_diagnostic: GcCell<GetSymbolAccessibilityDiagnostic>,
    #[unsafe_ignore_trace]
    needs_declare: Cell<bool>,
    #[unsafe_ignore_trace]
    is_bundled_emit: Cell<bool>,
    #[unsafe_ignore_trace]
    result_has_external_module_indicator: Cell<bool>,
    #[unsafe_ignore_trace]
    needs_scope_fix_marker: Cell<bool>,
    #[unsafe_ignore_trace]
    result_has_scope_marker: Cell<bool>,
    enclosing_declaration: GcCell<Option<Gc<Node>>>,
    #[unsafe_ignore_trace]
    necessary_type_references: RefCell<Option<HashSet<String>>>,
    late_marked_statements: GcCell<Option<Vec<Gc<Node /*LateVisibilityPaintedStatement*/>>>>,
    late_statement_replacement_map: GcCell<
        Option<
            HashMap<
                NodeId,
                VisitResult, /*<LateVisibilityPaintedStatement | ExportAssignment*/
            >,
        >,
    >,
    #[unsafe_ignore_trace]
    suppress_new_diagnostic_contexts: Cell<Option<bool>>,
    exported_modules_from_declaration_emit: GcCell<Option<Vec<Gc<Symbol>>>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    host: Gc<Box<dyn EmitHost>>,
    symbol_tracker: GcCell<Option<Gc<Box<dyn SymbolTracker>>>>,
    error_name_node: GcCell<Option<Gc<Node /*DeclarationName*/>>>,
    error_fallback_node: GcCell<Option<Gc<Node /*Declaration*/>>>,
    current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    refs: GcCell<Option<HashMap<NodeId, Gc<Node /*SourceFile*/>>>>,
    libs: GcCell<Option<HashMap<String, bool>>>,
    emitted_imports: GcCell<Option<Vec<Gc<Node /*AnyImportSyntax*/>>>>,
    resolver: Gc<Box<dyn EmitResolver>>,
    options: Gc<CompilerOptions>,
    no_resolve: Option<bool>,
    strip_internal: Option<bool>,
}

impl TransformDeclarations {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let options = context.get_compiler_options();
        let host = context.get_emit_host();
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            get_symbol_accessibility_diagnostic: GcCell::new(throw_diagnostic()),
            needs_declare: Cell::new(true),
            is_bundled_emit: Cell::new(false),
            result_has_external_module_indicator: Cell::new(false),
            needs_scope_fix_marker: Cell::new(false),
            result_has_scope_marker: Cell::new(false),
            enclosing_declaration: Default::default(),
            necessary_type_references: Default::default(),
            late_marked_statements: Default::default(),
            late_statement_replacement_map: Default::default(),
            suppress_new_diagnostic_contexts: Default::default(),
            exported_modules_from_declaration_emit: Default::default(),
            factory: context.factory(),
            host: host.clone(),
            symbol_tracker: Default::default(),
            error_name_node: Default::default(),
            error_fallback_node: Default::default(),
            current_source_file: Default::default(),
            refs: Default::default(),
            libs: Default::default(),
            emitted_imports: Default::default(),
            resolver: context.get_emit_resolver(),
            no_resolve: options.no_resolve,
            strip_internal: options.strip_internal,
            options,
            context,
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        *downcasted.symbol_tracker.borrow_mut() = Some(Gc::new(Box::new(
            TransformDeclarationsSymbolTracker::new(downcasted.clone(), host),
        )));
        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    fn get_symbol_accessibility_diagnostic(&self) -> GetSymbolAccessibilityDiagnostic {
        self.get_symbol_accessibility_diagnostic.borrow().clone()
    }

    fn set_get_symbol_accessibility_diagnostic(
        &self,
        get_symbol_accessibility_diagnostic: GetSymbolAccessibilityDiagnostic,
    ) {
        *self.get_symbol_accessibility_diagnostic.borrow_mut() =
            get_symbol_accessibility_diagnostic;
    }

    fn set_needs_declare(&self, needs_declare: bool) {
        self.needs_declare.set(needs_declare);
    }

    fn is_bundled_emit(&self) -> bool {
        self.is_bundled_emit.get()
    }

    fn set_is_bundled_emit(&self, is_bundled_emit: bool) {
        self.is_bundled_emit.set(is_bundled_emit);
    }

    fn result_has_external_module_indicator(&self) -> bool {
        self.result_has_external_module_indicator.get()
    }

    fn set_result_has_external_module_indicator(&self, result_has_external_module_indicator: bool) {
        self.result_has_external_module_indicator
            .set(result_has_external_module_indicator);
    }

    fn needs_scope_fix_marker(&self) -> bool {
        self.needs_scope_fix_marker.get()
    }

    fn set_needs_scope_fix_marker(&self, needs_scope_fix_marker: bool) {
        self.needs_scope_fix_marker.set(needs_scope_fix_marker);
    }

    fn result_has_scope_marker(&self) -> bool {
        self.result_has_scope_marker.get()
    }

    fn set_result_has_scope_marker(&self, result_has_scope_marker: bool) {
        self.result_has_scope_marker.set(result_has_scope_marker);
    }

    fn enclosing_declaration(&self) -> Gc<Node> {
        self.enclosing_declaration.borrow().clone().unwrap()
    }

    fn set_enclosing_declaration(&self, enclosing_declaration: Option<Gc<Node>>) {
        *self.enclosing_declaration.borrow_mut() = enclosing_declaration;
    }

    fn maybe_necessary_type_references(&self) -> Ref<Option<HashSet<String>>> {
        self.necessary_type_references.borrow()
    }

    fn maybe_necessary_type_references_mut(&self) -> RefMut<Option<HashSet<String>>> {
        self.necessary_type_references.borrow_mut()
    }

    fn set_necessary_type_references(&self, necessary_type_references: Option<HashSet<String>>) {
        *self.necessary_type_references.borrow_mut() = necessary_type_references;
    }

    fn maybe_late_marked_statements_mut(&self) -> GcCellRefMut<Option<Vec<Gc<Node>>>> {
        self.late_marked_statements.borrow_mut()
    }

    fn set_late_marked_statements(&self, late_marked_statements: Option<Vec<Gc<Node>>>) {
        *self.late_marked_statements.borrow_mut() = late_marked_statements;
    }

    fn set_late_statement_replacement_map(
        &self,
        late_statement_replacement_map: Option<HashMap<NodeId, VisitResult>>,
    ) {
        *self.late_statement_replacement_map.borrow_mut() = late_statement_replacement_map;
    }

    fn maybe_suppress_new_diagnostic_contexts(&self) -> Option<bool> {
        self.suppress_new_diagnostic_contexts.get()
    }

    fn set_suppress_new_diagnostic_contexts(&self, suppress_new_diagnostic_contexts: Option<bool>) {
        self.suppress_new_diagnostic_contexts
            .set(suppress_new_diagnostic_contexts);
    }

    fn maybe_exported_modules_from_declaration_emit(&self) -> GcCellRef<Option<Vec<Gc<Symbol>>>> {
        self.exported_modules_from_declaration_emit.borrow()
    }

    fn maybe_exported_modules_from_declaration_emit_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Symbol>>>> {
        self.exported_modules_from_declaration_emit.borrow_mut()
    }

    fn symbol_tracker(&self) -> Gc<Box<dyn SymbolTracker>> {
        self.symbol_tracker.borrow().clone().unwrap()
    }

    fn maybe_error_name_node(&self) -> Option<Gc<Node>> {
        self.error_name_node.borrow().clone()
    }

    fn set_error_name_node(&self, error_name_node: Option<Gc<Node>>) {
        *self.error_name_node.borrow_mut() = error_name_node;
    }

    fn maybe_error_fallback_node(&self) -> Option<Gc<Node>> {
        self.error_fallback_node.borrow().clone()
    }

    fn current_source_file(&self) -> Gc<Node> {
        self.current_source_file.borrow().clone().unwrap()
    }

    fn set_current_source_file(&self, current_source_file: Option<Gc<Node>>) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    fn refs(&self) -> GcCellRef<HashMap<NodeId, Gc<Node>>> {
        gc_cell_ref_unwrapped(&self.refs)
    }

    fn refs_mut(
        &self,
    ) -> GcCellRefMut<Option<HashMap<NodeId, Gc<Node>>>, HashMap<NodeId, Gc<Node>>> {
        gc_cell_ref_mut_unwrapped(&self.refs)
    }

    fn set_refs(&self, refs: Option<HashMap<NodeId, Gc<Node>>>) {
        *self.refs.borrow_mut() = refs;
    }

    fn libs(&self) -> GcCellRef<HashMap<String, bool>> {
        gc_cell_ref_unwrapped(&self.libs)
    }

    fn libs_mut(&self) -> GcCellRefMut<Option<HashMap<String, bool>>, HashMap<String, bool>> {
        gc_cell_ref_mut_unwrapped(&self.libs)
    }

    fn set_libs(&self, libs: Option<HashMap<String, bool>>) {
        *self.libs.borrow_mut() = libs;
    }

    fn maybe_emitted_imports(&self) -> GcCellRef<Option<Vec<Gc<Node>>>> {
        self.emitted_imports.borrow()
    }

    fn set_emitted_imports(&self, emitted_imports: Option<Vec<Gc<Node>>>) {
        *self.emitted_imports.borrow_mut() = emitted_imports;
    }

    fn record_type_reference_directives_if_necessary(
        &self,
        type_reference_directives: Option<&[String]>,
    ) {
        if type_reference_directives.is_none() {
            return;
        }
        let type_reference_directives = type_reference_directives.unwrap();
        let mut necessary_type_references = self.maybe_necessary_type_references_mut();
        let necessary_type_references =
            necessary_type_references.get_or_insert_with(|| Default::default());
        for ref_ in type_reference_directives {
            necessary_type_references.insert(ref_.clone());
        }
    }

    fn handle_symbol_accessibility_error(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> bool {
        if symbol_accessibility_result.accessibility == SymbolAccessibility::Accessible {
            if
            /*symbolAccessibilityResult &&*/
            let Some(symbol_accessibility_result_aliases_to_make_visible) =
                symbol_accessibility_result.aliases_to_make_visible.as_ref()
            {
                let mut late_marked_statements = self.maybe_late_marked_statements_mut();
                if late_marked_statements.is_none() {
                    *late_marked_statements =
                        Some(symbol_accessibility_result_aliases_to_make_visible.clone());
                } else {
                    let late_marked_statements = late_marked_statements.as_mut().unwrap();
                    for ref_ in symbol_accessibility_result_aliases_to_make_visible {
                        push_if_unique_gc(late_marked_statements, ref_);
                    }
                }
            }
        } else {
            let error_info = self
                .get_symbol_accessibility_diagnostic()
                .call(symbol_accessibility_result);
            if let Some(error_info) = error_info {
                if let Some(error_info_type_name) = error_info.type_name.as_ref() {
                    let mut args: Vec<String> =
                        vec![get_text_of_node(error_info_type_name, None).into_owned()];
                    if let Some(symbol_accessibility_result_error_symbol_name) =
                        symbol_accessibility_result.error_symbol_name.clone()
                    {
                        args.push(symbol_accessibility_result_error_symbol_name);
                    }
                    if let Some(symbol_accessibility_result_error_module_name) =
                        symbol_accessibility_result.error_module_name.clone()
                    {
                        args.push(symbol_accessibility_result_error_module_name);
                    }
                    self.context.add_diagnostic(
                        create_diagnostic_for_node(
                            symbol_accessibility_result
                                .error_node
                                .as_deref()
                                .unwrap_or(&*error_info.error_node),
                            error_info.diagnostic_message,
                            Some(args),
                        )
                        .into(),
                    );
                } else {
                    let mut args: Vec<String> = vec![];
                    if let Some(symbol_accessibility_result_error_symbol_name) =
                        symbol_accessibility_result.error_symbol_name.clone()
                    {
                        args.push(symbol_accessibility_result_error_symbol_name);
                    }
                    if let Some(symbol_accessibility_result_error_module_name) =
                        symbol_accessibility_result.error_module_name.clone()
                    {
                        args.push(symbol_accessibility_result_error_module_name);
                    }
                    self.context.add_diagnostic(
                        create_diagnostic_for_node(
                            symbol_accessibility_result
                                .error_node
                                .as_deref()
                                .unwrap_or(&*error_info.error_node),
                            error_info.diagnostic_message,
                            Some(args),
                        )
                        .into(),
                    );
                }
                return true;
            }
        }
        false
    }

    fn transform_declarations_for_js(
        &self,
        source_file: &Node, /*SourceFile*/
        bundled: Option<bool>,
    ) -> Option<Vec<Gc<Node>>> {
        let old_diag = self.get_symbol_accessibility_diagnostic();
        self.set_get_symbol_accessibility_diagnostic(Gc::new(Box::new(
            TransformDeclarationsForJSGetSymbolAccessibilityDiagnostic::new(
                source_file.node_wrapper(),
            ),
        )));
        let result = self.resolver.get_declaration_statements_for_source_file(
            source_file,
            declaration_emit_node_builder_flags(),
            &**self.symbol_tracker(),
            bundled,
        );
        self.set_get_symbol_accessibility_diagnostic(old_diag);
        result
    }

    fn transform_root(&self, node: &Node /*SourceFile | Bundle*/) -> Gc<Node> {
        if node.kind() == SyntaxKind::SourceFile && node.as_source_file().is_declaration_file() {
            return node.node_wrapper();
        }

        if node.kind() == SyntaxKind::Bundle {
            let node_as_bundle = node.as_bundle();
            self.set_is_bundled_emit(true);
            self.set_refs(Some(HashMap::new()));
            self.set_libs(Some(HashMap::new()));
            let mut has_no_default_lib = false;
            let mut bundle = with_synthetic_factory(|synthetic_factory_| {
                self.factory.create_bundle(
                    synthetic_factory_,
                    map(
                        &node_as_bundle.source_files,
                        |source_file: &Option<Gc<Node>>, _| -> Option<Gc<Node>> {
                            let source_file = source_file.as_ref().unwrap();
                            let source_file_as_source_file = source_file.as_source_file();
                            if source_file_as_source_file.is_declaration_file() {
                                return None
                            }
                            has_no_default_lib = has_no_default_lib || source_file_as_source_file.has_no_default_lib();
                            self.set_current_source_file(
                                Some(source_file.clone())
                            );
                            self.set_enclosing_declaration(
                                Some(source_file.clone())
                            );
                            self.set_late_marked_statements(None);
                            self.set_suppress_new_diagnostic_contexts(Some(false));
                            self.set_late_statement_replacement_map(
                                Some(HashMap::new())
                            );
                            self.set_get_symbol_accessibility_diagnostic(throw_diagnostic());
                            self.set_needs_scope_fix_marker(false);
                            self.set_result_has_scope_marker(false);
                            self.collect_references(
                                source_file,
                                &mut self.refs_mut()
                            );
                            self.collect_libs(
                                source_file,
                                &mut self.libs_mut()
                            );
                            if is_external_or_common_js_module(source_file) || is_json_source_file(source_file) {
                                self.set_result_has_external_module_indicator(false);
                                self.set_needs_declare(false);
                                let statements = if is_source_file_js(source_file) {
                                    self.factory.create_node_array(
                                        self.transform_declarations_for_js(
                                            source_file,
                                            Some(true)
                                        ),
                                        None,
                                    )
                                } else {
                                    visit_nodes(
                                        Some(source_file_as_source_file.statements()),
                                        Some(|node: &Node| self.visit_declaration_statements(node)),
                                        Option::<fn(&Node) -> bool>::None,
                                        None,
                                        None,
                                    ).unwrap()
                                };
                                let new_file = with_synthetic_factory(|synthetic_factory_| {
                                    self.factory.update_source_file(
                                        synthetic_factory_,
                                        source_file,
                                        vec![
                                            self.factory.create_module_declaration(
                                                synthetic_factory_,
                                                Some(vec![]),
                                                Some(vec![
                                                    self.factory.create_modifier(
                                                        synthetic_factory_,
                                                        SyntaxKind::DeclareKeyword,
                                                    ).into()
                                                ]),
                                                self.factory.create_string_literal(
                                                    synthetic_factory_,
                                                    get_resolved_external_module_name(
                                                        &**self.context.get_emit_host(),
                                                        source_file,
                                                        Option::<&Node>::None,
                                                    ),
                                                    None,
                                                    None,
                                                ).into(),
                                                Some(
                                                    self.factory.create_module_block(
                                                        synthetic_factory_,
                                                        Some(
                                                            set_text_range_node_array(
                                                                self.factory.create_node_array(
                                                                    Some(self.transform_and_replace_late_painted_statements(
                                                                        &statements,
                                                                    )),
                                                                    None,
                                                                ),
                                                                Some(source_file_as_source_file.statements()),
                                                            )
                                                        )
                                                    ).into()
                                                ),
                                                None,
                                            ).into()
                                        ],
                                        Some(true),
                                        Some(vec![]),
                                        Some(vec![]),
                                        Some(false),
                                        Some(vec![]),
                                    )
                                });
                                return Some(new_file);
                            }
                            self.set_needs_declare(true);
                            let updated = if is_source_file_js(source_file) {
                                self.factory.create_node_array(
                                    self.transform_declarations_for_js(source_file, None),
                                    None,
                                )
                            } else {
                                visit_nodes(
                                    Some(source_file_as_source_file.statements()),
                                    Some(|node: &Node| self.visit_declaration_statements(node)),
                                    Option::<fn(&Node) -> bool>::None,
                                    None,
                                    None,
                                ).unwrap()
                            };
                            Some(with_synthetic_factory(|synthetic_factory| {
                                self.factory.update_source_file(
                                    synthetic_factory_,
                                    source_file,
                                    self.transform_and_replace_late_painted_statements(
                                        &updated
                                    ),
                                    Some(true),
                                    Some(vec![]),
                                    Some(vec![]),
                                    Some(false),
                                    Some(vec![]),
                                )
                            }))
                        }
                    ),
                    Some(map_defined(
                        Some(&node_as_bundle.prepends),
                        |prepend: &Gc<Node>, _| -> Option<Gc<Node>> {
                            if prepend.kind() == SyntaxKind::InputFiles {
                                let source_file = create_unparsed_source_file(
                                    prepend.clone(),
                                    Some("dts"),
                                    self.strip_internal,
                                );
                                let source_file_as_unparsed_source = source_file.as_unparsed_source();
                                has_no_default_lib = has_no_default_lib || source_file_as_unparsed_source.has_no_default_lib == Some(true);
                                self.collect_references(
                                    &source_file,
                                    &mut self.refs_mut(),
                                );
                                self.record_type_reference_directives_if_necessary(
                                    source_file_as_unparsed_source.type_reference_directives.as_deref()
                                );
                                self.collect_libs(
                                    &source_file,
                                    &mut self.libs_mut(),
                                );
                                return Some(source_file);
                            }
                            Some(prepend.clone())
                        }
                    )),
                )
            });
            bundle.synthetic_file_references = Some(vec![]);
            bundle.synthetic_type_references =
                Some(self.get_file_references_for_used_type_references());
            bundle.synthetic_lib_references = Some(self.get_lib_references());
            bundle.has_no_default_lib = Some(has_no_default_lib);
            let output_file_path = get_directory_path(&normalize_slashes(
                get_output_paths_for(node, &**self.host, true)
                    .declaration_file_path
                    .as_ref()
                    .unwrap(),
            ));
            {
                let mut reference_visitor = self.map_references_into_array(
                    node,
                    bundle.synthetic_file_references.as_mut().unwrap(),
                    &output_file_path,
                );
                self.refs().values().for_each(|ref_: &Gc<Node>| {
                    reference_visitor(ref_);
                });
            }
            return bundle.into();
        }
        let node_as_source_file = node.as_source_file();

        self.set_needs_declare(true);
        self.set_needs_scope_fix_marker(false);
        self.set_result_has_scope_marker(false);
        self.set_enclosing_declaration(Some(node.node_wrapper()));
        self.set_current_source_file(Some(node.node_wrapper()));
        self.set_get_symbol_accessibility_diagnostic(throw_diagnostic());
        self.set_is_bundled_emit(false);
        self.set_result_has_external_module_indicator(false);
        self.set_suppress_new_diagnostic_contexts(Some(false));
        self.set_late_marked_statements(None);
        self.set_late_statement_replacement_map(Some(Default::default()));
        self.set_necessary_type_references(None);
        self.set_refs(Some(Default::default()));
        {
            let mut refs = self.refs_mut();
            self.collect_references(&self.current_source_file(), &mut refs);
        }
        self.set_libs(Some(Default::default()));
        {
            let mut libs = self.libs_mut();
            self.collect_libs(&self.current_source_file(), &mut libs);
        }
        let mut references: Vec<FileReference> = vec![];
        let output_file_path = get_directory_path(&normalize_slashes(
            get_output_paths_for(node, &**self.host, true)
                .declaration_file_path
                .as_ref()
                .unwrap(),
        ));
        let mut reference_visitor =
            self.map_references_into_array(node, &mut references, &output_file_path);
        let mut combined_statements: NodeArray/*<Statement>*/;
        if is_source_file_js(&self.current_source_file()) {
            combined_statements = self
                .factory
                .create_node_array(self.transform_declarations_for_js(node, None), None);
            self.refs().values().for_each(|ref_: &Gc<Node>| {
                reference_visitor(ref_);
            });
            self.set_emitted_imports(Some(filter(
                &combined_statements,
                |statement: &Gc<Node>| is_any_import_syntax(statement),
            )));
        } else {
            let statements = visit_nodes(
                Some(node_as_source_file.statements()),
                Some(|node: &Node| self.visit_declaration_statements(node)),
                Option::<fn(&Node) -> bool>::None,
                None,
                None,
            )
            .unwrap();
            combined_statements = set_text_range_node_array(
                self.factory.create_node_array(
                    Some(self.transform_and_replace_late_painted_statements(&statements)),
                    None,
                ),
                Some(node_as_source_file.statements()),
            );
            self.refs().values().for_each(|ref_: &Gc<Node>| {
                reference_visitor(ref_);
            });
            self.set_emitted_imports(Some(filter(
                &combined_statements,
                |statement: &Gc<Node>| is_any_import_syntax(statement),
            )));
            if is_external_module(node)
                && (!self.result_has_external_module_indicator()
                    || self.needs_scope_fix_marker() && !self.result_has_scope_marker())
            {
                combined_statements = set_text_range_node_array(
                    self.factory.create_node_array(
                        Some({
                            let mut combined_statements = combined_statements.to_vec();
                            combined_statements.push(create_empty_exports(&self.factory));
                            combined_statements
                        }),
                        None,
                    ),
                    Some(&combined_statements),
                );
            }
        }
        drop(reference_visitor);
        let updated = with_synthetic_factory(|synthetic_factory_| {
            self.factory.update_source_file(
                synthetic_factory_,
                node,
                combined_statements,
                Some(true),
                Some(references),
                Some(self.get_file_references_for_used_type_references()),
                Some(node_as_source_file.has_no_default_lib()),
                Some(self.get_lib_references()),
            )
        });
        *updated
            .as_source_file()
            .maybe_exported_modules_from_declaration_emit() =
            self.maybe_exported_modules_from_declaration_emit().clone();
        updated
    }

    fn get_lib_references(&self) -> Vec<FileReference> {
        map(self.libs().keys(), |lib: &String, _| {
            FileReference::new(-1, -1, lib.clone())
        })
    }

    fn get_file_references_for_used_type_references(&self) -> Vec<FileReference> {
        self.maybe_necessary_type_references()
            .as_ref()
            .map(|necessary_type_references| {
                map_defined(Some(necessary_type_references), |key: &String, _| {
                    self.get_file_reference_for_type_name(key)
                })
            })
            .unwrap_or_default()
    }

    fn get_file_reference_for_type_name(&self, type_name: &str) -> Option<FileReference> {
        if let Some(emitted_imports) = self.maybe_emitted_imports().as_ref() {
            for import_statement in emitted_imports {
                if is_import_equals_declaration(import_statement)
                    && is_external_module_reference(
                        &import_statement
                            .as_import_equals_declaration()
                            .module_reference,
                    )
                {
                    let expr = &import_statement
                        .as_import_equals_declaration()
                        .module_reference
                        .as_external_module_reference()
                        .expression;
                    if is_string_literal_like(expr)
                        && &**expr.as_literal_like_node().text() == type_name
                    {
                        return None;
                    }
                } else if is_import_declaration(import_statement) && {
                    let import_statement_as_import_declaration =
                        import_statement.as_import_declaration();
                    is_string_literal(&import_statement_as_import_declaration.module_specifier)
                        && &**import_statement_as_import_declaration
                            .module_specifier
                            .as_string_literal()
                            .text()
                            == type_name
                } {
                    return None;
                }
            }
        }
        Some(FileReference::new(-1, -1, type_name.to_owned()))
    }

    fn map_references_into_array<'arg>(
        &'arg self,
        node: &'arg Node,
        references: &'arg mut Vec<FileReference>,
        output_file_path: &'arg str,
    ) -> impl FnMut(&Node /*SourceFile*/) + 'arg {
        |file: &Node| {
            let file_as_source_file = file.as_source_file();
            let decl_file_name: String;
            if file_as_source_file.is_declaration_file() {
                decl_file_name = file_as_source_file.file_name().clone();
            } else {
                if self.is_bundled_emit()
                    && contains_comparer(
                        Some(&node.as_bundle().source_files),
                        &Some(file.node_wrapper()),
                        |a: &Option<Gc<Node>>, b: &Option<Gc<Node>>| {
                            are_option_gcs_equal(a.as_ref(), b.as_ref())
                        },
                    )
                {
                    return;
                }
                let paths = get_output_paths_for(file, &**self.host, true);
                decl_file_name = paths
                    .declaration_file_path
                    .clone()
                    .non_empty()
                    .or_else(|| paths.js_file_path.clone().non_empty())
                    .unwrap_or_else(|| file_as_source_file.file_name().clone());
            }

            if !decl_file_name.is_empty() {
                let specifier = module_specifiers::get_module_specifier(
                    &self.options,
                    &self.current_source_file(),
                    &to_path(
                        output_file_path,
                        Some(&ScriptReferenceHost::get_current_directory(&**self.host)),
                        |file_name| self.host.get_canonical_file_name(file_name),
                    ),
                    &to_path(
                        &decl_file_name,
                        Some(&ScriptReferenceHost::get_current_directory(&**self.host)),
                        |file_name| self.host.get_canonical_file_name(file_name),
                    ),
                    &**self.host,
                );
                if !path_is_relative(&specifier) {
                    self.record_type_reference_directives_if_necessary(Some(&[specifier]));
                    return;
                }

                let mut file_name = get_relative_path_to_directory_or_url(
                    output_file_path,
                    &decl_file_name,
                    &ScriptReferenceHost::get_current_directory(&**self.host),
                    |file_name: &str| self.host.get_canonical_file_name(file_name),
                    false,
                );
                if starts_with(&file_name, "./") && has_extension(&file_name) {
                    file_name = file_name[2..].to_owned();
                }

                if starts_with(&file_name, "node_modules/")
                    || path_contains_node_modules(&file_name)
                {
                    return;
                }

                references.push(FileReference::new(-1, -1, file_name));
            }
        }
    }

    fn collect_references(
        &self,
        source_file: &Node, /*SourceFile | UnparsedSource*/
        ret: &mut HashMap<NodeId, Gc<Node /*SourceFile*/>>,
    ) {
        if self.no_resolve == Some(true)
            || !is_unparsed_source(source_file) && is_source_file_js(source_file)
        {
            return /*ret*/;
        }
        maybe_for_each(
            source_file
                .as_source_file()
                .maybe_referenced_files()
                .as_deref(),
            |f: &FileReference, _| -> Option<()> {
                let elem = self.host.get_source_file_from_reference(source_file, f);
                if let Some(elem) = elem {
                    ret.insert(get_original_node_id(&elem), elem);
                }
                None
            },
        );
        // return ret;
    }

    fn collect_libs(
        &self,
        source_file: &Node, /*SourceFile | UnparsedSource*/
        ret: &mut HashMap<String, bool>,
    ) {
        let maybe_source_file_as_source_file_lib_reference_directives = match source_file.kind() {
            SyntaxKind::SourceFile => Some(
                source_file
                    .as_source_file()
                    .maybe_lib_reference_directives(),
            ),
            _ => None,
        };
        maybe_for_each(
            // TODO: expose some trait for unifying this?
            match source_file.kind() {
                SyntaxKind::SourceFile => maybe_source_file_as_source_file_lib_reference_directives
                    .as_ref()
                    .unwrap()
                    .as_deref(),
                SyntaxKind::UnparsedSource => {
                    Some(&*source_file.as_unparsed_source().lib_reference_directives)
                }
                _ => unreachable!(),
            },
            |ref_: &FileReference, _| -> Option<()> {
                let lib = self.host.get_lib_file_from_reference(ref_);
                if lib.is_some() {
                    ret.insert(to_file_name_lower_case(&ref_.file_name), true);
                }
                None
            },
        );
        // return ret;
    }

    fn filter_binding_pattern_initializers(&self, name: &Node /*BindingName*/) -> Gc<Node> {
        if name.kind() == SyntaxKind::Identifier {
            return name.node_wrapper();
        } else {
            if name.kind() == SyntaxKind::ArrayBindingPattern {
                return with_synthetic_factory(|synthetic_factory_| {
                    self.factory.update_array_binding_pattern(
                        synthetic_factory_,
                        name,
                        visit_nodes(
                            Some(&name.as_array_binding_pattern().elements),
                            Some(|node: &Node| Some(self.visit_binding_element(node).into())),
                            Option::<fn(&Node) -> bool>::None,
                            None,
                            None,
                        )
                        .unwrap(),
                    )
                });
            } else {
                return with_synthetic_factory(|synthetic_factory_| {
                    self.factory.update_object_binding_pattern(
                        synthetic_factory_,
                        name,
                        visit_nodes(
                            Some(&name.as_object_binding_pattern().elements),
                            Some(|node: &Node| Some(self.visit_binding_element(node).into())),
                            Option::<fn(&Node) -> bool>::None,
                            None,
                            None,
                        )
                        .unwrap(),
                    )
                });
            }
        }
    }

    fn visit_binding_element(&self, elem: &Node /*ArrayBindingElement*/) -> Gc<Node> {
        if elem.kind() == SyntaxKind::OmittedExpression {
            return elem.node_wrapper();
        }
        let elem_as_binding_element = elem.as_binding_element();
        with_synthetic_factory(|synthetic_factory_| {
            self.factory.update_binding_element(
                synthetic_factory_,
                elem,
                elem_as_binding_element.dot_dot_dot_token.clone(),
                elem_as_binding_element.property_name.clone(),
                self.filter_binding_pattern_initializers(&elem_as_binding_element.name()),
                if self.should_print_with_initializer(elem) {
                    elem_as_binding_element.maybe_initializer()
                } else {
                    None
                },
            )
        })
    }

    fn ensure_parameter(
        &self,
        p: &Node, /*ParameterDeclaration*/
        modifier_mask: Option<ModifierFlags>,
        type_: Option<&Node /*TypeNode*/>,
    ) -> Gc<Node /*ParameterDeclaration*/> {
        let p_as_parameter_declaration = p.as_parameter_declaration();
        let mut old_diag: Option<GetSymbolAccessibilityDiagnostic> = None;
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            old_diag = Some(self.get_symbol_accessibility_diagnostic());
            self.set_get_symbol_accessibility_diagnostic(
                create_get_symbol_accessibility_diagnostic_for_node(p),
            );
        }
        let new_param = with_synthetic_factory(|synthetic_factory_| {
            self.factory.update_parameter_declaration(
                synthetic_factory_,
                p,
                Option::<NodeArray>::None,
                Some(mask_modifiers(p, modifier_mask, None)),
                p_as_parameter_declaration.dot_dot_dot_token.clone(),
                Some(self.filter_binding_pattern_initializers(&p_as_parameter_declaration.name())),
                if self.resolver.is_optional_parameter(p) {
                    Some(
                        p_as_parameter_declaration
                            .question_token
                            .clone()
                            .unwrap_or_else(|| {
                                self.factory
                                    .create_token(synthetic_factory_, SyntaxKind::QuestionToken)
                                    .into()
                            }),
                    )
                } else {
                    None
                },
                self.ensure_type(
                    p,
                    type_
                        .map(Node::node_wrapper)
                        .or_else(|| p_as_parameter_declaration.maybe_type())
                        .as_deref(),
                    Some(true),
                ),
                self.ensure_no_initializer(p),
            )
        });
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(old_diag.unwrap());
        }
        new_param
    }

    fn should_print_with_initializer(&self, node: &Node) -> bool {
        can_have_literal_initializer(node)
            && self.resolver.is_literal_const_declaration(
                &get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None).unwrap(),
            )
    }

    fn ensure_no_initializer(
        &self,
        node: &Node, /*CanHaveLiteralInitializer*/
    ) -> Option<Gc<Node>> {
        if self.should_print_with_initializer(node) {
            return Some(self.resolver.create_literal_const_value(
                &get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None).unwrap(),
                &**self.symbol_tracker(),
            ));
        }
        None
    }

    fn ensure_type(
        &self,
        node: &Node, /*HasInferredType*/
        type_: Option<&Node /*TypeNode*/>,
        ignore_private: Option<bool>,
    ) -> Option<Gc<Node /*TypeNode*/>> {
        if ignore_private != Some(true) && has_effective_modifier(node, ModifierFlags::Private) {
            return None;
        }
        if self.should_print_with_initializer(node) {
            return None;
        }
        let should_use_resolver_type = node.kind() == SyntaxKind::Parameter
            && (self.resolver.is_required_initialized_parameter(node)
                || self
                    .resolver
                    .is_optional_uninitialized_parameter_property(node));
        if let Some(type_) = type_ {
            if !should_use_resolver_type {
                return visit_node(
                    Some(type_),
                    Some(|node: &Node| self.visit_declaration_subtree(node)),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                );
            }
        }
        if get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None).is_none() {
            return if let Some(type_) = type_ {
                visit_node(
                    Some(type_),
                    Some(|node: &Node| self.visit_declaration_subtree(node)),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
            } else {
                Some(with_synthetic_factory(|synthetic_factory_| {
                    self.factory
                        .create_keyword_type_node(synthetic_factory_, SyntaxKind::AnyKeyword)
                        .into()
                }))
            };
        }
        if node.kind() == SyntaxKind::SetAccessor {
            return Some(with_synthetic_factory(|synthetic_factory_| {
                self.factory
                    .create_keyword_type_node(synthetic_factory_, SyntaxKind::AnyKeyword)
                    .into()
            }));
        }
        self.set_error_name_node(node.as_named_declaration().maybe_name());
        let mut old_diag: Option<GetSymbolAccessibilityDiagnostic> = None;
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            old_diag = Some(self.get_symbol_accessibility_diagnostic());
            self.set_get_symbol_accessibility_diagnostic(
                create_get_symbol_accessibility_diagnostic_for_node(node),
            );
        }
        if matches!(
            node.kind(),
            SyntaxKind::VariableDeclaration | SyntaxKind::BindingElement
        ) {
            return Some(self.cleanup(
                old_diag,
                self.resolver.create_type_of_declaration(
                    node,
                    &self.enclosing_declaration(),
                    declaration_emit_node_builder_flags(),
                    &**self.symbol_tracker(),
                    None,
                ),
            ));
        }
        if matches!(
            node.kind(),
            SyntaxKind::Parameter | SyntaxKind::PropertyDeclaration | SyntaxKind::PropertySignature
        ) {
            if node.as_has_initializer().maybe_initializer().is_none() {
                return Some(self.cleanup(
                    old_diag,
                    self.resolver.create_type_of_declaration(
                        node,
                        &self.enclosing_declaration(),
                        declaration_emit_node_builder_flags(),
                        &**self.symbol_tracker(),
                        Some(should_use_resolver_type),
                    ),
                ));
            }
            return Some(
                self.cleanup(
                    old_diag,
                    self.resolver
                        .create_type_of_declaration(
                            node,
                            &self.enclosing_declaration(),
                            declaration_emit_node_builder_flags(),
                            &**self.symbol_tracker(),
                            None,
                        )
                        .or_else(|| {
                            self.resolver.create_type_of_expression(
                                &node.as_has_initializer().maybe_initializer().unwrap(),
                                &self.enclosing_declaration(),
                                declaration_emit_node_builder_flags(),
                                &**self.symbol_tracker(),
                            )
                        }),
                ),
            );
        }
        Some(self.cleanup(
            old_diag,
            self.resolver.create_return_type_of_signature_declaration(
                node,
                &self.enclosing_declaration(),
                declaration_emit_node_builder_flags(),
                &**self.symbol_tracker(),
            ),
        ))
    }

    fn cleanup(
        &self,
        old_diag: Option<GetSymbolAccessibilityDiagnostic>,
        return_value: Option<impl Borrow<Node> /*TypeNode*/>,
    ) -> Gc<Node> {
        self.set_error_name_node(None);
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(old_diag.unwrap())
        }
        return_value
            .map(|return_value| return_value.borrow().node_wrapper())
            .unwrap_or_else(|| {
                with_synthetic_factory(|synthetic_factory_| {
                    self.factory
                        .create_keyword_type_node(synthetic_factory_, SyntaxKind::AnyKeyword)
                        .into()
                })
            })
    }

    fn is_declaration_and_not_visible(&self, node: &Node /*NamedDeclaration*/) -> bool {
        let ref node = get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None).unwrap();
        match node.kind() {
            SyntaxKind::FunctionDeclaration
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::EnumDeclaration => !self.resolver.is_declaration_visible(node),
            SyntaxKind::VariableDeclaration => !self.get_binding_name_visible(node),
            SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::ImportDeclaration
            | SyntaxKind::ExportDeclaration
            | SyntaxKind::ExportAssignment => false,
            SyntaxKind::ClassStaticBlockDeclaration => true,
            _ => false,
        }
    }

    fn should_emit_function_properties(&self, input: &Node /*FunctionDeclaration*/) -> bool {
        if input.as_function_declaration().maybe_body().is_some() {
            return true;
        }

        let overload_signatures: Option<Vec<_>> =
            input
                .symbol()
                .maybe_declarations()
                .as_ref()
                .map(|input_symbol_declarations| {
                    input_symbol_declarations
                        .into_iter()
                        .filter(|decl: &&Gc<Node>| {
                            is_function_declaration(decl)
                                && decl.as_function_declaration().maybe_body().is_none()
                        })
                        .cloned()
                        .collect()
                });
        match overload_signatures {
            None => true,
            Some(overload_signatures) if overload_signatures.is_empty() => true,
            Some(overload_signatures) => {
                overload_signatures
                    .iter()
                    .position(|overload_signature: &Gc<Node>| ptr::eq(&**overload_signature, input))
                    == Some(overload_signatures.len() - 1)
            }
        }
    }

    fn get_binding_name_visible(
        &self,
        elem: &Node, /*BindingElement | VariableDeclaration | OmittedExpression*/
    ) -> bool {
        if is_omitted_expression(elem) {
            return false;
        }
        if let Some(elem_name) = elem
            .as_named_declaration()
            .maybe_name()
            .filter(|elem_name| is_binding_pattern(Some(&**elem_name)))
            .as_ref()
        {
            some(
                Some(elem_name.as_has_elements().elements()),
                Some(|element: &Gc<Node>| self.get_binding_name_visible(element)),
            )
        } else {
            self.resolver.is_declaration_visible(elem)
        }
    }

    fn update_params_list(
        &self,
        node: &Node,
        params: Option<&NodeArray /*<ParameterDeclaration>*/>,
        modifier_mask: Option<ModifierFlags>,
    ) -> Option<NodeArray /*<ParameterDeclaration>*/> {
        if has_effective_modifier(node, ModifierFlags::Private) {
            return None;
        }
        let new_params = maybe_map(params, |p: &Gc<Node>, _| {
            self.ensure_parameter(p, modifier_mask, None)
        })?;
        Some(
            self.factory
                .create_node_array(Some(new_params), Some(params.unwrap().has_trailing_comma)),
        )
    }

    fn update_accessor_params_list(
        &self,
        input: &Node, /*AccessorDeclaration*/
        is_private: bool,
    ) -> NodeArray {
        let mut new_params: Vec<Gc<Node /*ParameterDeclaration*/>> = Default::default();
        if !is_private {
            let this_parameter = get_this_parameter(input);
            if let Some(this_parameter) = this_parameter {
                new_params.push(self.ensure_parameter(&this_parameter, None, None));
            }
        }
        if is_set_accessor_declaration(input) {
            let mut new_value_parameter: Option<Gc<Node /*ParameterDeclaration*/>> = None;
            if !is_private {
                let value_parameter = get_set_accessor_value_parameter(input);
                if let Some(value_parameter) = value_parameter {
                    let accessor_type = self.get_type_annotation_from_all_accessor_declarations(
                        input,
                        &self.resolver.get_all_accessor_declarations(input),
                    );
                    new_value_parameter = Some(self.ensure_parameter(
                        &value_parameter,
                        None,
                        accessor_type.as_deref(),
                    ));
                }
            }
            if new_value_parameter.is_none() {
                new_value_parameter = Some(with_synthetic_factory(|synthetic_factory_| {
                    self.factory
                        .create_parameter_declaration(
                            synthetic_factory_,
                            Option::<NodeArray>::None,
                            Option::<NodeArray>::None,
                            None,
                            Some("value"),
                            None,
                            None,
                            None,
                        )
                        .into()
                }));
            }
            let new_value_parameter = new_value_parameter.unwrap();
            new_params.push(new_value_parameter);
        }
        self.factory.create_node_array(Some(new_params), None)
    }

    fn ensure_type_params(
        &self,
        node: &Node,
        params: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
    ) -> Option<NodeArray> {
        if has_effective_modifier(node, ModifierFlags::Private) {
            None
        } else {
            visit_nodes(
                params,
                Some(|node: &Node| self.visit_declaration_subtree(node)),
                Option::<fn(&Node) -> bool>::None,
                None,
                None,
            )
        }
    }

    fn is_enclosing_declaration(&self, node: &Node) -> bool {
        is_source_file(node)
            || is_type_alias_declaration(node)
            || is_module_declaration(node)
            || is_class_declaration(node)
            || is_interface_declaration(node)
            || is_function_like(Some(node))
            || is_index_signature_declaration(node)
            || is_mapped_type_node(node)
    }

    fn check_entity_name_visibility(
        &self,
        entity_name: &Node, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: &Node,
    ) {
        let visibility_result = self
            .resolver
            .is_entity_name_visible(entity_name, enclosing_declaration);
        self.handle_symbol_accessibility_error(
            &visibility_result.into_symbol_accessibility_result(),
        );
        self.record_type_reference_directives_if_necessary(
            self.resolver
                .get_type_reference_directives_for_entity_name(entity_name)
                .as_deref(),
        );
    }

    fn transform_and_replace_late_painted_statements(
        &self,
        statements: &NodeArray, /*<Statement>*/
    ) -> NodeArray /*<Statement>*/ {
        unimplemented!()
    }

    fn visit_declaration_subtree(&self, input: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    fn visit_declaration_statements(&self, input: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    fn get_type_annotation_from_all_accessor_declarations(
        &self,
        node: &Node, /*AccessorDeclaration*/
        accessors: &AllAccessorDeclarations,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }
}

impl TransformerInterface for TransformDeclarations {
    fn call(&self, node: &Node) -> Gc<Node> {
        self.transform_root(node)
    }
}

thread_local! {
    static throw_diagnostic_: GetSymbolAccessibilityDiagnostic = Gc::new(Box::new(ThrowDiagnostic))
}

fn throw_diagnostic() -> GetSymbolAccessibilityDiagnostic {
    throw_diagnostic_.with(|throw_diagnostic| throw_diagnostic.clone())
}

#[derive(Trace, Finalize)]
struct ThrowDiagnostic;

impl GetSymbolAccessibilityDiagnosticInterface for ThrowDiagnostic {
    fn call(
        &self,
        _symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        Debug_.fail(Some("Diagnostic emitted without context"));
    }
}

#[derive(Trace, Finalize)]
struct TransformDeclarationsSymbolTracker {
    transform_declarations: Gc<Box<TransformDeclarations>>,
    host: Gc<Box<dyn EmitHost>>,
}

impl TransformDeclarationsSymbolTracker {
    fn new(
        transform_declarations: Gc<Box<TransformDeclarations>>,
        host: Gc<Box<dyn EmitHost>>,
    ) -> Self {
        Self {
            transform_declarations,
            host,
        }
    }

    fn error_declaration_name_with_fallback(&self) -> Cow<'static, str> {
        if let Some(error_name_node) = self.transform_declarations.maybe_error_name_node() {
            declaration_name_to_string(Some(error_name_node))
        } else if let Some(error_fallback_node_name) = self
            .transform_declarations
            .maybe_error_fallback_node()
            .and_then(|error_fallback_node| get_name_of_declaration(Some(error_fallback_node)))
        {
            declaration_name_to_string(Some(error_fallback_node_name))
        } else if let Some(ref error_fallback_node) = self
            .transform_declarations
            .maybe_error_fallback_node()
            .filter(|error_fallback_node| is_export_assignment(error_fallback_node))
        {
            if error_fallback_node.as_export_assignment().is_export_equals == Some(true) {
                "export=".into()
            } else {
                "default".into()
            }
        } else {
            "(Missing)".into()
        }
    }
}

impl SymbolTracker for TransformDeclarationsSymbolTracker {
    fn track_symbol(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<Gc<Node>>,
        meaning: SymbolFlags,
    ) -> Option<bool> {
        if symbol.flags().intersects(SymbolFlags::TypeParameter) {
            return Some(false);
        }
        let issued_diagnostic = self
            .transform_declarations
            .handle_symbol_accessibility_error(
                &self.transform_declarations.resolver.is_symbol_accessible(
                    symbol,
                    // TODO: it sort of looks like maybe the signature for .track_symbol() should take
                    // an Option<&Node> instead?
                    enclosing_declaration.as_deref(),
                    Some(meaning),
                    true,
                ),
            );
        self.transform_declarations
            .record_type_reference_directives_if_necessary(
                self.transform_declarations
                    .resolver
                    .get_type_reference_directives_for_symbol(symbol, Some(meaning))
                    .as_deref(),
            );
        Some(issued_diagnostic)
    }

    fn is_track_symbol_supported(&self) -> bool {
        true
    }

    fn report_inaccessible_this_error(&self) {
        if let Some(ref error_name_node_or_error_fallback_node) = self
            .transform_declarations
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations.maybe_error_fallback_node())
        {
            self.transform_declarations.context.add_diagnostic(
                create_diagnostic_for_node(error_name_node_or_error_fallback_node, &Diagnostics::The_inferred_type_of_0_references_an_inaccessible_1_type_A_type_annotation_is_necessary, Some(vec![
                            self.error_declaration_name_with_fallback().into_owned(),
                            "this".to_owned()
                        ])).into()
            );
        }
    }

    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        true
    }

    fn report_inaccessible_unique_symbol_error(&self) {
        if let Some(ref error_name_node_or_error_fallback_node) = self
            .transform_declarations
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations.maybe_error_fallback_node())
        {
            self.transform_declarations.context.add_diagnostic(
                create_diagnostic_for_node(error_name_node_or_error_fallback_node, &Diagnostics::The_inferred_type_of_0_references_an_inaccessible_1_type_A_type_annotation_is_necessary, Some(vec![
                            self.error_declaration_name_with_fallback().into_owned(),
                            "unique symbol".to_owned(),
                        ])).into()
            );
        }
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        true
    }

    fn report_cyclic_structure_error(&self) {
        if let Some(ref error_name_node_or_error_fallback_node) = self
            .transform_declarations
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations.maybe_error_fallback_node())
        {
            self.transform_declarations.context.add_diagnostic(
                create_diagnostic_for_node(error_name_node_or_error_fallback_node, &Diagnostics::The_inferred_type_of_0_references_a_type_with_a_cyclic_structure_which_cannot_be_trivially_serialized_A_type_annotation_is_necessary, Some(vec![
                            self.error_declaration_name_with_fallback().into_owned(),
                        ])).into()
            );
        }
    }

    fn is_report_cyclic_structure_error_supported(&self) -> bool {
        true
    }

    fn report_private_in_base_of_class_expression(&self, property_name: &str) {
        if let Some(ref error_name_node_or_error_fallback_node) = self
            .transform_declarations
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations.maybe_error_fallback_node())
        {
            self.transform_declarations.context.add_diagnostic(
                create_diagnostic_for_node(error_name_node_or_error_fallback_node, &Diagnostics::Property_0_of_exported_class_expression_may_not_be_private_or_protected, Some(vec![
                            property_name.to_owned()
                        ])).into()
            );
        }
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        true
    }

    fn report_likely_unsafe_import_required_error(&self, specifier: &str) {
        if let Some(ref error_name_node_or_error_fallback_node) = self
            .transform_declarations
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations.maybe_error_fallback_node())
        {
            self.transform_declarations.context.add_diagnostic(
                create_diagnostic_for_node(error_name_node_or_error_fallback_node, &Diagnostics::The_inferred_type_of_0_cannot_be_named_without_a_reference_to_1_This_is_likely_not_portable_A_type_annotation_is_necessary, Some(vec![
                            self.error_declaration_name_with_fallback().into_owned(),
                            specifier.to_owned(),
                        ])).into()
            );
        }
    }

    fn is_report_likely_unsafe_import_required_error_supported(&self) -> bool {
        true
    }

    fn report_truncation_error(&self) {
        if let Some(ref error_name_node_or_error_fallback_node) = self
            .transform_declarations
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations.maybe_error_fallback_node())
        {
            self.transform_declarations.context.add_diagnostic(
                create_diagnostic_for_node(
                    error_name_node_or_error_fallback_node,
                    &Diagnostics::The_inferred_type_of_this_node_exceeds_the_maximum_length_the_compiler_will_serialize_An_explicit_type_annotation_is_needed,
                    None
                ).into()
            );
        }
    }

    fn is_module_resolver_host_supported(&self) -> bool {
        true
    }

    fn module_resolver_host(
        &self,
    ) -> Option<&dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory> {
        Some(
            self.host
                .as_module_specifier_resolution_host_and_get_common_source_directory(),
        )
    }

    fn track_referenced_ambient_module(
        &self,
        node: &Node, /*ModuleDeclaration*/
        symbol: &Symbol,
    ) {
        let directives = self
            .transform_declarations
            .resolver
            .get_type_reference_directives_for_symbol(symbol, Some(SymbolFlags::All));
        if let Some(directives) = directives.non_empty() {
            return self
                .transform_declarations
                .record_type_reference_directives_if_necessary(Some(&directives));
        }
        let container = get_source_file_of_node(node);
        self.transform_declarations
            .refs_mut()
            .insert(get_original_node_id(&container), container);
    }

    fn is_track_referenced_ambient_module_supported(&self) -> bool {
        true
    }

    fn track_external_module_symbol_of_import_type_node(&self, symbol: &Symbol) {
        if !self.transform_declarations.is_bundled_emit() {
            self.transform_declarations
                .maybe_exported_modules_from_declaration_emit_mut()
                .get_or_insert_with(|| vec![])
                .push(symbol.symbol_wrapper());
        }
    }

    fn report_nonlocal_augmentation(
        &self,
        containing_file: &Node, /*SourceFile*/
        parent_symbol: &Symbol,
        symbol: &Symbol,
    ) {
        let primary_declaration =
            parent_symbol
                .maybe_declarations()
                .as_ref()
                .and_then(|parent_symbol_declarations| {
                    parent_symbol_declarations
                        .into_iter()
                        .find(|d: &&Gc<Node>| {
                            ptr::eq(&*get_source_file_of_node(d), containing_file)
                        })
                        .cloned()
                });
        let augmenting_declarations =
            maybe_filter(symbol.maybe_declarations().as_deref(), |d: &Gc<Node>| {
                !ptr::eq(&*get_source_file_of_node(d), containing_file)
            });
        if let Some(augmenting_declarations) = augmenting_declarations {
            for augmentations in augmenting_declarations {
                self.transform_declarations.context.add_diagnostic(
                    add_related_info_rc(
                        create_diagnostic_for_node(
                            &augmentations,
                            &Diagnostics::Declaration_augments_declaration_in_another_file_This_cannot_be_serialized,
                            None,
                        ).into(),
                        vec![
                            create_diagnostic_for_node(
                                primary_declaration.as_ref().unwrap(),
                                &Diagnostics::This_is_the_declaration_being_augmented_Consider_moving_the_augmenting_declaration_into_the_same_file,
                                None,
                            ).into(),
                        ]
                    )
                );
            }
        }
    }

    fn is_report_nonlocal_augmentation_supported(&self) -> bool {
        true
    }

    fn report_non_serializable_property(&self, property_name: &str) {
        if let Some(ref error_name_node_or_error_fallback_node) = self
            .transform_declarations
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations.maybe_error_fallback_node())
        {
            self.transform_declarations.context.add_diagnostic(
                create_diagnostic_for_node(
                    error_name_node_or_error_fallback_node,
                    &Diagnostics::The_type_of_this_node_cannot_be_serialized_because_its_property_0_cannot_be_serialized,
                    Some(vec![
                        property_name.to_owned()
                    ])
                ).into()
            );
        }
    }

    fn is_report_non_serializable_property_supported(&self) -> bool {
        true
    }
}

#[derive(Trace, Finalize)]
struct TransformDeclarationsForJSGetSymbolAccessibilityDiagnostic {
    source_file: Gc<Node /*SourceFile*/>,
}

impl TransformDeclarationsForJSGetSymbolAccessibilityDiagnostic {
    fn new(source_file: Gc<Node /*SourceFile*/>) -> Self {
        Self { source_file }
    }
}

impl GetSymbolAccessibilityDiagnosticInterface
    for TransformDeclarationsForJSGetSymbolAccessibilityDiagnostic
{
    fn call(&self, s: &SymbolAccessibilityResult) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        if let Some(s_error_node) = s
            .error_node
            .as_ref()
            .filter(|s_error_node| can_produce_diagnostics(s_error_node))
        {
            create_get_symbol_accessibility_diagnostic_for_node(s_error_node).call(s)
        } else {
            Some(Gc::new(SymbolAccessibilityDiagnostic {
                diagnostic_message: if s.error_module_name.as_ref().non_empty().is_some() {
                    &*Diagnostics::Declaration_emit_for_this_file_requires_using_private_name_0_from_module_1_An_explicit_type_annotation_may_unblock_declaration_emit
                } else {
                    &*Diagnostics::Declaration_emit_for_this_file_requires_using_private_name_0_An_explicit_type_annotation_may_unblock_declaration_emit
                },
                error_node: s
                    .error_node
                    .clone()
                    .unwrap_or_else(|| self.source_file.clone()),
                type_name: None,
            }))
        }
    }
}

#[derive(Trace, Finalize)]
struct TransformDeclarationsFactory {}

impl TransformDeclarationsFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformDeclarationsFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        TransformDeclarations::new(context).as_transformer()
    }
}

pub fn transform_declarations() -> TransformerFactory {
    Gc::new(Box::new(TransformDeclarationsFactory::new()))
}

fn mask_modifiers(
    node: &Node,
    modifier_mask: Option<ModifierFlags>,
    modifier_additions: Option<ModifierFlags>,
) -> Vec<Gc<Node /*Modifier*/>> {
    unimplemented!()
}

fn can_have_literal_initializer(node: &Node) -> bool {
    unimplemented!()
}
