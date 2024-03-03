use std::{
    any::Any,
    borrow::Cow,
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    io,
    rc::Rc,
};

use id_arena::Id;

use super::{can_have_literal_initializer, mask_modifiers};
use crate::{
    add_related_info_rc, can_produce_diagnostics, contains, create_diagnostic_for_node,
    create_empty_exports, create_get_symbol_accessibility_diagnostic_for_node,
    create_unparsed_source_file, declaration_name_to_string, downcast_transformer_ref, filter,
    get_directory_path, get_factory_id, get_leading_comment_ranges,
    get_leading_comment_ranges_of_node, get_name_of_declaration, get_original_node_id,
    get_output_paths_for, get_parse_tree_node, get_relative_path_to_directory_or_url,
    get_resolved_external_module_name, get_source_file_of_node, get_text_of_node,
    get_trailing_comment_ranges, has_extension, impl_has_arena, is_any_import_syntax,
    is_export_assignment, is_external_module, is_external_module_reference,
    is_external_or_common_js_module, is_import_declaration, is_import_equals_declaration,
    is_json_source_file, is_source_file_js, is_source_file_not_json, is_string_literal,
    is_string_literal_like, is_unparsed_source, last, map, map_defined, maybe_concatenate,
    maybe_filter, maybe_for_each, maybe_for_each_bool, module_specifiers, normalize_slashes,
    path_contains_node_modules, path_is_relative, per_arena, push_if_unique_eq, ref_mut_unwrapped,
    ref_unwrapped, released, set_text_range_node_array, skip_trivia, starts_with, string_contains,
    to_file_name_lower_case, to_path, transform_nodes, try_map, try_map_defined,
    try_maybe_for_each, try_visit_nodes, AllArenas, CommentRange, CompilerOptions,
    CoreTransformationContext, Debug_, Diagnostic, Diagnostics, EmitHost, EmitResolver,
    FileReference, GetOrInsertDefault, GetSymbolAccessibilityDiagnostic,
    GetSymbolAccessibilityDiagnosticInterface, HasArena, HasInitializerInterface,
    HasStatementsInterface, HasTypeInterface,
    IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory, InArena,
    LiteralLikeNodeInterface, ModifierFlags, NamedDeclarationInterface, Node, NodeArray,
    NodeBuilderFlags, NodeFactory, NodeId, NodeInterface, NonEmpty, ReadonlyTextRange,
    ScriptReferenceHost, SourceFileLike, Symbol, SymbolAccessibility,
    SymbolAccessibilityDiagnostic, SymbolAccessibilityResult, SymbolFlags, SymbolInterface,
    SymbolTracker, SyntaxKind, TextRange, TransformNodesTransformationResult,
    TransformationContext, TransformationResult, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, VisitResult,
};

pub fn get_declaration_diagnostics(
    host: Id<Box<dyn EmitHost>>,
    resolver: Id<Box<dyn EmitResolver>>,
    file: Option<Id<Node> /*SourceFile*/>,
    arena: &impl HasArena,
) -> io::Result<Option<Vec<Id<Diagnostic /*DiagnosticWithLocation*/>>>> {
    let compiler_options = ScriptReferenceHost::get_compiler_options(&**host.ref_(arena));
    let result = transform_nodes(
        Some(resolver),
        Some(host.clone()),
        get_factory_id(arena),
        compiler_options,
        &if let Some(file) = file {
            vec![file]
        } else {
            filter(
                &host.ref_(arena).get_source_files(),
                |source_file: &Id<Node>| is_source_file_not_json(&source_file.ref_(arena)),
            )
        },
        &[transform_declarations(arena)],
        false,
        arena,
    )?;
    Ok(result.ref_(arena).diagnostics())
}

pub(super) fn has_internal_annotation(
    range: &CommentRange,
    current_source_file: Id<Node>, /*SourceFile*/
    arena: &impl HasArena,
) -> bool {
    let current_source_file_ref = current_source_file.ref_(arena);
    let current_source_file_text = current_source_file_ref.as_source_file().text();
    let comment =
        &current_source_file_text[range.pos().try_into().unwrap()..range.end().try_into().unwrap()];
    string_contains(comment, "@internal")
}

pub fn is_internal_declaration(
    node: Id<Node>,
    current_source_file: Id<Node>, /*SourceFile*/
    arena: &impl HasArena,
) -> bool {
    let parse_tree_node =
        get_parse_tree_node(Some(node), Option::<fn(Id<Node>) -> bool>::None, arena);
    if let Some(parse_tree_node) = parse_tree_node
        .filter(|parse_tree_node| parse_tree_node.ref_(arena).kind() == SyntaxKind::Parameter)
    {
        let param_idx = parse_tree_node
            .ref_(arena)
            .parent()
            .ref_(arena)
            .as_signature_declaration()
            .parameters()
            .ref_(arena)
            .into_iter()
            .position(|&parameter: &Id<Node>| parameter == parse_tree_node);
        let previous_sibling = if let Some(param_idx) = param_idx.filter(|param_idx| *param_idx > 0)
        {
            parse_tree_node
                .ref_(arena)
                .parent()
                .ref_(arena)
                .as_signature_declaration()
                .parameters()
                .ref_(arena)
                .get(param_idx - 1)
                .copied()
        } else {
            None
        };
        let current_source_file_ref = current_source_file.ref_(arena);
        let text = current_source_file_ref.as_source_file().text_as_chars();
        let comment_ranges = if let Some(previous_sibling) = previous_sibling {
            maybe_concatenate(
                get_trailing_comment_ranges(
                    &text,
                    skip_trivia(
                        &text,
                        previous_sibling.ref_(arena).end() + 1,
                        Some(false),
                        Some(true),
                        None,
                    ),
                ),
                get_leading_comment_ranges(&text, node.ref_(arena).pos()),
            )
        } else {
            get_trailing_comment_ranges(
                &text,
                skip_trivia(&text, node.ref_(arena).pos(), Some(false), Some(true), None),
            )
        };
        return matches!(
            comment_ranges,
            Some(comment_ranges) if !comment_ranges.is_empty() &&
                has_internal_annotation(
                    last(&comment_ranges),
                    current_source_file,
                    arena,
                )
        );
    }
    let leading_comment_ranges = parse_tree_node.and_then(|parse_tree_node| {
        get_leading_comment_ranges_of_node(
            &parse_tree_node.ref_(arena),
            &current_source_file.ref_(arena),
        )
    });
    maybe_for_each_bool(
        leading_comment_ranges.as_ref(),
        |range: &CommentRange, _| has_internal_annotation(range, current_source_file, arena),
    )
}

pub(super) fn declaration_emit_node_builder_flags() -> NodeBuilderFlags {
    NodeBuilderFlags::MultilineObjectLiterals
        | NodeBuilderFlags::WriteClassExpressionAsTypeLiteral
        | NodeBuilderFlags::UseTypeOfFunction
        | NodeBuilderFlags::UseStructuralFallback
        | NodeBuilderFlags::AllowEmptyTuple
        | NodeBuilderFlags::GenerateNamesForShadowedTypeParams
        | NodeBuilderFlags::NoTruncation
}

pub(super) struct TransformDeclarations {
    pub(super) arena: *const AllArenas,
    pub(super) context: Id<TransformNodesTransformationResult>,
    pub(super) get_symbol_accessibility_diagnostic: Cell<GetSymbolAccessibilityDiagnostic>,
    pub(super) needs_declare: Cell<bool>,
    pub(super) is_bundled_emit: Cell<bool>,
    pub(super) result_has_external_module_indicator: Cell<bool>,
    pub(super) needs_scope_fix_marker: Cell<bool>,
    pub(super) result_has_scope_marker: Cell<bool>,
    pub(super) enclosing_declaration: Cell<Option<Id<Node>>>,
    pub(super) necessary_type_references: RefCell<Option<HashSet<String>>>,
    pub(super) late_marked_statements:
        RefCell<Option<Vec<Id<Node /*LateVisibilityPaintedStatement*/>>>>,
    pub(super) late_statement_replacement_map: RefCell<
        Option<
            HashMap<
                NodeId,
                VisitResult, /*<LateVisibilityPaintedStatement | ExportAssignment*/
            >,
        >,
    >,
    pub(super) suppress_new_diagnostic_contexts: Cell<Option<bool>>,
    pub(super) exported_modules_from_declaration_emit: RefCell<Option<Vec<Id<Symbol>>>>,
    pub(super) factory: Id<NodeFactory>,
    pub(super) host: Id<Box<dyn EmitHost>>,
    pub(super) symbol_tracker: Cell<Option<Id<Box<dyn SymbolTracker>>>>,
    pub(super) error_name_node: Cell<Option<Id<Node /*DeclarationName*/>>>,
    pub(super) error_fallback_node: Cell<Option<Id<Node /*Declaration*/>>>,
    pub(super) current_source_file: Cell<Option<Id<Node /*SourceFile*/>>>,
    pub(super) refs: RefCell<Option<HashMap<NodeId, Id<Node /*SourceFile*/>>>>,
    pub(super) libs: RefCell<Option<HashMap<String, bool>>>,
    pub(super) emitted_imports: RefCell<Option<Vec<Id<Node /*AnyImportSyntax*/>>>>,
    pub(super) resolver: Id<Box<dyn EmitResolver>>,
    pub(super) options: Id<CompilerOptions>,
    pub(super) no_resolve: Option<bool>,
    pub(super) strip_internal: Option<bool>,
}

impl TransformDeclarations {
    pub(super) fn new(
        context: Id<TransformNodesTransformationResult>,
        arena: *const AllArenas,
    ) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let _context_ref = context.ref_(arena_ref);
        let options = context.ref_(arena_ref).get_compiler_options();
        let host = context.ref_(arena_ref).get_emit_host();
        let ret = arena_ref.alloc_transformer(Box::new(Self {
            arena,
            get_symbol_accessibility_diagnostic: Cell::new(throw_diagnostic(arena_ref)),
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
            factory: context.ref_(arena_ref).factory(),
            host: host.clone(),
            symbol_tracker: Default::default(),
            error_name_node: Default::default(),
            error_fallback_node: Default::default(),
            current_source_file: Default::default(),
            refs: Default::default(),
            libs: Default::default(),
            emitted_imports: Default::default(),
            resolver: context.ref_(arena_ref).get_emit_resolver(),
            no_resolve: options.ref_(arena_ref).no_resolve,
            strip_internal: options.ref_(arena_ref).strip_internal,
            options,
            context,
        }));
        downcast_transformer_ref::<Self>(ret, arena_ref)
            .symbol_tracker
            .set(Some(arena_ref.alloc_symbol_tracker(Box::new(
                TransformDeclarationsSymbolTracker::new(ret, host, arena_ref),
            ))));
        ret
    }

    pub(super) fn get_symbol_accessibility_diagnostic(&self) -> GetSymbolAccessibilityDiagnostic {
        self.get_symbol_accessibility_diagnostic.get()
    }

    pub(super) fn set_get_symbol_accessibility_diagnostic(
        &self,
        get_symbol_accessibility_diagnostic: GetSymbolAccessibilityDiagnostic,
    ) {
        self.get_symbol_accessibility_diagnostic
            .set(get_symbol_accessibility_diagnostic);
    }

    pub(super) fn needs_declare(&self) -> bool {
        self.needs_declare.get()
    }

    pub(super) fn set_needs_declare(&self, needs_declare: bool) {
        self.needs_declare.set(needs_declare);
    }

    pub(super) fn is_bundled_emit(&self) -> bool {
        self.is_bundled_emit.get()
    }

    pub(super) fn set_is_bundled_emit(&self, is_bundled_emit: bool) {
        self.is_bundled_emit.set(is_bundled_emit);
    }

    pub(super) fn result_has_external_module_indicator(&self) -> bool {
        self.result_has_external_module_indicator.get()
    }

    pub(super) fn set_result_has_external_module_indicator(
        &self,
        result_has_external_module_indicator: bool,
    ) {
        self.result_has_external_module_indicator
            .set(result_has_external_module_indicator);
    }

    pub(super) fn needs_scope_fix_marker(&self) -> bool {
        self.needs_scope_fix_marker.get()
    }

    pub(super) fn set_needs_scope_fix_marker(&self, needs_scope_fix_marker: bool) {
        self.needs_scope_fix_marker.set(needs_scope_fix_marker);
    }

    pub(super) fn result_has_scope_marker(&self) -> bool {
        self.result_has_scope_marker.get()
    }

    pub(super) fn set_result_has_scope_marker(&self, result_has_scope_marker: bool) {
        self.result_has_scope_marker.set(result_has_scope_marker);
    }

    pub(super) fn maybe_enclosing_declaration(&self) -> Option<Id<Node>> {
        self.enclosing_declaration.get()
    }

    pub(super) fn enclosing_declaration(&self) -> Id<Node> {
        self.enclosing_declaration.get().unwrap()
    }

    pub(super) fn set_enclosing_declaration(&self, enclosing_declaration: Option<Id<Node>>) {
        self.enclosing_declaration.set(enclosing_declaration);
    }

    pub(super) fn maybe_necessary_type_references(&self) -> Ref<Option<HashSet<String>>> {
        self.necessary_type_references.borrow()
    }

    pub(super) fn maybe_necessary_type_references_mut(&self) -> RefMut<Option<HashSet<String>>> {
        self.necessary_type_references.borrow_mut()
    }

    pub(super) fn set_necessary_type_references(
        &self,
        necessary_type_references: Option<HashSet<String>>,
    ) {
        *self.necessary_type_references.borrow_mut() = necessary_type_references;
    }

    pub(super) fn maybe_late_marked_statements(&self) -> Ref<Option<Vec<Id<Node>>>> {
        self.late_marked_statements.borrow()
    }

    pub(super) fn maybe_late_marked_statements_mut(&self) -> RefMut<Option<Vec<Id<Node>>>> {
        self.late_marked_statements.borrow_mut()
    }

    pub(super) fn late_marked_statements_mut(&self) -> RefMut<Vec<Id<Node>>> {
        ref_mut_unwrapped(&self.late_marked_statements)
    }

    pub(super) fn set_late_marked_statements(&self, late_marked_statements: Option<Vec<Id<Node>>>) {
        *self.late_marked_statements.borrow_mut() = late_marked_statements;
    }

    pub(super) fn late_statement_replacement_map(&self) -> Ref<HashMap<NodeId, VisitResult>> {
        ref_unwrapped(&self.late_statement_replacement_map)
    }

    pub(super) fn late_statement_replacement_map_mut(
        &self,
    ) -> RefMut<HashMap<NodeId, VisitResult>> {
        ref_mut_unwrapped(&self.late_statement_replacement_map)
    }

    pub(super) fn set_late_statement_replacement_map(
        &self,
        late_statement_replacement_map: Option<HashMap<NodeId, VisitResult>>,
    ) {
        *self.late_statement_replacement_map.borrow_mut() = late_statement_replacement_map;
    }

    pub(super) fn maybe_suppress_new_diagnostic_contexts(&self) -> Option<bool> {
        self.suppress_new_diagnostic_contexts.get()
    }

    pub(super) fn set_suppress_new_diagnostic_contexts(
        &self,
        suppress_new_diagnostic_contexts: Option<bool>,
    ) {
        self.suppress_new_diagnostic_contexts
            .set(suppress_new_diagnostic_contexts);
    }

    pub(super) fn maybe_exported_modules_from_declaration_emit(
        &self,
    ) -> Ref<Option<Vec<Id<Symbol>>>> {
        self.exported_modules_from_declaration_emit.borrow()
    }

    pub(super) fn maybe_exported_modules_from_declaration_emit_mut(
        &self,
    ) -> RefMut<Option<Vec<Id<Symbol>>>> {
        self.exported_modules_from_declaration_emit.borrow_mut()
    }

    pub(super) fn symbol_tracker(&self) -> Id<Box<dyn SymbolTracker>> {
        self.symbol_tracker.get().unwrap()
    }

    pub(super) fn maybe_error_name_node(&self) -> Option<Id<Node>> {
        self.error_name_node.get()
    }

    pub(super) fn set_error_name_node(&self, error_name_node: Option<Id<Node>>) {
        self.error_name_node.set(error_name_node);
    }

    pub(super) fn maybe_error_fallback_node(&self) -> Option<Id<Node>> {
        self.error_fallback_node.get()
    }

    pub(super) fn set_error_fallback_node(&self, error_fallback_node: Option<Id<Node>>) {
        self.error_fallback_node.set(error_fallback_node);
    }

    pub(super) fn current_source_file(&self) -> Id<Node> {
        self.current_source_file.get().unwrap()
    }

    pub(super) fn set_current_source_file(&self, current_source_file: Option<Id<Node>>) {
        self.current_source_file.set(current_source_file);
    }

    pub(super) fn refs(&self) -> Ref<HashMap<NodeId, Id<Node>>> {
        ref_unwrapped(&self.refs)
    }

    pub(super) fn refs_mut(&self) -> RefMut<HashMap<NodeId, Id<Node>>> {
        ref_mut_unwrapped(&self.refs)
    }

    pub(super) fn set_refs(&self, refs: Option<HashMap<NodeId, Id<Node>>>) {
        *self.refs.borrow_mut() = refs;
    }

    pub(super) fn libs(&self) -> Ref<HashMap<String, bool>> {
        ref_unwrapped(&self.libs)
    }

    pub(super) fn libs_mut(&self) -> RefMut<HashMap<String, bool>> {
        ref_mut_unwrapped(&self.libs)
    }

    pub(super) fn set_libs(&self, libs: Option<HashMap<String, bool>>) {
        *self.libs.borrow_mut() = libs;
    }

    pub(super) fn maybe_emitted_imports(&self) -> Ref<Option<Vec<Id<Node>>>> {
        self.emitted_imports.borrow()
    }

    pub(super) fn set_emitted_imports(&self, emitted_imports: Option<Vec<Id<Node>>>) {
        *self.emitted_imports.borrow_mut() = emitted_imports;
    }

    pub(super) fn record_type_reference_directives_if_necessary(
        &self,
        type_reference_directives: Option<&[String]>,
    ) {
        if type_reference_directives.is_none() {
            return;
        }
        let type_reference_directives = type_reference_directives.unwrap();
        let mut necessary_type_references = self.maybe_necessary_type_references_mut();
        let necessary_type_references = necessary_type_references.get_or_insert_default_();
        for ref_ in type_reference_directives {
            necessary_type_references.insert(ref_.clone());
        }
    }

    pub(super) fn handle_symbol_accessibility_error(
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
                    for &ref_ in symbol_accessibility_result_aliases_to_make_visible {
                        push_if_unique_eq(late_marked_statements, &ref_);
                    }
                }
            }
        } else {
            let error_info = self
                .get_symbol_accessibility_diagnostic()
                .ref_(self)
                .call(symbol_accessibility_result);
            if let Some(error_info) = error_info {
                if let Some(error_info_type_name) = error_info.ref_(self).type_name {
                    let mut args: Vec<String> =
                        vec![get_text_of_node(error_info_type_name, None, self).into_owned()];
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
                    self.context.ref_(self).add_diagnostic(
                        self.alloc_diagnostic(
                            create_diagnostic_for_node(
                                symbol_accessibility_result
                                    .error_node
                                    .unwrap_or_else(|| error_info.ref_(self).error_node),
                                error_info.ref_(self).diagnostic_message,
                                Some(args),
                                self,
                            )
                            .into(),
                        ),
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
                    self.context.ref_(self).add_diagnostic(
                        self.alloc_diagnostic(
                            create_diagnostic_for_node(
                                symbol_accessibility_result
                                    .error_node
                                    .unwrap_or_else(|| error_info.ref_(self).error_node),
                                error_info.ref_(self).diagnostic_message,
                                Some(args),
                                self,
                            )
                            .into(),
                        ),
                    );
                }
                return true;
            }
        }
        false
    }

    pub(super) fn transform_declarations_for_js(
        &self,
        source_file: Id<Node>, /*SourceFile*/
        bundled: Option<bool>,
    ) -> io::Result<Option<Vec<Id<Node>>>> {
        let old_diag = self.get_symbol_accessibility_diagnostic();
        self.set_get_symbol_accessibility_diagnostic(
            self.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(
                TransformDeclarationsForJSGetSymbolAccessibilityDiagnostic::new(source_file, self),
            )),
        );
        let result = self
            .resolver
            .ref_(self)
            .get_declaration_statements_for_source_file(
                source_file,
                declaration_emit_node_builder_flags(),
                self.symbol_tracker(),
                bundled,
            )?;
        self.set_get_symbol_accessibility_diagnostic(old_diag);
        Ok(result)
    }

    pub(super) fn transform_root(
        &self,
        node: Id<Node>, /*SourceFile | Bundle*/
    ) -> io::Result<Id<Node>> {
        if node.ref_(self).kind() == SyntaxKind::SourceFile
            && node.ref_(self).as_source_file().is_declaration_file()
        {
            return Ok(node);
        }

        if node.ref_(self).kind() == SyntaxKind::Bundle {
            self.set_is_bundled_emit(true);
            self.set_refs(Some(HashMap::new()));
            self.set_libs(Some(HashMap::new()));
            let mut has_no_default_lib = false;
            let mut bundle =
                self.factory.ref_(self).create_bundle_raw(
                    try_map(
                        &released!(node.ref_(self).as_bundle().source_files.clone()),
                        |source_file: &Option<Id<Node>>, _| -> io::Result<Option<Id<Node>>> {
                            let source_file = source_file.unwrap();
                            if source_file.ref_(self).as_source_file().is_declaration_file() {
                                return Ok(None);
                            }
                            has_no_default_lib = has_no_default_lib || source_file.ref_(self).as_source_file().has_no_default_lib();
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
                            self.set_get_symbol_accessibility_diagnostic(throw_diagnostic(self));
                            self.set_needs_scope_fix_marker(false);
                            self.set_result_has_scope_marker(false);
                            self.collect_references(
                                source_file,
                                &mut self.refs_mut()
                            )?;
                            self.collect_libs(
                                source_file,
                                &mut self.libs_mut()
                            );
                            if is_external_or_common_js_module(&source_file.ref_(self)) || is_json_source_file(&source_file.ref_(self)) {
                                self.set_result_has_external_module_indicator(false);
                                self.set_needs_declare(false);
                                let statements = if is_source_file_js(&source_file.ref_(self)) {
                                    self.factory.ref_(self).create_node_array(
                                        self.transform_declarations_for_js(
                                            source_file,
                                            Some(true)
                                        )?,
                                        None,
                                    )
                                } else {
                                    try_visit_nodes(
                                        released!(source_file.ref_(self).as_source_file().statements()),
                                        Some(|node: Id<Node>| self.visit_declaration_statements(node)),
                                        Option::<fn(Id<Node>) -> bool>::None,
                                        None,
                                        None,
                                        self,
                                    )?
                                };
                                let new_file =
                                    self.factory.ref_(self).update_source_file(
                                        source_file,
                                        vec![
                                            self.factory.ref_(self).create_module_declaration(
                                                Some(vec![]),
                                                Some(vec![
                                                    self.factory.ref_(self).create_modifier(
                                                        SyntaxKind::DeclareKeyword,
                                                    )
                                                ]),
                                                self.factory.ref_(self).create_string_literal(
                                                    released!(get_resolved_external_module_name(
                                                        &**self.context.ref_(self).get_emit_host().ref_(self),
                                                        &source_file.ref_(self),
                                                        None,
                                                    )),
                                                    None,
                                                    None,
                                                ),
                                                Some(
                                                    self.factory.ref_(self).create_module_block(
                                                        Some(
                                                            set_text_range_node_array(
                                                                self.factory.ref_(self).create_node_array(
                                                                    Some(self.transform_and_replace_late_painted_statements(
                                                                        statements,
                                                                    )?),
                                                                    None,
                                                                ),
                                                                Some(&*source_file.ref_(self).as_source_file().statements().ref_(self)),
                                                                self,
                                                            )
                                                        )
                                                    )
                                                ),
                                                None,
                                            )
                                        ],
                                        Some(true),
                                        Some(Default::default()),
                                        Some(Default::default()),
                                        Some(false),
                                        Some(Default::default()),
                                    );
                                return Ok(Some(new_file));
                            }
                            self.set_needs_declare(true);
                            let updated = if is_source_file_js(&source_file.ref_(self)) {
                                self.factory.ref_(self).create_node_array(
                                    self.transform_declarations_for_js(source_file, None)?,
                                    None,
                                )
                            } else {
                                try_visit_nodes(
                                    source_file.ref_(self).as_source_file().statements(),
                                    Some(|node: Id<Node>| self.visit_declaration_statements(node)),
                                    Option::<fn(Id<Node>) -> bool>::None,
                                    None,
                                    None,
                                    self,
                                )?
                            };
                            Ok(Some(
                                self.factory.ref_(self).update_source_file(
                                    source_file,
                                    self.transform_and_replace_late_painted_statements(
                                        updated
                                    )?,
                                    Some(true),
                                    Some(Default::default()),
                                    Some(Default::default()),
                                    Some(false),
                                    Some(Default::default()),
                                )
                            ))
                        }
                    )?,
                    Some(try_map_defined(
                        Some(&node.ref_(self).as_bundle().prepends),
                        |prepend: &Id<Node>, _| -> io::Result<Option<Id<Node>>> {
                            if prepend.ref_(self).kind() == SyntaxKind::InputFiles {
                                let source_file = create_unparsed_source_file(
                                    prepend.clone(),
                                    Some("dts"),
                                    self.strip_internal,
                                );
                                let source_file_ref = source_file.ref_(self);
                                let source_file_as_unparsed_source = source_file_ref.as_unparsed_source();
                                has_no_default_lib = has_no_default_lib || source_file_as_unparsed_source.has_no_default_lib == Some(true);
                                self.collect_references(
                                    source_file,
                                    &mut self.refs_mut(),
                                )?;
                                self.record_type_reference_directives_if_necessary(
                                    source_file_as_unparsed_source.type_reference_directives.as_deref()
                                );
                                self.collect_libs(
                                    source_file,
                                    &mut self.libs_mut(),
                                );
                                return Ok(Some(source_file));
                            }
                            Ok(Some(prepend.clone()))
                        }
                    )?),
                )
            ;
            bundle.synthetic_file_references = Some(vec![]);
            bundle.synthetic_type_references =
                Some(self.get_file_references_for_used_type_references());
            bundle.synthetic_lib_references = Some(self.get_lib_references());
            bundle.has_no_default_lib = Some(has_no_default_lib);
            let output_file_path = get_directory_path(&normalize_slashes(
                get_output_paths_for(node, &**self.host.ref_(self), true, self)
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
                self.refs()
                    .values()
                    .try_for_each(|&ref_: &Id<Node>| -> io::Result<_> {
                        reference_visitor(ref_)?;

                        Ok(())
                    })?;
            }
            return Ok(self.alloc_node(bundle.into()));
        }

        self.set_needs_declare(true);
        self.set_needs_scope_fix_marker(false);
        self.set_result_has_scope_marker(false);
        self.set_enclosing_declaration(Some(node));
        self.set_current_source_file(Some(node));
        self.set_get_symbol_accessibility_diagnostic(throw_diagnostic(self));
        self.set_is_bundled_emit(false);
        self.set_result_has_external_module_indicator(false);
        self.set_suppress_new_diagnostic_contexts(Some(false));
        self.set_late_marked_statements(None);
        self.set_late_statement_replacement_map(Some(Default::default()));
        self.set_necessary_type_references(None);
        self.set_refs(Some(Default::default()));
        {
            let mut refs = self.refs_mut();
            self.collect_references(self.current_source_file(), &mut refs)?;
        }
        self.set_libs(Some(Default::default()));
        {
            let mut libs = self.libs_mut();
            self.collect_libs(self.current_source_file(), &mut libs);
        }
        let mut references: Vec<FileReference> = vec![];
        let output_file_path = get_directory_path(&normalize_slashes(
            get_output_paths_for(node, &**self.host.ref_(self), true, self)
                .declaration_file_path
                .as_ref()
                .unwrap(),
        ));
        let mut reference_visitor =
            self.map_references_into_array(node, &mut references, &output_file_path);
        let mut combined_statements: Id<NodeArray>/*<Statement>*/;
        if is_source_file_js(&self.current_source_file().ref_(self)) {
            combined_statements = self
                .factory
                .ref_(self)
                .create_node_array(self.transform_declarations_for_js(node, None)?, None);
            self.refs()
                .values()
                .try_for_each(|&ref_: &Id<Node>| -> io::Result<_> {
                    reference_visitor(ref_)?;

                    Ok(())
                })?;
            self.set_emitted_imports(Some(filter(
                &combined_statements.ref_(self),
                |statement: &Id<Node>| is_any_import_syntax(&statement.ref_(self)),
            )));
        } else {
            let statements = try_visit_nodes(
                released!(node.ref_(self).as_source_file().statements()),
                Some(|node: Id<Node>| self.visit_declaration_statements(node)),
                Option::<fn(Id<Node>) -> bool>::None,
                None,
                None,
                self,
            )?;
            combined_statements = set_text_range_node_array(
                self.factory.ref_(self).create_node_array(
                    Some(self.transform_and_replace_late_painted_statements(statements)?),
                    None,
                ),
                Some(&*node.ref_(self).as_source_file().statements().ref_(self)),
                self,
            );
            self.refs()
                .values()
                .try_for_each(|&ref_: &Id<Node>| -> io::Result<_> {
                    reference_visitor(ref_)?;

                    Ok(())
                })?;
            self.set_emitted_imports(Some(filter(
                &combined_statements.ref_(self),
                |&statement: &Id<Node>| is_any_import_syntax(&statement.ref_(self)),
            )));
            if is_external_module(&node.ref_(self))
                && (!self.result_has_external_module_indicator()
                    || self.needs_scope_fix_marker() && !self.result_has_scope_marker())
            {
                combined_statements = set_text_range_node_array(
                    self.factory.ref_(self).create_node_array(
                        Some({
                            let mut combined_statements = combined_statements.ref_(self).to_vec();
                            combined_statements
                                .push(create_empty_exports(&self.factory.ref_(self)));
                            combined_statements
                        }),
                        None,
                    ),
                    Some(&*combined_statements.ref_(self)),
                    self,
                );
            }
        }
        drop(reference_visitor);
        let updated = self.factory.ref_(self).update_source_file(
            node,
            combined_statements,
            Some(true),
            Some(Rc::new(RefCell::new(references))),
            Some(Rc::new(RefCell::new(
                self.get_file_references_for_used_type_references(),
            ))),
            released!(Some(node.ref_(self).as_source_file().has_no_default_lib())),
            Some(Rc::new(RefCell::new(self.get_lib_references()))),
        );
        *updated
            .ref_(self)
            .as_source_file()
            .maybe_exported_modules_from_declaration_emit() =
            self.maybe_exported_modules_from_declaration_emit().clone();
        Ok(updated)
    }

    pub(super) fn get_lib_references(&self) -> Vec<FileReference> {
        map(self.libs().keys(), |lib: &String, _| {
            FileReference::new(-1, -1, lib.clone())
        })
    }

    pub(super) fn get_file_references_for_used_type_references(&self) -> Vec<FileReference> {
        self.maybe_necessary_type_references()
            .as_ref()
            .map(|necessary_type_references| {
                map_defined(Some(necessary_type_references), |key: &String, _| {
                    self.get_file_reference_for_type_name(key)
                })
            })
            .unwrap_or_default()
    }

    pub(super) fn get_file_reference_for_type_name(
        &self,
        type_name: &str,
    ) -> Option<FileReference> {
        if let Some(emitted_imports) = self.maybe_emitted_imports().as_ref() {
            for import_statement in emitted_imports {
                if is_import_equals_declaration(&import_statement.ref_(self))
                    && is_external_module_reference(
                        &import_statement
                            .ref_(self)
                            .as_import_equals_declaration()
                            .module_reference
                            .ref_(self),
                    )
                {
                    let expr = import_statement
                        .ref_(self)
                        .as_import_equals_declaration()
                        .module_reference
                        .ref_(self)
                        .as_external_module_reference()
                        .expression;
                    if is_string_literal_like(&expr.ref_(self))
                        && &**expr.ref_(self).as_literal_like_node().text() == type_name
                    {
                        return None;
                    }
                } else if is_import_declaration(&import_statement.ref_(self)) && {
                    let import_statement_ref = import_statement.ref_(self);
                    let import_statement_as_import_declaration =
                        import_statement_ref.as_import_declaration();
                    is_string_literal(
                        &import_statement_as_import_declaration
                            .module_specifier
                            .ref_(self),
                    ) && &**import_statement_as_import_declaration
                        .module_specifier
                        .ref_(self)
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

    pub(super) fn map_references_into_array<'a>(
        &'a self,
        node: Id<Node>,
        references: &'a mut Vec<FileReference>,
        output_file_path: &'a str,
    ) -> impl FnMut(Id<Node> /*SourceFile*/) -> io::Result<()> + 'a {
        move |file: Id<Node>| {
            let file_ref = file.ref_(self);
            let file_as_source_file = file_ref.as_source_file();
            let decl_file_name: String;
            if file_as_source_file.is_declaration_file() {
                decl_file_name = file_as_source_file.file_name().clone();
            } else {
                if self.is_bundled_emit()
                    && contains(Some(&node.ref_(self).as_bundle().source_files), &Some(file))
                {
                    return Ok(());
                }
                let paths = get_output_paths_for(file, &**self.host.ref_(self), true, self);
                decl_file_name = paths
                    .declaration_file_path
                    .clone()
                    .non_empty()
                    .or_else(|| paths.js_file_path.clone().non_empty())
                    .unwrap_or_else(|| file_as_source_file.file_name().clone());
            }

            if !decl_file_name.is_empty() {
                let specifier = module_specifiers::get_module_specifier(
                    self.options.clone(),
                    self.current_source_file(),
                    &to_path(
                        output_file_path,
                        Some(&ScriptReferenceHost::get_current_directory(
                            &**self.host.ref_(self),
                        )),
                        |file_name| self.host.ref_(self).get_canonical_file_name(file_name),
                    ),
                    &to_path(
                        &decl_file_name,
                        Some(&ScriptReferenceHost::get_current_directory(
                            &**self.host.ref_(self),
                        )),
                        |file_name| self.host.ref_(self).get_canonical_file_name(file_name),
                    ),
                    &**self.host.ref_(self),
                    self,
                )?;
                if !path_is_relative(&specifier) {
                    self.record_type_reference_directives_if_necessary(Some(&[specifier]));
                    return Ok(());
                }

                let mut file_name = get_relative_path_to_directory_or_url(
                    output_file_path,
                    &decl_file_name,
                    &ScriptReferenceHost::get_current_directory(&**self.host.ref_(self)),
                    |file_name: &str| self.host.ref_(self).get_canonical_file_name(file_name),
                    false,
                );
                if starts_with(&file_name, "./") && has_extension(&file_name) {
                    file_name = file_name[2..].to_owned();
                }

                if starts_with(&file_name, "node_modules/")
                    || path_contains_node_modules(&file_name)
                {
                    return Ok(());
                }

                references.push(FileReference::new(-1, -1, file_name));
            }

            Ok(())
        }
    }

    pub(super) fn collect_references(
        &self,
        source_file: Id<Node>, /*SourceFile | UnparsedSource*/
        ret: &mut HashMap<NodeId, Id<Node /*SourceFile*/>>,
    ) -> io::Result<()> {
        if self.no_resolve == Some(true)
            || !is_unparsed_source(&source_file.ref_(self))
                && is_source_file_js(&source_file.ref_(self))
        {
            return Ok(())/*ret*/;
        }
        try_maybe_for_each(
            source_file
                .ref_(self)
                .as_source_file()
                .maybe_referenced_files()
                .as_ref()
                .map(|source_file_referenced_files| (**source_file_referenced_files).borrow())
                .as_deref(),
            |f: &FileReference, _| -> io::Result<Option<()>> {
                let elem = self
                    .host
                    .ref_(self)
                    .get_source_file_from_reference(source_file, f)?;
                if let Some(elem) = elem {
                    ret.insert(get_original_node_id(elem, self), elem);
                }
                Ok(None)
            },
        )?;
        // return ret;

        Ok(())
    }

    pub(super) fn collect_libs(
        &self,
        source_file: Id<Node>, /*SourceFile | UnparsedSource*/
        ret: &mut HashMap<String, bool>,
    ) {
        // TODO: expose some trait for unifying this?
        match source_file.ref_(self).kind() {
            SyntaxKind::SourceFile => {
                maybe_for_each(
                    source_file
                        .ref_(self)
                        .as_source_file()
                        .maybe_lib_reference_directives()
                        .as_ref()
                        .map(|source_file_lib_reference_directives| {
                            (**source_file_lib_reference_directives).borrow()
                        })
                        .as_deref(),
                    |ref_: &FileReference, _| -> Option<()> {
                        let lib = self.host.ref_(self).get_lib_file_from_reference(ref_);
                        if lib.is_some() {
                            ret.insert(to_file_name_lower_case(&ref_.file_name), true);
                        }
                        None
                    },
                );
            }
            SyntaxKind::UnparsedSource => {
                maybe_for_each(
                    Some(
                        &source_file
                            .ref_(self)
                            .as_unparsed_source()
                            .lib_reference_directives,
                    ),
                    |ref_: &FileReference, _| -> Option<()> {
                        let lib = self.host.ref_(self).get_lib_file_from_reference(ref_);
                        if lib.is_some() {
                            ret.insert(to_file_name_lower_case(&ref_.file_name), true);
                        }
                        None
                    },
                );
            }
            _ => unreachable!(),
        }
        // return ret;
    }

    pub(super) fn filter_binding_pattern_initializers(
        &self,
        name: Id<Node>, /*BindingName*/
    ) -> io::Result<Id<Node>> {
        Ok(if name.ref_(self).kind() == SyntaxKind::Identifier {
            name
        } else {
            if name.ref_(self).kind() == SyntaxKind::ArrayBindingPattern {
                self.factory.ref_(self).update_array_binding_pattern(
                    name,
                    try_visit_nodes(
                        name.ref_(self).as_array_binding_pattern().elements,
                        Some(|node: Id<Node>| -> io::Result<_> {
                            Ok(Some(self.visit_binding_element(node)?.into()))
                        }),
                        Option::<fn(Id<Node>) -> bool>::None,
                        None,
                        None,
                        self,
                    )?,
                )
            } else {
                self.factory.ref_(self).update_object_binding_pattern(
                    name,
                    try_visit_nodes(
                        released!(name.ref_(self).as_object_binding_pattern().elements),
                        Some(|node: Id<Node>| -> io::Result<_> {
                            Ok(Some(self.visit_binding_element(node)?.into()))
                        }),
                        Option::<fn(Id<Node>) -> bool>::None,
                        None,
                        None,
                        self,
                    )?,
                )
            }
        })
    }

    pub(super) fn visit_binding_element(
        &self,
        elem: Id<Node>, /*ArrayBindingElement*/
    ) -> io::Result<Id<Node>> {
        if elem.ref_(self).kind() == SyntaxKind::OmittedExpression {
            return Ok(elem);
        }
        Ok(self.factory.ref_(self).update_binding_element(
            elem,
            released!(elem.ref_(self).as_binding_element().dot_dot_dot_token),
            released!(elem.ref_(self).as_binding_element().property_name),
            self.filter_binding_pattern_initializers(released!(elem
                .ref_(self)
                .as_binding_element()
                .name()))?,
            if self.should_print_with_initializer(elem)? {
                released!(elem.ref_(self).as_binding_element().maybe_initializer())
            } else {
                None
            },
        ))
    }

    pub(super) fn ensure_parameter(
        &self,
        p: Id<Node>, /*ParameterDeclaration*/
        modifier_mask: Option<ModifierFlags>,
        type_: Option<Id<Node> /*TypeNode*/>,
    ) -> io::Result<Id<Node /*ParameterDeclaration*/>> {
        let mut old_diag: Option<GetSymbolAccessibilityDiagnostic> = None;
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            old_diag = Some(self.get_symbol_accessibility_diagnostic());
            self.set_get_symbol_accessibility_diagnostic(
                create_get_symbol_accessibility_diagnostic_for_node(p, self),
            );
        }
        let new_param = self.factory.ref_(self).update_parameter_declaration(
            p,
            Option::<Id<NodeArray>>::None,
            Some(mask_modifiers(p, modifier_mask, None, self)),
            released!(p.ref_(self).as_parameter_declaration().dot_dot_dot_token),
            Some(self.filter_binding_pattern_initializers(released!(
                p.ref_(self).as_parameter_declaration().name()
            ))?),
            if self.resolver.ref_(self).is_optional_parameter(p)? {
                Some(
                    released!(p.ref_(self).as_parameter_declaration().question_token)
                        .unwrap_or_else(|| {
                            self.factory
                                .ref_(self)
                                .create_token(SyntaxKind::QuestionToken)
                        }),
                )
            } else {
                None
            },
            self.ensure_type(
                p,
                type_.or_else(|| p.ref_(self).as_parameter_declaration().maybe_type()),
                Some(true),
            )?,
            self.ensure_no_initializer(p)?,
        );
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(old_diag.unwrap());
        }
        Ok(new_param)
    }

    pub(super) fn should_print_with_initializer(&self, node: Id<Node>) -> io::Result<bool> {
        Ok(can_have_literal_initializer(node, self)
            && self.resolver.ref_(self).is_literal_const_declaration(
                get_parse_tree_node(Some(node), Option::<fn(Id<Node>) -> bool>::None, self)
                    .unwrap(),
            )?)
    }

    pub(super) fn ensure_no_initializer(
        &self,
        node: Id<Node>, /*CanHaveLiteralInitializer*/
    ) -> io::Result<Option<Id<Node>>> {
        if self.should_print_with_initializer(node)? {
            return Ok(Some(
                self.resolver.ref_(self).create_literal_const_value(
                    get_parse_tree_node(Some(node), Option::<fn(Id<Node>) -> bool>::None, self)
                        .unwrap(),
                    self.symbol_tracker(),
                )?,
            ));
        }
        Ok(None)
    }
}

impl_has_arena!(TransformDeclarations);

impl TransformerInterface for TransformDeclarations {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        self.transform_root(node)
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

pub(super) fn throw_diagnostic(arena: &impl HasArena) -> GetSymbolAccessibilityDiagnostic {
    per_arena!(
        Box<dyn GetSymbolAccessibilityDiagnosticInterface>,
        arena,
        arena.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(ThrowDiagnostic))
    )
}

pub(super) struct ThrowDiagnostic;

impl GetSymbolAccessibilityDiagnosticInterface for ThrowDiagnostic {
    fn call(
        &self,
        _symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        Debug_.fail(Some("Diagnostic emitted without context"));
    }
}

pub(super) struct TransformDeclarationsSymbolTracker {
    arena: *const AllArenas,
    is_track_symbol_disabled: Cell<bool>,
    pub(super) transform_declarations: Transformer,
    pub(super) host: Id<Box<dyn EmitHost>>,
}

impl TransformDeclarationsSymbolTracker {
    pub(super) fn new(
        transform_declarations: Transformer,
        host: Id<Box<dyn EmitHost>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            is_track_symbol_disabled: Default::default(),
            transform_declarations,
            host,
        }
    }

    pub(super) fn transform_declarations(&self) -> debug_cell::Ref<'_, TransformDeclarations> {
        downcast_transformer_ref(self.transform_declarations, self)
    }

    pub(super) fn error_declaration_name_with_fallback(&self) -> Cow<'static, str> {
        if let Some(error_name_node) = self.transform_declarations().maybe_error_name_node() {
            declaration_name_to_string(Some(error_name_node), self)
        } else if let Some(error_fallback_node_name) = self
            .transform_declarations()
            .maybe_error_fallback_node()
            .and_then(|error_fallback_node| {
                get_name_of_declaration(Some(error_fallback_node), self)
            })
        {
            declaration_name_to_string(Some(error_fallback_node_name), self)
        } else if let Some(ref error_fallback_node) = self
            .transform_declarations()
            .maybe_error_fallback_node()
            .filter(|error_fallback_node| is_export_assignment(&error_fallback_node.ref_(self)))
        {
            if error_fallback_node
                .ref_(self)
                .as_export_assignment()
                .is_export_equals
                == Some(true)
            {
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
        symbol: Id<Symbol>,
        enclosing_declaration: Option<Id<Node>>,
        meaning: SymbolFlags,
    ) -> Option<io::Result<bool>> {
        if self.is_track_symbol_disabled.get() {
            return Some(Ok(false));
        }
        if symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::TypeParameter)
        {
            return Some(Ok(false));
        }
        let issued_diagnostic = self
            .transform_declarations()
            .handle_symbol_accessibility_error(&match self
                .transform_declarations()
                .resolver
                .ref_(self)
                .is_symbol_accessible(
                    symbol,
                    // TODO: it sort of looks like maybe the signature for .track_symbol() should take
                    // an Option<Id<Node>> instead?
                    enclosing_declaration,
                    Some(meaning),
                    true,
                ) {
                Err(err) => return Some(Err(err)),
                Ok(value) => value,
            });
        self.transform_declarations()
            .record_type_reference_directives_if_necessary(
                match self
                    .transform_declarations()
                    .resolver
                    .ref_(self)
                    .get_type_reference_directives_for_symbol(symbol, Some(meaning))
                {
                    Err(err) => return Some(Err(err)),
                    Ok(value) => value,
                }
                .as_deref(),
            );
        Some(Ok(issued_diagnostic))
    }

    fn is_track_symbol_supported(&self) -> bool {
        true
    }

    fn disable_track_symbol(&self) {
        self.is_track_symbol_disabled.set(true);
    }

    fn reenable_track_symbol(&self) {
        self.is_track_symbol_disabled.set(false);
    }

    fn report_inaccessible_this_error(&self) {
        if let Some(error_name_node_or_error_fallback_node) = self
            .transform_declarations()
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations().maybe_error_fallback_node())
        {
            self.transform_declarations().context.ref_(self).add_diagnostic(
                self.alloc_diagnostic(create_diagnostic_for_node(
                    error_name_node_or_error_fallback_node,
                    &Diagnostics::The_inferred_type_of_0_references_an_inaccessible_1_type_A_type_annotation_is_necessary,
                    Some(vec![
                        self.error_declaration_name_with_fallback().into_owned(),
                        "this".to_owned()
                    ]),
                    self,
                ).into())
            );
        }
    }

    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        true
    }

    fn report_inaccessible_unique_symbol_error(&self) {
        if let Some(error_name_node_or_error_fallback_node) = self
            .transform_declarations()
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations().maybe_error_fallback_node())
        {
            self.transform_declarations().context.ref_(self).add_diagnostic(
                self.alloc_diagnostic(create_diagnostic_for_node(
                    error_name_node_or_error_fallback_node,
                    &Diagnostics::The_inferred_type_of_0_references_an_inaccessible_1_type_A_type_annotation_is_necessary,
                    Some(vec![
                        self.error_declaration_name_with_fallback().into_owned(),
                        "unique symbol".to_owned(),
                    ]),
                    self,
                ).into())
            );
        }
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        true
    }

    fn report_cyclic_structure_error(&self) {
        if let Some(error_name_node_or_error_fallback_node) = self
            .transform_declarations()
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations().maybe_error_fallback_node())
        {
            self.transform_declarations().context.ref_(self).add_diagnostic(
                self.alloc_diagnostic(create_diagnostic_for_node(
                    error_name_node_or_error_fallback_node,
                    &Diagnostics::The_inferred_type_of_0_references_a_type_with_a_cyclic_structure_which_cannot_be_trivially_serialized_A_type_annotation_is_necessary,
                    Some(vec![
                        self.error_declaration_name_with_fallback().into_owned(),
                    ]),
                    self,
                ).into())
        );
        }
    }

    fn is_report_cyclic_structure_error_supported(&self) -> bool {
        true
    }

    fn report_private_in_base_of_class_expression(&self, property_name: &str) {
        if let Some(error_name_node_or_error_fallback_node) = self
            .transform_declarations()
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations().maybe_error_fallback_node())
        {
            self.transform_declarations().context.ref_(self).add_diagnostic(
                self.alloc_diagnostic(create_diagnostic_for_node(
                    error_name_node_or_error_fallback_node,
                    &Diagnostics::Property_0_of_exported_class_expression_may_not_be_private_or_protected,
                    Some(vec![
                        property_name.to_owned()
                    ]),
                    self,
                ).into())
            );
        }
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        true
    }

    fn report_likely_unsafe_import_required_error(&self, specifier: &str) {
        if let Some(error_name_node_or_error_fallback_node) = self
            .transform_declarations()
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations().maybe_error_fallback_node())
        {
            self.transform_declarations().context.ref_(self).add_diagnostic(
                self.alloc_diagnostic(create_diagnostic_for_node(
                    error_name_node_or_error_fallback_node,
                    &Diagnostics::The_inferred_type_of_0_cannot_be_named_without_a_reference_to_1_This_is_likely_not_portable_A_type_annotation_is_necessary,
                    Some(vec![
                        self.error_declaration_name_with_fallback().into_owned(),
                        specifier.to_owned(),
                    ]),
                    self,
                ).into())
            );
        }
    }

    fn is_report_likely_unsafe_import_required_error_supported(&self) -> bool {
        true
    }

    fn report_truncation_error(&self) {
        if let Some(error_name_node_or_error_fallback_node) = self
            .transform_declarations()
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations().maybe_error_fallback_node())
        {
            self.transform_declarations().context.ref_(self).add_diagnostic(
                self.alloc_diagnostic(create_diagnostic_for_node(
                    error_name_node_or_error_fallback_node,
                    &Diagnostics::The_inferred_type_of_this_node_exceeds_the_maximum_length_the_compiler_will_serialize_An_explicit_type_annotation_is_needed,
                    None,
                    self,
                ).into())
            );
        }
    }

    fn is_module_resolver_host_supported(&self) -> bool {
        true
    }

    fn module_resolver_host(
        &self,
    ) -> Option<IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory> {
        Some(self.host.into())
    }

    fn track_referenced_ambient_module(
        &self,
        node: Id<Node>, /*ModuleDeclaration*/
        symbol: Id<Symbol>,
    ) -> io::Result<()> {
        let directives = self
            .transform_declarations()
            .resolver
            .ref_(self)
            .get_type_reference_directives_for_symbol(symbol, Some(SymbolFlags::All))?;
        if let Some(directives) = directives.non_empty() {
            return Ok(self
                .transform_declarations()
                .record_type_reference_directives_if_necessary(Some(&directives)));
        }
        let container = get_source_file_of_node(node, self);
        self.transform_declarations()
            .refs_mut()
            .insert(get_original_node_id(container, self), container);

        Ok(())
    }

    fn is_track_referenced_ambient_module_supported(&self) -> bool {
        true
    }

    fn track_external_module_symbol_of_import_type_node(&self, symbol: Id<Symbol>) {
        if !self.transform_declarations().is_bundled_emit() {
            self.transform_declarations()
                .maybe_exported_modules_from_declaration_emit_mut()
                .get_or_insert_default_()
                .push(symbol);
        }
    }

    fn report_nonlocal_augmentation(
        &self,
        containing_file: Id<Node>, /*SourceFile*/
        parent_symbol: Id<Symbol>,
        symbol: Id<Symbol>,
    ) {
        let primary_declaration = parent_symbol
            .ref_(self)
            .maybe_declarations()
            .as_ref()
            .and_then(|parent_symbol_declarations| {
                parent_symbol_declarations
                    .into_iter()
                    .find(|&&d: &&Id<Node>| get_source_file_of_node(d, self) == containing_file)
                    .cloned()
            });
        let augmenting_declarations = maybe_filter(
            symbol.ref_(self).maybe_declarations().as_deref(),
            |&d: &Id<Node>| get_source_file_of_node(d, self) != containing_file,
        );
        if let Some(augmenting_declarations) = augmenting_declarations {
            for augmentations in augmenting_declarations {
                self.transform_declarations().context.ref_(self).add_diagnostic(
                    add_related_info_rc(
                        self.alloc_diagnostic(create_diagnostic_for_node(
                            augmentations,
                            &Diagnostics::Declaration_augments_declaration_in_another_file_This_cannot_be_serialized,
                            None,
                            self,
                        ).into()),
                        vec![
                            self.alloc_diagnostic_related_information(create_diagnostic_for_node(
                                primary_declaration.unwrap(),
                                &Diagnostics::This_is_the_declaration_being_augmented_Consider_moving_the_augmenting_declaration_into_the_same_file,
                                None,
                                self,
                            ).into()),
                        ],
                        self
                    )
                );
            }
        }
    }

    fn is_report_nonlocal_augmentation_supported(&self) -> bool {
        true
    }

    fn report_non_serializable_property(&self, property_name: &str) {
        if let Some(error_name_node_or_error_fallback_node) = self
            .transform_declarations()
            .maybe_error_name_node()
            .or_else(|| self.transform_declarations().maybe_error_fallback_node())
        {
            self.transform_declarations().context.ref_(self).add_diagnostic(
                self.alloc_diagnostic(create_diagnostic_for_node(
                    error_name_node_or_error_fallback_node,
                    &Diagnostics::The_type_of_this_node_cannot_be_serialized_because_its_property_0_cannot_be_serialized,
                    Some(vec![
                        property_name.to_owned()
                    ]),
                    self,
                ).into())
            );
        }
    }

    fn is_report_non_serializable_property_supported(&self) -> bool {
        true
    }
}

impl_has_arena!(TransformDeclarationsSymbolTracker);

pub(super) struct TransformDeclarationsForJSGetSymbolAccessibilityDiagnostic {
    arena: *const AllArenas,
    pub(super) source_file: Id<Node /*SourceFile*/>,
}

impl TransformDeclarationsForJSGetSymbolAccessibilityDiagnostic {
    fn new(source_file: Id<Node /*SourceFile*/>, arena: &impl HasArena) -> Self {
        Self {
            source_file,
            arena: arena.arena(),
        }
    }
}

impl GetSymbolAccessibilityDiagnosticInterface
    for TransformDeclarationsForJSGetSymbolAccessibilityDiagnostic
{
    fn call(&self, s: &SymbolAccessibilityResult) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        if let Some(s_error_node) = s
            .error_node
            .filter(|s_error_node| can_produce_diagnostics(&s_error_node.ref_(self)))
        {
            create_get_symbol_accessibility_diagnostic_for_node(s_error_node, self)
                .ref_(self)
                .call(s)
        } else {
            Some(self.alloc_symbol_accessibility_diagnostic(SymbolAccessibilityDiagnostic {
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

impl_has_arena!(TransformDeclarationsForJSGetSymbolAccessibilityDiagnostic);

struct TransformDeclarationsFactory {
    arena: *const AllArenas,
}

impl TransformDeclarationsFactory {
    fn new(arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
        }
    }
}

impl TransformerFactoryInterface for TransformDeclarationsFactory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        TransformDeclarations::new(context, self.arena)
    }
}

impl_has_arena!(TransformDeclarationsFactory);

pub fn transform_declarations(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformDeclarationsFactory::new(arena)))
}
