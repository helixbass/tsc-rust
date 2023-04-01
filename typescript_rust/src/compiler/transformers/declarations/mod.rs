use std::{
    borrow::Cow,
    cell::{Cell, RefCell, RefMut},
    collections::{HashMap, HashSet},
    mem,
};

use gc::{Finalize, Gc, GcCell, GcCellRefMut, Trace};

use crate::{
    compiler::scanner::skip_trivia, create_diagnostic_for_node, declaration_name_to_string, filter,
    gc_cell_ref_mut_unwrapped, get_factory, get_leading_comment_ranges,
    get_leading_comment_ranges_of_node, get_name_of_declaration, get_original_node_id,
    get_parse_tree_node, get_source_file_of_node, get_text_of_node, get_trailing_comment_ranges,
    is_export_assignment, is_source_file_not_json, last, maybe_concatenate, maybe_for_each_bool,
    push_if_unique_gc, string_contains, transform_nodes, BaseNodeFactorySynthetic, CommentRange,
    CompilerOptions, Debug_, Diagnostic, Diagnostics, EmitHost, EmitResolver,
    GetSymbolAccessibilityDiagnostic, GetSymbolAccessibilityDiagnosticInterface,
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, Node, NodeBuilderFlags, NodeFactory,
    NodeId, NodeInterface, NonEmpty, ReadonlyTextRange, ScriptReferenceHost, SourceFileLike,
    Symbol, SymbolAccessibility, SymbolAccessibilityDiagnostic, SymbolAccessibilityResult,
    SymbolFlags, SymbolInterface, SymbolTracker, SyntaxKind, TextRange, TransformationContext,
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

    fn is_bundled_emit(&self) -> bool {
        self.is_bundled_emit.get()
    }

    fn maybe_necessary_type_references_mut(&self) -> RefMut<Option<HashSet<String>>> {
        self.necessary_type_references.borrow_mut()
    }

    fn maybe_late_marked_statements_mut(&self) -> GcCellRefMut<Option<Vec<Gc<Node>>>> {
        self.late_marked_statements.borrow_mut()
    }

    fn maybe_exported_modules_from_declaration_emit_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Symbol>>>> {
        self.exported_modules_from_declaration_emit.borrow_mut()
    }

    fn maybe_error_name_node(&self) -> Option<Gc<Node>> {
        self.error_name_node.borrow().clone()
    }

    fn maybe_error_fallback_node(&self) -> Option<Gc<Node>> {
        self.error_fallback_node.borrow().clone()
    }

    fn refs_mut(
        &self,
    ) -> GcCellRefMut<Option<HashMap<NodeId, Gc<Node>>>, HashMap<NodeId, Gc<Node>>> {
        gc_cell_ref_mut_unwrapped(&self.refs)
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

    fn transform_root(&self, node: &Node /*SourceFile | Bundle*/) -> Gc<Node> {
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
                create_diagnostic_for_node(error_name_node_or_error_fallback_node, &Diagnostics::The_inferred_type_of_this_node_exceeds_the_maximum_length_the_compiler_will_serialize_An_explicit_type_annotation_is_needed, None).into()
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
        augmenting_symbol: &Symbol,
    ) {
        unimplemented!()
    }

    fn is_report_nonlocal_augmentation_supported(&self) -> bool {
        true
    }

    fn report_non_serializable_property(&self, property_name: &str) {
        unimplemented!()
    }

    fn is_report_non_serializable_property_supported(&self) -> bool {
        true
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
