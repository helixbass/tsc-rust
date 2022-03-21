#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{
    get_next_merge_id, get_node_id, get_symbol_id, increment_next_merge_id,
    MembersOrExportsResolutionKind,
};
use crate::{
    add_range, add_related_info, are_option_rcs_equal, compare_diagnostics, compare_paths,
    create_compiler_diagnostic, create_diagnostic_for_file_from_message_chain,
    create_diagnostic_for_node_from_message_chain, create_file_diagnostic, create_symbol_table,
    filter, find_ancestor, for_each, for_each_bool, for_each_child_bool, get_ancestor,
    get_check_flags, get_containing_class, get_emit_script_target,
    get_enclosing_block_scope_container, get_expando_initializer, get_jsdoc_deprecated_tag,
    get_name_of_declaration, get_name_of_expando, get_or_update, has_static_modifier, index_of_rc,
    is_binding_element, is_class_declaration, is_class_static_block_declaration,
    is_computed_property_name, is_external_or_common_js_module, is_for_in_or_of_statement,
    is_function_like, is_global_scope_augmentation, is_identifier, is_interface_declaration,
    is_nullish_coalesce, is_object_binding_pattern, is_optional_chain, is_parameter,
    is_parameter_property_declaration, is_private_identifier, is_property_declaration, is_static,
    is_this_property, is_type_alias_declaration, is_type_node, length, maybe_for_each,
    null_transformation_context, out_file, push_if_unique_rc, set_text_range_pos_end,
    set_value_declaration, some, synthetic_factory, try_cast, visit_each_child,
    CancellationTokenDebuggable, Comparison, DiagnosticCategory, DiagnosticInterface,
    DiagnosticMessageChain, DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface,
    Diagnostics, DuplicateInfoForFiles, DuplicateInfoForSymbol, EmitResolverDebuggable,
    FindAncestorCallbackReturn, HasInitializerInterface, InternalSymbolName, ModuleKind,
    NamedDeclarationInterface, NodeArray, NodeFlags, PatternAmbientModule, ReadonlyTextRange,
    ScriptTarget, VisitResult, __String, create_diagnostic_for_node, escape_leading_underscores,
    factory, get_first_identifier, get_source_file_of_node, is_jsx_opening_fragment,
    parse_isolated_entity_name, unescape_leading_underscores, visit_node, BaseTransientSymbol,
    CheckFlags, Debug_, Diagnostic, DiagnosticMessage, Node, NodeInterface, NodeLinks, Symbol,
    SymbolFlags, SymbolInterface, SymbolLinks, SymbolTable, SyntaxKind, TransientSymbol,
    TransientSymbolInterface, TypeChecker,
};

impl TypeChecker {
    pub(super) fn get_jsx_namespace_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> __String {
        if let Some(location) = location {
            let location = location.borrow();
            let file = get_source_file_of_node(Some(location));
            if let Some(file) = file {
                let file_as_source_file = file.as_source_file();
                if is_jsx_opening_fragment(location) {
                    if let Some(file_local_jsx_fragment_namespace) = file_as_source_file
                        .maybe_local_jsx_fragment_namespace()
                        .as_ref()
                    {
                        return file_local_jsx_fragment_namespace.clone();
                    }
                    let file_pragmas = file_as_source_file.pragmas();
                    let jsx_fragment_pragma = file_pragmas.get("jsxfrag");
                    if let Some(jsx_fragment_pragma) = jsx_fragment_pragma {
                        let chosen_pragma = &jsx_fragment_pragma[0];
                        let mut file_local_jsx_fragment_factory =
                            file_as_source_file.maybe_local_jsx_fragment_factory();
                        *file_local_jsx_fragment_factory = parse_isolated_entity_name(
                            chosen_pragma
                                .arguments
                                .as_pragma_argument_type_factory()
                                .factory
                                .clone(),
                            self.language_version,
                        );
                        visit_node(
                            file_local_jsx_fragment_factory.as_deref(),
                            Some(|node: &Node| self.mark_as_synthetic(node)),
                            Option::<fn(&Node) -> bool>::None,
                            Option::<fn(&[Rc<Node>]) -> Rc<Node>>::None,
                        );
                        if let Some(file_local_jsx_fragment_factory) =
                            file_local_jsx_fragment_factory.as_ref()
                        {
                            let ret = get_first_identifier(file_local_jsx_fragment_factory)
                                .as_identifier()
                                .escaped_text
                                .clone();
                            *file_as_source_file.maybe_local_jsx_fragment_namespace() =
                                Some(ret.clone());
                            return ret;
                        }
                    }
                    let entity = self.get_jsx_fragment_factory_entity(location);
                    if let Some(entity) = entity {
                        *file_as_source_file.maybe_local_jsx_fragment_factory() =
                            Some(entity.clone());
                        let ret = get_first_identifier(&entity)
                            .as_identifier()
                            .escaped_text
                            .clone();
                        *file_as_source_file.maybe_local_jsx_fragment_namespace() =
                            Some(ret.clone());
                        return ret;
                    }
                } else {
                    let local_jsx_namespace = self.get_local_jsx_namespace(&file);
                    if let Some(local_jsx_namespace) = local_jsx_namespace {
                        *file_as_source_file.maybe_local_jsx_namespace() =
                            Some(local_jsx_namespace.clone());
                        return local_jsx_namespace;
                    }
                }
            }
        }
        let mut _jsx_namespace = self._jsx_namespace.borrow_mut();
        if _jsx_namespace.is_none() {
            *_jsx_namespace = Some(__String::new("React".to_owned()));
            if let Some(compiler_options_jsx_factory) = self.compiler_options.jsx_factory.as_ref() {
                let mut _jsx_factory_entity = self._jsx_factory_entity.borrow_mut();
                *_jsx_factory_entity = parse_isolated_entity_name(
                    compiler_options_jsx_factory.clone(),
                    self.language_version,
                );
                visit_node(
                    _jsx_factory_entity.as_deref(),
                    Some(|node: &Node| self.mark_as_synthetic(node)),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Rc<Node>]) -> Rc<Node>>::None,
                );
                if let Some(_jsx_factory_entity) = _jsx_factory_entity.as_ref() {
                    *_jsx_namespace = Some(
                        get_first_identifier(_jsx_factory_entity)
                            .as_identifier()
                            .escaped_text
                            .clone(),
                    );
                }
            } else if let Some(compiler_options_react_namespace) =
                self.compiler_options.react_namespace.as_ref()
            {
                *_jsx_namespace =
                    Some(escape_leading_underscores(compiler_options_react_namespace));
            }
        }
        let _jsx_namespace = _jsx_namespace.clone().unwrap();
        let mut _jsx_factory_entity = self._jsx_factory_entity.borrow_mut();
        if _jsx_factory_entity.is_none() {
            *_jsx_factory_entity = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Some(
                        factory_
                            .create_qualified_name(
                                synthetic_factory_,
                                factory_
                                    .create_identifier(
                                        synthetic_factory_,
                                        &unescape_leading_underscores(&_jsx_namespace),
                                        Option::<NodeArray>::None,
                                        None,
                                    )
                                    .into(),
                                "create_element".to_owned(),
                            )
                            .into(),
                    )
                })
            });
        }
        _jsx_namespace
    }

    pub(super) fn get_local_jsx_namespace(
        &self,
        file: &Node, /*SourceFile*/
    ) -> Option<__String> {
        let file_as_source_file = file.as_source_file();
        if let Some(file_local_jsx_namespace) =
            file_as_source_file.maybe_local_jsx_namespace().as_ref()
        {
            return Some(file_local_jsx_namespace.clone());
        }
        let file_pragmas = file_as_source_file.pragmas();
        let jsx_pragma = file_pragmas.get("jsx");
        if let Some(jsx_pragma) = jsx_pragma {
            let chosen_pragma = &jsx_pragma[0];
            let mut file_local_jsx_factory = file_as_source_file.maybe_local_jsx_factory();
            *file_local_jsx_factory = parse_isolated_entity_name(
                chosen_pragma
                    .arguments
                    .as_pragma_argument_type_factory()
                    .factory
                    .clone(),
                self.language_version,
            );
            visit_node(
                file_local_jsx_factory.as_deref(),
                Some(|node: &Node| self.mark_as_synthetic(node)),
                Option::<fn(&Node) -> bool>::None,
                Option::<fn(&[Rc<Node>]) -> Rc<Node>>::None,
            );
            if let Some(file_local_jsx_factory) = file_local_jsx_factory.as_ref() {
                let ret = get_first_identifier(file_local_jsx_factory)
                    .as_identifier()
                    .escaped_text
                    .clone();
                *file_as_source_file.maybe_local_jsx_namespace() = Some(ret.clone());
                return Some(ret);
            }
        }
        None
    }

    pub(super) fn mark_as_synthetic(&self, node: &Node) -> VisitResult {
        set_text_range_pos_end(node, -1, -1);
        visit_each_child(
            Some(node),
            |node: &Node| self.mark_as_synthetic(node),
            &*null_transformation_context,
            Option::<
                fn(
                    Option<&NodeArray>,
                    Option<fn(&Node) -> VisitResult>,
                    Option<fn(&Node) -> bool>,
                    Option<usize>,
                    Option<usize>,
                ) -> NodeArray,
            >::None,
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<fn(&Node) -> VisitResult>,
                    Option<fn(&Node) -> bool>,
                    Option<fn(&[Rc<Node>]) -> Rc<Node>>,
                ) -> Option<Rc<Node>>,
            >::None,
        )
        .map(|rc_node| vec![rc_node])
    }

    pub(super) fn get_emit_resolver(
        &self,
        source_file: &Node, /*SourceFile*/
        cancellation_token: Rc<dyn CancellationTokenDebuggable>,
    ) -> Rc<dyn EmitResolverDebuggable> {
        self.get_diagnostics(source_file, Some(cancellation_token));
        self.emit_resolver()
    }

    pub(super) fn lookup_or_issue_error<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Diagnostic> {
        let diagnostic: Rc<Diagnostic> = Rc::new(if let Some(location) = location {
            let location = location.borrow();
            create_diagnostic_for_node(location, message, args).into()
        } else {
            create_compiler_diagnostic(message, args).into()
        });
        let mut diagnostics = self.diagnostics();
        let existing = diagnostics.lookup(diagnostic.clone());
        existing.unwrap_or_else(|| {
            diagnostics.add(diagnostic.clone());
            diagnostic
        })
    }

    pub(super) fn error_skipped_on<TLocation: Borrow<Node>>(
        &self,
        key: String, /*keyof CompilerOptions*/
        location: Option<TLocation>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.error(location, message, args);
        *diagnostic.maybe_skipped_on() = Some(key);
        diagnostic
    }

    pub(super) fn create_error<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Diagnostic> {
        Rc::new(if let Some(location) = location {
            let location = location.borrow();
            create_diagnostic_for_node(location, message, args).into()
        } else {
            create_compiler_diagnostic(message, args).into()
        })
    }

    pub(super) fn error<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.create_error(location, message, args);
        self.diagnostics().add(diagnostic.clone());
        diagnostic
    }

    pub(super) fn add_error_or_suggestion(&self, is_error: bool, diagnostic: Rc<Diagnostic>) {
        if is_error {
            self.diagnostics().add(diagnostic);
        } else {
            diagnostic.set_category(DiagnosticCategory::Suggestion); // TODO: (also commented on this elsewhere) is it ok that this mutates the diagnostic?
            self.suggestion_diagnostics().add(diagnostic);
        }
    }

    pub(super) fn error_or_suggestion(
        &self,
        is_error: bool,
        location: &Node,
        message: DiagnosticMessageOrDiagnosticMessageChain,
        args: Option<Vec<String>>,
    ) {
        if location.pos() < 0 || location.end() < 0 {
            if !is_error {
                return;
            }
            let file = get_source_file_of_node(Some(location)).unwrap();
            self.add_error_or_suggestion(
                is_error,
                Rc::new(match message {
                    DiagnosticMessageOrDiagnosticMessageChain::DiagnosticMessage(message) => {
                        create_file_diagnostic(&file, 0, 0, &message, args).into()
                    }
                    DiagnosticMessageOrDiagnosticMessageChain::DiagnosticMessageChain(message) => {
                        create_diagnostic_for_file_from_message_chain(&file, message, None).into()
                    }
                }),
            );
            return;
        }
        self.add_error_or_suggestion(
            is_error,
            Rc::new(match message {
                DiagnosticMessageOrDiagnosticMessageChain::DiagnosticMessage(message) => {
                    create_diagnostic_for_node(location, &message, args).into()
                }
                DiagnosticMessageOrDiagnosticMessageChain::DiagnosticMessageChain(message) => {
                    create_diagnostic_for_node_from_message_chain(location, message, None).into()
                }
            }),
        );
    }

    pub(super) fn error_and_maybe_suggest_await(
        &self,
        location: &Node,
        maybe_missing_await: bool,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.error(Some(location), message, args);
        if maybe_missing_await {
            let related: Rc<DiagnosticRelatedInformation> = Rc::new(
                create_diagnostic_for_node(
                    location,
                    &Diagnostics::Did_you_forget_to_use_await,
                    None,
                )
                .into(),
            );
            add_related_info(&diagnostic, vec![related]);
        }
        diagnostic
    }

    pub(super) fn add_deprecated_suggestion_worker(
        &self,
        declarations: &[Rc<Node>],
        diagnostic: Rc<Diagnostic /*DiagnosticWithLocation*/>,
    ) -> Rc<Diagnostic> {
        let deprecated_tag = for_each(declarations, |declaration, _| {
            get_jsdoc_deprecated_tag(declaration)
        });
        if let Some(deprecated_tag) = deprecated_tag {
            add_related_info(
                &diagnostic,
                vec![Rc::new(
                    create_diagnostic_for_node(
                        &deprecated_tag,
                        &Diagnostics::The_declaration_was_marked_as_deprecated_here,
                        None,
                    )
                    .into(),
                )],
            );
        }
        self.suggestion_diagnostics().add(diagnostic.clone());
        diagnostic
    }

    pub(super) fn add_deprecated_suggestion(
        &self,
        location: &Node,
        declarations: &[Rc<Node>],
        deprecated_entity: &str,
    ) -> Rc<Diagnostic> {
        let diagnostic: Rc<Diagnostic> = Rc::new(
            create_diagnostic_for_node(
                location,
                &Diagnostics::_0_is_deprecated,
                Some(vec![deprecated_entity.to_owned()]),
            )
            .into(),
        );
        self.add_deprecated_suggestion_worker(declarations, diagnostic)
    }

    pub(super) fn add_deprecated_suggestion_with_signature(
        &self,
        location: &Node,
        declaration: &Node,
        deprecated_entity: Option<&str>,
        signature_string: &str,
    ) -> Rc<Diagnostic> {
        let diagnostic: Rc<Diagnostic> =
            Rc::new(if let Some(deprecated_entity) = deprecated_entity {
                create_diagnostic_for_node(
                    location,
                    &Diagnostics::The_signature_0_of_1_is_deprecated,
                    Some(vec![
                        signature_string.to_owned(),
                        deprecated_entity.to_owned(),
                    ]),
                )
                .into()
            } else {
                create_diagnostic_for_node(
                    location,
                    &Diagnostics::_0_is_deprecated,
                    Some(vec![signature_string.to_owned()]),
                )
                .into()
            });
        self.add_deprecated_suggestion_worker(&vec![declaration.node_wrapper()], diagnostic)
    }

    pub(super) fn create_symbol(
        &self,
        flags: SymbolFlags,
        name: __String,
        check_flags: Option<CheckFlags>,
    ) -> TransientSymbol {
        self.increment_symbol_count();
        let symbol = (self.Symbol)(flags | SymbolFlags::Transient, name);
        let symbol = BaseTransientSymbol::new(symbol, check_flags.unwrap_or(CheckFlags::None));
        symbol.into()
    }

    pub(super) fn get_excluded_symbol_flags(&self, flags: SymbolFlags) -> SymbolFlags {
        let mut result = SymbolFlags::None;
        if flags.intersects(SymbolFlags::BlockScopedVariable) {
            result |= SymbolFlags::BlockScopedVariableExcludes;
        }
        if flags.intersects(SymbolFlags::FunctionScopedVariable) {
            result |= SymbolFlags::FunctionScopedVariableExcludes;
        }
        if flags.intersects(SymbolFlags::Property) {
            result |= SymbolFlags::PropertyExcludes;
        }
        if flags.intersects(SymbolFlags::EnumMember) {
            result |= SymbolFlags::EnumMemberExcludes;
        }
        if flags.intersects(SymbolFlags::Function) {
            result |= SymbolFlags::FunctionExcludes;
        }
        if flags.intersects(SymbolFlags::Class) {
            result |= SymbolFlags::ClassExcludes;
        }
        if flags.intersects(SymbolFlags::Interface) {
            result |= SymbolFlags::InterfaceExcludes;
        }
        if flags.intersects(SymbolFlags::RegularEnum) {
            result |= SymbolFlags::RegularEnumExcludes;
        }
        if flags.intersects(SymbolFlags::ConstEnum) {
            result |= SymbolFlags::ConstEnumExcludes;
        }
        if flags.intersects(SymbolFlags::ValueModule) {
            result |= SymbolFlags::ValueModuleExcludes;
        }
        if flags.intersects(SymbolFlags::Method) {
            result |= SymbolFlags::MethodExcludes;
        }
        if flags.intersects(SymbolFlags::GetAccessor) {
            result |= SymbolFlags::GetAccessorExcludes;
        }
        if flags.intersects(SymbolFlags::SetAccessor) {
            result |= SymbolFlags::SetAccessorExcludes;
        }
        if flags.intersects(SymbolFlags::TypeParameter) {
            result |= SymbolFlags::TypeParameterExcludes;
        }
        if flags.intersects(SymbolFlags::TypeAlias) {
            result |= SymbolFlags::TypeAliasExcludes;
        }
        if flags.intersects(SymbolFlags::Alias) {
            result |= SymbolFlags::AliasExcludes;
        }
        result
    }

    pub(super) fn record_merged_symbol(&self, target: &Symbol, source: &Symbol) {
        if source.maybe_merge_id().is_none() {
            source.set_merge_id(get_next_merge_id());
            increment_next_merge_id();
        }
        self.merged_symbols()
            .insert(source.maybe_merge_id().unwrap(), target.symbol_wrapper());
    }

    pub(super) fn clone_symbol(&self, symbol: &Symbol) -> Symbol {
        let result: Symbol = self
            .create_symbol(symbol.flags(), symbol.escaped_name().clone(), None)
            .into();
        result.set_declarations(
            if let Some(symbol_declarations) = symbol.maybe_declarations().as_ref() {
                symbol_declarations.clone()
            } else {
                vec![]
            },
        );
        result.set_parent(symbol.maybe_parent().clone());
        if let Some(symbol_value_declaration) = symbol.maybe_value_declaration().as_ref() {
            result.set_value_declaration(symbol_value_declaration.clone());
        }
        if matches!(symbol.maybe_const_enum_only_module(), Some(true)) {
            result.set_const_enum_only_module(Some(true));
        }
        if let Some(symbol_members) = symbol.maybe_members().as_ref() {
            *result.maybe_members() = Some(Rc::new(RefCell::new(
                RefCell::borrow(symbol_members).clone(),
            )));
        }
        if let Some(symbol_exports) = symbol.maybe_exports().as_ref() {
            *result.maybe_exports() = Some(Rc::new(RefCell::new(
                RefCell::borrow(symbol_exports).clone(),
            )));
        }
        self.record_merged_symbol(&result, symbol);
        result
    }

    pub(super) fn merge_symbol(
        &self,
        target: &Symbol,
        source: &Symbol,
        unidirectional: Option<bool>,
    ) -> Rc<Symbol> {
        let unidirectional = unidirectional.unwrap_or(false);
        let mut target = target.symbol_wrapper();
        if !target
            .flags()
            .intersects(self.get_excluded_symbol_flags(source.flags()))
            || (source.flags() | target.flags()).intersects(SymbolFlags::Assignment)
        {
            if ptr::eq(source, &*target) {
                return target;
            }
            if !target.flags().intersects(SymbolFlags::Transient) {
                let resolved_target = self.resolve_symbol(Some(&*target), None).unwrap();
                if Rc::ptr_eq(&resolved_target, &self.unknown_symbol()) {
                    return source.symbol_wrapper();
                }
                target = Rc::new(self.clone_symbol(&resolved_target));
            }
            if source.flags().intersects(SymbolFlags::ValueModule)
                && target.flags().intersects(SymbolFlags::ValueModule)
                && matches!(target.maybe_const_enum_only_module(), Some(true))
                && !matches!(source.maybe_const_enum_only_module(), Some(true))
            {
                target.set_const_enum_only_module(Some(true));
            }
            target.set_flags(target.flags() | source.flags());
            if let Some(source_value_declaration) = source.maybe_value_declaration().as_ref() {
                set_value_declaration(&target, source_value_declaration);
            }
            if let Some(source_declarations) = source.maybe_declarations().as_deref() {
                let mut target_declarations = target.maybe_declarations_mut();
                if target_declarations.is_none() {
                    *target_declarations = Some(vec![]);
                }
                add_range(
                    target_declarations.as_mut().unwrap(),
                    Some(source_declarations),
                    None,
                    None,
                );
            }
            if let Some(source_members) = source.maybe_members().as_ref() {
                let mut target_members = target.maybe_members();
                if target_members.is_none() {
                    *target_members = Some(Rc::new(RefCell::new(create_symbol_table(None))));
                }
                self.merge_symbol_table(
                    &mut *target_members.as_ref().unwrap().borrow_mut(),
                    &RefCell::borrow(source_members),
                    Some(unidirectional),
                );
            }
            if let Some(source_exports) = source.maybe_exports().as_ref() {
                let mut target_exports = target.maybe_exports();
                if target_exports.is_none() {
                    *target_exports = Some(Rc::new(RefCell::new(create_symbol_table(None))));
                }
                self.merge_symbol_table(
                    &mut *target_exports.as_ref().unwrap().borrow_mut(),
                    &RefCell::borrow(source_exports),
                    Some(unidirectional),
                );
            }
            if !unidirectional {
                self.record_merged_symbol(&target, source);
            }
        } else if target.flags().intersects(SymbolFlags::NamespaceModule) {
            if !Rc::ptr_eq(&target, &self.global_this_symbol()) {
                self.error(
                    source.maybe_declarations().as_ref().and_then(|source_declarations| get_name_of_declaration(source_declarations.get(0).map(Clone::clone))),
                    &Diagnostics::Cannot_augment_module_0_with_value_exports_because_it_resolves_to_a_non_module_entity,
                    Some(vec![self.symbol_to_string_(&target, Option::<&Node>::None, None, None, None)])
                );
            }
        } else {
            let is_either_enum = target.flags().intersects(SymbolFlags::Enum)
                || source.flags().intersects(SymbolFlags::Enum);
            let is_either_block_scoped =
                target.flags().intersects(SymbolFlags::BlockScopedVariable)
                    || source.flags().intersects(SymbolFlags::BlockScopedVariable);
            let message = if is_either_enum {
                &Diagnostics::Enum_declarations_can_only_merge_with_namespace_or_other_enum_declarations
            } else if is_either_block_scoped {
                &Diagnostics::Cannot_redeclare_block_scoped_variable_0
            } else {
                &Diagnostics::Duplicate_identifier_0
            };
            let source_symbol_file =
                source
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|source_declarations| {
                        get_source_file_of_node(source_declarations.get(0).map(Clone::clone))
                    });
            let target_symbol_file =
                target
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|target_declarations| {
                        get_source_file_of_node(target_declarations.get(0).map(Clone::clone))
                    });
            let symbol_name =
                self.symbol_to_string_(source, Option::<&Node>::None, None, None, None);

            if source_symbol_file.is_some()
                && target_symbol_file.is_some()
                && self.maybe_amalgamated_duplicates().is_some()
                && !is_either_enum
                && !Rc::ptr_eq(
                    source_symbol_file.as_ref().unwrap(),
                    target_symbol_file.as_ref().unwrap(),
                )
            {
                let source_symbol_file = source_symbol_file.unwrap();
                let target_symbol_file = target_symbol_file.unwrap();
                let first_file = if compare_paths(
                    &source_symbol_file.as_source_file().path(),
                    &target_symbol_file.as_source_file().path(),
                    Option::<String>::None,
                    None,
                ) == Comparison::LessThan
                {
                    source_symbol_file.clone()
                } else {
                    target_symbol_file.clone()
                };
                let second_file = if Rc::ptr_eq(&first_file, &source_symbol_file) {
                    target_symbol_file.clone()
                } else {
                    source_symbol_file.clone()
                };
                let mut amalgamated_duplicates = self.maybe_amalgamated_duplicates();
                let amalgamated_duplicates = amalgamated_duplicates.as_mut().unwrap();
                let files_duplicates = get_or_update(
                    amalgamated_duplicates,
                    format!(
                        "{}|{}",
                        &**first_file.as_source_file().path(),
                        &**second_file.as_source_file().path()
                    ),
                    || DuplicateInfoForFiles {
                        first_file,
                        second_file,
                        conflicting_symbols: HashMap::new(),
                    },
                );
                let conflicting_symbol_info = get_or_update(
                    &mut files_duplicates.conflicting_symbols,
                    symbol_name,
                    || DuplicateInfoForSymbol {
                        first_file_locations: vec![],
                        second_file_locations: vec![],
                        is_block_scoped: is_either_block_scoped,
                    },
                );
                self.add_duplicate_locations(
                    &mut conflicting_symbol_info.first_file_locations,
                    source,
                );
                self.add_duplicate_locations(
                    &mut conflicting_symbol_info.second_file_locations,
                    &target,
                );
            } else {
                self.add_duplicate_declaration_errors_for_symbols(
                    source,
                    message,
                    &symbol_name,
                    &target,
                );
                self.add_duplicate_declaration_errors_for_symbols(
                    &target,
                    message,
                    &symbol_name,
                    source,
                );
            }
        }
        target
    }

    pub(super) fn add_duplicate_locations(
        &self,
        locs: &mut Vec<Rc<Node /*Declaration*/>>,
        symbol: &Symbol,
    ) {
        if let Some(symbol_declarations) = symbol.maybe_declarations().as_ref() {
            for decl in symbol_declarations {
                push_if_unique_rc(locs, decl.clone());
            }
        }
    }

    pub(super) fn add_duplicate_declaration_errors_for_symbols(
        &self,
        target: &Symbol,
        message: &DiagnosticMessage,
        symbol_name: &str,
        source: &Symbol,
    ) {
        maybe_for_each(target.maybe_declarations().as_deref(), |node, _| {
            self.add_duplicate_declaration_error(
                node,
                message,
                symbol_name,
                source.maybe_declarations().as_deref(),
            );
            Option::<()>::None
        });
    }

    pub(super) fn add_duplicate_declaration_error(
        &self,
        node: &Node, /*Declaration*/
        message: &DiagnosticMessage,
        symbol_name: &str,
        related_nodes: Option<&[Rc<Node /*Declaration*/>]>,
    ) {
        let error_node = (if get_expando_initializer(node, false).is_some() {
            get_name_of_expando(node)
        } else {
            get_name_of_declaration(Some(node))
        })
        .unwrap_or_else(|| node.node_wrapper());
        let err = self.lookup_or_issue_error(
            Some(&*error_node),
            message,
            Some(vec![symbol_name.to_owned()]),
        );
        if let Some(related_nodes) = related_nodes {
            for related_node in related_nodes {
                let adjusted_node = (if get_expando_initializer(related_node, false).is_some() {
                    get_name_of_expando(related_node)
                } else {
                    get_name_of_declaration(Some(&**related_node))
                })
                .unwrap_or_else(|| related_node.node_wrapper());
                if Rc::ptr_eq(&adjusted_node, &error_node) {
                    continue;
                }
                {
                    let mut err_related_information = err.related_information();
                    if err_related_information.is_none() {
                        *err_related_information = Some(vec![]);
                    }
                }
                let leading_message: Rc<DiagnosticRelatedInformation> = Rc::new(
                    create_diagnostic_for_node(
                        &adjusted_node,
                        &Diagnostics::_0_was_also_declared_here,
                        Some(vec![symbol_name.to_owned()]),
                    )
                    .into(),
                );
                let follow_on_message: Rc<DiagnosticRelatedInformation> = Rc::new(
                    create_diagnostic_for_node(&adjusted_node, &Diagnostics::and_here, None).into(),
                );
                if length(err.related_information().as_deref()) >= 5
                    || some(
                        err.related_information().as_deref(),
                        Some(|r: &Rc<DiagnosticRelatedInformation>| {
                            compare_diagnostics(&**r, &*follow_on_message) == Comparison::EqualTo
                                || compare_diagnostics(&**r, &*leading_message)
                                    == Comparison::EqualTo
                        }),
                    )
                {
                    continue;
                }
                add_related_info(
                    &err,
                    vec![if length(err.related_information().as_deref()) == 0 {
                        leading_message
                    } else {
                        follow_on_message
                    }],
                );
            }
        }
    }

    pub(super) fn combine_symbol_tables(
        &self,
        first: Option<Rc<RefCell<SymbolTable>>>,
        second: Option<Rc<RefCell<SymbolTable>>>,
    ) -> Option<Rc<RefCell<SymbolTable>>> {
        if first.is_none() {
            return second;
        }
        if second.is_none() {
            return first;
        }
        let first = first.unwrap();
        let second = second.unwrap();
        if RefCell::borrow(&first).is_empty() {
            return Some(second);
        }
        if RefCell::borrow(&second).is_empty() {
            return Some(first);
        }
        let mut combined = create_symbol_table(None);
        self.merge_symbol_table(&mut combined, &RefCell::borrow(&first), None);
        self.merge_symbol_table(&mut combined, &RefCell::borrow(&second), None);
        Some(Rc::new(RefCell::new(combined)))
    }

    pub(super) fn merge_symbol_table(
        &self,
        target: &mut SymbolTable,
        source: &SymbolTable,
        unidirectional: Option<bool>,
    ) {
        let unidirectional = unidirectional.unwrap_or(false);
        for (id, source_symbol) in source {
            let target_symbol = target.get(id);
            let value = if let Some(target_symbol) = target_symbol {
                self.merge_symbol(target_symbol, source_symbol, Some(unidirectional))
            } else {
                source_symbol.clone()
            };
            target.insert(id.clone(), value);
        }
    }

    pub(super) fn merge_module_augmentation(
        &self,
        module_name: &Node, /*StringLiteral | Identifier*/
    ) {
        let module_augmentation = module_name.parent();
        if !matches!(module_augmentation.symbol().maybe_declarations().as_ref().and_then(|declarations| declarations.get(0)), Some(declaration) if Rc::ptr_eq(declaration, &module_augmentation))
        {
            Debug_.assert(matches!(module_augmentation.symbol().maybe_declarations().as_ref(), Some(declarations) if declarations.len() > 1), None);
            return;
        }

        if is_global_scope_augmentation(&module_augmentation) {
            self.merge_symbol_table(
                &mut self.globals(),
                &mut module_augmentation.symbol().exports().borrow_mut(),
                None,
            );
        } else {
            let module_not_found_error = if !module_name
                .parent()
                .parent()
                .flags()
                .intersects(NodeFlags::Ambient)
            {
                Some(&Diagnostics::Invalid_module_name_in_augmentation_module_0_cannot_be_found)
            } else {
                None
            };
            let main_module = self.resolve_external_module_name_worker(
                &module_name,
                &module_name,
                module_not_found_error,
                Some(true),
            );
            if main_module.is_none() {
                return;
            }
            let main_module = main_module.unwrap();
            let main_module = self
                .resolve_external_module_symbol(Some(&*main_module), None)
                .unwrap();
            if main_module.flags().intersects(SymbolFlags::Namespace) {
                if some(
                    self.maybe_pattern_ambient_modules().as_deref(),
                    Some(|module: &PatternAmbientModule| Rc::ptr_eq(&main_module, &module.symbol)),
                ) {
                    let merged =
                        self.merge_symbol(&module_augmentation.symbol(), &main_module, Some(true));
                    let mut pattern_ambient_module_augmentations =
                        self.maybe_pattern_ambient_module_augmentations();
                    if pattern_ambient_module_augmentations.is_none() {
                        *pattern_ambient_module_augmentations = Some(HashMap::new());
                    }
                    pattern_ambient_module_augmentations
                        .as_mut()
                        .unwrap()
                        .insert(module_name.as_literal_like_node().text().clone(), merged);
                } else {
                    if main_module
                        .maybe_exports()
                        .as_ref()
                        .and_then(|exports| {
                            RefCell::borrow(exports)
                                .get(&InternalSymbolName::ExportStar())
                                .map(Clone::clone)
                        })
                        .is_some()
                        && matches!(module_augmentation.symbol().maybe_exports().as_ref(), Some(exports) if !RefCell::borrow(exports).is_empty())
                    {
                        let resolved_exports = self.get_resolved_members_or_exports_of_symbol(
                            &main_module,
                            MembersOrExportsResolutionKind::resolved_exports,
                        );
                        let main_module_exports = main_module.exports();
                        let main_module_exports = RefCell::borrow(&main_module_exports);
                        for (key, value) in
                            &*RefCell::borrow(&module_augmentation.symbol().exports())
                        {
                            if resolved_exports.contains_key(key)
                                && !main_module_exports.contains_key(key)
                            {
                                self.merge_symbol(resolved_exports.get(key).unwrap(), value, None);
                            }
                        }
                    }
                    self.merge_symbol(&main_module, &module_augmentation.symbol(), None);
                }
            } else {
                self.error(
                    Some(module_name),
                    &Diagnostics::Cannot_augment_module_0_because_it_resolves_to_a_non_module_entity,
                    Some(vec![module_name.as_literal_like_node().text().clone()])
                );
            }
        }
    }

    pub(super) fn add_to_symbol_table(
        &self,
        target: &mut SymbolTable,
        source: &SymbolTable,
        message: &DiagnosticMessage,
    ) {
        for (id, source_symbol) in source {
            let target_symbol = target.get(id);
            if let Some(target_symbol) = target_symbol {
                maybe_for_each(
                    target_symbol.maybe_declarations().as_deref(),
                    |declaration: &Rc<Node /*Declaration*/>, _| {
                        self.diagnostics().add(Rc::new(
                            create_diagnostic_for_node(
                                declaration,
                                message,
                                Some(vec![unescape_leading_underscores(id)]),
                            )
                            .into(),
                        ));
                        Option::<()>::None
                    },
                );
            } else {
                target.insert(id.clone(), source_symbol.clone());
            }
        }
    }

    pub(super) fn get_symbol_links(&self, symbol: &Symbol) -> Rc<RefCell<SymbolLinks>> {
        if let Symbol::TransientSymbol(symbol) = symbol {
            return symbol.symbol_links();
        }
        let id = get_symbol_id(symbol);
        let mut symbol_links_table = self.symbol_links.borrow_mut();
        if let Some(symbol_links) = symbol_links_table.get(&id) {
            return symbol_links.clone();
        }
        let symbol_links = Rc::new(RefCell::new(SymbolLinks::new()));
        symbol_links_table.insert(id, symbol_links.clone());
        symbol_links
    }

    pub(super) fn get_node_links(&self, node: &Node) -> Rc<RefCell<NodeLinks>> {
        let id = get_node_id(node);
        let mut node_links_table = self.node_links.borrow_mut();
        if let Some(node_links) = node_links_table.get(&id) {
            return node_links.clone();
        }
        let node_links = Rc::new(RefCell::new(NodeLinks::new()));
        node_links_table.insert(id, node_links.clone());
        node_links
    }

    pub(super) fn is_global_source_file(&self, node: &Node) -> bool {
        node.kind() == SyntaxKind::SourceFile && !is_external_or_common_js_module(node)
    }

    pub(super) fn get_symbol(
        &self,
        symbols: &SymbolTable,
        name: &__String,
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        if meaning != SymbolFlags::None {
            let symbol = self.get_merged_symbol(symbols.get(name).map(Clone::clone));
            if let Some(symbol) = symbol {
                Debug_.assert(
                    !get_check_flags(&symbol).intersects(CheckFlags::Instantiated),
                    Some("Should never get an instantiated symbol here."),
                );
                if symbol.flags().intersects(meaning) {
                    return Some(symbol);
                }
                if symbol.flags().intersects(SymbolFlags::Alias) {
                    let target = self.resolve_alias(&symbol);
                    if Rc::ptr_eq(&target, &self.unknown_symbol())
                        || target.flags().intersects(meaning)
                    {
                        return Some(symbol);
                    }
                }
            }
        }
        None
    }

    pub(super) fn get_symbols_of_parameter_property_declaration_(
        &self,
        parameter: &Node, /*ParameterDeclaration*/
        parameter_name: &__String,
    ) -> Vec<Rc<Symbol>> {
        let constructor_declaration = parameter.parent();
        let class_declaration = parameter.parent().parent();

        let parameter_symbol = self.get_symbol(
            &constructor_declaration.locals(),
            parameter_name,
            SymbolFlags::Value,
        );
        let property_symbol = self.get_symbol(
            &RefCell::borrow(&self.get_members_of_symbol(&class_declaration.symbol())),
            parameter_name,
            SymbolFlags::Value,
        );

        match (parameter_symbol, property_symbol) {
            (Some(parameter_symbol), Some(property_symbol)) => vec![parameter_symbol, property_symbol],
            _ => Debug_.fail(Some("There should exist two symbols, one as property declaration and one as parameter declaration")),
        }
    }

    pub(super) fn is_block_scoped_name_declared_before_use(
        &self,
        declaration: &Node, /*Declaration*/
        usage: &Node,
    ) -> bool {
        let declaration_file = get_source_file_of_node(Some(declaration)).unwrap();
        let use_file = get_source_file_of_node(Some(usage)).unwrap();
        let decl_container = get_enclosing_block_scope_container(declaration).unwrap();
        if !Rc::ptr_eq(&declaration_file, &use_file) {
            if self.module_kind != ModuleKind::None
                && (declaration_file
                    .as_source_file()
                    .maybe_external_module_indicator()
                    .is_some()
                    || use_file
                        .as_source_file()
                        .maybe_external_module_indicator()
                        .is_some())
                || !matches!(out_file(&self.compiler_options), Some(out_file) if !out_file.is_empty())
                || self.is_in_type_query(usage)
                || declaration.flags().intersects(NodeFlags::Ambient)
            {
                return true;
            }
            if self.is_used_in_function_or_instance_property(&decl_container, usage, declaration) {
                return true;
            }
            let source_files = self.host.get_source_files();
            return index_of_rc(source_files, &declaration_file)
                <= index_of_rc(source_files, &use_file);
        }

        if declaration.pos() <= usage.pos()
            && !(is_property_declaration(declaration) && is_this_property(&usage.parent()) && {
                let declaration_as_property_declaration = declaration.as_property_declaration();
                !declaration_as_property_declaration
                    .maybe_initializer()
                    .is_some()
                    && !declaration_as_property_declaration
                        .exclamation_token
                        .is_some()
            })
        {
            if declaration.kind() == SyntaxKind::BindingElement {
                let error_binding_element = get_ancestor(Some(usage), SyntaxKind::BindingElement);
                if let Some(error_binding_element) = error_binding_element {
                    return !are_option_rcs_equal(
                        find_ancestor(Some(&*error_binding_element), is_binding_element),
                        find_ancestor(Some(declaration), is_binding_element),
                    ) || declaration.pos() < error_binding_element.pos();
                }
                return self.is_block_scoped_name_declared_before_use(
                    &get_ancestor(Some(declaration), SyntaxKind::VariableDeclaration).unwrap(),
                    usage,
                );
            } else if declaration.kind() == SyntaxKind::VariableDeclaration {
                return !self.is_immediately_used_in_initializer_of_block_scoped_variable(
                    &decl_container,
                    declaration,
                    usage,
                );
            } else if is_class_declaration(declaration) {
                return find_ancestor(Some(usage), |n| {
                    is_computed_property_name(n) && ptr::eq(&*n.parent().parent(), declaration)
                })
                .is_none();
            } else if is_property_declaration(declaration) {
                return !self.is_property_immediately_referenced_within_declaration(
                    declaration,
                    usage,
                    false,
                );
            } else if is_parameter_property_declaration(declaration, &declaration.parent()) {
                return !(get_emit_script_target(&self.compiler_options) == ScriptTarget::ESNext
                    && self.use_define_for_class_fields
                    && are_option_rcs_equal(
                        get_containing_class(declaration),
                        get_containing_class(usage),
                    )
                    && self.is_used_in_function_or_instance_property(
                        &decl_container,
                        usage,
                        declaration,
                    ));
            }
            return true;
        }

        if usage.parent().kind() == SyntaxKind::ExportSpecifier
            || usage.parent().kind() == SyntaxKind::ExportAssignment
                && matches!(
                    usage.parent().as_export_assignment().is_export_equals,
                    Some(true)
                )
        {
            return true;
        }
        if usage.kind() == SyntaxKind::ExportAssignment
            && matches!(usage.as_export_assignment().is_export_equals, Some(true))
        {
            return true;
        }

        if usage.flags().intersects(NodeFlags::JSDoc)
            || self.is_in_type_query(usage)
            || self.usage_in_type_declaration(usage)
        {
            return true;
        }
        if self.is_used_in_function_or_instance_property(&decl_container, usage, declaration) {
            if get_emit_script_target(&self.compiler_options) == ScriptTarget::ESNext
                && self.use_define_for_class_fields
                && get_containing_class(declaration).is_some()
                && (is_property_declaration(declaration)
                    || is_parameter_property_declaration(declaration, &declaration.parent()))
            {
                return !self.is_property_immediately_referenced_within_declaration(
                    declaration,
                    usage,
                    true,
                );
            } else {
                return true;
            }
        }
        false
    }

    pub(super) fn usage_in_type_declaration(&self, usage: &Node) -> bool {
        find_ancestor(Some(usage), |node| {
            is_interface_declaration(node) || is_type_alias_declaration(node)
        })
        .is_some()
    }

    pub(super) fn is_immediately_used_in_initializer_of_block_scoped_variable(
        &self,
        decl_container: &Node,
        declaration: &Node, /*VariableDeclaration*/
        usage: &Node,
    ) -> bool {
        match declaration.parent().parent().kind() {
            SyntaxKind::VariableStatement
            | SyntaxKind::ForStatement
            | SyntaxKind::ForOfStatement => {
                if self.is_same_scope_descendent_of(usage, Some(declaration), decl_container) {
                    return true;
                }
            }
            _ => (),
        }

        let grandparent = declaration.parent().parent();
        is_for_in_or_of_statement(&grandparent)
            && self.is_same_scope_descendent_of(
                usage,
                Some(grandparent.as_has_expression().expression()),
                decl_container,
            )
    }

    pub(super) fn is_used_in_function_or_instance_property(
        &self,
        decl_container: &Node,
        usage: &Node,
        declaration: &Node,
    ) -> bool {
        find_ancestor(Some(usage), |current| {
            if ptr::eq(current, decl_container) {
                return FindAncestorCallbackReturn::Quit;
            }
            if is_function_like(Some(current)) {
                return true.into();
            }
            if is_class_static_block_declaration(current) {
                return (declaration.pos() < usage.pos()).into();
            }

            let property_declaration = current.maybe_parent().and_then(|parent| try_cast(parent, |node: &Rc<Node>| is_property_declaration(node)));
            if let Some(property_declaration) = property_declaration {
                let initializer_of_property = matches!(property_declaration.as_property_declaration().maybe_initializer(), Some(initializer) if ptr::eq(&*initializer, current));
                if initializer_of_property {
                    if is_static(&current.parent()) {
                        if declaration.kind() == SyntaxKind::MethodDeclaration {
                            return true.into();
                        }
                        if is_property_declaration(declaration) && are_option_rcs_equal(get_containing_class(usage), get_containing_class(declaration)) {
                            let prop_name = declaration.as_property_declaration().name();
                            if is_identifier(&prop_name) || is_private_identifier(&prop_name) {
                                let type_ = self.get_type_of_symbol(&self.get_symbol_of_node(declaration).unwrap());
                                let static_blocks = filter(Some(declaration.parent().as_class_like_declaration().members()), |node: &Rc<Node>| is_class_static_block_declaration(node)).unwrap();
                                if self.is_property_initialized_in_static_blocks(&prop_name, &type_, &static_blocks, declaration.parent().pos(), current.pos()) {
                                    return true.into();
                                }
                            }
                        }
                    } else {
                        let is_declaration_instance_property = declaration.kind() == SyntaxKind::PropertyDeclaration && !is_static(declaration);
                        if !is_declaration_instance_property || !are_option_rcs_equal(get_containing_class(usage), get_containing_class(declaration)) {
                            return true.into();
                        }
                    }
                }
            }
            false.into()
        })
        .is_some()
    }

    pub(super) fn is_property_immediately_referenced_within_declaration(
        &self,
        declaration: &Node, /*PropertyDeclaration | ParameterPropertyDeclaration*/
        usage: &Node,
        stop_at_any_property_declaration: bool,
    ) -> bool {
        if usage.end() > declaration.end() {
            return false;
        }

        let ancestor_changing_reference_scope = find_ancestor(Some(usage), |node| {
            if ptr::eq(node, declaration) {
                return FindAncestorCallbackReturn::Quit;
            }

            match node.kind() {
                SyntaxKind::ArrowFunction => true.into(),
                SyntaxKind::PropertyDeclaration => {
                    if stop_at_any_property_declaration
                        && (is_property_declaration(declaration)
                            && Rc::ptr_eq(&node.parent(), &declaration.parent())
                            || is_parameter_property_declaration(
                                declaration,
                                &declaration.parent(),
                            ) && Rc::ptr_eq(&node.parent(), &declaration.parent().parent()))
                    {
                        FindAncestorCallbackReturn::Quit
                    } else {
                        true.into()
                    }
                }
                SyntaxKind::Block => matches!(
                    node.parent().kind(),
                    SyntaxKind::GetAccessor
                        | SyntaxKind::MethodDeclaration
                        | SyntaxKind::SetAccessor
                )
                .into(),
                _ => false.into(),
            }
        });

        ancestor_changing_reference_scope.is_none()
    }

    pub(super) fn use_outer_variable_scope_in_parameter(
        &self,
        result: &Symbol,
        location: &Node,
        last_location: &Node,
    ) -> bool {
        let target = get_emit_script_target(&self.compiler_options);
        let function_location = location.as_function_like_declaration();
        if is_parameter(last_location)
            && match (
                function_location.maybe_body(),
                result.maybe_value_declaration().as_ref(),
            ) {
                (Some(function_location_body), Some(result_value_declaration)) => {
                    result_value_declaration.pos() >= function_location_body.pos()
                        && result_value_declaration.end() <= function_location_body.end()
                }
                _ => false,
            }
        {
            if target >= ScriptTarget::ES2015 {
                let links = self.get_node_links(location);
                let mut links = links.borrow_mut();
                if links.declaration_requires_scope_change.is_none() {
                    links.declaration_requires_scope_change = Some(
                        for_each_bool(function_location.parameters(), |node: &Rc<Node>, _| {
                            self.requires_scope_change(target, node)
                        }) || false,
                    );
                }
                return !links.declaration_requires_scope_change.unwrap();
            }
        }

        false
    }

    pub(super) fn requires_scope_change(
        &self,
        target: ScriptTarget,
        node: &Node, /*ParameterDeclaration*/
    ) -> bool {
        self.requires_scope_change_worker(target, node)
            || matches!(node.as_parameter_declaration().maybe_initializer(), Some(initializer) if self.requires_scope_change_worker(target, &initializer))
    }

    pub(super) fn requires_scope_change_worker(&self, target: ScriptTarget, node: &Node) -> bool {
        match node.kind() {
            SyntaxKind::ArrowFunction
            | SyntaxKind::FunctionExpression
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::Constructor => false,
            SyntaxKind::MethodDeclaration
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::PropertyAssignment => {
                self.requires_scope_change_worker(target, &node.as_named_declaration().name())
            }
            SyntaxKind::PropertyDeclaration => {
                if has_static_modifier(node) {
                    return target < ScriptTarget::ESNext || !self.use_define_for_class_fields;
                }
                self.requires_scope_change_worker(target, &node.as_property_declaration().name())
            }
            _ => {
                if is_nullish_coalesce(node) || is_optional_chain(node) {
                    return target < ScriptTarget::ES2020;
                }
                if is_binding_element(node)
                    && node.as_binding_element().dot_dot_dot_token.is_some()
                    && is_object_binding_pattern(&node.parent())
                {
                    return target < ScriptTarget::ES2017;
                }
                if is_type_node(node) {
                    return false;
                }
                for_each_child_bool(
                    node,
                    |child| self.requires_scope_change_worker(target, child),
                    Option::<fn(&NodeArray) -> bool>::None,
                ) || false
            }
        }
    }

    pub(super) fn resolve_name_<TLocation: Borrow<Node>, TNameArg: Into<ResolveNameNameArg>>(
        &self,
        location: Option<TLocation>,
        name: &__String,
        meaning: SymbolFlags,
        name_not_found_message: Option<DiagnosticMessage>,
        name_arg: Option<TNameArg>,
        is_use: bool,
        exclude_globals: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let exclude_globals = exclude_globals.unwrap_or(false);
        self.resolve_name_helper(
            location,
            name,
            meaning,
            name_not_found_message,
            name_arg,
            is_use,
            exclude_globals,
            TypeChecker::get_symbol,
        )
    }

    pub(super) fn resolve_name_helper<
        TLocation: Borrow<Node>,
        TNameArg: Into<ResolveNameNameArg>,
    >(
        &self,
        location: Option<TLocation>,
        name: &__String,
        meaning: SymbolFlags,
        name_not_found_message: Option<DiagnosticMessage>,
        name_arg: Option<TNameArg>,
        is_use: bool,
        exclude_globals: bool,
        lookup: fn(&TypeChecker, &SymbolTable, &__String, SymbolFlags) -> Option<Rc<Symbol>>,
    ) -> Option<Rc<Symbol>> {
        let mut location: Option<Rc<Node>> = location.map(|node| node.borrow().node_wrapper());
        let mut result: Option<Rc<Symbol>> = None;
        let mut last_location: Option<Rc<Node>> = None;
        let error_location = location.clone();

        while let Some(location_unwrapped) = location {
            let location_maybe_locals = location_unwrapped.maybe_locals();
            if let Some(location_locals) = &*location_maybe_locals {
                if !self.is_global_source_file(&*location_unwrapped) {
                    result = lookup(self, location_locals, name, meaning);
                    if let Some(result_unwrapped) = result.as_ref() {
                        let mut use_result = true;

                        if use_result {
                            break;
                        } else {
                            result = None;
                        }
                    }
                }
            }

            match location_unwrapped.kind() {
                SyntaxKind::InterfaceDeclaration => {
                    result = lookup(
                        self,
                        &*(*self
                            .get_symbol_of_node(&*location_unwrapped)
                            .unwrap()
                            .maybe_members()
                            .clone()
                            .unwrap_or_else(|| self.empty_symbols()))
                        .borrow(),
                        name,
                        meaning & SymbolFlags::Type,
                    );
                    if let Some(result) = &result {
                        break;
                    }
                }
                _ => (),
            }
            last_location = Some(location_unwrapped.clone());
            location = location_unwrapped.maybe_parent();
        }

        if result.is_none() {
            if let Some(last_location) = last_location {
                Debug_.assert(last_location.kind() == SyntaxKind::SourceFile, None);
            }

            if !exclude_globals {
                result = lookup(self, &self.globals(), name, meaning);
            }
        }

        if result.is_none() {
            if let Some(name_not_found_message) = name_not_found_message {
                if true {
                    let mut suggestion: Option<Rc<Symbol>> = None;
                    if let Some(name_arg) = name_arg {
                        let name_arg = name_arg.into();
                        if false {
                            unimplemented!()
                        } else {
                            self.error(
                                error_location,
                                &name_not_found_message,
                                Some(vec![self.diagnostic_name(name_arg).into_owned()]),
                            );
                        }
                    }
                }
            }
            return None;
        }

        result
    }
}

pub(super) enum DiagnosticMessageOrDiagnosticMessageChain {
    DiagnosticMessage(DiagnosticMessage),
    DiagnosticMessageChain(DiagnosticMessageChain),
}

pub(super) enum ResolveNameNameArg {
    Node(Rc<Node>),
    __String(__String),
}

impl From<Rc<Node>> for ResolveNameNameArg {
    fn from(node: Rc<Node>) -> Self {
        ResolveNameNameArg::Node(node)
    }
}

impl From<__String> for ResolveNameNameArg {
    fn from(string: __String) -> Self {
        ResolveNameNameArg::__String(string)
    }
}
