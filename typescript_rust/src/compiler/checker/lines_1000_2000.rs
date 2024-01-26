use std::{borrow::Borrow, collections::HashMap, io, ptr};

use gc::{Gc, GcCell};
use id_arena::Id;
use indexmap::IndexMap;

use super::{
    get_next_merge_id, get_node_id, get_symbol_id, increment_next_merge_id,
    MembersOrExportsResolutionKind,
};
use crate::{
    add_range, add_related_info, are_option_gcs_equal, compare_diagnostics, compare_paths,
    create_compiler_diagnostic, create_diagnostic_for_file_from_message_chain,
    create_diagnostic_for_node_from_message_chain, create_file_diagnostic, create_symbol_table,
    declaration_name_to_string, every, find_ancestor, for_each, for_each_bool, for_each_child_bool,
    get_ancestor, get_check_flags, get_containing_class, get_declaration_of_kind,
    get_effective_container_for_jsdoc_template_tag, get_emit_script_target,
    get_enclosing_block_scope_container, get_expando_initializer, get_jsdoc_deprecated_tag,
    get_jsdoc_root, get_local_symbol_for_export_default, get_name_of_declaration,
    get_name_of_expando, get_or_update, get_root_declaration, has_static_modifier, index_of_gc,
    is_ambient_module, is_binding_element, is_binding_pattern, is_class_declaration,
    is_class_element, is_class_like, is_class_static_block_declaration, is_computed_property_name,
    is_external_or_common_js_module, is_for_in_or_of_statement, is_function_like,
    is_global_scope_augmentation, is_identifier, is_in_js_file, is_interface_declaration,
    is_jsdoc_template_tag, is_jsdoc_type_alias, is_module_declaration,
    is_namespace_export_declaration, is_nullish_coalesce, is_object_binding_pattern,
    is_optional_chain, is_parameter, is_parameter_declaration, is_parameter_property_declaration,
    is_private_identifier, is_property_declaration, is_require_call, is_source_file, is_static,
    is_this_property, is_type_alias_declaration, is_type_node, length, maybe_for_each,
    node_is_synthesized, null_transformation_context, out_file, push_if_unique_gc,
    return_ok_default_if_none, set_text_range_pos_end, set_value_declaration, some, try_cast,
    CancellationTokenDebuggable, Comparison, DiagnosticCategory, DiagnosticInterface,
    DiagnosticMessageChain, DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface,
    Diagnostics, DuplicateInfoForFiles, DuplicateInfoForSymbol, EmitResolver,
    FindAncestorCallbackReturn, HasInitializerInterface, InternalSymbolName, ModuleKind,
    NamedDeclarationInterface, NodeArray, NodeFlags, PatternAmbientModule, PragmaArgumentName,
    PragmaName, ReadonlyTextRange, ScriptTarget, VisitResult, __String, create_diagnostic_for_node,
    escape_leading_underscores, get_first_identifier, get_or_update_indexmap,
    get_source_file_of_node, is_jsx_opening_fragment, maybe_get_source_file_of_node,
    maybe_visit_each_child, maybe_visit_node, parse_isolated_entity_name, try_find_ancestor,
    unescape_leading_underscores, BaseTransientSymbol, CheckFlags, Debug_, Diagnostic,
    DiagnosticMessage, HasArena, InArena, Node, NodeInterface, NodeLinks, Symbol, SymbolFlags,
    SymbolInterface, SymbolLinks, SymbolTable, SyntaxKind, TransientSymbol,
    TransientSymbolInterface, TypeChecker, _d,
    push_if_unique_eq, index_of_eq, get_factory,
};

impl TypeChecker {
    pub(super) fn get_jsx_namespace_(&self, location: Option<Id<Node>>) -> __String {
        if let Some(location) = location {
            let file = maybe_get_source_file_of_node(Some(location), self);
            if let Some(file) = file {
                let file_ref = file.ref_(self);
                let file_as_source_file = file_ref.as_source_file();
                if is_jsx_opening_fragment(&location.ref_(self)) {
                    if let Some(file_local_jsx_fragment_namespace) = file_as_source_file
                        .maybe_local_jsx_fragment_namespace()
                        .as_ref()
                    {
                        return file_local_jsx_fragment_namespace.clone();
                    }
                    let file_pragmas = file_as_source_file.pragmas();
                    let jsx_fragment_pragma = file_pragmas.get(&PragmaName::Jsxfrag);
                    if let Some(jsx_fragment_pragma) = jsx_fragment_pragma {
                        let chosen_pragma = &jsx_fragment_pragma[0];
                        let mut file_local_jsx_fragment_factory =
                            file_as_source_file.maybe_local_jsx_fragment_factory();
                        *file_local_jsx_fragment_factory = parse_isolated_entity_name(
                            chosen_pragma
                                .arguments
                                .get(&PragmaArgumentName::Factory)
                                .unwrap()
                                .as_without_captured_span()
                                .clone(),
                            self.language_version,
                            self,
                        );
                        maybe_visit_node(
                            file_local_jsx_fragment_factory.clone(),
                            Some(|node: Id<Node>| self.mark_as_synthetic(node)),
                            Option::<fn(Id<Node>) -> bool>::None,
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        );
                        if let Some(&file_local_jsx_fragment_factory) =
                            file_local_jsx_fragment_factory.as_ref()
                        {
                            let ret = get_first_identifier(file_local_jsx_fragment_factory, self)
                                .ref_(self).as_identifier()
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
                        let ret = get_first_identifier(entity, self)
                            .ref_(self).as_identifier()
                            .escaped_text
                            .clone();
                        *file_as_source_file.maybe_local_jsx_fragment_namespace() =
                            Some(ret.clone());
                        return ret;
                    }
                } else {
                    let local_jsx_namespace = self.get_local_jsx_namespace(file);
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
            *_jsx_namespace = Some("React".to_owned());
            if let Some(compiler_options_jsx_factory) = self.compiler_options.ref_(self).jsx_factory.as_ref() {
                let mut _jsx_factory_entity = self._jsx_factory_entity.borrow_mut();
                *_jsx_factory_entity = parse_isolated_entity_name(
                    compiler_options_jsx_factory.clone(),
                    self.language_version,
                    self,
                );
                maybe_visit_node(
                    _jsx_factory_entity.clone(),
                    Some(|node: Id<Node>| self.mark_as_synthetic(node)),
                    Option::<fn(Id<Node>) -> bool>::None,
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                );
                if let Some(_jsx_factory_entity) = _jsx_factory_entity.clone() {
                    *_jsx_namespace = Some(
                        get_first_identifier(_jsx_factory_entity, self)
                            .ref_(self).as_identifier()
                            .escaped_text
                            .clone(),
                    );
                }
            } else if let Some(compiler_options_react_namespace) =
                self.compiler_options.ref_(self).react_namespace.as_ref()
            {
                *_jsx_namespace =
                    Some(escape_leading_underscores(compiler_options_react_namespace).into_owned());
            }
        }
        let _jsx_namespace = _jsx_namespace.clone().unwrap();
        let mut _jsx_factory_entity = self._jsx_factory_entity.borrow_mut();
        if _jsx_factory_entity.is_none() {
            *_jsx_factory_entity =
                Some(get_factory(self).create_qualified_name(
                    get_factory(self).create_identifier(&unescape_leading_underscores(&_jsx_namespace)),
                    "createElement",
                ));
        }
        _jsx_namespace
    }

    pub(super) fn get_local_jsx_namespace(
        &self,
        file: Id<Node>, /*SourceFile*/
    ) -> Option<__String> {
        let file_ref = file.ref_(self);
        let file_as_source_file = file_ref.as_source_file();
        if let Some(file_local_jsx_namespace) =
            file_as_source_file.maybe_local_jsx_namespace().as_ref()
        {
            return Some(file_local_jsx_namespace.clone());
        }
        let file_pragmas = file_as_source_file.pragmas();
        let jsx_pragma = file_pragmas.get(&PragmaName::Jsx);
        if let Some(jsx_pragma) = jsx_pragma {
            let chosen_pragma = &jsx_pragma[0];
            let mut file_local_jsx_factory = file_as_source_file.maybe_local_jsx_factory();
            *file_local_jsx_factory = parse_isolated_entity_name(
                chosen_pragma
                    .arguments
                    .get(&PragmaArgumentName::Factory)
                    .unwrap()
                    .as_without_captured_span()
                    .clone(),
                self.language_version,
                self,
            );
            maybe_visit_node(
                file_local_jsx_factory.clone(),
                Some(|node: Id<Node>| self.mark_as_synthetic(node)),
                Option::<fn(Id<Node>) -> bool>::None,
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            );
            if let Some(&file_local_jsx_factory) = file_local_jsx_factory.as_ref() {
                let ret = get_first_identifier(file_local_jsx_factory, self)
                    .ref_(self).as_identifier()
                    .escaped_text
                    .clone();
                *file_as_source_file.maybe_local_jsx_namespace() = Some(ret.clone());
                return Some(ret);
            }
        }
        None
    }

    pub(super) fn mark_as_synthetic(&self, node: Id<Node>) -> VisitResult {
        set_text_range_pos_end(&*node.ref_(self), -1, -1);
        maybe_visit_each_child(
            Some(node),
            |node: Id<Node>| self.mark_as_synthetic(node),
            &*null_transformation_context,
            self,
        )
        .map(Into::into)
    }

    pub fn get_emit_resolver(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<Id<Box<dyn EmitResolver>>> {
        self.get_diagnostics(source_file, cancellation_token)?;
        Ok(self.emit_resolver())
    }

    pub(super) fn lookup_or_issue_error(
        &self,
        location: Option<Id<Node>>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Id<Diagnostic> {
        let diagnostic: Id<Diagnostic> = self.alloc_diagnostic(if let Some(location) = location {
            create_diagnostic_for_node(location, message, args, self).into()
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

    pub(super) fn error_skipped_on(
        &self,
        key: String, /*keyof CompilerOptions*/
        location: Option<Id<Node>>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Id<Diagnostic> {
        let diagnostic = self.error(location, message, args);
        *diagnostic.ref_(self).maybe_skipped_on_mut() = Some(key);
        diagnostic
    }

    pub(super) fn create_error(
        &self,
        location: Option<Id<Node>>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Id<Diagnostic> {
        self.alloc_diagnostic(if let Some(location) = location {
            create_diagnostic_for_node(location, message, args, self).into()
        } else {
            create_compiler_diagnostic(message, args).into()
        })
    }

    pub(super) fn error(
        &self,
        location: Option<Id<Node>>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Id<Diagnostic> {
        let diagnostic = self.create_error(location, message, args);
        self.diagnostics().add(diagnostic.clone());
        diagnostic
    }

    pub(super) fn add_error_or_suggestion(&self, is_error: bool, diagnostic: Id<Diagnostic>) {
        if is_error {
            self.diagnostics().add(diagnostic);
        } else {
            diagnostic.ref_(self).set_category(DiagnosticCategory::Suggestion); // TODO: (also commented on this elsewhere) is it ok that this mutates the diagnostic?
            self.suggestion_diagnostics().add(diagnostic);
        }
    }

    pub(super) fn error_or_suggestion(
        &self,
        is_error: bool,
        location: Id<Node>,
        message: impl Into<DiagnosticMessageOrDiagnosticMessageChain>,
        args: Option<Vec<String>>,
    ) {
        let message = message.into();
        if location.ref_(self).pos() < 0 || location.ref_(self).end() < 0 {
            if !is_error {
                return;
            }
            let file = get_source_file_of_node(location, self);
            self.add_error_or_suggestion(
                is_error,
                self.alloc_diagnostic(match message {
                    DiagnosticMessageOrDiagnosticMessageChain::DiagnosticMessage(message) => {
                        create_file_diagnostic(file, 0, 0, &message, args).into()
                    }
                    DiagnosticMessageOrDiagnosticMessageChain::DiagnosticMessageChain(message) => {
                        create_diagnostic_for_file_from_message_chain(file, message, None).into()
                    }
                }),
            );
            return;
        }
        self.add_error_or_suggestion(
            is_error,
            self.alloc_diagnostic(match message {
                DiagnosticMessageOrDiagnosticMessageChain::DiagnosticMessage(message) => {
                    create_diagnostic_for_node(location, &message, args, self).into()
                }
                DiagnosticMessageOrDiagnosticMessageChain::DiagnosticMessageChain(message) => {
                    create_diagnostic_for_node_from_message_chain(location, message, None, self).into()
                }
            }),
        );
    }

    pub(super) fn error_and_maybe_suggest_await(
        &self,
        location: Id<Node>,
        maybe_missing_await: bool,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Id<Diagnostic> {
        let diagnostic = self.error(Some(location), message, args);
        if maybe_missing_await {
            let related: Id<DiagnosticRelatedInformation> = self.alloc_diagnostic_related_information(create_diagnostic_for_node(
                location,
                &Diagnostics::Did_you_forget_to_use_await,
                None,
                self,
            )
            .into());
            add_related_info(&diagnostic.ref_(self), vec![related]);
        }
        diagnostic
    }

    pub(super) fn add_deprecated_suggestion_worker(
        &self,
        declarations: &[Id<Node>],
        diagnostic: Id<Diagnostic /*DiagnosticWithLocation*/>,
    ) -> Id<Diagnostic> {
        let deprecated_tag = for_each(declarations, |&declaration, _| {
            get_jsdoc_deprecated_tag(declaration, self)
        });
        if let Some(deprecated_tag) = deprecated_tag {
            add_related_info(
                &diagnostic.ref_(self),
                vec![self.alloc_diagnostic_related_information(create_diagnostic_for_node(
                    deprecated_tag,
                    &Diagnostics::The_declaration_was_marked_as_deprecated_here,
                    None,
                    self,
                )
                .into())],
            );
        }
        self.suggestion_diagnostics().add(diagnostic.clone());
        diagnostic
    }

    pub(super) fn add_deprecated_suggestion(
        &self,
        location: Id<Node>,
        declarations: &[Id<Node>],
        deprecated_entity: &str,
    ) -> Id<Diagnostic> {
        let diagnostic: Id<Diagnostic> = self.alloc_diagnostic(create_diagnostic_for_node(
            location,
            &Diagnostics::_0_is_deprecated,
            Some(vec![deprecated_entity.to_owned()]),
            self,
        )
        .into());
        self.add_deprecated_suggestion_worker(declarations, diagnostic)
    }

    pub(super) fn add_deprecated_suggestion_with_signature(
        &self,
        location: Id<Node>,
        declaration: Id<Node>,
        deprecated_entity: Option<&str>,
        signature_string: &str,
    ) -> Id<Diagnostic> {
        let diagnostic: Id<Diagnostic> =
            self.alloc_diagnostic(if let Some(deprecated_entity) = deprecated_entity {
                create_diagnostic_for_node(
                    location,
                    &Diagnostics::The_signature_0_of_1_is_deprecated,
                    Some(vec![
                        signature_string.to_owned(),
                        deprecated_entity.to_owned(),
                    ]),
                    self,
                )
                .into()
            } else {
                create_diagnostic_for_node(
                    location,
                    &Diagnostics::_0_is_deprecated,
                    Some(vec![signature_string.to_owned()]),
                    self,
                )
                .into()
            });
        self.add_deprecated_suggestion_worker(&vec![declaration], diagnostic)
    }

    pub(super) fn create_symbol(
        &self,
        flags: SymbolFlags,
        name: __String,
        check_flags: Option<CheckFlags>,
    ) -> TransientSymbol {
        self.increment_symbol_count();
        let symbol = (self.Symbol)(flags | SymbolFlags::Transient, name);
        let symbol = BaseTransientSymbol::new(symbol, check_flags.unwrap_or(CheckFlags::None), self);
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

    pub(super) fn record_merged_symbol(&self, target: Id<Symbol>, source: Id<Symbol>) {
        if source.ref_(self).maybe_merge_id().is_none() {
            source.ref_(self).set_merge_id(get_next_merge_id());
            increment_next_merge_id();
        }
        self.merged_symbols()
            .insert(source.ref_(self).maybe_merge_id().unwrap(), target);
    }

    pub(super) fn clone_symbol(&self, symbol: Id<Symbol>) -> Id<Symbol> {
        let result = self.alloc_symbol(
            self.create_symbol(
                symbol.ref_(self).flags(),
                symbol.ref_(self).escaped_name().to_owned(),
                None,
            )
            .into(),
        );
        result.ref_(self).set_declarations(
            if let Some(symbol_declarations) = symbol.ref_(self).maybe_declarations().as_ref() {
                symbol_declarations.clone()
            } else {
                vec![]
            },
        );
        result
            .ref_(self)
            .set_parent(symbol.ref_(self).maybe_parent().clone());
        if let Some(symbol_value_declaration) = symbol.ref_(self).maybe_value_declaration().as_ref()
        {
            result
                .ref_(self)
                .set_value_declaration(symbol_value_declaration.clone());
        }
        if matches!(symbol.ref_(self).maybe_const_enum_only_module(), Some(true)) {
            result.ref_(self).set_const_enum_only_module(Some(true));
        }
        if let Some(symbol_members) = symbol.ref_(self).maybe_members().as_ref() {
            *result.ref_(self).maybe_members_mut() =
                Some(Gc::new(GcCell::new((**symbol_members).borrow().clone())));
        }
        if let Some(symbol_exports) = symbol.ref_(self).maybe_exports().as_ref() {
            *result.ref_(self).maybe_exports_mut() =
                Some(Gc::new(GcCell::new((**symbol_exports).borrow().clone())));
        }
        self.record_merged_symbol(result, symbol);
        result
    }

    pub(super) fn merge_symbol(
        &self,
        mut target: Id<Symbol>,
        source: Id<Symbol>,
        unidirectional: Option<bool>,
    ) -> io::Result<Id<Symbol>> {
        let unidirectional = unidirectional.unwrap_or(false);
        if !target
            .ref_(self)
            .flags()
            .intersects(self.get_excluded_symbol_flags(source.ref_(self).flags()))
            || (source.ref_(self).flags() | target.ref_(self).flags())
                .intersects(SymbolFlags::Assignment)
        {
            if source == target {
                return Ok(target);
            }
            if !target.ref_(self).flags().intersects(SymbolFlags::Transient) {
                let resolved_target = self.resolve_symbol(Some(target), None)?.unwrap();
                if resolved_target == self.unknown_symbol() {
                    return Ok(source);
                }
                target = self.clone_symbol(resolved_target);
            }
            if source
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::ValueModule)
                && target
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::ValueModule)
                && matches!(target.ref_(self).maybe_const_enum_only_module(), Some(true))
                && !matches!(source.ref_(self).maybe_const_enum_only_module(), Some(true))
            {
                target.ref_(self).set_const_enum_only_module(Some(true));
            }
            target
                .ref_(self)
                .set_flags(target.ref_(self).flags() | source.ref_(self).flags());
            if let Some(source_value_declaration) =
                source.ref_(self).maybe_value_declaration()
            {
                set_value_declaration(target, source_value_declaration, self);
            }
            if let Some(source_declarations) = source.ref_(self).maybe_declarations().as_deref() {
                let target_ref = target.ref_(self);
                let mut target_declarations = target_ref.maybe_declarations_mut();
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
            if let Some(source_members) = source.ref_(self).maybe_members().as_ref() {
                let target_ref = target.ref_(self);
                let mut target_members = target_ref.maybe_members_mut();
                if target_members.is_none() {
                    *target_members = Some(Gc::new(GcCell::new(create_symbol_table(
                        self.arena(),
                        Option::<&[Id<Symbol>]>::None,
                    ))));
                }
                self.merge_symbol_table(
                    target_members.clone().unwrap(),
                    &(**source_members).borrow(),
                    Some(unidirectional),
                )?;
            }
            if let Some(source_exports) = source.ref_(self).maybe_exports().as_ref() {
                let target_ref = target.ref_(self);
                let mut target_exports = target_ref.maybe_exports_mut();
                if target_exports.is_none() {
                    *target_exports = Some(Gc::new(GcCell::new(create_symbol_table(
                        self.arena(),
                        Option::<&[Id<Symbol>]>::None,
                    ))));
                }
                self.merge_symbol_table(
                    target_exports.clone().unwrap(),
                    &(**source_exports).borrow(),
                    Some(unidirectional),
                )?;
            }
            if !unidirectional {
                self.record_merged_symbol(target, source);
            }
        } else if target
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::NamespaceModule)
        {
            if target != self.global_this_symbol() {
                self.error(
                    source.ref_(self).maybe_declarations().as_ref().and_then(|source_declarations| get_name_of_declaration(source_declarations.get(0).map(Clone::clone), self)),
                    &Diagnostics::Cannot_augment_module_0_with_value_exports_because_it_resolves_to_a_non_module_entity,
                    Some(vec![self.symbol_to_string_(target, Option::<Id<Node>>::None, None, None, None)?])
                );
            }
        } else {
            let is_either_enum = target.ref_(self).flags().intersects(SymbolFlags::Enum)
                || source.ref_(self).flags().intersects(SymbolFlags::Enum);
            let is_either_block_scoped = target
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::BlockScopedVariable)
                || source
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::BlockScopedVariable);
            let message = if is_either_enum {
                &*Diagnostics::Enum_declarations_can_only_merge_with_namespace_or_other_enum_declarations
            } else if is_either_block_scoped {
                &*Diagnostics::Cannot_redeclare_block_scoped_variable_0
            } else {
                &*Diagnostics::Duplicate_identifier_0
            };
            let source_symbol_file =
                source
                    .ref_(self)
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|source_declarations| {
                        maybe_get_source_file_of_node(source_declarations.get(0).copied(), self)
                    });
            let target_symbol_file =
                target
                    .ref_(self)
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|target_declarations| {
                        maybe_get_source_file_of_node(target_declarations.get(0).copied(), self)
                    });
            let symbol_name =
                self.symbol_to_string_(source, Option::<Id<Node>>::None, None, None, None)?;

            if source_symbol_file.is_some()
                && target_symbol_file.is_some()
                && self.maybe_amalgamated_duplicates().is_some()
                && !is_either_enum
                && source_symbol_file.unwrap() != target_symbol_file.unwrap()
            {
                let source_symbol_file = source_symbol_file.unwrap();
                let target_symbol_file = target_symbol_file.unwrap();
                let first_file = if compare_paths(
                    &source_symbol_file.ref_(self).as_source_file().path(),
                    &target_symbol_file.ref_(self).as_source_file().path(),
                    Option::<String>::None,
                    None,
                ) == Comparison::LessThan
                {
                    source_symbol_file.clone()
                } else {
                    target_symbol_file.clone()
                };
                let second_file = if first_file == source_symbol_file {
                    target_symbol_file
                } else {
                    source_symbol_file
                };
                let mut amalgamated_duplicates = self.maybe_amalgamated_duplicates();
                let amalgamated_duplicates = amalgamated_duplicates.as_mut().unwrap();
                let files_duplicates = get_or_update(
                    amalgamated_duplicates,
                    format!(
                        "{}|{}",
                        &**first_file.ref_(self).as_source_file().path(),
                        &**second_file.ref_(self).as_source_file().path()
                    ),
                    || DuplicateInfoForFiles {
                        first_file,
                        second_file,
                        conflicting_symbols: IndexMap::new(),
                    },
                );
                let conflicting_symbol_info = get_or_update_indexmap(
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
                    target,
                );
            } else {
                self.add_duplicate_declaration_errors_for_symbols(
                    source,
                    message,
                    &symbol_name,
                    target,
                );
                self.add_duplicate_declaration_errors_for_symbols(
                    target,
                    message,
                    &symbol_name,
                    source,
                );
            }
        }
        Ok(target)
    }

    pub(super) fn add_duplicate_locations(
        &self,
        locs: &mut Vec<Id<Node /*Declaration*/>>,
        symbol: Id<Symbol>,
    ) {
        if let Some(symbol_declarations) = symbol.ref_(self).maybe_declarations().as_ref() {
            for &decl in symbol_declarations {
                push_if_unique_eq(locs, &decl);
            }
        }
    }

    pub(super) fn add_duplicate_declaration_errors_for_symbols(
        &self,
        target: Id<Symbol>,
        message: &DiagnosticMessage,
        symbol_name: &str,
        source: Id<Symbol>,
    ) {
        maybe_for_each(
            target.ref_(self).maybe_declarations().as_deref(),
            |&node, _| {
                self.add_duplicate_declaration_error(
                    node,
                    message,
                    symbol_name,
                    source.ref_(self).maybe_declarations().as_deref(),
                );
                Option::<()>::None
            },
        );
    }

    pub(super) fn add_duplicate_declaration_error(
        &self,
        node: Id<Node>, /*Declaration*/
        message: &DiagnosticMessage,
        symbol_name: &str,
        related_nodes: Option<&[Id<Node /*Declaration*/>]>,
    ) {
        let error_node = (if get_expando_initializer(node, false, self).is_some() {
            get_name_of_expando(node, self)
        } else {
            get_name_of_declaration(Some(node), self)
        })
        .unwrap_or(node);
        let err = self.lookup_or_issue_error(
            Some(error_node),
            message,
            Some(vec![symbol_name.to_owned()]),
        );
        if let Some(related_nodes) = related_nodes {
            for &related_node in related_nodes {
                let adjusted_node = (if get_expando_initializer(related_node, false, self).is_some() {
                    get_name_of_expando(related_node, self)
                } else {
                    get_name_of_declaration(Some(related_node), self)
                })
                .unwrap_or(related_node);
                if adjusted_node == error_node {
                    continue;
                }
                {
                    let err_ref = err.ref_(self);
                    let mut err_related_information = err_ref.maybe_related_information_mut();
                    if err_related_information.is_none() {
                        *err_related_information = Some(vec![]);
                    }
                }
                let leading_message: Id<DiagnosticRelatedInformation> = self.alloc_diagnostic_related_information(create_diagnostic_for_node(
                    adjusted_node,
                    &Diagnostics::_0_was_also_declared_here,
                    Some(vec![symbol_name.to_owned()]),
                    self,
                )
                .into());
                let follow_on_message: Id<DiagnosticRelatedInformation> =
                    self.alloc_diagnostic_related_information(create_diagnostic_for_node(adjusted_node, &Diagnostics::and_here, None, self).into());
                if length(err.ref_(self).maybe_related_information().as_deref()) >= 5
                    || some(
                        err.ref_(self).maybe_related_information().as_deref(),
                        Some(|r: &Id<DiagnosticRelatedInformation>| {
                            compare_diagnostics(&*r.ref_(self), &*follow_on_message.ref_(self), self) == Comparison::EqualTo
                                || compare_diagnostics(&*r.ref_(self), &*leading_message.ref_(self), self)
                                    == Comparison::EqualTo
                        }),
                    )
                {
                    continue;
                }
                add_related_info(
                    &err.ref_(self),
                    vec![if length(err.ref_(self).maybe_related_information().as_deref()) == 0 {
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
        first: Option<Gc<GcCell<SymbolTable>>>,
        second: Option<Gc<GcCell<SymbolTable>>>,
    ) -> io::Result<Option<Gc<GcCell<SymbolTable>>>> {
        if first.is_none() {
            return Ok(second);
        }
        if second.is_none() {
            return Ok(first);
        }
        let first = first.unwrap();
        let second = second.unwrap();
        if (*first).borrow().is_empty() {
            return Ok(Some(second));
        }
        if (*second).borrow().is_empty() {
            return Ok(Some(first));
        }
        let combined = Gc::new(GcCell::new(create_symbol_table(
            self.arena(),
            Option::<&[Id<Symbol>]>::None,
        )));
        self.merge_symbol_table(combined.clone(), &(*first).borrow(), None)?;
        self.merge_symbol_table(combined.clone(), &(*second).borrow(), None)?;
        Ok(Some(combined))
    }

    pub(super) fn merge_symbol_table(
        &self,
        target: Gc<GcCell<SymbolTable>>,
        source: &SymbolTable,
        unidirectional: Option<bool>,
    ) -> io::Result<()> {
        let unidirectional = unidirectional.unwrap_or(false);
        for (id, &source_symbol) in source {
            let target_symbol = {
                let value = (*target).borrow().get(id).cloned();
                value
            };
            let value = if let Some(target_symbol) = target_symbol {
                self.merge_symbol(target_symbol, source_symbol, Some(unidirectional))?
            } else {
                source_symbol.clone()
            };
            target.borrow_mut().insert(id.clone(), value);
        }

        Ok(())
    }

    pub(super) fn merge_module_augmentation(
        &self,
        module_name: Id<Node>, /*StringLiteral | Identifier*/
    ) -> io::Result<()> {
        let module_augmentation = module_name.ref_(self).parent();
        if !matches!(
            module_augmentation.ref_(self).symbol().ref_(self).maybe_declarations().as_ref().and_then(|declarations| declarations.get(0)).copied(),
            Some(declaration) if declaration == module_augmentation
        ) {
            Debug_.assert(
                matches!(
                    module_augmentation.ref_(self).symbol().ref_(self).maybe_declarations().as_ref(),
                    Some(declarations) if declarations.len() > 1
                ),
                None
            );
            return Ok(());
        }

        if is_global_scope_augmentation(&module_augmentation.ref_(self)) {
            self.merge_symbol_table(
                self.globals_rc(),
                &mut module_augmentation
                    .ref_(self).symbol()
                    .ref_(self)
                    .exports()
                    .borrow_mut(),
                None,
            )?;
        } else {
            let module_not_found_error = if !module_name
                .ref_(self).parent()
                .ref_(self).parent()
                .ref_(self).flags()
                .intersects(NodeFlags::Ambient)
            {
                Some(&*Diagnostics::Invalid_module_name_in_augmentation_module_0_cannot_be_found)
            } else {
                None
            };
            let main_module = return_ok_default_if_none!(self
                .resolve_external_module_name_worker(
                    module_name,
                    module_name,
                    module_not_found_error,
                    Some(true),
                )?);
            let main_module = self
                .resolve_external_module_symbol(Some(main_module), None)?
                .unwrap();
            if main_module
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::Namespace)
            {
                if some(
                    self.maybe_pattern_ambient_modules().as_deref(),
                    Some(|module: &Gc<PatternAmbientModule>| main_module == module.symbol),
                ) {
                    let merged =
                        self.merge_symbol(module_augmentation.ref_(self).symbol(), main_module, Some(true))?;
                    let mut pattern_ambient_module_augmentations =
                        self.maybe_pattern_ambient_module_augmentations();
                    if pattern_ambient_module_augmentations.is_none() {
                        *pattern_ambient_module_augmentations = Some(HashMap::new());
                    }
                    pattern_ambient_module_augmentations
                        .as_mut()
                        .unwrap()
                        .insert(module_name.ref_(self).as_literal_like_node().text().clone(), merged);
                } else {
                    if main_module
                        .ref_(self)
                        .maybe_exports()
                        .as_ref()
                        .and_then(|exports| {
                            (**exports)
                                .borrow()
                                .get(InternalSymbolName::ExportStar)
                                .cloned()
                        })
                        .is_some()
                        && matches!(
                            module_augmentation.ref_(self).symbol().ref_(self).maybe_exports().as_ref(),
                            Some(exports) if !(**exports).borrow().is_empty()
                        )
                    {
                        let resolved_exports = self.get_resolved_members_or_exports_of_symbol(
                            main_module,
                            MembersOrExportsResolutionKind::resolved_exports,
                        )?;
                        let resolved_exports = (*resolved_exports).borrow();
                        let main_module_exports = main_module.ref_(self).exports();
                        let main_module_exports = (*main_module_exports).borrow();
                        for (key, &value) in
                            &*(*module_augmentation.ref_(self).symbol().ref_(self).exports()).borrow()
                        {
                            if resolved_exports.contains_key(key)
                                && !main_module_exports.contains_key(key)
                            {
                                self.merge_symbol(
                                    *resolved_exports.get(key).unwrap(),
                                    value,
                                    None,
                                )?;
                            }
                        }
                    }
                    self.merge_symbol(main_module, module_augmentation.ref_(self).symbol(), None)?;
                }
            } else {
                self.error(
                    Some(module_name),
                    &Diagnostics::Cannot_augment_module_0_because_it_resolves_to_a_non_module_entity,
                    Some(vec![module_name.ref_(self).as_literal_like_node().text().clone()])
                );
            }
        }

        Ok(())
    }

    pub(super) fn add_to_symbol_table(
        &self,
        target: &mut SymbolTable,
        source: &SymbolTable,
        message: &DiagnosticMessage,
    ) {
        for (id, source_symbol) in source {
            let target_symbol = target.get(id);
            if let Some(&target_symbol) = target_symbol {
                maybe_for_each(
                    target_symbol.ref_(self).maybe_declarations().as_deref(),
                    |&declaration: &Id<Node /*Declaration*/>, _| {
                        self.diagnostics().add(
                            self.alloc_diagnostic(create_diagnostic_for_node(
                                declaration,
                                message,
                                Some(vec![unescape_leading_underscores(id).to_owned()]),
                                self,
                            )
                            .into()),
                        );
                        Option::<()>::None
                    },
                );
            } else {
                target.insert(id.clone(), source_symbol.clone());
            }
        }
    }

    pub(super) fn get_symbol_links(&self, symbol: Id<Symbol>) -> Id<GcCell<SymbolLinks>> {
        if let Symbol::TransientSymbol(symbol) = &*symbol.ref_(self) {
            return symbol.symbol_links();
        }
        let id = get_symbol_id(&symbol.ref_(self));
        let mut symbol_links_table = self.symbol_links.borrow_mut();
        if let Some(symbol_links) = symbol_links_table.get(&id) {
            return symbol_links.clone();
        }
        let symbol_links: Id<GcCell<SymbolLinks>> = self.alloc_symbol_links(_d());
        symbol_links_table.insert(id, symbol_links.clone());
        symbol_links
    }

    pub(super) fn get_node_links(&self, node: Id<Node>) -> Gc<GcCell<NodeLinks>> {
        let id = get_node_id(&node.ref_(self));
        let mut node_links_table = self.node_links.borrow_mut();
        if let Some(node_links) = node_links_table.get(&id) {
            return node_links.clone();
        }
        let node_links = Gc::new(GcCell::new(NodeLinks::new()));
        node_links_table.insert(id, node_links.clone());
        node_links
    }

    pub(super) fn is_global_source_file(&self, node: Id<Node>) -> bool {
        node.ref_(self).kind() == SyntaxKind::SourceFile && !is_external_or_common_js_module(&node.ref_(self))
    }

    pub(super) fn get_symbol(
        &self,
        symbols: &SymbolTable,
        name: &str, /*__String*/
        meaning: SymbolFlags,
    ) -> io::Result<Option<Id<Symbol>>> {
        if meaning != SymbolFlags::None {
            let symbol = self.get_merged_symbol(symbols.get(name).map(Clone::clone));
            if let Some(symbol) = symbol {
                Debug_.assert(
                    !get_check_flags(&symbol.ref_(self)).intersects(CheckFlags::Instantiated),
                    Some("Should never get an instantiated symbol here."),
                );
                if symbol.ref_(self).flags().intersects(meaning) {
                    return Ok(Some(symbol));
                }
                if symbol.ref_(self).flags().intersects(SymbolFlags::Alias) {
                    let target = self.resolve_alias(symbol)?;
                    if target == self.unknown_symbol()
                        || target.ref_(self).flags().intersects(meaning)
                    {
                        return Ok(Some(symbol));
                    }
                }
            }
        }
        Ok(None)
    }

    pub(super) fn get_symbols_of_parameter_property_declaration_(
        &self,
        parameter: Id<Node>,  /*ParameterDeclaration*/
        parameter_name: &str, /*__String*/
    ) -> io::Result<Vec<Id<Symbol>>> {
        let constructor_declaration = parameter.ref_(self).parent();
        let class_declaration = parameter.ref_(self).parent().ref_(self).parent();

        let parameter_symbol = self.get_symbol(
            &(*constructor_declaration.ref_(self).locals()).borrow(),
            parameter_name,
            SymbolFlags::Value,
        )?;
        let property_symbol = self.get_symbol(
            &(*self.get_members_of_symbol(class_declaration.ref_(self).symbol())?).borrow(),
            parameter_name,
            SymbolFlags::Value,
        )?;

        Ok(match (parameter_symbol, property_symbol) {
            (Some(parameter_symbol), Some(property_symbol)) => vec![parameter_symbol, property_symbol],
            _ => Debug_.fail(Some("There should exist two symbols, one as property declaration and one as parameter declaration")),
        })
    }

    pub(super) fn is_block_scoped_name_declared_before_use(
        &self,
        declaration: Id<Node>, /*Declaration*/
        usage: Id<Node>,
    ) -> io::Result<bool> {
        let declaration_file = get_source_file_of_node(declaration, self);
        let use_file = get_source_file_of_node(usage, self);
        let decl_container = get_enclosing_block_scope_container(declaration, self).unwrap();
        if declaration_file != use_file {
            if self.module_kind != ModuleKind::None
                && (declaration_file
                    .ref_(self).as_source_file()
                    .maybe_external_module_indicator()
                    .is_some()
                    || use_file
                        .ref_(self).as_source_file()
                        .maybe_external_module_indicator()
                        .is_some())
                || !matches!(
                    out_file(&self.compiler_options.ref_(self)),
                    Some(out_file) if !out_file.is_empty()
                )
                || self.is_in_type_query(usage)
                || declaration.ref_(self).flags().intersects(NodeFlags::Ambient)
            {
                return Ok(true);
            }
            if self.is_used_in_function_or_instance_property(decl_container, usage, declaration)? {
                return Ok(true);
            }
            let host_ref = self.host.ref_(self);
            let source_files = host_ref.get_source_files();
            return Ok(index_of_eq(&*source_files, &declaration_file)
                <= index_of_eq(&*source_files, &use_file));
        }

        if declaration.ref_(self).pos() <= usage.ref_(self).pos()
            && !(is_property_declaration(&declaration.ref_(self)) && is_this_property(usage.ref_(self).parent(), self) && {
                let declaration_ref = declaration.ref_(self);
                let declaration_as_property_declaration = declaration_ref.as_property_declaration();
                !declaration_as_property_declaration
                    .maybe_initializer()
                    .is_some()
                    && !declaration_as_property_declaration
                        .exclamation_token
                        .is_some()
            })
        {
            if declaration.ref_(self).kind() == SyntaxKind::BindingElement {
                let error_binding_element = get_ancestor(Some(usage), SyntaxKind::BindingElement, self);
                if let Some(error_binding_element) = error_binding_element {
                    return Ok(
                        find_ancestor(Some(error_binding_element), |node: Id<Node>| is_binding_element(&node.ref_(self)), self) !=
                        find_ancestor(Some(declaration), |node: Id<Node>| is_binding_element(&node.ref_(self)), self)
                     || declaration.ref_(self).pos() < error_binding_element.ref_(self).pos());
                }
                return self.is_block_scoped_name_declared_before_use(
                    get_ancestor(Some(declaration), SyntaxKind::VariableDeclaration, self).unwrap(),
                    usage,
                );
            } else if declaration.ref_(self).kind() == SyntaxKind::VariableDeclaration {
                return Ok(
                    !self.is_immediately_used_in_initializer_of_block_scoped_variable(
                        decl_container,
                        declaration,
                        usage,
                    ),
                );
            } else if is_class_declaration(&declaration.ref_(self)) {
                return Ok(find_ancestor(Some(usage), |n| {
                    is_computed_property_name(&n.ref_(self)) && n.ref_(self).parent().ref_(self).parent() == declaration
                }, self)
                .is_none());
            } else if is_property_declaration(&declaration.ref_(self)) {
                return Ok(!self.is_property_immediately_referenced_within_declaration(
                    declaration,
                    usage,
                    false,
                ));
            } else if is_parameter_property_declaration(declaration, declaration.ref_(self).parent(), self) {
                return Ok(!(get_emit_script_target(&self.compiler_options.ref_(self))
                    == ScriptTarget::ESNext
                    && self.use_define_for_class_fields
                    && get_containing_class(declaration, self) == get_containing_class(usage, self)
                    && self.is_used_in_function_or_instance_property(
                        decl_container,
                        usage,
                        declaration,
                    )?));
            }
            return Ok(true);
        }

        if usage.ref_(self).parent().ref_(self).kind() == SyntaxKind::ExportSpecifier
            || usage.ref_(self).parent().ref_(self).kind() == SyntaxKind::ExportAssignment
                && usage.ref_(self).parent().ref_(self).as_export_assignment().is_export_equals == Some(true)
        {
            return Ok(true);
        }
        if usage.ref_(self).kind() == SyntaxKind::ExportAssignment
            && usage.ref_(self).as_export_assignment().is_export_equals == Some(true)
        {
            return Ok(true);
        }

        if usage.ref_(self).flags().intersects(NodeFlags::JSDoc)
            || self.is_in_type_query(usage)
            || self.usage_in_type_declaration(usage)
        {
            return Ok(true);
        }
        if self.is_used_in_function_or_instance_property(decl_container, usage, declaration)? {
            if get_emit_script_target(&self.compiler_options.ref_(self)) == ScriptTarget::ESNext
                && self.use_define_for_class_fields
                && get_containing_class(declaration, self).is_some()
                && (is_property_declaration(&declaration.ref_(self))
                    || is_parameter_property_declaration(declaration, declaration.ref_(self).parent(), self))
            {
                return Ok(!self.is_property_immediately_referenced_within_declaration(
                    declaration,
                    usage,
                    true,
                ));
            } else {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn usage_in_type_declaration(&self, usage: Id<Node>) -> bool {
        find_ancestor(Some(usage), |node| {
            is_interface_declaration(&node.ref_(self)) || is_type_alias_declaration(&node.ref_(self))
        }, self)
        .is_some()
    }

    pub(super) fn is_immediately_used_in_initializer_of_block_scoped_variable(
        &self,
        decl_container: Id<Node>,
        declaration: Id<Node>, /*VariableDeclaration*/
        usage: Id<Node>,
    ) -> bool {
        match declaration.ref_(self).parent().ref_(self).parent().ref_(self).kind() {
            SyntaxKind::VariableStatement
            | SyntaxKind::ForStatement
            | SyntaxKind::ForOfStatement => {
                if self.is_same_scope_descendent_of(usage, Some(declaration), decl_container) {
                    return true;
                }
            }
            _ => (),
        }

        let grandparent = declaration.ref_(self).parent().ref_(self).parent();
        is_for_in_or_of_statement(&grandparent.ref_(self))
            && self.is_same_scope_descendent_of(
                usage,
                Some(grandparent.ref_(self).as_has_expression().expression()),
                decl_container,
            )
    }

    pub(super) fn is_used_in_function_or_instance_property(
        &self,
        decl_container: Id<Node>,
        usage: Id<Node>,
        declaration: Id<Node>,
    ) -> io::Result<bool> {
        Ok(try_find_ancestor(Some(usage), |current| -> io::Result<_> {
            if current == decl_container {
                return Ok(FindAncestorCallbackReturn::Quit);
            }
            if is_function_like(Some(&current.ref_(self))) {
                return Ok(true.into());
            }
            if is_class_static_block_declaration(&current.ref_(self)) {
                return Ok((declaration.ref_(self).pos() < usage.ref_(self).pos()).into());
            }

            let property_declaration = current.ref_(self).maybe_parent().and_then(|parent| {
                try_cast(parent, |node: &Id<Node>| is_property_declaration(&node.ref_(self)))
            });
            if let Some(property_declaration) = property_declaration {
                let initializer_of_property =
                    property_declaration.ref_(self).as_property_declaration().maybe_initializer() == Some(current);
                if initializer_of_property {
                    if is_static(current.ref_(self).parent(), self) {
                        if declaration.ref_(self).kind() == SyntaxKind::MethodDeclaration {
                            return Ok(true.into());
                        }
                        if is_property_declaration(&declaration.ref_(self))
                            && get_containing_class(usage, self) == get_containing_class(declaration, self)
                        {
                            let prop_name = declaration.ref_(self).as_property_declaration().name();
                            if is_identifier(&prop_name.ref_(self)) || is_private_identifier(&prop_name.ref_(self)) {
                                let type_ = self.get_type_of_symbol(
                                    self.get_symbol_of_node(declaration)?.unwrap(),
                                )?;
                                let static_blocks = declaration
                                    .ref_(self).parent()
                                    .ref_(self).as_class_like_declaration()
                                    .members()
                                    .owned_iter()
                                    .filter(|node| is_class_static_block_declaration(&node.ref_(self)));
                                if self.is_property_initialized_in_static_blocks(
                                    prop_name,
                                    type_,
                                    static_blocks,
                                    declaration.ref_(self).parent().ref_(self).pos(),
                                    current.ref_(self).pos(),
                                )? {
                                    return Ok(true.into());
                                }
                            }
                        }
                    } else {
                        let is_declaration_instance_property = declaration.ref_(self).kind()
                            == SyntaxKind::PropertyDeclaration
                            && !is_static(declaration, self);
                        if !is_declaration_instance_property
                            || get_containing_class(usage, self) != get_containing_class(declaration, self)
                        {
                            return Ok(true.into());
                        }
                    }
                }
            }
            Ok(false.into())
        }, self)?
        .is_some())
    }

    pub(super) fn is_property_immediately_referenced_within_declaration(
        &self,
        declaration: Id<Node>, /*PropertyDeclaration | ParameterPropertyDeclaration*/
        usage: Id<Node>,
        stop_at_any_property_declaration: bool,
    ) -> bool {
        if usage.ref_(self).end() > declaration.ref_(self).end() {
            return false;
        }

        let ancestor_changing_reference_scope = find_ancestor(Some(usage), |node| {
            if node == declaration {
                return FindAncestorCallbackReturn::Quit;
            }

            match node.ref_(self).kind() {
                SyntaxKind::ArrowFunction => true.into(),
                SyntaxKind::PropertyDeclaration => {
                    if stop_at_any_property_declaration
                        && (is_property_declaration(&declaration.ref_(self))
                            && node.ref_(self).parent() == declaration.ref_(self).parent()
                            || is_parameter_property_declaration(
                                declaration,
                                declaration.ref_(self).parent(),
                                self,
                            ) && node.ref_(self).parent() == declaration.ref_(self).parent().ref_(self).parent())
                    {
                        FindAncestorCallbackReturn::Quit
                    } else {
                        true.into()
                    }
                }
                SyntaxKind::Block => matches!(
                    node.ref_(self).parent().ref_(self).kind(),
                    SyntaxKind::GetAccessor
                        | SyntaxKind::MethodDeclaration
                        | SyntaxKind::SetAccessor
                )
                .into(),
                _ => false.into(),
            }
        }, self);

        ancestor_changing_reference_scope.is_none()
    }

    pub(super) fn use_outer_variable_scope_in_parameter(
        &self,
        result: Id<Symbol>,
        location: Id<Node>,
        last_location: Id<Node>,
    ) -> bool {
        let target = get_emit_script_target(&self.compiler_options.ref_(self));
        let location_ref = location.ref_(self);
        let function_location = location_ref.maybe_as_function_like_declaration();
        if is_parameter(&last_location.ref_(self))
            && match (
                function_location.and_then(|function_location| function_location.maybe_body()),
                result.ref_(self).maybe_value_declaration().as_ref(),
            ) {
                (Some(function_location_body), Some(result_value_declaration)) => {
                    result_value_declaration.ref_(self).pos() >= function_location_body.ref_(self).pos()
                        && result_value_declaration.ref_(self).end() <= function_location_body.ref_(self).end()
                }
                _ => false,
            }
        {
            if target >= ScriptTarget::ES2015 {
                let links = self.get_node_links(location);
                let mut links = links.borrow_mut();
                if links.declaration_requires_scope_change.is_none() {
                    links.declaration_requires_scope_change = Some(
                        for_each_bool(
                            &function_location.unwrap().parameters(),
                            |&node: &Id<Node>, _| self.requires_scope_change(target, node),
                        ) || false,
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
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> bool {
        self.requires_scope_change_worker(target, node)
            || matches!(
                node.ref_(self).as_parameter_declaration().maybe_initializer(),
                Some(initializer) if self.requires_scope_change_worker(target, initializer)
            )
    }

    pub(super) fn requires_scope_change_worker(
        &self,
        target: ScriptTarget,
        node: Id<Node>,
    ) -> bool {
        match node.ref_(self).kind() {
            SyntaxKind::ArrowFunction
            | SyntaxKind::FunctionExpression
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::Constructor => false,
            SyntaxKind::MethodDeclaration
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::PropertyAssignment => {
                self.requires_scope_change_worker(target, node.ref_(self).as_named_declaration().name())
            }
            SyntaxKind::PropertyDeclaration => {
                if has_static_modifier(node, self) {
                    return target < ScriptTarget::ESNext || !self.use_define_for_class_fields;
                }
                self.requires_scope_change_worker(target, node.ref_(self).as_property_declaration().name())
            }
            _ => {
                if is_nullish_coalesce(node, self) || is_optional_chain(&node.ref_(self)) {
                    return target < ScriptTarget::ES2020;
                }
                if is_binding_element(&node.ref_(self))
                    && node.ref_(self).as_binding_element().dot_dot_dot_token.is_some()
                    && is_object_binding_pattern(&node.ref_(self).parent().ref_(self))
                {
                    return target < ScriptTarget::ES2017;
                }
                if is_type_node(&node.ref_(self)) {
                    return false;
                }
                for_each_child_bool(
                    node,
                    |child| self.requires_scope_change_worker(target, child),
                    Option::<fn(&NodeArray) -> bool>::None,
                    self,
                ) || false
            }
        }
    }

    pub(super) fn resolve_name_<'name_arg>(
        &self,
        location: Option<Id<Node>>,
        name: &str, /*__String*/
        meaning: SymbolFlags,
        name_not_found_message: Option<&DiagnosticMessage>,
        name_arg: Option<impl Into<ResolveNameNameArg<'name_arg>> + Clone>,
        is_use: bool,
        exclude_globals: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let exclude_globals = exclude_globals.unwrap_or(false);
        self.resolve_name_helper(
            location,
            name,
            meaning,
            name_not_found_message,
            name_arg,
            is_use,
            exclude_globals,
            |symbols: &SymbolTable, name: &str /*__String*/, meaning: SymbolFlags| {
                self.get_symbol(symbols, name, meaning)
            },
        )
    }

    pub(super) fn resolve_name_helper<'name_arg>(
        &self,
        mut location: Option<Id<Node>>,
        name: &str, /*__String*/
        meaning: SymbolFlags,
        name_not_found_message: Option<&DiagnosticMessage>,
        name_arg: Option<impl Into<ResolveNameNameArg<'name_arg>> + Clone>,
        is_use: bool,
        exclude_globals: bool,
        mut lookup: impl FnMut(
            &SymbolTable,
            &str, /*__String*/
            SymbolFlags,
        ) -> io::Result<Option<Id<Symbol>>>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let original_location = location;
        let mut result: Option<Id<Symbol>> = None;
        let mut last_location: Option<Id<Node>> = None;
        let mut last_self_reference_location: Option<Id<Node>> = None;
        let mut property_with_invalid_initializer: Option<Id<Node>> = None;
        let mut associated_declaration_for_containing_initializer_or_binding_name: Option<
            Id<Node /*ParameterDeclaration | BindingElement*/>,
        > = None;
        let mut within_deferred_context = false;
        let error_location = location;
        let mut grandparent: Id<Node>;
        let mut is_in_external_module = false;

        while let Some(mut location_unwrapped) = location {
            {
                let location_maybe_locals = location_unwrapped.ref_(self).maybe_locals();
                if let Some(location_locals) = location_maybe_locals.as_ref() {
                    if !self.is_global_source_file(location_unwrapped) {
                        result = lookup(&(**location_locals).borrow(), name, meaning)?;
                        if let Some(result_unwrapped) = result {
                            let mut use_result = true;
                            if is_function_like(Some(&location_unwrapped.ref_(self)))
                                && last_location.is_some()
                                && last_location !=
                                    location_unwrapped
                                        .ref_(self).maybe_as_function_like_declaration()
                                        .and_then(|location_unwrapped| {
                                            location_unwrapped.maybe_body()
                                        })
                            {
                                let last_location_unwrapped = last_location.unwrap();
                                if meaning & result_unwrapped.ref_(self).flags() & SymbolFlags::Type
                                    != SymbolFlags::None
                                    && last_location_unwrapped.ref_(self).kind() != SyntaxKind::JSDocComment
                                {
                                    use_result = if result_unwrapped
                                        .ref_(self)
                                        .flags()
                                        .intersects(SymbolFlags::TypeParameter)
                                    {
                                        last_location ==
                                            location_unwrapped
                                                .ref_(self).maybe_as_has_type()
                                                .and_then(|location_unwrapped| {
                                                    location_unwrapped.maybe_type()
                                                })
                                        || matches!(
                                            last_location_unwrapped.ref_(self).kind(),
                                            SyntaxKind::Parameter | SyntaxKind::TypeParameter
                                        )
                                    } else {
                                        false
                                    };
                                }
                                if meaning
                                    & result_unwrapped.ref_(self).flags()
                                    & SymbolFlags::Variable
                                    != SymbolFlags::None
                                {
                                    if self.use_outer_variable_scope_in_parameter(
                                        result_unwrapped,
                                        location_unwrapped,
                                        last_location_unwrapped,
                                    ) {
                                        use_result = false;
                                    } else if result_unwrapped
                                        .ref_(self)
                                        .flags()
                                        .intersects(SymbolFlags::FunctionScopedVariable)
                                    {
                                        use_result = last_location_unwrapped.ref_(self).kind()
                                            == SyntaxKind::Parameter
                                            || last_location ==
                                                location_unwrapped
                                                    .ref_(self).as_has_type()
                                                    .maybe_type()
                                            && find_ancestor(
                                                result_unwrapped
                                                    .ref_(self)
                                                    .maybe_value_declaration(),
                                                |node: Id<Node>| is_parameter(&node.ref_(self)),
                                                self,
                                            )
                                            .is_some()
                                    }
                                }
                            } else if location_unwrapped.ref_(self).kind() == SyntaxKind::ConditionalType {
                                use_result = 
                                    last_location == Some(location_unwrapped.ref_(self).as_conditional_type_node().true_type);
                            }

                            if use_result {
                                break;
                            } else {
                                result = None;
                            }
                        }
                    }
                }
            }
            within_deferred_context = within_deferred_context
                || self.get_is_deferred_context(location_unwrapped, last_location);
            match location_unwrapped.ref_(self).kind() {
                SyntaxKind::SourceFile | SyntaxKind::ModuleDeclaration => {
                    if !(location_unwrapped.ref_(self).kind() == SyntaxKind::SourceFile
                        && !is_external_or_common_js_module(&location_unwrapped.ref_(self)))
                    {
                        if location_unwrapped.ref_(self).kind() == SyntaxKind::SourceFile {
                            is_in_external_module = true;
                        }
                        let module_exports: Gc<GcCell<SymbolTable>> = self
                            .get_symbol_of_node(location_unwrapped)?
                            .and_then(|symbol| symbol.ref_(self).maybe_exports().clone())
                            .unwrap_or_else(|| self.empty_symbols());
                        let module_exports = (*module_exports).borrow();
                        let mut should_skip_rest_of_match_arm = false;
                        if location_unwrapped.ref_(self).kind() == SyntaxKind::SourceFile
                            || (is_module_declaration(&location_unwrapped.ref_(self))
                                && location_unwrapped.ref_(self).flags().intersects(NodeFlags::Ambient)
                                && !is_global_scope_augmentation(&location_unwrapped.ref_(self)))
                        {
                            result = module_exports.get(InternalSymbolName::Default).cloned();
                            if let Some(result_unwrapped) = result {
                                let local_symbol = get_local_symbol_for_export_default(
                                    result_unwrapped,
                                    self,
                                );
                                if local_symbol.is_some()
                                    && result_unwrapped.ref_(self).flags().intersects(meaning)
                                    && local_symbol.unwrap().ref_(self).escaped_name() == name
                                {
                                    break;
                                }
                                result = None;
                            }

                            let module_export = module_exports.get(name);
                            if matches!(
                                module_export,
                                Some(&module_export) if module_export.ref_(self).flags() == SymbolFlags::Alias &&
                                    (get_declaration_of_kind(module_export, SyntaxKind::ExportSpecifier, self).is_some()
                                     || get_declaration_of_kind(module_export, SyntaxKind::NamespaceExport, self).is_some())
                            ) {
                                should_skip_rest_of_match_arm = true;
                            }
                        }

                        if !should_skip_rest_of_match_arm {
                            if name != InternalSymbolName::Default {
                                result = lookup(
                                    &module_exports,
                                    name,
                                    meaning & SymbolFlags::ModuleMember,
                                )?;
                                if let Some(result_unwrapped) = result {
                                    if is_source_file(&location_unwrapped.ref_(self))
                                        && location_unwrapped
                                            .ref_(self).as_source_file()
                                            .maybe_common_js_module_indicator()
                                            .is_some()
                                        && !some(
                                            result_unwrapped
                                                .ref_(self)
                                                .maybe_declarations()
                                                .as_deref(),
                                            Some(|node: &Id<Node>| is_jsdoc_type_alias(&node.ref_(self))),
                                        )
                                    {
                                        result = None;
                                    } else {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
                SyntaxKind::EnumDeclaration => {
                    result = lookup(
                        &(*self
                            .get_symbol_of_node(location_unwrapped)?
                            .and_then(|symbol| symbol.ref_(self).maybe_exports().clone())
                            .unwrap_or_else(|| self.empty_symbols()))
                        .borrow(),
                        name,
                        meaning & SymbolFlags::EnumMember,
                    )?;
                    if result.is_some() {
                        break;
                    }
                }
                SyntaxKind::PropertyDeclaration => {
                    if !is_static(location_unwrapped, self) {
                        let ctor = self.find_constructor_declaration(location_unwrapped.ref_(self).parent());
                        if let Some(ctor) = ctor {
                            if let Some(ctor_locals) = ctor.ref_(self).maybe_locals().as_ref() {
                                if lookup(
                                    &(**ctor_locals).borrow(),
                                    name,
                                    meaning & SymbolFlags::Value,
                                )?
                                .is_some()
                                {
                                    property_with_invalid_initializer =
                                        Some(location_unwrapped.clone());
                                }
                            }
                        }
                    }
                }
                SyntaxKind::ClassDeclaration
                | SyntaxKind::ClassExpression
                | SyntaxKind::InterfaceDeclaration => {
                    result = lookup(
                        &*(*self
                            .get_symbol_of_node(location_unwrapped)?
                            .unwrap()
                            .ref_(self)
                            .maybe_members()
                            .clone()
                            .unwrap_or_else(|| self.empty_symbols()))
                        .borrow(),
                        name,
                        meaning & SymbolFlags::Type,
                    )?;
                    let mut should_skip_rest_of_match_arm = false;
                    if let Some(result_unwrapped) = result {
                        if !self.is_type_parameter_symbol_declared_in_container(
                            result_unwrapped,
                            location_unwrapped,
                        ) {
                            result = None;
                            should_skip_rest_of_match_arm = true;
                        }
                        if matches!(last_location, Some(last_location) if is_static(last_location, self))
                        {
                            self.error(
                                error_location,
                                &Diagnostics::Static_members_cannot_reference_class_type_parameters,
                                None,
                            );
                            return Ok(None);
                        }
                        if !should_skip_rest_of_match_arm {
                            break;
                        }
                    }
                    if !should_skip_rest_of_match_arm {
                        if location_unwrapped.ref_(self).kind() == SyntaxKind::ClassExpression
                            && meaning.intersects(SymbolFlags::Class)
                        {
                            let class_name = location_unwrapped.ref_(self).as_class_expression().maybe_name();
                            if matches!(class_name, Some(class_name) if name == &class_name.ref_(self).as_identifier().escaped_text)
                            {
                                result = Some(location_unwrapped.ref_(self).symbol());
                                break;
                            }
                        }
                    }
                }
                SyntaxKind::ExpressionWithTypeArguments => {
                    if last_location ==
                        Some(
                            location_unwrapped
                                .ref_(self).as_expression_with_type_arguments()
                                .expression,
                        )
                    && matches!(
                        location_unwrapped.ref_(self).parent().ref_(self).maybe_as_heritage_clause(),
                        Some(location_parent) if location_parent.token == SyntaxKind::ExtendsKeyword
                    ) {
                        let container = location_unwrapped.ref_(self).parent().ref_(self).parent();
                        if is_class_like(&container.ref_(self)) {
                            result = lookup(
                                &(*self
                                    .get_symbol_of_node(container)?
                                    .unwrap()
                                    .ref_(self)
                                    .members())
                                .borrow(),
                                name,
                                meaning & SymbolFlags::Type,
                            )?;
                            if result.is_some() {
                                if name_not_found_message.is_some() {
                                    self.error(error_location, &Diagnostics::Base_class_expressions_cannot_reference_class_type_parameters, None);
                                }
                                return Ok(None);
                            }
                        }
                    }
                }
                SyntaxKind::ComputedPropertyName => {
                    grandparent = location_unwrapped.ref_(self).parent().ref_(self).parent();
                    if is_class_like(&grandparent.ref_(self))
                        || grandparent.ref_(self).kind() == SyntaxKind::InterfaceDeclaration
                    {
                        result = lookup(
                            &(*self
                                .get_symbol_of_node(grandparent)?
                                .unwrap()
                                .ref_(self)
                                .members())
                            .borrow(),
                            name,
                            meaning & SymbolFlags::Type,
                        )?;
                        if result.is_some() {
                            self.error(error_location, &Diagnostics::A_computed_property_name_cannot_reference_a_type_parameter_from_its_containing_type, None);
                            return Ok(None);
                        }
                    }
                }
                SyntaxKind::ArrowFunction
                | SyntaxKind::MethodDeclaration
                | SyntaxKind::Constructor
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor
                | SyntaxKind::FunctionDeclaration => {
                    if !(location_unwrapped.ref_(self).kind() == SyntaxKind::ArrowFunction
                        && get_emit_script_target(&self.compiler_options.ref_(self)) >= ScriptTarget::ES2015)
                    {
                        if meaning.intersects(SymbolFlags::Variable) && name == "arguments" {
                            result = Some(self.arguments_symbol());
                            break;
                        }
                    }
                }
                SyntaxKind::FunctionExpression => {
                    if meaning.intersects(SymbolFlags::Variable) && name == "arguments" {
                        result = Some(self.arguments_symbol());
                        break;
                    }

                    if meaning.intersects(SymbolFlags::Function) {
                        let function_name =
                            location_unwrapped.ref_(self).as_function_expression().maybe_name();
                        if matches!(function_name, Some(function_name) if name == &function_name.ref_(self).as_identifier().escaped_text)
                        {
                            result = Some(location_unwrapped.ref_(self).symbol());
                            break;
                        }
                    }
                }
                SyntaxKind::Decorator => {
                    if matches!(
                        location_unwrapped.ref_(self).maybe_parent(),
                        Some(parent) if parent.ref_(self).kind() == SyntaxKind::Parameter
                    ) {
                        location = location_unwrapped.ref_(self).maybe_parent();
                        location_unwrapped = location.unwrap();
                    }
                    if matches!(
                        location_unwrapped.ref_(self).maybe_parent(),
                        Some(parent) if is_class_element(&parent.ref_(self)) || parent.ref_(self).kind() == SyntaxKind::ClassDeclaration
                    ) {
                        location = location_unwrapped.ref_(self).maybe_parent();
                        location_unwrapped = location.unwrap();
                    }
                }
                SyntaxKind::JSDocTypedefTag
                | SyntaxKind::JSDocCallbackTag
                | SyntaxKind::JSDocEnumTag => {
                    let root = get_jsdoc_root(location_unwrapped, self);
                    if let Some(root) = root {
                        location = root.ref_(self).maybe_parent();
                        location_unwrapped = location.unwrap();
                    }
                }
                SyntaxKind::Parameter => {
                    if matches!(last_location, Some(last_location) if {
                        let location_unwrapped_ref = location_unwrapped.ref_(self);
                        let location_as_parameter_declaration = location_unwrapped_ref.as_parameter_declaration();
                        Some(last_location) == location_as_parameter_declaration.maybe_initializer() ||
                        Some(last_location) == location_as_parameter_declaration.maybe_name() && is_binding_pattern(Some(&last_location.ref_(self)))
                    }) {
                        if associated_declaration_for_containing_initializer_or_binding_name
                            .is_none()
                        {
                            associated_declaration_for_containing_initializer_or_binding_name =
                                Some(location_unwrapped.clone());
                        }
                    }
                }
                SyntaxKind::BindingElement => {
                    if matches!(last_location, Some(last_location) if {
                        let location_unwrapped_ref = location_unwrapped.ref_(self);
                        let location_as_binding_element = location_unwrapped_ref.as_binding_element();
                        Some(last_location) == location_as_binding_element.maybe_initializer() ||
                        Some(last_location) == location_as_binding_element.maybe_name() && is_binding_pattern(Some(&last_location.ref_(self)))
                    }) {
                        if is_parameter_declaration(location_unwrapped, self)
                            && associated_declaration_for_containing_initializer_or_binding_name
                                .is_none()
                        {
                            associated_declaration_for_containing_initializer_or_binding_name =
                                Some(location_unwrapped.clone());
                        }
                    }
                }
                SyntaxKind::InferType => {
                    if meaning.intersects(SymbolFlags::TypeParameter) {
                        let location_type_parameter =
                            location_unwrapped.ref_(self).as_infer_type_node().type_parameter;
                        let parameter_name = location_type_parameter
                            .ref_(self).as_type_parameter_declaration()
                            .maybe_name();
                        if matches!(
                            parameter_name,
                            Some(parameter_name) if name == &parameter_name.ref_(self).as_identifier().escaped_text
                        ) {
                            result = Some(location_type_parameter.ref_(self).symbol());
                            break;
                        }
                    }
                }
                _ => (),
            }
            if self.is_self_reference_location(location_unwrapped) {
                last_self_reference_location = Some(location_unwrapped);
            }
            last_location = Some(location_unwrapped);
            location = if is_jsdoc_template_tag(&location_unwrapped.ref_(self)) {
                get_effective_container_for_jsdoc_template_tag(location_unwrapped, self)
                    .or_else(|| location_unwrapped.ref_(self).maybe_parent())
            } else {
                location_unwrapped.ref_(self).maybe_parent()
            };
        }

        if is_use {
            if let Some(result) = result {
                if match last_self_reference_location {
                    None => true,
                    Some(last_self_reference_location) => {
                        result != last_self_reference_location.ref_(self).symbol()
                    }
                } {
                    result.ref_(self).set_is_referenced(Some(
                        result
                            .ref_(self)
                            .maybe_is_referenced()
                            .unwrap_or(SymbolFlags::None)
                            | meaning,
                    ));
                }
            }
        }

        if result.is_none() {
            if let Some(last_location) = last_location {
                Debug_.assert(last_location.ref_(self).kind() == SyntaxKind::SourceFile, None);
                if last_location
                    .ref_(self).as_source_file()
                    .maybe_common_js_module_indicator()
                    .is_some()
                    && name == "exports"
                    && meaning.intersects(last_location.ref_(self).symbol().ref_(self).flags())
                {
                    return Ok(Some(last_location.ref_(self).symbol()));
                }
            }

            if !exclude_globals {
                result = lookup(&self.globals(), name, meaning)?;
            }
        }
        if result.is_none() {
            if let Some(original_location) = original_location {
                if is_in_js_file(Some(&original_location.ref_(self))) {
                    if let Some(original_location_parent) = original_location.ref_(self).maybe_parent() {
                        if is_require_call(original_location_parent, false, self) {
                            return Ok(Some(self.require_symbol()));
                        }
                    }
                }
            }
        }
        if result.is_none() {
            if let Some(name_not_found_message) = name_not_found_message {
                if error_location.is_none() || {
                    let error_location = error_location.unwrap();
                    !self.check_and_report_error_for_missing_prefix(
                        error_location,
                        name,
                        name_arg.clone().unwrap().into(),
                    )? && !self.check_and_report_error_for_extending_interface(error_location)?
                        && !self.check_and_report_error_for_using_type_as_namespace(
                            error_location,
                            name,
                            meaning,
                        )?
                        && !self.check_and_report_error_for_exporting_primitive_type(
                            error_location,
                            name,
                        )
                        && !self.check_and_report_error_for_using_type_as_value(
                            error_location,
                            name,
                            meaning,
                        )?
                        && !self.check_and_report_error_for_using_namespace_module_as_value(
                            error_location,
                            name,
                            meaning,
                        )?
                        && !self.check_and_report_error_for_using_value_as_type(
                            error_location,
                            name,
                            meaning,
                        )?
                } {
                    let mut suggestion: Option<Id<Symbol>> = None;
                    if self.suggestion_count() < self.maximum_suggestion_count {
                        suggestion = self.get_suggested_symbol_for_nonexistent_symbol_(
                            original_location,
                            name,
                            meaning,
                        )?;
                        let is_global_scope_augmentation_declaration = matches!(
                            suggestion.and_then(|suggestion| suggestion.ref_(self).maybe_value_declaration()),
                            Some(value_declaration) if is_ambient_module(value_declaration, self) && is_global_scope_augmentation(&value_declaration.ref_(self))
                        );
                        if is_global_scope_augmentation_declaration {
                            suggestion = None;
                        }
                        if let Some(suggestion) = suggestion {
                            let suggestion_name = self.symbol_to_string_(
                                suggestion,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                                None,
                            )?;
                            let is_unchecked_js = self.is_unchecked_js_suggestion(
                                original_location,
                                Some(suggestion),
                                false,
                            );
                            let message = if meaning == SymbolFlags::Namespace
                                || matches!(
                                    name_arg.as_ref(),
                                    Some(name_arg) if matches!(
                                        name_arg.clone().into(),
                                        ResolveNameNameArg::Node(name_arg) if node_is_synthesized(&*name_arg.ref_(self))
                                    )
                                )
                            {
                                &*Diagnostics::Cannot_find_namespace_0_Did_you_mean_1
                            } else if is_unchecked_js {
                                &*Diagnostics::Could_not_find_name_0_Did_you_mean_1
                            } else {
                                &*Diagnostics::Cannot_find_name_0_Did_you_mean_1
                            };
                            let diagnostic = self.create_error(
                                error_location,
                                message,
                                Some(vec![
                                    self.diagnostic_name(name_arg.clone().unwrap().into())
                                        .into_owned(),
                                    suggestion_name.clone(),
                                ]),
                            );
                            self.add_error_or_suggestion(!is_unchecked_js, diagnostic.clone());
                            if let Some(suggestion_value_declaration) =
                                suggestion.ref_(self).maybe_value_declaration()
                            {
                                add_related_info(
                                    &diagnostic.ref_(self),
                                    vec![self.alloc_diagnostic_related_information(create_diagnostic_for_node(
                                        suggestion_value_declaration,
                                        &Diagnostics::_0_is_declared_here,
                                        Some(vec![suggestion_name]),
                                        self,
                                    )
                                    .into())],
                                );
                            }
                        }
                    }
                    if suggestion.is_none() {
                        if let Some(name_arg) = name_arg {
                            let name_arg = name_arg.into();
                            let lib =
                                self.get_suggested_lib_for_non_existent_name(name_arg.clone());
                            if let Some(lib) = lib {
                                self.error(
                                    error_location,
                                    name_not_found_message,
                                    Some(vec![self.diagnostic_name(name_arg).into_owned(), lib]),
                                );
                            } else {
                                self.error(
                                    error_location,
                                    name_not_found_message,
                                    Some(vec![self.diagnostic_name(name_arg).into_owned()]),
                                );
                            }
                        }
                    }
                    self.increment_suggestion_count();
                }
            }
            return Ok(None);
        }
        let result = result.unwrap();

        if name_not_found_message.is_some() {
            if let Some(property_with_invalid_initializer) =
                property_with_invalid_initializer.as_ref()
            {
                if !(get_emit_script_target(&self.compiler_options.ref_(self)) == ScriptTarget::ESNext
                    && self.use_define_for_class_fields)
                {
                    let property_name = property_with_invalid_initializer
                        .ref_(self).as_named_declaration()
                        .name();
                    self.error(
                        error_location,
                        &Diagnostics::Initializer_of_instance_member_variable_0_cannot_reference_identifier_1_declared_in_the_constructor,
                        Some(vec![declaration_name_to_string(Some(property_name), self).into_owned(), self.diagnostic_name(name_arg.unwrap().into()).into_owned()])
                    );
                    return Ok(None);
                }
            }

            if let Some(error_location) = error_location {
                if meaning.intersects(SymbolFlags::BlockScopedVariable)
                    || (meaning.intersects(SymbolFlags::Class)
                        || meaning.intersects(SymbolFlags::Enum))
                        && meaning & SymbolFlags::Value == SymbolFlags::Value
                {
                    let export_or_local_symbol = self
                        .get_export_symbol_of_value_symbol_if_exported(Some(result))
                        .unwrap();
                    if export_or_local_symbol
                        .ref_(self)
                        .flags()
                        .intersects(SymbolFlags::BlockScopedVariable)
                        || export_or_local_symbol
                            .ref_(self)
                            .flags()
                            .intersects(SymbolFlags::Class)
                        || export_or_local_symbol
                            .ref_(self)
                            .flags()
                            .intersects(SymbolFlags::Enum)
                    {
                        self.check_resolved_block_scoped_variable(
                            export_or_local_symbol,
                            error_location,
                        )?;
                    }
                }
            }

            if
            /*result &&*/
            is_in_external_module
                && meaning & SymbolFlags::Value == SymbolFlags::Value
                && !original_location
                    .unwrap()
                    .ref_(self).flags()
                    .intersects(NodeFlags::JSDoc)
            {
                let merged = self.get_merged_symbol(Some(result)).unwrap();
                if length(merged.ref_(self).maybe_declarations().as_deref()) > 0
                    && every(
                        merged.ref_(self).maybe_declarations().as_deref().unwrap(),
                        |d, _| {
                            is_namespace_export_declaration(&d.ref_(self))
                                || is_source_file(&d.ref_(self))
                                    && d.ref_(self).symbol().ref_(self).maybe_global_exports().is_some()
                        },
                    )
                {
                    self.error_or_suggestion(
                        !matches!(self.compiler_options.ref_(self).allow_umd_global_access, Some(true)),
                        error_location.unwrap(),
                        &*Diagnostics::_0_refers_to_a_UMD_global_but_the_current_file_is_a_module_Consider_adding_an_import_instead,
                        Some(vec![unescape_leading_underscores(name).to_owned()])
                    );
                }
            }

            if
            /*result &&*/
            let Some(associated_declaration_for_containing_initializer_or_binding_name) =
                associated_declaration_for_containing_initializer_or_binding_name
            {
                if !within_deferred_context && meaning & SymbolFlags::Value == SymbolFlags::Value {
                    let candidate = self
                        .get_merged_symbol(Some(self.get_late_bound_symbol(result)?))
                        .unwrap();
                    let root = get_root_declaration(
                        associated_declaration_for_containing_initializer_or_binding_name,
                        self,
                    );
                    if Some(candidate)
                        == self.get_symbol_of_node(
                            associated_declaration_for_containing_initializer_or_binding_name,
                        )?
                    {
                        self.error(
                            error_location,
                            &Diagnostics::Parameter_0_cannot_reference_itself,
                            Some(vec![declaration_name_to_string(Some(
                                associated_declaration_for_containing_initializer_or_binding_name
                                    .ref_(self).as_named_declaration()
                                    .name(),
                            ), self)
                            .into_owned()]),
                        );
                    } else if matches!(
                        candidate.ref_(self).maybe_value_declaration(),
                        Some(value_declaration) if value_declaration.ref_(self).pos() > associated_declaration_for_containing_initializer_or_binding_name.ref_(self).pos() &&
                            matches!(
                                root.ref_(self).parent().ref_(self).maybe_locals().as_ref(),
                                Some(locals) if lookup(&(**locals).borrow(), candidate.ref_(self).escaped_name(), meaning)? == Some(candidate)
                            )
                    ) {
                        self.error(
                            error_location,
                            &Diagnostics::Parameter_0_cannot_reference_identifier_1_declared_after_it,
                            Some(vec![declaration_name_to_string(
                                Some(associated_declaration_for_containing_initializer_or_binding_name
                                    .ref_(self).as_named_declaration()
                                    .name()),
                                self,
                            ).into_owned(), declaration_name_to_string(error_location, self).into_owned()]),
                        );
                    }
                }
            }
            if
            /*result &&*/
            let Some(error_location) = error_location {
                if meaning.intersects(SymbolFlags::Value)
                    && result.ref_(self).flags().intersects(SymbolFlags::Alias)
                {
                    self.check_symbol_usage_in_expression_context(result, name, error_location);
                }
            }
        }
        Ok(Some(result))
    }
}

pub enum DiagnosticMessageOrDiagnosticMessageChain {
    DiagnosticMessage(&'static DiagnosticMessage),
    DiagnosticMessageChain(DiagnosticMessageChain),
}

impl DiagnosticMessageOrDiagnosticMessageChain {
    pub fn category(&self) -> DiagnosticCategory {
        match self {
            Self::DiagnosticMessage(value) => value.category,
            Self::DiagnosticMessageChain(value) => value.category,
        }
    }

    pub fn code(&self) -> u32 {
        match self {
            Self::DiagnosticMessage(value) => value.code,
            Self::DiagnosticMessageChain(value) => value.code,
        }
    }
}

impl From<&'static DiagnosticMessage> for DiagnosticMessageOrDiagnosticMessageChain {
    fn from(value: &'static DiagnosticMessage) -> Self {
        Self::DiagnosticMessage(value)
    }
}

impl From<DiagnosticMessageChain> for DiagnosticMessageOrDiagnosticMessageChain {
    fn from(value: DiagnosticMessageChain) -> Self {
        Self::DiagnosticMessageChain(value)
    }
}

#[derive(Clone)]
pub(super) enum ResolveNameNameArg<'str> {
    Node(Id<Node>),
    Str(&'str str),
}

impl<'str> From<Id<Node>> for ResolveNameNameArg<'str> {
    fn from(value: Id<Node>) -> Self {
        ResolveNameNameArg::Node(value)
    }
}

impl<'str> From<&'str str> for ResolveNameNameArg<'str> {
    fn from(value: &'str str) -> Self {
        ResolveNameNameArg::Str(value)
    }
}
