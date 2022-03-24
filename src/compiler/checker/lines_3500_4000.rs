#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::MembersOrExportsResolutionKind;
use crate::{
    add_range, chain_diagnostic_messages, create_diagnostic_for_node, create_symbol_table,
    get_declaration_of_kind, get_es_module_interop, get_namespace_declaration_node,
    get_object_flags, get_text_of_node, get_types_package_name, is_external_module_name_relative,
    is_import_call, is_import_declaration, mangle_scoped_package_name, push_if_unique_rc,
    unescape_leading_underscores, Diagnostics, InternalSymbolName, ModuleKind, Node, NodeInterface,
    ObjectFlags, ResolvedModuleFull, SignatureKind, Symbol, SymbolFlags, SymbolInterface,
    SymbolTable, SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface,
    UnderscoreEscapedMap, __String,
};

impl TypeChecker {
    pub(super) fn error_on_implicit_any_module(
        &self,
        is_error: bool,
        error_node: &Node,
        resolved_module: &ResolvedModuleFull,
        module_reference: &str,
    ) {
        let package_id = &resolved_module.package_id;
        let resolved_file_name = &resolved_module.resolved_file_name;
        let error_info = if !is_external_module_name_relative(module_reference)
            && package_id.is_some()
        {
            let package_id = package_id.as_ref().unwrap();
            Some(if self.types_package_exists(&package_id.name) {
                chain_diagnostic_messages(
                    None,
                    &Diagnostics::If_the_0_package_actually_exposes_this_module_consider_sending_a_pull_request_to_amend_https_Colon_Slash_Slashgithub_com_SlashDefinitelyTyped_SlashDefinitelyTyped_Slashtree_Slashmaster_Slashtypes_Slash_1,
                    Some(vec![
                        package_id.name.clone(),
                        mangle_scoped_package_name(&package_id.name),
                    ])
                )
            } else {
                if self.package_bundles_types(&package_id.name) {
                    chain_diagnostic_messages(
                        None,
                        &Diagnostics::If_the_0_package_actually_exposes_this_module_try_adding_a_new_declaration_d_ts_file_containing_declare_module_1,
                        Some(vec![
                            package_id.name.clone(),
                            module_reference.to_owned(),
                        ])
                    )
                } else {
                    chain_diagnostic_messages(
                        None,
                        &Diagnostics::Try_npm_i_save_dev_types_Slash_1_if_it_exists_or_add_a_new_declaration_d_ts_file_containing_declare_module_0,
                        Some(vec![
                            module_reference.to_owned(),
                            mangle_scoped_package_name(&package_id.name)
                        ])
                    )
                }
            })
        } else {
            None
        };
        self.error_or_suggestion(
            is_error,
            error_node,
            chain_diagnostic_messages(
                error_info,
                &Diagnostics::Could_not_find_a_declaration_file_for_module_0_1_implicitly_has_an_any_type,
                Some(vec![
                    module_reference.to_owned(),
                    resolved_file_name.clone()
                ])
            ).into(),
            None
        );
    }

    pub(super) fn types_package_exists(&self, package_name: &str) -> bool {
        self.get_packages_map()
            .contains_key(&get_types_package_name(package_name))
    }

    pub(super) fn package_bundles_types(&self, package_name: &str) -> bool {
        matches!(self.get_packages_map().get(package_name), Some(true))
    }

    pub(super) fn resolve_external_module_symbol<TModuleSymbol: Borrow<Symbol>>(
        &self,
        module_symbol: Option<TModuleSymbol>,
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let module_symbol = module_symbol?;
        let module_symbol = module_symbol.borrow();
        let module_symbol_exports = module_symbol.maybe_exports();
        let module_symbol_exports = module_symbol_exports.as_ref()?;
        let export_equals = self.resolve_symbol(
            RefCell::borrow(module_symbol_exports)
                .get(&InternalSymbolName::ExportEquals())
                .map(Clone::clone),
            dont_resolve_alias,
        );
        let exported = self.get_common_js_export_equals(
            self.get_merged_symbol(export_equals),
            &self.get_merged_symbol(Some(module_symbol)).unwrap(),
        );
        Some(
            self.get_merged_symbol(exported)
                .unwrap_or_else(|| module_symbol.symbol_wrapper()),
        )
    }

    pub(super) fn get_common_js_export_equals<TExported: Borrow<Symbol>>(
        &self,
        exported: Option<TExported>,
        module_symbol: &Symbol,
    ) -> Option<Rc<Symbol>> {
        let exported = exported?;
        let exported = exported.borrow();
        if ptr::eq(exported, &*self.unknown_symbol())
            || ptr::eq(exported, module_symbol)
            || RefCell::borrow(&module_symbol.exports()).len() == 1
            || exported.flags().intersects(SymbolFlags::Alias)
        {
            return Some(exported.symbol_wrapper());
        }
        let links = self.get_symbol_links(exported);
        if let Some(links_cjs_export_merged) = RefCell::borrow(&links)
            .cjs_export_merged
            .as_ref()
            .map(|cjs_export_merged| cjs_export_merged.clone())
        {
            return Some(links_cjs_export_merged);
        }
        let merged = if exported.flags().intersects(SymbolFlags::Transient) {
            exported.symbol_wrapper()
        } else {
            Rc::new(self.clone_symbol(exported))
        };
        merged.set_flags(merged.flags() | SymbolFlags::ValueModule);
        let mut merged_exports = merged.maybe_exports();
        if merged_exports.is_none() {
            *merged_exports = Some(Rc::new(RefCell::new(create_symbol_table(None))));
        }
        let mut merged_exports = merged_exports.as_ref().unwrap().borrow_mut();
        for (name, s) in &*RefCell::borrow(&module_symbol.exports()) {
            if name == &InternalSymbolName::ExportEquals() {
                continue;
            }
            let value = if merged_exports.contains_key(name) {
                self.merge_symbol(merged_exports.get(name).unwrap(), s, None)
            } else {
                s.symbol_wrapper()
            };
            merged_exports.insert(name.clone(), value);
        }
        self.get_symbol_links(&merged)
            .borrow_mut()
            .cjs_export_merged = Some(merged.clone());
        links.borrow_mut().cjs_export_merged = Some(merged.clone());
        Some(merged.clone())
    }

    pub(super) fn resolve_es_module_symbol<TModuleSymbol: Borrow<Symbol>>(
        &self,
        module_symbol: Option<TModuleSymbol>,
        referencing_location: &Node,
        dont_resolve_alias: bool,
        suppress_interop_error: bool,
    ) -> Option<Rc<Symbol>> {
        let module_symbol =
            module_symbol.map(|module_symbol| module_symbol.borrow().symbol_wrapper());
        let symbol = self
            .resolve_external_module_symbol(module_symbol.as_deref(), Some(dont_resolve_alias))?;

        if !dont_resolve_alias {
            if !suppress_interop_error
                && !symbol
                    .flags()
                    .intersects(SymbolFlags::Module | SymbolFlags::Variable)
                && get_declaration_of_kind(&symbol, SyntaxKind::SourceFile).is_none()
            {
                let compiler_option_name = if self.module_kind >= ModuleKind::ES2015 {
                    "allowSyntheticDefaultImports"
                } else {
                    "esModuleInterop"
                };

                self.error(
                    Some(referencing_location),
                    &Diagnostics::This_module_can_only_be_referenced_with_ECMAScript_imports_Slashexports_by_turning_on_the_0_flag_and_referencing_its_default_export,
                    Some(vec![compiler_option_name.to_owned()])
                );

                return Some(symbol);
            }

            let reference_parent = referencing_location.parent();
            if is_import_declaration(&reference_parent)
                && get_namespace_declaration_node(&reference_parent).is_some()
                || is_import_call(&reference_parent)
            {
                let reference = if is_import_call(&reference_parent) {
                    &reference_parent.as_call_expression().arguments[0]
                } else {
                    &reference_parent.as_import_declaration().module_specifier
                };
                let type_ = self.get_type_of_symbol(&symbol);
                let default_only_type = self.get_type_with_synthetic_default_only(
                    &type_,
                    &symbol,
                    module_symbol.as_ref().unwrap(),
                    reference,
                );
                if let Some(default_only_type) = default_only_type {
                    return Some(self.clone_type_as_module_type(
                        &symbol,
                        &default_only_type,
                        &reference_parent,
                    ));
                }

                if matches!(get_es_module_interop(&self.compiler_options), Some(true)) {
                    let mut sigs =
                        self.get_signatures_of_structured_type(&type_, SignatureKind::Call);
                    if
                    /* !sigs ||*/
                    sigs.is_empty() {
                        sigs = self
                            .get_signatures_of_structured_type(&type_, SignatureKind::Construct);
                    }
                    if
                    /*sigs &&*/
                    !sigs.is_empty()
                        || self
                            .get_property_of_type_(&type_, &InternalSymbolName::Default(), None)
                            .is_some()
                    {
                        let module_type = self.get_type_with_synthetic_default_import_type(
                            &type_,
                            &symbol,
                            module_symbol.as_ref().unwrap(),
                            reference,
                        );
                        return Some(self.clone_type_as_module_type(
                            &symbol,
                            &module_type,
                            &reference_parent,
                        ));
                    }
                }
            }
        }
        Some(symbol)
    }

    pub(super) fn clone_type_as_module_type(
        &self,
        symbol: &Symbol,
        module_type: &Type,
        reference_parent: &Node, /*ImportDeclaration | ImportCall*/
    ) -> Rc<Symbol> {
        let result: Rc<Symbol> = self
            .create_symbol(symbol.flags(), symbol.escaped_name().clone(), None)
            .into();
        result.set_declarations(
            if let Some(symbol_declarations) = symbol.maybe_declarations().as_ref() {
                symbol_declarations.clone()
            } else {
                vec![]
            },
        );
        result.set_parent(symbol.maybe_parent());
        let result_links = result.as_transient_symbol().symbol_links();
        let mut result_links = result_links.borrow_mut();
        result_links.target = Some(symbol.symbol_wrapper());
        result_links.originating_import = Some(reference_parent.node_wrapper());
        if let Some(symbol_value_declaration) = symbol.maybe_value_declaration() {
            result.set_value_declaration(symbol_value_declaration);
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
        let resolved_module_type = self.resolve_structured_type_members(module_type);
        let resolved_module_type_as_resolved_type = resolved_module_type.as_resolved_type();
        result_links.type_ = Some(
            self.create_anonymous_type(
                Some(result.clone()),
                resolved_module_type_as_resolved_type.members(),
                vec![],
                vec![],
                resolved_module_type_as_resolved_type.index_infos().clone(),
            )
            .into(),
        );
        result
    }

    pub(super) fn has_export_assignment_symbol(&self, module_symbol: &Symbol) -> bool {
        RefCell::borrow(&module_symbol.exports())
            .get(&InternalSymbolName::ExportEquals())
            .is_some()
    }

    pub(super) fn get_exports_of_module_as_array(&self, module_symbol: &Symbol) -> Vec<Rc<Symbol>> {
        self.symbols_to_array(&RefCell::borrow(
            &self.get_exports_of_module_(module_symbol),
        ))
    }

    pub(super) fn get_exports_and_properties_of_module(
        &self,
        module_symbol: &Symbol,
    ) -> Vec<Rc<Symbol>> {
        let mut exports = self.get_exports_of_module_as_array(module_symbol);
        let export_equals = self
            .resolve_external_module_symbol(Some(module_symbol), None)
            .unwrap();
        if !ptr::eq(&*export_equals, module_symbol) {
            let type_ = self.get_type_of_symbol(&export_equals);
            if self.should_treat_properties_of_external_module_as_exports(&type_) {
                add_range(
                    &mut exports,
                    Some(&*self.get_properties_of_type(&type_)),
                    None,
                    None,
                );
            }
        }
        exports
    }

    pub(super) fn for_each_export_and_property_of_module<TCallback: FnMut(&Symbol, &__String)>(
        &self,
        module_symbol: &Symbol,
        mut cb: TCallback,
    ) {
        let exports = self.get_exports_of_module_(module_symbol);
        for (key, symbol) in &*RefCell::borrow(&exports) {
            if !self.is_reserved_member_name(key) {
                cb(symbol, key);
            }
        }
        let export_equals = self
            .resolve_external_module_symbol(Some(module_symbol), None)
            .unwrap();
        if !ptr::eq(&*export_equals, module_symbol) {
            let type_ = self.get_type_of_symbol(&export_equals);
            if self.should_treat_properties_of_external_module_as_exports(&type_) {
                self.for_each_property_of_type(&type_, |symbol, escaped_name| {
                    cb(symbol, escaped_name)
                });
            }
        }
    }

    pub(super) fn try_get_member_in_module_exports_(
        &self,
        member_name: &__String,
        module_symbol: &Symbol,
    ) -> Option<Rc<Symbol>> {
        let symbol_table = self.get_exports_of_module_(module_symbol);
        // if (symbolTable) {
        let ret = RefCell::borrow(&symbol_table)
            .get(member_name)
            .map(Clone::clone);
        ret
        // }
    }

    pub(super) fn try_get_member_in_module_exports_and_properties_(
        &self,
        member_name: &__String,
        module_symbol: &Symbol,
    ) -> Option<Rc<Symbol>> {
        let symbol = self.try_get_member_in_module_exports_(member_name, module_symbol);
        if symbol.is_some() {
            return symbol;
        }

        let export_equals = self
            .resolve_external_module_symbol(Some(module_symbol), None)
            .unwrap();
        if ptr::eq(&*export_equals, module_symbol) {
            return None;
        }

        let type_ = self.get_type_of_symbol(&export_equals);
        if self.should_treat_properties_of_external_module_as_exports(&type_) {
            self.get_property_of_type_(&type_, member_name, None)
        } else {
            None
        }
    }

    pub(super) fn should_treat_properties_of_external_module_as_exports(
        &self,
        resolved_external_module_type: &Type,
    ) -> bool {
        !(resolved_external_module_type
            .flags()
            .intersects(TypeFlags::Primitive)
            || get_object_flags(resolved_external_module_type).intersects(ObjectFlags::Class)
            || self.is_array_type(resolved_external_module_type)
            || self.is_tuple_type(resolved_external_module_type))
    }

    pub(super) fn get_exports_of_symbol(&self, symbol: &Symbol) -> Rc<RefCell<SymbolTable>> {
        if symbol.flags().intersects(SymbolFlags::LateBindingContainer) {
            self.get_resolved_members_or_exports_of_symbol(
                symbol,
                MembersOrExportsResolutionKind::resolved_exports,
            )
        } else if symbol.flags().intersects(SymbolFlags::Module) {
            self.get_exports_of_module_(symbol)
        } else {
            symbol
                .maybe_exports()
                .as_ref()
                .map_or_else(|| self.empty_symbols(), |exports| exports.clone())
        }
    }

    pub(super) fn get_exports_of_module_(
        &self,
        module_symbol: &Symbol,
    ) -> Rc<RefCell<SymbolTable>> {
        let links = self.get_symbol_links(module_symbol);
        let resolved_exports = RefCell::borrow(&links).resolved_exports.clone();
        resolved_exports.unwrap_or_else(|| {
            let resolved_exports = self.get_exports_of_module_worker(module_symbol);
            links.borrow_mut().resolved_exports = Some(resolved_exports.clone());
            resolved_exports
        })
    }

    pub(super) fn extend_export_symbols<TSource: Borrow<SymbolTable>, TExportNode: Borrow<Node>>(
        &self,
        target: &mut SymbolTable,
        source: Option<TSource>,
        mut lookup_table: Option<&mut ExportCollisionTrackerTable>,
        export_node: Option<TExportNode>,
    ) {
        if source.is_none() {
            return;
        }
        let source = source.unwrap();
        let source = source.borrow();
        for (id, source_symbol) in source {
            if id == &InternalSymbolName::Default() {
                continue;
            }

            let target_symbol = target.get(id).map(Clone::clone);
            if target_symbol.is_none() {
                target.insert(id.clone(), source_symbol.clone());
                if let Some(lookup_table) = lookup_table.as_mut() {
                    if let Some(export_node) = export_node.as_ref() {
                        let export_node = export_node.borrow();
                        lookup_table.insert(
                            id.clone(),
                            ExportCollisionTracker {
                                specifier_text: get_text_of_node(
                                    export_node
                                        .as_export_declaration()
                                        .module_specifier
                                        .as_ref()
                                        .unwrap(),
                                    None,
                                )
                                .into_owned(),
                                exports_with_duplicate: None,
                            },
                        );
                    }
                }
            } else if let Some(lookup_table) = lookup_table.as_mut() {
                if let Some(export_node) = export_node.as_ref() {
                    let export_node = export_node.borrow();
                    if matches!(
                        target_symbol.as_ref(),
                        Some(target_symbol) if !Rc::ptr_eq(&self.resolve_symbol(Some(&**target_symbol), None).unwrap(), &self.resolve_symbol(Some(&**source_symbol), None).unwrap())
                    ) {
                        let collision_tracker = lookup_table.get_mut(id).unwrap();
                        if collision_tracker.exports_with_duplicate.is_none() {
                            collision_tracker.exports_with_duplicate =
                                Some(vec![export_node.node_wrapper()]);
                        } else {
                            collision_tracker
                                .exports_with_duplicate
                                .as_mut()
                                .unwrap()
                                .push(export_node.node_wrapper());
                        }
                    }
                }
            }
        }
    }

    pub(super) fn get_exports_of_module_worker(
        &self,
        module_symbol: &Symbol,
    ) -> Rc<RefCell<SymbolTable>> {
        let mut visited_symbols: Vec<Rc<Symbol>> = vec![];

        let module_symbol = self
            .resolve_external_module_symbol(Some(module_symbol), None)
            .unwrap();

        self.visit_get_exports_of_module_worker(&mut visited_symbols, Some(module_symbol))
            .map_or_else(
                || self.empty_symbols(),
                |symbol_table| Rc::new(RefCell::new(symbol_table)),
            )
    }

    pub(super) fn visit_get_exports_of_module_worker(
        &self,
        visited_symbols: &mut Vec<Rc<Symbol>>,
        symbol: Option<Rc<Symbol>>,
    ) -> Option<SymbolTable> {
        let symbol = symbol?;
        if !(symbol.maybe_exports().is_some() && push_if_unique_rc(visited_symbols, &symbol)) {
            return None;
        }
        let symbol_exports = symbol.maybe_exports();
        let symbol_exports = symbol_exports.as_ref().unwrap();
        let symbol_exports = RefCell::borrow(&symbol_exports);
        let mut symbols = symbol_exports.clone();
        let export_stars = symbol_exports.get(&InternalSymbolName::ExportStar());
        if let Some(export_stars) = export_stars {
            let mut nested_symbols = create_symbol_table(None);
            let mut lookup_table = ExportCollisionTrackerTable::new();
            if let Some(export_stars_declarations) = export_stars.maybe_declarations().as_ref() {
                for node in export_stars_declarations {
                    let resolved_module = self.resolve_external_module_name_(
                        node,
                        node.as_export_declaration()
                            .module_specifier
                            .as_ref()
                            .unwrap(),
                        None,
                    );
                    let exported_symbols =
                        self.visit_get_exports_of_module_worker(visited_symbols, resolved_module);
                    self.extend_export_symbols(
                        &mut nested_symbols,
                        exported_symbols.as_ref(),
                        Some(&mut lookup_table),
                        Some(&**node),
                    );
                }
            }
            for (id, export_collision_tracker) in &lookup_table {
                let exports_with_duplicate =
                    export_collision_tracker.exports_with_duplicate.as_ref();
                if id.eq_str("export=")
                    || !matches!(exports_with_duplicate, Some(exports_with_duplicate) if !exports_with_duplicate.is_empty())
                    || symbols.contains_key(id)
                {
                    continue;
                }
                let exports_with_duplicate = exports_with_duplicate.unwrap();
                for node in exports_with_duplicate {
                    self.diagnostics().add(Rc::new(create_diagnostic_for_node(
                        node,
                        &Diagnostics::Module_0_has_already_exported_a_member_named_1_Consider_explicitly_re_exporting_to_resolve_the_ambiguity,
                        Some(vec![
                            lookup_table.get(id).unwrap().specifier_text.clone(),
                            unescape_leading_underscores(id)
                        ])
                    ).into()));
                }
            }
            self.extend_export_symbols(
                &mut symbols,
                Some(&nested_symbols),
                None,
                Option::<&Node>::None,
            );
        }
        Some(symbols)
    }

    pub(super) fn get_merged_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
    ) -> Option<Rc<Symbol>> {
        symbol.map(|symbol| symbol.borrow().symbol_wrapper())
    }

    pub(super) fn get_symbol_of_node(&self, node: &Node) -> Option<Rc<Symbol>> {
        self.get_merged_symbol(node.maybe_symbol())
    }
}

pub(super) struct ExportCollisionTracker {
    pub specifier_text: String,
    pub exports_with_duplicate: Option<Vec<Rc<Node /*ExportDeclaration*/>>>,
}

pub(super) type ExportCollisionTrackerTable = UnderscoreEscapedMap<ExportCollisionTracker>;
