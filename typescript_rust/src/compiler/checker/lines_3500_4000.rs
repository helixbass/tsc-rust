use std::{borrow::Borrow, collections::HashMap, io, ptr};

use gc::{Gc, GcCell};
use id_arena::Id;
use itertools::Itertools;

use super::{get_node_id, MembersOrExportsResolutionKind};
use crate::{
    SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface,
    UnderscoreEscapedMap, __String, add_range, chain_diagnostic_messages,
    create_diagnostic_for_node, create_symbol_table, first, get_declaration_of_kind,
    get_es_module_interop, get_namespace_declaration_node, get_object_flags,
    get_source_file_of_node, get_text_of_node, get_types_package_name, is_access_expression,
    is_ambient_module, is_binary_expression, is_class_expression, is_entity_name_expression,
    is_exports_identifier, is_external_module, is_external_module_name_relative, is_import_call,
    is_import_declaration, is_module_exports_access_expression, is_object_literal_expression,
    is_type_literal_node, is_variable_declaration, length, mangle_scoped_package_name,
    node_is_synthesized, push_if_unique_eq, push_if_unique_gc, return_ok_default_if_none,
    return_ok_none_if_none, try_for_each_entry, try_map_defined, unescape_leading_underscores,
    Diagnostics, HasArena, HasInitializerInterface, HasTypeInterface, InArena, InternalSymbolName,
    ModuleKind, Node, NodeInterface, ObjectFlags, OptionTry, ResolvedModuleFull, SignatureKind,
    Symbol, SymbolFlags, SymbolInterface, SymbolTable,
};

impl TypeChecker {
    pub(super) fn error_on_implicit_any_module(
        &self,
        is_error: bool,
        error_node: Id<Node>,
        resolved_module: Id<ResolvedModuleFull>,
        module_reference: &str,
    ) {
        let resolved_module_ref = resolved_module.ref_(self);
        let package_id = &resolved_module_ref.package_id;
        let resolved_file_name = &resolved_module_ref.resolved_file_name;
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
            ),
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

    pub(super) fn resolve_external_module_symbol(
        &self,
        module_symbol: Option<Id<Symbol>>,
        dont_resolve_alias: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        if module_symbol.is_none() {
            return Ok(None);
        }
        let module_symbol = module_symbol.unwrap();
        let module_symbol_ref = module_symbol.ref_(self);
        let module_symbol_exports = module_symbol_ref.maybe_exports();
        let module_symbol_exports = module_symbol_exports.as_ref();
        if module_symbol_exports.is_none() {
            return Ok(None);
        }
        let module_symbol_exports = module_symbol_exports.unwrap();
        let export_equals = self.resolve_symbol(
            (**module_symbol_exports)
                .borrow()
                .get(InternalSymbolName::ExportEquals)
                .cloned(),
            dont_resolve_alias,
        )?;
        let exported = self.get_common_js_export_equals(
            self.get_merged_symbol(export_equals),
            self.get_merged_symbol(Some(module_symbol)).unwrap(),
        )?;
        Ok(Some(
            self.get_merged_symbol(exported)
                .unwrap_or_else(|| module_symbol),
        ))
    }

    pub(super) fn get_common_js_export_equals(
        &self,
        exported: Option<Id<Symbol>>,
        module_symbol: Id<Symbol>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let exported = return_ok_none_if_none!(exported);
        if exported == self.unknown_symbol()
            || exported == module_symbol
            || (*module_symbol.ref_(self).exports()).borrow().len() == 1
            || exported.ref_(self).flags().intersects(SymbolFlags::Alias)
        {
            return Ok(Some(exported));
        }
        let links = self.get_symbol_links(exported);
        if let Some(links_cjs_export_merged) = (*links.ref_(self))
            .borrow()
            .cjs_export_merged
            .as_ref()
            .map(|cjs_export_merged| cjs_export_merged.clone())
        {
            return Ok(Some(links_cjs_export_merged));
        }
        let merged = if exported
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::Transient)
        {
            exported
        } else {
            self.clone_symbol(exported)
        };
        merged
            .ref_(self)
            .set_flags(merged.ref_(self).flags() | SymbolFlags::ValueModule);
        let merged_exports = merged
            .ref_(self)
            .maybe_exports_mut()
            .get_or_insert_with(|| {
                Gc::new(GcCell::new(create_symbol_table(
                    self.arena(),
                    Option::<&[Id<Symbol>]>::None,
                )))
            })
            .clone();
        for (name, &s) in &*(*module_symbol.ref_(self).exports()).borrow() {
            if name == InternalSymbolName::ExportEquals {
                continue;
            }
            let value = if (*merged_exports).borrow().contains_key(name) {
                self.merge_symbol(*(*merged_exports).borrow().get(name).unwrap(), s, None)?
            } else {
                s
            };
            merged_exports.borrow_mut().insert(name.clone(), value);
        }
        self.get_symbol_links(merged).ref_(self).borrow_mut().cjs_export_merged = Some(merged.clone());
        links.ref_(self).borrow_mut().cjs_export_merged = Some(merged.clone());
        Ok(Some(merged.clone()))
    }

    pub(super) fn resolve_es_module_symbol(
        &self,
        module_symbol: Option<Id<Symbol>>,
        referencing_location: Id<Node>,
        dont_resolve_alias: bool,
        suppress_interop_error: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let symbol =
            self.resolve_external_module_symbol(module_symbol, Some(dont_resolve_alias))?;
        if symbol.is_none() {
            return Ok(None);
        }
        let symbol = symbol.unwrap();

        if !dont_resolve_alias {
            if !suppress_interop_error
                && !symbol
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::Module | SymbolFlags::Variable)
                && get_declaration_of_kind(symbol, SyntaxKind::SourceFile, self).is_none()
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

                return Ok(Some(symbol));
            }

            let reference_parent = referencing_location.ref_(self).parent();
            if is_import_declaration(&reference_parent.ref_(self))
                && get_namespace_declaration_node(reference_parent, self).is_some()
                || is_import_call(reference_parent, self)
            {
                let reference = if is_import_call(reference_parent, self) {
                    reference_parent.ref_(self).as_call_expression().arguments.ref_(self)[0]
                } else {
                    reference_parent.ref_(self).as_import_declaration().module_specifier
                };
                let type_ = self.get_type_of_symbol(symbol)?;
                let default_only_type = self.get_type_with_synthetic_default_only(
                    type_,
                    symbol,
                    module_symbol.unwrap(),
                    reference,
                )?;
                if let Some(default_only_type) = default_only_type {
                    return Ok(Some(self.clone_type_as_module_type(
                        symbol,
                        default_only_type,
                        reference_parent,
                    )?));
                }

                if matches!(get_es_module_interop(&self.compiler_options.ref_(self)), Some(true)) {
                    let mut sigs =
                        self.get_signatures_of_structured_type(type_, SignatureKind::Call)?;
                    if
                    /* !sigs ||*/
                    sigs.is_empty() {
                        sigs = self
                            .get_signatures_of_structured_type(type_, SignatureKind::Construct)?;
                    }
                    if
                    /*sigs &&*/
                    !sigs.is_empty()
                        || self
                            .get_property_of_type_(type_, InternalSymbolName::Default, None)?
                            .is_some()
                    {
                        let module_type = self.get_type_with_synthetic_default_import_type(
                            type_,
                            symbol,
                            module_symbol.unwrap(),
                            reference,
                        )?;
                        return Ok(Some(self.clone_type_as_module_type(
                            symbol,
                            module_type,
                            reference_parent,
                        )?));
                    }
                }
            }
        }
        Ok(Some(symbol))
    }

    pub(super) fn clone_type_as_module_type(
        &self,
        symbol: Id<Symbol>,
        module_type: Id<Type>,
        reference_parent: Id<Node>, /*ImportDeclaration | ImportCall*/
    ) -> io::Result<Id<Symbol>> {
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
            .set_parent(symbol.ref_(self).maybe_parent());
        let result_links = result.ref_(self).as_transient_symbol().symbol_links();
        let result_links_ref = result_links.ref_(self);
        let mut result_links = result_links_ref.borrow_mut();
        result_links.target = Some(symbol);
        result_links.originating_import = Some(reference_parent);
        if let Some(symbol_value_declaration) = symbol.ref_(self).maybe_value_declaration() {
            result
                .ref_(self)
                .set_value_declaration(symbol_value_declaration);
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
        let resolved_module_type = self.resolve_structured_type_members(module_type)?;
        result_links.type_ = Some(
            self.create_anonymous_type(
                Some(result.clone()),
                resolved_module_type.ref_(self).as_resolved_type().members(),
                vec![],
                vec![],
                resolved_module_type
                    .ref_(self)
                    .as_resolved_type()
                    .index_infos()
                    .clone(),
            )?,
        );
        Ok(result)
    }

    pub(super) fn has_export_assignment_symbol(&self, module_symbol: Id<Symbol>) -> bool {
        (*module_symbol.ref_(self).exports())
            .borrow()
            .get(InternalSymbolName::ExportEquals)
            .is_some()
    }

    pub(super) fn get_exports_of_module_as_array(
        &self,
        module_symbol: Id<Symbol>,
    ) -> io::Result<Vec<Id<Symbol>>> {
        Ok(self.symbols_to_array(&(*self.get_exports_of_module_(module_symbol)?).borrow()))
    }

    pub fn get_exports_and_properties_of_module(
        &self,
        module_symbol: Id<Symbol>,
    ) -> io::Result<Vec<Id<Symbol>>> {
        let mut exports = self.get_exports_of_module_as_array(module_symbol)?;
        let export_equals = self
            .resolve_external_module_symbol(Some(module_symbol), None)?
            .unwrap();
        if export_equals != module_symbol {
            let type_ = self.get_type_of_symbol(export_equals)?;
            if self.should_treat_properties_of_external_module_as_exports(type_) {
                add_range(
                    &mut exports,
                    Some(&*self.get_properties_of_type(type_)?.collect_vec()),
                    None,
                    None,
                );
            }
        }
        Ok(exports)
    }

    pub fn for_each_export_and_property_of_module(
        &self,
        module_symbol: Id<Symbol>,
        mut cb: impl FnMut(Id<Symbol>, &__String),
    ) -> io::Result<()> {
        let exports = self.get_exports_of_module_(module_symbol)?;
        for (key, &symbol) in &*(*exports).borrow() {
            if !self.is_reserved_member_name(key) {
                cb(symbol, key);
            }
        }
        let export_equals = self
            .resolve_external_module_symbol(Some(module_symbol), None)?
            .unwrap();
        if export_equals != module_symbol {
            let type_ = self.get_type_of_symbol(export_equals)?;
            if self.should_treat_properties_of_external_module_as_exports(type_) {
                self.for_each_property_of_type(type_, |symbol, escaped_name| {
                    cb(symbol, escaped_name)
                })?;
            }
        }

        Ok(())
    }

    pub(super) fn try_get_member_in_module_exports_(
        &self,
        member_name: &str, /*__String*/
        module_symbol: Id<Symbol>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let symbol_table = self.get_exports_of_module_(module_symbol)?;
        // if (symbolTable) {
        let ret = (*symbol_table).borrow().get(member_name).cloned();
        Ok(ret)
        // }
    }

    pub(super) fn try_get_member_in_module_exports_and_properties_(
        &self,
        member_name: &str, /*__String*/
        module_symbol: Id<Symbol>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let symbol = self.try_get_member_in_module_exports_(member_name, module_symbol)?;
        if symbol.is_some() {
            return Ok(symbol);
        }

        let export_equals = self
            .resolve_external_module_symbol(Some(module_symbol), None)?
            .unwrap();
        if export_equals == module_symbol {
            return Ok(None);
        }

        let type_ = self.get_type_of_symbol(export_equals)?;
        Ok(
            if self.should_treat_properties_of_external_module_as_exports(type_) {
                self.get_property_of_type_(type_, member_name, None)?
            } else {
                None
            },
        )
    }

    pub(super) fn should_treat_properties_of_external_module_as_exports(
        &self,
        resolved_external_module_type: Id<Type>,
    ) -> bool {
        !(resolved_external_module_type
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Primitive)
            || get_object_flags(&resolved_external_module_type.ref_(self))
                .intersects(ObjectFlags::Class)
            || self.is_array_type(resolved_external_module_type)
            || self.is_tuple_type(resolved_external_module_type))
    }

    pub(super) fn get_exports_of_symbol(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<Gc<GcCell<SymbolTable>>> {
        Ok(
            if symbol
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::LateBindingContainer)
            {
                self.get_resolved_members_or_exports_of_symbol(
                    symbol,
                    MembersOrExportsResolutionKind::resolved_exports,
                )?
            } else if symbol.ref_(self).flags().intersects(SymbolFlags::Module) {
                self.get_exports_of_module_(symbol)?
            } else {
                symbol
                    .ref_(self)
                    .maybe_exports()
                    .as_ref()
                    .map_or_else(|| self.empty_symbols(), |exports| exports.clone())
            },
        )
    }

    pub(super) fn get_exports_of_module_(
        &self,
        module_symbol: Id<Symbol>,
    ) -> io::Result<Gc<GcCell<SymbolTable>>> {
        let links = self.get_symbol_links(module_symbol);
        let resolved_exports = (*links.ref_(self)).borrow().resolved_exports.clone();
        resolved_exports.try_unwrap_or_else(|| {
            let resolved_exports = self.get_exports_of_module_worker(module_symbol)?;
            links.ref_(self).borrow_mut().resolved_exports = Some(resolved_exports.clone());
            Ok(resolved_exports)
        })
    }

    pub(super) fn extend_export_symbols(
        &self,
        target: &mut SymbolTable,
        source: Option<impl Borrow<SymbolTable>>,
        mut lookup_table: Option<&mut ExportCollisionTrackerTable>,
        export_node: Option<Id<Node>>,
    ) -> io::Result<()> {
        if source.is_none() {
            return Ok(());
        }
        let source = source.unwrap();
        let source: &SymbolTable = source.borrow();
        for (id, &source_symbol) in source {
            if id == InternalSymbolName::Default {
                continue;
            }

            let target_symbol = target.get(id).cloned();
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
                                        .ref_(self).as_export_declaration()
                                        .module_specifier
                                        .unwrap(),
                                    None,
                                    self,
                                )
                                .into_owned(),
                                exports_with_duplicate: None,
                            },
                        );
                    }
                }
            } else if let Some(lookup_table) = lookup_table.as_mut() {
                if let Some(export_node) = export_node {
                    if matches!(
                        target_symbol,
                        Some(target_symbol) if self.resolve_symbol(Some(target_symbol), None)?.unwrap() !=
                            self.resolve_symbol(Some(source_symbol), None)?.unwrap()
                    ) {
                        let collision_tracker = lookup_table.get_mut(id).unwrap();
                        if collision_tracker.exports_with_duplicate.is_none() {
                            collision_tracker.exports_with_duplicate =
                                Some(vec![export_node]);
                        } else {
                            collision_tracker
                                .exports_with_duplicate
                                .as_mut()
                                .unwrap()
                                .push(export_node);
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub(super) fn get_exports_of_module_worker(
        &self,
        module_symbol: Id<Symbol>,
    ) -> io::Result<Gc<GcCell<SymbolTable>>> {
        let mut visited_symbols: Vec<Id<Symbol>> = vec![];

        let module_symbol = self.resolve_external_module_symbol(Some(module_symbol), None)?;

        Ok(self
            .visit_get_exports_of_module_worker(&mut visited_symbols, module_symbol)?
            .map_or_else(
                || self.empty_symbols(),
                |symbol_table| Gc::new(GcCell::new(symbol_table)),
            ))
    }

    pub(super) fn visit_get_exports_of_module_worker(
        &self,
        visited_symbols: &mut Vec<Id<Symbol>>,
        symbol: Option<Id<Symbol>>,
    ) -> io::Result<Option<SymbolTable>> {
        let symbol = return_ok_default_if_none!(symbol);
        if !(symbol.ref_(self).maybe_exports().is_some()
            && push_if_unique_eq(visited_symbols, &symbol))
        {
            return Ok(None);
        }
        let symbol_ref = symbol.ref_(self);
        let symbol_exports = symbol_ref.maybe_exports();
        let symbol_exports = symbol_exports.as_ref().unwrap();
        let symbol_exports = (**symbol_exports).borrow();
        let mut symbols = symbol_exports.clone();
        let export_stars = symbol_exports.get(InternalSymbolName::ExportStar);
        if let Some(&export_stars) = export_stars {
            let mut nested_symbols =
                create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None);
            let mut lookup_table = ExportCollisionTrackerTable::new();
            if let Some(export_stars_declarations) =
                export_stars.ref_(self).maybe_declarations().as_ref()
            {
                for &node in export_stars_declarations {
                    let resolved_module = self.resolve_external_module_name_(
                        node,
                        node.ref_(self).as_export_declaration()
                            .module_specifier
                            .unwrap(),
                        None,
                    )?;
                    let exported_symbols =
                        self.visit_get_exports_of_module_worker(visited_symbols, resolved_module)?;
                    self.extend_export_symbols(
                        &mut nested_symbols,
                        exported_symbols.as_ref(),
                        Some(&mut lookup_table),
                        Some(node),
                    )?;
                }
            }
            for (id, export_collision_tracker) in &lookup_table {
                let exports_with_duplicate =
                    export_collision_tracker.exports_with_duplicate.as_ref();
                if id == "export="
                    || !matches!(exports_with_duplicate, Some(exports_with_duplicate) if !exports_with_duplicate.is_empty())
                    || symbols.contains_key(id)
                {
                    continue;
                }
                let exports_with_duplicate = exports_with_duplicate.unwrap();
                for &node in exports_with_duplicate {
                    self.diagnostics().add(
                        self.alloc_diagnostic(create_diagnostic_for_node(
                            node,
                            &Diagnostics::Module_0_has_already_exported_a_member_named_1_Consider_explicitly_re_exporting_to_resolve_the_ambiguity,
                            Some(vec![
                                lookup_table.get(id).unwrap().specifier_text.clone(),
                                unescape_leading_underscores(id).to_owned()
                            ]),
                            self,
                        ).into())
                    );
                }
            }
            self.extend_export_symbols(
                &mut symbols,
                Some(&nested_symbols),
                None,
                Option::<Id<Node>>::None,
            )?;
        }
        Ok(Some(symbols))
    }

    pub(super) fn get_merged_symbol(&self, symbol: Option<Id<Symbol>>) -> Option<Id<Symbol>> {
        let symbol = symbol?;
        if let Some(symbol_merge_id) = symbol.ref_(self).maybe_merge_id() {
            let merged = self.merged_symbols().get(&symbol_merge_id).cloned();
            Some(merged.unwrap_or_else(|| symbol))
        } else {
            Some(symbol)
        }
    }

    pub(super) fn get_symbol_of_node(&self, node: Id<Node>) -> io::Result<Option<Id<Symbol>>> {
        Ok(self.get_merged_symbol(
            node.ref_(self).maybe_symbol()
                .try_map(|node_symbol| self.get_late_bound_symbol(node_symbol))?,
        ))
    }

    pub(super) fn get_parent_of_symbol(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<Option<Id<Symbol>>> {
        Ok(self.get_merged_symbol(
            symbol
                .ref_(self)
                .maybe_parent()
                .try_map(|symbol_parent| self.get_late_bound_symbol(symbol_parent))?,
        ))
    }

    pub(super) fn get_alternative_containing_modules(
        &self,
        symbol: Id<Symbol>,
        enclosing_declaration: Id<Node>,
    ) -> io::Result<Vec<Id<Symbol>>> {
        let containing_file = get_source_file_of_node(enclosing_declaration, self);
        let id = get_node_id(&containing_file.ref_(self));
        let links = self.get_symbol_links(symbol);
        let mut results: Option<Vec<Id<Symbol>>> = None;
        if let Some(links_extended_containers_by_file) =
            (*links.ref_(self)).borrow().extended_containers_by_file.as_ref()
        {
            results = links_extended_containers_by_file.get(&id).map(Clone::clone);
            if results.is_some() {
                return Ok(results.unwrap());
            }
        }
        if
        /*containingFile &&*/
        let Some(containing_file_imports) =
            containing_file.ref_(self).as_source_file().maybe_imports().as_ref()
        {
            for &import_ref in containing_file_imports {
                if node_is_synthesized(&*import_ref.ref_(self)) {
                    continue;
                }
                let resolved_module = self.resolve_external_module_name_(
                    enclosing_declaration,
                    import_ref,
                    Some(true),
                )?;
                if resolved_module.is_none() {
                    continue;
                }
                let resolved_module = resolved_module.unwrap();
                let ref_ = self.get_alias_for_symbol_in_container(resolved_module, symbol)?;
                if ref_.is_none() {
                    continue;
                }
                if results.is_none() {
                    results = Some(vec![]);
                }
                results.as_mut().unwrap().push(resolved_module);
            }
            if length(results.as_deref()) > 0 {
                let links_ref = links.ref_(self);
                let mut links = links_ref.borrow_mut();
                if links.extended_containers_by_file.is_none() {
                    links.extended_containers_by_file = Some(HashMap::new());
                }
                links
                    .extended_containers_by_file
                    .as_mut()
                    .unwrap()
                    .insert(id, results.clone().unwrap());
                return Ok(results.unwrap());
            }
        }
        if let Some(links_extended_containers) = (*links.ref_(self)).borrow().extended_containers.as_ref() {
            return Ok(links_extended_containers.clone());
        }
        let host_ref = self.host.ref_(self);
        let other_files = host_ref.get_source_files();
        for &file in &*other_files {
            if !is_external_module(&file.ref_(self)) {
                continue;
            }
            let sym = self.get_symbol_of_node(file)?.unwrap();
            let ref_ = self.get_alias_for_symbol_in_container(sym, symbol)?;
            if ref_.is_none() {
                continue;
            }
            if results.is_none() {
                results = Some(vec![]);
            }
            results.as_mut().unwrap().push(sym);
        }
        let ret = results.unwrap_or_else(|| vec![]);
        links.ref_(self).borrow_mut().extended_containers = Some(ret.clone());
        Ok(ret)
    }

    pub(super) fn get_containers_of_symbol(
        &self,
        symbol: Id<Symbol>,
        enclosing_declaration: Option<Id<Node>>,
        meaning: SymbolFlags,
    ) -> io::Result<Option<Vec<Id<Symbol>>>> {
        let container = self.get_parent_of_symbol(symbol)?;
        if let Some(container) = container {
            if !symbol
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::TypeParameter)
            {
                let mut additional_containers: Vec<Id<Symbol>> = try_map_defined(
                    container.ref_(self).maybe_declarations().as_ref(),
                    |&d: &Id<Node>, _| {
                        self.get_file_symbol_if_file_symbol_export_equals_container(d, container)
                    },
                )?;
                let mut reexport_containers = enclosing_declaration
                    .map(|enclosing_declaration| {
                        self.get_alternative_containing_modules(symbol, enclosing_declaration)
                    })
                    .transpose()?;
                let object_literal_container =
                    self.get_variable_declaration_of_object_literal(container, meaning)?;
                if let Some(enclosing_declaration) = enclosing_declaration {
                    if container
                        .ref_(self)
                        .flags()
                        .intersects(self.get_qualified_left_meaning(meaning))
                        && self
                            .get_accessible_symbol_chain(
                                Some(container),
                                Some(enclosing_declaration),
                                SymbolFlags::Namespace,
                                false,
                                None,
                            )?
                            .is_some()
                    {
                        let mut ret = vec![container];
                        ret.append(&mut additional_containers);
                        if let Some(reexport_containers) = reexport_containers.as_mut() {
                            ret.append(reexport_containers);
                        }
                        if let Some(object_literal_container) = object_literal_container {
                            ret.push(object_literal_container);
                        }
                        return Ok(Some(ret));
                    }
                }
                let first_variable_match = if !(container
                    .ref_(self)
                    .flags()
                    .intersects(self.get_qualified_left_meaning(meaning)))
                    && container.ref_(self).flags().intersects(SymbolFlags::Type)
                    && self
                        .get_declared_type_of_symbol(container)?
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::Object)
                    && meaning == SymbolFlags::Value
                {
                    self.try_for_each_symbol_table_in_scope(
                        enclosing_declaration,
                        |t, _, _, _| -> io::Result<_> {
                            try_for_each_entry(
                                &*(*t).borrow(),
                                |&s: &Id<Symbol>, _| -> io::Result<_> {
                                    if s.ref_(self)
                                        .flags()
                                        .intersects(self.get_qualified_left_meaning(meaning))
                                        && self.get_type_of_symbol(s)?
                                            == self.get_declared_type_of_symbol(container)?
                                    {
                                        return Ok(Some(s.clone()));
                                    }
                                    Ok(None)
                                },
                            )
                        },
                    )?
                } else {
                    None
                };
                let mut res = if let Some(first_variable_match) = first_variable_match {
                    vec![first_variable_match]
                } else {
                    vec![]
                };
                res.append(&mut additional_containers);
                res.push(container);
                if let Some(object_literal_container) = object_literal_container {
                    res.push(object_literal_container);
                }
                if let Some(reexport_containers) = reexport_containers.as_mut() {
                    res.append(reexport_containers);
                }
                return Ok(Some(res));
            }
        }
        let candidates = try_map_defined(
            symbol.ref_(self).maybe_declarations().as_deref(),
            |&d: &Id<Node>, _| -> io::Result<_> {
                if !is_ambient_module(d, self) {
                    if let Some(d_parent) = d.ref_(self).maybe_parent() {
                        if self.has_non_global_augmentation_external_module_symbol(d_parent) {
                            return self.get_symbol_of_node(d_parent);
                        }
                    }
                }
                if is_class_expression(&d.ref_(self)) {
                    let d_parent = d.ref_(self).parent();
                    if is_binary_expression(&d_parent.ref_(self)) {
                        let d_parent_ref = d_parent.ref_(self);
                        let d_parent_as_binary_expression = d_parent_ref.as_binary_expression();
                        if d_parent_as_binary_expression.operator_token.ref_(self).kind()
                            == SyntaxKind::EqualsToken
                            && is_access_expression(&d_parent_as_binary_expression.left.ref_(self))
                        {
                            let d_parent_left_expression = d_parent_as_binary_expression
                                .left
                                .ref_(self).as_has_expression()
                                .expression();
                            if is_entity_name_expression(d_parent_left_expression, self) {
                                if is_module_exports_access_expression(
                                    d_parent_as_binary_expression.left,
                                    self,
                                ) || is_exports_identifier(&d_parent_left_expression.ref_(self))
                                {
                                    return self.get_symbol_of_node(get_source_file_of_node(d, self));
                                }
                                self.check_expression_cached(d_parent_left_expression, None)?;
                                return Ok((*self.get_node_links(d_parent_left_expression))
                                    .borrow()
                                    .resolved_symbol
                                    .clone());
                            }
                        }
                    }
                }
                Ok(None)
            },
        )?;
        if length(Some(&candidates)) == 0 {
            return Ok(None);
        }
        Ok(Some(try_map_defined(
            Some(candidates),
            |candidate: Id<Symbol>, _| -> io::Result<_> {
                Ok(
                    if self
                        .get_alias_for_symbol_in_container(candidate, symbol)?
                        .is_some()
                    {
                        Some(candidate)
                    } else {
                        None
                    },
                )
            },
        )?))
    }

    pub(super) fn get_variable_declaration_of_object_literal(
        &self,
        symbol: Id<Symbol>,
        meaning: SymbolFlags,
    ) -> io::Result<Option<Id<Symbol>>> {
        let first_decl: Option<Id<Node>> =
            if length(symbol.ref_(self).maybe_declarations().as_deref()) > 0 {
                Some(first(symbol.ref_(self).maybe_declarations().as_deref().unwrap()).clone())
            } else {
                None
            };
        if meaning.intersects(SymbolFlags::Value) {
            if let Some(first_decl) = first_decl {
                if let Some(first_decl_parent) = first_decl.ref_(self).maybe_parent() {
                    if is_variable_declaration(&first_decl_parent.ref_(self)) {
                        let first_decl_parent_ref = first_decl_parent.ref_(self);
                        let first_decl_parent_as_variable_declaration =
                            first_decl_parent_ref.as_variable_declaration();
                        if is_object_literal_expression(&first_decl.ref_(self))
                            && first_decl_parent_as_variable_declaration.maybe_initializer() == Some(first_decl)
                            || is_type_literal_node(&first_decl.ref_(self))
                                && first_decl_parent_as_variable_declaration.maybe_type() == Some(first_decl)
                        {
                            return self.get_symbol_of_node(first_decl_parent);
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    pub(super) fn get_file_symbol_if_file_symbol_export_equals_container(
        &self,
        d: Id<Node>, /*Declaration*/
        container: Id<Symbol>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let file_symbol = self.get_external_module_container(d)?;
        let exported = return_ok_none_if_none!(file_symbol
            .and_then(|file_symbol| file_symbol.ref_(self).maybe_exports().clone())
            .and_then(|exports| {
                (*exports)
                    .borrow()
                    .get(InternalSymbolName::ExportEquals)
                    .cloned()
            }));
        Ok(
            if self
                .get_symbol_if_same_reference(exported, container)?
                .is_some()
            {
                file_symbol
            } else {
                None
            },
        )
    }
}

pub(super) struct ExportCollisionTracker {
    pub specifier_text: String,
    pub exports_with_duplicate: Option<Vec<Id<Node /*ExportDeclaration*/>>>,
}

pub(super) type ExportCollisionTrackerTable = UnderscoreEscapedMap<ExportCollisionTracker>;
