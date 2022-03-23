#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use crate::{
    chain_diagnostic_messages, create_symbol_table, get_types_package_name,
    is_external_module_name_relative, mangle_scoped_package_name, Diagnostics, InternalSymbolName,
    Node, NodeInterface, ResolvedModuleFull, Symbol, SymbolFlags, SymbolInterface, SymbolTable,
    TypeChecker, __String,
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
        unimplemented!()
    }

    pub(super) fn has_export_assignment_symbol(&self, module_symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn get_exports_of_module_as_array(&self, module_symbol: &Symbol) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn try_get_member_in_module_exports_(
        &self,
        member_name: &__String,
        module_symbol: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn try_get_member_in_module_exports_and_properties_(
        &self,
        member_name: &__String,
        module_symbol: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_exports_of_symbol(&self, symbol: &Symbol) -> &SymbolTable {
        unimplemented!()
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
