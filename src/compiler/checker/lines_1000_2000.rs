#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use super::{get_node_id, get_symbol_id};
use crate::{
    __String, create_diagnostic_for_node, BaseTransientSymbol, CheckFlags, Debug_, Diagnostic,
    DiagnosticMessage, Node, NodeInterface, NodeLinks, Symbol, SymbolFlags, SymbolInterface,
    SymbolLinks, SymbolTable, SyntaxKind, TransientSymbol, TransientSymbolInterface, TypeChecker,
};

impl TypeChecker {
    pub(super) fn get_jsx_namespace_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> __String {
        unimplemented!()
    }

    pub(super) fn create_error<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Diagnostic> {
        if let Some(location) = location {
            Rc::new(create_diagnostic_for_node(location.borrow(), message, args).into())
        } else {
            unimplemented!()
        }
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

    pub(super) fn error_and_maybe_suggest_await(
        &self,
        location: &Node,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.error(Some(location), message, args);
        diagnostic
    }

    pub(super) fn create_symbol(
        &self,
        flags: SymbolFlags,
        name: __String,
        check_flags: Option<CheckFlags>,
    ) -> TransientSymbol {
        let symbol = (self.Symbol)(flags | SymbolFlags::Transient, name);
        let symbol = BaseTransientSymbol::new(symbol, check_flags.unwrap_or(CheckFlags::None));
        symbol.into()
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
                unimplemented!()
            } else {
                source_symbol.clone()
            };
            target.insert(id.clone(), value);
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
        node.kind() == SyntaxKind::SourceFile && true
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
                if symbol.flags().intersects(meaning) {
                    return Some(symbol);
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
        unimplemented!()
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
