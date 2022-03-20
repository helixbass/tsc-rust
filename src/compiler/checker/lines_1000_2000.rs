#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use super::{get_node_id, get_symbol_id};
use crate::{
    null_transformation_context, set_text_range_pos_end, synthetic_factory, visit_each_child,
    CancellationTokenDebuggable, EmitResolverDebuggable, NodeArray, VisitResult, __String,
    create_diagnostic_for_node, escape_leading_underscores, factory, get_first_identifier,
    get_source_file_of_node, is_jsx_opening_fragment, parse_isolated_entity_name,
    unescape_leading_underscores, visit_node, BaseTransientSymbol, CheckFlags, Debug_, Diagnostic,
    DiagnosticMessage, Node, NodeInterface, NodeLinks, Symbol, SymbolFlags, SymbolInterface,
    SymbolLinks, SymbolTable, SyntaxKind, TransientSymbol, TransientSymbolInterface, TypeChecker,
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
