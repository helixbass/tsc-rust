#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::rc::Rc;

use super::ResolveNameNameArg;
use crate::{
    declaration_name_to_string, get_first_identifier, node_is_missing,
    unescape_leading_underscores, Debug_, Node, NodeInterface, Symbol, SymbolFlags,
    SymbolInterface, TypeChecker,
};

impl TypeChecker {
    pub(super) fn diagnostic_name(&self, name_arg: ResolveNameNameArg) -> Cow<'static, str> {
        match name_arg {
            ResolveNameNameArg::__String(__string) => {
                unescape_leading_underscores(&__string).into()
            }
            ResolveNameNameArg::Node(node) => declaration_name_to_string(Some(&*node)),
        }
    }

    pub(super) fn resolve_alias(&self, symbol: &Symbol) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn resolve_entity_name(
        &self,
        name: &Node, /*EntityNameOrEntityNameExpression*/
        meaning: SymbolFlags,
        ignore_errors: Option<bool>,
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let ignore_errors = ignore_errors.unwrap_or(false);
        let dont_resolve_alias = dont_resolve_alias.unwrap_or(false);
        if node_is_missing(Some(name)) {
            return None;
        }

        let symbol: Option<Rc<Symbol>>;
        match name {
            Node::Identifier(name_as_identifier) => {
                let message = if false {
                    unimplemented!()
                } else {
                    self.get_cannot_find_name_diagnostic_for_name(&*get_first_identifier(name))
                };
                let symbol_from_js_prototype: Option<Rc<Symbol>> =
                    if false { unimplemented!() } else { None };
                symbol = self.get_merged_symbol(self.resolve_name(
                    Some(name),
                    &name_as_identifier.escaped_text,
                    meaning,
                    if ignore_errors || symbol_from_js_prototype.is_some() {
                        None
                    } else {
                        Some(message)
                    },
                    Some(name.node_wrapper()),
                    true,
                    Some(false),
                ));
                if symbol.is_none() {
                    unimplemented!()
                }
            }
            // else if name.kind() == SyntaxKind::QualifiedName
            //     || name.kind() == SyntaxKind::PropertyAccessExpression
            //     unimplemented!()
            _ => Debug_.assert_never(name, Some("Unknown entity name kind.")),
        }
        let symbol = symbol.unwrap();
        if symbol.flags().intersects(meaning) || dont_resolve_alias {
            Some(symbol)
        } else {
            Some(self.resolve_alias(&symbol))
        }
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
