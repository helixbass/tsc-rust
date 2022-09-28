#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::rc::Rc;

use super::NodeBuilderContext;
use crate::{
    factory, get_emit_script_target, is_identifier_text, synthetic_factory,
    unescape_leading_underscores, using_single_line_string_writer, EmitTextWriter, Node, NodeArray,
    NodeBuilder, NodeInterface, Signature, Symbol, SymbolFlags, SymbolInterface, SymbolTable, Type,
    TypeChecker, TypeFormatFlags, TypeInterface, TypePredicate,
};

impl NodeBuilder {
    pub(super) fn create_access_from_symbol_chain(
        &self,
        context: &NodeBuilderContext,
        chain: &[Rc<Symbol>],
        index: usize,
        stopper: usize,
    ) -> Rc<Node> {
        let type_parameter_nodes = Option::<NodeArray>::None; // TODO: this is wrong
        let symbol = chain[index].clone();

        let mut symbol_name: Option<Cow<'static, str>>;
        if index == 0 {
            symbol_name = Some(
                self.type_checker
                    .get_name_of_symbol_as_written(&symbol, Some(context)),
            );
        } else {
            unimplemented!()
        }
        if symbol_name.is_none() {
            symbol_name = Some(
                self.type_checker
                    .get_name_of_symbol_as_written(&symbol, Some(context)),
            );
        }
        let symbol_name = symbol_name.unwrap();

        let identifier = synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| {
                factory_.create_identifier(
                    synthetic_factory_,
                    &symbol_name,
                    type_parameter_nodes,
                    None,
                )
            })
        });
        identifier.set_symbol(symbol);

        identifier.into()
    }

    pub(super) fn type_parameter_to_name(
        &self,
        type_: &Type, /*TypeParameter*/
        context: &NodeBuilderContext,
    ) -> Rc<Node> {
        unimplemented!()
    }

    pub(super) fn symbol_to_name(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        expects_identifier: bool,
    ) -> Rc<Node /*EntityName*/> {
        unimplemented!()
    }

    pub(super) fn symbol_to_expression_(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
    ) -> Rc<Node> {
        let chain = self.lookup_symbol_chain(symbol, context, meaning, None);
        let index = chain.len() - 1;
        self.create_expression_from_symbol_chain(context, chain, index)
    }

    pub(super) fn get_property_name_node_for_symbol(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
    ) -> Rc<Node> {
        let single_quote = false;
        let string_named = false;
        let raw_name = unescape_leading_underscores(symbol.escaped_name());
        self.create_property_name_node_for_identifier_or_literal(
            raw_name,
            Some(string_named),
            Some(single_quote),
        )
    }

    pub(super) fn create_property_name_node_for_identifier_or_literal(
        &self,
        name: String,
        string_named: Option<bool>,
        single_quote: Option<bool>,
    ) -> Rc<Node> {
        if is_identifier_text(
            &name,
            Some(get_emit_script_target(&self.type_checker.compiler_options)),
            None,
        ) {
            synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_.create_identifier(
                        synthetic_factory_,
                        &name,
                        Option::<NodeArray>::None,
                        None,
                    )
                })
            })
        } else {
            unimplemented!()
        }
        .into()
    }

    pub(super) fn create_expression_from_symbol_chain(
        &self,
        context: &NodeBuilderContext,
        chain: Vec<Rc<Symbol>>,
        index: usize,
    ) -> Rc<Node> {
        let type_parameter_nodes = Option::<NodeArray>::None; // TODO: this is wrong
        let symbol = &*(&chain)[index];

        let symbol_name = self
            .type_checker
            .get_name_of_symbol_as_written(symbol, Some(context));

        if index == 0 || false {
            let identifier = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_.create_identifier(
                        synthetic_factory_,
                        &symbol_name,
                        type_parameter_nodes,
                        None,
                    )
                })
            });
            identifier.set_symbol(symbol.symbol_wrapper());
            return identifier.into();
        } else {
            unimplemented!()
        }
    }

    pub(super) fn serialize_type_for_declaration<
        TEnclosingDeclaration: Borrow<Node>,
        TIncludePrivateSymbol: Fn(&Symbol),
    >(
        &self,
        context: &NodeBuilderContext,
        type_: &Type,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        include_private_symbol: Option<&TIncludePrivateSymbol>,
        bundled: Option<bool>,
    ) -> Rc<Node> {
        let result = self.type_to_type_node_helper(Some(type_), context);
        result.unwrap()
    }

    pub(super) fn serialize_return_type_for_signature<TIncludePrivateSymbol: Fn(&Symbol)>(
        &self,
        context: &NodeBuilderContext,
        type_: &Type,
        signature: &Signature,
        include_private_symbol: Option<&TIncludePrivateSymbol>,
        bundled: Option<bool>,
    ) -> Rc<Node> {
        unimplemented!()
    }

    pub(super) fn symbol_table_to_declaration_statements_(
        &self,
        symbol_table: &SymbolTable,
        context: &NodeBuilderContext,
        bundled: Option<bool>,
    ) -> Option<Vec<Rc<Node /*Statement*/>>> {
        unimplemented!()
    }
}

impl TypeChecker {
    pub fn type_predicate_to_string_<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_predicate: &TypePredicate,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    ) -> String {
        let flags = flags.unwrap_or(TypeFormatFlags::UseAliasDefinedOutsideCurrentScope);
        if let Some(writer) = writer {
            self.type_predicate_to_string_worker(
                type_predicate,
                enclosing_declaration,
                flags,
                writer.clone(),
            );
            RefCell::borrow(&writer).get_text()
        } else {
            using_single_line_string_writer(|writer| {
                self.type_predicate_to_string_worker(
                    type_predicate,
                    enclosing_declaration,
                    flags,
                    writer,
                )
            })
        }
    }
}
