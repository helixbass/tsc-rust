#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::NodeBuilderContext;
use crate::{
    factory, get_emit_script_target, is_entity_name, is_identifier_text,
    is_indexed_access_type_node, set_emit_flags, synthetic_factory, unescape_leading_underscores,
    using_single_line_string_writer, with_synthetic_factory_and_factory, Debug_, EmitFlags,
    EmitTextWriter, HasTypeArgumentsInterface, InternalSymbolName, Node, NodeArray, NodeBuilder,
    NodeInterface, Signature, Symbol, SymbolFlags, SymbolInterface, SymbolTable, SyntaxKind, Type,
    TypeChecker, TypeFormatFlags, TypeInterface, TypePredicate, __String, for_each_entry_bool,
    NodeBuilderFlags,
};

impl NodeBuilder {
    pub(super) fn create_access_from_symbol_chain(
        &self,
        context: &NodeBuilderContext,
        override_type_arguments: Option<&[Rc<Node /*TypeNode*/>]>,
        chain: &[Rc<Symbol>],
        index: usize,
        stopper: usize,
    ) -> Rc<Node> {
        let type_parameter_nodes = if index == chain.len() - 1 {
            override_type_arguments.map(ToOwned::to_owned)
        } else {
            self.lookup_type_parameter_nodes(chain, index, context)
        };
        let symbol = &chain[index];

        let parent = if index == 0 {
            None
        } else {
            chain.get(index - 1)
        };
        let mut symbol_name: Option<String> = None;
        if index == 0 {
            context.set_flags(context.flags() | NodeBuilderFlags::InInitialEntityName);
            symbol_name = Some(
                self.type_checker
                    .get_name_of_symbol_as_written(&symbol, Some(context))
                    .into_owned(),
            );
            context.increment_approximate_length_by(
                symbol_name
                    .as_ref()
                    .map_or(0, |symbol_name| symbol_name.len())
                    + 1,
            );
            context.set_flags(context.flags() ^ NodeBuilderFlags::InInitialEntityName);
        } else {
            if let Some(parent) = parent.as_ref()
            /*&& getExportsOfSymbol(parent)*/
            {
                let exports = self.type_checker.get_exports_of_symbol(parent);
                for_each_entry_bool(&(*exports).borrow(), |ex: &Rc<Symbol>, name: &__String| {
                    if self
                        .type_checker
                        .get_symbol_if_same_reference(ex, symbol)
                        .is_some()
                        && !self.type_checker.is_late_bound_name(name)
                        && name != &InternalSymbolName::ExportEquals()
                    {
                        symbol_name = Some(unescape_leading_underscores(name));
                        return true;
                    }
                    false
                });
            }
        }
        if symbol_name.is_none() {
            symbol_name = Some(
                self.type_checker
                    .get_name_of_symbol_as_written(&symbol, Some(context))
                    .into_owned(),
            );
        }
        let symbol_name = symbol_name.unwrap();
        context.increment_approximate_length_by(symbol_name.len() + 1);

        if !context
            .flags()
            .intersects(NodeBuilderFlags::ForbidIndexedAccessSymbolReferences)
        {
            if let Some(parent) = parent.filter(|parent| {
                matches!(
                    (*self.type_checker.get_members_of_symbol(parent))
                        .borrow()
                        .get(symbol.escaped_name()),
                    Some(got_member_of_symbol) if self.type_checker.get_symbol_if_same_reference(
                        got_member_of_symbol,
                        symbol,
                    ).is_some()
                )
            }) {
                let ref lhs = self.create_access_from_symbol_chain(
                    context,
                    override_type_arguments,
                    chain,
                    index - 1,
                    stopper,
                );
                if is_indexed_access_type_node(lhs) {
                    return with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                        factory_
                            .create_indexed_access_type_node(
                                synthetic_factory_,
                                lhs.clone(),
                                factory_
                                    .create_literal_type_node(
                                        synthetic_factory_,
                                        factory_
                                            .create_string_literal(
                                                synthetic_factory_,
                                                symbol_name,
                                                None,
                                                None,
                                            )
                                            .into(),
                                    )
                                    .into(),
                            )
                            .into()
                    });
                } else {
                    return with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                        factory_
                            .create_indexed_access_type_node(
                                synthetic_factory_,
                                factory_
                                    .create_type_reference_node(
                                        synthetic_factory_,
                                        lhs.clone(),
                                        type_parameter_nodes,
                                    )
                                    .into(),
                                factory_
                                    .create_literal_type_node(
                                        synthetic_factory_,
                                        factory_
                                            .create_string_literal(
                                                synthetic_factory_,
                                                symbol_name,
                                                None,
                                                None,
                                            )
                                            .into(),
                                    )
                                    .into(),
                            )
                            .into()
                    });
                }
            }
        }

        let identifier = with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
            set_emit_flags(
                factory_
                    .create_identifier(synthetic_factory_, &symbol_name, type_parameter_nodes, None)
                    .into(),
                EmitFlags::NoAsciiEscaping,
            )
        });
        identifier.set_symbol(symbol.clone());

        if index > stopper {
            let ref lhs = self.create_access_from_symbol_chain(
                context,
                override_type_arguments,
                chain,
                index - 1,
                stopper,
            );
            if !is_entity_name(lhs) {
                Debug_.fail(Some(
                    "Impossible construct - an export of an indexed access cannot be reachable",
                ));
            }
            return with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_qualified_name(synthetic_factory_, lhs.clone(), identifier)
                    .into()
            });
        }
        identifier
    }

    pub(super) fn type_parameter_shadows_name_in_scope(
        &self,
        escaped_name: &__String,
        context: &NodeBuilderContext,
        type_: &Type, /*TypeParameter*/
    ) -> bool {
        let result = self.type_checker.resolve_name_(
            context.maybe_enclosing_declaration(),
            escaped_name,
            SymbolFlags::Type,
            None,
            Some(escaped_name.clone()),
            false,
            None,
        );
        if let Some(result) = result.as_ref() {
            if result.flags().intersects(SymbolFlags::TypeParameter)
                && matches!(
                    type_.maybe_symbol().as_ref(),
                    Some(type_symbol) if Rc::ptr_eq(
                        result,
                        type_symbol,
                    )
                )
            {
                return false;
            }
            return true;
        }
        false
    }

    pub(super) fn type_parameter_to_name(
        &self,
        type_: &Type, /*TypeParameter*/
        context: &NodeBuilderContext,
    ) -> Rc<Node> {
        if context
            .flags()
            .intersects(NodeBuilderFlags::GenerateNamesForShadowedTypeParams)
        {
            if let Some(context_type_parameter_names) =
                context.type_parameter_names.borrow().as_ref()
            {
                let cached =
                    context_type_parameter_names.get(&self.type_checker.get_type_id(type_));
                if let Some(cached) = cached {
                    return cached.clone();
                }
            }
        }
        let mut result =
            self.symbol_to_name(&type_.symbol(), context, Some(SymbolFlags::Type), true);
        // TODO: the Typescript version has & SyntaxKind.Identifier which is presumably a bug?
        if result.kind() != SyntaxKind::Identifier {
            return with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_identifier(
                        synthetic_factory_,
                        "(Missing type parameter)",
                        Option::<NodeArray>::None,
                        None,
                    )
                    .into()
            });
        }
        if context
            .flags()
            .intersects(NodeBuilderFlags::GenerateNamesForShadowedTypeParams)
        {
            let rawtext = (&*result.as_identifier().escaped_text).to_owned();
            let mut i = context
                .type_parameter_names_by_text_next_name_count
                .borrow()
                .as_ref()
                .and_then(|context_type_parameter_names_by_text_next_name_count| {
                    context_type_parameter_names_by_text_next_name_count
                        .get(&rawtext)
                        .copied()
                })
                .unwrap_or(0);
            let mut text = rawtext.clone();
            while matches!(
                context.type_parameter_names_by_text_next_name_count.borrow().as_ref(),
                Some(context_type_parameter_names_by_text_next_name_count) if context_type_parameter_names_by_text_next_name_count.contains_key(&text)
            ) || self.type_parameter_shadows_name_in_scope(
                &__String::new(text.clone()),
                context,
                type_,
            ) {
                i += 1;
                text = format!("{}_{}", rawtext, i);
            }
            if text != rawtext {
                result = with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                    factory_
                        .create_identifier(
                            synthetic_factory_,
                            &text,
                            result.as_identifier().maybe_type_arguments().clone(),
                            None,
                        )
                        .into()
                });
            }
            context
                .type_parameter_names_by_text_next_name_count
                .borrow_mut()
                .get_or_insert_with(|| HashMap::new())
                .insert(rawtext.clone(), i);
            context
                .type_parameter_names
                .borrow_mut()
                .get_or_insert_with(|| HashMap::new())
                .insert(self.type_checker.get_type_id(type_), result.clone());
            context
                .type_parameter_names_by_text
                .borrow_mut()
                .get_or_insert_with(|| HashSet::new())
                .insert(rawtext.clone());
        }
        result
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
