#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::rc::Rc;

use super::NodeBuilderContext;
use crate::{
    factory, get_emit_script_target, get_text_of_jsdoc_comment, is_identifier_text,
    set_comment_range, set_synthetic_leading_comments, some, synthetic_factory,
    unescape_leading_underscores, using_single_line_string_writer, EmitTextWriter, IndexInfo, Node,
    NodeArray, NodeBuilder, NodeBuilderFlags, NodeInterface, Signature, StrOrNodeArrayRef,
    StringOrNodeArray, Symbol, SymbolFlags, SymbolInterface, SymbolTable, SyntaxKind,
    SynthesizedComment, Type, TypeChecker, TypeFormatFlags, TypePredicate,
};

impl NodeBuilder {
    pub(super) fn preserve_comments_on(
        &self,
        property_symbol: &Symbol,
        node: Rc<Node>,
    ) -> Rc<Node> {
        if some(
            property_symbol.maybe_declarations().as_deref(),
            Some(|d: &Rc<Node>| d.kind() == SyntaxKind::JSDocPropertyTag),
        ) {
            let d: Rc<Node> = property_symbol
                .maybe_declarations()
                .as_ref()
                .unwrap()
                .into_iter()
                .find(|d| d.kind() == SyntaxKind::JSDocPropertyTag)
                .cloned()
                .unwrap();
            let comment_text = get_text_of_jsdoc_comment(d.as_jsdoc_tag().maybe_comment().map(
                |d_comment| -> StrOrNodeArrayRef {
                    match d_comment {
                        StringOrNodeArray::String(d_comment) => (&**d_comment).into(),
                        StringOrNodeArray::NodeArray(d_comment) => d_comment.into(),
                    }
                },
            ));
            if let Some(comment_text) = comment_text
                .as_deref()
                .filter(|comment_text| !comment_text.is_empty())
            {
                set_synthetic_leading_comments(
                    &node,
                    Some(vec![Rc::new(SynthesizedComment {
                        kind: SyntaxKind::MultiLineCommentTrivia,
                        text: format!("*\n * {}\n ", comment_text.replace("\n", "\n * ")),
                        has_trailing_new_line: Some(true),
                        has_leading_new_line: None,
                    })]),
                );
            }
        } else if let Some(property_symbol_value_declaration) =
            property_symbol.maybe_value_declaration().as_ref()
        {
            set_comment_range(&node, &**property_symbol_value_declaration);
        }
        node
    }

    pub(super) fn map_to_type_nodes(
        &self,
        types: Option<&[Rc<Type>]>,
        context: &NodeBuilderContext,
        is_bare_list: Option<bool>,
    ) -> Option<Vec<Rc<Node /*TypeNode*/>>> {
        if let Some(types) = types {
            if !types.is_empty()
            /*some(types)*/
            {
                let may_have_name_collisions = !context
                    .flags()
                    .intersects(NodeBuilderFlags::UseFullyQualifiedType);
                let mut result: Vec<Rc<Node>> = vec![];
                for (i, type_) in types.iter().enumerate() {
                    let type_node = self.type_to_type_node_helper(Some(&**type_), context);
                    result.push(type_node.unwrap().into());
                }

                return Some(result);
            }
        }
        None
    }

    pub(super) fn index_info_to_index_signature_declaration_helper<TTypeNode: Borrow<Node>>(
        &self,
        index_info: &IndexInfo,
        context: &NodeBuilderContext,
        type_node: Option<TTypeNode /*TypeNode*/>,
    ) -> Rc<Node /*IndexSignatureDeclaration*/> {
        unimplemented!()
    }

    pub(super) fn signature_to_signature_declaration_helper(
        &self,
        signature: &Signature,
        kind: SyntaxKind,
        context: &NodeBuilderContext,
        options: Option<SignatureToSignatureDeclarationOptions>,
    ) -> Rc<Node /*SignatureDeclaration*/> {
        unimplemented!()
    }

    pub(super) fn type_parameter_to_declaration_with_constraint(
        &self,
        type_: &Type, /*TypeParameter*/
        context: &NodeBuilderContext,
        constraint_node: Option<Rc<Node>>,
    ) -> Rc<Node /*TypeParameterDeclaration*/> {
        unimplemented!()
    }

    pub(super) fn type_parameter_to_declaration_(
        &self,
        type_: &Type, /*TypeParameter*/
        context: &NodeBuilderContext,
        constraint: Option<Rc<Type>>,
    ) -> Rc<Node /*TypeParameterDeclaration*/> {
        let constraint =
            constraint.or_else(|| self.type_checker.get_constraint_of_type_parameter(type_));
        unimplemented!()
    }

    pub(super) fn symbol_to_parameter_declaration_<TPrivateSymbolVisitor: FnMut(&Symbol)>(
        &self,
        parameter_symbol: &Symbol,
        context: &NodeBuilderContext,
        preserve_modifier_flags: Option<bool>,
        private_symbol_visitor: Option<TPrivateSymbolVisitor>,
        bundled_imports: Option<bool>,
    ) -> Rc<Node /*ParameterDeclaration*/> {
        unimplemented!()
    }

    pub(super) fn track_computed_name<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        access_expression: &Node, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: Option<TEnclosingDeclaration>,
        context: &NodeBuilderContext,
    ) {
        unimplemented!()
    }

    pub(super) fn lookup_symbol_chain(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
    ) -> Vec<Rc<Symbol>> {
        self.lookup_symbol_chain_worker(symbol, context, meaning)
    }

    pub(super) fn lookup_symbol_chain_worker(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
    ) -> Vec<Rc<Symbol>> {
        let chain: Vec<Rc<Symbol>>;
        if false {
            unimplemented!()
        } else {
            chain = vec![symbol.symbol_wrapper()];
        }
        chain
    }

    pub(super) fn type_parameters_to_type_parameter_declarations(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
    ) -> Option<NodeArray /*<TypeParameterDeclaration>*/> {
        unimplemented!()
    }

    pub(super) fn symbol_to_entity_name_node(&self, symbol: &Symbol) -> Rc<Node /*EntityName*/> {
        unimplemented!()
    }

    pub(super) fn symbol_to_type_node(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: SymbolFlags,
        override_type_arguments: Option<&[Rc<Node /*TypeNode*/>]>,
    ) -> Rc<Node> {
        let chain = self.lookup_symbol_chain(symbol, context, Some(meaning));

        let chain_index = chain.len() - 1;
        let entity_name = self.create_access_from_symbol_chain(context, chain, chain_index, 0);
        if false {
            unimplemented!()
        } else {
            // let last_id = if is_identifier(entity_name) {
            //     entity_name
            // } else {
            //     unimplemented!()
            // };
            let last_type_args: Option<NodeArray> = None;
            synthetic_factory
                .with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_.create_type_reference_node(
                            synthetic_factory_,
                            entity_name,
                            last_type_args,
                        )
                    })
                })
                .into()
        }
    }

    pub(super) fn create_access_from_symbol_chain(
        &self,
        context: &NodeBuilderContext,
        chain: Vec<Rc<Symbol>>,
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
        let chain = self.lookup_symbol_chain(symbol, context, meaning);
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
        TIncludePrivateSymbol: FnMut(&Symbol),
    >(
        &self,
        context: &NodeBuilderContext,
        type_: &Type,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        include_private_symbol: Option<TIncludePrivateSymbol>,
        bundled: Option<bool>,
    ) -> Rc<Node> {
        let result = self.type_to_type_node_helper(Some(type_), context);
        result.unwrap()
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

pub(super) struct SignatureToSignatureDeclarationOptions {
    pub modifiers: Option<Vec<Rc<Node /*Modifier*/>>>,
    pub name: Option<Rc<Node /*PropertyName*/>>,
    pub question_token: Option<Rc<Node /*QuestionToken*/>>,
    // pub private_symbol_visitor:
    pub bundled_imports: Option<bool>,
}
