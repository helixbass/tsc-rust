#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::rc::Rc;

use super::{MappedTypeModifiers, NodeBuilderContext};
use crate::{
    factory, get_emit_script_target, is_identifier_text, set_emit_flags_unwrapped,
    synthetic_factory, unescape_leading_underscores, Debug_, EmitFlags, IndexInfo, Node, NodeArray,
    NodeBuilder, NodeBuilderFlags, NodeInterface, Signature, Symbol, SymbolFlags, SymbolInterface,
    SymbolTable, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
};

impl NodeBuilder {
    pub(super) fn conditional_type_to_type_node(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: &Type, /*ConditionalType*/
    ) -> Node {
        let type_as_conditional_type = type_.as_conditional_type();
        let check_type_node = self
            .type_to_type_node_helper(
                type_checker,
                Some(&*type_as_conditional_type.check_type),
                context,
            )
            .unwrap();
        let save_infer_type_parameters = context.infer_type_parameters.borrow().clone();
        *context.infer_type_parameters.borrow_mut() =
            type_as_conditional_type.root.infer_type_parameters.clone();
        let extends_type_node = self
            .type_to_type_node_helper(
                type_checker,
                Some(&*type_as_conditional_type.extends_type),
                context,
            )
            .unwrap();
        *context.infer_type_parameters.borrow_mut() = save_infer_type_parameters;
        let true_type_node = self.type_to_type_node_or_circularity_elision(
            type_checker,
            context,
            &type_checker.get_true_type_from_conditional_type(type_),
        );
        let false_type_node = self.type_to_type_node_or_circularity_elision(
            type_checker,
            context,
            &type_checker.get_false_type_from_conditional_type(type_),
        );
        context.increment_approximate_length_by(15);
        synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| {
                factory_
                    .create_conditional_type_node(
                        synthetic_factory_,
                        check_type_node.wrap(),
                        extends_type_node.wrap(),
                        true_type_node.wrap(),
                        false_type_node.wrap(),
                    )
                    .into()
            })
        })
    }

    pub(super) fn type_to_type_node_or_circularity_elision(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: &Type,
    ) -> Node {
        if type_.flags().intersects(TypeFlags::Union) {
            if matches!(context.visited_types.borrow().as_ref(), Some(visited_types) if visited_types.contains(&type_checker.get_type_id(type_)))
            {
                if !context
                    .flags()
                    .intersects(NodeBuilderFlags::AllowAnonymousIdentifier)
                {
                    context.encountered_error.set(true);
                    context.tracker.report_cyclic_structure_error();
                }
                return self.create_elided_information_placeholder(context);
            }
            return self.visit_and_transform_type(type_, |type_| {
                self.type_to_type_node_helper(type_checker, Some(type_), context)
                    .unwrap()
            });
        }
        self.type_to_type_node_helper(type_checker, Some(type_), context)
            .unwrap()
    }

    pub(super) fn create_mapped_type_node_from_type(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: &Type, /*MappedType*/
    ) -> Node {
        Debug_.assert(type_.flags().intersects(TypeFlags::Object), None);
        let type_as_mapped_type = type_.as_mapped_type();
        let type_declaration_as_mapped_type_node =
            type_as_mapped_type.declaration.as_mapped_type_node();
        let readonly_token: Option<Rc<Node>> = type_declaration_as_mapped_type_node
            .readonly_token
            .as_ref()
            .map(|readonly_token| {
                synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_token(synthetic_factory_, readonly_token.kind())
                            .into()
                    })
                })
            });
        let question_token: Option<Rc<Node>> = type_declaration_as_mapped_type_node
            .question_token
            .as_ref()
            .map(|question_token| {
                synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_token(synthetic_factory_, question_token.kind())
                            .into()
                    })
                })
            });
        let appropriate_constraint_type_node: Rc<Node /*TypeNode*/>;
        if type_checker.is_mapped_type_with_keyof_constraint_declaration(type_) {
            appropriate_constraint_type_node = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_type_operator_node(
                            synthetic_factory_,
                            SyntaxKind::KeyOfKeyword,
                            self.type_to_type_node_helper(
                                type_checker,
                                Some(type_checker.get_modifiers_type_from_mapped_type(type_)),
                                context,
                            )
                            .unwrap()
                            .wrap(),
                        )
                        .into()
                })
            });
        } else {
            appropriate_constraint_type_node = self
                .type_to_type_node_helper(
                    type_checker,
                    Some(type_checker.get_constraint_type_from_mapped_type(type_)),
                    context,
                )
                .unwrap()
                .wrap();
        }
        let type_parameter_node: Rc<Node> = self
            .type_parameter_to_declaration_with_constraint(
                &type_checker.get_type_parameter_from_mapped_type(type_),
                context,
                Some(appropriate_constraint_type_node),
            )
            .wrap();
        let name_type_node: Option<Rc<Node>> =
            if type_declaration_as_mapped_type_node.name_type.is_some() {
                self.type_to_type_node_helper(
                    type_checker,
                    type_checker.get_name_type_from_mapped_type(type_),
                    context,
                )
                .map(Node::wrap)
            } else {
                None
            };
        let template_type_node: Option<Rc<Node>> = self
            .type_to_type_node_helper(
                type_checker,
                Some(
                    type_checker.remove_missing_type(
                        &type_checker.get_template_type_from_mapped_type(type_),
                        type_checker
                            .get_mapped_type_modifiers(type_)
                            .intersects(MappedTypeModifiers::IncludeOptional),
                    ),
                ),
                context,
            )
            .map(Node::wrap);
        let mapped_type_node: Node = synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| {
                factory_
                    .create_mapped_type_node(
                        synthetic_factory_,
                        readonly_token,
                        type_parameter_node,
                        name_type_node,
                        question_token,
                        template_type_node,
                        Option::<NodeArray>::None,
                    )
                    .into()
            })
        });
        context.increment_approximate_length_by(10);
        set_emit_flags_unwrapped(mapped_type_node, EmitFlags::SingleLine)
    }

    pub(super) fn create_anonymous_type_node(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: &Type, /*ObjectType*/
    ) -> Node {
        let symbol = type_.maybe_symbol();
        if let Some(symbol) = symbol {
            if false {
                unimplemented!()
            } else {
                return self.visit_and_transform_type(type_, |type_| {
                    self.create_type_node_from_object_type(type_checker, context, type_)
                });
            }
        } else {
            unimplemented!()
        }
    }

    pub(super) fn visit_and_transform_type<TTransform: FnMut(&Type) -> Node>(
        &self,
        type_: &Type,
        mut transform: TTransform,
    ) -> Node {
        let result = transform(type_);
        result
    }

    pub(super) fn create_type_node_from_object_type(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: &Type, /*ObjectType*/
    ) -> Node {
        let resolved = type_checker.resolve_structured_type_members(type_);

        let members = self.create_type_nodes_from_resolved_type(type_checker, context, &resolved);
        let type_literal_node = synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| factory_.create_type_literal_node(synthetic_factory_, members))
        });
        type_literal_node.into()
    }

    pub(super) fn type_reference_to_type_node(&self, type_: &Type /*TypeReference*/) -> Node {
        unimplemented!()
    }

    pub(super) fn append_reference_to_type(
        &self,
        root: &Node, /*TypeReferenceNode | ImportTypeNode*/
        ref_: &Node, /*TypeReferenceNode*/
    ) -> Node /*TypeReferenceNode | ImportTypeNode*/ {
        unimplemented!()
    }

    pub(super) fn create_type_nodes_from_resolved_type(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        resolved_type: &Type, /*ResolvedType*/
    ) -> Option<Vec<Rc<Node /*TypeElement*/>>> {
        let mut type_elements: Vec<Rc<Node>> = vec![];

        let properties = resolved_type.as_resolved_type().properties();

        for property_symbol in &*properties {
            self.add_property_to_element_list(
                type_checker,
                &property_symbol,
                context,
                &mut type_elements,
            );
        }
        if !type_elements.is_empty() {
            Some(type_elements)
        } else {
            None
        }
    }

    pub(super) fn create_elided_information_placeholder(
        &self,
        context: &NodeBuilderContext,
    ) -> Node {
        unimplemented!()
    }

    pub(super) fn add_property_to_element_list(
        &self,
        type_checker: &TypeChecker,
        property_symbol: &Symbol,
        context: &NodeBuilderContext,
        type_elements: &mut Vec<Rc<Node /*TypeElement*/>>,
    ) {
        let property_type = if false {
            unimplemented!()
        } else {
            type_checker.get_non_missing_type_of_symbol(property_symbol)
        };
        let property_name =
            self.get_property_name_node_for_symbol(type_checker, property_symbol, context);
        let optional_token = if property_symbol.flags().intersects(SymbolFlags::Optional) {
            synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Some(factory_.create_token(synthetic_factory_, SyntaxKind::QuestionToken))
                })
            })
        } else {
            None
        };
        if false {
            unimplemented!()
        } else {
            let property_type_node: Node;
            if false {
                unimplemented!()
            } else {
                property_type_node = if true {
                    self.serialize_type_for_declaration(
                        type_checker,
                        context,
                        &property_type,
                        property_symbol,
                    )
                } else {
                    unimplemented!()
                };
            }

            let modifiers = if false {
                unimplemented!()
            } else {
                Option::<NodeArray>::None
            };
            let property_signature = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_.create_property_signature(
                        synthetic_factory_,
                        modifiers,
                        property_name,
                        optional_token.map(Into::into),
                        Some(property_type_node.wrap()),
                    )
                })
            });

            type_elements.push(property_signature.into());
        }
    }

    pub(super) fn map_to_type_nodes(
        &self,
        type_checker: &TypeChecker,
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
                    let type_node =
                        self.type_to_type_node_helper(type_checker, Some(&**type_), context);
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
    ) -> Node /*IndexSignatureDeclaration*/ {
        unimplemented!()
    }

    pub(super) fn signature_to_signature_declaration_helper(
        &self,
        signature: &Signature,
        kind: SyntaxKind,
        context: &NodeBuilderContext,
        options: Option<SignatureToSignatureDeclarationOptions>,
    ) -> Node /*SignatureDeclaration*/ {
        unimplemented!()
    }

    pub(super) fn type_parameter_to_declaration_with_constraint(
        &self,
        type_: &Type, /*TypeParameter*/
        context: &NodeBuilderContext,
        constraint_node: Option<Rc<Node>>,
    ) -> Node /*TypeParameterDeclaration*/ {
        unimplemented!()
    }

    pub(super) fn type_parameter_to_declaration_(
        &self,
        type_checker: &TypeChecker,
        type_: &Type, /*TypeParameter*/
        context: &NodeBuilderContext,
        constraint: Option<Rc<Type>>,
    ) -> Node /*TypeParameterDeclaration*/ {
        let constraint =
            constraint.or_else(|| type_checker.get_constraint_of_type_parameter(type_));
        unimplemented!()
    }

    pub(super) fn symbol_to_parameter_declaration_<TPrivateSymbolVisitor: FnMut(&Symbol)>(
        &self,
        parameter_symbol: &Symbol,
        context: &NodeBuilderContext,
        preserve_modifier_flags: Option<bool>,
        private_symbol_visitor: Option<TPrivateSymbolVisitor>,
        bundled_imports: Option<bool>,
    ) -> Node /*ParameterDeclaration*/ {
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

    pub(super) fn symbol_to_entity_name_node(&self, symbol: &Symbol) -> Node /*EntityName*/ {
        unimplemented!()
    }

    pub(super) fn symbol_to_type_node(
        &self,
        type_checker: &TypeChecker,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: SymbolFlags,
        override_type_arguments: Option<&[Rc<Node /*TypeNode*/>]>,
    ) -> Node {
        let chain = self.lookup_symbol_chain(symbol, context, Some(meaning));

        let chain_index = chain.len() - 1;
        let entity_name =
            self.create_access_from_symbol_chain(type_checker, context, chain, chain_index, 0);
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
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        chain: Vec<Rc<Symbol>>,
        index: usize,
        stopper: usize,
    ) -> Rc<Node> {
        let type_parameter_nodes = Option::<NodeArray>::None; // TODO: this is wrong
        let symbol = chain[index].clone();

        let mut symbol_name: Option<Cow<'static, str>>;
        if index == 0 {
            symbol_name = Some(type_checker.get_name_of_symbol_as_written(&symbol, Some(context)));
        } else {
            unimplemented!()
        }
        if symbol_name.is_none() {
            symbol_name = Some(type_checker.get_name_of_symbol_as_written(&symbol, Some(context)));
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
    ) -> Node {
        unimplemented!()
    }

    pub(super) fn symbol_to_name(
        &self,
        type_checker: &TypeChecker,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        expects_identifier: bool,
    ) -> Node /*EntityName*/ {
        unimplemented!()
    }

    pub(super) fn symbol_to_expression_(
        &self,
        type_checker: &TypeChecker,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
    ) -> Node {
        let chain = self.lookup_symbol_chain(symbol, context, meaning);
        let index = chain.len() - 1;
        self.create_expression_from_symbol_chain(type_checker, context, chain, index)
    }

    pub(super) fn get_property_name_node_for_symbol(
        &self,
        type_checker: &TypeChecker,
        symbol: &Symbol,
        context: &NodeBuilderContext,
    ) -> Rc<Node> {
        let single_quote = false;
        let string_named = false;
        let raw_name = unescape_leading_underscores(symbol.escaped_name());
        self.create_property_name_node_for_identifier_or_literal(
            type_checker,
            raw_name,
            Some(string_named),
            Some(single_quote),
        )
    }

    pub(super) fn create_property_name_node_for_identifier_or_literal(
        &self,
        type_checker: &TypeChecker,
        name: String,
        string_named: Option<bool>,
        single_quote: Option<bool>,
    ) -> Rc<Node> {
        if is_identifier_text(
            &name,
            Some(get_emit_script_target(&type_checker.compiler_options)),
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
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        chain: Vec<Rc<Symbol>>,
        index: usize,
    ) -> Node {
        let type_parameter_nodes = Option::<NodeArray>::None; // TODO: this is wrong
        let symbol = &*(&chain)[index];

        let symbol_name = type_checker.get_name_of_symbol_as_written(symbol, Some(context));

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

    pub(super) fn serialize_type_for_declaration(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: &Type,
        symbol: &Symbol,
    ) -> Node {
        let result = self.type_to_type_node_helper(type_checker, Some(type_), context);
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

pub(super) struct SignatureToSignatureDeclarationOptions {}
