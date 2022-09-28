#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::NodeBuilderContext;
use crate::{
    are_option_rcs_equal, array_is_homogeneous, cast_present, create_underscore_escaped_multi_map,
    factory, get_check_flags, get_emit_script_target, get_name_from_index_info,
    get_text_of_jsdoc_comment, is_identifier, is_identifier_text, is_identifier_type_reference,
    modifiers_to_flags, set_comment_range, set_emit_flags, set_synthetic_leading_comments, some,
    synthetic_factory, unescape_leading_underscores, using_single_line_string_writer,
    with_synthetic_factory_and_factory, CheckFlags, Debug_, EmitFlags, EmitTextWriter, IndexInfo,
    ModifierFlags, Node, NodeArray, NodeBuilder, NodeBuilderFlags, NodeInterface, Signature,
    SignatureFlags, StrOrNodeArrayRef, StringOrNodeArray, Symbol, SymbolFlags, SymbolInterface,
    SymbolTable, SyntaxKind, SynthesizedComment, Type, TypeChecker, TypeFormatFlags, TypeInterface,
    TypePredicate, TypePredicateKind, UnderscoreEscapedMultiMap,
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
                if self.check_truncation_length(context) {
                    if is_bare_list != Some(true) {
                        return Some(vec![with_synthetic_factory_and_factory(
                            |synthetic_factory_, factory_| {
                                factory_
                                    .create_type_reference_node(
                                        synthetic_factory_,
                                        "...".to_owned(),
                                        Option::<NodeArray>::None,
                                    )
                                    .into()
                            },
                        )]);
                    } else if types.len() > 2 {
                        return Some(vec![
                            self.type_to_type_node_helper(Some(&*types[0]), context)
                                .unwrap(),
                            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                                factory_
                                    .create_type_reference_node(
                                        synthetic_factory_,
                                        format!("... {} more ...", types.len() - 2),
                                        Option::<NodeArray>::None,
                                    )
                                    .into()
                            }),
                            self.type_to_type_node_helper(Some(&*types[types.len() - 1]), context)
                                .unwrap(),
                        ]);
                    }
                }
                let may_have_name_collisions = !context
                    .flags()
                    .intersects(NodeBuilderFlags::UseFullyQualifiedType);
                let mut seen_names: Option<UnderscoreEscapedMultiMap<(Rc<Type>, usize)>> =
                    if may_have_name_collisions {
                        Some(create_underscore_escaped_multi_map())
                    } else {
                        None
                    };
                let mut result: Vec<Rc<Node /*TypeNode*/>> = vec![];
                let mut i = 0;
                for type_ in types {
                    i += 1;
                    if self.check_truncation_length(context) && i + 2 < types.len() - 1 {
                        result.push(with_synthetic_factory_and_factory(
                            |synthetic_factory_, factory_| {
                                factory_
                                    .create_type_reference_node(
                                        synthetic_factory_,
                                        format!("... {} more ...", types.len() - i),
                                        Option::<NodeArray>::None,
                                    )
                                    .into()
                            },
                        ));
                        let type_node =
                            self.type_to_type_node_helper(Some(&*types[types.len() - 1]), context);
                        if let Some(type_node) = type_node {
                            result.push(type_node);
                        }
                        break;
                    }
                    context.increment_approximate_length_by(2);
                    let type_node = self.type_to_type_node_helper(Some(&**type_), context);
                    if let Some(type_node) = type_node.as_ref() {
                        result.push(type_node.clone());
                        if let Some(seen_names) = seen_names.as_mut() {
                            if is_identifier_type_reference(type_node) {
                                seen_names.add(
                                    type_node
                                        .as_type_reference_node()
                                        .type_name
                                        .as_identifier()
                                        .escaped_text
                                        .clone(),
                                    (type_.clone(), result.len() - 1),
                                );
                            }
                        }
                    }
                }

                if let Some(seen_names) = seen_names {
                    let save_context_flags = context.flags.get();
                    context
                        .flags
                        .set(context.flags.get() | NodeBuilderFlags::UseFullyQualifiedType);
                    for types in seen_names.0.values() {
                        if !array_is_homogeneous(types, |(a, _), (b, _)| {
                            self.types_are_same_reference(a, b)
                        }) {
                            for (type_, result_index) in types {
                                result[*result_index] = self
                                    .type_to_type_node_helper(Some(&**type_), context)
                                    .unwrap();
                            }
                        }
                    }
                    context.flags.set(save_context_flags);
                }

                return Some(result);
            }
        }
        None
    }

    pub(super) fn types_are_same_reference(&self, a: &Type, b: &Type) -> bool {
        ptr::eq(a, b)
            || a.maybe_symbol().is_some()
                && are_option_rcs_equal(a.maybe_symbol().as_ref(), b.maybe_symbol().as_ref())
            || a.maybe_alias_symbol().is_some()
                && are_option_rcs_equal(
                    a.maybe_alias_symbol().as_ref(),
                    b.maybe_alias_symbol().as_ref(),
                )
    }

    pub(super) fn index_info_to_index_signature_declaration_helper<TTypeNode: Borrow<Node>>(
        &self,
        index_info: &IndexInfo,
        context: &NodeBuilderContext,
        type_node: Option<TTypeNode /*TypeNode*/>,
    ) -> Rc<Node /*IndexSignatureDeclaration*/> {
        let name = get_name_from_index_info(index_info)
            .unwrap_or_else(|| Cow::Borrowed("x"))
            .into_owned();
        let indexer_type_node = self.type_to_type_node_helper(Some(&*index_info.key_type), context);

        let indexing_parameter: Rc<Node> =
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_parameter_declaration(
                        synthetic_factory_,
                        Option::<NodeArray>::None,
                        Option::<NodeArray>::None,
                        None,
                        Some(name.clone()),
                        None,
                        indexer_type_node,
                        None,
                    )
                    .into()
            });
        let type_node = type_node
            .map(|type_node| type_node.borrow().node_wrapper())
            .or_else(|| {
                self.type_to_type_node_helper(Some(&*index_info.type_ /*|| anyType*/), context)
            });
        // if (!indexInfo.type && !(context.flags & NodeBuilderFlags.AllowEmptyIndexInfoType)) {
        //     context.encounteredError = true;
        // }
        context.increment_approximate_length_by(name.len() + 4);
        with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
            factory_
                .create_index_signature(
                    synthetic_factory_,
                    Option::<NodeArray>::None,
                    if index_info.is_readonly {
                        Some(vec![factory_
                            .create_token(synthetic_factory_, SyntaxKind::ReadonlyKeyword)
                            .into()])
                    } else {
                        None
                    },
                    vec![indexing_parameter],
                    type_node,
                )
                .into()
        })
    }

    pub(super) fn signature_to_signature_declaration_helper<TPrivateSymbolVisitor: Fn(&Symbol)>(
        &self,
        signature: Rc<Signature>,
        kind: SyntaxKind,
        context: &NodeBuilderContext,
        options: Option<SignatureToSignatureDeclarationOptions<TPrivateSymbolVisitor>>,
    ) -> Rc<Node /*SignatureDeclaration*/> {
        let suppress_any = context
            .flags()
            .intersects(NodeBuilderFlags::SuppressAnyReturnType);
        if suppress_any {
            context.set_flags(context.flags() & !NodeBuilderFlags::SuppressAnyReturnType);
        }
        context.increment_approximate_length_by(3);
        let mut type_parameters: Option<Vec<Rc<Node /*TypeParameterDeclaration(*/>>> = None;
        let mut type_arguments: Option<Vec<Rc<Node /*TypeNode(*/>>> = None;
        let mut passed_if_condition = false;
        if context
            .flags()
            .intersects(NodeBuilderFlags::WriteTypeArgumentsOfSignature)
        {
            if let (Some(signature_target), Some(signature_mapper)) =
                (signature.target.as_ref(), signature.mapper.as_ref())
            {
                if let Some(signature_target_type_parameters) =
                    signature_target.maybe_type_parameters().as_ref()
                {
                    passed_if_condition = true;
                    type_arguments = Some(
                        signature_target_type_parameters
                            .into_iter()
                            .map(|parameter| {
                                self.type_to_type_node_helper(
                                    Some(
                                        self.type_checker
                                            .instantiate_type(parameter, Some(signature_mapper)),
                                    ),
                                    context,
                                )
                                .unwrap()
                            })
                            .collect(),
                    );
                }
            }
        }
        if !passed_if_condition {
            type_parameters =
                signature
                    .maybe_type_parameters()
                    .as_ref()
                    .map(|signature_type_parameters| {
                        signature_type_parameters
                            .into_iter()
                            .map(|parameter| {
                                self.type_parameter_to_declaration_(parameter, context, None)
                            })
                            .collect()
                    });
        }

        let expanded_params = self
            .type_checker
            .get_expanded_parameters(&signature, Some(true))
            .into_iter()
            .next()
            .unwrap();
        let mut parameters = if some(
            Some(&*expanded_params),
            Some(|p: &Rc<Symbol>| {
                !Rc::ptr_eq(p, &expanded_params[expanded_params.len() - 1])
                    && get_check_flags(p).intersects(CheckFlags::RestParameter)
            }),
        ) {
            signature.parameters()
        } else {
            &*expanded_params
        }
        .into_iter()
        .map(|parameter| {
            self.symbol_to_parameter_declaration_(
                parameter,
                context,
                Some(kind == SyntaxKind::Constructor),
                options
                    .as_ref()
                    .and_then(|options| options.private_symbol_visitor.as_ref()),
                options.as_ref().and_then(|options| options.bundled_imports),
            )
        })
        .collect::<Vec<_>>();
        if let Some(signature_this_parameter) = signature.maybe_this_parameter().as_ref() {
            let this_parameter = self.symbol_to_parameter_declaration_(
                signature_this_parameter,
                context,
                None,
                Option::<&fn(&Symbol)>::None,
                None,
            );
            parameters.insert(0, this_parameter);
        }

        let mut return_type_node: Option<Rc<Node /*TypeNode*/>> = None;
        let type_predicate = self
            .type_checker
            .get_type_predicate_of_signature(&signature);
        if let Some(type_predicate) = type_predicate.as_ref() {
            let asserts_modifier: Option<Rc<Node>> = if matches!(
                type_predicate.kind,
                TypePredicateKind::AssertsThis | TypePredicateKind::AssertsIdentifier
            ) {
                Some(with_synthetic_factory_and_factory(
                    |synthetic_factory_, factory_| {
                        factory_
                            .create_token(synthetic_factory_, SyntaxKind::AssertsKeyword)
                            .into()
                    },
                ))
            } else {
                None
            };
            let parameter_name = if matches!(
                type_predicate.kind,
                TypePredicateKind::Identifier | TypePredicateKind::AssertsIdentifier
            ) {
                set_emit_flags(
                    with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                        factory_
                            .create_identifier(
                                synthetic_factory_,
                                type_predicate.parameter_name.as_ref().unwrap(),
                                Option::<NodeArray>::None,
                                None,
                            )
                            .into()
                    }),
                    EmitFlags::NoAsciiEscaping,
                )
            } else {
                with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                    factory_.create_this_type_node(synthetic_factory_).into()
                })
            };
            let type_node = type_predicate
                .type_
                .as_ref()
                .and_then(|type_predicate_type| {
                    self.type_to_type_node_helper(Some(&**type_predicate_type), context)
                });
            return_type_node = Some(with_synthetic_factory_and_factory(
                |synthetic_factory_, factory_| {
                    factory_
                        .create_type_predicate_node(
                            synthetic_factory_,
                            asserts_modifier,
                            parameter_name,
                            type_node,
                        )
                        .into()
                },
            ));
        } else {
            let ref return_type = self
                .type_checker
                .get_return_type_of_signature(signature.clone());
            if
            /*returnType &&*/
            !(suppress_any && self.type_checker.is_type_any(Some(&**return_type))) {
                return_type_node = Some(
                    self.serialize_return_type_for_signature(
                        context,
                        return_type,
                        &signature,
                        options
                            .as_ref()
                            .and_then(|options| options.private_symbol_visitor.as_ref()),
                        options.as_ref().and_then(|options| options.bundled_imports),
                    ),
                );
            } else if !suppress_any {
                return_type_node = Some(with_synthetic_factory_and_factory(
                    |synthetic_factory_, factory_| {
                        factory_
                            .create_keyword_type_node(synthetic_factory_, SyntaxKind::AnyKeyword)
                            .into()
                    },
                ));
            }
        }
        let mut modifiers = options
            .as_ref()
            .and_then(|options| options.modifiers.clone());
        if kind == SyntaxKind::ConstructorType
            && signature.flags.intersects(SignatureFlags::Abstract)
        {
            let flags = modifiers_to_flags(modifiers.as_deref());
            modifiers = Some(with_synthetic_factory_and_factory(
                |synthetic_factory_, factory_| {
                    factory_.create_modifiers_from_modifier_flags(
                        synthetic_factory_,
                        flags | ModifierFlags::Abstract,
                    )
                },
            ));
        }

        let node: Rc<Node> =
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| match kind {
                SyntaxKind::CallSignature => factory_
                    .create_call_signature(
                        synthetic_factory_,
                        type_parameters,
                        parameters,
                        return_type_node,
                    )
                    .into(),
                SyntaxKind::ConstructSignature => factory_
                    .create_construct_signature(
                        synthetic_factory_,
                        type_parameters,
                        parameters,
                        return_type_node,
                    )
                    .into(),
                SyntaxKind::MethodSignature => factory_
                    .create_method_signature(
                        synthetic_factory_,
                        modifiers,
                        options.as_ref().and_then(|options| options.name.clone()),
                        options
                            .as_ref()
                            .and_then(|options| options.question_token.clone()),
                        type_parameters,
                        Some(parameters),
                        return_type_node,
                    )
                    .into(),
                SyntaxKind::MethodDeclaration => factory_
                    .create_method_declaration(
                        synthetic_factory_,
                        Option::<NodeArray>::None,
                        modifiers,
                        None,
                        options
                            .as_ref()
                            .and_then(|options| options.name.clone())
                            .unwrap_or_else(|| {
                                factory_
                                    .create_identifier(
                                        synthetic_factory_,
                                        "",
                                        Option::<NodeArray>::None,
                                        None,
                                    )
                                    .into()
                            }),
                        None,
                        type_parameters,
                        parameters,
                        return_type_node,
                        None,
                    )
                    .into(),
                SyntaxKind::Constructor => factory_
                    .create_constructor_declaration(
                        synthetic_factory_,
                        Option::<NodeArray>::None,
                        modifiers,
                        parameters,
                        None,
                    )
                    .into(),
                SyntaxKind::GetAccessor => factory_
                    .create_get_accessor_declaration(
                        synthetic_factory_,
                        Option::<NodeArray>::None,
                        modifiers,
                        options
                            .as_ref()
                            .and_then(|options| options.name.clone())
                            .unwrap_or_else(|| {
                                factory_
                                    .create_identifier(
                                        synthetic_factory_,
                                        "",
                                        Option::<NodeArray>::None,
                                        None,
                                    )
                                    .into()
                            }),
                        parameters,
                        return_type_node,
                        None,
                    )
                    .into(),
                SyntaxKind::SetAccessor => factory_
                    .create_set_accessor_declaration(
                        synthetic_factory_,
                        Option::<NodeArray>::None,
                        modifiers,
                        options
                            .as_ref()
                            .and_then(|options| options.name.clone())
                            .unwrap_or_else(|| {
                                factory_
                                    .create_identifier(
                                        synthetic_factory_,
                                        "",
                                        Option::<NodeArray>::None,
                                        None,
                                    )
                                    .into()
                            }),
                        parameters,
                        None,
                    )
                    .into(),
                SyntaxKind::IndexSignature => factory_
                    .create_index_signature(
                        synthetic_factory_,
                        Option::<NodeArray>::None,
                        modifiers,
                        parameters,
                        return_type_node,
                    )
                    .into(),
                SyntaxKind::JSDocFunctionType => factory_
                    .create_jsdoc_function_type(synthetic_factory_, parameters, return_type_node)
                    .into(),
                SyntaxKind::FunctionType => factory_
                    .create_function_type_node(
                        synthetic_factory_,
                        type_parameters,
                        parameters,
                        Some(return_type_node.unwrap_or_else(|| {
                            factory_
                                .create_type_reference_node(
                                    synthetic_factory_,
                                    Into::<Rc<Node>>::into(factory_.create_identifier(
                                        synthetic_factory_,
                                        "",
                                        Option::<NodeArray>::None,
                                        None,
                                    )),
                                    Option::<NodeArray>::None,
                                )
                                .into()
                        })),
                    )
                    .into(),
                SyntaxKind::ConstructorType => factory_
                    .create_constructor_type_node(
                        synthetic_factory_,
                        modifiers,
                        type_parameters,
                        parameters,
                        Some(return_type_node.unwrap_or_else(|| {
                            factory_
                                .create_type_reference_node(
                                    synthetic_factory_,
                                    Into::<Rc<Node>>::into(factory_.create_identifier(
                                        synthetic_factory_,
                                        "",
                                        Option::<NodeArray>::None,
                                        None,
                                    )),
                                    Option::<NodeArray>::None,
                                )
                                .into()
                        })),
                    )
                    .into(),
                SyntaxKind::FunctionDeclaration => factory_
                    .create_function_declaration(
                        synthetic_factory_,
                        Option::<NodeArray>::None,
                        modifiers,
                        None,
                        Some(
                            options
                                .as_ref()
                                .and_then(|options| options.name.clone())
                                .map(|name| {
                                    cast_present(name, |name: &Rc<Node>| is_identifier(name))
                                })
                                .unwrap_or_else(|| {
                                    factory_
                                        .create_identifier(
                                            synthetic_factory_,
                                            "",
                                            Option::<NodeArray>::None,
                                            None,
                                        )
                                        .into()
                                }),
                        ),
                        type_parameters,
                        parameters,
                        return_type_node,
                        None,
                    )
                    .into(),
                SyntaxKind::FunctionExpression => factory_
                    .create_function_expression(
                        synthetic_factory_,
                        modifiers,
                        None,
                        Some(
                            options
                                .as_ref()
                                .and_then(|options| options.name.clone())
                                .map(|name| {
                                    cast_present(name, |name: &Rc<Node>| is_identifier(name))
                                })
                                .unwrap_or_else(|| {
                                    factory_
                                        .create_identifier(
                                            synthetic_factory_,
                                            "",
                                            Option::<NodeArray>::None,
                                            None,
                                        )
                                        .into()
                                }),
                        ),
                        type_parameters,
                        parameters,
                        return_type_node,
                        factory_
                            .create_block(synthetic_factory_, vec![], None)
                            .into(),
                    )
                    .into(),
                SyntaxKind::ArrowFunction => factory_
                    .create_arrow_function(
                        synthetic_factory_,
                        modifiers,
                        type_parameters,
                        parameters,
                        return_type_node,
                        None,
                        factory_
                            .create_block(synthetic_factory_, vec![], None)
                            .into(),
                    )
                    .into(),
                _ => Debug_.assert_never(kind, None),
            });

        // TODO: this looks like it's only used for appending a "nonexistent" .typeArguments
        // property to drive showing type arguments instead of type parameters (so will need to
        // extend the relevant AST node types to support mutating in a type_arguments value I
        // guess)
        // type_arguments.map(|type_arguments| {
        //     factory_.create_node_array(Some(type_arguments), None)
        // })

        node
    }

    pub(super) fn type_parameter_to_declaration_with_constraint(
        &self,
        type_: &Type, /*TypeParameter*/
        context: &NodeBuilderContext,
        constraint_node: Option<Rc<Node>>,
    ) -> Rc<Node /*TypeParameterDeclaration*/> {
        let saved_context_flags = context.flags();
        context.set_flags(context.flags() & !NodeBuilderFlags::WriteTypeParametersInQualifiedName);
        let name = self.type_parameter_to_name(type_, context);
        let default_parameter = self.type_checker.get_default_from_type_parameter_(type_);
        let default_parameter_node = default_parameter.as_ref().and_then(|default_parameter| {
            self.type_to_type_node_helper(Some(&**default_parameter), context)
        });
        context.set_flags(saved_context_flags);
        with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
            factory_
                .create_type_parameter_declaration(
                    synthetic_factory_,
                    name,
                    constraint_node,
                    default_parameter_node,
                )
                .into()
        })
    }

    pub(super) fn type_parameter_to_declaration_(
        &self,
        type_: &Type, /*TypeParameter*/
        context: &NodeBuilderContext,
        constraint: Option<Rc<Type>>,
    ) -> Rc<Node /*TypeParameterDeclaration*/> {
        let constraint =
            constraint.or_else(|| self.type_checker.get_constraint_of_type_parameter(type_));
        let constraint_node = constraint
            .as_ref()
            .and_then(|constraint| self.type_to_type_node_helper(Some(&**constraint), context));
        self.type_parameter_to_declaration_with_constraint(type_, context, constraint_node)
    }

    pub(super) fn symbol_to_parameter_declaration_<TPrivateSymbolVisitor: Fn(&Symbol)>(
        &self,
        parameter_symbol: &Symbol,
        context: &NodeBuilderContext,
        preserve_modifier_flags: Option<bool>,
        private_symbol_visitor: Option<&TPrivateSymbolVisitor>,
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
        TIncludePrivateSymbol: Fn(&Symbol),
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

pub(super) struct SignatureToSignatureDeclarationOptions<TPrivateSymbolVisitor: Fn(&Symbol)> {
    pub modifiers: Option<Vec<Rc<Node /*Modifier*/>>>,
    pub name: Option<Rc<Node /*PropertyName*/>>,
    pub question_token: Option<Rc<Node /*QuestionToken*/>>,
    pub private_symbol_visitor: Option<TPrivateSymbolVisitor>,
    pub bundled_imports: Option<bool>,
}
