use std::{
    borrow::{Borrow, Cow},
    cmp::Ordering,
    convert::TryInto,
    io, ptr,
    rc::Rc,
};

use id_arena::Id;

use super::{ambient_module_symbol_regex, get_symbol_id, NodeBuilderContext, TypeFacts};
use crate::{
    array_is_homogeneous, cast_present, create_underscore_escaped_multi_map,
    first, get_check_flags, get_declaration_of_kind, get_emit_module_resolution_kind, get_factory,
    get_first_identifier, get_name_from_index_info, get_non_augmentation_declaration,
    get_original_node, get_source_file_of_node, get_text_of_jsdoc_comment, is_ambient_module,
    is_binding_element, is_computed_property_name, is_entity_name, is_identifier,
    is_identifier_type_reference, is_indexed_access_type_node, is_jsdoc_parameter_tag,
    is_rest_parameter, is_transient_symbol, length, maybe_filter, maybe_for_each_bool,
    modifiers_to_flags, module_specifiers, node_is_synthesized, null_transformation_context,
    out_file, path_is_relative, set_comment_range, set_emit_flags, set_synthetic_leading_comments,
    some, symbol_name, try_maybe_first_defined, try_maybe_map, try_visit_each_child,
    unescape_leading_underscores, CheckFlags, CompilerOptions, Debug_, EmitFlags,
    GetOrInsertDefault, HasArena, HasInitializerInterface, InArena, IndexInfo, InternalSymbolName,
    ModifierFlags, ModuleResolutionKind, NamedDeclarationInterface, Node, NodeArray, NodeBuilder,
    NodeBuilderFlags, NodeInterface, OptionTry, Signature, SignatureFlags, StrOrNodeArray,
    StrOrRcNode, StringOrNodeArray, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    SynthesizedComment, TransientSymbolInterface, Type, TypeInterface, TypePredicateKind,
    UnderscoreEscapedMultiMap, UserPreferencesBuilder,
};

impl NodeBuilder {
    pub(super) fn preserve_comments_on(
        &self,
        property_symbol: Id<Symbol>,
        node: Id<Node>,
    ) -> Id<Node> {
        if some(
            property_symbol.ref_(self).maybe_declarations().as_deref(),
            Some(|d: &Id<Node>| d.ref_(self).kind() == SyntaxKind::JSDocPropertyTag),
        ) {
            let d: Id<Node> = property_symbol
                .ref_(self)
                .maybe_declarations()
                .as_ref()
                .unwrap()
                .into_iter()
                .find(|d| d.ref_(self).kind() == SyntaxKind::JSDocPropertyTag)
                .copied()
                .unwrap();
            let d_ref = d.ref_(self);
            let comment_text = get_text_of_jsdoc_comment(d_ref.as_jsdoc_tag().maybe_comment().map(
                |d_comment| -> StrOrNodeArray {
                    match d_comment {
                        StringOrNodeArray::String(d_comment) => (&**d_comment).into(),
                        StringOrNodeArray::NodeArray(d_comment) => d_comment.clone().into(),
                    }
                },
            ), self);
            if let Some(comment_text) = comment_text
                .as_deref()
                .filter(|comment_text| !comment_text.is_empty())
            {
                set_synthetic_leading_comments(
                    node,
                    Some(vec![Rc::new(SynthesizedComment {
                        kind: SyntaxKind::MultiLineCommentTrivia,
                        text: format!("*\n * {}\n ", comment_text.replace("\n", "\n * ")),
                        has_trailing_new_line: Some(true),
                        has_leading_new_line: None,
                    })]),
                    self,
                );
            }
        } else if let Some(property_symbol_value_declaration) = property_symbol
            .ref_(self)
            .maybe_value_declaration()
            .as_ref()
        {
            set_comment_range(node, &*property_symbol_value_declaration.ref_(self), self);
        }
        node
    }

    pub(super) fn map_to_type_nodes(
        &self,
        types: Option<&[Id<Type>]>,
        context: &NodeBuilderContext,
        is_bare_list: Option<bool>,
    ) -> io::Result<Option<Vec<Id<Node /*TypeNode*/>>>> {
        if let Some(types) = types {
            if !types.is_empty()
            /*some(types)*/
            {
                if self.check_truncation_length(context) {
                    if is_bare_list != Some(true) {
                        return Ok(Some(vec![get_factory(self).create_type_reference_node(
                            "...",
                            Option::<Id<NodeArray>>::None,
                        )]));
                    } else if types.len() > 2 {
                        return Ok(Some(vec![
                            self.type_to_type_node_helper(Some(types[0]), context)?
                                .unwrap(),
                            get_factory(self).create_type_reference_node(
                                &*format!("... {} more ...", types.len() - 2),
                                Option::<Id<NodeArray>>::None,
                            ),
                            self.type_to_type_node_helper(Some(types[types.len() - 1]), context)?
                                .unwrap(),
                        ]));
                    }
                }
                let may_have_name_collisions = !context
                    .flags()
                    .intersects(NodeBuilderFlags::UseFullyQualifiedType);
                let mut seen_names: Option<UnderscoreEscapedMultiMap<(Id<Type>, usize)>> =
                    if may_have_name_collisions {
                        Some(create_underscore_escaped_multi_map())
                    } else {
                        None
                    };
                let mut result: Vec<Id<Node /*TypeNode*/>> = vec![];
                let mut i = 0;
                for &type_ in types {
                    i += 1;
                    if self.check_truncation_length(context) && i + 2 < types.len() - 1 {
                        result.push(get_factory(self).create_type_reference_node(
                            &*format!("... {} more ...", types.len() - i),
                            Option::<Id<NodeArray>>::None,
                        ));
                        let type_node =
                            self.type_to_type_node_helper(Some(types[types.len() - 1]), context)?;
                        if let Some(type_node) = type_node {
                            result.push(type_node);
                        }
                        break;
                    }
                    context.increment_approximate_length_by(2);
                    let type_node = self.type_to_type_node_helper(Some(type_), context)?;
                    if let Some(type_node) = type_node {
                        result.push(type_node);
                        if let Some(seen_names) = seen_names.as_mut() {
                            if is_identifier_type_reference(type_node, self) {
                                seen_names.add(
                                    type_node
                                        .ref_(self).as_type_reference_node()
                                        .type_name
                                        .ref_(self).as_identifier()
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
                            self.types_are_same_reference(*a, *b)
                        }) {
                            for (type_, result_index) in types {
                                result[*result_index] = self
                                    .type_to_type_node_helper(Some(*type_), context)?
                                    .unwrap();
                            }
                        }
                    }
                    context.flags.set(save_context_flags);
                }

                return Ok(Some(result));
            }
        }
        Ok(None)
    }

    pub(super) fn types_are_same_reference(&self, a: Id<Type>, b: Id<Type>) -> bool {
        a == b
            || a.ref_(self).maybe_symbol().is_some()
                && a.ref_(self).maybe_symbol() == b.ref_(self).maybe_symbol()
            || a.ref_(self).maybe_alias_symbol().is_some()
                && a.ref_(self).maybe_alias_symbol() == b.ref_(self).maybe_alias_symbol()
    }

    pub(super) fn index_info_to_index_signature_declaration_helper(
        &self,
        index_info: Id<IndexInfo>,
        context: &NodeBuilderContext,
        type_node: Option<Id<Node> /*TypeNode*/>,
    ) -> io::Result<Id<Node /*IndexSignatureDeclaration*/>> {
        let name = get_name_from_index_info(index_info, self).unwrap_or_else(|| Cow::Borrowed("x"));
        let indexer_type_node =
            self.type_to_type_node_helper(Some(index_info.ref_(self).key_type), context)?;

        let indexing_parameter = get_factory(self).create_parameter_declaration(
            Option::<Id<NodeArray>>::None,
            Option::<Id<NodeArray>>::None,
            None,
            Some(&*name),
            None,
            indexer_type_node,
            None,
        );
        let type_node = type_node
            .try_or_else(|| {
                self.type_to_type_node_helper(Some(index_info.ref_(self).type_ /*|| anyType*/), context)
            })?;
        // if (!indexInfo.type && !(context.flags & NodeBuilderFlags.AllowEmptyIndexInfoType)) {
        //     context.encounteredError = true;
        // }
        context.increment_approximate_length_by(name.len() + 4);
        Ok(get_factory(self).create_index_signature(
            Option::<Id<NodeArray>>::None,
            if index_info.ref_(self).is_readonly {
                Some(vec![get_factory(self).create_token(SyntaxKind::ReadonlyKeyword)])
            } else {
                None
            },
            vec![indexing_parameter],
            type_node,
        ))
    }

    pub(super) fn signature_to_signature_declaration_helper(
        &self,
        signature: Id<Signature>,
        kind: SyntaxKind,
        context: &NodeBuilderContext,
        options: Option<SignatureToSignatureDeclarationOptions<impl Fn(Id<Symbol>)>>,
    ) -> io::Result<Id<Node /*SignatureDeclaration*/>> {
        let suppress_any = context
            .flags()
            .intersects(NodeBuilderFlags::SuppressAnyReturnType);
        if suppress_any {
            context.set_flags(context.flags() & !NodeBuilderFlags::SuppressAnyReturnType);
        }
        context.increment_approximate_length_by(3);
        let mut type_parameters: Option<Vec<Id<Node /*TypeParameterDeclaration(*/>>> = None;
        let mut _type_arguments: Option<Vec<Id<Node /*TypeNode(*/>>> = None;
        let mut passed_if_condition = false;
        if context
            .flags()
            .intersects(NodeBuilderFlags::WriteTypeArgumentsOfSignature)
        {
            if let (Some(signature_target), Some(signature_mapper)) =
                (signature.ref_(self).target, signature.ref_(self).mapper.clone())
            {
                if let Some(signature_target_type_parameters) =
                    signature_target.ref_(self).maybe_type_parameters().as_ref()
                {
                    passed_if_condition = true;
                    _type_arguments = Some(
                        signature_target_type_parameters
                            .into_iter()
                            .map(|&parameter| {
                                Ok(self
                                    .type_to_type_node_helper(
                                        Some(self.type_checker.ref_(self).instantiate_type(
                                            parameter,
                                            Some(signature_mapper.clone()),
                                        )?),
                                        context,
                                    )?
                                    .unwrap())
                            })
                            .collect::<io::Result<Vec<_>>>()?,
                    );
                }
            }
        }
        if !passed_if_condition {
            type_parameters = signature.ref_(self).maybe_type_parameters().as_ref().try_map(
                |signature_type_parameters| {
                    signature_type_parameters
                        .into_iter()
                        .map(|&parameter| {
                            self.type_parameter_to_declaration_(parameter, context, None)
                        })
                        .collect::<io::Result<Vec<_>>>()
                },
            )?;
        }

        let expanded_params = self
            .type_checker
            .ref_(self).get_expanded_parameters(signature, Some(true))?
            .into_iter()
            .next()
            .unwrap();
        let signature_ref = signature.ref_(self);
        let mut parameters = if some(
            Some(&*expanded_params),
            Some(|&p: &Id<Symbol>| {
                p != expanded_params[expanded_params.len() - 1]
                    && get_check_flags(&p.ref_(self)).intersects(CheckFlags::RestParameter)
            }),
        ) {
            signature_ref.parameters()
        } else {
            &*expanded_params
        }
        .into_iter()
        .map(|&parameter| {
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
        .collect::<io::Result<Vec<_>>>()?;
        if let Some(signature_this_parameter) = *signature.ref_(self).maybe_this_parameter() {
            let this_parameter = self.symbol_to_parameter_declaration_(
                signature_this_parameter,
                context,
                None,
                Option::<&fn(Id<Symbol>)>::None,
                None,
            )?;
            parameters.insert(0, this_parameter);
        }

        let mut return_type_node: Option<Id<Node /*TypeNode*/>> = None;
        let type_predicate = self
            .type_checker
            .ref_(self).get_type_predicate_of_signature(signature)?;
        if let Some(type_predicate) = type_predicate.as_ref() {
            let asserts_modifier: Option<Id<Node>> = if matches!(
                type_predicate.ref_(self).kind,
                TypePredicateKind::AssertsThis | TypePredicateKind::AssertsIdentifier
            ) {
                Some(get_factory(self).create_token(SyntaxKind::AssertsKeyword))
            } else {
                None
            };
            let parameter_name = if matches!(
                type_predicate.ref_(self).kind,
                TypePredicateKind::Identifier | TypePredicateKind::AssertsIdentifier
            ) {
                set_emit_flags(
                    get_factory(self)
                        .create_identifier(type_predicate.ref_(self).parameter_name.as_ref().unwrap()),
                    EmitFlags::NoAsciiEscaping,
                    self,
                )
            } else {
                get_factory(self).create_this_type_node()
            };
            let type_node = type_predicate.ref_(self).type_.try_and_then(|type_predicate_type| {
                self.type_to_type_node_helper(Some(type_predicate_type), context)
            })?;
            return_type_node = Some(get_factory(self).create_type_predicate_node(
                asserts_modifier,
                parameter_name,
                type_node,
            ));
        } else {
            let return_type = self
                .type_checker
                .ref_(self).get_return_type_of_signature(signature.clone())?;
            if
            /*returnType &&*/
            !(suppress_any && self.type_checker.ref_(self).is_type_any(Some(return_type))) {
                return_type_node = Some(
                    self.serialize_return_type_for_signature(
                        context,
                        return_type,
                        signature,
                        options
                            .as_ref()
                            .and_then(|options| options.private_symbol_visitor.as_ref()),
                        options.as_ref().and_then(|options| options.bundled_imports),
                    )?,
                );
            } else if !suppress_any {
                return_type_node =
                    Some(get_factory(self).create_keyword_type_node(SyntaxKind::AnyKeyword));
            }
        }
        let mut modifiers = options
            .as_ref()
            .and_then(|options| options.modifiers.clone());
        if kind == SyntaxKind::ConstructorType
            && signature.ref_(self).flags.intersects(SignatureFlags::Abstract)
        {
            let flags = modifiers_to_flags(modifiers.as_deref(), self);
            modifiers = Some(
                get_factory(self).create_modifiers_from_modifier_flags(flags | ModifierFlags::Abstract),
            );
        }

        let node: Id<Node> = match kind {
            SyntaxKind::CallSignature => {
                get_factory(self).create_call_signature(type_parameters, parameters, return_type_node)
            }
            SyntaxKind::ConstructSignature => get_factory(self).create_construct_signature(
                type_parameters,
                parameters,
                return_type_node,
            ),
            SyntaxKind::MethodSignature => get_factory(self).create_method_signature(
                modifiers,
                options.as_ref().and_then(|options| options.name.clone()),
                options
                    .as_ref()
                    .and_then(|options| options.question_token.clone()),
                type_parameters,
                Some(parameters),
                return_type_node,
            ),
            SyntaxKind::MethodDeclaration => get_factory(self).create_method_declaration(
                Option::<Id<NodeArray>>::None,
                modifiers,
                None,
                options
                    .as_ref()
                    .and_then(|options| options.name.clone())
                    .unwrap_or_else(|| get_factory(self).create_identifier("")),
                None,
                type_parameters,
                parameters,
                return_type_node,
                None,
            ),
            SyntaxKind::Constructor => get_factory(self).create_constructor_declaration(
                Option::<Id<NodeArray>>::None,
                modifiers,
                Some(parameters),
                None,
            ),
            SyntaxKind::GetAccessor => get_factory(self).create_get_accessor_declaration(
                Option::<Id<NodeArray>>::None,
                modifiers,
                options
                    .as_ref()
                    .and_then(|options| options.name.clone())
                    .unwrap_or_else(|| get_factory(self).create_identifier("")),
                parameters,
                return_type_node,
                None,
            ),
            SyntaxKind::SetAccessor => get_factory(self).create_set_accessor_declaration(
                Option::<Id<NodeArray>>::None,
                modifiers,
                options
                    .as_ref()
                    .and_then(|options| options.name.clone())
                    .unwrap_or_else(|| get_factory(self).create_identifier("")),
                parameters,
                None,
            ),
            SyntaxKind::IndexSignature => get_factory(self).create_index_signature(
                Option::<Id<NodeArray>>::None,
                modifiers,
                parameters,
                return_type_node,
            ),
            SyntaxKind::JSDocFunctionType => {
                get_factory(self).create_jsdoc_function_type(parameters, return_type_node)
            }
            SyntaxKind::FunctionType => get_factory(self).create_function_type_node(
                type_parameters,
                parameters,
                Some(return_type_node.unwrap_or_else(|| {
                    get_factory(self).create_type_reference_node(
                        get_factory(self).create_identifier(""),
                        Option::<Id<NodeArray>>::None,
                    )
                })),
            ),
            SyntaxKind::ConstructorType => get_factory(self).create_constructor_type_node(
                modifiers,
                type_parameters,
                parameters,
                Some(return_type_node.unwrap_or_else(|| {
                    get_factory(self).create_type_reference_node(
                        get_factory(self).create_identifier(""),
                        Option::<Id<NodeArray>>::None,
                    )
                })),
            ),
            SyntaxKind::FunctionDeclaration => get_factory(self).create_function_declaration(
                Option::<Id<NodeArray>>::None,
                modifiers,
                None,
                Some(
                    options
                        .as_ref()
                        .and_then(|options| options.name)
                        .map(|name| cast_present(name, |name: &Id<Node>| is_identifier(&name.ref_(self))))
                        .unwrap_or_else(|| get_factory(self).create_identifier("")),
                ),
                type_parameters,
                parameters,
                return_type_node,
                None,
            ),
            SyntaxKind::FunctionExpression => get_factory(self).create_function_expression(
                modifiers,
                None,
                Some(
                    options
                        .as_ref()
                        .and_then(|options| options.name)
                        .map(|name| cast_present(name, |name: &Id<Node>| is_identifier(&name.ref_(self))))
                        .unwrap_or_else(|| get_factory(self).create_identifier("")),
                ),
                type_parameters,
                Some(parameters),
                return_type_node,
                get_factory(self).create_block(vec![], None),
            ),
            SyntaxKind::ArrowFunction => get_factory(self).create_arrow_function(
                modifiers,
                type_parameters,
                parameters,
                return_type_node,
                None,
                get_factory(self).create_block(vec![], None),
            ),
            _ => Debug_.assert_never(kind, None),
        };

        // TODO: this looks like it's only used for appending a "nonexistent" .typeArguments
        // property to drive showing type arguments instead of type parameters (so will need to
        // extend the relevant AST node types to support mutating in a type_arguments value I
        // guess)
        // type_arguments.map(|type_arguments| {
        //     get_factory(self).create_node_array(Some(type_arguments), None)
        // })

        Ok(node)
    }

    pub(super) fn type_parameter_to_declaration_with_constraint(
        &self,
        type_: Id<Type>, /*TypeParameter*/
        context: &NodeBuilderContext,
        constraint_node: Option<Id<Node>>,
    ) -> io::Result<Id<Node /*TypeParameterDeclaration*/>> {
        let saved_context_flags = context.flags();
        context.set_flags(context.flags() & !NodeBuilderFlags::WriteTypeParametersInQualifiedName);
        let name = self.type_parameter_to_name(type_, context)?;
        let default_parameter = self.type_checker.ref_(self).get_default_from_type_parameter_(type_)?;
        let default_parameter_node = default_parameter.try_and_then(|default_parameter| {
            self.type_to_type_node_helper(Some(default_parameter), context)
        })?;
        context.set_flags(saved_context_flags);
        Ok(get_factory(self).create_type_parameter_declaration(
            name,
            constraint_node,
            default_parameter_node,
        ))
    }

    pub(super) fn type_parameter_to_declaration_(
        &self,
        type_: Id<Type>, /*TypeParameter*/
        context: &NodeBuilderContext,
        constraint: Option<Id<Type>>,
    ) -> io::Result<Id<Node /*TypeParameterDeclaration*/>> {
        let constraint =
            constraint.try_or_else(|| self.type_checker.ref_(self).get_constraint_of_type_parameter(type_))?;
        let constraint_node = constraint
            .try_and_then(|constraint| self.type_to_type_node_helper(Some(constraint), context))?;
        self.type_parameter_to_declaration_with_constraint(type_, context, constraint_node)
    }

    pub(super) fn symbol_to_parameter_declaration_(
        &self,
        parameter_symbol: Id<Symbol>,
        context: &NodeBuilderContext,
        preserve_modifier_flags: Option<bool>,
        private_symbol_visitor: Option<&impl Fn(Id<Symbol>)>,
        bundled_imports: Option<bool>,
    ) -> io::Result<Id<Node /*ParameterDeclaration*/>> {
        let mut parameter_declaration: Option<
            Id<Node /*ParameterDeclaration | JSDocParameterTag*/>,
        > = get_declaration_of_kind(parameter_symbol, SyntaxKind::Parameter, self);
        if parameter_declaration.is_none() && !is_transient_symbol(&parameter_symbol.ref_(self)) {
            parameter_declaration = get_declaration_of_kind(
                parameter_symbol,
                SyntaxKind::JSDocParameterTag,
                self,
            );
        }

        let mut parameter_type = self.type_checker.ref_(self).get_type_of_symbol(parameter_symbol)?;
        if matches!(
            parameter_declaration,
            Some(parameter_declaration) if self.type_checker.ref_(self).is_required_initialized_parameter(parameter_declaration)?
        ) {
            parameter_type = self.type_checker.ref_(self).get_optional_type_(parameter_type, None)?;
        }
        if context
            .flags()
            .intersects(NodeBuilderFlags::NoUndefinedOptionalParameterType)
            && matches!(
                parameter_declaration,
                Some(parameter_declaration) if !is_jsdoc_parameter_tag(&parameter_declaration.ref_(self)) &&
                    self.type_checker.ref_(self).is_optional_uninitialized_parameter_(parameter_declaration)?
            )
        {
            parameter_type = self
                .type_checker
                .ref_(self).get_type_with_facts(parameter_type, TypeFacts::NEUndefined)?;
        }
        let parameter_type_node = self.serialize_type_for_declaration(
            context,
            parameter_type,
            parameter_symbol,
            context.maybe_enclosing_declaration(),
            private_symbol_visitor,
            bundled_imports,
        )?;

        let modifiers: Option<Vec<Id<Node>>> = if !context
            .flags()
            .intersects(NodeBuilderFlags::OmitParameterModifiers)
            && preserve_modifier_flags == Some(true)
        {
            parameter_declaration
                .and_then(|parameter_declaration| {
                    parameter_declaration.ref_(self).maybe_modifiers().map(
                        |parameter_declaration_modifiers| {
                            parameter_declaration_modifiers
                                .ref_(self).into_iter()
                                .map(|&modifier| get_factory(self).clone_node(modifier))
                                .collect()
                        },
                    )
                })
        } else {
            None
        };
        let is_rest = matches!(
            parameter_declaration,
            Some(parameter_declaration) if is_rest_parameter(parameter_declaration, self)
        ) || get_check_flags(&parameter_symbol.ref_(self))
            .intersects(CheckFlags::RestParameter);
        let dot_dot_dot_token: Option<Id<Node>> = if is_rest {
            Some(get_factory(self).create_token(SyntaxKind::DotDotDotToken))
        } else {
            None
        };
        let parameter_symbol_name: Option<String>;
        let name: StrOrRcNode<'_> =
            if let Some(parameter_declaration) = parameter_declaration {
                if let Some(parameter_declaration_name) = parameter_declaration
                    .ref_(self).as_named_declaration()
                    .maybe_name()
                {
                    match parameter_declaration_name.ref_(self).kind() {
                        SyntaxKind::Identifier => set_emit_flags(
                            get_factory(self).clone_node(parameter_declaration_name),
                            EmitFlags::NoAsciiEscaping,
                            self,
                        ),
                        SyntaxKind::QualifiedName => set_emit_flags(
                            get_factory(self)
                                .clone_node(parameter_declaration_name.ref_(self).as_qualified_name().right),
                            EmitFlags::NoAsciiEscaping,
                            self,
                        ),
                        _ => self.clone_binding_name(context, parameter_declaration_name)?,
                    }
                    .into()
                } else {
                    parameter_symbol_name = Some(symbol_name(parameter_symbol, self));
                    parameter_symbol_name.as_deref().unwrap().into()
                }
            } else {
                parameter_symbol_name = Some(symbol_name(parameter_symbol, self));
                parameter_symbol_name.as_deref().unwrap().into()
            };
        let is_optional = matches!(
            parameter_declaration,
            Some(parameter_declaration) if self.type_checker.ref_(self).is_optional_parameter_(parameter_declaration)?
        ) || get_check_flags(&parameter_symbol.ref_(self))
            .intersects(CheckFlags::OptionalParameter);
        let question_token = if is_optional {
            Some(get_factory(self).create_token(SyntaxKind::QuestionToken))
        } else {
            None
        };
        let parameter_node = get_factory(self).create_parameter_declaration(
            Option::<Id<NodeArray>>::None,
            modifiers,
            dot_dot_dot_token,
            Some(name),
            question_token,
            Some(parameter_type_node),
            None,
        );
        context
            .increment_approximate_length_by(symbol_name(parameter_symbol, self).len() + 3);
        Ok(parameter_node)
    }

    pub(super) fn clone_binding_name(
        &self,
        context: &NodeBuilderContext,
        node: Id<Node>, /*BindingName*/
    ) -> io::Result<Id<Node /*BIndingName*/>> {
        self.elide_initializer_and_set_emit_flags(context, node)
    }

    pub(super) fn elide_initializer_and_set_emit_flags(
        &self,
        context: &NodeBuilderContext,
        node: Id<Node>,
    ) -> io::Result<Id<Node>> {
        if context.tracker_ref().is_track_symbol_supported()
            && is_computed_property_name(&node.ref_(self))
            && self.type_checker.ref_(self).is_late_bindable_name(node)?
        {
            self.track_computed_name(
                node.ref_(self).as_computed_property_name().expression,
                context.maybe_enclosing_declaration(),
                context,
            )?;
        }
        let mut visited = try_visit_each_child(
            node,
            |node: Id<Node>| -> io::Result<_> {
                Ok(Some(
                    self.elide_initializer_and_set_emit_flags(context, node)?
                        .into(),
                ))
            },
            &*null_transformation_context,
            self,
        )?;
        if is_binding_element(&visited.ref_(self)) {
            let visited_ref = visited.ref_(self);
            let visited_as_binding_element = visited_ref.as_binding_element();
            visited = get_factory(self).update_binding_element(
                visited,
                visited_as_binding_element.dot_dot_dot_token,
                visited_as_binding_element.property_name,
                visited_as_binding_element.name(),
                visited_as_binding_element.maybe_initializer(),
            );
        }
        if !node_is_synthesized(&*visited.ref_(self)) {
            visited = get_factory(self).clone_node(visited);
        }
        Ok(set_emit_flags(
            visited,
            EmitFlags::SingleLine | EmitFlags::NoAsciiEscaping,
            self,
        ))
    }

    pub(super) fn track_computed_name(
        &self,
        access_expression: Id<Node>, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: Option<Id<Node>>,
        context: &NodeBuilderContext,
    ) -> io::Result<()> {
        if !context.tracker_ref().is_track_symbol_supported() {
            return Ok(());
        }
        let first_identifier = get_first_identifier(access_expression, self);
        let name = self.type_checker.ref_(self).resolve_name_(
            Some(first_identifier),
            &first_identifier.ref_(self).as_identifier().escaped_text,
            SymbolFlags::Value | SymbolFlags::ExportValue,
            None,
            Option::<Id<Node>>::None,
            true,
            None,
        )?;
        if let Some(name) = name {
            context.tracker_ref().track_symbol(
                name,
                enclosing_declaration,
                SymbolFlags::Value,
            );
        }

        Ok(())
    }

    pub(super) fn lookup_symbol_chain(
        &self,
        symbol: Id<Symbol>,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        yield_module_symbol: Option<bool>,
    ) -> io::Result<Vec<Id<Symbol>>> {
        context.tracker_ref().track_symbol(
            symbol,
            context.maybe_enclosing_declaration(),
            // TODO: it looks like this is a place where the Typescript version "lied", I don't
            // know if we should "bubble down" the "real" Option<SymbolFlags> type into the
            // signature of .track_symbol()?
            meaning.unwrap_or(SymbolFlags::None),
        );
        self.lookup_symbol_chain_worker(symbol, context, meaning, yield_module_symbol)
    }

    pub(super) fn lookup_symbol_chain_worker(
        &self,
        symbol: Id<Symbol>,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        yield_module_symbol: Option<bool>,
    ) -> io::Result<Vec<Id<Symbol>>> {
        let chain: Vec<Id<Symbol>>;
        let is_type_parameter = symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::TypeParameter);
        if !is_type_parameter
            && (context.maybe_enclosing_declaration().is_some()
                || context
                    .flags()
                    .intersects(NodeBuilderFlags::UseFullyQualifiedType))
            && !context
                .flags()
                .intersects(NodeBuilderFlags::DoNotIncludeSymbolChain)
        {
            chain = Debug_.check_defined(
                self.get_symbol_chain(context, yield_module_symbol, symbol, meaning, true)?,
                None,
            );
            Debug_.assert(/*chain &&*/ !chain.is_empty(), None);
        } else {
            chain = vec![symbol];
        }
        Ok(chain)
    }

    pub(super) fn get_symbol_chain(
        &self,
        context: &NodeBuilderContext,
        yield_module_symbol: Option<bool>,
        symbol: Id<Symbol>,
        meaning: Option<SymbolFlags>,
        end_of_chain: bool,
    ) -> io::Result<Option<Vec<Id<Symbol>>>> {
        let mut accessible_symbol_chain = self.type_checker.ref_(self).get_accessible_symbol_chain(
            Some(symbol),
            context.maybe_enclosing_declaration(),
            // TODO: ...again here not sure where/how to "stop bubbling down" the actual
            // Option<SymbolFlags> type
            meaning.unwrap(),
            context
                .flags()
                .intersects(NodeBuilderFlags::UseOnlyExternalAliasing),
            None,
        )?;
        let parent_specifiers: Vec<Option<String>>;
        if match accessible_symbol_chain.as_ref() {
            None => true,
            Some(accessible_symbol_chain) => self.type_checker.ref_(self).needs_qualification(
                accessible_symbol_chain[0],
                context.maybe_enclosing_declaration(),
                if accessible_symbol_chain.len() == 1 {
                    // TODO: ...and here
                    meaning.unwrap()
                } else {
                    self.type_checker.ref_(self).get_qualified_left_meaning(
                        // TODO: ...and here
                        meaning.unwrap(),
                    )
                },
            )?,
        } {
            let parents = self.type_checker.ref_(self).get_containers_of_symbol(
                accessible_symbol_chain
                    .as_ref()
                    .map_or(symbol, |accessible_symbol_chain| accessible_symbol_chain[0]),
                context.maybe_enclosing_declaration(),
                // TODO: ...or here
                meaning.unwrap(),
            )?;
            if length(parents.as_deref()) > 0 {
                let parents = parents.as_ref().unwrap();
                parent_specifiers = parents
                    .into_iter()
                    .map(|&symbol| -> io::Result<_> {
                        Ok(
                            if some(
                                symbol.ref_(self).maybe_declarations().as_deref(),
                                Some(|&declaration: &Id<Node>| {
                                    self.type_checker
                                        .ref_(self).has_non_global_augmentation_external_module_symbol(
                                            declaration,
                                        )
                                }),
                            ) {
                                Some(self.get_specifier_for_module_symbol(symbol, context)?)
                            } else {
                                None
                            },
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let mut indices: Vec<usize> =
                    parents.into_iter().enumerate().map(|(i, _)| i).collect();
                indices.sort_by(|a, b| self.sort_by_best_name(&parent_specifiers, *a, *b));
                let sorted_parents = indices.into_iter().map(|i| parents[i].clone());
                for parent in sorted_parents {
                    let parent_chain = self.get_symbol_chain(
                        context,
                        yield_module_symbol,
                        parent,
                        Some(self.type_checker.ref_(self).get_qualified_left_meaning(
                            // TODO: ...or here
                            meaning.unwrap(),
                        )),
                        false,
                    )?;
                    if let Some(mut parent_chain) = parent_chain {
                        if matches!(
                            parent.ref_(self).maybe_exports().as_ref(),
                            Some(parent_exports) if matches!(
                                parent_exports.ref_(self).get(InternalSymbolName::ExportEquals),
                                Some(&parent_exports_get) if self.type_checker.ref_(self).get_symbol_if_same_reference(
                                    parent_exports_get,
                                    symbol,
                                )?.is_some()
                            )
                        ) {
                            accessible_symbol_chain = Some(parent_chain);
                            break;
                        }
                        parent_chain.append(&mut accessible_symbol_chain.try_unwrap_or_else(
                            || -> io::Result<_> {
                                Ok(vec![self
                                    .type_checker
                                    .ref_(self).get_alias_for_symbol_in_container(parent, symbol)?
                                    .unwrap_or_else(|| symbol)])
                            },
                        )?);
                        accessible_symbol_chain = Some(parent_chain);
                        break;
                    }
                }
            }
        }

        if accessible_symbol_chain.is_some() {
            return Ok(accessible_symbol_chain);
        }
        if end_of_chain
            || !symbol
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::TypeLiteral | SymbolFlags::ObjectLiteral)
        {
            if !end_of_chain
                && yield_module_symbol != Some(true)
                && maybe_for_each_bool(
                    symbol.ref_(self).maybe_declarations().as_ref(),
                    |&declaration: &Id<Node>, _| {
                        self.type_checker
                            .ref_(self).has_non_global_augmentation_external_module_symbol(declaration)
                    },
                )
            {
                return Ok(None);
            }
            return Ok(Some(vec![symbol]));
        }
        Ok(None)
    }

    pub(super) fn sort_by_best_name(
        &self,
        parent_specifiers: &[Option<String>],
        a: usize,
        b: usize,
    ) -> Ordering {
        let specifier_a = parent_specifiers[a].as_ref();
        let specifier_b = parent_specifiers[b].as_ref();
        if let (Some(specifier_a), Some(specifier_b)) = (
            specifier_a.filter(|specifier_a| !specifier_a.is_empty()),
            specifier_b.filter(|specifier_b| !specifier_b.is_empty()),
        ) {
            let is_b_relative = path_is_relative(specifier_b);
            if path_is_relative(specifier_a) == is_b_relative {
                let count_difference = TryInto::<isize>::try_into(
                    module_specifiers::count_path_components(specifier_a),
                )
                .unwrap()
                    - TryInto::<isize>::try_into(module_specifiers::count_path_components(
                        specifier_b,
                    ))
                    .unwrap();
                return if count_difference < 0 {
                    Ordering::Less
                } else if count_difference == 0 {
                    Ordering::Equal
                } else {
                    Ordering::Greater
                };
            }
            if is_b_relative {
                return Ordering::Less;
            }
            return Ordering::Greater;
        }
        Ordering::Equal
    }

    pub(super) fn type_parameters_to_type_parameter_declarations(
        &self,
        symbol: Id<Symbol>,
        context: &NodeBuilderContext,
    ) -> io::Result<Option<Id<NodeArray> /*<TypeParameterDeclaration>*/>> {
        let mut type_parameter_nodes: Option<Id<NodeArray> /*TypeParameterDeclaration*/> = None;
        let target_symbol = self.type_checker.ref_(self).get_target_symbol(symbol);
        if target_symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface | SymbolFlags::TypeAlias)
        {
            type_parameter_nodes = Some(
                get_factory(self).create_node_array(
                    try_maybe_map(
                        self.type_checker
                            .ref_(self).get_local_type_parameters_of_class_or_interface_or_type_alias(symbol)?
                            .as_ref(),
                        |&tp: &Id<Type>, _| self.type_parameter_to_declaration_(tp, context, None),
                    )
                    .transpose()?,
                    None,
                ),
            );
        }
        Ok(type_parameter_nodes)
    }

    pub(super) fn lookup_type_parameter_nodes(
        &self,
        chain: &[Id<Symbol>],
        index: usize,
        context: &NodeBuilderContext,
    ) -> io::Result<Option<Vec<Id<Node>>>> {
        Debug_.assert(/*chain && 0 <= index &&*/ index < chain.len(), None);
        let symbol = chain[index];
        let symbol_id = get_symbol_id(&symbol.ref_(self));
        if matches!(
            (*context.type_parameter_symbol_list).borrow().as_ref(),
            Some(context_type_parameter_symbol_list) if context_type_parameter_symbol_list.contains(
                &symbol_id
            )
        ) {
            return Ok(None);
        }
        context
            .type_parameter_symbol_list
            .borrow_mut()
            .get_or_insert_default_()
            .insert(symbol_id);
        let mut type_parameter_nodes: Option<
            Vec<Id<Node /*TypeNode[] | TypeParameterDeclaration[]*/>>,
        > = None;
        if context
            .flags()
            .intersects(NodeBuilderFlags::WriteTypeParametersInQualifiedName)
            && index < chain.len() - 1
        {
            let parent_symbol = symbol;
            let next_symbol = chain[index + 1];
            if get_check_flags(&next_symbol.ref_(self)).intersects(CheckFlags::Instantiated) {
                let params = self
                    .type_checker
                    .ref_(self).get_type_parameters_of_class_or_interface(
                        if parent_symbol
                            .ref_(self)
                            .flags()
                            .intersects(SymbolFlags::Alias)
                        {
                            self.type_checker.ref_(self).resolve_alias(parent_symbol)?
                        } else {
                            parent_symbol.clone()
                        },
                    )?;
                type_parameter_nodes = self.map_to_type_nodes(
                    try_maybe_map(params.as_ref(), |&t: &Id<Type>, _| {
                        self.type_checker.ref_(self).get_mapped_type(
                            t,
                            (*next_symbol.ref_(self).as_transient_symbol().symbol_links().ref_(self))
                                .borrow()
                                .mapper
                                .unwrap(),
                        )
                    })
                    .transpose()?
                    .as_deref(),
                    context,
                    None,
                )?;
            } else {
                type_parameter_nodes = self
                    .type_parameters_to_type_parameter_declarations(symbol, context)?
                    .map(|node_array| node_array.ref_(self).to_vec());
            }
        }
        Ok(type_parameter_nodes)
    }

    pub(super) fn get_topmost_indexed_access_type(
        &self,
        top: Id<Node>, /*IndexedAccessTypeNode*/
    ) -> Id<Node /*IndexedAccessTypeNode*/> {
        let top_ref = top.ref_(self);
        let top_as_indexed_access_type_node = top_ref.as_indexed_access_type_node();
        if is_indexed_access_type_node(&top_as_indexed_access_type_node.object_type.ref_(self)) {
            return self
                .get_topmost_indexed_access_type(top_as_indexed_access_type_node.object_type);
        }
        top
    }

    pub(super) fn get_specifier_for_module_symbol(
        &self,
        symbol: Id<Symbol>,
        context: &NodeBuilderContext,
    ) -> io::Result<String> {
        let mut file = get_declaration_of_kind(symbol, SyntaxKind::SourceFile, self);
        if file.is_none() {
            let equivalent_file_symbol = try_maybe_first_defined(
                symbol.ref_(self).maybe_declarations().as_ref(),
                |&d: &Id<Node>, _| {
                    self.type_checker
                        .ref_(self).get_file_symbol_if_file_symbol_export_equals_container(d, symbol)
                },
            )?;
            if let Some(equivalent_file_symbol) = equivalent_file_symbol {
                file = get_declaration_of_kind(
                    equivalent_file_symbol,
                    SyntaxKind::SourceFile,
                    self,
                );
            }
        }
        if let Some(file_module_name) = file
            .and_then(|file| file.ref_(self).as_source_file().maybe_module_name().clone())
        {
            return Ok(file_module_name);
        }
        if file.is_none() {
            if context
                .tracker_ref()
                .is_track_referenced_ambient_module_supported()
            {
                let ambient_decls = maybe_filter(
                    symbol.ref_(self).maybe_declarations().as_deref(),
                    |&declaration: &Id<Node>| is_ambient_module(declaration, self),
                );
                if length(ambient_decls.as_deref()) > 0 {
                    for &decl in ambient_decls.as_ref().unwrap() {
                        context
                            .tracker_ref()
                            .track_referenced_ambient_module(decl, symbol)?;
                    }
                }
            }
            if ambient_module_symbol_regex.is_match(symbol.ref_(self).escaped_name()) {
                return Ok(symbol.ref_(self).escaped_name()
                    [1..symbol.ref_(self).escaped_name().len() - 1]
                    .to_owned());
            }
        }
        if context.maybe_enclosing_declaration().is_none()
            || !context.tracker_ref().is_module_resolver_host_supported()
        {
            if ambient_module_symbol_regex.is_match(symbol.ref_(self).escaped_name()) {
                return Ok(symbol.ref_(self).escaped_name()
                    [1..symbol.ref_(self).escaped_name().len() - 1]
                    .to_owned());
            }
            return Ok(get_source_file_of_node(
                get_non_augmentation_declaration(symbol, self).unwrap(),
                self,
            )
            .ref_(self).as_source_file()
            .file_name()
            .clone());
        }
        let context_file =
            get_source_file_of_node(get_original_node(context.enclosing_declaration(), self), self);
        let links = self.type_checker.ref_(self).get_symbol_links(symbol);
        let mut specifier =
            (*links.ref_(self))
                .borrow()
                .specifier_cache
                .as_ref()
                .and_then(|links_specifier_cache| {
                    links_specifier_cache
                        .get(&**context_file.ref_(self).as_source_file().path())
                        .cloned()
                });
        if specifier.is_none() {
            let is_bundle = matches!(
                out_file(&self.type_checker.ref_(self).compiler_options.ref_(self)),
                Some(out_file) if !out_file.is_empty()
            );
            let context_tracker = context.tracker_ref();
            let module_resolver_host = context_tracker.module_resolver_host().unwrap();
            let specifier_compiler_options = if is_bundle {
                self.alloc_compiler_options(CompilerOptions {
                    base_url: Some(module_resolver_host.ref_(self).get_common_source_directory()),
                    ..(*self.type_checker.ref_(self).compiler_options.ref_(self)).clone()
                })
            } else {
                self.type_checker.ref_(self).compiler_options.clone()
            };
            specifier = Some(
                first(&module_specifiers::get_module_specifiers(
                    symbol,
                    &self.type_checker.ref_(self),
                    specifier_compiler_options.clone(),
                    context_file,
                    module_resolver_host.ref_(self).as_dyn_module_specifier_resolution_host(),
                    &UserPreferencesBuilder::default()
                        .import_module_specifier_preference(Some(if is_bundle {
                            "non-relative".to_owned()
                        } else {
                            "project-relative".to_owned()
                        }))
                        .import_module_specifier_ending(if is_bundle {
                            Some("minimal".to_owned())
                        } else {
                            None
                        })
                        .build()
                        .unwrap(),
                    self,
                )?)
                .clone(),
            );
            links
                .ref_mut(self)
                .specifier_cache
                .get_or_insert_default_()
                .insert(
                    context_file.ref_(self).as_source_file().path().to_string(),
                    specifier.clone().unwrap(),
                );
        }
        Ok(specifier.unwrap())
    }

    pub(super) fn symbol_to_entity_name_node(&self, symbol: Id<Symbol>) -> Id<Node /*EntityName*/> {
        let identifier = get_factory(self).create_identifier(&unescape_leading_underscores(
            symbol.ref_(self).escaped_name(),
        ));
        if let Some(symbol_parent) = symbol.ref_(self).maybe_parent() {
            get_factory(self)
                .create_qualified_name(self.symbol_to_entity_name_node(symbol_parent), identifier)
        } else {
            identifier
        }
    }

    pub(super) fn symbol_to_type_node(
        &self,
        symbol: Id<Symbol>,
        context: &NodeBuilderContext,
        meaning: SymbolFlags,
        override_type_arguments: Option<&[Id<Node /*TypeNode*/>]>,
    ) -> io::Result<Id<Node /*TypeNode*/>> {
        let chain = self.lookup_symbol_chain(
            symbol,
            context,
            Some(meaning),
            Some(
                !context
                    .flags()
                    .intersects(NodeBuilderFlags::UseAliasDefinedOutsideCurrentScope),
            ),
        )?;

        let is_type_of = meaning == SymbolFlags::Value;
        if some(
            chain[0].ref_(self).maybe_declarations().as_deref(),
            Some(|&declaration: &Id<Node>| {
                self.type_checker
                    .ref_(self).has_non_global_augmentation_external_module_symbol(declaration)
            }),
        ) {
            let non_root_parts = if chain.len() > 1 {
                Some(self.create_access_from_symbol_chain(
                    context,
                    override_type_arguments,
                    &chain,
                    chain.len() - 1,
                    1,
                )?)
            } else {
                None
            };
            let type_parameter_nodes = override_type_arguments
                .map(ToOwned::to_owned)
                .try_or_else(|| self.lookup_type_parameter_nodes(&chain, 0, context))?;
            let specifier = self.get_specifier_for_module_symbol(chain[0], context)?;
            if !context
                .flags()
                .intersects(NodeBuilderFlags::AllowNodeModulesRelativePaths)
                && get_emit_module_resolution_kind(&self.type_checker.ref_(self).compiler_options.ref_(self))
                    != ModuleResolutionKind::Classic
                && specifier.contains("/node_modules/")
            {
                context.set_encountered_error(true);
                context
                    .tracker_ref()
                    .report_likely_unsafe_import_required_error(&specifier);
            }
            let lit: Id<Node> = get_factory(self).create_literal_type_node(
                get_factory(self).create_string_literal(specifier.clone(), None, None),
            );
            context
                .tracker_ref()
                .track_external_module_symbol_of_import_type_node(chain[0]);
            context.increment_approximate_length_by(specifier.len() + 10);
            if match non_root_parts {
                None => true,
                Some(non_root_parts) => is_entity_name(&non_root_parts.ref_(self)),
            } {
                if let Some(non_root_parts) = non_root_parts {
                    let last_id = if is_identifier(&non_root_parts.ref_(self)) {
                        non_root_parts
                    } else {
                        non_root_parts.ref_(self).as_qualified_name().right
                    };
                    last_id.ref_(self).as_identifier().set_type_arguments(None);
                }
                return Ok(get_factory(self).create_import_type_node(
                    lit,
                    non_root_parts,
                    type_parameter_nodes,
                    Some(is_type_of),
                ));
            } else {
                let split_node =
                    self.get_topmost_indexed_access_type(non_root_parts.unwrap());
                let qualifier = split_node
                    .ref_(self).as_indexed_access_type_node()
                    .object_type
                    .ref_(self).as_type_reference_node()
                    .type_name;
                return Ok(get_factory(self).create_indexed_access_type_node(
                    get_factory(self).create_import_type_node(
                        lit,
                        Some(qualifier.clone()),
                        type_parameter_nodes,
                        Some(is_type_of),
                    ),
                    split_node.ref_(self).as_indexed_access_type_node().index_type,
                ));
            }
        }

        let entity_name = self.create_access_from_symbol_chain(
            context,
            override_type_arguments,
            &chain,
            chain.len() - 1,
            0,
        )?;
        if is_indexed_access_type_node(&entity_name.ref_(self)) {
            return Ok(entity_name);
        }
        Ok(if is_type_of {
            get_factory(self).create_type_query_node(entity_name)
        } else {
            let last_id = if is_identifier(&entity_name.ref_(self)) {
                entity_name
            } else {
                entity_name.ref_(self).as_qualified_name().right
            };
            let last_type_args = last_id.ref_(self).as_identifier().maybe_type_arguments();
            last_id.ref_(self).as_identifier().set_type_arguments(None);
            get_factory(self).create_type_reference_node(entity_name, last_type_args)
        })
    }
}

pub(super) struct SignatureToSignatureDeclarationOptions<TPrivateSymbolVisitor: Fn(Id<Symbol>)> {
    pub modifiers: Option<Vec<Id<Node /*Modifier*/>>>,
    pub name: Option<Id<Node /*PropertyName*/>>,
    pub question_token: Option<Id<Node /*QuestionToken*/>>,
    pub private_symbol_visitor: Option<TPrivateSymbolVisitor>,
    pub bundled_imports: Option<bool>,
}
