use std::{
    borrow::{Borrow, Cow},
    cmp::Ordering,
    convert::TryInto,
    io, ptr,
    rc::Rc,
};

use gc::Gc;
use id_arena::Id;

use super::{ambient_module_symbol_regex, get_symbol_id, NodeBuilderContext, TypeFacts};
use crate::{
    are_option_gcs_equal, array_is_homogeneous, cast_present, create_underscore_escaped_multi_map,
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
    GetOrInsertDefault, HasInitializerInterface, IndexInfo, InternalSymbolName, ModifierFlags,
    ModuleResolutionKind, NamedDeclarationInterface, Node, NodeArray, NodeBuilder,
    NodeBuilderFlags, NodeInterface, OptionTry, Signature, SignatureFlags, StrOrNodeArray,
    StrOrRcNode, StringOrNodeArray, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    SynthesizedComment, TransientSymbolInterface, Type, TypeInterface, TypePredicateKind,
    UnderscoreEscapedMultiMap, UserPreferencesBuilder,
};

impl NodeBuilder {
    pub(super) fn preserve_comments_on(
        &self,
        property_symbol: &Symbol,
        node: Gc<Node>,
    ) -> Gc<Node> {
        if some(
            property_symbol.maybe_declarations().as_deref(),
            Some(|d: &Gc<Node>| d.kind() == SyntaxKind::JSDocPropertyTag),
        ) {
            let d: Gc<Node> = property_symbol
                .maybe_declarations()
                .as_ref()
                .unwrap()
                .into_iter()
                .find(|d| d.kind() == SyntaxKind::JSDocPropertyTag)
                .cloned()
                .unwrap();
            let comment_text = get_text_of_jsdoc_comment(d.as_jsdoc_tag().maybe_comment().map(
                |d_comment| -> StrOrNodeArray {
                    match d_comment {
                        StringOrNodeArray::String(d_comment) => (&**d_comment).into(),
                        StringOrNodeArray::NodeArray(d_comment) => d_comment.clone().into(),
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
        types: Option<&[Id<Type>]>,
        context: &NodeBuilderContext,
        is_bare_list: Option<bool>,
    ) -> io::Result<Option<Vec<Gc<Node /*TypeNode*/>>>> {
        if let Some(types) = types {
            if !types.is_empty()
            /*some(types)*/
            {
                if self.check_truncation_length(context) {
                    if is_bare_list != Some(true) {
                        return Ok(Some(vec![get_factory().create_type_reference_node(
                            "...",
                            Option::<Gc<NodeArray>>::None,
                        )]));
                    } else if types.len() > 2 {
                        return Ok(Some(vec![
                            self.type_to_type_node_helper(Some(&*types[0]), context)?
                                .unwrap(),
                            get_factory().create_type_reference_node(
                                &*format!("... {} more ...", types.len() - 2),
                                Option::<Gc<NodeArray>>::None,
                            ),
                            self.type_to_type_node_helper(Some(&*types[types.len() - 1]), context)?
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
                let mut result: Vec<Gc<Node /*TypeNode*/>> = vec![];
                let mut i = 0;
                for type_ in types {
                    i += 1;
                    if self.check_truncation_length(context) && i + 2 < types.len() - 1 {
                        result.push(get_factory().create_type_reference_node(
                            &*format!("... {} more ...", types.len() - i),
                            Option::<Gc<NodeArray>>::None,
                        ));
                        let type_node =
                            self.type_to_type_node_helper(Some(&*types[types.len() - 1]), context)?;
                        if let Some(type_node) = type_node {
                            result.push(type_node);
                        }
                        break;
                    }
                    context.increment_approximate_length_by(2);
                    let type_node = self.type_to_type_node_helper(Some(&**type_), context)?;
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
                                    .type_to_type_node_helper(Some(&**type_), context)?
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

    pub(super) fn types_are_same_reference(&self, a: &Type, b: &Type) -> bool {
        ptr::eq(a, b)
            || a.maybe_symbol().is_some()
                && are_option_gcs_equal(a.maybe_symbol().as_ref(), b.maybe_symbol().as_ref())
            || a.maybe_alias_symbol().is_some()
                && are_option_gcs_equal(
                    a.maybe_alias_symbol().as_ref(),
                    b.maybe_alias_symbol().as_ref(),
                )
    }

    pub(super) fn index_info_to_index_signature_declaration_helper(
        &self,
        index_info: &IndexInfo,
        context: &NodeBuilderContext,
        type_node: Option<impl Borrow<Node> /*TypeNode*/>,
    ) -> io::Result<Gc<Node /*IndexSignatureDeclaration*/>> {
        let name = get_name_from_index_info(index_info).unwrap_or_else(|| Cow::Borrowed("x"));
        let indexer_type_node =
            self.type_to_type_node_helper(Some(&*index_info.key_type), context)?;

        let indexing_parameter = get_factory().create_parameter_declaration(
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            None,
            Some(&*name),
            None,
            indexer_type_node,
            None,
        );
        let type_node = type_node
            .map(|type_node| type_node.borrow().node_wrapper())
            .try_or_else(|| {
                self.type_to_type_node_helper(Some(&*index_info.type_ /*|| anyType*/), context)
            })?;
        // if (!indexInfo.type && !(context.flags & NodeBuilderFlags.AllowEmptyIndexInfoType)) {
        //     context.encounteredError = true;
        // }
        context.increment_approximate_length_by(name.len() + 4);
        Ok(get_factory().create_index_signature(
            Option::<Gc<NodeArray>>::None,
            if index_info.is_readonly {
                Some(vec![get_factory().create_token(SyntaxKind::ReadonlyKeyword)])
            } else {
                None
            },
            vec![indexing_parameter],
            type_node,
        ))
    }

    pub(super) fn signature_to_signature_declaration_helper(
        &self,
        signature: Gc<Signature>,
        kind: SyntaxKind,
        context: &NodeBuilderContext,
        options: Option<SignatureToSignatureDeclarationOptions<impl Fn(&Symbol)>>,
    ) -> io::Result<Gc<Node /*SignatureDeclaration*/>> {
        let suppress_any = context
            .flags()
            .intersects(NodeBuilderFlags::SuppressAnyReturnType);
        if suppress_any {
            context.set_flags(context.flags() & !NodeBuilderFlags::SuppressAnyReturnType);
        }
        context.increment_approximate_length_by(3);
        let mut type_parameters: Option<Vec<Gc<Node /*TypeParameterDeclaration(*/>>> = None;
        let mut _type_arguments: Option<Vec<Gc<Node /*TypeNode(*/>>> = None;
        let mut passed_if_condition = false;
        if context
            .flags()
            .intersects(NodeBuilderFlags::WriteTypeArgumentsOfSignature)
        {
            if let (Some(signature_target), Some(signature_mapper)) =
                (signature.target.as_ref(), signature.mapper.clone())
            {
                if let Some(signature_target_type_parameters) =
                    signature_target.maybe_type_parameters().as_ref()
                {
                    passed_if_condition = true;
                    _type_arguments = Some(
                        signature_target_type_parameters
                            .into_iter()
                            .map(|parameter| {
                                Ok(self
                                    .type_to_type_node_helper(
                                        Some(self.type_checker.instantiate_type(
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
            type_parameters = signature.maybe_type_parameters().as_ref().try_map(
                |signature_type_parameters| {
                    signature_type_parameters
                        .into_iter()
                        .map(|parameter| {
                            self.type_parameter_to_declaration_(parameter, context, None)
                        })
                        .collect::<io::Result<Vec<_>>>()
                },
            )?;
        }

        let expanded_params = self
            .type_checker
            .get_expanded_parameters(&signature, Some(true))?
            .into_iter()
            .next()
            .unwrap();
        let mut parameters = if some(
            Some(&*expanded_params),
            Some(|p: &Gc<Symbol>| {
                !Gc::ptr_eq(p, &expanded_params[expanded_params.len() - 1])
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
        .collect::<io::Result<Vec<_>>>()?;
        if let Some(signature_this_parameter) = signature.maybe_this_parameter().as_ref() {
            let this_parameter = self.symbol_to_parameter_declaration_(
                signature_this_parameter,
                context,
                None,
                Option::<&fn(&Symbol)>::None,
                None,
            )?;
            parameters.insert(0, this_parameter);
        }

        let mut return_type_node: Option<Gc<Node /*TypeNode*/>> = None;
        let type_predicate = self
            .type_checker
            .get_type_predicate_of_signature(&signature)?;
        if let Some(type_predicate) = type_predicate.as_ref() {
            let asserts_modifier: Option<Gc<Node>> = if matches!(
                type_predicate.kind,
                TypePredicateKind::AssertsThis | TypePredicateKind::AssertsIdentifier
            ) {
                Some(get_factory().create_token(SyntaxKind::AssertsKeyword))
            } else {
                None
            };
            let parameter_name = if matches!(
                type_predicate.kind,
                TypePredicateKind::Identifier | TypePredicateKind::AssertsIdentifier
            ) {
                set_emit_flags(
                    get_factory()
                        .create_identifier(type_predicate.parameter_name.as_ref().unwrap()),
                    EmitFlags::NoAsciiEscaping,
                )
            } else {
                get_factory().create_this_type_node()
            };
            let type_node = type_predicate
                .type_
                .as_ref()
                .try_and_then(|type_predicate_type| {
                    self.type_to_type_node_helper(Some(&**type_predicate_type), context)
                })?;
            return_type_node = Some(get_factory().create_type_predicate_node(
                asserts_modifier,
                parameter_name,
                type_node,
            ));
        } else {
            let ref return_type = self
                .type_checker
                .get_return_type_of_signature(signature.clone())?;
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
                    )?,
                );
            } else if !suppress_any {
                return_type_node =
                    Some(get_factory().create_keyword_type_node(SyntaxKind::AnyKeyword));
            }
        }
        let mut modifiers = options
            .as_ref()
            .and_then(|options| options.modifiers.clone());
        if kind == SyntaxKind::ConstructorType
            && signature.flags.intersects(SignatureFlags::Abstract)
        {
            let flags = modifiers_to_flags(modifiers.as_deref());
            modifiers = Some(
                get_factory().create_modifiers_from_modifier_flags(flags | ModifierFlags::Abstract),
            );
        }

        let node: Gc<Node> = match kind {
            SyntaxKind::CallSignature => {
                get_factory().create_call_signature(type_parameters, parameters, return_type_node)
            }
            SyntaxKind::ConstructSignature => get_factory().create_construct_signature(
                type_parameters,
                parameters,
                return_type_node,
            ),
            SyntaxKind::MethodSignature => get_factory().create_method_signature(
                modifiers,
                options.as_ref().and_then(|options| options.name.clone()),
                options
                    .as_ref()
                    .and_then(|options| options.question_token.clone()),
                type_parameters,
                Some(parameters),
                return_type_node,
            ),
            SyntaxKind::MethodDeclaration => get_factory().create_method_declaration(
                Option::<Gc<NodeArray>>::None,
                modifiers,
                None,
                options
                    .as_ref()
                    .and_then(|options| options.name.clone())
                    .unwrap_or_else(|| get_factory().create_identifier("")),
                None,
                type_parameters,
                parameters,
                return_type_node,
                None,
            ),
            SyntaxKind::Constructor => get_factory().create_constructor_declaration(
                Option::<Gc<NodeArray>>::None,
                modifiers,
                Some(parameters),
                None,
            ),
            SyntaxKind::GetAccessor => get_factory().create_get_accessor_declaration(
                Option::<Gc<NodeArray>>::None,
                modifiers,
                options
                    .as_ref()
                    .and_then(|options| options.name.clone())
                    .unwrap_or_else(|| get_factory().create_identifier("")),
                parameters,
                return_type_node,
                None,
            ),
            SyntaxKind::SetAccessor => get_factory().create_set_accessor_declaration(
                Option::<Gc<NodeArray>>::None,
                modifiers,
                options
                    .as_ref()
                    .and_then(|options| options.name.clone())
                    .unwrap_or_else(|| get_factory().create_identifier("")),
                parameters,
                None,
            ),
            SyntaxKind::IndexSignature => get_factory().create_index_signature(
                Option::<Gc<NodeArray>>::None,
                modifiers,
                parameters,
                return_type_node,
            ),
            SyntaxKind::JSDocFunctionType => {
                get_factory().create_jsdoc_function_type(parameters, return_type_node)
            }
            SyntaxKind::FunctionType => get_factory().create_function_type_node(
                type_parameters,
                parameters,
                Some(return_type_node.unwrap_or_else(|| {
                    get_factory().create_type_reference_node(
                        get_factory().create_identifier(""),
                        Option::<Gc<NodeArray>>::None,
                    )
                })),
            ),
            SyntaxKind::ConstructorType => get_factory().create_constructor_type_node(
                modifiers,
                type_parameters,
                parameters,
                Some(return_type_node.unwrap_or_else(|| {
                    get_factory().create_type_reference_node(
                        get_factory().create_identifier(""),
                        Option::<Gc<NodeArray>>::None,
                    )
                })),
            ),
            SyntaxKind::FunctionDeclaration => get_factory().create_function_declaration(
                Option::<Gc<NodeArray>>::None,
                modifiers,
                None,
                Some(
                    options
                        .as_ref()
                        .and_then(|options| options.name.clone())
                        .map(|name| cast_present(name, |name: &Gc<Node>| is_identifier(name)))
                        .unwrap_or_else(|| get_factory().create_identifier("")),
                ),
                type_parameters,
                parameters,
                return_type_node,
                None,
            ),
            SyntaxKind::FunctionExpression => get_factory().create_function_expression(
                modifiers,
                None,
                Some(
                    options
                        .as_ref()
                        .and_then(|options| options.name.clone())
                        .map(|name| cast_present(name, |name: &Gc<Node>| is_identifier(name)))
                        .unwrap_or_else(|| get_factory().create_identifier("")),
                ),
                type_parameters,
                Some(parameters),
                return_type_node,
                get_factory().create_block(vec![], None),
            ),
            SyntaxKind::ArrowFunction => get_factory().create_arrow_function(
                modifiers,
                type_parameters,
                parameters,
                return_type_node,
                None,
                get_factory().create_block(vec![], None),
            ),
            _ => Debug_.assert_never(kind, None),
        };

        // TODO: this looks like it's only used for appending a "nonexistent" .typeArguments
        // property to drive showing type arguments instead of type parameters (so will need to
        // extend the relevant AST node types to support mutating in a type_arguments value I
        // guess)
        // type_arguments.map(|type_arguments| {
        //     get_factory().create_node_array(Some(type_arguments), None)
        // })

        Ok(node)
    }

    pub(super) fn type_parameter_to_declaration_with_constraint(
        &self,
        type_: &Type, /*TypeParameter*/
        context: &NodeBuilderContext,
        constraint_node: Option<Gc<Node>>,
    ) -> io::Result<Gc<Node /*TypeParameterDeclaration*/>> {
        let saved_context_flags = context.flags();
        context.set_flags(context.flags() & !NodeBuilderFlags::WriteTypeParametersInQualifiedName);
        let name = self.type_parameter_to_name(type_, context)?;
        let default_parameter = self.type_checker.get_default_from_type_parameter_(type_)?;
        let default_parameter_node =
            default_parameter
                .as_ref()
                .try_and_then(|default_parameter| {
                    self.type_to_type_node_helper(Some(&**default_parameter), context)
                })?;
        context.set_flags(saved_context_flags);
        Ok(get_factory().create_type_parameter_declaration(
            name,
            constraint_node,
            default_parameter_node,
        ))
    }

    pub(super) fn type_parameter_to_declaration_(
        &self,
        type_: &Type, /*TypeParameter*/
        context: &NodeBuilderContext,
        constraint: Option<Id<Type>>,
    ) -> io::Result<Gc<Node /*TypeParameterDeclaration*/>> {
        let constraint =
            constraint.try_or_else(|| self.type_checker.get_constraint_of_type_parameter(type_))?;
        let constraint_node = constraint.as_ref().try_and_then(|constraint| {
            self.type_to_type_node_helper(Some(&**constraint), context)
        })?;
        self.type_parameter_to_declaration_with_constraint(type_, context, constraint_node)
    }

    pub(super) fn symbol_to_parameter_declaration_(
        &self,
        parameter_symbol: &Symbol,
        context: &NodeBuilderContext,
        preserve_modifier_flags: Option<bool>,
        private_symbol_visitor: Option<&impl Fn(&Symbol)>,
        bundled_imports: Option<bool>,
    ) -> io::Result<Gc<Node /*ParameterDeclaration*/>> {
        let mut parameter_declaration: Option<
            Gc<Node /*ParameterDeclaration | JSDocParameterTag*/>,
        > = get_declaration_of_kind(parameter_symbol, SyntaxKind::Parameter);
        if parameter_declaration.is_none() && !is_transient_symbol(parameter_symbol) {
            parameter_declaration =
                get_declaration_of_kind(parameter_symbol, SyntaxKind::JSDocParameterTag);
        }

        let mut parameter_type = self.type_checker.get_type_of_symbol(parameter_symbol)?;
        if matches!(
            parameter_declaration.as_ref(),
            Some(parameter_declaration) if self.type_checker.is_required_initialized_parameter(parameter_declaration)?
        ) {
            parameter_type = self
                .type_checker
                .get_optional_type_(&parameter_type, None)?;
        }
        if context
            .flags()
            .intersects(NodeBuilderFlags::NoUndefinedOptionalParameterType)
            && matches!(
                parameter_declaration.as_ref(),
                Some(parameter_declaration) if !is_jsdoc_parameter_tag(parameter_declaration) &&
                    self.type_checker.is_optional_uninitialized_parameter_(parameter_declaration)?
            )
        {
            parameter_type = self
                .type_checker
                .get_type_with_facts(&parameter_type, TypeFacts::NEUndefined)?;
        }
        let parameter_type_node = self.serialize_type_for_declaration(
            context,
            &parameter_type,
            parameter_symbol,
            context.maybe_enclosing_declaration(),
            private_symbol_visitor,
            bundled_imports,
        )?;

        let modifiers: Option<Vec<Gc<Node>>> = if !context
            .flags()
            .intersects(NodeBuilderFlags::OmitParameterModifiers)
            && preserve_modifier_flags == Some(true)
        {
            parameter_declaration
                .as_ref()
                .and_then(|parameter_declaration| {
                    parameter_declaration.maybe_modifiers().as_ref().map(
                        |parameter_declaration_modifiers| {
                            parameter_declaration_modifiers
                                .into_iter()
                                .map(|modifier| get_factory().clone_node(modifier))
                                .collect()
                        },
                    )
                })
        } else {
            None
        };
        let is_rest = matches!(
            parameter_declaration.as_ref(),
            Some(parameter_declaration) if is_rest_parameter(parameter_declaration)
        ) || get_check_flags(parameter_symbol).intersects(CheckFlags::RestParameter);
        let dot_dot_dot_token: Option<Gc<Node>> = if is_rest {
            Some(get_factory().create_token(SyntaxKind::DotDotDotToken))
        } else {
            None
        };
        let parameter_symbol_name: Option<Cow<'_, str>>;
        let name: StrOrRcNode<'_> =
            if let Some(parameter_declaration) = parameter_declaration.as_ref() {
                if let Some(parameter_declaration_name) = parameter_declaration
                    .as_named_declaration()
                    .maybe_name()
                    .as_ref()
                {
                    match parameter_declaration_name.kind() {
                        SyntaxKind::Identifier => set_emit_flags(
                            get_factory().clone_node(parameter_declaration_name),
                            EmitFlags::NoAsciiEscaping,
                        ),
                        SyntaxKind::QualifiedName => set_emit_flags(
                            get_factory()
                                .clone_node(&parameter_declaration_name.as_qualified_name().right),
                            EmitFlags::NoAsciiEscaping,
                        ),
                        _ => self.clone_binding_name(context, parameter_declaration_name)?,
                    }
                    .into()
                } else {
                    parameter_symbol_name = Some(symbol_name(parameter_symbol));
                    parameter_symbol_name.as_deref().unwrap().into()
                }
            } else {
                parameter_symbol_name = Some(symbol_name(parameter_symbol));
                parameter_symbol_name.as_deref().unwrap().into()
            };
        let is_optional = matches!(
            parameter_declaration.as_ref(),
            Some(parameter_declaration) if self.type_checker.is_optional_parameter_(parameter_declaration)?
        ) || get_check_flags(parameter_symbol)
            .intersects(CheckFlags::OptionalParameter);
        let question_token = if is_optional {
            Some(get_factory().create_token(SyntaxKind::QuestionToken))
        } else {
            None
        };
        let parameter_node = get_factory().create_parameter_declaration(
            Option::<Gc<NodeArray>>::None,
            modifiers,
            dot_dot_dot_token,
            Some(name),
            question_token,
            Some(parameter_type_node),
            None,
        );
        context.increment_approximate_length_by(symbol_name(parameter_symbol).len() + 3);
        Ok(parameter_node)
    }

    pub(super) fn clone_binding_name(
        &self,
        context: &NodeBuilderContext,
        node: &Node, /*BindingName*/
    ) -> io::Result<Gc<Node /*BIndingName*/>> {
        self.elide_initializer_and_set_emit_flags(context, node)
    }

    pub(super) fn elide_initializer_and_set_emit_flags(
        &self,
        context: &NodeBuilderContext,
        node: &Node,
    ) -> io::Result<Gc<Node>> {
        if context.tracker().is_track_symbol_supported()
            && is_computed_property_name(node)
            && self.type_checker.is_late_bindable_name(node)?
        {
            self.track_computed_name(
                &node.as_computed_property_name().expression,
                context.maybe_enclosing_declaration(),
                context,
            )?;
        }
        let mut visited = try_visit_each_child(
            node,
            |node: &Node| -> io::Result<_> {
                Ok(Some(
                    self.elide_initializer_and_set_emit_flags(context, node)?
                        .into(),
                ))
            },
            &*null_transformation_context,
        )?;
        if is_binding_element(&visited) {
            let visited_as_binding_element = visited.as_binding_element();
            visited = get_factory().update_binding_element(
                &visited,
                visited_as_binding_element.dot_dot_dot_token.clone(),
                visited_as_binding_element.property_name.clone(),
                visited_as_binding_element.name(),
                visited_as_binding_element.maybe_initializer(),
            );
        }
        if !node_is_synthesized(&*visited) {
            visited = get_factory().clone_node(&visited);
        }
        Ok(set_emit_flags(
            visited,
            EmitFlags::SingleLine | EmitFlags::NoAsciiEscaping,
        ))
    }

    pub(super) fn track_computed_name(
        &self,
        access_expression: &Node, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: Option<impl Borrow<Node>>,
        context: &NodeBuilderContext,
    ) -> io::Result<()> {
        if !context.tracker().is_track_symbol_supported() {
            return Ok(());
        }
        let first_identifier = get_first_identifier(access_expression);
        let name = self.type_checker.resolve_name_(
            Some(&*first_identifier),
            &first_identifier.as_identifier().escaped_text,
            SymbolFlags::Value | SymbolFlags::ExportValue,
            None,
            Option::<Gc<Node>>::None,
            true,
            None,
        )?;
        if let Some(name) = name.as_ref() {
            context.tracker().track_symbol(
                name,
                enclosing_declaration
                    .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper()),
                SymbolFlags::Value,
            );
        }

        Ok(())
    }

    pub(super) fn lookup_symbol_chain(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        yield_module_symbol: Option<bool>,
    ) -> io::Result<Vec<Gc<Symbol>>> {
        context.tracker().track_symbol(
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
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        yield_module_symbol: Option<bool>,
    ) -> io::Result<Vec<Gc<Symbol>>> {
        let chain: Vec<Gc<Symbol>>;
        let is_type_parameter = symbol.flags().intersects(SymbolFlags::TypeParameter);
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
            chain = vec![symbol.symbol_wrapper()];
        }
        Ok(chain)
    }

    pub(super) fn get_symbol_chain(
        &self,
        context: &NodeBuilderContext,
        yield_module_symbol: Option<bool>,
        symbol: &Symbol,
        meaning: Option<SymbolFlags>,
        end_of_chain: bool,
    ) -> io::Result<Option<Vec<Gc<Symbol>>>> {
        let mut accessible_symbol_chain = self.type_checker.get_accessible_symbol_chain(
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
            Some(accessible_symbol_chain) => self.type_checker.needs_qualification(
                &accessible_symbol_chain[0],
                context.maybe_enclosing_declaration(),
                if accessible_symbol_chain.len() == 1 {
                    // TODO: ...and here
                    meaning.unwrap()
                } else {
                    self.type_checker.get_qualified_left_meaning(
                        // TODO: ...and here
                        meaning.unwrap(),
                    )
                },
            )?,
        } {
            let parents = self.type_checker.get_containers_of_symbol(
                accessible_symbol_chain
                    .as_ref()
                    .map_or(symbol, |accessible_symbol_chain| {
                        &*accessible_symbol_chain[0]
                    }),
                context.maybe_enclosing_declaration(),
                // TODO: ...or here
                meaning.unwrap(),
            )?;
            if length(parents.as_deref()) > 0 {
                let parents = parents.as_ref().unwrap();
                parent_specifiers = parents
                    .into_iter()
                    .map(|symbol| -> io::Result<_> {
                        Ok(
                            if some(
                                symbol.maybe_declarations().as_deref(),
                                Some(|declaration: &Gc<Node>| {
                                    self.type_checker
                                        .has_non_global_augmentation_external_module_symbol(
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
                for ref parent in sorted_parents {
                    let parent_chain = self.get_symbol_chain(
                        context,
                        yield_module_symbol,
                        parent,
                        Some(self.type_checker.get_qualified_left_meaning(
                            // TODO: ...or here
                            meaning.unwrap(),
                        )),
                        false,
                    )?;
                    if let Some(mut parent_chain) = parent_chain {
                        if matches!(
                            parent.maybe_exports().as_ref(),
                            Some(parent_exports) if matches!(
                                (**parent_exports).borrow().get(InternalSymbolName::ExportEquals),
                                Some(parent_exports_get) if self.type_checker.get_symbol_if_same_reference(
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
                                    .get_alias_for_symbol_in_container(parent, symbol)?
                                    .unwrap_or_else(|| symbol.symbol_wrapper())])
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
                .flags()
                .intersects(SymbolFlags::TypeLiteral | SymbolFlags::ObjectLiteral)
        {
            if !end_of_chain
                && yield_module_symbol != Some(true)
                && maybe_for_each_bool(
                    symbol.maybe_declarations().as_ref(),
                    |declaration: &Gc<Node>, _| {
                        self.type_checker
                            .has_non_global_augmentation_external_module_symbol(declaration)
                    },
                )
            {
                return Ok(None);
            }
            return Ok(Some(vec![symbol.symbol_wrapper()]));
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
        symbol: &Symbol,
        context: &NodeBuilderContext,
    ) -> io::Result<Option<Gc<NodeArray> /*<TypeParameterDeclaration>*/>> {
        let mut type_parameter_nodes: Option<Gc<NodeArray> /*TypeParameterDeclaration*/> = None;
        let target_symbol = self.type_checker.get_target_symbol(symbol);
        if target_symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface | SymbolFlags::TypeAlias)
        {
            type_parameter_nodes = Some(
                get_factory().create_node_array(
                    try_maybe_map(
                        self.type_checker
                            .get_local_type_parameters_of_class_or_interface_or_type_alias(symbol)?
                            .as_ref(),
                        |tp: &Id<Type>, _| self.type_parameter_to_declaration_(tp, context, None),
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
        chain: &[Gc<Symbol>],
        index: usize,
        context: &NodeBuilderContext,
    ) -> io::Result<Option<Vec<Gc<Node>>>> {
        Debug_.assert(/*chain && 0 <= index &&*/ index < chain.len(), None);
        let symbol = &chain[index];
        let symbol_id = get_symbol_id(symbol);
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
            Vec<Gc<Node /*TypeNode[] | TypeParameterDeclaration[]*/>>,
        > = None;
        if context
            .flags()
            .intersects(NodeBuilderFlags::WriteTypeParametersInQualifiedName)
            && index < chain.len() - 1
        {
            let parent_symbol = symbol;
            let next_symbol = &chain[index + 1];
            if get_check_flags(next_symbol).intersects(CheckFlags::Instantiated) {
                let params = self
                    .type_checker
                    .get_type_parameters_of_class_or_interface(&*if parent_symbol
                        .flags()
                        .intersects(SymbolFlags::Alias)
                    {
                        self.type_checker.resolve_alias(parent_symbol)?
                    } else {
                        parent_symbol.clone()
                    })?;
                type_parameter_nodes = self.map_to_type_nodes(
                    try_maybe_map(params.as_ref(), |t: &Id<Type>, _| {
                        self.type_checker.get_mapped_type(
                            t,
                            (*next_symbol.as_transient_symbol().symbol_links())
                                .borrow()
                                .mapper
                                .as_ref()
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
                    .map(|node_array| node_array.to_vec());
            }
        }
        Ok(type_parameter_nodes)
    }

    pub(super) fn get_topmost_indexed_access_type(
        &self,
        top: &Node, /*IndexedAccessTypeNode*/
    ) -> Gc<Node /*IndexedAccessTypeNode*/> {
        let top_as_indexed_access_type_node = top.as_indexed_access_type_node();
        if is_indexed_access_type_node(&top_as_indexed_access_type_node.object_type) {
            return self
                .get_topmost_indexed_access_type(&top_as_indexed_access_type_node.object_type);
        }
        top.node_wrapper()
    }

    pub(super) fn get_specifier_for_module_symbol(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
    ) -> io::Result<String> {
        let mut file = get_declaration_of_kind(symbol, SyntaxKind::SourceFile);
        if file.is_none() {
            let equivalent_file_symbol = try_maybe_first_defined(
                symbol.maybe_declarations().as_ref(),
                |d: &Gc<Node>, _| {
                    self.type_checker
                        .get_file_symbol_if_file_symbol_export_equals_container(d, symbol)
                },
            )?;
            if let Some(equivalent_file_symbol) = equivalent_file_symbol.as_ref() {
                file = get_declaration_of_kind(equivalent_file_symbol, SyntaxKind::SourceFile);
            }
        }
        if let Some(file_module_name) = file
            .as_ref()
            .and_then(|file| file.as_source_file().maybe_module_name().clone())
        {
            return Ok(file_module_name);
        }
        if file.is_none() {
            if context
                .tracker()
                .is_track_referenced_ambient_module_supported()
            {
                let ambient_decls = maybe_filter(
                    symbol.maybe_declarations().as_deref(),
                    |declaration: &Gc<Node>| is_ambient_module(declaration),
                );
                if length(ambient_decls.as_deref()) > 0 {
                    for decl in ambient_decls.as_ref().unwrap() {
                        context
                            .tracker()
                            .track_referenced_ambient_module(decl, symbol)?;
                    }
                }
            }
            if ambient_module_symbol_regex.is_match(symbol.escaped_name()) {
                return Ok(symbol.escaped_name()[1..symbol.escaped_name().len() - 1].to_owned());
            }
        }
        if context.maybe_enclosing_declaration().is_none()
            || !context.tracker().is_module_resolver_host_supported()
        {
            if ambient_module_symbol_regex.is_match(symbol.escaped_name()) {
                return Ok(symbol.escaped_name()[1..symbol.escaped_name().len() - 1].to_owned());
            }
            return Ok(
                get_source_file_of_node(&get_non_augmentation_declaration(symbol).unwrap())
                    .as_source_file()
                    .file_name()
                    .clone(),
            );
        }
        let context_file =
            get_source_file_of_node(&get_original_node(&context.enclosing_declaration()));
        let links = self.type_checker.get_symbol_links(symbol);
        let mut specifier =
            (*links)
                .borrow()
                .specifier_cache
                .as_ref()
                .and_then(|links_specifier_cache| {
                    links_specifier_cache
                        .get(&**context_file.as_source_file().path())
                        .cloned()
                });
        if specifier.is_none() {
            let is_bundle = matches!(
                out_file(&self.type_checker.compiler_options),
                Some(out_file) if !out_file.is_empty()
            );
            let context_tracker = context.tracker();
            let module_resolver_host = context_tracker.module_resolver_host().unwrap();
            let specifier_compiler_options = if is_bundle {
                Gc::new(CompilerOptions {
                    base_url: Some(module_resolver_host.get_common_source_directory()),
                    ..(*self.type_checker.compiler_options).clone()
                })
            } else {
                self.type_checker.compiler_options.clone()
            };
            specifier = Some(
                first(&module_specifiers::get_module_specifiers(
                    symbol,
                    &self.type_checker,
                    specifier_compiler_options.clone(),
                    &context_file,
                    module_resolver_host.as_dyn_module_specifier_resolution_host(),
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
                )?)
                .clone(),
            );
            links
                .borrow_mut()
                .specifier_cache
                .get_or_insert_default_()
                .insert(
                    context_file.as_source_file().path().to_string(),
                    specifier.clone().unwrap(),
                );
        }
        Ok(specifier.unwrap())
    }

    pub(super) fn symbol_to_entity_name_node(&self, symbol: &Symbol) -> Gc<Node /*EntityName*/> {
        let identifier =
            get_factory().create_identifier(&unescape_leading_underscores(symbol.escaped_name()));
        if let Some(symbol_parent) = symbol.maybe_parent().as_ref() {
            get_factory()
                .create_qualified_name(self.symbol_to_entity_name_node(symbol_parent), identifier)
        } else {
            identifier
        }
    }

    pub(super) fn symbol_to_type_node(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: SymbolFlags,
        override_type_arguments: Option<&[Gc<Node /*TypeNode*/>]>,
    ) -> io::Result<Gc<Node /*TypeNode*/>> {
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
            chain[0].maybe_declarations().as_deref(),
            Some(|declaration: &Gc<Node>| {
                self.type_checker
                    .has_non_global_augmentation_external_module_symbol(declaration)
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
            let specifier = self.get_specifier_for_module_symbol(&chain[0], context)?;
            if !context
                .flags()
                .intersects(NodeBuilderFlags::AllowNodeModulesRelativePaths)
                && get_emit_module_resolution_kind(&self.type_checker.compiler_options)
                    != ModuleResolutionKind::Classic
                && specifier.contains("/node_modules/")
            {
                context.set_encountered_error(true);
                context
                    .tracker()
                    .report_likely_unsafe_import_required_error(&specifier);
            }
            let lit: Gc<Node> = get_factory().create_literal_type_node(
                get_factory().create_string_literal(specifier.clone(), None, None),
            );
            context
                .tracker()
                .track_external_module_symbol_of_import_type_node(&chain[0]);
            context.increment_approximate_length_by(specifier.len() + 10);
            if match non_root_parts.as_ref() {
                None => true,
                Some(non_root_parts) => is_entity_name(non_root_parts),
            } {
                if let Some(non_root_parts) = non_root_parts.as_ref() {
                    let last_id = if is_identifier(non_root_parts) {
                        non_root_parts.clone()
                    } else {
                        non_root_parts.as_qualified_name().right.clone()
                    };
                    *last_id.as_identifier().maybe_type_arguments_mut() = None;
                }
                return Ok(get_factory().create_import_type_node(
                    lit,
                    non_root_parts,
                    type_parameter_nodes,
                    Some(is_type_of),
                ));
            } else {
                let split_node =
                    self.get_topmost_indexed_access_type(non_root_parts.as_ref().unwrap());
                let qualifier = &split_node
                    .as_indexed_access_type_node()
                    .object_type
                    .as_type_reference_node()
                    .type_name;
                return Ok(get_factory().create_indexed_access_type_node(
                    get_factory().create_import_type_node(
                        lit,
                        Some(qualifier.clone()),
                        type_parameter_nodes,
                        Some(is_type_of),
                    ),
                    split_node.as_indexed_access_type_node().index_type.clone(),
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
        if is_indexed_access_type_node(&entity_name) {
            return Ok(entity_name);
        }
        Ok(if is_type_of {
            get_factory().create_type_query_node(entity_name)
        } else {
            let last_id = if is_identifier(&entity_name) {
                entity_name.clone()
            } else {
                entity_name.as_qualified_name().right.clone()
            };
            let last_type_args = last_id.as_identifier().maybe_type_arguments_mut().take();
            get_factory().create_type_reference_node(entity_name, last_type_args)
        })
    }
}

pub(super) struct SignatureToSignatureDeclarationOptions<TPrivateSymbolVisitor: Fn(&Symbol)> {
    pub modifiers: Option<Vec<Gc<Node /*Modifier*/>>>,
    pub name: Option<Gc<Node /*PropertyName*/>>,
    pub question_token: Option<Gc<Node /*QuestionToken*/>>,
    pub private_symbol_visitor: Option<TPrivateSymbolVisitor>,
    pub bundled_imports: Option<bool>,
}
