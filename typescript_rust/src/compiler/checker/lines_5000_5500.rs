#![allow(non_upper_case_globals)]

use std::cell::RefCell;
use std::cmp;
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

use super::{
    get_node_id, get_symbol_id, MappedTypeModifiers, NodeBuilderContext,
    SignatureToSignatureDeclarationOptions,
};
use crate::{
    contains_rc, count_where, factory, filter, first, get_check_flags,
    get_declaration_modifier_flags_from_symbol, get_name_of_declaration, get_object_flags,
    get_parse_tree_node, is_binary_expression, is_class_like, is_element_access_expression,
    is_identifier, is_import_type_node, is_property_access_entity_name_expression, is_static, last,
    length, map, maybe_for_each_bool, node_is_synthesized, null_transformation_context,
    range_equals_rc, same_map, set_emit_flags, set_text_range, some, symbol_name,
    synthetic_factory, unescape_leading_underscores, visit_each_child, CheckFlags, Debug_,
    ElementFlags, EmitFlags, HasTypeArgumentsInterface, InterfaceTypeInterface, KeywordTypeNode,
    ModifierFlags, Node, NodeArray, NodeBuilder, NodeBuilderFlags, NodeInterface,
    NodeLinksSerializedType, ObjectFlags, ObjectFlagsTypeInterface, Signature, SignatureFlags,
    SignatureKind, Symbol, SymbolFlags, SymbolInterface, SymbolTracker, SyntaxKind, Type,
    TypeFlags, TypeId, TypeInterface, VisitResult,
};

impl NodeBuilder {
    pub(super) fn conditional_type_to_type_node(
        &self,
        context: &NodeBuilderContext,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Node> {
        let type_as_conditional_type = type_.as_conditional_type();
        let check_type_node = self
            .type_to_type_node_helper(Some(&*type_as_conditional_type.check_type), context)
            .unwrap();
        let save_infer_type_parameters = context.infer_type_parameters.borrow().clone();
        *context.infer_type_parameters.borrow_mut() = (*type_as_conditional_type.root)
            .borrow()
            .infer_type_parameters
            .clone();
        let extends_type_node = self
            .type_to_type_node_helper(Some(&*type_as_conditional_type.extends_type), context)
            .unwrap();
        *context.infer_type_parameters.borrow_mut() = save_infer_type_parameters;
        let true_type_node = self.type_to_type_node_or_circularity_elision(
            context,
            &self.type_checker.get_true_type_from_conditional_type(type_),
        );
        let false_type_node = self.type_to_type_node_or_circularity_elision(
            context,
            &self
                .type_checker
                .get_false_type_from_conditional_type(type_),
        );
        context.increment_approximate_length_by(15);
        synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| {
                factory_
                    .create_conditional_type_node(
                        synthetic_factory_,
                        check_type_node,
                        extends_type_node,
                        true_type_node,
                        false_type_node,
                    )
                    .into()
            })
        })
    }

    pub(super) fn type_to_type_node_or_circularity_elision(
        &self,
        context: &NodeBuilderContext,
        type_: &Type,
    ) -> Rc<Node> {
        if type_.flags().intersects(TypeFlags::Union) {
            if matches!(context.visited_types.borrow().as_ref(), Some(visited_types) if visited_types.contains(&self.type_checker.get_type_id(type_)))
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
            return self.visit_and_transform_type(context, type_, |type_| {
                self.type_to_type_node_helper(Some(type_), context).unwrap()
            });
        }
        self.type_to_type_node_helper(Some(type_), context).unwrap()
    }

    pub(super) fn create_mapped_type_node_from_type(
        &self,
        context: &NodeBuilderContext,
        type_: &Type, /*MappedType*/
    ) -> Rc<Node> {
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
        if self
            .type_checker
            .is_mapped_type_with_keyof_constraint_declaration(type_)
        {
            appropriate_constraint_type_node = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_type_operator_node(
                            synthetic_factory_,
                            SyntaxKind::KeyOfKeyword,
                            self.type_to_type_node_helper(
                                Some(self.type_checker.get_modifiers_type_from_mapped_type(type_)),
                                context,
                            )
                            .unwrap(),
                        )
                        .into()
                })
            });
        } else {
            appropriate_constraint_type_node = self
                .type_to_type_node_helper(
                    Some(
                        self.type_checker
                            .get_constraint_type_from_mapped_type(type_),
                    ),
                    context,
                )
                .unwrap();
        }
        let type_parameter_node: Rc<Node> = self.type_parameter_to_declaration_with_constraint(
            &self.type_checker.get_type_parameter_from_mapped_type(type_),
            context,
            Some(appropriate_constraint_type_node),
        );
        let name_type_node: Option<Rc<Node>> =
            if type_declaration_as_mapped_type_node.name_type.is_some() {
                self.type_to_type_node_helper(
                    self.type_checker.get_name_type_from_mapped_type(type_),
                    context,
                )
            } else {
                None
            };
        let template_type_node: Option<Rc<Node>> = self.type_to_type_node_helper(
            Some(
                self.type_checker.remove_missing_type(
                    &self.type_checker.get_template_type_from_mapped_type(type_),
                    self.type_checker
                        .get_mapped_type_modifiers(type_)
                        .intersects(MappedTypeModifiers::IncludeOptional),
                ),
            ),
            context,
        );
        let mapped_type_node: Rc<Node> = synthetic_factory.with(|synthetic_factory_| {
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
        set_emit_flags(mapped_type_node, EmitFlags::SingleLine)
    }

    pub(super) fn create_anonymous_type_node(
        &self,
        context: &NodeBuilderContext,
        type_: &Type, /*ObjectType*/
    ) -> Rc<Node> {
        let type_id = type_.id();
        let symbol = type_.maybe_symbol();
        if let Some(symbol) = symbol {
            let is_instance_type = if self.type_checker.is_class_instance_side(type_) {
                SymbolFlags::Type
            } else {
                SymbolFlags::Value
            };
            if self
                .type_checker
                .is_js_constructor(symbol.maybe_value_declaration())
            {
                self.symbol_to_type_node(&symbol, context, is_instance_type, None)
            } else if symbol.flags().intersects(SymbolFlags::Class)
                && self
                    .type_checker
                    .get_base_type_variable_of_class(&symbol)
                    .is_none()
                && !(matches!(symbol.maybe_value_declaration(), Some(value_declaration) if value_declaration.kind() == SyntaxKind::ClassExpression)
                    && context
                        .flags()
                        .intersects(NodeBuilderFlags::WriteClassExpressionAsTypeLiteral))
                || symbol
                    .flags()
                    .intersects(SymbolFlags::Enum | SymbolFlags::ValueModule)
                || self.should_write_type_of_function_symbol(&symbol, context, type_id)
            {
                self.symbol_to_type_node(&symbol, context, is_instance_type, None)
            } else if matches!(context.visited_types.borrow().as_ref(), Some(visited_types) if visited_types.contains(&type_id))
            {
                let type_alias = self.type_checker.get_type_alias_for_type_literal(type_);
                if let Some(type_alias) = type_alias {
                    self.symbol_to_type_node(&type_alias, context, SymbolFlags::Type, None)
                } else {
                    self.create_elided_information_placeholder(context)
                }
            } else {
                self.visit_and_transform_type(context, type_, |type_| {
                    self.create_type_node_from_object_type(context, type_)
                })
            }
        } else {
            self.create_type_node_from_object_type(context, type_)
        }
    }

    pub(super) fn should_write_type_of_function_symbol(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        type_id: TypeId,
    ) -> bool {
        let is_static_method_symbol = symbol.flags().intersects(SymbolFlags::Method)
            && some(
                symbol.maybe_declarations().as_deref(),
                Some(|declaration: &Rc<Node>| is_static(declaration)),
            );
        let is_non_local_function_symbol = symbol.flags().intersects(SymbolFlags::Function)
            && (symbol.maybe_parent().is_some()
                || maybe_for_each_bool(
                    symbol.maybe_declarations().as_deref(),
                    |declaration: &Rc<Node>, _| {
                        matches!(
                            declaration.parent().kind(),
                            SyntaxKind::SourceFile | SyntaxKind::ModuleBlock
                        )
                    },
                ));
        if is_static_method_symbol || is_non_local_function_symbol {
            return (context
                .flags()
                .intersects(NodeBuilderFlags::UseTypeOfFunction)
                || matches!(context.visited_types.borrow().as_ref(), Some(visited_types) if visited_types.contains(&type_id)))
                && (!context
                    .flags()
                    .intersects(NodeBuilderFlags::UseStructuralFallback)
                    || self.type_checker.is_value_symbol_accessible(
                        symbol,
                        context.maybe_enclosing_declaration().as_deref(),
                    ));
        }
        false
    }

    pub(super) fn visit_and_transform_type<TTransform: FnMut(&Type) -> Rc<Node>>(
        &self,
        context: &NodeBuilderContext,
        type_: &Type,
        mut transform: TTransform,
    ) -> Rc<Node> {
        let type_id = type_.id();
        let is_constructor_object = get_object_flags(type_).intersects(ObjectFlags::Anonymous)
            && matches!(type_.maybe_symbol(), Some(symbol) if symbol.flags().intersects(SymbolFlags::Class));
        let id = if get_object_flags(type_).intersects(ObjectFlags::Reference)
            && type_
                .maybe_as_type_reference()
                .and_then(|type_| type_.node.borrow().clone())
                .is_some()
        {
            Some(format!(
                "N{}",
                get_node_id(type_.as_type_reference().node.borrow().as_ref().unwrap())
            ))
        } else if type_.flags().intersects(TypeFlags::Conditional) {
            Some(format!(
                "N{}",
                get_node_id(&(*type_.as_conditional_type().root).borrow().node.clone())
            ))
        } else if let Some(type_symbol) = type_.maybe_symbol() {
            Some(format!(
                "{}{}",
                if is_constructor_object { "+" } else { "" },
                get_symbol_id(&type_symbol)
            ))
        } else {
            None
        };
        if context.visited_types.borrow().is_none() {
            *context.visited_types.borrow_mut() = Some(HashSet::new());
        }
        if id.is_some() && context.symbol_depth.borrow().is_none() {
            *context.symbol_depth.borrow_mut() = Some(HashMap::new());
        }
        let links = context
            .maybe_enclosing_declaration()
            .as_ref()
            .map(|enclosing_declaration| self.type_checker.get_node_links(enclosing_declaration));
        let key = format!(
            "{}|{}",
            self.type_checker.get_type_id(type_),
            context.flags().bits()
        );
        if let Some(links) = links.as_ref() {
            let mut links = links.borrow_mut();
            if links.serialized_types.is_none() {
                links.serialized_types = Some(HashMap::new());
            }
        }
        let cached_result = links.as_ref().and_then(|links| {
            RefCell::borrow(links)
                .serialized_types
                .as_ref()
                .unwrap()
                .get(&key)
                .map(Clone::clone)
        });
        if let Some(cached_result) = cached_result.as_ref() {
            if matches!(cached_result.truncating, Some(true)) {
                context.truncating.set(Some(true));
            }
            context.increment_approximate_length_by(cached_result.added_length);
            return self.deep_clone_or_reuse_node(&cached_result.node);
        }
        let mut depth: Option<usize> = None;
        if let Some(id) = id.as_ref() {
            depth = Some(
                context
                    .symbol_depth
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .get(id)
                    .copied()
                    .unwrap_or(0),
            );
            if depth.unwrap() > 10 {
                return self.create_elided_information_placeholder(context);
            }
            context
                .symbol_depth
                .borrow_mut()
                .as_mut()
                .unwrap()
                .insert(id.clone(), depth.unwrap() + 1);
        }
        context
            .visited_types
            .borrow_mut()
            .as_mut()
            .unwrap()
            .insert(type_id);
        let start_length = context.approximate_length.get();
        let result = transform(type_);
        let added_length = context.approximate_length.get() - start_length;
        if !context.reported_diagnostic.get() && !context.encountered_error.get() {
            if let Some(links) = links.as_ref() {
                links
                    .borrow_mut()
                    .serialized_types
                    .as_mut()
                    .unwrap()
                    .insert(
                        key,
                        NodeLinksSerializedType {
                            truncating: if matches!(context.truncating.get(), Some(true)) {
                                Some(true)
                            } else {
                                None
                            },
                            added_length,
                            node: result.clone(),
                        },
                    );
            }
        }
        context
            .visited_types
            .borrow_mut()
            .as_mut()
            .unwrap()
            .remove(&type_id);
        if let Some(id) = id {
            context
                .symbol_depth
                .borrow_mut()
                .as_mut()
                .unwrap()
                .insert(id, depth.unwrap());
        }
        result
    }

    pub(super) fn deep_clone_or_reuse_node(&self, node: &Node) -> Rc<Node> {
        if !node_is_synthesized(node)
            && matches!(get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None), Some(parse_tree_node) if ptr::eq(&*parse_tree_node, node))
        {
            return node.node_wrapper();
        }
        let ret = synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| {
                factory_.clone_node(
                    synthetic_factory_,
                    &visit_each_child(
                        Some(node),
                        |node: &Node| Some(vec![self.deep_clone_or_reuse_node(node)]),
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
                    .unwrap(),
                )
            })
        });
        set_text_range(&*ret, Some(node));
        ret
    }

    pub(super) fn create_type_node_from_object_type(
        &self,
        context: &NodeBuilderContext,
        type_: &Type, /*ObjectType*/
    ) -> Rc<Node> {
        if self.type_checker.is_generic_mapped_type(type_)
            || matches!(
                type_
                    .maybe_as_mapped_type()
                    .and_then(|type_| type_.maybe_contains_error()),
                Some(true)
            )
        {
            return self.create_mapped_type_node_from_type(context, type_);
        }

        let resolved = self.type_checker.resolve_structured_type_members(type_);
        let resolved_as_resolved_type = resolved.as_resolved_type();
        if resolved_as_resolved_type.properties().is_empty()
            && resolved_as_resolved_type.index_infos().is_empty()
        {
            if resolved_as_resolved_type.call_signatures().is_empty()
                && resolved_as_resolved_type.construct_signatures().is_empty()
            {
                context.increment_approximate_length_by(2);
                return set_emit_flags(
                    synthetic_factory.with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_
                                .create_type_literal_node(
                                    synthetic_factory_,
                                    Option::<NodeArray>::None,
                                )
                                .into()
                        })
                    }),
                    EmitFlags::SingleLine,
                );
            }

            if resolved_as_resolved_type.call_signatures().len() == 1
                && resolved_as_resolved_type.construct_signatures().is_empty()
            {
                let signature = &resolved_as_resolved_type.call_signatures()[0];
                let signature_node = self.signature_to_signature_declaration_helper(
                    signature.clone(),
                    SyntaxKind::FunctionType,
                    context,
                    Option::<SignatureToSignatureDeclarationOptions<fn(&Symbol)>>::None,
                );
                return signature_node;
            }

            if resolved_as_resolved_type.construct_signatures().len() == 1
                && resolved_as_resolved_type.call_signatures().is_empty()
            {
                let signature = &resolved_as_resolved_type.construct_signatures()[0];
                let signature_node = self.signature_to_signature_declaration_helper(
                    signature.clone(),
                    SyntaxKind::ConstructorType,
                    context,
                    Option::<SignatureToSignatureDeclarationOptions<fn(&Symbol)>>::None,
                );
                return signature_node;
            }
        }

        let abstract_signatures = filter(
            &*resolved_as_resolved_type.construct_signatures(),
            |signature: &Rc<Signature>| signature.flags.intersects(SignatureFlags::Abstract),
        );
        if some(
            Some(&abstract_signatures),
            Option::<fn(&Rc<Signature>) -> bool>::None,
        ) {
            let mut types = map(&abstract_signatures, |signature: &Rc<Signature>, _| {
                self.type_checker
                    .get_or_create_type_from_signature(signature.clone())
            });
            let type_element_count = resolved_as_resolved_type.call_signatures().len()
                + (resolved_as_resolved_type.construct_signatures().len()
                    - abstract_signatures.len())
                + resolved_as_resolved_type.index_infos().len()
                + if context
                    .flags()
                    .intersects(NodeBuilderFlags::WriteClassExpressionAsTypeLiteral)
                {
                    count_where(
                        Some(&*resolved_as_resolved_type.properties()),
                        |p: &Rc<Symbol>, _| !p.flags().intersects(SymbolFlags::Prototype),
                    )
                } else {
                    length(Some(&*resolved_as_resolved_type.properties()))
                };
            if type_element_count > 0 {
                types.push(
                    self.type_checker
                        .get_resolved_type_without_abstract_construct_signatures(&resolved),
                );
            }
            return self
                .type_to_type_node_helper(
                    Some(&*self.type_checker.get_intersection_type(
                        &types,
                        Option::<&Symbol>::None,
                        None,
                    )),
                    context,
                )
                .unwrap();
        }

        let saved_flags = context.flags();
        context.set_flags(context.flags() | NodeBuilderFlags::InObjectTypeLiteral);
        let members = self.create_type_nodes_from_resolved_type(context, &resolved);
        context.set_flags(saved_flags);
        let type_literal_node: Rc<Node> = synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| {
                factory_
                    .create_type_literal_node(synthetic_factory_, members)
                    .into()
            })
        });
        context.increment_approximate_length_by(2);
        set_emit_flags(
            type_literal_node.clone(),
            if context
                .flags()
                .intersects(NodeBuilderFlags::MultilineObjectLiterals)
            {
                EmitFlags::None
            } else {
                EmitFlags::SingleLine
            },
        );
        type_literal_node
    }

    pub(super) fn type_reference_to_type_node(
        &self,
        context: &NodeBuilderContext,
        type_: &Type, /*TypeReference*/
    ) -> Option<Rc<Node>> {
        let type_arguments = self.type_checker.get_type_arguments(type_);
        let type_as_type_reference = type_.as_type_reference_interface();
        let type_target = type_as_type_reference.target();
        if Rc::ptr_eq(&type_target, &self.type_checker.global_array_type())
            || Rc::ptr_eq(
                &type_target,
                &self.type_checker.global_readonly_array_type(),
            )
        {
            if context
                .flags()
                .intersects(NodeBuilderFlags::WriteArrayAsGenericType)
            {
                let type_argument_node = self
                    .type_to_type_node_helper(Some(&*type_arguments[0]), context)
                    .unwrap();
                return Some(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_type_reference_node(
                                synthetic_factory_,
                                if Rc::ptr_eq(&type_target, &self.type_checker.global_array_type())
                                {
                                    "Array"
                                } else {
                                    "ReadonlyArray"
                                },
                                Some(vec![type_argument_node]),
                            )
                            .into()
                    })
                }));
            }
            let element_type = self
                .type_to_type_node_helper(Some(&*type_arguments[0]), context)
                .unwrap();
            let array_type: Rc<Node> = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_array_type_node(synthetic_factory_, element_type)
                        .into()
                })
            });
            Some(
                if Rc::ptr_eq(&type_target, &self.type_checker.global_array_type()) {
                    array_type
                } else {
                    synthetic_factory.with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_
                                .create_type_operator_node(
                                    synthetic_factory_,
                                    SyntaxKind::ReadonlyKeyword,
                                    array_type,
                                )
                                .into()
                        })
                    })
                },
            )
        } else if type_target
            .as_object_type()
            .object_flags()
            .intersects(ObjectFlags::Tuple)
        {
            let type_target_as_tuple_type = type_target.as_tuple_type();
            let type_arguments = same_map(&type_arguments, |t: &Rc<Type>, i| {
                self.type_checker.remove_missing_type(
                    t,
                    type_target_as_tuple_type.element_flags[i].intersects(ElementFlags::Optional),
                )
            });
            if !type_arguments.is_empty() {
                let arity = self.type_checker.get_type_reference_arity(type_);
                let tuple_constituent_nodes =
                    self.map_to_type_nodes(Some(&type_arguments[0..arity]), context, None);
                if let Some(mut tuple_constituent_nodes) = tuple_constituent_nodes {
                    if let Some(type_target_labeled_element_declarations) =
                        type_target_as_tuple_type
                            .labeled_element_declarations
                            .as_ref()
                    {
                        for i in 0..tuple_constituent_nodes.len() {
                            let tuple_constituent_node = tuple_constituent_nodes[i].clone();
                            let flags = type_target_as_tuple_type.element_flags[i];
                            tuple_constituent_nodes[i] = synthetic_factory.with(|synthetic_factory_| {
                                factory.with(|factory_| {
                                    factory_
                                        .create_named_tuple_member(
                                            synthetic_factory_,
                                            if flags.intersects(ElementFlags::Variable) {
                                                Some(factory_.create_token(synthetic_factory_, SyntaxKind::DotDotDotToken).into())
                                            } else {
                                                None
                                            },
                                            factory_.create_identifier(
                                                synthetic_factory_,
                                                &unescape_leading_underscores(
                                                    &self.type_checker.get_tuple_element_label(
                                                        &type_target_labeled_element_declarations[i]
                                                    )
                                                ),
                                                Option::<NodeArray>::None,
                                                None,
                                            ).into(),
                                            if flags.intersects(ElementFlags::Optional) {
                                                Some(factory_.create_token(synthetic_factory_, SyntaxKind::QuestionToken).into())
                                            } else {
                                                None
                                            },
                                            if flags.intersects(ElementFlags::Rest) {
                                                factory_.create_array_type_node(synthetic_factory_, tuple_constituent_node).into()
                                            } else {
                                                tuple_constituent_node
                                            },
                                        )
                                        .into()
                                })
                            });
                        }
                    } else {
                        for i in 0..cmp::min(arity, tuple_constituent_nodes.len()) {
                            let tuple_constituent_node = tuple_constituent_nodes[i].clone();
                            let flags = type_target_as_tuple_type.element_flags[i];
                            tuple_constituent_nodes[i] = if flags.intersects(ElementFlags::Variable)
                            {
                                synthetic_factory.with(|synthetic_factory_| {
                                    factory.with(|factory_| {
                                        factory_
                                            .create_rest_type_node(
                                                synthetic_factory_,
                                                if flags.intersects(ElementFlags::Rest) {
                                                    factory_
                                                        .create_array_type_node(
                                                            synthetic_factory_,
                                                            tuple_constituent_node,
                                                        )
                                                        .into()
                                                } else {
                                                    tuple_constituent_node
                                                },
                                            )
                                            .into()
                                    })
                                })
                            } else if flags.intersects(ElementFlags::Optional) {
                                synthetic_factory.with(|synthetic_factory_| {
                                    factory.with(|factory_| {
                                        factory_
                                            .create_optional_type_node(
                                                synthetic_factory_,
                                                tuple_constituent_node,
                                            )
                                            .into()
                                    })
                                })
                            } else {
                                tuple_constituent_node
                            };
                        }
                    }
                    let tuple_type_node = set_emit_flags(
                        synthetic_factory.with(|synthetic_factory_| {
                            factory.with(|factory_| {
                                factory_
                                    .create_tuple_type_node(
                                        synthetic_factory_,
                                        Some(tuple_constituent_nodes),
                                    )
                                    .into()
                            })
                        }),
                        EmitFlags::SingleLine,
                    );
                    return Some(if type_target_as_tuple_type.readonly {
                        synthetic_factory.with(|synthetic_factory_| {
                            factory.with(|factory_| {
                                factory_
                                    .create_type_operator_node(
                                        synthetic_factory_,
                                        SyntaxKind::ReadonlyKeyword,
                                        tuple_type_node,
                                    )
                                    .into()
                            })
                        })
                    } else {
                        tuple_type_node
                    });
                }
            }
            if context.encountered_error.get()
                || context
                    .flags()
                    .intersects(NodeBuilderFlags::AllowEmptyTuple)
            {
                let tuple_type_node = set_emit_flags(
                    synthetic_factory.with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_
                                .create_tuple_type_node(synthetic_factory_, Some(vec![]))
                                .into()
                        })
                    }),
                    EmitFlags::SingleLine,
                );
                return Some(if type_target_as_tuple_type.readonly {
                    synthetic_factory.with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_
                                .create_type_operator_node(
                                    synthetic_factory_,
                                    SyntaxKind::ReadonlyKeyword,
                                    tuple_type_node,
                                )
                                .into()
                        })
                    })
                } else {
                    tuple_type_node
                });
            }
            context.encountered_error.set(true);
            return None;
        } else if context
            .flags()
            .intersects(NodeBuilderFlags::WriteClassExpressionAsTypeLiteral)
            && matches!(type_.symbol().maybe_value_declaration(), Some(value_declaration) if is_class_like(&value_declaration))
            && !self.type_checker.is_value_symbol_accessible(
                &type_.symbol(),
                context.maybe_enclosing_declaration().as_deref(),
            )
        {
            Some(self.create_anonymous_type_node(context, type_))
        } else {
            let outer_type_parameters = type_target
                .as_interface_type()
                .maybe_outer_type_parameters();
            let mut i = 0;
            let mut result_type: Option<Rc<Node /*TypeReferenceNode | ImportTypeNode*/>> = None;
            if let Some(outer_type_parameters) = outer_type_parameters {
                let length = outer_type_parameters.len();
                while i < length {
                    let start = i;
                    let parent = self
                        .type_checker
                        .get_parent_symbol_of_type_parameter(&outer_type_parameters[i])
                        .unwrap();
                    while {
                        i += 1;
                        i < length
                            && matches!(self.type_checker.get_parent_symbol_of_type_parameter(&outer_type_parameters[i]), Some(parent_symbol) if Rc::ptr_eq(&parent_symbol, &parent))
                    } {}
                    if !range_equals_rc(outer_type_parameters, &type_arguments, start, i) {
                        let type_argument_slice =
                            self.map_to_type_nodes(Some(&type_arguments[start..i]), context, None);
                        let flags = context.flags();
                        context.set_flags(
                            context.flags() | NodeBuilderFlags::ForbidIndexedAccessSymbolReferences,
                        );
                        let ref_ = self.symbol_to_type_node(
                            &parent,
                            context,
                            SymbolFlags::Type,
                            type_argument_slice.as_deref(),
                        );
                        context.set_flags(flags);
                        result_type = match result_type {
                            None => Some(ref_),
                            Some(result_type) => {
                                Some(self.append_reference_to_type(&result_type, &ref_))
                            }
                        };
                    }
                }
            }
            let mut type_argument_nodes: Option<Vec<Rc<Node /*TypeNode*/>>> = None;
            if !type_arguments.is_empty() {
                let type_parameter_count = type_target
                    .as_interface_type()
                    .maybe_type_parameters()
                    .map_or(0, |type_parameters| type_parameters.len());
                type_argument_nodes = self.map_to_type_nodes(
                    Some(&type_arguments[i..type_parameter_count]),
                    context,
                    None,
                );
            }
            let flags = context.flags();
            context
                .set_flags(context.flags() | NodeBuilderFlags::ForbidIndexedAccessSymbolReferences);
            let final_ref = self.symbol_to_type_node(
                &type_.symbol(),
                context,
                SymbolFlags::Type,
                type_argument_nodes.as_deref(),
            );
            context.set_flags(flags);
            Some(match result_type {
                None => final_ref,
                Some(result_type) => self.append_reference_to_type(&result_type, &final_ref),
            })
        }
    }

    pub(super) fn append_reference_to_type(
        &self,
        root: &Node, /*TypeReferenceNode | ImportTypeNode*/
        ref_: &Node, /*TypeReferenceNode*/
    ) -> Rc<Node /*TypeReferenceNode | ImportTypeNode*/> {
        if is_import_type_node(root) {
            let root_as_import_type_node = root.as_import_type_node();
            let type_arguments = root_as_import_type_node.maybe_type_arguments();
            let type_arguments = type_arguments.as_ref();
            let mut qualifier: Option<Rc<Node>> = root_as_import_type_node.qualifier.clone();
            if let Some(qualifier_present) = qualifier {
                if is_identifier(&qualifier_present) {
                    qualifier = Some(synthetic_factory.with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_.update_identifier(
                                synthetic_factory_,
                                &qualifier_present,
                                type_arguments.cloned(),
                            )
                        })
                    }));
                } else {
                    qualifier = Some(synthetic_factory.with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_.update_qualified_name(
                                synthetic_factory_,
                                &qualifier_present,
                                qualifier_present.as_qualified_name().left.clone(),
                                factory_.update_identifier(
                                    synthetic_factory_,
                                    &qualifier_present.as_qualified_name().right,
                                    type_arguments.cloned(),
                                ),
                            )
                        })
                    }));
                }
            }
            let type_arguments = ref_.as_type_reference_node().maybe_type_arguments();
            let type_arguments = type_arguments.as_deref();
            let ids = self.get_access_stack(ref_);
            for id in ids {
                qualifier = Some(if let Some(qualifier) = qualifier {
                    synthetic_factory.with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_
                                .create_qualified_name(synthetic_factory_, qualifier, id)
                                .into()
                        })
                    })
                } else {
                    id
                });
            }
            synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .update_import_type_node(
                            synthetic_factory_,
                            root,
                            root_as_import_type_node.argument.clone(),
                            qualifier,
                            type_arguments.map(ToOwned::to_owned),
                            Some(root_as_import_type_node.is_type_of()),
                        )
                        .into()
                })
            })
        } else {
            let root_as_type_reference_node = root.as_type_reference_node();
            let type_arguments = root_as_type_reference_node.maybe_type_arguments();
            let type_arguments = type_arguments.as_ref();
            let mut type_name: Rc<Node> = root_as_type_reference_node.type_name.clone();
            if is_identifier(&type_name) {
                type_name = synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_.update_identifier(
                            synthetic_factory_,
                            &type_name,
                            type_arguments.cloned(),
                        )
                    })
                });
            } else {
                type_name = synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_.update_qualified_name(
                            synthetic_factory_,
                            &type_name,
                            type_name.as_qualified_name().left.clone(),
                            factory_.update_identifier(
                                synthetic_factory_,
                                &type_name.as_qualified_name().right,
                                type_arguments.cloned(),
                            ),
                        )
                    })
                });
            }
            let type_arguments = ref_.as_type_reference_node().maybe_type_arguments();
            let type_arguments = type_arguments.as_ref();
            let ids = self.get_access_stack(ref_);
            for id in ids {
                type_name = synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_qualified_name(synthetic_factory_, type_name, id)
                            .into()
                    })
                });
            }
            synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_.update_type_reference_node(
                        synthetic_factory_,
                        root,
                        type_name,
                        type_arguments.cloned(),
                    )
                })
            })
        }
    }

    pub(super) fn get_access_stack(
        &self,
        ref_: &Node, /*TypeReferenceNode*/
    ) -> Vec<Rc<Node /*Identifier*/>> {
        let mut state: &Rc<Node> = &ref_.as_type_reference_node().type_name;
        let mut ids = vec![];
        while !is_identifier(&state) {
            let state_as_qualified_name = state.as_qualified_name();
            ids.insert(0, state_as_qualified_name.right.clone());
            state = &state_as_qualified_name.left;
        }
        ids.insert(0, state.clone());
        ids
    }

    pub(super) fn create_type_nodes_from_resolved_type(
        &self,
        context: &NodeBuilderContext,
        resolved_type: &Type, /*ResolvedType*/
    ) -> Option<Vec<Rc<Node /*TypeElement*/>>> {
        if self.check_truncation_length(context) {
            return Some(vec![synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_property_signature(
                            synthetic_factory_,
                            Option::<NodeArray>::None,
                            "...",
                            None,
                            None,
                        )
                        .into()
                })
            })]);
        }
        let mut type_elements: Vec<Rc<Node>> = vec![];
        let resolved_type_as_resolved_type = resolved_type.as_resolved_type();
        for signature in &*resolved_type_as_resolved_type.call_signatures() {
            type_elements.push(self.signature_to_signature_declaration_helper(
                signature.clone(),
                SyntaxKind::CallSignature,
                context,
                Option::<SignatureToSignatureDeclarationOptions<fn(&Symbol)>>::None,
            ));
        }
        for signature in &*resolved_type_as_resolved_type.construct_signatures() {
            if signature.flags.intersects(SignatureFlags::Abstract) {
                continue;
            }
            type_elements.push(self.signature_to_signature_declaration_helper(
                signature.clone(),
                SyntaxKind::ConstructSignature,
                context,
                Option::<SignatureToSignatureDeclarationOptions<fn(&Symbol)>>::None,
            ));
        }
        for info in &*resolved_type_as_resolved_type.index_infos() {
            type_elements.push(
                self.index_info_to_index_signature_declaration_helper(
                    info,
                    context,
                    if resolved_type_as_resolved_type
                        .object_flags()
                        .intersects(ObjectFlags::ReverseMapped)
                    {
                        Some(self.create_elided_information_placeholder(context))
                    } else {
                        None
                    },
                ),
            );
        }

        let properties = resolved_type_as_resolved_type.properties();
        // if (!properties) {
        //     return typeElements;
        // }

        let mut i = 0;
        for property_symbol in &*properties {
            i += 1;
            if context
                .flags()
                .intersects(NodeBuilderFlags::WriteClassExpressionAsTypeLiteral)
            {
                if property_symbol.flags().intersects(SymbolFlags::Prototype) {
                    continue;
                }
                if get_declaration_modifier_flags_from_symbol(property_symbol, None)
                    .intersects(ModifierFlags::Private | ModifierFlags::Protected)
                /*&& context.tracker.reportPrivateInBaseOfClassExpression*/
                {
                    context.tracker.report_private_in_base_of_class_expression(
                        &unescape_leading_underscores(property_symbol.escaped_name()),
                    );
                }
            }
            if self.check_truncation_length(context) && i + 2 < properties.len() - 1 {
                type_elements.push(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_property_signature(
                                synthetic_factory_,
                                Option::<NodeArray>::None,
                                &*format!("... {} more ...", properties.len() - 1),
                                None,
                                None,
                            )
                            .into()
                    })
                }));
                self.add_property_to_element_list(
                    &properties[properties.len() - 1],
                    context,
                    &mut type_elements,
                );
                break;
            }
            self.add_property_to_element_list(property_symbol, context, &mut type_elements);
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
    ) -> Rc<Node> {
        context.increment_approximate_length_by(3);
        if !context.flags().intersects(NodeBuilderFlags::NoTruncation) {
            return synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_type_reference_node(
                            synthetic_factory_,
                            Into::<Rc<Node>>::into(factory_.create_identifier(
                                synthetic_factory_,
                                "...",
                                Option::<NodeArray>::None,
                                None,
                            )),
                            Option::<NodeArray>::None,
                        )
                        .into()
                })
            });
        }
        synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| {
                Into::<KeywordTypeNode>::into(
                    factory_.create_keyword_type_node(synthetic_factory_, SyntaxKind::AnyKeyword),
                )
                .into()
            })
        })
    }

    pub(super) fn should_use_placeholder_for_property(
        &self,
        property_symbol: &Symbol,
        context: &NodeBuilderContext,
    ) -> bool {
        get_check_flags(property_symbol).intersects(CheckFlags::ReverseMapped)
            && (contains_rc(
                context.reverse_mapped_stack.borrow().as_deref(),
                &property_symbol.symbol_wrapper(),
            ) || matches!(
                context.reverse_mapped_stack.borrow().as_ref(),
                Some(reverse_mapped_stack) if reverse_mapped_stack.get(0).is_some()
            ) && !get_object_flags(
                &last(context.reverse_mapped_stack.borrow().as_deref().unwrap())
                    .as_reverse_mapped_symbol()
                    .property_type,
            )
            .intersects(ObjectFlags::Anonymous))
    }

    pub(super) fn add_property_to_element_list(
        &self,
        property_symbol: &Symbol,
        context: &NodeBuilderContext,
        type_elements: &mut Vec<Rc<Node /*TypeElement*/>>,
    ) {
        let property_is_reverse_mapped =
            get_check_flags(property_symbol).intersects(CheckFlags::ReverseMapped);
        let property_type = if self.should_use_placeholder_for_property(property_symbol, context) {
            self.type_checker.any_type()
        } else {
            self.type_checker
                .get_non_missing_type_of_symbol(property_symbol)
        };
        let save_enclosing_declaration = context.maybe_enclosing_declaration();
        context.set_enclosing_declaration(None);
        if context.tracker.is_track_symbol_supported()
            && get_check_flags(property_symbol).intersects(CheckFlags::Late)
            && self
                .type_checker
                .is_late_bound_name(property_symbol.escaped_name())
        {
            if let Some(property_symbol_declarations) =
                property_symbol.maybe_declarations().as_ref()
            {
                let decl: &Rc<Node> = first(property_symbol_declarations);
                if self.type_checker.has_late_bindable_name(decl) {
                    if is_binary_expression(decl) {
                        let name = get_name_of_declaration(Some(&**decl));
                        if let Some(name) = name.as_ref().filter(|name| {
                            is_element_access_expression(name)
                                && is_property_access_entity_name_expression(
                                    &name.as_element_access_expression().argument_expression,
                                )
                        }) {
                            self.track_computed_name(
                                &name.as_element_access_expression().argument_expression,
                                save_enclosing_declaration.as_deref(),
                                context,
                            );
                        }
                    } else {
                        self.track_computed_name(
                            &decl
                                .as_named_declaration()
                                .name()
                                .as_has_expression()
                                .expression(),
                            save_enclosing_declaration.as_deref(),
                            context,
                        );
                    }
                }
            } else if context
                .tracker
                .is_report_non_serializable_property_supported()
            {
                context.tracker.report_non_serializable_property(
                    &self.type_checker.symbol_to_string_(
                        property_symbol,
                        Option::<&Node>::None,
                        None,
                        None,
                        None,
                    ),
                );
            }
        }
        context.set_enclosing_declaration(
            property_symbol
                .maybe_value_declaration()
                .or_else(|| {
                    property_symbol.maybe_declarations().as_ref().and_then(
                        |property_symbol_declarations| property_symbol_declarations.get(0).cloned(),
                    )
                })
                .or_else(|| save_enclosing_declaration.clone()),
        );
        let property_name = self.get_property_name_node_for_symbol(property_symbol, context);
        context.set_enclosing_declaration(save_enclosing_declaration.clone());
        context.increment_approximate_length_by(symbol_name(property_symbol).len() + 1);
        let optional_token: Option<Rc<Node>> =
            if property_symbol.flags().intersects(SymbolFlags::Optional) {
                synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        Some(
                            factory_
                                .create_token(synthetic_factory_, SyntaxKind::QuestionToken)
                                .into(),
                        )
                    })
                })
            } else {
                None
            };
        if property_symbol
            .flags()
            .intersects(SymbolFlags::Function | SymbolFlags::Method)
            && self
                .type_checker
                .get_properties_of_object_type(&property_type)
                .is_empty()
            && !self.type_checker.is_readonly_symbol(property_symbol)
        {
            let signatures = self.type_checker.get_signatures_of_type(
                &self.type_checker.filter_type(&property_type, |t| {
                    !t.flags().intersects(TypeFlags::Undefined)
                }),
                SignatureKind::Call,
            );
            for signature in &signatures {
                let method_declaration = self.signature_to_signature_declaration_helper(
                    signature.clone(),
                    SyntaxKind::MethodSignature,
                    context,
                    Some(SignatureToSignatureDeclarationOptions {
                        modifiers: None,
                        name: Some(property_name.clone()),
                        question_token: optional_token.clone(),
                        bundled_imports: None,
                        private_symbol_visitor: Option::<fn(&Symbol)>::None,
                    }),
                );
                type_elements.push(self.preserve_comments_on(property_symbol, method_declaration));
            }
        } else {
            let property_type_node: Rc<Node>;
            if self.should_use_placeholder_for_property(property_symbol, context) {
                property_type_node = self.create_elided_information_placeholder(context);
            } else {
                if property_is_reverse_mapped {
                    context
                        .reverse_mapped_stack
                        .borrow_mut()
                        .get_or_insert_with(|| vec![])
                        .push(property_symbol.symbol_wrapper());
                }
                property_type_node = if true
                /*propertyType*/
                {
                    self.serialize_type_for_declaration(
                        context,
                        &property_type,
                        property_symbol,
                        save_enclosing_declaration.as_deref(),
                        Option::<&fn(&Symbol)>::None,
                        None,
                    )
                } else {
                    synthetic_factory.with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_
                                .create_keyword_type_node(
                                    synthetic_factory_,
                                    SyntaxKind::AnyKeyword,
                                )
                                .into()
                        })
                    })
                };
                if property_is_reverse_mapped {
                    context
                        .reverse_mapped_stack
                        .borrow_mut()
                        .as_mut()
                        .unwrap()
                        .pop();
                }
            }

            let modifiers: Option<Vec<Rc<Node>>> =
                if self.type_checker.is_readonly_symbol(property_symbol) {
                    Some(vec![synthetic_factory.with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_
                                .create_token(synthetic_factory_, SyntaxKind::ReadonlyKeyword)
                                .into()
                        })
                    })])
                } else {
                    None
                };
            if modifiers.is_some() {
                context.increment_approximate_length_by(9);
            }
            let property_signature: Rc<Node> = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_property_signature(
                            synthetic_factory_,
                            modifiers,
                            property_name,
                            optional_token,
                            Some(property_type_node),
                        )
                        .into()
                })
            });

            type_elements.push(self.preserve_comments_on(property_symbol, property_signature));
        }
    }
}
