use std::{ptr, rc::Rc};

use gc::{Gc, GcCell};
use itertools::Itertools;

use crate::{
    are_option_gcs_equal, array_to_multi_map, can_have_modifiers, create_symbol_table, filter,
    find_ancestor, flat_map, get_assignment_declaration_kind, get_effective_modifier_flags,
    get_source_file_of_node, get_symbol_id, get_text_of_jsdoc_comment, has_syntactic_modifier,
    is_ambient_module, is_binary_expression, is_class_declaration, is_enum_declaration,
    is_enum_member, is_export_assignment, is_export_declaration, is_external_module_reference,
    is_external_or_common_js_module, is_function_declaration, is_global_scope_augmentation,
    is_identifier, is_import_equals_declaration, is_interface_declaration, is_jsdoc_type_alias,
    is_jsdoc_type_expression, is_json_source_file, is_module_declaration, is_namespace_export,
    is_parameter_declaration, is_source_file, is_variable_declaration, is_variable_statement,
    length, map, map_defined, maybe_get_source_file_of_node, maybe_map, parse_node_factory,
    set_parent, set_synthetic_leading_comments_rc, set_text_range_rc_node, some,
    unescape_leading_underscores, with_parse_base_node_factory_and_factory,
    with_synthetic_factory_and_factory, AssignmentDeclarationKind, Debug_, InternalSymbolName,
    MapOrDefault, ModifierFlags, Node, NodeArray, NodeBuilderFlags, NodeFlags, NodeInterface,
    Signature, SignatureKind, StringOrNumber, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    SynthesizedComment, ThenAnd, Type,
};

use super::{SignatureToSignatureDeclarationOptions, SymbolTableToDeclarationStatements};

impl SymbolTableToDeclarationStatements {
    pub(super) fn include_private_symbol(&self, symbol: &Symbol) {
        if some(
            symbol.maybe_declarations().as_deref(),
            Some(|declaration: &Gc<Node>| is_parameter_declaration(declaration)),
        ) {
            return;
        }
        // Debug_.assertIsDefined(deferredPrivatesStack[deferredPrivatesStack.length - 1]);
        Debug_.assert(!self.deferred_privates_stack().is_empty(), None);
        self.get_unused_name(
            unescape_leading_underscores(symbol.escaped_name()),
            Some(symbol),
        );
        let is_external_import_alias = symbol.flags().intersects(SymbolFlags::Alias)
            && !some(
                symbol.maybe_declarations().as_deref(),
                Some(|d: &Gc<Node>| {
                    find_ancestor(Some(&**d), |node: &Node| is_export_declaration(node)).is_some()
                        || is_namespace_export(d)
                        || is_import_equals_declaration(d)
                            && !is_external_module_reference(
                                &d.as_import_equals_declaration().module_reference,
                            )
                }),
            );
        let mut deferred_privates_stack = self.deferred_privates_stack_mut();
        let deferred_privates_stack_index = if is_external_import_alias {
            0
        } else {
            deferred_privates_stack.len() - 1
        };
        deferred_privates_stack[deferred_privates_stack_index]
            .insert(get_symbol_id(symbol), symbol.symbol_wrapper());
    }

    pub(super) fn is_exporting_scope(&self, enclosing_declaration: &Node) -> bool {
        is_source_file(enclosing_declaration)
            && (is_external_or_common_js_module(enclosing_declaration)
                || is_json_source_file(enclosing_declaration))
            || is_ambient_module(enclosing_declaration)
                && !is_global_scope_augmentation(enclosing_declaration)
    }

    pub(super) fn add_result(
        &self,
        node: &Node, /*Statement*/
        additional_modifier_flags: ModifierFlags,
    ) {
        let mut node = node.node_wrapper();
        if can_have_modifiers(&node) {
            let mut new_modifier_flags = ModifierFlags::None;
            let enclosing_declaration = self.context().maybe_enclosing_declaration().and_then(
                |context_enclosing_declaration| {
                    if is_jsdoc_type_alias(&context_enclosing_declaration) {
                        maybe_get_source_file_of_node(Some(&*context_enclosing_declaration))
                    } else {
                        Some(context_enclosing_declaration)
                    }
                },
            );
            if additional_modifier_flags.intersects(ModifierFlags::Export)
                && matches!(
                    enclosing_declaration.as_ref(),
                    Some(enclosing_declaration) if self.is_exporting_scope(enclosing_declaration) ||
                        is_module_declaration(enclosing_declaration)
                )
                && self.can_have_export_modifier(&node)
            {
                new_modifier_flags |= ModifierFlags::Export;
            }
            if self.adding_declare()
                && !new_modifier_flags.intersects(ModifierFlags::Export)
                && match enclosing_declaration.as_ref() {
                    None => true,
                    Some(enclosing_declaration) => {
                        !enclosing_declaration.flags().intersects(NodeFlags::Ambient)
                    }
                }
                && (is_enum_declaration(&node)
                    || is_variable_statement(&node)
                    || is_function_declaration(&node)
                    || is_class_declaration(&node)
                    || is_module_declaration(&node))
            {
                new_modifier_flags |= ModifierFlags::Ambient;
            }
            if additional_modifier_flags.intersects(ModifierFlags::Default)
                && (is_class_declaration(&node)
                    || is_interface_declaration(&node)
                    || is_function_declaration(&node))
            {
                new_modifier_flags |= ModifierFlags::Default;
            }
            if new_modifier_flags != ModifierFlags::None {
                node = with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                    factory_.update_modifiers(
                        synthetic_factory_,
                        &node,
                        new_modifier_flags | get_effective_modifier_flags(&node),
                    )
                });
            }
        }
        self.results_mut().push(node);
    }

    pub(super) fn serialize_type_alias(
        &self,
        symbol: &Symbol,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        let alias_type = self.type_checker.get_declared_type_of_type_alias(symbol);
        let type_params = (*self.type_checker.get_symbol_links(symbol))
            .borrow()
            .type_parameters
            .clone();
        let type_param_decls = maybe_map(type_params.as_ref(), |p: &Gc<Type>, _| {
            self.node_builder
                .type_parameter_to_declaration_(p, &self.context(), None)
        });
        let jsdoc_alias_decl =
            symbol
                .maybe_declarations()
                .as_ref()
                .and_then(|symbol_declarations| {
                    symbol_declarations
                        .iter()
                        .find(|declaration: &&Gc<Node>| is_jsdoc_type_alias(declaration))
                        .cloned()
                });
        let comment = jsdoc_alias_decl.as_ref().and_then(|jsdoc_alias_decl| {
            jsdoc_alias_decl
                .as_jsdoc_tag()
                .maybe_comment()
                .cloned()
                .or_else(|| jsdoc_alias_decl.parent().as_jsdoc().comment.clone())
        });
        let comment_text = get_text_of_jsdoc_comment(comment.as_ref());
        let old_flags = self.context().flags();
        self.context()
            .set_flags(self.context().flags() | NodeBuilderFlags::InTypeAlias);
        let old_enclosing_decl = self.context().maybe_enclosing_declaration();
        self.context()
            .set_enclosing_declaration(jsdoc_alias_decl.clone());
        let type_node = jsdoc_alias_decl
            .as_ref()
            .and_then(|jsdoc_alias_decl| {
                jsdoc_alias_decl
                    .as_jsdoc_type_like_tag()
                    .maybe_type_expression()
            })
            .filter(|jsdoc_alias_decl_type_expression| {
                is_jsdoc_type_expression(jsdoc_alias_decl_type_expression)
            })
            .and_then(|jsdoc_alias_decl_type_expression| {
                self.node_builder.serialize_existing_type_node(
                    &self.context(),
                    &jsdoc_alias_decl_type_expression
                        .as_jsdoc_type_expression()
                        .type_,
                    Some(&|symbol: &Symbol| {
                        self.include_private_symbol(symbol);
                    }),
                    self.bundled,
                )
            })
            .unwrap_or_else(|| {
                self.node_builder
                    .type_to_type_node_helper(Some(&*alias_type), &self.context())
                    .unwrap()
            });
        self.add_result(
            &set_synthetic_leading_comments_rc(
                with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                    factory_
                        .create_type_alias_declaration(
                            synthetic_factory_,
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            &*self.get_internal_symbol_name(symbol, symbol_name),
                            type_param_decls,
                            type_node,
                        )
                        .into()
                }),
                Some(match comment_text {
                    None => vec![],
                    Some(comment_text) => vec![Rc::new(SynthesizedComment {
                        kind: SyntaxKind::MultiLineCommentTrivia,
                        text: format!("*\n * {}\n ", comment_text.replace("\n", "\n * ")),
                        /*pos: -1, end: -1*/
                        has_trailing_new_line: Some(true),
                        has_leading_new_line: Default::default(),
                    })],
                }),
            ),
            modifier_flags,
        );
        self.context().set_flags(old_flags);
        self.context().set_enclosing_declaration(old_enclosing_decl);
    }

    pub(super) fn serialize_interface(
        &self,
        symbol: &Symbol,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        let ref interface_type = self
            .type_checker
            .get_declared_type_of_class_or_interface(symbol);
        let local_params = self
            .type_checker
            .get_local_type_parameters_of_class_or_interface_or_type_alias(symbol);
        let type_param_decls = maybe_map(local_params.as_deref(), |p: &Gc<Type>, _| {
            self.node_builder
                .type_parameter_to_declaration_(p, &self.context(), None)
        });
        let base_types = self.type_checker.get_base_types(interface_type);
        let base_type = if !base_types.is_empty() {
            Some(self.type_checker.get_intersection_type(
                &base_types,
                Option::<&Symbol>::None,
                None,
            ))
        } else {
            None
        };
        let members = flat_map(
            Some(&self.type_checker.get_properties_of_type(interface_type)),
            |p: &Gc<Symbol>, _| {
                self.serialize_property_symbol_for_interface(p, base_type.as_deref())
            },
        );
        let call_signatures = self.serialize_signatures(
            SignatureKind::Call,
            interface_type,
            base_type.as_deref(),
            SyntaxKind::CallSignature,
        );
        let construct_signatures = self.serialize_signatures(
            SignatureKind::Construct,
            interface_type,
            base_type.as_deref(),
            SyntaxKind::ConstructSignature,
        );
        let index_signatures =
            self.serialize_index_signatures(interface_type, base_type.as_deref());

        let heritage_clauses: Option<Vec<Gc<Node>>> = if base_types.is_empty() {
            None
        } else {
            Some(vec![with_synthetic_factory_and_factory(
                |synthetic_factory_, factory_| {
                    factory_
                        .create_heritage_clause(
                            synthetic_factory_,
                            SyntaxKind::ExtendsKeyword,
                            map_defined(Some(&base_types), |b: &Gc<Type>, _| {
                                self.try_serialize_as_type_reference(b, SymbolFlags::Value)
                            }),
                        )
                        .into()
                },
            )])
        };
        self.add_result(
            &with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                Gc::<Node>::from(
                    factory_.create_interface_declaration(
                        synthetic_factory_,
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        &*self.get_internal_symbol_name(symbol, symbol_name),
                        type_param_decls,
                        heritage_clauses,
                        Itertools::concat(
                            [
                                index_signatures,
                                construct_signatures,
                                call_signatures,
                                members,
                            ]
                            .into_iter(),
                        ),
                    ),
                )
            }),
            modifier_flags,
        );
    }

    pub(super) fn get_namespace_members_for_serialization(
        &self,
        symbol: &Symbol,
    ) -> Vec<Gc<Symbol>> {
        symbol
            .maybe_exports()
            .as_ref()
            .map_or_default(|symbol_exports| {
                (**symbol_exports)
                    .borrow()
                    .values()
                    .filter(|value| self.is_namespace_member(value))
                    .cloned()
                    .collect::<Vec<_>>()
            })
    }

    pub(super) fn is_type_only_namespace(&self, symbol: &Symbol) -> bool {
        self.get_namespace_members_for_serialization(symbol)
            .iter()
            .all(|m| {
                !self
                    .type_checker
                    .resolve_symbol(Some(&**m), None)
                    .unwrap()
                    .flags()
                    .intersects(SymbolFlags::Value)
            })
    }

    pub(super) fn serialize_module(
        &self,
        symbol: &Symbol,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        let members = self.get_namespace_members_for_serialization(symbol);
        let location_map = array_to_multi_map(
            &members,
            |m: &Gc<Symbol>| -> &'static str {
                if matches!(
                    m.maybe_parent(),
                    Some(m_parent) if ptr::eq(
                        &*m_parent,
                        symbol
                    )
                ) {
                    "real"
                } else {
                    "merged"
                }
            },
            |value: &Gc<Symbol>| value.clone(),
        );
        let real_members = location_map.get(&"real").cloned().unwrap_or_default();
        let merged_members = location_map.get(&"merged").cloned().unwrap_or_default();
        if !real_members.is_empty() {
            let local_name = self.get_internal_symbol_name(symbol, symbol_name);
            self.serialize_as_namespace_declaration(
                &real_members,
                &local_name,
                modifier_flags,
                symbol
                    .flags()
                    .intersects(SymbolFlags::Function | SymbolFlags::Assignment),
            );
        }
        if !merged_members.is_empty() {
            let containing_file =
                maybe_get_source_file_of_node(self.context().maybe_enclosing_declaration());
            let local_name = self.get_internal_symbol_name(symbol, symbol_name);
            let ns_body: Gc<Node> = with_synthetic_factory_and_factory(
                |synthetic_factory_, factory_| {
                    factory_.create_module_block(
                        synthetic_factory_,
                        Some(vec![
                            factory_.create_export_declaration(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Option::<Gc<NodeArray>>::None,
                                false,
                                Some(factory_.create_named_exports(
                                    synthetic_factory_,
                                    map_defined(
                                        Some(&filter(
                                            &merged_members,
                                            |n: &Gc<Symbol>| n.escaped_name() != InternalSymbolName::ExportEquals
                                        )),
                                        |s: &Gc<Symbol>, _| -> Option<Gc<Node>> {
                                            let name = unescape_leading_underscores(s.escaped_name());
                                            let local_name = self.get_internal_symbol_name(s, name);
                                            let alias_decl = s.maybe_declarations().is_some().then_and(|| self.type_checker.get_declaration_of_alias_symbol(s));
                                            if let Some(containing_file) = containing_file.as_ref().filter(|containing_file| {
                                                if let Some(alias_decl) = alias_decl.as_ref() {
                                                    !Gc::ptr_eq(
                                                        *containing_file,
                                                        &get_source_file_of_node(alias_decl)
                                                    )
                                                } else {
                                                    !some(
                                                        s.maybe_declarations().as_deref(),
                                                        Some(|d: &Gc<Node>| Gc::ptr_eq(
                                                            &get_source_file_of_node(d),
                                                            *containing_file
                                                        ))
                                                    )
                                                }
                                            }) {
                                                self.context().tracker().report_nonlocal_augmentation(
                                                    containing_file,
                                                    symbol,
                                                    s
                                                );
                                                return None;
                                            }
                                            let target = alias_decl.as_ref().and_then(|alias_decl| {
                                                self.type_checker.get_target_of_alias_declaration(
                                                    alias_decl,
                                                    Some(true)
                                                )
                                            });
                                            self.include_private_symbol(
                                                target.as_deref().unwrap_or(&**s)
                                            );
                                            let target_name = target.as_ref().map_or_else(|| local_name.clone(), |target| {
                                                self.get_internal_symbol_name(target, unescape_leading_underscores(target.escaped_name()))
                                            });
                                            Some(with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                                                factory_.create_export_specifier(
                                                    synthetic_factory_,
                                                    false,
                                                    if name == target_name {
                                                        None
                                                    } else {
                                                        Some(&*target_name)
                                                    },
                                                    name
                                                ).into()
                                            }))
                                        }
                                    )
                                ).into()),
                                None, None,
                            ).into()
                        ])
                    ).into()
                },
            );
            self.add_result(
                &with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                    Gc::<Node>::from(
                        factory_.create_module_declaration(
                            synthetic_factory_,
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            factory_
                                .create_identifier(
                                    synthetic_factory_,
                                    &local_name,
                                    Option::<Gc<NodeArray>>::None,
                                    None,
                                )
                                .into(),
                            Some(ns_body),
                            Some(NodeFlags::Namespace),
                        ),
                    )
                }),
                ModifierFlags::None,
            );
        }
    }

    pub(super) fn serialize_enum(
        &self,
        symbol: &Symbol,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        self.add_result(
            &with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                Gc::<Node>::from(
                    factory_.create_enum_declaration(
                        synthetic_factory_,
                        Option::<Gc<NodeArray>>::None,
                        Some(factory_.create_modifiers_from_modifier_flags(
                            synthetic_factory_,
                            if self.type_checker.is_const_enum_symbol(symbol) {
                                ModifierFlags::Const
                            } else {
                                ModifierFlags::None
                            },
                        )),
                        &*self.get_internal_symbol_name(symbol, symbol_name),
                        Some(
                            self.type_checker
                                .get_properties_of_type(
                                    &self.type_checker.get_type_of_symbol(symbol),
                                )
                                .iter()
                                .filter(|p| p.flags().intersects(SymbolFlags::EnumMember))
                                .map(|p| -> Gc<Node> {
                                    let initialized_value = p
                                        .maybe_declarations()
                                        .as_ref()
                                        .and_then(|p_declarations| p_declarations.get(0).cloned())
                                        .filter(|p_declarations_0| is_enum_member(p_declarations_0))
                                        .as_ref()
                                        .and_then(|p_declarations_0| {
                                            self.type_checker.get_constant_value_(p_declarations_0)
                                        });
                                    with_synthetic_factory_and_factory(
                                        |synthetic_factory_, factory_| {
                                            factory_
                                                .create_enum_member(
                                                    synthetic_factory_,
                                                    unescape_leading_underscores(p.escaped_name()),
                                                    initialized_value.map(|initialized_value| {
                                                        match initialized_value {
                                                            StringOrNumber::String(
                                                                initialized_value,
                                                            ) => factory_
                                                                .create_string_literal(
                                                                    synthetic_factory_,
                                                                    initialized_value,
                                                                    None,
                                                                    None,
                                                                )
                                                                .into(),
                                                            StringOrNumber::Number(
                                                                initialized_value,
                                                            ) => factory_
                                                                .create_numeric_literal(
                                                                    synthetic_factory_,
                                                                    initialized_value,
                                                                    None,
                                                                )
                                                                .into(),
                                                        }
                                                    }),
                                                )
                                                .into()
                                        },
                                    )
                                })
                                .collect::<Vec<_>>(),
                        ),
                    ),
                )
            }),
            modifier_flags,
        );
    }

    pub(super) fn serialize_as_function_namespace_merge(
        &self,
        type_: &Type,
        symbol: &Symbol,
        local_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        let signatures = self
            .type_checker
            .get_signatures_of_type(type_, SignatureKind::Call);
        for sig in &signatures {
            let decl = self.node_builder.signature_to_signature_declaration_helper(
                sig.clone(),
                SyntaxKind::FunctionDeclaration,
                &self.context(),
                Some(SignatureToSignatureDeclarationOptions {
                    name: Some(with_synthetic_factory_and_factory(
                        |synthetic_factory_, factory_| {
                            factory_
                                .create_identifier(
                                    synthetic_factory_,
                                    local_name,
                                    Option::<Gc<NodeArray>>::None,
                                    None,
                                )
                                .into()
                        },
                    )),
                    private_symbol_visitor: Some(|symbol: &Symbol| {
                        self.include_private_symbol(symbol);
                    }),
                    bundled_imports: self.bundled,
                    modifiers: None,
                    question_token: None,
                }),
            );
            self.add_result(
                &set_text_range_rc_node(
                    decl,
                    self.get_signature_text_range_location(sig).as_deref(),
                ),
                modifier_flags,
            );
        }
        if !(symbol
            .flags()
            .intersects(SymbolFlags::ValueModule | SymbolFlags::NamespaceModule)
            && matches!(
                symbol.maybe_exports().as_ref(),
                Some(symbol_exports) if !(**symbol_exports).borrow().is_empty()
            ))
        {
            let props = filter(
                &self.type_checker.get_properties_of_type(type_),
                |property: &Gc<Symbol>| self.is_namespace_member(property),
            );
            self.serialize_as_namespace_declaration(&props, local_name, modifier_flags, true);
        }
    }

    pub(super) fn get_signature_text_range_location(
        &self,
        signature: &Signature,
    ) -> Option<Gc<Node>> {
        if let Some(signature_declaration) = signature
            .declaration
            .as_ref()
            .filter(|signature_declaration| signature_declaration.maybe_parent().is_some())
        {
            let ref signature_declaration_parent = signature_declaration.parent();
            if is_binary_expression(signature_declaration_parent)
                && get_assignment_declaration_kind(signature_declaration_parent)
                    == AssignmentDeclarationKind::Property
            {
                return Some(signature_declaration_parent.clone());
            }
            if is_variable_declaration(signature_declaration_parent)
                && signature_declaration_parent.maybe_parent().is_some()
            {
                return signature_declaration_parent.maybe_parent();
            }
        }
        signature.declaration.clone()
    }

    pub(super) fn serialize_as_namespace_declaration(
        &self,
        props: &[Gc<Symbol>],
        local_name: &str,
        modifier_flags: ModifierFlags,
        suppress_new_private_context: bool,
    ) {
        if !props.is_empty() {
            let local_vs_remote_map = array_to_multi_map(
                props,
                |p: &Gc<Symbol>| -> &'static str {
                    if length(p.maybe_declarations().as_deref()) == 0
                        || some(
                            p.maybe_declarations().as_deref(),
                            Some(|d: &Gc<Node>| {
                                are_option_gcs_equal(
                                    maybe_get_source_file_of_node(Some(&**d)).as_ref(),
                                    maybe_get_source_file_of_node(
                                        self.context().maybe_enclosing_declaration(),
                                    )
                                    .as_ref(),
                                )
                            }),
                        )
                    {
                        "local"
                    } else {
                        "remote"
                    }
                },
                Clone::clone,
            );
            let local_props = local_vs_remote_map
                .get(&"local")
                .cloned()
                .unwrap_or_default();
            let mut fakespace: Gc<Node> =
                with_parse_base_node_factory_and_factory(|base_factory, parse_node_factory_| {
                    parse_node_factory_
                        .create_module_declaration(
                            base_factory,
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                                factory_
                                    .create_identifier(
                                        synthetic_factory_,
                                        local_name,
                                        Option::<Gc<NodeArray>>::None,
                                        None,
                                    )
                                    .into()
                            }),
                            Some(with_synthetic_factory_and_factory(
                                |synthetic_factory_, factory_| {
                                    factory_
                                        .create_module_block(synthetic_factory_, Some(vec![]))
                                        .into()
                                },
                            )),
                            Some(NodeFlags::Namespace),
                        )
                        .into()
                });
            set_parent(&fakespace, Some(&*self.enclosing_declaration));
            fakespace.set_locals(Some(Gc::new(GcCell::new(create_symbol_table(Some(props))))));
            if let Some(props_0_parent) = props[0].maybe_parent() {
                fakespace.set_symbol(props_0_parent);
            }

            let old_results = self.results().clone();
            self.set_results(vec![]);
            let old_adding_declare = self.adding_declare();
            self.set_adding_declare(false);
            let subcontext = Gc::new((*self.context()).clone());
            subcontext.set_rc_wrapper(Some(subcontext.clone()));
            subcontext.set_enclosing_declaration(Some(fakespace.clone()));
            let old_context = self.context();
            self.set_context(subcontext);
            self.visit_symbol_table(
                Gc::new(GcCell::new(create_symbol_table(Some(&local_props)))),
                Some(suppress_new_private_context),
                Some(true),
            );
            self.set_context(old_context);
            self.set_adding_declare(old_adding_declare);
            let declarations = self.results().clone();
            self.set_results(old_results);
            let default_replaced = map(&declarations, |d: &Gc<Node>, _| {
                if is_export_assignment(d) && {
                    let d_as_export_assignment = d.as_export_assignment();
                    d_as_export_assignment.is_export_equals != Some(true)
                        && is_identifier(&d_as_export_assignment.expression)
                } {
                    with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                        factory_
                            .create_export_declaration(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Option::<Gc<NodeArray>>::None,
                                false,
                                Some(
                                    factory_
                                        .create_named_exports(
                                            synthetic_factory_,
                                            vec![factory_
                                                .create_export_specifier(
                                                    synthetic_factory_,
                                                    false,
                                                    Some(
                                                        d.as_export_assignment().expression.clone(),
                                                    ),
                                                    Gc::<Node>::from(factory_.create_identifier(
                                                        synthetic_factory_,
                                                        InternalSymbolName::Default,
                                                        Option::<Gc<NodeArray>>::None,
                                                        None,
                                                    )),
                                                )
                                                .into()],
                                        )
                                        .into(),
                                ),
                                None,
                                None,
                            )
                            .into()
                    })
                } else {
                    d.clone()
                }
            });
            let export_modifier_stripped = if default_replaced
                .iter()
                .all(|d| has_syntactic_modifier(d, ModifierFlags::Export))
            {
                map(&default_replaced, |node: &Gc<Node>, _| {
                    self.remove_export_modifier(node)
                })
            } else {
                default_replaced
            };
            fakespace = with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_.update_module_declaration(
                    synthetic_factory_,
                    &fakespace,
                    fakespace.maybe_decorators(),
                    fakespace.maybe_modifiers(),
                    fakespace.as_module_declaration().name.clone(),
                    Some(
                        factory_
                            .create_module_block(synthetic_factory_, Some(export_modifier_stripped))
                            .into(),
                    ),
                )
            });
            self.add_result(&fakespace, modifier_flags);
        }
    }

    pub(super) fn is_namespace_member(&self, p: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn serialize_as_class(
        &self,
        symbol: &Symbol,
        local_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        unimplemented!()
    }

    pub(super) fn serialize_as_alias(
        &self,
        symbol: &Symbol,
        local_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        unimplemented!()
    }
}
