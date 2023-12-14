use std::{io, ptr, rc::Rc};

use gc::{Gc, GcCell};
use id_arena::Id;
use itertools::Itertools;

use super::{
    SignatureToSignatureDeclarationOptions, SymbolTableToDeclarationStatements,
    TrackExistingEntityNameReturn,
};
use crate::{
    are_option_gcs_equal, array_to_multi_map, can_have_modifiers, create_symbol_table,
    debug_fail_if_none, filter, find_ancestor, get_assignment_declaration_kind,
    get_effective_implements_type_nodes, get_effective_modifier_flags, get_es_module_interop,
    get_factory, get_name_of_declaration, get_parse_node_factory, get_source_file_of_node,
    get_symbol_id, get_text_of_jsdoc_comment, has_syntactic_modifier, id_text, is_ambient_module,
    is_binary_expression, is_class_declaration, is_class_expression, is_class_like,
    is_entity_name_expression, is_enum_declaration, is_enum_member, is_export_assignment,
    is_export_declaration, is_export_specifier, is_external_module_reference,
    is_external_or_common_js_module, is_function_declaration, is_global_scope_augmentation,
    is_identifier, is_import_equals_declaration, is_import_specifier, is_in_js_file,
    is_interface_declaration, is_jsdoc_type_alias, is_jsdoc_type_expression, is_json_source_file,
    is_module_declaration, is_named_declaration, is_namespace_export, is_parameter_declaration,
    is_private_identifier, is_property_access_expression, is_shorthand_ambient_module_symbol,
    is_source_file, is_static, is_string_literal_like, is_variable_declaration,
    is_variable_statement, length, map, maybe_get_source_file_of_node, return_ok_default_if_none,
    set_parent, set_synthetic_leading_comments_rc, set_text_range_rc_node, some, try_flat_map,
    try_map, try_map_defined, try_maybe_first_defined, try_maybe_map, unescape_leading_underscores,
    AsDoubleDeref, AssignmentDeclarationKind, BoolExt, Debug_, HasInitializerInterface,
    HasTypeArgumentsInterface, InternalSymbolName, IteratorExt, MapOrDefault, ModifierFlags,
    NamedDeclarationInterface, Node, NodeArray, NodeBuilderFlags, NodeFlags, NodeInterface,
    OptionTry, Signature, SignatureKind, StringOrNumber, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, SynthesizedComment, Type, TypeInterface, HasArena, InArena,
};

impl SymbolTableToDeclarationStatements {
    pub(super) fn include_private_symbol(&self, symbol: Id<Symbol>) {
        if some(
            self.type_checker
                .symbol(symbol)
                .maybe_declarations()
                .as_deref(),
            Some(|declaration: &Gc<Node>| is_parameter_declaration(declaration)),
        ) {
            return;
        }
        // Debug_.assertIsDefined(deferredPrivatesStack[deferredPrivatesStack.length - 1]);
        Debug_.assert(!self.deferred_privates_stack().is_empty(), None);
        self.get_unused_name(
            unescape_leading_underscores(self.type_checker.symbol(symbol).escaped_name()),
            Some(symbol),
        );
        let is_external_import_alias = self
            .type_checker
            .symbol(symbol)
            .flags()
            .intersects(SymbolFlags::Alias)
            && !some(
                self.type_checker
                    .symbol(symbol)
                    .maybe_declarations()
                    .as_deref(),
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
            .insert(get_symbol_id(&self.type_checker.symbol(symbol)), symbol);
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
                node = get_factory().update_modifiers(
                    &node,
                    new_modifier_flags | get_effective_modifier_flags(&node),
                );
            }
        }
        self.results_mut().push(node);
    }

    pub(super) fn serialize_type_alias(
        &self,
        symbol: Id<Symbol>,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) -> io::Result<()> {
        let alias_type = self.type_checker.get_declared_type_of_type_alias(symbol)?;
        let type_params = (*self.type_checker.get_symbol_links(symbol))
            .borrow()
            .type_parameters
            .clone();
        let type_param_decls = try_maybe_map(type_params.as_ref(), |&p: &Id<Type>, _| {
            self.node_builder
                .type_parameter_to_declaration_(p, &self.context(), None)
        })
        .transpose()?;
        let jsdoc_alias_decl = self
            .type_checker
            .symbol(symbol)
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
            .try_and_then(|jsdoc_alias_decl_type_expression| {
                self.node_builder.serialize_existing_type_node(
                    &self.context(),
                    &jsdoc_alias_decl_type_expression
                        .as_jsdoc_type_expression()
                        .type_,
                    Some(&|symbol: Id<Symbol>| {
                        self.include_private_symbol(symbol);
                    }),
                    self.bundled,
                )
            })?
            .try_unwrap_or_else(|| -> io::Result<_> {
                Ok(self
                    .node_builder
                    .type_to_type_node_helper(Some(alias_type), &self.context())?
                    .unwrap())
            })?;
        self.add_result(
            &set_synthetic_leading_comments_rc(
                get_factory().create_type_alias_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    &*self.get_internal_symbol_name(symbol, symbol_name),
                    type_param_decls,
                    type_node,
                ),
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

        Ok(())
    }

    pub(super) fn serialize_interface(
        &self,
        symbol: Id<Symbol>,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) -> io::Result<()> {
        let interface_type = self
            .type_checker
            .get_declared_type_of_class_or_interface(symbol)?;
        let local_params = self
            .type_checker
            .get_local_type_parameters_of_class_or_interface_or_type_alias(symbol)?;
        let type_param_decls = try_maybe_map(local_params.as_deref(), |&p: &Id<Type>, _| {
            self.node_builder
                .type_parameter_to_declaration_(p, &self.context(), None)
        })
        .transpose()?;
        let base_types = self.type_checker.get_base_types(interface_type)?;
        let base_type = if !base_types.is_empty() {
            Some(self.type_checker.get_intersection_type(
                &base_types,
                Option::<Id<Symbol>>::None,
                None,
            )?)
        } else {
            None
        };
        let members = try_flat_map(
            Some(
                &self
                    .type_checker
                    .get_properties_of_type(interface_type)?
                    .collect_vec(),
            ),
            |&p: &Id<Symbol>, _| self.serialize_property_symbol_for_interface(p, base_type),
        )?;
        let call_signatures = self.serialize_signatures(
            SignatureKind::Call,
            interface_type,
            base_type,
            SyntaxKind::CallSignature,
        )?;
        let construct_signatures = self.serialize_signatures(
            SignatureKind::Construct,
            interface_type,
            base_type,
            SyntaxKind::ConstructSignature,
        )?;
        let index_signatures = self.serialize_index_signatures(interface_type, base_type)?;

        let heritage_clauses: Option<Vec<Gc<Node>>> = if base_types.is_empty() {
            None
        } else {
            Some(vec![get_factory().create_heritage_clause(
                SyntaxKind::ExtendsKeyword,
                try_map_defined(Some(&base_types), |&b: &Id<Type>, _| {
                    self.try_serialize_as_type_reference(b, SymbolFlags::Value)
                })?,
            )])
        };
        self.add_result(
            &get_factory().create_interface_declaration(
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
            modifier_flags,
        );

        Ok(())
    }

    pub(super) fn get_namespace_members_for_serialization(
        &self,
        symbol: Id<Symbol>,
    ) -> Vec<Id<Symbol>> {
        self.type_checker
            .symbol(symbol)
            .maybe_exports()
            .as_ref()
            .map_or_default(|symbol_exports| {
                (**symbol_exports)
                    .borrow()
                    .values()
                    .filter(|&&value| self.is_namespace_member(value))
                    .cloned()
                    .collect::<Vec<_>>()
            })
    }

    pub(super) fn is_type_only_namespace(&self, symbol: Id<Symbol>) -> io::Result<bool> {
        self.get_namespace_members_for_serialization(symbol)
            .iter()
            .try_all(|&m| {
                Ok(!self
                    .type_checker
                    .symbol(self.type_checker.resolve_symbol(Some(m), None)?.unwrap())
                    .flags()
                    .intersects(SymbolFlags::Value))
            })
    }

    pub(super) fn serialize_module(
        &self,
        symbol: Id<Symbol>,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) -> io::Result<()> {
        let members = self.get_namespace_members_for_serialization(symbol);
        let location_map = array_to_multi_map(
            &members,
            |&m: &Id<Symbol>| -> &'static str {
                if matches!(
                    self.type_checker.symbol(m).maybe_parent(),
                    Some(m_parent) if m_parent == symbol
                ) {
                    "real"
                } else {
                    "merged"
                }
            },
            |value: &Id<Symbol>| value.clone(),
        );
        let real_members = location_map.get(&"real").cloned().unwrap_or_default();
        let merged_members = location_map.get(&"merged").cloned().unwrap_or_default();
        if !real_members.is_empty() {
            let local_name = self.get_internal_symbol_name(symbol, symbol_name);
            self.serialize_as_namespace_declaration(
                &real_members,
                &local_name,
                modifier_flags,
                self.type_checker
                    .symbol(symbol)
                    .flags()
                    .intersects(SymbolFlags::Function | SymbolFlags::Assignment),
            )?;
        }
        if !merged_members.is_empty() {
            let containing_file =
                maybe_get_source_file_of_node(self.context().maybe_enclosing_declaration());
            let local_name = self.get_internal_symbol_name(symbol, symbol_name);
            let ns_body = get_factory().create_module_block(Some(vec![get_factory()
                .create_export_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    false,
                    Some(get_factory().create_named_exports(try_map_defined(
                        Some(&filter(&merged_members, |&n: &Id<Symbol>| {
                            self.type_checker.symbol(n).escaped_name()
                                != InternalSymbolName::ExportEquals
                        })),
                        |&s: &Id<Symbol>, _| -> io::Result<Option<Gc<Node>>> {
                            let s_ref = self.type_checker.symbol(s);
                            let name = unescape_leading_underscores(s_ref.escaped_name());
                            let local_name = self.get_internal_symbol_name(s, name);
                            let alias_decl = self
                                .type_checker
                                .symbol(s)
                                .maybe_declarations()
                                .is_some()
                                .try_then_and(|| {
                                    self.type_checker.get_declaration_of_alias_symbol(s)
                                })?;
                            if let Some(containing_file) =
                                containing_file.as_ref().filter(|containing_file| {
                                    if let Some(alias_decl) = alias_decl.as_ref() {
                                        !Gc::ptr_eq(
                                            *containing_file,
                                            &get_source_file_of_node(alias_decl),
                                        )
                                    } else {
                                        !some(
                                            self.type_checker
                                                .symbol(s)
                                                .maybe_declarations()
                                                .as_deref(),
                                            Some(|d: &Gc<Node>| {
                                                Gc::ptr_eq(
                                                    &get_source_file_of_node(d),
                                                    *containing_file,
                                                )
                                            }),
                                        )
                                    }
                                })
                            {
                                self.context().tracker().report_nonlocal_augmentation(
                                    containing_file,
                                    symbol,
                                    s,
                                );
                                return Ok(None);
                            }
                            let target = alias_decl.as_ref().try_and_then(|alias_decl| {
                                self.type_checker
                                    .get_target_of_alias_declaration(alias_decl, Some(true))
                            })?;
                            self.include_private_symbol(target.unwrap_or(s));
                            let target_name = target.map_or_else(
                                || local_name.clone(),
                                |target| {
                                    self.get_internal_symbol_name(
                                        target,
                                        unescape_leading_underscores(
                                            self.type_checker.symbol(target).escaped_name(),
                                        ),
                                    )
                                },
                            );
                            Ok(Some(get_factory().create_export_specifier(
                                false,
                                if name == target_name {
                                    None
                                } else {
                                    Some(&*target_name)
                                },
                                name,
                            )))
                        },
                    )?)),
                    None,
                    None,
                )]));
            self.add_result(
                &get_factory().create_module_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    get_factory().create_identifier(&local_name),
                    Some(ns_body),
                    Some(NodeFlags::Namespace),
                ),
                ModifierFlags::None,
            );
        }

        Ok(())
    }

    pub(super) fn serialize_enum(
        &self,
        symbol: Id<Symbol>,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) -> io::Result<()> {
        self.add_result(
            &get_factory().create_enum_declaration(
                Option::<Gc<NodeArray>>::None,
                Some(get_factory().create_modifiers_from_modifier_flags(
                    if self.type_checker.is_const_enum_symbol(symbol) {
                        ModifierFlags::Const
                    } else {
                        ModifierFlags::None
                    },
                )),
                &*self.get_internal_symbol_name(symbol, symbol_name),
                Some(
                    self.type_checker
                        .get_properties_of_type(self.type_checker.get_type_of_symbol(symbol)?)?
                        .filter(|&p| {
                            self.type_checker
                                .symbol(p)
                                .flags()
                                .intersects(SymbolFlags::EnumMember)
                        })
                        .map(|p| -> io::Result<Gc<Node>> {
                            let initialized_value = self
                                .type_checker
                                .symbol(p)
                                .maybe_declarations()
                                .as_ref()
                                .and_then(|p_declarations| p_declarations.get(0).cloned())
                                .filter(|p_declarations_0| is_enum_member(p_declarations_0))
                                .as_ref()
                                .try_and_then(|p_declarations_0| {
                                    self.type_checker.get_constant_value_(p_declarations_0)
                                })?;
                            Ok(get_factory().create_enum_member(
                                unescape_leading_underscores(
                                    self.type_checker.symbol(p).escaped_name(),
                                ),
                                initialized_value.map(
                                    |initialized_value| match initialized_value {
                                        StringOrNumber::String(initialized_value) => get_factory()
                                            .create_string_literal(initialized_value, None, None),
                                        StringOrNumber::Number(initialized_value) => get_factory()
                                            .create_numeric_literal(initialized_value, None),
                                    },
                                ),
                            ))
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                ),
            ),
            modifier_flags,
        );

        Ok(())
    }

    pub(super) fn serialize_as_function_namespace_merge(
        &self,
        type_: Id<Type>,
        symbol: Id<Symbol>,
        local_name: &str,
        modifier_flags: ModifierFlags,
    ) -> io::Result<()> {
        let signatures = self
            .type_checker
            .get_signatures_of_type(type_, SignatureKind::Call)?;
        for sig in &signatures {
            let decl = self
                .node_builder
                .signature_to_signature_declaration_helper(
                    sig.clone(),
                    SyntaxKind::FunctionDeclaration,
                    &self.context(),
                    Some(SignatureToSignatureDeclarationOptions {
                        name: Some(get_factory().create_identifier(local_name)),
                        private_symbol_visitor: Some(|symbol: Id<Symbol>| {
                            self.include_private_symbol(symbol);
                        }),
                        bundled_imports: self.bundled,
                        modifiers: None,
                        question_token: None,
                    }),
                )?;
            self.add_result(
                &set_text_range_rc_node(
                    decl,
                    self.get_signature_text_range_location(sig).as_deref(),
                ),
                modifier_flags,
            );
        }
        if !(self
            .type_checker
            .symbol(symbol)
            .flags()
            .intersects(SymbolFlags::ValueModule | SymbolFlags::NamespaceModule)
            && matches!(
                self.type_checker.symbol(symbol).maybe_exports().as_ref(),
                Some(symbol_exports) if !(**symbol_exports).borrow().is_empty()
            ))
        {
            let props = self
                .type_checker
                .get_properties_of_type(type_)?
                .filter(|&property| self.is_namespace_member(property));
            self.serialize_as_namespace_declaration(
                &props.collect_vec(),
                local_name,
                modifier_flags,
                true,
            )?;
        }

        Ok(())
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
        props: &[Id<Symbol>],
        local_name: &str,
        modifier_flags: ModifierFlags,
        suppress_new_private_context: bool,
    ) -> io::Result<()> {
        if !props.is_empty() {
            let local_vs_remote_map = array_to_multi_map(
                props,
                |&p: &Id<Symbol>| -> &'static str {
                    if length(self.type_checker.symbol(p).maybe_declarations().as_deref()) == 0
                        || some(
                            self.type_checker.symbol(p).maybe_declarations().as_deref(),
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
            let mut fakespace = get_parse_node_factory().create_module_declaration(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                get_factory().create_identifier(local_name),
                Some(get_factory().create_module_block(Some(vec![]))),
                Some(NodeFlags::Namespace),
            );
            set_parent(&fakespace, Some(&*self.enclosing_declaration));
            fakespace.set_locals(Some(Gc::new(GcCell::new(create_symbol_table(
                self.type_checker.arena(),
                Some(props),
            )))));
            if let Some(props_0_parent) = self.type_checker.symbol(props[0]).maybe_parent() {
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
                Gc::new(GcCell::new(create_symbol_table(
                    self.type_checker.arena(),
                    Some(&local_props),
                ))),
                Some(suppress_new_private_context),
                Some(true),
            )?;
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
                    get_factory().create_export_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        false,
                        Some(get_factory().create_named_exports(vec![
                            get_factory().create_export_specifier(
                                false,
                                Some(d.as_export_assignment().expression.clone()),
                                get_factory().create_identifier(InternalSymbolName::Default),
                            ),
                        ])),
                        None,
                        None,
                    )
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
            fakespace = get_factory().update_module_declaration(
                &fakespace,
                fakespace.maybe_decorators(),
                fakespace.maybe_modifiers(),
                fakespace.as_module_declaration().name.clone(),
                Some(get_factory().create_module_block(Some(export_modifier_stripped))),
            );
            self.add_result(&fakespace, modifier_flags);
        }

        Ok(())
    }

    pub(super) fn is_namespace_member(&self, p: Id<Symbol>) -> bool {
        self.type_checker
            .symbol(p)
            .flags()
            .intersects(SymbolFlags::Type | SymbolFlags::Namespace | SymbolFlags::Alias)
            || !(self
                .type_checker
                .symbol(p)
                .flags()
                .intersects(SymbolFlags::Prototype)
                || self.type_checker.symbol(p).escaped_name() == "prototype"
                || matches!(
                    self.type_checker.symbol(p).maybe_value_declaration().as_ref(),
                    Some(p_value_declaration) if is_static(p_value_declaration) && is_class_like(&p_value_declaration.parent())
                ))
    }

    pub(super) fn sanitize_jsdoc_implements(
        &self,
        clauses: &[Gc<Node /*ExpressionWithTypeArguments*/>],
    ) -> io::Result<Option<Vec<Gc<Node /*ExpressionWithTypeArguments*/>>>> {
        let result = try_map_defined(Some(clauses), |e: &Gc<Node>, _| -> io::Result<_> {
            let e_as_expression_with_type_arguments = e.as_expression_with_type_arguments();
            let old_enclosing = self.context().maybe_enclosing_declaration();
            self.context().set_enclosing_declaration(Some(e.clone()));
            let mut expr = e_as_expression_with_type_arguments.expression.clone();
            if is_entity_name_expression(&expr) {
                if is_identifier(&expr) && id_text(&expr) == "" {
                    return Ok(self.sanitize_jsdoc_implements_cleanup(old_enclosing, None));
                }
                let TrackExistingEntityNameReturn {
                    introduces_error,
                    node,
                } = self.node_builder.track_existing_entity_name(
                    &expr,
                    &self.context(),
                    Some(&|symbol: Id<Symbol>| {
                        self.include_private_symbol(symbol);
                    }),
                )?;
                expr = node;
                if introduces_error {
                    return Ok(self.sanitize_jsdoc_implements_cleanup(old_enclosing, None));
                }
            }
            Ok(self.sanitize_jsdoc_implements_cleanup(
                old_enclosing,
                Some(
                    get_factory().create_expression_with_type_arguments(
                        expr,
                        try_maybe_map(
                            e_as_expression_with_type_arguments
                                .maybe_type_arguments()
                                .as_deref(),
                            |a: &Gc<Node>, _| -> io::Result<_> {
                                self.node_builder
                                    .serialize_existing_type_node(
                                        &self.context(),
                                        a,
                                        Some(&|symbol: Id<Symbol>| {
                                            self.include_private_symbol(symbol);
                                        }),
                                        self.bundled,
                                    )?
                                    .try_unwrap_or_else(|| {
                                        Ok(self
                                            .node_builder
                                            .type_to_type_node_helper(
                                                Some(
                                                    self.type_checker
                                                        .get_type_from_type_node_(a)?,
                                                ),
                                                &self.context(),
                                            )?
                                            .unwrap())
                                    })
                            },
                        )
                        .transpose()?,
                    ),
                ),
            ))
        })?;
        if result.len() == clauses.len() {
            return Ok(Some(result));
        }
        Ok(None)
    }

    pub(super) fn sanitize_jsdoc_implements_cleanup(
        &self,
        old_enclosing: Option<Gc<Node>>,
        result: Option<Gc<Node>>,
    ) -> Option<Gc<Node>> {
        self.context().set_enclosing_declaration(old_enclosing);
        result
    }

    pub(super) fn serialize_as_class(
        &self,
        symbol: Id<Symbol>,
        local_name: &str,
        modifier_flags: ModifierFlags,
    ) -> io::Result<()> {
        let original_decl = self
            .type_checker
            .symbol(symbol)
            .maybe_declarations()
            .as_ref()
            .and_then(|symbol_declarations| {
                symbol_declarations
                    .iter()
                    .find(|declaration| is_class_like(declaration))
                    .cloned()
            });
        let old_enclosing = self.context().maybe_enclosing_declaration();
        self.context()
            .set_enclosing_declaration(original_decl.clone().or_else(|| old_enclosing.clone()));
        let local_params = self
            .type_checker
            .get_local_type_parameters_of_class_or_interface_or_type_alias(symbol)?;
        let type_param_decls = try_maybe_map(local_params.as_ref(), |&p: &Id<Type>, _| {
            self.node_builder
                .type_parameter_to_declaration_(p, &self.context(), None)
        })
        .transpose()?;
        let class_type = self
            .type_checker
            .get_declared_type_of_class_or_interface(symbol)?;
        let base_types = self.type_checker.get_base_types(class_type)?;
        let original_implements = original_decl
            .as_ref()
            .and_then(|original_decl| get_effective_implements_type_nodes(original_decl));
        let implements_expressions = original_implements
            .as_ref()
            .try_and_then(|original_implements| {
                self.sanitize_jsdoc_implements(original_implements)
            })?
            .try_unwrap_or_else(|| {
                try_map_defined(
                    Some(&self.type_checker.get_implements_types(class_type)?),
                    |&type_: &Id<Type>, _| self.serialize_implemented_type(type_),
                )
            })?;
        let static_type = self.type_checker.get_type_of_symbol(symbol)?;
        let is_class = matches!(
            static_type.ref_(self).maybe_symbol().and_then(|static_type_symbol| self.type_checker.symbol(static_type_symbol).maybe_value_declaration()).as_ref(),
            Some(static_type_symbol_value_declaration) if is_class_like(static_type_symbol_value_declaration)
        );
        let static_base_type = if is_class {
            self.type_checker
                .get_base_constructor_type_of_class(static_type)?
        } else {
            self.type_checker.any_type()
        };
        let heritage_clauses: Vec<Gc<Node>> =
            {
                let mut heritage_clauses = vec![];
                if !base_types.is_empty() {
                    heritage_clauses.push(get_factory().create_heritage_clause(
                        SyntaxKind::ExtendsKeyword,
                        try_map(&base_types, |&b: &Id<Type>, _| {
                            self.serialize_base_type(b, static_base_type, local_name)
                        })?,
                    ));
                }
                if !implements_expressions.is_empty() {
                    heritage_clauses.push(get_factory().create_heritage_clause(
                        SyntaxKind::ImplementsKeyword,
                        implements_expressions,
                    ));
                }
                heritage_clauses
            };
        let mut symbol_props = self.type_checker.get_non_interhited_properties(
            class_type,
            &base_types,
            self.type_checker.get_properties_of_type(class_type)?,
        )?;
        let public_symbol_props = symbol_props.clone().filter(|&s| {
            let value_decl = self.type_checker.symbol(s).maybe_value_declaration();
            matches!(
                value_decl.as_ref(),
                Some(value_decl) if !(
                    is_named_declaration(value_decl) &&
                    is_private_identifier(&value_decl.as_named_declaration().name())
                )
            )
        });
        let has_private_identifier = symbol_props.any(|s| {
            let value_decl = self.type_checker.symbol(s).maybe_value_declaration();
            matches!(
                value_decl.as_ref(),
                Some(value_decl) if is_named_declaration(value_decl) &&
                    is_private_identifier(&value_decl.as_named_declaration().name())
            )
        });
        let private_properties: Vec<Gc<Node>> = if has_private_identifier {
            vec![get_factory().create_property_declaration(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                get_factory().create_private_identifier("#private"),
                None,
                None,
                None,
            )]
        } else {
            vec![]
        };
        let public_properties = try_flat_map(
            Some(&public_symbol_props.collect_vec()),
            |&p: &Id<Symbol>, _| {
                self.serialize_property_symbol_for_class().call(
                    p,
                    false,
                    base_types.get(0).copied(),
                )
            },
        )?;
        let static_members = try_flat_map(
            Some(&filter(
                &self
                    .type_checker
                    .get_properties_of_type(static_type)?
                    .collect_vec(),
                |&p: &Id<Symbol>| {
                    !self
                        .type_checker
                        .symbol(p)
                        .flags()
                        .intersects(SymbolFlags::Prototype)
                        && self.type_checker.symbol(p).escaped_name() != "prototype"
                        && !self.is_namespace_member(p)
                },
            )),
            |&p: &Id<Symbol>, _| {
                self.serialize_property_symbol_for_class()
                    .call(p, true, Some(static_base_type))
            },
        )?;
        let is_non_constructable_class_like_in_js_file = !is_class
            && matches!(
                self.type_checker.symbol(symbol).maybe_value_declaration().as_ref(),
                Some(symbol_value_declaration) if is_in_js_file(Some(&**symbol_value_declaration))
            )
            && !some(
                Some(
                    &self
                        .type_checker
                        .get_signatures_of_type(static_type, SignatureKind::Construct)?,
                ),
                Option::<fn(&Gc<Signature>) -> bool>::None,
            );
        let constructors = if is_non_constructable_class_like_in_js_file {
            vec![get_factory().create_constructor_declaration(
                Option::<Gc<NodeArray>>::None,
                Some(get_factory().create_modifiers_from_modifier_flags(ModifierFlags::Private)),
                Some(vec![]),
                None,
            )]
        } else {
            self.serialize_signatures(
                SignatureKind::Construct,
                static_type,
                Some(static_base_type),
                SyntaxKind::Constructor,
            )?
        };
        let index_signatures =
            self.serialize_index_signatures(class_type, base_types.get(0).copied())?;
        self.context().set_enclosing_declaration(old_enclosing);
        self.add_result(
            &set_text_range_rc_node(
                get_factory().create_class_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    Some(local_name),
                    type_param_decls,
                    Some(heritage_clauses),
                    Itertools::concat(
                        [
                            index_signatures,
                            static_members,
                            constructors,
                            public_properties,
                            private_properties,
                        ]
                        .into_iter(),
                    ),
                ),
                self.type_checker
                    .symbol(symbol)
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|symbol_declarations| {
                        symbol_declarations
                            .iter()
                            .filter(|d| is_class_declaration(d) || is_class_expression(d))
                            .cloned()
                            .next()
                    })
                    .as_deref(),
            ),
            modifier_flags,
        );

        Ok(())
    }

    pub(super) fn get_some_target_name_from_declarations(
        &self,
        declarations: Option<&[Gc<Node /*Declaration*/>]>,
    ) -> io::Result<Option<String>> {
        try_maybe_first_defined(declarations, |d: &Gc<Node>, _| {
            if is_import_specifier(d) || is_export_specifier(d) {
                return Ok(Some(
                    id_text(
                        &d.as_has_property_name()
                            .maybe_property_name()
                            .unwrap_or_else(|| d.as_named_declaration().name()),
                    )
                    .to_owned(),
                ));
            }
            if is_binary_expression(d) || is_export_assignment(d) {
                let expression = if is_export_assignment(d) {
                    &d.as_export_assignment().expression
                } else {
                    &d.as_binary_expression().right
                };
                if is_property_access_expression(expression) {
                    return Ok(Some(
                        id_text(&expression.as_property_access_expression().name).to_owned(),
                    ));
                }
            }
            if self.type_checker.is_alias_symbol_declaration(d)? {
                let name = get_name_of_declaration(Some(&**d));
                if let Some(name) = name.as_ref().filter(|name| is_identifier(name)) {
                    return Ok(Some(id_text(name).to_owned()));
                }
            }
            Ok(None)
        })
    }

    pub(super) fn serialize_as_alias(
        &self,
        symbol: Id<Symbol>,
        local_name: &str,
        modifier_flags: ModifierFlags,
    ) -> io::Result<()> {
        let ref node =
            debug_fail_if_none!(self.type_checker.get_declaration_of_alias_symbol(symbol)?);
        let target = return_ok_default_if_none!(self.type_checker.get_merged_symbol(
            self.type_checker
                .get_target_of_alias_declaration(node, Some(true))?,
        ));
        let mut verbatim_target_name =
            is_shorthand_ambient_module_symbol(&self.type_checker.symbol(target))
                .try_then_and(|| {
                    self.get_some_target_name_from_declarations(
                        self.type_checker
                            .symbol(symbol)
                            .maybe_declarations()
                            .as_deref(),
                    )
                })?
                .unwrap_or_else(|| {
                    unescape_leading_underscores(self.type_checker.symbol(target).escaped_name())
                        .to_owned()
                });
        if verbatim_target_name == InternalSymbolName::ExportEquals
            && (get_es_module_interop(&self.type_checker.compiler_options) == Some(true)
                || self
                    .type_checker
                    .compiler_options
                    .allow_synthetic_default_imports
                    == Some(true))
        {
            verbatim_target_name = InternalSymbolName::Default.to_owned();
        }
        let target_name = self.get_internal_symbol_name(target, &verbatim_target_name);
        self.include_private_symbol(target);
        match node.kind() {
            SyntaxKind::BindingElement => 'case: {
                if matches!(
                    node.maybe_parent().and_then(|node_parent| node_parent.maybe_parent()).as_ref(),
                    Some(node_parent_parent) if node_parent_parent.kind() == SyntaxKind::VariableDeclaration
                ) {
                    let specifier = self.node_builder.get_specifier_for_module_symbol(
                        self.type_checker
                            .symbol(target)
                            .maybe_parent()
                            .unwrap_or(target),
                        &self.context(),
                    )?;
                    let property_name = node.as_binding_element().property_name.as_ref();
                    self.add_result(
                        &get_factory().create_import_declaration(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            Some(get_factory().create_import_clause(
                                false,
                                None,
                                Some(get_factory().create_named_imports(vec![get_factory()
                                                        .create_import_specifier(
                                                            false,
                                                            property_name
                                                                .filter(|property_name| {
                                                                    is_identifier(property_name)
                                                                })
                                                                .map(|property_name| {
                                                                    get_factory().create_identifier(
                                                                        id_text(property_name),
                                                                    )
                                                                }),
                                                            get_factory()
                                                                .create_identifier(local_name),
                                                        )
                                                        ])),
                            )),
                            get_factory().create_string_literal(specifier, None, None),
                            None,
                        ),
                        ModifierFlags::None,
                    );
                    break 'case;
                }
                Debug_.fail_bad_syntax_kind(
                    node.maybe_parent()
                        .and_then(|node_parent| node_parent.maybe_parent())
                        .as_ref()
                        .unwrap_or(node),
                    Some(
                        "Unhandled binding element grandparent kind in declaration serialization!",
                    ),
                )
            }
            SyntaxKind::ShorthandPropertyAssignment => {
                if matches!(
                    node.maybe_parent().and_then(|node_parent| node_parent.maybe_parent()).as_ref(),
                    Some(node_parent_parent) if node_parent_parent.kind() == SyntaxKind::BinaryExpression
                ) {
                    self.serialize_export_specifier(
                        unescape_leading_underscores(
                            self.type_checker.symbol(symbol).escaped_name(),
                        ),
                        &target_name,
                        Option::<&Node>::None,
                    );
                }
            }
            SyntaxKind::VariableDeclaration => 'case: {
                let node_as_variable_declaration = node.as_variable_declaration();
                if is_property_access_expression(
                    &node_as_variable_declaration.maybe_initializer().unwrap(),
                ) {
                    let ref initializer = node_as_variable_declaration.maybe_initializer().unwrap();
                    let unique_name = get_factory().create_unique_name(local_name, None);
                    let specifier = self.node_builder.get_specifier_for_module_symbol(
                        self.type_checker
                            .symbol(target)
                            .maybe_parent()
                            .unwrap_or(target),
                        &self.context(),
                    )?;
                    self.add_result(
                        &get_factory().create_import_equals_declaration(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            false,
                            unique_name.clone(),
                            get_factory().create_external_module_reference(
                                get_factory().create_string_literal(specifier, None, None),
                            ),
                        ),
                        ModifierFlags::None,
                    );
                    self.add_result(
                        &get_factory().create_import_equals_declaration(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            false,
                            get_factory().create_identifier(local_name),
                            get_factory().create_qualified_name(
                                unique_name,
                                initializer.as_property_access_expression().name.clone(),
                            ),
                        ),
                        modifier_flags,
                    );
                }

                if self.type_checker.symbol(target).escaped_name()
                    == InternalSymbolName::ExportEquals
                    && some(
                        self.type_checker
                            .symbol(target)
                            .maybe_declarations()
                            .as_deref(),
                        Some(|declaration: &Gc<Node>| is_json_source_file(declaration)),
                    )
                {
                    self.serialize_maybe_alias_assignment(symbol)?;
                    break 'case;
                }
                let is_local_import = !self
                    .type_checker
                    .symbol(target)
                    .flags()
                    .intersects(SymbolFlags::ValueModule)
                    && !is_variable_declaration(node);
                self.add_result(
                    &get_factory().create_import_equals_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        false,
                        get_factory().create_identifier(local_name),
                        if is_local_import {
                            self.node_builder.symbol_to_name(
                                target,
                                &self.context(),
                                Some(SymbolFlags::All),
                                false,
                            )?
                        } else {
                            get_factory().create_external_module_reference(
                                get_factory().create_string_literal(
                                    self.node_builder
                                        .get_specifier_for_module_symbol(target, &self.context())?,
                                    None,
                                    None,
                                ),
                            )
                        },
                    ),
                    if is_local_import {
                        modifier_flags
                    } else {
                        ModifierFlags::None
                    },
                );
            }
            SyntaxKind::ImportEqualsDeclaration => 'case: {
                if self.type_checker.symbol(target).escaped_name()
                    == InternalSymbolName::ExportEquals
                    && some(
                        self.type_checker
                            .symbol(target)
                            .maybe_declarations()
                            .as_deref(),
                        Some(|declaration: &Gc<Node>| is_json_source_file(declaration)),
                    )
                {
                    self.serialize_maybe_alias_assignment(symbol)?;
                    break 'case;
                }
                let is_local_import = !self
                    .type_checker
                    .symbol(target)
                    .flags()
                    .intersects(SymbolFlags::ValueModule)
                    && !is_variable_declaration(node);
                self.add_result(
                    &get_factory().create_import_equals_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        false,
                        get_factory().create_identifier(local_name),
                        if is_local_import {
                            self.node_builder.symbol_to_name(
                                target,
                                &self.context(),
                                Some(SymbolFlags::All),
                                false,
                            )?
                        } else {
                            get_factory().create_external_module_reference(
                                get_factory().create_string_literal(
                                    self.node_builder
                                        .get_specifier_for_module_symbol(target, &self.context())?,
                                    None,
                                    None,
                                ),
                            )
                        },
                    ),
                    if is_local_import {
                        modifier_flags
                    } else {
                        ModifierFlags::None
                    },
                );
            }
            SyntaxKind::NamespaceExportDeclaration => {
                self.add_result(
                    &get_factory().create_namespace_export_declaration(id_text(
                        &node.as_namespace_export_declaration().name(),
                    )),
                    ModifierFlags::None,
                );
            }
            SyntaxKind::ImportClause => {
                self.add_result(
                    &get_factory().create_import_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        Some(get_factory().create_import_clause(
                            false,
                            Some(get_factory().create_identifier(local_name)),
                            None,
                        )),
                        get_factory().create_string_literal(
                            self.node_builder.get_specifier_for_module_symbol(
                                self.type_checker
                                    .symbol(target)
                                    .maybe_parent()
                                    .unwrap_or(target),
                                &self.context(),
                            )?,
                            None,
                            None,
                        ),
                        None,
                    ),
                    ModifierFlags::None,
                );
            }
            SyntaxKind::NamespaceImport => {
                self.add_result(
                    &get_factory().create_import_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        Some(get_factory().create_import_clause(
                            false,
                            None,
                            Some(get_factory().create_namespace_import(
                                get_factory().create_identifier(local_name),
                            )),
                        )),
                        get_factory().create_string_literal(
                            self.node_builder
                                .get_specifier_for_module_symbol(target, &self.context())?,
                            None,
                            None,
                        ),
                        None,
                    ),
                    ModifierFlags::None,
                );
            }
            SyntaxKind::NamespaceExport => {
                self.add_result(
                    &get_factory().create_export_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        false,
                        Some(
                            get_factory().create_namespace_export(
                                get_factory().create_identifier(local_name),
                            ),
                        ),
                        Some(
                            get_factory().create_string_literal(
                                self.node_builder
                                    .get_specifier_for_module_symbol(target, &self.context())?,
                                None,
                                None,
                            ),
                        ),
                        None,
                    ),
                    ModifierFlags::None,
                );
            }
            SyntaxKind::ImportSpecifier => {
                self.add_result(
                    &get_factory().create_import_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        Some(get_factory().create_import_clause(
                            false,
                            None,
                            Some(get_factory().create_named_imports(vec![
                                get_factory().create_import_specifier(
                                    false,
                                    (local_name != verbatim_target_name).then(|| {
                                        get_factory().create_identifier(&verbatim_target_name)
                                    }),
                                    get_factory().create_identifier(local_name),
                                ),
                            ])),
                        )),
                        get_factory().create_string_literal(
                            self.node_builder.get_specifier_for_module_symbol(
                                self.type_checker
                                    .symbol(target)
                                    .maybe_parent()
                                    .unwrap_or(target),
                                &self.context(),
                            )?,
                            None,
                            None,
                        ),
                        None,
                    ),
                    ModifierFlags::None,
                );
            }
            SyntaxKind::ExportSpecifier => {
                let specifier = node
                    .parent()
                    .parent()
                    .as_export_declaration()
                    .module_specifier
                    .clone();
                self.serialize_export_specifier(
                    unescape_leading_underscores(self.type_checker.symbol(symbol).escaped_name()),
                    &specifier
                        .is_some()
                        .then_some(verbatim_target_name)
                        .unwrap_or(target_name),
                    specifier
                        .as_ref()
                        .filter(|specifier| is_string_literal_like(specifier))
                        .map(|specifier| {
                            get_factory().create_string_literal(
                                specifier.as_literal_like_node().text().clone(),
                                None,
                                None,
                            )
                        }),
                );
            }
            SyntaxKind::ExportAssignment => {
                self.serialize_maybe_alias_assignment(symbol)?;
            }
            SyntaxKind::BinaryExpression
            | SyntaxKind::PropertyAccessExpression
            | SyntaxKind::ElementAccessExpression => {
                if self.type_checker.symbol(symbol).escaped_name() == InternalSymbolName::Default
                    || self.type_checker.symbol(symbol).escaped_name()
                        == InternalSymbolName::ExportEquals
                {
                    self.serialize_maybe_alias_assignment(symbol)?;
                } else {
                    self.serialize_export_specifier(
                        local_name,
                        &target_name,
                        Option::<&Node>::None,
                    );
                }
            }
            _ => Debug_.fail_bad_syntax_kind(
                node,
                Some("Unhandled alias declaration kind in symbol serializer!"),
            ),
        }

        Ok(())
    }
}
