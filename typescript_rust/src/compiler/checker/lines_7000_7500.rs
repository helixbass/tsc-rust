use std::{io, ptr, rc::Rc};

use id_arena::Id;
use itertools::Itertools;

use super::{
    SignatureToSignatureDeclarationOptions, SymbolTableToDeclarationStatements,
    TrackExistingEntityNameReturn,
};
use crate::{
    array_to_multi_map, can_have_modifiers, create_symbol_table, debug_fail_if_none, filter,
    find_ancestor, get_assignment_declaration_kind, get_effective_implements_type_nodes,
    get_effective_modifier_flags, get_es_module_interop, get_factory, get_name_of_declaration,
    get_parse_node_factory, get_source_file_of_node, get_symbol_id, get_text_of_jsdoc_comment,
    has_syntactic_modifier, id_text, is_ambient_module, is_binary_expression, is_class_declaration,
    is_class_expression, is_class_like, is_entity_name_expression, is_enum_declaration,
    is_enum_member, is_export_assignment, is_export_declaration, is_export_specifier,
    is_external_module_reference, is_external_or_common_js_module, is_function_declaration,
    is_global_scope_augmentation, is_identifier, is_import_equals_declaration, is_import_specifier,
    is_in_js_file, is_interface_declaration, is_jsdoc_type_alias, is_jsdoc_type_expression,
    is_json_source_file, is_module_declaration, is_named_declaration, is_namespace_export,
    is_parameter_declaration, is_private_identifier, is_property_access_expression,
    is_shorthand_ambient_module_symbol, is_source_file, is_static, is_string_literal_like,
    is_variable_declaration, is_variable_statement, length, map, maybe_get_source_file_of_node,
    return_ok_default_if_none, set_parent, set_synthetic_leading_comments, set_text_range_id_node,
    some, try_flat_map, try_map, try_map_defined, try_maybe_first_defined, try_maybe_map,
    unescape_leading_underscores, AsDoubleDeref, AssignmentDeclarationKind, BoolExt, Debug_,
    HasArena, HasInitializerInterface, HasTypeArgumentsInterface, InArena, InternalSymbolName,
    IteratorExt, MapOrDefault, ModifierFlags, NamedDeclarationInterface, Node, NodeArray,
    NodeBuilderFlags, NodeFlags, NodeInterface, OptionInArena, OptionTry, Signature, SignatureKind,
    StringOrNumber, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, SynthesizedComment, Type,
    TypeInterface,
};

impl SymbolTableToDeclarationStatements {
    pub(super) fn include_private_symbol(&self, symbol: Id<Symbol>) {
        if some(
            symbol.ref_(self).maybe_declarations().as_deref(),
            Some(|&declaration: &Id<Node>| is_parameter_declaration(declaration, self)),
        ) {
            return;
        }
        // Debug_.assertIsDefined(deferredPrivatesStack[deferredPrivatesStack.length - 1]);
        Debug_.assert(!self.deferred_privates_stack().is_empty(), None);
        self.get_unused_name(
            unescape_leading_underscores(symbol.ref_(self).escaped_name()),
            Some(symbol),
        );
        let is_external_import_alias = symbol.ref_(self).flags().intersects(SymbolFlags::Alias)
            && !some(
                symbol.ref_(self).maybe_declarations().as_deref(),
                Some(|&d: &Id<Node>| {
                    find_ancestor(
                        Some(d),
                        |node: Id<Node>| is_export_declaration(&node.ref_(self)),
                        self,
                    )
                    .is_some()
                        || is_namespace_export(&d.ref_(self))
                        || is_import_equals_declaration(&d.ref_(self))
                            && !is_external_module_reference(
                                &d.ref_(self)
                                    .as_import_equals_declaration()
                                    .module_reference
                                    .ref_(self),
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
            .insert(get_symbol_id(&symbol.ref_(self)), symbol);
    }

    pub(super) fn is_exporting_scope(&self, enclosing_declaration: Id<Node>) -> bool {
        is_source_file(&enclosing_declaration.ref_(self))
            && (is_external_or_common_js_module(&enclosing_declaration.ref_(self))
                || is_json_source_file(&enclosing_declaration.ref_(self)))
            || is_ambient_module(enclosing_declaration, self)
                && !is_global_scope_augmentation(&enclosing_declaration.ref_(self))
    }

    pub(super) fn add_result(
        &self,
        mut node: Id<Node>, /*Statement*/
        additional_modifier_flags: ModifierFlags,
    ) {
        if can_have_modifiers(&node.ref_(self)) {
            let mut new_modifier_flags = ModifierFlags::None;
            let enclosing_declaration = self
                .context()
                .ref_(self)
                .maybe_enclosing_declaration()
                .and_then(|context_enclosing_declaration| {
                    if is_jsdoc_type_alias(&context_enclosing_declaration.ref_(self)) {
                        maybe_get_source_file_of_node(Some(context_enclosing_declaration), self)
                    } else {
                        Some(context_enclosing_declaration)
                    }
                });
            if additional_modifier_flags.intersects(ModifierFlags::Export)
                && matches!(
                    enclosing_declaration,
                    Some(enclosing_declaration) if self.is_exporting_scope(enclosing_declaration) ||
                        is_module_declaration(&enclosing_declaration.ref_(self))
                )
                && self.can_have_export_modifier(node)
            {
                new_modifier_flags |= ModifierFlags::Export;
            }
            if self.adding_declare()
                && !new_modifier_flags.intersects(ModifierFlags::Export)
                && match enclosing_declaration.as_ref() {
                    None => true,
                    Some(enclosing_declaration) => !enclosing_declaration
                        .ref_(self)
                        .flags()
                        .intersects(NodeFlags::Ambient),
                }
                && (is_enum_declaration(&node.ref_(self))
                    || is_variable_statement(&node.ref_(self))
                    || is_function_declaration(&node.ref_(self))
                    || is_class_declaration(&node.ref_(self))
                    || is_module_declaration(&node.ref_(self)))
            {
                new_modifier_flags |= ModifierFlags::Ambient;
            }
            if additional_modifier_flags.intersects(ModifierFlags::Default)
                && (is_class_declaration(&node.ref_(self))
                    || is_interface_declaration(&node.ref_(self))
                    || is_function_declaration(&node.ref_(self)))
            {
                new_modifier_flags |= ModifierFlags::Default;
            }
            if new_modifier_flags != ModifierFlags::None {
                node = get_factory(self).update_modifiers(
                    node,
                    new_modifier_flags | get_effective_modifier_flags(node, self),
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
        let alias_type = self
            .type_checker
            .ref_(self)
            .get_declared_type_of_type_alias(symbol)?;
        let type_params = self
            .type_checker
            .ref_(self)
            .get_symbol_links(symbol)
            .ref_(self)
            .type_parameters
            .clone();
        let type_param_decls = try_maybe_map(type_params.as_ref(), |&p: &Id<Type>, _| {
            self.node_builder.ref_(self).type_parameter_to_declaration_(
                p,
                &self.context().ref_(self),
                None,
            )
        })
        .transpose()?;
        let jsdoc_alias_decl =
            symbol
                .ref_(self)
                .maybe_declarations()
                .as_ref()
                .and_then(|symbol_declarations| {
                    symbol_declarations
                        .iter()
                        .find(|declaration: &&Id<Node>| {
                            is_jsdoc_type_alias(&declaration.ref_(self))
                        })
                        .copied()
                });
        let comment = jsdoc_alias_decl.and_then(|jsdoc_alias_decl| {
            jsdoc_alias_decl
                .ref_(self)
                .as_jsdoc_tag()
                .maybe_comment()
                .cloned()
                .or_else(|| {
                    jsdoc_alias_decl
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .as_jsdoc()
                        .comment
                        .clone()
                })
        });
        let comment_text = get_text_of_jsdoc_comment(comment.as_ref(), self);
        let old_flags = self.context().ref_(self).flags();
        self.context()
            .ref_(self)
            .set_flags(self.context().ref_(self).flags() | NodeBuilderFlags::InTypeAlias);
        let old_enclosing_decl = self.context().ref_(self).maybe_enclosing_declaration();
        self.context()
            .ref_(self)
            .set_enclosing_declaration(jsdoc_alias_decl.clone());
        let type_node = jsdoc_alias_decl
            .and_then(|jsdoc_alias_decl| {
                jsdoc_alias_decl
                    .ref_(self)
                    .as_jsdoc_type_like_tag()
                    .maybe_type_expression()
            })
            .filter(|jsdoc_alias_decl_type_expression| {
                is_jsdoc_type_expression(&jsdoc_alias_decl_type_expression.ref_(self))
            })
            .try_and_then(|jsdoc_alias_decl_type_expression| {
                self.node_builder.ref_(self).serialize_existing_type_node(
                    &self.context().ref_(self),
                    jsdoc_alias_decl_type_expression
                        .ref_(self)
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
                    .ref_(self)
                    .type_to_type_node_helper(Some(alias_type), &self.context().ref_(self))?
                    .unwrap())
            })?;
        self.add_result(
            set_synthetic_leading_comments(
                get_factory(self).create_type_alias_declaration(
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
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
                self,
            ),
            modifier_flags,
        );
        self.context().ref_(self).set_flags(old_flags);
        self.context()
            .ref_(self)
            .set_enclosing_declaration(old_enclosing_decl);

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
            .ref_(self)
            .get_declared_type_of_class_or_interface(symbol)?;
        let local_params = self
            .type_checker
            .ref_(self)
            .get_local_type_parameters_of_class_or_interface_or_type_alias(symbol)?;
        let type_param_decls = try_maybe_map(local_params.as_deref(), |&p: &Id<Type>, _| {
            self.node_builder.ref_(self).type_parameter_to_declaration_(
                p,
                &self.context().ref_(self),
                None,
            )
        })
        .transpose()?;
        let base_types = self
            .type_checker
            .ref_(self)
            .get_base_types(interface_type)?;
        let base_type = if !base_types.is_empty() {
            Some(self.type_checker.ref_(self).get_intersection_type(
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
                    .ref_(self)
                    .get_properties_of_type(interface_type)?,
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

        let heritage_clauses: Option<Vec<Id<Node>>> = if base_types.is_empty() {
            None
        } else {
            Some(vec![get_factory(self).create_heritage_clause(
                SyntaxKind::ExtendsKeyword,
                try_map_defined(Some(&base_types), |&b: &Id<Type>, _| {
                    self.try_serialize_as_type_reference(b, SymbolFlags::Value)
                })?,
            )])
        };
        self.add_result(
            get_factory(self).create_interface_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
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
        symbol
            .ref_(self)
            .maybe_exports()
            .map_or_default(|symbol_exports| {
                symbol_exports
                    .ref_(self)
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
                    .ref_(self)
                    .resolve_symbol(Some(m), None)?
                    .unwrap()
                    .ref_(self)
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
                    m.ref_(self).maybe_parent(),
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
                symbol
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::Function | SymbolFlags::Assignment),
            )?;
        }
        if !merged_members.is_empty() {
            let containing_file = maybe_get_source_file_of_node(
                self.context().ref_(self).maybe_enclosing_declaration(),
                self,
            );
            let local_name = self.get_internal_symbol_name(symbol, symbol_name);
            let ns_body = get_factory(self).create_module_block(Some(vec![get_factory(self)
                .create_export_declaration(
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
                    false,
                    Some(get_factory(self).create_named_exports(try_map_defined(
                        Some(&filter(&merged_members, |&n: &Id<Symbol>| {
                            n.ref_(self).escaped_name() != InternalSymbolName::ExportEquals
                        })),
                        |&s: &Id<Symbol>, _| -> io::Result<Option<Id<Node>>> {
                            let s_ref = s.ref_(self);
                            let name = unescape_leading_underscores(s_ref.escaped_name());
                            let local_name = self.get_internal_symbol_name(s, name);
                            let alias_decl = s
                                .ref_(self)
                                .maybe_declarations()
                                .is_some()
                                .try_then_and(|| {
                                    self.type_checker
                                        .ref_(self)
                                        .get_declaration_of_alias_symbol(s)
                                })?;
                            if let Some(containing_file) =
                                containing_file.filter(|&containing_file| {
                                    if let Some(alias_decl) = alias_decl {
                                        containing_file != get_source_file_of_node(alias_decl, self)
                                    } else {
                                        !some(
                                            s.ref_(self).maybe_declarations().as_deref(),
                                            Some(|&d: &Id<Node>| {
                                                get_source_file_of_node(d, self) == containing_file
                                            }),
                                        )
                                    }
                                })
                            {
                                self.context()
                                    .ref_(self)
                                    .tracker_ref()
                                    .report_nonlocal_augmentation(containing_file, symbol, s);
                                return Ok(None);
                            }
                            let target = alias_decl.try_and_then(|alias_decl| {
                                self.type_checker
                                    .ref_(self)
                                    .get_target_of_alias_declaration(alias_decl, Some(true))
                            })?;
                            self.include_private_symbol(target.unwrap_or(s));
                            let target_name = target.map_or_else(
                                || local_name.clone(),
                                |target| {
                                    self.get_internal_symbol_name(
                                        target,
                                        unescape_leading_underscores(
                                            target.ref_(self).escaped_name(),
                                        ),
                                    )
                                },
                            );
                            Ok(Some(get_factory(self).create_export_specifier(
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
                get_factory(self).create_module_declaration(
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
                    get_factory(self).create_identifier(&local_name),
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
            get_factory(self).create_enum_declaration(
                Option::<Id<NodeArray>>::None,
                Some(get_factory(self).create_modifiers_from_modifier_flags(
                    if self.type_checker.ref_(self).is_const_enum_symbol(symbol) {
                        ModifierFlags::Const
                    } else {
                        ModifierFlags::None
                    },
                )),
                &*self.get_internal_symbol_name(symbol, symbol_name),
                Some(
                    self.type_checker
                        .ref_(self)
                        .get_properties_of_type(
                            self.type_checker.ref_(self).get_type_of_symbol(symbol)?,
                        )?
                        .into_iter()
                        .filter(|&p| p.ref_(self).flags().intersects(SymbolFlags::EnumMember))
                        .map(|p| -> io::Result<Id<Node>> {
                            let initialized_value = p
                                .ref_(self)
                                .maybe_declarations()
                                .as_ref()
                                .and_then(|p_declarations| p_declarations.get(0).copied())
                                .filter(|p_declarations_0| {
                                    is_enum_member(&p_declarations_0.ref_(self))
                                })
                                .try_and_then(|p_declarations_0| {
                                    self.type_checker
                                        .ref_(self)
                                        .get_constant_value_(p_declarations_0)
                                })?;
                            Ok(get_factory(self).create_enum_member(
                                unescape_leading_underscores(p.ref_(self).escaped_name()),
                                initialized_value.map(
                                    |initialized_value| match initialized_value {
                                        StringOrNumber::String(initialized_value) => get_factory(
                                            self,
                                        )
                                        .create_string_literal(initialized_value, None, None),
                                        StringOrNumber::Number(initialized_value) => {
                                            get_factory(self)
                                                .create_numeric_literal(initialized_value, None)
                                        }
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
            .ref_(self)
            .get_signatures_of_type(type_, SignatureKind::Call)?;
        for &sig in &signatures {
            let decl = self
                .node_builder
                .ref_(self)
                .signature_to_signature_declaration_helper(
                    sig.clone(),
                    SyntaxKind::FunctionDeclaration,
                    &self.context().ref_(self),
                    Some(SignatureToSignatureDeclarationOptions {
                        name: Some(get_factory(self).create_identifier(local_name)),
                        private_symbol_visitor: Some(|symbol: Id<Symbol>| {
                            self.include_private_symbol(symbol);
                        }),
                        bundled_imports: self.bundled,
                        modifiers: None,
                        question_token: None,
                    }),
                )?;
            self.add_result(
                set_text_range_id_node(
                    decl,
                    self.get_signature_text_range_location(sig)
                        .refed(self)
                        .as_deref(),
                    self,
                ),
                modifier_flags,
            );
        }
        if !(symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::ValueModule | SymbolFlags::NamespaceModule)
            && matches!(
                symbol.ref_(self).maybe_exports().as_ref(),
                Some(symbol_exports) if !symbol_exports.ref_(self).is_empty()
            ))
        {
            let props = self
                .type_checker
                .ref_(self)
                .get_properties_of_type(type_)?
                .into_iter()
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
        signature: Id<Signature>,
    ) -> Option<Id<Node>> {
        if let Some(signature_declaration) =
            signature
                .ref_(self)
                .declaration
                .filter(|signature_declaration| {
                    signature_declaration.ref_(self).maybe_parent().is_some()
                })
        {
            let signature_declaration_parent = signature_declaration.ref_(self).parent();
            if is_binary_expression(&signature_declaration_parent.ref_(self))
                && get_assignment_declaration_kind(signature_declaration_parent, self)
                    == AssignmentDeclarationKind::Property
            {
                return Some(signature_declaration_parent);
            }
            if is_variable_declaration(&signature_declaration_parent.ref_(self))
                && signature_declaration_parent
                    .ref_(self)
                    .maybe_parent()
                    .is_some()
            {
                return signature_declaration_parent.ref_(self).maybe_parent();
            }
        }
        signature.ref_(self).declaration
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
                    if length(p.ref_(self).maybe_declarations().as_deref()) == 0
                        || some(
                            p.ref_(self).maybe_declarations().as_deref(),
                            Some(|&d: &Id<Node>| {
                                maybe_get_source_file_of_node(Some(d), self)
                                    == maybe_get_source_file_of_node(
                                        self.context().ref_(self).maybe_enclosing_declaration(),
                                        self,
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
            let mut fakespace = get_parse_node_factory(self).create_module_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                get_factory(self).create_identifier(local_name),
                Some(get_factory(self).create_module_block(Some(vec![]))),
                Some(NodeFlags::Namespace),
            );
            set_parent(&fakespace.ref_(self), Some(self.enclosing_declaration));
            fakespace
                .ref_(self)
                .set_locals(Some(self.alloc_symbol_table(create_symbol_table(
                    self.type_checker.ref_(self).arena(),
                    Some(props),
                ))));
            if let Some(props_0_parent) = props[0].ref_(self).maybe_parent() {
                fakespace.ref_(self).set_symbol(props_0_parent);
            }

            let old_results = self.results().clone();
            self.set_results(vec![]);
            let old_adding_declare = self.adding_declare();
            self.set_adding_declare(false);
            let subcontext = self.alloc_node_builder_context(self.context().ref_(self).clone());
            subcontext
                .ref_(self)
                .set_enclosing_declaration(Some(fakespace.clone()));
            let old_context = self.context();
            self.set_context(subcontext);
            self.visit_symbol_table(
                self.alloc_symbol_table(create_symbol_table(
                    self.type_checker.ref_(self).arena(),
                    Some(&local_props),
                )),
                Some(suppress_new_private_context),
                Some(true),
            )?;
            self.set_context(old_context);
            self.set_adding_declare(old_adding_declare);
            let declarations = self.results().clone();
            self.set_results(old_results);
            let default_replaced = map(&declarations, |&d: &Id<Node>, _| {
                if is_export_assignment(&d.ref_(self)) && {
                    let d_ref = d.ref_(self);
                    let d_as_export_assignment = d_ref.as_export_assignment();
                    d_as_export_assignment.is_export_equals != Some(true)
                        && is_identifier(&d_as_export_assignment.expression.ref_(self))
                } {
                    get_factory(self).create_export_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        false,
                        Some(get_factory(self).create_named_exports(vec![
                            get_factory(self).create_export_specifier(
                                false,
                                Some(d.ref_(self).as_export_assignment().expression),
                                get_factory(self).create_identifier(InternalSymbolName::Default),
                            ),
                        ])),
                        None,
                        None,
                    )
                } else {
                    d
                }
            });
            let export_modifier_stripped = if default_replaced
                .iter()
                .all(|&d| has_syntactic_modifier(d, ModifierFlags::Export, self))
            {
                map(&default_replaced, |&node: &Id<Node>, _| {
                    self.remove_export_modifier(node)
                })
            } else {
                default_replaced
            };
            fakespace = get_factory(self).update_module_declaration(
                fakespace,
                fakespace.ref_(self).maybe_decorators(),
                fakespace.ref_(self).maybe_modifiers(),
                fakespace.ref_(self).as_module_declaration().name,
                Some(get_factory(self).create_module_block(Some(export_modifier_stripped))),
            );
            self.add_result(fakespace, modifier_flags);
        }

        Ok(())
    }

    pub(super) fn is_namespace_member(&self, p: Id<Symbol>) -> bool {
        p.ref_(self)
            .flags()
            .intersects(SymbolFlags::Type | SymbolFlags::Namespace | SymbolFlags::Alias)
            || !(p.ref_(self).flags().intersects(SymbolFlags::Prototype)
                || p.ref_(self).escaped_name() == "prototype"
                || matches!(
                    p.ref_(self).maybe_value_declaration(),
                    Some(p_value_declaration) if is_static(p_value_declaration, self)
                        && is_class_like(&p_value_declaration.ref_(self).parent().ref_(self))
                ))
    }

    pub(super) fn sanitize_jsdoc_implements(
        &self,
        clauses: &[Id<Node /*ExpressionWithTypeArguments*/>],
    ) -> io::Result<Option<Vec<Id<Node /*ExpressionWithTypeArguments*/>>>> {
        let result = try_map_defined(Some(clauses), |&e: &Id<Node>, _| -> io::Result<_> {
            let e_ref = e.ref_(self);
            let e_as_expression_with_type_arguments = e_ref.as_expression_with_type_arguments();
            let old_enclosing = self.context().ref_(self).maybe_enclosing_declaration();
            self.context().ref_(self).set_enclosing_declaration(Some(e));
            let mut expr = e_as_expression_with_type_arguments.expression;
            if is_entity_name_expression(expr, self) {
                if is_identifier(&expr.ref_(self)) && id_text(&expr.ref_(self)) == "" {
                    return Ok(self.sanitize_jsdoc_implements_cleanup(old_enclosing, None));
                }
                let TrackExistingEntityNameReturn {
                    introduces_error,
                    node,
                } = self.node_builder.ref_(self).track_existing_entity_name(
                    expr,
                    &self.context().ref_(self),
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
                    get_factory(self).create_expression_with_type_arguments(
                        expr,
                        try_maybe_map(
                            e_as_expression_with_type_arguments
                                .maybe_type_arguments()
                                .refed(self)
                                .as_deref(),
                            |&a: &Id<Node>, _| -> io::Result<_> {
                                self.node_builder
                                    .ref_(self)
                                    .serialize_existing_type_node(
                                        &self.context().ref_(self),
                                        a,
                                        Some(&|symbol: Id<Symbol>| {
                                            self.include_private_symbol(symbol);
                                        }),
                                        self.bundled,
                                    )?
                                    .try_unwrap_or_else(|| {
                                        Ok(self
                                            .node_builder
                                            .ref_(self)
                                            .type_to_type_node_helper(
                                                Some(
                                                    self.type_checker
                                                        .ref_(self)
                                                        .get_type_from_type_node_(a)?,
                                                ),
                                                &self.context().ref_(self),
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
        old_enclosing: Option<Id<Node>>,
        result: Option<Id<Node>>,
    ) -> Option<Id<Node>> {
        self.context()
            .ref_(self)
            .set_enclosing_declaration(old_enclosing);
        result
    }

    pub(super) fn serialize_as_class(
        &self,
        symbol: Id<Symbol>,
        local_name: &str,
        modifier_flags: ModifierFlags,
    ) -> io::Result<()> {
        let original_decl =
            symbol
                .ref_(self)
                .maybe_declarations()
                .as_ref()
                .and_then(|symbol_declarations| {
                    symbol_declarations
                        .iter()
                        .find(|declaration| is_class_like(&declaration.ref_(self)))
                        .cloned()
                });
        let old_enclosing = self.context().ref_(self).maybe_enclosing_declaration();
        self.context()
            .ref_(self)
            .set_enclosing_declaration(original_decl.clone().or_else(|| old_enclosing.clone()));
        let local_params = self
            .type_checker
            .ref_(self)
            .get_local_type_parameters_of_class_or_interface_or_type_alias(symbol)?;
        let type_param_decls = try_maybe_map(local_params.as_ref(), |&p: &Id<Type>, _| {
            self.node_builder.ref_(self).type_parameter_to_declaration_(
                p,
                &self.context().ref_(self),
                None,
            )
        })
        .transpose()?;
        let class_type = self
            .type_checker
            .ref_(self)
            .get_declared_type_of_class_or_interface(symbol)?;
        let base_types = self.type_checker.ref_(self).get_base_types(class_type)?;
        let original_implements = original_decl
            .and_then(|original_decl| get_effective_implements_type_nodes(original_decl, self));
        let implements_expressions = original_implements
            .as_ref()
            .try_and_then(|original_implements| {
                self.sanitize_jsdoc_implements(original_implements)
            })?
            .try_unwrap_or_else(|| {
                try_map_defined(
                    Some(
                        &self
                            .type_checker
                            .ref_(self)
                            .get_implements_types(class_type)?,
                    ),
                    |&type_: &Id<Type>, _| self.serialize_implemented_type(type_),
                )
            })?;
        let static_type = self.type_checker.ref_(self).get_type_of_symbol(symbol)?;
        let is_class = matches!(
            static_type.ref_(self).maybe_symbol().and_then(|static_type_symbol| static_type_symbol.ref_(self).maybe_value_declaration()).as_ref(),
            Some(static_type_symbol_value_declaration) if is_class_like(&static_type_symbol_value_declaration.ref_(self))
        );
        let static_base_type = if is_class {
            self.type_checker
                .ref_(self)
                .get_base_constructor_type_of_class(static_type)?
        } else {
            self.type_checker.ref_(self).any_type()
        };
        let heritage_clauses: Vec<Id<Node>> =
            {
                let mut heritage_clauses = vec![];
                if !base_types.is_empty() {
                    heritage_clauses.push(get_factory(self).create_heritage_clause(
                        SyntaxKind::ExtendsKeyword,
                        try_map(&base_types, |&b: &Id<Type>, _| {
                            self.serialize_base_type(b, static_base_type, local_name)
                        })?,
                    ));
                }
                if !implements_expressions.is_empty() {
                    heritage_clauses.push(get_factory(self).create_heritage_clause(
                        SyntaxKind::ImplementsKeyword,
                        implements_expressions,
                    ));
                }
                heritage_clauses
            };
        let symbol_props = self.type_checker.ref_(self).get_non_interhited_properties(
            class_type,
            &base_types,
            self.type_checker
                .ref_(self)
                .get_properties_of_type(class_type)?,
        )?;
        let public_symbol_props = symbol_props.iter().copied().filter(|&s| {
            let value_decl = s.ref_(self).maybe_value_declaration();
            matches!(
                value_decl.as_ref(),
                Some(value_decl) if !(
                    is_named_declaration(&value_decl.ref_(self)) &&
                    is_private_identifier(&value_decl.ref_(self).as_named_declaration().name().ref_(self))
                )
            )
        });
        let has_private_identifier = symbol_props.iter().any(|s| {
            let value_decl = s.ref_(self).maybe_value_declaration();
            matches!(
                value_decl,
                Some(value_decl) if is_named_declaration(&value_decl.ref_(self)) &&
                    is_private_identifier(&value_decl.ref_(self).as_named_declaration().name().ref_(self))
            )
        });
        let private_properties: Vec<Id<Node>> = if has_private_identifier {
            vec![get_factory(self).create_property_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                get_factory(self).create_private_identifier("#private"),
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
                    .ref_(self)
                    .get_properties_of_type(static_type)?,
                |&p: &Id<Symbol>| {
                    !p.ref_(self).flags().intersects(SymbolFlags::Prototype)
                        && p.ref_(self).escaped_name() != "prototype"
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
                symbol.ref_(self).maybe_value_declaration(),
                Some(symbol_value_declaration) if is_in_js_file(Some(&symbol_value_declaration.ref_(self)))
            )
            && !some(
                Some(
                    &self
                        .type_checker
                        .ref_(self)
                        .get_signatures_of_type(static_type, SignatureKind::Construct)?,
                ),
                Option::<fn(&Id<Signature>) -> bool>::None,
            );
        let constructors = if is_non_constructable_class_like_in_js_file {
            vec![get_factory(self).create_constructor_declaration(
                Option::<Id<NodeArray>>::None,
                Some(
                    get_factory(self).create_modifiers_from_modifier_flags(ModifierFlags::Private),
                ),
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
        self.context()
            .ref_(self)
            .set_enclosing_declaration(old_enclosing);
        self.add_result(
            set_text_range_id_node(
                get_factory(self).create_class_declaration(
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
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
                symbol
                    .ref_(self)
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|symbol_declarations| {
                        symbol_declarations
                            .iter()
                            .filter(|d| {
                                is_class_declaration(&d.ref_(self))
                                    || is_class_expression(&d.ref_(self))
                            })
                            .copied()
                            .next()
                    })
                    .refed(self)
                    .as_deref(),
                self,
            ),
            modifier_flags,
        );

        Ok(())
    }

    pub(super) fn get_some_target_name_from_declarations(
        &self,
        declarations: Option<&[Id<Node /*Declaration*/>]>,
    ) -> io::Result<Option<String>> {
        try_maybe_first_defined(declarations, |&d: &Id<Node>, _| {
            if is_import_specifier(&d.ref_(self)) || is_export_specifier(&d.ref_(self)) {
                return Ok(Some(
                    id_text(
                        &d.ref_(self)
                            .as_has_property_name()
                            .maybe_property_name()
                            .unwrap_or_else(|| d.ref_(self).as_named_declaration().name())
                            .ref_(self),
                    )
                    .to_owned(),
                ));
            }
            if is_binary_expression(&d.ref_(self)) || is_export_assignment(&d.ref_(self)) {
                let expression = if is_export_assignment(&d.ref_(self)) {
                    d.ref_(self).as_export_assignment().expression
                } else {
                    d.ref_(self).as_binary_expression().right
                };
                if is_property_access_expression(&expression.ref_(self)) {
                    return Ok(Some(
                        id_text(
                            &expression
                                .ref_(self)
                                .as_property_access_expression()
                                .name
                                .ref_(self),
                        )
                        .to_owned(),
                    ));
                }
            }
            if self
                .type_checker
                .ref_(self)
                .is_alias_symbol_declaration(d)?
            {
                let name = get_name_of_declaration(Some(d), self);
                if let Some(name) = name.as_ref().filter(|name| is_identifier(&name.ref_(self))) {
                    return Ok(Some(id_text(&name.ref_(self)).to_owned()));
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
        let node = debug_fail_if_none!(self
            .type_checker
            .ref_(self)
            .get_declaration_of_alias_symbol(symbol)?);
        let target = return_ok_default_if_none!(self.type_checker.ref_(self).get_merged_symbol(
            self.type_checker
                .ref_(self)
                .get_target_of_alias_declaration(node, Some(true))?,
        ));
        let mut verbatim_target_name = is_shorthand_ambient_module_symbol(target, self)
            .try_then_and(|| {
                self.get_some_target_name_from_declarations(
                    symbol.ref_(self).maybe_declarations().as_deref(),
                )
            })?
            .unwrap_or_else(|| {
                unescape_leading_underscores(target.ref_(self).escaped_name()).to_owned()
            });
        if verbatim_target_name == InternalSymbolName::ExportEquals
            && (get_es_module_interop(&self.type_checker.ref_(self).compiler_options.ref_(self))
                == Some(true)
                || self
                    .type_checker
                    .ref_(self)
                    .compiler_options
                    .ref_(self)
                    .allow_synthetic_default_imports
                    == Some(true))
        {
            verbatim_target_name = InternalSymbolName::Default.to_owned();
        }
        let target_name = self.get_internal_symbol_name(target, &verbatim_target_name);
        self.include_private_symbol(target);
        match node.ref_(self).kind() {
            SyntaxKind::BindingElement => 'case: {
                if matches!(
                    node.ref_(self).maybe_parent().and_then(|node_parent| node_parent.ref_(self).maybe_parent()),
                    Some(node_parent_parent) if node_parent_parent.ref_(self).kind() == SyntaxKind::VariableDeclaration
                ) {
                    let specifier = self
                        .node_builder
                        .ref_(self)
                        .get_specifier_for_module_symbol(
                            target.ref_(self).maybe_parent().unwrap_or(target),
                            &self.context().ref_(self),
                        )?;
                    let property_name = node.ref_(self).as_binding_element().property_name;
                    self.add_result(
                        get_factory(self).create_import_declaration(
                            Option::<Id<NodeArray>>::None,
                            Option::<Id<NodeArray>>::None,
                            Some(
                                get_factory(self).create_import_clause(
                                    false,
                                    None,
                                    Some(
                                        get_factory(self).create_named_imports(vec![get_factory(
                                            self,
                                        )
                                        .create_import_specifier(
                                            false,
                                            property_name
                                                .filter(|property_name| {
                                                    is_identifier(&property_name.ref_(self))
                                                })
                                                .map(|property_name| {
                                                    get_factory(self).create_identifier(id_text(
                                                        &property_name.ref_(self),
                                                    ))
                                                }),
                                            get_factory(self).create_identifier(local_name),
                                        )]),
                                    ),
                                ),
                            ),
                            get_factory(self).create_string_literal(specifier, None, None),
                            None,
                        ),
                        ModifierFlags::None,
                    );
                    break 'case;
                }
                Debug_.fail_bad_syntax_kind(
                    &node
                        .ref_(self)
                        .maybe_parent()
                        .and_then(|node_parent| node_parent.ref_(self).maybe_parent())
                        .unwrap_or(node)
                        .ref_(self),
                    Some(
                        "Unhandled binding element grandparent kind in declaration serialization!",
                    ),
                )
            }
            SyntaxKind::ShorthandPropertyAssignment => {
                if matches!(
                    node.ref_(self).maybe_parent().and_then(|node_parent| node_parent.ref_(self).maybe_parent()),
                    Some(node_parent_parent) if node_parent_parent.ref_(self).kind() == SyntaxKind::BinaryExpression
                ) {
                    self.serialize_export_specifier(
                        unescape_leading_underscores(symbol.ref_(self).escaped_name()),
                        &target_name,
                        None,
                    );
                }
            }
            SyntaxKind::VariableDeclaration => 'case: {
                let node_ref = node.ref_(self);
                let node_as_variable_declaration = node_ref.as_variable_declaration();
                if is_property_access_expression(
                    &node_as_variable_declaration
                        .maybe_initializer()
                        .unwrap()
                        .ref_(self),
                ) {
                    let ref initializer = node_as_variable_declaration.maybe_initializer().unwrap();
                    let unique_name = get_factory(self).create_unique_name(local_name, None);
                    let specifier = self
                        .node_builder
                        .ref_(self)
                        .get_specifier_for_module_symbol(
                            target.ref_(self).maybe_parent().unwrap_or(target),
                            &self.context().ref_(self),
                        )?;
                    self.add_result(
                        get_factory(self).create_import_equals_declaration(
                            Option::<Id<NodeArray>>::None,
                            Option::<Id<NodeArray>>::None,
                            false,
                            unique_name.clone(),
                            get_factory(self).create_external_module_reference(
                                get_factory(self).create_string_literal(specifier, None, None),
                            ),
                        ),
                        ModifierFlags::None,
                    );
                    self.add_result(
                        get_factory(self).create_import_equals_declaration(
                            Option::<Id<NodeArray>>::None,
                            Option::<Id<NodeArray>>::None,
                            false,
                            get_factory(self).create_identifier(local_name),
                            get_factory(self).create_qualified_name(
                                unique_name,
                                initializer.ref_(self).as_property_access_expression().name,
                            ),
                        ),
                        modifier_flags,
                    );
                }

                if target.ref_(self).escaped_name() == InternalSymbolName::ExportEquals
                    && some(
                        target.ref_(self).maybe_declarations().as_deref(),
                        Some(|declaration: &Id<Node>| is_json_source_file(&declaration.ref_(self))),
                    )
                {
                    self.serialize_maybe_alias_assignment(symbol)?;
                    break 'case;
                }
                let is_local_import = !target
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::ValueModule)
                    && !is_variable_declaration(&node.ref_(self));
                self.add_result(
                    get_factory(self).create_import_equals_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        false,
                        get_factory(self).create_identifier(local_name),
                        if is_local_import {
                            self.node_builder.ref_(self).symbol_to_name(
                                target,
                                &self.context().ref_(self),
                                Some(SymbolFlags::All),
                                false,
                            )?
                        } else {
                            get_factory(self).create_external_module_reference(
                                get_factory(self).create_string_literal(
                                    self.node_builder
                                        .ref_(self)
                                        .get_specifier_for_module_symbol(
                                            target,
                                            &self.context().ref_(self),
                                        )?,
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
                if target.ref_(self).escaped_name() == InternalSymbolName::ExportEquals
                    && some(
                        target.ref_(self).maybe_declarations().as_deref(),
                        Some(|declaration: &Id<Node>| is_json_source_file(&declaration.ref_(self))),
                    )
                {
                    self.serialize_maybe_alias_assignment(symbol)?;
                    break 'case;
                }
                let is_local_import = !target
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::ValueModule)
                    && !is_variable_declaration(&node.ref_(self));
                self.add_result(
                    get_factory(self).create_import_equals_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        false,
                        get_factory(self).create_identifier(local_name),
                        if is_local_import {
                            self.node_builder.ref_(self).symbol_to_name(
                                target,
                                &self.context().ref_(self),
                                Some(SymbolFlags::All),
                                false,
                            )?
                        } else {
                            get_factory(self).create_external_module_reference(
                                get_factory(self).create_string_literal(
                                    self.node_builder
                                        .ref_(self)
                                        .get_specifier_for_module_symbol(
                                            target,
                                            &self.context().ref_(self),
                                        )?,
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
                    get_factory(self).create_namespace_export_declaration(id_text(
                        &node
                            .ref_(self)
                            .as_namespace_export_declaration()
                            .name()
                            .ref_(self),
                    )),
                    ModifierFlags::None,
                );
            }
            SyntaxKind::ImportClause => {
                self.add_result(
                    get_factory(self).create_import_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        Some(get_factory(self).create_import_clause(
                            false,
                            Some(get_factory(self).create_identifier(local_name)),
                            None,
                        )),
                        get_factory(self).create_string_literal(
                            self.node_builder
                                .ref_(self)
                                .get_specifier_for_module_symbol(
                                    target.ref_(self).maybe_parent().unwrap_or(target),
                                    &self.context().ref_(self),
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
                    get_factory(self).create_import_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        Some(get_factory(self).create_import_clause(
                            false,
                            None,
                            Some(get_factory(self).create_namespace_import(
                                get_factory(self).create_identifier(local_name),
                            )),
                        )),
                        get_factory(self).create_string_literal(
                            self.node_builder
                                .ref_(self)
                                .get_specifier_for_module_symbol(
                                    target,
                                    &self.context().ref_(self),
                                )?,
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
                    get_factory(self).create_export_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        false,
                        Some(get_factory(self).create_namespace_export(
                            get_factory(self).create_identifier(local_name),
                        )),
                        Some(
                            get_factory(self).create_string_literal(
                                self.node_builder
                                    .ref_(self)
                                    .get_specifier_for_module_symbol(
                                        target,
                                        &self.context().ref_(self),
                                    )?,
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
                    get_factory(self).create_import_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        Some(get_factory(self).create_import_clause(
                            false,
                            None,
                            Some(get_factory(self).create_named_imports(vec![
                                get_factory(self).create_import_specifier(
                                    false,
                                    (local_name != verbatim_target_name).then(|| {
                                        get_factory(self).create_identifier(&verbatim_target_name)
                                    }),
                                    get_factory(self).create_identifier(local_name),
                                ),
                            ])),
                        )),
                        get_factory(self).create_string_literal(
                            self.node_builder
                                .ref_(self)
                                .get_specifier_for_module_symbol(
                                    target.ref_(self).maybe_parent().unwrap_or(target),
                                    &self.context().ref_(self),
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
                    .ref_(self)
                    .parent()
                    .ref_(self)
                    .parent()
                    .ref_(self)
                    .as_export_declaration()
                    .module_specifier;
                self.serialize_export_specifier(
                    unescape_leading_underscores(symbol.ref_(self).escaped_name()),
                    &specifier
                        .is_some()
                        .then_some(verbatim_target_name)
                        .unwrap_or(target_name),
                    specifier
                        .filter(|specifier| is_string_literal_like(&specifier.ref_(self)))
                        .map(|specifier| {
                            get_factory(self).create_string_literal(
                                specifier.ref_(self).as_literal_like_node().text().clone(),
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
                if symbol.ref_(self).escaped_name() == InternalSymbolName::Default
                    || symbol.ref_(self).escaped_name() == InternalSymbolName::ExportEquals
                {
                    self.serialize_maybe_alias_assignment(symbol)?;
                } else {
                    self.serialize_export_specifier(local_name, &target_name, None);
                }
            }
            _ => Debug_.fail_bad_syntax_kind(
                &node.ref_(self),
                Some("Unhandled alias declaration kind in symbol serializer!"),
            ),
        }

        Ok(())
    }
}
