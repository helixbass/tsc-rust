use std::borrow::Borrow;

use gc::{Finalize, Gc, Trace};

use crate::{
    are_option_gcs_equal, get_declaration_modifier_flags_from_symbol,
    get_export_assignment_expression, get_object_flags,
    get_property_assignment_alias_like_expression, get_selected_effective_modifier_flags,
    get_source_file_of_node, id_text, is_accessor, is_binary_expression, is_class_expression,
    is_entity_name_expression, is_export_assignment, is_function_like_declaration, is_get_accessor,
    is_identifier_text, is_property_access_expression, is_property_declaration,
    is_property_signature, is_prototype_property_assignment, is_set_accessor,
    is_variable_declaration, length, maybe_get_source_file_of_node, set_text_range_rc_node, some,
    symbol_name, unescape_leading_underscores, with_synthetic_factory_and_factory, Debug_, Empty,
    InternalSymbolName, ModifierFlags, Node, NodeArray, NodeArrayOrVec, NodeBuilder, NodeFlags,
    NodeInterface, NodeWrappered, ObjectFlags, SignatureKind, StrOrRcNode, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, Ternary, ThenAnd, Type, TypeChecker, TypeInterface, TypeWrappered,
};

use super::{
    NodeBuilderContext, SignatureToSignatureDeclarationOptions, SymbolTableToDeclarationStatements,
};

impl SymbolTableToDeclarationStatements {
    pub(super) fn serialize_export_specifier(
        &self,
        local_name: &str,
        target_name: &str,
        specifier: Option<impl Borrow<Node /*Expression*/>>,
    ) {
        self.add_result(
            &with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                Gc::<Node>::from(
                    factory_.create_export_declaration(
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
                                            (local_name != target_name).then_some(target_name),
                                            local_name,
                                        )
                                        .into()],
                                )
                                .into(),
                        ),
                        specifier.node_wrappered(),
                        None,
                    ),
                )
            }),
            ModifierFlags::None,
        );
    }

    pub(super) fn serialize_maybe_alias_assignment(&self, symbol: &Symbol) -> bool {
        if symbol.flags().intersects(SymbolFlags::Prototype) {
            return false;
        }
        let name = unescape_leading_underscores(symbol.escaped_name());
        let is_export_equals = name == InternalSymbolName::ExportEquals;
        let is_default = name == InternalSymbolName::Default;
        let is_export_assignment_compatible_symbol_name = is_export_equals || is_default;
        let alias_decl = symbol
            .maybe_declarations()
            .is_some()
            .then_and(|| self.type_checker.get_declaration_of_alias_symbol(symbol));
        let target = alias_decl.as_ref().and_then(|alias_decl| {
            self.type_checker
                .get_target_of_alias_declaration(alias_decl, Some(true))
        });
        if let Some(target) = target.as_ref().filter(|target| {
            length(target.maybe_declarations().as_deref()) > 0
                && some(
                    target.maybe_declarations().as_deref(),
                    Some(|d: &Gc<Node>| {
                        Gc::ptr_eq(
                            &get_source_file_of_node(d),
                            &get_source_file_of_node(&self.enclosing_declaration),
                        )
                    }),
                )
        }) {
            let expr = alias_decl.as_ref().map(|alias_decl| {
                if is_export_assignment(alias_decl) || is_binary_expression(alias_decl) {
                    get_export_assignment_expression(alias_decl)
                } else {
                    get_property_assignment_alias_like_expression(alias_decl)
                }
            });
            let first = expr
                .as_ref()
                .filter(|expr| is_entity_name_expression(expr))
                .map(|expr| {
                    self.type_checker
                        .get_first_non_module_exports_identifier(expr)
                });
            let referenced = first.as_ref().and_then(|first| {
                self.type_checker.resolve_entity_name(
                    first,
                    SymbolFlags::All,
                    Some(true),
                    Some(true),
                    Some(&*self.enclosing_declaration),
                )
            });
            // if (referenced || target) {
            self.include_private_symbol(referenced.as_ref().unwrap_or(target));
            // }
            self.context().tracker().disable_track_symbol();
            if is_export_assignment_compatible_symbol_name {
                self.results_mut().push(with_synthetic_factory_and_factory(
                    |synthetic_factory_, factory_| {
                        factory_
                            .create_export_assignment(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Option::<Gc<NodeArray>>::None,
                                Some(is_export_equals),
                                self.node_builder.symbol_to_expression_(
                                    target,
                                    &self.context(),
                                    Some(SymbolFlags::All),
                                ),
                            )
                            .into()
                    },
                ));
            } else {
                if let Some(first) = first.as_ref().filter(|first| {
                    matches!(
                        expr.as_ref(),
                        Some(expr) if Gc::ptr_eq(*first, expr)
                    )
                }) {
                    self.serialize_export_specifier(name, id_text(first), Option::<&Node>::None);
                } else if let Some(expr) = expr.as_ref().filter(|expr| is_class_expression(expr)) {
                    self.serialize_export_specifier(
                        name,
                        &self.get_internal_symbol_name(target, &symbol_name(target)),
                        Option::<&Node>::None,
                    );
                } else {
                    let var_name = self.get_unused_name(name, Some(symbol));
                    self.add_result(
                        &with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                            Gc::<Node>::from(factory_.create_import_equals_declaration(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Option::<Gc<NodeArray>>::None,
                                false,
                                Gc::<Node>::from(factory_.create_identifier(
                                    synthetic_factory_,
                                    &var_name,
                                    Option::<Gc<NodeArray>>::None,
                                    None,
                                )),
                                self.node_builder.symbol_to_name(
                                    target,
                                    &self.context(),
                                    Some(SymbolFlags::All),
                                    false,
                                ),
                            ))
                        }),
                        ModifierFlags::None,
                    );
                    self.serialize_export_specifier(name, &var_name, Option::<&Node>::None);
                }
            }
            self.context().tracker().reenable_track_symbol();
            true
        } else {
            let var_name = self.get_unused_name(name, Some(symbol));
            let ref type_to_serialize =
                self.type_checker
                    .get_widened_type(&self.type_checker.get_type_of_symbol(
                        &self.type_checker.get_merged_symbol(Some(symbol)).unwrap(),
                    ));
            if self.is_type_representable_as_function_namespace_merge(type_to_serialize, symbol) {
                self.serialize_as_function_namespace_merge(
                    type_to_serialize,
                    symbol,
                    &var_name,
                    if is_export_assignment_compatible_symbol_name {
                        ModifierFlags::None
                    } else {
                        ModifierFlags::Export
                    },
                );
            } else {
                let statement: Gc<Node> =
                    with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                        factory_
                            .create_variable_statement(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Gc::<Node>::from(factory_.create_variable_declaration_list(
                                    synthetic_factory_,
                                    vec![
                                    factory_.create_variable_declaration(
                                        synthetic_factory_,
                                        Some(&*var_name),
                                        None,
                                        Some(self.node_builder.serialize_type_for_declaration(
                                            &self.context(),
                                            type_to_serialize,
                                            symbol,
                                            Some(&*self.enclosing_declaration),
                                            Some(&|symbol: &Symbol| {
                                                self.include_private_symbol(symbol);
                                            }),
                                            self.bundled,
                                        )),
                                        None,
                                    ).into()
                                ],
                                    Some(NodeFlags::Const),
                                )),
                            )
                            .into()
                    });
                self.add_result(
                    &statement,
                    if matches!(
                        target.as_ref(),
                        Some(target) if target.flags().intersects(SymbolFlags::Property) &&
                            target.escaped_name() == InternalSymbolName::ExportEquals
                    ) {
                        ModifierFlags::Ambient
                    } else if name == var_name {
                        ModifierFlags::Export
                    } else {
                        ModifierFlags::None
                    },
                );
            }
            if is_export_assignment_compatible_symbol_name {
                self.results_mut().push(with_synthetic_factory_and_factory(
                    |synthetic_factory_, factory_| {
                        factory_
                            .create_export_assignment(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Option::<Gc<NodeArray>>::None,
                                Some(is_export_equals),
                                factory_
                                    .create_identifier(
                                        synthetic_factory_,
                                        &var_name,
                                        Option::<Gc<NodeArray>>::None,
                                        None,
                                    )
                                    .into(),
                            )
                            .into()
                    },
                ));
                return true;
            } else if name != var_name {
                self.serialize_export_specifier(name, &var_name, Option::<&Node>::None);
                return true;
            }
            false
        }
    }

    pub(super) fn is_type_representable_as_function_namespace_merge(
        &self,
        type_to_serialize: &Type,
        host_symbol: &Symbol,
    ) -> bool {
        let ctx_src = maybe_get_source_file_of_node(self.context().maybe_enclosing_declaration());
        get_object_flags(type_to_serialize).intersects(ObjectFlags::Anonymous | ObjectFlags::Mapped)
            && self
                .type_checker
                .get_index_infos_of_type(type_to_serialize)
                .is_empty()
            && !self.type_checker.is_class_instance_side(type_to_serialize)
            && (!self
                .type_checker
                .get_properties_of_type(type_to_serialize)
                .into_iter()
                .filter(|property| self.is_namespace_member(property))
                .empty()
                || !self
                    .type_checker
                    .get_signatures_of_type(type_to_serialize, SignatureKind::Call)
                    .is_empty())
            && self
                .type_checker
                .get_signatures_of_type(type_to_serialize, SignatureKind::Construct)
                .is_empty()
            && self
                .node_builder
                .get_declaration_with_type_annotation(
                    host_symbol,
                    Some(&*self.enclosing_declaration),
                )
                .is_none()
            && !matches!(
                type_to_serialize.maybe_symbol(),
                Some(type_to_serialize_symbol) if some(
                    type_to_serialize_symbol.maybe_declarations().as_deref(),
                    Some(|d: &Gc<Node>| !are_option_gcs_equal(
                        maybe_get_source_file_of_node(Some(&**d)).as_ref(),
                        ctx_src.as_ref()
                    ))
                )
            )
            && !self
                .type_checker
                .get_properties_of_type(type_to_serialize)
                .iter()
                .any(|p| self.type_checker.is_late_bound_name(p.escaped_name()))
            && !self
                .type_checker
                .get_properties_of_type(type_to_serialize)
                .iter()
                .any(|p| {
                    some(
                        p.maybe_declarations().as_deref(),
                        Some(|d: &Gc<Node>| {
                            !are_option_gcs_equal(
                                maybe_get_source_file_of_node(Some(&**d)).as_ref(),
                                ctx_src.as_ref(),
                            )
                        }),
                    )
                })
            && self
                .type_checker
                .get_properties_of_type(type_to_serialize)
                .iter()
                .all(|p| {
                    is_identifier_text(
                        &symbol_name(p),
                        Some(self.type_checker.language_version),
                        None,
                    )
                })
    }

    pub(super) fn make_serialize_property_symbol(
        &self,
        create_property: Gc<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
        method_kind: SyntaxKind,
        use_accessors: bool,
    ) -> MakeSerializePropertySymbol {
        MakeSerializePropertySymbol::new(
            self.type_checker.clone(),
            self.node_builder.clone(),
            self.context(),
            self.rc_wrapper(),
            create_property,
            method_kind,
            use_accessors,
        )
    }

    pub(super) fn serialize_property_symbol_for_interface(
        &self,
        p: &Symbol,
        base_type: Option<impl Borrow<Type>>,
    ) -> Vec<Gc<Node>> {
        self.serialize_property_symbol_for_interface_worker().call(
            p,
            false,
            base_type.type_wrappered().as_deref(),
        )
    }

    pub(super) fn serialize_signatures(
        &self,
        kind: SignatureKind,
        input: &Type,
        base_type: Option<impl Borrow<Type>>,
        output_kind: SyntaxKind,
    ) -> Vec<Gc<Node>> {
        let signatures = self.type_checker.get_signatures_of_type(input, kind);
        if kind == SignatureKind::Construct {
            if base_type.is_none() && signatures.iter().all(|s| s.parameters().is_empty()) {
                return vec![];
            }
            if let Some(base_type) = base_type {
                let base_type = base_type.borrow();
                let base_sigs = self
                    .type_checker
                    .get_signatures_of_type(base_type, SignatureKind::Construct);
                if base_sigs.is_empty() && signatures.iter().all(|s| s.parameters().is_empty()) {
                    return vec![];
                }
                if base_sigs.len() == signatures.len() {
                    let mut failed = false;
                    for i in 0..base_sigs.len() {
                        if self.type_checker.compare_signatures_identical(
                            signatures[i].clone(),
                            base_sigs[i].clone(),
                            false,
                            false,
                            true,
                            |a: &Type, b: &Type| self.type_checker.compare_types_identical(a, b),
                        ) == Ternary::False
                        {
                            failed = true;
                            break;
                        }
                    }
                    if !failed {
                        return vec![];
                    }
                }
            }
            let mut private_protected = ModifierFlags::None;
            for s in &signatures {
                if let Some(s_declaration) = s.declaration.as_ref() {
                    private_protected |= get_selected_effective_modifier_flags(
                        s_declaration,
                        ModifierFlags::Private | ModifierFlags::Protected,
                    );
                }
            }
            if private_protected != ModifierFlags::None {
                return vec![set_text_range_rc_node(
                    with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                        factory_
                            .create_constructor_declaration(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Some(factory_.create_modifiers_from_modifier_flags(
                                    synthetic_factory_,
                                    private_protected,
                                )),
                                Some(vec![]),
                                None,
                            )
                            .into()
                    }),
                    signatures[0].declaration.as_deref(),
                )];
            }
        }

        let mut results: Vec<Gc<Node>> = Default::default();
        for sig in &signatures {
            let decl = self.node_builder.signature_to_signature_declaration_helper(
                sig.clone(),
                output_kind,
                &self.context(),
                Option::<SignatureToSignatureDeclarationOptions<fn(&Symbol)>>::None,
            );
            results.push(set_text_range_rc_node(decl, sig.declaration.as_deref()));
        }
        results
    }

    pub(super) fn serialize_index_signatures(
        &self,
        input: &Type,
        base_type: Option<impl Borrow<Type>>,
    ) -> Vec<Gc<Node>> {
        let mut results: Vec<Gc<Node /*IndexSignatureDeclaration*/>> = Default::default();
        let base_type = base_type.type_wrappered();
        for info in &self.type_checker.get_index_infos_of_type(input) {
            if let Some(base_type) = base_type.as_ref() {
                let base_info = self
                    .type_checker
                    .get_index_info_of_type_(base_type, &info.key_type);
                if let Some(base_info) = base_info.as_ref() {
                    if self
                        .type_checker
                        .is_type_identical_to(&info.type_, &base_info.type_)
                    {
                        continue;
                    }
                }
            }
            results.push(
                self.node_builder
                    .index_info_to_index_signature_declaration_helper(
                        info,
                        &self.context(),
                        Option::<&Node>::None,
                    ),
            );
        }
        results
    }

    pub(super) fn serialize_base_type(
        &self,
        t: &Type,
        static_type: &Type,
        root_name: &str,
    ) -> Gc<Node> {
        let ref_ = self.try_serialize_as_type_reference(t, SymbolFlags::Value);
        if let Some(ref_) = ref_ {
            return ref_;
        }
        let temp_name = self.get_unused_name(&format!("{root_name}_base"), Option::<&Symbol>::None);
        let statement: Gc<Node> =
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_variable_statement(
                        synthetic_factory_,
                        Option::<Gc<NodeArray>>::None,
                        Gc::<Node>::from(factory_.create_variable_declaration_list(
                            synthetic_factory_,
                            vec![
                            factory_.create_variable_declaration(
                                synthetic_factory_,
                                Some(&*temp_name),
                                None,
                                self.node_builder.type_to_type_node_helper(
                                    Some(static_type),
                                    &self.context(),
                                ),
                                None,
                            ).into()
                        ],
                            Some(NodeFlags::Const),
                        )),
                    )
                    .into()
            });
        self.add_result(&statement, ModifierFlags::None);
        with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
            factory_
                .create_expression_with_type_arguments(
                    synthetic_factory_,
                    factory_
                        .create_identifier(
                            synthetic_factory_,
                            &temp_name,
                            Option::<Gc<NodeArray>>::None,
                            None,
                        )
                        .into(),
                    Option::<Gc<NodeArray>>::None,
                )
                .into()
        })
    }

    pub(super) fn try_serialize_as_type_reference(
        &self,
        t: &Type,
        flags: SymbolFlags,
    ) -> Option<Gc<Node>> {
        let mut type_args: Option<Vec<Gc<Node /*TypeNode*/>>> = Default::default();
        let mut reference: Option<Gc<Node /*Expression*/>> = Default::default();

        if let Some(t_target) = t
            .maybe_as_type_reference_interface()
            .map(|t| t.target())
            .filter(|t_target| {
                self.type_checker.is_symbol_accessible_by_flags(
                    &t_target.symbol(),
                    Some(&*self.enclosing_declaration),
                    flags,
                )
            })
        {
            type_args = Some(
                self.type_checker
                    .get_type_arguments(t)
                    .iter()
                    .map(|t| {
                        self.node_builder
                            .type_to_type_node_helper(Some(&**t), &self.context())
                            .unwrap()
                    })
                    .collect(),
            );
            reference = Some(self.node_builder.symbol_to_expression_(
                &t_target.symbol(),
                &self.context(),
                Some(SymbolFlags::Type),
            ));
        } else if let Some(t_symbol) = t.maybe_symbol().as_ref().filter(|t_symbol| {
            self.type_checker.is_symbol_accessible_by_flags(
                t_symbol,
                Some(&*self.enclosing_declaration),
                flags,
            )
        }) {
            reference = Some(self.node_builder.symbol_to_expression_(
                &t.symbol(),
                &self.context(),
                Some(SymbolFlags::Type),
            ));
        }
        reference.map(|reference| {
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_expression_with_type_arguments(synthetic_factory_, reference, type_args)
                    .into()
            })
        })
    }

    pub(super) fn serialize_implemented_type(&self, t: &Type) -> Option<Gc<Node>> {
        let ref_ = self.try_serialize_as_type_reference(t, SymbolFlags::Type);
        if ref_.is_some() {
            return ref_;
        }
        t.maybe_symbol().as_ref().map(|t_symbol| {
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_expression_with_type_arguments(
                        synthetic_factory_,
                        self.node_builder.symbol_to_expression_(
                            t_symbol,
                            &self.context(),
                            Some(SymbolFlags::Type),
                        ),
                        Option::<Gc<NodeArray>>::None,
                    )
                    .into()
            })
        })
    }

    pub(super) fn get_unused_name(
        &self,
        input: &str,
        symbol: Option<impl Borrow<Symbol>>,
    ) -> String {
        unimplemented!()
    }

    pub(super) fn get_internal_symbol_name(&self, symbol: &Symbol, local_name: &str) -> String {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct MakeSerializePropertySymbol {
    type_checker: Gc<TypeChecker>,
    node_builder: Gc<NodeBuilder>,
    context: Gc<NodeBuilderContext>,
    symbol_table_to_declaration_statements: Gc<SymbolTableToDeclarationStatements>,
    create_property: Gc<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
    method_kind: SyntaxKind,
    use_accessors: bool,
}

impl MakeSerializePropertySymbol {
    pub(super) fn new(
        type_checker: Gc<TypeChecker>,
        node_builder: Gc<NodeBuilder>,
        context: Gc<NodeBuilderContext>,
        symbol_table_to_declaration_statements: Gc<SymbolTableToDeclarationStatements>,
        create_property: Gc<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
        method_kind: SyntaxKind,
        use_accessors: bool,
    ) -> Self {
        Self {
            type_checker,
            node_builder,
            context,
            symbol_table_to_declaration_statements,
            create_property,
            method_kind,
            use_accessors,
        }
    }

    pub(super) fn call(
        &self,
        p: &Symbol,
        is_static: bool,
        base_type: Option<&Type>,
    ) -> Vec<Gc<Node>> {
        let modifier_flags = get_declaration_modifier_flags_from_symbol(p, None);
        let is_private = modifier_flags.intersects(ModifierFlags::Private);
        if is_static
            && p.flags()
                .intersects(SymbolFlags::Type | SymbolFlags::Namespace | SymbolFlags::Alias)
        {
            return vec![];
        }
        if p.flags().intersects(SymbolFlags::Prototype)
            || matches!(
                base_type,
                Some(base_type) if self.type_checker.get_property_of_type_(base_type, p.escaped_name(), None).is_some() &&
                    self.type_checker.is_readonly_symbol(
                        &self.type_checker.get_property_of_type_(base_type, p.escaped_name(), None).unwrap()
                    ) == self.type_checker.is_readonly_symbol(p) &&
                    p.flags() & SymbolFlags::Optional ==
                        self.type_checker.get_property_of_type_(base_type, p.escaped_name(), None).unwrap().flags() & SymbolFlags::Optional &&
                    self.type_checker.is_type_identical_to(
                        &self.type_checker.get_type_of_symbol(p),
                        &self.type_checker.get_type_of_property_of_type_(
                            base_type,
                            p.escaped_name()
                        ).unwrap()
                    )
            )
        {
            return vec![];
        }
        let flag = (modifier_flags & !ModifierFlags::Async)
            | (if is_static {
                ModifierFlags::Static
            } else {
                ModifierFlags::None
            });
        let ref name = self
            .node_builder
            .get_property_name_node_for_symbol(p, &self.context);
        let first_property_like_decl = p.maybe_declarations().as_ref().and_then(|p_declarations| {
            p_declarations
                .iter()
                .find(|declaration| {
                    is_property_declaration(declaration)
                        || is_accessor(declaration)
                        || is_variable_declaration(declaration)
                        || is_property_signature(declaration)
                        || is_binary_expression(declaration)
                        || is_property_access_expression(declaration)
                })
                .cloned()
        });
        if p.flags().intersects(SymbolFlags::Accessor) && self.use_accessors {
            let mut result: Vec<Gc<Node /*AccessorDeclaration*/>> = Default::default();
            if p.flags().intersects(SymbolFlags::SetAccessor) {
                result.push(set_text_range_rc_node(
                    with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                        factory_
                            .create_set_accessor_declaration(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Some(factory_.create_modifiers_from_modifier_flags(
                                    synthetic_factory_,
                                    flag,
                                )),
                                name.clone(),
                                vec![factory_
                                    .create_parameter_declaration(
                                        synthetic_factory_,
                                        Option::<Gc<NodeArray>>::None,
                                        Option::<Gc<NodeArray>>::None,
                                        None,
                                        Some("arg"),
                                        None,
                                        if is_private {
                                            None
                                        } else {
                                            Some(
                                                self.node_builder.serialize_type_for_declaration(
                                                    &self.context,
                                                    &self.type_checker.get_type_of_symbol(p),
                                                    p,
                                                    Some(
                                                        &*self
                                                            .symbol_table_to_declaration_statements
                                                            .enclosing_declaration,
                                                    ),
                                                    Some(&|symbol: &Symbol| {
                                                        self.symbol_table_to_declaration_statements
                                                            .include_private_symbol(symbol);
                                                    }),
                                                    self.symbol_table_to_declaration_statements
                                                        .bundled,
                                                ),
                                            )
                                        },
                                        None,
                                    )
                                    .into()],
                                None,
                            )
                            .into()
                    }),
                    p.maybe_declarations()
                        .as_ref()
                        .and_then(|p_declarations| {
                            p_declarations
                                .iter()
                                .find(|declaration| is_set_accessor(declaration))
                                .cloned()
                                .or_else(|| first_property_like_decl.clone())
                        })
                        .as_deref(),
                ));
            }
            if p.flags().intersects(SymbolFlags::GetAccessor) {
                let is_private = modifier_flags.intersects(ModifierFlags::Private);
                result.push(set_text_range_rc_node(
                    with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                        factory_
                            .create_get_accessor_declaration(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Some(factory_.create_modifiers_from_modifier_flags(
                                    synthetic_factory_,
                                    flag,
                                )),
                                name.clone(),
                                vec![],
                                if is_private {
                                    None
                                } else {
                                    Some(
                                        self.node_builder.serialize_type_for_declaration(
                                            &self.context,
                                            &self.type_checker.get_type_of_symbol(p),
                                            p,
                                            Some(
                                                &*self
                                                    .symbol_table_to_declaration_statements
                                                    .enclosing_declaration,
                                            ),
                                            Some(&|symbol: &Symbol| {
                                                self.symbol_table_to_declaration_statements
                                                    .include_private_symbol(symbol);
                                            }),
                                            self.symbol_table_to_declaration_statements.bundled,
                                        ),
                                    )
                                },
                                None,
                            )
                            .into()
                    }),
                    p.maybe_declarations()
                        .as_ref()
                        .and_then(|p_declarations| {
                            p_declarations
                                .iter()
                                .find(|declaration| is_get_accessor(declaration))
                                .cloned()
                                .or_else(|| first_property_like_decl.clone())
                        })
                        .as_deref(),
                ));
            }
            return result;
        } else if p
            .flags()
            .intersects(SymbolFlags::Property | SymbolFlags::Variable | SymbolFlags::Accessor)
        {
            return vec![set_text_range_rc_node(
                self.create_property.call(
                    None,
                    Some(with_synthetic_factory_and_factory(
                        |synthetic_factory_, factory_| {
                            factory_
                                .create_modifiers_from_modifier_flags(
                                    synthetic_factory_,
                                    if self.type_checker.is_readonly_symbol(p) {
                                        ModifierFlags::Readonly
                                    } else {
                                        ModifierFlags::None
                                    } | flag,
                                )
                                .into()
                        },
                    )),
                    name.clone().into(),
                    p.flags().intersects(SymbolFlags::Optional).then(|| {
                        with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                            factory_
                                .create_token(synthetic_factory_, SyntaxKind::QuestionToken)
                                .into()
                        })
                    }),
                    (!is_private).then(|| {
                        self.node_builder.serialize_type_for_declaration(
                            &self.context,
                            &self.type_checker.get_type_of_symbol(p),
                            p,
                            Some(
                                &*self
                                    .symbol_table_to_declaration_statements
                                    .enclosing_declaration,
                            ),
                            Some(&|symbol: &Symbol| {
                                self.symbol_table_to_declaration_statements
                                    .include_private_symbol(symbol);
                            }),
                            self.symbol_table_to_declaration_statements.bundled,
                        )
                    }),
                    None,
                ),
                p.maybe_declarations()
                    .as_ref()
                    .and_then(|p_declarations| {
                        p_declarations
                            .iter()
                            .find(|declaration| {
                                is_property_declaration(declaration)
                                    || is_variable_declaration(declaration)
                            })
                            .cloned()
                            .or_else(|| first_property_like_decl.clone())
                    })
                    .as_deref(),
            )];
        }
        if p.flags()
            .intersects(SymbolFlags::Method | SymbolFlags::Function)
        {
            let ref type_ = self.type_checker.get_type_of_symbol(p);
            let signatures = self
                .type_checker
                .get_signatures_of_type(type_, SignatureKind::Call);
            if flag.intersects(ModifierFlags::Private) {
                return vec![set_text_range_rc_node(
                    self.create_property.call(
                        None,
                        Some(with_synthetic_factory_and_factory(
                            |synthetic_factory_, factory_| {
                                factory_
                                    .create_modifiers_from_modifier_flags(
                                        synthetic_factory_,
                                        if self.type_checker.is_readonly_symbol(p) {
                                            ModifierFlags::Readonly
                                        } else {
                                            ModifierFlags::None
                                        } | flag,
                                    )
                                    .into()
                            },
                        )),
                        name.clone().into(),
                        p.flags().intersects(SymbolFlags::Optional).then(|| {
                            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                                factory_
                                    .create_token(synthetic_factory_, SyntaxKind::QuestionToken)
                                    .into()
                            })
                        }),
                        None,
                        None,
                    ),
                    p.maybe_declarations()
                        .as_ref()
                        .and_then(|p_declarations| {
                            p_declarations
                                .iter()
                                .find(|declaration| is_function_like_declaration(declaration))
                                .cloned()
                                .or_else(|| {
                                    signatures
                                        .get(0)
                                        .and_then(|signatures_0| signatures_0.declaration.clone())
                                })
                                .or_else(|| {
                                    p.maybe_declarations()
                                        .as_ref()
                                        .and_then(|p_declarations| p_declarations.get(0).cloned())
                                })
                        })
                        .as_deref(),
                )];
            }

            let mut results: Vec<Gc<Node>> = Default::default();
            for ref sig in signatures {
                let decl = self.node_builder.signature_to_signature_declaration_helper(
                    sig.clone(),
                    self.method_kind,
                    &self.context,
                    Some(SignatureToSignatureDeclarationOptions {
                        name: Some(name.clone()),
                        question_token: p.flags().intersects(SymbolFlags::Optional).then(|| {
                            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                                factory_
                                    .create_token(synthetic_factory_, SyntaxKind::QuestionToken)
                                    .into()
                            })
                        }),
                        modifiers: (flag != ModifierFlags::None).then(|| {
                            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                                factory_
                                    .create_modifiers_from_modifier_flags(synthetic_factory_, flag)
                            })
                        }),
                        private_symbol_visitor: Option::<fn(&Symbol)>::None,
                        bundled_imports: None,
                    }),
                );
                let location = sig
                    .declaration
                    .as_ref()
                    .filter(|sig_declaration| {
                        is_prototype_property_assignment(&sig_declaration.parent())
                    })
                    .map(|sig_declaration| sig_declaration.parent())
                    .or_else(|| sig.declaration.clone());
                results.push(set_text_range_rc_node(decl, location.as_deref()));
            }
            return results;
        }
        Debug_.fail(Some(&format!(
            "Unhandled class member kind! {:?}",
            /*(p as any).__debugFlags ||*/ p.flags()
        )))
    }
}

pub(super) trait MakeSerializePropertySymbolCreateProperty: Trace + Finalize {
    fn call(
        &self,
        decorators: Option<NodeArrayOrVec /*Decorator*/>,
        modifiers: Option<NodeArrayOrVec /*Modifier*/>,
        name: StrOrRcNode<'_>, /*PropertyName*/
        question_or_exclamation_token: Option<Gc<Node /*QuestionToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node>;
}
