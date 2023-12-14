use std::{borrow::Borrow, io};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;
use regex::Regex;

use super::{
    NodeBuilderContext, SignatureToSignatureDeclarationOptions, SymbolTableToDeclarationStatements,
};
use crate::{
    are_option_gcs_equal, get_declaration_modifier_flags_from_symbol,
    get_export_assignment_expression, get_factory, get_object_flags,
    get_property_assignment_alias_like_expression, get_selected_effective_modifier_flags,
    get_source_file_of_node, get_symbol_id, id_text, is_accessor, is_binary_expression,
    is_class_expression, is_entity_name_expression, is_export_assignment,
    is_function_like_declaration, is_get_accessor, is_identifier_text,
    is_property_access_expression, is_property_declaration, is_property_signature,
    is_prototype_property_assignment, is_set_accessor, is_single_or_double_quote,
    is_string_a_non_contextual_keyword, is_variable_declaration, length,
    maybe_get_source_file_of_node, set_text_range_rc_node, some, strip_quotes, symbol_name,
    unescape_leading_underscores, BoolExt, Debug_, InternalSymbolName, IteratorExt, Matches,
    ModifierFlags, Node, NodeArray, NodeArrayOrVec, NodeBuilder, NodeBuilderFlags, NodeFlags,
    NodeInterface, NodeWrappered, ObjectFlags, OptionTry, SignatureKind, StrOrRcNode, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, Ternary, Type, TypeChecker, TypeInterface,
};

impl SymbolTableToDeclarationStatements {
    pub(super) fn serialize_export_specifier(
        &self,
        local_name: &str,
        target_name: &str,
        specifier: Option<impl Borrow<Node /*Expression*/>>,
    ) {
        self.add_result(
            &get_factory().create_export_declaration(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                false,
                Some(get_factory().create_named_exports(vec![
                    get_factory().create_export_specifier(
                        false,
                        (local_name != target_name).then_some(target_name),
                        local_name,
                    ),
                ])),
                specifier.node_wrappered(),
                None,
            ),
            ModifierFlags::None,
        );
    }

    pub(super) fn serialize_maybe_alias_assignment(&self, symbol: Id<Symbol>) -> io::Result<bool> {
        if self
            .type_checker
            .symbol(symbol)
            .flags()
            .intersects(SymbolFlags::Prototype)
        {
            return Ok(false);
        }
        let symbol_ref = self.type_checker.symbol(symbol);
        let name = unescape_leading_underscores(symbol_ref.escaped_name());
        let is_export_equals = name == InternalSymbolName::ExportEquals;
        let is_default = name == InternalSymbolName::Default;
        let is_export_assignment_compatible_symbol_name = is_export_equals || is_default;
        let alias_decl = self
            .type_checker
            .symbol(symbol)
            .maybe_declarations()
            .is_some()
            .try_then_and(|| self.type_checker.get_declaration_of_alias_symbol(symbol))?;
        let target = alias_decl.as_ref().try_and_then(|alias_decl| {
            self.type_checker
                .get_target_of_alias_declaration(alias_decl, Some(true))
        })?;
        Ok(
            if let Some(target) = target.filter(|&target| {
                length(
                    self.type_checker
                        .symbol(target)
                        .maybe_declarations()
                        .as_deref(),
                ) > 0
                    && some(
                        self.type_checker
                            .symbol(target)
                            .maybe_declarations()
                            .as_deref(),
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
                let referenced = first.as_ref().try_and_then(|first| {
                    self.type_checker.resolve_entity_name(
                        first,
                        SymbolFlags::All,
                        Some(true),
                        Some(true),
                        Some(&*self.enclosing_declaration),
                    )
                })?;
                // if (referenced || target) {
                self.include_private_symbol(referenced.unwrap_or(target));
                // }
                self.context().tracker().disable_track_symbol();
                if is_export_assignment_compatible_symbol_name {
                    self.results_mut()
                        .push(get_factory().create_export_assignment(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            Some(is_export_equals),
                            self.node_builder.symbol_to_expression_(
                                target,
                                &self.context(),
                                Some(SymbolFlags::All),
                            )?,
                        ));
                } else {
                    if let Some(first) = first.as_ref().filter(|first| {
                        matches!(
                            expr.as_ref(),
                            Some(expr) if Gc::ptr_eq(*first, expr)
                        )
                    }) {
                        self.serialize_export_specifier(
                            name,
                            id_text(first),
                            Option::<&Node>::None,
                        );
                    } else if expr.as_ref().matches(|expr| is_class_expression(expr)) {
                        self.serialize_export_specifier(
                            name,
                            &self.get_internal_symbol_name(
                                target,
                                &symbol_name(&self.type_checker.symbol(target)),
                            ),
                            Option::<&Node>::None,
                        );
                    } else {
                        let var_name = self.get_unused_name(name, Some(symbol));
                        self.add_result(
                            &get_factory().create_import_equals_declaration(
                                Option::<Gc<NodeArray>>::None,
                                Option::<Gc<NodeArray>>::None,
                                false,
                                get_factory().create_identifier(&var_name),
                                self.node_builder.symbol_to_name(
                                    target,
                                    &self.context(),
                                    Some(SymbolFlags::All),
                                    false,
                                )?,
                            ),
                            ModifierFlags::None,
                        );
                        self.serialize_export_specifier(name, &var_name, Option::<&Node>::None);
                    }
                }
                self.context().tracker().reenable_track_symbol();
                true
            } else {
                let var_name = self.get_unused_name(name, Some(symbol));
                let type_to_serialize =
                    self.type_checker
                        .get_widened_type(self.type_checker.get_type_of_symbol(
                            self.type_checker.get_merged_symbol(Some(symbol)).unwrap(),
                        )?)?;
                if self
                    .is_type_representable_as_function_namespace_merge(type_to_serialize, symbol)?
                {
                    self.serialize_as_function_namespace_merge(
                        type_to_serialize,
                        symbol,
                        &var_name,
                        if is_export_assignment_compatible_symbol_name {
                            ModifierFlags::None
                        } else {
                            ModifierFlags::Export
                        },
                    )?;
                } else {
                    let statement = get_factory().create_variable_statement(
                        Option::<Gc<NodeArray>>::None,
                        get_factory().create_variable_declaration_list(
                            vec![get_factory().create_variable_declaration(
                                Some(&*var_name),
                                None,
                                Some(self.node_builder.serialize_type_for_declaration(
                                    &self.context(),
                                    type_to_serialize,
                                    symbol,
                                    Some(&*self.enclosing_declaration),
                                    Some(&|symbol: Id<Symbol>| {
                                        self.include_private_symbol(symbol);
                                    }),
                                    self.bundled,
                                )?),
                                None,
                            )],
                            Some(NodeFlags::Const),
                        ),
                    );
                    self.add_result(
                        &statement,
                        if matches!(
                            target,
                            Some(target) if self.type_checker.symbol(target).flags().intersects(SymbolFlags::Property) &&
                                self.type_checker.symbol(target).escaped_name() == InternalSymbolName::ExportEquals
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
                    self.results_mut()
                        .push(get_factory().create_export_assignment(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            Some(is_export_equals),
                            get_factory().create_identifier(&var_name),
                        ));
                    return Ok(true);
                } else if name != var_name {
                    self.serialize_export_specifier(name, &var_name, Option::<&Node>::None);
                    return Ok(true);
                }
                false
            },
        )
    }

    pub(super) fn is_type_representable_as_function_namespace_merge(
        &self,
        type_to_serialize: Id<Type>,
        host_symbol: Id<Symbol>,
    ) -> io::Result<bool> {
        let ctx_src = maybe_get_source_file_of_node(self.context().maybe_enclosing_declaration());
        Ok(
            get_object_flags(&self.type_checker.type_(type_to_serialize))
                .intersects(ObjectFlags::Anonymous | ObjectFlags::Mapped)
                && self
                    .type_checker
                    .get_index_infos_of_type(type_to_serialize)?
                    .is_empty()
                && !self
                    .type_checker
                    .is_class_instance_side(type_to_serialize)?
                && (!self
                    .type_checker
                    .get_properties_of_type(type_to_serialize)?
                    .into_iter()
                    .filter(|&property| self.is_namespace_member(property))
                    .empty()
                    || !self
                        .type_checker
                        .get_signatures_of_type(type_to_serialize, SignatureKind::Call)?
                        .is_empty())
                && self
                    .type_checker
                    .get_signatures_of_type(type_to_serialize, SignatureKind::Construct)?
                    .is_empty()
                && self
                    .node_builder
                    .get_declaration_with_type_annotation(
                        host_symbol,
                        Some(&*self.enclosing_declaration),
                    )
                    .is_none()
                && !matches!(
                    self.type_checker.type_(type_to_serialize).maybe_symbol(),
                    Some(type_to_serialize_symbol) if some(
                        self.type_checker.symbol(type_to_serialize_symbol).maybe_declarations().as_deref(),
                        Some(|d: &Gc<Node>| !are_option_gcs_equal(
                            maybe_get_source_file_of_node(Some(&**d)).as_ref(),
                            ctx_src.as_ref()
                        ))
                    )
                )
                && !self
                    .type_checker
                    .get_properties_of_type(type_to_serialize)?
                    .any(|p| {
                        self.type_checker
                            .is_late_bound_name(self.type_checker.symbol(p).escaped_name())
                    })
                && !self
                    .type_checker
                    .get_properties_of_type(type_to_serialize)?
                    .any(|p| {
                        some(
                            self.type_checker.symbol(p).maybe_declarations().as_deref(),
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
                    .get_properties_of_type(type_to_serialize)?
                    .all(|p| {
                        is_identifier_text(
                            &symbol_name(&self.type_checker.symbol(p)),
                            Some(self.type_checker.language_version),
                            None,
                        )
                    }),
        )
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
        p: Id<Symbol>,
        base_type: Option<Id<Type>>,
    ) -> io::Result<Vec<Gc<Node>>> {
        self.serialize_property_symbol_for_interface_worker()
            .call(p, false, base_type)
    }

    pub(super) fn serialize_signatures(
        &self,
        kind: SignatureKind,
        input: Id<Type>,
        base_type: Option<Id<Type>>,
        output_kind: SyntaxKind,
    ) -> io::Result<Vec<Gc<Node>>> {
        let signatures = self.type_checker.get_signatures_of_type(input, kind)?;
        if kind == SignatureKind::Construct {
            if base_type.is_none() && signatures.iter().all(|s| s.parameters().is_empty()) {
                return Ok(vec![]);
            }
            if let Some(base_type) = base_type {
                let base_sigs = self
                    .type_checker
                    .get_signatures_of_type(base_type, SignatureKind::Construct)?;
                if base_sigs.is_empty() && signatures.iter().all(|s| s.parameters().is_empty()) {
                    return Ok(vec![]);
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
                            |a: Id<Type>, b: Id<Type>| {
                                self.type_checker.compare_types_identical(a, b)
                            },
                        )? == Ternary::False
                        {
                            failed = true;
                            break;
                        }
                    }
                    if !failed {
                        return Ok(vec![]);
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
                return Ok(vec![set_text_range_rc_node(
                    get_factory().create_constructor_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Some(get_factory().create_modifiers_from_modifier_flags(private_protected)),
                        Some(vec![]),
                        None,
                    ),
                    signatures[0].declaration.as_deref(),
                )]);
            }
        }

        let mut results: Vec<Gc<Node>> = Default::default();
        for sig in &signatures {
            let decl = self
                .node_builder
                .signature_to_signature_declaration_helper(
                    sig.clone(),
                    output_kind,
                    &self.context(),
                    Option::<SignatureToSignatureDeclarationOptions<fn(Id<Symbol>)>>::None,
                )?;
            results.push(set_text_range_rc_node(decl, sig.declaration.as_deref()));
        }
        Ok(results)
    }

    pub(super) fn serialize_index_signatures(
        &self,
        input: Id<Type>,
        base_type: Option<Id<Type>>,
    ) -> io::Result<Vec<Gc<Node>>> {
        let mut results: Vec<Gc<Node /*IndexSignatureDeclaration*/>> = Default::default();
        for info in &self.type_checker.get_index_infos_of_type(input)? {
            if let Some(base_type) = base_type {
                let base_info = self
                    .type_checker
                    .get_index_info_of_type_(base_type, info.key_type)?;
                if let Some(base_info) = base_info.as_ref() {
                    if self
                        .type_checker
                        .is_type_identical_to(info.type_, base_info.type_)?
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
                    )?,
            );
        }
        Ok(results)
    }

    pub(super) fn serialize_base_type(
        &self,
        t: Id<Type>,
        static_type: Id<Type>,
        root_name: &str,
    ) -> io::Result<Gc<Node>> {
        let ref_ = self.try_serialize_as_type_reference(t, SymbolFlags::Value)?;
        if let Some(ref_) = ref_ {
            return Ok(ref_);
        }
        let temp_name =
            self.get_unused_name(&format!("{root_name}_base"), Option::<Id<Symbol>>::None);
        let statement = get_factory().create_variable_statement(
            Option::<Gc<NodeArray>>::None,
            get_factory().create_variable_declaration_list(
                vec![get_factory().create_variable_declaration(
                    Some(&*temp_name),
                    None,
                    self.node_builder
                        .type_to_type_node_helper(Some(static_type), &self.context())?,
                    None,
                )],
                Some(NodeFlags::Const),
            ),
        );
        self.add_result(&statement, ModifierFlags::None);
        Ok(get_factory().create_expression_with_type_arguments(
            get_factory().create_identifier(&temp_name),
            Option::<Gc<NodeArray>>::None,
        ))
    }

    pub(super) fn try_serialize_as_type_reference(
        &self,
        t: Id<Type>,
        flags: SymbolFlags,
    ) -> io::Result<Option<Gc<Node>>> {
        let mut type_args: Option<Vec<Gc<Node /*TypeNode*/>>> = Default::default();
        let mut reference: Option<Gc<Node /*Expression*/>> = Default::default();

        if let Some(t_target) = self
            .type_checker
            .type_(t)
            .maybe_as_type_reference_interface()
            .map(|t| t.target())
            .try_filter(|&t_target| {
                self.type_checker.is_symbol_accessible_by_flags(
                    self.type_checker.type_(t_target).symbol(),
                    Some(&*self.enclosing_declaration),
                    flags,
                )
            })?
        {
            type_args = Some(
                self.type_checker
                    .get_type_arguments(t)?
                    .iter()
                    .map(|&t| -> io::Result<_> {
                        Ok(self
                            .node_builder
                            .type_to_type_node_helper(Some(t), &self.context())?
                            .unwrap())
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            );
            reference = Some(self.node_builder.symbol_to_expression_(
                self.type_checker.type_(t_target).symbol(),
                &self.context(),
                Some(SymbolFlags::Type),
            )?);
        } else if let Some(t_symbol) =
            self.type_checker
                .type_(t)
                .maybe_symbol()
                .try_filter(|&t_symbol| {
                    self.type_checker.is_symbol_accessible_by_flags(
                        t_symbol,
                        Some(&*self.enclosing_declaration),
                        flags,
                    )
                })?
        {
            reference = Some(self.node_builder.symbol_to_expression_(
                t_symbol,
                &self.context(),
                Some(SymbolFlags::Type),
            )?);
        }
        Ok(reference.map(|reference| {
            get_factory().create_expression_with_type_arguments(reference, type_args)
        }))
    }

    pub(super) fn serialize_implemented_type(&self, t: Id<Type>) -> io::Result<Option<Gc<Node>>> {
        let ref_ = self.try_serialize_as_type_reference(t, SymbolFlags::Type)?;
        if ref_.is_some() {
            return Ok(ref_);
        }
        self.type_checker
            .type_(t)
            .maybe_symbol()
            .try_map(|t_symbol| -> io::Result<_> {
                Ok(get_factory().create_expression_with_type_arguments(
                    self.node_builder.symbol_to_expression_(
                        t_symbol,
                        &self.context(),
                        Some(SymbolFlags::Type),
                    )?,
                    Option::<Gc<NodeArray>>::None,
                ))
            })
    }

    pub(super) fn get_unused_name(&self, input: &str, symbol: Option<Id<Symbol>>) -> String {
        let id = symbol.map(|symbol| get_symbol_id(&self.type_checker.symbol(symbol)));
        if let Some(id) = id {
            if self.context().remapped_symbol_names().contains_key(&id) {
                return self
                    .context()
                    .remapped_symbol_names()
                    .get(&id)
                    .unwrap()
                    .clone();
            }
        }
        let mut input = input.to_owned();
        if let Some(symbol) = symbol {
            input = self.get_name_candidate_worker(symbol, &input);
        }
        let mut i = 0;
        let original = input.clone();
        while self
            .context()
            .maybe_used_symbol_names()
            .as_ref()
            .matches(|context_used_symbol_names| context_used_symbol_names.contains(&input))
        {
            i += 1;
            input = format!("{original}_{i}");
        }
        if let Some(context_used_symbol_names) =
            self.context().maybe_used_symbol_names_mut().as_mut()
        {
            context_used_symbol_names.insert(input.clone());
        }
        if let Some(id) = id {
            self.context()
                .remapped_symbol_names_mut()
                .insert(id, input.clone());
        }
        input
    }

    pub(super) fn get_name_candidate_worker(&self, symbol: Id<Symbol>, local_name: &str) -> String {
        let mut local_name = local_name.to_owned();
        if matches!(
            &*local_name,
            InternalSymbolName::Default | InternalSymbolName::Class | InternalSymbolName::Function
        ) {
            let flags = self.context().flags();
            self.context()
                .set_flags(self.context().flags() | NodeBuilderFlags::InInitialEntityName);
            let name_candidate = self
                .type_checker
                .get_name_of_symbol_as_written(symbol, Some(&self.context()));
            self.context().set_flags(flags);
            local_name = if !name_candidate.is_empty()
                && is_single_or_double_quote(name_candidate.chars().next().unwrap())
            {
                strip_quotes(&name_candidate).into_owned()
            } else {
                name_candidate.into_owned()
            };
        }
        if local_name == InternalSymbolName::Default {
            local_name = "_default".to_owned();
        } else if local_name == InternalSymbolName::ExportEquals {
            local_name = "_exports".to_owned();
        }
        local_name =
            if is_identifier_text(&local_name, Some(self.type_checker.language_version), None)
                && !is_string_a_non_contextual_keyword(&local_name)
            {
                local_name
            } else {
                format!("_{}", {
                    lazy_static! {
                        static ref non_alphanumeric_regex: Regex =
                            Regex::new("[^a-zA-Z0-9]").unwrap();
                    }
                    non_alphanumeric_regex.replace_all(&local_name, "_")
                })
            };
        local_name
    }

    pub(super) fn get_internal_symbol_name(&self, symbol: Id<Symbol>, local_name: &str) -> String {
        let id = get_symbol_id(&self.type_checker.symbol(symbol));
        if self.context().remapped_symbol_names().contains_key(&id) {
            return self
                .context()
                .remapped_symbol_names()
                .get(&id)
                .unwrap()
                .clone();
        }
        let local_name = self.get_name_candidate_worker(symbol, local_name);
        self.context()
            .remapped_symbol_names_mut()
            .insert(id, local_name.clone());
        local_name
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
        p: Id<Symbol>,
        is_static: bool,
        base_type: Option<Id<Type>>,
    ) -> io::Result<Vec<Gc<Node>>> {
        let modifier_flags = get_declaration_modifier_flags_from_symbol(
            self.type_checker.arena(),
            &self.type_checker.symbol(p),
            None,
        );
        let is_private = modifier_flags.intersects(ModifierFlags::Private);
        if is_static
            && self
                .type_checker
                .symbol(p)
                .flags()
                .intersects(SymbolFlags::Type | SymbolFlags::Namespace | SymbolFlags::Alias)
        {
            return Ok(vec![]);
        }
        if self
            .type_checker
            .symbol(p)
            .flags()
            .intersects(SymbolFlags::Prototype)
            || matches!(
                base_type,
                Some(base_type) if self.type_checker.get_property_of_type_(
                    base_type,
                    self.type_checker.symbol(p).escaped_name(),
                    None
                )?.is_some() &&
                    self.type_checker.is_readonly_symbol(
                        self.type_checker.get_property_of_type_(base_type, self.type_checker.symbol(p).escaped_name(), None)?.unwrap()
                    )? == self.type_checker.is_readonly_symbol(p)? &&
                    self.type_checker.symbol(p).flags() & SymbolFlags::Optional ==
                        self.type_checker.symbol(self.type_checker.get_property_of_type_(base_type, self.type_checker.symbol(p).escaped_name(), None)?.unwrap()).flags() & SymbolFlags::Optional &&
                    self.type_checker.is_type_identical_to(
                        self.type_checker.get_type_of_symbol(p)?,
                        self.type_checker.get_type_of_property_of_type_(
                            base_type,
                            self.type_checker.symbol(p).escaped_name()
                        )?.unwrap()
                    )?
            )
        {
            return Ok(vec![]);
        }
        let flag = (modifier_flags & !ModifierFlags::Async)
            | (if is_static {
                ModifierFlags::Static
            } else {
                ModifierFlags::None
            });
        let ref name = self
            .node_builder
            .get_property_name_node_for_symbol(p, &self.context)?;
        let first_property_like_decl = self
            .type_checker
            .symbol(p)
            .maybe_declarations()
            .as_ref()
            .and_then(|p_declarations| {
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
        if self
            .type_checker
            .symbol(p)
            .flags()
            .intersects(SymbolFlags::Accessor)
            && self.use_accessors
        {
            let mut result: Vec<Gc<Node /*AccessorDeclaration*/>> = Default::default();
            if self
                .type_checker
                .symbol(p)
                .flags()
                .intersects(SymbolFlags::SetAccessor)
            {
                result.push(set_text_range_rc_node(
                    get_factory().create_set_accessor_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Some(get_factory().create_modifiers_from_modifier_flags(flag)),
                        name.clone(),
                        vec![get_factory().create_parameter_declaration(
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
                                        self.type_checker.get_type_of_symbol(p)?,
                                        p,
                                        Some(
                                            &*self
                                                .symbol_table_to_declaration_statements
                                                .enclosing_declaration,
                                        ),
                                        Some(&|symbol: Id<Symbol>| {
                                            self.symbol_table_to_declaration_statements
                                                .include_private_symbol(symbol);
                                        }),
                                        self.symbol_table_to_declaration_statements.bundled,
                                    )?,
                                )
                            },
                            None,
                        )],
                        None,
                    ),
                    self.type_checker
                        .symbol(p)
                        .maybe_declarations()
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
            if self
                .type_checker
                .symbol(p)
                .flags()
                .intersects(SymbolFlags::GetAccessor)
            {
                let is_private = modifier_flags.intersects(ModifierFlags::Private);
                result.push(set_text_range_rc_node(
                    get_factory().create_get_accessor_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Some(get_factory().create_modifiers_from_modifier_flags(flag)),
                        name.clone(),
                        vec![],
                        if is_private {
                            None
                        } else {
                            Some(
                                self.node_builder.serialize_type_for_declaration(
                                    &self.context,
                                    self.type_checker.get_type_of_symbol(p)?,
                                    p,
                                    Some(
                                        &*self
                                            .symbol_table_to_declaration_statements
                                            .enclosing_declaration,
                                    ),
                                    Some(&|symbol: Id<Symbol>| {
                                        self.symbol_table_to_declaration_statements
                                            .include_private_symbol(symbol);
                                    }),
                                    self.symbol_table_to_declaration_statements.bundled,
                                )?,
                            )
                        },
                        None,
                    ),
                    self.type_checker
                        .symbol(p)
                        .maybe_declarations()
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
            return Ok(result);
        } else if self
            .type_checker
            .symbol(p)
            .flags()
            .intersects(SymbolFlags::Property | SymbolFlags::Variable | SymbolFlags::Accessor)
        {
            return Ok(vec![set_text_range_rc_node(
                self.create_property.call(
                    None,
                    Some(
                        get_factory()
                            .create_modifiers_from_modifier_flags(
                                if self.type_checker.is_readonly_symbol(p)? {
                                    ModifierFlags::Readonly
                                } else {
                                    ModifierFlags::None
                                } | flag,
                            )
                            .into(),
                    ),
                    name.clone().into(),
                    self.type_checker
                        .symbol(p)
                        .flags()
                        .intersects(SymbolFlags::Optional)
                        .then(|| get_factory().create_token(SyntaxKind::QuestionToken)),
                    if !is_private {
                        Some(
                            self.node_builder.serialize_type_for_declaration(
                                &self.context,
                                self.type_checker.get_type_of_symbol(p)?,
                                p,
                                Some(
                                    &*self
                                        .symbol_table_to_declaration_statements
                                        .enclosing_declaration,
                                ),
                                Some(&|symbol: Id<Symbol>| {
                                    self.symbol_table_to_declaration_statements
                                        .include_private_symbol(symbol);
                                }),
                                self.symbol_table_to_declaration_statements.bundled,
                            )?,
                        )
                    } else {
                        None
                    },
                    None,
                ),
                self.type_checker
                    .symbol(p)
                    .maybe_declarations()
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
            )]);
        }
        if self
            .type_checker
            .symbol(p)
            .flags()
            .intersects(SymbolFlags::Method | SymbolFlags::Function)
        {
            let type_ = self.type_checker.get_type_of_symbol(p)?;
            let signatures = self
                .type_checker
                .get_signatures_of_type(type_, SignatureKind::Call)?;
            if flag.intersects(ModifierFlags::Private) {
                return Ok(vec![set_text_range_rc_node(
                    self.create_property.call(
                        None,
                        Some(
                            get_factory()
                                .create_modifiers_from_modifier_flags(
                                    if self.type_checker.is_readonly_symbol(p)? {
                                        ModifierFlags::Readonly
                                    } else {
                                        ModifierFlags::None
                                    } | flag,
                                )
                                .into(),
                        ),
                        name.clone().into(),
                        self.type_checker
                            .symbol(p)
                            .flags()
                            .intersects(SymbolFlags::Optional)
                            .then(|| get_factory().create_token(SyntaxKind::QuestionToken)),
                        None,
                        None,
                    ),
                    self.type_checker
                        .symbol(p)
                        .maybe_declarations()
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
                                    self.type_checker
                                        .symbol(p)
                                        .maybe_declarations()
                                        .as_ref()
                                        .and_then(|p_declarations| p_declarations.get(0).cloned())
                                })
                        })
                        .as_deref(),
                )]);
            }

            let mut results: Vec<Gc<Node>> = Default::default();
            for ref sig in signatures {
                let decl = self
                    .node_builder
                    .signature_to_signature_declaration_helper(
                        sig.clone(),
                        self.method_kind,
                        &self.context,
                        Some(SignatureToSignatureDeclarationOptions {
                            name: Some(name.clone()),
                            question_token: self
                                .type_checker
                                .symbol(p)
                                .flags()
                                .intersects(SymbolFlags::Optional)
                                .then(|| get_factory().create_token(SyntaxKind::QuestionToken)),
                            modifiers: (flag != ModifierFlags::None)
                                .then(|| get_factory().create_modifiers_from_modifier_flags(flag)),
                            private_symbol_visitor: Option::<fn(Id<Symbol>)>::None,
                            bundled_imports: None,
                        }),
                    )?;
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
            return Ok(results);
        }
        Debug_.fail(Some(&format!(
            "Unhandled class member kind! {:?}",
            /*(p as any).__debugFlags ||*/ self.type_checker.symbol(p).flags()
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
