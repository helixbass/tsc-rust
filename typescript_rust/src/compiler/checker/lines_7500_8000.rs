use std::{borrow::Borrow, io};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;
use regex::Regex;

use super::{
    NodeBuilderContext, SignatureToSignatureDeclarationOptions, SymbolTableToDeclarationStatements,
};
use crate::{
    get_declaration_modifier_flags_from_symbol,
    get_export_assignment_expression, get_factory, get_object_flags,
    get_property_assignment_alias_like_expression, get_selected_effective_modifier_flags,
    get_source_file_of_node, get_symbol_id, id_text, is_accessor, is_binary_expression,
    is_class_expression, is_entity_name_expression, is_export_assignment,
    is_function_like_declaration, is_get_accessor, is_identifier_text,
    is_property_access_expression, is_property_declaration, is_property_signature,
    is_prototype_property_assignment, is_set_accessor, is_single_or_double_quote,
    is_string_a_non_contextual_keyword, is_variable_declaration, length,
    maybe_get_source_file_of_node, set_text_range_id_node, some, strip_quotes, symbol_name,
    unescape_leading_underscores, AllArenas, BoolExt, Debug_, HasArena, InArena,
    InternalSymbolName, IteratorExt, Matches, ModifierFlags, Node, NodeArray, NodeArrayOrVec,
    NodeBuilder, NodeBuilderFlags, NodeFlags, NodeInterface, ObjectFlags, OptionTry,
    SignatureKind, StrOrRcNode, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Ternary, Type,
    TypeChecker, TypeInterface,
    OptionInArena,
};

impl SymbolTableToDeclarationStatements {
    pub(super) fn serialize_export_specifier(
        &self,
        local_name: &str,
        target_name: &str,
        specifier: Option<Id<Node /*Expression*/>>,
    ) {
        self.add_result(
            get_factory(self).create_export_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                false,
                Some(get_factory(self).create_named_exports(vec![
                    get_factory(self).create_export_specifier(
                        false,
                        (local_name != target_name).then_some(target_name),
                        local_name,
                    ),
                ])),
                specifier,
                None,
            ),
            ModifierFlags::None,
        );
    }

    pub(super) fn serialize_maybe_alias_assignment(&self, symbol: Id<Symbol>) -> io::Result<bool> {
        if symbol.ref_(self).flags().intersects(SymbolFlags::Prototype) {
            return Ok(false);
        }
        let symbol_ref = symbol.ref_(self);
        let name = unescape_leading_underscores(symbol_ref.escaped_name());
        let is_export_equals = name == InternalSymbolName::ExportEquals;
        let is_default = name == InternalSymbolName::Default;
        let is_export_assignment_compatible_symbol_name = is_export_equals || is_default;
        let alias_decl = symbol
            .ref_(self)
            .maybe_declarations()
            .is_some()
            .try_then_and(|| self.type_checker.ref_(self).get_declaration_of_alias_symbol(symbol))?;
        let target = alias_decl.try_and_then(|alias_decl| {
            self.type_checker
                .ref_(self).get_target_of_alias_declaration(alias_decl, Some(true))
        })?;
        Ok(
            if let Some(target) = target.filter(|&target| {
                length(target.ref_(self).maybe_declarations().as_deref()) > 0
                    && some(
                        target.ref_(self).maybe_declarations().as_deref(),
                        Some(|&d: &Id<Node>| {
                            get_source_file_of_node(d, self) ==
                            get_source_file_of_node(self.enclosing_declaration, self)
                        }),
                    )
            }) {
                let expr = alias_decl.map(|alias_decl| {
                    if is_export_assignment(&alias_decl.ref_(self)) || is_binary_expression(&alias_decl.ref_(self)) {
                        get_export_assignment_expression(&alias_decl.ref_(self))
                    } else {
                        get_property_assignment_alias_like_expression(alias_decl, self)
                    }
                });
                let first = expr
                    .filter(|&expr| is_entity_name_expression(expr, self))
                    .map(|expr| {
                        self.type_checker
                            .ref_(self).get_first_non_module_exports_identifier(expr)
                    });
                let referenced = first.try_and_then(|first| {
                    self.type_checker.ref_(self).resolve_entity_name(
                        first,
                        SymbolFlags::All,
                        Some(true),
                        Some(true),
                        Some(self.enclosing_declaration),
                    )
                })?;
                // if (referenced || target) {
                self.include_private_symbol(referenced.unwrap_or(target));
                // }
                self.context().ref_(self).tracker_ref().disable_track_symbol();
                if is_export_assignment_compatible_symbol_name {
                    self.results_mut()
                        .push(get_factory(self).create_export_assignment(
                            Option::<Id<NodeArray>>::None,
                            Option::<Id<NodeArray>>::None,
                            Some(is_export_equals),
                            self.node_builder.ref_(self).symbol_to_expression_(
                                target,
                                &self.context().ref_(self),
                                Some(SymbolFlags::All),
                            )?,
                        ));
                } else {
                    if let Some(first) = first.filter(|&first| {
                        expr == Some(first)
                    }) {
                        self.serialize_export_specifier(
                            name,
                            id_text(&first.ref_(self)),
                            None,
                        );
                    } else if expr.matches(|expr| is_class_expression(&expr.ref_(self))) {
                        self.serialize_export_specifier(
                            name,
                            &self
                                .get_internal_symbol_name(target, &symbol_name(target, self)),
                            Option::<Id<Node>>::None,
                        );
                    } else {
                        let var_name = self.get_unused_name(name, Some(symbol));
                        self.add_result(
                            get_factory(self).create_import_equals_declaration(
                                Option::<Id<NodeArray>>::None,
                                Option::<Id<NodeArray>>::None,
                                false,
                                get_factory(self).create_identifier(&var_name),
                                self.node_builder.ref_(self).symbol_to_name(
                                    target,
                                    &self.context().ref_(self),
                                    Some(SymbolFlags::All),
                                    false,
                                )?,
                            ),
                            ModifierFlags::None,
                        );
                        self.serialize_export_specifier(name, &var_name, Option::<Id<Node>>::None);
                    }
                }
                self.context().ref_(self).tracker_ref().reenable_track_symbol();
                true
            } else {
                let var_name = self.get_unused_name(name, Some(symbol));
                let type_to_serialize =
                    self.type_checker
                        .ref_(self).get_widened_type(self.type_checker.ref_(self).get_type_of_symbol(
                            self.type_checker.ref_(self).get_merged_symbol(Some(symbol)).unwrap(),
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
                    let statement = get_factory(self).create_variable_statement(
                        Option::<Id<NodeArray>>::None,
                        get_factory(self).create_variable_declaration_list(
                            vec![get_factory(self).create_variable_declaration(
                                Some(&*var_name),
                                None,
                                Some(self.node_builder.ref_(self).serialize_type_for_declaration(
                                    &self.context().ref_(self),
                                    type_to_serialize,
                                    symbol,
                                    Some(self.enclosing_declaration),
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
                        statement,
                        if matches!(
                            target,
                            Some(target) if target.ref_(self).flags().intersects(SymbolFlags::Property) &&
                                target.ref_(self).escaped_name() == InternalSymbolName::ExportEquals
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
                        .push(get_factory(self).create_export_assignment(
                            Option::<Id<NodeArray>>::None,
                            Option::<Id<NodeArray>>::None,
                            Some(is_export_equals),
                            get_factory(self).create_identifier(&var_name),
                        ));
                    return Ok(true);
                } else if name != var_name {
                    self.serialize_export_specifier(name, &var_name, Option::<Id<Node>>::None);
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
        let ctx_src = maybe_get_source_file_of_node(self.context().ref_(self).maybe_enclosing_declaration(), self);
        Ok(get_object_flags(&type_to_serialize.ref_(self))
            .intersects(ObjectFlags::Anonymous | ObjectFlags::Mapped)
            && self
                .type_checker
                .ref_(self).get_index_infos_of_type(type_to_serialize)?
                .is_empty()
            && !self
                .type_checker
                .ref_(self).is_class_instance_side(type_to_serialize)?
            && (!self
                .type_checker
                .ref_(self).get_properties_of_type(type_to_serialize)?
                .into_iter()
                .filter(|&property| self.is_namespace_member(property))
                .empty()
                || !self
                    .type_checker
                    .ref_(self).get_signatures_of_type(type_to_serialize, SignatureKind::Call)?
                    .is_empty())
            && self
                .type_checker
                .ref_(self).get_signatures_of_type(type_to_serialize, SignatureKind::Construct)?
                .is_empty()
            && self
                .node_builder
                .ref_(self).get_declaration_with_type_annotation(
                    host_symbol,
                    Some(self.enclosing_declaration),
                )
                .is_none()
            && !matches!(
                type_to_serialize.ref_(self).maybe_symbol(),
                Some(type_to_serialize_symbol) if some(
                    type_to_serialize_symbol.ref_(self).maybe_declarations().as_deref(),
                    Some(|&d: &Id<Node>| 
                        maybe_get_source_file_of_node(Some(d), self) !=
                        ctx_src
                    )
                )
            )
            && !self
                .type_checker
                .ref_(self).get_properties_of_type(type_to_serialize)?
                .into_iter()
                .any(|p| {
                    self.type_checker
                        .ref_(self).is_late_bound_name(p.ref_(self).escaped_name())
                })
            && !self
                .type_checker
                .ref_(self).get_properties_of_type(type_to_serialize)?
                .into_iter()
                .any(|p| {
                    some(
                        p.ref_(self).maybe_declarations().as_deref(),
                        Some(|&d: &Id<Node>| {
                            maybe_get_source_file_of_node(Some(d), self) !=
                            ctx_src
                        }),
                    )
                })
            && self
                .type_checker
                .ref_(self).get_properties_of_type(type_to_serialize)?
                .into_iter()
                .all(|p| {
                    is_identifier_text(
                        &symbol_name(p, self),
                        Some(self.type_checker.ref_(self).language_version),
                        None,
                    )
                }))
    }

    pub(super) fn make_serialize_property_symbol(
        &self,
        create_property: Id<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
        method_kind: SyntaxKind,
        use_accessors: bool,
    ) -> MakeSerializePropertySymbol {
        MakeSerializePropertySymbol::new(
            self.type_checker.clone(),
            self.node_builder.clone(),
            self.context(),
            self.arena_id(),
            create_property,
            method_kind,
            use_accessors,
        )
    }

    pub(super) fn serialize_property_symbol_for_interface(
        &self,
        p: Id<Symbol>,
        base_type: Option<Id<Type>>,
    ) -> io::Result<Vec<Id<Node>>> {
        self.serialize_property_symbol_for_interface_worker()
            .call(p, false, base_type)
    }

    pub(super) fn serialize_signatures(
        &self,
        kind: SignatureKind,
        input: Id<Type>,
        base_type: Option<Id<Type>>,
        output_kind: SyntaxKind,
    ) -> io::Result<Vec<Id<Node>>> {
        let signatures = self.type_checker.ref_(self).get_signatures_of_type(input, kind)?;
        if kind == SignatureKind::Construct {
            if base_type.is_none() && signatures.iter().all(|s| s.ref_(self).parameters().is_empty()) {
                return Ok(vec![]);
            }
            if let Some(base_type) = base_type {
                let base_sigs = self
                    .type_checker
                    .ref_(self).get_signatures_of_type(base_type, SignatureKind::Construct)?;
                if base_sigs.is_empty() && signatures.iter().all(|s| s.ref_(self).parameters().is_empty()) {
                    return Ok(vec![]);
                }
                if base_sigs.len() == signatures.len() {
                    let mut failed = false;
                    for i in 0..base_sigs.len() {
                        if self.type_checker.ref_(self).compare_signatures_identical(
                            signatures[i].clone(),
                            base_sigs[i].clone(),
                            false,
                            false,
                            true,
                            |a: Id<Type>, b: Id<Type>| {
                                self.type_checker.ref_(self).compare_types_identical(a, b)
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
                if let Some(s_declaration) = s.ref_(self).declaration {
                    private_protected |= get_selected_effective_modifier_flags(
                        s_declaration,
                        ModifierFlags::Private | ModifierFlags::Protected,
                        self,
                    );
                }
            }
            if private_protected != ModifierFlags::None {
                return Ok(vec![set_text_range_id_node(
                    get_factory(self).create_constructor_declaration(
                        Option::<Id<NodeArray>>::None,
                        Some(get_factory(self).create_modifiers_from_modifier_flags(private_protected)),
                        Some(vec![]),
                        None,
                    ),
                    signatures[0].ref_(self).declaration.refed(self).as_deref(),
                    self,
                )]);
            }
        }

        let mut results: Vec<Id<Node>> = Default::default();
        for sig in &signatures {
            let decl = self
                .node_builder
                .ref_(self).signature_to_signature_declaration_helper(
                    sig.clone(),
                    output_kind,
                    &self.context().ref_(self),
                    Option::<SignatureToSignatureDeclarationOptions<fn(Id<Symbol>)>>::None,
                )?;
            results.push(set_text_range_id_node(decl, sig.ref_(self).declaration.refed(self).as_deref(), self));
        }
        Ok(results)
    }

    pub(super) fn serialize_index_signatures(
        &self,
        input: Id<Type>,
        base_type: Option<Id<Type>>,
    ) -> io::Result<Vec<Id<Node>>> {
        let mut results: Vec<Id<Node /*IndexSignatureDeclaration*/>> = Default::default();
        for &info in &self.type_checker.ref_(self).get_index_infos_of_type(input)? {
            if let Some(base_type) = base_type {
                let base_info = self
                    .type_checker
                    .ref_(self).get_index_info_of_type_(base_type, info.ref_(self).key_type)?;
                if let Some(base_info) = base_info {
                    if self
                        .type_checker
                        .ref_(self).is_type_identical_to(info.ref_(self).type_, base_info.ref_(self).type_)?
                    {
                        continue;
                    }
                }
            }
            results.push(
                self.node_builder
                    .ref_(self).index_info_to_index_signature_declaration_helper(
                        info,
                        &self.context().ref_(self),
                        Option::<Id<Node>>::None,
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
    ) -> io::Result<Id<Node>> {
        let ref_ = self.try_serialize_as_type_reference(t, SymbolFlags::Value)?;
        if let Some(ref_) = ref_ {
            return Ok(ref_);
        }
        let temp_name =
            self.get_unused_name(&format!("{root_name}_base"), Option::<Id<Symbol>>::None);
        let statement = get_factory(self).create_variable_statement(
            Option::<Id<NodeArray>>::None,
            get_factory(self).create_variable_declaration_list(
                vec![get_factory(self).create_variable_declaration(
                    Some(&*temp_name),
                    None,
                    self.node_builder
                        .ref_(self).type_to_type_node_helper(Some(static_type), &self.context().ref_(self))?,
                    None,
                )],
                Some(NodeFlags::Const),
            ),
        );
        self.add_result(statement, ModifierFlags::None);
        Ok(get_factory(self).create_expression_with_type_arguments(
            get_factory(self).create_identifier(&temp_name),
            Option::<Id<NodeArray>>::None,
        ))
    }

    pub(super) fn try_serialize_as_type_reference(
        &self,
        t: Id<Type>,
        flags: SymbolFlags,
    ) -> io::Result<Option<Id<Node>>> {
        let mut type_args: Option<Vec<Id<Node /*TypeNode*/>>> = Default::default();
        let mut reference: Option<Id<Node /*Expression*/>> = Default::default();

        if let Some(t_target) = t
            .ref_(self)
            .maybe_as_type_reference_interface()
            .map(|t| t.target())
            .try_filter(|&t_target| {
                self.type_checker.ref_(self).is_symbol_accessible_by_flags(
                    t_target.ref_(self).symbol(),
                    Some(self.enclosing_declaration),
                    flags,
                )
            })?
        {
            type_args = Some(
                self.type_checker
                    .ref_(self).get_type_arguments(t)?
                    .iter()
                    .map(|&t| -> io::Result<_> {
                        Ok(self
                            .node_builder
                            .ref_(self).type_to_type_node_helper(Some(t), &self.context().ref_(self))?
                            .unwrap())
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            );
            reference = Some(self.node_builder.ref_(self).symbol_to_expression_(
                t_target.ref_(self).symbol(),
                &self.context().ref_(self),
                Some(SymbolFlags::Type),
            )?);
        } else if let Some(t_symbol) = t.ref_(self).maybe_symbol().try_filter(|&t_symbol| {
            self.type_checker.ref_(self).is_symbol_accessible_by_flags(
                t_symbol,
                Some(self.enclosing_declaration),
                flags,
            )
        })? {
            reference = Some(self.node_builder.ref_(self).symbol_to_expression_(
                t_symbol,
                &self.context().ref_(self),
                Some(SymbolFlags::Type),
            )?);
        }
        Ok(reference.map(|reference| {
            get_factory(self).create_expression_with_type_arguments(reference, type_args)
        }))
    }

    pub(super) fn serialize_implemented_type(&self, t: Id<Type>) -> io::Result<Option<Id<Node>>> {
        let ref_ = self.try_serialize_as_type_reference(t, SymbolFlags::Type)?;
        if ref_.is_some() {
            return Ok(ref_);
        }
        t.ref_(self)
            .maybe_symbol()
            .try_map(|t_symbol| -> io::Result<_> {
                Ok(get_factory(self).create_expression_with_type_arguments(
                    self.node_builder.ref_(self).symbol_to_expression_(
                        t_symbol,
                        &self.context().ref_(self),
                        Some(SymbolFlags::Type),
                    )?,
                    Option::<Id<NodeArray>>::None,
                ))
            })
    }

    pub(super) fn get_unused_name(&self, input: &str, symbol: Option<Id<Symbol>>) -> String {
        let id = symbol.map(|symbol| get_symbol_id(&symbol.ref_(self)));
        if let Some(id) = id {
            if self.context().ref_(self).remapped_symbol_names().contains_key(&id) {
                return self
                    .context()
                    .ref_(self).remapped_symbol_names()
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
            .ref_(self).maybe_used_symbol_names()
            .as_ref()
            .matches(|context_used_symbol_names| context_used_symbol_names.contains(&input))
        {
            i += 1;
            input = format!("{original}_{i}");
        }
        if let Some(context_used_symbol_names) =
            self.context().ref_(self).maybe_used_symbol_names_mut().as_mut()
        {
            context_used_symbol_names.insert(input.clone());
        }
        if let Some(id) = id {
            self.context()
                .ref_(self).remapped_symbol_names_mut()
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
            let flags = self.context().ref_(self).flags();
            self.context()
                .ref_(self).set_flags(self.context().ref_(self).flags() | NodeBuilderFlags::InInitialEntityName);
            let name_candidate = self
                .type_checker
                .ref_(self).get_name_of_symbol_as_written(symbol, Some(&self.context().ref_(self)));
            self.context().ref_(self).set_flags(flags);
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
            if is_identifier_text(&local_name, Some(self.type_checker.ref_(self).language_version), None)
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
        let id = get_symbol_id(&symbol.ref_(self));
        if self.context().ref_(self).remapped_symbol_names().contains_key(&id) {
            return self
                .context()
                .ref_(self).remapped_symbol_names()
                .get(&id)
                .unwrap()
                .clone();
        }
        let local_name = self.get_name_candidate_worker(symbol, local_name);
        self.context()
            .ref_(self).remapped_symbol_names_mut()
            .insert(id, local_name.clone());
        local_name
    }
}

#[derive(Copy, Clone, Trace, Finalize)]
pub(super) struct MakeSerializePropertySymbol {
    type_checker: Id<TypeChecker>,
    node_builder: Id<NodeBuilder>,
    context: Id<NodeBuilderContext>,
    symbol_table_to_declaration_statements: Id<SymbolTableToDeclarationStatements>,
    create_property: Id<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
    method_kind: SyntaxKind,
    use_accessors: bool,
}

impl HasArena for MakeSerializePropertySymbol {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

impl MakeSerializePropertySymbol {
    pub(super) fn new(
        type_checker: Id<TypeChecker>,
        node_builder: Id<NodeBuilder>,
        context: Id<NodeBuilderContext>,
        symbol_table_to_declaration_statements: Id<SymbolTableToDeclarationStatements>,
        create_property: Id<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
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
    ) -> io::Result<Vec<Id<Node>>> {
        let modifier_flags = get_declaration_modifier_flags_from_symbol(
            p,
            None,
            self,
        );
        let is_private = modifier_flags.intersects(ModifierFlags::Private);
        if is_static
            && p.ref_(self)
                .flags()
                .intersects(SymbolFlags::Type | SymbolFlags::Namespace | SymbolFlags::Alias)
        {
            return Ok(vec![]);
        }
        if p.ref_(self).flags().intersects(SymbolFlags::Prototype)
            || matches!(
                base_type,
                Some(base_type) if self.type_checker.ref_(self).get_property_of_type_(
                    base_type,
                    p.ref_(self).escaped_name(),
                    None
                )?.is_some() &&
                    self.type_checker.ref_(self).is_readonly_symbol(
                        self.type_checker.ref_(self).get_property_of_type_(base_type, p.ref_(self).escaped_name(), None)?.unwrap()
                    )? == self.type_checker.ref_(self).is_readonly_symbol(p)? &&
                    p.ref_(self).flags() & SymbolFlags::Optional ==
                        self.type_checker.ref_(self).get_property_of_type_(base_type, p.ref_(self).escaped_name(), None)?.unwrap().ref_(self).flags() & SymbolFlags::Optional &&
                    self.type_checker.ref_(self).is_type_identical_to(
                        self.type_checker.ref_(self).get_type_of_symbol(p)?,
                        self.type_checker.ref_(self).get_type_of_property_of_type_(
                            base_type,
                            p.ref_(self).escaped_name()
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
            .ref_(self).get_property_name_node_for_symbol(p, &self.context.ref_(self))?;
        let first_property_like_decl =
            p.ref_(self)
                .maybe_declarations()
                .as_ref()
                .and_then(|p_declarations| {
                    p_declarations
                        .iter()
                        .find(|declaration| {
                            is_property_declaration(&declaration.ref_(self))
                                || is_accessor(&declaration.ref_(self))
                                || is_variable_declaration(&declaration.ref_(self))
                                || is_property_signature(&declaration.ref_(self))
                                || is_binary_expression(&declaration.ref_(self))
                                || is_property_access_expression(&declaration.ref_(self))
                        })
                        .copied()
                });
        if p.ref_(self)
            .flags()
            .intersects(SymbolFlags::Accessor)
            && self.use_accessors
        {
            let mut result: Vec<Id<Node /*AccessorDeclaration*/>> = Default::default();
            if p.ref_(self).flags().intersects(SymbolFlags::SetAccessor) {
                result.push(set_text_range_id_node(
                    get_factory(self).create_set_accessor_declaration(
                        Option::<Id<NodeArray>>::None,
                        Some(get_factory(self).create_modifiers_from_modifier_flags(flag)),
                        name.clone(),
                        vec![get_factory(self).create_parameter_declaration(
                            Option::<Id<NodeArray>>::None,
                            Option::<Id<NodeArray>>::None,
                            None,
                            Some("arg"),
                            None,
                            if is_private {
                                None
                            } else {
                                Some(
                                    self.node_builder.ref_(self).serialize_type_for_declaration(
                                        &self.context.ref_(self),
                                        self.type_checker.ref_(self).get_type_of_symbol(p)?,
                                        p,
                                        Some(
                                            self
                                                .symbol_table_to_declaration_statements
                                                .ref_(self).enclosing_declaration,
                                        ),
                                        Some(&|symbol: Id<Symbol>| {
                                            self.symbol_table_to_declaration_statements
                                                .ref_(self).include_private_symbol(symbol);
                                        }),
                                        self.symbol_table_to_declaration_statements.ref_(self).bundled,
                                    )?,
                                )
                            },
                            None,
                        )],
                        None,
                    ),
                    p.ref_(self)
                        .maybe_declarations()
                        .as_ref()
                        .and_then(|p_declarations| {
                            p_declarations
                                .iter()
                                .find(|declaration| is_set_accessor(&declaration.ref_(self)))
                                .copied()
                                .or(first_property_like_decl)
                        })
                        .refed(self).as_deref(),
                    self,
                ));
            }
            if p.ref_(self).flags().intersects(SymbolFlags::GetAccessor) {
                let is_private = modifier_flags.intersects(ModifierFlags::Private);
                result.push(set_text_range_id_node(
                    get_factory(self).create_get_accessor_declaration(
                        Option::<Id<NodeArray>>::None,
                        Some(get_factory(self).create_modifiers_from_modifier_flags(flag)),
                        name.clone(),
                        vec![],
                        if is_private {
                            None
                        } else {
                            Some(
                                self.node_builder.ref_(self).serialize_type_for_declaration(
                                    &self.context.ref_(self),
                                    self.type_checker.ref_(self).get_type_of_symbol(p)?,
                                    p,
                                    Some(
                                        self
                                            .symbol_table_to_declaration_statements
                                            .ref_(self).enclosing_declaration,
                                    ),
                                    Some(&|symbol: Id<Symbol>| {
                                        self.symbol_table_to_declaration_statements
                                            .ref_(self).include_private_symbol(symbol);
                                    }),
                                    self.symbol_table_to_declaration_statements.ref_(self).bundled,
                                )?,
                            )
                        },
                        None,
                    ),
                    p.ref_(self)
                        .maybe_declarations()
                        .as_ref()
                        .and_then(|p_declarations| {
                            p_declarations
                                .iter()
                                .find(|declaration| is_get_accessor(&declaration.ref_(self)))
                                .copied()
                                .or(first_property_like_decl)
                        })
                        .refed(self).as_deref(),
                    self,
                ));
            }
            return Ok(result);
        } else if p
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::Property | SymbolFlags::Variable | SymbolFlags::Accessor)
        {
            return Ok(vec![set_text_range_id_node(
                self.create_property.ref_(self).call(
                    None,
                    Some(
                        get_factory(self)
                            .create_modifiers_from_modifier_flags(
                                if self.type_checker.ref_(self).is_readonly_symbol(p)? {
                                    ModifierFlags::Readonly
                                } else {
                                    ModifierFlags::None
                                } | flag,
                            )
                            .into(),
                    ),
                    name.clone().into(),
                    p.ref_(self)
                        .flags()
                        .intersects(SymbolFlags::Optional)
                        .then(|| get_factory(self).create_token(SyntaxKind::QuestionToken)),
                    if !is_private {
                        Some(
                            self.node_builder.ref_(self).serialize_type_for_declaration(
                                &self.context.ref_(self),
                                self.type_checker.ref_(self).get_type_of_symbol(p)?,
                                p,
                                Some(
                                    self
                                        .symbol_table_to_declaration_statements
                                        .ref_(self).enclosing_declaration,
                                ),
                                Some(&|symbol: Id<Symbol>| {
                                    self.symbol_table_to_declaration_statements
                                        .ref_(self).include_private_symbol(symbol);
                                }),
                                self.symbol_table_to_declaration_statements.ref_(self).bundled,
                            )?,
                        )
                    } else {
                        None
                    },
                    None,
                ),
                p.ref_(self)
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|p_declarations| {
                        p_declarations
                            .iter()
                            .find(|declaration| {
                                is_property_declaration(&declaration.ref_(self))
                                    || is_variable_declaration(&declaration.ref_(self))
                            })
                            .copied()
                            .or(first_property_like_decl)
                    })
                    .refed(self).as_deref(),
                self,
            )]);
        }
        if p.ref_(self)
            .flags()
            .intersects(SymbolFlags::Method | SymbolFlags::Function)
        {
            let type_ = self.type_checker.ref_(self).get_type_of_symbol(p)?;
            let signatures = self
                .type_checker
                .ref_(self).get_signatures_of_type(type_, SignatureKind::Call)?;
            if flag.intersects(ModifierFlags::Private) {
                return Ok(vec![set_text_range_id_node(
                    self.create_property.ref_(self).call(
                        None,
                        Some(
                            get_factory(self)
                                .create_modifiers_from_modifier_flags(
                                    if self.type_checker.ref_(self).is_readonly_symbol(p)? {
                                        ModifierFlags::Readonly
                                    } else {
                                        ModifierFlags::None
                                    } | flag,
                                )
                                .into(),
                        ),
                        name.clone().into(),
                        p.ref_(self)
                            .flags()
                            .intersects(SymbolFlags::Optional)
                            .then(|| get_factory(self).create_token(SyntaxKind::QuestionToken)),
                        None,
                        None,
                    ),
                    p.ref_(self)
                        .maybe_declarations()
                        .as_ref()
                        .and_then(|p_declarations| {
                            p_declarations
                                .iter()
                                .find(|declaration| is_function_like_declaration(&declaration.ref_(self)))
                                .copied()
                                .or_else(|| {
                                    signatures
                                        .get(0)
                                        .and_then(|signatures_0| signatures_0.ref_(self).declaration)
                                })
                                .or_else(|| {
                                    p.ref_(self)
                                        .maybe_declarations()
                                        .as_ref()
                                        .and_then(|p_declarations| p_declarations.get(0).copied())
                                })
                        })
                        .refed(self).as_deref(),
                    self,
                )]);
            }

            let mut results: Vec<Id<Node>> = Default::default();
            for ref sig in signatures {
                let decl = self
                    .node_builder
                    .ref_(self).signature_to_signature_declaration_helper(
                        sig.clone(),
                        self.method_kind,
                        &self.context.ref_(self),
                        Some(SignatureToSignatureDeclarationOptions {
                            name: Some(name.clone()),
                            question_token: p
                                .ref_(self)
                                .flags()
                                .intersects(SymbolFlags::Optional)
                                .then(|| get_factory(self).create_token(SyntaxKind::QuestionToken)),
                            modifiers: (flag != ModifierFlags::None)
                                .then(|| get_factory(self).create_modifiers_from_modifier_flags(flag)),
                            private_symbol_visitor: Option::<fn(Id<Symbol>)>::None,
                            bundled_imports: None,
                        }),
                    )?;
                let location = sig
                    .ref_(self).declaration
                    .as_ref()
                    .filter(|sig_declaration| {
                        is_prototype_property_assignment(sig_declaration.ref_(self).parent(), self)
                    })
                    .map(|sig_declaration| sig_declaration.ref_(self).parent())
                    .or_else(|| sig.ref_(self).declaration);
                results.push(set_text_range_id_node(decl, location.refed(self).as_deref(), self));
            }
            return Ok(results);
        }
        Debug_.fail(Some(&format!(
            "Unhandled class member kind! {:?}",
            /*(p as any).__debugFlags ||*/ p.ref_(self).flags()
        )))
    }
}

pub trait MakeSerializePropertySymbolCreateProperty: Trace + Finalize {
    fn call(
        &self,
        decorators: Option<NodeArrayOrVec /*Decorator*/>,
        modifiers: Option<NodeArrayOrVec /*Modifier*/>,
        name: StrOrRcNode<'_>, /*PropertyName*/
        question_or_exclamation_token: Option<Id<Node /*QuestionToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node>;
}
