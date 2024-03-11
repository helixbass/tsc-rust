use std::{
    borrow::{Borrow, Cow},
    cmp,
    collections::HashSet,
    io,
    rc::Rc,
};

use id_arena::Id;

use super::{
    anon, CheckTypeRelatedTo, ErrorCalculationState, IntersectionState, RecursionFlags,
    ReportUnmeasurableMarkers, ReportUnreliableMarkers, SignatureCheckMode,
    TypeComparerIsRelatedToWorker, TypeFacts,
};
use crate::{
    cartesian_product, create_diagnostic_for_node, get_declaration_modifier_flags_from_symbol,
    get_symbol_name_for_private_identifier, is_named_declaration, is_private_identifier, length,
    reduce_left, some, CheckFlags, DiagnosticMessage, DiagnosticMessageChain, Diagnostics,
    ElementFlags, IndexInfo, ModifierFlags, Node, NodeInterface, ObjectFlags, Signature,
    SignatureFlags, SignatureKind, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Ternary, Type,
    TypeFlags, TypeFormatFlags, TypeInterface, VarianceFlags, __String, get_check_flags,
    get_factory, get_object_flags, push_if_unique_eq, released, return_ok_default_if_none,
    HasArena, InArena,
};

impl CheckTypeRelatedTo {
    pub(super) fn count_message_chain_breadth<TItem: Borrow<DiagnosticMessageChain>>(
        &self,
        info: Option<&[TItem]>,
    ) -> usize {
        if info.is_none() {
            return 0;
        }
        reduce_left(
            info.unwrap(),
            |value, chain: &TItem, _| {
                value + 1 + self.count_message_chain_breadth(chain.borrow().next.as_deref())
            },
            0,
            None,
            None,
        )
    }

    pub(super) fn relate_variances(
        self_: Id<Self>,
        arena: &impl HasArena,
        result: &mut Ternary,
        report_errors: bool,
        original_error_info: &mut Option<Rc<DiagnosticMessageChain>>,
        save_error_info: &ErrorCalculationState,
        variance_check_failed: &mut bool,
        source_type_arguments: Option<&[Id<Type>]>,
        target_type_arguments: Option<&[Id<Type>]>,
        variances: &[VarianceFlags],
        intersection_state: IntersectionState,
    ) -> io::Result<Option<Ternary>> {
        *result = Self::type_arguments_related_to(
            self_,
            arena,
            source_type_arguments.map(ToOwned::to_owned),
            target_type_arguments.map(ToOwned::to_owned),
            Some(variances.to_owned()),
            report_errors,
            intersection_state,
        )?;
        if *result != Ternary::False {
            return Ok(Some(*result));
        }
        if some(
            Some(variances),
            Some(|v: &VarianceFlags| v.intersects(VarianceFlags::AllowsStructuralFallback)),
        ) {
            *original_error_info = None;
            self_.ref_(arena).reset_error_info(save_error_info.clone());
            return Ok(None);
        }
        let allow_structural_fallback = matches!(
            target_type_arguments,
            Some(target_type_arguments) if self_.ref_(arena).type_checker.ref_(arena).has_covariant_void_argument(target_type_arguments, variances)
        );
        *variance_check_failed = !allow_structural_fallback;
        if !variances.is_empty() && !allow_structural_fallback {
            if *variance_check_failed
                && !(report_errors
                    && some(
                        Some(variances),
                        Some(|v: &VarianceFlags| {
                            *v & VarianceFlags::VarianceMask == VarianceFlags::Invariant
                        }),
                    ))
            {
                return Ok(Some(Ternary::False));
            }
            *original_error_info = self_.ref_(arena).maybe_error_info();
            self_.ref_(arena).reset_error_info(save_error_info.clone());
        }
        Ok(None)
    }

    pub(super) fn mapped_type_related_to(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>, /*MappedType*/
        target: Id<Type>, /*MappedType*/
        report_errors: bool,
    ) -> io::Result<Ternary> {
        let modifiers_related = Rc::ptr_eq(
            &self_.ref_(arena).relation,
            &self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .comparable_relation,
        ) || if Rc::ptr_eq(
            &self_.ref_(arena).relation,
            &self_.ref_(arena).type_checker.ref_(arena).identity_relation,
        ) {
            self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_mapped_type_modifiers(source)
                == self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_mapped_type_modifiers(target)
        } else {
            self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_combined_mapped_type_optionality(source)?
                <= self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_combined_mapped_type_optionality(target)?
        };
        if modifiers_related {
            let result: Ternary;
            let target_constraint = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_constraint_type_from_mapped_type(target)?;
            let source_constraint = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .instantiate_type(
                    self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_constraint_type_from_mapped_type(source)?,
                    Some(
                        if self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_combined_mapped_type_optionality(source)?
                            < 0
                        {
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .make_function_type_mapper(ReportUnmeasurableMarkers)
                        } else {
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .make_function_type_mapper(ReportUnreliableMarkers)
                        },
                    ),
                )?;
            result = Self::is_related_to(
                self_,
                arena,
                target_constraint,
                source_constraint,
                Some(RecursionFlags::Both),
                Some(report_errors),
                None,
                None,
            )?;
            if result != Ternary::False {
                let mapper = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .create_type_mapper(
                        vec![self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_type_parameter_from_mapped_type(source)?],
                        Some(vec![self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_type_parameter_from_mapped_type(target)?]),
                    );
                if self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .maybe_instantiate_type(
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_name_type_from_mapped_type(source)?,
                        Some(mapper.clone()),
                    )?
                    == self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .maybe_instantiate_type(
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .get_name_type_from_mapped_type(target)?,
                            Some(mapper.clone()),
                        )?
                {
                    return Ok(result
                        & Self::is_related_to(
                            self_,
                            arena,
                            released!(self_.ref_(arena).type_checker)
                                .ref_(arena)
                                .instantiate_type(
                                    released!(self_.ref_(arena).type_checker)
                                        .ref_(arena)
                                        .get_template_type_from_mapped_type(source)?,
                                    Some(mapper),
                                )?,
                            released!(self_.ref_(arena).type_checker)
                                .ref_(arena)
                                .get_template_type_from_mapped_type(target)?,
                            Some(RecursionFlags::Both),
                            Some(report_errors),
                            None,
                            None,
                        )?);
                }
            }
        }
        Ok(Ternary::False)
    }

    pub(super) fn type_related_to_discriminated_type(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>, /*UnionType*/
    ) -> io::Result<Ternary> {
        let source_properties = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_properties_of_type(source)?;
        let source_properties_filtered = released!(self_.ref_(arena).type_checker)
            .ref_(arena)
            .find_discriminant_properties(&source_properties, target)?;
        if source_properties_filtered.is_none() {
            return Ok(Ternary::False);
        }
        let source_properties_filtered = source_properties_filtered.unwrap();

        let mut num_combinations = 1;
        for &source_property in &source_properties_filtered {
            num_combinations *= self_.ref_(arena).type_checker.ref_(arena).count_types(
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_non_missing_type_of_symbol(source_property)?,
            );
            if num_combinations > 25 {
                // tracing?.instant(tracing.Phase.CheckTypes, "typeRelatedToDiscriminatedType_DepthLimit", { sourceId: source.id, targetId: target.id, numCombinations });
                return Ok(Ternary::False);
            }
        }

        let mut source_discriminant_types: Vec<Vec<Id<Type>>> =
            Vec::with_capacity(source_properties_filtered.len());
        let mut excluded_properties: HashSet<__String> = HashSet::new();
        for &source_property in source_properties_filtered.iter() {
            let source_property_type = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_non_missing_type_of_symbol(source_property)?;
            source_discriminant_types.push(
                if source_property_type
                    .ref_(arena)
                    .flags()
                    .intersects(TypeFlags::Union)
                {
                    source_property_type
                        .ref_(arena)
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned()
                } else {
                    vec![source_property_type]
                },
            );
            excluded_properties.insert(source_property.ref_(arena).escaped_name().to_owned());
        }

        let discriminant_combinations = cartesian_product(&source_discriminant_types);
        let mut matching_types: Vec<Id<Type>> = vec![];
        for combination in &discriminant_combinations {
            let mut has_match = false;
            'outer: for &type_ in target
                .ref_(arena)
                .as_union_or_intersection_type_interface()
                .types()
            {
                for (i, &source_property) in source_properties_filtered.iter().enumerate() {
                    let target_property = self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_property_of_type_(
                            type_,
                            source_property.ref_(arena).escaped_name(),
                            None,
                        )?;
                    if target_property.is_none() {
                        continue 'outer;
                    }
                    let target_property = target_property.unwrap();
                    if source_property == target_property {
                        continue;
                    }
                    let related = Self::property_related_to(
                        self_,
                        arena,
                        source,
                        target,
                        source_property,
                        target_property,
                        |_| Ok(combination[i].clone()),
                        false,
                        IntersectionState::None,
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .strict_null_checks
                            || Rc::ptr_eq(
                                &self_.ref_(arena).relation,
                                &self_
                                    .ref_(arena)
                                    .type_checker
                                    .ref_(arena)
                                    .comparable_relation,
                            ),
                    )?;

                    if related == Ternary::False {
                        continue 'outer;
                    }
                }
                push_if_unique_eq(&mut matching_types, &type_);
                has_match = true;
            }
            if !has_match {
                return Ok(Ternary::False);
            }
        }

        let mut result = Ternary::True;
        for &type_ in &matching_types {
            result &= Self::properties_related_to(
                self_,
                arena,
                source,
                type_,
                false,
                Some(&excluded_properties),
                IntersectionState::None,
            )?;
            if result != Ternary::False {
                result &= Self::signatures_related_to(
                    self_,
                    arena,
                    source,
                    type_,
                    SignatureKind::Call,
                    false,
                )?;
                if result != Ternary::False {
                    result &= Self::signatures_related_to(
                        self_,
                        arena,
                        source,
                        type_,
                        SignatureKind::Construct,
                        false,
                    )?;
                    if result != Ternary::False
                        && !(self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .is_tuple_type(source)
                            && self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .is_tuple_type(type_))
                    {
                        result &= Self::index_signatures_related_to(
                            self_,
                            arena,
                            source,
                            type_,
                            false,
                            false,
                            IntersectionState::None,
                        )?;
                    }
                }
            }
            if result == Ternary::False {
                return Ok(result);
            }
        }
        Ok(result)
    }

    pub(super) fn exclude_properties(
        &self,
        properties: &[Id<Symbol>],
        excluded_properties: Option<&HashSet<__String>>,
    ) -> Vec<Id<Symbol>> {
        if excluded_properties.is_none() || properties.is_empty() {
            return properties.to_owned();
        }
        let excluded_properties = excluded_properties.unwrap();
        let mut result: Option<Vec<Id<Symbol>>> = None;
        for i in 0..properties.len() {
            if !excluded_properties.contains(properties[i].ref_(self).escaped_name()) {
                if let Some(result) = result.as_mut() {
                    result.push(properties[i].clone());
                }
            } else if result.is_none() {
                result = Some(properties[0..i].to_owned());
            }
        }
        result.unwrap_or_else(|| properties.to_owned())
    }

    pub(super) fn is_property_symbol_type_related(
        self_: Id<Self>,
        arena: &impl HasArena,
        source_prop: Id<Symbol>,
        target_prop: Id<Symbol>,
        mut get_type_of_source_property: impl FnMut(Id<Symbol>) -> io::Result<Id<Type>>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        let target_is_optional = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .strict_null_checks
            && get_check_flags(&target_prop.ref_(arena)).intersects(CheckFlags::Partial);
        let effective_target = released!(self_.ref_(arena).type_checker)
            .ref_(arena)
            .add_optionality(
                released!(self_.ref_(arena).type_checker)
                    .ref_(arena)
                    .get_non_missing_type_of_symbol(target_prop)?,
                Some(false),
                Some(target_is_optional),
            )?;
        let effective_source = get_type_of_source_property(source_prop)?;
        Self::is_related_to(
            self_,
            arena,
            effective_source,
            effective_target,
            Some(RecursionFlags::Both),
            Some(report_errors),
            None,
            Some(intersection_state),
        )
    }

    pub(super) fn property_related_to(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
        source_prop: Id<Symbol>,
        target_prop: Id<Symbol>,
        get_type_of_source_property: impl FnMut(Id<Symbol>) -> io::Result<Id<Type>>,
        report_errors: bool,
        intersection_state: IntersectionState,
        skip_optional: bool,
    ) -> io::Result<Ternary> {
        let source_prop_flags =
            get_declaration_modifier_flags_from_symbol(source_prop, None, arena);
        let target_prop_flags =
            get_declaration_modifier_flags_from_symbol(target_prop, None, arena);
        if source_prop_flags.intersects(ModifierFlags::Private)
            || target_prop_flags.intersects(ModifierFlags::Private)
        {
            if source_prop.ref_(arena).maybe_value_declaration()
                != target_prop.ref_(arena).maybe_value_declaration()
            {
                if report_errors {
                    if source_prop_flags.intersects(ModifierFlags::Private)
                        && target_prop_flags.intersects(ModifierFlags::Private)
                    {
                        Self::report_error(
                            self_,
                            arena,
                            Cow::Borrowed(&Diagnostics::Types_have_separate_declarations_of_a_private_property_0),
                            Some(vec![
                                self_.ref_(arena).type_checker.ref_(arena).symbol_to_string_(
                                    target_prop,
                                    Option::<Id<Node>>::None,
                                    None, None, None
                                )?
                            ])
                        )?;
                    } else {
                        Self::report_error(
                            self_,
                            arena,
                            Cow::Borrowed(
                                &Diagnostics::Property_0_is_private_in_type_1_but_not_in_type_2,
                            ),
                            Some(vec![
                                self_
                                    .ref_(arena)
                                    .type_checker
                                    .ref_(arena)
                                    .symbol_to_string_(
                                        target_prop,
                                        Option::<Id<Node>>::None,
                                        None,
                                        None,
                                        None,
                                    )?,
                                self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                                    if source_prop_flags.intersects(ModifierFlags::Private) {
                                        source
                                    } else {
                                        target
                                    },
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                )?,
                                self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                                    if source_prop_flags.intersects(ModifierFlags::Private) {
                                        target
                                    } else {
                                        source
                                    },
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                )?,
                            ]),
                        )?;
                    }
                }
                return Ok(Ternary::False);
            }
        } else if target_prop_flags.intersects(ModifierFlags::Protected) {
            if !self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_valid_override_of(source_prop, target_prop)?
            {
                if report_errors {
                    Self::report_error(
                        self_,
                        arena,
                        Cow::Borrowed(&Diagnostics::Property_0_is_protected_but_type_1_is_not_a_class_derived_from_2),
                        Some(vec![
                            self_.ref_(arena).type_checker.ref_(arena).symbol_to_string_(
                                target_prop,
                                Option::<Id<Node>>::None,
                                None, None, None
                            )?,
                            self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                                self_.ref_(arena).type_checker.ref_(arena).get_declaring_class(source_prop)?.unwrap_or_else(|| source),
                                Option::<Id<Node>>::None,
                                None, None,
                            )?,
                            self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                                self_.ref_(arena).type_checker.ref_(arena).get_declaring_class(target_prop)?.unwrap_or_else(|| target),
                                Option::<Id<Node>>::None,
                                None, None,
                            )?,
                        ])
                    )?;
                }
                return Ok(Ternary::False);
            }
        } else if source_prop_flags.intersects(ModifierFlags::Protected) {
            if report_errors {
                Self::report_error(
                    self_,
                    arena,
                    Cow::Borrowed(
                        &Diagnostics::Property_0_is_protected_in_type_1_but_public_in_type_2,
                    ),
                    Some(vec![
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .symbol_to_string_(
                                target_prop,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                                None,
                            )?,
                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                            source,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                            target,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                    ]),
                )?;
            }
            return Ok(Ternary::False);
        }
        let related = Self::is_property_symbol_type_related(
            self_,
            arena,
            source_prop,
            target_prop,
            get_type_of_source_property,
            report_errors,
            intersection_state,
        )?;
        if related == Ternary::False {
            if report_errors {
                self_.ref_(arena).report_incompatible_error(
                    &Diagnostics::Types_of_property_0_are_incompatible,
                    Some(vec![self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .symbol_to_string_(
                            target_prop,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                            None,
                        )?]),
                );
            }
            return Ok(Ternary::False);
        }
        if !skip_optional
            && source_prop
                .ref_(arena)
                .flags()
                .intersects(SymbolFlags::Optional)
            && !target_prop
                .ref_(arena)
                .flags()
                .intersects(SymbolFlags::Optional)
        {
            if report_errors {
                Self::report_error(
                    self_,
                    arena,
                    Cow::Borrowed(
                        &Diagnostics::Property_0_is_optional_in_type_1_but_required_in_type_2,
                    ),
                    Some(vec![
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .symbol_to_string_(
                                target_prop,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                                None,
                            )?,
                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                            source,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                            target,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                    ]),
                )?;
            }
            return Ok(Ternary::False);
        }
        Ok(related)
    }

    pub(super) fn report_unmatched_property(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
        unmatched_property: Id<Symbol>,
        require_optional_properties: bool,
    ) -> io::Result<()> {
        let mut should_skip_elaboration = false;
        if let Some(unmatched_property_value_declaration) = unmatched_property
            .ref_(arena)
            .maybe_value_declaration()
            .filter(|unmatched_property_value_declaration| {
                is_named_declaration(&unmatched_property_value_declaration.ref_(arena))
                    && is_private_identifier(
                        &unmatched_property_value_declaration
                            .ref_(arena)
                            .as_named_declaration()
                            .name()
                            .ref_(arena),
                    )
            })
        {
            if let Some(source_symbol) =
                source.ref_(arena).maybe_symbol().filter(|&source_symbol| {
                    source_symbol
                        .ref_(arena)
                        .flags()
                        .intersects(SymbolFlags::Class)
                })
            {
                let unmatched_property_value_declaration_name =
                    unmatched_property_value_declaration
                        .ref_(arena)
                        .as_named_declaration()
                        .name();
                let ref private_identifier_description = unmatched_property_value_declaration_name
                    .ref_(arena)
                    .as_private_identifier()
                    .escaped_text
                    .clone();
                let symbol_table_key = get_symbol_name_for_private_identifier(
                    &source_symbol.ref_(arena),
                    private_identifier_description,
                );
                if
                /*symbolTableKey &&*/
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_property_of_type_(source, &symbol_table_key, None)?
                    .is_some()
                {
                    let source_name = get_factory(arena).get_declaration_name(
                        source_symbol.ref_(arena).maybe_value_declaration(),
                        None,
                        None,
                    );
                    let target_name = get_factory(arena).get_declaration_name(
                        target
                            .ref_(arena)
                            .symbol()
                            .ref_(arena)
                            .maybe_value_declaration(),
                        None,
                        None,
                    );
                    Self::report_error(
                        self_,
                        arena,
                        Cow::Borrowed(&Diagnostics::Property_0_in_type_1_refers_to_a_different_member_that_cannot_be_accessed_from_within_type_2),
                        Some(vec![
                            self_.ref_(arena).type_checker.ref_(arena).diagnostic_name((&**private_identifier_description).into()).into_owned(),
                            self_.ref_(arena).type_checker.ref_(arena).diagnostic_name(if source_name.ref_(arena).as_identifier().escaped_text == "" {
                                anon.into()
                            } else {
                                source_name.into()
                            }).into_owned(),
                            self_.ref_(arena).type_checker.ref_(arena).diagnostic_name(if target_name.ref_(arena).as_identifier().escaped_text == "" {
                                anon.into()
                            } else {
                                target_name.into()
                            }).into_owned(),
                        ])
                    )?;
                    return Ok(());
                }
            }
        }
        let props = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_unmatched_properties(source, target, require_optional_properties, false)?;
        if match self_.ref_(arena).head_message.as_ref() {
            None => true,
            Some(head_message) => head_message.code != Diagnostics::Class_0_incorrectly_implements_interface_1.code &&
                head_message.code != Diagnostics::Class_0_incorrectly_implements_class_1_Did_you_mean_to_extend_1_and_inherit_its_members_as_a_subclass.code
        } {
            should_skip_elaboration = true;
        }
        if props.len() == 1 {
            let prop_name = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .symbol_to_string_(
                    unmatched_property,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                    None,
                )?;
            let (source_string, target_string) = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_type_names_for_error_display(source, target)?;
            Self::report_error(
                self_,
                arena,
                Cow::Borrowed(&Diagnostics::Property_0_is_missing_in_type_1_but_required_in_type_2),
                Some(vec![prop_name.clone(), source_string, target_string]),
            )?;
            if length(
                unmatched_property
                    .ref_(arena)
                    .maybe_declarations()
                    .as_deref(),
            ) > 0
            {
                self_.ref_(arena).associate_related_info(
                    create_diagnostic_for_node(
                        unmatched_property
                            .ref_(arena)
                            .maybe_declarations()
                            .as_ref()
                            .unwrap()[0],
                        &Diagnostics::_0_is_declared_here,
                        Some(vec![prop_name]),
                        arena,
                    )
                    .into(),
                );
            }
            if should_skip_elaboration && self_.ref_(arena).maybe_error_info().is_some() {
                self_
                    .ref_(arena)
                    .set_override_next_error_info(self_.ref_(arena).override_next_error_info() + 1);
            }
        } else if Self::try_elaborate_array_like_errors(self_, arena, source, target, false)? {
            if props.len() > 5 {
                Self::report_error(
                    self_,
                    arena,
                    Cow::Borrowed(&Diagnostics::Type_0_is_missing_the_following_properties_from_type_1_Colon_2_and_3_more),
                    Some(vec![
                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                            source,
                            Option::<Id<Node>>::None,
                            None, None,
                        )?,
                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                            target,
                            Option::<Id<Node>>::None,
                            None, None,
                        )?,
                        (&props[0..4]).into_iter().map(|&p: &Id<Symbol>|
                            self_.ref_(arena).type_checker.ref_(arena).symbol_to_string_(
                                p,
                                Option::<Id<Node>>::None,
                                None, None, None,
                            )
                        ).collect::<Result<Vec<_>, _>>()?.join(", "),
                        (props.len() - 4).to_string(),
                    ])
                )?
            } else {
                Self::report_error(
                    self_,
                    arena,
                    Cow::Borrowed(&Diagnostics::Type_0_is_missing_the_following_properties_from_type_1_Colon_2),
                    Some(vec![
                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                            source,
                            Option::<Id<Node>>::None,
                            None, None,
                        )?,
                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                            target,
                            Option::<Id<Node>>::None,
                            None, None,
                        )?,
                        props.iter().map(|&p: &Id<Symbol>|
                            self_.ref_(arena).type_checker.ref_(arena).symbol_to_string_(
                                p,
                                Option::<Id<Node>>::None,
                                None, None, None,
                            )
                        ).collect::<Result<Vec<_>, _>>()?.join(", "),
                    ])
                )?
            }
            if should_skip_elaboration && self_.ref_(arena).maybe_error_info().is_some() {
                self_
                    .ref_(arena)
                    .set_override_next_error_info(self_.ref_(arena).override_next_error_info() + 1);
            }
        }

        Ok(())
    }

    pub(super) fn properties_related_to(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
        report_errors: bool,
        excluded_properties: Option<&HashSet<__String>>,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        if Rc::ptr_eq(
            &self_.ref_(arena).relation,
            &self_.ref_(arena).type_checker.ref_(arena).identity_relation,
        ) {
            return Self::properties_identical_to(
                self_,
                arena,
                source,
                target,
                excluded_properties,
            );
        }
        let mut result = Ternary::True;
        if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_tuple_type(target)
        {
            let target_target = target.ref_(arena).as_type_reference_interface().target();
            if self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_array_type(source)
                || self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_tuple_type(source)
            {
                if !target_target.ref_(arena).as_tuple_type().readonly
                    && (self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_readonly_array_type(source)
                        || self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .is_tuple_type(source)
                            && source
                                .ref_(arena)
                                .as_type_reference_interface()
                                .target()
                                .ref_(arena)
                                .as_tuple_type()
                                .readonly)
                {
                    return Ok(Ternary::False);
                }
                let source_arity = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_type_reference_arity(source);
                let target_arity = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_type_reference_arity(target);
                let source_rest_flag = if self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_tuple_type(source)
                {
                    source
                        .ref_(arena)
                        .as_type_reference_interface()
                        .target()
                        .ref_(arena)
                        .as_tuple_type()
                        .combined_flags
                        & ElementFlags::Rest
                } else {
                    ElementFlags::Rest
                };
                let target_rest_flag =
                    target_target.ref_(arena).as_tuple_type().combined_flags & ElementFlags::Rest;
                let source_min_length = if self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_tuple_type(source)
                {
                    source
                        .ref_(arena)
                        .as_type_reference_interface()
                        .target()
                        .ref_(arena)
                        .as_tuple_type()
                        .min_length
                } else {
                    0
                };
                let target_min_length = target_target.ref_(arena).as_tuple_type().min_length;
                if source_rest_flag == ElementFlags::None && source_arity < target_min_length {
                    if report_errors {
                        Self::report_error(
                            self_,
                            arena,
                            Cow::Borrowed(
                                &Diagnostics::Source_has_0_element_s_but_target_requires_1,
                            ),
                            Some(vec![
                                source_arity.to_string(),
                                target_min_length.to_string(),
                            ]),
                        )?;
                    }
                    return Ok(Ternary::False);
                }
                if target_rest_flag == ElementFlags::None && target_arity < source_min_length {
                    if report_errors {
                        Self::report_error(
                            self_,
                            arena,
                            Cow::Borrowed(
                                &Diagnostics::Source_has_0_element_s_but_target_allows_only_1,
                            ),
                            Some(vec![
                                source_min_length.to_string(),
                                target_arity.to_string(),
                            ]),
                        )?;
                    }
                    return Ok(Ternary::False);
                }
                if target_rest_flag == ElementFlags::None
                    && (source_rest_flag != ElementFlags::None || target_arity < source_arity)
                {
                    if report_errors {
                        if source_min_length < target_min_length {
                            Self::report_error(
                                self_,
                                arena,
                                Cow::Borrowed(&Diagnostics::Target_requires_0_element_s_but_source_may_have_fewer),
                                Some(vec![
                                    target_min_length.to_string(),
                                ])
                            )?;
                        } else {
                            Self::report_error(
                                self_,
                                arena,
                                Cow::Borrowed(&Diagnostics::Target_allows_only_0_element_s_but_source_may_have_more),
                                Some(vec![
                                    target_arity.to_string(),
                                ])
                            )?;
                        }
                    }
                    return Ok(Ternary::False);
                }
                let source_type_arguments = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_type_arguments(source)?;
                let target_type_arguments = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_type_arguments(target)?;
                let start_count = cmp::min(
                    if self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_tuple_type(source)
                    {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_start_element_count(
                                source.ref_(arena).as_type_reference_interface().target(),
                                ElementFlags::NonRest,
                            )
                    } else {
                        0
                    },
                    self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_start_element_count(target_target, ElementFlags::NonRest),
                );
                let end_count = cmp::min(
                    if self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_tuple_type(source)
                    {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_end_element_count(
                                source.ref_(arena).as_type_reference_interface().target(),
                                ElementFlags::NonRest,
                            )
                    } else {
                        0
                    },
                    if target_rest_flag != ElementFlags::None {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_end_element_count(target_target, ElementFlags::NonRest)
                    } else {
                        0
                    },
                );
                let mut can_exclude_discriminants = excluded_properties.is_some();
                for i in 0..target_arity {
                    let source_index = if i < target_arity - end_count {
                        i
                    } else {
                        i + source_arity - target_arity
                    };
                    let source_flags = if self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_tuple_type(source)
                        && (i < start_count || i >= target_arity - end_count)
                    {
                        source
                            .ref_(arena)
                            .as_type_reference()
                            .target
                            .ref_(arena)
                            .as_tuple_type()
                            .element_flags[source_index]
                    } else {
                        ElementFlags::Rest
                    };
                    let target_flags = target_target.ref_(arena).as_tuple_type().element_flags[i];
                    if target_flags.intersects(ElementFlags::Variadic)
                        && !source_flags.intersects(ElementFlags::Variadic)
                    {
                        if report_errors {
                            Self::report_error(
                                self_,
                                arena,
                                Cow::Borrowed(&Diagnostics::Source_provides_no_match_for_variadic_element_at_position_0_in_target),
                                Some(vec![
                                    i.to_string(),
                                ])
                            )?;
                        }
                        return Ok(Ternary::False);
                    }
                    if source_flags.intersects(ElementFlags::Variadic)
                        && !target_flags.intersects(ElementFlags::Variable)
                    {
                        if report_errors {
                            Self::report_error(
                                self_,
                                arena,
                                Cow::Borrowed(&Diagnostics::Variadic_element_at_position_0_in_source_does_not_match_element_at_position_1_in_target),
                                Some(vec![
                                    source_index.to_string(),
                                    i.to_string(),
                                ])
                            )?;
                        }
                        return Ok(Ternary::False);
                    }
                    if target_flags.intersects(ElementFlags::Required)
                        && !source_flags.intersects(ElementFlags::Required)
                    {
                        if report_errors {
                            Self::report_error(
                                self_,
                                arena,
                                Cow::Borrowed(&Diagnostics::Source_provides_no_match_for_required_element_at_position_0_in_target),
                                Some(vec![
                                    i.to_string(),
                                ])
                            )?;
                        }
                        return Ok(Ternary::False);
                    }
                    if can_exclude_discriminants {
                        if source_flags.intersects(ElementFlags::Variable)
                            || target_flags.intersects(ElementFlags::Variable)
                        {
                            can_exclude_discriminants = false;
                        }
                        if can_exclude_discriminants
                            && matches!(
                                excluded_properties,
                                Some(excluded_properties) if excluded_properties.contains(&i.to_string())
                            )
                        {
                            continue;
                        }
                    }
                    let source_type = if !self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_tuple_type(source)
                    {
                        source_type_arguments[0].clone()
                    } else if i < start_count || i >= target_arity - end_count {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .remove_missing_type(
                                source_type_arguments[source_index],
                                (source_flags & target_flags).intersects(ElementFlags::Optional),
                            )
                    } else {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_element_type_of_slice_of_tuple_type(
                                source,
                                start_count,
                                Some(end_count),
                                None,
                            )?
                            .unwrap_or_else(|| {
                                self_.ref_(arena).type_checker.ref_(arena).never_type()
                            })
                    };
                    let target_type = target_type_arguments[i];
                    let target_check_type = if source_flags.intersects(ElementFlags::Variadic)
                        && target_flags.intersects(ElementFlags::Rest)
                    {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .create_array_type(target_type, None)
                    } else {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .remove_missing_type(
                                target_type,
                                target_flags.intersects(ElementFlags::Optional),
                            )
                    };
                    let related = Self::is_related_to(
                        self_,
                        arena,
                        source_type,
                        target_check_type,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        Some(intersection_state),
                    )?;
                    if related == Ternary::False {
                        if report_errors && (target_arity > 1 || source_arity > 1) {
                            if i < start_count
                                || i >= target_arity - end_count
                                || source_arity - start_count - end_count == 1
                            {
                                self_.ref_(arena).report_incompatible_error(
                                    &Diagnostics::Type_at_position_0_in_source_is_not_compatible_with_type_at_position_1_in_target,
                                    Some(vec![
                                        source_index.to_string(),
                                        i.to_string(),
                                    ])
                                );
                            } else {
                                self_.ref_(arena).report_incompatible_error(
                                    &Diagnostics::Type_at_positions_0_through_1_in_source_is_not_compatible_with_type_at_position_2_in_target,
                                    Some(vec![
                                        start_count.to_string(),
                                        (source_arity - end_count - 1).to_string(),
                                        i.to_string(),
                                    ])
                                );
                            }
                        }
                        return Ok(Ternary::False);
                    }
                    result &= related;
                }
                return Ok(result);
            }
            if target_target
                .ref_(arena)
                .as_tuple_type()
                .combined_flags
                .intersects(ElementFlags::Variable)
            {
                return Ok(Ternary::False);
            }
        }
        let require_optional_properties = (Rc::ptr_eq(
            &self_.ref_(arena).relation,
            &self_.ref_(arena).type_checker.ref_(arena).subtype_relation,
        ) || Rc::ptr_eq(
            &self_.ref_(arena).relation,
            &self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .strict_subtype_relation,
        )) && !self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_object_literal_type(source)
            && !self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_empty_array_literal_type(source)?
            && !self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_tuple_type(source);
        let unmatched_property = released!(self_.ref_(arena).type_checker)
            .ref_(arena)
            .get_unmatched_property(source, target, require_optional_properties, false)?;
        if let Some(unmatched_property) = unmatched_property {
            if report_errors {
                Self::report_unmatched_property(
                    self_,
                    arena,
                    source,
                    target,
                    unmatched_property,
                    require_optional_properties,
                )?;
            }
            return Ok(Ternary::False);
        }
        if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_object_literal_type(target)
        {
            for &source_prop in &self_.ref_(arena).exclude_properties(
                &self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_properties_of_type(source)?,
                excluded_properties,
            ) {
                if self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_property_of_object_type(target, source_prop.ref_(arena).escaped_name())?
                    .is_none()
                {
                    let source_type = self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_type_of_symbol(source_prop)?;
                    if !source_type
                        .ref_(arena)
                        .flags()
                        .intersects(TypeFlags::Undefined)
                    {
                        if report_errors {
                            Self::report_error(
                                self_,
                                arena,
                                Cow::Borrowed(&Diagnostics::Property_0_does_not_exist_on_type_1),
                                Some(vec![
                                    self_
                                        .ref_(arena)
                                        .type_checker
                                        .ref_(arena)
                                        .symbol_to_string_(
                                            source_prop,
                                            Option::<Id<Node>>::None,
                                            None,
                                            None,
                                            None,
                                        )?,
                                    self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                                        target,
                                        Option::<Id<Node>>::None,
                                        None,
                                        None,
                                    )?,
                                ]),
                            )?;
                        }
                        return Ok(Ternary::False);
                    }
                }
            }
        }
        let properties = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_properties_of_type(target)?;
        let numeric_names_only = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_tuple_type(source)
            && self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_tuple_type(target);
        for &target_prop in &released!(self_
            .ref_(arena)
            .exclude_properties(&properties, excluded_properties))
        {
            let ref name = target_prop.ref_(arena).escaped_name().to_owned();
            if !target_prop
                .ref_(arena)
                .flags()
                .intersects(SymbolFlags::Prototype)
                && (!numeric_names_only
                    || self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_numeric_literal_name(name)
                    || name == "length")
            {
                let source_prop = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_property_of_type_(source, name, None)?;
                if let Some(source_prop) = source_prop {
                    if source_prop != target_prop {
                        let related = Self::property_related_to(
                            self_,
                            arena,
                            source,
                            target,
                            source_prop,
                            target_prop,
                            |symbol| {
                                released!(self_.ref_(arena).type_checker)
                                    .ref_(arena)
                                    .get_non_missing_type_of_symbol(symbol)
                            },
                            report_errors,
                            intersection_state,
                            released!(Rc::ptr_eq(
                                &self_.ref_(arena).relation,
                                &self_
                                    .ref_(arena)
                                    .type_checker
                                    .ref_(arena)
                                    .comparable_relation,
                            )),
                        )?;
                        if related == Ternary::False {
                            return Ok(Ternary::False);
                        }
                        result &= related;
                    }
                }
            }
        }
        Ok(result)
    }

    pub(super) fn properties_identical_to(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
        excluded_properties: Option<&HashSet<__String>>,
    ) -> io::Result<Ternary> {
        if !(source.ref_(arena).flags().intersects(TypeFlags::Object)
            && target.ref_(arena).flags().intersects(TypeFlags::Object))
        {
            return Ok(Ternary::False);
        }
        let source_properties = self_.ref_(arena).exclude_properties(
            &self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_properties_of_object_type(source)?,
            excluded_properties,
        );
        let target_properties = self_.ref_(arena).exclude_properties(
            &self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_properties_of_object_type(target)?,
            excluded_properties,
        );
        if source_properties.len() != target_properties.len() {
            return Ok(Ternary::False);
        }
        let mut result = Ternary::True;
        for &source_prop in &source_properties {
            let target_prop = return_ok_default_if_none!(self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_property_of_object_type(target, source_prop.ref_(arena).escaped_name())?);
            let related = released!(self_.ref_(arena).type_checker)
                .ref_(arena)
                .compare_properties(source_prop, target_prop, |source, target| {
                    Self::is_related_to(self_, arena, source, target, None, None, None, None)
                })?;
            if related == Ternary::False {
                return Ok(Ternary::False);
            }
            result &= related;
        }
        Ok(result)
    }

    pub(super) fn signatures_related_to(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
        kind: SignatureKind,
        report_errors: bool,
    ) -> io::Result<Ternary> {
        if Rc::ptr_eq(
            &self_.ref_(arena).relation,
            &self_.ref_(arena).type_checker.ref_(arena).identity_relation,
        ) {
            return Self::signatures_identical_to(self_, arena, source, target, kind);
        }
        if target
            == self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .any_function_type()
            || source
                == self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .any_function_type()
        {
            return Ok(Ternary::True);
        }

        let source_is_js_constructor = matches!(
            source.ref_(arena).maybe_symbol(),
            Some(source_symbol) if self_.ref_(arena).type_checker.ref_(arena).is_js_constructor(source_symbol.ref_(arena).maybe_value_declaration())?
        );
        let target_is_js_constructor = matches!(
            target.ref_(arena).maybe_symbol(),
            Some(target_symbol) if self_.ref_(arena).type_checker.ref_(arena).is_js_constructor(target_symbol.ref_(arena).maybe_value_declaration())?
        );

        let source_signatures = released!(self_.ref_(arena).type_checker)
            .ref_(arena)
            .get_signatures_of_type(
                source,
                if source_is_js_constructor && kind == SignatureKind::Construct {
                    SignatureKind::Call
                } else {
                    kind
                },
            )?;
        let target_signatures = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_signatures_of_type(
                target,
                if target_is_js_constructor && kind == SignatureKind::Construct {
                    SignatureKind::Call
                } else {
                    kind
                },
            )?;

        if kind == SignatureKind::Construct
            && !source_signatures.is_empty()
            && !target_signatures.is_empty()
        {
            let source_is_abstract = source_signatures[0]
                .ref_(arena)
                .flags
                .intersects(SignatureFlags::Abstract);
            let target_is_abstract = target_signatures[0]
                .ref_(arena)
                .flags
                .intersects(SignatureFlags::Abstract);
            if source_is_abstract && !target_is_abstract {
                if report_errors {
                    Self::report_error(
                        self_,
                        arena,
                        Cow::Borrowed(&Diagnostics::Cannot_assign_an_abstract_constructor_type_to_a_non_abstract_constructor_type),
                        None,
                    )?;
                }
                return Ok(Ternary::False);
            }
            if !Self::constructor_visibilities_are_compatible(
                self_,
                arena,
                source_signatures[0],
                target_signatures[0],
                report_errors,
            )? {
                return Ok(Ternary::False);
            }
        }

        let mut result = Ternary::True;
        let save_error_info = self_.ref_(arena).capture_error_calculation_state();
        let incompatible_reporter: fn(
            &Self,
            Id<Signature>,
            Id<Signature>,
        ) -> fn(&Self, Id<Type>, Id<Type>) -> io::Result<()> = if kind == SignatureKind::Construct {
            Self::report_incompatible_construct_signature_return
        } else {
            Self::report_incompatible_call_signature_return
        };
        let source_object_flags = get_object_flags(&source.ref_(arena));
        let target_object_flags = get_object_flags(&target.ref_(arena));
        if source_object_flags.intersects(ObjectFlags::Instantiated)
            && target_object_flags.intersects(ObjectFlags::Instantiated)
            && source.ref_(arena).maybe_symbol() == target.ref_(arena).maybe_symbol()
        {
            for i in 0..target_signatures.len() {
                let related = Self::signature_related_to(
                    self_,
                    arena,
                    source_signatures[i].clone(),
                    target_signatures[i].clone(),
                    true,
                    report_errors,
                    released!(incompatible_reporter(
                        &self_.ref_(arena),
                        source_signatures[i],
                        target_signatures[i],
                    )),
                )?;
                if related == Ternary::False {
                    return Ok(Ternary::False);
                }
                result &= related;
            }
        } else if source_signatures.len() == 1 && target_signatures.len() == 1 {
            let erase_generics = Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .comparable_relation,
            ) || matches!(
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .compiler_options
                    .ref_(arena)
                    .no_strict_generic_checks,
                Some(true)
            );
            let source_signature = source_signatures[0];
            let target_signature = target_signatures[0];
            result = Self::signature_related_to(
                self_,
                arena,
                source_signature.clone(),
                target_signature.clone(),
                erase_generics,
                report_errors,
                released!(incompatible_reporter(
                    &self_.ref_(arena),
                    source_signature,
                    target_signature
                )),
            )?;
            if result == Ternary::False
                && report_errors
                && kind == SignatureKind::Construct
                && source_object_flags & target_object_flags != ObjectFlags::None
                && (matches!(
                    target_signature.ref_(arena).declaration,
                    Some(target_signature_declaration) if target_signature_declaration.ref_(arena).kind() == SyntaxKind::Constructor
                ) || matches!(
                    source_signature.ref_(arena).declaration,
                    Some(source_signature_declaration) if source_signature_declaration.ref_(arena).kind() == SyntaxKind::Constructor
                ))
            {
                let construct_signature_to_string =
                    |signature: Id<Signature>| -> io::Result<String> {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .signature_to_string_(
                                signature,
                                Option::<Id<Node>>::None,
                                Some(TypeFormatFlags::WriteArrowStyleSignature),
                                Some(kind),
                                None,
                            )
                    };
                Self::report_error(
                    self_,
                    arena,
                    Cow::Borrowed(&Diagnostics::Type_0_is_not_assignable_to_type_1),
                    Some(vec![
                        construct_signature_to_string(source_signature.clone())?,
                        construct_signature_to_string(target_signature.clone())?,
                    ]),
                )?;
                Self::report_error(
                    self_,
                    arena,
                    Cow::Borrowed(&Diagnostics::Types_of_construct_signatures_are_incompatible),
                    None,
                )?;
                return Ok(result);
            }
        } else {
            'outer: for &t in &target_signatures {
                let mut should_elaborate_errors = report_errors;
                for &s in &source_signatures {
                    let related = Self::signature_related_to(
                        self_,
                        arena,
                        s.clone(),
                        t.clone(),
                        true,
                        should_elaborate_errors,
                        released!(incompatible_reporter(&self_.ref_(arena), s, t)),
                    )?;
                    if related != Ternary::False {
                        result &= related;
                        self_.ref_(arena).reset_error_info(save_error_info.clone());
                        continue 'outer;
                    }
                    should_elaborate_errors = false;
                }

                if should_elaborate_errors {
                    Self::report_error(
                        self_,
                        arena,
                        Cow::Borrowed(&Diagnostics::Type_0_provides_no_match_for_the_signature_1),
                        Some(vec![
                            self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                                source,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                            )?,
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .signature_to_string_(
                                    t.clone(),
                                    Option::<Id<Node>>::None,
                                    None,
                                    Some(kind),
                                    None,
                                )?,
                        ]),
                    )?;
                }
                return Ok(Ternary::False);
            }
        }
        Ok(result)
    }

    pub(super) fn report_incompatible_call_signature_return(
        &self,
        siga: Id<Signature>,
        sigb: Id<Signature>,
    ) -> fn(&Self, Id<Type>, Id<Type>) -> io::Result<()> {
        if siga.ref_(self).parameters().is_empty() && sigb.ref_(self).parameters().is_empty() {
            Self::report_incompatible_call_signature_return_no_arguments
        } else {
            Self::report_incompatible_call_signature_return_some_arguments
        }
    }

    pub(super) fn report_incompatible_call_signature_return_no_arguments(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<()> {
        self.report_incompatible_error(
            &Diagnostics::Call_signatures_with_no_arguments_have_incompatible_return_types_0_and_1,
            Some(vec![
                self.type_checker.ref_(self).type_to_string_(
                    source,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?,
                self.type_checker.ref_(self).type_to_string_(
                    target,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?,
            ]),
        );

        Ok(())
    }

    pub(super) fn report_incompatible_call_signature_return_some_arguments(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<()> {
        self.report_incompatible_error(
            &Diagnostics::Call_signature_return_types_0_and_1_are_incompatible,
            Some(vec![
                self.type_checker.ref_(self).type_to_string_(
                    source,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?,
                self.type_checker.ref_(self).type_to_string_(
                    target,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?,
            ]),
        );

        Ok(())
    }

    pub(super) fn report_incompatible_construct_signature_return(
        &self,
        siga: Id<Signature>,
        sigb: Id<Signature>,
    ) -> fn(&Self, Id<Type>, Id<Type>) -> io::Result<()> {
        if siga.ref_(self).parameters().is_empty() && sigb.ref_(self).parameters().is_empty() {
            Self::report_incompatible_construct_signature_return_no_arguments
        } else {
            Self::report_incompatible_construct_signature_return_some_arguments
        }
    }

    pub(super) fn report_incompatible_construct_signature_return_no_arguments(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<()> {
        self.report_incompatible_error(
            &Diagnostics::Construct_signatures_with_no_arguments_have_incompatible_return_types_0_and_1,
            Some(vec![
                self.type_checker
                    .ref_(self).type_to_string_(source, Option::<Id<Node>>::None, None, None)?,
                self.type_checker
                    .ref_(self).type_to_string_(target, Option::<Id<Node>>::None, None, None)?,
            ]),
        );

        Ok(())
    }

    pub(super) fn report_incompatible_construct_signature_return_some_arguments(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<()> {
        self.report_incompatible_error(
            &Diagnostics::Construct_signature_return_types_0_and_1_are_incompatible,
            Some(vec![
                self.type_checker.ref_(self).type_to_string_(
                    source,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?,
                self.type_checker.ref_(self).type_to_string_(
                    target,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?,
            ]),
        );

        Ok(())
    }

    pub(super) fn signature_related_to(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Signature>,
        target: Id<Signature>,
        erase: bool,
        report_errors: bool,
        incompatible_reporter: fn(&Self, Id<Type>, Id<Type>) -> io::Result<()>,
    ) -> io::Result<Ternary> {
        released!(self_.ref_(arena).type_checker)
            .ref_(arena)
            .compare_signatures_related(
                if erase {
                    self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_erased_signature(source)?
                } else {
                    source
                },
                if erase {
                    self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_erased_signature(target)?
                } else {
                    target
                },
                if Rc::ptr_eq(
                    &self_.ref_(arena).relation,
                    &self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .strict_subtype_relation,
                ) {
                    SignatureCheckMode::StrictArity
                } else {
                    SignatureCheckMode::None
                },
                report_errors,
                &mut Some(&mut |message: Cow<'static, DiagnosticMessage>,
                                args: Option<Vec<String>>| {
                    Self::report_error(self_, arena, message, args)
                }),
                Some(&|source: Id<Type>, target: Id<Type>| {
                    incompatible_reporter(&self_.ref_(arena), source, target)
                }),
                released!(self_.ref_(arena).alloc_type_comparer(Box::new(
                    TypeComparerIsRelatedToWorker::new(self_, arena),
                ))),
                Some(
                    released!(self_.ref_(arena).type_checker)
                        .ref_(arena)
                        .make_function_type_mapper(ReportUnreliableMarkers),
                ),
            )
    }

    pub(super) fn signatures_identical_to(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
        kind: SignatureKind,
    ) -> io::Result<Ternary> {
        let source_signatures = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_signatures_of_type(source, kind)?;
        let target_signatures = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_signatures_of_type(target, kind)?;
        if source_signatures.len() != target_signatures.len() {
            return Ok(Ternary::False);
        }
        let mut result = Ternary::True;
        for i in 0..source_signatures.len() {
            let related = released!(self_.ref_(arena).type_checker)
                .ref_(arena)
                .compare_signatures_identical(
                    source_signatures[i].clone(),
                    target_signatures[i].clone(),
                    false,
                    false,
                    false,
                    |source: Id<Type>, target: Id<Type>| {
                        Self::is_related_to(self_, arena, source, target, None, None, None, None)
                    },
                )?;
            if related == Ternary::False {
                return Ok(Ternary::False);
            }
            result &= related;
        }
        Ok(result)
    }

    pub(super) fn members_related_to_index_info(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target_info: Id<IndexInfo>,
        report_errors: bool,
    ) -> io::Result<Ternary> {
        let mut result = Ternary::True;
        let key_type = target_info.ref_(arena).key_type;
        let props = if source
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::Intersection)
        {
            self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_properties_of_union_or_intersection_type(source)?
        } else {
            self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_properties_of_object_type(source)?
        };
        for prop in props {
            if self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_ignored_jsx_property(source, prop)
            {
                continue;
            }
            if released!(self_.ref_(arena).type_checker)
                .ref_(arena)
                .is_applicable_index_type(
                    released!(self_.ref_(arena).type_checker)
                        .ref_(arena)
                        .get_literal_type_from_property(
                            prop,
                            TypeFlags::StringOrNumberLiteralOrUnique,
                            None,
                        )?,
                    key_type,
                )?
            {
                let prop_type = released!(self_.ref_(arena).type_checker)
                    .ref_(arena)
                    .get_non_missing_type_of_symbol(prop)?;
                let type_ = if self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .exact_optional_property_types
                    == Some(true)
                    || prop_type
                        .ref_(arena)
                        .flags()
                        .intersects(TypeFlags::Undefined)
                    || key_type == self_.ref_(arena).type_checker.ref_(arena).number_type()
                    || !prop.ref_(arena).flags().intersects(SymbolFlags::Optional)
                {
                    prop_type
                } else {
                    self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_type_with_facts(prop_type, TypeFacts::NEUndefined)?
                };
                let related = Self::is_related_to(
                    self_,
                    arena,
                    type_,
                    released!(target_info.ref_(arena).type_),
                    Some(RecursionFlags::Both),
                    Some(report_errors),
                    None,
                    None,
                )?;
                if related == Ternary::False {
                    if report_errors {
                        Self::report_error(
                            self_,
                            arena,
                            Cow::Borrowed(
                                &Diagnostics::Property_0_is_incompatible_with_index_signature,
                            ),
                            Some(vec![self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .symbol_to_string_(
                                    prop,
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                    None,
                                )?]),
                        )?;
                    }
                    return Ok(Ternary::False);
                }
                result &= related;
            }
        }
        for &info in &released!(self_.ref_(arena).type_checker)
            .ref_(arena)
            .get_index_infos_of_type(source)?
        {
            if released!(self_.ref_(arena).type_checker)
                .ref_(arena)
                .is_applicable_index_type(info.ref_(arena).key_type, key_type)?
            {
                let related =
                    Self::index_info_related_to(self_, arena, info, target_info, report_errors)?;
                if related == Ternary::False {
                    return Ok(Ternary::False);
                }
                result &= related;
            }
        }
        Ok(result)
    }
}
