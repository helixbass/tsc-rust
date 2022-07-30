#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cmp;
use std::collections::HashSet;
use std::ptr;
use std::rc::Rc;

use super::{
    anon, CheckTypeContainingMessageChain, CheckTypeRelatedTo, ErrorCalculationState,
    IntersectionState, RecursionFlags, ReportUnmeasurableMarkers, ReportUnreliableMarkers,
    SignatureCheckMode, TypeFacts,
};
use crate::{
    are_option_rcs_equal, cartesian_product, create_diagnostic_for_node, factory,
    get_declaration_modifier_flags_from_symbol, get_symbol_name_for_private_identifier,
    is_named_declaration, is_private_identifier, length, push_if_unique_rc, reduce_left, some,
    CheckFlags, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, ElementFlags, IndexInfo,
    ModifierFlags, Node, NodeInterface, ObjectFlags, Signature, SignatureFlags, SignatureKind,
    Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Ternary, Type, TypeFlags, TypeFormatFlags,
    TypeInterface, VarianceFlags, __String, get_check_flags, get_object_flags,
};

impl<'type_checker, TContainingMessageChain: CheckTypeContainingMessageChain>
    CheckTypeRelatedTo<'type_checker, TContainingMessageChain>
{
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
        &self,
        result: &mut Ternary,
        report_errors: bool,
        original_error_info: &mut Option<Rc<DiagnosticMessageChain>>,
        save_error_info: &ErrorCalculationState,
        variance_check_failed: &mut bool,
        source_type_arguments: Option<&[Rc<Type>]>,
        target_type_arguments: Option<&[Rc<Type>]>,
        variances: &[VarianceFlags],
        intersection_state: IntersectionState,
    ) -> Option<Ternary> {
        *result = self.type_arguments_related_to(
            source_type_arguments.map(ToOwned::to_owned),
            target_type_arguments.map(ToOwned::to_owned),
            Some(variances.to_owned()),
            report_errors,
            intersection_state,
        );
        if *result != Ternary::False {
            return Some(*result);
        }
        if some(
            Some(&variances),
            Some(|v: &VarianceFlags| v.intersects(VarianceFlags::AllowsStructuralFallback)),
        ) {
            *original_error_info = None;
            self.reset_error_info(save_error_info.clone());
            return None;
        }
        let allow_structural_fallback = matches!(
            target_type_arguments,
            Some(target_type_arguments) if self.type_checker.has_covariant_void_argument(target_type_arguments, variances)
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
                return Some(Ternary::False);
            }
            *original_error_info = self.maybe_error_info().clone();
            self.reset_error_info(save_error_info.clone());
        }
        None
    }

    pub(super) fn mapped_type_related_to(
        &self,
        source: &Type, /*MappedType*/
        target: &Type, /*MappedType*/
        report_errors: bool,
    ) -> Ternary {
        let modifiers_related = Rc::ptr_eq(&self.relation, &self.type_checker.comparable_relation)
            || if Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation) {
                self.type_checker.get_mapped_type_modifiers(source)
                    == self.type_checker.get_mapped_type_modifiers(target)
            } else {
                self.type_checker
                    .get_combined_mapped_type_optionality(source)
                    <= self
                        .type_checker
                        .get_combined_mapped_type_optionality(target)
            };
        if modifiers_related {
            let result: Ternary;
            let target_constraint = self
                .type_checker
                .get_constraint_type_from_mapped_type(target);
            let source_constraint = self.type_checker.instantiate_type(
                &self
                    .type_checker
                    .get_constraint_type_from_mapped_type(source),
                Some(&if self
                    .type_checker
                    .get_combined_mapped_type_optionality(source)
                    < 0
                {
                    self.type_checker
                        .make_function_type_mapper(ReportUnmeasurableMarkers)
                } else {
                    self.type_checker
                        .make_function_type_mapper(ReportUnreliableMarkers)
                }),
            );
            result = self.is_related_to(
                &target_constraint,
                &source_constraint,
                Some(RecursionFlags::Both),
                Some(report_errors),
                None,
                None,
            );
            if result != Ternary::False {
                let mapper = self.type_checker.create_type_mapper(
                    vec![self
                        .type_checker
                        .get_type_parameter_from_mapped_type(source)],
                    Some(vec![self
                        .type_checker
                        .get_type_parameter_from_mapped_type(target)]),
                );
                if are_option_rcs_equal(
                    self.type_checker
                        .maybe_instantiate_type(
                            self.type_checker.get_name_type_from_mapped_type(source),
                            Some(&mapper),
                        )
                        .as_ref(),
                    self.type_checker
                        .maybe_instantiate_type(
                            self.type_checker.get_name_type_from_mapped_type(target),
                            Some(&mapper),
                        )
                        .as_ref(),
                ) {
                    return result
                        & self.is_related_to(
                            &self.type_checker.instantiate_type(
                                &self.type_checker.get_template_type_from_mapped_type(source),
                                Some(&mapper),
                            ),
                            &self.type_checker.get_template_type_from_mapped_type(target),
                            Some(RecursionFlags::Both),
                            Some(report_errors),
                            None,
                            None,
                        );
                }
            }
        }
        Ternary::False
    }

    pub(super) fn type_related_to_discriminated_type(
        &self,
        source: &Type,
        target: &Type, /*UnionType*/
    ) -> Ternary {
        let source_properties = self.type_checker.get_properties_of_type(source);
        let source_properties_filtered = self
            .type_checker
            .find_discriminant_properties(&source_properties, target);
        if source_properties_filtered.is_none() {
            return Ternary::False;
        }
        let source_properties_filtered = source_properties_filtered.unwrap();

        let mut num_combinations = 1;
        for source_property in &source_properties_filtered {
            num_combinations *= self.type_checker.count_types(
                &self
                    .type_checker
                    .get_non_missing_type_of_symbol(source_property),
            );
            if num_combinations > 25 {
                // tracing?.instant(tracing.Phase.CheckTypes, "typeRelatedToDiscriminatedType_DepthLimit", { sourceId: source.id, targetId: target.id, numCombinations });
                return Ternary::False;
            }
        }

        let mut source_discriminant_types: Vec<Vec<Rc<Type>>> =
            Vec::with_capacity(source_properties_filtered.len());
        let mut excluded_properties: HashSet<__String> = HashSet::new();
        for (i, source_property) in source_properties_filtered.iter().enumerate() {
            let source_property_type = self
                .type_checker
                .get_non_missing_type_of_symbol(source_property);
            source_discriminant_types.push(
                if source_property_type.flags().intersects(TypeFlags::Union) {
                    source_property_type
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned()
                } else {
                    vec![source_property_type]
                },
            );
            excluded_properties.insert(source_property.escaped_name().clone());
        }

        let discriminant_combinations = cartesian_product(&source_discriminant_types);
        let mut matching_types: Vec<Rc<Type>> = vec![];
        for combination in &discriminant_combinations {
            let mut has_match = false;
            'outer: for type_ in target.as_union_or_intersection_type_interface().types() {
                for (i, source_property) in source_properties_filtered.iter().enumerate() {
                    let target_property = self.type_checker.get_property_of_type_(
                        type_,
                        source_property.escaped_name(),
                        None,
                    );
                    if target_property.is_none() {
                        continue 'outer;
                    }
                    let target_property = target_property.unwrap();
                    if Rc::ptr_eq(source_property, &target_property) {
                        continue;
                    }
                    let related = self.property_related_to(
                        source,
                        target,
                        source_property,
                        &target_property,
                        |_| combination[i].clone(),
                        false,
                        IntersectionState::None,
                        self.type_checker.strict_null_checks
                            || Rc::ptr_eq(&self.relation, &self.type_checker.comparable_relation),
                    );

                    if related == Ternary::False {
                        continue 'outer;
                    }
                }
                push_if_unique_rc(&mut matching_types, type_);
                has_match = true;
            }
            if !has_match {
                return Ternary::False;
            }
        }

        let mut result = Ternary::False;
        for type_ in &matching_types {
            result &= self.properties_related_to(
                source,
                type_,
                false,
                Some(&excluded_properties),
                IntersectionState::None,
            );
            if result != Ternary::False {
                result &= self.signatures_related_to(source, type_, SignatureKind::Call, false);
                if result != Ternary::False {
                    result &=
                        self.signatures_related_to(source, type_, SignatureKind::Construct, false);
                    if result != Ternary::False
                        && !(self.type_checker.is_tuple_type(source)
                            && self.type_checker.is_tuple_type(type_))
                    {
                        result &= self.index_signatures_related_to(
                            source,
                            type_,
                            false,
                            false,
                            IntersectionState::None,
                        );
                    }
                }
            }
            if result == Ternary::False {
                return result;
            }
        }
        result
    }

    pub(super) fn exclude_properties(
        &self,
        properties: &[Rc<Symbol>],
        excluded_properties: Option<&HashSet<__String>>,
    ) -> Vec<Rc<Symbol>> {
        if excluded_properties.is_none() || properties.is_empty() {
            return properties.to_owned();
        }
        let excluded_properties = excluded_properties.unwrap();
        let mut result: Option<Vec<Rc<Symbol>>> = None;
        for i in 0..properties.len() {
            if !excluded_properties.contains(properties[i].escaped_name()) {
                if let Some(result) = result.as_mut() {
                    result.push(properties[i].clone());
                }
            } else if result.is_none() {
                result = Some(properties[0..i].to_owned());
            }
        }
        result.unwrap_or_else(|| properties.to_owned())
    }

    pub(super) fn is_property_symbol_type_related<
        TGetTypeOfSourceProperty: FnMut(&Symbol) -> Rc<Type>,
    >(
        &self,
        source_prop: &Symbol,
        target_prop: &Symbol,
        mut get_type_of_source_property: TGetTypeOfSourceProperty,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let target_is_optional = self.type_checker.strict_null_checks
            && get_check_flags(target_prop).intersects(CheckFlags::Partial);
        let effective_target = self.type_checker.add_optionality(
            &self
                .type_checker
                .get_non_missing_type_of_symbol(target_prop),
            Some(false),
            Some(target_is_optional),
        );
        let effective_source = get_type_of_source_property(source_prop);
        self.is_related_to(
            &effective_source,
            &effective_target,
            Some(RecursionFlags::Both),
            Some(report_errors),
            None,
            Some(intersection_state),
        )
    }

    pub(super) fn property_related_to<TGetTypeOfSourceProperty: FnMut(&Symbol) -> Rc<Type>>(
        &self,
        source: &Type,
        target: &Type,
        source_prop: &Symbol,
        target_prop: &Symbol,
        get_type_of_source_property: TGetTypeOfSourceProperty,
        report_errors: bool,
        intersection_state: IntersectionState,
        skip_optional: bool,
    ) -> Ternary {
        let source_prop_flags = get_declaration_modifier_flags_from_symbol(source_prop, None);
        let target_prop_flags = get_declaration_modifier_flags_from_symbol(target_prop, None);
        if source_prop_flags.intersects(ModifierFlags::Private)
            || target_prop_flags.intersects(ModifierFlags::Private)
        {
            if !are_option_rcs_equal(
                source_prop.maybe_value_declaration().as_ref(),
                target_prop.maybe_value_declaration().as_ref(),
            ) {
                if report_errors {
                    if source_prop_flags.intersects(ModifierFlags::Private)
                        && target_prop_flags.intersects(ModifierFlags::Private)
                    {
                        self.report_error(
                            Cow::Borrowed(&Diagnostics::Types_have_separate_declarations_of_a_private_property_0),
                            Some(vec![
                                self.type_checker.symbol_to_string_(
                                    target_prop,
                                    Option::<&Node>::None,
                                    None, None, None
                                )
                            ])
                        );
                    } else {
                        self.report_error(
                            Cow::Borrowed(
                                &Diagnostics::Property_0_is_private_in_type_1_but_not_in_type_2,
                            ),
                            Some(vec![
                                self.type_checker.symbol_to_string_(
                                    target_prop,
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                    None,
                                ),
                                self.type_checker.type_to_string_(
                                    if source_prop_flags.intersects(ModifierFlags::Private) {
                                        source
                                    } else {
                                        target
                                    },
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                ),
                                self.type_checker.type_to_string_(
                                    if source_prop_flags.intersects(ModifierFlags::Private) {
                                        target
                                    } else {
                                        source
                                    },
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                ),
                            ]),
                        );
                    }
                }
                return Ternary::False;
            }
        } else if target_prop_flags.intersects(ModifierFlags::Protected) {
            if !self
                .type_checker
                .is_valid_override_of(source_prop, target_prop)
            {
                if report_errors {
                    self.report_error(
                        Cow::Borrowed(&Diagnostics::Property_0_is_protected_but_type_1_is_not_a_class_derived_from_2),
                        Some(vec![
                            self.type_checker.symbol_to_string_(
                                target_prop,
                                Option::<&Node>::None,
                                None, None, None
                            ),
                            self.type_checker.type_to_string_(
                                &self.type_checker.get_declaring_class(source_prop).unwrap_or_else(|| source.type_wrapper()),
                                Option::<&Node>::None,
                                None, None,
                            ),
                            self.type_checker.type_to_string_(
                                &self.type_checker.get_declaring_class(target_prop).unwrap_or_else(|| target.type_wrapper()),
                                Option::<&Node>::None,
                                None, None,
                            ),
                        ])
                    );
                }
                return Ternary::False;
            }
        } else if source_prop_flags.intersects(ModifierFlags::Protected) {
            if report_errors {
                self.report_error(
                    Cow::Borrowed(
                        &Diagnostics::Property_0_is_protected_in_type_1_but_public_in_type_2,
                    ),
                    Some(vec![
                        self.type_checker.symbol_to_string_(
                            target_prop,
                            Option::<&Node>::None,
                            None,
                            None,
                            None,
                        ),
                        self.type_checker.type_to_string_(
                            source,
                            Option::<&Node>::None,
                            None,
                            None,
                        ),
                        self.type_checker.type_to_string_(
                            target,
                            Option::<&Node>::None,
                            None,
                            None,
                        ),
                    ]),
                );
            }
            return Ternary::False;
        }
        let related = self.is_property_symbol_type_related(
            source_prop,
            target_prop,
            get_type_of_source_property,
            report_errors,
            intersection_state,
        );
        if related == Ternary::False {
            if report_errors {
                self.report_incompatible_error(
                    &Diagnostics::Types_of_property_0_are_incompatible,
                    Some(vec![self.type_checker.symbol_to_string_(
                        target_prop,
                        Option::<&Node>::None,
                        None,
                        None,
                        None,
                    )]),
                );
            }
            return Ternary::False;
        }
        if !skip_optional
            && source_prop.flags().intersects(SymbolFlags::Optional)
            && !target_prop.flags().intersects(SymbolFlags::Optional)
        {
            if report_errors {
                self.report_error(
                    Cow::Borrowed(
                        &Diagnostics::Property_0_is_optional_in_type_1_but_required_in_type_2,
                    ),
                    Some(vec![
                        self.type_checker.symbol_to_string_(
                            target_prop,
                            Option::<&Node>::None,
                            None,
                            None,
                            None,
                        ),
                        self.type_checker.type_to_string_(
                            source,
                            Option::<&Node>::None,
                            None,
                            None,
                        ),
                        self.type_checker.type_to_string_(
                            target,
                            Option::<&Node>::None,
                            None,
                            None,
                        ),
                    ]),
                );
            }
            return Ternary::False;
        }
        related
    }

    pub(super) fn report_unmatched_property(
        &self,
        source: &Type,
        target: &Type,
        unmatched_property: &Symbol,
        require_optional_properties: bool,
    ) {
        let mut should_skip_elaboration = false;
        if let Some(unmatched_property_value_declaration) = unmatched_property
            .maybe_value_declaration()
            .as_ref()
            .filter(|unmatched_property_value_declaration| {
                is_named_declaration(unmatched_property_value_declaration)
                    && is_private_identifier(
                        &unmatched_property_value_declaration
                            .as_named_declaration()
                            .name(),
                    )
            })
        {
            if let Some(source_symbol) = source
                .maybe_symbol()
                .as_ref()
                .filter(|source_symbol| source_symbol.flags().intersects(SymbolFlags::Class))
            {
                let private_identifier_description = unmatched_property_value_declaration
                    .as_named_declaration()
                    .name()
                    .as_private_identifier()
                    .escaped_text
                    .clone();
                let symbol_table_key = get_symbol_name_for_private_identifier(
                    source_symbol,
                    &private_identifier_description,
                );
                if
                /*symbolTableKey &&*/
                self
                    .type_checker
                    .get_property_of_type_(source, &symbol_table_key, None)
                    .is_some()
                {
                    let source_name = factory.with(|factory_| {
                        factory_.get_declaration_name(
                            source_symbol.maybe_value_declaration(),
                            None,
                            None,
                        )
                    });
                    let target_name = factory.with(|factory_| {
                        factory_.get_declaration_name(
                            target.symbol().maybe_value_declaration(),
                            None,
                            None,
                        )
                    });
                    self.report_error(
                        Cow::Borrowed(&Diagnostics::Property_0_in_type_1_refers_to_a_different_member_that_cannot_be_accessed_from_within_type_2),
                        Some(vec![
                            self.type_checker.diagnostic_name(private_identifier_description.clone().into()).into_owned(),
                            self.type_checker.diagnostic_name(if source_name.as_identifier().escaped_text.eq_str("") {
                                anon.clone().into()
                            } else {
                                source_name.into()
                            }).into_owned(),
                            self.type_checker.diagnostic_name(if target_name.as_identifier().escaped_text.eq_str("") {
                                anon.clone().into()
                            } else {
                                target_name.into()
                            }).into_owned(),
                        ])
                    );
                    return;
                }
            }
        }
        let props = self.type_checker.get_unmatched_properties(
            source,
            target,
            require_optional_properties,
            false,
        );
        if match self.head_message.as_ref() {
            None => true,
            Some(head_message) => head_message.code != Diagnostics::Class_0_incorrectly_implements_interface_1.code &&
                head_message.code != Diagnostics::Class_0_incorrectly_implements_class_1_Did_you_mean_to_extend_1_and_inherit_its_members_as_a_subclass.code
        } {
            should_skip_elaboration = true;
        }
        if props.len() == 1 {
            let prop_name = self.type_checker.symbol_to_string_(
                unmatched_property,
                Option::<&Node>::None,
                None,
                None,
                None,
            );
            let (source_string, target_string) = self
                .type_checker
                .get_type_names_for_error_display(source, target);
            self.report_error(
                Cow::Borrowed(&Diagnostics::Property_0_is_missing_in_type_1_but_required_in_type_2),
                Some(vec![prop_name.clone(), source_string, target_string]),
            );
            if length(unmatched_property.maybe_declarations().as_deref()) > 0 {
                self.associate_related_info(
                    create_diagnostic_for_node(
                        &unmatched_property.maybe_declarations().as_ref().unwrap()[0],
                        &Diagnostics::_0_is_declared_here,
                        Some(vec![prop_name]),
                    )
                    .into(),
                );
            }
            if should_skip_elaboration && self.maybe_error_info().is_some() {
                self.set_override_next_error_info(self.override_next_error_info() + 1);
            }
        } else if self.try_elaborate_array_like_errors(source, target, false) {
            if props.len() > 5 {
                self.report_error(
                    Cow::Borrowed(&Diagnostics::Type_0_is_missing_the_following_properties_from_type_1_Colon_2_and_3_more),
                    Some(vec![
                        self.type_checker.type_to_string_(
                            source,
                            Option::<&Node>::None,
                            None, None,
                        ),
                        self.type_checker.type_to_string_(
                            target,
                            Option::<&Node>::None,
                            None, None,
                        ),
                        (&props[0..3]).into_iter().map(|p: &Rc<Symbol>|
                            self.type_checker.symbol_to_string_(
                                p,
                                Option::<&Node>::None,
                                None, None, None,
                            )
                        ).collect::<Vec<_>>().join(", "),
                        (props.len() - 4).to_string(),
                    ])
                )
            } else {
                self.report_error(
                    Cow::Borrowed(&Diagnostics::Type_0_is_missing_the_following_properties_from_type_1_Colon_2),
                    Some(vec![
                        self.type_checker.type_to_string_(
                            source,
                            Option::<&Node>::None,
                            None, None,
                        ),
                        self.type_checker.type_to_string_(
                            target,
                            Option::<&Node>::None,
                            None, None,
                        ),
                        props.iter().map(|p: &Rc<Symbol>|
                            self.type_checker.symbol_to_string_(
                                p,
                                Option::<&Node>::None,
                                None, None, None,
                            )
                        ).collect::<Vec<_>>().join(", "),
                    ])
                )
            }
            if should_skip_elaboration && self.maybe_error_info().is_some() {
                self.set_override_next_error_info(self.override_next_error_info() + 1);
            }
        }
    }

    pub(super) fn properties_related_to(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
        excluded_properties: Option<&HashSet<__String>>,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation) {
            return self.properties_identical_to(source, target, excluded_properties);
        }
        let mut result = Ternary::True;
        if self.type_checker.is_tuple_type(target) {
            let target_as_type_reference = target.as_type_reference();
            let target_target_as_tuple_type = target_as_type_reference.target.as_tuple_type();
            if self.type_checker.is_array_type(source) || self.type_checker.is_tuple_type(source) {
                if !target_target_as_tuple_type.readonly
                    && (self.type_checker.is_readonly_array_type(source)
                        || self.type_checker.is_tuple_type(source)
                            && source.as_type_reference().target.as_tuple_type().readonly)
                {
                    return Ternary::False;
                }
                let source_arity = self.type_checker.get_type_reference_arity(source);
                let target_arity = self.type_checker.get_type_reference_arity(target);
                let source_rest_flag = if self.type_checker.is_tuple_type(source) {
                    source
                        .as_type_reference()
                        .target
                        .as_tuple_type()
                        .combined_flags
                        & ElementFlags::Rest
                } else {
                    ElementFlags::Rest
                };
                let target_rest_flag =
                    target_target_as_tuple_type.combined_flags & ElementFlags::Rest;
                let source_min_length = if self.type_checker.is_tuple_type(source) {
                    source.as_type_reference().target.as_tuple_type().min_length
                } else {
                    0
                };
                let target_min_length = target_target_as_tuple_type.min_length;
                if source_rest_flag == ElementFlags::None && source_arity < target_min_length {
                    if report_errors {
                        self.report_error(
                            Cow::Borrowed(
                                &Diagnostics::Source_has_0_element_s_but_target_requires_1,
                            ),
                            Some(vec![
                                source_arity.to_string(),
                                target_min_length.to_string(),
                            ]),
                        );
                    }
                    return Ternary::False;
                }
                if target_rest_flag == ElementFlags::None && target_arity < source_min_length {
                    if report_errors {
                        self.report_error(
                            Cow::Borrowed(
                                &Diagnostics::Source_has_0_element_s_but_target_allows_only_1,
                            ),
                            Some(vec![
                                source_min_length.to_string(),
                                target_arity.to_string(),
                            ]),
                        );
                    }
                    return Ternary::False;
                }
                if target_rest_flag == ElementFlags::None
                    && (source_rest_flag == ElementFlags::None || target_arity < source_arity)
                {
                    if report_errors {
                        if source_min_length < target_min_length {
                            self.report_error(
                                Cow::Borrowed(&Diagnostics::Target_requires_0_element_s_but_source_may_have_fewer),
                                Some(vec![
                                    target_min_length.to_string(),
                                ])
                            );
                        } else {
                            self.report_error(
                                Cow::Borrowed(&Diagnostics::Target_allows_only_0_element_s_but_source_may_have_more),
                                Some(vec![
                                    target_arity.to_string(),
                                ])
                            );
                        }
                    }
                    return Ternary::False;
                }
                let source_type_arguments = self.type_checker.get_type_arguments(source);
                let target_type_arguments = self.type_checker.get_type_arguments(target);
                let start_count = cmp::min(
                    if self.type_checker.is_tuple_type(source) {
                        self.type_checker.get_start_element_count(
                            &source.as_type_reference().target,
                            ElementFlags::NonRest,
                        )
                    } else {
                        0
                    },
                    self.type_checker.get_start_element_count(
                        &target_as_type_reference.target,
                        ElementFlags::NonRest,
                    ),
                );
                let end_count = cmp::min(
                    if self.type_checker.is_tuple_type(source) {
                        self.type_checker.get_end_element_count(
                            &source.as_type_reference().target,
                            ElementFlags::NonRest,
                        )
                    } else {
                        0
                    },
                    if target_rest_flag != ElementFlags::None {
                        self.type_checker.get_end_element_count(
                            &target_as_type_reference.target,
                            ElementFlags::NonRest,
                        )
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
                    let source_flags = if self.type_checker.is_tuple_type(source)
                        && (i < start_count || i >= target_arity - end_count)
                    {
                        source
                            .as_type_reference()
                            .target
                            .as_tuple_type()
                            .element_flags[source_index]
                    } else {
                        ElementFlags::Rest
                    };
                    let target_flags = target_target_as_tuple_type.element_flags[i];
                    if target_flags.intersects(ElementFlags::Variadic)
                        && !source_flags.intersects(ElementFlags::Variadic)
                    {
                        if report_errors {
                            self.report_error(
                                Cow::Borrowed(&Diagnostics::Source_provides_no_match_for_variadic_element_at_position_0_in_target),
                                Some(vec![
                                    i.to_string(),
                                ])
                            );
                        }
                        return Ternary::False;
                    }
                    if source_flags.intersects(ElementFlags::Variadic)
                        && !target_flags.intersects(ElementFlags::Variable)
                    {
                        if report_errors {
                            self.report_error(
                                Cow::Borrowed(&Diagnostics::Variadic_element_at_position_0_in_source_does_not_match_element_at_position_1_in_target),
                                Some(vec![
                                    source_index.to_string(),
                                    i.to_string(),
                                ])
                            );
                        }
                        return Ternary::False;
                    }
                    if target_flags.intersects(ElementFlags::Required)
                        && !source_flags.intersects(ElementFlags::Required)
                    {
                        if report_errors {
                            self.report_error(
                                Cow::Borrowed(&Diagnostics::Source_provides_no_match_for_required_element_at_position_0_in_target),
                                Some(vec![
                                    i.to_string(),
                                ])
                            );
                        }
                        return Ternary::False;
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
                                Some(excluded_properties) if excluded_properties.contains(&__String::new(i.to_string()))
                            )
                        {
                            continue;
                        }
                    }
                    let source_type = if !self.type_checker.is_tuple_type(source) {
                        source_type_arguments[0].clone()
                    } else if i < start_count || i >= target_arity - end_count {
                        self.type_checker.remove_missing_type(
                            &source_type_arguments[source_index],
                            (source_flags & target_flags).intersects(ElementFlags::Optional),
                        )
                    } else {
                        self.type_checker
                            .get_element_type_of_slice_of_tuple_type(
                                source,
                                start_count,
                                Some(end_count),
                                None,
                            )
                            .unwrap_or_else(|| self.type_checker.never_type())
                    };
                    let target_type = &target_type_arguments[i];
                    let target_check_type = if source_flags.intersects(ElementFlags::Variadic)
                        && target_flags.intersects(ElementFlags::Rest)
                    {
                        self.type_checker.create_array_type(target_type, None)
                    } else {
                        self.type_checker.remove_missing_type(
                            target_type,
                            target_flags.intersects(ElementFlags::Optional),
                        )
                    };
                    let related = self.is_related_to(
                        &source_type,
                        &target_check_type,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        Some(intersection_state),
                    );
                    if related == Ternary::False {
                        if report_errors && (target_arity > 1 || source_arity > 1) {
                            if i < start_count
                                || i >= target_arity - end_count
                                || source_arity - start_count - end_count == 1
                            {
                                self.report_incompatible_error(
                                    &Diagnostics::Type_at_position_0_in_source_is_not_compatible_with_type_at_position_1_in_target,
                                    Some(vec![
                                        source_index.to_string(),
                                        i.to_string(),
                                    ])
                                );
                            } else {
                                self.report_incompatible_error(
                                    &Diagnostics::Type_at_positions_0_through_1_in_source_is_not_compatible_with_type_at_position_2_in_target,
                                    Some(vec![
                                        start_count.to_string(),
                                        (source_arity - end_count - 1).to_string(),
                                        i.to_string(),
                                    ])
                                );
                            }
                        }
                        return Ternary::False;
                    }
                    result &= related;
                }
                return result;
            }
            if target_target_as_tuple_type
                .combined_flags
                .intersects(ElementFlags::Variable)
            {
                return Ternary::False;
            }
        }
        let require_optional_properties =
            (Rc::ptr_eq(&self.relation, &self.type_checker.subtype_relation)
                || Rc::ptr_eq(&self.relation, &self.type_checker.strict_subtype_relation))
                && !self.type_checker.is_object_literal_type(source)
                && !self.type_checker.is_empty_array_literal_type(source)
                && !self.type_checker.is_tuple_type(source);
        let unmatched_property = self.type_checker.get_unmatched_property(
            source,
            target,
            require_optional_properties,
            false,
        );
        if let Some(unmatched_property) = unmatched_property.as_ref() {
            if report_errors {
                self.report_unmatched_property(
                    source,
                    target,
                    unmatched_property,
                    require_optional_properties,
                );
            }
            return Ternary::False;
        }
        if self.type_checker.is_object_literal_type(target) {
            for source_prop in &self.exclude_properties(
                &self.type_checker.get_properties_of_type(source),
                excluded_properties,
            ) {
                if self
                    .type_checker
                    .get_property_of_object_type(target, source_prop.escaped_name())
                    .is_none()
                {
                    let source_type = self.type_checker.get_type_of_symbol(source_prop);
                    if !source_type.flags().intersects(TypeFlags::Undefined) {
                        if report_errors {
                            self.report_error(
                                Cow::Borrowed(&Diagnostics::Property_0_does_not_exist_on_type_1),
                                Some(vec![
                                    self.type_checker.symbol_to_string_(
                                        source_prop,
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                        None,
                                    ),
                                    self.type_checker.type_to_string_(
                                        target,
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                    ),
                                ]),
                            );
                        }
                        return Ternary::False;
                    }
                }
            }
        }
        let properties = self.type_checker.get_properties_of_type(target);
        let numeric_names_only =
            self.type_checker.is_tuple_type(source) && self.type_checker.is_tuple_type(target);
        for target_prop in &self.exclude_properties(&properties, excluded_properties) {
            let name = target_prop.escaped_name();
            if !target_prop.flags().intersects(SymbolFlags::Prototype)
                && (!numeric_names_only
                    || self.type_checker.is_numeric_literal_name(&**name)
                    || name.eq_str("length"))
            {
                let source_prop = self.type_checker.get_property_of_type_(source, name, None);
                if let Some(source_prop) = source_prop {
                    if !Rc::ptr_eq(&source_prop, &target_prop) {
                        let related = self.property_related_to(
                            source,
                            target,
                            &source_prop,
                            &target_prop,
                            |symbol| self.type_checker.get_non_missing_type_of_symbol(symbol),
                            report_errors,
                            intersection_state,
                            Rc::ptr_eq(&self.relation, &self.type_checker.comparable_relation),
                        );
                        if related == Ternary::False {
                            return Ternary::False;
                        }
                        result &= related;
                    }
                }
            }
        }
        result
    }

    pub(super) fn properties_identical_to(
        &self,
        source: &Type,
        target: &Type,
        excluded_properties: Option<&HashSet<__String>>,
    ) -> Ternary {
        if !(source.flags().intersects(TypeFlags::Object)
            && target.flags().intersects(TypeFlags::Object))
        {
            return Ternary::False;
        }
        let source_properties = self.exclude_properties(
            &self.type_checker.get_properties_of_object_type(source),
            excluded_properties,
        );
        let target_properties = self.exclude_properties(
            &self.type_checker.get_properties_of_object_type(target),
            excluded_properties,
        );
        if source_properties.len() != target_properties.len() {
            return Ternary::False;
        }
        let mut result = Ternary::True;
        for source_prop in &source_properties {
            let target_prop = self
                .type_checker
                .get_property_of_object_type(target, source_prop.escaped_name());
            if target_prop.is_none() {
                return Ternary::False;
            }
            let target_prop = target_prop.unwrap();
            let related = self.type_checker.compare_properties(
                source_prop,
                &target_prop,
                |source, target| self.is_related_to(source, target, None, None, None, None),
            );
            if related == Ternary::False {
                return Ternary::False;
            }
            result &= related;
        }
        result
    }

    pub(super) fn signatures_related_to(
        &self,
        source: &Type,
        target: &Type,
        kind: SignatureKind,
        report_errors: bool,
    ) -> Ternary {
        if Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation) {
            return self.signatures_identical_to(source, target, kind);
        }
        if ptr::eq(target, &*self.type_checker.any_function_type())
            || ptr::eq(source, &*self.type_checker.any_function_type())
        {
            return Ternary::True;
        }

        let source_is_js_constructor = matches!(
            source.maybe_symbol().as_ref(),
            Some(source_symbol) if self.type_checker.is_js_constructor(source_symbol.maybe_value_declaration())
        );
        let target_is_js_constructor = matches!(
            target.maybe_symbol().as_ref(),
            Some(target_symbol) if self.type_checker.is_js_constructor(target_symbol.maybe_value_declaration())
        );

        let source_signatures = self.type_checker.get_signatures_of_type(
            source,
            if source_is_js_constructor && kind == SignatureKind::Construct {
                SignatureKind::Call
            } else {
                kind
            },
        );
        let target_signatures = self.type_checker.get_signatures_of_type(
            target,
            if target_is_js_constructor && kind == SignatureKind::Construct {
                SignatureKind::Call
            } else {
                kind
            },
        );

        if kind == SignatureKind::Construct
            && !source_signatures.is_empty()
            && !target_signatures.is_empty()
        {
            let source_is_abstract = source_signatures[0]
                .flags
                .intersects(SignatureFlags::Abstract);
            let target_is_abstract = target_signatures[0]
                .flags
                .intersects(SignatureFlags::Abstract);
            if source_is_abstract && !target_is_abstract {
                if report_errors {
                    self.report_error(
                        Cow::Borrowed(&Diagnostics::Cannot_assign_an_abstract_constructor_type_to_a_non_abstract_constructor_type),
                        None,
                    );
                }
                return Ternary::False;
            }
            if !self.constructor_visibilities_are_compatible(
                &source_signatures[0],
                &target_signatures[0],
                report_errors,
            ) {
                return Ternary::False;
            }
        }

        let mut result = Ternary::True;
        let save_error_info = self.capture_error_calculation_state();
        let incompatible_reporter: fn(&Self, &Signature, &Signature) -> fn(&Self, &Type, &Type) =
            if kind == SignatureKind::Construct {
                Self::report_incompatible_call_signature_return
            } else {
                Self::report_incompatible_construct_signature_return
            };
        let source_object_flags = get_object_flags(source);
        let target_object_flags = get_object_flags(target);
        if source_object_flags.intersects(ObjectFlags::Instantiated)
            && target_object_flags.intersects(ObjectFlags::Instantiated)
            && are_option_rcs_equal(
                source.maybe_symbol().as_ref(),
                target.maybe_symbol().as_ref(),
            )
        {
            for i in 0..target_signatures.len() {
                let related = self.signature_related_to(
                    source_signatures[i].clone(),
                    target_signatures[i].clone(),
                    true,
                    report_errors,
                    incompatible_reporter(self, &source_signatures[i], &target_signatures[i]),
                );
                if related == Ternary::False {
                    return Ternary::False;
                }
                result &= related;
            }
        } else if source_signatures.len() == 1 && target_signatures.len() == 1 {
            let erase_generics = Rc::ptr_eq(&self.relation, &self.type_checker.comparable_relation)
                || matches!(
                    self.type_checker.compiler_options.no_strict_generic_checks,
                    Some(true)
                );
            let source_signature = &source_signatures[0];
            let target_signature = &target_signatures[0];
            result = self.signature_related_to(
                source_signature.clone(),
                target_signature.clone(),
                erase_generics,
                report_errors,
                incompatible_reporter(self, source_signature, target_signature),
            );
            if result == Ternary::False
                && report_errors
                && kind == SignatureKind::Construct
                && source_object_flags & target_object_flags != ObjectFlags::None
                && (matches!(
                    target_signature.declaration.as_ref(),
                    Some(target_signature_declaration) if target_signature_declaration.kind() == SyntaxKind::Constructor
                ) || matches!(
                    source_signature.declaration.as_ref(),
                    Some(source_signature_declaration) if source_signature_declaration.kind() == SyntaxKind::Constructor
                ))
            {
                let construct_signature_to_string = |signature: &Signature| -> String {
                    self.type_checker.signature_to_string_(
                        signature,
                        Option::<&Node>::None,
                        Some(TypeFormatFlags::WriteArrowStyleSignature),
                        Some(kind),
                        None,
                    )
                };
                self.report_error(
                    Cow::Borrowed(&Diagnostics::Type_0_is_not_assignable_to_type_1),
                    Some(vec![
                        construct_signature_to_string(source_signature),
                        construct_signature_to_string(target_signature),
                    ]),
                );
                self.report_error(
                    Cow::Borrowed(&Diagnostics::Types_of_construct_signatures_are_incompatible),
                    None,
                );
                return result;
            }
        } else {
            'outer: for t in &target_signatures {
                let mut should_elaborate_errors = report_errors;
                for s in &source_signatures {
                    let related = self.signature_related_to(
                        s.clone(),
                        t.clone(),
                        true,
                        should_elaborate_errors,
                        incompatible_reporter(self, s, t),
                    );
                    if related != Ternary::False {
                        result &= related;
                        self.reset_error_info(save_error_info.clone());
                        continue 'outer;
                    }
                    should_elaborate_errors = false;
                }

                if should_elaborate_errors {
                    self.report_error(
                        Cow::Borrowed(&Diagnostics::Type_0_provides_no_match_for_the_signature_1),
                        Some(vec![
                            self.type_checker.type_to_string_(
                                source,
                                Option::<&Node>::None,
                                None,
                                None,
                            ),
                            self.type_checker.signature_to_string_(
                                t,
                                Option::<&Node>::None,
                                None,
                                Some(kind),
                                None,
                            ),
                        ]),
                    );
                }
                return Ternary::False;
            }
        }
        result
    }

    pub(super) fn report_incompatible_call_signature_return(
        &self,
        siga: &Signature,
        sigb: &Signature,
    ) -> fn(&Self, &Type, &Type) {
        if siga.parameters().is_empty() && sigb.parameters().is_empty() {
            Self::report_incompatible_call_signature_return_no_arguments
        } else {
            Self::report_incompatible_call_signature_return_some_arguments
        }
    }

    pub(super) fn report_incompatible_call_signature_return_no_arguments(
        &self,
        source: &Type,
        target: &Type,
    ) {
        self.report_incompatible_error(
            &Diagnostics::Call_signatures_with_no_arguments_have_incompatible_return_types_0_and_1,
            Some(vec![
                self.type_checker
                    .type_to_string_(source, Option::<&Node>::None, None, None),
                self.type_checker
                    .type_to_string_(target, Option::<&Node>::None, None, None),
            ]),
        )
    }

    pub(super) fn report_incompatible_call_signature_return_some_arguments(
        &self,
        source: &Type,
        target: &Type,
    ) {
        self.report_incompatible_error(
            &Diagnostics::Call_signature_return_types_0_and_1_are_incompatible,
            Some(vec![
                self.type_checker
                    .type_to_string_(source, Option::<&Node>::None, None, None),
                self.type_checker
                    .type_to_string_(target, Option::<&Node>::None, None, None),
            ]),
        )
    }

    pub(super) fn report_incompatible_construct_signature_return(
        &self,
        siga: &Signature,
        sigb: &Signature,
    ) -> fn(&Self, &Type, &Type) {
        if siga.parameters().is_empty() && sigb.parameters().is_empty() {
            Self::report_incompatible_construct_signature_return_no_arguments
        } else {
            Self::report_incompatible_construct_signature_return_some_arguments
        }
    }

    pub(super) fn report_incompatible_construct_signature_return_no_arguments(
        &self,
        source: &Type,
        target: &Type,
    ) {
        self.report_incompatible_error(
            &Diagnostics::Construct_signatures_with_no_arguments_have_incompatible_return_types_0_and_1,
            Some(vec![
                self.type_checker
                    .type_to_string_(source, Option::<&Node>::None, None, None),
                self.type_checker
                    .type_to_string_(target, Option::<&Node>::None, None, None),
            ]),
        )
    }

    pub(super) fn report_incompatible_construct_signature_return_some_arguments(
        &self,
        source: &Type,
        target: &Type,
    ) {
        self.report_incompatible_error(
            &Diagnostics::Construct_signature_return_types_0_and_1_are_incompatible,
            Some(vec![
                self.type_checker
                    .type_to_string_(source, Option::<&Node>::None, None, None),
                self.type_checker
                    .type_to_string_(target, Option::<&Node>::None, None, None),
            ]),
        )
    }

    pub(super) fn signature_related_to(
        &self,
        source: Rc<Signature>,
        target: Rc<Signature>,
        erase: bool,
        report_errors: bool,
        incompatible_reporter: fn(&Self, &Type, &Type),
    ) -> Ternary {
        self.type_checker.compare_signatures_related(
            if erase {
                self.type_checker.get_erased_signature(source)
            } else {
                source
            },
            if erase {
                self.type_checker.get_erased_signature(target)
            } else {
                target
            },
            if Rc::ptr_eq(&self.relation, &self.type_checker.strict_subtype_relation) {
                SignatureCheckMode::StrictArity
            } else {
                SignatureCheckMode::None
            },
            report_errors,
            &mut Some(&mut |message: Cow<'static, DiagnosticMessage>,
                            args: Option<Vec<String>>| {
                self.report_error(message, args)
            }),
            Some(&|source: &Type, target: &Type| incompatible_reporter(self, source, target)),
            &mut |source: &Type, target: &Type, report_errors: Option<bool>| {
                self.is_related_to_worker(
                    source,
                    target,
                    report_errors.unwrap_or(false), // again here this default false isn't in the Typescript version but appears to be a type-checking error there
                )
            },
            Some(
                &self
                    .type_checker
                    .make_function_type_mapper(ReportUnreliableMarkers),
            ),
        )
    }

    pub(super) fn signatures_identical_to(
        &self,
        source: &Type,
        target: &Type,
        kind: SignatureKind,
    ) -> Ternary {
        let source_signatures = self.type_checker.get_signatures_of_type(source, kind);
        let target_signatures = self.type_checker.get_signatures_of_type(target, kind);
        if source_signatures.len() != target_signatures.len() {
            return Ternary::False;
        }
        let mut result = Ternary::True;
        for i in 0..source_signatures.len() {
            let related = self.type_checker.compare_signatures_identical(
                source_signatures[i].clone(),
                target_signatures[i].clone(),
                false,
                false,
                false,
                |source: &Type, target: &Type| {
                    self.is_related_to(source, target, None, None, None, None)
                },
            );
            if related == Ternary::False {
                return Ternary::False;
            }
            result &= related;
        }
        result
    }

    pub(super) fn members_related_to_index_info(
        &self,
        source: &Type,
        target_info: &IndexInfo,
        report_errors: bool,
    ) -> Ternary {
        let mut result = Ternary::True;
        let key_type = &target_info.key_type;
        let props = if source.flags().intersects(TypeFlags::Intersection) {
            self.type_checker
                .get_properties_of_union_or_intersection_type(source)
        } else {
            self.type_checker.get_properties_of_object_type(source)
        };
        for prop in &props {
            if self.type_checker.is_ignored_jsx_property(source, prop) {
                continue;
            }
            if self.type_checker.is_applicable_index_type(
                &self.type_checker.get_literal_type_from_property(
                    prop,
                    TypeFlags::StringOrNumberLiteralOrUnique,
                    None,
                ),
                key_type,
            ) {
                let prop_type = self.type_checker.get_non_missing_type_of_symbol(prop);
                let type_ = if matches!(self.type_checker.exact_optional_property_types, Some(true))
                    || prop_type.flags().intersects(TypeFlags::Undefined)
                    || Rc::ptr_eq(key_type, &self.type_checker.number_type())
                    || !prop.flags().intersects(SymbolFlags::Optional)
                {
                    prop_type
                } else {
                    self.type_checker
                        .get_type_with_facts(&prop_type, TypeFacts::NEUndefined)
                };
                let related = self.is_related_to(
                    &type_,
                    &target_info.type_,
                    Some(RecursionFlags::Both),
                    Some(report_errors),
                    None,
                    None,
                );
                if related == Ternary::False {
                    if report_errors {
                        self.report_error(
                            Cow::Borrowed(
                                &Diagnostics::Property_0_is_incompatible_with_index_signature,
                            ),
                            Some(vec![self.type_checker.symbol_to_string_(
                                prop,
                                Option::<&Node>::None,
                                None,
                                None,
                                None,
                            )]),
                        );
                    }
                    return Ternary::False;
                }
                result &= related;
            }
        }
        for info in &self.type_checker.get_index_infos_of_type(source) {
            if self
                .type_checker
                .is_applicable_index_type(&info.key_type, key_type)
            {
                let related = self.index_info_related_to(info, target_info, report_errors);
                if related == Ternary::False {
                    return Ternary::False;
                }
                result &= related;
            }
        }
        result
    }
}
