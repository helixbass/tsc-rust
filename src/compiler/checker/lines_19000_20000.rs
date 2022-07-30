#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::collections::HashSet;
use std::rc::Rc;

use super::{
    CheckTypeContainingMessageChain, CheckTypeRelatedTo, ErrorCalculationState, IntersectionState,
    RecursionFlags, ReportUnmeasurableMarkers, ReportUnreliableMarkers,
};
use crate::{
    are_option_rcs_equal, cartesian_product, push_if_unique_rc, reduce_left, some, CheckFlags,
    DiagnosticMessageChain, Diagnostics, Node, SignatureKind, Symbol, SymbolInterface, Ternary,
    Type, TypeFlags, TypeInterface, VarianceFlags, __String, get_check_flags,
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
        related
    }

    pub(super) fn properties_related_to(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
        excluded_properties: Option<&HashSet<__String>>,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::True;
        let require_optional_properties = false;
        // let unmatched_property =
        //     self.get_unmatched_property(source, target, require_optional_properties, false);
        // if let Some(unmatched_property) = unmatched_property {
        //     if report_errors {
        //         self.report_unmatched_property(
        //             source,
        //             target,
        //             unmatched_property,
        //             require_optional_properties,
        //         );
        //     }
        //     return Ternary::False;
        // }
        let properties = self.type_checker.get_properties_of_type(target);
        for target_prop in self.exclude_properties(&properties, excluded_properties) {
            let name = target_prop.escaped_name();
            if true {
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
                            true,
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

    pub(super) fn signatures_related_to(
        &self,
        source: &Type,
        target: &Type,
        kind: SignatureKind,
        report_errors: bool,
    ) -> Ternary {
        unimplemented!()
    }

    pub(super) fn index_signatures_related_to(
        &self,
        source: &Type,
        target: &Type,
        source_is_primitive: bool,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        unimplemented!()
    }
}
