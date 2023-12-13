use std::{
    borrow::{Borrow, Cow},
    cell::{Cell, RefCell},
    collections::HashMap,
    io, ptr,
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;
use itertools::Itertools;

use super::{CheckTypeRelatedTo, IntersectionState, RecursionFlags};
use crate::{
    get_declaration_modifier_flags_from_symbol, get_object_flags, try_every, ConditionalRoot,
    OutofbandVarianceMarkerHandler, SymbolLinks, TransientSymbolInterface, __String, every,
    get_check_flags, get_selected_effective_modifier_flags, maybe_map, some, try_filter,
    try_for_each_bool, try_some, CheckFlags, Diagnostics, IndexInfo, ModifierFlags, Node,
    ObjectFlags, ObjectFlagsTypeInterface, OptionTry, RelationComparisonResult, Signature, Symbol,
    SymbolFlags, SymbolInterface, Ternary, Type, TypeChecker, TypeFlags, TypeInterface,
    UnionOrIntersectionTypeInterface, VarianceFlags,
};

impl CheckTypeRelatedTo {
    pub(super) fn index_info_related_to(
        &self,
        source_info: &IndexInfo,
        target_info: &IndexInfo,
        report_errors: bool,
    ) -> io::Result<Ternary> {
        let related = self.is_related_to(
            source_info.type_,
            target_info.type_,
            Some(RecursionFlags::Both),
            Some(report_errors),
            None,
            None,
        )?;
        if related == Ternary::False && report_errors {
            if source_info.key_type == target_info.key_type {
                self.report_error(
                    Cow::Borrowed(&Diagnostics::_0_index_signatures_are_incompatible),
                    Some(vec![self.type_checker.type_to_string_(
                        source_info.key_type,
                        Option::<&Node>::None,
                        None,
                        None,
                    )?]),
                )?;
            } else {
                self.report_error(
                    Cow::Borrowed(&Diagnostics::_0_and_1_index_signatures_are_incompatible),
                    Some(vec![
                        self.type_checker.type_to_string_(
                            source_info.key_type,
                            Option::<&Node>::None,
                            None,
                            None,
                        )?,
                        self.type_checker.type_to_string_(
                            target_info.key_type,
                            Option::<&Node>::None,
                            None,
                            None,
                        )?,
                    ]),
                )?;
            };
        }
        Ok(related)
    }

    pub(super) fn index_signatures_related_to(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        source_is_primitive: bool,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        if Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation) {
            return self.index_signatures_identical_to(source, target);
        }
        let index_infos = self.type_checker.get_index_infos_of_type(target)?;
        let target_has_string_index = some(
            Some(&index_infos),
            Some(|info: &Gc<IndexInfo>| info.key_type == self.type_checker.string_type()),
        );
        let mut result = Ternary::True;
        for target_info in &index_infos {
            let related = if !source_is_primitive
                && target_has_string_index
                && self
                    .type_checker
                    .type_(target_info.type_)
                    .flags()
                    .intersects(TypeFlags::Any)
            {
                Ternary::True
            } else if self.type_checker.is_generic_mapped_type(source)? && target_has_string_index {
                self.is_related_to(
                    self.type_checker
                        .get_template_type_from_mapped_type(source)?,
                    target_info.type_,
                    Some(RecursionFlags::Both),
                    Some(report_errors),
                    None,
                    None,
                )?
            } else {
                self.type_related_to_index_info(
                    source,
                    target_info,
                    report_errors,
                    intersection_state,
                )?
            };
            if related == Ternary::False {
                return Ok(Ternary::False);
            }
            result &= related;
        }
        Ok(result)
    }

    pub(super) fn type_related_to_index_info(
        &self,
        source: Id<Type>,
        target_info: &IndexInfo,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        let source_info = self
            .type_checker
            .get_applicable_index_info(source, target_info.key_type)?;
        if let Some(source_info) = source_info.as_ref() {
            return self.index_info_related_to(source_info, target_info, report_errors);
        }
        if !intersection_state.intersects(IntersectionState::Source)
            && self
                .type_checker
                .is_object_type_with_inferable_index(source)?
        {
            return self.members_related_to_index_info(source, target_info, report_errors);
        }
        if report_errors {
            self.report_error(
                Cow::Borrowed(&Diagnostics::Index_signature_for_type_0_is_missing_in_type_1),
                Some(vec![
                    self.type_checker.type_to_string_(
                        target_info.key_type,
                        Option::<&Node>::None,
                        None,
                        None,
                    )?,
                    self.type_checker
                        .type_to_string_(source, Option::<&Node>::None, None, None)?,
                ]),
            )?;
        }
        Ok(Ternary::False)
    }

    pub(super) fn index_signatures_identical_to(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<Ternary> {
        let source_infos = self.type_checker.get_index_infos_of_type(source)?;
        let target_infos = self.type_checker.get_index_infos_of_type(target)?;
        if source_infos.len() != target_infos.len() {
            return Ok(Ternary::False);
        }
        for target_info in &target_infos {
            let source_info = self
                .type_checker
                .get_index_info_of_type_(source, target_info.key_type)?;
            if !matches!(
                source_info.as_ref(),
                Some(source_info) if self.is_related_to(
                    source_info.type_,
                    target_info.type_,
                    Some(RecursionFlags::Both),
                    None, None, None
                )? != Ternary::False && source_info.is_readonly == target_info.is_readonly
            ) {
                return Ok(Ternary::False);
            }
        }
        Ok(Ternary::True)
    }

    pub(super) fn constructor_visibilities_are_compatible(
        &self,
        source_signature: &Signature,
        target_signature: &Signature,
        report_errors: bool,
    ) -> io::Result<bool> {
        if source_signature.declaration.is_none() || target_signature.declaration.is_none() {
            return Ok(true);
        }
        let source_signature_declaration = source_signature.declaration.as_ref().unwrap();
        let target_signature_declaration = target_signature.declaration.as_ref().unwrap();

        let source_accessibility = get_selected_effective_modifier_flags(
            source_signature_declaration,
            ModifierFlags::NonPublicAccessibilityModifier,
        );
        let target_accessibility = get_selected_effective_modifier_flags(
            target_signature_declaration,
            ModifierFlags::NonPublicAccessibilityModifier,
        );

        if target_accessibility == ModifierFlags::Private {
            return Ok(true);
        }

        if target_accessibility == ModifierFlags::Protected
            && source_accessibility != ModifierFlags::Private
        {
            return Ok(true);
        }

        if target_accessibility != ModifierFlags::Protected
            && source_accessibility == ModifierFlags::None
        {
            return Ok(true);
        }

        if report_errors {
            self.report_error(
                Cow::Borrowed(
                    &Diagnostics::Cannot_assign_a_0_constructor_type_to_a_1_constructor_type,
                ),
                Some(vec![
                    self.type_checker
                        .visibility_to_string(source_accessibility)
                        .to_owned(),
                    self.type_checker
                        .visibility_to_string(target_accessibility)
                        .to_owned(),
                ]),
            )?;
        }

        Ok(false)
    }
}

impl TypeChecker {
    pub(super) fn type_could_have_top_level_singleton_types(
        &self,
        type_: Id<Type>,
    ) -> io::Result<bool> {
        if self.type_(type_).flags().intersects(TypeFlags::Boolean) {
            return Ok(false);
        }

        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
        {
            return try_for_each_bool(
                self.type_(type_)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |&type_: &Id<Type>, _| self.type_could_have_top_level_singleton_types(type_),
            );
        }

        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Instantiable)
        {
            let constraint = self.get_constraint_of_type(type_)?;
            if let Some(constraint) = constraint.filter(|&constraint| constraint != type_) {
                return self.type_could_have_top_level_singleton_types(constraint);
            }
        }

        Ok(self.is_unit_type(type_)
            || self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::TemplateLiteral))
    }

    pub(super) fn get_exact_optional_unassignable_properties(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<Vec<Id<Symbol>>> {
        if self.is_tuple_type(source) && self.is_tuple_type(target) {
            return Ok(vec![]);
        }
        try_filter(
            &self.get_properties_of_type(target)?.collect_vec(),
            |&target_prop: &Id<Symbol>| -> io::Result<_> {
                Ok(self.is_exact_optional_property_mismatch(
                    self.get_type_of_property_of_type_(
                        source,
                        self.symbol(target_prop).escaped_name(),
                    )?,
                    Some(self.get_type_of_symbol(target_prop)?),
                ))
            },
        )
    }

    pub(super) fn is_exact_optional_property_mismatch(
        &self,
        source: Option<Id<Type>>,
        target: Option<Id<Type>>,
    ) -> bool {
        if source.is_none() || target.is_none() {
            return false;
        }
        let source = source.unwrap();
        let target = target.unwrap();
        self.maybe_type_of_kind(source, TypeFlags::Undefined) && self.contains_missing_type(target)
    }

    pub fn get_exact_optional_properties(&self, type_: Id<Type>) -> io::Result<Vec<Id<Symbol>>> {
        try_filter(
            &self.get_properties_of_type(type_)?.collect_vec(),
            |&target_prop: &Id<Symbol>| -> io::Result<_> {
                Ok(self.contains_missing_type(self.get_type_of_symbol(target_prop)?))
            },
        )
    }

    pub(super) fn get_best_matching_type(
        &self,
        source: Id<Type>,
        target: Id<Type>, /*UnionOrIntersectionType*/
        is_related_to: Option<impl Fn(Id<Type>, Id<Type>) -> io::Result<Ternary>>,
    ) -> io::Result<Option<Id<Type>>> {
        let is_related_to = |source: Id<Type>, target: Id<Type>| {
            Ok(match is_related_to.as_ref() {
                None => self.compare_types_assignable(source, target)?,
                Some(is_related_to) => is_related_to(source, target)?,
            })
        };
        self.find_matching_discriminant_type(source, target, is_related_to, Some(true))?
            .or_else(|| self.find_matching_type_reference_or_type_alias_reference(source, target))
            .try_or_else(|| self.find_best_type_for_object_literal(source, target))?
            .try_or_else(|| self.find_best_type_for_invokable(source, target))?
            .try_or_else(|| self.find_most_overlappy_type(source, target))
    }

    pub(super) fn discriminate_type_by_discriminable_items(
        &self,
        target: Id<Type>, /*UnionType*/
        discriminators: impl IntoIterator<Item = (Box<dyn Fn() -> io::Result<Id<Type>>>, __String)>,
        mut related: impl FnMut(Id<Type>, Id<Type>) -> io::Result<bool>,
        default_value: Option<Id<Type>>,
        skip_partial: Option<bool>,
    ) -> io::Result<Option<Id<Type>>> {
        let target_ref = self.type_(target);
        let target_types = target_ref.as_union_type().types();
        let mut discriminable: Vec<Option<bool>> = target_types
            .into_iter()
            .map(|_| Option::<bool>::None)
            .collect();
        for (get_discriminating_type, ref property_name) in discriminators {
            let target_prop =
                self.get_union_or_intersection_property(target, property_name, None)?;
            if matches!(skip_partial, Some(true))
                && matches!(
                    target_prop,
                    Some(target_prop) if get_check_flags(&self.symbol(target_prop)).intersects(CheckFlags::ReadPartial)
                )
            {
                continue;
            }
            let mut i = 0;
            for &type_ in target_types {
                let target_type = self.get_type_of_property_of_type_(type_, property_name)?;
                if matches!(
                    target_type,
                    Some(target_type) if related(get_discriminating_type()?, target_type)?
                ) {
                    discriminable[i] = if discriminable[i].is_none() {
                        Some(true)
                    } else {
                        discriminable[i]
                    };
                } else {
                    discriminable[i] = Some(false);
                }
                i += 1;
            }
        }
        let match_ = discriminable.iter().position(|item| *item == Some(true));
        if match_.is_none() {
            return Ok(default_value);
        }
        let match_ = match_.unwrap();
        let mut next_match = discriminable
            .iter()
            .skip(match_ + 1)
            .position(|item| *item == Some(true))
            .map(|position| position + match_ + 1);
        while let Some(next_match_present) = next_match {
            if !self.is_type_identical_to(target_types[match_], target_types[next_match_present])? {
                return Ok(default_value);
            }
            next_match = discriminable
                .iter()
                .skip(next_match_present + 1)
                .position(|item| *item == Some(true))
                .map(|position| position + next_match_present + 1);
        }
        Ok(Some(target_types[match_].clone()))
    }

    pub(super) fn is_weak_type(&self, type_: Id<Type>) -> io::Result<bool> {
        if self.type_(type_).flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_)?;
            return Ok(self
                .type_(resolved)
                .as_resolved_type()
                .call_signatures()
                .is_empty()
                && self
                    .type_(resolved)
                    .as_resolved_type()
                    .construct_signatures()
                    .is_empty()
                && self
                    .type_(resolved)
                    .as_resolved_type()
                    .index_infos()
                    .is_empty()
                && !self
                    .type_(resolved)
                    .as_resolved_type()
                    .properties()
                    .is_empty()
                && every(
                    &*self.type_(resolved).as_resolved_type().properties(),
                    |&p: &Id<Symbol>, _| self.symbol(p).flags().intersects(SymbolFlags::Optional),
                ));
        }
        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Intersection)
        {
            return try_every(
                self.type_(type_)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |&type_: &Id<Type>, _| self.is_weak_type(type_),
            );
        }
        Ok(false)
    }

    pub(super) fn has_common_properties(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        is_comparing_jsx_attributes: bool,
    ) -> io::Result<bool> {
        for prop in self.get_properties_of_type(source)? {
            if self.is_known_property(
                target,
                self.symbol(prop).escaped_name(),
                is_comparing_jsx_attributes,
            )? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn get_marker_type_reference(
        &self,
        type_: Id<Type>,  /*GenericType*/
        source: Id<Type>, /*TypeParameter*/
        target: Id<Type>,
    ) -> Id<Type /*TypeReference*/> {
        let result = self.create_type_reference(
            type_,
            maybe_map(
                {
                    let type_parameters = self
                        .type_(type_)
                        .as_generic_type()
                        .maybe_type_parameters()
                        .map(ToOwned::to_owned);
                    type_parameters
                }
                .as_deref(),
                |&t: &Id<Type>, _| {
                    if t == source {
                        target
                    } else {
                        t.clone()
                    }
                },
            ),
        );
        self.type_(result).as_type_reference().set_object_flags(
            self.type_(result).as_type_reference().object_flags() | ObjectFlags::MarkerType,
        );
        result
    }

    pub(super) fn get_alias_variances(&self, symbol: Id<Symbol>) -> io::Result<Vec<VarianceFlags>> {
        let links = self.get_symbol_links(symbol);
        let links_type_parameters = (*links).borrow().type_parameters.clone();
        let ret = self.try_get_variances_worker(
            links_type_parameters.as_deref(),
            links.clone(),
            |_links: &GetVariancesCache,
             param: Id<Type>, /*TypeParameter*/
             marker: Id<Type>|
             -> io::Result<_> {
                let type_ = self.get_type_alias_instantiation(
                    symbol,
                    self.instantiate_types(
                        {
                            let value = (*links).borrow().type_parameters.clone();
                            value
                        }
                        .as_deref(),
                        Some(self.make_unary_type_mapper(param, marker)),
                    )?
                    .as_deref(),
                    Option::<Id<Symbol>>::None,
                    None,
                )?;
                self.type_(type_)
                    .set_alias_type_arguments_contains_marker(Some(true));
                Ok(type_)
            },
        )?;
        Ok(ret)
    }

    pub(super) fn get_variances_worker(
        &self,
        type_parameters: Option<&[Id<Type /*TypeParameter*/>]>,
        cache: impl Into<GetVariancesCache>,
        mut create_marker_type: impl FnMut(
            &GetVariancesCache,
            Id<Type>, /*TypeParameter*/
            Id<Type>,
        ) -> Id<Type>,
    ) -> Vec<VarianceFlags> {
        self.try_get_variances_worker(
            type_parameters,
            cache,
            |a: &GetVariancesCache, b: Id<Type>, c: Id<Type>| Ok(create_marker_type(a, b, c)),
        )
        .unwrap()
    }

    pub(super) fn try_get_variances_worker(
        &self,
        type_parameters: Option<&[Id<Type /*TypeParameter*/>]>,
        cache: impl Into<GetVariancesCache>,
        mut create_marker_type: impl FnMut(
            &GetVariancesCache,
            Id<Type>, /*TypeParameter*/
            Id<Type>,
        ) -> io::Result<Id<Type>>,
    ) -> io::Result<Vec<VarianceFlags>> {
        let type_parameters = type_parameters.map_or_else(|| vec![], ToOwned::to_owned);
        let cache = cache.into();
        if (*cache.maybe_variances(self)).borrow().is_none() {
            // tracing?.push(tracing.Phase.CheckTypes, "getVariancesWorker", { arity: typeParameters.length, id: (cache as any).id ?? (cache as any).declaredType?.id ?? -1 });
            *cache.maybe_variances(self).borrow_mut() = Some(vec![]);
            let mut variances: Vec<VarianceFlags> = vec![];
            for &tp in &type_parameters {
                let unmeasurable: Rc<Cell<bool>> = Rc::new(Cell::new(false));
                let unreliable: Rc<Cell<bool>> = Rc::new(Cell::new(false));
                let old_handler = self.maybe_outofband_variance_marker_handler();
                self.set_outofband_variance_marker_handler(Some(Gc::new(Box::new(
                    GetVariancesWorkerOutofbandVarianceMarkerHandler::new(
                        unmeasurable.clone(),
                        unreliable.clone(),
                    ),
                ))));
                let type_with_super = create_marker_type(&cache, tp, self.marker_super_type())?;
                let type_with_sub = create_marker_type(&cache, tp, self.marker_sub_type())?;
                let mut variance =
                    if self.is_type_assignable_to(type_with_sub, type_with_super)? {
                        VarianceFlags::Covariant
                    } else {
                        VarianceFlags::Invariant
                    } | if self.is_type_assignable_to(type_with_super, type_with_sub)? {
                        VarianceFlags::Contravariant
                    } else {
                        VarianceFlags::Invariant
                    };
                if variance == VarianceFlags::Bivariant
                    && self.is_type_assignable_to(
                        create_marker_type(&cache, tp, self.marker_other_type())?,
                        type_with_super,
                    )?
                {
                    variance = VarianceFlags::Independent;
                }
                self.set_outofband_variance_marker_handler(old_handler);
                if unmeasurable.get() || unreliable.get() {
                    if unmeasurable.get() {
                        variance |= VarianceFlags::Unmeasurable;
                    }
                    if unreliable.get() {
                        variance |= VarianceFlags::Unreliable;
                    }
                }
                variances.push(variance);
            }
            *cache.maybe_variances(self).borrow_mut() = Some(variances);
            // tracing?.pop();
        }
        let ret = (*cache.maybe_variances(self)).borrow().clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_variances(&self, type_: Id<Type> /*GenericType*/) -> Vec<VarianceFlags> {
        if type_ == self.global_array_type()
            || type_ == self.global_readonly_array_type()
            || self
                .type_(type_)
                .as_generic_type()
                .object_flags()
                .intersects(ObjectFlags::Tuple)
        {
            return self.array_variances();
        }
        self.get_variances_worker(
            {
                let type_parameters = self
                    .type_(type_)
                    .as_generic_type()
                    .maybe_type_parameters()
                    .map(ToOwned::to_owned);
                type_parameters
            }
            .as_deref(),
            type_,
            |input: &GetVariancesCache, param: Id<Type>, marker: Id<Type>| {
                self.get_marker_type_reference(
                    // enum_unwrapped!(input, [GetVariancesCache, GenericType]),
                    match input {
                        GetVariancesCache::GenericType(input) => *input,
                        _ => panic!("Expected GenericType"),
                    },
                    param,
                    marker,
                )
            },
        )
    }

    pub(super) fn has_covariant_void_argument(
        &self,
        type_arguments: &[Id<Type>],
        variances: &[VarianceFlags],
    ) -> bool {
        for i in 0..variances.len() {
            if variances[i] & VarianceFlags::VarianceMask == VarianceFlags::Covariant
                && self
                    .type_(type_arguments[i])
                    .flags()
                    .intersects(TypeFlags::Void)
            {
                return true;
            }
        }
        false
    }

    pub(super) fn is_unconstrained_type_parameter(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::TypeParameter)
            && self.get_constraint_of_type_parameter(type_)?.is_none())
    }

    pub(super) fn is_non_deferred_type_reference(&self, type_: Id<Type>) -> bool {
        get_object_flags(&self.type_(type_)).intersects(ObjectFlags::Reference)
            && self
                .type_(type_)
                .as_type_reference_interface()
                .maybe_node()
                .is_none()
    }

    pub(super) fn is_type_reference_with_generic_arguments(
        &self,
        type_: Id<Type>,
    ) -> io::Result<bool> {
        Ok(self.is_non_deferred_type_reference(type_)
            && try_some(
                Some(&*self.get_type_arguments(type_)?),
                Some(|&t: &Id<Type>| -> io::Result<_> {
                    Ok(self.type_(t).flags().intersects(TypeFlags::TypeParameter)
                        || self.is_type_reference_with_generic_arguments(t)?)
                }),
            )?)
    }

    pub(super) fn get_type_reference_id(
        &self,
        type_: Id<Type>, /*TypeReference*/
        type_parameters: &mut Vec<Id<Type>>,
        depth: Option<usize>,
    ) -> io::Result<String> {
        let depth = depth.unwrap_or(0);
        let mut result = self
            .type_(self.type_(type_).as_type_reference_interface().target())
            .id()
            .to_string();
        for &t in &self.get_type_arguments(type_)? {
            if self.is_unconstrained_type_parameter(t)? {
                let mut index = type_parameters.iter().position(|&type_| type_ == t);
                if index.is_none() {
                    index = Some(type_parameters.len());
                    type_parameters.push(t.clone());
                }
                let index = index.unwrap();
                result.push_str(&format!("={}", index));
            } else if depth < 4 && self.is_type_reference_with_generic_arguments(t)? {
                result.push_str(&format!(
                    "<{}>",
                    self.get_type_reference_id(t, type_parameters, Some(depth + 1))?
                ));
            } else {
                result.push_str(&format!("-{}", self.type_(t).id()));
            }
        }
        Ok(result)
    }

    pub(super) fn get_relation_key(
        &self,
        mut source: Id<Type>,
        mut target: Id<Type>,
        intersection_state: IntersectionState,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> io::Result<String> {
        if ptr::eq(relation, &*self.identity_relation())
            && self.type_(source).id() > self.type_(target).id()
        {
            let temp = source.clone();
            source = target.clone();
            target = temp;
        }
        let post_fix = if intersection_state != IntersectionState::None {
            format!(":{}", intersection_state.bits())
        } else {
            "".to_owned()
        };
        if self.is_type_reference_with_generic_arguments(source)?
            && self.is_type_reference_with_generic_arguments(target)?
        {
            let mut type_parameters: Vec<Id<Type>> = vec![];
            return Ok(format!(
                "{},{}{}",
                self.get_type_reference_id(source, &mut type_parameters, None)?,
                self.get_type_reference_id(target, &mut type_parameters, None)?,
                post_fix
            ));
        }
        Ok(format!(
            "{},{}{}",
            self.type_(source).id(),
            self.type_(target).id(),
            post_fix
        ))
    }

    pub(super) fn for_each_property<TReturn>(
        &self,
        prop: Id<Symbol>,
        callback: &mut impl FnMut(Id<Symbol>) -> io::Result<Option<TReturn>>,
    ) -> io::Result<Option<TReturn>> {
        if get_check_flags(&self.symbol(prop)).intersects(CheckFlags::Synthetic) {
            for &t in self
                .type_(
                    (*self.symbol(prop).as_transient_symbol().symbol_links())
                        .borrow()
                        .containing_type
                        .unwrap(),
                )
                .as_union_or_intersection_type_interface()
                .types()
            {
                let p = self.get_property_of_type_(t, self.symbol(prop).escaped_name(), None)?;
                let result = p.try_and_then(|p| self.for_each_property(p, callback))?;
                if result.is_some() {
                    return Ok(result);
                }
            }
            return Ok(None);
        }
        callback(prop)
    }

    pub(super) fn for_each_property_bool(
        &self,
        prop: Id<Symbol>,
        callback: &mut impl FnMut(Id<Symbol>) -> io::Result<bool>,
    ) -> io::Result<bool> {
        Ok(self
            .for_each_property(prop, &mut |symbol: Id<Symbol>| {
                Ok(if callback(symbol)? { Some(()) } else { None })
            })?
            .is_some())
    }

    pub(super) fn get_declaring_class(
        &self,
        prop: Id<Symbol>,
    ) -> io::Result<Option<Id<Type /*InterfaceType*/>>> {
        self.symbol(prop)
            .maybe_parent()
            .filter(|prop_parent| prop_parent.flags().intersects(SymbolFlags::Class))
            .try_map(|_prop_parent| {
                self.get_declared_type_of_symbol(self.get_parent_of_symbol(prop)?.unwrap())
            })
    }

    pub(super) fn get_type_of_property_in_base_class(
        &self,
        property: Id<Symbol>,
    ) -> io::Result<Option<Id<Type>>> {
        let class_type = self.get_declaring_class(property)?;
        let base_class_type = class_type.try_and_then(|class_type| -> io::Result<_> {
            Ok(self.get_base_types(class_type)?.get(0).cloned())
        })?;
        base_class_type.try_and_then(|base_class_type| {
            self.get_type_of_property_of_type_(
                base_class_type,
                self.symbol(property).escaped_name(),
            )
        })
    }

    pub(super) fn is_property_in_class_derived_from(
        &self,
        prop: Id<Symbol>,
        base_class: Option<Id<Type>>,
    ) -> io::Result<bool> {
        self.for_each_property_bool(prop, &mut |sp: Id<Symbol>| {
            let source_class = self.get_declaring_class(sp)?;
            Ok(if let Some(source_class) = source_class {
                self.has_base_type(source_class, base_class)?
            } else {
                false
            })
        })
    }

    pub(super) fn is_valid_override_of(
        &self,
        source_prop: Id<Symbol>,
        target_prop: Id<Symbol>,
    ) -> io::Result<bool> {
        Ok(
            !self.for_each_property_bool(target_prop, &mut |tp: Id<Symbol>| {
                Ok(
                    if get_declaration_modifier_flags_from_symbol(self.arena(), tp, None)
                        .intersects(ModifierFlags::Protected)
                    {
                        !self.is_property_in_class_derived_from(
                            source_prop,
                            self.get_declaring_class(tp)?,
                        )?
                    } else {
                        false
                    },
                )
            })?,
        )
    }

    pub(super) fn is_class_derived_from_declaring_classes(
        &self,
        check_class: Id<Type>,
        prop: Id<Symbol>,
        writing: bool,
    ) -> io::Result<Option<Id<Type>>> {
        Ok(
            if self.for_each_property_bool(prop, &mut |p: Id<Symbol>| {
                Ok(
                    if get_declaration_modifier_flags_from_symbol(self.arena(), p, Some(writing))
                        .intersects(ModifierFlags::Protected)
                    {
                        !self.has_base_type(check_class, self.get_declaring_class(p)?)?
                    } else {
                        false
                    },
                )
            })? {
                None
            } else {
                Some(check_class)
            },
        )
    }

    pub(super) fn is_deeply_nested_type(
        &self,
        type_: Id<Type>,
        stack: &[Id<Type>],
        depth: usize,
        max_depth: Option<usize>,
    ) -> bool {
        let max_depth = max_depth.unwrap_or(5);
        if depth >= max_depth {
            let identity = self.get_recursion_identity(type_);
            let mut count = 0;
            for i in 0..depth {
                if self.get_recursion_identity(stack[i]) == identity {
                    count += 1;
                    if count >= max_depth {
                        return true;
                    }
                }
            }
        }
        false
    }

    pub(super) fn get_recursion_identity(&self, mut type_: Id<Type>) -> RecursionIdentity {
        if self.type_(type_).flags().intersects(TypeFlags::Object)
            && !self.is_object_or_array_literal_type(type_)
        {
            if get_object_flags(&self.type_(type_)).intersects(ObjectFlags::Reference) {
                if let Some(type_node) = self
                    .type_(type_)
                    .as_type_reference_interface()
                    .maybe_node()
                    .clone()
                {
                    return type_node.into();
                }
            }
            if let Some(type_symbol) = self.type_(type_).maybe_symbol().filter(|&type_symbol| {
                !(get_object_flags(&self.type_(type_)).intersects(ObjectFlags::Anonymous)
                    && self
                        .symbol(type_symbol)
                        .flags()
                        .intersects(SymbolFlags::Class))
            }) {
                return type_symbol.into();
            }
            if self.is_tuple_type(type_) {
                return self
                    .type_(type_)
                    .as_type_reference_interface()
                    .target()
                    .into();
            }
        }
        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::TypeParameter)
        {
            return match self.type_(type_).maybe_symbol() {
                None => RecursionIdentity::None,
                Some(type_symbol) => type_symbol.into(),
            };
        }
        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::IndexedAccess)
        {
            while {
                type_ = self
                    .type_(type_)
                    .as_indexed_access_type()
                    .object_type
                    .clone();
                self.type_(type_)
                    .flags()
                    .intersects(TypeFlags::IndexedAccess)
            } {}
            return type_.into();
        }
        if self.type_(type_).flags().intersects(TypeFlags::Conditional) {
            return self.type_(type_).as_conditional_type().root.clone().into();
        }
        type_.into()
    }

    pub(super) fn is_property_identical_to(
        &self,
        source_prop: Id<Symbol>,
        target_prop: Id<Symbol>,
    ) -> io::Result<bool> {
        Ok(
            self.compare_properties(source_prop, target_prop, |a: Id<Type>, b: Id<Type>| {
                self.compare_types_identical(a, b)
            })? != Ternary::False,
        )
    }

    pub(super) fn compare_properties(
        &self,
        source_prop: Id<Symbol>,
        target_prop: Id<Symbol>,
        mut compare_types: impl FnMut(Id<Type>, Id<Type>) -> io::Result<Ternary>,
    ) -> io::Result<Ternary> {
        if source_prop == target_prop {
            return Ok(Ternary::True);
        }
        let source_prop_accessibility =
            get_declaration_modifier_flags_from_symbol(self.arena(), source_prop, None)
                & ModifierFlags::NonPublicAccessibilityModifier;
        let target_prop_accessibility =
            get_declaration_modifier_flags_from_symbol(self.arena(), target_prop, None)
                & ModifierFlags::NonPublicAccessibilityModifier;
        if source_prop_accessibility != target_prop_accessibility {
            return Ok(Ternary::False);
        }
        if source_prop_accessibility != ModifierFlags::None {
            if 
                self.get_target_symbol(source_prop) !=
                self.get_target_symbol(target_prop)
            {
                return Ok(Ternary::False);
            }
        } else {
            if self.symbol(source_prop).flags() & SymbolFlags::Optional
                != self.symbol(target_prop).flags() & SymbolFlags::Optional
            {
                return Ok(Ternary::False);
            }
        }
        if self.is_readonly_symbol(source_prop)? != self.is_readonly_symbol(target_prop)? {
            return Ok(Ternary::False);
        }
        compare_types(
            self.get_type_of_symbol(source_prop)?,
            self.get_type_of_symbol(target_prop)?,
        )
    }

    pub(super) fn is_matching_signature(
        &self,
        source: &Signature,
        target: &Signature,
        partial_match: bool,
    ) -> io::Result<bool> {
        let source_parameter_count = self.get_parameter_count(source)?;
        let target_parameter_count = self.get_parameter_count(target)?;
        let source_min_argument_count = self.get_min_argument_count(source, None)?;
        let target_min_argument_count = self.get_min_argument_count(target, None)?;
        let source_has_rest_parameter = self.has_effective_rest_parameter(source)?;
        let target_has_rest_parameter = self.has_effective_rest_parameter(target)?;
        if source_parameter_count == target_parameter_count
            && source_min_argument_count == target_min_argument_count
            && source_has_rest_parameter == target_has_rest_parameter
        {
            return Ok(true);
        }
        if partial_match && source_min_argument_count <= target_min_argument_count {
            return Ok(true);
        }
        Ok(false)
    }
}

pub(super) enum GetVariancesCache {
    SymbolLinks(Gc<GcCell<SymbolLinks>>),
    GenericType(Id<Type /*GenericType*/>),
}

impl GetVariancesCache {
    pub(super) fn maybe_variances(
        &self,
        type_checker: &TypeChecker,
    ) -> Rc<RefCell<Option<Vec<VarianceFlags>>>> {
        match self {
            Self::SymbolLinks(symbol_links) => (**symbol_links).borrow().variances.clone(),
            Self::GenericType(generic_type) => type_checker
                .type_(*generic_type)
                .as_generic_type()
                .maybe_variances(),
        }
    }
}

impl From<Gc<GcCell<SymbolLinks>>> for GetVariancesCache {
    fn from(value: Gc<GcCell<SymbolLinks>>) -> Self {
        Self::SymbolLinks(value)
    }
}

impl From<Id<Type>> for GetVariancesCache {
    fn from(value: Id<Type>) -> Self {
        Self::GenericType(value)
    }
}

#[derive(Trace, Finalize)]
pub(super) enum RecursionIdentity {
    Node(Gc<Node>),
    Symbol(Id<Symbol>),
    Type(Id<Type>),
    ConditionalRoot(Gc<GcCell<ConditionalRoot>>),
    None,
}

impl PartialEq for RecursionIdentity {
    fn eq(&self, other: &RecursionIdentity) -> bool {
        match (self, other) {
            (Self::Node(a), Self::Node(b)) => Gc::ptr_eq(a, b),
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::Type(a), Self::Type(b)) => a == b,
            (Self::ConditionalRoot(a), Self::ConditionalRoot(b)) => Gc::ptr_eq(a, b),
            (Self::None, Self::None) => true,
            _ => false,
        }
    }
}

impl Eq for RecursionIdentity {}

impl From<Gc<Node>> for RecursionIdentity {
    fn from(value: Gc<Node>) -> Self {
        Self::Node(value)
    }
}

impl From<Id<Symbol>> for RecursionIdentity {
    fn from(value: Id<Symbol>) -> Self {
        Self::Symbol(value)
    }
}

impl From<Id<Type>> for RecursionIdentity {
    fn from(value: Id<Type>) -> Self {
        Self::Type(value)
    }
}

impl From<Gc<GcCell<ConditionalRoot>>> for RecursionIdentity {
    fn from(value: Gc<GcCell<ConditionalRoot>>) -> Self {
        Self::ConditionalRoot(value)
    }
}

#[derive(Trace, Finalize)]
struct GetVariancesWorkerOutofbandVarianceMarkerHandler {
    #[unsafe_ignore_trace]
    unmeasurable: Rc<Cell<bool>>,
    #[unsafe_ignore_trace]
    unreliable: Rc<Cell<bool>>,
}

impl GetVariancesWorkerOutofbandVarianceMarkerHandler {
    pub fn new(unmeasurable: Rc<Cell<bool>>, unreliable: Rc<Cell<bool>>) -> Self {
        Self {
            unmeasurable,
            unreliable,
        }
    }
}

impl OutofbandVarianceMarkerHandler for GetVariancesWorkerOutofbandVarianceMarkerHandler {
    fn call(&self, only_unreliable: bool) {
        if only_unreliable {
            self.unreliable.set(true);
        } else {
            self.unmeasurable.set(true);
        }
    }
}
