use std::{cmp, io, iter::once, ptr};

use id_arena::Id;
use itertools::Either;
use peekmore::PeekMore;

use super::InferTypes;
use crate::{
    every, find, flat_map, get_object_flags, try_some, DiagnosticMessage, Diagnostics,
    ElementFlags, HasArena, InArena, InferenceContext, InferenceFlags, InferenceInfo,
    InferencePriority, Node, NodeInterface, ObjectFlags, OptionTry, PeekMoreExt, PeekableExt,
    Signature, SignatureKind, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Ternary, Type,
    TypeChecker, TypeFlags, TypeInterface, UnionReduction, VecExt,
};

impl InferTypes {
    pub(super) fn get_inference_info_for_type(&self, type_: Id<Type>) -> Option<Id<InferenceInfo>> {
        if type_.ref_(self).flags().intersects(TypeFlags::TypeVariable) {
            for inference in &self.inferences {
                if type_ == inference.ref_(self).type_parameter {
                    return Some(inference.clone());
                }
            }
        }
        None
    }

    pub(super) fn get_single_type_variable_from_intersection_types(
        &self,
        types: &[Id<Type>],
    ) -> Option<Id<Type>> {
        let mut type_variable: Option<Id<Type>> = None;
        for &type_ in types {
            let t = if type_.ref_(self).flags().intersects(TypeFlags::Intersection) {
                find(
                    type_
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types(),
                    |&t: &Id<Type>, _| self.get_inference_info_for_type(t).is_some(),
                )
                .map(Clone::clone)
            } else {
                None
            };
            if t.is_none() {
                return None;
            }
            let t = t.unwrap();
            if matches!(
                type_variable,
                Some(type_variable) if t != type_variable
            ) {
                return None;
            }
            type_variable = Some(t);
        }
        type_variable
    }

    pub(super) fn infer_to_multiple_types(
        &self,
        source: Id<Type>,
        targets: &[Id<Type>],
        target_flags: TypeFlags,
    ) -> io::Result<()> {
        let mut type_variable_count = 0;
        if target_flags.intersects(TypeFlags::Union) {
            let mut naked_type_variable: Option<Id<Type>> = None;
            let sources = if source.ref_(self).flags().intersects(TypeFlags::Union) {
                source
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types()
                    .to_owned()
            } else {
                vec![source]
            };
            let mut matched = vec![false; sources.len()];
            let mut inference_circularity = false;
            for &t in targets {
                if self.get_inference_info_for_type(t).is_some() {
                    naked_type_variable = Some(t.clone());
                    type_variable_count += 1;
                } else {
                    for i in 0..sources.len() {
                        let save_inference_priority = self.inference_priority();
                        self.set_inference_priority(InferencePriority::MaxValue);
                        self.infer_from_types(sources[i], t)?;
                        if self.inference_priority() == self.priority() {
                            matched[i] = true;
                        }
                        inference_circularity = inference_circularity
                            || self.inference_priority() == InferencePriority::Circularity;
                        self.set_inference_priority(cmp::min(
                            self.inference_priority(),
                            save_inference_priority,
                        ));
                    }
                }
            }
            if type_variable_count == 0 {
                let intersection_type_variable =
                    self.get_single_type_variable_from_intersection_types(targets);
                if let Some(intersection_type_variable) = intersection_type_variable {
                    self.infer_with_priority(
                        source,
                        intersection_type_variable,
                        InferencePriority::NakedTypeVariable,
                    )?;
                }
                return Ok(());
            }
            if type_variable_count == 1 && !inference_circularity {
                let unmatched = flat_map(Some(&sources), |s: &Id<Type>, i| {
                    if matched[i] {
                        vec![]
                    } else {
                        vec![s.clone()]
                    }
                });
                if !unmatched.is_empty() {
                    self.infer_from_types(
                        self.type_checker.ref_(self).get_union_type(
                            &unmatched,
                            None,
                            Option::<Id<Symbol>>::None,
                            None,
                            None,
                        )?,
                        naked_type_variable.unwrap(),
                    )?;
                    return Ok(());
                }
            }
        } else {
            for &t in targets {
                if self.get_inference_info_for_type(t).is_some() {
                    type_variable_count += 1;
                } else {
                    self.infer_from_types(source, t)?;
                }
            }
        }
        if if target_flags.intersects(TypeFlags::Intersection) {
            type_variable_count == 1
        } else {
            type_variable_count > 0
        } {
            for &t in targets {
                if self.get_inference_info_for_type(t).is_some() {
                    self.infer_with_priority(source, t, InferencePriority::NakedTypeVariable)?;
                }
            }
        }

        Ok(())
    }

    pub(super) fn infer_to_mapped_type(
        &self,
        source: Id<Type>,
        target: Id<Type>, /*MappedType*/
        constraint_type: Id<Type>,
    ) -> io::Result<bool> {
        if constraint_type
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Union)
        {
            let mut result = false;
            for &type_ in constraint_type
                .ref_(self)
                .as_union_or_intersection_type_interface()
                .types()
            {
                result = self.infer_to_mapped_type(source, target, type_)? || result;
            }
            return Ok(result);
        }
        if constraint_type
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Index)
        {
            let inference =
                self.get_inference_info_for_type(constraint_type.ref_(self).as_index_type().type_);
            if let Some(inference) = inference.as_ref().filter(|inference| {
                !inference.ref_(self).is_fixed()
                    && !self
                        .type_checker
                        .ref_(self)
                        .is_from_inference_blocked_source(source)
            }) {
                let inferred_type = self
                    .type_checker
                    .ref_(self)
                    .infer_type_for_homomorphic_mapped_type(source, target, constraint_type)?;
                if let Some(inferred_type) = inferred_type {
                    self.infer_with_priority(
                        inferred_type,
                        inference.ref_(self).type_parameter,
                        if get_object_flags(&source.ref_(self))
                            .intersects(ObjectFlags::NonInferrableType)
                        {
                            InferencePriority::PartialHomomorphicMappedType
                        } else {
                            InferencePriority::HomomorphicMappedType
                        },
                    )?;
                }
            }
            return Ok(true);
        }
        if constraint_type
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TypeParameter)
        {
            self.infer_with_priority(
                self.type_checker
                    .ref_(self)
                    .get_index_type(source, None, None)?,
                constraint_type,
                InferencePriority::MappedTypeConstraint,
            )?;
            let extended_constraint = self
                .type_checker
                .ref_(self)
                .get_constraint_of_type(constraint_type)?;
            if matches!(
                extended_constraint,
                Some(extended_constraint) if self.infer_to_mapped_type(source, target, extended_constraint)?
            ) {
                return Ok(true);
            }
            let prop_types = self
                .type_checker
                .ref_(self)
                .get_properties_of_type(source)?
                .into_iter()
                .map(|property| self.type_checker.ref_(self).get_type_of_symbol(property))
                .collect::<Result<Vec<_>, _>>()?;
            let index_infos = self
                .type_checker
                .ref_(self)
                .get_index_infos_of_type(source)?;
            let index_types = index_infos.iter().map(|&info| {
                if info != self.type_checker.ref_(self).enum_number_index_info() {
                    info.ref_(self).type_.clone()
                } else {
                    self.type_checker.ref_(self).never_type()
                }
            });
            self.infer_from_types(
                self.type_checker.ref_(self).get_union_type(
                    prop_types.and_extend(index_types),
                    None,
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?,
                self.type_checker
                    .ref_(self)
                    .get_template_type_from_mapped_type(target)?,
            )?;
            return Ok(true);
        }
        Ok(false)
    }

    pub(super) fn infer_to_conditional_type(
        &self,
        source: Id<Type>,
        target: Id<Type>, /*ConditionalType*/
    ) -> io::Result<()> {
        if source.ref_(self).flags().intersects(TypeFlags::Conditional) {
            self.infer_from_types(
                source.ref_(self).as_conditional_type().check_type,
                target.ref_(self).as_conditional_type().check_type,
            )?;
            self.infer_from_types(
                source.ref_(self).as_conditional_type().extends_type,
                target.ref_(self).as_conditional_type().extends_type,
            )?;
            self.infer_from_types(
                self.type_checker
                    .ref_(self)
                    .get_true_type_from_conditional_type(source)?,
                self.type_checker
                    .ref_(self)
                    .get_true_type_from_conditional_type(target)?,
            )?;
            self.infer_from_types(
                self.type_checker
                    .ref_(self)
                    .get_false_type_from_conditional_type(source)?,
                self.type_checker
                    .ref_(self)
                    .get_false_type_from_conditional_type(target)?,
            )?;
        } else {
            let save_priority = self.priority();
            self.set_priority(
                self.priority()
                    | if self.contravariant() {
                        InferencePriority::ContravariantConditional
                    } else {
                        InferencePriority::None
                    },
            );
            let target_types = vec![
                self.type_checker
                    .ref_(self)
                    .get_true_type_from_conditional_type(target)?,
                self.type_checker
                    .ref_(self)
                    .get_false_type_from_conditional_type(target)?,
            ];
            self.infer_to_multiple_types(source, &target_types, target.ref_(self).flags())?;
            self.set_priority(save_priority);
        }

        Ok(())
    }

    pub(super) fn infer_to_template_literal_type(
        &self,
        source: Id<Type>,
        target: Id<Type>, /*TemplateLiteralType*/
    ) -> io::Result<()> {
        let matches = self
            .type_checker
            .ref_(self)
            .infer_types_from_template_literal_type(source, target)?;
        let target_ref = &target.ref_(self);
        let types = &target_ref.as_template_literal_type().types;
        if matches.is_some()
            || every(
                &target.ref_(self).as_template_literal_type().texts,
                |s: &String, _| s.is_empty(),
            )
        {
            for i in 0..types.len() {
                self.infer_from_types(
                    if let Some(matches) = matches.as_ref() {
                        matches[i].clone()
                    } else {
                        self.type_checker.ref_(self).never_type()
                    },
                    types[i],
                )?;
            }
        }

        Ok(())
    }

    pub(super) fn infer_from_object_types(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<()> {
        if get_object_flags(&source.ref_(self)).intersects(ObjectFlags::Reference)
            && get_object_flags(&target.ref_(self)).intersects(ObjectFlags::Reference)
            && (source.ref_(self).as_type_reference_interface().target()
                == target.ref_(self).as_type_reference_interface().target()
                || self.type_checker.ref_(self).is_array_type(source)
                    && self.type_checker.ref_(self).is_array_type(target))
        {
            self.infer_from_type_arguments(
                &*self.type_checker.ref_(self).get_type_arguments(source)?,
                &*self.type_checker.ref_(self).get_type_arguments(target)?,
                &self
                    .type_checker
                    .ref_(self)
                    .get_variances(source.ref_(self).as_type_reference_interface().target()),
            )?;
            return Ok(());
        }
        if self
            .type_checker
            .ref_(self)
            .is_generic_mapped_type(source)?
            && self
                .type_checker
                .ref_(self)
                .is_generic_mapped_type(target)?
        {
            self.infer_from_types(
                self.type_checker
                    .ref_(self)
                    .get_constraint_type_from_mapped_type(source)?,
                self.type_checker
                    .ref_(self)
                    .get_constraint_type_from_mapped_type(target)?,
            )?;
            self.infer_from_types(
                self.type_checker
                    .ref_(self)
                    .get_template_type_from_mapped_type(source)?,
                self.type_checker
                    .ref_(self)
                    .get_template_type_from_mapped_type(target)?,
            )?;
            let source_name_type = self
                .type_checker
                .ref_(self)
                .get_name_type_from_mapped_type(source)?;
            let target_name_type = self
                .type_checker
                .ref_(self)
                .get_name_type_from_mapped_type(target)?;
            if let (Some(source_name_type), Some(target_name_type)) =
                (source_name_type, target_name_type)
            {
                self.infer_from_types(source_name_type, target_name_type)?;
            }
        }
        if get_object_flags(&target.ref_(self)).intersects(ObjectFlags::Mapped)
            && target
                .ref_(self)
                .as_mapped_type()
                .declaration
                .ref_(self)
                .as_mapped_type_node()
                .name_type
                .is_none()
        {
            let constraint_type = self
                .type_checker
                .ref_(self)
                .get_constraint_type_from_mapped_type(target)?;
            if self.infer_to_mapped_type(source, target, constraint_type)? {
                return Ok(());
            }
        }
        if !self
            .type_checker
            .ref_(self)
            .types_definitely_unrelated(source, target)?
        {
            if self.type_checker.ref_(self).is_array_type(source)
                || self.type_checker.ref_(self).is_tuple_type(source)
            {
                if self.type_checker.ref_(self).is_tuple_type(target) {
                    let source_arity = self
                        .type_checker
                        .ref_(self)
                        .get_type_reference_arity(source);
                    let target_arity = self
                        .type_checker
                        .ref_(self)
                        .get_type_reference_arity(target);
                    let element_types = self.type_checker.ref_(self).get_type_arguments(target)?;
                    let target_target = target.ref_(self).as_type_reference().target;
                    let element_flags = &{
                        let element_flags = target_target
                            .ref_(self)
                            .as_tuple_type()
                            .element_flags
                            .clone();
                        element_flags
                    };
                    if self.type_checker.ref_(self).is_tuple_type(source)
                        && self
                            .type_checker
                            .ref_(self)
                            .is_tuple_type_structure_matching(source, target)
                    {
                        for i in 0..target_arity {
                            self.infer_from_types(
                                self.type_checker.ref_(self).get_type_arguments(source)?[i],
                                element_types[i],
                            )?;
                        }
                        return Ok(());
                    }
                    let start_length = if self.type_checker.ref_(self).is_tuple_type(source) {
                        cmp::min(
                            source
                                .ref_(self)
                                .as_type_reference_interface()
                                .target()
                                .ref_(self)
                                .as_tuple_type()
                                .fixed_length,
                            target_target.ref_(self).as_tuple_type().fixed_length,
                        )
                    } else {
                        0
                    };
                    let end_length = cmp::min(
                        if self.type_checker.ref_(self).is_tuple_type(source) {
                            self.type_checker.ref_(self).get_end_element_count(
                                source.ref_(self).as_type_reference_interface().target(),
                                ElementFlags::Fixed,
                            )
                        } else {
                            0
                        },
                        if target_target.ref_(self).as_tuple_type().has_rest_element {
                            self.type_checker.ref_(self).get_end_element_count(
                                target.ref_(self).as_type_reference().target,
                                ElementFlags::Fixed,
                            )
                        } else {
                            0
                        },
                    );
                    for i in 0..start_length {
                        self.infer_from_types(
                            self.type_checker.ref_(self).get_type_arguments(source)?[i],
                            element_types[i],
                        )?;
                    }
                    if !self.type_checker.ref_(self).is_tuple_type(source)
                        || source_arity - start_length - end_length == 1
                            && source
                                .ref_(self)
                                .as_type_reference()
                                .target
                                .ref_(self)
                                .as_tuple_type()
                                .element_flags[start_length]
                                .intersects(ElementFlags::Rest)
                    {
                        let rest_type = self.type_checker.ref_(self).get_type_arguments(source)?
                            [start_length]
                            .clone();
                        for i in start_length..target_arity - end_length {
                            self.infer_from_types(
                                if element_flags[i].intersects(ElementFlags::Variadic) {
                                    self.type_checker
                                        .ref_(self)
                                        .create_array_type(rest_type, None)
                                } else {
                                    rest_type.clone()
                                },
                                element_types[i],
                            )?;
                        }
                    } else {
                        let middle_length = target_arity - start_length - end_length;
                        if middle_length == 2
                            && (element_flags[start_length] & element_flags[start_length + 1])
                                .intersects(ElementFlags::Variadic)
                            && self.type_checker.ref_(self).is_tuple_type(source)
                        {
                            let target_info =
                                self.get_inference_info_for_type(element_types[start_length]);
                            if let Some(target_info) = target_info.as_ref().filter(|target_info| {
                                target_info.ref_(self).maybe_implied_arity().is_some()
                            }) {
                                self.infer_from_types(
                                    self.type_checker.ref_(self).slice_tuple_type(
                                        source,
                                        start_length,
                                        Some(
                                            end_length + source_arity
                                                - target_info
                                                    .ref_(self)
                                                    .maybe_implied_arity()
                                                    .unwrap(),
                                        ),
                                    )?,
                                    element_types[start_length],
                                )?;
                                self.infer_from_types(
                                    self.type_checker.ref_(self).slice_tuple_type(
                                        source,
                                        start_length
                                            + target_info.ref_(self).maybe_implied_arity().unwrap(),
                                        Some(end_length),
                                    )?,
                                    element_types[start_length + 1],
                                )?;
                            }
                        } else if middle_length == 1
                            && element_flags[start_length].intersects(ElementFlags::Variadic)
                        {
                            let ends_in_optional =
                                target_target.ref_(self).as_tuple_type().element_flags
                                    [target_arity - 1]
                                    .intersects(ElementFlags::Optional);
                            let source_slice = if self.type_checker.ref_(self).is_tuple_type(source)
                            {
                                self.type_checker.ref_(self).slice_tuple_type(
                                    source,
                                    start_length,
                                    Some(end_length),
                                )?
                            } else {
                                self.type_checker.ref_(self).create_array_type(
                                    self.type_checker.ref_(self).get_type_arguments(source)?[0],
                                    None,
                                )
                            };
                            self.infer_with_priority(
                                source_slice,
                                element_types[start_length],
                                if ends_in_optional {
                                    InferencePriority::SpeculativeTuple
                                } else {
                                    InferencePriority::None
                                },
                            )?;
                        } else if middle_length == 1
                            && element_flags[start_length].intersects(ElementFlags::Rest)
                        {
                            let rest_type = if self.type_checker.ref_(self).is_tuple_type(source) {
                                self.type_checker
                                    .ref_(self)
                                    .get_element_type_of_slice_of_tuple_type(
                                        source,
                                        start_length,
                                        Some(end_length),
                                        None,
                                    )?
                            } else {
                                self.type_checker
                                    .ref_(self)
                                    .get_type_arguments(source)?
                                    .get(0)
                                    .cloned()
                            };
                            if let Some(rest_type) = rest_type {
                                self.infer_from_types(rest_type, element_types[start_length])?;
                            }
                        }
                    }
                    for i in 0..end_length {
                        self.infer_from_types(
                            self.type_checker.ref_(self).get_type_arguments(source)?
                                [source_arity - i - 1],
                            element_types[target_arity - i - 1],
                        )?;
                    }
                    return Ok(());
                }
                if self.type_checker.ref_(self).is_array_type(target) {
                    self.infer_from_index_types(source, target)?;
                    return Ok(());
                }
            }
            self.infer_from_properties(source, target)?;
            self.infer_from_signatures(source, target, SignatureKind::Call)?;
            self.infer_from_signatures(source, target, SignatureKind::Construct)?;
            self.infer_from_index_types(source, target)?;
        }

        Ok(())
    }

    pub(super) fn infer_from_properties(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<()> {
        let properties = self
            .type_checker
            .ref_(self)
            .get_properties_of_object_type(target)?;
        for target_prop in properties {
            let source_prop = self.type_checker.ref_(self).get_property_of_type_(
                source,
                target_prop.ref_(self).escaped_name(),
                None,
            )?;
            if let Some(source_prop) = source_prop {
                self.infer_from_types(
                    self.type_checker
                        .ref_(self)
                        .get_type_of_symbol(source_prop)?,
                    self.type_checker
                        .ref_(self)
                        .get_type_of_symbol(target_prop)?,
                )?;
            }
        }

        Ok(())
    }

    pub(super) fn infer_from_signatures(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        kind: SignatureKind,
    ) -> io::Result<()> {
        let source_signatures = self
            .type_checker
            .ref_(self)
            .get_signatures_of_type(source, kind)?;
        let target_signatures = self
            .type_checker
            .ref_(self)
            .get_signatures_of_type(target, kind)?;
        let source_len = source_signatures.len();
        let target_len = target_signatures.len();
        let len = if source_len < target_len {
            source_len
        } else {
            target_len
        };
        let skip_parameters =
            get_object_flags(&source.ref_(self)).intersects(ObjectFlags::NonInferrableType);
        for i in 0..len {
            self.infer_from_signature(
                self.type_checker
                    .ref_(self)
                    .get_base_signature(source_signatures[source_len - len + i].clone())?,
                self.type_checker
                    .ref_(self)
                    .get_erased_signature(target_signatures[target_len - len + i].clone())?,
                skip_parameters,
            )?;
        }

        Ok(())
    }

    pub(super) fn infer_from_signature(
        &self,
        source: Id<Signature>,
        target: Id<Signature>,
        skip_parameters: bool,
    ) -> io::Result<()> {
        if !skip_parameters {
            let save_bivariant = self.bivariant();
            let kind = if let Some(target_declaration) = target.ref_(self).declaration {
                target_declaration.ref_(self).kind()
            } else {
                SyntaxKind::Unknown
            };
            self.set_bivariant(
                self.bivariant()
                    || matches!(
                        kind,
                        SyntaxKind::MethodDeclaration
                            | SyntaxKind::MethodSignature
                            | SyntaxKind::Constructor
                    ),
            );
            self.type_checker.ref_(self).apply_to_parameter_types(
                source,
                target,
                |s: Id<Type>, t: Id<Type>| self.infer_from_contravariant_types(s, t),
            )?;
            self.set_bivariant(save_bivariant);
        }
        self.type_checker.ref_(self).apply_to_return_types(
            source,
            target,
            |s: Id<Type>, t: Id<Type>| self.infer_from_types(s, t),
        )?;

        Ok(())
    }

    pub(super) fn infer_from_index_types(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<()> {
        let priority = if (get_object_flags(&source.ref_(self))
            & get_object_flags(&target.ref_(self)))
        .intersects(ObjectFlags::Mapped)
        {
            InferencePriority::HomomorphicMappedType
        } else {
            InferencePriority::None
        };
        let index_infos = self
            .type_checker
            .ref_(self)
            .get_index_infos_of_type(target)?;
        if self
            .type_checker
            .ref_(self)
            .is_object_type_with_inferable_index(source)?
        {
            for target_info in &index_infos {
                let mut prop_types: Vec<Id<Type>> = vec![];
                for prop in self
                    .type_checker
                    .ref_(self)
                    .get_properties_of_type(source)?
                {
                    if self.type_checker.ref_(self).is_applicable_index_type(
                        self.type_checker
                            .ref_(self)
                            .get_literal_type_from_property(
                                prop,
                                TypeFlags::StringOrNumberLiteralOrUnique,
                                None,
                            )?,
                        target_info.ref_(self).key_type,
                    )? {
                        let prop_type = self.type_checker.ref_(self).get_type_of_symbol(prop)?;
                        prop_types.push(
                            if prop.ref_(self).flags().intersects(SymbolFlags::Optional) {
                                self.type_checker
                                    .ref_(self)
                                    .remove_missing_or_undefined_type(prop_type)?
                            } else {
                                prop_type
                            },
                        );
                    }
                }
                for info in &self
                    .type_checker
                    .ref_(self)
                    .get_index_infos_of_type(source)?
                {
                    if self.type_checker.ref_(self).is_applicable_index_type(
                        info.ref_(self).key_type,
                        target_info.ref_(self).key_type,
                    )? {
                        prop_types.push(info.ref_(self).type_.clone());
                    }
                }
                if !prop_types.is_empty() {
                    self.infer_with_priority(
                        self.type_checker.ref_(self).get_union_type(
                            &prop_types,
                            None,
                            Option::<Id<Symbol>>::None,
                            None,
                            None,
                        )?,
                        target_info.ref_(self).type_,
                        priority,
                    )?;
                }
            }
        }
        for target_info in &index_infos {
            let source_info = self
                .type_checker
                .ref_(self)
                .get_applicable_index_info(source, target_info.ref_(self).key_type)?;
            if let Some(source_info) = source_info.as_ref() {
                self.infer_with_priority(
                    source_info.ref_(self).type_,
                    target_info.ref_(self).type_,
                    priority,
                )?;
            }
        }

        Ok(())
    }
}

impl TypeChecker {
    pub(super) fn is_type_or_base_identical_to(
        &self,
        s: Id<Type>,
        t: Id<Type>,
    ) -> io::Result<bool> {
        Ok(
            if self.exact_optional_property_types == Some(true) && t == self.missing_type() {
                s == t
            } else {
                self.is_type_identical_to(s, t)?
                    || (t.ref_(self).flags().intersects(TypeFlags::String)
                        && s.ref_(self).flags().intersects(TypeFlags::StringLiteral)
                        || t.ref_(self).flags().intersects(TypeFlags::Number)
                            && s.ref_(self).flags().intersects(TypeFlags::NumberLiteral))
            },
        )
    }

    pub(super) fn is_type_closely_matched_by(&self, s: Id<Type>, t: Id<Type>) -> bool {
        s.ref_(self).flags().intersects(TypeFlags::Object)
            && t.ref_(self).flags().intersects(TypeFlags::Object)
            && matches!(
                s.ref_(self).maybe_symbol(),
                Some(s_symbol) if matches!(
                    t.ref_(self).maybe_symbol(),
                    Some(t_symbol) if s_symbol == t_symbol
                )
            )
            || matches!(
                s.ref_(self).maybe_alias_symbol(),
                Some(s_alias_symbol) if matches!(
                    t.ref_(self).maybe_alias_symbol(),
                    Some(t_alias_symbol) if s_alias_symbol == t_alias_symbol
                )
            ) && s.ref_(self).maybe_alias_type_arguments().is_some()
    }

    pub(super) fn has_primitive_constraint(
        &self,
        type_: Id<Type>, /*TypeParameter*/
    ) -> io::Result<bool> {
        let constraint = self.get_constraint_of_type_parameter(type_)?;
        Ok(matches!(
            constraint,
            Some(constraint) if self.maybe_type_of_kind(
                if constraint.ref_(self).flags().intersects(TypeFlags::Conditional) {
                    self.get_default_constraint_of_conditional_type(constraint)?
                } else {
                    constraint.clone()
                },
                TypeFlags::Primitive | TypeFlags::Index | TypeFlags::TemplateLiteral | TypeFlags::StringMapping
            )
        ))
    }

    pub(super) fn is_object_literal_type(&self, type_: Id<Type>) -> bool {
        get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::ObjectLiteral)
    }

    pub(super) fn is_object_or_array_literal_type(&self, type_: Id<Type>) -> bool {
        get_object_flags(&type_.ref_(self))
            .intersects(ObjectFlags::ObjectLiteral | ObjectFlags::ArrayLiteral)
    }

    pub(super) fn union_object_and_array_literal_candidates<'self_and_candidates, TCandidates>(
        &'self_and_candidates self,
        candidates: TCandidates,
    ) -> io::Result<impl Iterator<Item = Id<Type>> + 'self_and_candidates>
    where
        TCandidates: IntoIterator<Item = &'self_and_candidates Id<Type>> + Clone,
        TCandidates::IntoIter: Clone + 'self_and_candidates,
    {
        let candidates = candidates.into_iter();
        if candidates.clone().peekmore().is_len_greater_than(1) {
            let mut object_literals = candidates
                .clone()
                .filter(|&&candidate| self.is_object_or_array_literal_type(candidate))
                .peekable();
            if !object_literals.is_empty_() {
                let literals_type = self.get_union_type(
                    object_literals,
                    Some(UnionReduction::Subtype),
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?;
                return Ok(Either::Left(
                    candidates
                        .filter(|&&t| !self.is_object_or_array_literal_type(t))
                        .cloned()
                        .chain(once(literals_type)),
                ));
            }
        }
        Ok(Either::Right(candidates.cloned()))
    }

    pub(super) fn get_contravariant_inference(
        &self,
        inference: &InferenceInfo,
    ) -> io::Result<Id<Type>> {
        Ok(
            if matches!(
                inference.maybe_priority(),
                Some(inference_priority) if inference_priority.intersects(InferencePriority::PriorityImpliesCombination)
            ) {
                self.get_intersection_type(
                    inference.maybe_contra_candidates().as_deref().unwrap(),
                    Option::<Id<Symbol>>::None,
                    None,
                )?
            } else {
                self.get_common_subtype(inference.maybe_contra_candidates().as_deref().unwrap())?
            },
        )
    }

    pub(super) fn get_covariant_inference(
        &self,
        inference: &InferenceInfo,
        signature: Id<Signature>,
    ) -> io::Result<Id<Type>> {
        let inference_candidates = inference.maybe_candidates();
        let candidates = self
            .union_object_and_array_literal_candidates(inference_candidates.as_deref().unwrap())?;
        let primitive_constraint = self.has_primitive_constraint(inference.type_parameter)?;
        let widen_literal_types = !primitive_constraint
            && inference.top_level()
            && (inference.is_fixed()
                || !self.is_type_parameter_at_top_level(
                    self.get_return_type_of_signature(signature)?,
                    inference.type_parameter,
                )?);
        let base_candidates: Vec<_> = if primitive_constraint {
            candidates
                .map(|candidate| self.get_regular_type_of_literal_type(candidate))
                .collect()
        } else if widen_literal_types {
            candidates
                .map(|candidate| self.get_widened_literal_type(candidate))
                .collect::<Result<Vec<_>, _>>()?
        } else {
            candidates.collect()
        };
        let unwidened_type = if matches!(
            inference.maybe_priority(),
            Some(inference_priority) if inference_priority.intersects(InferencePriority::PriorityImpliesCombination)
        ) {
            self.get_union_type(
                &base_candidates,
                Some(UnionReduction::Subtype),
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?
        } else {
            self.get_common_supertype(&base_candidates)?
        };
        self.get_widened_type(unwidened_type)
    }

    pub(super) fn get_inferred_type(
        &self,
        context: Id<InferenceContext>,
        index: usize,
    ) -> io::Result<Id<Type>> {
        let inference = context.ref_(self).inferences()[index].clone();
        if inference.ref_(self).maybe_inferred_type().is_none() {
            let mut inferred_type: Option<Id<Type>> = None;
            let signature = context.ref_(self).signature;
            if let Some(signature) = signature {
                let inferred_covariant_type = if inference.ref_(self).maybe_candidates().is_some() {
                    Some(self.get_covariant_inference(&inference.ref_(self), signature.clone())?)
                } else {
                    None
                };
                if let Some(inference_contra_candidates) =
                    inference.ref_(self).maybe_contra_candidates().as_ref()
                {
                    inferred_type = Some(
                        if let Some(inferred_covariant_type) = inferred_covariant_type.try_filter(
                            |&inferred_covariant_type| -> io::Result<_> {
                                Ok(!inferred_covariant_type
                                    .ref_(self)
                                    .flags()
                                    .intersects(TypeFlags::Never)
                                    && try_some(
                                        Some(inference_contra_candidates),
                                        Some(|&t: &Id<Type>| {
                                            self.is_type_subtype_of(inferred_covariant_type, t)
                                        }),
                                    )?)
                            },
                        )? {
                            inferred_covariant_type.clone()
                        } else {
                            self.get_contravariant_inference(&inference.ref_(self))?
                        },
                    );
                } else if let Some(inferred_covariant_type) = inferred_covariant_type.as_ref() {
                    inferred_type = Some(inferred_covariant_type.clone());
                } else if context
                    .ref_(self)
                    .flags()
                    .intersects(InferenceFlags::NoDefault)
                {
                    inferred_type = Some(self.silent_never_type());
                } else {
                    let default_type =
                        self.get_default_from_type_parameter_(inference.ref_(self).type_parameter)?;
                    if let Some(default_type) = default_type {
                        inferred_type = Some(self.instantiate_type(
                            default_type,
                            Some(self.merge_type_mappers(
                                Some(self.create_backreference_mapper(context, index)),
                                context.ref_(self).non_fixing_mapper().clone(),
                            )),
                        )?);
                    }
                }
            } else {
                inferred_type = self.get_type_from_inference(&inference.ref_(self))?;
            }

            inference
                .ref_(self)
                .set_inferred_type(Some(inferred_type.clone().unwrap_or_else(|| {
                    self.get_default_type_argument_type(
                        context
                            .ref_(self)
                            .flags()
                            .intersects(InferenceFlags::AnyDefault),
                    )
                })));

            let constraint =
                self.get_constraint_of_type_parameter(inference.ref_(self).type_parameter)?;
            if let Some(constraint) = constraint {
                let instantiated_constraint = self
                    .instantiate_type(constraint, Some(context.ref_(self).non_fixing_mapper()))?;
                if match inferred_type {
                    None => true,
                    Some(inferred_type) => {
                        context.ref_(self).compare_types.ref_(self).call(
                            inferred_type,
                            self.get_type_with_this_argument(
                                instantiated_constraint,
                                Some(inferred_type),
                                None,
                            )?,
                            None,
                        )? == Ternary::False
                    }
                } {
                    #[allow(unused_assignments)]
                    {
                        inferred_type = Some(instantiated_constraint.clone());
                    }
                    inference
                        .ref_(self)
                        .set_inferred_type(Some(instantiated_constraint));
                }
            }
        }

        let ret = inference.ref_(self).maybe_inferred_type().unwrap();
        Ok(ret)
    }

    pub(super) fn get_default_type_argument_type(&self, is_in_java_script_file: bool) -> Id<Type> {
        if is_in_java_script_file {
            self.any_type()
        } else {
            self.unknown_type()
        }
    }

    pub(super) fn get_inferred_types(
        &self,
        context: Id<InferenceContext>,
    ) -> io::Result<Vec<Id<Type>>> {
        let mut result: Vec<Id<Type>> = vec![];
        for i in 0..context.ref_(self).inferences().len() {
            result.push(self.get_inferred_type(context, i)?);
        }
        Ok(result)
    }

    pub(super) fn get_cannot_find_name_diagnostic_for_name(
        &self,
        node: Id<Node>,
    ) -> &'static DiagnosticMessage {
        match &*node.ref_(self).as_identifier().escaped_text {
            "document" | "console" => &Diagnostics::Cannot_find_name_0_Do_you_need_to_change_your_target_library_Try_changing_the_lib_compiler_option_to_include_dom,
            "$" => {
                if self.compiler_options.ref_(self).types.is_some() {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_jQuery_Try_npm_i_save_dev_types_Slashjquery_and_then_add_jquery_to_the_types_field_in_your_tsconfig
                } else {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_jQuery_Try_npm_i_save_dev_types_Slashjquery
                }
            }
            "describe" | "suite" | "it" | "test" => {
                if self.compiler_options.ref_(self).types.is_some() {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_a_test_runner_Try_npm_i_save_dev_types_Slashjest_or_npm_i_save_dev_types_Slashmocha_and_then_add_jest_or_mocha_to_the_types_field_in_your_tsconfig
                } else {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_a_test_runner_Try_npm_i_save_dev_types_Slashjest_or_npm_i_save_dev_types_Slashmocha
                }
            }
            "process" | "require" | "Buffer" | "module" => {
                if self.compiler_options.ref_(self).types.is_some() {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_node_Try_npm_i_save_dev_types_Slashnode_and_then_add_node_to_the_types_field_in_your_tsconfig
                } else {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_node_Try_npm_i_save_dev_types_Slashnode
                }
            }
            "Map" | "Set" | "Promise" | "Symbol" | "WeakMap" | "WeakSet" | "Iterator" | "AsyncIterator" | "SharedArrayBuffer" | "Atomics" | "AsyncIterable" | "AsyncIterableIterator" | "AsyncGenerator" | "AsyncGeneratorFunction" | "BigInt" | "Reflect" | "BigInt64Array" | "BigUint64Array" => &Diagnostics::Cannot_find_name_0_Do_you_need_to_change_your_target_library_Try_changing_the_lib_compiler_option_to_1_or_later,
            _ => {
                if node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ShorthandPropertyAssignment {
                    &Diagnostics::No_value_exists_in_scope_for_the_shorthand_property_0_Either_declare_one_or_provide_an_initializer
                } else {
                    &Diagnostics::Cannot_find_name_0
                }
            }
        }
    }
}
