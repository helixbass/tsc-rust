use gc::Gc;
use itertools::Either;
use peekmore::PeekMore;
use std::cmp;
use std::io;
use std::iter::once;
use std::ptr;

use super::InferTypes;
use crate::try_some;
use crate::OptionTry;
use crate::PeekMoreExt;
use crate::PeekableExt;
use crate::VecExt;
use crate::{
    every, find, flat_map, get_object_flags, DiagnosticMessage, Diagnostics, ElementFlags,
    InferenceContext, InferenceFlags, InferenceInfo, InferencePriority, Node, NodeInterface,
    ObjectFlags, Signature, SignatureKind, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    Ternary, Type, TypeChecker, TypeFlags, TypeInterface, UnionReduction,
};

impl InferTypes {
    pub(super) fn get_inference_info_for_type(&self, type_: &Type) -> Option<Gc<InferenceInfo>> {
        if type_.flags().intersects(TypeFlags::TypeVariable) {
            for inference in &self.inferences {
                if ptr::eq(type_, &*inference.type_parameter) {
                    return Some(inference.clone());
                }
            }
        }
        None
    }

    pub(super) fn get_single_type_variable_from_intersection_types(
        &self,
        types: &[Gc<Type>],
    ) -> Option<Gc<Type>> {
        let mut type_variable: Option<Gc<Type>> = None;
        for type_ in types {
            let t = if type_.flags().intersects(TypeFlags::Intersection) {
                find(
                    type_.as_union_or_intersection_type_interface().types(),
                    |t: &Gc<Type>, _| self.get_inference_info_for_type(t).is_some(),
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
                type_variable.as_ref(),
                Some(type_variable) if !Gc::ptr_eq(&t, type_variable)
            ) {
                return None;
            }
            type_variable = Some(t);
        }
        type_variable
    }

    pub(super) fn infer_to_multiple_types(
        &self,
        source: &Type,
        targets: &[Gc<Type>],
        target_flags: TypeFlags,
    ) -> io::Result<()> {
        let mut type_variable_count = 0;
        if target_flags.intersects(TypeFlags::Union) {
            let mut naked_type_variable: Option<Gc<Type>> = None;
            let sources = if source.flags().intersects(TypeFlags::Union) {
                source
                    .as_union_or_intersection_type_interface()
                    .types()
                    .to_owned()
            } else {
                vec![source.type_wrapper()]
            };
            let mut matched = vec![false; sources.len()];
            let mut inference_circularity = false;
            for t in targets {
                if self.get_inference_info_for_type(t).is_some() {
                    naked_type_variable = Some(t.clone());
                    type_variable_count += 1;
                } else {
                    for i in 0..sources.len() {
                        let save_inference_priority = self.inference_priority();
                        self.set_inference_priority(InferencePriority::MaxValue);
                        self.infer_from_types(&sources[i], t)?;
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
                if let Some(intersection_type_variable) = intersection_type_variable.as_ref() {
                    self.infer_with_priority(
                        source,
                        intersection_type_variable,
                        InferencePriority::NakedTypeVariable,
                    )?;
                }
                return Ok(());
            }
            if type_variable_count == 1 && !inference_circularity {
                let unmatched = flat_map(Some(&sources), |s: &Gc<Type>, i| {
                    if matched[i] {
                        vec![]
                    } else {
                        vec![s.clone()]
                    }
                });
                if !unmatched.is_empty() {
                    self.infer_from_types(
                        &*self.type_checker.get_union_type(
                            &unmatched,
                            None,
                            Option::<&Symbol>::None,
                            None,
                            Option::<&Type>::None,
                        )?,
                        naked_type_variable.as_deref().unwrap(),
                    )?;
                    return Ok(());
                }
            }
        } else {
            for t in targets {
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
            for t in targets {
                if self.get_inference_info_for_type(t).is_some() {
                    self.infer_with_priority(source, t, InferencePriority::NakedTypeVariable)?;
                }
            }
        }

        Ok(())
    }

    pub(super) fn infer_to_mapped_type(
        &self,
        source: &Type,
        target: &Type, /*MappedType*/
        constraint_type: &Type,
    ) -> io::Result<bool> {
        if constraint_type.flags().intersects(TypeFlags::Union) {
            let mut result = false;
            for type_ in constraint_type
                .as_union_or_intersection_type_interface()
                .types()
            {
                result = self.infer_to_mapped_type(source, target, type_)? || result;
            }
            return Ok(result);
        }
        if constraint_type.flags().intersects(TypeFlags::Index) {
            let inference =
                self.get_inference_info_for_type(&constraint_type.as_index_type().type_);
            if let Some(inference) = inference.as_ref().filter(|inference| {
                !inference.is_fixed() && !self.type_checker.is_from_inference_blocked_source(source)
            }) {
                let inferred_type = self.type_checker.infer_type_for_homomorphic_mapped_type(
                    source,
                    target,
                    constraint_type,
                )?;
                if let Some(inferred_type) = inferred_type.as_ref() {
                    self.infer_with_priority(
                        inferred_type,
                        &inference.type_parameter,
                        if get_object_flags(source).intersects(ObjectFlags::NonInferrableType) {
                            InferencePriority::PartialHomomorphicMappedType
                        } else {
                            InferencePriority::HomomorphicMappedType
                        },
                    )?;
                }
            }
            return Ok(true);
        }
        if constraint_type.flags().intersects(TypeFlags::TypeParameter) {
            self.infer_with_priority(
                &*self.type_checker.get_index_type(source, None, None)?,
                constraint_type,
                InferencePriority::MappedTypeConstraint,
            )?;
            let extended_constraint = self.type_checker.get_constraint_of_type(constraint_type)?;
            if matches!(
                extended_constraint.as_ref(),
                Some(extended_constraint) if self.infer_to_mapped_type(source, target, extended_constraint)?
            ) {
                return Ok(true);
            }
            let prop_types = self
                .type_checker
                .get_properties_of_type(source)?
                .map(|ref property| self.type_checker.get_type_of_symbol(property))
                .collect::<Result<Vec<_>, _>>()?;
            let index_infos = self.type_checker.get_index_infos_of_type(source)?;
            let index_types = index_infos.iter().map(|info| {
                if !Gc::ptr_eq(info, &self.type_checker.enum_number_index_info()) {
                    info.type_.clone()
                } else {
                    self.type_checker.never_type()
                }
            });
            self.infer_from_types(
                &*self.type_checker.get_union_type(
                    prop_types.and_extend(index_types),
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                )?,
                &*self
                    .type_checker
                    .get_template_type_from_mapped_type(target)?,
            )?;
            return Ok(true);
        }
        Ok(false)
    }

    pub(super) fn infer_to_conditional_type(
        &self,
        source: &Type,
        target: &Type, /*ConditionalType*/
    ) -> io::Result<()> {
        let target_as_conditional_type = target.as_conditional_type();
        if source.flags().intersects(TypeFlags::Conditional) {
            let source_as_conditional_type = source.as_conditional_type();
            self.infer_from_types(
                &source_as_conditional_type.check_type,
                &target_as_conditional_type.check_type,
            )?;
            self.infer_from_types(
                &source_as_conditional_type.extends_type,
                &target_as_conditional_type.extends_type,
            )?;
            self.infer_from_types(
                &*self
                    .type_checker
                    .get_true_type_from_conditional_type(source)?,
                &*self
                    .type_checker
                    .get_true_type_from_conditional_type(target)?,
            )?;
            self.infer_from_types(
                &*self
                    .type_checker
                    .get_false_type_from_conditional_type(source)?,
                &*self
                    .type_checker
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
                    .get_true_type_from_conditional_type(target)?,
                self.type_checker
                    .get_false_type_from_conditional_type(target)?,
            ];
            self.infer_to_multiple_types(source, &target_types, target.flags())?;
            self.set_priority(save_priority);
        }

        Ok(())
    }

    pub(super) fn infer_to_template_literal_type(
        &self,
        source: &Type,
        target: &Type, /*TemplateLiteralType*/
    ) -> io::Result<()> {
        let matches = self
            .type_checker
            .infer_types_from_template_literal_type(source, target)?;
        let target_as_template_literal_type = target.as_template_literal_type();
        let types = &target_as_template_literal_type.types;
        if matches.is_some()
            || every(&target_as_template_literal_type.texts, |s: &String, _| {
                s.is_empty()
            })
        {
            for i in 0..types.len() {
                self.infer_from_types(
                    &*if let Some(matches) = matches.as_ref() {
                        matches[i].clone()
                    } else {
                        self.type_checker.never_type()
                    },
                    &types[i],
                )?;
            }
        }

        Ok(())
    }

    pub(super) fn infer_from_object_types(&self, source: &Type, target: &Type) -> io::Result<()> {
        if get_object_flags(source).intersects(ObjectFlags::Reference)
            && get_object_flags(target).intersects(ObjectFlags::Reference)
            && (Gc::ptr_eq(
                &source.as_type_reference_interface().target(),
                &target.as_type_reference_interface().target(),
            ) || self.type_checker.is_array_type(source)
                && self.type_checker.is_array_type(target))
        {
            self.infer_from_type_arguments(
                &*self.type_checker.get_type_arguments(source)?,
                &*self.type_checker.get_type_arguments(target)?,
                &self
                    .type_checker
                    .get_variances(&source.as_type_reference_interface().target()),
            )?;
            return Ok(());
        }
        if self.type_checker.is_generic_mapped_type(source)?
            && self.type_checker.is_generic_mapped_type(target)?
        {
            self.infer_from_types(
                &*self
                    .type_checker
                    .get_constraint_type_from_mapped_type(source)?,
                &*self
                    .type_checker
                    .get_constraint_type_from_mapped_type(target)?,
            )?;
            self.infer_from_types(
                &*self
                    .type_checker
                    .get_template_type_from_mapped_type(source)?,
                &*self
                    .type_checker
                    .get_template_type_from_mapped_type(target)?,
            )?;
            let source_name_type = self.type_checker.get_name_type_from_mapped_type(source)?;
            let target_name_type = self.type_checker.get_name_type_from_mapped_type(target)?;
            if let (Some(source_name_type), Some(target_name_type)) =
                (source_name_type.as_ref(), target_name_type.as_ref())
            {
                self.infer_from_types(source_name_type, target_name_type)?;
            }
        }
        if get_object_flags(target).intersects(ObjectFlags::Mapped)
            && target
                .as_mapped_type()
                .declaration
                .as_mapped_type_node()
                .name_type
                .is_none()
        {
            let ref constraint_type = self
                .type_checker
                .get_constraint_type_from_mapped_type(target)?;
            if self.infer_to_mapped_type(source, target, constraint_type)? {
                return Ok(());
            }
        }
        if !self
            .type_checker
            .types_definitely_unrelated(source, target)?
        {
            if self.type_checker.is_array_type(source) || self.type_checker.is_tuple_type(source) {
                if self.type_checker.is_tuple_type(target) {
                    let source_arity = self.type_checker.get_type_reference_arity(source);
                    let target_arity = self.type_checker.get_type_reference_arity(target);
                    let element_types = self.type_checker.get_type_arguments(target)?;
                    let target_target_as_tuple_type =
                        target.as_type_reference().target.as_tuple_type();
                    let element_flags = &target_target_as_tuple_type.element_flags;
                    if self.type_checker.is_tuple_type(source)
                        && self
                            .type_checker
                            .is_tuple_type_structure_matching(source, target)
                    {
                        for i in 0..target_arity {
                            self.infer_from_types(
                                &self.type_checker.get_type_arguments(source)?[i],
                                &element_types[i],
                            )?;
                        }
                        return Ok(());
                    }
                    let start_length = if self.type_checker.is_tuple_type(source) {
                        cmp::min(
                            source
                                .as_type_reference_interface()
                                .target()
                                .as_tuple_type()
                                .fixed_length,
                            target_target_as_tuple_type.fixed_length,
                        )
                    } else {
                        0
                    };
                    let end_length = cmp::min(
                        if self.type_checker.is_tuple_type(source) {
                            self.type_checker.get_end_element_count(
                                &source.as_type_reference_interface().target(),
                                ElementFlags::Fixed,
                            )
                        } else {
                            0
                        },
                        if target_target_as_tuple_type.has_rest_element {
                            self.type_checker.get_end_element_count(
                                &target.as_type_reference().target,
                                ElementFlags::Fixed,
                            )
                        } else {
                            0
                        },
                    );
                    for i in 0..start_length {
                        self.infer_from_types(
                            &self.type_checker.get_type_arguments(source)?[i],
                            &element_types[i],
                        )?;
                    }
                    if !self.type_checker.is_tuple_type(source)
                        || source_arity - start_length - end_length == 1
                            && source
                                .as_type_reference()
                                .target
                                .as_tuple_type()
                                .element_flags[start_length]
                                .intersects(ElementFlags::Rest)
                    {
                        let rest_type =
                            self.type_checker.get_type_arguments(source)?[start_length].clone();
                        for i in start_length..target_arity - end_length {
                            self.infer_from_types(
                                &*if element_flags[i].intersects(ElementFlags::Variadic) {
                                    self.type_checker.create_array_type(&rest_type, None)
                                } else {
                                    rest_type.clone()
                                },
                                &element_types[i],
                            )?;
                        }
                    } else {
                        let middle_length = target_arity - start_length - end_length;
                        if middle_length == 2
                            && (element_flags[start_length] & element_flags[start_length + 1])
                                .intersects(ElementFlags::Variadic)
                            && self.type_checker.is_tuple_type(source)
                        {
                            let target_info =
                                self.get_inference_info_for_type(&element_types[start_length]);
                            if let Some(target_info) = target_info
                                .as_ref()
                                .filter(|target_info| target_info.maybe_implied_arity().is_some())
                            {
                                self.infer_from_types(
                                    &*self.type_checker.slice_tuple_type(
                                        source,
                                        start_length,
                                        Some(
                                            end_length + source_arity
                                                - target_info.maybe_implied_arity().unwrap(),
                                        ),
                                    )?,
                                    &element_types[start_length],
                                )?;
                                self.infer_from_types(
                                    &*self.type_checker.slice_tuple_type(
                                        source,
                                        start_length + target_info.maybe_implied_arity().unwrap(),
                                        Some(end_length),
                                    )?,
                                    &element_types[start_length + 1],
                                )?;
                            }
                        } else if middle_length == 1
                            && element_flags[start_length].intersects(ElementFlags::Variadic)
                        {
                            let ends_in_optional = target_target_as_tuple_type.element_flags
                                [target_arity - 1]
                                .intersects(ElementFlags::Optional);
                            let source_slice = if self.type_checker.is_tuple_type(source) {
                                self.type_checker.slice_tuple_type(
                                    source,
                                    start_length,
                                    Some(end_length),
                                )?
                            } else {
                                self.type_checker.create_array_type(
                                    &self.type_checker.get_type_arguments(source)?[0],
                                    None,
                                )
                            };
                            self.infer_with_priority(
                                &source_slice,
                                &element_types[start_length],
                                if ends_in_optional {
                                    InferencePriority::SpeculativeTuple
                                } else {
                                    InferencePriority::None
                                },
                            )?;
                        } else if middle_length == 1
                            && element_flags[start_length].intersects(ElementFlags::Rest)
                        {
                            let rest_type = if self.type_checker.is_tuple_type(source) {
                                self.type_checker.get_element_type_of_slice_of_tuple_type(
                                    source,
                                    start_length,
                                    Some(end_length),
                                    None,
                                )?
                            } else {
                                self.type_checker
                                    .get_type_arguments(source)?
                                    .get(0)
                                    .cloned()
                            };
                            if let Some(rest_type) = rest_type.as_ref() {
                                self.infer_from_types(rest_type, &element_types[start_length])?;
                            }
                        }
                    }
                    for i in 0..end_length {
                        self.infer_from_types(
                            &self.type_checker.get_type_arguments(source)?[source_arity - i - 1],
                            &element_types[target_arity - i - 1],
                        )?;
                    }
                    return Ok(());
                }
                if self.type_checker.is_array_type(target) {
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

    pub(super) fn infer_from_properties(&self, source: &Type, target: &Type) -> io::Result<()> {
        let properties = self.type_checker.get_properties_of_object_type(target)?;
        for ref target_prop in properties {
            let source_prop = self.type_checker.get_property_of_type_(
                source,
                target_prop.escaped_name(),
                None,
            )?;
            if let Some(source_prop) = source_prop.as_ref() {
                self.infer_from_types(
                    &*self.type_checker.get_type_of_symbol(source_prop)?,
                    &*self.type_checker.get_type_of_symbol(target_prop)?,
                )?;
            }
        }

        Ok(())
    }

    pub(super) fn infer_from_signatures(
        &self,
        source: &Type,
        target: &Type,
        kind: SignatureKind,
    ) -> io::Result<()> {
        let source_signatures = self.type_checker.get_signatures_of_type(source, kind)?;
        let target_signatures = self.type_checker.get_signatures_of_type(target, kind)?;
        let source_len = source_signatures.len();
        let target_len = target_signatures.len();
        let len = if source_len < target_len {
            source_len
        } else {
            target_len
        };
        let skip_parameters = get_object_flags(source).intersects(ObjectFlags::NonInferrableType);
        for i in 0..len {
            self.infer_from_signature(
                self.type_checker
                    .get_base_signature(source_signatures[source_len - len + i].clone())?,
                self.type_checker
                    .get_erased_signature(target_signatures[target_len - len + i].clone())?,
                skip_parameters,
            )?;
        }

        Ok(())
    }

    pub(super) fn infer_from_signature(
        &self,
        source: Gc<Signature>,
        target: Gc<Signature>,
        skip_parameters: bool,
    ) -> io::Result<()> {
        if !skip_parameters {
            let save_bivariant = self.bivariant();
            let kind = if let Some(target_declaration) = target.declaration.as_ref() {
                target_declaration.kind()
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
            self.type_checker.apply_to_parameter_types(
                &source,
                &target,
                |s: &Type, t: &Type| self.infer_from_contravariant_types(s, t),
            )?;
            self.set_bivariant(save_bivariant);
        }
        self.type_checker
            .apply_to_return_types(source, target, |s: &Type, t: &Type| {
                self.infer_from_types(s, t)
            })?;

        Ok(())
    }

    pub(super) fn infer_from_index_types(&self, source: &Type, target: &Type) -> io::Result<()> {
        let priority = if (get_object_flags(source) & get_object_flags(target))
            .intersects(ObjectFlags::Mapped)
        {
            InferencePriority::HomomorphicMappedType
        } else {
            InferencePriority::None
        };
        let index_infos = self.type_checker.get_index_infos_of_type(target)?;
        if self
            .type_checker
            .is_object_type_with_inferable_index(source)?
        {
            for target_info in &index_infos {
                let mut prop_types: Vec<Gc<Type>> = vec![];
                for ref prop in self.type_checker.get_properties_of_type(source)? {
                    if self.type_checker.is_applicable_index_type(
                        &*self.type_checker.get_literal_type_from_property(
                            prop,
                            TypeFlags::StringOrNumberLiteralOrUnique,
                            None,
                        )?,
                        &target_info.key_type,
                    )? {
                        let prop_type = self.type_checker.get_type_of_symbol(prop)?;
                        prop_types.push(if prop.flags().intersects(SymbolFlags::Optional) {
                            self.type_checker
                                .remove_missing_or_undefined_type(&prop_type)?
                        } else {
                            prop_type
                        });
                    }
                }
                for info in &self.type_checker.get_index_infos_of_type(source)? {
                    if self
                        .type_checker
                        .is_applicable_index_type(&info.key_type, &target_info.key_type)?
                    {
                        prop_types.push(info.type_.clone());
                    }
                }
                if !prop_types.is_empty() {
                    self.infer_with_priority(
                        &*self.type_checker.get_union_type(
                            &prop_types,
                            None,
                            Option::<&Symbol>::None,
                            None,
                            Option::<&Type>::None,
                        )?,
                        &target_info.type_,
                        priority,
                    )?;
                }
            }
        }
        for target_info in &index_infos {
            let source_info = self
                .type_checker
                .get_applicable_index_info(source, &target_info.key_type)?;
            if let Some(source_info) = source_info.as_ref() {
                self.infer_with_priority(&source_info.type_, &target_info.type_, priority)?;
            }
        }

        Ok(())
    }
}

impl TypeChecker {
    pub(super) fn is_type_or_base_identical_to(&self, s: &Type, t: &Type) -> io::Result<bool> {
        Ok(
            if self.exact_optional_property_types == Some(true) && ptr::eq(t, &*self.missing_type())
            {
                ptr::eq(s, t)
            } else {
                self.is_type_identical_to(s, t)?
                    || (t.flags().intersects(TypeFlags::String)
                        && s.flags().intersects(TypeFlags::StringLiteral)
                        || t.flags().intersects(TypeFlags::Number)
                            && s.flags().intersects(TypeFlags::NumberLiteral))
            },
        )
    }

    pub(super) fn is_type_closely_matched_by(&self, s: &Type, t: &Type) -> bool {
        s.flags().intersects(TypeFlags::Object)
            && t.flags().intersects(TypeFlags::Object)
            && matches!(
                s.maybe_symbol().as_ref(),
                Some(s_symbol) if matches!(
                    t.maybe_symbol().as_ref(),
                    Some(t_symbol) if Gc::ptr_eq(s_symbol, t_symbol)
                )
            )
            || matches!(
                s.maybe_alias_symbol().as_ref(),
                Some(s_alias_symbol) if matches!(
                    t.maybe_alias_symbol().as_ref(),
                    Some(t_alias_symbol) if Gc::ptr_eq(s_alias_symbol, t_alias_symbol)
                )
            ) && s.maybe_alias_type_arguments().is_some()
    }

    pub(super) fn has_primitive_constraint(
        &self,
        type_: &Type, /*TypeParameter*/
    ) -> io::Result<bool> {
        let constraint = self.get_constraint_of_type_parameter(type_)?;
        Ok(matches!(
            constraint.as_ref(),
            Some(constraint) if self.maybe_type_of_kind(
                &*if constraint.flags().intersects(TypeFlags::Conditional) {
                    self.get_default_constraint_of_conditional_type(constraint)?
                } else {
                    constraint.clone()
                },
                TypeFlags::Primitive | TypeFlags::Index | TypeFlags::TemplateLiteral | TypeFlags::StringMapping
            )
        ))
    }

    pub(super) fn is_object_literal_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::ObjectLiteral)
    }

    pub(super) fn is_object_or_array_literal_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::ObjectLiteral | ObjectFlags::ArrayLiteral)
    }

    pub(super) fn union_object_and_array_literal_candidates<'self_and_candidates, TCandidates>(
        &'self_and_candidates self,
        candidates: TCandidates,
    ) -> io::Result<impl Iterator<Item = Gc<Type>> + 'self_and_candidates>
    where
        TCandidates: IntoIterator<Item = &'self_and_candidates Gc<Type>> + Clone,
        TCandidates::IntoIter: Clone + 'self_and_candidates,
    {
        let candidates = candidates.into_iter();
        if candidates.clone().peekmore().is_len_greater_than(1) {
            let mut object_literals = candidates
                .clone()
                .filter(|candidate| self.is_object_or_array_literal_type(candidate))
                .peekable();
            if !object_literals.is_empty_() {
                let literals_type = self.get_union_type(
                    object_literals,
                    Some(UnionReduction::Subtype),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                )?;
                return Ok(Either::Left(
                    candidates
                        .filter(|t| !self.is_object_or_array_literal_type(t))
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
    ) -> io::Result<Gc<Type>> {
        Ok(
            if matches!(
                inference.maybe_priority(),
                Some(inference_priority) if inference_priority.intersects(InferencePriority::PriorityImpliesCombination)
            ) {
                self.get_intersection_type(
                    inference.maybe_contra_candidates().as_deref().unwrap(),
                    Option::<&Symbol>::None,
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
        signature: Gc<Signature>,
    ) -> io::Result<Gc<Type>> {
        let inference_candidates = inference.maybe_candidates();
        let candidates = self
            .union_object_and_array_literal_candidates(inference_candidates.as_deref().unwrap())?;
        let primitive_constraint = self.has_primitive_constraint(&inference.type_parameter)?;
        let widen_literal_types = !primitive_constraint
            && inference.top_level()
            && (inference.is_fixed()
                || !self.is_type_parameter_at_top_level(
                    &*self.get_return_type_of_signature(signature)?,
                    &inference.type_parameter,
                )?);
        let base_candidates: Vec<_> = if primitive_constraint {
            candidates
                .map(|candidate| self.get_regular_type_of_literal_type(&candidate))
                .collect()
        } else if widen_literal_types {
            candidates
                .map(|candidate| self.get_widened_literal_type(&candidate))
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
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?
        } else {
            self.get_common_supertype(&base_candidates)?
        };
        self.get_widened_type(&unwidened_type)
    }

    pub(super) fn get_inferred_type(
        &self,
        context: &InferenceContext,
        index: usize,
    ) -> io::Result<Gc<Type>> {
        let inference = context.inferences()[index].clone();
        if inference.maybe_inferred_type().is_none() {
            let mut inferred_type: Option<Gc<Type>> = None;
            let signature = context.signature.as_ref();
            if let Some(signature) = signature {
                let inferred_covariant_type = if inference.maybe_candidates().is_some() {
                    Some(self.get_covariant_inference(&inference, signature.clone())?)
                } else {
                    None
                };
                if let Some(inference_contra_candidates) =
                    inference.maybe_contra_candidates().as_ref()
                {
                    inferred_type = Some(
                        if let Some(inferred_covariant_type) = inferred_covariant_type
                            .as_ref()
                            .try_filter(|inferred_covariant_type| -> io::Result<_> {
                                Ok(
                                    !inferred_covariant_type.flags().intersects(TypeFlags::Never)
                                        && try_some(
                                            Some(inference_contra_candidates),
                                            Some(|t: &Gc<Type>| {
                                                self.is_type_subtype_of(inferred_covariant_type, t)
                                            }),
                                        )?,
                                )
                            })?
                        {
                            inferred_covariant_type.clone()
                        } else {
                            self.get_contravariant_inference(&inference)?
                        },
                    );
                } else if let Some(inferred_covariant_type) = inferred_covariant_type.as_ref() {
                    inferred_type = Some(inferred_covariant_type.clone());
                } else if context.flags().intersects(InferenceFlags::NoDefault) {
                    inferred_type = Some(self.silent_never_type());
                } else {
                    let default_type =
                        self.get_default_from_type_parameter_(&inference.type_parameter)?;
                    if let Some(default_type) = default_type.as_ref() {
                        inferred_type = Some(self.instantiate_type(
                            default_type,
                            Some(self.merge_type_mappers(
                                Some(Gc::new(self.create_backreference_mapper(context, index))),
                                context.non_fixing_mapper().clone(),
                            )),
                        )?);
                    }
                }
            } else {
                inferred_type = self.get_type_from_inference(&inference)?;
            }

            *inference.maybe_inferred_type_mut() =
                Some(inferred_type.clone().unwrap_or_else(|| {
                    self.get_default_type_argument_type(
                        context.flags().intersects(InferenceFlags::AnyDefault),
                    )
                }));

            let constraint = self.get_constraint_of_type_parameter(&inference.type_parameter)?;
            if let Some(constraint) = constraint.as_ref() {
                let instantiated_constraint =
                    self.instantiate_type(constraint, Some(context.non_fixing_mapper()))?;
                if match inferred_type.as_ref() {
                    None => true,
                    Some(inferred_type) => {
                        context.compare_types.call(
                            inferred_type,
                            &*self.get_type_with_this_argument(
                                &instantiated_constraint,
                                Some(&**inferred_type),
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
                    *inference.maybe_inferred_type_mut() = Some(instantiated_constraint);
                }
            }
        }

        let ret = inference.maybe_inferred_type().unwrap();
        Ok(ret)
    }

    pub(super) fn get_default_type_argument_type(&self, is_in_java_script_file: bool) -> Gc<Type> {
        if is_in_java_script_file {
            self.any_type()
        } else {
            self.unknown_type()
        }
    }

    pub(super) fn get_inferred_types(
        &self,
        context: &InferenceContext,
    ) -> io::Result<Vec<Gc<Type>>> {
        let mut result: Vec<Gc<Type>> = vec![];
        for i in 0..context.inferences().len() {
            result.push(self.get_inferred_type(context, i)?);
        }
        Ok(result)
    }

    pub(super) fn get_cannot_find_name_diagnostic_for_name(
        &self,
        node: &Node,
    ) -> &'static DiagnosticMessage {
        match &*node.as_identifier().escaped_text {
            "document" | "console" => &Diagnostics::Cannot_find_name_0_Do_you_need_to_change_your_target_library_Try_changing_the_lib_compiler_option_to_include_dom,
            "$" => {
                if self.compiler_options.types.is_some() {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_jQuery_Try_npm_i_save_dev_types_Slashjquery_and_then_add_jquery_to_the_types_field_in_your_tsconfig
                } else {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_jQuery_Try_npm_i_save_dev_types_Slashjquery
                }
            }
            "describe" | "suite" | "it" | "test" => {
                if self.compiler_options.types.is_some() {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_a_test_runner_Try_npm_i_save_dev_types_Slashjest_or_npm_i_save_dev_types_Slashmocha_and_then_add_jest_or_mocha_to_the_types_field_in_your_tsconfig
                } else {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_a_test_runner_Try_npm_i_save_dev_types_Slashjest_or_npm_i_save_dev_types_Slashmocha
                }
            }
            "process" | "require" | "Buffer" | "module" => {
                if self.compiler_options.types.is_some() {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_node_Try_npm_i_save_dev_types_Slashnode_and_then_add_node_to_the_types_field_in_your_tsconfig
                } else {
                    &Diagnostics::Cannot_find_name_0_Do_you_need_to_install_type_definitions_for_node_Try_npm_i_save_dev_types_Slashnode
                }
            }
            "Map" | "Set" | "Promise" | "Symbol" | "WeakMap" | "WeakSet" | "Iterator" | "AsyncIterator" | "SharedArrayBuffer" | "Atomics" | "AsyncIterable" | "AsyncIterableIterator" | "AsyncGenerator" | "AsyncGeneratorFunction" | "BigInt" | "Reflect" | "BigInt64Array" | "BigUint64Array" => &Diagnostics::Cannot_find_name_0_Do_you_need_to_change_your_target_library_Try_changing_the_lib_compiler_option_to_1_or_later,
            _ => {
                if node.parent().kind() == SyntaxKind::ShorthandPropertyAssignment {
                    &Diagnostics::No_value_exists_in_scope_for_the_shorthand_property_0_Either_declare_one_or_provide_an_initializer
                } else {
                    &Diagnostics::Cannot_find_name_0
                }
            }
        }
    }
}
