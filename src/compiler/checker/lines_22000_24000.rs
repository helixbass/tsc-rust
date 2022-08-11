#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cmp;
use std::ptr;
use std::rc::Rc;

use super::{InferTypes, TypeFacts};
use crate::{
    concatenate, every, filter, find, flat_map, get_object_flags, is_write_only_access, map,
    node_is_missing, DiagnosticMessage, Diagnostics, ElementFlags, IndexInfo, InferenceContext,
    InferenceInfo, InferencePriority, Node, NodeInterface, ObjectFlags, Signature, SignatureKind,
    Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    UnionReduction,
};

impl InferTypes {
    pub(super) fn get_inference_info_for_type(&self, type_: &Type) -> Option<Rc<InferenceInfo>> {
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
        types: &[Rc<Type>],
    ) -> Option<Rc<Type>> {
        let mut type_variable: Option<Rc<Type>> = None;
        for type_ in types {
            let t = if type_.flags().intersects(TypeFlags::Intersection) {
                find(
                    type_.as_union_or_intersection_type_interface().types(),
                    |t: &Rc<Type>, _| self.get_inference_info_for_type(t).is_some(),
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
                Some(type_variable) if !Rc::ptr_eq(&t, type_variable)
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
        targets: &[Rc<Type>],
        target_flags: TypeFlags,
    ) {
        let mut type_variable_count = 0;
        if target_flags.intersects(TypeFlags::Union) {
            let mut naked_type_variable: Option<Rc<Type>> = None;
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
                        self.infer_from_types(&sources[i], t);
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
                    );
                }
                return;
            }
            if type_variable_count == 1 && !inference_circularity {
                let unmatched = flat_map(Some(&sources), |s: &Rc<Type>, i| {
                    if matched[i] {
                        vec![]
                    } else {
                        vec![s.clone()]
                    }
                });
                if !unmatched.is_empty() {
                    self.infer_from_types(
                        &self.type_checker.get_union_type(
                            unmatched,
                            None,
                            Option::<&Symbol>::None,
                            None,
                            Option::<&Type>::None,
                        ),
                        naked_type_variable.as_deref().unwrap(),
                    );
                    return;
                }
            }
        } else {
            for t in targets {
                if self.get_inference_info_for_type(t).is_some() {
                    type_variable_count += 1;
                } else {
                    self.infer_from_types(source, t);
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
                    self.infer_with_priority(source, t, InferencePriority::NakedTypeVariable);
                }
            }
        }
    }

    pub(super) fn infer_to_mapped_type(
        &self,
        source: &Type,
        target: &Type, /*MappedType*/
        constraint_type: &Type,
    ) -> bool {
        if constraint_type.flags().intersects(TypeFlags::Union) {
            let mut result = false;
            for type_ in constraint_type
                .as_union_or_intersection_type_interface()
                .types()
            {
                result = self.infer_to_mapped_type(source, target, type_) || result;
            }
            return result;
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
                );
                if let Some(inferred_type) = inferred_type.as_ref() {
                    self.infer_with_priority(
                        inferred_type,
                        &inference.type_parameter,
                        if get_object_flags(source).intersects(ObjectFlags::NonInferrableType) {
                            InferencePriority::PartialHomomorphicMappedType
                        } else {
                            InferencePriority::HomomorphicMappedType
                        },
                    );
                }
            }
            return true;
        }
        if constraint_type.flags().intersects(TypeFlags::TypeParameter) {
            self.infer_with_priority(
                &self.type_checker.get_index_type(source, None, None),
                constraint_type,
                InferencePriority::MappedTypeConstraint,
            );
            let extended_constraint = self.type_checker.get_constraint_of_type(constraint_type);
            if matches!(
                extended_constraint.as_ref(),
                Some(extended_constraint) if self.infer_to_mapped_type(source, target, extended_constraint)
            ) {
                return true;
            }
            let prop_types = map(
                &self.type_checker.get_properties_of_type(source),
                |property: &Rc<Symbol>, _| self.type_checker.get_type_of_symbol(property),
            );
            let index_types = map(
                &self.type_checker.get_index_infos_of_type(source),
                |info: &Rc<IndexInfo>, _| {
                    if !Rc::ptr_eq(info, &self.type_checker.enum_number_index_info()) {
                        info.type_.clone()
                    } else {
                        self.type_checker.never_type()
                    }
                },
            );
            self.infer_from_types(
                &self.type_checker.get_union_type(
                    concatenate(prop_types, index_types),
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                ),
                &self.type_checker.get_template_type_from_mapped_type(target),
            );
            return true;
        }
        false
    }

    pub(super) fn infer_to_conditional_type(
        &self,
        source: &Type,
        target: &Type, /*ConditionalType*/
    ) {
        let target_as_conditional_type = target.as_conditional_type();
        if source.flags().intersects(TypeFlags::Conditional) {
            let source_as_conditional_type = source.as_conditional_type();
            self.infer_from_types(
                &source_as_conditional_type.check_type,
                &target_as_conditional_type.check_type,
            );
            self.infer_from_types(
                &source_as_conditional_type.extends_type,
                &target_as_conditional_type.extends_type,
            );
            self.infer_from_types(
                &self
                    .type_checker
                    .get_true_type_from_conditional_type(source),
                &self
                    .type_checker
                    .get_true_type_from_conditional_type(target),
            );
            self.infer_from_types(
                &self
                    .type_checker
                    .get_false_type_from_conditional_type(source),
                &self
                    .type_checker
                    .get_false_type_from_conditional_type(target),
            );
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
                    .get_true_type_from_conditional_type(target),
                self.type_checker
                    .get_false_type_from_conditional_type(target),
            ];
            self.infer_to_multiple_types(source, &target_types, target.flags());
            self.set_priority(save_priority);
        }
    }

    pub(super) fn infer_to_template_literal_type(
        &self,
        source: &Type,
        target: &Type, /*TemplateLiteralType*/
    ) {
        let matches = self
            .type_checker
            .infer_types_from_template_literal_type(source, target);
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
                );
            }
        }
    }

    pub(super) fn infer_from_object_types(&self, source: &Type, target: &Type) {
        if get_object_flags(source).intersects(ObjectFlags::Reference)
            && get_object_flags(target).intersects(ObjectFlags::Reference)
            && (Rc::ptr_eq(
                &source.as_type_reference().target,
                &target.as_type_reference().target,
            ) || self.type_checker.is_array_type(source)
                && self.type_checker.is_array_type(target))
        {
            self.infer_from_type_arguments(
                &self.type_checker.get_type_arguments(source),
                &self.type_checker.get_type_arguments(target),
                &self
                    .type_checker
                    .get_variances(&source.as_type_reference().target),
            );
            return;
        }
        if self.type_checker.is_generic_mapped_type(source)
            && self.type_checker.is_generic_mapped_type(target)
        {
            self.infer_from_types(
                &self
                    .type_checker
                    .get_constraint_type_from_mapped_type(source),
                &self
                    .type_checker
                    .get_constraint_type_from_mapped_type(target),
            );
            self.infer_from_types(
                &self.type_checker.get_template_type_from_mapped_type(source),
                &self.type_checker.get_template_type_from_mapped_type(target),
            );
            let source_name_type = self.type_checker.get_name_type_from_mapped_type(source);
            let target_name_type = self.type_checker.get_name_type_from_mapped_type(target);
            if let (Some(source_name_type), Some(target_name_type)) =
                (source_name_type.as_ref(), target_name_type.as_ref())
            {
                self.infer_from_types(source_name_type, target_name_type);
            }
        }
        if !self.type_checker.types_definitely_unrelated(source, target) {
            if self.type_checker.is_array_type(source) || self.type_checker.is_tuple_type(source) {
                if self.type_checker.is_tuple_type(target) {
                    let source_arity = self.type_checker.get_type_reference_arity(source);
                    let target_arity = self.type_checker.get_type_reference_arity(target);
                    let element_types = self.type_checker.get_type_arguments(target);
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
                                &self.type_checker.get_type_arguments(source)[i],
                                &element_types[i],
                            );
                        }
                        return;
                    }
                    let start_length = if self.type_checker.is_tuple_type(source) {
                        cmp::min(
                            source
                                .as_type_reference()
                                .target
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
                                &source.as_type_reference().target,
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
                            &self.type_checker.get_type_arguments(source)[i],
                            &element_types[i],
                        );
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
                            self.type_checker.get_type_arguments(source)[start_length].clone();
                        for i in start_length..target_arity - end_length {
                            self.infer_from_types(
                                &*if element_flags[i].intersects(ElementFlags::Variadic) {
                                    self.type_checker.create_array_type(&rest_type, None)
                                } else {
                                    rest_type.clone()
                                },
                                &element_types[i],
                            );
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
                                .filter(|target_info| target_info.implied_arity.is_some())
                            {
                                self.infer_from_types(
                                    &self.type_checker.slice_tuple_type(
                                        source,
                                        start_length,
                                        Some(
                                            end_length + source_arity
                                                - target_info.implied_arity.unwrap(),
                                        ),
                                    ),
                                    &element_types[start_length],
                                );
                                self.infer_from_types(
                                    &self.type_checker.slice_tuple_type(
                                        source,
                                        start_length + target_info.implied_arity.unwrap(),
                                        Some(end_length),
                                    ),
                                    &element_types[start_length + 1],
                                );
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
                                )
                            } else {
                                self.type_checker.create_array_type(
                                    &self.type_checker.get_type_arguments(source)[0],
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
                            );
                        } else if middle_length == 1
                            && element_flags[start_length].intersects(ElementFlags::Rest)
                        {
                            let rest_type = if self.type_checker.is_tuple_type(source) {
                                self.type_checker.get_element_type_of_slice_of_tuple_type(
                                    source,
                                    start_length,
                                    Some(end_length),
                                    None,
                                )
                            } else {
                                self.type_checker
                                    .get_type_arguments(source)
                                    .get(0)
                                    .map(Clone::clone)
                            };
                            if let Some(rest_type) = rest_type.as_ref() {
                                self.infer_from_types(rest_type, &element_types[start_length]);
                            }
                        }
                    }
                    for i in 0..end_length {
                        self.infer_from_types(
                            &self.type_checker.get_type_arguments(source)[source_arity - i - 1],
                            &element_types[target_arity - i - 1],
                        );
                    }
                    return;
                }
                if self.type_checker.is_array_type(target) {
                    self.infer_from_index_types(source, target);
                    return;
                }
            }
            self.infer_from_properties(source, target);
            self.infer_from_signatures(source, target, SignatureKind::Call);
            self.infer_from_signatures(source, target, SignatureKind::Construct);
            self.infer_from_index_types(source, target);
        }
    }

    pub(super) fn infer_from_properties(&self, source: &Type, target: &Type) {
        let properties = self.type_checker.get_properties_of_object_type(target);
        for target_prop in &properties {
            let source_prop =
                self.type_checker
                    .get_property_of_type_(source, target_prop.escaped_name(), None);
            if let Some(source_prop) = source_prop.as_ref() {
                self.infer_from_types(
                    &self.type_checker.get_type_of_symbol(source_prop),
                    &self.type_checker.get_type_of_symbol(target_prop),
                );
            }
        }
    }

    pub(super) fn infer_from_signatures(&self, source: &Type, target: &Type, kind: SignatureKind) {
        let source_signatures = self.type_checker.get_signatures_of_type(source, kind);
        let target_signatures = self.type_checker.get_signatures_of_type(target, kind);
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
                    .get_base_signature(source_signatures[source_len - len + i].clone()),
                self.type_checker
                    .get_erased_signature(target_signatures[target_len - len + i].clone()),
                skip_parameters,
            );
        }
    }

    pub(super) fn infer_from_signature(
        &self,
        source: Rc<Signature>,
        target: Rc<Signature>,
        skip_parameters: bool,
    ) {
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
            self.type_checker
                .apply_to_parameter_types(&source, &target, |s: &Type, t: &Type| {
                    self.infer_from_contravariant_types(s, t)
                });
            self.set_bivariant(save_bivariant);
        }
        self.type_checker
            .apply_to_return_types(source, target, |s: &Type, t: &Type| {
                self.infer_from_types(s, t)
            });
    }

    pub(super) fn infer_from_index_types(&self, source: &Type, target: &Type) {
        let priority = if (get_object_flags(source) & get_object_flags(target))
            .intersects(ObjectFlags::Mapped)
        {
            InferencePriority::HomomorphicMappedType
        } else {
            InferencePriority::None
        };
        let index_infos = self.type_checker.get_index_infos_of_type(target);
        if self
            .type_checker
            .is_object_type_with_inferable_index(source)
        {
            for target_info in &index_infos {
                let mut prop_types: Vec<Rc<Type>> = vec![];
                for prop in &self.type_checker.get_properties_of_type(source) {
                    if self.type_checker.is_applicable_index_type(
                        &self.type_checker.get_literal_type_from_property(
                            prop,
                            TypeFlags::StringOrNumberLiteralOrUnique,
                            None,
                        ),
                        &target_info.key_type,
                    ) {
                        let prop_type = self.type_checker.get_type_of_symbol(prop);
                        prop_types.push(if prop.flags().intersects(SymbolFlags::Optional) {
                            self.type_checker
                                .remove_missing_or_undefined_type(&prop_type)
                        } else {
                            prop_type
                        });
                    }
                }
                for info in &self.type_checker.get_index_infos_of_type(source) {
                    if self
                        .type_checker
                        .is_applicable_index_type(&info.key_type, &target_info.key_type)
                    {
                        prop_types.push(info.type_.clone());
                    }
                }
                if !prop_types.is_empty() {
                    self.infer_with_priority(
                        &self.type_checker.get_union_type(
                            prop_types,
                            None,
                            Option::<&Symbol>::None,
                            None,
                            Option::<&Type>::None,
                        ),
                        &target_info.type_,
                        priority,
                    );
                }
            }
        }
        for target_info in &index_infos {
            let source_info = self
                .type_checker
                .get_applicable_index_info(source, &target_info.key_type);
            if let Some(source_info) = source_info.as_ref() {
                self.infer_with_priority(&source_info.type_, &target_info.type_, priority);
            }
        }
    }
}

impl TypeChecker {
    pub(super) fn is_type_or_base_identical_to(&self, s: &Type, t: &Type) -> bool {
        if self.exact_optional_property_types == Some(true) && ptr::eq(t, &*self.missing_type()) {
            ptr::eq(s, t)
        } else {
            self.is_type_identical_to(s, t)
                || (t.flags().intersects(TypeFlags::String)
                    && s.flags().intersects(TypeFlags::StringLiteral)
                    || t.flags().intersects(TypeFlags::Number)
                        && s.flags().intersects(TypeFlags::NumberLiteral))
        }
    }

    pub(super) fn is_type_closely_matched_by(&self, s: &Type, t: &Type) -> bool {
        s.flags().intersects(TypeFlags::Object)
            && t.flags().intersects(TypeFlags::Object)
            && matches!(
                s.maybe_symbol().as_ref(),
                Some(s_symbol) if matches!(
                    t.maybe_symbol().as_ref(),
                    Some(t_symbol) if Rc::ptr_eq(s_symbol, t_symbol)
                )
            )
            || matches!(
                s.maybe_alias_symbol().as_ref(),
                Some(s_alias_symbol) if matches!(
                    t.maybe_alias_symbol().as_ref(),
                    Some(t_alias_symbol) if Rc::ptr_eq(s_alias_symbol, t_alias_symbol)
                )
            ) && s.maybe_alias_type_arguments().is_some()
    }

    pub(super) fn has_primitive_constraint(&self, type_: &Type /*TypeParameter*/) -> bool {
        let constraint = self.get_constraint_of_type_parameter(type_);
        matches!(
            constraint.as_ref(),
            Some(constraint) if self.maybe_type_of_kind(
                &*if constraint.flags().intersects(TypeFlags::Conditional) {
                    self.get_default_constraint_of_conditional_type(constraint)
                } else {
                    constraint.clone()
                },
                TypeFlags::Primitive | TypeFlags::Index | TypeFlags::TemplateLiteral | TypeFlags::StringMapping
            )
        )
    }

    pub(super) fn is_object_literal_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::ObjectLiteral)
    }

    pub(super) fn is_object_or_array_literal_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::ObjectLiteral | ObjectFlags::ArrayLiteral)
    }

    pub(super) fn union_object_and_array_literal_candidates(
        &self,
        candidates: &[Rc<Type>],
    ) -> Vec<Rc<Type>> {
        if candidates.len() > 1 {
            let object_literals = filter(candidates, |candidate: &Rc<Type>| {
                self.is_object_or_array_literal_type(candidate)
            });
            if !object_literals.is_empty() {
                let literals_type = self.get_union_type(
                    object_literals,
                    Some(UnionReduction::Subtype),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                );
                return concatenate(
                    filter(candidates, |t: &Rc<Type>| {
                        !self.is_object_or_array_literal_type(t)
                    }),
                    vec![literals_type],
                );
            }
        }
        candidates.to_owned()
    }

    pub(super) fn get_contravariant_inference(&self, inference: &InferenceInfo) -> Rc<Type> {
        if matches!(
            inference.maybe_priority(),
            Some(inference_priority) if inference_priority.intersects(InferencePriority::PriorityImpliesCombination)
        ) {
            self.get_intersection_type(
                inference.maybe_contra_candidates().as_deref().unwrap(),
                Option::<&Symbol>::None,
                None,
            )
        } else {
            self.get_common_subtype(inference.maybe_contra_candidates().as_deref().unwrap())
        }
    }

    pub(super) fn get_inferred_type(&self, context: &InferenceContext, index: usize) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_default_type_argument_type(&self, is_in_java_script_file: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_cannot_find_name_diagnostic_for_name(
        &self,
        node: &Node,
    ) -> &'static DiagnosticMessage {
        match node.as_identifier().escaped_text {
            _ => {
                if false {
                    unimplemented!()
                } else {
                    &Diagnostics::Cannot_find_name_0
                }
            }
        }
    }

    pub(super) fn get_resolved_symbol(&self, node: &Node /*Identifier*/) -> Rc<Symbol> {
        let links = self.get_node_links(node);
        let mut links_ref = links.borrow_mut();
        if links_ref.resolved_symbol.is_none() {
            links_ref.resolved_symbol = Some(if !node_is_missing(Some(node)) {
                self.resolve_name_(
                    Some(node),
                    &node.as_identifier().escaped_text,
                    SymbolFlags::Value | SymbolFlags::ExportValue,
                    Some(self.get_cannot_find_name_diagnostic_for_name(node)),
                    Some(node.node_wrapper()),
                    !is_write_only_access(node),
                    Some(false),
                )
                .unwrap_or_else(|| self.unknown_symbol())
            } else {
                self.unknown_symbol()
            });
        }
        links_ref.resolved_symbol.clone().unwrap()
    }

    pub(super) fn is_in_type_query(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_matching_reference(&self, source: &Node, target: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn find_discriminant_properties(
        &self,
        source_properties: &[Rc<Symbol>],
        target: &Type,
    ) -> Option<Vec<Rc<Symbol>>> {
        unimplemented!()
    }

    pub(super) fn get_matching_union_constituent_for_type(
        &self,
        union_type: &Type, /*UnionType*/
        type_: &Type,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn has_matching_argument(
        &self,
        expression: &Node, /*CallExpression | NewExpression*/
        reference: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_function_object_type(&self, type_: &Type /*ObjectType*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_type_facts(&self, type_: &Type, ignore_objects: Option<bool>) -> TypeFacts {
        let ignore_objects = ignore_objects.unwrap_or(false);
        unimplemented!()
    }

    pub(super) fn get_type_with_facts(&self, type_: &Type, include: TypeFacts) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_of_initializer(&self, node: &Node /*Expression*/) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_type_subset_of(&self, source: &Type, target: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn for_each_type<TReturn, TCallback: FnMut(&Type) -> Option<TReturn>>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> Option<TReturn> {
        unimplemented!()
    }

    pub(super) fn some_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn every_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn filter_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            unimplemented!()
        }
        if type_.flags().intersects(TypeFlags::Never) || f(type_) {
            type_.type_wrapper()
        } else {
            self.never_type()
        }
    }

    pub(super) fn remove_type(&self, type_: &Type, target_type: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn count_types(&self, type_: &Type) -> usize {
        unimplemented!()
    }

    pub(super) fn map_type<TMapper: FnMut(&Type) -> Option<Rc<Type>>>(
        &self,
        type_: &Type,
        mapper: &mut TMapper,
        no_reductions: Option<bool>,
    ) -> Option<Rc<Type>> {
        let no_reductions = no_reductions.unwrap_or(false);
        if type_.flags().intersects(TypeFlags::Never) {
            return Some(type_.type_wrapper());
        }
        if !type_.flags().intersects(TypeFlags::Union) {
            return mapper(type_);
        }
        let types = type_.as_union_or_intersection_type_interface().types();
        let mut mapped_types: Vec<Rc<Type>> = vec![];
        let mut changed = false;
        for t in types {
            let mapped = if t.flags().intersects(TypeFlags::Union) {
                self.map_type(&t, mapper, Some(no_reductions))
            } else {
                mapper(&t)
            };
            changed = changed
                || match mapped.as_ref() {
                    None => true,
                    Some(mapped) => !Rc::ptr_eq(t, mapped),
                };
            if let Some(mapped) = mapped {
                mapped_types.push(mapped);
            }
        }
        if changed {
            if !mapped_types.is_empty() {
                Some(self.get_union_type(
                    mapped_types,
                    Some(if no_reductions {
                        UnionReduction::None
                    } else {
                        UnionReduction::Literal
                    }),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                ))
            } else {
                None
            }
        } else {
            Some(type_.type_wrapper())
        }
    }

    pub(super) fn map_type_with_alias<
        TMapper: FnMut(&Type) -> Rc<Type>,
        TAliasSymbol: Borrow<Symbol>,
    >(
        &self,
        type_: &Type,
        mapper: &mut TMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_constituent_count(&self, type_: &Type) -> usize {
        if type_.flags().intersects(TypeFlags::Union) {
            type_
                .as_union_or_intersection_type_interface()
                .types()
                .len()
        } else {
            1
        }
    }

    pub(super) fn extract_types_of_kind(&self, type_: &Type, kind: TypeFlags) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_flow_type_of_reference<
        TInitialType: Borrow<Type>,
        TFlowContainer: Borrow<Node>,
    >(
        &self,
        reference: &Node,
        declared_type: &Type,
        initial_type: Option<TInitialType>,
        flow_container: Option<TFlowContainer>,
    ) -> Rc<Type> {
        let initial_type = initial_type.map_or_else(
            || declared_type.type_wrapper(),
            |initial_type| initial_type.borrow().type_wrapper(),
        );
        unimplemented!()
    }
}
