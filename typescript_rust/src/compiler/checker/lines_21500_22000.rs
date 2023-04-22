#![allow(non_upper_case_globals)]

use gc::{Finalize, Gc, GcCell, GcCellRefMut, Trace};
use std::cell::{Cell, RefCell, RefMut};
use std::cmp;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{ExpandingFlags, RecursionIdentity};
use crate::{
    append_if_unique_gc, append_if_unique_rc, arrays_equal, contains, contains_gc, contains_rc,
    create_scanner, every, filter, get_check_flags, get_object_flags, map, some, CheckFlags,
    ElementFlags, InferenceInfo, InferencePriority, Node, ObjectFlags, ScriptTarget, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, TokenFlags, Type, TypeChecker, TypeFlags,
    TypeInterface, UnionReduction, VarianceFlags,
};

impl TypeChecker {
    pub(super) fn infer_reverse_mapped_type(
        &self,
        source_type: &Type,
        target: &Type,     /*MappedType*/
        constraint: &Type, /*IndexType*/
    ) -> Gc<Type> {
        let type_parameter = self.get_indexed_access_type(
            &constraint.as_index_type().type_,
            &self.get_type_parameter_from_mapped_type(target),
            None,
            Option::<&Node>::None,
            Option::<&Symbol>::None,
            None,
        );
        let template_type = self.get_template_type_from_mapped_type(target);
        let inference = Gc::new(self.create_inference_info(&type_parameter));
        self.infer_types(
            &vec![inference.clone()],
            source_type,
            &template_type,
            None,
            None,
        );
        self.get_type_from_inference(&inference)
            .unwrap_or_else(|| self.unknown_type())
    }

    pub(super) fn get_unmatched_properties(
        &self,
        source: &Type,
        target: &Type,
        require_optional_properties: bool,
        match_discriminant_properties: bool,
    ) -> Vec<Gc<Symbol>> {
        let properties = self.get_properties_of_type(target);
        let mut ret: Vec<Gc<Symbol>> = vec![];
        for ref target_prop in properties {
            if self.is_static_private_identifier_property(target_prop) {
                continue;
            }
            if require_optional_properties
                || !(target_prop.flags().intersects(SymbolFlags::Optional)
                    || get_check_flags(target_prop).intersects(CheckFlags::Partial))
            {
                let source_prop =
                    self.get_property_of_type_(source, target_prop.escaped_name(), None);
                match source_prop {
                    None => {
                        ret.push(target_prop.clone());
                    }
                    Some(ref source_prop) => {
                        if match_discriminant_properties {
                            let target_type = self.get_type_of_symbol(target_prop);
                            if target_type.flags().intersects(TypeFlags::Unit) {
                                let source_type = self.get_type_of_symbol(source_prop);
                                if !(source_type.flags().intersects(TypeFlags::Any)
                                    || Gc::ptr_eq(
                                        &self.get_regular_type_of_literal_type(&source_type),
                                        &self.get_regular_type_of_literal_type(&target_type),
                                    ))
                                {
                                    ret.push(target_prop.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
        ret
    }

    pub(super) fn get_unmatched_property(
        &self,
        source: &Type,
        target: &Type,
        require_optional_properties: bool,
        match_discriminant_properties: bool,
    ) -> Option<Gc<Symbol>> {
        let result = self.get_unmatched_properties(
            source,
            target,
            require_optional_properties,
            match_discriminant_properties,
        );
        result.get(0).map(Clone::clone)
    }

    pub(super) fn tuple_types_definitely_unrelated(
        &self,
        source: &Type, /*TupleTypeReference*/
        target: &Type, /*TupleTypeReference*/
    ) -> bool {
        let source_target = source.as_type_reference_interface().target();
        let source_target_as_tuple_type = source_target.as_tuple_type();
        let target_target_as_tuple_type = target.as_type_reference().target.as_tuple_type();
        !target_target_as_tuple_type
            .combined_flags
            .intersects(ElementFlags::Variadic)
            && target_target_as_tuple_type.min_length > source_target_as_tuple_type.min_length
            || !target_target_as_tuple_type.has_rest_element
                && (source_target_as_tuple_type.has_rest_element
                    || target_target_as_tuple_type.fixed_length
                        < source_target_as_tuple_type.fixed_length)
    }

    pub(super) fn types_definitely_unrelated(&self, source: &Type, target: &Type) -> bool {
        if self.is_tuple_type(source) && self.is_tuple_type(target) {
            self.tuple_types_definitely_unrelated(source, target)
        } else {
            self.get_unmatched_property(source, target, false, true)
                .is_some()
                && self
                    .get_unmatched_property(target, source, false, false)
                    .is_some()
        }
    }

    pub(super) fn get_type_from_inference(&self, inference: &InferenceInfo) -> Option<Gc<Type>> {
        if let Some(inference_candidates) = inference.maybe_candidates().as_ref() {
            Some(self.get_union_type(
                inference_candidates,
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            ))
        } else if let Some(inference_contra_candidates) =
            inference.maybe_contra_candidates().as_ref()
        {
            Some(self.get_intersection_type(
                inference_contra_candidates,
                Option::<&Symbol>::None,
                None,
            ))
        } else {
            None
        }
    }

    pub(super) fn has_skip_direct_inference_flag(&self, node: &Node) -> bool {
        (*self.get_node_links(node)).borrow().skip_direct_inference == Some(true)
    }

    pub(super) fn is_from_inference_blocked_source(&self, type_: &Type) -> bool {
        matches!(
            type_.maybe_symbol().as_ref(),
            Some(type_symbol) if some(
                type_symbol.maybe_declarations().as_deref(),
                Some(|declaration: &Gc<Node>| self.has_skip_direct_inference_flag(declaration))
            )
        )
    }

    pub(super) fn template_literal_types_definitely_unrelated(
        &self,
        source: &Type, /*TemplateLiteralType*/
        target: &Type, /*TemplateLiteralType*/
    ) -> bool {
        let source_as_template_literal_type = source.as_template_literal_type();
        let target_as_template_literal_type = target.as_template_literal_type();
        let source_start = &source_as_template_literal_type.texts[0];
        let target_start = &target_as_template_literal_type.texts[0];
        let source_end =
            &source_as_template_literal_type.texts[source_as_template_literal_type.texts.len() - 1];
        let target_end =
            &target_as_template_literal_type.texts[target_as_template_literal_type.texts.len() - 1];
        let start_len = cmp::min(source_start.len(), target_start.len());
        let end_len = cmp::min(source_end.len(), target_end.len());
        &source_start[0..start_len] != &target_start[0..start_len]
            || &source_end[source_end.len() - end_len..]
                != &target_end[target_end.len() - end_len..]
    }

    pub(super) fn is_valid_big_int_string(&self, s: &str) -> bool {
        let mut scanner = create_scanner(ScriptTarget::ESNext, false, None, None, None, None, None);
        // TODO: if ErrorCallback was FnMut instead of Fn then using Cell here presumably wouldn't be necessary
        let success = Cell::new(true);
        let text = format!("{}n", s);
        scanner.set_text(Some(text.chars().collect()), Some(text), None, None);
        let mut result = scanner.scan(Some(&|_, _| {
            success.set(false);
        }));
        if result == SyntaxKind::MinusToken {
            result = scanner.scan(Some(&|_, _| {
                success.set(false);
            }));
        }
        let flags = scanner.get_token_flags();
        success.get()
            && result == SyntaxKind::BigIntLiteral
            && scanner.get_text_pos() == s.len() + 1
            && !flags.intersects(TokenFlags::ContainsSeparator)
    }

    pub(super) fn is_valid_type_for_template_literal_placeholder(
        &self,
        source: &Type,
        target: &Type, /*TemplateLiteralType*/
    ) -> bool {
        if ptr::eq(source, target)
            || target
                .flags()
                .intersects(TypeFlags::Any | TypeFlags::String)
        {
            return true;
        }
        if source.flags().intersects(TypeFlags::StringLiteral) {
            let value = &source.as_string_literal_type().value;
            return target.flags().intersects(TypeFlags::Number)
                && value != ""
                && value.parse::<f64>().is_ok()
                || target.flags().intersects(TypeFlags::BigInt)
                    && value != ""
                    && self.is_valid_big_int_string(value)
                || target
                    .flags()
                    .intersects(TypeFlags::BooleanLiteral | TypeFlags::Nullable)
                    && value == target.as_intrinsic_type().intrinsic_name();
        }
        if source.flags().intersects(TypeFlags::TemplateLiteral) {
            let texts = &source.as_template_literal_type().texts;
            return texts.len() == 2
                && texts[0] == ""
                && texts[1] == ""
                && self.is_type_assignable_to(&source.as_template_literal_type().types[0], target);
        }
        self.is_type_assignable_to(source, target)
    }

    pub(super) fn infer_types_from_template_literal_type(
        &self,
        source: &Type,
        target: &Type, /*TemplateLiteralType*/
    ) -> Option<Vec<Gc<Type>>> {
        let target_as_template_literal_type = target.as_template_literal_type();
        if source.flags().intersects(TypeFlags::StringLiteral) {
            self.infer_from_literal_parts_to_template_literal(
                &vec![source.as_string_literal_type().value.clone()],
                &vec![],
                target,
            )
        } else if source.flags().intersects(TypeFlags::TemplateLiteral) {
            let source_as_template_literal_type = source.as_template_literal_type();
            if arrays_equal(
                &source_as_template_literal_type.texts,
                &target_as_template_literal_type.texts,
            ) {
                Some(map(
                    &source_as_template_literal_type.types,
                    |type_: &Gc<Type>, _| self.get_string_like_type_for_type(type_),
                ))
            } else {
                self.infer_from_literal_parts_to_template_literal(
                    &source_as_template_literal_type.texts,
                    &source_as_template_literal_type.types,
                    target,
                )
            }
        } else {
            None
        }
    }

    pub(super) fn is_type_matched_by_template_literal_type(
        &self,
        source: &Type,
        target: &Type, /*TemplateLiteralType*/
    ) -> bool {
        let inferences = self.infer_types_from_template_literal_type(source, target);
        let target_as_template_literal_type = target.as_template_literal_type();
        matches!(
            inferences.as_ref(),
            Some(inferences) if every(
                inferences,
                |r: &Gc<Type>, i| {
                    self.is_valid_type_for_template_literal_placeholder(
                        r,
                        &target_as_template_literal_type.types[i],
                    )
                }
            )
        )
    }

    pub(super) fn get_string_like_type_for_type(&self, type_: &Type) -> Gc<Type> {
        if type_
            .flags()
            .intersects(TypeFlags::Any | TypeFlags::StringLike)
        {
            type_.type_wrapper()
        } else {
            self.get_template_literal_type(
                &vec!["".to_owned(), "".to_owned()],
                &vec![type_.type_wrapper()],
            )
        }
    }

    pub(super) fn infer_from_literal_parts_to_template_literal(
        &self,
        source_texts: &[String],
        source_types: &[Gc<Type>],
        target: &Type, /*TemplateLiteralType*/
    ) -> Option<Vec<Gc<Type>>> {
        let last_source_index = source_texts.len() - 1;
        let source_start_text = &source_texts[0];
        // TODO: should be using chars for any of this instead?
        let source_end_text = &source_texts[last_source_index];
        let target_texts = &target.as_template_literal_type().texts;
        let last_target_index = target_texts.len() - 1;
        let target_start_text = &target_texts[0];
        let target_end_text = &target_texts[last_target_index];
        if last_source_index == 0
            && source_start_text.len() < target_start_text.len() + target_end_text.len()
            || !source_start_text.starts_with(target_start_text)
            || !source_end_text.ends_with(target_end_text)
        {
            return None;
        }
        let remaining_end_text = &source_end_text[0..source_end_text.len() - target_end_text.len()];
        let mut matches: Vec<Gc<Type>> = vec![];
        let mut seg = 0;
        let mut pos = target_start_text.len();
        for i in 1..last_target_index {
            let delim = &target_texts[i];
            if !delim.is_empty() {
                let mut s = seg;
                let mut p = pos;
                loop {
                    let maybe_p = self.get_source_text(
                        source_texts,
                        remaining_end_text,
                        last_source_index,
                        s,
                    )[p..]
                        .find(delim)
                        .map(|position| position + p);
                    if let Some(maybe_p) = maybe_p {
                        p = maybe_p;
                        break;
                    }
                    s += 1;
                    if s == source_texts.len() {
                        return None;
                    }
                    p = 0;
                }
                self.add_match(
                    source_texts,
                    remaining_end_text,
                    last_source_index,
                    &mut seg,
                    &mut pos,
                    &mut matches,
                    source_types,
                    s,
                    p,
                );
                pos += delim.len();
            } else if pos
                < self
                    .get_source_text(source_texts, remaining_end_text, last_source_index, seg)
                    .len()
            {
                let s = seg;
                let p = pos + 1;
                self.add_match(
                    source_texts,
                    remaining_end_text,
                    last_source_index,
                    &mut seg,
                    &mut pos,
                    &mut matches,
                    source_types,
                    s,
                    p,
                );
            } else if seg < last_source_index {
                let s = seg + 1;
                self.add_match(
                    source_texts,
                    remaining_end_text,
                    last_source_index,
                    &mut seg,
                    &mut pos,
                    &mut matches,
                    source_types,
                    s,
                    0,
                );
            } else {
                return None;
            }
        }
        self.add_match(
            source_texts,
            remaining_end_text,
            last_source_index,
            &mut seg,
            &mut pos,
            &mut matches,
            source_types,
            last_source_index,
            self.get_source_text(
                source_texts,
                remaining_end_text,
                last_source_index,
                last_source_index,
            )
            .len(),
        );
        Some(matches)
    }

    pub(super) fn get_source_text<'args>(
        &self,
        source_texts: &'args [String],
        remaining_end_text: &'args str,
        last_source_index: usize,
        index: usize,
    ) -> &'args str {
        if index < last_source_index {
            &source_texts[index]
        } else {
            remaining_end_text
        }
    }

    pub(super) fn add_match(
        &self,
        source_texts: &[String],
        remaining_end_text: &str,
        last_source_index: usize,
        seg: &mut usize,
        pos: &mut usize,
        matches: &mut Vec<Gc<Type>>,
        source_types: &[Gc<Type>],
        s: usize,
        p: usize,
    ) {
        let match_type = if s == *seg {
            self.get_string_literal_type(
                &self.get_source_text(source_texts, remaining_end_text, last_source_index, s)
                    [*pos..p],
            )
        } else {
            self.get_template_literal_type(
                &{
                    let mut texts: Vec<String> = vec![source_texts[*seg][*pos..].to_owned()];
                    texts.extend_from_slice(&source_texts[*seg + 1..s]);
                    texts.push(
                        self.get_source_text(
                            source_texts,
                            remaining_end_text,
                            last_source_index,
                            s,
                        )[0..p]
                            .to_owned(),
                    );
                    texts
                },
                &source_types[*seg..s],
            )
        };
        matches.push(match_type);
        *seg = s;
        *pos = p;
    }

    pub(super) fn infer_types(
        &self,
        inferences: &[Gc<InferenceInfo>],
        original_source: &Type,
        original_target: &Type,
        priority: Option<InferencePriority>,
        contravariant: Option<bool>,
    ) {
        let priority = priority.unwrap_or(InferencePriority::None);
        let contravariant = contravariant.unwrap_or(false);
        InferTypes::new(
            self.rc_wrapper(),
            inferences.to_owned(),
            original_target.type_wrapper(),
            priority,
            contravariant,
        )
        .infer_from_types(original_source, original_target);
    }
}

#[derive(Trace, Finalize)]
pub(super) struct InferTypes {
    pub type_checker: Gc<TypeChecker>,
    pub inferences: Vec<Gc<InferenceInfo>>,
    pub original_target: Gc<Type>,
    #[unsafe_ignore_trace]
    priority: Cell<InferencePriority>,
    #[unsafe_ignore_trace]
    contravariant: Cell<bool>,
    #[unsafe_ignore_trace]
    bivariant: Cell<bool>,
    propagation_type: GcCell<Option<Gc<Type>>>,
    #[unsafe_ignore_trace]
    inference_priority: Cell<InferencePriority>,
    #[unsafe_ignore_trace]
    allow_complex_constraint_inference: Cell<bool>,
    #[unsafe_ignore_trace]
    visited: RefCell<Option<HashMap<String, InferencePriority>>>,
    source_stack: GcCell<Option<Vec<RecursionIdentity>>>,
    target_stack: GcCell<Option<Vec<RecursionIdentity>>>,
    #[unsafe_ignore_trace]
    expanding_flags: Cell<ExpandingFlags>,
}

impl InferTypes {
    pub(super) fn new(
        type_checker: Gc<TypeChecker>,
        inferences: Vec<Gc<InferenceInfo>>,
        original_target: Gc<Type>,
        priority: InferencePriority,
        contravariant: bool,
    ) -> Self {
        Self {
            type_checker,
            inferences,
            original_target,
            priority: Cell::new(priority),
            contravariant: Cell::new(contravariant),
            bivariant: Default::default(),
            propagation_type: Default::default(),
            inference_priority: Cell::new(InferencePriority::MaxValue),
            allow_complex_constraint_inference: Cell::new(true),
            visited: Default::default(),
            source_stack: Default::default(),
            target_stack: Default::default(),
            expanding_flags: Cell::new(ExpandingFlags::None),
        }
    }

    pub(super) fn priority(&self) -> InferencePriority {
        self.priority.get()
    }

    pub(super) fn set_priority(&self, priority: InferencePriority) {
        self.priority.set(priority);
    }

    pub(super) fn contravariant(&self) -> bool {
        self.contravariant.get()
    }

    pub(super) fn set_contravariant(&self, contravariant: bool) {
        self.contravariant.set(contravariant);
    }

    pub(super) fn bivariant(&self) -> bool {
        self.bivariant.get()
    }

    pub(super) fn set_bivariant(&self, bivariant: bool) {
        self.bivariant.set(bivariant);
    }

    pub(super) fn maybe_propagation_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.propagation_type.borrow_mut()
    }

    pub(super) fn inference_priority(&self) -> InferencePriority {
        self.inference_priority.get()
    }

    pub(super) fn set_inference_priority(&self, inference_priority: InferencePriority) {
        self.inference_priority.set(inference_priority);
    }

    pub(super) fn allow_complex_constraint_inference(&self) -> bool {
        self.allow_complex_constraint_inference.get()
    }

    pub(super) fn set_allow_complex_constraint_inference(
        &self,
        allow_complex_constraint_inference: bool,
    ) {
        self.allow_complex_constraint_inference
            .set(allow_complex_constraint_inference);
    }

    pub(super) fn maybe_visited(&self) -> RefMut<Option<HashMap<String, InferencePriority>>> {
        self.visited.borrow_mut()
    }

    pub(super) fn maybe_source_stack(&self) -> GcCellRefMut<Option<Vec<RecursionIdentity>>> {
        self.source_stack.borrow_mut()
    }

    pub(super) fn maybe_target_stack(&self) -> GcCellRefMut<Option<Vec<RecursionIdentity>>> {
        self.target_stack.borrow_mut()
    }

    pub(super) fn expanding_flags(&self) -> ExpandingFlags {
        self.expanding_flags.get()
    }

    pub(super) fn set_expanding_flags(&self, expanding_flags: ExpandingFlags) {
        self.expanding_flags.set(expanding_flags);
    }

    pub(super) fn infer_from_types(&self, source: &Type, target: &Type) {
        if !self.type_checker.could_contain_type_variables(target) {
            return;
        }
        if ptr::eq(source, &*self.type_checker.wildcard_type()) {
            let save_propagation_type = self.maybe_propagation_type().clone();
            *self.maybe_propagation_type() = Some(source.type_wrapper());
            self.infer_from_types(target, target);
            *self.maybe_propagation_type() = save_propagation_type;
            return;
        }
        if let Some(ref source_alias_symbol) = source.maybe_alias_symbol().clone().filter(|source_alias_symbol| {
            matches!(
                target.maybe_alias_symbol().as_ref(),
                Some(target_alias_symbol) if Gc::ptr_eq(source_alias_symbol, target_alias_symbol)
            )
        }) {
            if let Some(source_alias_type_arguments) = source.maybe_alias_type_arguments().as_ref() {
                self.infer_from_type_arguments(
                    source_alias_type_arguments,
                    target.maybe_alias_type_arguments().as_ref().unwrap(),
                    &self.type_checker.get_alias_variances(source_alias_symbol),
                );
                return;
            }
        }
        if ptr::eq(source, target) && source.flags().intersects(TypeFlags::UnionOrIntersection) {
            for t in source.as_union_or_intersection_type_interface().types() {
                self.infer_from_types(t, t);
            }
            return;
        }
        let mut target = target.type_wrapper();
        let mut source = source.type_wrapper();
        if target.flags().intersects(TypeFlags::Union) {
            let (temp_sources, temp_targets) = self.infer_from_matching_types(
                &if source.flags().intersects(TypeFlags::Union) {
                    source
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned()
                } else {
                    vec![source.clone()]
                },
                target.as_union_or_intersection_type_interface().types(),
                |s: &Type, t: &Type| self.type_checker.is_type_or_base_identical_to(s, t),
            );
            let (sources, targets) = self.infer_from_matching_types(
                &temp_sources,
                &temp_targets,
                |s: &Type, t: &Type| self.type_checker.is_type_closely_matched_by(s, t),
            );
            if targets.is_empty() {
                return;
            }
            target = self.type_checker.get_union_type(
                &targets,
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            );
            if sources.is_empty() {
                self.infer_with_priority(&source, &target, InferencePriority::NakedTypeVariable);
                return;
            }
            source = self.type_checker.get_union_type(
                &sources,
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            );
        } else if target.flags().intersects(TypeFlags::Intersection)
            && some(
                Some(target.as_union_or_intersection_type_interface().types()),
                Some(|t: &Gc<Type>| {
                    self.get_inference_info_for_type(t).is_some()
                        || (self.type_checker.is_generic_mapped_type(t)
                            && self
                                .get_inference_info_for_type(
                                    &self
                                        .type_checker
                                        .get_homomorphic_type_variable(t)
                                        .unwrap_or_else(|| self.type_checker.never_type()),
                                )
                                .is_some())
                }),
            )
        {
            if !source.flags().intersects(TypeFlags::Union) {
                let (sources, targets) = self.infer_from_matching_types(
                    &if source.flags().intersects(TypeFlags::Intersection) {
                        source
                            .as_union_or_intersection_type_interface()
                            .types()
                            .to_owned()
                    } else {
                        vec![source.clone()]
                    },
                    target.as_union_or_intersection_type_interface().types(),
                    |s: &Type, t: &Type| self.type_checker.is_type_identical_to(s, t),
                );
                if sources.is_empty() || targets.is_empty() {
                    return;
                }
                source = self.type_checker.get_intersection_type(
                    &sources,
                    Option::<&Symbol>::None,
                    None,
                );
                target = self.type_checker.get_intersection_type(
                    &targets,
                    Option::<&Symbol>::None,
                    None,
                );
            }
        } else if target
            .flags()
            .intersects(TypeFlags::IndexedAccess | TypeFlags::Substitution)
        {
            target = self.type_checker.get_actual_type_variable(&target);
        }
        if target.flags().intersects(TypeFlags::TypeVariable) {
            if get_object_flags(&source).intersects(ObjectFlags::NonInferrableType)
                || Gc::ptr_eq(&source, &self.type_checker.non_inferrable_any_type())
                || Gc::ptr_eq(&source, &self.type_checker.silent_never_type())
                || self.priority().intersects(InferencePriority::ReturnType)
                    && (Gc::ptr_eq(&source, &self.type_checker.auto_type())
                        || Gc::ptr_eq(&source, &self.type_checker.auto_array_type()))
                || self.type_checker.is_from_inference_blocked_source(&source)
            {
                return;
            }
            let inference = self.get_inference_info_for_type(&target);
            if let Some(inference) = inference.as_ref() {
                if !inference.is_fixed() {
                    if match inference.maybe_priority() {
                        None => true,
                        Some(inference_priority) => self.priority() < inference_priority,
                    } {
                        *inference.maybe_candidates_mut() = None;
                        *inference.maybe_contra_candidates_mut() = None;
                        inference.set_top_level(true);
                        inference.set_priority(Some(self.priority()));
                    }
                    if Some(self.priority()) == inference.maybe_priority() {
                        let candidate = self
                            .maybe_propagation_type()
                            .clone()
                            .unwrap_or_else(|| source.clone());
                        if self.contravariant() && !self.bivariant() {
                            if !contains_gc(
                                inference.maybe_contra_candidates().as_deref(),
                                &candidate,
                            ) {
                                if inference.maybe_contra_candidates().is_none() {
                                    *inference.maybe_contra_candidates_mut() = Some(vec![]);
                                }
                                inference
                                    .maybe_contra_candidates_mut()
                                    .as_mut()
                                    .unwrap()
                                    .push(candidate);
                                self.type_checker.clear_cached_inferences(&self.inferences);
                            }
                        } else if !contains_gc(inference.maybe_candidates().as_deref(), &candidate)
                        {
                            if inference.maybe_candidates().is_none() {
                                *inference.maybe_candidates_mut() = Some(vec![]);
                            }
                            inference
                                .maybe_candidates_mut()
                                .as_mut()
                                .unwrap()
                                .push(candidate);
                            self.type_checker.clear_cached_inferences(&self.inferences);
                        }
                    }
                    if !self.priority().intersects(InferencePriority::ReturnType)
                        && target.flags().intersects(TypeFlags::TypeParameter)
                        && inference.top_level()
                        && !self
                            .type_checker
                            .is_type_parameter_at_top_level(&self.original_target, &target)
                    {
                        inference.set_top_level(false);
                        self.type_checker.clear_cached_inferences(&self.inferences);
                    }
                }
                self.set_inference_priority(cmp::min(self.inference_priority(), self.priority()));
                return;
            } else {
                let simplified = self.type_checker.get_simplified_type(&target, false);
                if !Gc::ptr_eq(&simplified, &target) {
                    self.invoke_once(&source, &simplified, |source: &Type, target: &Type| {
                        self.infer_from_types(source, target)
                    });
                } else if target.flags().intersects(TypeFlags::IndexedAccess) {
                    let target_as_indexed_access_type = target.as_indexed_access_type();
                    let index_type = self
                        .type_checker
                        .get_simplified_type(&target_as_indexed_access_type.index_type, false);
                    if index_type.flags().intersects(TypeFlags::Instantiable) {
                        let simplified = self.type_checker.distribute_index_over_object_type(
                            &self.type_checker.get_simplified_type(
                                &target_as_indexed_access_type.object_type,
                                false,
                            ),
                            &index_type,
                            false,
                        );
                        if let Some(simplified) = simplified
                            .as_ref()
                            .filter(|simplified| !Gc::ptr_eq(simplified, &target))
                        {
                            self.invoke_once(
                                &source,
                                simplified,
                                |source: &Type, target: &Type| {
                                    self.infer_from_types(source, target)
                                },
                            );
                        }
                    }
                }
            }
        }
        if get_object_flags(&source).intersects(ObjectFlags::Reference)
            && get_object_flags(&target).intersects(ObjectFlags::Reference)
            && {
                let source_as_type_reference = source.as_type_reference_interface();
                let target_as_type_reference = target.as_type_reference_interface();
                (Gc::ptr_eq(
                    &source_as_type_reference.target(),
                    &target_as_type_reference.target(),
                ) || self.type_checker.is_array_type(&source)
                    && self.type_checker.is_array_type(&target))
                    && !(source_as_type_reference.maybe_node().is_some()
                        && target_as_type_reference.maybe_node().is_some())
            }
        {
            self.infer_from_type_arguments(
                &self.type_checker.get_type_arguments(&source),
                &self.type_checker.get_type_arguments(&target),
                &self
                    .type_checker
                    .get_variances(&source.as_type_reference_interface().target()),
            );
        } else if source.flags().intersects(TypeFlags::Index)
            && target.flags().intersects(TypeFlags::Index)
        {
            self.set_contravariant(!self.contravariant());
            self.infer_from_types(&source.as_index_type().type_, &target.as_index_type().type_);
            self.set_contravariant(!self.contravariant());
        } else if (self.type_checker.is_literal_type(&source)
            || source.flags().intersects(TypeFlags::String))
            && target.flags().intersects(TypeFlags::Index)
        {
            let empty = self
                .type_checker
                .create_empty_object_type_from_string_literal(&source);
            self.set_contravariant(!self.contravariant());
            self.infer_with_priority(
                &empty,
                &target.as_index_type().type_,
                InferencePriority::LiteralKeyof,
            );
            self.set_contravariant(!self.contravariant());
        } else if source.flags().intersects(TypeFlags::IndexedAccess)
            && target.flags().intersects(TypeFlags::IndexedAccess)
        {
            let source_as_indexed_access_type = source.as_indexed_access_type();
            let target_as_indexed_access_type = target.as_indexed_access_type();
            self.infer_from_types(
                &source_as_indexed_access_type.object_type,
                &target_as_indexed_access_type.object_type,
            );
            self.infer_from_types(
                &source_as_indexed_access_type.index_type,
                &target_as_indexed_access_type.index_type,
            );
        } else if source.flags().intersects(TypeFlags::StringMapping)
            && target.flags().intersects(TypeFlags::StringMapping)
        {
            let source_as_string_mapping_type = source.as_string_mapping_type();
            let target_as_string_mapping_type = target.as_string_mapping_type();
            if Gc::ptr_eq(&source.symbol(), &target.symbol()) {
                self.infer_from_types(
                    &source_as_string_mapping_type.type_,
                    &target_as_string_mapping_type.type_,
                );
            }
        } else if source.flags().intersects(TypeFlags::Substitution) {
            let source_as_substitution_type = source.as_substitution_type();
            self.infer_from_types(&source_as_substitution_type.base_type, &target);
            let old_priority = self.priority();
            self.set_priority(self.priority() | InferencePriority::SubstituteSource);
            self.infer_from_types(&source_as_substitution_type.substitute, &target);
            self.set_priority(old_priority);
        } else if target.flags().intersects(TypeFlags::Conditional) {
            self.invoke_once(&source, &target, |source: &Type, target: &Type| {
                self.infer_to_conditional_type(source, target)
            });
        } else if target.flags().intersects(TypeFlags::UnionOrIntersection) {
            self.infer_to_multiple_types(
                &source,
                target.as_union_or_intersection_type_interface().types(),
                target.flags(),
            );
        } else if source.flags().intersects(TypeFlags::Union) {
            let source_types = source.as_union_or_intersection_type_interface().types();
            for source_type in source_types {
                self.infer_from_types(source_type, &target);
            }
        } else if target.flags().intersects(TypeFlags::TemplateLiteral) {
            self.infer_to_template_literal_type(&source, &target);
        } else {
            source = self.type_checker.get_reduced_type(&source);
            if !(self.priority().intersects(InferencePriority::NoConstraints)
                && source
                    .flags()
                    .intersects(TypeFlags::Intersection | TypeFlags::Instantiable))
            {
                let apparent_source = self.type_checker.get_apparent_type(&source);
                if !Gc::ptr_eq(&apparent_source, &source)
                    && self.allow_complex_constraint_inference()
                    && !apparent_source
                        .flags()
                        .intersects(TypeFlags::Object | TypeFlags::Intersection)
                {
                    self.set_allow_complex_constraint_inference(false);
                    return self.infer_from_types(&apparent_source, &target);
                }
                source = apparent_source;
            }
            if source
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
            {
                self.invoke_once(&source, &target, |source: &Type, target: &Type| {
                    self.infer_from_object_types(source, target)
                });
            }
        }
    }

    pub(super) fn infer_with_priority(
        &self,
        source: &Type,
        target: &Type,
        new_priority: InferencePriority,
    ) {
        let save_priority = self.priority();
        self.set_priority(self.priority() | new_priority);
        self.infer_from_types(source, target);
        self.set_priority(save_priority);
    }

    pub(super) fn invoke_once<TAction: FnMut(&Type, &Type)>(
        &self,
        source: &Type,
        target: &Type,
        mut action: TAction,
    ) {
        let key = format!("{},{}", source.id(), target.id());
        let status = self
            .maybe_visited()
            .as_ref()
            .and_then(|visited| visited.get(&key).copied());
        if let Some(status) = status {
            self.set_inference_priority(cmp::min(self.inference_priority(), status));
            return;
        }
        if self.maybe_visited().is_none() {
            *self.maybe_visited() = Some(HashMap::new());
        }
        self.maybe_visited()
            .as_mut()
            .unwrap()
            .insert(key.clone(), InferencePriority::Circularity);
        let save_inference_priority = self.inference_priority();
        self.set_inference_priority(InferencePriority::MaxValue);
        let save_expanding_flags = self.expanding_flags();
        let source_identity = self.type_checker.get_recursion_identity(source);
        let target_identity = self.type_checker.get_recursion_identity(target);
        if contains(self.maybe_source_stack().as_deref(), &source_identity) {
            self.set_expanding_flags(self.expanding_flags() | ExpandingFlags::Source);
        }
        if contains(self.maybe_target_stack().as_deref(), &target_identity) {
            self.set_expanding_flags(self.expanding_flags() | ExpandingFlags::Target);
        }
        if self.expanding_flags() != ExpandingFlags::Both {
            if self.maybe_source_stack().is_none() {
                *self.maybe_source_stack() = Some(vec![]);
            }
            self.maybe_source_stack()
                .as_mut()
                .unwrap()
                .push(source_identity);
            if self.maybe_target_stack().is_none() {
                *self.maybe_target_stack() = Some(vec![]);
            }
            self.maybe_target_stack()
                .as_mut()
                .unwrap()
                .push(target_identity);
            action(source, target);
            self.maybe_target_stack().as_mut().unwrap().pop();
            self.maybe_source_stack().as_mut().unwrap().pop();
        } else {
            self.set_inference_priority(InferencePriority::Circularity);
        }
        self.set_expanding_flags(save_expanding_flags);
        self.maybe_visited()
            .as_mut()
            .unwrap()
            .insert(key, self.inference_priority());
        self.set_inference_priority(cmp::min(self.inference_priority(), save_inference_priority));
    }

    pub(super) fn infer_from_matching_types(
        &self,
        sources: &[Gc<Type>],
        targets: &[Gc<Type>],
        mut matches: impl FnMut(&Type, &Type) -> bool,
    ) -> (Vec<Gc<Type>>, Vec<Gc<Type>>) {
        let mut matched_sources: Option<Vec<Gc<Type>>> = None;
        let mut matched_targets: Option<Vec<Gc<Type>>> = None;
        for t in targets {
            for s in sources {
                if matches(s, t) {
                    self.infer_from_types(s, t);
                    if matched_sources.is_none() {
                        matched_sources = Some(vec![]);
                    }
                    append_if_unique_gc(matched_sources.as_mut().unwrap(), s);
                    if matched_targets.is_none() {
                        matched_targets = Some(vec![]);
                    }
                    append_if_unique_gc(matched_targets.as_mut().unwrap(), t);
                }
            }
        }
        (
            if let Some(matched_sources) = matched_sources.as_ref() {
                filter(sources, |t: &Gc<Type>| {
                    !contains_gc(Some(matched_sources), t)
                })
            } else {
                sources.to_owned()
            },
            if let Some(matched_targets) = matched_targets.as_ref() {
                filter(targets, |t: &Gc<Type>| {
                    !contains_gc(Some(matched_targets), t)
                })
            } else {
                targets.to_owned()
            },
        )
    }

    pub(super) fn infer_from_type_arguments(
        &self,
        source_types: &[Gc<Type>],
        target_types: &[Gc<Type>],
        variances: &[VarianceFlags],
    ) {
        let count = if source_types.len() < target_types.len() {
            source_types.len()
        } else {
            target_types.len()
        };
        for i in 0..count {
            if i < variances.len()
                && variances[i] & VarianceFlags::VarianceMask == VarianceFlags::Contravariant
            {
                self.infer_from_contravariant_types(&source_types[i], &target_types[i]);
            } else {
                self.infer_from_types(&source_types[i], &target_types[i]);
            }
        }
    }

    pub(super) fn infer_from_contravariant_types(&self, source: &Type, target: &Type) {
        if self.type_checker.strict_function_types
            || self.priority().intersects(InferencePriority::AlwaysStrict)
        {
            self.set_contravariant(!self.contravariant());
            self.infer_from_types(source, target);
            self.set_contravariant(!self.contravariant());
        } else {
            self.infer_from_types(source, target);
        }
    }
}
