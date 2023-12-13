use std::{
    cell::{Cell, RefCell, RefMut},
    cmp,
    collections::HashMap,
    io, ptr,
};

use gc::{Finalize, Gc, GcCell, GcCellRefMut, Trace};
use id_arena::Id;

use super::{ExpandingFlags, RecursionIdentity};
use crate::{
    append_if_unique_eq, append_if_unique_gc, arrays_equal, contains, contains_gc, create_scanner,
    filter, get_check_flags, get_object_flags, some, try_every, try_map, try_some, CheckFlags,
    ElementFlags, InferenceInfo, InferencePriority, Node, ObjectFlags, ScriptTarget, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, TokenFlags, Type, TypeChecker, TypeFlags,
    TypeInterface, UnionReduction, VarianceFlags,
};

impl TypeChecker {
    pub(super) fn infer_reverse_mapped_type(
        &self,
        source_type: Id<Type>,
        target: Id<Type>,     /*MappedType*/
        constraint: Id<Type>, /*IndexType*/
    ) -> io::Result<Id<Type>> {
        let type_parameter = self.get_indexed_access_type(
            self.type_(constraint).as_index_type().type_,
            self.get_type_parameter_from_mapped_type(target)?,
            None,
            Option::<&Node>::None,
            Option::<&Symbol>::None,
            None,
        )?;
        let template_type = self.get_template_type_from_mapped_type(target)?;
        let inference = Gc::new(self.create_inference_info(type_parameter));
        self.infer_types(
            &vec![inference.clone()],
            source_type,
            template_type,
            None,
            None,
        )?;
        Ok(self
            .get_type_from_inference(&inference)?
            .unwrap_or_else(|| self.unknown_type()))
    }

    pub(super) fn get_unmatched_properties(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        require_optional_properties: bool,
        match_discriminant_properties: bool,
    ) -> io::Result<Vec<Id<Symbol>>> {
        let properties = self.get_properties_of_type(target)?;
        let mut ret: Vec<Id<Symbol>> = vec![];
        for ref target_prop in properties {
            if self.is_static_private_identifier_property(target_prop) {
                continue;
            }
            if require_optional_properties
                || !(target_prop.flags().intersects(SymbolFlags::Optional)
                    || get_check_flags(target_prop).intersects(CheckFlags::Partial))
            {
                let source_prop =
                    self.get_property_of_type_(source, target_prop.escaped_name(), None)?;
                match source_prop {
                    None => {
                        ret.push(target_prop.clone());
                    }
                    Some(ref source_prop) => {
                        if match_discriminant_properties {
                            let target_type = self.get_type_of_symbol(target_prop)?;
                            if self.type_(target_type).flags().intersects(TypeFlags::Unit) {
                                let source_type = self.get_type_of_symbol(source_prop)?;
                                if !(self.type_(source_type).flags().intersects(TypeFlags::Any)
                                    || self.get_regular_type_of_literal_type(source_type)
                                        == self.get_regular_type_of_literal_type(target_type))
                                {
                                    ret.push(target_prop.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(ret)
    }

    pub(super) fn get_unmatched_property(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        require_optional_properties: bool,
        match_discriminant_properties: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let result = self.get_unmatched_properties(
            source,
            target,
            require_optional_properties,
            match_discriminant_properties,
        )?;
        Ok(result.get(0).cloned())
    }

    pub(super) fn tuple_types_definitely_unrelated(
        &self,
        source: Id<Type>, /*TupleTypeReference*/
        target: Id<Type>, /*TupleTypeReference*/
    ) -> bool {
        let source_target = self.type_(source).as_type_reference_interface().target();
        let target_target = self.type_(target).as_type_reference_interface().target();
        !self
            .type_(target_target)
            .as_tuple_type()
            .combined_flags
            .intersects(ElementFlags::Variadic)
            && self.type_(target_target).as_tuple_type().min_length
                > self.type_(source_target).as_tuple_type().min_length
            || !self.type_(target_target).as_tuple_type().has_rest_element
                && (self.type_(source_target).as_tuple_type().has_rest_element
                    || self.type_(target_target).as_tuple_type().fixed_length
                        < self.type_(source_target).as_tuple_type().fixed_length)
    }

    pub(super) fn types_definitely_unrelated(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<bool> {
        Ok(
            if self.is_tuple_type(source) && self.is_tuple_type(target) {
                self.tuple_types_definitely_unrelated(source, target)
            } else {
                self.get_unmatched_property(source, target, false, true)?
                    .is_some()
                    && self
                        .get_unmatched_property(target, source, false, false)?
                        .is_some()
            },
        )
    }

    pub(super) fn get_type_from_inference(
        &self,
        inference: &InferenceInfo,
    ) -> io::Result<Option<Id<Type>>> {
        Ok(
            if let Some(inference_candidates) = inference.maybe_candidates().as_ref() {
                Some(self.get_union_type(
                    inference_candidates,
                    Some(UnionReduction::Subtype),
                    Option::<&Symbol>::None,
                    None,
                    None,
                )?)
            } else if let Some(inference_contra_candidates) =
                inference.maybe_contra_candidates().as_ref()
            {
                Some(self.get_intersection_type(
                    inference_contra_candidates,
                    Option::<&Symbol>::None,
                    None,
                )?)
            } else {
                None
            },
        )
    }

    pub(super) fn has_skip_direct_inference_flag(&self, node: &Node) -> bool {
        (*self.get_node_links(node)).borrow().skip_direct_inference == Some(true)
    }

    pub(super) fn is_from_inference_blocked_source(&self, type_: Id<Type>) -> bool {
        matches!(
            self.type_(type_).maybe_symbol().as_ref(),
            Some(type_symbol) if some(
                type_symbol.maybe_declarations().as_deref(),
                Some(|declaration: &Gc<Node>| self.has_skip_direct_inference_flag(declaration))
            )
        )
    }

    pub(super) fn template_literal_types_definitely_unrelated(
        &self,
        source: Id<Type>, /*TemplateLiteralType*/
        target: Id<Type>, /*TemplateLiteralType*/
    ) -> bool {
        let source_ref = self.type_(source);
        let source_start = &source_ref.as_template_literal_type().texts[0];
        let target_ref = self.type_(target);
        let target_start = &target_ref.as_template_literal_type().texts[0];
        let source_end = &source_ref.as_template_literal_type().texts
            [self.type_(source).as_template_literal_type().texts.len() - 1];
        let target_end = &target_ref.as_template_literal_type().texts
            [self.type_(target).as_template_literal_type().texts.len() - 1];
        let start_len = cmp::min(source_start.len(), target_start.len());
        let end_len = cmp::min(source_end.len(), target_end.len());
        &source_start[0..start_len] != &target_start[0..start_len]
            || &source_end[source_end.len() - end_len..]
                != &target_end[target_end.len() - end_len..]
    }

    pub(super) fn is_valid_big_int_string(&self, s: &str) -> bool {
        let scanner = create_scanner(ScriptTarget::ESNext, false, None, None, None, None, None);
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
        source: Id<Type>,
        target: Id<Type>, /*TemplateLiteralType*/
    ) -> io::Result<bool> {
        if source == target
            || self
                .type_(target)
                .flags()
                .intersects(TypeFlags::Any | TypeFlags::String)
        {
            return Ok(true);
        }
        if self
            .type_(source)
            .flags()
            .intersects(TypeFlags::StringLiteral)
        {
            let source_ref = self.type_(source);
            let value = &source_ref.as_string_literal_type().value;
            return Ok(self.type_(target).flags().intersects(TypeFlags::Number)
                && value != ""
                && value.parse::<f64>().is_ok()
                || self.type_(target).flags().intersects(TypeFlags::BigInt)
                    && value != ""
                    && self.is_valid_big_int_string(value)
                || self
                    .type_(target)
                    .flags()
                    .intersects(TypeFlags::BooleanLiteral | TypeFlags::Nullable)
                    && value == self.type_(target).as_intrinsic_type().intrinsic_name());
        }
        if self
            .type_(source)
            .flags()
            .intersects(TypeFlags::TemplateLiteral)
        {
            let source_ref = self.type_(source);
            let texts = &source_ref.as_template_literal_type().texts;
            return Ok(texts.len() == 2
                && texts[0] == ""
                && texts[1] == ""
                && self.is_type_assignable_to(
                    self.type_(source).as_template_literal_type().types[0],
                    target,
                )?);
        }
        self.is_type_assignable_to(source, target)
    }

    pub(super) fn infer_types_from_template_literal_type(
        &self,
        source: Id<Type>,
        target: Id<Type>, /*TemplateLiteralType*/
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        Ok(
            if self
                .type_(source)
                .flags()
                .intersects(TypeFlags::StringLiteral)
            {
                self.infer_from_literal_parts_to_template_literal(
                    &vec![{
                        let value = self.type_(source).as_string_literal_type().value.clone();
                        value
                    }],
                    &vec![],
                    target,
                )?
            } else if self
                .type_(source)
                .flags()
                .intersects(TypeFlags::TemplateLiteral)
            {
                if arrays_equal(
                    &self.type_(source).as_template_literal_type().texts,
                    &self.type_(target).as_template_literal_type().texts,
                ) {
                    Some(try_map(
                        &{
                            let types = self.type_(source).as_template_literal_type().types.clone();
                            types
                        },
                        |&type_: &Id<Type>, _| self.get_string_like_type_for_type(type_),
                    )?)
                } else {
                    self.infer_from_literal_parts_to_template_literal(
                        &{
                            let texts = self.type_(source).as_template_literal_type().texts.clone();
                            texts
                        },
                        &{
                            let types = self.type_(source).as_template_literal_type().types.clone();
                            types
                        },
                        target,
                    )?
                }
            } else {
                None
            },
        )
    }

    pub(super) fn is_type_matched_by_template_literal_type(
        &self,
        source: Id<Type>,
        target: Id<Type>, /*TemplateLiteralType*/
    ) -> io::Result<bool> {
        let inferences = self.infer_types_from_template_literal_type(source, target)?;
        Ok(matches!(
            inferences.as_ref(),
            Some(inferences) if try_every(
                inferences,
                |&r: &Id<Type>, i| {
                    self.is_valid_type_for_template_literal_placeholder(
                        r,
                        self.type_(target).as_template_literal_type().types[i],
                    )
                }
            )?
        ))
    }

    pub(super) fn get_string_like_type_for_type(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        Ok(
            if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Any | TypeFlags::StringLike)
            {
                type_
            } else {
                self.get_template_literal_type(&vec!["".to_owned(), "".to_owned()], &vec![type_])?
            },
        )
    }

    pub(super) fn infer_from_literal_parts_to_template_literal(
        &self,
        source_texts: &[String],
        source_types: &[Id<Type>],
        target: Id<Type>, /*TemplateLiteralType*/
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        let last_source_index = source_texts.len() - 1;
        let source_start_text = &source_texts[0];
        // TODO: should be using chars for any of this instead?
        let source_end_text = &source_texts[last_source_index];
        let target_texts = &self.type_(target).as_template_literal_type().texts.clone();
        let last_target_index = target_texts.len() - 1;
        let target_start_text = &target_texts[0];
        let target_end_text = &target_texts[last_target_index];
        if last_source_index == 0
            && source_start_text.len() < target_start_text.len() + target_end_text.len()
            || !source_start_text.starts_with(target_start_text)
            || !source_end_text.ends_with(target_end_text)
        {
            return Ok(None);
        }
        let remaining_end_text = &source_end_text[0..source_end_text.len() - target_end_text.len()];
        let mut matches: Vec<Id<Type>> = vec![];
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
                        return Ok(None);
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
                )?;
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
                )?;
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
                )?;
            } else {
                return Ok(None);
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
        )?;
        Ok(Some(matches))
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
        matches: &mut Vec<Id<Type>>,
        source_types: &[Id<Type>],
        s: usize,
        p: usize,
    ) -> io::Result<()> {
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
            )?
        };
        matches.push(match_type);
        *seg = s;
        *pos = p;

        Ok(())
    }

    pub(super) fn infer_types(
        &self,
        inferences: &[Gc<InferenceInfo>],
        original_source: Id<Type>,
        original_target: Id<Type>,
        priority: Option<InferencePriority>,
        contravariant: Option<bool>,
    ) -> io::Result<()> {
        let priority = priority.unwrap_or(InferencePriority::None);
        let contravariant = contravariant.unwrap_or(false);
        InferTypes::new(
            self.rc_wrapper(),
            inferences.to_owned(),
            original_target,
            priority,
            contravariant,
        )
        .infer_from_types(original_source, original_target)?;

        Ok(())
    }
}

#[derive(Trace, Finalize)]
pub(super) struct InferTypes {
    pub type_checker: Gc<TypeChecker>,
    pub inferences: Vec<Gc<InferenceInfo>>,
    pub original_target: Id<Type>,
    #[unsafe_ignore_trace]
    priority: Cell<InferencePriority>,
    #[unsafe_ignore_trace]
    contravariant: Cell<bool>,
    #[unsafe_ignore_trace]
    bivariant: Cell<bool>,
    propagation_type: GcCell<Option<Id<Type>>>,
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
        original_target: Id<Type>,
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

    pub(super) fn maybe_propagation_type(&self) -> GcCellRefMut<Option<Id<Type>>> {
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

    pub(super) fn infer_from_types(
        &self,
        mut source: Id<Type>,
        mut target: Id<Type>,
    ) -> io::Result<()> {
        if !self.type_checker.could_contain_type_variables(target)? {
            return Ok(());
        }
        if source == self.type_checker.wildcard_type() {
            let save_propagation_type = self.maybe_propagation_type().clone();
            *self.maybe_propagation_type() = Some(source);
            self.infer_from_types(target, target)?;
            *self.maybe_propagation_type() = save_propagation_type;
            return Ok(());
        }
        if let Some(ref source_alias_symbol) = {
            let alias_symbol = self.type_checker.type_(source).maybe_alias_symbol();
            alias_symbol
        }
        .filter(|source_alias_symbol| {
            matches!(
                self.type_checker.type_(target).maybe_alias_symbol().as_ref(),
                Some(target_alias_symbol) if Gc::ptr_eq(source_alias_symbol, target_alias_symbol)
            )
        }) {
            if let Some(source_alias_type_arguments) = {
                let alias_type_arguments =
                    self.type_checker.type_(source).maybe_alias_type_arguments();
                alias_type_arguments
            }
            .as_ref()
            {
                self.infer_from_type_arguments(
                    source_alias_type_arguments,
                    {
                        let alias_type_arguments =
                            self.type_checker.type_(target).maybe_alias_type_arguments();
                        alias_type_arguments
                    }
                    .as_ref()
                    .unwrap(),
                    &*self.type_checker.get_alias_variances(source_alias_symbol)?,
                )?;
                return Ok(());
            }
        }
        if source == target
            && self
                .type_checker
                .type_(source)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
        {
            for &t in self
                .type_checker
                .type_(source)
                .as_union_or_intersection_type_interface()
                .types()
            {
                self.infer_from_types(t, t)?;
            }
            return Ok(());
        }
        if self
            .type_checker
            .type_(target)
            .flags()
            .intersects(TypeFlags::Union)
        {
            let (temp_sources, temp_targets) = self.infer_from_matching_types(
                &if self
                    .type_checker
                    .type_(source)
                    .flags()
                    .intersects(TypeFlags::Union)
                {
                    self.type_checker
                        .type_(source)
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned()
                } else {
                    vec![source.clone()]
                },
                self.type_checker
                    .type_(target)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |s: Id<Type>, t: Id<Type>| self.type_checker.is_type_or_base_identical_to(s, t),
            )?;
            let (sources, targets) = self.infer_from_matching_types(
                &temp_sources,
                &temp_targets,
                |s: Id<Type>, t: Id<Type>| Ok(self.type_checker.is_type_closely_matched_by(s, t)),
            )?;
            if targets.is_empty() {
                return Ok(());
            }
            target = self.type_checker.get_union_type(
                &targets,
                None,
                Option::<&Symbol>::None,
                None,
                None,
            )?;
            if sources.is_empty() {
                self.infer_with_priority(source, target, InferencePriority::NakedTypeVariable)?;
                return Ok(());
            }
            source = self.type_checker.get_union_type(
                &sources,
                None,
                Option::<&Symbol>::None,
                None,
                None,
            )?;
        } else if self
            .type_checker
            .type_(target)
            .flags()
            .intersects(TypeFlags::Intersection)
            && try_some(
                Some(
                    self.type_checker
                        .type_(target)
                        .as_union_or_intersection_type_interface()
                        .types(),
                ),
                Some(|&t: &Id<Type>| -> io::Result<_> {
                    Ok(self.get_inference_info_for_type(t).is_some()
                        || (self.type_checker.is_generic_mapped_type(t)?
                            && self
                                .get_inference_info_for_type(
                                    self.type_checker
                                        .get_homomorphic_type_variable(t)?
                                        .unwrap_or_else(|| self.type_checker.never_type()),
                                )
                                .is_some()))
                }),
            )?
        {
            if !self
                .type_checker
                .type_(source)
                .flags()
                .intersects(TypeFlags::Union)
            {
                let (sources, targets) = self.infer_from_matching_types(
                    &if self
                        .type_checker
                        .type_(source)
                        .flags()
                        .intersects(TypeFlags::Intersection)
                    {
                        self.type_checker
                            .type_(source)
                            .as_union_or_intersection_type_interface()
                            .types()
                            .to_owned()
                    } else {
                        vec![source.clone()]
                    },
                    self.type_checker
                        .type_(target)
                        .as_union_or_intersection_type_interface()
                        .types(),
                    |s: Id<Type>, t: Id<Type>| self.type_checker.is_type_identical_to(s, t),
                )?;
                if sources.is_empty() || targets.is_empty() {
                    return Ok(());
                }
                source = self.type_checker.get_intersection_type(
                    &sources,
                    Option::<&Symbol>::None,
                    None,
                )?;
                target = self.type_checker.get_intersection_type(
                    &targets,
                    Option::<&Symbol>::None,
                    None,
                )?;
            }
        } else if self
            .type_checker
            .type_(target)
            .flags()
            .intersects(TypeFlags::IndexedAccess | TypeFlags::Substitution)
        {
            target = self.type_checker.get_actual_type_variable(target)?;
        }
        if self
            .type_checker
            .type_(target)
            .flags()
            .intersects(TypeFlags::TypeVariable)
        {
            if get_object_flags(&self.type_checker.type_(source))
                .intersects(ObjectFlags::NonInferrableType)
                || source == self.type_checker.non_inferrable_any_type()
                || source == self.type_checker.silent_never_type()
                || self.priority().intersects(InferencePriority::ReturnType)
                    && (source == self.type_checker.auto_type()
                        || source == self.type_checker.auto_array_type())
                || self.type_checker.is_from_inference_blocked_source(source)
            {
                return Ok(());
            }
            let inference = self.get_inference_info_for_type(target);
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
                            if !contains(inference.maybe_contra_candidates().as_deref(), &candidate)
                            {
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
                        } else if !contains(inference.maybe_candidates().as_deref(), &candidate) {
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
                        && self
                            .type_checker
                            .type_(target)
                            .flags()
                            .intersects(TypeFlags::TypeParameter)
                        && inference.top_level()
                        && !self
                            .type_checker
                            .is_type_parameter_at_top_level(self.original_target, target)?
                    {
                        inference.set_top_level(false);
                        self.type_checker.clear_cached_inferences(&self.inferences);
                    }
                }
                self.set_inference_priority(cmp::min(self.inference_priority(), self.priority()));
                return Ok(());
            } else {
                let simplified = self.type_checker.get_simplified_type(target, false)?;
                if simplified != target {
                    self.try_invoke_once(
                        source,
                        simplified,
                        |source: Id<Type>, target: Id<Type>| self.infer_from_types(source, target),
                    )?;
                } else if self
                    .type_checker
                    .type_(target)
                    .flags()
                    .intersects(TypeFlags::IndexedAccess)
                {
                    let index_type = self.type_checker.get_simplified_type(
                        self.type_checker
                            .type_(target)
                            .as_indexed_access_type()
                            .index_type,
                        false,
                    )?;
                    if self
                        .type_checker
                        .type_(index_type)
                        .flags()
                        .intersects(TypeFlags::Instantiable)
                    {
                        let simplified = self.type_checker.distribute_index_over_object_type(
                            self.type_checker.get_simplified_type(
                                self.type_checker
                                    .type_(target)
                                    .as_indexed_access_type()
                                    .object_type,
                                false,
                            )?,
                            index_type,
                            false,
                        )?;
                        if let Some(simplified) =
                            simplified.filter(|&simplified| simplified != target)
                        {
                            self.try_invoke_once(
                                source,
                                simplified,
                                |source: Id<Type>, target: Id<Type>| {
                                    self.infer_from_types(source, target)
                                },
                            )?;
                        }
                    }
                }
            }
        }
        if get_object_flags(&self.type_checker.type_(source)).intersects(ObjectFlags::Reference)
            && get_object_flags(&self.type_checker.type_(target)).intersects(ObjectFlags::Reference)
            && {
                (self
                    .type_checker
                    .type_(source)
                    .as_type_reference_interface()
                    .target()
                    == self
                        .type_checker
                        .type_(target)
                        .as_type_reference_interface()
                        .target()
                    || self.type_checker.is_array_type(source)
                        && self.type_checker.is_array_type(target))
                    && !(self
                        .type_checker
                        .type_(source)
                        .as_type_reference_interface()
                        .maybe_node()
                        .is_some()
                        && self
                            .type_checker
                            .type_(target)
                            .as_type_reference_interface()
                            .maybe_node()
                            .is_some())
            }
        {
            self.infer_from_type_arguments(
                &*self.type_checker.get_type_arguments(source)?,
                &*self.type_checker.get_type_arguments(target)?,
                &self.type_checker.get_variances(
                    self.type_checker
                        .type_(source)
                        .as_type_reference_interface()
                        .target(),
                ),
            )?;
        } else if self
            .type_checker
            .type_(source)
            .flags()
            .intersects(TypeFlags::Index)
            && self
                .type_checker
                .type_(target)
                .flags()
                .intersects(TypeFlags::Index)
        {
            self.set_contravariant(!self.contravariant());
            self.infer_from_types(
                self.type_checker.type_(source).as_index_type().type_,
                self.type_checker.type_(target).as_index_type().type_,
            )?;
            self.set_contravariant(!self.contravariant());
        } else if (self.type_checker.is_literal_type(source)
            || self
                .type_checker
                .type_(source)
                .flags()
                .intersects(TypeFlags::String))
            && self
                .type_checker
                .type_(target)
                .flags()
                .intersects(TypeFlags::Index)
        {
            let empty = self
                .type_checker
                .create_empty_object_type_from_string_literal(source)?;
            self.set_contravariant(!self.contravariant());
            self.infer_with_priority(
                empty,
                self.type_checker.type_(target).as_index_type().type_,
                InferencePriority::LiteralKeyof,
            )?;
            self.set_contravariant(!self.contravariant());
        } else if self
            .type_checker
            .type_(source)
            .flags()
            .intersects(TypeFlags::IndexedAccess)
            && self
                .type_checker
                .type_(target)
                .flags()
                .intersects(TypeFlags::IndexedAccess)
        {
            self.infer_from_types(
                self.type_checker
                    .type_(source)
                    .as_indexed_access_type()
                    .object_type,
                self.type_checker
                    .type_(target)
                    .as_indexed_access_type()
                    .object_type,
            )?;
            self.infer_from_types(
                self.type_checker
                    .type_(source)
                    .as_indexed_access_type()
                    .index_type,
                self.type_checker
                    .type_(target)
                    .as_indexed_access_type()
                    .index_type,
            )?;
        } else if self
            .type_checker
            .type_(source)
            .flags()
            .intersects(TypeFlags::StringMapping)
            && self
                .type_checker
                .type_(target)
                .flags()
                .intersects(TypeFlags::StringMapping)
        {
            if Gc::ptr_eq(
                &self.type_checker.type_(source).symbol(),
                &self.type_checker.type_(target).symbol(),
            ) {
                self.infer_from_types(
                    self.type_checker
                        .type_(source)
                        .as_string_mapping_type()
                        .type_,
                    self.type_checker
                        .type_(target)
                        .as_string_mapping_type()
                        .type_,
                )?;
            }
        } else if self
            .type_checker
            .type_(source)
            .flags()
            .intersects(TypeFlags::Substitution)
        {
            self.infer_from_types(
                self.type_checker
                    .type_(source)
                    .as_substitution_type()
                    .base_type,
                target,
            )?;
            let old_priority = self.priority();
            self.set_priority(self.priority() | InferencePriority::SubstituteSource);
            self.infer_from_types(
                self.type_checker
                    .type_(source)
                    .as_substitution_type()
                    .substitute,
                target,
            )?;
            self.set_priority(old_priority);
        } else if self
            .type_checker
            .type_(target)
            .flags()
            .intersects(TypeFlags::Conditional)
        {
            self.try_invoke_once(source, target, |source: Id<Type>, target: Id<Type>| {
                self.infer_to_conditional_type(source, target)
            })?;
        } else if self
            .type_checker
            .type_(target)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
        {
            self.infer_to_multiple_types(
                source,
                self.type_checker
                    .type_(target)
                    .as_union_or_intersection_type_interface()
                    .types(),
                self.type_checker.type_(target).flags(),
            )?;
        } else if self
            .type_checker
            .type_(source)
            .flags()
            .intersects(TypeFlags::Union)
        {
            let source_types = &self.type_checker.type_(source).as_union_or_intersection_type_interface().types().to_owned();
            for &source_type in source_types {
                self.infer_from_types(source_type, target)?;
            }
        } else if self
            .type_checker
            .type_(target)
            .flags()
            .intersects(TypeFlags::TemplateLiteral)
        {
            self.infer_to_template_literal_type(source, target)?;
        } else {
            source = self.type_checker.get_reduced_type(source)?;
            if !(self.priority().intersects(InferencePriority::NoConstraints)
                && self
                    .type_checker
                    .type_(source)
                    .flags()
                    .intersects(TypeFlags::Intersection | TypeFlags::Instantiable))
            {
                let apparent_source = self.type_checker.get_apparent_type(source)?;
                if apparent_source != source
                    && self.allow_complex_constraint_inference()
                    && !self
                        .type_checker
                        .type_(apparent_source)
                        .flags()
                        .intersects(TypeFlags::Object | TypeFlags::Intersection)
                {
                    self.set_allow_complex_constraint_inference(false);
                    return self.infer_from_types(apparent_source, target);
                }
                source = apparent_source;
            }
            if self
                .type_checker
                .type_(source)
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
            {
                self.try_invoke_once(source, target, |source: Id<Type>, target: Id<Type>| {
                    self.infer_from_object_types(source, target)
                })?;
            }
        }

        Ok(())
    }

    pub(super) fn infer_with_priority(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        new_priority: InferencePriority,
    ) -> io::Result<()> {
        let save_priority = self.priority();
        self.set_priority(self.priority() | new_priority);
        self.infer_from_types(source, target)?;
        self.set_priority(save_priority);

        Ok(())
    }

    #[allow(dead_code)]
    pub(super) fn invoke_once(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        mut action: impl FnMut(Id<Type>, Id<Type>),
    ) {
        self.try_invoke_once(source, target, |a: Id<Type>, b: Id<Type>| Ok(action(a, b)))
            .unwrap()
    }

    pub(super) fn try_invoke_once(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        mut action: impl FnMut(Id<Type>, Id<Type>) -> io::Result<()>,
    ) -> io::Result<()> {
        let key = format!(
            "{},{}",
            self.type_checker.type_(source).id(),
            self.type_checker.type_(target).id()
        );
        let status = self
            .maybe_visited()
            .as_ref()
            .and_then(|visited| visited.get(&key).copied());
        if let Some(status) = status {
            self.set_inference_priority(cmp::min(self.inference_priority(), status));
            return Ok(());
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
            action(source, target)?;
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

        Ok(())
    }

    pub(super) fn infer_from_matching_types(
        &self,
        sources: &[Id<Type>],
        targets: &[Id<Type>],
        mut matches: impl FnMut(Id<Type>, Id<Type>) -> io::Result<bool>,
    ) -> io::Result<(Vec<Id<Type>>, Vec<Id<Type>>)> {
        let mut matched_sources: Option<Vec<Id<Type>>> = None;
        let mut matched_targets: Option<Vec<Id<Type>>> = None;
        for &t in targets {
            for &s in sources {
                if matches(s, t)? {
                    self.infer_from_types(s, t)?;
                    if matched_sources.is_none() {
                        matched_sources = Some(vec![]);
                    }
                    append_if_unique_eq(matched_sources.as_mut().unwrap(), &s);
                    if matched_targets.is_none() {
                        matched_targets = Some(vec![]);
                    }
                    append_if_unique_eq(matched_targets.as_mut().unwrap(), &t);
                }
            }
        }
        Ok((
            if let Some(matched_sources) = matched_sources.as_ref() {
                filter(sources, |t: &Id<Type>| !contains(Some(matched_sources), t))
            } else {
                sources.to_owned()
            },
            if let Some(matched_targets) = matched_targets.as_ref() {
                filter(targets, |t: &Id<Type>| !contains(Some(matched_targets), t))
            } else {
                targets.to_owned()
            },
        ))
    }

    pub(super) fn infer_from_type_arguments(
        &self,
        source_types: &[Id<Type>],
        target_types: &[Id<Type>],
        variances: &[VarianceFlags],
    ) -> io::Result<()> {
        let count = if source_types.len() < target_types.len() {
            source_types.len()
        } else {
            target_types.len()
        };
        for i in 0..count {
            if i < variances.len()
                && variances[i] & VarianceFlags::VarianceMask == VarianceFlags::Contravariant
            {
                self.infer_from_contravariant_types(source_types[i], target_types[i])?;
            } else {
                self.infer_from_types(source_types[i], target_types[i])?;
            }
        }

        Ok(())
    }

    pub(super) fn infer_from_contravariant_types(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<()> {
        if self.type_checker.strict_function_types
            || self.priority().intersects(InferencePriority::AlwaysStrict)
        {
            self.set_contravariant(!self.contravariant());
            self.infer_from_types(source, target)?;
            self.set_contravariant(!self.contravariant());
        } else {
            self.infer_from_types(source, target)?;
        }

        Ok(())
    }
}
