use std::{
    cell::{Cell, RefCell, RefMut},
    cmp,
    collections::HashMap,
    io, ptr,
};

use id_arena::Id;

use super::{ExpandingFlags, RecursionIdentity};
use crate::{
    append_if_unique_eq, arrays_equal, contains, create_scanner,
    filter, get_check_flags, get_object_flags, some, try_every, try_map, try_some, AllArenas,
    CheckFlags, ElementFlags, HasArena, InArena, InferenceInfo, InferencePriority, Node,
    ObjectFlags, ScriptTarget, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, TokenFlags, Type,
    TypeChecker, TypeFlags, TypeInterface, UnionReduction, VarianceFlags,
};

impl TypeChecker {
    pub(super) fn infer_reverse_mapped_type(
        &self,
        source_type: Id<Type>,
        target: Id<Type>,     /*MappedType*/
        constraint: Id<Type>, /*IndexType*/
    ) -> io::Result<Id<Type>> {
        let type_parameter = self.get_indexed_access_type(
            constraint.ref_(self).as_index_type().type_,
            self.get_type_parameter_from_mapped_type(target)?,
            None,
            Option::<Id<Node>>::None,
            Option::<Id<Symbol>>::None,
            None,
        )?;
        let template_type = self.get_template_type_from_mapped_type(target)?;
        let inference = self.alloc_inference_info(self.create_inference_info(type_parameter));
        self.infer_types(
            &vec![inference.clone()],
            source_type,
            template_type,
            None,
            None,
        )?;
        Ok(self
            .get_type_from_inference(&inference.ref_(self))?
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
        for target_prop in properties {
            if self.is_static_private_identifier_property(target_prop) {
                continue;
            }
            if require_optional_properties
                || !(target_prop
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::Optional)
                    || get_check_flags(&target_prop.ref_(self)).intersects(CheckFlags::Partial))
            {
                let source_prop = self.get_property_of_type_(
                    source,
                    target_prop.ref_(self).escaped_name(),
                    None,
                )?;
                match source_prop {
                    None => {
                        ret.push(target_prop.clone());
                    }
                    Some(source_prop) => {
                        if match_discriminant_properties {
                            let target_type = self.get_type_of_symbol(target_prop)?;
                            if target_type.ref_(self).flags().intersects(TypeFlags::Unit) {
                                let source_type = self.get_type_of_symbol(source_prop)?;
                                if !(source_type.ref_(self).flags().intersects(TypeFlags::Any)
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
        let source_target = source.ref_(self).as_type_reference_interface().target();
        let target_target = target.ref_(self).as_type_reference_interface().target();
        !target_target
            .ref_(self)
            .as_tuple_type()
            .combined_flags
            .intersects(ElementFlags::Variadic)
            && target_target.ref_(self).as_tuple_type().min_length
                > source_target.ref_(self).as_tuple_type().min_length
            || !target_target.ref_(self).as_tuple_type().has_rest_element
                && (source_target.ref_(self).as_tuple_type().has_rest_element
                    || target_target.ref_(self).as_tuple_type().fixed_length
                        < source_target.ref_(self).as_tuple_type().fixed_length)
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
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?)
            } else if let Some(inference_contra_candidates) =
                inference.maybe_contra_candidates().as_ref()
            {
                Some(self.get_intersection_type(
                    inference_contra_candidates,
                    Option::<Id<Symbol>>::None,
                    None,
                )?)
            } else {
                None
            },
        )
    }

    pub(super) fn has_skip_direct_inference_flag(&self, node: Id<Node>) -> bool {
        self.get_node_links(node).ref_(self).skip_direct_inference == Some(true)
    }

    pub(super) fn is_from_inference_blocked_source(&self, type_: Id<Type>) -> bool {
        matches!(
            type_.ref_(self).maybe_symbol(),
            Some(type_symbol) if some(
                type_symbol.ref_(self).maybe_declarations().as_deref(),
                Some(|&declaration: &Id<Node>| self.has_skip_direct_inference_flag(declaration))
            )
        )
    }

    pub(super) fn template_literal_types_definitely_unrelated(
        &self,
        source: Id<Type>, /*TemplateLiteralType*/
        target: Id<Type>, /*TemplateLiteralType*/
    ) -> bool {
        let source_ref = source.ref_(self);
        let source_start = &source_ref.as_template_literal_type().texts[0];
        let target_ref = target.ref_(self);
        let target_start = &target_ref.as_template_literal_type().texts[0];
        let source_end = &source_ref.as_template_literal_type().texts
            [source.ref_(self).as_template_literal_type().texts.len() - 1];
        let target_end = &target_ref.as_template_literal_type().texts
            [target.ref_(self).as_template_literal_type().texts.len() - 1];
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
            || target
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Any | TypeFlags::String)
        {
            return Ok(true);
        }
        if source
            .ref_(self)
            .flags()
            .intersects(TypeFlags::StringLiteral)
        {
            let source_ref = source.ref_(self);
            let value = &source_ref.as_string_literal_type().value;
            return Ok(target.ref_(self).flags().intersects(TypeFlags::Number)
                && value != ""
                && value.parse::<f64>().is_ok()
                || target.ref_(self).flags().intersects(TypeFlags::BigInt)
                    && value != ""
                    && self.is_valid_big_int_string(value)
                || target
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::BooleanLiteral | TypeFlags::Nullable)
                    && value == target.ref_(self).as_intrinsic_type().intrinsic_name());
        }
        if source
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TemplateLiteral)
        {
            let source_ref = source.ref_(self);
            let texts = &source_ref.as_template_literal_type().texts;
            return Ok(texts.len() == 2
                && texts[0] == ""
                && texts[1] == ""
                && self.is_type_assignable_to(
                    source.ref_(self).as_template_literal_type().types[0],
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
            if source
                .ref_(self)
                .flags()
                .intersects(TypeFlags::StringLiteral)
            {
                self.infer_from_literal_parts_to_template_literal(
                    &vec![{
                        let value = source.ref_(self).as_string_literal_type().value.clone();
                        value
                    }],
                    &vec![],
                    target,
                )?
            } else if source
                .ref_(self)
                .flags()
                .intersects(TypeFlags::TemplateLiteral)
            {
                if arrays_equal(
                    &source.ref_(self).as_template_literal_type().texts,
                    &target.ref_(self).as_template_literal_type().texts,
                ) {
                    Some(try_map(
                        &{
                            let types = source.ref_(self).as_template_literal_type().types.clone();
                            types
                        },
                        |&type_: &Id<Type>, _| self.get_string_like_type_for_type(type_),
                    )?)
                } else {
                    self.infer_from_literal_parts_to_template_literal(
                        &{
                            let texts = source.ref_(self).as_template_literal_type().texts.clone();
                            texts
                        },
                        &{
                            let types = source.ref_(self).as_template_literal_type().types.clone();
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
                        target.ref_(self).as_template_literal_type().types[i],
                    )
                }
            )?
        ))
    }

    pub(super) fn get_string_like_type_for_type(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        Ok(
            if type_
                .ref_(self)
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
        let target_texts = &target.ref_(self).as_template_literal_type().texts.clone();
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
        inferences: &[Id<InferenceInfo>],
        original_source: Id<Type>,
        original_target: Id<Type>,
        priority: Option<InferencePriority>,
        contravariant: Option<bool>,
    ) -> io::Result<()> {
        let priority = priority.unwrap_or(InferencePriority::None);
        let contravariant = contravariant.unwrap_or(false);
        InferTypes::new(
            self.arena_id(),
            inferences.to_owned(),
            original_target,
            priority,
            contravariant,
        )
        .infer_from_types(original_source, original_target)?;

        Ok(())
    }
}

pub(super) struct InferTypes {
    pub type_checker: Id<TypeChecker>,
    pub inferences: Vec<Id<InferenceInfo>>,
    pub original_target: Id<Type>,
    priority: Cell<InferencePriority>,
    contravariant: Cell<bool>,
    bivariant: Cell<bool>,
    propagation_type: Cell<Option<Id<Type>>>,
    inference_priority: Cell<InferencePriority>,
    allow_complex_constraint_inference: Cell<bool>,
    visited: RefCell<Option<HashMap<String, InferencePriority>>>,
    source_stack: RefCell<Option<Vec<RecursionIdentity>>>,
    target_stack: RefCell<Option<Vec<RecursionIdentity>>>,
    expanding_flags: Cell<ExpandingFlags>,
}

impl HasArena for InferTypes {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

impl InferTypes {
    pub(super) fn new(
        type_checker: Id<TypeChecker>,
        inferences: Vec<Id<InferenceInfo>>,
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

    pub(super) fn maybe_propagation_type(&self) -> Option<Id<Type>> {
        self.propagation_type.get()
    }

    pub(super) fn set_propagation_type(&self, propagation_type: Option<Id<Type>>) {
        self.propagation_type.set(propagation_type);
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

    pub(super) fn maybe_source_stack(&self) -> Ref<Option<Vec<RecursionIdentity>>> {
        self.source_stack.borrow()
    }

    pub(super) fn maybe_source_stack_mut(&self) -> RefMut<Option<Vec<RecursionIdentity>>> {
        self.source_stack.borrow_mut()
    }

    pub(super) fn set_source_stack(&self, source_stack: Option<Vec<RecursionIdentity>>) {
        *self.source_stack.borrow_mut() = source_stack;
    }

    pub(super) fn maybe_target_stack(&self) -> Ref<Option<Vec<RecursionIdentity>>> {
        self.target_stack.borrow()
    }

    pub(super) fn maybe_target_stack_mut(&self) -> RefMut<Option<Vec<RecursionIdentity>>> {
        self.target_stack.borrow_mut()
    }

    pub(super) fn set_target_stack(&self, target_stack: Option<Vec<RecursionIdentity>>) {
        *self.target_stack.borrow_mut() = target_stack;
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
        if !self.type_checker.ref_(self).could_contain_type_variables(target)? {
            return Ok(());
        }
        if source == self.type_checker.ref_(self).wildcard_type() {
            let save_propagation_type = self.maybe_propagation_type();
            self.set_propagation_type(Some(source));
            self.infer_from_types(target, target)?;
            self.set_propagation_type(save_propagation_type);
            return Ok(());
        }
        if let Some(source_alias_symbol) = {
            let alias_symbol = source.ref_(self).maybe_alias_symbol();
            alias_symbol
        }
        .filter(|&source_alias_symbol| {
            matches!(
                target.ref_(self).maybe_alias_symbol(),
                Some(target_alias_symbol) if source_alias_symbol == target_alias_symbol
            )
        }) {
            if let Some(source_alias_type_arguments) = {
                let alias_type_arguments = source.ref_(self).maybe_alias_type_arguments();
                alias_type_arguments
            }
            .as_ref()
            {
                self.infer_from_type_arguments(
                    source_alias_type_arguments,
                    {
                        let alias_type_arguments = target.ref_(self).maybe_alias_type_arguments();
                        alias_type_arguments
                    }
                    .as_ref()
                    .unwrap(),
                    &*self.type_checker.ref_(self).get_alias_variances(source_alias_symbol)?,
                )?;
                return Ok(());
            }
        }
        if source == target
            && source
                .ref_(self)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
        {
            for &t in source
                .ref_(self)
                .as_union_or_intersection_type_interface()
                .types()
            {
                self.infer_from_types(t, t)?;
            }
            return Ok(());
        }
        if target.ref_(self).flags().intersects(TypeFlags::Union) {
            let (temp_sources, temp_targets) = self.infer_from_matching_types(
                &if source.ref_(self).flags().intersects(TypeFlags::Union) {
                    source
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned()
                } else {
                    vec![source.clone()]
                },
                target
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |s: Id<Type>, t: Id<Type>| self.type_checker.ref_(self).is_type_or_base_identical_to(s, t),
            )?;
            let (sources, targets) = self.infer_from_matching_types(
                &temp_sources,
                &temp_targets,
                |s: Id<Type>, t: Id<Type>| Ok(self.type_checker.ref_(self).is_type_closely_matched_by(s, t)),
            )?;
            if targets.is_empty() {
                return Ok(());
            }
            target = self.type_checker.ref_(self).get_union_type(
                &targets,
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?;
            if sources.is_empty() {
                self.infer_with_priority(source, target, InferencePriority::NakedTypeVariable)?;
                return Ok(());
            }
            source = self.type_checker.ref_(self).get_union_type(
                &sources,
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?;
        } else if target
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Intersection)
            && try_some(
                Some(
                    target
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types(),
                ),
                Some(|&t: &Id<Type>| -> io::Result<_> {
                    Ok(self.get_inference_info_for_type(t).is_some()
                        || (self.type_checker.ref_(self).is_generic_mapped_type(t)?
                            && self
                                .get_inference_info_for_type(
                                    self.type_checker
                                        .ref_(self).get_homomorphic_type_variable(t)?
                                        .unwrap_or_else(|| self.type_checker.ref_(self).never_type()),
                                )
                                .is_some()))
                }),
            )?
        {
            if !source.ref_(self).flags().intersects(TypeFlags::Union) {
                let (sources, targets) = self.infer_from_matching_types(
                    &if source
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::Intersection)
                    {
                        source
                            .ref_(self)
                            .as_union_or_intersection_type_interface()
                            .types()
                            .to_owned()
                    } else {
                        vec![source.clone()]
                    },
                    target
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types(),
                    |s: Id<Type>, t: Id<Type>| self.type_checker.ref_(self).is_type_identical_to(s, t),
                )?;
                if sources.is_empty() || targets.is_empty() {
                    return Ok(());
                }
                source = self.type_checker.ref_(self).get_intersection_type(
                    &sources,
                    Option::<Id<Symbol>>::None,
                    None,
                )?;
                target = self.type_checker.ref_(self).get_intersection_type(
                    &targets,
                    Option::<Id<Symbol>>::None,
                    None,
                )?;
            }
        } else if target
            .ref_(self)
            .flags()
            .intersects(TypeFlags::IndexedAccess | TypeFlags::Substitution)
        {
            target = self.type_checker.ref_(self).get_actual_type_variable(target)?;
        }
        if target
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TypeVariable)
        {
            if get_object_flags(&source.ref_(self)).intersects(ObjectFlags::NonInferrableType)
                || source == self.type_checker.ref_(self).non_inferrable_any_type()
                || source == self.type_checker.ref_(self).silent_never_type()
                || self.priority().intersects(InferencePriority::ReturnType)
                    && (source == self.type_checker.ref_(self).auto_type()
                        || source == self.type_checker.ref_(self).auto_array_type())
                || self.type_checker.ref_(self).is_from_inference_blocked_source(source)
            {
                return Ok(());
            }
            let inference = self.get_inference_info_for_type(target);
            if let Some(inference) = inference {
                if !inference.ref_(self).is_fixed() {
                    if match inference.ref_(self).maybe_priority() {
                        None => true,
                        Some(inference_priority) => self.priority() < inference_priority,
                    } {
                        *inference.ref_(self).maybe_candidates_mut() = None;
                        *inference.ref_(self).maybe_contra_candidates_mut() = None;
                        inference.ref_(self).set_top_level(true);
                        inference.ref_(self).set_priority(Some(self.priority()));
                    }
                    if Some(self.priority()) == inference.ref_(self).maybe_priority() {
                        let candidate = self
                            .maybe_propagation_type()
                            .unwrap_or_else(|| source.clone());
                        if self.contravariant() && !self.bivariant() {
                            if !contains(inference.ref_(self).maybe_contra_candidates().as_deref(), &candidate)
                            {
                                if inference.ref_(self).maybe_contra_candidates().is_none() {
                                    *inference.ref_(self).maybe_contra_candidates_mut() = Some(vec![]);
                                }
                                inference
                                    .ref_(self).maybe_contra_candidates_mut()
                                    .as_mut()
                                    .unwrap()
                                    .push(candidate);
                                self.type_checker.ref_(self).clear_cached_inferences(&self.inferences);
                            }
                        } else if !contains(inference.ref_(self).maybe_candidates().as_deref(), &candidate) {
                            if inference.ref_(self).maybe_candidates().is_none() {
                                *inference.ref_(self).maybe_candidates_mut() = Some(vec![]);
                            }
                            inference
                                .ref_(self).maybe_candidates_mut()
                                .as_mut()
                                .unwrap()
                                .push(candidate);
                            self.type_checker.ref_(self).clear_cached_inferences(&self.inferences);
                        }
                    }
                    if !self.priority().intersects(InferencePriority::ReturnType)
                        && target
                            .ref_(self)
                            .flags()
                            .intersects(TypeFlags::TypeParameter)
                        && inference.ref_(self).top_level()
                        && !self
                            .type_checker
                            .ref_(self).is_type_parameter_at_top_level(self.original_target, target)?
                    {
                        inference.ref_(self).set_top_level(false);
                        self.type_checker.ref_(self).clear_cached_inferences(&self.inferences);
                    }
                }
                self.set_inference_priority(cmp::min(self.inference_priority(), self.priority()));
                return Ok(());
            } else {
                let simplified = self.type_checker.ref_(self).get_simplified_type(target, false)?;
                if simplified != target {
                    self.try_invoke_once(
                        source,
                        simplified,
                        |source: Id<Type>, target: Id<Type>| self.infer_from_types(source, target),
                    )?;
                } else if target
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::IndexedAccess)
                {
                    let index_type = self.type_checker.ref_(self).get_simplified_type(
                        target.ref_(self).as_indexed_access_type().index_type,
                        false,
                    )?;
                    if index_type
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::Instantiable)
                    {
                        let simplified = self.type_checker.ref_(self).distribute_index_over_object_type(
                            self.type_checker.ref_(self).get_simplified_type(
                                target.ref_(self).as_indexed_access_type().object_type,
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
        if get_object_flags(&source.ref_(self)).intersects(ObjectFlags::Reference)
            && get_object_flags(&target.ref_(self)).intersects(ObjectFlags::Reference)
            && {
                (source.ref_(self).as_type_reference_interface().target()
                    == target.ref_(self).as_type_reference_interface().target()
                    || self.type_checker.ref_(self).is_array_type(source)
                        && self.type_checker.ref_(self).is_array_type(target))
                    && !(source
                        .ref_(self)
                        .as_type_reference_interface()
                        .maybe_node()
                        .is_some()
                        && target
                            .ref_(self)
                            .as_type_reference_interface()
                            .maybe_node()
                            .is_some())
            }
        {
            self.infer_from_type_arguments(
                &*self.type_checker.ref_(self).get_type_arguments(source)?,
                &*self.type_checker.ref_(self).get_type_arguments(target)?,
                &self
                    .type_checker
                    .ref_(self).get_variances(source.ref_(self).as_type_reference_interface().target()),
            )?;
        } else if source.ref_(self).flags().intersects(TypeFlags::Index)
            && target.ref_(self).flags().intersects(TypeFlags::Index)
        {
            self.set_contravariant(!self.contravariant());
            self.infer_from_types(
                source.ref_(self).as_index_type().type_,
                target.ref_(self).as_index_type().type_,
            )?;
            self.set_contravariant(!self.contravariant());
        } else if (self.type_checker.ref_(self).is_literal_type(source)
            || source.ref_(self).flags().intersects(TypeFlags::String))
            && target.ref_(self).flags().intersects(TypeFlags::Index)
        {
            let empty = self
                .type_checker
                .ref_(self).create_empty_object_type_from_string_literal(source)?;
            self.set_contravariant(!self.contravariant());
            self.infer_with_priority(
                empty,
                target.ref_(self).as_index_type().type_,
                InferencePriority::LiteralKeyof,
            )?;
            self.set_contravariant(!self.contravariant());
        } else if source
            .ref_(self)
            .flags()
            .intersects(TypeFlags::IndexedAccess)
            && target
                .ref_(self)
                .flags()
                .intersects(TypeFlags::IndexedAccess)
        {
            self.infer_from_types(
                source.ref_(self).as_indexed_access_type().object_type,
                target.ref_(self).as_indexed_access_type().object_type,
            )?;
            self.infer_from_types(
                source.ref_(self).as_indexed_access_type().index_type,
                target.ref_(self).as_indexed_access_type().index_type,
            )?;
        } else if source
            .ref_(self)
            .flags()
            .intersects(TypeFlags::StringMapping)
            && target
                .ref_(self)
                .flags()
                .intersects(TypeFlags::StringMapping)
        {
            if source.ref_(self).symbol() == target.ref_(self).symbol() {
                self.infer_from_types(
                    source.ref_(self).as_string_mapping_type().type_,
                    target.ref_(self).as_string_mapping_type().type_,
                )?;
            }
        } else if source
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Substitution)
        {
            self.infer_from_types(source.ref_(self).as_substitution_type().base_type, target)?;
            let old_priority = self.priority();
            self.set_priority(self.priority() | InferencePriority::SubstituteSource);
            self.infer_from_types(source.ref_(self).as_substitution_type().substitute, target)?;
            self.set_priority(old_priority);
        } else if target.ref_(self).flags().intersects(TypeFlags::Conditional) {
            self.try_invoke_once(source, target, |source: Id<Type>, target: Id<Type>| {
                self.infer_to_conditional_type(source, target)
            })?;
        } else if target
            .ref_(self)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
        {
            self.infer_to_multiple_types(
                source,
                target
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types(),
                target.ref_(self).flags(),
            )?;
        } else if source.ref_(self).flags().intersects(TypeFlags::Union) {
            let source_types = &source
                .ref_(self)
                .as_union_or_intersection_type_interface()
                .types()
                .to_owned();
            for &source_type in source_types {
                self.infer_from_types(source_type, target)?;
            }
        } else if target
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TemplateLiteral)
        {
            self.infer_to_template_literal_type(source, target)?;
        } else {
            source = self.type_checker.ref_(self).get_reduced_type(source)?;
            if !(self.priority().intersects(InferencePriority::NoConstraints)
                && source
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::Intersection | TypeFlags::Instantiable))
            {
                let apparent_source = self.type_checker.ref_(self).get_apparent_type(source)?;
                if apparent_source != source
                    && self.allow_complex_constraint_inference()
                    && !apparent_source
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::Object | TypeFlags::Intersection)
                {
                    self.set_allow_complex_constraint_inference(false);
                    return self.infer_from_types(apparent_source, target);
                }
                source = apparent_source;
            }
            if source
                .ref_(self)
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
        let key = format!("{},{}", source.ref_(self).id(), target.ref_(self).id());
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
        let source_identity = self.type_checker.ref_(self).get_recursion_identity(source);
        let target_identity = self.type_checker.ref_(self).get_recursion_identity(target);
        if contains(self.maybe_source_stack().as_deref(), &source_identity) {
            self.set_expanding_flags(self.expanding_flags() | ExpandingFlags::Source);
        }
        if contains(self.maybe_target_stack().as_deref(), &target_identity) {
            self.set_expanding_flags(self.expanding_flags() | ExpandingFlags::Target);
        }
        if self.expanding_flags() != ExpandingFlags::Both {
            if self.maybe_source_stack().is_none() {
                self.set_source_stack(Some(vec![]));
            }
            self.maybe_source_stack_mut()
                .as_mut()
                .unwrap()
                .push(source_identity);
            if self.maybe_target_stack().is_none() {
                self.set_target_stack(Some(vec![]));
            }
            self.maybe_target_stack_mut()
                .as_mut()
                .unwrap()
                .push(target_identity);
            action(source, target)?;
            self.maybe_target_stack_mut().as_mut().unwrap().pop();
            self.maybe_source_stack_mut().as_mut().unwrap().pop();
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
        if self.type_checker.ref_(self).strict_function_types
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
