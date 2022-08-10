#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::{Cell, RefCell, RefMut};
use std::cmp;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{ExpandingFlags, RecursionIdentity, TypeFacts};
use crate::{
    arrays_equal, contains_rc, create_scanner, every, get_check_flags, get_object_flags,
    is_write_only_access, map, node_is_missing, some, CheckFlags, DiagnosticMessage, Diagnostics,
    ElementFlags, InferenceContext, InferenceInfo, InferencePriority, Node, NodeInterface,
    ObjectFlags, ScriptTarget, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, TokenFlags, Type,
    TypeChecker, TypeFlags, TypeInterface, TypeReferenceInterface, UnionReduction, VarianceFlags,
};

impl TypeChecker {
    pub(super) fn infer_reverse_mapped_type(
        &self,
        source_type: &Type,
        target: &Type,     /*MappedType*/
        constraint: &Type, /*IndexType*/
    ) -> Rc<Type> {
        let type_parameter = self.get_indexed_access_type(
            &constraint.as_index_type().type_,
            &self.get_type_parameter_from_mapped_type(target),
            None,
            Option::<&Node>::None,
            Option::<&Symbol>::None,
            None,
        );
        let template_type = self.get_template_type_from_mapped_type(target);
        let inference = Rc::new(self.create_inference_info(&type_parameter));
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
    ) -> Vec<Rc<Symbol>> {
        let properties = self.get_properties_of_type(target);
        let mut ret = vec![];
        for target_prop in &properties {
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
                                    || Rc::ptr_eq(
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
    ) -> Option<Rc<Symbol>> {
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
        let source_target_as_tuple_type = source.as_type_reference().target.as_tuple_type();
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

    pub(super) fn get_type_from_inference(&self, inference: &InferenceInfo) -> Option<Rc<Type>> {
        if let Some(inference_candidates) = inference.maybe_candidates().as_ref() {
            Some(self.get_union_type(
                inference_candidates.clone(),
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
                Some(|declaration: &Rc<Node>| self.has_skip_direct_inference_flag(declaration))
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
    ) -> Option<Vec<Rc<Type>>> {
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
                    |type_: &Rc<Type>, _| self.get_string_like_type_for_type(type_),
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
                |r: &Rc<Type>, i| {
                    self.is_valid_type_for_template_literal_placeholder(
                        r,
                        &target_as_template_literal_type.types[i],
                    )
                }
            )
        )
    }

    pub(super) fn get_string_like_type_for_type(&self, type_: &Type) -> Rc<Type> {
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
        source_types: &[Rc<Type>],
        target: &Type, /*TemplateLiteralType*/
    ) -> Option<Vec<Rc<Type>>> {
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
        let mut matches: Vec<Rc<Type>> = vec![];
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
        matches: &mut Vec<Rc<Type>>,
        source_types: &[Rc<Type>],
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
        inferences: &[Rc<InferenceInfo>],
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

    pub(super) fn is_type_or_base_identical_to(&self, s: &Type, t: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_type_closely_matched_by(&self, s: &Type, t: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_object_literal_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::ObjectLiteral)
    }

    pub(super) fn is_object_or_array_literal_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::ObjectLiteral | ObjectFlags::ArrayLiteral)
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

pub(super) struct InferTypes {
    pub type_checker: Rc<TypeChecker>,
    pub inferences: Vec<Rc<InferenceInfo>>,
    pub original_target: Rc<Type>,
    priority: Cell<InferencePriority>,
    contravariant: Cell<bool>,
    bivariant: Cell<bool>,
    propagation_type: RefCell<Option<Rc<Type>>>,
    inference_priority: Cell<InferencePriority>,
    allow_complex_constraint_inference: Cell<bool>,
    visited: RefCell<Option<HashMap<String, InferencePriority>>>,
    source_stack: RefCell<Option<Vec<RecursionIdentity>>>,
    target_stack: RefCell<Option<Vec<RecursionIdentity>>>,
    expanding_flags: Cell<ExpandingFlags>,
}

impl InferTypes {
    pub(super) fn new(
        type_checker: Rc<TypeChecker>,
        inferences: Vec<Rc<InferenceInfo>>,
        original_target: Rc<Type>,
        priority: InferencePriority,
        contravariant: bool,
    ) -> Self {
        Self {
            type_checker,
            inferences,
            original_target,
            priority: Cell::new(priority),
            contravariant: Cell::new(contravariant),
            bivariant: Cell::new(false),
            propagation_type: RefCell::new(None),
            inference_priority: Cell::new(InferencePriority::MaxValue),
            allow_complex_constraint_inference: Cell::new(true),
            visited: RefCell::new(None),
            source_stack: RefCell::new(None),
            target_stack: RefCell::new(None),
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

    pub(super) fn maybe_propagation_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.propagation_type.borrow_mut()
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
                Some(target_alias_symbol) if Rc::ptr_eq(source_alias_symbol, target_alias_symbol)
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
                targets,
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
                sources,
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            );
        } else if target.flags().intersects(TypeFlags::Intersection)
            && some(
                Some(target.as_union_or_intersection_type_interface().types()),
                Some(|t: &Rc<Type>| {
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
                || Rc::ptr_eq(&source, &self.type_checker.non_inferrable_any_type())
                || Rc::ptr_eq(&source, &self.type_checker.silent_never_type())
                || self.priority().intersects(InferencePriority::ReturnType)
                    && (Rc::ptr_eq(&source, &self.type_checker.auto_type())
                        || Rc::ptr_eq(&source, &self.type_checker.auto_array_type()))
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
                        *inference.maybe_candidates() = None;
                        *inference.maybe_contra_candidates() = None;
                        inference.set_top_level(true);
                        inference.set_priority(Some(self.priority()));
                    }
                    if Some(self.priority()) == inference.maybe_priority() {
                        let candidate = self
                            .maybe_propagation_type()
                            .clone()
                            .unwrap_or_else(|| source.clone());
                        if self.contravariant() && !self.bivariant() {
                            if !contains_rc(
                                inference.maybe_contra_candidates().as_deref(),
                                &candidate,
                            ) {
                                if inference.maybe_contra_candidates().is_none() {
                                    *inference.maybe_contra_candidates() = Some(vec![]);
                                }
                                inference
                                    .maybe_contra_candidates()
                                    .as_mut()
                                    .unwrap()
                                    .push(candidate);
                                self.type_checker.clear_cached_inferences(&self.inferences);
                            }
                        } else if !contains_rc(inference.maybe_candidates().as_deref(), &candidate)
                        {
                            if inference.maybe_candidates().is_none() {
                                *inference.maybe_candidates() = Some(vec![]);
                            }
                            inference
                                .maybe_candidates()
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
                if !Rc::ptr_eq(&simplified, &target) {
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
                            .filter(|simplified| !Rc::ptr_eq(simplified, &target))
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
                let source_as_type_reference = source.as_type_reference();
                let target_as_type_reference = target.as_type_reference();
                (Rc::ptr_eq(
                    &source_as_type_reference.target,
                    &target_as_type_reference.target,
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
                    .get_variances(&source.as_type_reference().target),
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
            if Rc::ptr_eq(&source.symbol(), &target.symbol()) {
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
                if !Rc::ptr_eq(&apparent_source, &source)
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
        unimplemented!()
    }

    pub(super) fn invoke_once<TAction: FnMut(&Type, &Type)>(
        &self,
        source: &Type,
        target: &Type,
        action: TAction,
    ) {
        unimplemented!()
    }

    pub(super) fn infer_from_matching_types<TMatches: FnMut(&Type, &Type) -> bool>(
        &self,
        sources: &[Rc<Type>],
        targets: &[Rc<Type>],
        matches: TMatches,
    ) -> (Vec<Rc<Type>>, Vec<Rc<Type>>) {
        unimplemented!()
    }

    pub(super) fn infer_from_type_arguments(
        &self,
        source_types: &[Rc<Type>],
        target_types: &[Rc<Type>],
        variances: &[VarianceFlags],
    ) {
        unimplemented!()
    }

    pub(super) fn get_inference_info_for_type(&self, type_: &Type) -> Option<Rc<InferenceInfo>> {
        unimplemented!()
    }

    pub(super) fn infer_to_multiple_types(
        &self,
        source: &Type,
        targets: &[Rc<Type>],
        target_flags: TypeFlags,
    ) {
        unimplemented!()
    }

    pub(super) fn infer_to_conditional_type(
        &self,
        source: &Type,
        target: &Type, /*ConditionalType*/
    ) {
        unimplemented!()
    }

    pub(super) fn infer_to_template_literal_type(
        &self,
        source: &Type,
        target: &Type, /*TemplateLiteralType*/
    ) {
        unimplemented!()
    }

    pub(super) fn infer_from_object_types(&self, source: &Type, target: &Type) {
        unimplemented!()
    }
}
