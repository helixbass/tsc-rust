#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{TypeFacts, WideningKind};
use crate::{
    __String, are_option_rcs_equal, every, filter, get_object_flags, is_write_only_access, last,
    length, node_is_missing, reduce_left_no_initial_value, some, Debug_, DiagnosticMessage,
    Diagnostics, InferenceContext, InferenceFlags, InferenceInfo, InferencePriority,
    InterfaceTypeInterface, Node, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, Signature,
    Symbol, SymbolFlags, SyntaxKind, Ternary, Type, TypeChecker, TypeFlags, TypeInterface,
    TypePredicate, UnionReduction,
};

impl TypeChecker {
    pub(super) fn compare_signatures_identical<TCompareTypes: FnMut(&Type, &Type) -> Ternary>(
        &self,
        mut source: Rc<Signature>,
        target: Rc<Signature>,
        partial_match: bool,
        ignore_this_types: bool,
        ignore_return_types: bool,
        mut compare_types: TCompareTypes,
    ) -> Ternary {
        if Rc::ptr_eq(&source, &target) {
            return Ternary::True;
        }
        if !self.is_matching_signature(&source, &target, partial_match) {
            return Ternary::False;
        }
        if length(source.type_parameters.as_deref()) != length(target.type_parameters.as_deref()) {
            return Ternary::False;
        }
        if let Some(target_type_parameters) = target.type_parameters.as_ref() {
            let source_type_parameters = source.type_parameters.as_ref().unwrap();
            let mapper = self.create_type_mapper(
                source_type_parameters.clone(),
                Some(target_type_parameters.clone()),
            );
            for (i, t) in target_type_parameters.iter().enumerate() {
                let s = &source_type_parameters[i];
                if !(Rc::ptr_eq(s, t)
                    || compare_types(
                        &self
                            .maybe_instantiate_type(
                                self.get_constraint_from_type_parameter(s),
                                Some(&mapper),
                            )
                            .unwrap_or_else(|| self.unknown_type()),
                        &self
                            .get_constraint_from_type_parameter(t)
                            .unwrap_or_else(|| self.unknown_type()),
                    ) != Ternary::False
                        && compare_types(
                            &self
                                .maybe_instantiate_type(
                                    self.get_default_from_type_parameter_(s),
                                    Some(&mapper),
                                )
                                .unwrap_or_else(|| self.unknown_type()),
                            &self
                                .get_default_from_type_parameter_(t)
                                .unwrap_or_else(|| self.unknown_type()),
                        ) != Ternary::False)
                {
                    return Ternary::False;
                }
            }
            source = Rc::new(self.instantiate_signature(source, &mapper, Some(true)));
        }
        let mut result = Ternary::True;
        if !ignore_this_types {
            let source_this_type = self.get_this_type_of_signature(&source);
            if let Some(source_this_type) = source_this_type {
                let target_this_type = self.get_this_type_of_signature(&target);
                if let Some(target_this_type) = target_this_type {
                    let related = compare_types(&source_this_type, &target_this_type);
                    if related == Ternary::False {
                        return Ternary::False;
                    }
                    result &= related;
                }
            }
        }
        let target_len = self.get_parameter_count(&target);
        for i in 0..target_len {
            let s = self.get_type_at_position(&source, i);
            let t = self.get_type_at_position(&target, i);
            let related = compare_types(&t, &s);
            if related == Ternary::False {
                return Ternary::False;
            }
            result &= related;
        }
        if !ignore_return_types {
            let source_type_predicate = self.get_type_predicate_of_signature(&source);
            let target_type_predicate = self.get_type_predicate_of_signature(&target);
            result &= if source_type_predicate.is_some() || target_type_predicate.is_some() {
                self.compare_type_predicates_identical(
                    source_type_predicate,
                    target_type_predicate,
                    |s, t| compare_types(s, t),
                )
            } else {
                compare_types(
                    &self.get_return_type_of_signature(source),
                    &self.get_return_type_of_signature(target),
                )
            };
        }
        result
    }

    pub(super) fn compare_type_predicates_identical<
        TSource: Borrow<TypePredicate>,
        TTarget: Borrow<TypePredicate>,
        TCompareTypes: FnOnce(&Type, &Type) -> Ternary,
    >(
        &self,
        source: Option<TSource>,
        target: Option<TTarget>,
        compare_types: TCompareTypes,
    ) -> Ternary {
        match (source, target) {
            (Some(source), Some(target)) => {
                let source = source.borrow();
                let target = target.borrow();
                if !self.type_predicate_kinds_match(source, target) {
                    Ternary::False
                } else if are_option_rcs_equal(source.type_.as_ref(), target.type_.as_ref()) {
                    Ternary::True
                } else if let (Some(source_type), Some(target_type)) =
                    (source.type_.as_ref(), target.type_.as_ref())
                {
                    compare_types(source_type, target_type)
                } else {
                    Ternary::False
                }
            }
            _ => Ternary::False,
        }
    }

    pub(super) fn literal_types_with_same_base_type(&self, types: &[Rc<Type>]) -> bool {
        let mut common_base_type: Option<Rc<Type>> = None;
        for t in types {
            let base_type = self.get_base_type_of_literal_type(t);
            if common_base_type.is_none() {
                common_base_type = Some(base_type.clone());
            }
            if Rc::ptr_eq(&base_type, t)
                || !Rc::ptr_eq(&base_type, common_base_type.as_ref().unwrap())
            {
                return false;
            }
        }
        true
    }

    pub(super) fn get_supertype_or_union(&self, types: &[Rc<Type>]) -> Rc<Type> {
        if types.len() == 1 {
            return types[0].clone();
        }
        if self.literal_types_with_same_base_type(types) {
            self.get_union_type(
                types.to_owned(),
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )
        } else {
            reduce_left_no_initial_value(
                types,
                |s: Rc<Type>, t: &Rc<Type>, _| {
                    if self.is_type_subtype_of(&s, t) {
                        t.clone()
                    } else {
                        s
                    }
                },
                None,
                None,
            )
        }
    }

    pub(super) fn get_common_supertype(&self, types: &[Rc<Type>]) -> Rc<Type> {
        if !self.strict_null_checks {
            return self.get_supertype_or_union(types);
        }
        let primary_types = filter(Some(types), |t: &Rc<Type>| {
            !t.flags().intersects(TypeFlags::Nullable)
        })
        .unwrap();
        if !primary_types.is_empty() {
            self.get_nullable_type(
                &self.get_supertype_or_union(&primary_types),
                self.get_falsy_flags_of_types(types) & TypeFlags::Nullable,
            )
        } else {
            self.get_union_type(
                types.to_owned(),
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )
        }
    }

    pub(super) fn get_common_subtype(&self, types: &[Rc<Type>]) -> Rc<Type> {
        reduce_left_no_initial_value(
            types,
            |s: Rc<Type>, t: &Rc<Type>, _| {
                if self.is_type_subtype_of(t, &s) {
                    t.clone()
                } else {
                    s
                }
            },
            None,
            None,
        )
    }

    pub(super) fn is_array_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::Reference)
            && (Rc::ptr_eq(&type_.as_type_reference().target, &self.global_array_type())
                || Rc::ptr_eq(
                    &type_.as_type_reference().target,
                    &self.global_readonly_array_type(),
                ))
    }

    pub(super) fn is_readonly_array_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::Reference)
            && Rc::ptr_eq(
                &type_.as_type_reference().target,
                &self.global_readonly_array_type(),
            )
    }

    pub(super) fn is_mutable_array_or_tuple(&self, type_: &Type) -> bool {
        self.is_array_type(type_) && !self.is_readonly_array_type(type_)
            || self.is_tuple_type(type_)
                && !type_.as_type_reference().target.as_tuple_type().readonly
    }

    pub(super) fn get_element_type_of_array_type(&self, type_: &Type) -> Option<Rc<Type>> {
        if self.is_array_type(type_) {
            self.get_type_arguments(type_).get(0).map(Clone::clone)
        } else {
            None
        }
    }

    pub(super) fn is_array_like_type(&self, type_: &Type) -> bool {
        self.is_array_type(type_)
            || !type_.flags().intersects(TypeFlags::Nullable)
                && self.is_type_assignable_to(type_, &self.any_readonly_array_type())
    }

    pub(super) fn get_single_base_for_non_augmenting_subtype(
        &self,
        type_: &Type,
    ) -> Option<Rc<Type>> {
        if !get_object_flags(type_).intersects(ObjectFlags::Reference)
            || !get_object_flags(&type_.as_type_reference().target)
                .intersects(ObjectFlags::ClassOrInterface)
        {
            return None;
        }
        let type_as_type_reference = type_.as_type_reference();
        if get_object_flags(type_).intersects(ObjectFlags::IdenticalBaseTypeCalculated) {
            return if get_object_flags(type_).intersects(ObjectFlags::IdenticalBaseTypeExists) {
                type_as_type_reference
                    .maybe_cached_equivalent_base_type()
                    .clone()
            } else {
                None
            };
        }
        type_as_type_reference.set_object_flags(
            type_as_type_reference.object_flags() | ObjectFlags::IdenticalBaseTypeCalculated,
        );
        let target = &type_as_type_reference.target;
        if get_object_flags(target).intersects(ObjectFlags::Class) {
            let base_type_node = self.get_base_type_node_of_class(target);
            if matches!(
                base_type_node.as_ref(),
                Some(base_type_node) if !matches!(
                    base_type_node.as_expression_with_type_arguments().expression.kind(),
                    SyntaxKind::Identifier | SyntaxKind::PropertyAccessExpression
                )
            ) {
                return None;
            }
        }
        let bases = self.get_base_types(target);
        if bases.len() != 1 {
            return None;
        }
        if !(*self.get_members_of_symbol(&type_.symbol()))
            .borrow()
            .is_empty()
        {
            return None;
        }
        let target_as_interface_type = target.as_interface_type();
        let mut instantiated_base = if length(target_as_interface_type.maybe_type_parameters()) == 0
        {
            bases[0].clone()
        } else {
            let target_type_parameters = target_as_interface_type
                .maybe_type_parameters()
                .map(ToOwned::to_owned)
                .unwrap();
            let target_type_parameters_len = target_type_parameters.len();
            self.instantiate_type(
                &bases[0],
                Some(&self.create_type_mapper(
                    target_type_parameters,
                    Some(self.get_type_arguments(type_)[0..target_type_parameters_len].to_owned()),
                )),
            )
        };
        if length(Some(&self.get_type_arguments(type_)))
            > length(target_as_interface_type.maybe_type_parameters())
        {
            instantiated_base = self.get_type_with_this_argument(
                &instantiated_base,
                Some(&**last(&self.get_type_arguments(type_))),
                None,
            );
        }
        type_as_type_reference.set_object_flags(
            type_as_type_reference.object_flags() | ObjectFlags::IdenticalBaseTypeExists,
        );
        *type_as_type_reference.maybe_cached_equivalent_base_type() =
            Some(instantiated_base.clone());
        Some(instantiated_base)
    }

    pub(super) fn is_empty_literal_type(&self, type_: &Type) -> bool {
        if self.strict_null_checks {
            ptr::eq(type_, &*self.implicit_never_type())
        } else {
            ptr::eq(type_, &*self.undefined_widening_type())
        }
    }

    pub(super) fn is_empty_array_literal_type(&self, type_: &Type) -> bool {
        let element_type = self.get_element_type_of_array_type(type_);
        matches!(
            element_type.as_ref(),
            Some(element_type) if self.is_empty_literal_type(element_type)
        )
    }

    pub(super) fn is_tuple_like_type(&self, type_: &Type) -> bool {
        self.is_tuple_type(type_)
            || self
                .get_property_of_type_(type_, &__String::new("0".to_owned()), None)
                .is_some()
    }

    pub(super) fn is_array_or_tuple_like_type(&self, type_: &Type) -> bool {
        self.is_array_like_type(type_) || self.is_tuple_like_type(type_)
    }

    pub(super) fn get_tuple_element_type(&self, type_: &Type, index: usize) -> Option<Rc<Type>> {
        let prop_type =
            self.get_type_of_property_of_type_(type_, &__String::new(index.to_string()));
        if prop_type.is_some() {
            return prop_type;
        }
        if self.every_type(type_, |type_: &Type| self.is_tuple_type(type_)) {
            return self.map_type(
                type_,
                &mut |t: &Type| {
                    Some(
                        self.get_rest_type_of_tuple_type(t)
                            .unwrap_or_else(|| self.undefined_type()),
                    )
                },
                None,
            );
        }
        None
    }

    pub(super) fn is_neither_unit_type_nor_never(&self, type_: &Type) -> bool {
        !type_.flags().intersects(TypeFlags::Unit | TypeFlags::Never)
    }

    pub(super) fn is_unit_type(&self, type_: &Type) -> bool {
        type_.flags().intersects(TypeFlags::Unit)
    }

    pub(super) fn is_unit_like_type(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Intersection) {
            some(
                Some(type_.as_union_or_intersection_type_interface().types()),
                Some(|type_: &Rc<Type>| self.is_unit_type(type_)),
            )
        } else {
            type_.flags().intersects(TypeFlags::Unit)
        }
    }

    pub(super) fn is_literal_type(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Boolean) {
            true
        } else if type_.flags().intersects(TypeFlags::Union) {
            if type_.flags().intersects(TypeFlags::EnumLiteral) {
                true
            } else {
                every(
                    type_.as_union_or_intersection_type_interface().types(),
                    |type_, _| self.is_unit_type(&**type_),
                )
            }
        } else {
            self.is_unit_type(type_)
        }
    }

    pub(super) fn get_base_type_of_literal_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::EnumLiteral) {
            unimplemented!()
        } else if type_.flags().intersects(TypeFlags::StringLiteral) {
            unimplemented!()
        } else if type_.flags().intersects(TypeFlags::NumberLiteral) {
            self.number_type()
        } else if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            self.bigint_type()
        } else if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            self.boolean_type()
        } else if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.get_base_type_of_literal_type(type_)),
                None,
            )
            .unwrap()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_widened_literal_type(&self, type_: &Type) -> Rc<Type> {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::EnumLiteral) && self.is_fresh_literal_type(type_) {
            unimplemented!()
        } else if flags.intersects(TypeFlags::StringLiteral) && self.is_fresh_literal_type(type_) {
            unimplemented!()
        } else if flags.intersects(TypeFlags::NumberLiteral) && self.is_fresh_literal_type(type_) {
            self.number_type()
        } else if flags.intersects(TypeFlags::BigIntLiteral) && self.is_fresh_literal_type(type_) {
            self.bigint_type()
        } else if flags.intersects(TypeFlags::BooleanLiteral) && self.is_fresh_literal_type(type_) {
            self.boolean_type()
        } else if flags.intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.get_widened_literal_type(type_)),
                None,
            )
            .unwrap()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_widened_unique_es_symbol_type(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn get_widened_literal_like_type_for_contextual_type<TTypeRef: Borrow<Type>>(
        &self,
        type_: &Type,
        contextual_type: Option<TTypeRef>,
    ) -> Rc<Type> {
        let mut type_ = type_.type_wrapper();
        if !self.is_literal_of_contextual_type(&type_, contextual_type) {
            type_ = self.get_widened_unique_es_symbol_type(&self.get_widened_literal_type(&type_));
        }
        type_
    }

    pub(super) fn is_tuple_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::Reference)
            && type_
                .as_type_reference()
                .target
                .as_object_type()
                .object_flags()
                .intersects(ObjectFlags::Tuple)
    }

    pub(super) fn is_generic_tuple_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_single_element_generic_tuple_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_rest_type_of_tuple_type(
        &self,
        type_: &Type, /*TupleTypeReference*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_rest_array_type_of_tuple_type(
        &self,
        type_: &Type, /*TupleTypeReference*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_element_type_of_slice_of_tuple_type(
        &self,
        type_: &Type, /*TupleTypeReference*/
        index: usize,
        end_skip_count: Option<usize>,
        writing: Option<bool>,
    ) -> Option<Rc<Type>> {
        let end_skip_count = end_skip_count.unwrap_or(0);
        let writing = writing.unwrap_or(false);
        unimplemented!()
    }

    pub(super) fn get_falsy_flags_of_types(&self, types: &[Rc<Type>]) -> TypeFlags {
        unimplemented!()
    }

    pub(super) fn get_falsy_flags(&self, type_: &Type) -> TypeFlags {
        unimplemented!()
    }

    pub(super) fn get_nullable_type(&self, type_: &Type, flags: TypeFlags) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_optional_type_(&self, type_: &Type, is_property: Option<bool>) -> Rc<Type> {
        let is_property = is_property.unwrap_or(false);
        Debug_.assert(self.strict_null_checks, None);
        if type_.flags().intersects(TypeFlags::Undefined) {
            type_.type_wrapper()
        } else {
            unimplemented!()
        }
    }

    pub(super) fn get_non_nullable_type(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn add_optional_type_marker(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn remove_optional_type_marker(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn remove_missing_type(&self, type_: &Type, is_optional: bool) -> Rc<Type> {
        if matches!(self.exact_optional_property_types, Some(true)) && is_optional {
            unimplemented!()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn contains_missing_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn remove_missing_or_undefined_type(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_object_type_with_inferable_index(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn create_symbol_with_type<TType: Borrow<Type>>(
        &self,
        source: &Symbol,
        type_: Option<TType>,
    ) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn get_regular_type_of_object_literal(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn get_widened_type(&self, type_: &Type) -> Rc<Type> {
        self.get_widened_type_with_context(type_)
    }

    pub(super) fn get_widened_type_with_context(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn report_widening_errors_in_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn report_implicit_any(
        &self,
        declaration: &Node, /*Declaration*/
        type_: &Type,
        widening_kind: Option<WideningKind>,
    ) {
        unimplemented!()
    }

    pub(super) fn report_errors_from_widening(
        &self,
        declaration: &Node, /*Declaration*/
        type_: &Type,
        widening_kind: Option<WideningKind>,
    ) {
        if self.produce_diagnostics
            && self.no_implicit_any
            && get_object_flags(type_).intersects(ObjectFlags::ContainsWideningType)
            && (widening_kind.is_none()
                || self
                    .get_contextual_signature_for_function_like_declaration(declaration)
                    .is_none())
        {
            if !self.report_widening_errors_in_type(type_) {
                self.report_implicit_any(declaration, type_, widening_kind);
            }
        }
    }

    pub(super) fn create_inference_context<
        TSignature: Borrow<Signature>,
        TCompareTypes: FnMut(&Type, &Type, Option<bool>) -> Ternary,
    >(
        &self,
        type_parameters: &[Rc<Type /*TypeParameter*/>],
        signature: Option<TSignature>,
        flags: InferenceFlags,
        compare_types: Option<TCompareTypes>,
    ) -> InferenceContext {
        unimplemented!()
    }

    pub(super) fn could_contain_type_variables(&self, type_: &Type) -> bool {
        let object_flags = get_object_flags(&type_);
        if object_flags.intersects(ObjectFlags::CouldContainTypeVariablesComputed) {
            return object_flags.intersects(ObjectFlags::CouldContainTypeVariables);
        }
        let result = type_.flags().intersects(TypeFlags::Instantiable) || unimplemented!();
        if type_.flags().intersects(TypeFlags::ObjectFlagsType) {
            let type_as_has_object_flags = type_.as_object_flags_type();
            type_as_has_object_flags.set_object_flags(
                type_as_has_object_flags.object_flags()
                    | ObjectFlags::CouldContainTypeVariablesComputed
                    | if result {
                        ObjectFlags::CouldContainTypeVariables
                    } else {
                        ObjectFlags::None
                    },
            );
        }
        result
    }

    pub(super) fn infer_type_for_homomorphic_mapped_type(
        &self,
        source: &Type,
        target: &Type,     /*MappedType*/
        constraint: &Type, /*IndexType*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_reverse_mapped_symbol(
        &self,
        symbol: &Symbol, /*ReverseMappedSymbol*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn infer_reverse_mapped_type(
        &self,
        source_type: &Type,
        target: &Type,     /*MappedType*/
        constraint: &Type, /*IndexType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_unmatched_properties(
        &self,
        source: &Type,
        target: &Type,
        require_optional_properties: bool,
        match_discriminant_properties: bool,
    ) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_unmatched_property(
        &self,
        source: &Type,
        target: &Type,
        require_optional_properties: bool,
        match_discriminant_properties: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn template_literal_types_definitely_unrelated(
        &self,
        source: &Type, /*TemplateLiteralType*/
        target: &Type, /*TemplateLiteralType*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_type_matched_by_template_literal_type(
        &self,
        source: &Type,
        target: &Type, /*TemplateLiteralType*/
    ) -> bool {
        unimplemented!()
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
        unimplemented!()
    }

    pub(super) fn is_object_literal_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::ObjectLiteral)
    }

    pub(super) fn is_object_or_array_literal_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::ObjectLiteral | ObjectFlags::ArrayLiteral)
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
