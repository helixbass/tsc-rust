#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{IterationTypeKind, TypeFacts};
use crate::{
    get_check_flags, is_outermost_optional_chain, CheckFlags, ElementFlags, Number,
    SymbolInterface, SymbolTable, TransientSymbolInterface, __String, are_option_rcs_equal,
    compiler::utilities_public::is_expression_of_optional_chain_root, create_symbol_table, every,
    filter, find, get_object_flags, is_optional_chain, last, length, reduce_left_no_initial_value,
    some, Debug_, InterfaceTypeInterface, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, Signature, Symbol, SymbolFlags, SyntaxKind, Ternary, Type,
    TypeChecker, TypeFlags, TypeInterface, TypePredicate, UnionReduction,
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
        let primary_types = filter(types, |t: &Rc<Type>| {
            !t.flags().intersects(TypeFlags::Nullable)
        });
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

    pub(super) fn extract_unit_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Intersection) {
            find(
                type_.as_union_or_intersection_type_interface().types(),
                |type_: &Rc<Type>, _| self.is_unit_type(type_),
            )
            .map(Clone::clone)
            .unwrap_or_else(|| type_.type_wrapper())
        } else {
            type_.type_wrapper()
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
                    |type_, _| self.is_unit_type(type_),
                )
            }
        } else {
            self.is_unit_type(type_)
        }
    }

    pub(super) fn get_base_type_of_literal_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::EnumLiteral) {
            self.get_base_type_of_enum_literal_type(type_)
        } else if type_.flags().intersects(TypeFlags::StringLiteral) {
            self.string_type()
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
            self.get_base_type_of_enum_literal_type(type_)
        } else if flags.intersects(TypeFlags::StringLiteral) && self.is_fresh_literal_type(type_) {
            self.string_type()
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
        if type_.flags().intersects(TypeFlags::UniqueESSymbol) {
            self.es_symbol_type()
        } else if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.get_widened_unique_es_symbol_type(type_)),
                None,
            )
            .unwrap()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_widened_literal_like_type_for_contextual_type<
        TContextualType: Borrow<Type>,
    >(
        &self,
        type_: &Type,
        contextual_type: Option<TContextualType>,
    ) -> Rc<Type> {
        let mut type_ = type_.type_wrapper();
        if !self.is_literal_of_contextual_type(&type_, contextual_type) {
            type_ = self.get_widened_unique_es_symbol_type(&self.get_widened_literal_type(&type_));
        }
        type_
    }

    pub(super) fn get_widened_literal_like_type_for_contextual_return_type_if_needed<
        TType: Borrow<Type>,
        TContextualSignatureReturnType: Borrow<Type>,
    >(
        &self,
        type_: Option<TType>,
        contextual_signature_return_type: Option<TContextualSignatureReturnType>,
        is_async: bool,
    ) -> Option<Rc<Type>> {
        let mut type_ = type_.map(|type_| type_.borrow().type_wrapper());
        if let Some(type_present) = type_.as_ref().filter(|type_| self.is_unit_type(type_)) {
            let contextual_type =
                contextual_signature_return_type.and_then(|contextual_signature_return_type| {
                    let contextual_signature_return_type =
                        contextual_signature_return_type.borrow();
                    if is_async {
                        self.get_promised_type_of_promise(
                            contextual_signature_return_type,
                            Option::<&Node>::None,
                        )
                    } else {
                        Some(contextual_signature_return_type.type_wrapper())
                    }
                });
            type_ =
                Some(self.get_widened_literal_like_type_for_contextual_type(
                    type_present,
                    contextual_type,
                ));
        }
        type_
    }

    pub(super) fn get_widened_literal_like_type_for_contextual_iteration_type_if_needed<
        TType: Borrow<Type>,
        TContextualSignatureReturnType: Borrow<Type>,
    >(
        &self,
        type_: Option<TType>,
        contextual_signature_return_type: Option<TContextualSignatureReturnType>,
        kind: IterationTypeKind,
        is_async_generator: bool,
    ) -> Option<Rc<Type>> {
        let mut type_ = type_.map(|type_| type_.borrow().type_wrapper());
        if let Some(type_present) = type_.as_ref().filter(|type_| self.is_unit_type(type_)) {
            let contextual_type =
                contextual_signature_return_type.and_then(|contextual_signature_return_type| {
                    let contextual_signature_return_type =
                        contextual_signature_return_type.borrow();
                    self.get_iteration_type_of_generator_function_return_type(
                        kind,
                        contextual_signature_return_type,
                        is_async_generator,
                    )
                });
            type_ =
                Some(self.get_widened_literal_like_type_for_contextual_type(
                    type_present,
                    contextual_type,
                ));
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
        self.is_tuple_type(type_)
            && type_
                .as_type_reference()
                .target
                .as_tuple_type()
                .combined_flags
                .intersects(ElementFlags::Variadic)
    }

    pub(super) fn is_single_element_generic_tuple_type(&self, type_: &Type) -> bool {
        self.is_generic_tuple_type(type_)
            && type_
                .as_type_reference()
                .target
                .as_tuple_type()
                .element_flags
                .len()
                == 1
    }

    pub(super) fn get_rest_type_of_tuple_type(
        &self,
        type_: &Type, /*TupleTypeReference*/
    ) -> Option<Rc<Type>> {
        self.get_element_type_of_slice_of_tuple_type(
            type_,
            type_
                .as_type_reference()
                .target
                .as_tuple_type()
                .fixed_length,
            None,
            None,
        )
    }

    pub(super) fn get_rest_array_type_of_tuple_type(
        &self,
        type_: &Type, /*TupleTypeReference*/
    ) -> Option<Rc<Type>> {
        let rest_type = self.get_rest_type_of_tuple_type(type_);
        rest_type
            .as_ref()
            .map(|rest_type| self.create_array_type(rest_type, None))
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
        let length = self.get_type_reference_arity(type_) - end_skip_count;
        if index < length {
            let type_arguments = self.get_type_arguments(type_);
            let mut element_types: Vec<Rc<Type>> = vec![];
            for i in index..length {
                let t = &type_arguments[i];
                element_types.push(
                    if type_
                        .as_type_reference()
                        .target
                        .as_tuple_type()
                        .element_flags[i]
                        .intersects(ElementFlags::Variadic)
                    {
                        self.get_indexed_access_type(
                            t,
                            &self.number_type(),
                            None,
                            Option::<&Node>::None,
                            Option::<&Symbol>::None,
                            None,
                        )
                    } else {
                        t.clone()
                    },
                );
            }
            return Some(if writing {
                self.get_intersection_type(&element_types, Option::<&Symbol>::None, None)
            } else {
                self.get_union_type(
                    element_types,
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                )
            });
        }
        None
    }

    pub(super) fn is_tuple_type_structure_matching(
        &self,
        t1: &Type, /*TupleTypeReference*/
        t2: &Type, /*TupleTypeReference*/
    ) -> bool {
        self.get_type_reference_arity(t1) == self.get_type_reference_arity(t2)
            && every(
                &t1.as_type_reference().target.as_tuple_type().element_flags,
                |f: &ElementFlags, i| {
                    *f & ElementFlags::Variable
                        == t2.as_type_reference().target.as_tuple_type().element_flags[i]
                            & ElementFlags::Variable
                },
            )
    }

    pub(super) fn is_zero_big_int(&self, type_: &Type /*BigIntLiteralType*/) -> bool {
        type_.as_big_int_literal_type().value.base_10_value == "0"
    }

    pub(super) fn get_falsy_flags_of_types(&self, types: &[Rc<Type>]) -> TypeFlags {
        let mut result = TypeFlags::None;
        for t in types {
            result |= self.get_falsy_flags(t);
        }
        result
    }

    pub(super) fn get_falsy_flags(&self, type_: &Type) -> TypeFlags {
        if type_.flags().intersects(TypeFlags::Union) {
            self.get_falsy_flags_of_types(type_.as_union_or_intersection_type_interface().types())
        } else if type_.flags().intersects(TypeFlags::StringLiteral) {
            if type_.as_string_literal_type().value == "" {
                TypeFlags::StringLiteral
            } else {
                TypeFlags::None
            }
        } else if type_.flags().intersects(TypeFlags::NumberLiteral) {
            if type_.as_number_literal_type().value == Number::new(0.0) {
                TypeFlags::NumberLiteral
            } else {
                TypeFlags::None
            }
        } else if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            if self.is_zero_big_int(type_) {
                TypeFlags::BigIntLiteral
            } else {
                TypeFlags::None
            }
        } else if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            if ptr::eq(type_, &*self.false_type()) || ptr::eq(type_, &*self.regular_false_type()) {
                TypeFlags::BooleanLiteral
            } else {
                TypeFlags::None
            }
        } else {
            TypeFlags::PossiblyFalsy
        }
    }

    pub(super) fn remove_definitely_falsy_types(&self, type_: &Type) -> Rc<Type> {
        if self
            .get_falsy_flags(type_)
            .intersects(TypeFlags::DefinitelyFalsy)
        {
            self.filter_type(type_, |t: &Type| {
                !self
                    .get_falsy_flags(t)
                    .intersects(TypeFlags::DefinitelyFalsy)
            })
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn extract_definitely_falsy_types(&self, type_: &Type) -> Rc<Type> {
        self.map_type(
            type_,
            &mut |type_: &Type| Some(self.get_definitely_falsy_part_of_type(type_)),
            None,
        )
        .unwrap()
    }

    pub(super) fn get_definitely_falsy_part_of_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::String) {
            self.empty_string_type()
        } else if type_.flags().intersects(TypeFlags::Number) {
            self.zero_type()
        } else if type_.flags().intersects(TypeFlags::BigInt) {
            self.zero_big_int_type()
        } else if ptr::eq(type_, &*self.regular_false_type())
            || ptr::eq(type_, &*self.false_type())
            || type_.flags().intersects(
                TypeFlags::Void | TypeFlags::Undefined | TypeFlags::Null | TypeFlags::AnyOrUnknown,
            )
            || type_.flags().intersects(TypeFlags::StringLiteral)
                && type_.as_string_literal_type().value == ""
            || type_.flags().intersects(TypeFlags::BigIntLiteral) && self.is_zero_big_int(type_)
        {
            type_.type_wrapper()
        } else {
            self.never_type()
        }
    }

    pub(super) fn get_nullable_type(&self, type_: &Type, flags: TypeFlags) -> Rc<Type> {
        let missing = (flags & !type_.flags()) & (TypeFlags::Undefined | TypeFlags::Null);
        if missing == TypeFlags::None {
            type_.type_wrapper()
        } else if missing == TypeFlags::Undefined {
            self.get_union_type(
                vec![type_.type_wrapper(), self.undefined_type()],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )
        } else if missing == TypeFlags::Null {
            self.get_union_type(
                vec![type_.type_wrapper(), self.null_type()],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )
        } else {
            self.get_union_type(
                vec![
                    type_.type_wrapper(),
                    self.undefined_type(),
                    self.null_type(),
                ],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )
        }
    }

    pub(super) fn get_optional_type_(&self, type_: &Type, is_property: Option<bool>) -> Rc<Type> {
        let is_property = is_property.unwrap_or(false);
        Debug_.assert(self.strict_null_checks, None);
        if type_.flags().intersects(TypeFlags::Undefined) {
            type_.type_wrapper()
        } else {
            self.get_union_type(
                vec![
                    type_.type_wrapper(),
                    if is_property {
                        self.missing_type()
                    } else {
                        self.undefined_type()
                    },
                ],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )
        }
    }

    pub(super) fn get_global_non_nullable_type_instantiation(&self, type_: &Type) -> Rc<Type> {
        let reduced_type = self.get_type_with_facts(type_, TypeFacts::NEUndefinedOrNull);
        if self
            .maybe_deferred_global_non_nullable_type_alias()
            .is_none()
        {
            *self.maybe_deferred_global_non_nullable_type_alias() = Some(
                self.get_global_symbol(
                    &__String::new("NonNullable".to_owned()),
                    SymbolFlags::TypeAlias,
                    None,
                )
                .unwrap_or_else(|| self.unknown_symbol()),
            );
        }
        if let Some(deferred_global_non_nullable_type_alias) = self
            .maybe_deferred_global_non_nullable_type_alias()
            .clone()
            .as_ref()
            .filter(|deferred_global_non_nullable_type_alias| {
                !Rc::ptr_eq(
                    deferred_global_non_nullable_type_alias,
                    &self.unknown_symbol(),
                )
            })
        {
            self.get_type_alias_instantiation(
                deferred_global_non_nullable_type_alias,
                Some(&vec![reduced_type]),
                Option::<&Symbol>::None,
                None,
            )
        } else {
            reduced_type
        }
    }

    pub(super) fn get_non_nullable_type(&self, type_: &Type) -> Rc<Type> {
        if self.strict_null_checks {
            self.get_global_non_nullable_type_instantiation(type_)
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn add_optional_type_marker(&self, type_: &Type) -> Rc<Type> {
        if self.strict_null_checks {
            self.get_union_type(
                vec![type_.type_wrapper(), self.optional_type()],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn remove_optional_type_marker(&self, type_: &Type) -> Rc<Type> {
        if self.strict_null_checks {
            self.remove_type(type_, &self.optional_type())
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn propagate_optional_type_marker(
        &self,
        type_: &Type,
        node: &Node, /*OptionalChain*/
        was_optional: bool,
    ) -> Rc<Type> {
        if was_optional {
            if is_outermost_optional_chain(node) {
                self.get_optional_type_(type_, None)
            } else {
                self.add_optional_type_marker(type_)
            }
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_optional_expression_type(
        &self,
        expr_type: &Type,
        expression: &Node, /*Expression*/
    ) -> Rc<Type> {
        if is_expression_of_optional_chain_root(expression) {
            self.get_non_nullable_type(expr_type)
        } else if is_optional_chain(expression) {
            self.remove_optional_type_marker(expr_type)
        } else {
            expr_type.type_wrapper()
        }
    }

    pub(super) fn remove_missing_type(&self, type_: &Type, is_optional: bool) -> Rc<Type> {
        if self.exact_optional_property_types == Some(true) && is_optional {
            self.remove_type(type_, &self.missing_type())
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn contains_missing_type(&self, type_: &Type) -> bool {
        self.exact_optional_property_types == Some(true)
            && (ptr::eq(type_, &*self.missing_type())
                || type_.flags().intersects(TypeFlags::Union)
                    && self.contains_type(
                        type_.as_union_or_intersection_type_interface().types(),
                        &self.missing_type(),
                    ))
    }

    pub(super) fn remove_missing_or_undefined_type(&self, type_: &Type) -> Rc<Type> {
        if self.exact_optional_property_types == Some(true) {
            self.remove_type(type_, &self.missing_type())
        } else {
            self.get_type_with_facts(type_, TypeFacts::NEUndefined)
        }
    }

    pub(super) fn is_coercible_under_double_equals(&self, source: &Type, target: &Type) -> bool {
        source
            .flags()
            .intersects(TypeFlags::Number | TypeFlags::String | TypeFlags::BooleanLiteral)
            && target
                .flags()
                .intersects(TypeFlags::Number | TypeFlags::String | TypeFlags::Boolean)
    }

    pub(super) fn is_object_type_with_inferable_index(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Intersection) {
            every(
                type_.as_union_or_intersection_type_interface().types(),
                |type_: &Rc<Type>, _| self.is_object_type_with_inferable_index(type_),
            )
        } else {
            matches!(
                type_.maybe_symbol().as_ref(),
                Some(type_symbol) if type_symbol.flags().intersects(SymbolFlags::ObjectLiteral | SymbolFlags::TypeLiteral | SymbolFlags::Enum | SymbolFlags::ValueModule)
            ) && !self.type_has_call_or_construct_signatures(type_)
                || get_object_flags(type_).intersects(ObjectFlags::ReverseMapped)
                    && self
                        .is_object_type_with_inferable_index(&type_.as_reverse_mapped_type().source)
        }
    }

    pub(super) fn create_symbol_with_type<TType: Borrow<Type>>(
        &self,
        source: &Symbol,
        type_: Option<TType>,
    ) -> Rc<Symbol> {
        let symbol: Rc<Symbol> = self
            .create_symbol(
                source.flags(),
                source.escaped_name().clone(),
                Some(get_check_flags(source) & CheckFlags::Readonly),
            )
            .into();
        *symbol.maybe_declarations_mut() = source.maybe_declarations().clone();
        symbol.set_parent(source.maybe_parent());
        let symbol_links = symbol.as_transient_symbol().symbol_links();
        let mut symbol_links = symbol_links.borrow_mut();
        symbol_links.type_ = type_.map(|type_| type_.borrow().type_wrapper());
        symbol_links.target = Some(source.symbol_wrapper());
        if let Some(source_value_declaration) = source.maybe_value_declaration() {
            symbol.set_value_declaration(source_value_declaration);
        }
        let name_type = (*self.get_symbol_links(source)).borrow().name_type.clone();
        if let Some(name_type) = name_type {
            symbol_links.name_type = Some(name_type);
        }
        symbol
    }

    pub(super) fn transform_type_of_members<TCallback: FnMut(&Type) -> Rc<Type>>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> SymbolTable {
        let mut members = create_symbol_table(None);
        for property in &self.get_properties_of_object_type(type_) {
            let original = self.get_type_of_symbol(property);
            let updated = f(&original);
            members.insert(
                property.escaped_name().clone(),
                if Rc::ptr_eq(&updated, &original) {
                    property.clone()
                } else {
                    self.create_symbol_with_type(property, Some(updated))
                },
            );
        }
        members
    }
}
