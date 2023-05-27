use gc::Gc;
use peekmore::PeekMore;
use std::ptr;
use std::{borrow::Borrow, io};

use super::{IterationTypeKind, TypeFacts};
use crate::try_every;
use crate::{
    are_option_gcs_equal, compiler::utilities_public::is_expression_of_optional_chain_root,
    create_symbol_table, every, find, get_check_flags, get_object_flags, is_optional_chain,
    is_outermost_optional_chain, last, length, some, CheckFlags,
    Debug_, ElementFlags, InterfaceTypeInterface, Node, NodeInterface, Number, ObjectFlags,
    ObjectFlagsTypeInterface, PeekMoreExt, PeekableExt, Signature, Symbol, SymbolFlags,
    SymbolInterface, SymbolTable, SyntaxKind, Ternary, TransientSymbolInterface, Type, TypeChecker,
    TypeFlags, TypeInterface, TypePredicate, UnionReduction,
};
use crate::{try_reduce_left_no_initial_value, OptionTry};

impl TypeChecker {
    pub(super) fn compare_signatures_identical(
        &self,
        mut source: Gc<Signature>,
        target: Gc<Signature>,
        partial_match: bool,
        ignore_this_types: bool,
        ignore_return_types: bool,
        mut compare_types: impl FnMut(&Type, &Type) -> io::Result<Ternary>,
    ) -> io::Result<Ternary> {
        if Gc::ptr_eq(&source, &target) {
            return Ok(Ternary::True);
        }
        if !self.is_matching_signature(&source, &target, partial_match)? {
            return Ok(Ternary::False);
        }
        if length(source.maybe_type_parameters().as_deref())
            != length(target.maybe_type_parameters().as_deref())
        {
            return Ok(Ternary::False);
        }
        if let Some(ref target_type_parameters) = target.maybe_type_parameters().clone() {
            let source_type_parameters = source.maybe_type_parameters().clone().unwrap();
            let mapper = Gc::new(self.create_type_mapper(
                source_type_parameters.clone(),
                Some(target_type_parameters.clone()),
            ));
            for (i, t) in target_type_parameters.iter().enumerate() {
                let s = &source_type_parameters[i];
                if !(Gc::ptr_eq(s, t)
                    || compare_types(
                        &self
                            .maybe_instantiate_type(
                                self.get_constraint_from_type_parameter(s)?,
                                Some(mapper.clone()),
                            )?
                            .unwrap_or_else(|| self.unknown_type()),
                        &self
                            .get_constraint_from_type_parameter(t)?
                            .unwrap_or_else(|| self.unknown_type()),
                    )? != Ternary::False
                        && compare_types(
                            &self
                                .maybe_instantiate_type(
                                    self.get_default_from_type_parameter_(s)?,
                                    Some(mapper.clone()),
                                )?
                                .unwrap_or_else(|| self.unknown_type()),
                            &self
                                .get_default_from_type_parameter_(t)?
                                .unwrap_or_else(|| self.unknown_type()),
                        )? != Ternary::False)
                {
                    return Ok(Ternary::False);
                }
            }
            source = Gc::new(self.instantiate_signature(source, mapper, Some(true))?);
        }
        let mut result = Ternary::True;
        if !ignore_this_types {
            let source_this_type = self.get_this_type_of_signature(&source)?;
            if let Some(source_this_type) = source_this_type {
                let target_this_type = self.get_this_type_of_signature(&target)?;
                if let Some(target_this_type) = target_this_type {
                    let related = compare_types(&source_this_type, &target_this_type)?;
                    if related == Ternary::False {
                        return Ok(Ternary::False);
                    }
                    result &= related;
                }
            }
        }
        let target_len = self.get_parameter_count(&target)?;
        for i in 0..target_len {
            let s = self.get_type_at_position(&source, i)?;
            let t = self.get_type_at_position(&target, i)?;
            let related = compare_types(&t, &s)?;
            if related == Ternary::False {
                return Ok(Ternary::False);
            }
            result &= related;
        }
        if !ignore_return_types {
            let source_type_predicate = self.get_type_predicate_of_signature(&source)?;
            let target_type_predicate = self.get_type_predicate_of_signature(&target)?;
            result &= if source_type_predicate.is_some() || target_type_predicate.is_some() {
                self.try_compare_type_predicates_identical(
                    source_type_predicate,
                    target_type_predicate,
                    |s, t| compare_types(s, t),
                )?
            } else {
                compare_types(
                    &*self.get_return_type_of_signature(source)?,
                    &*self.get_return_type_of_signature(target)?,
                )?
            };
        }
        Ok(result)
    }

    #[allow(dead_code)]
    pub(super) fn compare_type_predicates_identical(
        &self,
        source: Option<impl Borrow<TypePredicate>>,
        target: Option<impl Borrow<TypePredicate>>,
        compare_types: impl FnOnce(&Type, &Type) -> Ternary,
    ) -> Ternary {
        self.try_compare_type_predicates_identical(source, target, |a: &Type, b: &Type| {
            Ok(compare_types(a, b))
        })
        .unwrap()
    }

    pub(super) fn try_compare_type_predicates_identical(
        &self,
        source: Option<impl Borrow<TypePredicate>>,
        target: Option<impl Borrow<TypePredicate>>,
        compare_types: impl FnOnce(&Type, &Type) -> io::Result<Ternary>,
    ) -> io::Result<Ternary> {
        Ok(match (source, target) {
            (Some(source), Some(target)) => {
                let source = source.borrow();
                let target = target.borrow();
                if !self.type_predicate_kinds_match(source, target) {
                    Ternary::False
                } else if are_option_gcs_equal(source.type_.as_ref(), target.type_.as_ref()) {
                    Ternary::True
                } else if let (Some(source_type), Some(target_type)) =
                    (source.type_.as_ref(), target.type_.as_ref())
                {
                    compare_types(source_type, target_type)?
                } else {
                    Ternary::False
                }
            }
            _ => Ternary::False,
        })
    }

    pub(super) fn literal_types_with_same_base_type<'types>(
        &self,
        types: impl IntoIterator<Item = &'types Gc<Type>>,
    ) -> io::Result<bool> {
        let mut common_base_type: Option<Gc<Type>> = None;
        for t in types {
            let base_type = self.get_base_type_of_literal_type(t)?;
            if common_base_type.is_none() {
                common_base_type = Some(base_type.clone());
            }
            if Gc::ptr_eq(&base_type, t)
                || !Gc::ptr_eq(&base_type, common_base_type.as_ref().unwrap())
            {
                return Ok(false);
            }
        }
        Ok(true)
    }

    pub(super) fn get_supertype_or_union<'types, TTypes>(
        &self,
        types: TTypes,
    ) -> io::Result<Gc<Type>>
    where
        TTypes: IntoIterator<Item = &'types Gc<Type>>,
        TTypes::IntoIter: Clone,
    {
        let mut types = types.into_iter();
        let mut types_peekmore = types.clone().peekmore();
        if types_peekmore.is_len_equal_to(1) {
            return Ok(types.next().unwrap().clone());
        }
        Ok(if self.literal_types_with_same_base_type(types.clone())? {
            self.get_union_type(
                types,
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?
        } else {
            try_reduce_left_no_initial_value(
                &types.cloned().collect::<Vec<_>>(),
                |s: Gc<Type>, t: &Gc<Type>, _| -> io::Result<_> {
                    Ok(if self.is_type_subtype_of(&s, t)? {
                        t.clone()
                    } else {
                        s
                    })
                },
                None,
                None,
            )?
        })
    }

    pub(super) fn get_common_supertype<'types, TTypes>(&self, types: TTypes) -> io::Result<Gc<Type>>
    where
        TTypes: IntoIterator<Item = &'types Gc<Type>>,
        TTypes::IntoIter: Clone,
    {
        let types = types.into_iter();
        if !self.strict_null_checks {
            return self.get_supertype_or_union(types);
        }
        let mut primary_types = types
            .clone()
            .filter(|t| !t.flags().intersects(TypeFlags::Nullable))
            .cloned()
            .peekable();
        Ok(if !primary_types.is_empty_() {
            self.get_nullable_type(
                &*self.get_supertype_or_union(&primary_types.collect::<Vec<_>>())?,
                self.get_falsy_flags_of_types(types) & TypeFlags::Nullable,
            )?
        } else {
            self.get_union_type(
                types,
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?
        })
    }

    pub(super) fn get_common_subtype(&self, types: &[Gc<Type>]) -> io::Result<Gc<Type>> {
        try_reduce_left_no_initial_value(
            types,
            |s: Gc<Type>, t: &Gc<Type>, _| -> io::Result<_> {
                Ok(if self.is_type_subtype_of(t, &s)? {
                    t.clone()
                } else {
                    s
                })
            },
            None,
            None,
        )
    }

    pub(super) fn is_array_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::Reference)
            && (Gc::ptr_eq(
                &type_.as_type_reference_interface().target(),
                &self.global_array_type(),
            ) || Gc::ptr_eq(
                &type_.as_type_reference_interface().target(),
                &self.global_readonly_array_type(),
            ))
    }

    pub(super) fn is_readonly_array_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::Reference)
            && Gc::ptr_eq(
                &type_.as_type_reference_interface().target(),
                &self.global_readonly_array_type(),
            )
    }

    pub(super) fn is_mutable_array_or_tuple(&self, type_: &Type) -> bool {
        self.is_array_type(type_) && !self.is_readonly_array_type(type_)
            || self.is_tuple_type(type_)
                && !type_
                    .as_type_reference_interface()
                    .target()
                    .as_tuple_type()
                    .readonly
    }

    pub(super) fn get_element_type_of_array_type(
        &self,
        type_: &Type,
    ) -> io::Result<Option<Gc<Type>>> {
        Ok(if self.is_array_type(type_) {
            self.get_type_arguments(type_)?.get(0).cloned()
        } else {
            None
        })
    }

    pub(super) fn is_array_like_type(&self, type_: &Type) -> io::Result<bool> {
        Ok(self.is_array_type(type_)
            || !type_.flags().intersects(TypeFlags::Nullable)
                && self.is_type_assignable_to(type_, &self.any_readonly_array_type())?)
    }

    pub(super) fn get_single_base_for_non_augmenting_subtype(
        &self,
        type_: &Type,
    ) -> io::Result<Option<Gc<Type>>> {
        if !get_object_flags(type_).intersects(ObjectFlags::Reference)
            || !get_object_flags(&type_.as_type_reference_interface().target())
                .intersects(ObjectFlags::ClassOrInterface)
        {
            return Ok(None);
        }
        let type_as_type_reference = type_.as_type_reference_interface();
        if get_object_flags(type_).intersects(ObjectFlags::IdenticalBaseTypeCalculated) {
            return Ok(
                if get_object_flags(type_).intersects(ObjectFlags::IdenticalBaseTypeExists) {
                    type_as_type_reference
                        .maybe_cached_equivalent_base_type()
                        .clone()
                } else {
                    None
                },
            );
        }
        type_as_type_reference.set_object_flags(
            type_as_type_reference.object_flags() | ObjectFlags::IdenticalBaseTypeCalculated,
        );
        let ref target = type_as_type_reference.target();
        if get_object_flags(target).intersects(ObjectFlags::Class) {
            let base_type_node = self.get_base_type_node_of_class(target);
            if matches!(
                base_type_node.as_ref(),
                Some(base_type_node) if !matches!(
                    base_type_node.as_expression_with_type_arguments().expression.kind(),
                    SyntaxKind::Identifier | SyntaxKind::PropertyAccessExpression
                )
            ) {
                return Ok(None);
            }
        }
        let bases = self.get_base_types(target)?;
        if bases.len() != 1 {
            return Ok(None);
        }
        if !(*self.get_members_of_symbol(&type_.symbol())?)
            .borrow()
            .is_empty()
        {
            return Ok(None);
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
                Some(Gc::new(self.create_type_mapper(
                    target_type_parameters,
                    Some(self.get_type_arguments(type_)?[0..target_type_parameters_len].to_owned()),
                ))),
            )?
        };
        if length(Some(&self.get_type_arguments(type_)?))
            > length(target_as_interface_type.maybe_type_parameters())
        {
            instantiated_base = self.get_type_with_this_argument(
                &instantiated_base,
                Some(&**last(&self.get_type_arguments(type_)?)),
                None,
            )?;
        }
        type_as_type_reference.set_object_flags(
            type_as_type_reference.object_flags() | ObjectFlags::IdenticalBaseTypeExists,
        );
        *type_as_type_reference.maybe_cached_equivalent_base_type() =
            Some(instantiated_base.clone());
        Ok(Some(instantiated_base))
    }

    pub(super) fn is_empty_literal_type(&self, type_: &Type) -> bool {
        if self.strict_null_checks {
            ptr::eq(type_, &*self.implicit_never_type())
        } else {
            ptr::eq(type_, &*self.undefined_widening_type())
        }
    }

    pub(super) fn is_empty_array_literal_type(&self, type_: &Type) -> io::Result<bool> {
        let element_type = self.get_element_type_of_array_type(type_)?;
        Ok(matches!(
            element_type.as_ref(),
            Some(element_type) if self.is_empty_literal_type(element_type)
        ))
    }

    pub(super) fn is_tuple_like_type(&self, type_: &Type) -> io::Result<bool> {
        Ok(self.is_tuple_type(type_) || self.get_property_of_type_(type_, "0", None)?.is_some())
    }

    pub(super) fn is_array_or_tuple_like_type(&self, type_: &Type) -> io::Result<bool> {
        Ok(self.is_array_like_type(type_)? || self.is_tuple_like_type(type_)?)
    }

    pub(super) fn get_tuple_element_type(
        &self,
        type_: &Type,
        index: usize,
    ) -> io::Result<Option<Gc<Type>>> {
        let prop_type = self.get_type_of_property_of_type_(type_, &index.to_string())?;
        if prop_type.is_some() {
            return Ok(prop_type);
        }
        if self.every_type(type_, |type_: &Type| self.is_tuple_type(type_)) {
            return self.try_map_type(
                type_,
                &mut |t: &Type| {
                    Ok(Some(
                        self.get_rest_type_of_tuple_type(t)?
                            .unwrap_or_else(|| self.undefined_type()),
                    ))
                },
                None,
            );
        }
        Ok(None)
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
                Some(|type_: &Gc<Type>| self.is_unit_type(type_)),
            )
        } else {
            type_.flags().intersects(TypeFlags::Unit)
        }
    }

    pub(super) fn extract_unit_type(&self, type_: &Type) -> Gc<Type> {
        if type_.flags().intersects(TypeFlags::Intersection) {
            find(
                type_.as_union_or_intersection_type_interface().types(),
                |type_: &Gc<Type>, _| self.is_unit_type(type_),
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

    pub(super) fn get_base_type_of_literal_type(&self, type_: &Type) -> io::Result<Gc<Type>> {
        Ok(if type_.flags().intersects(TypeFlags::EnumLiteral) {
            self.get_base_type_of_enum_literal_type(type_)?
        } else if type_.flags().intersects(TypeFlags::StringLiteral) {
            self.string_type()
        } else if type_.flags().intersects(TypeFlags::NumberLiteral) {
            self.number_type()
        } else if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            self.bigint_type()
        } else if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            self.boolean_type()
        } else if type_.flags().intersects(TypeFlags::Union) {
            self.try_map_type(
                type_,
                &mut |type_| Ok(Some(self.get_base_type_of_literal_type(type_)?)),
                None,
            )?
            .unwrap()
        } else {
            type_.type_wrapper()
        })
    }

    pub(super) fn get_widened_literal_type(&self, type_: &Type) -> io::Result<Gc<Type>> {
        let flags = type_.flags();
        Ok(
            if flags.intersects(TypeFlags::EnumLiteral) && self.is_fresh_literal_type(type_) {
                self.get_base_type_of_enum_literal_type(type_)?
            } else if flags.intersects(TypeFlags::StringLiteral)
                && self.is_fresh_literal_type(type_)
            {
                self.string_type()
            } else if flags.intersects(TypeFlags::NumberLiteral)
                && self.is_fresh_literal_type(type_)
            {
                self.number_type()
            } else if flags.intersects(TypeFlags::BigIntLiteral)
                && self.is_fresh_literal_type(type_)
            {
                self.bigint_type()
            } else if flags.intersects(TypeFlags::BooleanLiteral)
                && self.is_fresh_literal_type(type_)
            {
                self.boolean_type()
            } else if flags.intersects(TypeFlags::Union) {
                self.try_map_type(
                    type_,
                    &mut |type_| Ok(Some(self.get_widened_literal_type(type_)?)),
                    None,
                )?
                .unwrap()
            } else {
                type_.type_wrapper()
            },
        )
    }

    pub(super) fn get_widened_unique_es_symbol_type(&self, type_: &Type) -> Gc<Type> {
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

    pub(super) fn get_widened_literal_like_type_for_contextual_type(
        &self,
        type_: &Type,
        contextual_type: Option<impl Borrow<Type>>,
    ) -> io::Result<Gc<Type>> {
        let mut type_ = type_.type_wrapper();
        if !self.is_literal_of_contextual_type(&type_, contextual_type)? {
            type_ =
                self.get_widened_unique_es_symbol_type(&*self.get_widened_literal_type(&type_)?);
        }
        Ok(type_)
    }

    pub(super) fn get_widened_literal_like_type_for_contextual_return_type_if_needed(
        &self,
        type_: Option<impl Borrow<Type>>,
        contextual_signature_return_type: Option<impl Borrow<Type>>,
        is_async: bool,
    ) -> io::Result<Option<Gc<Type>>> {
        let mut type_ = type_.map(|type_| type_.borrow().type_wrapper());
        if let Some(type_present) = type_.as_ref().filter(|type_| self.is_unit_type(type_)) {
            let contextual_type = contextual_signature_return_type.try_and_then(
                |contextual_signature_return_type| -> io::Result<_> {
                    let contextual_signature_return_type =
                        contextual_signature_return_type.borrow();
                    Ok(if is_async {
                        self.get_promised_type_of_promise(
                            contextual_signature_return_type,
                            Option::<&Node>::None,
                        )?
                    } else {
                        Some(contextual_signature_return_type.type_wrapper())
                    })
                },
            )?;
            type_ = Some(self.get_widened_literal_like_type_for_contextual_type(
                type_present,
                contextual_type,
            )?);
        }
        Ok(type_)
    }

    pub(super) fn get_widened_literal_like_type_for_contextual_iteration_type_if_needed(
        &self,
        type_: Option<impl Borrow<Type>>,
        contextual_signature_return_type: Option<impl Borrow<Type>>,
        kind: IterationTypeKind,
        is_async_generator: bool,
    ) -> io::Result<Option<Gc<Type>>> {
        let mut type_ = type_.map(|type_| type_.borrow().type_wrapper());
        if let Some(type_present) = type_.as_ref().filter(|type_| self.is_unit_type(type_)) {
            let contextual_type = contextual_signature_return_type.try_and_then(
                |contextual_signature_return_type| {
                    let contextual_signature_return_type =
                        contextual_signature_return_type.borrow();
                    self.get_iteration_type_of_generator_function_return_type(
                        kind,
                        contextual_signature_return_type,
                        is_async_generator,
                    )
                },
            )?;
            type_ = Some(self.get_widened_literal_like_type_for_contextual_type(
                type_present,
                contextual_type,
            )?);
        }
        Ok(type_)
    }

    pub(super) fn is_tuple_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::Reference)
            && type_
                .as_type_reference_interface()
                .target()
                .as_object_type()
                .object_flags()
                .intersects(ObjectFlags::Tuple)
    }

    pub(super) fn is_generic_tuple_type(&self, type_: &Type) -> bool {
        self.is_tuple_type(type_)
            && type_
                .as_type_reference_interface()
                .target()
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
    ) -> io::Result<Option<Gc<Type>>> {
        self.get_element_type_of_slice_of_tuple_type(
            type_,
            type_
                .as_type_reference_interface()
                .target()
                .as_tuple_type()
                .fixed_length,
            None,
            None,
        )
    }

    pub(super) fn get_rest_array_type_of_tuple_type(
        &self,
        type_: &Type, /*TupleTypeReference*/
    ) -> io::Result<Option<Gc<Type>>> {
        let rest_type = self.get_rest_type_of_tuple_type(type_)?;
        Ok(rest_type
            .as_ref()
            .map(|rest_type| self.create_array_type(rest_type, None)))
    }

    pub(super) fn get_element_type_of_slice_of_tuple_type(
        &self,
        type_: &Type, /*TupleTypeReference*/
        index: usize,
        end_skip_count: Option<usize>,
        writing: Option<bool>,
    ) -> io::Result<Option<Gc<Type>>> {
        let end_skip_count = end_skip_count.unwrap_or(0);
        let writing = writing.unwrap_or(false);
        let length = self.get_type_reference_arity(type_) - end_skip_count;
        if index < length {
            let type_arguments = self.get_type_arguments(type_)?;
            let mut element_types: Vec<Gc<Type>> = vec![];
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
                        )?
                    } else {
                        t.clone()
                    },
                );
            }
            return Ok(Some(if writing {
                self.get_intersection_type(&element_types, Option::<&Symbol>::None, None)?
            } else {
                self.get_union_type(
                    &element_types,
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                )?
            }));
        }
        Ok(None)
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

    pub(super) fn get_falsy_flags_of_types<'types>(
        &self,
        types: impl IntoIterator<Item = &'types Gc<Type>>,
    ) -> TypeFlags {
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
            type_.flags() & TypeFlags::PossiblyFalsy
        }
    }

    pub(super) fn remove_definitely_falsy_types(&self, type_: &Type) -> Gc<Type> {
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

    pub(super) fn extract_definitely_falsy_types(&self, type_: &Type) -> Gc<Type> {
        self.map_type(
            type_,
            &mut |type_: &Type| Some(self.get_definitely_falsy_part_of_type(type_)),
            None,
        )
        .unwrap()
    }

    pub(super) fn get_definitely_falsy_part_of_type(&self, type_: &Type) -> Gc<Type> {
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

    pub(super) fn get_nullable_type(&self, type_: &Type, flags: TypeFlags) -> io::Result<Gc<Type>> {
        let missing = (flags & !type_.flags()) & (TypeFlags::Undefined | TypeFlags::Null);
        Ok(if missing == TypeFlags::None {
            type_.type_wrapper()
        } else if missing == TypeFlags::Undefined {
            self.get_union_type(
                &[type_.type_wrapper(), self.undefined_type()],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?
        } else if missing == TypeFlags::Null {
            self.get_union_type(
                &[type_.type_wrapper(), self.null_type()],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?
        } else {
            self.get_union_type(
                &[
                    type_.type_wrapper(),
                    self.undefined_type(),
                    self.null_type(),
                ],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?
        })
    }

    pub(super) fn get_optional_type_(
        &self,
        type_: &Type,
        is_property: Option<bool>,
    ) -> io::Result<Gc<Type>> {
        let is_property = is_property.unwrap_or(false);
        Debug_.assert(self.strict_null_checks, None);
        Ok(if type_.flags().intersects(TypeFlags::Undefined) {
            type_.type_wrapper()
        } else {
            self.get_union_type(
                &[
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
            )?
        })
    }

    pub(super) fn get_global_non_nullable_type_instantiation(
        &self,
        type_: &Type,
    ) -> io::Result<Gc<Type>> {
        let reduced_type = self.get_type_with_facts(type_, TypeFacts::NEUndefinedOrNull)?;
        if self
            .maybe_deferred_global_non_nullable_type_alias()
            .is_none()
        {
            *self.maybe_deferred_global_non_nullable_type_alias() = Some(
                self.get_global_symbol("NonNullable", SymbolFlags::TypeAlias, None)?
                    .unwrap_or_else(|| self.unknown_symbol()),
            );
        }
        Ok(
            if let Some(deferred_global_non_nullable_type_alias) = self
                .maybe_deferred_global_non_nullable_type_alias()
                .clone()
                .as_ref()
                .filter(|deferred_global_non_nullable_type_alias| {
                    !Gc::ptr_eq(
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
                )?
            } else {
                reduced_type
            },
        )
    }

    pub(super) fn get_non_nullable_type(&self, type_: &Type) -> io::Result<Gc<Type>> {
        Ok(if self.strict_null_checks {
            self.get_global_non_nullable_type_instantiation(type_)?
        } else {
            type_.type_wrapper()
        })
    }

    pub(super) fn add_optional_type_marker(&self, type_: &Type) -> io::Result<Gc<Type>> {
        Ok(if self.strict_null_checks {
            self.get_union_type(
                &[type_.type_wrapper(), self.optional_type()],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?
        } else {
            type_.type_wrapper()
        })
    }

    pub(super) fn remove_optional_type_marker(&self, type_: &Type) -> Gc<Type> {
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
    ) -> io::Result<Gc<Type>> {
        Ok(if was_optional {
            if is_outermost_optional_chain(node) {
                self.get_optional_type_(type_, None)?
            } else {
                self.add_optional_type_marker(type_)?
            }
        } else {
            type_.type_wrapper()
        })
    }

    pub(super) fn get_optional_expression_type(
        &self,
        expr_type: &Type,
        expression: &Node, /*Expression*/
    ) -> io::Result<Gc<Type>> {
        Ok(if is_expression_of_optional_chain_root(expression) {
            self.get_non_nullable_type(expr_type)?
        } else if is_optional_chain(expression) {
            self.remove_optional_type_marker(expr_type)
        } else {
            expr_type.type_wrapper()
        })
    }

    pub(super) fn remove_missing_type(&self, type_: &Type, is_optional: bool) -> Gc<Type> {
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

    pub(super) fn remove_missing_or_undefined_type(&self, type_: &Type) -> io::Result<Gc<Type>> {
        Ok(if self.exact_optional_property_types == Some(true) {
            self.remove_type(type_, &self.missing_type())
        } else {
            self.get_type_with_facts(type_, TypeFacts::NEUndefined)?
        })
    }

    pub(super) fn is_coercible_under_double_equals(&self, source: &Type, target: &Type) -> bool {
        source
            .flags()
            .intersects(TypeFlags::Number | TypeFlags::String | TypeFlags::BooleanLiteral)
            && target
                .flags()
                .intersects(TypeFlags::Number | TypeFlags::String | TypeFlags::Boolean)
    }

    pub(super) fn is_object_type_with_inferable_index(&self, type_: &Type) -> io::Result<bool> {
        Ok(if type_.flags().intersects(TypeFlags::Intersection) {
            try_every(
                type_.as_union_or_intersection_type_interface().types(),
                |type_: &Gc<Type>, _| self.is_object_type_with_inferable_index(type_),
            )?
        } else {
            matches!(
                type_.maybe_symbol().as_ref(),
                Some(type_symbol) if type_symbol.flags().intersects(SymbolFlags::ObjectLiteral | SymbolFlags::TypeLiteral | SymbolFlags::Enum | SymbolFlags::ValueModule)
            ) && !self.type_has_call_or_construct_signatures(type_)?
                || get_object_flags(type_).intersects(ObjectFlags::ReverseMapped)
                    && self.is_object_type_with_inferable_index(
                        &type_.as_reverse_mapped_type().source,
                    )?
        })
    }

    pub(super) fn create_symbol_with_type(
        &self,
        source: &Symbol,
        type_: Option<impl Borrow<Type>>,
    ) -> Gc<Symbol> {
        let symbol: Gc<Symbol> = self
            .create_symbol(
                source.flags(),
                source.escaped_name().to_owned(),
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

    pub(super) fn transform_type_of_members(
        &self,
        type_: &Type,
        mut f: impl FnMut(&Type) -> io::Result<Gc<Type>>,
    ) -> io::Result<SymbolTable> {
        let mut members = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
        for ref property in self.get_properties_of_object_type(type_)? {
            let original = self.get_type_of_symbol(property)?;
            let updated = f(&original)?;
            members.insert(
                property.escaped_name().to_owned(),
                if Gc::ptr_eq(&updated, &original) {
                    property.clone()
                } else {
                    self.create_symbol_with_type(property, Some(updated))
                },
            );
        }
        Ok(members)
    }
}
