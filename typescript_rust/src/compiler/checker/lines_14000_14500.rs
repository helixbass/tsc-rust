use std::{
    borrow::Borrow,
    convert::{TryFrom, TryInto},
    io, ptr,
};

use gc::Gc;
use id_arena::Id;
use indexmap::IndexMap;
use local_macros::enum_unwrapped;
use peekmore::PeekMore;

use crate::{
    array_of, binary_search_copy_key, compare_values, filter, find_index,
    find_last_index_returns_isize, for_each, get_object_flags, is_part_of_type_node, map,
    ordered_remove_item_at, push_if_unique_gc, reduce_left, replace_element,
    return_ok_default_if_none, some, try_map, try_some, BaseUnionOrIntersectionType, Diagnostics,
    ElementFlags, IntersectionType, IntrinsicType, IteratorExt, LiteralTypeInterface, Node,
    ObjectFlags, OptionTry, PeekMoreExt, Signature, Symbol, SymbolInterface, Type, TypeChecker,
    TypeFlags, TypeId, TypeInterface, TypePredicate, TypePredicateKind, TypeReferenceInterface,
    UnionOrIntersectionTypeInterface, UnionReduction, UnionType,
};

impl TypeChecker {
    pub(super) fn create_normalized_type_reference(
        &self,
        target: &Type, /*GenericType*/
        type_arguments: Option<Vec<Id<Type>>>,
    ) -> io::Result<Id<Type>> {
        Ok(
            if target
                .as_object_flags_type()
                .object_flags()
                .intersects(ObjectFlags::Tuple)
            {
                self.create_normalized_tuple_type(target, type_arguments.unwrap())?
            } else {
                self.create_type_reference(target, type_arguments)
            },
        )
    }

    pub(super) fn create_normalized_tuple_type(
        &self,
        target: &Type, /*TupleType*/
        element_types: Vec<Id<Type>>,
    ) -> io::Result<Id<Type>> {
        let target_as_tuple_type = target.as_tuple_type();
        if !target_as_tuple_type
            .combined_flags
            .intersects(ElementFlags::NonRequired)
        {
            return Ok(self.create_type_reference(target, Some(element_types)));
        }
        if target_as_tuple_type
            .combined_flags
            .intersects(ElementFlags::Variadic)
        {
            let union_index = find_index(
                &element_types,
                |t: &Id<Type>, i| {
                    target_as_tuple_type.element_flags[i].intersects(ElementFlags::Variadic)
                        && t.flags().intersects(TypeFlags::Never | TypeFlags::Union)
                },
                None,
            );
            if let Some(union_index) = union_index {
                return Ok(
                    if self.check_cross_product_union(&map(&element_types, |t: &Id<Type>, i| {
                        if target_as_tuple_type.element_flags[i].intersects(ElementFlags::Variadic)
                        {
                            t.clone()
                        } else {
                            self.unknown_type()
                        }
                    })) {
                        self.try_map_type(
                            &element_types[union_index],
                            &mut |t: &Type| {
                                Ok(Some(self.create_normalized_tuple_type(
                                    target,
                                    replace_element(&element_types, union_index, t.type_wrapper()),
                                )?))
                            },
                            None,
                        )?
                        .unwrap()
                    } else {
                        self.error_type()
                    },
                );
            }
        }
        let mut expanded_types: Vec<Id<Type>> = vec![];
        let mut expanded_flags: Vec<ElementFlags> = vec![];
        let mut expanded_declarations: Option<
            Vec<Gc<Node /*NamedTupleMember | ParameterDeclaration*/>>,
        > = Some(vec![]);
        let mut last_required_index: isize = -1;
        let mut first_rest_index: isize = -1;
        let mut last_optional_or_rest_index: isize = -1;
        for (i, type_) in element_types.iter().enumerate() {
            let flags = target_as_tuple_type.element_flags[i];
            if flags.intersects(ElementFlags::Variadic) {
                if type_
                    .flags()
                    .intersects(TypeFlags::InstantiableNonPrimitive)
                    || self.is_generic_mapped_type(type_)?
                {
                    self.add_element(
                        &mut last_required_index,
                        &mut expanded_flags,
                        &mut first_rest_index,
                        &mut last_optional_or_rest_index,
                        &mut expanded_types,
                        &mut expanded_declarations,
                        type_,
                        ElementFlags::Variadic,
                        target_as_tuple_type
                            .labeled_element_declarations
                            .as_ref()
                            .map(|labeled_element_declarations| {
                                labeled_element_declarations[i].clone()
                            }),
                    );
                } else if self.is_tuple_type(type_) {
                    let elements = self.get_type_arguments(type_)?;
                    if elements.len() + expanded_types.len() >= 10_000 {
                        self.error(
                            self.maybe_current_node(),
                            if is_part_of_type_node(&self.maybe_current_node().unwrap()) {
                                &Diagnostics::Type_produces_a_tuple_type_that_is_too_large_to_represent
                            } else {
                                &Diagnostics::Expression_produces_a_tuple_type_that_is_too_large_to_represent
                            },
                            None
                        );
                        return Ok(self.error_type());
                    }
                    for_each(&elements, |t: &Id<Type>, n| {
                        self.add_element(
                            &mut last_required_index,
                            &mut expanded_flags,
                            &mut first_rest_index,
                            &mut last_optional_or_rest_index,
                            &mut expanded_types,
                            &mut expanded_declarations,
                            t,
                            type_
                                .as_type_reference()
                                .target()
                                .as_tuple_type()
                                .element_flags[n],
                            type_
                                .as_type_reference()
                                .target()
                                .as_tuple_type()
                                .labeled_element_declarations
                                .as_ref()
                                .map(|labeled_element_declarations| {
                                    labeled_element_declarations[n].clone()
                                }),
                        );
                        Option::<()>::None
                    });
                } else {
                    self.add_element(
                        &mut last_required_index,
                        &mut expanded_flags,
                        &mut first_rest_index,
                        &mut last_optional_or_rest_index,
                        &mut expanded_types,
                        &mut expanded_declarations,
                        &*if self.is_array_like_type(type_)? {
                            self.get_index_type_of_type_(type_, &self.number_type())?
                                .unwrap_or_else(|| self.error_type())
                        } else {
                            self.error_type()
                        },
                        ElementFlags::Rest,
                        target_as_tuple_type
                            .labeled_element_declarations
                            .as_ref()
                            .map(|labeled_element_declarations| {
                                labeled_element_declarations[i].clone()
                            }),
                    );
                }
            } else {
                self.add_element(
                    &mut last_required_index,
                    &mut expanded_flags,
                    &mut first_rest_index,
                    &mut last_optional_or_rest_index,
                    &mut expanded_types,
                    &mut expanded_declarations,
                    type_,
                    flags,
                    target_as_tuple_type
                        .labeled_element_declarations
                        .as_ref()
                        .map(|labeled_element_declarations| {
                            labeled_element_declarations[i].clone()
                        }),
                );
            }
        }
        if last_required_index >= 0 {
            let last_required_index: usize = last_required_index.try_into().unwrap();
            for i in 0..last_required_index {
                if expanded_flags[i].intersects(ElementFlags::Optional) {
                    expanded_flags[i] = ElementFlags::Required;
                }
            }
        }
        if first_rest_index >= 0 && first_rest_index < last_optional_or_rest_index {
            let first_rest_index: usize = first_rest_index.try_into().unwrap();
            let last_optional_or_rest_index: usize =
                last_optional_or_rest_index.try_into().unwrap();
            expanded_types[first_rest_index] = self.get_union_type(
                &try_map(
                    &expanded_types[first_rest_index..last_optional_or_rest_index + 1],
                    |t: &Id<Type>, i| -> io::Result<_> {
                        Ok(
                            if expanded_flags[first_rest_index + i]
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
                        )
                    },
                )?,
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?;
            expanded_types.splice(first_rest_index + 1..last_optional_or_rest_index + 1, []);
            expanded_flags.splice(first_rest_index + 1..last_optional_or_rest_index + 1, []);
            if let Some(expanded_declarations) = expanded_declarations.as_mut() {
                expanded_declarations
                    .splice(first_rest_index + 1..last_optional_or_rest_index + 1, []);
            }
        }
        let tuple_target = self.get_tuple_target_type(
            &expanded_flags,
            target_as_tuple_type.readonly,
            expanded_declarations.as_deref(),
        )?;
        Ok(if Gc::ptr_eq(&tuple_target, &self.empty_generic_type()) {
            self.empty_object_type()
        } else if !expanded_flags.is_empty() {
            self.create_type_reference(&tuple_target, Some(expanded_types))
        } else {
            tuple_target
        })
    }

    pub(super) fn add_element(
        &self,
        last_required_index: &mut isize,
        expanded_flags: &mut Vec<ElementFlags>,
        first_rest_index: &mut isize,
        last_optional_or_rest_index: &mut isize,
        expanded_types: &mut Vec<Id<Type>>,
        expanded_declarations: &mut Option<Vec<Gc<Node>>>,
        type_: &Type,
        flags: ElementFlags,
        declaration: Option<Gc<Node /*NamedTupleMember | ParameterDeclaration*/>>,
    ) {
        if flags.intersects(ElementFlags::Required) {
            *last_required_index = expanded_flags.len().try_into().unwrap();
        }
        if flags.intersects(ElementFlags::Rest) && *first_rest_index < 0 {
            *first_rest_index = expanded_flags.len().try_into().unwrap();
        }
        if flags.intersects(ElementFlags::Optional | ElementFlags::Rest) {
            *last_optional_or_rest_index = expanded_flags.len().try_into().unwrap();
        }
        expanded_types.push(type_.type_wrapper());
        expanded_flags.push(flags);
        if expanded_declarations.is_some() && declaration.is_some() {
            expanded_declarations
                .as_mut()
                .unwrap()
                .push(declaration.unwrap());
        } else {
            *expanded_declarations = None;
        }
    }

    pub(super) fn slice_tuple_type(
        &self,
        type_: &Type, /*TupleTypeReference*/
        index: usize,
        end_skip_count: Option<usize>,
    ) -> io::Result<Id<Type>> {
        let end_skip_count = end_skip_count.unwrap_or(0);
        let target = type_.as_type_reference().target();
        let end_index = self.get_type_reference_arity(type_) - end_skip_count;
        let target_as_tuple_type = target.as_tuple_type();
        Ok(if index > target_as_tuple_type.fixed_length {
            self.get_rest_array_type_of_tuple_type(type_)?
                .try_unwrap_or_else(|| self.create_tuple_type(&[], None, None, None))?
        } else {
            self.create_tuple_type(
                &self.get_type_arguments(type_)?[index..end_index],
                Some(&target_as_tuple_type.element_flags[index..end_index]),
                Some(false),
                target_as_tuple_type
                    .labeled_element_declarations
                    .as_ref()
                    .map(|labeled_element_declarations| {
                        labeled_element_declarations[index..end_index].to_owned()
                    })
                    .as_deref(),
            )?
        })
    }

    pub(super) fn get_known_keys_of_tuple_type(
        &self,
        type_: &Type, /*TupleTypeReference*/
    ) -> io::Result<Id<Type>> {
        let type_target = type_.as_type_reference().target();
        let type_target_as_tuple_type = type_target.as_tuple_type();
        self.get_union_type(
            &{
                let mut ret = array_of(type_target_as_tuple_type.fixed_length, |i| {
                    self.get_string_literal_type(&i.to_string())
                });
                ret.push(self.get_index_type(
                    &*if type_target_as_tuple_type.readonly {
                        self.global_readonly_array_type()
                    } else {
                        self.global_array_type()
                    },
                    None,
                    None,
                )?);
                ret
            },
            None,
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        )
    }

    pub(super) fn get_start_element_count(
        &self,
        type_: &Type, /*TupleType*/
        flags: ElementFlags,
    ) -> usize {
        let type_as_tuple_type = type_.as_tuple_type();
        let index = find_index(
            &type_as_tuple_type.element_flags,
            |f: &ElementFlags, _| !f.intersects(flags),
            None,
        );
        index.unwrap_or_else(|| type_as_tuple_type.element_flags.len())
    }

    pub(super) fn get_end_element_count(
        &self,
        type_: &Type, /*TupleType*/
        flags: ElementFlags,
    ) -> usize {
        let type_as_tuple_type = type_.as_tuple_type();
        let ret: isize = isize::try_from(type_as_tuple_type.element_flags.len()).unwrap()
            - find_last_index_returns_isize(
                &*type_as_tuple_type.element_flags,
                |f: &ElementFlags, _| !f.intersects(flags),
                None,
            )
            - 1;
        ret.try_into().unwrap()
    }

    pub(super) fn get_type_from_optional_type_node(
        &self,
        node: &Node, /*OptionalTypeNode*/
    ) -> io::Result<Id<Type>> {
        self.add_optionality(
            &*self.get_type_from_type_node_(&node.as_optional_type_node().type_)?,
            Some(true),
            None,
        )
    }

    pub(super) fn get_type_id(&self, type_: &Type) -> TypeId {
        type_.id()
    }

    pub(super) fn contains_type(&self, types: &[Id<Type>], type_: &Type) -> bool {
        binary_search_copy_key(
            types,
            &type_.type_wrapper(),
            |t, _| Some(self.get_type_id(t)),
            compare_values,
            None,
        ) >= 0
    }

    pub(super) fn insert_type(&self, types: &mut Vec<Id<Type>>, type_: &Type) -> bool {
        let index = binary_search_copy_key(
            types,
            &type_.type_wrapper(),
            |t, _| Some(self.get_type_id(t)),
            compare_values,
            None,
        );
        if index < 0 {
            types.insert((!index).try_into().unwrap(), type_.type_wrapper());
            return true;
        }
        false
    }

    pub(super) fn add_type_to_union(
        &self,
        type_set: &mut Vec<Id<Type>>,
        mut includes: TypeFlags,
        type_: &Type,
    ) -> TypeFlags {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::Union) {
            return self.add_types_to_union(
                type_set,
                includes
                    | if self.is_named_union_type(type_) {
                        TypeFlags::Union
                    } else {
                        TypeFlags::None
                    },
                type_.as_union_or_intersection_type_interface().types(),
            );
        }
        if !flags.intersects(TypeFlags::Never) {
            includes |= flags & TypeFlags::IncludesMask;
            if flags.intersects(TypeFlags::Instantiable) {
                includes |= TypeFlags::IncludesInstantiable;
            }
            if ptr::eq(type_, &*self.wildcard_type()) {
                includes |= TypeFlags::IncludesWildcard;
            }
            if !self.strict_null_checks && flags.intersects(TypeFlags::Nullable) {
                if !(get_object_flags(type_).intersects(ObjectFlags::ContainsWideningType)) {
                    includes |= TypeFlags::IncludesNonWideningType;
                }
            } else {
                let len = type_set.len();
                let index: isize = if len > 0 && type_.id() > type_set[len - 1].id() {
                    !isize::try_from(len).unwrap()
                } else {
                    binary_search_copy_key(
                        type_set,
                        &type_.type_wrapper(),
                        |type_, _| Some(self.get_type_id(type_)),
                        compare_values,
                        None,
                    )
                };
                if index < 0 {
                    type_set.insert(usize::try_from(!index).unwrap(), type_.type_wrapper());
                }
            }
        }
        includes
    }

    pub(super) fn add_types_to_union(
        &self,
        type_set: &mut Vec<Id<Type>>,
        mut includes: TypeFlags,
        types: impl IntoIterator<Item = impl Borrow<Id<Type>>>,
    ) -> TypeFlags {
        for type_ in types {
            let type_ = type_.borrow();
            includes = self.add_type_to_union(type_set, includes, type_);
        }
        includes
    }

    pub(super) fn remove_subtypes(
        &self,
        mut types: Vec<Id<Type>>,
        has_object_types: bool,
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        let id = self.get_type_list_id(Some(&types));
        let match_ = self.subtype_reduction_cache().get(&id).map(Clone::clone);
        if match_.is_some() {
            return Ok(match_);
        }
        let has_empty_object = has_object_types
            && try_some(
                Some(&*types),
                Some(|t: &Id<Type>| -> io::Result<_> {
                    Ok(t.flags().intersects(TypeFlags::Object)
                        && !self.is_generic_mapped_type(t)?
                        && self.is_empty_resolved_type(&*self.resolve_structured_type_members(t)?))
                }),
            )?;
        let len = types.len();
        let mut i = len;
        let mut count = 0;
        while i > 0 {
            i -= 1;
            let source = types[i].clone();
            if has_empty_object
                || source
                    .flags()
                    .intersects(TypeFlags::StructuredOrInstantiable)
            {
                let key_property = if source.flags().intersects(
                    TypeFlags::Object
                        | TypeFlags::Intersection
                        | TypeFlags::InstantiableNonPrimitive,
                ) {
                    self.get_properties_of_type(&source)?
                        .try_find_(|p| -> io::Result<_> {
                            Ok(self.is_unit_type(&*self.get_type_of_symbol(p)?))
                        })?
                } else {
                    None
                };
                let key_property_type =
                    key_property
                        .as_ref()
                        .try_map(|key_property| -> io::Result<_> {
                            Ok(self.get_regular_type_of_literal_type(
                                &*self.get_type_of_symbol(key_property)?,
                            ))
                        })?;
                for target in &types.clone() {
                    if !Gc::ptr_eq(&source, target) {
                        if count == 100000 {
                            let estimated_count = (count / (len - i)) * len;
                            if estimated_count > 1000000 {
                                // tracing?.instant(tracing.Phase.CheckTypes, "removeSubtypes_DepthLimit", { typeIds: types.map(t => t.id) });
                                self.error(
                                    self.maybe_current_node(),
                                    &Diagnostics::Expression_produces_a_union_type_that_is_too_complex_to_represent,
                                    None
                                );
                                return Ok(None);
                            }
                        }
                        count += 1;
                        if let Some(key_property) = key_property.as_ref() {
                            if target.flags().intersects(
                                TypeFlags::Object
                                    | TypeFlags::Intersection
                                    | TypeFlags::InstantiableNonPrimitive,
                            ) {
                                let t = self.get_type_of_property_of_type_(
                                    target,
                                    key_property.escaped_name(),
                                )?;
                                if matches!(
                                    t.as_ref(),
                                    Some(t) if self.is_unit_type(t) && !matches!(
                                        key_property_type.as_ref(),
                                        Some(key_property_type) if Gc::ptr_eq(&self.get_regular_type_of_literal_type(t), key_property_type)
                                    )
                                ) {
                                    continue;
                                }
                            }
                        }
                        if self.is_type_related_to(
                            &source,
                            target,
                            self.strict_subtype_relation.clone(),
                        )? && (!get_object_flags(&self.get_target_type(&source))
                            .intersects(ObjectFlags::Class)
                            || !get_object_flags(&self.get_target_type(target))
                                .intersects(ObjectFlags::Class)
                            || self.is_type_derived_from(&source, target)?)
                        {
                            ordered_remove_item_at(&mut types, i);
                            break;
                        }
                    }
                }
            }
        }
        self.subtype_reduction_cache().insert(id, types.clone());
        Ok(Some(types))
    }

    pub(super) fn remove_redundant_literal_types(
        &self,
        types: &mut Vec<Id<Type>>,
        includes: TypeFlags,
        reduce_void_undefined: bool,
    ) {
        let mut i = types.len();
        while i > 0 {
            i -= 1;
            let t = types[i].clone();
            let flags = t.flags();
            let remove = flags.intersects(
                TypeFlags::StringLiteral | TypeFlags::TemplateLiteral | TypeFlags::StringMapping,
            ) && includes.intersects(TypeFlags::String)
                || flags.intersects(TypeFlags::NumberLiteral)
                    && includes.intersects(TypeFlags::Number)
                || flags.intersects(TypeFlags::BigIntLiteral)
                    && includes.intersects(TypeFlags::BigInt)
                || flags.intersects(TypeFlags::UniqueESSymbol)
                    && includes.intersects(TypeFlags::ESSymbol)
                || reduce_void_undefined
                    && flags.intersects(TypeFlags::Undefined)
                    && includes.intersects(TypeFlags::Void)
                || self.is_fresh_literal_type(&t)
                    && self.contains_type(
                        types,
                        &*match &*t {
                            Type::IntrinsicType(intrinsic_type) => enum_unwrapped!(
                                intrinsic_type,
                                [IntrinsicType, FreshableIntrinsicType]
                            )
                            .regular_type(),
                            Type::LiteralType(literal_type) => literal_type.regular_type(),
                            _ => panic!("Expected IntrinsicType or LiteralType"),
                        },
                    );
            if remove {
                ordered_remove_item_at(types, i);
            }
        }
    }

    pub(super) fn remove_string_literals_matched_by_template_literals(
        &self,
        types: &mut Vec<Id<Type>>,
    ) -> io::Result<()> {
        let templates = filter(types, |type_: &Id<Type>| {
            self.is_pattern_literal_type(type_)
        });
        if !templates.is_empty() {
            let mut i = types.len();
            while i > 0 {
                i -= 1;
                let t = types[i].clone();
                if t.flags().intersects(TypeFlags::StringLiteral)
                    && try_some(
                        Some(&templates),
                        Some(|template: &Id<Type>| {
                            self.is_type_matched_by_template_literal_type(&t, template)
                        }),
                    )?
                {
                    ordered_remove_item_at(types, i);
                }
            }
        }

        Ok(())
    }

    pub(super) fn is_named_union_type(&self, type_: &Type) -> bool {
        type_.flags().intersects(TypeFlags::Union)
            && (type_.maybe_alias_symbol().is_some() || type_.as_union_type().origin.is_some())
    }

    pub(super) fn add_named_unions(
        &self,
        named_unions: &mut Vec<Id<Type>>,
        types: impl IntoIterator<Item = impl Borrow<Id<Type>>>,
    ) {
        for t in types {
            let t: &Id<Type> = t.borrow();
            if t.flags().intersects(TypeFlags::Union) {
                let origin = t.as_union_type().origin.clone();
                if t.maybe_alias_symbol().is_some()
                    || matches!(origin.as_ref(), Some(origin) if !origin.flags().intersects(TypeFlags::Union))
                {
                    push_if_unique_gc(named_unions, t);
                } else if matches!(origin.as_ref(), Some(origin) if origin.flags().intersects(TypeFlags::Union))
                {
                    self.add_named_unions(named_unions, origin.unwrap().as_union_type().types());
                }
            }
        }
    }

    pub(super) fn create_origin_union_or_intersection_type(
        &self,
        flags: TypeFlags,
        types: Vec<Id<Type>>,
    ) -> Id<Type> {
        let result = self.create_origin_type(flags);
        let result = BaseUnionOrIntersectionType::new(
            result,
            types,
            ObjectFlags::None, // this was made up
        );
        // so was this
        let result: Id<Type> = if flags.intersects(TypeFlags::Union) {
            UnionType::new(result).into()
        } else {
            IntersectionType::new(result).into()
        };
        result
    }

    pub(super) fn get_union_type<TTypesItem, TTypes>(
        &self,
        types: TTypes,
        union_reduction: Option<UnionReduction>,
        alias_symbol: Option<impl Borrow<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
        origin: Option<Id<Type>>,
    ) -> io::Result<Id<Type>>
    where
        TTypesItem: Borrow<Id<Type>>,
        TTypes: IntoIterator<Item = TTypesItem>,
        TTypes::IntoIter: Clone,
    {
        let union_reduction = union_reduction.unwrap_or(UnionReduction::Literal);
        let mut types = types.into_iter();
        let mut types_peekmore = types.clone().peekmore();
        if types_peekmore.is_empty() {
            return Ok(self.never_type());
        }
        if types_peekmore.is_len_equal_to(1) {
            return Ok(types.next().unwrap().borrow().clone());
        }
        let mut type_set: Vec<Id<Type>> = vec![];
        let includes = self.add_types_to_union(&mut type_set, TypeFlags::None, types.clone());
        if union_reduction != UnionReduction::None {
            if includes.intersects(TypeFlags::AnyOrUnknown) {
                return Ok(if includes.intersects(TypeFlags::Any) {
                    if includes.intersects(TypeFlags::IncludesWildcard) {
                        self.wildcard_type()
                    } else {
                        self.any_type()
                    }
                } else {
                    if includes.intersects(TypeFlags::Null)
                        || self.contains_type(&type_set, &self.unknown_type())
                    {
                        self.unknown_type()
                    } else {
                        self.non_null_unknown_type()
                    }
                });
            }
            if matches!(self.exact_optional_property_types, Some(true))
                && includes.intersects(TypeFlags::Undefined)
            {
                let missing_index = binary_search_copy_key(
                    &type_set,
                    &self.missing_type(),
                    |type_, _| Some(self.get_type_id(type_)),
                    compare_values,
                    None,
                );
                if missing_index >= 0 && self.contains_type(&type_set, &self.undefined_type()) {
                    ordered_remove_item_at(&mut type_set, missing_index.try_into().unwrap());
                }
            }
            if includes.intersects(
                TypeFlags::Literal
                    | TypeFlags::UniqueESSymbol
                    | TypeFlags::TemplateLiteral
                    | TypeFlags::StringMapping,
            ) || includes.intersects(TypeFlags::Void)
                && includes.intersects(TypeFlags::Undefined)
            {
                self.remove_redundant_literal_types(
                    &mut type_set,
                    includes,
                    union_reduction == UnionReduction::Subtype,
                );
            }
            if includes.intersects(TypeFlags::StringLiteral)
                && includes.intersects(TypeFlags::TemplateLiteral)
            {
                self.remove_string_literals_matched_by_template_literals(&mut type_set)?;
            }
            if union_reduction == UnionReduction::Subtype {
                let maybe_type_set =
                    self.remove_subtypes(type_set, includes.intersects(TypeFlags::Object))?;
                match maybe_type_set {
                    Some(maybe_type_set) => {
                        type_set = maybe_type_set;
                    }
                    None => {
                        return Ok(self.error_type());
                    }
                }
            }
            if type_set.is_empty() {
                return Ok(if includes.intersects(TypeFlags::Null) {
                    if includes.intersects(TypeFlags::IncludesNonWideningType) {
                        self.null_type()
                    } else {
                        self.null_widening_type()
                    }
                } else if includes.intersects(TypeFlags::Undefined) {
                    if includes.intersects(TypeFlags::IncludesNonWideningType) {
                        self.undefined_type()
                    } else {
                        self.undefined_widening_type()
                    }
                } else {
                    self.never_type()
                });
            }
        }
        let mut origin = origin.map(|origin| origin.borrow().type_wrapper());
        if origin.is_none() && includes.intersects(TypeFlags::Union) {
            let mut named_unions: Vec<Id<Type>> = vec![];
            self.add_named_unions(&mut named_unions, types);
            let mut reduced_types: Vec<Id<Type>> = vec![];
            for t in &type_set {
                if !some(
                    Some(&named_unions),
                    Some(|union: &Id<Type>| self.contains_type(union.as_union_type().types(), t)),
                ) {
                    reduced_types.push(t.clone());
                }
            }
            if alias_symbol.is_none() && named_unions.len() == 1 && reduced_types.is_empty() {
                return Ok(named_unions[0].clone());
            }
            let named_types_count = reduce_left(
                &named_unions,
                |sum, union: &Id<Type>, _| sum + union.as_union_type().types().len(),
                0,
                None,
                None,
            );
            if named_types_count + reduced_types.len() == type_set.len() {
                for t in &named_unions {
                    self.insert_type(&mut reduced_types, t);
                }
                origin = Some(
                    self.create_origin_union_or_intersection_type(TypeFlags::Union, reduced_types),
                );
            }
        }
        let object_flags = (if includes.intersects(TypeFlags::NotPrimitiveUnion) {
            ObjectFlags::None
        } else {
            ObjectFlags::PrimitiveUnion
        }) | (if includes.intersects(TypeFlags::Intersection) {
            ObjectFlags::ContainsIntersections
        } else {
            ObjectFlags::None
        });
        Ok(self.get_union_type_from_sorted_list(
            type_set,
            object_flags,
            alias_symbol,
            alias_type_arguments,
            origin,
        ))
    }

    pub(super) fn get_union_or_intersection_type_predicate(
        &self,
        signatures: &[Gc<Signature>],
        kind: Option<TypeFlags>,
    ) -> io::Result<Option<TypePredicate>> {
        let mut first: Option<Gc<TypePredicate>> = None;
        let mut types: Vec<Id<Type>> = vec![];
        for sig in signatures {
            let pred = self.get_type_predicate_of_signature(sig)?;
            if match pred.as_ref() {
                None => true,
                Some(pred) => matches!(
                    pred.kind,
                    TypePredicateKind::AssertsThis | TypePredicateKind::AssertsIdentifier
                ),
            } {
                if !matches!(kind, Some(TypeFlags::Intersection)) {
                    continue;
                } else {
                    return Ok(None);
                }
            }
            let pred = pred.unwrap();

            if let Some(first) = first.as_ref() {
                if !self.type_predicate_kinds_match(first, &pred) {
                    return Ok(None);
                }
            } else {
                first = Some(pred.clone());
            }
            types.push(pred.type_.clone().unwrap());
        }
        let first = return_ok_default_if_none!(first);
        let composite_type = self.get_union_or_intersection_type(&types, kind, None)?;
        Ok(Some(self.create_type_predicate(
            first.kind,
            first.parameter_name.clone(),
            first.parameter_index,
            Some(composite_type),
        )))
    }

    pub(super) fn type_predicate_kinds_match(&self, a: &TypePredicate, b: &TypePredicate) -> bool {
        a.kind == b.kind && a.parameter_index == b.parameter_index
    }

    pub(super) fn get_union_type_from_sorted_list<
        TAliasSymbol: Borrow<Symbol>,
        TOrigin: Borrow<Type>,
    >(
        &self,
        types: Vec<Id<Type>>,
        object_flags: ObjectFlags,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Id<Type>]>,
        origin: Option<TOrigin>,
    ) -> Id<Type> {
        if types.is_empty() {
            return self.never_type();
        }
        if types.len() == 1 {
            return types[0].clone();
        }
        let origin = origin.map(|origin| origin.borrow().type_wrapper());
        let type_key = match origin.as_ref() {
            None => self.get_type_list_id(Some(&types)),
            Some(origin) => {
                if origin.flags().intersects(TypeFlags::Union) {
                    format!(
                        "|{}",
                        self.get_type_list_id(Some(origin.as_union_type().types()))
                    )
                } else if origin.flags().intersects(TypeFlags::Intersection) {
                    format!(
                        "&{}",
                        self.get_type_list_id(Some(origin.as_intersection_type().types()))
                    )
                } else {
                    format!(
                        "#{}|{}",
                        origin.as_index_type().type_.id(),
                        self.get_type_list_id(Some(&types))
                    )
                }
            }
        };
        let alias_symbol = alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
        let id = format!(
            "{}{}",
            type_key,
            self.get_alias_id(alias_symbol.as_deref(), alias_type_arguments)
        );
        let mut type_: Option<Id<Type>> = self.union_types().get(&id).map(Clone::clone);
        if type_.is_none() {
            let is_boolean = types.len() == 2
                && types[0].flags().intersects(TypeFlags::BooleanLiteral)
                && types[1].flags().intersects(TypeFlags::BooleanLiteral);
            let base_type = self.create_type(if is_boolean {
                TypeFlags::Union | TypeFlags::Boolean
            } else {
                TypeFlags::Union
            });
            let object_flags_to_set =
                object_flags | self.get_propagating_flags_of_types(&types, TypeFlags::Nullable);
            let mut union_type = UnionType::new(BaseUnionOrIntersectionType::new(
                base_type,
                types,
                object_flags_to_set,
            ));
            union_type.origin = origin;
            type_ = Some(union_type.into());
            let type_ = type_.as_ref().unwrap();
            *type_.maybe_alias_symbol_mut() = alias_symbol;
            *type_.maybe_alias_type_arguments_mut() = alias_type_arguments.map(ToOwned::to_owned);
            // TODO: also treat union type as intrinsic type with intrinsic_name = "boolean" if
            // is_boolean - should expose maybe_intrinsic_name on UnionType or something?
            self.union_types().insert(id, type_.clone());
        }
        type_.unwrap()
    }

    pub(super) fn get_type_from_union_type_node(
        &self,
        node: &Node, /*UnionTypeNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            let alias_symbol = self.get_alias_symbol_for_type_node(node)?;
            links.borrow_mut().resolved_type = Some(
                self.get_union_type(
                    &try_map(&node.as_union_type_node().types, |type_: &Gc<Node>, _| {
                        self.get_type_from_type_node_(type_)
                    })?,
                    Some(UnionReduction::Literal),
                    alias_symbol.clone(),
                    self.get_type_arguments_for_alias_symbol(alias_symbol)?
                        .as_deref(),
                    Option::<&Type>::None,
                )?,
            );
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn add_type_to_intersection(
        &self,
        type_set: &mut IndexMap<TypeId, Id<Type>>,
        mut includes: TypeFlags,
        type_: &Type,
    ) -> io::Result<TypeFlags> {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::Intersection) {
            return self.add_types_to_intersection(
                type_set,
                includes,
                type_.as_intersection_type().types(),
            );
        }
        let mut type_ = type_.type_wrapper();
        if self.is_empty_anonymous_object_type(&type_)? {
            if !includes.intersects(TypeFlags::IncludesEmptyObject) {
                includes |= TypeFlags::IncludesEmptyObject;
                type_set.insert(type_.id(), type_.clone());
            }
        } else {
            if flags.intersects(TypeFlags::AnyOrUnknown) {
                if Gc::ptr_eq(&type_, &self.wildcard_type()) {
                    includes |= TypeFlags::IncludesWildcard;
                }
            } else if self.strict_null_checks || !flags.intersects(TypeFlags::Nullable) {
                if matches!(self.exact_optional_property_types, Some(true))
                    && Gc::ptr_eq(&type_, &self.missing_type())
                {
                    includes |= TypeFlags::IncludesMissingType;
                    type_ = self.undefined_type();
                }
                if !type_set.contains_key(&type_.id()) {
                    if type_.flags().intersects(TypeFlags::Unit)
                        && includes.intersects(TypeFlags::Unit)
                    {
                        includes |= TypeFlags::NonPrimitive;
                    }
                    type_set.insert(type_.id(), type_);
                }
            }
            includes |= flags & TypeFlags::IncludesMask;
        }
        Ok(includes)
    }

    pub(super) fn add_types_to_intersection(
        &self,
        type_set: &mut IndexMap<TypeId, Id<Type>>,
        mut includes: TypeFlags,
        types: &[Id<Type>],
    ) -> io::Result<TypeFlags> {
        for type_ in types {
            includes = self.add_type_to_intersection(
                type_set,
                includes,
                &self.get_regular_type_of_literal_type(type_),
            )?;
        }
        Ok(includes)
    }

    pub(super) fn remove_redundant_primitive_types(
        &self,
        types: &mut Vec<Id<Type>>,
        includes: TypeFlags,
    ) {
        let mut i = types.len();
        while i > 0 {
            i -= 1;
            let t = types[i].clone();
            let remove = t.flags().intersects(TypeFlags::String)
                && includes.intersects(TypeFlags::StringLiteral)
                || t.flags().intersects(TypeFlags::Number)
                    && includes.intersects(TypeFlags::NumberLiteral)
                || t.flags().intersects(TypeFlags::BigInt)
                    && includes.intersects(TypeFlags::BigIntLiteral)
                || t.flags().intersects(TypeFlags::ESSymbol)
                    && includes.intersects(TypeFlags::UniqueESSymbol);
            if remove {
                ordered_remove_item_at(types, i);
            }
        }
    }
}
