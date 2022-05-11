#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::convert::{TryFrom, TryInto};
use std::ptr;
use std::rc::Rc;

use crate::{
    array_of, filter, find, find_index, find_last_index, for_each, get_object_flags,
    is_part_of_type_node, ordered_remove_item_at, push_if_unique_rc, replace_element, same_map,
    some, AccessFlags, Diagnostics, ElementFlags, IntersectionType, LiteralTypeInterface,
    Signature, TypePredicate, TypeReferenceInterface, UnionOrIntersectionTypeInterface, UnionType,
    __String, binary_search_copy_key, compare_values, get_name_of_declaration, map,
    unescape_leading_underscores, BaseUnionOrIntersectionType, Node, ObjectFlags, Symbol,
    SymbolInterface, Type, TypeChecker, TypeFlags, TypeId, TypeInterface, UnionReduction,
};

impl TypeChecker {
    pub(super) fn create_normalized_type_reference(
        &self,
        target: &Type, /*GenericType*/
        type_arguments: Option<Vec<Rc<Type>>>,
    ) -> Rc<Type> {
        if target
            .as_object_flags_type()
            .object_flags()
            .intersects(ObjectFlags::Tuple)
        {
            self.create_normalized_tuple_type(target, type_arguments.unwrap())
        } else {
            self.create_type_reference(target, type_arguments)
        }
    }

    pub(super) fn create_normalized_tuple_type(
        &self,
        target: &Type, /*TupleType*/
        element_types: Vec<Rc<Type>>,
    ) -> Rc<Type> {
        let target_as_tuple_type = target.as_tuple_type();
        if !target_as_tuple_type
            .combined_flags
            .intersects(ElementFlags::NonRequired)
        {
            return self.create_type_reference(target, Some(element_types));
        }
        if target_as_tuple_type
            .combined_flags
            .intersects(ElementFlags::Variadic)
        {
            let union_index = find_index(
                &element_types,
                |t: &Rc<Type>, i| {
                    target_as_tuple_type.element_flags[i].intersects(ElementFlags::Variadic)
                        && t.flags().intersects(TypeFlags::Never | TypeFlags::Union)
                },
                None,
            );
            if let Some(union_index) = union_index {
                return if self.check_cross_product_union(
                    &map(Some(&*element_types), |t: &Rc<Type>, i| {
                        if target_as_tuple_type.element_flags[i].intersects(ElementFlags::Variadic)
                        {
                            t.clone()
                        } else {
                            self.unknown_type()
                        }
                    })
                    .unwrap(),
                ) {
                    self.map_type(
                        &element_types[union_index],
                        &mut |t: &Type| {
                            Some(self.create_normalized_tuple_type(
                                target,
                                replace_element(&element_types, union_index, t.type_wrapper()),
                            ))
                        },
                        None,
                    )
                    .unwrap()
                } else {
                    self.error_type()
                };
            }
        }
        let mut expanded_types: Vec<Rc<Type>> = vec![];
        let mut expanded_flags: Vec<ElementFlags> = vec![];
        let mut expanded_declarations: Option<
            Vec<Rc<Node /*NamedTupleMember | ParameterDeclaration*/>>,
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
                    || self.is_generic_mapped_type(type_)
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
                    let elements = self.get_type_arguments(type_);
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
                        return self.error_type();
                    }
                    for_each(&elements, |t: &Rc<Type>, n| {
                        self.add_element(
                            &mut last_required_index,
                            &mut expanded_flags,
                            &mut first_rest_index,
                            &mut last_optional_or_rest_index,
                            &mut expanded_types,
                            &mut expanded_declarations,
                            t,
                            type_.as_tuple_type().target().as_tuple_type().element_flags[n],
                            type_
                                .as_tuple_type()
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
                        &*if self.is_array_like_type(type_) {
                            self.get_index_type_of_type_(type_, &self.number_type())
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
                same_map(
                    Some(&expanded_types[first_rest_index..last_optional_or_rest_index + 1]),
                    |t: &Rc<Type>, i| {
                        if expanded_flags[first_rest_index + i].intersects(ElementFlags::Variadic) {
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
                        }
                    },
                )
                .unwrap(),
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            );
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
        );
        if Rc::ptr_eq(&tuple_target, &self.empty_generic_type()) {
            self.empty_object_type()
        } else if !expanded_flags.is_empty() {
            self.create_type_reference(&tuple_target, Some(expanded_types))
        } else {
            tuple_target
        }
    }

    pub(super) fn add_element(
        &self,
        last_required_index: &mut isize,
        expanded_flags: &mut Vec<ElementFlags>,
        first_rest_index: &mut isize,
        last_optional_or_rest_index: &mut isize,
        expanded_types: &mut Vec<Rc<Type>>,
        expanded_declarations: &mut Option<Vec<Rc<Node>>>,
        type_: &Type,
        flags: ElementFlags,
        declaration: Option<Rc<Node /*NamedTupleMember | ParameterDeclaration*/>>,
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
    ) -> Rc<Type> {
        let end_skip_count = end_skip_count.unwrap_or(0);
        let target = type_.as_type_reference().target();
        let end_index = self.get_type_reference_arity(type_) - end_skip_count;
        let target_as_tuple_type = target.as_tuple_type();
        if index > target_as_tuple_type.fixed_length {
            self.get_rest_array_type_of_tuple_type(type_)
                .unwrap_or_else(|| self.create_tuple_type(&[], None, None, None))
        } else {
            self.create_tuple_type(
                &self.get_type_arguments(type_)[index..end_index],
                Some(&target_as_tuple_type.element_flags[index..end_index]),
                Some(false),
                target_as_tuple_type
                    .labeled_element_declarations
                    .as_ref()
                    .map(|labeled_element_declarations| {
                        labeled_element_declarations[index..end_index].to_owned()
                    })
                    .as_deref(),
            )
        }
    }

    pub(super) fn get_known_keys_of_tuple_type(
        &self,
        type_: &Type, /*TupleTypeReference*/
    ) -> Rc<Type> {
        let type_target = type_.as_type_reference().target();
        let type_target_as_tuple_type = type_target.as_tuple_type();
        self.get_union_type(
            {
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
                ));
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
            - find_last_index(
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
    ) -> Rc<Type> {
        self.add_optionality(
            &self.get_type_from_type_node_(&node.as_optional_type_node().type_),
            Some(true),
            None,
        )
    }

    pub(super) fn get_type_id(&self, type_: &Type) -> TypeId {
        type_.id()
    }

    pub(super) fn contains_type(&self, types: &[Rc<Type>], type_: &Type) -> bool {
        binary_search_copy_key(
            types,
            &type_.type_wrapper(),
            |t, _| Some(self.get_type_id(t)),
            compare_values,
            None,
        ) >= 0
    }

    pub(super) fn insert_type(&self, types: &mut Vec<Rc<Type>>, type_: &Type) -> bool {
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
        type_set: &mut Vec<Rc<Type>>,
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
        type_set: &mut Vec<Rc<Type>>,
        mut includes: TypeFlags,
        types: &[Rc<Type>],
    ) -> TypeFlags {
        for type_ in types {
            includes = self.add_type_to_union(type_set, includes, &type_);
        }
        includes
    }

    pub(super) fn remove_subtypes(
        &self,
        mut types: Vec<Rc<Type>>,
        has_object_types: bool,
    ) -> Option<Vec<Rc<Type>>> {
        let id = self.get_type_list_id(Some(&types));
        let match_ = self.subtype_reduction_cache().get(&id).map(Clone::clone);
        if match_.is_some() {
            return match_;
        }
        let has_empty_object = has_object_types
            && some(
                Some(&*types),
                Some(|t: &Rc<Type>| {
                    t.flags().intersects(TypeFlags::Object)
                        && !self.is_generic_mapped_type(t)
                        && self.is_empty_resolved_type(&self.resolve_structured_type_members(t))
                }),
            );
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
                    find(
                        &*self.get_properties_of_type(&source),
                        |p: &Rc<Symbol>, _| self.is_unit_type(&self.get_type_of_symbol(p)),
                    )
                    .map(Clone::clone)
                } else {
                    None
                };
                let key_property_type = key_property.as_ref().map(|key_property| {
                    self.get_regular_type_of_literal_type(&self.get_type_of_symbol(key_property))
                });
                for target in &types.clone() {
                    if !Rc::ptr_eq(&source, target) {
                        if count == 100000 {
                            let estimated_count = (count / (len - i)) * len;
                            if estimated_count > 1000000 {
                                // tracing?.instant(tracing.Phase.CheckTypes, "removeSubtypes_DepthLimit", { typeIds: types.map(t => t.id) });
                                self.error(
                                    self.maybe_current_node(),
                                    &Diagnostics::Expression_produces_a_union_type_that_is_too_complex_to_represent,
                                    None
                                );
                                return None;
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
                                );
                                if matches!(
                                    t.as_ref(),
                                    Some(t) if self.is_unit_type(t) && !matches!(
                                        key_property_type.as_ref(),
                                        Some(key_property_type) if Rc::ptr_eq(&self.get_regular_type_of_literal_type(t), key_property_type)
                                    )
                                ) {
                                    continue;
                                }
                            }
                        }
                        if self.is_type_related_to(&source, target, &self.strict_subtype_relation())
                            && (!get_object_flags(&self.get_target_type(&source))
                                .intersects(ObjectFlags::Class)
                                || !get_object_flags(&self.get_target_type(target))
                                    .intersects(ObjectFlags::Class)
                                || self.is_type_derived_from(&source, target))
                        {
                            ordered_remove_item_at(&mut types, i);
                            break;
                        }
                    }
                }
            }
        }
        self.subtype_reduction_cache().insert(id, types.clone());
        Some(types)
    }

    pub(super) fn remove_redundant_literal_types(
        &self,
        types: &mut Vec<Rc<Type>>,
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
                    && self.contains_type(types, &t.as_literal_type().regular_type());
            if remove {
                ordered_remove_item_at(types, i);
            }
        }
    }

    pub(super) fn remove_string_literals_matched_by_template_literals(
        &self,
        types: &mut Vec<Rc<Type>>,
    ) {
        let templates = filter(Some(types), |type_: &Rc<Type>| {
            self.is_pattern_literal_type(type_)
        })
        .unwrap();
        if !templates.is_empty() {
            let mut i = types.len();
            while i > 0 {
                i -= 1;
                let t = types[i].clone();
                if t.flags().intersects(TypeFlags::StringLiteral)
                    && some(
                        Some(&templates),
                        Some(|template: &Rc<Type>| {
                            self.is_type_matched_by_template_literal_type(&t, template)
                        }),
                    )
                {
                    ordered_remove_item_at(types, i);
                }
            }
        }
    }

    pub(super) fn is_named_union_type(&self, type_: &Type) -> bool {
        type_.flags().intersects(TypeFlags::Union)
            && (type_.maybe_alias_symbol().is_some() || type_.as_union_type().origin.is_some())
    }

    pub(super) fn add_named_unions(&self, named_unions: &mut Vec<Rc<Type>>, types: &[Rc<Type>]) {
        for t in types {
            if t.flags().intersects(TypeFlags::Union) {
                let origin = t.as_union_type().origin.clone();
                if t.maybe_alias_symbol().is_some()
                    || matches!(origin.as_ref(), Some(origin) if !origin.flags().intersects(TypeFlags::Union))
                {
                    push_if_unique_rc(named_unions, t);
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
        types: Vec<Rc<Type>>,
    ) -> Rc<Type> {
        let result = self.create_origin_type(flags);
        let result = BaseUnionOrIntersectionType::new(
            result,
            types,
            ObjectFlags::None, // this was made up
        );
        // so was this
        let result: Rc<Type> = if flags.intersects(TypeFlags::Union) {
            UnionType::new(result).into()
        } else {
            IntersectionType::new(result).into()
        };
        result
    }

    pub(super) fn get_union_type<TAliasSymbol: Borrow<Symbol>, TOrigin: Borrow<Type>>(
        &self,
        types: Vec<Rc<Type>>,
        union_reduction: Option<UnionReduction>,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
        origin: Option<TOrigin>,
    ) -> Rc<Type> {
        let union_reduction = union_reduction.unwrap_or(UnionReduction::Literal);
        if types.is_empty() {
            return self.never_type();
        }
        if types.len() == 1 {
            return types[0].clone();
        }
        let mut type_set: Vec<Rc<Type>> = vec![];
        let includes = self.add_types_to_union(&mut type_set, TypeFlags::None, &types);
        if union_reduction != UnionReduction::None {}
        let object_flags = (if includes.intersects(TypeFlags::NotPrimitiveUnion) {
            ObjectFlags::None
        } else {
            ObjectFlags::PrimitiveUnion
        }) | (if includes.intersects(TypeFlags::Intersection) {
            ObjectFlags::ContainsIntersections
        } else {
            ObjectFlags::None
        });
        self.get_union_type_from_sorted_list(type_set, object_flags)
    }

    pub(super) fn get_union_or_intersection_type_predicate(
        &self,
        signatures: &[Rc<Signature>],
        kind: Option<TypeFlags>,
    ) -> Option<TypePredicate> {
        unimplemented!()
    }

    pub(super) fn get_union_type_from_sorted_list(
        &self,
        types: Vec<Rc<Type>>,
        object_flags: ObjectFlags,
    ) -> Rc<Type> {
        let mut type_: Option<Rc<Type>> = None;
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
            type_ = Some(
                UnionType::new(BaseUnionOrIntersectionType::new(
                    base_type,
                    types,
                    object_flags_to_set,
                ))
                .into(),
            );
            // TODO: also treat union type as intrinsic type with intrinsic_name = "boolean" if
            // is_boolean - should expose maybe_intrinsic_name on UnionType or something?
        }
        type_.unwrap()
    }

    pub(super) fn get_type_from_union_type_node(
        &self,
        node: &Node, /*UnionTypeNode*/
    ) -> Rc<Type> {
        let node_as_union_type_node = node.as_union_type_node();
        let links = self.get_node_links(node);
        let mut links_ref = links.borrow_mut();
        if links_ref.resolved_type.is_none() {
            // let alias_symbol = self.get_alias_symbol_for_type_node(node);
            links_ref.resolved_type = Some(
                self.get_union_type(
                    map(Some(&node_as_union_type_node.types), |type_, _| {
                        self.get_type_from_type_node_(type_)
                    })
                    .unwrap(),
                    Some(UnionReduction::Literal),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                ),
            );
        }
        links_ref.resolved_type.clone().unwrap()
    }

    pub(super) fn get_intersection_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        types: &[Rc<Type>],
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_cross_product_union(&self, types: &[Rc<Type>]) -> bool {
        unimplemented!()
    }

    pub(super) fn get_literal_type_from_property_name(
        &self,
        name: &Node, /*PropertyName*/
    ) -> Rc<Type> {
        if let Node::Identifier(identifier) = name {
            self.get_string_literal_type(&unescape_leading_underscores(&identifier.escaped_text))
        } else {
            unimplemented!()
        }
    }

    pub(super) fn get_literal_type_from_property(
        &self,
        prop: &Symbol,
        include: TypeFlags,
        include_non_public: Option<bool>,
    ) -> Rc<Type> {
        let include_non_public = include_non_public.unwrap_or(false);
        if include_non_public || true {
            let mut type_ = None;
            if type_.is_none() {
                let name = prop
                    .maybe_value_declaration()
                    .and_then(|value_declaration| {
                        get_name_of_declaration(Some(&*value_declaration))
                    });
                type_ = if false {
                    unimplemented!()
                } else if let Some(name) = name {
                    Some(self.get_literal_type_from_property_name(&*name))
                } else {
                    unimplemented!()
                }
            }
            if let Some(type_) = type_ {
                if type_.flags().intersects(include) {
                    return type_;
                }
            }
        }
        unimplemented!()
    }

    pub(super) fn get_index_type(
        &self,
        type_: &Type,
        strings_only: Option<bool>,
        no_index_signatures: Option<bool>,
    ) -> Rc<Type> {
        let strings_only = strings_only.unwrap_or(self.keyof_strings_only);
        unimplemented!()
    }

    pub(super) fn get_extract_string_type(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_template_literal_type(
        &self,
        texts: &[String],
        types: &[Rc<Type>],
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_string_mapping_type(&self, symbol: &Symbol, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_property_name_from_index(&self, index_type: &Type) -> Option<__String> {
        if self.is_type_usable_as_property_name(index_type) {
            Some(self.get_property_name_from_type(index_type))
        } else {
            unimplemented!()
        }
    }

    pub(super) fn get_property_type_for_index_type(
        &self,
        original_object_type: &Type,
        object_type: &Type,
        index_type: &Type,
        full_index_type: &Type,
    ) -> Option<Rc<Type>> {
        let prop_name = if false {
            unimplemented!()
        } else {
            self.get_property_name_from_index(index_type)
        };
        if let Some(prop_name) = prop_name {
            let prop = self.get_property_of_type_(object_type, &prop_name, None);
            if let Some(prop) = prop {
                let prop_type = self.get_type_of_symbol(&*prop);
                return if false {
                    unimplemented!()
                } else {
                    Some(prop_type)
                };
            }
        }
        None
    }

    pub(super) fn is_pattern_literal_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_generic_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_generic_object_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_generic_index_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_this_type_parameter(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_simplified_type(&self, type_: &Type, writing: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_indexed_access_type<
        TAccessNode: Borrow<Node>,
        TAliasSymbol: Borrow<Symbol>,
    >(
        &self,
        object_type: &Type,
        index_type: &Type,
        access_flags: Option<AccessFlags>,
        access_node: Option<
            TAccessNode, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let access_flags = access_flags.unwrap_or(AccessFlags::None);
        unimplemented!()
    }

    pub(super) fn get_indexed_access_type_or_undefined<
        TAccessNode: Borrow<Node>,
        TAliasSymbol: Borrow<Symbol>,
    >(
        &self,
        object_type: &Type,
        index_type: &Type,
        access_flags: Option<AccessFlags>,
        access_node: Option<
            TAccessNode, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Option<Rc<Type>> {
        let access_flags = access_flags.unwrap_or(AccessFlags::None);
        let apparent_object_type = self.get_reduced_apparent_type(object_type);
        self.get_property_type_for_index_type(
            object_type,
            &apparent_object_type,
            index_type,
            index_type,
        )
    }

    pub(super) fn get_type_from_mapped_type_node(
        &self,
        node: &Node, /*MappedTypeNode*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_actual_type_variable(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_true_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_false_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_inferred_true_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_infer_type_parameters(
        &self,
        node: &Node, /*ConditionalTypeNode*/
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        unimplemented!()
    }

    pub(super) fn get_alias_symbol_for_type_node(&self, node: &Node) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_arguments_for_alias_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }
}
