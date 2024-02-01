use std::{
    borrow::{Borrow, Cow},
    convert::TryInto,
    io, ptr,
};

use gc::Gc;
use id_arena::Id;
use indexmap::IndexMap;

use crate::{
    contains, every, filter, find_index, get_declaration_modifier_flags_from_symbol,
    get_name_of_declaration, get_object_flags, index_of_eq, index_of_gc, is_computed_property_name,
    is_identifier, is_known_symbol, is_private_identifier, map, ordered_remove_item_at,
    reduce_left, replace_element, some, symbol_name, try_find_index, try_map,
    unescape_leading_underscores, walk_up_parenthesized_types, BaseUnionOrIntersectionType, Debug_,
    Diagnostics, HasArena, InArena, IndexType, InternalSymbolName, IntersectionType, ModifierFlags,
    Node, NodeInterface, ObjectFlags, ObjectTypeInterface, Symbol, SymbolInterface, SyntaxKind,
    Type, TypeChecker, TypeFlags, TypeId, TypeInterface, UnionOrIntersectionTypeInterface,
    UnionReduction, VecExt,
};

impl TypeChecker {
    pub(super) fn each_union_contains(
        &self,
        union_types: &[Id<Type /*UnionType*/>],
        type_: Id<Type>,
    ) -> bool {
        for &u in union_types {
            if !self.contains_type(u.ref_(self).as_union_type().types(), type_) {
                let primitive = if type_
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::StringLiteral)
                {
                    Some(self.string_type())
                } else if type_
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::NumberLiteral)
                {
                    Some(self.number_type())
                } else if type_
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::BigIntLiteral)
                {
                    Some(self.bigint_type())
                } else if type_
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::UniqueESSymbol)
                {
                    Some(self.es_symbol_type())
                } else {
                    None
                };
                if match primitive {
                    None => true,
                    Some(primitive) => {
                        !self.contains_type(u.ref_(self).as_union_type().types(), primitive)
                    }
                } {
                    return false;
                }
            }
        }
        true
    }

    pub(super) fn extract_redundant_template_literals(
        &self,
        types: &mut Vec<Id<Type>>,
    ) -> io::Result<bool> {
        let mut i = types.len();
        let literals = filter(types, |&t: &Id<Type>| {
            t.ref_(self).flags().intersects(TypeFlags::StringLiteral)
        });
        while i > 0 {
            i -= 1;
            let t = types[i].clone();
            if !t.ref_(self).flags().intersects(TypeFlags::TemplateLiteral) {
                continue;
            }
            for &t2 in &literals {
                if self.is_type_subtype_of(t2, t)? {
                    ordered_remove_item_at(types, i);
                    break;
                } else if self.is_pattern_literal_type(t) {
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    pub(super) fn each_is_union_containing(&self, types: &[Id<Type>], flag: TypeFlags) -> bool {
        every(types, |&t: &Id<Type>, _| {
            t.ref_(self).flags().intersects(TypeFlags::Union)
                && some(
                    Some(t.ref_(self).as_union_type().types()),
                    Some(|&tt: &Id<Type>| tt.ref_(self).flags().intersects(flag)),
                )
        })
    }

    pub(super) fn remove_from_each(&self, types: &mut Vec<Id<Type>>, flag: TypeFlags) {
        for i in 0..types.len() {
            types[i] = self.filter_type(types[i].clone(), |t: Id<Type>| {
                !t.ref_(self).flags().intersects(flag)
            });
        }
    }

    pub(super) fn intersect_unions_of_primitive_types(&self, types: &mut Vec<Id<Type>>) -> bool {
        let mut union_types: Option<Vec<Id<Type /*UnionType*/>>> = None;
        let index = find_index(
            types,
            |&t: &Id<Type>, _| {
                get_object_flags(&t.ref_(self)).intersects(ObjectFlags::PrimitiveUnion)
            },
            None,
        );
        if index.is_none() {
            return false;
        }
        let index = index.unwrap();
        let mut i = index + 1;
        while i < types.len() {
            let t = types[i].clone();
            if get_object_flags(&t.ref_(self)).intersects(ObjectFlags::PrimitiveUnion) {
                if union_types.is_none() {
                    union_types = Some(vec![types[index].clone()]);
                }
                union_types.as_mut().unwrap().push(t);
                ordered_remove_item_at(types, i);
            } else {
                i += 1;
            }
        }
        if union_types.is_none() {
            return false;
        }
        let union_types = union_types.unwrap();
        let mut checked: Vec<Id<Type>> = vec![];
        let mut result: Vec<Id<Type>> = vec![];
        for &u in &union_types {
            for &t in u.ref_(self).as_union_type().types() {
                if self.insert_type(&mut checked, t) {
                    if self.each_union_contains(&union_types, t) {
                        self.insert_type(&mut result, t);
                    }
                }
            }
        }
        types[index] = self.get_union_type_from_sorted_list(
            result,
            ObjectFlags::PrimitiveUnion,
            Option::<Id<Symbol>>::None,
            None,
            None,
        );
        true
    }

    pub(super) fn create_intersection_type(
        &self,
        types: Vec<Id<Type>>,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> Id<Type> {
        let result = self.create_type(TypeFlags::Intersection);
        let object_flags = self.get_propagating_flags_of_types(&types, TypeFlags::Nullable);
        let result = BaseUnionOrIntersectionType::new(result, types, object_flags);
        let result = self.alloc_type(IntersectionType::new(result).into());
        *result.ref_(self).maybe_alias_symbol_mut() = alias_symbol;
        *result.ref_(self).maybe_alias_type_arguments_mut() =
            alias_type_arguments.map(ToOwned::to_owned);
        result
    }

    pub(super) fn get_intersection_type(
        &self,
        types: &[Id<Type>],
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type>> {
        let mut type_membership_map: IndexMap<TypeId, Id<Type>> = IndexMap::new();
        let includes =
            self.add_types_to_intersection(&mut type_membership_map, TypeFlags::None, types)?;
        let mut type_set: Vec<Id<Type>> = type_membership_map.values().map(Clone::clone).collect();
        if includes.intersects(TypeFlags::Never) {
            return Ok(if contains(Some(&type_set), &self.silent_never_type()) {
                self.silent_never_type()
            } else {
                self.never_type()
            });
        }
        if self.strict_null_checks
            && includes.intersects(TypeFlags::Nullable)
            && includes.intersects(
                TypeFlags::Object | TypeFlags::NonPrimitive | TypeFlags::IncludesEmptyObject,
            )
            || includes.intersects(TypeFlags::NonPrimitive)
                && includes.intersects(TypeFlags::DisjointDomains & !TypeFlags::NonPrimitive)
            || includes.intersects(TypeFlags::StringLike)
                && includes.intersects(TypeFlags::DisjointDomains & !TypeFlags::StringLike)
            || includes.intersects(TypeFlags::NumberLike)
                && includes.intersects(TypeFlags::DisjointDomains & !TypeFlags::NumberLike)
            || includes.intersects(TypeFlags::BigIntLike)
                && includes.intersects(TypeFlags::DisjointDomains & !TypeFlags::BigIntLike)
            || includes.intersects(TypeFlags::ESSymbolLike)
                && includes.intersects(TypeFlags::DisjointDomains & !TypeFlags::ESSymbolLike)
            || includes.intersects(TypeFlags::VoidLike)
                && includes.intersects(TypeFlags::DisjointDomains & !TypeFlags::VoidLike)
        {
            return Ok(self.never_type());
        }
        if includes.intersects(TypeFlags::TemplateLiteral)
            && includes.intersects(TypeFlags::StringLiteral)
            && self.extract_redundant_template_literals(&mut type_set)?
        {
            return Ok(self.never_type());
        }
        if includes.intersects(TypeFlags::Any) {
            return Ok(if includes.intersects(TypeFlags::IncludesWildcard) {
                self.wildcard_type()
            } else {
                self.any_type()
            });
        }
        if !self.strict_null_checks && includes.intersects(TypeFlags::Nullable) {
            return Ok(if includes.intersects(TypeFlags::Undefined) {
                self.undefined_type()
            } else {
                self.null_type()
            });
        }
        if includes.intersects(TypeFlags::String) && includes.intersects(TypeFlags::StringLiteral)
            || includes.intersects(TypeFlags::Number)
                && includes.intersects(TypeFlags::NumberLiteral)
            || includes.intersects(TypeFlags::BigInt)
                && includes.intersects(TypeFlags::BigIntLiteral)
            || includes.intersects(TypeFlags::ESSymbol)
                && includes.intersects(TypeFlags::UniqueESSymbol)
        {
            self.remove_redundant_primitive_types(&mut type_set, includes);
        }
        if includes.intersects(TypeFlags::IncludesEmptyObject)
            && includes.intersects(TypeFlags::Object)
        {
            let index = try_find_index(
                &type_set,
                |&type_: &Id<Type>, _| self.is_empty_anonymous_object_type(type_),
                None,
            )?
            .unwrap();
            ordered_remove_item_at(&mut type_set, index);
        }
        if includes.intersects(TypeFlags::IncludesMissingType) {
            let index: usize = index_of_eq(&type_set, &self.undefined_type())
                .try_into()
                .unwrap();
            type_set[index] = self.missing_type();
        }
        if type_set.is_empty() {
            return Ok(self.unknown_type());
        }
        if type_set.len() == 1 {
            return Ok(type_set[0].clone());
        }
        let id = format!(
            "{}{}",
            self.get_type_list_id(Some(&type_set)),
            self.get_alias_id(alias_symbol, alias_type_arguments)
        );
        let mut result = self.intersection_types().get(&id).map(Clone::clone);
        if result.is_none() {
            if includes.intersects(TypeFlags::Union) {
                if self.intersect_unions_of_primitive_types(&mut type_set) {
                    result = Some(self.get_intersection_type(
                        &type_set,
                        alias_symbol,
                        alias_type_arguments,
                    )?);
                } else if self.each_is_union_containing(&type_set, TypeFlags::Undefined) {
                    let undefined_or_missing_type =
                        if matches!(self.exact_optional_property_types, Some(true))
                            && some(
                                Some(&type_set),
                                Some(|&t: &Id<Type>| {
                                    self.contains_type(
                                        t.ref_(self).as_union_type().types(),
                                        self.missing_type(),
                                    )
                                }),
                            )
                        {
                            self.missing_type()
                        } else {
                            self.undefined_type()
                        };
                    self.remove_from_each(&mut type_set, TypeFlags::Undefined);
                    result = Some(self.get_union_type(
                        &[
                            self.get_intersection_type(
                                &type_set,
                                Option::<Id<Symbol>>::None,
                                None,
                            )?,
                            undefined_or_missing_type,
                        ],
                        Some(UnionReduction::Literal),
                        alias_symbol,
                        alias_type_arguments,
                        None,
                    )?);
                } else if self.each_is_union_containing(&type_set, TypeFlags::Null) {
                    self.remove_from_each(&mut type_set, TypeFlags::Null);
                    result = Some(self.get_union_type(
                        &[
                            self.get_intersection_type(
                                &type_set,
                                Option::<Id<Symbol>>::None,
                                None,
                            )?,
                            self.null_type(),
                        ],
                        Some(UnionReduction::Literal),
                        alias_symbol,
                        alias_type_arguments,
                        None,
                    )?);
                } else {
                    if !self.check_cross_product_union(&type_set) {
                        return Ok(self.error_type());
                    }
                    let constituents = self.get_cross_product_intersections(&type_set)?;
                    let origin = if some(
                        Some(&constituents),
                        Some(|&t: &Id<Type>| {
                            t.ref_(self).flags().intersects(TypeFlags::Intersection)
                        }),
                    ) {
                        Some(self.create_origin_union_or_intersection_type(
                            TypeFlags::Intersection,
                            type_set,
                        ))
                    } else {
                        None
                    };
                    result = Some(self.get_union_type(
                        &constituents,
                        Some(UnionReduction::Literal),
                        alias_symbol,
                        alias_type_arguments,
                        origin,
                    )?);
                }
            } else {
                result = Some(self.create_intersection_type(
                    type_set,
                    alias_symbol,
                    alias_type_arguments,
                ));
            }
            self.intersection_types()
                .insert(id, result.clone().unwrap());
        }
        Ok(result.unwrap())
    }

    pub(super) fn get_cross_product_union_size(&self, types: &[Id<Type>]) -> usize {
        reduce_left(
            types,
            |n, &t: &Id<Type>, _| {
                if t.ref_(self).flags().intersects(TypeFlags::Union) {
                    n * t.ref_(self).as_union_type().types().len()
                } else if t.ref_(self).flags().intersects(TypeFlags::Never) {
                    0
                } else {
                    n
                }
            },
            1,
            None,
            None,
        )
    }

    pub(super) fn check_cross_product_union(&self, types: &[Id<Type>]) -> bool {
        let size = self.get_cross_product_union_size(types);
        if size >= 100000 {
            // tracing?.instant(tracing.Phase.CheckTypes, "checkCrossProductUnion_DepthLimit", { typeIds: types.map(t => t.id), size });
            self.error(
                self.maybe_current_node(),
                &Diagnostics::Expression_produces_a_union_type_that_is_too_complex_to_represent,
                None,
            );
            return false;
        }
        true
    }

    pub(super) fn get_cross_product_intersections(
        &self,
        types: &[Id<Type>],
    ) -> io::Result<Vec<Id<Type>>> {
        let count = self.get_cross_product_union_size(types);
        let mut intersections: Vec<Id<Type>> = vec![];
        let mut did_manually_allocate = false;
        for i in 0..count {
            let mut constituents = Cow::Borrowed(types);
            let mut n = i;
            let mut j: isize = (types.len() - 1).try_into().unwrap();
            while j >= 0 {
                let j_as_usize: usize = j.try_into().unwrap();
                if types[j_as_usize]
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::Union)
                {
                    let types_j_ref = types[j_as_usize].ref_(self);
                    let source_types = types_j_ref.as_union_type().types();
                    let length = source_types.len();
                    constituents.to_mut()[j_as_usize] = source_types[n % length].clone();
                    n = (n as f64 / length as f64).floor() as usize;
                }
                j -= 1;
            }
            let t = self.get_intersection_type(&constituents, Option::<Id<Symbol>>::None, None)?;
            if !t.ref_(self).flags().intersects(TypeFlags::Never) {
                // in one project it looked like often the length of intersections either ends up
                // being 1 or a number close to or equal to `count`, so defer allocating space for
                // `count` items until we're seeing our second item
                if intersections.len() == 1 {
                    // println!("get_cross_product_intersections() allocating");
                    intersections.reserve_exact(count - i - 1);
                    did_manually_allocate = true;
                }
                intersections.push(t);
            }
        }
        let should_release_extra_allocation = did_manually_allocate
            && count > 20
            && (count as f64 / intersections.len() as f64) < 0.25;
        if should_release_extra_allocation {
            // println!("get_cross_product_intersections() releasing over-allocation (allocated {}, used {})", intersections.capacity(), intersections.len());
            intersections.shrink_to_fit();
        }
        Ok(intersections)
    }

    pub(super) fn get_type_from_intersection_type_node(
        &self,
        node: Id<Node>, /*IntersectionTypeNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            let alias_symbol = self.get_alias_symbol_for_type_node(node)?;
            links.ref_mut(self).resolved_type = Some(
                self.get_intersection_type(
                    &try_map(
                        &*node.ref_(self).as_intersection_type_node().types.ref_(self),
                        |&type_: &Id<Node>, _| self.get_type_from_type_node_(type_),
                    )?,
                    alias_symbol,
                    self.get_type_arguments_for_alias_symbol(alias_symbol)?
                        .as_deref(),
                )?,
            );
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn create_index_type(
        &self,
        type_: Id<Type>, /*InstantiableType | UnionOrIntersectionType*/
        strings_only: bool,
    ) -> Id<Type> {
        let result = self.create_type(TypeFlags::Index);
        let result = self.alloc_type(IndexType::new(result, type_, strings_only).into());
        result
    }

    pub(super) fn create_origin_index_type(
        &self,
        type_: Id<Type>, /*InstantiableType | UnionOrIntersectionType*/
    ) -> Id<Type> {
        let result = self.create_origin_type(TypeFlags::Index);
        let result =
            self.alloc_type(IndexType::new(result, type_, false /*this is made up*/).into());
        result
    }

    pub(super) fn get_index_type_for_generic_type(
        &self,
        type_: Id<Type>, /*InstantiableType | UnionOrIntersectionType*/
        strings_only: bool,
    ) -> Id<Type> {
        if strings_only {
            let type_ref = type_.ref_(self);
            let mut type_resolved_string_index_type = type_ref.maybe_resolved_string_index_type();
            if type_resolved_string_index_type.is_none() {
                *type_resolved_string_index_type = Some(self.create_index_type(type_, true));
            }
            type_resolved_string_index_type.clone().unwrap()
        } else {
            if type_.ref_(self).maybe_resolved_index_type().is_none() {
                *type_.ref_(self).maybe_resolved_index_type() =
                    Some(self.create_index_type(type_, false));
            }
            type_
                .ref_(self)
                .maybe_resolved_index_type()
                .clone()
                .unwrap()
        }
    }

    pub(super) fn get_index_type_for_mapped_type(
        &self,
        type_: Id<Type>, /*MappedType*/
        strings_only: bool,
        no_index_signatures: Option<bool>,
    ) -> io::Result<Id<Type>> {
        let type_parameter = self.get_type_parameter_from_mapped_type(type_)?;
        let constraint_type = self.get_constraint_type_from_mapped_type(type_)?;
        let name_type = self.get_name_type_from_mapped_type(
            type_
                .ref_(self)
                .as_mapped_type()
                .maybe_target()
                .unwrap_or_else(|| type_),
        )?;
        if name_type.is_none() && !matches!(no_index_signatures, Some(true)) {
            return Ok(constraint_type);
        }
        let mut key_types: Vec<Id<Type>> = vec![];
        if self.is_mapped_type_with_keyof_constraint_declaration(type_) {
            if !self.is_generic_index_type(constraint_type)? {
                let modifiers_type =
                    self.get_apparent_type(self.get_modifiers_type_from_mapped_type(type_)?)?;
                self.try_for_each_mapped_type_property_key_type_and_index_signature_key_type(
                    modifiers_type,
                    TypeFlags::StringOrNumberLiteralOrUnique,
                    strings_only,
                    |t| {
                        self.add_member_for_key_type_get_index_type_for_mapped_type(
                            name_type,
                            type_parameter,
                            &mut key_types,
                            type_,
                            t,
                        )
                    },
                )?;
            } else {
                return Ok(self.get_index_type_for_generic_type(type_, strings_only));
            }
        } else {
            self.try_for_each_type(self.get_lower_bound_of_key_type(constraint_type)?, |t| {
                self.add_member_for_key_type_get_index_type_for_mapped_type(
                    name_type,
                    type_parameter,
                    &mut key_types,
                    type_,
                    t,
                )?;
                Ok(Option::<()>::None)
            })?;
        }
        if self.is_generic_index_type(constraint_type)? {
            self.try_for_each_type(constraint_type, |t| {
                self.add_member_for_key_type_get_index_type_for_mapped_type(
                    name_type,
                    type_parameter,
                    &mut key_types,
                    type_,
                    t,
                )?;
                Ok(Option::<()>::None)
            })?;
        }
        let result = if matches!(no_index_signatures, Some(true)) {
            self.filter_type(
                self.get_union_type(&key_types, None, Option::<Id<Symbol>>::None, None, None)?,
                |t| {
                    !t.ref_(self)
                        .flags()
                        .intersects(TypeFlags::Any | TypeFlags::String)
                },
            )
        } else {
            self.get_union_type(&key_types, None, Option::<Id<Symbol>>::None, None, None)?
        };
        if result.ref_(self).flags().intersects(TypeFlags::Union)
            && constraint_type
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Union)
            && self.get_type_list_id(Some(result.ref_(self).as_union_type().types()))
                == self.get_type_list_id(Some(constraint_type.ref_(self).as_union_type().types()))
        {
            return Ok(constraint_type);
        }
        Ok(result)
    }

    pub(super) fn add_member_for_key_type_get_index_type_for_mapped_type(
        &self,
        name_type: Option<Id<Type>>,
        type_parameter: Id<Type>,
        key_types: &mut Vec<Id<Type>>,
        type_: Id<Type>,
        key_type: Id<Type>,
    ) -> io::Result<()> {
        let prop_name_type = if let Some(name_type) = name_type {
            self.instantiate_type(
                name_type,
                Some(self.append_type_mapping(
                    type_.ref_(self).as_mapped_type().maybe_mapper(),
                    type_parameter,
                    key_type,
                )),
            )?
        } else {
            key_type
        };
        key_types.push(if prop_name_type == self.string_type() {
            self.string_or_number_type()
        } else {
            prop_name_type
        });

        Ok(())
    }

    pub(super) fn has_distributive_name_type(
        &self,
        mapped_type: Id<Type>, /*MappedType*/
    ) -> io::Result<bool> {
        let type_variable = self.get_type_parameter_from_mapped_type(mapped_type)?;
        Ok(self.is_distributive(
            type_variable,
            self.get_name_type_from_mapped_type(mapped_type)?
                .unwrap_or_else(|| type_variable.clone()),
        ))
    }

    pub(super) fn is_distributive(&self, type_variable: Id<Type>, type_: Id<Type>) -> bool {
        if type_.ref_(self).flags().intersects(
            TypeFlags::AnyOrUnknown
                | TypeFlags::Primitive
                | TypeFlags::Never
                | TypeFlags::TypeParameter
                | TypeFlags::Object
                | TypeFlags::NonPrimitive,
        ) {
            true
        } else if type_.ref_(self).flags().intersects(TypeFlags::Conditional) {
            type_.ref_(self).as_conditional_type().root
                .ref_(self)
                .is_distributive
                && type_.ref_(self).as_conditional_type().check_type == type_variable
        } else if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection | TypeFlags::TemplateLiteral)
        {
            let type_ref = type_.ref_(self);
            every(
                if type_
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::UnionOrIntersection)
                {
                    type_ref.as_union_or_intersection_type_interface().types()
                } else {
                    &*type_ref.as_template_literal_type().types
                },
                |&t: &Id<Type>, _| self.is_distributive(type_variable, t),
            )
        } else if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::IndexedAccess)
        {
            self.is_distributive(
                type_variable,
                type_.ref_(self).as_indexed_access_type().object_type,
            ) && self.is_distributive(
                type_variable,
                type_.ref_(self).as_indexed_access_type().index_type,
            )
        } else if type_.ref_(self).flags().intersects(TypeFlags::Substitution) {
            self.is_distributive(
                type_variable,
                type_.ref_(self).as_substitution_type().substitute,
            )
        } else if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::StringMapping)
        {
            self.is_distributive(
                type_variable,
                type_.ref_(self).as_string_mapping_type().type_,
            )
        } else {
            false
        }
    }

    pub(super) fn get_literal_type_from_property_name(
        &self,
        name: Id<Node>, /*PropertyName*/
    ) -> io::Result<Id<Type>> {
        if is_private_identifier(&name.ref_(self)) {
            return Ok(self.never_type());
        }
        Ok(if is_identifier(&name.ref_(self)) {
            self.get_string_literal_type(&unescape_leading_underscores(
                &name.ref_(self).as_identifier().escaped_text,
            ))
        } else {
            self.get_regular_type_of_literal_type(if is_computed_property_name(&name.ref_(self)) {
                self.check_computed_property_name(name)?
            } else {
                self.check_expression(name, None, None)?
            })
        })
    }

    pub(super) fn get_literal_type_from_property(
        &self,
        prop: Id<Symbol>,
        include: TypeFlags,
        include_non_public: Option<bool>,
    ) -> io::Result<Id<Type>> {
        let include_non_public = include_non_public.unwrap_or(false);
        if include_non_public
            || !get_declaration_modifier_flags_from_symbol(prop, None, self)
                .intersects(ModifierFlags::NonPublicAccessibilityModifier)
        {
            let mut type_ = (*self.get_symbol_links(self.get_late_bound_symbol(prop)?).ref_(self))
                .borrow()
                .name_type
                .clone();
            if type_.is_none() {
                let name = get_name_of_declaration(prop.ref_(self).maybe_value_declaration(), self);
                type_ = if prop.ref_(self).escaped_name() == InternalSymbolName::Default {
                    Some(self.get_string_literal_type("default"))
                } else if let Some(name) = name {
                    Some(self.get_literal_type_from_property_name(name)?)
                } else {
                    if !is_known_symbol(&prop.ref_(self)) {
                        Some(self.get_string_literal_type(&symbol_name(prop, self)))
                    } else {
                        None
                    }
                }
            }
            if let Some(type_) = type_ {
                if type_.ref_(self).flags().intersects(include) {
                    return Ok(type_);
                }
            }
        }
        Ok(self.never_type())
    }

    pub(super) fn is_key_type_included(&self, key_type: Id<Type>, include: TypeFlags) -> bool {
        key_type.ref_(self).flags().intersects(include)
            || key_type
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Intersection)
                && some(
                    Some(key_type.ref_(self).as_intersection_type().types()),
                    Some(|&t: &Id<Type>| self.is_key_type_included(t, include)),
                )
    }

    pub(super) fn get_literal_type_from_properties(
        &self,
        type_: Id<Type>,
        include: TypeFlags,
        include_origin: bool,
    ) -> io::Result<Id<Type>> {
        let origin = if include_origin
            && (get_object_flags(&type_.ref_(self))
                .intersects(ObjectFlags::ClassOrInterface | ObjectFlags::Reference)
                || type_.ref_(self).maybe_alias_symbol().is_some())
        {
            Some(self.create_origin_index_type(type_))
        } else {
            None
        };
        let property_types = self
            .get_properties_of_type(type_)?
            .map(|prop| self.get_literal_type_from_property(prop, include, None))
            .collect::<Result<Vec<_>, _>>()?;
        let index_infos = self.get_index_infos_of_type(type_)?;
        let index_key_types = index_infos.iter().map(|&info| {
            if info != self.enum_number_index_info()
                && self.is_key_type_included(info.ref_(self).key_type, include)
            {
                if info.ref_(self).key_type == self.string_type() && include.intersects(TypeFlags::Number) {
                    self.string_or_number_type()
                } else {
                    info.ref_(self).key_type.clone()
                }
            } else {
                self.never_type()
            }
        });
        self.get_union_type(
            &property_types.and_extend(index_key_types),
            Some(UnionReduction::Literal),
            Option::<Id<Symbol>>::None,
            None,
            origin,
        )
    }

    pub(super) fn get_index_type(
        &self,
        type_: Id<Type>,
        strings_only: Option<bool>,
        no_index_signatures: Option<bool>,
    ) -> io::Result<Id<Type>> {
        let strings_only = strings_only.unwrap_or(self.keyof_strings_only);
        let type_ = self.get_reduced_type(type_)?;
        Ok(if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            self.get_intersection_type(
                &try_map(
                    type_.ref_(self).as_union_type().types(),
                    |&t: &Id<Type>, _| {
                        self.get_index_type(t, Some(strings_only), no_index_signatures)
                    },
                )?,
                Option::<Id<Symbol>>::None,
                None,
            )?
        } else if type_.ref_(self).flags().intersects(TypeFlags::Intersection) {
            self.get_union_type(
                &try_map(
                    &{
                        let types = type_.ref_(self).as_intersection_type().types().to_owned();
                        types
                    },
                    |&t: &Id<Type>, _| {
                        self.get_index_type(t, Some(strings_only), no_index_signatures)
                    },
                )?,
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?
        } else if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::InstantiableNonPrimitive)
            || self.is_generic_tuple_type(type_)
            || self.is_generic_mapped_type(type_)? && !self.has_distributive_name_type(type_)?
        {
            self.get_index_type_for_generic_type(type_, strings_only)
        } else if get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::Mapped) {
            self.get_index_type_for_mapped_type(type_, strings_only, no_index_signatures)?
        } else if type_ == self.wildcard_type() {
            self.wildcard_type()
        } else if type_.ref_(self).flags().intersects(TypeFlags::Unknown) {
            self.never_type()
        } else if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Any | TypeFlags::Never)
        {
            self.keyof_constraint_type()
        } else {
            self.get_literal_type_from_properties(
                type_,
                (if matches!(no_index_signatures, Some(true)) {
                    TypeFlags::StringLiteral
                } else {
                    TypeFlags::StringLike
                }) | if strings_only {
                    TypeFlags::None
                } else {
                    TypeFlags::NumberLike | TypeFlags::ESSymbolLike
                },
                strings_only == self.keyof_strings_only
                    && !matches!(no_index_signatures, Some(true)),
            )?
        })
    }

    pub(super) fn get_extract_string_type(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        if self.keyof_strings_only {
            return Ok(type_);
        }
        let extract_type_alias = self.get_global_extract_symbol()?;
        Ok(if let Some(extract_type_alias) = extract_type_alias {
            self.get_type_alias_instantiation(
                extract_type_alias,
                Some(&[type_, self.string_type()]),
                Option::<Id<Symbol>>::None,
                None,
            )?
        } else {
            self.string_type()
        })
    }

    pub(super) fn get_index_type_or_string(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        let index_type = self.get_extract_string_type(self.get_index_type(type_, None, None)?)?;
        Ok(
            if index_type.ref_(self).flags().intersects(TypeFlags::Never) {
                self.string_type()
            } else {
                index_type
            },
        )
    }

    pub(super) fn get_type_from_type_operator_node(
        &self,
        node: Id<Node>, /*TypeOperatorNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            let node_ref = node.ref_(self);
            let node_as_type_operator_node = node_ref.as_type_operator_node();
            match node_as_type_operator_node.operator {
                SyntaxKind::KeyOfKeyword => {
                    links.ref_mut(self).resolved_type = Some(self.get_index_type(
                        self.get_type_from_type_node_(node_as_type_operator_node.type_)?,
                        None,
                        None,
                    )?);
                }
                SyntaxKind::UniqueKeyword => {
                    links.ref_mut(self).resolved_type = Some(
                        if node_as_type_operator_node.type_.ref_(self).kind() == SyntaxKind::SymbolKeyword {
                            self.get_es_symbol_like_type_for_node(
                                walk_up_parenthesized_types(node.ref_(self).parent(), self).unwrap(),
                            )?
                        } else {
                            self.error_type()
                        },
                    );
                }
                SyntaxKind::ReadonlyKeyword => {
                    links.ref_mut(self).resolved_type =
                        Some(self.get_type_from_type_node_(node_as_type_operator_node.type_)?);
                }
                _ => {
                    Debug_.assert_never(node_as_type_operator_node.operator, None);
                }
            }
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_type_from_template_type_node(
        &self,
        node: Id<Node>, /*TemplateLiteralTypeNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            let node_ref = node.ref_(self);
            let node_as_template_literal_type_node = node_ref.as_template_literal_type_node();
            let mut texts = vec![node_as_template_literal_type_node
                .head
                .ref_(self).as_literal_like_node()
                .text()
                .clone()];
            texts.extend(
                map(
                    &*node_as_template_literal_type_node.template_spans.ref_(self),
                    |span: &Id<Node>, _| {
                        span.ref_(self).as_template_literal_type_span()
                            .literal
                            .ref_(self).as_literal_like_node()
                            .text()
                            .clone()
                    },
                )
                .into_iter(),
            );
            links.ref_mut(self).resolved_type = Some(self.get_template_literal_type(
                &texts,
                &try_map(
                    &*node_as_template_literal_type_node.template_spans.ref_(self),
                    |span: &Id<Node>, _| {
                        self.get_type_from_type_node_(span.ref_(self).as_template_literal_type_span().type_)
                    },
                )?,
            )?);
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_template_literal_type(
        &self,
        texts: &[String],
        types: &[Id<Type>],
    ) -> io::Result<Id<Type>> {
        let union_index = find_index(
            types,
            |&t: &Id<Type>, _| {
                t.ref_(self)
                    .flags()
                    .intersects(TypeFlags::Never | TypeFlags::Union)
            },
            None,
        );
        if let Some(union_index) = union_index {
            return Ok(if self.check_cross_product_union(types) {
                self.try_map_type(
                    types[union_index],
                    &mut |t| {
                        Ok(Some(self.get_template_literal_type(
                            texts,
                            &replace_element(types, union_index, t),
                        )?))
                    },
                    None,
                )?
                .unwrap()
            } else {
                self.error_type()
            });
        }
        if contains(Some(types), &self.wildcard_type()) {
            return Ok(self.wildcard_type());
        }
        let mut new_types: Vec<Id<Type>> = vec![];
        let mut new_texts: Vec<String> = vec![];
        let mut text = texts[0].clone();
        if !self.add_spans(&mut text, &mut new_types, &mut new_texts, texts, types)? {
            return Ok(self.string_type());
        }
        if new_types.is_empty() {
            return Ok(self.get_string_literal_type(&text));
        }
        new_texts.push(text);
        if every(&new_texts, |t: &String, _| t == "")
            && every(&new_types, |&t: &Id<Type>, _| {
                t.ref_(self).flags().intersects(TypeFlags::String)
            })
        {
            return Ok(self.string_type());
        }
        let id = format!(
            "{}|{}|{}",
            self.get_type_list_id(Some(&new_types)),
            map(&new_texts, |t: &String, _| t.len().to_string()).join(","),
            new_texts.join("")
        );
        let mut type_ = self.template_literal_types().get(&id).map(Clone::clone);
        if type_.is_none() {
            type_ = Some(self.create_template_literal_type(new_texts, new_types));
            self.template_literal_types()
                .insert(id, type_.clone().unwrap());
        }
        Ok(type_.unwrap())
    }

    pub(super) fn add_spans(
        &self,
        text: &mut String,
        new_types: &mut Vec<Id<Type>>,
        new_texts: &mut Vec<String>,
        texts: &[String],
        types: &[Id<Type>],
    ) -> io::Result<bool> {
        for (i, t) in types.iter().enumerate() {
            let t = *t;
            if t.ref_(self)
                .flags()
                .intersects(TypeFlags::Literal | TypeFlags::Null | TypeFlags::Undefined)
            {
                text.push_str(
                    &self
                        .get_template_string_for_type(t)
                        .unwrap_or_else(|| "".to_owned()),
                );
                text.push_str(&texts[i + 1]);
            } else if t.ref_(self).flags().intersects(TypeFlags::TemplateLiteral) {
                text.push_str(&t.ref_(self).as_template_literal_type().texts[0]);
                if !self.add_spans(
                    text,
                    new_types,
                    new_texts,
                    &t.ref_(self).as_template_literal_type().texts,
                    &t.ref_(self).as_template_literal_type().types,
                )? {
                    return Ok(false);
                }
                text.push_str(&texts[i + 1]);
            } else if self.is_generic_index_type(t)? || self.is_pattern_literal_placeholder_type(t)
            {
                new_types.push(t.clone());
                new_texts.push(text.clone());
                *text = texts[i + 1].clone();
            } else {
                return Ok(false);
            }
        }
        Ok(true)
    }
}
