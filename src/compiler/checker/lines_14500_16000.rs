#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    contains_rc, every, filter, find_index, get_object_flags, index_of_rc, ordered_remove_item_at,
    some, AccessFlags, BaseUnionOrIntersectionType, IntersectionType, ObjectFlags,
    UnionOrIntersectionTypeInterface, UnionReduction, __String, get_name_of_declaration,
    unescape_leading_underscores, Node, Symbol, SymbolInterface, Type, TypeChecker, TypeFlags,
    TypeInterface,
};

impl TypeChecker {
    pub(super) fn each_union_contains(
        &self,
        union_types: &[Rc<Type /*UnionType*/>],
        type_: &Type,
    ) -> bool {
        for u in union_types {
            let u_as_union_type = u.as_union_type();
            if !self.contains_type(u_as_union_type.types(), type_) {
                let primitive = if type_.flags().intersects(TypeFlags::StringLiteral) {
                    Some(self.string_type())
                } else if type_.flags().intersects(TypeFlags::NumberLiteral) {
                    Some(self.number_type())
                } else if type_.flags().intersects(TypeFlags::BigIntLiteral) {
                    Some(self.bigint_type())
                } else if type_.flags().intersects(TypeFlags::UniqueESSymbol) {
                    Some(self.es_symbol_type())
                } else {
                    None
                };
                if match primitive.as_ref() {
                    None => true,
                    Some(primitive) => !self.contains_type(u_as_union_type.types(), primitive),
                } {
                    return false;
                }
            }
        }
        true
    }

    pub(super) fn extract_redundant_template_literals(&self, types: &mut Vec<Rc<Type>>) -> bool {
        let mut i = types.len();
        let literals = filter(Some(types), |t: &Rc<Type>| {
            t.flags().intersects(TypeFlags::StringLiteral)
        })
        .unwrap();
        while i > 0 {
            i -= 1;
            let t = types[i].clone();
            if !t.flags().intersects(TypeFlags::TemplateLiteral) {
                continue;
            }
            for t2 in &literals {
                if self.is_type_subtype_of(t2, &t) {
                    ordered_remove_item_at(types, i);
                    break;
                } else if self.is_pattern_literal_type(&t) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn each_is_union_containing(&self, types: &[Rc<Type>], flag: TypeFlags) -> bool {
        every(types, |t: &Rc<Type>, _| {
            t.flags().intersects(TypeFlags::Union)
                && some(
                    Some(t.as_union_type().types()),
                    Some(|tt: &Rc<Type>| tt.flags().intersects(flag)),
                )
        })
    }

    pub(super) fn remove_from_each(&self, types: &mut Vec<Rc<Type>>, flag: TypeFlags) {
        for i in 0..types.len() {
            types[i] = self.filter_type(&types[i].clone(), |t: &Type| !t.flags().intersects(flag));
        }
    }

    pub(super) fn intersect_unions_of_primitive_types(&self, types: &mut Vec<Rc<Type>>) -> bool {
        let mut union_types: Option<Vec<Rc<Type /*UnionType*/>>> = None;
        let index = find_index(
            types,
            |t: &Rc<Type>, _| get_object_flags(t).intersects(ObjectFlags::PrimitiveUnion),
            None,
        );
        if index.is_none() {
            return false;
        }
        let index = index.unwrap();
        let mut i = index + 1;
        while i < types.len() {
            let t = types[i].clone();
            if get_object_flags(&t).intersects(ObjectFlags::PrimitiveUnion) {
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
        let mut checked: Vec<Rc<Type>> = vec![];
        let mut result: Vec<Rc<Type>> = vec![];
        for u in &union_types {
            for t in u.as_union_type().types() {
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
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        );
        true
    }

    pub(super) fn create_intersection_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        types: Vec<Rc<Type>>,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let result = self.create_type(TypeFlags::Intersection);
        let object_flags = self.get_propagating_flags_of_types(&types, TypeFlags::Nullable);
        let result = BaseUnionOrIntersectionType::new(result, types, object_flags);
        let result: Rc<Type> = IntersectionType::new(result).into();
        *result.maybe_alias_symbol() =
            alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
        *result.maybe_alias_type_arguments() = alias_type_arguments.map(ToOwned::to_owned);
        result
    }

    pub(super) fn get_intersection_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        types: &[Rc<Type>],
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let mut type_membership_map: HashMap<String, Rc<Type>> = HashMap::new();
        let includes =
            self.add_types_to_intersection(&mut type_membership_map, TypeFlags::None, types);
        let mut type_set: Vec<Rc<Type>> = type_membership_map.values().map(Clone::clone).collect();
        if includes.intersects(TypeFlags::Never) {
            return if contains_rc(Some(&type_set), &self.silent_never_type()) {
                self.silent_never_type()
            } else {
                self.never_type()
            };
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
            return self.never_type();
        }
        if includes.intersects(TypeFlags::TemplateLiteral)
            && includes.intersects(TypeFlags::StringLiteral)
            && self.extract_redundant_template_literals(&mut type_set)
        {
            return self.never_type();
        }
        if includes.intersects(TypeFlags::Any) {
            return if includes.intersects(TypeFlags::IncludesWildcard) {
                self.wildcard_type()
            } else {
                self.any_type()
            };
        }
        if !self.strict_null_checks && includes.intersects(TypeFlags::Nullable) {
            return if includes.intersects(TypeFlags::Undefined) {
                self.undefined_type()
            } else {
                self.null_type()
            };
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
            let index = find_index(
                &type_set,
                |type_: &Rc<Type>, _| self.is_empty_anonymous_object_type(type_),
                None,
            )
            .unwrap();
            ordered_remove_item_at(&mut type_set, index);
        }
        if includes.intersects(TypeFlags::IncludesMissingType) {
            let index: usize = index_of_rc(&type_set, &self.undefined_type())
                .try_into()
                .unwrap();
            type_set[index] = self.missing_type();
        }
        if type_set.is_empty() {
            return self.unknown_type();
        }
        if type_set.len() == 1 {
            return type_set[0].clone();
        }
        let alias_symbol = alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
        let id = format!(
            "{}{}",
            self.get_type_list_id(Some(&type_set)),
            self.get_alias_id(alias_symbol.as_deref(), alias_type_arguments)
        );
        let mut result = self.intersection_types().get(&id).map(Clone::clone);
        if result.is_none() {
            if includes.intersects(TypeFlags::Union) {
                if self.intersect_unions_of_primitive_types(&mut type_set) {
                    result = Some(self.get_intersection_type(
                        &type_set,
                        alias_symbol.as_deref(),
                        alias_type_arguments,
                    ));
                } else if self.each_is_union_containing(&type_set, TypeFlags::Undefined) {
                    let undefined_or_missing_type =
                        if matches!(self.exact_optional_property_types, Some(true))
                            && some(
                                Some(&type_set),
                                Some(|t: &Rc<Type>| {
                                    self.contains_type(
                                        t.as_union_type().types(),
                                        &self.missing_type(),
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
                        vec![
                            self.get_intersection_type(&type_set, Option::<&Symbol>::None, None),
                            undefined_or_missing_type,
                        ],
                        Some(UnionReduction::Literal),
                        alias_symbol.as_deref(),
                        alias_type_arguments,
                        Option::<&Type>::None,
                    ));
                } else if self.each_is_union_containing(&type_set, TypeFlags::Null) {
                    self.remove_from_each(&mut type_set, TypeFlags::Null);
                    result = Some(self.get_union_type(
                        vec![
                            self.get_intersection_type(&type_set, Option::<&Symbol>::None, None),
                            self.null_type(),
                        ],
                        Some(UnionReduction::Literal),
                        alias_symbol.as_deref(),
                        alias_type_arguments,
                        Option::<&Type>::None,
                    ));
                } else {
                    if !self.check_cross_product_union(&type_set) {
                        return self.error_type();
                    }
                    let constituents = self.get_cross_product_intersections(&type_set);
                    let origin = if some(
                        Some(&constituents),
                        Some(|t: &Rc<Type>| t.flags().intersects(TypeFlags::Intersection)),
                    ) {
                        Some(self.create_origin_union_or_intersection_type(
                            TypeFlags::Intersection,
                            type_set,
                        ))
                    } else {
                        None
                    };
                    result = Some(self.get_union_type(
                        constituents,
                        Some(UnionReduction::Literal),
                        alias_symbol.as_deref(),
                        alias_type_arguments,
                        origin,
                    ));
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
        result.unwrap()
    }

    pub(super) fn check_cross_product_union(&self, types: &[Rc<Type>]) -> bool {
        unimplemented!()
    }

    pub(super) fn get_cross_product_intersections(&self, types: &[Rc<Type>]) -> Vec<Rc<Type>> {
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
