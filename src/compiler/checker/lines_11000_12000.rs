#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::MappedTypeModifiers;
use crate::{
    add_range, append, concatenate, count_where, create_symbol_table, every, index_of, map,
    map_defined, reduce_left, some, IndexInfo, InternalSymbolName, ObjectFlags,
    ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvedTypeInterface, Signature,
    SignatureFlags, SignatureKind, Symbol, SymbolFlags, SymbolInterface, SymbolTable, Ternary,
    TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    UnionOrIntersectionTypeInterface, __String,
};

impl TypeChecker {
    pub(super) fn combine_union_this_param<TLeft: Borrow<Symbol>, TRight: Borrow<Symbol>>(
        &self,
        left: Option<TLeft>,
        right: Option<TRight>,
        mapper: Option<&TypeMapper>,
    ) -> Option<Rc<Symbol>> {
        let left = left.map(|left| left.borrow().symbol_wrapper());
        let right = right.map(|right| right.borrow().symbol_wrapper());
        if left.is_none() || right.is_none() {
            return left.or(right);
        }
        let left = left.unwrap();
        let right = right.unwrap();
        let this_type = self.get_intersection_type(
            &vec![
                self.get_type_of_symbol(&left),
                self.instantiate_type(Some(self.get_type_of_symbol(&right)), mapper)
                    .unwrap(),
            ],
            Option::<&Symbol>::None,
            None,
        );
        Some(self.create_symbol_with_type(&left, Some(this_type)))
    }

    pub(super) fn combine_union_parameters(
        &self,
        left: &Signature,
        right: &Signature,
        mapper: Option<&TypeMapper>,
    ) -> Vec<Rc<Symbol>> {
        let left_count = self.get_parameter_count(left);
        let right_count = self.get_parameter_count(right);
        let longest = if left_count >= right_count {
            left
        } else {
            right
        };
        let shorter = if ptr::eq(longest, left) { right } else { left };
        let longest_count = if ptr::eq(longest, left) {
            left_count
        } else {
            right_count
        };
        let either_has_effective_rest =
            self.has_effective_rest_parameter(left) || self.has_effective_rest_parameter(right);
        let needs_extra_rest_element =
            either_has_effective_rest && !self.has_effective_rest_parameter(longest);
        let mut params: Vec<Rc<Symbol>> =
            Vec::with_capacity(longest_count + if needs_extra_rest_element { 1 } else { 0 });
        for i in 0..longest_count {
            let mut longest_param_type = self.try_get_type_at_position(longest, i).unwrap();
            if ptr::eq(longest, right) {
                longest_param_type = self
                    .instantiate_type(Some(longest_param_type), mapper)
                    .unwrap();
            }
            let mut shorter_param_type = self
                .try_get_type_at_position(shorter, i)
                .unwrap_or_else(|| self.unknown_type());
            if ptr::eq(shorter, right) {
                shorter_param_type = self
                    .instantiate_type(Some(shorter_param_type), mapper)
                    .unwrap();
            }
            let union_param_type = self.get_intersection_type(
                &vec![longest_param_type, shorter_param_type],
                Option::<&Symbol>::None,
                None,
            );
            let is_rest_param =
                either_has_effective_rest && !needs_extra_rest_element && i == longest_count - 1;
            let is_optional = i >= self.get_min_argument_count(longest, None)
                && i >= self.get_min_argument_count(shorter, None);
            let left_name = if i >= left_count {
                None
            } else {
                Some(self.get_parameter_name_at_position(left, i, Option::<&Type>::None))
            };
            let right_name = if i >= right_count {
                None
            } else {
                Some(self.get_parameter_name_at_position(right, i, Option::<&Type>::None))
            };

            let param_name = if left_name == right_name {
                left_name
            } else if left_name.is_none() {
                right_name
            } else if right_name.is_none() {
                left_name
            } else {
                None
            };
            let param_symbol: Rc<Symbol> = self
                .create_symbol(
                    SymbolFlags::FunctionScopedVariable
                        | if is_optional && !is_rest_param {
                            SymbolFlags::Optional
                        } else {
                            SymbolFlags::None
                        },
                    param_name.unwrap_or_else(|| __String::new(format!("arg{}", i))),
                    None,
                )
                .into();
            param_symbol
                .as_transient_symbol()
                .symbol_links()
                .borrow_mut()
                .type_ = Some(if is_rest_param {
                self.create_array_type(&union_param_type, None)
            } else {
                union_param_type
            });
            params.push(param_symbol);
        }
        if needs_extra_rest_element {
            let rest_param_symbol: Rc<Symbol> = self
                .create_symbol(
                    SymbolFlags::FunctionScopedVariable,
                    __String::new("args".to_owned()),
                    None,
                )
                .into();
            rest_param_symbol
                .as_transient_symbol()
                .symbol_links()
                .borrow_mut()
                .type_ = Some(
                self.create_array_type(&self.get_type_at_position(shorter, longest_count), None),
            );
            if ptr::eq(shorter, right) {
                let type_ = self.instantiate_type(
                    (*rest_param_symbol.as_transient_symbol().symbol_links())
                        .borrow()
                        .type_
                        .as_deref(),
                    mapper,
                );
                rest_param_symbol
                    .as_transient_symbol()
                    .symbol_links()
                    .borrow_mut()
                    .type_ = type_;
            }
            params.push(rest_param_symbol);
        }
        params
    }

    pub(super) fn combine_signatures_of_union_members(
        &self,
        left: Rc<Signature>,
        right: Rc<Signature>,
    ) -> Signature {
        let type_params = left
            .type_parameters
            .clone()
            .or_else(|| right.type_parameters.clone());
        let mut param_mapper: Option<TypeMapper> = None;
        if left.type_parameters.is_some() && right.type_parameters.is_some() {
            param_mapper = Some(self.create_type_mapper(
                right.type_parameters.clone().unwrap(),
                left.type_parameters.clone(),
            ));
        }
        let declaration = left.declaration.clone();
        let params = self.combine_union_parameters(&left, &right, param_mapper.as_ref());
        let this_param = self.combine_union_this_param(
            left.this_parameter.as_deref(),
            right.this_parameter.as_deref(),
            param_mapper.as_ref(),
        );
        let min_arg_count = cmp::max(left.min_argument_count(), right.min_argument_count());
        let mut result = self.create_signature(
            declaration,
            type_params,
            this_param,
            params,
            None,
            None,
            min_arg_count,
            (left.flags | right.flags) & SignatureFlags::PropagatingFlags,
        );
        result.composite_kind = Some(TypeFlags::Union);
        result.composite_signatures = Some(concatenate(
            if !matches!(left.composite_kind, Some(TypeFlags::Intersection)) {
                left.composite_signatures.clone()
            } else {
                None
            }
            .unwrap_or_else(|| vec![left.clone()]),
            vec![right],
        ));
        if let Some(param_mapper) = param_mapper {
            result.mapper = Some(
                if !matches!(left.composite_kind, Some(TypeFlags::Intersection))
                    && left.mapper.is_some()
                    && left.composite_signatures.is_some()
                {
                    self.combine_type_mappers(left.mapper.clone(), param_mapper)
                } else {
                    param_mapper
                },
            );
        }
        result
    }

    pub(super) fn get_union_index_infos(&self, types: &[Rc<Type>]) -> Vec<Rc<IndexInfo>> {
        let source_infos = self.get_index_infos_of_type(&types[0]);
        // if (sourceInfos) {
        let mut result = vec![];
        for info in source_infos {
            let index_type = &info.key_type;
            if every(types, |t: &Rc<Type>, _| {
                self.get_index_info_of_type_(t, index_type).is_some()
            }) {
                result.push(Rc::new(
                    self.create_index_info(
                        index_type.clone(),
                        self.get_union_type(
                            map(Some(types), |t: &Rc<Type>, _| {
                                self.get_index_type_of_type_(t, index_type).unwrap()
                            })
                            .unwrap(),
                            None,
                            Option::<&Symbol>::None,
                            None,
                            Option::<&Type>::None,
                        ),
                        some(
                            Some(types),
                            Some(|t: &Rc<Type>| {
                                self.get_index_info_of_type_(t, index_type)
                                    .unwrap()
                                    .is_readonly
                            }),
                        ),
                        None,
                    ),
                ));
            }
        }
        result
        // }
        // return emptyArray;
    }

    pub(super) fn resolve_union_type_members(&self, type_: &Type /*UnionType*/) {
        let type_as_union_type = type_.as_union_type();
        let call_signatures = self.get_union_signatures(
            &map(Some(type_as_union_type.types()), |t: &Rc<Type>, _| {
                if Rc::ptr_eq(t, &self.global_function_type()) {
                    vec![self.unknown_signature()]
                } else {
                    self.get_signatures_of_type(t, SignatureKind::Call)
                }
            })
            .unwrap(),
        );
        let construct_signatures = self.get_union_signatures(
            &map(Some(type_as_union_type.types()), |t: &Rc<Type>, _| {
                self.get_signatures_of_type(t, SignatureKind::Construct)
            })
            .unwrap(),
        );
        let index_infos = self.get_union_index_infos(type_as_union_type.types());
        self.set_structured_type_members(
            type_as_union_type,
            self.empty_symbols(),
            call_signatures,
            construct_signatures,
            index_infos,
        );
    }

    pub(super) fn intersect_types<TType1: Borrow<Type>, TType2: Borrow<Type>>(
        &self,
        type1: Option<TType1>,
        type2: Option<TType2>,
    ) -> Option<Rc<Type>> {
        let type1 = type1.map(|type1| type1.borrow().type_wrapper());
        let type2 = type2.map(|type2| type2.borrow().type_wrapper());
        if type1.is_none() {
            type2
        } else if type2.is_none() {
            type1
        } else {
            let type1 = type1.unwrap();
            let type2 = type2.unwrap();
            Some(self.get_intersection_type(&vec![type1, type2], Option::<&Symbol>::None, None))
        }
    }

    pub(super) fn find_mixins(&self, types: &[Rc<Type>]) -> Vec<bool> {
        let constructor_type_count = count_where(Some(types), |t: &Rc<Type>, _| {
            !self
                .get_signatures_of_type(t, SignatureKind::Construct)
                .is_empty()
        });
        let mut mixin_flags = map(Some(types), |t: &Rc<Type>, _| {
            self.is_mixin_constructor_type(t)
        })
        .unwrap();
        if constructor_type_count > 0
            && constructor_type_count == count_where(Some(&mixin_flags), |b: &bool, _| *b)
        {
            let first_mixin_index: usize =
                index_of(&mixin_flags, &true, |a: &bool, b: &bool| a == b)
                    .try_into()
                    .unwrap();
            mixin_flags[first_mixin_index] = false;
        }
        mixin_flags
    }

    pub(super) fn include_mixin_type(
        &self,
        type_: &Type,
        types: &[Rc<Type>],
        mixin_flags: &[bool],
        index: usize,
    ) -> Rc<Type> {
        let mut mixed_types: Vec<Rc<Type>> = vec![];
        for i in 0..types.len() {
            if i == index {
                mixed_types.push(type_.type_wrapper());
            } else if mixin_flags[i] {
                mixed_types.push(self.get_return_type_of_signature(
                    &self.get_signatures_of_type(&types[i], SignatureKind::Construct)[0],
                ));
            }
        }
        self.get_intersection_type(&mixed_types, Option::<&Symbol>::None, None)
    }

    pub(super) fn resolve_intersection_type_members(&self, type_: &Type /*IntersectionType*/) {
        let mut call_signatures: Vec<Rc<Signature>> = vec![];
        let mut construct_signatures: Vec<Rc<Signature>> = vec![];
        let mut index_infos: Vec<Rc<IndexInfo>> = vec![];
        let type_as_intersection_type = type_.as_intersection_type();
        let types = type_as_intersection_type.types();
        let mixin_flags = self.find_mixins(types);
        let mixin_count = count_where(Some(&mixin_flags), |b: &bool, _| *b);
        for (i, t) in types.iter().enumerate() {
            if !mixin_flags[i] {
                let mut signatures = self.get_signatures_of_type(t, SignatureKind::Construct);
                if !signatures.is_empty() && mixin_count > 0 {
                    signatures = map(Some(&signatures), |s: &Rc<Signature>, _| {
                        let clone = self.clone_signature(s);
                        *clone.maybe_resolved_return_type() = Some(self.include_mixin_type(
                            &self.get_return_type_of_signature(s),
                            types,
                            mixin_flags.as_ref(),
                            i,
                        ));
                        Rc::new(clone)
                    })
                    .unwrap();
                }
                self.append_signatures(&mut construct_signatures, &signatures);
            }
            self.append_signatures(
                &mut call_signatures,
                &self.get_signatures_of_type(t, SignatureKind::Call),
            );
            index_infos = reduce_left(
                &self.get_index_infos_of_type(t),
                |mut infos: Vec<Rc<IndexInfo>>, new_info: &Rc<IndexInfo>, _| {
                    self.append_index_info(&mut infos, new_info, false);
                    infos
                },
                index_infos,
                None,
                None,
            );
        }
        self.set_structured_type_members(
            type_as_intersection_type,
            self.empty_symbols(),
            call_signatures,
            construct_signatures,
            index_infos,
        );
    }

    pub(super) fn append_signatures(
        &self,
        signatures: &mut Vec<Rc<Signature>>,
        new_signatures: &[Rc<Signature>],
    ) {
        for sig in new_signatures {
            if
            /* !signatures ||*/
            every(signatures, |s: &Rc<Signature>, _| {
                self.compare_signatures_identical(s.clone(), sig, false, false, false, |a, b| {
                    self.compare_types_identical(a, b)
                }) != Ternary::False
            }) {
                append(signatures, Some(sig.clone()));
            }
        }
        // return signatures;
    }

    pub(super) fn append_index_info(
        &self,
        index_infos: &mut Vec<Rc<IndexInfo>>,
        new_info: &Rc<IndexInfo>,
        union: bool,
    ) {
        // if (indexInfos) {
        for i in 0..index_infos.len() {
            let info = index_infos[i].clone();
            if Rc::ptr_eq(&info.key_type, &new_info.key_type) {
                index_infos[i] = Rc::new(self.create_index_info(
                    info.key_type.clone(),
                    if union {
                        self.get_union_type(
                            vec![info.type_.clone(), new_info.type_.clone()],
                            None,
                            Option::<&Symbol>::None,
                            None,
                            Option::<&Type>::None,
                        )
                    } else {
                        self.get_intersection_type(
                            &vec![info.type_.clone(), new_info.type_.clone()],
                            Option::<&Symbol>::None,
                            None,
                        )
                    },
                    if union {
                        info.is_readonly || new_info.is_readonly
                    } else {
                        info.is_readonly && new_info.is_readonly
                    },
                    None,
                ));
                return /*indexInfos*/;
            }
        }
        // }
        /*return*/
        append(index_infos, Some(new_info.clone()));
    }

    pub(super) fn resolve_anonymous_type_members(&self, type_: &Type /*AnonymousType*/) {
        let symbol = self.get_merged_symbol(type_.maybe_symbol()).unwrap();
        let type_as_object_type = type_.as_object_type();
        if let Some(type_target) = type_as_object_type.maybe_target() {
            self.set_structured_type_members(
                type_as_object_type,
                self.empty_symbols(),
                vec![],
                vec![],
                vec![],
            );
            let members = self.create_instantiated_symbol_table(
                &self.get_properties_of_object_type(&type_target),
                type_as_object_type.maybe_mapper().unwrap(),
                false,
            );
            let call_signatures = self.instantiate_signatures(
                &self.get_signatures_of_type(&type_target, SignatureKind::Call),
                type_as_object_type.maybe_mapper().unwrap(),
            );
            let construct_signatures = self.instantiate_signatures(
                &self.get_signatures_of_type(&type_target, SignatureKind::Construct),
                type_as_object_type.maybe_mapper().unwrap(),
            );
            let index_infos = self.instantiate_index_infos(
                &self.get_index_infos_of_type(&type_target),
                type_as_object_type.maybe_mapper().unwrap(),
            );
            self.set_structured_type_members(
                type_as_object_type,
                Rc::new(RefCell::new(members)),
                call_signatures,
                construct_signatures,
                index_infos,
            );
        } else if symbol.flags().intersects(SymbolFlags::TypeLiteral) {
            self.set_structured_type_members(
                type_as_object_type,
                self.empty_symbols(),
                vec![],
                vec![],
                vec![],
            );
            let members = self.get_members_of_symbol(&symbol);
            let call_signatures = self.get_signatures_of_symbol(
                (*members)
                    .borrow()
                    .get(&InternalSymbolName::Call())
                    .map(Clone::clone),
            );
            let construct_signatures = self.get_signatures_of_symbol(
                (*members)
                    .borrow()
                    .get(&InternalSymbolName::New())
                    .map(Clone::clone),
            );
            let index_infos = self.get_index_infos_of_symbol(&symbol);
            self.set_structured_type_members(
                type_as_object_type,
                members,
                call_signatures,
                construct_signatures,
                index_infos,
            );
        } else {
            let mut members = self.empty_symbols();
            let mut index_infos: Vec<Rc<IndexInfo>> = vec![];
            let symbol_exports = symbol.maybe_exports();
            if let Some(symbol_exports) = symbol_exports.as_ref() {
                members = self.get_exports_of_symbol(&symbol);
                if Rc::ptr_eq(&symbol, &self.global_this_symbol()) {
                    let mut vars_only = SymbolTable::new();
                    for (_, p) in &*(*members).borrow() {
                        if !p.flags().intersects(SymbolFlags::BlockScoped) {
                            vars_only.insert(p.escaped_name().clone(), p.clone());
                        }
                    }
                    members = Rc::new(RefCell::new(vars_only));
                }
            }
            let mut base_constructor_index_info: Option<Rc<IndexInfo>> = None;
            self.set_structured_type_members(
                type_as_object_type,
                members.clone(),
                vec![],
                vec![],
                vec![],
            );
            if symbol.flags().intersects(SymbolFlags::Class) {
                let class_type = self.get_declared_type_of_class_or_interface(&symbol);
                let base_constructor_type = self.get_base_constructor_type_of_class(&class_type);
                if base_constructor_type.flags().intersects(
                    TypeFlags::Object | TypeFlags::Intersection | TypeFlags::TypeVariable,
                ) {
                    let members_new = Rc::new(RefCell::new(create_symbol_table(Some(
                        &self.get_named_or_index_signature_members(&*(*members).borrow()),
                    ))));
                    members = members_new;
                    self.add_inherited_members(
                        &mut members.borrow_mut(),
                        &self.get_properties_of_type(&base_constructor_type),
                    );
                } else if Rc::ptr_eq(&base_constructor_type, &self.any_type()) {
                    base_constructor_index_info = Some(Rc::new(self.create_index_info(
                        self.string_type(),
                        self.any_type(),
                        false,
                        None,
                    )));
                }
            }

            let index_symbol = self.get_index_symbol_from_symbol_table(&(*members).borrow());
            if let Some(index_symbol) = index_symbol {
                index_infos = self.get_index_infos_of_index_symbol(&index_symbol);
            } else {
                if let Some(base_constructor_index_info) = base_constructor_index_info.as_ref() {
                    append(&mut index_infos, Some(base_constructor_index_info.clone()));
                }
                if symbol.flags().intersects(SymbolFlags::Enum)
                    && (self
                        .get_declared_type_of_symbol(&symbol)
                        .flags()
                        .intersects(TypeFlags::Enum)
                        || some(
                            type_as_object_type.maybe_properties().as_deref(),
                            Some(|prop: &Rc<Symbol>| {
                                self.get_type_of_symbol(prop)
                                    .flags()
                                    .intersects(TypeFlags::NumberLike)
                            }),
                        ))
                {
                    append(&mut index_infos, Some(self.enum_number_index_info()));
                }
            }
            self.set_structured_type_members(
                type_as_object_type,
                members.clone(),
                vec![],
                vec![],
                index_infos,
            );
            if symbol
                .flags()
                .intersects(SymbolFlags::Function | SymbolFlags::Method)
            {
                type_as_object_type
                    .set_call_signatures(self.get_signatures_of_symbol(Some(&*symbol)));
            }
            if symbol.flags().intersects(SymbolFlags::Class) {
                let class_type = self.get_declared_type_of_class_or_interface(&symbol);
                let symbol_members = symbol.maybe_members();
                let mut construct_signatures = if let Some(symbol_members) = symbol_members.as_ref()
                {
                    self.get_signatures_of_symbol(
                        (**symbol_members)
                            .borrow()
                            .get(&InternalSymbolName::Constructor())
                            .map(Clone::clone),
                    )
                } else {
                    vec![]
                };
                if symbol.flags().intersects(SymbolFlags::Function) {
                    add_range(
                        &mut construct_signatures,
                        Some(&map_defined(
                            type_as_object_type.maybe_call_signatures().as_deref(),
                            |sig: &Rc<Signature>, _| {
                                if self.is_js_constructor(sig.declaration.as_deref()) {
                                    Some(Rc::new(self.create_signature(
                                        sig.declaration.clone(),
                                        sig.type_parameters.clone(),
                                        sig.this_parameter.clone(),
                                        sig.parameters().to_owned(),
                                        Some(class_type.clone()),
                                        None,
                                        sig.min_argument_count(),
                                        sig.flags & SignatureFlags::PropagatingFlags,
                                    )))
                                } else {
                                    None
                                }
                            },
                        )),
                        None,
                        None,
                    );
                }
                if construct_signatures.is_empty() {
                    construct_signatures = self.get_default_construct_signatures(&class_type);
                }
                type_as_object_type.set_construct_signatures(construct_signatures);
            }
        }
    }

    pub(super) fn get_type_of_mapped_symbol(
        &self,
        symbol: &Symbol, /*MappedSymbol*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_parameter_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_constraint_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_name_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_template_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_mapped_type_with_keyof_constraint_declaration(
        &self,
        type_: &Type, /*MappedType*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_modifiers_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_mapped_type_modifiers(
        &self,
        type_: &Type, /*MappedType*/
    ) -> MappedTypeModifiers {
        unimplemented!()
    }

    pub(super) fn is_generic_mapped_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn resolve_structured_type_members(
        &self,
        type_: &Type, /*StructuredType*/
    ) -> Rc<Type /*ResolvedType*/> {
        if !type_.as_resolvable_type().is_resolved() {
            if let Type::ObjectType(object_type) = &*type_
            /*type_.flags().intersects(TypeFlags::Object)*/
            {
                if object_type
                    .object_flags()
                    .intersects(ObjectFlags::Reference)
                {
                    self.resolve_type_reference_members(type_);
                } else if object_type
                    .object_flags()
                    .intersects(ObjectFlags::ClassOrInterface)
                {
                    self.resolve_class_or_interface_members(type_);
                } else {
                    unimplemented!()
                }
            } else {
                unimplemented!()
            }
        }
        type_.type_wrapper()
    }

    pub(super) fn get_properties_of_object_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        if type_.flags().intersects(TypeFlags::Object) {
            return self
                .resolve_structured_type_members(type_)
                .as_resolved_type()
                .properties()
                .iter()
                .map(Clone::clone)
                .collect();
        }
        unimplemented!()
    }

    pub(super) fn get_property_of_object_type(
        &self,
        type_: &Type,
        name: &__String,
    ) -> Option<Rc<Symbol>> {
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_);
            let symbol = (*resolved.as_resolved_type().members())
                .borrow()
                .get(name)
                .map(Clone::clone);
            if let Some(symbol) = symbol {
                if self.symbol_is_value(&symbol) {
                    return Some(symbol);
                }
            }
        }
        None
    }

    pub(super) fn get_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            unimplemented!()
        } else {
            self.get_properties_of_object_type(&type_)
        }
    }

    pub(super) fn for_each_property_of_type<TAction: FnMut(&Symbol, &__String)>(
        &self,
        type_: &Type,
        action: TAction,
    ) {
        unimplemented!()
    }

    pub(super) fn get_constraint_of_type_parameter(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> Option<Rc<Type>> {
        if self.has_non_circular_base_constraint(type_parameter) {
            self.get_constraint_from_type_parameter(type_parameter)
        } else {
            None
        }
    }

    pub(super) fn get_base_constraint_of_type(&self, type_: &Type) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_base_constraint_or_type(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn has_non_circular_base_constraint(
        &self,
        type_: &Type, /*InstantiableType*/
    ) -> bool {
        !Rc::ptr_eq(
            &self.get_resolved_base_constraint(type_),
            &self.circular_constraint_type(),
        )
    }

    pub(super) fn get_resolved_base_constraint(
        &self,
        type_: &Type, /*InstantiableType | UnionOrIntersectionType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_default_from_type_parameter_(
        &self,
        type_: &Type, /*TypeParameter*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }
}
