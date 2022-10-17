#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::MappedTypeModifiers;
use crate::{
    add_range, append, concatenate, count_where, create_symbol_table, every, get_check_flags,
    index_of, map, map_defined, reduce_left, same_map, some, CheckFlags, Diagnostics, IndexInfo,
    InternalSymbolName, Node, Number, ObjectTypeInterface, ResolvedTypeInterface, Signature,
    SignatureFlags, SignatureKind, Symbol, SymbolFlags, SymbolInterface, SymbolTable, Ternary,
    TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    TypeSystemPropertyName, UnionOrIntersectionTypeInterface, __String,
};

impl TypeChecker {
    pub(super) fn combine_union_this_param<TLeft: Borrow<Symbol>, TRight: Borrow<Symbol>>(
        &self,
        left: Option<TLeft>,
        right: Option<TRight>,
        mapper: Option<Rc<TypeMapper>>,
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
                self.instantiate_type(&self.get_type_of_symbol(&right), mapper),
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
        mapper: Option<Rc<TypeMapper>>,
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
                longest_param_type = self.instantiate_type(&longest_param_type, mapper.clone());
            }
            let mut shorter_param_type = self
                .try_get_type_at_position(shorter, i)
                .unwrap_or_else(|| self.unknown_type());
            if ptr::eq(shorter, right) {
                shorter_param_type = self.instantiate_type(&shorter_param_type, mapper.clone());
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
                    param_name.unwrap_or_else(|| format!("arg{}", i)),
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
                .create_symbol(SymbolFlags::FunctionScopedVariable, "args".to_owned(), None)
                .into();
            rest_param_symbol
                .as_transient_symbol()
                .symbol_links()
                .borrow_mut()
                .type_ = Some(
                self.create_array_type(&self.get_type_at_position(shorter, longest_count), None),
            );
            if ptr::eq(shorter, right) {
                let type_ = self.maybe_instantiate_type(
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
            .maybe_type_parameters()
            .clone()
            .or_else(|| right.maybe_type_parameters().clone());
        let mut param_mapper: Option<Rc<TypeMapper>> = None;
        if left.maybe_type_parameters().is_some() && right.maybe_type_parameters().is_some() {
            param_mapper = Some(Rc::new(self.create_type_mapper(
                right.maybe_type_parameters().clone().unwrap(),
                left.maybe_type_parameters().clone(),
            )));
        }
        let declaration = left.declaration.clone();
        let params = self.combine_union_parameters(&left, &right, param_mapper.clone());
        let this_param = self.combine_union_this_param(
            left.maybe_this_parameter().as_deref(),
            right.maybe_this_parameter().as_deref(),
            param_mapper.clone(),
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
                result.push(Rc::new(self.create_index_info(
                    index_type.clone(),
                    self.get_union_type(
                        map(types, |t: &Rc<Type>, _| {
                            self.get_index_type_of_type_(t, index_type).unwrap()
                        }),
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
                )));
            }
        }
        result
        // }
        // return emptyArray;
    }

    pub(super) fn resolve_union_type_members(&self, type_: &Type /*UnionType*/) {
        let type_as_union_type = type_.as_union_type();
        let call_signatures =
            self.get_union_signatures(&map(type_as_union_type.types(), |t: &Rc<Type>, _| {
                if Rc::ptr_eq(t, &self.global_function_type()) {
                    vec![self.unknown_signature()]
                } else {
                    self.get_signatures_of_type(t, SignatureKind::Call)
                }
            }));
        let construct_signatures = self
            .get_union_signatures(&map(type_as_union_type.types(), |t: &Rc<Type>, _| {
                self.get_signatures_of_type(t, SignatureKind::Construct)
            }));
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
        let mut mixin_flags = map(types, |t: &Rc<Type>, _| self.is_mixin_constructor_type(t));
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
                    self.get_signatures_of_type(&types[i], SignatureKind::Construct)[0].clone(),
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
                    signatures = map(&signatures, |s: &Rc<Signature>, _| {
                        let clone = self.clone_signature(s);
                        *clone.maybe_resolved_return_type_mut() = Some(self.include_mixin_type(
                            &self.get_return_type_of_signature(s.clone()),
                            types,
                            mixin_flags.as_ref(),
                            i,
                        ));
                        Rc::new(clone)
                    });
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
                self.compare_signatures_identical(
                    s.clone(),
                    sig.clone(),
                    false,
                    false,
                    false,
                    |a, b| self.compare_types_identical(a, b),
                ) == Ternary::False
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
                (*members).borrow().get(InternalSymbolName::Call).cloned(),
            );
            let construct_signatures = self.get_signatures_of_symbol(
                (*members).borrow().get(InternalSymbolName::New).cloned(),
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
                            vars_only.insert(p.escaped_name().to_owned(), p.clone());
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
                            .get(InternalSymbolName::Constructor)
                            .cloned(),
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
                                        sig.maybe_type_parameters().clone(),
                                        sig.maybe_this_parameter().clone(),
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

    pub(super) fn replace_indexed_access(
        &self,
        instantiable: &Type,
        type_: &Type, /*ReplaceableIndexedAccessType*/
        replacement: &Type,
    ) -> Rc<Type> {
        let type_as_indexed_access_type = type_.as_indexed_access_type();
        self.instantiate_type(
            instantiable,
            Some(Rc::new(self.create_type_mapper(
                vec![
                    type_as_indexed_access_type.index_type.clone(),
                    type_as_indexed_access_type.object_type.clone(),
                ],
                Some(vec![
                    self.get_number_literal_type(Number::new(0.0)),
                    self.create_tuple_type(&[replacement.type_wrapper()], None, None, None),
                ]),
            ))),
        )
    }

    pub(super) fn resolve_reverse_mapped_type_members(
        &self,
        type_: &Type, /*ReverseMappedType*/
    ) {
        let type_as_reverse_mapped_type = type_.as_reverse_mapped_type();
        let index_info =
            self.get_index_info_of_type_(&type_as_reverse_mapped_type.source, &self.string_type());
        let modifiers = self.get_mapped_type_modifiers(&type_as_reverse_mapped_type.mapped_type);
        let readonly_mask = if modifiers.intersects(MappedTypeModifiers::IncludeReadonly) {
            false
        } else {
            true
        };
        let optional_mask = if modifiers.intersects(MappedTypeModifiers::IncludeOptional) {
            SymbolFlags::None
        } else {
            SymbolFlags::Optional
        };
        let index_infos = if let Some(index_info) = index_info {
            vec![Rc::new(self.create_index_info(
                self.string_type(),
                self.infer_reverse_mapped_type(
                    &index_info.type_,
                    &type_as_reverse_mapped_type.mapped_type,
                    &type_as_reverse_mapped_type.constraint_type,
                ),
                readonly_mask && index_info.is_readonly,
                None,
            ))]
        } else {
            vec![]
        };
        let mut members = create_symbol_table(None);
        for prop in self.get_properties_of_type(&type_as_reverse_mapped_type.source) {
            let check_flags = CheckFlags::ReverseMapped
                | if readonly_mask && self.is_readonly_symbol(&prop) {
                    CheckFlags::Readonly
                } else {
                    CheckFlags::None
                };
            let inferred_prop = self.create_symbol(
                SymbolFlags::Property | prop.flags() & optional_mask,
                prop.escaped_name().to_owned(),
                Some(check_flags),
            );
            if let Some(prop_declarations) = prop.maybe_declarations().clone() {
                inferred_prop.set_declarations(prop_declarations);
            }
            inferred_prop.symbol_links().borrow_mut().name_type =
                (*self.get_symbol_links(&prop)).borrow().name_type.clone();
            let property_type = self.get_type_of_symbol(&prop);
            let mapped_type: Rc<Type>;
            let constraint_type: Rc<Type>;
            let type_constraint_type_type = &type_as_reverse_mapped_type
                .constraint_type
                .as_index_type()
                .type_;
            if type_constraint_type_type
                .flags()
                .intersects(TypeFlags::IndexedAccess)
                && type_constraint_type_type
                    .as_indexed_access_type()
                    .object_type
                    .flags()
                    .intersects(TypeFlags::TypeParameter)
                && type_constraint_type_type
                    .as_indexed_access_type()
                    .index_type
                    .flags()
                    .intersects(TypeFlags::TypeParameter)
            {
                let new_type_param = &type_constraint_type_type
                    .as_indexed_access_type()
                    .object_type;
                let new_mapped_type = self.replace_indexed_access(
                    &type_as_reverse_mapped_type.mapped_type,
                    type_constraint_type_type,
                    new_type_param,
                );
                mapped_type = new_mapped_type;
                constraint_type = self.get_index_type(new_type_param, None, None);
            } else {
                mapped_type = type_as_reverse_mapped_type.mapped_type.clone();
                constraint_type = type_as_reverse_mapped_type.constraint_type.clone();
            }
            let inferred_prop: Rc<Symbol> = inferred_prop
                .into_reverse_mapped_symbol(property_type, mapped_type, constraint_type)
                .into();
            members.insert(prop.escaped_name().to_owned(), inferred_prop);
        }
        self.set_structured_type_members(
            type_as_reverse_mapped_type,
            Rc::new(RefCell::new(members)),
            vec![],
            vec![],
            index_infos,
        );
    }

    pub(super) fn get_lower_bound_of_key_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Index) {
            let t = self.get_apparent_type(&type_.as_index_type().type_);
            return if self.is_generic_tuple_type(&t) {
                self.get_known_keys_of_tuple_type(&t)
            } else {
                self.get_index_type(&t, None, None)
            };
        }
        if type_.flags().intersects(TypeFlags::Conditional) {
            let type_as_conditional_type = type_.as_conditional_type();
            if (*type_as_conditional_type.root).borrow().is_distributive {
                let check_type = &type_as_conditional_type.check_type;
                let constraint = self.get_lower_bound_of_key_type(check_type);
                if !Rc::ptr_eq(&constraint, check_type) {
                    return self.get_conditional_type_instantiation(
                        type_,
                        &self.prepend_type_mapping(
                            &(*type_as_conditional_type.root).borrow().check_type.clone(),
                            &constraint,
                            type_as_conditional_type.mapper.clone(),
                        ),
                        Option::<&Symbol>::None,
                        None,
                    );
                }
            }
            return type_.type_wrapper();
        }
        if type_.flags().intersects(TypeFlags::Union) {
            return self
                .map_type(
                    type_,
                    &mut |type_| Some(self.get_lower_bound_of_key_type(type_)),
                    None,
                )
                .unwrap();
        }
        if type_.flags().intersects(TypeFlags::Intersection) {
            return self.get_intersection_type(
                &same_map(
                    type_.as_union_or_intersection_type_interface().types(),
                    |type_: &Rc<Type>, _| self.get_lower_bound_of_key_type(type_),
                ),
                Option::<&Symbol>::None,
                None,
            );
        }
        type_.type_wrapper()
    }

    pub(super) fn get_is_late_check_flag(&self, s: &Symbol) -> CheckFlags {
        get_check_flags(s) & CheckFlags::Late
    }

    pub(super) fn for_each_mapped_type_property_key_type_and_index_signature_key_type<
        TCallback: FnMut(&Type),
    >(
        &self,
        type_: &Type,
        include: TypeFlags,
        strings_only: bool,
        mut cb: TCallback,
    ) {
        for prop in self.get_properties_of_type(type_) {
            cb(&self.get_literal_type_from_property(&prop, include, None));
        }
        if type_.flags().intersects(TypeFlags::Any) {
            cb(&self.string_type());
        } else {
            for info in self.get_index_infos_of_type(type_) {
                if !strings_only
                    || info
                        .key_type
                        .flags()
                        .intersects(TypeFlags::String | TypeFlags::TemplateLiteral)
                {
                    cb(&info.key_type);
                }
            }
        }
    }

    pub(super) fn resolve_mapped_type_members(&self, type_: &Type /*MappedType*/) {
        let mut members = create_symbol_table(None);
        let mut index_infos: Vec<Rc<IndexInfo>> = vec![];
        let type_as_mapped_type = type_.as_mapped_type();
        self.set_structured_type_members(
            type_as_mapped_type,
            self.empty_symbols(),
            vec![],
            vec![],
            vec![],
        );
        let type_parameter = self.get_type_parameter_from_mapped_type(type_);
        let constraint_type = self.get_constraint_type_from_mapped_type(type_);
        let name_type = self.get_name_type_from_mapped_type(
            &type_as_mapped_type
                .maybe_target()
                .unwrap_or_else(|| type_.type_wrapper()),
        );
        let template_type = self.get_template_type_from_mapped_type(
            &type_as_mapped_type
                .maybe_target()
                .unwrap_or_else(|| type_.type_wrapper()),
        );
        let modifiers_type =
            self.get_apparent_type(&self.get_modifiers_type_from_mapped_type(type_));
        let template_modifiers = self.get_mapped_type_modifiers(type_);
        let include = if self.keyof_strings_only {
            TypeFlags::StringLiteral
        } else {
            TypeFlags::StringOrNumberLiteralOrUnique
        };
        if self.is_mapped_type_with_keyof_constraint_declaration(type_) {
            self.for_each_mapped_type_property_key_type_and_index_signature_key_type(
                &modifiers_type,
                include,
                self.keyof_strings_only,
                |key_type| {
                    self.add_member_for_key_type_resolved_mapped_type_members(
                        type_,
                        name_type.as_deref(),
                        &type_parameter,
                        &mut members,
                        &modifiers_type,
                        template_modifiers,
                        &template_type,
                        &mut index_infos,
                        key_type,
                    )
                },
            );
        } else {
            self.for_each_type(
                &self.get_lower_bound_of_key_type(&constraint_type),
                |key_type| {
                    self.add_member_for_key_type_resolved_mapped_type_members(
                        type_,
                        name_type.as_deref(),
                        &type_parameter,
                        &mut members,
                        &modifiers_type,
                        template_modifiers,
                        &template_type,
                        &mut index_infos,
                        key_type,
                    );
                    Option::<()>::None
                },
            );
        }
        self.set_structured_type_members(
            type_as_mapped_type,
            Rc::new(RefCell::new(members)),
            vec![],
            vec![],
            index_infos,
        );
    }

    pub(super) fn add_member_for_key_type_resolved_mapped_type_members(
        &self,
        type_: &Type, /*MappedType*/
        name_type: Option<&Type>,
        type_parameter: &Type,
        members: &mut SymbolTable,
        modifiers_type: &Type,
        template_modifiers: MappedTypeModifiers,
        template_type: &Type,
        index_infos: &mut Vec<Rc<IndexInfo>>,
        key_type: &Type,
    ) {
        let prop_name_type = if let Some(name_type) = name_type {
            self.instantiate_type(
                name_type,
                Some(Rc::new(self.append_type_mapping(
                    type_.as_mapped_type().maybe_mapper(),
                    type_parameter,
                    key_type,
                ))),
            )
        } else {
            key_type.type_wrapper()
        };
        self.for_each_type(&prop_name_type, |t| {
            self.add_member_for_key_type_worker(
                members,
                modifiers_type,
                template_modifiers,
                type_,
                name_type,
                template_type,
                type_parameter,
                index_infos,
                key_type,
                t,
            );
            Option::<()>::None
        });
    }

    pub(super) fn add_member_for_key_type_worker(
        &self,
        members: &mut SymbolTable,
        modifiers_type: &Type,
        template_modifiers: MappedTypeModifiers,
        type_: &Type,
        name_type: Option<&Type>,
        template_type: &Type,
        type_parameter: &Type,
        index_infos: &mut Vec<Rc<IndexInfo>>,
        key_type: &Type,
        prop_name_type: &Type,
    ) {
        if self.is_type_usable_as_property_name(prop_name_type) {
            let prop_name = self.get_property_name_from_type(prop_name_type);
            let existing_prop = members.get(&*prop_name);
            if let Some(existing_prop) = existing_prop {
                let existing_prop_as_mapped_symbol = existing_prop.as_mapped_symbol();
                let existing_prop_name_type = (*existing_prop_as_mapped_symbol.symbol_links())
                    .borrow()
                    .name_type
                    .clone()
                    .unwrap();
                existing_prop_as_mapped_symbol
                    .symbol_links()
                    .borrow_mut()
                    .name_type = Some(self.get_union_type(
                    vec![existing_prop_name_type, prop_name_type.type_wrapper()],
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                ));
                existing_prop_as_mapped_symbol.set_key_type(self.get_union_type(
                    vec![
                        existing_prop_as_mapped_symbol.key_type(),
                        key_type.type_wrapper(),
                    ],
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                ));
            } else {
                let modifiers_prop = if self.is_type_usable_as_property_name(key_type) {
                    self.get_property_of_type_(
                        modifiers_type,
                        &self.get_property_name_from_type(key_type),
                        None,
                    )
                } else {
                    None
                };
                let is_optional = template_modifiers
                    .intersects(MappedTypeModifiers::IncludeOptional)
                    || !template_modifiers.intersects(MappedTypeModifiers::ExcludeOptional)
                        && matches!(modifiers_prop.as_ref(), Some(modifiers_prop) if modifiers_prop.flags().intersects(SymbolFlags::Optional));
                let is_readonly = template_modifiers
                    .intersects(MappedTypeModifiers::IncludeReadonly)
                    || !template_modifiers.intersects(MappedTypeModifiers::ExcludeReadonly)
                        && matches!(modifiers_prop.as_ref(), Some(modifiers_prop) if self.is_readonly_symbol(modifiers_prop));
                let strip_optional = self.strict_null_checks
                    && !is_optional
                    && matches!(modifiers_prop.as_ref(), Some(modifiers_prop) if modifiers_prop.flags().intersects(SymbolFlags::Optional));
                let late_flag = if let Some(modifiers_prop) = modifiers_prop.as_ref() {
                    self.get_is_late_check_flag(modifiers_prop)
                } else {
                    CheckFlags::None
                };
                let prop = self.create_symbol(
                    SymbolFlags::Property
                        | if is_optional {
                            SymbolFlags::Optional
                        } else {
                            SymbolFlags::None
                        },
                    (*prop_name).to_owned(),
                    Some(
                        late_flag
                            | CheckFlags::Mapped
                            | if is_readonly {
                                CheckFlags::Readonly
                            } else {
                                CheckFlags::None
                            }
                            | if strip_optional {
                                CheckFlags::StripOptional
                            } else {
                                CheckFlags::None
                            },
                    ),
                );
                let prop: Rc<Symbol> = prop
                    .into_mapped_symbol(type_.type_wrapper(), key_type.type_wrapper())
                    .into();
                let prop_as_mapped_symbol = prop.as_mapped_symbol();
                prop_as_mapped_symbol.symbol_links().borrow_mut().name_type =
                    Some(prop_name_type.type_wrapper());
                if let Some(modifiers_prop) = modifiers_prop {
                    prop_as_mapped_symbol
                        .symbol_links()
                        .borrow_mut()
                        .synthetic_origin = Some(modifiers_prop.clone());
                    let declarations = if name_type.is_some() {
                        None
                    } else {
                        modifiers_prop.maybe_declarations().clone()
                    };
                    if let Some(declarations) = declarations {
                        prop.set_declarations(declarations);
                    }
                }
                members.insert(prop_name.into_owned(), prop);
            }
        } else if self.is_valid_index_key_type(prop_name_type)
            || prop_name_type
                .flags()
                .intersects(TypeFlags::Any | TypeFlags::Enum)
        {
            let index_key_type = if prop_name_type
                .flags()
                .intersects(TypeFlags::Any | TypeFlags::String)
            {
                self.string_type()
            } else if prop_name_type
                .flags()
                .intersects(TypeFlags::Number | TypeFlags::Enum)
            {
                self.number_type()
            } else {
                prop_name_type.type_wrapper()
            };
            let prop_type = self.instantiate_type(
                template_type,
                Some(Rc::new(self.append_type_mapping(
                    type_.as_mapped_type().maybe_mapper(),
                    type_parameter,
                    key_type,
                ))),
            );
            let index_info = Rc::new(self.create_index_info(
                index_key_type,
                prop_type,
                template_modifiers.intersects(MappedTypeModifiers::IncludeReadonly),
                None,
            ));
            self.append_index_info(index_infos, &index_info, true);
        }
    }

    pub(super) fn get_type_of_mapped_symbol(
        &self,
        symbol: &Symbol, /*MappedSymbol*/
    ) -> Rc<Type> {
        let symbol_as_mapped_symbol = symbol.as_mapped_symbol();
        if (*symbol_as_mapped_symbol.symbol_links())
            .borrow()
            .type_
            .is_none()
        {
            let mapped_type = &symbol_as_mapped_symbol.mapped_type;
            let mapped_type_as_mapped_type = mapped_type.as_mapped_type();
            if !self.push_type_resolution(
                &symbol.symbol_wrapper().into(),
                TypeSystemPropertyName::Type,
            ) {
                mapped_type_as_mapped_type.set_contains_error(Some(true));
                return self.error_type();
            }
            let template_type = self.get_template_type_from_mapped_type(
                &mapped_type_as_mapped_type
                    .maybe_target()
                    .unwrap_or_else(|| mapped_type.type_wrapper()),
            );
            let mapper = Rc::new(self.append_type_mapping(
                mapped_type_as_mapped_type.maybe_mapper(),
                &self.get_type_parameter_from_mapped_type(mapped_type),
                &symbol_as_mapped_symbol.key_type(),
            ));
            let prop_type = self.instantiate_type(&template_type, Some(mapper));
            let mut type_ = if self.strict_null_checks
                && symbol.flags().intersects(SymbolFlags::Optional)
                && !self.maybe_type_of_kind(&prop_type, TypeFlags::Undefined | TypeFlags::Void)
            {
                self.get_optional_type_(&prop_type, Some(true))
            } else if symbol_as_mapped_symbol
                .check_flags()
                .intersects(CheckFlags::StripOptional)
            {
                self.remove_missing_or_undefined_type(&prop_type)
            } else {
                prop_type
            };
            if !self.pop_type_resolution() {
                self.error(
                    self.maybe_current_node(),
                    &Diagnostics::Type_of_property_0_circularly_references_itself_in_mapped_type_1,
                    Some(vec![
                        self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None),
                        self.type_to_string_(mapped_type, Option::<&Node>::None, None, None),
                    ]),
                );
                type_ = self.error_type();
            }
            symbol_as_mapped_symbol.symbol_links().borrow_mut().type_ = Some(type_);
        }
        (*symbol_as_mapped_symbol.symbol_links())
            .borrow()
            .type_
            .clone()
            .unwrap()
    }

    pub(super) fn get_type_parameter_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        let type_as_mapped_type = type_.as_mapped_type();
        {
            let value = type_as_mapped_type.maybe_type_parameter().clone();
            value
        }
        .unwrap_or_else(|| {
            let ret = self.get_declared_type_of_type_parameter(
                &self
                    .get_symbol_of_node(
                        &type_as_mapped_type
                            .declaration
                            .as_mapped_type_node()
                            .type_parameter,
                    )
                    .unwrap(),
            );
            *type_as_mapped_type.maybe_type_parameter() = Some(ret.clone());
            ret
        })
    }

    pub(super) fn get_constraint_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        let type_as_mapped_type = type_.as_mapped_type();
        {
            let value = type_as_mapped_type.maybe_constraint_type().clone();
            value
        }
        .unwrap_or_else(|| {
            let ret = self
                .get_constraint_of_type_parameter(&self.get_type_parameter_from_mapped_type(type_))
                .unwrap_or_else(|| self.error_type());
            *type_as_mapped_type.maybe_constraint_type() = Some(ret.clone());
            ret
        })
    }
}
