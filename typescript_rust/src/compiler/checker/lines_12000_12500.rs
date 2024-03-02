use std::{convert::TryInto, io};

use id_arena::Id;
use indexmap::IndexMap;

use super::{get_symbol_id, MinArgumentCountFlags};
use crate::{
    add_range, append, chain_diagnostic_messages, create_symbol_table, find, get_check_flags,
    get_declaration_modifier_flags_from_symbol, get_effective_type_parameter_declarations,
    get_immediately_invoked_function_expression, get_jsdoc_parameter_tags, get_object_flags,
    has_question_token, index_of_eq, is_external_module_name_relative, is_in_js_file,
    is_jsdoc_property_like_tag, is_property_declaration, length, map, maybe_append_if_unique_eq,
    reduce_left, released, try_filter, try_map, unescape_leading_underscores, CheckFlags, Debug_,
    DiagnosticMessageChain, Diagnostics, HasArena, HasInitializerInterface, HasTypeInterface,
    InArena, IndexInfo, IteratorExt, ModifierFlags, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, ScriptTarget, Signature, SignatureKind, Symbol, SymbolFlags,
    SymbolId, SymbolInterface, SymbolTable, SyntaxKind, Ternary, TransientSymbolInterface, Type,
    TypeChecker, TypeFlags, TypeFormatFlags, TypeInterface, TypePredicate, TypePredicateKind,
    UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub(super) fn get_apparent_type(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        let t = if type_.ref_(self).flags().intersects(TypeFlags::Instantiable) {
            self.get_base_constraint_of_type(type_)?
                .unwrap_or_else(|| self.unknown_type())
        } else {
            type_
        };
        Ok(
            if get_object_flags(&t.ref_(self)).intersects(ObjectFlags::Mapped) {
                self.get_apparent_type_of_mapped_type(t)?
            } else if t.ref_(self).flags().intersects(TypeFlags::Intersection) {
                self.get_apparent_type_of_intersection_type(t)?
            } else if t.ref_(self).flags().intersects(TypeFlags::StringLike) {
                self.global_string_type()
            } else if t.ref_(self).flags().intersects(TypeFlags::NumberLike) {
                self.global_number_type()
            } else if t.ref_(self).flags().intersects(TypeFlags::BigIntLike) {
                self.get_global_big_int_type(self.language_version >= ScriptTarget::ES2020)?
            } else if t.ref_(self).flags().intersects(TypeFlags::BooleanLike) {
                self.global_boolean_type()
            } else if t.ref_(self).flags().intersects(TypeFlags::ESSymbolLike) {
                self.get_global_es_symbol_type(self.language_version >= ScriptTarget::ES2015)?
            } else if t.ref_(self).flags().intersects(TypeFlags::NonPrimitive) {
                self.empty_object_type()
            } else if t.ref_(self).flags().intersects(TypeFlags::Index) {
                self.keyof_constraint_type()
            } else if t.ref_(self).flags().intersects(TypeFlags::Unknown)
                && !self.strict_null_checks
            {
                self.empty_object_type()
            } else {
                t
            },
        )
    }

    pub(super) fn get_reduced_apparent_type(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        self.get_reduced_type(self.get_apparent_type(self.get_reduced_type(type_)?)?)
    }

    pub(super) fn create_union_or_intersection_property(
        &self,
        containing_type: Id<Type>, /*UnionOrIntersectionType*/
        name: &str,                /*__String*/
        skip_object_function_property_augment: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let mut single_prop: Option<Id<Symbol>> = None;
        let mut prop_set: Option<IndexMap<SymbolId, Id<Symbol>>> = None;
        let mut index_types: Option<Vec<Id<Type>>> = None;
        let is_union = containing_type
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Union);
        let mut optional_flag = if is_union {
            SymbolFlags::None
        } else {
            SymbolFlags::Optional
        };
        let mut synthetic_flag = CheckFlags::SyntheticMethod;
        let mut check_flags = if is_union {
            CheckFlags::None
        } else {
            CheckFlags::Readonly
        };
        let mut merged_instantiations = false;
        for &current in &{
            let types = containing_type
                .ref_(self)
                .as_union_or_intersection_type_interface()
                .types()
                .to_owned();
            types
        } {
            let type_ = self.get_apparent_type(current)?;
            if !(self.is_error_type(type_) || type_.ref_(self).flags().intersects(TypeFlags::Never))
            {
                let prop =
                    self.get_property_of_type_(type_, name, skip_object_function_property_augment)?;
                let modifiers = if let Some(prop) = prop {
                    get_declaration_modifier_flags_from_symbol(prop, None, self)
                } else {
                    ModifierFlags::None
                };
                if let Some(prop) = prop {
                    if is_union {
                        optional_flag |= prop.ref_(self).flags() & SymbolFlags::Optional;
                    } else {
                        optional_flag &= prop.ref_(self).flags();
                    }
                    if single_prop.is_none() {
                        single_prop = Some(prop.clone());
                    } else if prop != single_prop.unwrap() {
                        let single_prop = single_prop.unwrap();
                        let is_instantiation =
                            self.get_target_symbol(prop) ==       /*|| prop*/
                            self.get_target_symbol(single_prop) /*|| singleProp*/
                        ;
                        if is_instantiation
                            && self.compare_properties(single_prop, prop, |a, b| {
                                Ok(if a == b {
                                    Ternary::True
                                } else {
                                    Ternary::False
                                })
                            })? == Ternary::True
                        {
                            merged_instantiations = matches!(
                                single_prop.ref_(self).maybe_parent(),
                                Some(parent) if length(
                                    self.get_local_type_parameters_of_class_or_interface_or_type_alias(parent)?.as_deref()
                                ) != 0
                            );
                        } else {
                            if prop_set.is_none() {
                                prop_set = Some(IndexMap::new());
                                prop_set.as_mut().unwrap().insert(
                                    get_symbol_id(&single_prop.ref_(self)),
                                    single_prop.clone(),
                                );
                            }
                            let prop_set = prop_set.as_mut().unwrap();
                            let id = get_symbol_id(&prop.ref_(self));
                            if !prop_set.contains_key(&id) {
                                prop_set.insert(id, prop.clone());
                            }
                        }
                    }
                    if is_union && self.is_readonly_symbol(prop)? {
                        check_flags |= CheckFlags::Readonly;
                    } else if !is_union && !self.is_readonly_symbol(prop)? {
                        check_flags &= !CheckFlags::Readonly;
                    }
                    check_flags |=
                        if !modifiers.intersects(ModifierFlags::NonPublicAccessibilityModifier) {
                            CheckFlags::ContainsPublic
                        } else {
                            CheckFlags::None
                        } | if modifiers.intersects(ModifierFlags::Protected) {
                            CheckFlags::ContainsProtected
                        } else {
                            CheckFlags::None
                        } | if modifiers.intersects(ModifierFlags::Private) {
                            CheckFlags::ContainsPrivate
                        } else {
                            CheckFlags::None
                        } | if modifiers.intersects(ModifierFlags::Static) {
                            CheckFlags::ContainsStatic
                        } else {
                            CheckFlags::None
                        };
                    if !self.is_prototype_property(prop) {
                        synthetic_flag = CheckFlags::SyntheticProperty;
                    }
                } else if is_union {
                    let index_info = if !self.is_late_bound_name(name) {
                        self.get_applicable_index_info_for_name(type_, name)?
                    } else {
                        None
                    };
                    if let Some(index_info) = index_info {
                        check_flags |= CheckFlags::WritePartial
                            | if index_info.ref_(self).is_readonly {
                                CheckFlags::Readonly
                            } else {
                                CheckFlags::None
                            };
                        if index_types.is_none() {
                            index_types = Some(vec![]);
                        }
                        append(
                            index_types.as_mut().unwrap(),
                            Some(if self.is_tuple_type(type_) {
                                self.get_rest_type_of_tuple_type(type_)?
                                    .unwrap_or_else(|| self.undefined_type())
                            } else {
                                index_info.ref_(self).type_.clone()
                            }),
                        );
                    } else if self.is_object_literal_type(type_)
                        && !get_object_flags(&type_.ref_(self))
                            .intersects(ObjectFlags::ContainsSpread)
                    {
                        check_flags |= CheckFlags::WritePartial;
                        if index_types.is_none() {
                            index_types = Some(vec![]);
                        }
                        append(index_types.as_mut().unwrap(), Some(self.undefined_type()));
                    } else {
                        check_flags |= CheckFlags::ReadPartial;
                    }
                }
            }
        }
        if single_prop.is_none()
            || is_union
                && (prop_set.is_some() || check_flags.intersects(CheckFlags::Partial))
                && check_flags
                    .intersects(CheckFlags::ContainsPrivate | CheckFlags::ContainsProtected)
        {
            return Ok(None);
        }
        let single_prop = single_prop.unwrap();
        if prop_set.is_none()
            && !check_flags.intersects(CheckFlags::ReadPartial)
            && index_types.is_none()
        {
            if merged_instantiations {
                let clone = self.create_symbol_with_type(
                    single_prop,
                    released!(single_prop
                        .ref_(self)
                        .maybe_as_transient_symbol()
                        .and_then(|single_prop| single_prop.symbol_links().ref_(self).type_)),
                );
                clone.ref_(self).set_parent(
                    single_prop
                        .ref_(self)
                        .maybe_value_declaration()
                        .and_then(|value_declaration| value_declaration.ref_(self).maybe_symbol())
                        .and_then(|symbol| symbol.ref_(self).maybe_parent()),
                );
                let clone_symbol_links = clone.ref_(self).as_transient_symbol().symbol_links();
                let mut clone_symbol_links = clone_symbol_links.ref_mut(self);
                clone_symbol_links.containing_type = Some(containing_type);
                clone_symbol_links.mapper = single_prop
                    .ref_(self)
                    .maybe_as_transient_symbol()
                    .and_then(|single_prop| single_prop.symbol_links().ref_(self).mapper);
                return Ok(Some(clone));
            } else {
                return Ok(Some(single_prop));
            }
        }
        let props = if let Some(prop_set) = prop_set.as_ref() {
            prop_set.values().map(Clone::clone).collect()
        } else {
            vec![single_prop]
        };
        let mut declarations: Option<Vec<Id<Node /*Declaration*/>>> = None;
        let mut first_type: Option<Id<Type>> = None;
        let mut name_type: Option<Id<Type>> = None;
        let mut prop_types: Vec<Id<Type>> = vec![];
        let mut first_value_declaration: Option<Id<Node /*Declaration*/>> = None;
        let mut has_non_uniform_value_declaration = false;
        for prop in props {
            if first_value_declaration.is_none() {
                first_value_declaration = prop.ref_(self).maybe_value_declaration();
            } else if matches!(
                prop.ref_(self).maybe_value_declaration(),
                Some(value_declaration) if value_declaration != first_value_declaration.unwrap()
            ) {
                has_non_uniform_value_declaration = true;
            }
            if let Some(prop_declarations) = prop.ref_(self).maybe_declarations().as_deref() {
                if !prop_declarations.is_empty() {
                    if declarations.is_none() {
                        declarations = Some(vec![]);
                    }
                    add_range(
                        declarations.as_mut().unwrap(),
                        Some(prop_declarations),
                        None,
                        None,
                    );
                }
            }
            let type_ = self.get_type_of_symbol(prop)?;
            if first_type.is_none() {
                first_type = Some(type_.clone());
                name_type = self.get_symbol_links(prop).ref_(self).name_type;
            } else if type_ != first_type.unwrap() {
                check_flags |= CheckFlags::HasNonUniformType;
            }
            if self.is_literal_type(type_) || self.is_pattern_literal_type(type_) {
                check_flags |= CheckFlags::HasLiteralType;
            }
            if type_.ref_(self).flags().intersects(TypeFlags::Never) {
                check_flags |= CheckFlags::HasNeverType;
            }
            prop_types.push(type_);
        }
        add_range(&mut prop_types, index_types.as_deref(), None, None);
        let result = self.alloc_symbol(
            self.create_symbol(
                SymbolFlags::Property | optional_flag,
                name.to_owned(),
                Some(synthetic_flag | check_flags),
            )
            .into(),
        );
        let result_links = result.ref_(self).as_transient_symbol().symbol_links();
        result_links.ref_mut(self).containing_type = Some(containing_type);
        if !has_non_uniform_value_declaration {
            if let Some(first_value_declaration) = first_value_declaration {
                result
                    .ref_(self)
                    .set_value_declaration(first_value_declaration.clone());

                if let Some(first_value_declaration_symbol_parent) = first_value_declaration
                    .ref_(self)
                    .symbol()
                    .ref_(self)
                    .maybe_parent()
                {
                    result
                        .ref_(self)
                        .set_parent(Some(first_value_declaration_symbol_parent));
                }
            }
        }

        if let Some(declarations) = declarations {
            result.ref_(self).set_declarations(declarations);
        }
        result_links.ref_mut(self).name_type = name_type;
        if prop_types.len() > 2 {
            let result_ref = result.ref_(self);
            let result_as_transient_symbol = result_ref.as_transient_symbol();
            result_as_transient_symbol.set_check_flags(
                result_as_transient_symbol.check_flags() | CheckFlags::DeferredType,
            );
            result_links.ref_mut(self).deferral_parent = Some(containing_type);
            result_links.ref_mut(self).deferral_constituents = Some(prop_types);
        } else {
            result_links.ref_mut(self).type_ = Some(if is_union {
                self.get_union_type(&prop_types, None, Option::<Id<Symbol>>::None, None, None)?
            } else {
                self.get_intersection_type(&prop_types, Option::<Id<Symbol>>::None, None)?
            });
        }
        Ok(Some(result))
    }

    pub(super) fn get_union_or_intersection_property(
        &self,
        type_: Id<Type>, /*UnionOrIntersectionType*/
        name: &str,      /*__String*/
        skip_object_function_property_augment: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let mut property = type_
            .ref_(self)
            .as_union_or_intersection_type_interface()
            .maybe_property_cache_without_object_function_property_augment()
            .as_ref()
            .and_then(|property_cache_without_object_function_property_augment| {
                property_cache_without_object_function_property_augment
                    .get(name)
                    .map(Clone::clone)
            })
            .or_else(|| {
                if !matches!(skip_object_function_property_augment, Some(true)) {
                    type_
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .maybe_property_cache()
                        .as_ref()
                        .and_then(|property_cache| property_cache.get(name).map(Clone::clone))
                } else {
                    None
                }
            });
        if property.is_none() {
            property = self.create_union_or_intersection_property(
                type_,
                name,
                skip_object_function_property_augment,
            )?;
            if let Some(property) = property.as_ref() {
                if matches!(skip_object_function_property_augment, Some(true)) {
                    let type_ref = type_.ref_(self);
                    let mut property_cache_without_object_function_property_augment = type_ref
                        .as_union_or_intersection_type_interface()
                        .maybe_property_cache_without_object_function_property_augment();
                    if property_cache_without_object_function_property_augment.is_none() {
                        *property_cache_without_object_function_property_augment =
                            Some(create_symbol_table(Option::<&[Id<Symbol>]>::None, self));
                    }
                    property_cache_without_object_function_property_augment
                        .as_mut()
                        .unwrap()
                        .insert(name.to_owned(), property.clone());
                } else {
                    let type_ref = type_.ref_(self);
                    let mut property_cache = type_ref
                        .as_union_or_intersection_type_interface()
                        .maybe_property_cache();
                    if property_cache.is_none() {
                        *property_cache =
                            Some(create_symbol_table(Option::<&[Id<Symbol>]>::None, self));
                    }
                    property_cache
                        .as_mut()
                        .unwrap()
                        .insert(name.to_owned(), property.clone());
                };
            }
        }
        Ok(property)
    }

    pub(super) fn get_property_of_union_or_intersection_type(
        &self,
        type_: Id<Type>, /*UnionOrIntersectionType*/
        name: &str,      /*__String*/
        skip_object_function_property_augment: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let property = self.get_union_or_intersection_property(
            type_,
            name,
            skip_object_function_property_augment,
        )?;
        Ok(property.filter(|&property| {
            !get_check_flags(&property.ref_(self)).intersects(CheckFlags::ReadPartial)
        }))
    }

    pub(super) fn get_reduced_type(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        if type_.ref_(self).flags().intersects(TypeFlags::Union)
            && type_
                .ref_(self)
                .as_union_type()
                .object_flags()
                .intersects(ObjectFlags::ContainsIntersections)
        {
            if type_
                .ref_(self)
                .as_union_type()
                .maybe_resolved_reduced_type()
                .is_none()
            {
                let resolved_reduced_type = self.get_reduced_union_type(type_)?;
                type_
                    .ref_(self)
                    .as_union_type()
                    .set_resolved_reduced_type(Some(resolved_reduced_type));
            }
            return Ok(type_
                .ref_(self)
                .as_union_type()
                .maybe_resolved_reduced_type()
                .unwrap());
        } else if type_.ref_(self).flags().intersects(TypeFlags::Intersection) {
            if !type_
                .ref_(self)
                .as_intersection_type()
                .object_flags()
                .intersects(ObjectFlags::IsNeverIntersectionComputed)
            {
                let mut object_flags = type_.ref_(self).as_intersection_type().object_flags();
                object_flags |= ObjectFlags::IsNeverIntersectionComputed
                    | if self
                        .get_properties_of_union_or_intersection_type(type_)?
                        .into_iter()
                        .try_any(|symbol| self.is_never_reduced_property(symbol))?
                    {
                        ObjectFlags::IsNeverIntersection
                    } else {
                        ObjectFlags::None
                    };
                type_
                    .ref_(self)
                    .as_intersection_type()
                    .set_object_flags(object_flags);
            }
            return Ok(
                if type_
                    .ref_(self)
                    .as_intersection_type()
                    .object_flags()
                    .intersects(ObjectFlags::IsNeverIntersection)
                {
                    self.never_type()
                } else {
                    type_
                },
            );
        }
        Ok(type_)
    }

    pub(super) fn get_reduced_union_type(
        &self,
        union_type: Id<Type>, /*UnionType*/
    ) -> io::Result<Id<Type>> {
        let reduced_types = try_map(
            &released!(union_type.ref_(self).as_union_type().types().to_owned()),
            |&type_: &Id<Type>, _| self.get_reduced_type(type_),
        )?;
        if &reduced_types == union_type.ref_(self).as_union_type().types() {
            return Ok(union_type);
        }
        let reduced = self.get_union_type(&reduced_types, None, None, None, None)?;
        if reduced.ref_(self).flags().intersects(TypeFlags::Union) {
            reduced
                .ref_(self)
                .as_union_type()
                .set_resolved_reduced_type(Some(reduced.clone()));
        }
        Ok(reduced)
    }

    pub(super) fn is_never_reduced_property(&self, prop: Id<Symbol>) -> io::Result<bool> {
        Ok(self.is_discriminant_with_never_type(prop)?
            || self.is_conflicting_private_property(prop))
    }

    pub(super) fn is_discriminant_with_never_type(&self, prop: Id<Symbol>) -> io::Result<bool> {
        Ok(!prop.ref_(self).flags().intersects(SymbolFlags::Optional)
            && get_check_flags(&prop.ref_(self))
                & (CheckFlags::Discriminant | CheckFlags::HasNeverType)
                == CheckFlags::Discriminant
            && self
                .get_type_of_symbol(prop)?
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Never))
    }

    pub(super) fn is_conflicting_private_property(&self, prop: Id<Symbol>) -> bool {
        prop.ref_(self).maybe_value_declaration().is_none()
            && get_check_flags(&prop.ref_(self)).intersects(CheckFlags::ContainsPrivate)
    }

    pub(super) fn elaborate_never_intersection(
        &self,
        error_info: Option<DiagnosticMessageChain>,
        type_: Id<Type>,
    ) -> io::Result<Option<DiagnosticMessageChain>> {
        if type_.ref_(self).flags().intersects(TypeFlags::Intersection)
            && get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::IsNeverIntersection)
        {
            let never_prop = self
                .get_properties_of_union_or_intersection_type(type_)?
                .into_iter()
                .try_find_(|&property| self.is_discriminant_with_never_type(property))?;
            if let Some(never_prop) = never_prop {
                return Ok(Some(
                    chain_diagnostic_messages(
                        error_info,
                        &Diagnostics::The_intersection_0_was_reduced_to_never_because_property_1_has_conflicting_types_in_some_constituents,
                        Some(vec![
                            self.type_to_string_(type_, Option::<Id<Node>>::None, Some(TypeFormatFlags::NoTypeReduction), None)?,
                            self.symbol_to_string_(never_prop, Option::<Id<Node>>::None, None, None, None)?
                        ])
                    )
                ));
            }
            let private_prop = self
                .get_properties_of_union_or_intersection_type(type_)?
                .into_iter()
                .find(|&property| self.is_conflicting_private_property(property));
            if let Some(private_prop) = private_prop {
                return Ok(Some(
                    chain_diagnostic_messages(
                        error_info,
                        &Diagnostics::The_intersection_0_was_reduced_to_never_because_property_1_exists_in_multiple_constituents_and_is_private_in_some,
                        Some(vec![
                            self.type_to_string_(type_, Option::<Id<Node>>::None, Some(TypeFormatFlags::NoTypeReduction), None)?,
                            self.symbol_to_string_(private_prop, Option::<Id<Node>>::None, None, None, None)?
                        ])
                    )
                ));
            }
        }
        Ok(error_info)
    }

    pub(super) fn get_property_of_type_(
        &self,
        type_: Id<Type>,
        name: &str, /*__String*/
        skip_object_function_property_augment: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let type_ = self.get_reduced_apparent_type(type_)?;
        if type_.ref_(self).flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_)?;
            let symbol = resolved
                .ref_(self)
                .as_resolved_type()
                .members()
                .ref_(self)
                .get(name)
                .map(Clone::clone);
            if let Some(symbol) = symbol {
                if self.symbol_is_value(symbol)? {
                    return Ok(Some(symbol));
                }
            }
            if matches!(skip_object_function_property_augment, Some(true)) {
                return Ok(None);
            }
            let function_type = if resolved == self.any_function_type() {
                Some(self.global_function_type())
            } else if !resolved
                .ref_(self)
                .as_resolved_type()
                .call_signatures()
                .is_empty()
            {
                Some(self.global_callable_function_type())
            } else if !resolved
                .ref_(self)
                .as_resolved_type()
                .construct_signatures()
                .is_empty()
            {
                Some(self.global_newable_function_type())
            } else {
                None
            };
            if let Some(function_type) = function_type {
                let symbol = self.get_property_of_object_type(function_type, name)?;
                if symbol.is_some() {
                    return Ok(symbol);
                }
            }
            return self.get_property_of_object_type(self.global_object_type(), name);
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
        {
            return self.get_property_of_union_or_intersection_type(
                type_,
                name,
                skip_object_function_property_augment,
            );
        }
        Ok(None)
    }

    pub(super) fn get_signatures_of_structured_type(
        &self,
        type_: Id<Type>,
        kind: SignatureKind,
    ) -> io::Result<Vec<Id<Signature>>> {
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::StructuredType)
        {
            let resolved = self.resolve_structured_type_members(type_)?;
            return Ok(if kind == SignatureKind::Call {
                resolved
                    .ref_(self)
                    .as_resolved_type()
                    .call_signatures()
                    .clone()
            } else {
                resolved
                    .ref_(self)
                    .as_resolved_type()
                    .construct_signatures()
                    .clone()
            });
        }
        Ok(vec![])
    }

    pub fn get_signatures_of_type(
        &self,
        type_: Id<Type>,
        kind: SignatureKind,
    ) -> io::Result<Vec<Id<Signature>>> {
        self.get_signatures_of_structured_type(self.get_reduced_apparent_type(type_)?, kind)
    }

    pub(super) fn find_index_info(
        &self,
        index_infos: &[Id<IndexInfo>],
        key_type: Id<Type>,
    ) -> Option<Id<IndexInfo>> {
        find(index_infos, |info: &Id<IndexInfo>, _| {
            info.ref_(self).key_type == key_type
        })
        .map(Clone::clone)
    }

    pub(super) fn find_applicable_index_info(
        &self,
        index_infos: &[Id<IndexInfo>],
        key_type: Id<Type>,
    ) -> io::Result<Option<Id<IndexInfo>>> {
        let mut string_index_info: Option<Id<IndexInfo>> = None;
        let mut applicable_info: Option<Id<IndexInfo>> = None;
        let mut applicable_infos: Option<Vec<Id<IndexInfo>>> = None;
        for info in index_infos {
            if info.ref_(self).key_type == self.string_type() {
                string_index_info = Some(info.clone());
            } else if self.is_applicable_index_type(key_type, info.ref_(self).key_type)? {
                if applicable_info.is_none() {
                    applicable_info = Some(info.clone());
                } else {
                    if applicable_infos.is_none() {
                        applicable_infos = Some(vec![applicable_info.clone().unwrap()]);
                    }
                    applicable_infos.as_mut().unwrap().push(info.clone());
                }
            }
        }
        Ok(if let Some(applicable_infos) = applicable_infos {
            Some(self.alloc_index_info(self.create_index_info(
                self.unknown_type(),
                self.get_intersection_type(
                    &map(&applicable_infos, |info: &Id<IndexInfo>, _| {
                        info.ref_(self).type_.clone()
                    }),
                    Option::<Id<Symbol>>::None,
                    None,
                )?,
                reduce_left(
                    &applicable_infos,
                    |is_readonly, info: &Id<IndexInfo>, _| {
                        is_readonly && info.ref_(self).is_readonly
                    },
                    true,
                    None,
                    None,
                ),
                None,
            )))
        } else if applicable_info.is_some() {
            applicable_info
        } else if string_index_info.is_some()
            && self.is_applicable_index_type(key_type, self.string_type())?
        {
            string_index_info
        } else {
            None
        })
    }

    pub(super) fn is_applicable_index_type(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<bool> {
        Ok(self.is_type_assignable_to(source, target)?
            || target == self.string_type()
                && self.is_type_assignable_to(source, self.number_type())?
            || target == self.number_type()
                && source
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::StringLiteral)
                && self.is_numeric_literal_name(&source.ref_(self).as_string_literal_type().value))
    }

    pub(super) fn get_index_infos_of_structured_type(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Vec<Id<IndexInfo>>> {
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::StructuredType)
        {
            let resolved = self.resolve_structured_type_members(type_)?;
            return Ok(resolved.ref_(self).as_resolved_type().index_infos().clone());
        }
        Ok(vec![])
    }

    pub(super) fn get_index_infos_of_type(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Vec<Id<IndexInfo>>> {
        self.get_index_infos_of_structured_type(self.get_reduced_apparent_type(type_)?)
    }

    pub(super) fn get_index_info_of_type_(
        &self,
        type_: Id<Type>,
        key_type: Id<Type>,
    ) -> io::Result<Option<Id<IndexInfo>>> {
        Ok(self.find_index_info(&self.get_index_infos_of_type(type_)?, key_type))
    }

    pub(super) fn get_index_type_of_type_(
        &self,
        type_: Id<Type>,
        key_type: Id<Type>,
    ) -> io::Result<Option<Id<Type>>> {
        Ok(self
            .get_index_info_of_type_(type_, key_type)?
            .map(|index_info| index_info.ref_(self).type_.clone()))
    }

    pub(super) fn get_applicable_index_infos(
        &self,
        type_: Id<Type>,
        key_type: Id<Type>,
    ) -> io::Result<Vec<Id<IndexInfo>>> {
        try_filter(
            &self.get_index_infos_of_type(type_)?,
            |info: &Id<IndexInfo>| {
                self.is_applicable_index_type(key_type, info.ref_(self).key_type)
            },
        )
    }

    pub(super) fn get_applicable_index_info(
        &self,
        type_: Id<Type>,
        key_type: Id<Type>,
    ) -> io::Result<Option<Id<IndexInfo>>> {
        self.find_applicable_index_info(&*self.get_index_infos_of_type(type_)?, key_type)
    }

    pub(super) fn get_applicable_index_info_for_name(
        &self,
        type_: Id<Type>,
        name: &str, /*__String*/
    ) -> io::Result<Option<Id<IndexInfo>>> {
        self.get_applicable_index_info(
            type_,
            if self.is_late_bound_name(name) {
                self.es_symbol_type()
            } else {
                self.get_string_literal_type(&unescape_leading_underscores(name))
            },
        )
    }

    pub(super) fn get_type_parameters_from_declaration(
        &self,
        declaration: Id<Node>, /*DeclarationWithTypeParameters*/
    ) -> Option<Vec<Id<Type /*<TypeParameter>*/>>> {
        let mut result: Option<Vec<Id<Type>>> = None;
        for node in get_effective_type_parameter_declarations(declaration, self) {
            result = Some(maybe_append_if_unique_eq(
                result,
                &self.get_declared_type_of_type_parameter(node.ref_(self).symbol()),
            ));
        }
        result
    }

    pub(super) fn symbols_to_array(&self, symbols: &SymbolTable) -> Vec<Id<Symbol>> {
        let mut result: Vec<Id<Symbol>> = vec![];
        for (id, symbol) in symbols {
            if !self.is_reserved_member_name(id) {
                result.push(symbol.clone());
            }
        }
        result
    }

    pub(super) fn is_jsdoc_optional_parameter(
        &self,
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> bool {
        is_in_js_file(Some(&node.ref_(self))) && (
            matches!(
                node.ref_(self).as_parameter_declaration().maybe_type(),
                Some(type_) if type_.ref_(self).kind() == SyntaxKind::JSDocOptionalType
            ) || get_jsdoc_parameter_tags(node, self).into_iter().any(|tag: Id<Node>| {
                let tag_ref = tag.ref_(self);
                let tag_as_jsdoc_property_like_tag = tag_ref.as_jsdoc_property_like_tag();
                let is_bracketed = tag_as_jsdoc_property_like_tag.is_bracketed;
                let type_expression = tag_as_jsdoc_property_like_tag.type_expression;
                is_bracketed || matches!(
                    type_expression,
                    Some(type_expression) if type_expression.ref_(self).as_jsdoc_type_expression().type_.ref_(self).kind() == SyntaxKind::JSDocOptionalType
                )
            })
        )
    }

    pub(super) fn try_find_ambient_module_(
        &self,
        module_name: &str,
        with_augmentations: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        if is_external_module_name_relative(module_name) {
            return Ok(None);
        }
        let symbol = self.get_symbol(
            self.globals,
            &format!("\"{}\"", module_name),
            SymbolFlags::ValueModule,
        )?;
        Ok(if symbol.is_some() && with_augmentations {
            self.get_merged_symbol(symbol)
        } else {
            symbol
        })
    }

    pub(super) fn is_optional_parameter_(
        &self,
        node: Id<Node>, /*ParameterDeclaration | JSDocParameterTag | JSDocPropertyTag*/
    ) -> io::Result<bool> {
        if has_question_token(&node.ref_(self))
            || self.is_optional_jsdoc_property_like_tag(node)
            || self.is_jsdoc_optional_parameter(node)
        {
            return Ok(true);
        }

        let node_ref = node.ref_(self);
        let node_as_parameter_declaration = node_ref.as_parameter_declaration();
        if node_as_parameter_declaration.maybe_initializer().is_some() {
            let signature = self.get_signature_from_declaration_(node.ref_(self).parent())?;
            let parameter_index = index_of_eq(
                &node
                    .ref_(self)
                    .parent()
                    .ref_(self)
                    .as_signature_declaration()
                    .parameters()
                    .ref_(self),
                &node,
            );
            Debug_.assert(parameter_index >= 0, None);
            let parameter_index: usize = parameter_index.try_into().unwrap();
            return Ok(parameter_index
                >= self.get_min_argument_count(
                    signature,
                    Some(
                        MinArgumentCountFlags::StrongArityForUntypedJS
                            | MinArgumentCountFlags::VoidIsNonOptional,
                    ),
                )?);
        }
        let iife = get_immediately_invoked_function_expression(node.ref_(self).parent(), self);
        if let Some(iife) = iife {
            return Ok(node_as_parameter_declaration.maybe_type().is_none()
                && node_as_parameter_declaration.dot_dot_dot_token.is_none()
                && index_of_eq(
                    &node
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .as_signature_declaration()
                        .parameters()
                        .ref_(self),
                    &node,
                ) >= iife
                    .ref_(self)
                    .as_call_expression()
                    .arguments
                    .ref_(self)
                    .len()
                    .try_into()
                    .unwrap());
        }

        Ok(false)
    }

    pub(super) fn is_optional_property_declaration(
        &self,
        node: Id<Node>, /*Declaration*/
    ) -> bool {
        is_property_declaration(&node.ref_(self))
            && node
                .ref_(self)
                .as_property_declaration()
                .question_token
                .is_some()
    }

    pub(super) fn is_optional_jsdoc_property_like_tag(&self, node: Id<Node>) -> bool {
        if !is_jsdoc_property_like_tag(&node.ref_(self)) {
            return false;
        }
        let node_ref = node.ref_(self);
        let node_as_jsdoc_property_like_tag = node_ref.as_jsdoc_property_like_tag();
        let is_bracketed = node_as_jsdoc_property_like_tag.is_bracketed;
        let type_expression = node_as_jsdoc_property_like_tag.type_expression;
        is_bracketed
            || matches!(
                type_expression,
                Some(type_expression) if type_expression.ref_(self).as_jsdoc_type_expression().type_.ref_(self).kind() == SyntaxKind::JSDocOptionalType
            )
    }

    pub(super) fn create_type_predicate(
        &self,
        kind: TypePredicateKind,
        parameter_name: Option<String>,
        parameter_index: Option<usize>,
        type_: Option<Id<Type>>,
    ) -> TypePredicate {
        TypePredicate {
            kind,
            parameter_name,
            parameter_index,
            type_,
        }
    }

    pub(super) fn get_min_type_argument_count(
        &self,
        type_parameters: Option<&[Id<Type /*TypeParameter*/>]>,
    ) -> usize {
        let mut min_type_argument_count = 0;
        if let Some(type_parameters) = type_parameters {
            for (i, type_parameter) in type_parameters.iter().enumerate() {
                if !self.has_type_parameter_default(*type_parameter) {
                    min_type_argument_count = i + 1;
                }
            }
        }
        min_type_argument_count
    }
}
