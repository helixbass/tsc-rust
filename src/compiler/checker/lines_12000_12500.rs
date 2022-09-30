#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::{get_symbol_id, MinArgumentCountFlags};
use crate::{
    add_range, append, are_rc_slices_equal, chain_diagnostic_messages, create_symbol_table, find,
    get_check_flags, get_declaration_modifier_flags_from_symbol,
    get_effective_type_parameter_declarations, get_immediately_invoked_function_expression,
    get_jsdoc_parameter_tags, has_question_token, index_of_rc, is_external_module_name_relative,
    is_in_js_file, is_jsdoc_property_like_tag, is_property_declaration, length,
    maybe_append_if_unique_rc, reduce_left, same_map, some, CheckFlags, Debug_,
    DiagnosticMessageChain, HasInitializerInterface, HasTypeInterface, IndexInfo, ModifierFlags,
    ScriptTarget, Signature, SignatureKind, SymbolId, SymbolTable, Ternary,
    TransientSymbolInterface, TypeFormatFlags, TypePredicate, TypePredicateKind,
    UnionOrIntersectionTypeInterface, __String, get_object_flags, map,
    unescape_leading_underscores, Diagnostics, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_apparent_type(&self, type_: &Type) -> Rc<Type> {
        let t = if type_.flags().intersects(TypeFlags::Instantiable) {
            self.get_base_constraint_of_type(type_)
                .unwrap_or_else(|| self.unknown_type())
        } else {
            type_.type_wrapper()
        };
        if get_object_flags(&t).intersects(ObjectFlags::Mapped) {
            self.get_apparent_type_of_mapped_type(&t)
        } else if t.flags().intersects(TypeFlags::Intersection) {
            self.get_apparent_type_of_intersection_type(&t)
        } else if t.flags().intersects(TypeFlags::StringLike) {
            self.global_string_type()
        } else if t.flags().intersects(TypeFlags::NumberLike) {
            self.global_number_type()
        } else if t.flags().intersects(TypeFlags::BigIntLike) {
            self.get_global_big_int_type(self.language_version >= ScriptTarget::ES2020)
        } else if t.flags().intersects(TypeFlags::BooleanLike) {
            self.global_boolean_type()
        } else if t.flags().intersects(TypeFlags::ESSymbolLike) {
            self.get_global_es_symbol_type(self.language_version >= ScriptTarget::ES2015)
        } else if t.flags().intersects(TypeFlags::NonPrimitive) {
            self.empty_object_type()
        } else if t.flags().intersects(TypeFlags::Index) {
            self.keyof_constraint_type()
        } else if t.flags().intersects(TypeFlags::Unknown) && !self.strict_null_checks {
            self.empty_object_type()
        } else {
            t
        }
    }

    pub(super) fn get_reduced_apparent_type(&self, type_: &Type) -> Rc<Type> {
        self.get_reduced_type(&self.get_apparent_type(&self.get_reduced_type(type_)))
    }

    pub(super) fn create_union_or_intersection_property(
        &self,
        containing_type: &Type, /*UnionOrIntersectionType*/
        name: &__String,
        skip_object_function_property_augment: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let mut single_prop: Option<Rc<Symbol>> = None;
        let mut prop_set: Option<HashMap<SymbolId, Rc<Symbol>>> = None;
        let mut index_types: Option<Vec<Rc<Type>>> = None;
        let is_union = containing_type.flags().intersects(TypeFlags::Union);
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
        for current in containing_type
            .as_union_or_intersection_type_interface()
            .types()
        {
            let type_ = self.get_apparent_type(current);
            if !(self.is_error_type(&type_) || type_.flags().intersects(TypeFlags::Never)) {
                let prop =
                    self.get_property_of_type_(&type_, name, skip_object_function_property_augment);
                let modifiers = if let Some(prop) = prop.as_ref() {
                    get_declaration_modifier_flags_from_symbol(prop, None)
                } else {
                    ModifierFlags::None
                };
                if let Some(prop) = prop {
                    if is_union {
                        optional_flag |= prop.flags() & SymbolFlags::Optional;
                    } else {
                        optional_flag &= prop.flags();
                    }
                    if single_prop.is_none() {
                        single_prop = Some(prop.clone());
                    } else if !Rc::ptr_eq(&prop, single_prop.as_ref().unwrap()) {
                        let single_prop = single_prop.as_ref().unwrap();
                        let is_instantiation = Rc::ptr_eq(
                            &self.get_target_symbol(&prop),       /*|| prop*/
                            &self.get_target_symbol(single_prop), /*|| singleProp*/
                        );
                        if is_instantiation
                            && self.compare_properties(single_prop, &prop, |a, b| {
                                if ptr::eq(a, b) {
                                    Ternary::True
                                } else {
                                    Ternary::False
                                }
                            }) == Ternary::True
                        {
                            merged_instantiations = matches!(
                                single_prop.maybe_parent(),
                                Some(parent) if length(self.get_local_type_parameters_of_class_or_interface_or_type_alias(&parent).as_deref()) != 0
                            );
                        } else {
                            if prop_set.is_none() {
                                prop_set = Some(HashMap::new());
                                prop_set
                                    .as_mut()
                                    .unwrap()
                                    .insert(get_symbol_id(single_prop), single_prop.clone());
                            }
                            let prop_set = prop_set.as_mut().unwrap();
                            let id = get_symbol_id(&prop);
                            if !prop_set.contains_key(&id) {
                                prop_set.insert(id, prop.clone());
                            }
                        }
                    }
                    if is_union && self.is_readonly_symbol(&prop) {
                        check_flags |= CheckFlags::Readonly;
                    } else if !is_union && !self.is_readonly_symbol(&prop) {
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
                    if !self.is_prototype_property(&prop) {
                        synthetic_flag = CheckFlags::SyntheticProperty;
                    }
                } else if is_union {
                    let index_info = if !self.is_late_bound_name(name) {
                        self.get_applicable_index_info_for_name(&type_, name)
                    } else {
                        None
                    };
                    if let Some(index_info) = index_info {
                        check_flags |= CheckFlags::WritePartial
                            | if index_info.is_readonly {
                                CheckFlags::Readonly
                            } else {
                                CheckFlags::None
                            };
                        if index_types.is_none() {
                            index_types = Some(vec![]);
                        }
                        append(
                            index_types.as_mut().unwrap(),
                            Some(if self.is_tuple_type(&type_) {
                                self.get_rest_type_of_tuple_type(&type_)
                                    .unwrap_or_else(|| self.undefined_type())
                            } else {
                                index_info.type_.clone()
                            }),
                        );
                    } else if self.is_object_literal_type(&type_)
                        && !get_object_flags(&type_).intersects(ObjectFlags::ContainsSpread)
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
            return None;
        }
        let single_prop = single_prop.unwrap();
        if prop_set.is_none()
            && !check_flags.intersects(CheckFlags::ReadPartial)
            && index_types.is_none()
        {
            if merged_instantiations {
                let clone = self.create_symbol_with_type(
                    &single_prop,
                    (*single_prop.as_transient_symbol().symbol_links())
                        .borrow()
                        .type_
                        .as_deref(),
                );
                clone.set_parent(
                    single_prop
                        .maybe_value_declaration()
                        .and_then(|value_declaration| value_declaration.maybe_symbol())
                        .and_then(|symbol| symbol.maybe_parent()),
                );
                let clone_symbol_links = clone.as_transient_symbol().symbol_links();
                let mut clone_symbol_links = clone_symbol_links.borrow_mut();
                clone_symbol_links.containing_type = Some(containing_type.type_wrapper());
                clone_symbol_links.mapper = (*single_prop.as_transient_symbol().symbol_links())
                    .borrow()
                    .mapper
                    .clone();
                return Some(clone);
            } else {
                return Some(single_prop);
            }
        }
        let props = if let Some(prop_set) = prop_set.as_ref() {
            prop_set.values().map(Clone::clone).collect()
        } else {
            vec![single_prop]
        };
        let mut declarations: Option<Vec<Rc<Node /*Declaration*/>>> = None;
        let mut first_type: Option<Rc<Type>> = None;
        let mut name_type: Option<Rc<Type>> = None;
        let mut prop_types: Vec<Rc<Type>> = vec![];
        let mut first_value_declaration: Option<Rc<Node /*Declaration*/>> = None;
        let mut has_non_uniform_value_declaration = false;
        for prop in props {
            if first_value_declaration.is_none() {
                first_value_declaration = prop.maybe_value_declaration();
            } else if matches!(
                prop.maybe_value_declaration(),
                Some(value_declaration) if !Rc::ptr_eq(&value_declaration, first_value_declaration.as_ref().unwrap())
            ) {
                has_non_uniform_value_declaration = true;
            }
            if let Some(prop_declarations) = prop.maybe_declarations().as_deref() {
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
            let type_ = self.get_type_of_symbol(&prop);
            if first_type.is_none() {
                first_type = Some(type_.clone());
                name_type = (*self.get_symbol_links(&prop)).borrow().name_type.clone();
            } else if !Rc::ptr_eq(&type_, first_type.as_ref().unwrap()) {
                check_flags |= CheckFlags::HasNonUniformType;
            }
            if self.is_literal_type(&type_) || self.is_pattern_literal_type(&type_) {
                check_flags |= CheckFlags::HasLiteralType;
            }
            if type_.flags().intersects(TypeFlags::Never) {
                check_flags |= CheckFlags::HasNeverType;
            }
            prop_types.push(type_);
        }
        add_range(&mut prop_types, index_types.as_deref(), None, None);
        let result: Rc<Symbol> = self
            .create_symbol(
                SymbolFlags::Property | optional_flag,
                name.clone(),
                Some(synthetic_flag | check_flags),
            )
            .into();
        let result_links = result.as_transient_symbol().symbol_links();
        let mut result_links = result_links.borrow_mut();
        result_links.containing_type = Some(containing_type.type_wrapper());
        if !has_non_uniform_value_declaration {
            if let Some(first_value_declaration) = first_value_declaration {
                result.set_value_declaration(first_value_declaration.clone());

                if let Some(first_value_declaration_symbol_parent) =
                    first_value_declaration.symbol().maybe_parent()
                {
                    result.set_parent(Some(first_value_declaration_symbol_parent));
                }
            }
        }

        if let Some(declarations) = declarations {
            result.set_declarations(declarations);
        }
        result_links.name_type = name_type;
        if prop_types.len() > 2 {
            let result_as_transient_symbol = result.as_transient_symbol();
            result_as_transient_symbol.set_check_flags(
                result_as_transient_symbol.check_flags() | CheckFlags::DeferredType,
            );
            result_links.deferral_parent = Some(containing_type.type_wrapper());
            result_links.deferral_constituents = Some(prop_types);
        } else {
            result_links.type_ = Some(if is_union {
                self.get_union_type(
                    prop_types,
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                )
            } else {
                self.get_intersection_type(&prop_types, Option::<&Symbol>::None, None)
            });
        }
        Some(result)
    }

    pub(super) fn get_union_or_intersection_property(
        &self,
        type_: &Type, /*UnionOrIntersectionType*/
        name: &__String,
        skip_object_function_property_augment: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let type_as_union_or_intersection_type = type_.as_union_or_intersection_type_interface();
        let mut property = type_as_union_or_intersection_type
            .maybe_property_cache_without_object_function_property_augment()
            .as_ref()
            .and_then(|property_cache_without_object_function_property_augment| {
                property_cache_without_object_function_property_augment
                    .get(name)
                    .map(Clone::clone)
            })
            .or_else(|| {
                if !matches!(skip_object_function_property_augment, Some(true)) {
                    type_as_union_or_intersection_type
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
            );
            if let Some(property) = property.as_ref() {
                if matches!(skip_object_function_property_augment, Some(true)) {
                    let mut property_cache_without_object_function_property_augment =
                        type_as_union_or_intersection_type
                            .maybe_property_cache_without_object_function_property_augment();
                    if property_cache_without_object_function_property_augment.is_none() {
                        *property_cache_without_object_function_property_augment =
                            Some(create_symbol_table(None));
                    }
                    property_cache_without_object_function_property_augment
                        .as_mut()
                        .unwrap()
                        .insert(name.clone(), property.clone());
                } else {
                    let mut property_cache =
                        type_as_union_or_intersection_type.maybe_property_cache();
                    if property_cache.is_none() {
                        *property_cache = Some(create_symbol_table(None));
                    }
                    property_cache
                        .as_mut()
                        .unwrap()
                        .insert(name.clone(), property.clone());
                };
            }
        }
        property
    }

    pub(super) fn get_property_of_union_or_intersection_type(
        &self,
        type_: &Type, /*UnionOrIntersectionType*/
        name: &__String,
        skip_object_function_property_augment: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let property = self.get_union_or_intersection_property(
            type_,
            name,
            skip_object_function_property_augment,
        );
        property.filter(|property| !get_check_flags(property).intersects(CheckFlags::ReadPartial))
    }

    pub(super) fn get_reduced_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union)
            && type_
                .as_union_type()
                .object_flags()
                .intersects(ObjectFlags::ContainsIntersections)
        {
            let type_as_union_type = type_.as_union_type();
            if type_as_union_type.maybe_resolved_reduced_type().is_none() {
                *type_as_union_type.maybe_resolved_reduced_type() =
                    Some(self.get_reduced_union_type(type_));
            }
            return (*type_as_union_type.maybe_resolved_reduced_type())
                .borrow()
                .clone()
                .unwrap();
        } else if type_.flags().intersects(TypeFlags::Intersection) {
            let type_as_intersection_type = type_.as_intersection_type();
            if !type_as_intersection_type
                .object_flags()
                .intersects(ObjectFlags::IsNeverIntersectionComputed)
            {
                type_as_intersection_type.set_object_flags(
                    type_as_intersection_type.object_flags()
                        | ObjectFlags::IsNeverIntersectionComputed
                        | if some(
                            Some(&self.get_properties_of_union_or_intersection_type(type_)),
                            Some(|symbol: &Rc<Symbol>| self.is_never_reduced_property(symbol)),
                        ) {
                            ObjectFlags::IsNeverIntersection
                        } else {
                            ObjectFlags::None
                        },
                );
            }
            return if type_as_intersection_type
                .object_flags()
                .intersects(ObjectFlags::IsNeverIntersection)
            {
                self.never_type()
            } else {
                type_.type_wrapper()
            };
        }
        type_.type_wrapper()
    }

    pub(super) fn get_reduced_union_type(&self, union_type: &Type /*UnionType*/) -> Rc<Type> {
        let union_type_as_union_type = union_type.as_union_type();
        let reduced_types = same_map(union_type_as_union_type.types(), |type_: &Rc<Type>, _| {
            self.get_reduced_type(type_)
        });
        if are_rc_slices_equal(&reduced_types, union_type_as_union_type.types()) {
            return union_type.type_wrapper();
        }
        let reduced = self.get_union_type(
            reduced_types,
            None,
            Option::<Symbol>::None,
            None,
            Option::<&Type>::None,
        );
        if reduced.flags().intersects(TypeFlags::Union) {
            *reduced.as_union_type().maybe_resolved_reduced_type() = Some(reduced.clone());
        }
        reduced
    }

    pub(super) fn is_never_reduced_property(&self, prop: &Symbol) -> bool {
        self.is_discriminant_with_never_type(prop) || self.is_conflicting_private_property(prop)
    }

    pub(super) fn is_discriminant_with_never_type(&self, prop: &Symbol) -> bool {
        !prop.flags().intersects(SymbolFlags::Optional)
            && get_check_flags(prop) & (CheckFlags::Discriminant | CheckFlags::HasNeverType)
                == CheckFlags::Discriminant
            && self
                .get_type_of_symbol(prop)
                .flags()
                .intersects(TypeFlags::Never)
    }

    pub(super) fn is_conflicting_private_property(&self, prop: &Symbol) -> bool {
        prop.maybe_value_declaration().is_none()
            && get_check_flags(prop).intersects(CheckFlags::ContainsPrivate)
    }

    pub(super) fn elaborate_never_intersection(
        &self,
        error_info: Option<DiagnosticMessageChain>,
        type_: &Type,
    ) -> Option<DiagnosticMessageChain> {
        if type_.flags().intersects(TypeFlags::Intersection)
            && get_object_flags(type_).intersects(ObjectFlags::IsNeverIntersection)
        {
            let never_prop = find(
                &self.get_properties_of_union_or_intersection_type(type_),
                |property: &Rc<Symbol>, _| self.is_discriminant_with_never_type(property),
            )
            .map(Clone::clone);
            if let Some(never_prop) = never_prop {
                return Some(
                    chain_diagnostic_messages(
                        error_info,
                        &Diagnostics::The_intersection_0_was_reduced_to_never_because_property_1_has_conflicting_types_in_some_constituents,
                        Some(vec![
                            self.type_to_string_(type_, Option::<&Node>::None, Some(TypeFormatFlags::NoTypeReduction), None),
                            self.symbol_to_string_(&never_prop, Option::<&Node>::None, None, None, None)
                        ])
                    )
                );
            }
            let private_prop = find(
                &self.get_properties_of_union_or_intersection_type(type_),
                |property: &Rc<Symbol>, _| self.is_conflicting_private_property(property),
            )
            .map(Clone::clone);
            if let Some(private_prop) = private_prop {
                return Some(
                    chain_diagnostic_messages(
                        error_info,
                        &Diagnostics::The_intersection_0_was_reduced_to_never_because_property_1_exists_in_multiple_constituents_and_is_private_in_some,
                        Some(vec![
                            self.type_to_string_(type_, Option::<&Node>::None, Some(TypeFormatFlags::NoTypeReduction), None),
                            self.symbol_to_string_(&private_prop, Option::<&Node>::None, None, None, None)
                        ])
                    )
                );
            }
        }
        error_info
    }

    pub(super) fn get_property_of_type_(
        &self,
        type_: &Type,
        name: &__String,
        skip_object_function_property_augment: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(&type_);
            let symbol = (*resolved.as_resolved_type().members())
                .borrow()
                .get(name)
                .map(Clone::clone);
            if let Some(symbol) = symbol {
                if self.symbol_is_value(&symbol) {
                    return Some(symbol);
                }
            }
            if matches!(skip_object_function_property_augment, Some(true)) {
                return None;
            }
            let resolved_as_resolved_type = resolved.as_resolved_type();
            let function_type = if Rc::ptr_eq(&resolved, &self.any_function_type()) {
                Some(self.global_function_type())
            } else if !resolved_as_resolved_type.call_signatures().is_empty() {
                Some(self.global_callable_function_type())
            } else if !resolved_as_resolved_type.construct_signatures().is_empty() {
                Some(self.global_newable_function_type())
            } else {
                None
            };
            if let Some(function_type) = function_type {
                let symbol = self.get_property_of_object_type(&function_type, name);
                if symbol.is_some() {
                    return symbol;
                }
            }
            return self.get_property_of_object_type(&self.global_object_type(), name);
        }
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            return self.get_property_of_union_or_intersection_type(
                &type_,
                name,
                skip_object_function_property_augment,
            );
        }
        None
    }

    pub(super) fn get_signatures_of_structured_type(
        &self,
        type_: &Type,
        kind: SignatureKind,
    ) -> Vec<Rc<Signature>> {
        if type_.flags().intersects(TypeFlags::StructuredType) {
            let resolved = self.resolve_structured_type_members(type_);
            let resolved_as_resolved_type = resolved.as_resolved_type();
            return if kind == SignatureKind::Call {
                resolved_as_resolved_type.call_signatures().clone()
            } else {
                resolved_as_resolved_type.construct_signatures().clone()
            };
        }
        vec![]
    }

    pub fn get_signatures_of_type(&self, type_: &Type, kind: SignatureKind) -> Vec<Rc<Signature>> {
        self.get_signatures_of_structured_type(&self.get_reduced_apparent_type(type_), kind)
    }

    pub(super) fn find_index_info(
        &self,
        index_infos: &[Rc<IndexInfo>],
        key_type: &Type,
    ) -> Option<Rc<IndexInfo>> {
        find(index_infos, |info: &Rc<IndexInfo>, _| {
            ptr::eq(&*info.key_type, key_type)
        })
        .map(Clone::clone)
    }

    pub(super) fn find_applicable_index_info(
        &self,
        index_infos: &[Rc<IndexInfo>],
        key_type: &Type,
    ) -> Option<Rc<IndexInfo>> {
        let mut string_index_info: Option<Rc<IndexInfo>> = None;
        let mut applicable_info: Option<Rc<IndexInfo>> = None;
        let mut applicable_infos: Option<Vec<Rc<IndexInfo>>> = None;
        for info in index_infos {
            if Rc::ptr_eq(&info.key_type, &self.string_type()) {
                string_index_info = Some(info.clone());
            } else if self.is_applicable_index_type(key_type, &info.key_type) {
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
        if let Some(applicable_infos) = applicable_infos {
            Some(Rc::new(self.create_index_info(
                self.unknown_type(),
                self.get_intersection_type(
                    &map(&applicable_infos, |info: &Rc<IndexInfo>, _| {
                        info.type_.clone()
                    }),
                    Option::<&Symbol>::None,
                    None,
                ),
                reduce_left(
                    &applicable_infos,
                    |is_readonly, info: &Rc<IndexInfo>, _| is_readonly && info.is_readonly,
                    true,
                    None,
                    None,
                ),
                None,
            )))
        } else if applicable_info.is_some() {
            applicable_info
        } else if string_index_info.is_some()
            && self.is_applicable_index_type(key_type, &self.string_type())
        {
            string_index_info
        } else {
            None
        }
    }

    pub(super) fn is_applicable_index_type(&self, source: &Type, target: &Type) -> bool {
        self.is_type_assignable_to(source, target)
            || ptr::eq(target, &*self.string_type())
                && self.is_type_assignable_to(source, &self.number_type())
            || ptr::eq(target, &*self.number_type())
                && source.flags().intersects(TypeFlags::StringLiteral)
                && self.is_numeric_literal_name(&source.as_string_literal_type().value)
    }

    pub(super) fn get_index_infos_of_structured_type(&self, type_: &Type) -> Vec<Rc<IndexInfo>> {
        if type_.flags().intersects(TypeFlags::StructuredType) {
            let resolved = self.resolve_structured_type_members(type_);
            return resolved.as_resolved_type().index_infos().clone();
        }
        vec![]
    }

    pub(super) fn get_index_infos_of_type(&self, type_: &Type) -> Vec<Rc<IndexInfo>> {
        self.get_index_infos_of_structured_type(&self.get_reduced_apparent_type(type_))
    }

    pub(super) fn get_index_info_of_type_(
        &self,
        type_: &Type,
        key_type: &Type,
    ) -> Option<Rc<IndexInfo>> {
        self.find_index_info(&self.get_index_infos_of_type(type_), key_type)
    }

    pub(super) fn get_index_type_of_type_(
        &self,
        type_: &Type,
        key_type: &Type,
    ) -> Option<Rc<Type>> {
        self.get_index_info_of_type_(type_, key_type)
            .map(|index_info| index_info.type_.clone())
    }

    pub(super) fn get_applicable_index_infos(
        &self,
        type_: &Type,
        key_type: &Type,
    ) -> Vec<Rc<IndexInfo>> {
        self.get_index_infos_of_type(type_)
            .into_iter()
            .filter(|info: &Rc<IndexInfo>| self.is_applicable_index_type(key_type, &info.key_type))
            .collect()
    }

    pub(super) fn get_applicable_index_info(
        &self,
        type_: &Type,
        key_type: &Type,
    ) -> Option<Rc<IndexInfo>> {
        self.find_applicable_index_info(&self.get_index_infos_of_type(type_), key_type)
    }

    pub(super) fn get_applicable_index_info_for_name(
        &self,
        type_: &Type,
        name: &__String,
    ) -> Option<Rc<IndexInfo>> {
        self.get_applicable_index_info(
            type_,
            &*if self.is_late_bound_name(name) {
                self.es_symbol_type()
            } else {
                self.get_string_literal_type(&unescape_leading_underscores(name))
            },
        )
    }

    pub(super) fn get_type_parameters_from_declaration(
        &self,
        declaration: &Node, /*DeclarationWithTypeParameters*/
    ) -> Option<Vec<Rc<Type /*<TypeParameter>*/>>> {
        let mut result: Option<Vec<Rc<Type>>> = None;
        for node in get_effective_type_parameter_declarations(declaration) {
            result = Some(maybe_append_if_unique_rc(
                result,
                &self.get_declared_type_of_type_parameter(&node.symbol()),
            ));
        }
        result
    }

    pub(super) fn symbols_to_array(&self, symbols: &SymbolTable) -> Vec<Rc<Symbol>> {
        let mut result: Vec<Rc<Symbol>> = vec![];
        for (id, symbol) in symbols {
            if !self.is_reserved_member_name(id) {
                result.push(symbol.clone());
            }
        }
        result
    }

    pub(super) fn is_jsdoc_optional_parameter(
        &self,
        node: &Node, /*ParameterDeclaration*/
    ) -> bool {
        is_in_js_file(Some(node)) && (
            matches!(node.as_parameter_declaration().maybe_type(), Some(type_) if type_.kind() == SyntaxKind::JSDocOptionalType) ||
            get_jsdoc_parameter_tags(node).iter().any(|tag: &Rc<Node>| {
                let tag_as_jsdoc_property_like_tag = tag.as_jsdoc_property_like_tag();
                let is_bracketed = tag_as_jsdoc_property_like_tag.is_bracketed;
                let type_expression = tag_as_jsdoc_property_like_tag.type_expression.as_ref();
                is_bracketed || matches!(
                    type_expression,
                    Some(type_expression) if type_expression.as_jsdoc_type_expression().type_.kind() == SyntaxKind::JSDocOptionalType
                )
            })
        )
    }

    pub(super) fn try_find_ambient_module_(
        &self,
        module_name: &str,
        with_augmentations: bool,
    ) -> Option<Rc<Symbol>> {
        if is_external_module_name_relative(module_name) {
            return None;
        }
        let symbol = self.get_symbol(
            &self.globals(),
            &__String::new(format!("\"{}\"", module_name)),
            SymbolFlags::ValueModule,
        );
        if symbol.is_some() && with_augmentations {
            self.get_merged_symbol(symbol)
        } else {
            symbol
        }
    }

    pub(super) fn is_optional_parameter_(
        &self,
        node: &Node, /*ParameterDeclaration | JSDocParameterTag | JSDocPropertyTag*/
    ) -> bool {
        if has_question_token(node)
            || self.is_optional_jsdoc_property_like_tag(node)
            || self.is_jsdoc_optional_parameter(node)
        {
            return true;
        }

        let node_as_parameter_declaration = node.as_parameter_declaration();
        if node_as_parameter_declaration.maybe_initializer().is_some() {
            let signature = self.get_signature_from_declaration_(&node.parent());
            let parameter_index = index_of_rc(
                node.parent().as_signature_declaration().parameters(),
                &node.node_wrapper(),
            );
            Debug_.assert(parameter_index >= 0, None);
            let parameter_index: usize = parameter_index.try_into().unwrap();
            return parameter_index
                >= self.get_min_argument_count(
                    &signature,
                    Some(
                        MinArgumentCountFlags::StrongArityForUntypedJS
                            | MinArgumentCountFlags::VoidIsNonOptional,
                    ),
                );
        }
        let iife = get_immediately_invoked_function_expression(&node.parent());
        if let Some(iife) = iife {
            return node_as_parameter_declaration.maybe_type().is_none()
                && node_as_parameter_declaration.dot_dot_dot_token.is_none()
                && index_of_rc(
                    node.parent().as_signature_declaration().parameters(),
                    &node.node_wrapper(),
                ) >= iife
                    .as_call_expression()
                    .arguments
                    .len()
                    .try_into()
                    .unwrap();
        }

        false
    }

    pub(super) fn is_optional_property_declaration(
        &self,
        node: &Node, /*Declaration*/
    ) -> bool {
        is_property_declaration(node) && node.as_property_declaration().question_token.is_some()
    }

    pub(super) fn is_optional_jsdoc_property_like_tag(&self, node: &Node) -> bool {
        if !is_jsdoc_property_like_tag(node) {
            return false;
        }
        let node_as_jsdoc_property_like_tag = node.as_jsdoc_property_like_tag();
        let is_bracketed = node_as_jsdoc_property_like_tag.is_bracketed;
        let type_expression = node_as_jsdoc_property_like_tag.type_expression.as_ref();
        is_bracketed
            || matches!(
                type_expression,
                Some(type_expression) if type_expression.as_jsdoc_type_expression().type_.kind() == SyntaxKind::JSDocOptionalType
            )
    }

    pub(super) fn create_type_predicate(
        &self,
        kind: TypePredicateKind,
        parameter_name: Option<String>,
        parameter_index: Option<usize>,
        type_: Option<Rc<Type>>,
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
        type_parameters: Option<&[Rc<Type /*TypeParameter*/>]>,
    ) -> usize {
        let mut min_type_argument_count = 0;
        if let Some(type_parameters) = type_parameters {
            for (i, type_parameter) in type_parameters.iter().enumerate() {
                if !self.has_type_parameter_default(type_parameter) {
                    min_type_argument_count = i + 1;
                }
            }
        }
        min_type_argument_count
    }
}
