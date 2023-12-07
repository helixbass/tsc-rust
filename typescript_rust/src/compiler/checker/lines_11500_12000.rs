use std::{io, iter, ptr};

use gc::Gc;
use id_arena::Id;
use itertools::Either;

use super::MappedTypeModifiers;
use crate::{
    add_related_info, append, create_diagnostic_for_node, create_symbol_table,
    get_effective_constraint_of_type_parameter, get_object_flags, is_node_descendant_of,
    is_type_parameter_declaration, maybe_for_each, maybe_for_each_bool, Diagnostics, IteratorExt,
    Node, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, ObjectTypeInterface, OptionTry,
    Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeSystemPropertyName, __String, _d, try_map_defined,
};

impl TypeChecker {
    pub(super) fn get_name_type_from_mapped_type(
        &self,
        type_: Id<Type>, /*MappedType*/
    ) -> io::Result<Option<Id<Type>>> {
        self.type_(type_)
            .as_mapped_type()
            .declaration
            .as_mapped_type_node()
            .name_type
            .as_ref()
            .try_map(|type_declaration_name_type| -> io::Result<_> {
                if self
                    .type_(type_)
                    .as_mapped_type()
                    .maybe_name_type()
                    .is_none()
                {
                    let name_type = self.instantiate_type(
                        self.get_type_from_type_node_(type_declaration_name_type)?,
                        self.type_(type_).as_mapped_type().maybe_mapper(),
                    )?;
                    *self.type_(type_).as_mapped_type().maybe_name_type() = Some(name_type);
                }
                Ok(self
                    .type_(type_)
                    .as_mapped_type()
                    .maybe_name_type()
                    .clone()
                    .unwrap())
            })
    }

    pub(super) fn get_template_type_from_mapped_type(
        &self,
        type_: Id<Type>, /*MappedType*/
    ) -> io::Result<Id<Type>> {
        if self
            .type_(type_)
            .as_mapped_type()
            .maybe_template_type()
            .is_none()
        {
            let template_type = if let Some(type_declaration_type) = self
                .type_(type_)
                .as_mapped_type()
                .declaration
                .as_mapped_type_node()
                .type_
                .as_ref()
            {
                self.instantiate_type(
                    self.add_optionality(
                        self.get_type_from_type_node_(type_declaration_type)?,
                        Some(true),
                        Some(
                            self.get_mapped_type_modifiers(type_)
                                .intersects(MappedTypeModifiers::IncludeOptional),
                        ),
                    )?,
                    self.type_(type_).as_mapped_type().maybe_mapper(),
                )?
            } else {
                self.error_type()
            };
            *self.type_(type_).as_mapped_type().maybe_template_type() = Some(template_type);
        }
        Ok(self
            .type_(type_)
            .as_mapped_type()
            .maybe_template_type()
            .clone()
            .unwrap())
    }

    pub(super) fn get_constraint_declaration_for_mapped_type(
        &self,
        type_: Id<Type>, /*MappedType*/
    ) -> Option<Gc<Node>> {
        get_effective_constraint_of_type_parameter(
            &self
                .type_(type_)
                .as_mapped_type()
                .declaration
                .as_mapped_type_node()
                .type_parameter,
        )
    }

    pub(super) fn is_mapped_type_with_keyof_constraint_declaration(
        &self,
        type_: Id<Type>, /*MappedType*/
    ) -> bool {
        let constraint_declaration = self
            .get_constraint_declaration_for_mapped_type(type_)
            .unwrap();
        constraint_declaration.kind() == SyntaxKind::TypeOperator
            && constraint_declaration.as_type_operator_node().operator == SyntaxKind::KeyOfKeyword
    }

    pub(super) fn get_modifiers_type_from_mapped_type(
        &self,
        type_: Id<Type>, /*MappedType*/
    ) -> io::Result<Id<Type>> {
        if self
            .type_(type_)
            .as_mapped_type()
            .maybe_modifiers_type()
            .is_none()
        {
            if self.is_mapped_type_with_keyof_constraint_declaration(type_) {
                *self.type_(type_).as_mapped_type().maybe_modifiers_type() = Some(
                    self.instantiate_type(
                        self.get_type_from_type_node_(
                            &self
                                .get_constraint_declaration_for_mapped_type(type_)
                                .unwrap()
                                .as_type_operator_node()
                                .type_,
                        )?,
                        self.type_(type_).as_mapped_type().maybe_mapper(),
                    )?,
                );
            } else {
                let declared_type = self.get_type_from_mapped_type_node(
                    &self.type_(type_).as_mapped_type().declaration,
                )?;
                let constraint = self.get_constraint_type_from_mapped_type(declared_type)?;
                let extended_constraint = /*constraint &&*/ if self.type_(constraint).flags().intersects(TypeFlags::TypeParameter) {
                    self.get_constraint_of_type_parameter(constraint)?
                } else {
                    Some(constraint)
                };
                *self.type_(type_).as_mapped_type().maybe_modifiers_type() = Some(
                    if let Some(extended_constraint) =
                        extended_constraint.filter(|&extended_constraint| {
                            self.type_(extended_constraint)
                                .flags()
                                .intersects(TypeFlags::Index)
                        })
                    {
                        self.instantiate_type(
                            self.type_(extended_constraint).as_index_type().type_,
                            self.type_(type_).as_mapped_type().maybe_mapper(),
                        )?
                    } else {
                        self.unknown_type()
                    },
                );
            }
        }
        Ok(self
            .type_(type_)
            .as_mapped_type()
            .maybe_modifiers_type()
            .clone()
            .unwrap())
    }

    pub(super) fn get_mapped_type_modifiers(
        &self,
        type_: Id<Type>, /*MappedType*/
    ) -> MappedTypeModifiers {
        let declaration = &self.type_(type_).as_mapped_type().declaration.clone();
        let declaration_as_mapped_type_node = declaration.as_mapped_type_node();
        (if let Some(declaration_readonly_token) =
            declaration_as_mapped_type_node.readonly_token.as_ref()
        {
            if declaration_readonly_token.kind() == SyntaxKind::MinusToken {
                MappedTypeModifiers::ExcludeReadonly
            } else {
                MappedTypeModifiers::IncludeReadonly
            }
        } else {
            MappedTypeModifiers::None
        }) | if let Some(declaration_question_token) =
            declaration_as_mapped_type_node.question_token.as_ref()
        {
            if declaration_question_token.kind() == SyntaxKind::MinusToken {
                MappedTypeModifiers::ExcludeOptional
            } else {
                MappedTypeModifiers::IncludeOptional
            }
        } else {
            MappedTypeModifiers::None
        }
    }

    pub(super) fn get_mapped_type_optionality(&self, type_: Id<Type> /*MappedType*/) -> i32 {
        let modifiers = self.get_mapped_type_modifiers(type_);
        if modifiers.intersects(MappedTypeModifiers::ExcludeOptional) {
            -1
        } else if modifiers.intersects(MappedTypeModifiers::IncludeOptional) {
            1
        } else {
            0
        }
    }

    pub(super) fn get_combined_mapped_type_optionality(
        &self,
        type_: Id<Type>, /*MappedType*/
    ) -> io::Result<i32> {
        let optionality = self.get_mapped_type_optionality(type_);
        let modifiers_type = self.get_modifiers_type_from_mapped_type(type_)?;
        Ok(if optionality != 0 {
            optionality
        } else {
            if self.is_generic_mapped_type(modifiers_type)? {
                self.get_mapped_type_optionality(modifiers_type)
            } else {
                0
            }
        })
    }

    pub(super) fn is_partial_mapped_type(&self, type_: Id<Type>) -> bool {
        get_object_flags(&self.type_(type_)).intersects(ObjectFlags::Mapped)
            && self
                .get_mapped_type_modifiers(type_)
                .intersects(MappedTypeModifiers::IncludeOptional)
    }

    pub(super) fn is_generic_mapped_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(
            get_object_flags(&self.type_(type_)).intersects(ObjectFlags::Mapped)
                && self.is_generic_index_type(self.get_constraint_type_from_mapped_type(type_)?)?,
        )
    }

    pub(super) fn resolve_structured_type_members(
        &self,
        type_: Id<Type>, /*StructuredType*/
    ) -> io::Result<Id<Type /*ResolvedType*/>> {
        if !self.type_(type_).as_resolvable_type().is_resolved() {
            if self.type_(type_).flags().intersects(TypeFlags::Object) {
                if self
                    .type_(type_)
                    .as_object_type()
                    .object_flags()
                    .intersects(ObjectFlags::Reference)
                {
                    self.resolve_type_reference_members(type_)?;
                } else if self
                    .type_(type_)
                    .as_object_type()
                    .object_flags()
                    .intersects(ObjectFlags::ClassOrInterface)
                {
                    self.resolve_class_or_interface_members(type_)?;
                } else if self
                    .type_(type_)
                    .as_object_type()
                    .object_flags()
                    .intersects(ObjectFlags::ReverseMapped)
                {
                    self.resolve_reverse_mapped_type_members(type_)?;
                } else if self
                    .type_(type_)
                    .as_object_type()
                    .object_flags()
                    .intersects(ObjectFlags::Anonymous)
                {
                    self.resolve_anonymous_type_members(type_)?;
                } else if self
                    .type_(type_)
                    .as_object_type()
                    .object_flags()
                    .intersects(ObjectFlags::Mapped)
                {
                    self.resolve_mapped_type_members(type_)?;
                }
            } else if self.type_(type_).flags().intersects(TypeFlags::Union) {
                self.resolve_union_type_members(type_)?;
            } else if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Intersection)
            {
                self.resolve_intersection_type_members(type_)?;
            }
        }
        Ok(type_)
    }

    pub(super) fn get_properties_of_object_type(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Vec<Gc<Symbol>>> {
        if self.type_(type_).flags().intersects(TypeFlags::Object) {
            return Ok((*self
                .type_(self.resolve_structured_type_members(type_)?)
                .as_resolved_type()
                .properties())
            .clone());
        }
        Ok(_d())
    }

    pub(super) fn get_property_of_object_type(
        &self,
        type_: Id<Type>,
        name: &str, /*__String*/
    ) -> io::Result<Option<Gc<Symbol>>> {
        if self.type_(type_).flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_)?;
            let symbol = (*self.type_(resolved).as_resolved_type().members())
                .borrow()
                .get(name)
                .map(Clone::clone);
            if let Some(symbol) = symbol {
                if self.symbol_is_value(&symbol)? {
                    return Ok(Some(symbol));
                }
            }
        }
        Ok(None)
    }

    pub(super) fn get_properties_of_union_or_intersection_type(
        &self,
        type_: Id<Type>, /*UnionOrIntersectionType*/
    ) -> io::Result<impl ExactSizeIterator<Item = Gc<Symbol>> + Clone> {
        if self
            .type_(type_)
            .as_union_or_intersection_type_interface()
            .maybe_resolved_properties()
            .is_none()
        {
            let mut members = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
            for current in {
                let types = self
                    .type_(type_)
                    .as_union_or_intersection_type_interface()
                    .types()
                    .to_owned();
                types
            } {
                for prop in self.get_properties_of_type(current)? {
                    if !members.contains_key(prop.escaped_name()) {
                        let combined_prop = self.get_property_of_union_or_intersection_type(
                            type_,
                            prop.escaped_name(),
                            None,
                        )?;
                        if let Some(combined_prop) = combined_prop {
                            members.insert(prop.escaped_name().to_owned(), combined_prop);
                        }
                    }
                }
                if self.type_(type_).flags().intersects(TypeFlags::Union)
                    && self.get_index_infos_of_type(current)?.is_empty()
                {
                    break;
                }
            }
            *self
                .type_(type_)
                .as_union_or_intersection_type_interface()
                .maybe_resolved_properties_mut() = Some(self.get_named_members(&members)?.into());
        }
        Ok(self
            .type_(type_)
            .as_union_or_intersection_type_interface()
            .maybe_resolved_properties()
            .unwrap()
            .owned_iter())
    }

    pub(super) fn get_properties_of_type(
        &self,
        type_: Id<Type>,
    ) -> io::Result<impl ExactSizeIterator<Item = Gc<Symbol>> + Clone> {
        let type_ = self.get_reduced_apparent_type(type_)?;
        Ok(
            if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
            {
                Either::Left(self.get_properties_of_union_or_intersection_type(type_)?)
            } else {
                Either::Right(self.get_properties_of_object_type(type_)?.into_iter())
            },
        )
    }

    pub(super) fn for_each_property_of_type(
        &self,
        type_: Id<Type>,
        mut action: impl FnMut(&Symbol, &__String),
    ) -> io::Result<()> {
        let type_ = self.get_reduced_apparent_type(type_)?;
        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::StructuredType)
        {
            for (escaped_name, symbol) in &*(*self
                .type_(self.resolve_structured_type_members(type_)?)
                .as_resolved_type()
                .members())
            .borrow()
            {
                if self.is_named_member(symbol, escaped_name)? {
                    action(symbol, escaped_name);
                }
            }
        }

        Ok(())
    }

    pub fn is_type_invalid_due_to_union_discriminant(
        &self,
        contextual_type: Id<Type>,
        obj: &Node, /*ObjectLiteralExpression | JsxAttributes*/
    ) -> io::Result<bool> {
        let list = obj.as_has_properties().properties();
        let ret = list.iter().try_any(|property| -> io::Result<_> {
            let name_type = property.as_named_declaration().maybe_name().try_map(|name| self.get_literal_type_from_property_name(&name))?;
            let name = name_type.filter(|&name_type| self.is_type_usable_as_property_name(name_type)).map(|name_type| self.get_property_name_from_type(name_type));
            let expected = name.try_and_then(|name| self.get_type_of_property_of_type_(contextual_type, &name))?;
            Ok(matches!(
                expected,
                Some(expected) if self.is_literal_type(expected) && !self.is_type_assignable_to(self.get_type_of_node(property)?, expected)?
            ))
        })?;
        Ok(ret)
    }

    pub fn get_all_possible_properties_of_types(
        &self,
        types: &[Id<Type>],
    ) -> io::Result<Vec<Gc<Symbol>>> {
        let union_type = self.get_union_type(types, None, Option::<&Symbol>::None, None, None)?;
        if !self.type_(union_type).flags().intersects(TypeFlags::Union) {
            return self.get_augmented_properties_of_type(union_type);
        }

        let mut props = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
        for &member_type in types {
            for augmented_property in self.get_augmented_properties_of_type(member_type)? {
                let escaped_name = augmented_property.escaped_name();
                if !props.contains_key(escaped_name) {
                    let prop =
                        self.create_union_or_intersection_property(union_type, escaped_name, None)?;
                    if let Some(prop) = prop {
                        props.insert(escaped_name.to_owned(), prop);
                    }
                }
            }
        }
        Ok(props.values().map(Clone::clone).collect())
    }

    pub(super) fn get_constraint_of_type(
        &self,
        type_: Id<Type>, /*InstantiableType | UnionOrIntersectionType*/
    ) -> io::Result<Option<Id<Type>>> {
        Ok(
            if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::TypeParameter)
            {
                self.get_constraint_of_type_parameter(type_)?
            } else if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::IndexedAccess)
            {
                self.get_constraint_of_indexed_access(type_)?
            } else if self.type_(type_).flags().intersects(TypeFlags::Conditional) {
                self.get_constraint_of_conditional_type(type_)?
            } else {
                self.get_base_constraint_of_type(type_)?
            },
        )
    }

    pub(super) fn get_constraint_of_type_parameter(
        &self,
        type_parameter: Id<Type>, /*TypeParameter*/
    ) -> io::Result<Option<Id<Type>>> {
        Ok(if self.has_non_circular_base_constraint(type_parameter)? {
            self.get_constraint_from_type_parameter(type_parameter)?
        } else {
            None
        })
    }

    pub(super) fn get_constraint_of_indexed_access(
        &self,
        type_: Id<Type>, /*IndexedAccessType*/
    ) -> io::Result<Option<Id<Type>>> {
        Ok(if self.has_non_circular_base_constraint(type_)? {
            self.get_constraint_from_indexed_access(type_)?
        } else {
            None
        })
    }

    pub(super) fn get_simplified_type_or_constraint(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Option<Id<Type>>> {
        let simplified = self.get_simplified_type(type_, false)?;
        Ok(if simplified != type_ {
            Some(simplified)
        } else {
            self.get_constraint_of_type(type_)?
        })
    }

    pub(super) fn get_constraint_from_indexed_access(
        &self,
        type_: Id<Type>, /*IndexedAccessType*/
    ) -> io::Result<Option<Id<Type>>> {
        let index_constraint = self.get_simplified_type_or_constraint(
            self.type_(type_).as_indexed_access_type().index_type,
        )?;
        if let Some(index_constraint) = index_constraint.filter(|&index_constraint| {
            index_constraint != self.type_(type_).as_indexed_access_type().index_type
        }) {
            let indexed_access = self.get_indexed_access_type_or_undefined(
                self.type_(type_).as_indexed_access_type().object_type,
                index_constraint,
                Some(self.type_(type_).as_indexed_access_type().access_flags),
                Option::<&Node>::None,
                Option::<&Symbol>::None,
                None,
            )?;
            if indexed_access.is_some() {
                return Ok(indexed_access);
            }
        }
        let object_constraint = self.get_simplified_type_or_constraint(
            self.type_(type_).as_indexed_access_type().object_type,
        )?;
        if let Some(object_constraint) = object_constraint.filter(|&object_constraint| {
            object_constraint != self.type_(type_).as_indexed_access_type().object_type
        }) {
            return self.get_indexed_access_type_or_undefined(
                object_constraint,
                self.type_(type_).as_indexed_access_type().index_type,
                Some(self.type_(type_).as_indexed_access_type().access_flags),
                Option::<&Node>::None,
                Option::<&Symbol>::None,
                None,
            );
        }
        Ok(None)
    }

    pub(super) fn get_default_constraint_of_conditional_type(
        &self,
        type_: Id<Type>, /*ConditionalType*/
    ) -> io::Result<Id<Type>> {
        if self
            .type_(type_)
            .as_conditional_type()
            .maybe_resolved_default_constraint()
            .is_none()
        {
            let true_constraint = self.get_inferred_true_type_from_conditional_type(type_)?;
            let false_constraint = self.get_false_type_from_conditional_type(type_)?;
            *self
                .type_(type_)
                .as_conditional_type()
                .maybe_resolved_default_constraint() =
                Some(if self.is_type_any(Some(true_constraint)) {
                    false_constraint
                } else if self.is_type_any(Some(false_constraint)) {
                    true_constraint
                } else {
                    self.get_union_type(
                        &[true_constraint, false_constraint],
                        None,
                        Option::<&Symbol>::None,
                        None,
                        None,
                    )?
                });
        }
        Ok(self
            .type_(type_)
            .as_conditional_type()
            .maybe_resolved_default_constraint()
            .clone()
            .unwrap())
    }

    pub(super) fn get_constraint_of_distributive_conditional_type(
        &self,
        type_: Id<Type>, /*ConditionalType*/
    ) -> io::Result<Option<Id<Type>>> {
        if (*self.type_(type_).as_conditional_type().root)
            .borrow()
            .is_distributive
            && !matches!(
                *self.type_(type_).maybe_restrictive_instantiation(),
                Some(restrictive_instantiation) if restrictive_instantiation == type_
            )
        {
            let simplified = self
                .get_simplified_type(self.type_(type_).as_conditional_type().check_type, false)?;
            let constraint = if simplified == self.type_(type_).as_conditional_type().check_type {
                self.get_constraint_of_type(simplified)?
            } else {
                Some(simplified)
            };
            if let Some(constraint) = constraint.filter(|&constraint| {
                constraint != self.type_(type_).as_conditional_type().check_type
            }) {
                let instantiated = self.get_conditional_type_instantiation(
                    type_,
                    &self.prepend_type_mapping(
                        (*self.type_(type_).as_conditional_type().root)
                            .borrow()
                            .check_type
                            .clone(),
                        constraint,
                        self.type_(type_).as_conditional_type().mapper.clone(),
                    ),
                    Option::<&Symbol>::None,
                    None,
                )?;
                if !self
                    .type_(instantiated)
                    .flags()
                    .intersects(TypeFlags::Never)
                {
                    return Ok(Some(instantiated));
                }
            }
        }
        Ok(None)
    }

    pub(super) fn get_constraint_from_conditional_type(
        &self,
        type_: Id<Type>, /*ConditionalType*/
    ) -> io::Result<Id<Type>> {
        self.get_constraint_of_distributive_conditional_type(type_)?
            .try_unwrap_or_else(|| self.get_default_constraint_of_conditional_type(type_))
    }

    pub(super) fn get_constraint_of_conditional_type(
        &self,
        type_: Id<Type>, /*ConditionalType*/
    ) -> io::Result<Option<Id<Type>>> {
        Ok(if self.has_non_circular_base_constraint(type_)? {
            Some(self.get_constraint_from_conditional_type(type_)?)
        } else {
            None
        })
    }

    pub(super) fn get_effective_constraint_of_intersection(
        &self,
        types: &[Id<Type>],
        target_is_union: bool,
    ) -> io::Result<Option<Id<Type>>> {
        let mut constraints: Option<Vec<Id<Type>>> = None;
        let mut has_disjoint_domain_type = false;
        for &t in types {
            if self.type_(t).flags().intersects(TypeFlags::Instantiable) {
                let mut constraint = self.get_constraint_of_type(t)?;
                while let Some(constraint_present) = constraint.filter(|&constraint| {
                    self.type_(constraint).flags().intersects(
                        TypeFlags::TypeParameter | TypeFlags::Index | TypeFlags::Conditional,
                    )
                }) {
                    constraint = self.get_constraint_of_type(constraint_present)?;
                }
                if let Some(constraint) = constraint {
                    if constraints.is_none() {
                        constraints = Some(vec![]);
                    }
                    append(constraints.as_mut().unwrap(), Some(constraint));
                    if target_is_union {
                        append(constraints.as_mut().unwrap(), Some(t.clone()));
                    }
                }
            } else if self.type_(t).flags().intersects(TypeFlags::DisjointDomains) {
                has_disjoint_domain_type = true;
            }
        }
        constraints.try_and_then(|mut constraints| -> io::Result<_> {
            if target_is_union || has_disjoint_domain_type {
                if has_disjoint_domain_type {
                    for &t in types {
                        if self.type_(t).flags().intersects(TypeFlags::DisjointDomains) {
                            append(&mut constraints, Some(t.clone()));
                        }
                    }
                }
                return Ok(Some(self.get_intersection_type(
                    &constraints,
                    Option::<&Symbol>::None,
                    None,
                )?));
            }
            Ok(None)
        })
    }

    pub(super) fn get_base_constraint_of_type(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Option<Id<Type>>> {
        if self.type_(type_).flags().intersects(
            TypeFlags::InstantiableNonPrimitive
                | TypeFlags::UnionOrIntersection
                | TypeFlags::TemplateLiteral
                | TypeFlags::StringMapping,
        ) {
            let constraint = self.get_resolved_base_constraint(type_)?;
            return Ok(
                if constraint != self.no_constraint_type()
                    && constraint != self.circular_constraint_type()
                {
                    Some(constraint)
                } else {
                    None
                },
            );
        }
        Ok(if self.type_(type_).flags().intersects(TypeFlags::Index) {
            Some(self.keyof_constraint_type())
        } else {
            None
        })
    }

    pub(super) fn get_base_constraint_or_type(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        Ok(self.get_base_constraint_of_type(type_)?.unwrap_or(type_))
    }

    pub(super) fn has_non_circular_base_constraint(
        &self,
        type_: Id<Type>, /*InstantiableType*/
    ) -> io::Result<bool> {
        Ok(self.get_resolved_base_constraint(type_)? != self.circular_constraint_type())
    }

    pub(super) fn get_resolved_base_constraint(
        &self,
        type_: Id<Type>, /*InstantiableType | UnionOrIntersectionType*/
    ) -> io::Result<Id<Type>> {
        if let Some(type_resolved_base_constraint) =
            self.type_(type_).maybe_resolved_base_constraint().clone()
        {
            return Ok(type_resolved_base_constraint);
        }
        let mut stack: Vec<Id<Type>> = vec![];
        let ret = self.get_type_with_this_argument(
            self.get_immediate_base_constraint(&mut stack, type_)?,
            Some(type_),
            None,
        )?;
        *self.type_(type_).maybe_resolved_base_constraint() = Some(ret.clone());
        Ok(ret)
    }

    pub(super) fn get_immediate_base_constraint(
        &self,
        stack: &mut Vec<Id<Type>>,
        t: Id<Type>,
    ) -> io::Result<Id<Type>> {
        if self.type_(t).maybe_immediate_base_constraint().is_none() {
            if !self
                .push_type_resolution(&t.into(), TypeSystemPropertyName::ImmediateBaseConstraint)
            {
                return Ok(self.circular_constraint_type());
            }
            let mut result: Option<Id<Type>> = None;
            if stack.len() < 10
                || stack.len() < 50 && !self.is_deeply_nested_type(t, stack, stack.len(), None)
            {
                stack.push(t);
                result =
                    self.compute_base_constraint(stack, self.get_simplified_type(t, false)?)?;
                stack.pop();
            }
            if !self.pop_type_resolution() {
                if self.type_(t).flags().intersects(TypeFlags::TypeParameter) {
                    let error_node = self.get_constraint_declaration(t);
                    if let Some(error_node) = error_node {
                        let diagnostic = self.error(
                            Some(&*error_node),
                            &Diagnostics::Type_parameter_0_has_a_circular_constraint,
                            Some(vec![self.type_to_string_(
                                t,
                                Option::<&Node>::None,
                                None,
                                None,
                            )?]),
                        );
                        if let Some(current_node) = self.maybe_current_node() {
                            if !is_node_descendant_of(&error_node, Some(&*current_node))
                                && !is_node_descendant_of(&current_node, Some(&*error_node))
                            {
                                add_related_info(
                                    &diagnostic,
                                    vec![
                                        create_diagnostic_for_node(&current_node, &Diagnostics::Circularity_originates_in_type_at_this_location, None).into()
                                    ]
                                );
                            }
                        }
                    }
                }
                result = Some(self.circular_constraint_type());
            }
            *self.type_(t).maybe_immediate_base_constraint() =
                Some(result.unwrap_or_else(|| self.no_constraint_type()));
        }
        Ok(self
            .type_(t)
            .maybe_immediate_base_constraint()
            .clone()
            .unwrap())
    }

    pub(super) fn get_base_constraint(
        &self,
        stack: &mut Vec<Id<Type>>,
        t: Id<Type>,
    ) -> io::Result<Option<Id<Type>>> {
        let c = self.get_immediate_base_constraint(stack, t)?;
        Ok(
            if c != self.no_constraint_type() && c != self.circular_constraint_type() {
                Some(c)
            } else {
                None
            },
        )
    }

    pub(super) fn compute_base_constraint(
        &self,
        stack: &mut Vec<Id<Type>>,
        t: Id<Type>,
    ) -> io::Result<Option<Id<Type>>> {
        if self.type_(t).flags().intersects(TypeFlags::TypeParameter) {
            let constraint = self.get_constraint_from_type_parameter(t)?;
            return Ok(
                if matches!(self.type_(t).as_type_parameter().is_this_type, Some(true))
                    || constraint.is_none()
                {
                    constraint
                } else {
                    self.get_base_constraint(stack, constraint.unwrap())?
                },
            );
        }
        if self
            .type_(t)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
        {
            let t_ref = self.type_(t);
            let types = t_ref.as_union_or_intersection_type_interface().types();
            let mut base_types: Vec<Id<Type>> = vec![];
            let mut different = false;
            for &type_ in types {
                let base_type = self.get_base_constraint(stack, type_)?;
                if let Some(base_type) = base_type {
                    if base_type != type_ {
                        different = true;
                    }
                    base_types.push(base_type);
                } else {
                    different = true;
                }
            }
            if !different {
                return Ok(Some(t));
            }
            return Ok(
                if self.type_(t).flags().intersects(TypeFlags::Union)
                    && base_types.len() == types.len()
                {
                    Some(self.get_union_type(
                        &base_types,
                        None,
                        Option::<&Symbol>::None,
                        None,
                        None,
                    )?)
                } else if self.type_(t).flags().intersects(TypeFlags::Intersection)
                    && !base_types.is_empty()
                {
                    Some(self.get_intersection_type(&base_types, Option::<&Symbol>::None, None)?)
                } else {
                    None
                },
            );
        }
        if self.type_(t).flags().intersects(TypeFlags::Index) {
            return Ok(Some(self.keyof_constraint_type()));
        }
        if self.type_(t).flags().intersects(TypeFlags::TemplateLiteral) {
            let types = &self.type_(t).as_template_literal_type().types.clone();
            let constraints = try_map_defined(Some(types), |&type_: &Id<Type>, _| {
                self.get_base_constraint(stack, type_)
            })?;
            return Ok(Some(if constraints.len() == types.len() {
                self.get_template_literal_type(
                    &self.type_(t).as_template_literal_type().texts,
                    &constraints,
                )?
            } else {
                self.string_type()
            }));
        }
        if self.type_(t).flags().intersects(TypeFlags::StringMapping) {
            let constraint =
                self.get_base_constraint(stack, self.type_(t).as_string_mapping_type().type_)?;
            return Ok(Some(if let Some(constraint) = constraint {
                self.get_string_mapping_type(&self.type_(t).symbol(), constraint)?
            } else {
                self.string_type()
            }));
        }
        if self.type_(t).flags().intersects(TypeFlags::IndexedAccess) {
            let base_object_type = self
                .get_base_constraint(stack, self.type_(t).as_indexed_access_type().object_type)?;
            let base_index_type =
                self.get_base_constraint(stack, self.type_(t).as_indexed_access_type().index_type)?;
            let base_indexed_access = if let (Some(base_object_type), Some(base_index_type)) =
                (base_object_type, base_index_type)
            {
                self.get_indexed_access_type_or_undefined(
                    base_object_type,
                    base_index_type,
                    Some(self.type_(t).as_indexed_access_type().access_flags),
                    Option::<&Node>::None,
                    Option::<&Symbol>::None,
                    None,
                )?
            } else {
                None
            };
            return base_indexed_access.try_and_then(|base_indexed_access| {
                self.get_base_constraint(stack, base_indexed_access)
            });
        }
        if self.type_(t).flags().intersects(TypeFlags::Conditional) {
            let constraint = self.get_constraint_from_conditional_type(t)?;
            return /*constraint &&*/ self.get_base_constraint(stack, constraint);
        }
        if self.type_(t).flags().intersects(TypeFlags::Substitution) {
            return self
                .get_base_constraint(stack, self.type_(t).as_substitution_type().substitute);
        }
        Ok(Some(t))
    }

    pub(super) fn get_apparent_type_of_intersection_type(
        &self,
        type_: Id<Type>, /*IntersectionType*/
    ) -> io::Result<Id<Type>> {
        if self
            .type_(type_)
            .as_intersection_type()
            .maybe_resolved_apparent_type()
            .is_none()
        {
            let resolved = self.get_type_with_this_argument(type_, Some(type_), Some(true))?;
            *self
                .type_(type_)
                .as_intersection_type()
                .maybe_resolved_apparent_type() = Some(resolved);
        }
        Ok(self
            .type_(type_)
            .as_intersection_type()
            .maybe_resolved_apparent_type()
            .clone()
            .unwrap())
    }

    pub(super) fn get_resolved_type_parameter_default(
        &self,
        type_parameter: Id<Type>, /*TypeParameter*/
    ) -> io::Result<Option<Id<Type>>> {
        if self
            .type_(type_parameter)
            .as_type_parameter()
            .maybe_default()
            .is_none()
        {
            if let Some(type_parameter_target) =
                self.type_(type_parameter).as_type_parameter().target
            {
                let target_default =
                    self.get_resolved_type_parameter_default(type_parameter_target)?;
                *self
                    .type_(type_parameter)
                    .as_type_parameter()
                    .maybe_default() = Some(if let Some(target_default) = target_default {
                    self.instantiate_type(
                        target_default,
                        self.type_(type_parameter)
                            .as_type_parameter()
                            .maybe_mapper(),
                    )?
                } else {
                    self.no_constraint_type()
                });
            } else {
                *self
                    .type_(type_parameter)
                    .as_type_parameter()
                    .maybe_default() = Some(self.resolving_default_type());
                let default_declaration =
                    self.type_(type_parameter)
                        .maybe_symbol()
                        .and_then(|symbol| {
                            maybe_for_each(
                                symbol.maybe_declarations().as_deref(),
                                |decl: &Gc<Node>, _| {
                                    if is_type_parameter_declaration(decl) {
                                        decl.as_type_parameter_declaration().default.clone()
                                    } else {
                                        None
                                    }
                                },
                            )
                        });
                let default_type = if let Some(default_declaration) = default_declaration {
                    self.get_type_from_type_node_(&default_declaration)?
                } else {
                    self.no_constraint_type()
                };
                if matches!(
                    *self.type_(type_parameter).as_type_parameter().maybe_default(),
                    Some(default) if default == self.resolving_default_type()
                ) {
                    *self
                        .type_(type_parameter)
                        .as_type_parameter()
                        .maybe_default() = Some(default_type);
                }
            }
        } else if self
            .type_(type_parameter)
            .as_type_parameter()
            .maybe_default()
            .unwrap()
            == self.resolving_default_type()
        {
            *self
                .type_(type_parameter)
                .as_type_parameter()
                .maybe_default() = Some(self.circular_constraint_type());
        }
        Ok(self
            .type_(type_parameter)
            .as_type_parameter()
            .maybe_default()
            .clone())
    }

    pub(super) fn get_default_from_type_parameter_(
        &self,
        type_parameter: Id<Type>, /*TypeParameter*/
    ) -> io::Result<Option<Id<Type>>> {
        let default_type = self.get_resolved_type_parameter_default(type_parameter)?;
        Ok(default_type.filter(|&default_type| {
            default_type != self.no_constraint_type()
                && default_type != self.circular_constraint_type()
        }))
    }

    pub(super) fn has_non_circular_type_parameter_default(
        &self,
        type_parameter: Id<Type>, /*TypeParameter*/
    ) -> io::Result<bool> {
        Ok(!matches!(
            self.get_resolved_type_parameter_default(type_parameter)?,
            Some(resolved) if resolved == self.circular_constraint_type()
        ))
    }

    pub(super) fn has_type_parameter_default(
        &self,
        type_parameter: Id<Type>, /*TypeParameter*/
    ) -> bool {
        matches!(
            self.type_(type_parameter).maybe_symbol(),
            Some(symbol) if maybe_for_each_bool(symbol.maybe_declarations().as_deref(), |decl: &Gc<Node>, _| {
                is_type_parameter_declaration(decl) && decl.as_type_parameter_declaration().default.is_some()
            })
        )
    }

    pub(super) fn get_apparent_type_of_mapped_type(
        &self,
        type_: Id<Type>, /*MappedType*/
    ) -> io::Result<Id<Type>> {
        if self
            .type_(type_)
            .as_mapped_type()
            .maybe_resolved_apparent_type()
            .is_none()
        {
            let resolved = self.get_resolved_apparent_type_of_mapped_type(type_)?;
            *self
                .type_(type_)
                .as_mapped_type()
                .maybe_resolved_apparent_type() = Some(resolved);
        }
        Ok(self
            .type_(type_)
            .as_mapped_type()
            .maybe_resolved_apparent_type()
            .clone()
            .unwrap())
    }

    pub(super) fn get_resolved_apparent_type_of_mapped_type(
        &self,
        type_: Id<Type>, /*MappedType*/
    ) -> io::Result<Id<Type>> {
        let type_variable = self.get_homomorphic_type_variable(type_)?;
        if let Some(type_variable) = type_variable {
            if self
                .type_(type_)
                .as_mapped_type()
                .declaration
                .as_mapped_type_node()
                .name_type
                .is_none()
            {
                let constraint = self.get_constraint_of_type_parameter(type_variable)?;
                if let Some(constraint) = constraint.filter(|&constraint| {
                    self.is_array_type(constraint) || self.is_tuple_type(constraint)
                }) {
                    return self.instantiate_type(
                        type_,
                        Some(Gc::new(self.prepend_type_mapping(
                            type_variable,
                            constraint,
                            self.type_(type_).as_mapped_type().maybe_mapper(),
                        ))),
                    );
                }
            }
        }
        Ok(type_)
    }
}
