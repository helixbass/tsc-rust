#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::MappedTypeModifiers;
use crate::{
    create_symbol_table, get_effective_constraint_of_type_parameter, get_object_flags, Node,
    NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, ObjectTypeInterface, Symbol,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, __String,
};

impl TypeChecker {
    pub(super) fn get_name_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Option<Rc<Type>> {
        let type_as_mapped_type = type_.as_mapped_type();
        type_as_mapped_type
            .declaration
            .as_mapped_type_node()
            .name_type
            .as_ref()
            .map(|type_declaration_name_type| {
                if type_as_mapped_type.maybe_name_type().is_none() {
                    let name_type = self
                        .instantiate_type(
                            Some(self.get_type_from_type_node_(type_declaration_name_type)),
                            type_as_mapped_type.maybe_mapper(),
                        )
                        .unwrap();
                    *type_as_mapped_type.maybe_name_type() = Some(name_type);
                }
                type_as_mapped_type.maybe_name_type().clone().unwrap()
            })
    }

    pub(super) fn get_template_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        let type_as_mapped_type = type_.as_mapped_type();
        if type_as_mapped_type.maybe_template_type().is_none() {
            let template_type = if let Some(type_declaration_type) = type_as_mapped_type
                .declaration
                .as_mapped_type_node()
                .type_
                .as_ref()
            {
                self.instantiate_type(
                    Some(
                        self.add_optionality(
                            &self.get_type_from_type_node_(type_declaration_type),
                            Some(true),
                            Some(
                                self.get_mapped_type_modifiers(type_)
                                    .intersects(MappedTypeModifiers::IncludeOptional),
                            ),
                        ),
                    ),
                    type_as_mapped_type.maybe_mapper(),
                )
                .unwrap()
            } else {
                self.error_type()
            };
            *type_as_mapped_type.maybe_template_type() = Some(template_type);
        }
        type_as_mapped_type.maybe_template_type().clone().unwrap()
    }

    pub(super) fn get_constraint_declaration_for_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Option<Rc<Node>> {
        get_effective_constraint_of_type_parameter(
            &type_
                .as_mapped_type()
                .declaration
                .as_mapped_type_node()
                .type_parameter,
        )
    }

    pub(super) fn is_mapped_type_with_keyof_constraint_declaration(
        &self,
        type_: &Type, /*MappedType*/
    ) -> bool {
        let constraint_declaration = self
            .get_constraint_declaration_for_mapped_type(type_)
            .unwrap();
        constraint_declaration.kind() == SyntaxKind::TypeOperator
            && constraint_declaration.as_type_operator_node().operator == SyntaxKind::KeyOfKeyword
    }

    pub(super) fn get_modifiers_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        let type_as_mapped_type = type_.as_mapped_type();
        if type_as_mapped_type.maybe_modifiers_type().is_none() {
            if self.is_mapped_type_with_keyof_constraint_declaration(type_) {
                *type_as_mapped_type.maybe_modifiers_type() = self.instantiate_type(
                    Some(
                        self.get_type_from_type_node_(
                            &self
                                .get_constraint_declaration_for_mapped_type(type_)
                                .unwrap()
                                .as_type_operator_node()
                                .type_,
                        ),
                    ),
                    type_as_mapped_type.maybe_mapper(),
                );
            } else {
                let declared_type =
                    self.get_type_from_mapped_type_node(&type_as_mapped_type.declaration);
                let constraint = self.get_constraint_type_from_mapped_type(&declared_type);
                let extended_constraint = /*constraint &&*/ if constraint.flags().intersects(TypeFlags::TypeParameter) {
                    self.get_constraint_of_type_parameter(&constraint)
                } else {
                    Some(constraint)
                };
                *type_as_mapped_type.maybe_modifiers_type() = Some(
                    if let Some(extended_constraint) =
                        extended_constraint.filter(|extended_constraint| {
                            extended_constraint.flags().intersects(TypeFlags::Index)
                        })
                    {
                        self.instantiate_type(
                            Some(&*extended_constraint.as_index_type().type_),
                            type_as_mapped_type.maybe_mapper(),
                        )
                        .unwrap()
                    } else {
                        self.unknown_type()
                    },
                );
            }
        }
        type_as_mapped_type.maybe_modifiers_type().clone().unwrap()
    }

    pub(super) fn get_mapped_type_modifiers(
        &self,
        type_: &Type, /*MappedType*/
    ) -> MappedTypeModifiers {
        let declaration = &type_.as_mapped_type().declaration;
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

    pub(super) fn get_mapped_type_optionality(&self, type_: &Type /*MappedType*/) -> i32 {
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
        type_: &Type, /*MappedType*/
    ) -> i32 {
        let optionality = self.get_mapped_type_optionality(type_);
        let modifiers_type = self.get_modifiers_type_from_mapped_type(type_);
        if optionality != 0 {
            optionality
        } else {
            if self.is_generic_mapped_type(&modifiers_type) {
                self.get_mapped_type_optionality(&modifiers_type)
            } else {
                0
            }
        }
    }

    pub(super) fn is_partial_mapped_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::Mapped)
            && self
                .get_mapped_type_modifiers(type_)
                .intersects(MappedTypeModifiers::IncludeOptional)
    }

    pub(super) fn is_generic_mapped_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::Mapped)
            && self.is_generic_index_type(&self.get_constraint_type_from_mapped_type(type_))
    }

    pub(super) fn resolve_structured_type_members(
        &self,
        type_: &Type, /*StructuredType*/
    ) -> Rc<Type /*ResolvedType*/> {
        if !type_.as_resolvable_type().is_resolved() {
            if type_.flags().intersects(TypeFlags::Object) {
                let type_as_object_type = type_.as_object_type();
                if type_as_object_type
                    .object_flags()
                    .intersects(ObjectFlags::Reference)
                {
                    self.resolve_type_reference_members(type_);
                } else if type_as_object_type
                    .object_flags()
                    .intersects(ObjectFlags::ClassOrInterface)
                {
                    self.resolve_class_or_interface_members(type_);
                } else if type_as_object_type
                    .object_flags()
                    .intersects(ObjectFlags::ReverseMapped)
                {
                    self.resolve_reverse_mapped_type_members(type_);
                } else if type_as_object_type
                    .object_flags()
                    .intersects(ObjectFlags::Anonymous)
                {
                    self.resolve_anonymous_type_members(type_);
                } else if type_as_object_type
                    .object_flags()
                    .intersects(ObjectFlags::Mapped)
                {
                    self.resolve_mapped_type_members(type_);
                }
            } else if type_.flags().intersects(TypeFlags::Union) {
                self.resolve_union_type_members(type_);
            } else if type_.flags().intersects(TypeFlags::Intersection) {
                self.resolve_intersection_type_members(type_);
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
                .clone();
        }
        vec![]
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

    pub(super) fn get_properties_of_union_or_intersection_type(
        &self,
        type_: &Type, /*UnionOrIntersectionType*/
    ) -> Vec<Rc<Symbol>> {
        let type_as_union_or_intersection_type = type_.as_union_or_intersection_type_interface();
        if type_as_union_or_intersection_type
            .maybe_resolved_properties()
            .is_none()
        {
            let mut members = create_symbol_table(None);
            for current in type_as_union_or_intersection_type.types() {
                for prop in self.get_properties_of_type(current) {
                    if !members.contains_key(prop.escaped_name()) {
                        let combined_prop = self.get_property_of_union_or_intersection_type(
                            type_,
                            prop.escaped_name(),
                            None,
                        );
                        if let Some(combined_prop) = combined_prop {
                            members.insert(prop.escaped_name().clone(), combined_prop);
                        }
                    }
                }
                if type_.flags().intersects(TypeFlags::Union)
                    && self.get_index_infos_of_type(current).is_empty()
                {
                    break;
                }
            }
            *type_as_union_or_intersection_type.maybe_resolved_properties() =
                Some(self.get_named_members(&members));
        }
        type_as_union_or_intersection_type
            .maybe_resolved_properties()
            .clone()
            .unwrap()
    }

    pub(super) fn get_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            self.get_properties_of_union_or_intersection_type(&type_)
        } else {
            self.get_properties_of_object_type(&type_)
        }
    }

    pub(super) fn for_each_property_of_type<TAction: FnMut(&Symbol, &__String)>(
        &self,
        type_: &Type,
        mut action: TAction,
    ) {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::StructuredType) {
            for (escaped_name, symbol) in &*(*self
                .resolve_structured_type_members(&type_)
                .as_resolved_type()
                .members())
            .borrow()
            {
                if self.is_named_member(symbol, escaped_name) {
                    action(symbol, escaped_name);
                }
            }
        }
    }

    pub(super) fn is_type_invalid_due_to_union_discriminant(
        &self,
        contextual_type: &Type,
        obj: &Node, /*ObjectLiteralExpression | JsxAttributes*/
    ) -> bool {
        let list = obj.as_has_properties().properties();
        list.iter().any(|property| {
            let name_type = property.as_named_declaration().maybe_name().map(|name| self.get_literal_type_from_property_name(&name));
            let name = name_type.filter(|name_type| self.is_type_usable_as_property_name(name_type)).map(|name_type| self.get_property_name_from_type(&name_type));
            let expected = name.and_then(|name| self.get_type_of_property_of_type_(contextual_type, &name));
            matches!(
                expected,
                Some(expected) if self.is_literal_type(&expected) && !self.is_type_assignable_to(&self.get_type_of_node(property), &expected)
            )
        })
    }

    pub(super) fn get_all_possible_properties_of_types(
        &self,
        types: &[Rc<Type>],
    ) -> Vec<Rc<Symbol>> {
        let union_type = self.get_union_type(
            types.to_owned(),
            None,
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        );
        if !union_type.flags().intersects(TypeFlags::Union) {
            return self.get_augmented_properties_of_type(&union_type);
        }

        let mut props = create_symbol_table(None);
        for member_type in types {
            for augmented_property in self.get_augmented_properties_of_type(member_type) {
                let escaped_name = augmented_property.escaped_name();
                if !props.contains_key(escaped_name) {
                    let prop =
                        self.create_union_or_intersection_property(&union_type, escaped_name, None);
                    if let Some(prop) = prop {
                        props.insert(escaped_name.clone(), prop);
                    }
                }
            }
        }
        props.values().map(Clone::clone).collect()
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
