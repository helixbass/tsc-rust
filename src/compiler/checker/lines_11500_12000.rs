#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::MappedTypeModifiers;
use crate::{
    get_effective_constraint_of_type_parameter, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, ObjectTypeInterface, Symbol, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeInterface, __String,
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
