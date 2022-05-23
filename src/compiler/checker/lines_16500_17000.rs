#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{MappedTypeModifiers, TypeFacts};
use crate::{
    contains_rc, for_each_child_bool, is_part_of_type_node, map, some, ElementFlags, IndexInfo,
    MappedType, Node, NodeArray, NodeInterface, ObjectFlags, ObjectTypeInterface, Symbol,
    SymbolInterface, SyntaxKind, Ternary, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    TypeSystemPropertyName,
};

impl TypeChecker {
    pub(super) fn maybe_type_parameter_reference(&self, node: &Node) -> bool {
        !(node.parent().kind() == SyntaxKind::TypeReference && {
            let node_parent = node.parent();
            let node_parent_as_type_reference_node = node_parent.as_type_reference_node();
            node_parent_as_type_reference_node.type_arguments.is_some()
                && ptr::eq(node, &*node_parent_as_type_reference_node.type_name)
        } || node.parent().kind() == SyntaxKind::ImportType && {
            let node_parent = node.parent();
            let node_parent_as_import_type_node = node_parent.as_import_type_node();
            node_parent_as_import_type_node.type_arguments.is_some()
                && matches!(
                    node_parent_as_import_type_node.qualifier.as_deref(),
                    Some(qualifier) if ptr::eq(node, qualifier)
                )
        })
    }

    pub(super) fn is_type_parameter_possibly_referenced(
        &self,
        tp: &Type, /*TypeParameter*/
        node: &Node,
    ) -> bool {
        if let Some(tp_symbol) = tp.maybe_symbol() {
            let tp_symbol_declarations = tp_symbol.maybe_declarations();
            if let Some(tp_symbol_declarations) = tp_symbol_declarations.as_ref() {
                if tp_symbol_declarations.len() == 1 {
                    let container = tp_symbol_declarations[0].parent();
                    let mut n = Some(node.node_wrapper());
                    while !match n.as_ref() {
                        None => false,
                        Some(n) => Rc::ptr_eq(n, &container),
                    } {
                        if match n.as_ref() {
                            None => true,
                            Some(n) => {
                                n.kind() == SyntaxKind::Block
                                    || n.kind() == SyntaxKind::ConditionalType
                                        && for_each_child_bool(
                                            &n.as_conditional_type_node().extends_type,
                                            |child| self.contains_reference(tp, child),
                                            Option::<fn(&NodeArray) -> bool>::None,
                                        )
                            }
                        } {
                            return true;
                        }
                        n = n.as_ref().unwrap().maybe_parent();
                    }
                    return self.contains_reference(tp, node);
                }
            }
        }
        true
    }

    pub(super) fn contains_reference(&self, tp: &Type /*TypeParameter*/, node: &Node) -> bool {
        let tp_as_type_parameter = tp.as_type_parameter();
        match node.kind() {
            SyntaxKind::ThisType => matches!(tp_as_type_parameter.is_this_type, Some(true)),
            SyntaxKind::Identifier => {
                !matches!(tp_as_type_parameter.is_this_type, Some(true))
                    && is_part_of_type_node(node)
                    && self.maybe_type_parameter_reference(node)
                    && ptr::eq(&*self.get_type_from_type_node_worker(node), tp)
            }
            SyntaxKind::TypeQuery => true,
            SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature => {
                let node_as_function_like_declaration = node.as_function_like_declaration();
                node_as_function_like_declaration.maybe_type().is_none()
                    && node_as_function_like_declaration.maybe_body().is_some()
                    || some(
                        node_as_function_like_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(|type_parameter: &Rc<Node>| {
                            self.contains_reference(tp, type_parameter)
                        }),
                    )
                    || some(
                        Some(node_as_function_like_declaration.parameters()),
                        Some(|parameter: &Rc<Node>| self.contains_reference(tp, parameter)),
                    )
                    || matches!(
                        node_as_function_like_declaration.maybe_type(),
                        Some(type_) if self.contains_reference(tp, &type_)
                    )
            }
            _ => for_each_child_bool(
                node,
                |child| self.contains_reference(tp, child),
                Option::<fn(&NodeArray) -> bool>::None,
            ),
        }
    }

    pub(super) fn get_homomorphic_type_variable(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Option<Rc<Type>> {
        let constraint_type = self.get_constraint_type_from_mapped_type(type_);
        if constraint_type.flags().intersects(TypeFlags::Index) {
            let type_variable =
                self.get_actual_type_variable(&constraint_type.as_index_type().type_);
            if type_variable.flags().intersects(TypeFlags::TypeParameter) {
                return Some(type_variable);
            }
        }
        None
    }

    pub(super) fn instantiate_mapped_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type, /*MappedType*/
        mapper: TypeMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let type_variable = self.get_homomorphic_type_variable(type_);
        if let Some(type_variable) = type_variable.as_ref() {
            let mapped_type_variable = self
                .instantiate_type(Some(&**type_variable), Some(&mapper))
                .unwrap();
            if !Rc::ptr_eq(type_variable, &mapped_type_variable) {
                let type_as_mapped_type = type_.as_mapped_type();
                return self.map_type_with_alias(
                    &self.get_reduced_type(&mapped_type_variable),
                    &mut |t| {
                        if t.flags().intersects(TypeFlags::AnyOrUnknown | TypeFlags::InstantiableNonPrimitive | TypeFlags::Object | TypeFlags::Intersection) && !ptr::eq(t, &*self.wildcard_type()) && !self.is_error_type(t) {
                            if type_as_mapped_type.declaration.as_mapped_type_node().name_type.is_none() {
                                if self.is_array_type(t) || t.flags().intersects(TypeFlags::Any) && self.find_resolution_cycle_start_index(&type_variable.clone().into(), TypeSystemPropertyName::ImmediateBaseConstraint) < 0 && {
                                    let constraint = self.get_constraint_of_type_parameter(type_variable);
                                    matches!(
                                        constraint.as_ref(),
                                        Some(constraint) if self.every_type(constraint, |type_| self.is_array_type(type_) || self.is_tuple_type(type_))
                                    )
                                } {
                                    return self.instantiate_mapped_array_type(t, type_, self.prepend_type_mapping(type_variable, t, Some(mapper.clone())));
                                }
                                if self.is_generic_tuple_type(t) {
                                    return self.instantiate_mapped_generic_tuple_type(t, type_, type_variable, mapper.clone());
                                }
                                if self.is_tuple_type(t) {
                                    return self.instantiate_mapped_tuple_type(t, type_, self.prepend_type_mapping(type_variable, t, Some(mapper.clone())));
                                }
                            }
                            return self.instantiate_anonymous_type(type_, self.prepend_type_mapping(type_variable, t, Some(mapper.clone())), Option::<&Symbol>::None, None);
                        }
                        t.type_wrapper()
                    },
                    alias_symbol,
                    alias_type_arguments,
                );
            }
        }
        if Rc::ptr_eq(
            &self
                .instantiate_type(
                    Some(self.get_constraint_type_from_mapped_type(type_)),
                    Some(&mapper),
                )
                .unwrap(),
            &self.wildcard_type(),
        ) {
            self.wildcard_type()
        } else {
            self.instantiate_anonymous_type(type_, mapper, alias_symbol, alias_type_arguments)
        }
    }

    pub(super) fn get_modified_readonly_state(
        &self,
        state: bool,
        modifiers: MappedTypeModifiers,
    ) -> bool {
        if modifiers.intersects(MappedTypeModifiers::IncludeReadonly) {
            true
        } else if modifiers.intersects(MappedTypeModifiers::ExcludeReadonly) {
            false
        } else {
            state
        }
    }

    pub(super) fn instantiate_mapped_generic_tuple_type(
        &self,
        tuple_type: &Type,    /*TupleTypeReference*/
        mapped_type: &Type,   /*MappedType*/
        type_variable: &Type, /*TypeVariable*/
        mapper: TypeMapper,
    ) -> Rc<Type> {
        let tuple_type_as_type_reference = tuple_type.as_type_reference();
        let element_flags = &tuple_type_as_type_reference
            .target
            .as_tuple_type()
            .element_flags;
        let element_types = map(
            Some(&self.get_type_arguments(tuple_type)),
            |t: &Rc<Type>, i| {
                let singleton = if element_flags[i].intersects(ElementFlags::Variadic) {
                    t.clone()
                } else if element_flags[i].intersects(ElementFlags::Rest) {
                    self.create_array_type(t, None)
                } else {
                    self.create_tuple_type(&[t.clone()], Some(&[element_flags[i]]), None, None)
                };
                self.instantiate_mapped_type(
                    mapped_type,
                    self.prepend_type_mapping(type_variable, &singleton, Some(mapper.clone())),
                    Option::<&Symbol>::None,
                    None,
                )
            },
        )
        .unwrap();
        let new_readonly = self.get_modified_readonly_state(
            tuple_type_as_type_reference.target.as_tuple_type().readonly,
            self.get_mapped_type_modifiers(mapped_type),
        );
        self.create_tuple_type(
            &element_types,
            map(Some(&element_types), |_, _| ElementFlags::Variadic).as_deref(),
            Some(new_readonly),
            None,
        )
    }

    pub(super) fn instantiate_mapped_array_type(
        &self,
        array_type: &Type,
        mapped_type: &Type, /*MappedType*/
        mapper: TypeMapper,
    ) -> Rc<Type> {
        let element_type =
            self.instantiate_mapped_type_template(mapped_type, &self.number_type(), true, mapper);
        if self.is_error_type(&element_type) {
            self.error_type()
        } else {
            self.create_array_type(
                &element_type,
                Some(self.get_modified_readonly_state(
                    self.is_readonly_array_type(array_type),
                    self.get_mapped_type_modifiers(mapped_type),
                )),
            )
        }
    }

    pub(super) fn instantiate_mapped_tuple_type(
        &self,
        tuple_type: &Type,  /*TupleTypeReference*/
        mapped_type: &Type, /*MappedType*/
        mapper: TypeMapper,
    ) -> Rc<Type> {
        let tuple_type_as_type_reference = tuple_type.as_type_reference();
        let element_flags = &tuple_type_as_type_reference
            .target
            .as_tuple_type()
            .element_flags;
        let element_types = map(Some(&self.get_type_arguments(tuple_type)), |_, i| {
            self.instantiate_mapped_type_template(
                mapped_type,
                &self.get_string_literal_type(&i.to_string()),
                element_flags[i].intersects(ElementFlags::Optional),
                mapper.clone(),
            )
        })
        .unwrap();
        let modifiers = self.get_mapped_type_modifiers(mapped_type);
        let new_tuple_modifiers = if modifiers.intersects(MappedTypeModifiers::IncludeOptional) {
            map(Some(element_flags), |f: &ElementFlags, _| {
                if f.intersects(ElementFlags::Required) {
                    ElementFlags::Optional
                } else {
                    *f
                }
            })
            .unwrap()
        } else if modifiers.intersects(MappedTypeModifiers::ExcludeOptional) {
            map(Some(element_flags), |f: &ElementFlags, _| {
                if f.intersects(ElementFlags::Optional) {
                    ElementFlags::Required
                } else {
                    *f
                }
            })
            .unwrap()
        } else {
            element_flags.clone()
        };
        let new_readonly = self.get_modified_readonly_state(
            tuple_type_as_type_reference.target.as_tuple_type().readonly,
            modifiers,
        );
        if contains_rc(Some(&element_types), &self.error_type()) {
            self.error_type()
        } else {
            self.create_tuple_type(
                &element_types,
                Some(&new_tuple_modifiers),
                Some(new_readonly),
                tuple_type_as_type_reference
                    .target
                    .as_tuple_type()
                    .labeled_element_declarations
                    .as_deref(),
            )
        }
    }

    pub(super) fn instantiate_mapped_type_template(
        &self,
        type_: &Type, /*MappedType*/
        key: &Type,
        is_optional: bool,
        mapper: TypeMapper,
    ) -> Rc<Type> {
        let template_mapper = self.append_type_mapping(
            Some(mapper),
            &self.get_type_parameter_from_mapped_type(type_),
            key,
        );
        let prop_type = self
            .instantiate_type(
                Some(
                    self.get_template_type_from_mapped_type(
                        &type_
                            .as_mapped_type()
                            .maybe_target()
                            .unwrap_or_else(|| type_.type_wrapper()),
                    ),
                ),
                Some(&template_mapper),
            )
            .unwrap();
        let modifiers = self.get_mapped_type_modifiers(type_);
        if self.strict_null_checks
            && modifiers.intersects(MappedTypeModifiers::IncludeOptional)
            && !self.maybe_type_of_kind(&prop_type, TypeFlags::Undefined | TypeFlags::Void)
        {
            self.get_optional_type_(&prop_type, Some(true))
        } else if self.strict_null_checks
            && modifiers.intersects(MappedTypeModifiers::ExcludeOptional)
            && is_optional
        {
            self.get_type_with_facts(&prop_type, TypeFacts::NEUndefined)
        } else {
            prop_type
        }
    }

    pub(super) fn instantiate_anonymous_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type, /*AnonymousType*/
        mut mapper: TypeMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type /*AnonymousType*/> {
        let type_as_object_flags_type = type_.as_object_flags_type();
        let mut result = self.create_object_type(
            type_as_object_flags_type.object_flags() | ObjectFlags::Instantiated,
            type_.maybe_symbol(),
        );
        let is_mapped_type = type_as_object_flags_type
            .object_flags()
            .intersects(ObjectFlags::Mapped);
        let mut mapped_type_type_parameter: Option<Rc<Type>> = None;
        if is_mapped_type {
            let orig_type_parameter = self.get_type_parameter_from_mapped_type(type_);
            let fresh_type_parameter = self.clone_type_parameter(&orig_type_parameter);
            mapped_type_type_parameter = Some(fresh_type_parameter.clone());
            mapper = self.combine_type_mappers(
                Some(self.make_unary_type_mapper(&orig_type_parameter, &fresh_type_parameter)),
                mapper,
            );
            fresh_type_parameter
                .as_type_parameter()
                .set_mapper(mapper.clone());
        }
        result.target = Some(type_.type_wrapper());
        result.mapper = Some(mapper.clone());
        let result: Rc<Type> = if is_mapped_type {
            let result = MappedType::new(result, type_.as_mapped_type().declaration.clone());
            *result.maybe_type_parameter() = mapped_type_type_parameter;
            result.into()
        } else {
            result.into()
        };
        let alias_symbol = alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
        *result.maybe_alias_symbol() = alias_symbol
            .clone()
            .or_else(|| type_.maybe_alias_symbol().clone());
        *result.maybe_alias_type_arguments() = if alias_symbol.is_some() {
            alias_type_arguments.map(ToOwned::to_owned)
        } else {
            self.instantiate_types(type_.maybe_alias_type_arguments().as_deref(), &mapper)
        };
        result
    }

    pub(super) fn get_conditional_type_instantiation<TAliasSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type, /*ConditionalType*/
        mapper: &TypeMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn instantiate_type<TTypeRef: Borrow<Type>>(
        &self,
        type_: Option<TTypeRef>,
        mapper: Option<&TypeMapper>,
    ) -> Option<Rc<Type>> {
        match (type_.as_ref(), mapper) {
            (Some(type_), Some(mapper)) => Some(self.instantiate_type_with_alias(
                type_.borrow(),
                mapper,
                Option::<&Symbol>::None,
                None,
            )),
            _ => type_.map(|type_| type_.borrow().type_wrapper()),
        }
    }

    pub(super) fn instantiate_type_with_alias<TSymbolRef: Borrow<Symbol>>(
        &self,
        type_: &Type,
        mapper: &TypeMapper,
        alias_symbol: Option<TSymbolRef>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let result =
            self.instantiate_type_worker(type_, mapper, alias_symbol, alias_type_arguments);
        result
    }

    pub(super) fn instantiate_type_worker<TSymbolRef: Borrow<Symbol>>(
        &self,
        type_: &Type,
        mapper: &TypeMapper,
        alias_symbol: Option<TSymbolRef>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::TypeParameter) {
            return self.get_mapped_type(type_, mapper);
        }
        unimplemented!()
    }

    pub(super) fn get_permissive_instantiation(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_restrictive_instantiation(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn instantiate_index_info(
        &self,
        info: &IndexInfo,
        mapper: &TypeMapper,
    ) -> Rc<IndexInfo> {
        unimplemented!()
    }

    pub(super) fn is_context_sensitive(
        &self,
        node: &Node, /*Expression | MethodDeclaration | ObjectLiteralElementLike | JsxAttributeLike | JsxChild*/
    ) -> bool {
        // match node {
        // }
        false
    }

    pub(super) fn is_type_identical_to(&self, source: &Type, target: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn compare_types_identical(&self, source: &Type, target: &Type) -> Ternary {
        unimplemented!()
    }

    pub(super) fn compare_types_subtype_of(&self, source: &Type, target: &Type) -> Ternary {
        unimplemented!()
    }

    pub(super) fn is_type_subtype_of(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, &self.subtype_relation())
    }

    pub(super) fn is_type_assignable_to(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, &self.assignable_relation())
    }

    pub(super) fn is_type_derived_from(&self, source: &Type, target: &Type) -> bool {
        unimplemented!()
    }
}
