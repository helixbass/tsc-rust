use std::{
    borrow::{Borrow, Cow},
    io, ptr,
};

use id_arena::Id;

use super::{
    CheckTypeContainingMessageChain, CheckTypeErrorOutputContainer, MappedTypeModifiers, TypeFacts,
};
use crate::{
    contains,
    get_effective_return_type_node, get_object_flags, has_context_sensitive_parameters,
    is_function_declaration, is_function_expression_or_arrow_function, is_in_js_file,
    is_jsx_opening_element, is_object_literal_method, is_part_of_type_node, map, try_every,
    try_for_each_child_bool, try_map, try_some, AsDoubleDeref, Debug_, DiagnosticMessage,
    Diagnostics, ElementFlags, HasArena, HasTypeArgumentsInterface, InArena, IndexInfo, MappedType,
    Node, NodeArray, NodeInterface, ObjectFlags, ObjectTypeInterface, ResolvableTypeInterface,
    Symbol, SymbolInterface, SyntaxKind, Ternary, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeMapper, TypeSystemPropertyName, UnionOrIntersectionTypeInterface, UnionReduction,
    OptionInArena,
};

impl TypeChecker {
    pub(super) fn maybe_type_parameter_reference(&self, node: Id<Node>) -> bool {
        !(node.ref_(self).parent().ref_(self).kind() == SyntaxKind::TypeReference && {
            let node_parent = node.ref_(self).parent();
            let node_parent_ref = node_parent.ref_(self);
            let node_parent_as_type_reference_node = node_parent_ref.as_type_reference_node();
            ({
                let value = node_parent_as_type_reference_node
                    .maybe_type_arguments()
                    .is_some();
                value
            }) && node == node_parent_as_type_reference_node.type_name
        } || node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ImportType && {
            let node_parent = node.ref_(self).parent();
            let node_parent_ref = node_parent.ref_(self);
            let node_parent_as_import_type_node = node_parent_ref.as_import_type_node();
            ({
                let value = node_parent_as_import_type_node
                    .maybe_type_arguments()
                    .is_some();
                value
            }) && node_parent_as_import_type_node.qualifier == Some(node)
        })
    }

    pub(super) fn is_type_parameter_possibly_referenced(
        &self,
        tp: Id<Type>, /*TypeParameter*/
        node: Id<Node>,
    ) -> io::Result<bool> {
        if let Some(tp_symbol) = tp.ref_(self).maybe_symbol() {
            let tp_symbol_ref = tp_symbol.ref_(self);
            let tp_symbol_declarations = tp_symbol_ref.maybe_declarations();
            if let Some(tp_symbol_declarations) = tp_symbol_declarations.as_ref() {
                if tp_symbol_declarations.len() == 1 {
                    let container = tp_symbol_declarations[0].ref_(self).parent();
                    let mut n = Some(node);
                    while !match n {
                        None => false,
                        Some(n) => n == container,
                    } {
                        if match n.as_ref() {
                            None => true,
                            Some(n) => {
                                n.ref_(self).kind() == SyntaxKind::Block
                                    || n.ref_(self).kind() == SyntaxKind::ConditionalType
                                        && try_for_each_child_bool(
                                            n.ref_(self).as_conditional_type_node().extends_type,
                                            |child| self.contains_reference(tp, child),
                                            Option::<fn(Id<NodeArray>) -> io::Result<bool>>::None,
                                            self,
                                        )?
                            }
                        } {
                            return Ok(true);
                        }
                        n = n.unwrap().ref_(self).maybe_parent();
                    }
                    return self.contains_reference(tp, node);
                }
            }
        }
        Ok(true)
    }

    pub(super) fn contains_reference(
        &self,
        tp: Id<Type>, /*TypeParameter*/
        node: Id<Node>,
    ) -> io::Result<bool> {
        Ok(match node.ref_(self).kind() {
            SyntaxKind::ThisType => {
                tp.ref_(self).as_type_parameter().is_this_type == Some(true)
            }
            SyntaxKind::Identifier => {
                !matches!(tp.ref_(self).as_type_parameter().is_this_type, Some(true))
                    && is_part_of_type_node(node, self)
                    && self.maybe_type_parameter_reference(node)
                    && self.get_type_from_type_node_worker(node)? == tp
            }
            SyntaxKind::TypeQuery => true,
            SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature => {
                let node_ref = node.ref_(self);
                let node_as_signature_declaration = node_ref.as_signature_declaration();
                node_as_signature_declaration.maybe_type().is_none()
                    && node
                        .ref_(self).maybe_as_function_like_declaration()
                        .and_then(|node| node.maybe_body())
                        .is_some()
                    || try_some(
                        node_as_signature_declaration
                            .maybe_type_parameters()
                            .refed(self).as_double_deref(),
                        Some(|&type_parameter: &Id<Node>| {
                            self.contains_reference(tp, type_parameter)
                        }),
                    )?
                    || try_some(
                        Some(&*node_as_signature_declaration.parameters().ref_(self)),
                        Some(|&parameter: &Id<Node>| self.contains_reference(tp, parameter)),
                    )?
                    || matches!(
                        node_as_signature_declaration.maybe_type(),
                        Some(type_) if self.contains_reference(tp, type_)?
                    )
            }
            _ => try_for_each_child_bool(
                node,
                |child| self.contains_reference(tp, child),
                Option::<fn(Id<NodeArray>) -> io::Result<bool>>::None,
                self,
            )?,
        })
    }

    pub(super) fn get_homomorphic_type_variable(
        &self,
        type_: Id<Type>, /*MappedType*/
    ) -> io::Result<Option<Id<Type>>> {
        let constraint_type = self.get_constraint_type_from_mapped_type(type_)?;
        if constraint_type
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Index)
        {
            let type_variable = self
                .get_actual_type_variable(constraint_type)?
                .ref_(self)
                .as_index_type()
                .type_;
            if type_variable
                .ref_(self)
                .flags()
                .intersects(TypeFlags::TypeParameter)
            {
                return Ok(Some(type_variable));
            }
        }
        Ok(None)
    }

    pub(super) fn instantiate_mapped_type(
        &self,
        type_: Id<Type>, /*MappedType*/
        mapper: Id<TypeMapper>,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type>> {
        let type_variable = self.get_homomorphic_type_variable(type_)?;
        if let Some(type_variable) = type_variable {
            let mapped_type_variable =
                self.instantiate_type(type_variable, Some(mapper.clone()))?;
            if type_variable != mapped_type_variable {
                return self.try_map_type_with_alias(
                    self.get_reduced_type(mapped_type_variable)?,
                    &mut |t| {
                        if t.ref_(self).flags().intersects(
                            TypeFlags::AnyOrUnknown | TypeFlags::InstantiableNonPrimitive | TypeFlags::Object | TypeFlags::Intersection
                        ) && t != self.wildcard_type() && !self.is_error_type(t) {
                            if type_.ref_(self).as_mapped_type().declaration.ref_(self).as_mapped_type_node().name_type.is_none() {
                                if self.is_array_type(t)
                                    || t.ref_(self).flags().intersects(TypeFlags::Any)
                                        && self.find_resolution_cycle_start_index(
                                            &type_variable.clone().into(),
                                            TypeSystemPropertyName::ImmediateBaseConstraint
                                        ) < 0 && {
                                            let constraint = self.get_constraint_of_type_parameter(type_variable)?;
                                            matches!(
                                                constraint,
                                                Some(constraint) if self.every_type(constraint, |type_| self.is_array_type(type_) || self.is_tuple_type(type_))
                                            )
                                        } {
                                    return self.instantiate_mapped_array_type(
                                        t,
                                        type_,
                                        self.prepend_type_mapping(type_variable, t, Some(mapper.clone()))
                                    );
                                }
                                if self.is_generic_tuple_type(t) {
                                    return self.instantiate_mapped_generic_tuple_type(t, type_, type_variable, mapper.clone());
                                }
                                if self.is_tuple_type(t) {
                                    return self.instantiate_mapped_tuple_type(t, type_, self.prepend_type_mapping(type_variable, t, Some(mapper.clone())));
                                }
                            }
                            return self.instantiate_anonymous_type(type_, self.prepend_type_mapping(type_variable, t, Some(mapper.clone())), Option::<Id<Symbol>>::None, None);
                        }
                        Ok(t)
                    },
                    alias_symbol,
                    alias_type_arguments,
                );
            }
        }
        Ok(
            if self.instantiate_type(
                self.get_constraint_type_from_mapped_type(type_)?,
                Some(mapper.clone()),
            )? == self.wildcard_type()
            {
                self.wildcard_type()
            } else {
                self.instantiate_anonymous_type(type_, mapper, alias_symbol, alias_type_arguments)?
            },
        )
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
        tuple_type: Id<Type>,    /*TupleTypeReference*/
        mapped_type: Id<Type>,   /*MappedType*/
        type_variable: Id<Type>, /*TypeVariable*/
        mapper: Id<TypeMapper>,
    ) -> io::Result<Id<Type>> {
        let tuple_type_target = tuple_type.ref_(self).as_type_reference().target.ref_(self);
        let element_flags = &tuple_type_target.as_tuple_type().element_flags;
        let element_types = try_map(&self.get_type_arguments(tuple_type)?, |&t: &Id<Type>, i| {
            let singleton = if element_flags[i].intersects(ElementFlags::Variadic) {
                t.clone()
            } else if element_flags[i].intersects(ElementFlags::Rest) {
                self.create_array_type(t, None)
            } else {
                self.create_tuple_type(&[t.clone()], Some(&[element_flags[i]]), None, None)?
            };
            self.instantiate_mapped_type(
                mapped_type,
                self.prepend_type_mapping(type_variable, singleton, Some(mapper.clone())),
                Option::<Id<Symbol>>::None,
                None,
            )
        })?;
        let new_readonly = self.get_modified_readonly_state(
            tuple_type
                .ref_(self)
                .as_type_reference()
                .target
                .ref_(self)
                .as_tuple_type()
                .readonly,
            self.get_mapped_type_modifiers(mapped_type),
        );
        self.create_tuple_type(
            &element_types,
            Some(&map(&element_types, |_, _| ElementFlags::Variadic)),
            Some(new_readonly),
            None,
        )
    }

    pub(super) fn instantiate_mapped_array_type(
        &self,
        array_type: Id<Type>,
        mapped_type: Id<Type>, /*MappedType*/
        mapper: Id<TypeMapper>,
    ) -> io::Result<Id<Type>> {
        let element_type =
            self.instantiate_mapped_type_template(mapped_type, self.number_type(), true, mapper)?;
        Ok(if self.is_error_type(element_type) {
            self.error_type()
        } else {
            self.create_array_type(
                element_type,
                Some(self.get_modified_readonly_state(
                    self.is_readonly_array_type(array_type),
                    self.get_mapped_type_modifiers(mapped_type),
                )),
            )
        })
    }

    pub(super) fn instantiate_mapped_tuple_type(
        &self,
        tuple_type: Id<Type>,  /*TupleTypeReference*/
        mapped_type: Id<Type>, /*MappedType*/
        mapper: Id<TypeMapper>,
    ) -> io::Result<Id<Type>> {
        let tuple_type_target = tuple_type.ref_(self).as_type_reference_interface().target();
        let tuple_type_target_ref = tuple_type_target.ref_(self);
        let element_flags = &tuple_type_target_ref.as_tuple_type().element_flags;
        let element_types = try_map(&self.get_type_arguments(tuple_type), |_, i| {
            self.instantiate_mapped_type_template(
                mapped_type,
                self.get_string_literal_type(&i.to_string()),
                element_flags[i].intersects(ElementFlags::Optional),
                mapper.clone(),
            )
        })?;
        let modifiers = self.get_mapped_type_modifiers(mapped_type);
        let new_tuple_modifiers = if modifiers.intersects(MappedTypeModifiers::IncludeOptional) {
            map(element_flags, |f: &ElementFlags, _| {
                if f.intersects(ElementFlags::Required) {
                    ElementFlags::Optional
                } else {
                    *f
                }
            })
        } else if modifiers.intersects(MappedTypeModifiers::ExcludeOptional) {
            map(element_flags, |f: &ElementFlags, _| {
                if f.intersects(ElementFlags::Optional) {
                    ElementFlags::Required
                } else {
                    *f
                }
            })
        } else {
            element_flags.clone()
        };
        let new_readonly = self.get_modified_readonly_state(
            tuple_type_target.ref_(self).as_tuple_type().readonly,
            modifiers,
        );
        Ok(if contains(Some(&element_types), &self.error_type()) {
            self.error_type()
        } else {
            self.create_tuple_type(
                &element_types,
                Some(&new_tuple_modifiers),
                Some(new_readonly),
                tuple_type_target
                    .ref_(self)
                    .as_tuple_type()
                    .labeled_element_declarations
                    .as_deref(),
            )?
        })
    }

    pub(super) fn instantiate_mapped_type_template(
        &self,
        type_: Id<Type>, /*MappedType*/
        key: Id<Type>,
        is_optional: bool,
        mapper: Id<TypeMapper>,
    ) -> io::Result<Id<Type>> {
        let template_mapper = self.append_type_mapping(
            Some(mapper),
            self.get_type_parameter_from_mapped_type(type_)?,
            key,
        );
        let prop_type = self.instantiate_type(
            self.get_template_type_from_mapped_type(
                type_
                    .ref_(self)
                    .as_mapped_type()
                    .maybe_target()
                    .unwrap_or_else(|| type_),
            )?,
            Some(template_mapper),
        )?;
        let modifiers = self.get_mapped_type_modifiers(type_);
        Ok(
            if self.strict_null_checks
                && modifiers.intersects(MappedTypeModifiers::IncludeOptional)
                && !self.maybe_type_of_kind(prop_type, TypeFlags::Undefined | TypeFlags::Void)
            {
                self.get_optional_type_(prop_type, Some(true))?
            } else if self.strict_null_checks
                && modifiers.intersects(MappedTypeModifiers::ExcludeOptional)
                && is_optional
            {
                self.get_type_with_facts(prop_type, TypeFacts::NEUndefined)?
            } else {
                prop_type
            },
        )
    }

    pub(super) fn instantiate_anonymous_type(
        &self,
        type_: Id<Type>, /*AnonymousType*/
        mut mapper: Id<TypeMapper>,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type /*AnonymousType*/>> {
        let mut result = self.create_object_type(
            type_.ref_(self).as_object_flags_type().object_flags() | ObjectFlags::Instantiated,
            type_.ref_(self).maybe_symbol(),
        );
        let is_mapped_type = type_
            .ref_(self)
            .as_object_flags_type()
            .object_flags()
            .intersects(ObjectFlags::Mapped);
        let mut mapped_type_type_parameter: Option<Id<Type>> = None;
        if is_mapped_type {
            let orig_type_parameter = self.get_type_parameter_from_mapped_type(type_)?;
            let fresh_type_parameter = self.clone_type_parameter(orig_type_parameter);
            mapped_type_type_parameter = Some(fresh_type_parameter.clone());
            mapper = self.combine_type_mappers(
                Some(self.make_unary_type_mapper(orig_type_parameter, fresh_type_parameter)),
                mapper,
            );
            fresh_type_parameter
                .ref_(self)
                .as_type_parameter()
                .set_mapper(mapper.clone());
        }
        result.target = Some(type_);
        result.mapper = Some(mapper.clone());
        let result = if is_mapped_type {
            let result = MappedType::new(
                result,
                type_.ref_(self).as_mapped_type().declaration.clone(),
            );
            result.set_type_parameter(mapped_type_type_parameter);
            self.alloc_type(result.into())
        } else {
            self.alloc_type(result.into())
        };
        result.ref_(self).set_alias_symbol(alias_symbol
            .clone()
            .or_else(|| type_.ref_(self).maybe_alias_symbol().clone()));
        result.ref_(self).set_alias_type_arguments(if alias_symbol.is_some() {
            alias_type_arguments.map(ToOwned::to_owned)
        } else {
            self.instantiate_types(
                type_.ref_(self).maybe_alias_type_arguments().as_deref(),
                Some(mapper),
            )?
        });
        Ok(result)
    }

    pub(super) fn get_conditional_type_instantiation(
        &self,
        type_: Id<Type>, /*ConditionalType*/
        mapper: Id<TypeMapper>,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type>> {
        let root = type_.ref_(self).as_conditional_type().root.clone();
        if let Some(root_outer_type_parameters) =
            root.ref_(self).outer_type_parameters.clone().as_deref()
        {
            let type_arguments = try_map(root_outer_type_parameters, |&t: &Id<Type>, _| {
                self.get_mapped_type(t, mapper)
            })?;
            let id = format!(
                "{}{}",
                self.get_type_list_id(Some(&type_arguments)),
                self.get_alias_id(alias_symbol, alias_type_arguments)
            );
            let mut result = root
                .ref_(self)
                .maybe_instantiations()
                .as_ref()
                .unwrap()
                .get(&id)
                .map(Clone::clone);
            if result.is_none() {
                let new_mapper = self.create_type_mapper(
                    root_outer_type_parameters.to_owned(),
                    Some(type_arguments),
                );
                let check_type = root.ref_(self).check_type.clone();
                let distribution_type = if root.ref_(self).is_distributive {
                    Some(self.get_mapped_type(check_type, new_mapper)?)
                } else {
                    None
                };
                result = Some(
                    if let Some(distribution_type) =
                        distribution_type.filter(|&distribution_type| {
                            check_type != distribution_type
                                && distribution_type
                                    .ref_(self)
                                    .flags()
                                    .intersects(TypeFlags::Union | TypeFlags::Never)
                        })
                    {
                        self.try_map_type_with_alias(
                            distribution_type,
                            &mut |t| {
                                self.get_conditional_type(
                                    root.clone(),
                                    Some(self.prepend_type_mapping(
                                        check_type,
                                        t,
                                        Some(new_mapper.clone()),
                                    )),
                                    Option::<Id<Symbol>>::None,
                                    None,
                                )
                            },
                            alias_symbol,
                            alias_type_arguments,
                        )?
                    } else {
                        self.get_conditional_type(
                            root.clone(),
                            Some(new_mapper),
                            alias_symbol,
                            alias_type_arguments,
                        )?
                    },
                );
                root
                    .ref_(self)
                    .maybe_instantiations()
                    .as_mut()
                    .unwrap()
                    .insert(id, result.clone().unwrap());
            }
            return Ok(result.unwrap());
        }
        Ok(type_)
    }

    pub(super) fn instantiate_type(
        &self,
        type_: Id<Type>,
        mapper: Option<Id<TypeMapper>>,
    ) -> io::Result<Id<Type>> {
        Ok(self.maybe_instantiate_type(Some(type_), mapper)?.unwrap())
    }

    pub(super) fn maybe_instantiate_type(
        &self,
        type_: Option<Id<Type>>,
        mapper: Option<Id<TypeMapper>>,
    ) -> io::Result<Option<Id<Type>>> {
        Ok(match (type_, mapper) {
            (Some(type_), Some(mapper)) => Some(self.instantiate_type_with_alias(
                type_,
                mapper,
                Option::<Id<Symbol>>::None,
                None,
            )?),
            _ => type_,
        })
    }

    pub(super) fn instantiate_type_with_alias(
        &self,
        type_: Id<Type>,
        mapper: Id<TypeMapper>,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type>> {
        if !self.could_contain_type_variables(type_)? {
            return Ok(type_);
        }
        if self.instantiation_depth() == 100 || self.instantiation_count() >= 5000000 {
            // tracing?.instant(tracing.Phase.CheckTypes, "instantiateType_DepthLimit", { typeId: type.id, instantiationDepth, instantiationCount });
            self.error(
                self.maybe_current_node(),
                &Diagnostics::Type_instantiation_is_excessively_deep_and_possibly_infinite,
                None,
            );
            return Ok(self.error_type());
        }
        self.set_total_instantiation_count(self.total_instantiation_count() + 1);
        self.set_instantiation_count(self.instantiation_count() + 1);
        self.set_instantiation_depth(self.instantiation_depth() + 1);
        let result =
            self.instantiate_type_worker(type_, mapper, alias_symbol, alias_type_arguments)?;
        self.set_instantiation_depth(self.instantiation_depth() - 1);
        Ok(result)
    }

    pub(super) fn instantiate_type_worker(
        &self,
        type_: Id<Type>,
        mapper: Id<TypeMapper>,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type>> {
        let flags = type_.ref_(self).flags();
        if flags.intersects(TypeFlags::TypeParameter) {
            return self.get_mapped_type(type_, mapper);
        }
        if flags.intersects(TypeFlags::Object) {
            let object_flags = type_.ref_(self).as_object_flags_type().object_flags();
            if object_flags
                .intersects(ObjectFlags::Reference | ObjectFlags::Anonymous | ObjectFlags::Mapped)
            {
                if object_flags.intersects(ObjectFlags::Reference)
                    && type_
                        .ref_(self)
                        .as_type_reference_interface()
                        .maybe_node()
                        .is_none()
                {
                    let resolved_type_arguments = {
                        let resolved_type_arguments = type_
                            .ref_(self)
                            .as_type_reference_interface()
                            .maybe_resolved_type_arguments()
                            .clone();
                        resolved_type_arguments
                    };
                    let resolved_type_arguments = resolved_type_arguments.as_deref();
                    let new_type_arguments =
                        self.instantiate_types(resolved_type_arguments, Some(mapper))?;
                    return Ok(
                        if !match (resolved_type_arguments, new_type_arguments.as_deref()) {
                            (None, None) => true,
                            (Some(resolved_type_arguments), Some(new_type_arguments)) => {
                                new_type_arguments == resolved_type_arguments
                            }
                            _ => false,
                        } {
                            self.create_normalized_type_reference(
                                {
                                    let target =
                                        type_.ref_(self).as_type_reference_interface().target();
                                    target
                                },
                                new_type_arguments,
                            )?
                        } else {
                            type_
                        },
                    );
                }
                if object_flags.intersects(ObjectFlags::ReverseMapped) {
                    return self.instantiate_reverse_mapped_type(type_, mapper);
                }
                return self.get_object_type_instantiation(
                    type_,
                    mapper,
                    alias_symbol,
                    alias_type_arguments,
                );
            }
            return Ok(type_);
        }
        if flags.intersects(TypeFlags::UnionOrIntersection) {
            let origin = if type_.ref_(self).flags().intersects(TypeFlags::Union) {
                type_.ref_(self).as_union_type().origin.clone()
            } else {
                None
            };
            let types = if let Some(origin) = origin.filter(|&origin| {
                origin
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::UnionOrIntersection)
            }) {
                origin
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types()
                    .to_owned()
            } else {
                type_
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types()
                    .to_owned()
            };
            let new_types = self
                .instantiate_types(Some(&types), Some(mapper.clone()))?
                .unwrap();
            if &new_types == &types && alias_symbol == type_.ref_(self).maybe_alias_symbol() {
                return Ok(type_);
            }
            let new_alias_symbol = alias_symbol
                .clone()
                .or_else(|| type_.ref_(self).maybe_alias_symbol().clone());
            let new_alias_type_arguments = if alias_symbol.is_some() {
                alias_type_arguments.map(ToOwned::to_owned)
            } else {
                self.instantiate_types(
                    type_.ref_(self).maybe_alias_type_arguments().as_deref(),
                    Some(mapper),
                )?
            };
            return Ok(
                if flags.intersects(TypeFlags::Intersection)
                    || matches!(
                        origin,
                        Some(origin) if origin.ref_(self).flags().intersects(TypeFlags::Intersection)
                    )
                {
                    self.get_intersection_type(
                        &new_types,
                        new_alias_symbol,
                        new_alias_type_arguments.as_deref(),
                    )?
                } else {
                    self.get_union_type(
                        &new_types,
                        Some(UnionReduction::Literal),
                        new_alias_symbol,
                        new_alias_type_arguments.as_deref(),
                        None,
                    )?
                },
            );
        }
        if flags.intersects(TypeFlags::Index) {
            return self.get_index_type(
                self.instantiate_type(
                    {
                        let type_ = type_.ref_(self).as_index_type().type_;
                        type_
                    },
                    Some(mapper),
                )?,
                None,
                None,
            );
        }
        if flags.intersects(TypeFlags::TemplateLiteral) {
            return self.get_template_literal_type(
                &{
                    let texts = type_.ref_(self).as_template_literal_type().texts.clone();
                    texts
                },
                &self
                    .instantiate_types(
                        Some(&{
                            let types = type_.ref_(self).as_template_literal_type().types.clone();
                            types
                        }),
                        Some(mapper),
                    )?
                    .unwrap(),
            );
        }
        if flags.intersects(TypeFlags::StringMapping) {
            return self.get_string_mapping_type(
                {
                    let symbol = type_.ref_(self).symbol();
                    symbol
                },
                self.instantiate_type(
                    {
                        let type_ = type_.ref_(self).as_string_mapping_type().type_;
                        type_
                    },
                    Some(mapper),
                )?,
            );
        }
        if flags.intersects(TypeFlags::IndexedAccess) {
            let new_alias_symbol = alias_symbol
                .clone()
                .or_else(|| type_.ref_(self).maybe_alias_symbol().clone());
            let new_alias_type_arguments = if alias_symbol.is_some() {
                alias_type_arguments.map(ToOwned::to_owned)
            } else {
                self.instantiate_types(
                    type_.ref_(self).maybe_alias_type_arguments().as_deref(),
                    Some(mapper.clone()),
                )?
            };
            return self.get_indexed_access_type(
                self.instantiate_type(
                    {
                        let object_type = type_.ref_(self).as_indexed_access_type().object_type;
                        object_type
                    },
                    Some(mapper.clone()),
                )?,
                self.instantiate_type(
                    {
                        let index_type = type_.ref_(self).as_indexed_access_type().index_type;
                        index_type
                    },
                    Some(mapper),
                )?,
                Some({
                    let access_flags = type_.ref_(self).as_indexed_access_type().access_flags;
                    access_flags
                }),
                Option::<Id<Node>>::None,
                new_alias_symbol,
                new_alias_type_arguments.as_deref(),
            );
        }
        if flags.intersects(TypeFlags::Conditional) {
            return self.get_conditional_type_instantiation(
                type_,
                self.combine_type_mappers(
                    {
                        let mapper = type_.ref_(self).as_conditional_type().mapper.clone();
                        mapper
                    },
                    mapper.clone(),
                ),
                alias_symbol,
                alias_type_arguments,
            );
        }
        if flags.intersects(TypeFlags::Substitution) {
            let maybe_variable = self.instantiate_type(
                type_.ref_(self).as_substitution_type().base_type,
                Some(mapper.clone()),
            )?;
            if maybe_variable
                .ref_(self)
                .flags()
                .intersects(TypeFlags::TypeVariable)
            {
                return Ok(self.get_substitution_type(
                    maybe_variable,
                    self.instantiate_type(
                        type_.ref_(self).as_substitution_type().substitute,
                        Some(mapper),
                    )?,
                ));
            } else {
                let sub = self.instantiate_type(
                    {
                        let substitute = type_.ref_(self).as_substitution_type().substitute;
                        substitute
                    },
                    Some(mapper),
                )?;
                if sub.ref_(self).flags().intersects(TypeFlags::AnyOrUnknown)
                    || self.is_type_assignable_to(
                        self.get_restrictive_instantiation(maybe_variable)?,
                        self.get_restrictive_instantiation(sub)?,
                    )?
                {
                    return Ok(maybe_variable);
                }
                return Ok(sub);
            }
        }
        Ok(type_)
    }

    pub(super) fn instantiate_reverse_mapped_type(
        &self,
        type_: Id<Type>, /*ReverseMappedType*/
        mapper: Id<TypeMapper>,
    ) -> io::Result<Id<Type>> {
        let inner_mapped_type = self.instantiate_type(
            type_.ref_(self).as_reverse_mapped_type().mapped_type,
            Some(mapper.clone()),
        )?;
        if !get_object_flags(&inner_mapped_type.ref_(self)).intersects(ObjectFlags::Mapped) {
            return Ok(type_);
        }
        let inner_index_type = self.instantiate_type(
            type_.ref_(self).as_reverse_mapped_type().constraint_type,
            Some(mapper.clone()),
        )?;
        if !inner_index_type
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Index)
        {
            return Ok(type_);
        }
        let instantiated = self.infer_type_for_homomorphic_mapped_type(
            self.instantiate_type(
                type_.ref_(self).as_reverse_mapped_type().source,
                Some(mapper),
            )?,
            inner_mapped_type,
            inner_index_type,
        )?;
        if let Some(instantiated) = instantiated {
            return Ok(instantiated);
        }
        Ok(type_)
    }

    pub(super) fn get_permissive_instantiation(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        Ok(
            if type_
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Primitive | TypeFlags::AnyOrUnknown | TypeFlags::Never)
            {
                type_
            } else {
                if type_.ref_(self).maybe_permissive_instantiation().is_none() {
                    type_.ref_(self).set_permissive_instantiation(
                        Some(self.instantiate_type(type_, self.permissive_mapper.clone())?)
                    );
                }
                type_
                    .ref_(self)
                    .maybe_permissive_instantiation()
                    .unwrap()
            },
        )
    }

    pub(super) fn get_restrictive_instantiation(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Primitive | TypeFlags::AnyOrUnknown | TypeFlags::Never)
        {
            return Ok(type_);
        }
        if let Some(type_restrictive_instantiation) =
            type_.ref_(self).maybe_restrictive_instantiation()
        {
            return Ok(type_restrictive_instantiation);
        }
        let ret = self.instantiate_type(type_, self.restrictive_mapper.clone())?;
        type_.ref_(self).set_restrictive_instantiation(Some(ret));
        ret.ref_(self).set_restrictive_instantiation(Some(ret));
        Ok(ret)
    }

    pub(super) fn instantiate_index_info(
        &self,
        info: Id<IndexInfo>,
        mapper: Id<TypeMapper>,
    ) -> io::Result<Id<IndexInfo>> {
        Ok(self.alloc_index_info(self.create_index_info(
            info.ref_(self).key_type.clone(),
            self.instantiate_type(info.ref_(self).type_, Some(mapper))?,
            info.ref_(self).is_readonly,
            info.ref_(self).declaration.clone(),
        )))
    }

    pub(super) fn is_context_sensitive(
        &self,
        node: Id<Node>, /*Expression | MethodDeclaration | ObjectLiteralElementLike | JsxAttributeLike | JsxChild*/
    ) -> io::Result<bool> {
        Debug_.assert(
            node.ref_(self).kind() != SyntaxKind::MethodDeclaration || is_object_literal_method(node, self),
            None,
        );
        Ok(match node.ref_(self).kind() {
            SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::FunctionDeclaration => {
                self.is_context_sensitive_function_like_declaration(node)?
            }
            SyntaxKind::ObjectLiteralExpression => try_some(
                Some(&*node.ref_(self).as_object_literal_expression().properties.ref_(self)),
                Some(|&property: &Id<Node>| self.is_context_sensitive(property)),
            )?,
            SyntaxKind::ArrayLiteralExpression => try_some(
                Some(&*node.ref_(self).as_array_literal_expression().elements.ref_(self)),
                Some(|&element: &Id<Node>| self.is_context_sensitive(element)),
            )?,
            SyntaxKind::ConditionalExpression => {
                let node_ref = node.ref_(self);
                let node_as_conditional_expression = node_ref.as_conditional_expression();
                self.is_context_sensitive(node_as_conditional_expression.when_true)?
                    || self.is_context_sensitive(node_as_conditional_expression.when_false)?
            }
            SyntaxKind::BinaryExpression => {
                let node_ref = node.ref_(self);
                let node_as_binary_expression = node_ref.as_binary_expression();
                matches!(
                    node_as_binary_expression.operator_token.ref_(self).kind(),
                    SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
                ) && (self.is_context_sensitive(node_as_binary_expression.left)?
                    || self.is_context_sensitive(node_as_binary_expression.right)?)
            }
            SyntaxKind::PropertyAssignment => {
                self.is_context_sensitive(node.ref_(self).as_property_assignment().initializer)?
            }
            SyntaxKind::ParenthesizedExpression => {
                self.is_context_sensitive(node.ref_(self).as_parenthesized_expression().expression)?
            }
            SyntaxKind::JsxAttributes => {
                try_some(
                    Some(&*node.ref_(self).as_jsx_attributes().properties.ref_(self)),
                    Some(|&property: &Id<Node>| self.is_context_sensitive(property)),
                )? || is_jsx_opening_element(&node.ref_(self).parent().ref_(self))
                    && try_some(
                        Some(&*node.ref_(self).parent().ref_(self).parent().ref_(self).as_jsx_element().children.ref_(self)),
                        Some(|&child: &Id<Node>| self.is_context_sensitive(child)),
                    )?
            }
            SyntaxKind::JsxAttribute => {
                let initializer = node.ref_(self).as_jsx_attribute().initializer;
                matches!(
                    initializer,
                    Some(initializer) if self.is_context_sensitive(initializer)?
                )
            }
            SyntaxKind::JsxExpression => {
                let expression = node.ref_(self).as_jsx_expression().expression;
                matches!(
                    expression,
                    Some(expression) if self.is_context_sensitive(expression)?
                )
            }
            _ => false,
        })
    }

    pub(super) fn is_context_sensitive_function_like_declaration(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration*/
    ) -> io::Result<bool> {
        Ok((!is_function_declaration(&node.ref_(self))
            || is_in_js_file(Some(&node.ref_(self)))
                && self
                    .get_type_for_declaration_from_jsdoc_comment(node)?
                    .is_some())
            && (has_context_sensitive_parameters(node, self)
                || self.has_context_sensitive_return_expression(node)?))
    }

    pub(super) fn has_context_sensitive_return_expression(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration*/
    ) -> io::Result<bool> {
        let node_ref = node.ref_(self);
        let node_as_function_like_declaration = node_ref.as_function_like_declaration();
        Ok(node_as_function_like_declaration
            .maybe_type_parameters()
            .is_none()
            && get_effective_return_type_node(node, self).is_none()
            && matches!(
                node_as_function_like_declaration.maybe_body(),
                Some(node_body) if node_body.ref_(self).kind() != SyntaxKind::Block && self.is_context_sensitive(node_body)?
            ))
    }

    pub(super) fn is_context_sensitive_function_or_object_literal_method(
        &self,
        func: Id<Node>,
    ) -> io::Result<bool> {
        Ok((is_in_js_file(Some(&func.ref_(self))) && is_function_declaration(&func.ref_(self))
            || is_function_expression_or_arrow_function(&func.ref_(self))
            || is_object_literal_method(func, self))
            && self.is_context_sensitive_function_like_declaration(func)?)
    }

    pub(super) fn get_type_without_signatures(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        if type_.ref_(self).flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_)?;
            if !resolved
                .ref_(self)
                .as_resolved_type()
                .construct_signatures()
                .is_empty()
                || !resolved
                    .ref_(self)
                    .as_resolved_type()
                    .call_signatures()
                    .is_empty()
            {
                let result = self
                    .create_object_type(ObjectFlags::Anonymous, type_.ref_(self).maybe_symbol());
                result.resolve(
                    resolved.ref_(self).as_resolved_type().members(),
                    resolved.ref_(self).as_resolved_type().properties(),
                    vec![],
                    vec![],
                    vec![],
                );
                return Ok(self.alloc_type(result.into()));
            }
        } else if type_.ref_(self).flags().intersects(TypeFlags::Intersection) {
            return self.get_intersection_type(
                &try_map(
                    type_.ref_(self).as_intersection_type().types(),
                    |&type_: &Id<Type>, _| self.get_type_without_signatures(type_),
                )?,
                Option::<Id<Symbol>>::None,
                None,
            );
        }
        Ok(type_)
    }

    pub(super) fn is_type_identical_to(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<bool> {
        self.is_type_related_to(source, target, self.identity_relation.clone())
    }

    pub(super) fn compare_types_identical(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<Ternary> {
        Ok(
            if self.is_type_related_to(source, target, self.identity_relation.clone())? {
                Ternary::True
            } else {
                Ternary::False
            },
        )
    }

    pub(super) fn compare_types_assignable(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<Ternary> {
        Ok(
            if self.is_type_related_to(source, target, self.assignable_relation.clone())? {
                Ternary::True
            } else {
                Ternary::False
            },
        )
    }

    pub(super) fn compare_types_subtype_of(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<Ternary> {
        Ok(
            if self.is_type_related_to(source, target, self.subtype_relation.clone())? {
                Ternary::True
            } else {
                Ternary::False
            },
        )
    }

    pub(super) fn is_type_subtype_of(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<bool> {
        self.is_type_related_to(source, target, self.subtype_relation.clone())
    }

    pub(super) fn is_type_assignable_to(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<bool> {
        self.is_type_related_to(source, target, self.assignable_relation.clone())
    }

    pub(super) fn is_type_derived_from(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<bool> {
        Ok(if source.ref_(self).flags().intersects(TypeFlags::Union) {
            try_every(
                source.ref_(self).as_union_type().types(),
                |&t: &Id<Type>, _| self.is_type_derived_from(t, target),
            )?
        } else if target.ref_(self).flags().intersects(TypeFlags::Union) {
            try_some(
                Some(target.ref_(self).as_union_type().types()),
                Some(|&t: &Id<Type>| self.is_type_derived_from(source, t)),
            )?
        } else if source
            .ref_(self)
            .flags()
            .intersects(TypeFlags::InstantiableNonPrimitive)
        {
            self.is_type_derived_from(
                self.get_base_constraint_of_type(source)?
                    .unwrap_or_else(|| self.unknown_type()),
                target,
            )?
        } else if target == self.global_object_type() {
            source
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::NonPrimitive)
        } else if target == self.global_function_type() {
            source.ref_(self).flags().intersects(TypeFlags::Object)
                && self.is_function_object_type(source)?
        } else {
            self.has_base_type(source, Some(self.get_target_type(target)))?
                || self.is_array_type(target)
                    && !self.is_readonly_array_type(target)
                    && self.is_type_derived_from(source, self.global_readonly_array_type())?
        })
    }

    pub(super) fn is_type_comparable_to(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<bool> {
        self.is_type_related_to(source, target, self.comparable_relation.clone())
    }

    pub(super) fn are_types_comparable(
        &self,
        type1: Id<Type>,
        type2: Id<Type>,
    ) -> io::Result<bool> {
        Ok(
            self.is_type_comparable_to(type1, type2)?
                || self.is_type_comparable_to(type2, type1)?,
        )
    }

    pub(super) fn check_type_assignable_to(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        error_node: Option<Id<Node>>,
        head_message: Option<&'static DiagnosticMessage>,
        containing_message_chain: Option<Id<Box<dyn CheckTypeContainingMessageChain>>>,
        error_output_object: Option<Id<Box<dyn CheckTypeErrorOutputContainer>>>,
    ) -> io::Result<bool> {
        self.check_type_related_to(
            source,
            target,
            self.assignable_relation.clone(),
            error_node,
            head_message.map(Cow::Borrowed),
            containing_message_chain,
            error_output_object,
        )
    }
}
