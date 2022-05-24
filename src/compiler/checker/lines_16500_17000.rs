#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{
    CheckTypeContainingMessageChain, CheckTypeErrorOutputContainer, MappedTypeModifiers, TypeFacts,
};
use crate::{
    are_option_rcs_equal, are_rc_slices_equal, contains_rc, every, for_each_child_bool,
    get_effective_return_type_node, get_object_flags, has_context_sensitive_parameters,
    is_function_declaration, is_function_expression_or_arrow_function, is_in_js_file,
    is_jsx_opening_element, is_object_literal_method, is_part_of_type_node, map, some, Debug_,
    DiagnosticMessage, Diagnostics, ElementFlags, IndexInfo, MappedType, Node, NodeArray,
    NodeInterface, ObjectFlags, ObjectTypeInterface, ResolvableTypeInterface, Symbol,
    SymbolInterface, SyntaxKind, Ternary, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    TypeReferenceInterface, TypeSystemPropertyName, UnionOrIntersectionTypeInterface,
    UnionReduction,
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
            let mapped_type_variable = self.instantiate_type(type_variable, Some(&mapper));
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
            &self.instantiate_type(
                &self.get_constraint_type_from_mapped_type(type_),
                Some(&mapper),
            ),
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
        let prop_type = self.instantiate_type(
            &self.get_template_type_from_mapped_type(
                &type_
                    .as_mapped_type()
                    .maybe_target()
                    .unwrap_or_else(|| type_.type_wrapper()),
            ),
            Some(&template_mapper),
        );
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
        let root = &type_.as_conditional_type().root;
        if let Some(root_outer_type_parameters) = root.outer_type_parameters.as_deref() {
            let type_arguments = map(Some(root_outer_type_parameters), |t: &Rc<Type>, _| {
                self.get_mapped_type(t, mapper)
            })
            .unwrap();
            let alias_symbol =
                alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
            let id = format!(
                "{}{}",
                self.get_type_list_id(Some(&type_arguments)),
                self.get_alias_id(alias_symbol.as_deref(), alias_type_arguments)
            );
            let mut result = root
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
                let check_type = &root.check_type;
                let distribution_type = if root.is_distributive {
                    Some(self.get_mapped_type(check_type, &new_mapper))
                } else {
                    None
                };
                result = Some(
                    if let Some(distribution_type) =
                        distribution_type.as_ref().filter(|distribution_type| {
                            !Rc::ptr_eq(check_type, distribution_type)
                                && distribution_type
                                    .flags()
                                    .intersects(TypeFlags::Union | TypeFlags::Never)
                        })
                    {
                        self.map_type_with_alias(
                            distribution_type,
                            &mut |t| {
                                self.get_conditional_type(
                                    root,
                                    Some(self.prepend_type_mapping(
                                        check_type,
                                        t,
                                        Some(new_mapper.clone()),
                                    )),
                                    Option::<&Symbol>::None,
                                    None,
                                )
                            },
                            alias_symbol,
                            alias_type_arguments,
                        )
                    } else {
                        self.get_conditional_type(
                            root,
                            Some(new_mapper),
                            alias_symbol,
                            alias_type_arguments,
                        )
                    },
                );
                root.maybe_instantiations()
                    .as_mut()
                    .unwrap()
                    .insert(id, result.clone().unwrap());
            }
            return result.unwrap();
        }
        type_.type_wrapper()
    }

    pub(super) fn instantiate_type(&self, type_: &Type, mapper: Option<&TypeMapper>) -> Rc<Type> {
        self.maybe_instantiate_type(Some(type_), mapper).unwrap()
    }

    pub(super) fn maybe_instantiate_type<TType: Borrow<Type>>(
        &self,
        type_: Option<TType>,
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

    pub(super) fn instantiate_type_with_alias<TAliasSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type,
        mapper: &TypeMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        if !self.could_contain_type_variables(type_) {
            return type_.type_wrapper();
        }
        if self.instantiation_depth() == 100 || self.instantiation_count() >= 5000000 {
            // tracing?.instant(tracing.Phase.CheckTypes, "instantiateType_DepthLimit", { typeId: type.id, instantiationDepth, instantiationCount });
            self.error(
                self.maybe_current_node(),
                &Diagnostics::Type_instantiation_is_excessively_deep_and_possibly_infinite,
                None,
            );
            return self.error_type();
        }
        self.set_total_instantiation_count(self.total_instantiation_count() + 1);
        self.set_instantiation_count(self.instantiation_count() + 1);
        self.set_instantiation_depth(self.instantiation_depth() + 1);
        let result =
            self.instantiate_type_worker(type_, mapper, alias_symbol, alias_type_arguments);
        self.set_instantiation_depth(self.instantiation_depth() - 1);
        result
    }

    pub(super) fn instantiate_type_worker<TSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type,
        mapper: &TypeMapper,
        alias_symbol: Option<TSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::TypeParameter) {
            return self.get_mapped_type(type_, mapper);
        }
        if flags.intersects(TypeFlags::Object) {
            let object_flags = type_.as_object_flags_type().object_flags();
            if object_flags
                .intersects(ObjectFlags::Reference | ObjectFlags::Anonymous | ObjectFlags::Mapped)
            {
                if object_flags.intersects(ObjectFlags::Reference)
                    && type_.as_type_reference().maybe_node().is_none()
                {
                    let type_as_type_reference = type_.as_type_reference();
                    let resolved_type_arguments =
                        type_as_type_reference.maybe_resolved_type_arguments();
                    let resolved_type_arguments = resolved_type_arguments.as_deref();
                    let new_type_arguments =
                        self.instantiate_types(resolved_type_arguments, mapper);
                    return if !match (resolved_type_arguments, new_type_arguments.as_deref()) {
                        (None, None) => true,
                        (Some(resolved_type_arguments), Some(new_type_arguments)) => {
                            are_rc_slices_equal(new_type_arguments, resolved_type_arguments)
                        }
                        _ => false,
                    } {
                        self.create_normalized_type_reference(
                            &type_as_type_reference.target,
                            new_type_arguments,
                        )
                    } else {
                        type_.type_wrapper()
                    };
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
            return type_.type_wrapper();
        }
        let alias_symbol = alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
        if flags.intersects(TypeFlags::UnionOrIntersection) {
            let origin = if type_.flags().intersects(TypeFlags::Union) {
                type_.as_union_type().origin.clone()
            } else {
                None
            };
            let types = if let Some(origin) = origin
                .as_ref()
                .filter(|origin| origin.flags().intersects(TypeFlags::UnionOrIntersection))
            {
                origin
                    .as_union_or_intersection_type_interface()
                    .types()
                    .to_owned()
            } else {
                type_
                    .as_union_or_intersection_type_interface()
                    .types()
                    .to_owned()
            };
            let new_types = self.instantiate_types(Some(&types), mapper).unwrap();
            if are_rc_slices_equal(&new_types, &types)
                && are_option_rcs_equal(alias_symbol.as_ref(), type_.maybe_alias_symbol().as_ref())
            {
                return type_.type_wrapper();
            }
            let new_alias_symbol = alias_symbol
                .clone()
                .or_else(|| type_.maybe_alias_symbol().clone());
            let new_alias_type_arguments = if alias_symbol.is_some() {
                alias_type_arguments.map(ToOwned::to_owned)
            } else {
                self.instantiate_types(type_.maybe_alias_type_arguments().as_deref(), mapper)
            };
            return if flags.intersects(TypeFlags::Intersection)
                || matches!(
                    origin.as_ref(),
                    Some(origin) if origin.flags().intersects(TypeFlags::Intersection)
                ) {
                self.get_intersection_type(
                    &new_types,
                    new_alias_symbol,
                    new_alias_type_arguments.as_deref(),
                )
            } else {
                self.get_union_type(
                    new_types,
                    Some(UnionReduction::Literal),
                    new_alias_symbol,
                    new_alias_type_arguments.as_deref(),
                    Option::<&Type>::None,
                )
            };
        }
        if flags.intersects(TypeFlags::Index) {
            return self.get_index_type(
                &self.instantiate_type(&type_.as_index_type().type_, Some(mapper)),
                None,
                None,
            );
        }
        if flags.intersects(TypeFlags::TemplateLiteral) {
            let type_as_template_literal_type = type_.as_template_literal_type();
            return self.get_template_literal_type(
                &type_as_template_literal_type.texts,
                &self
                    .instantiate_types(Some(&type_as_template_literal_type.types), mapper)
                    .unwrap(),
            );
        }
        if flags.intersects(TypeFlags::StringMapping) {
            return self.get_string_mapping_type(
                &type_.symbol(),
                &self.instantiate_type(&type_.as_string_mapping_type().type_, Some(mapper)),
            );
        }
        if flags.intersects(TypeFlags::IndexedAccess) {
            let new_alias_symbol = alias_symbol
                .clone()
                .or_else(|| type_.maybe_alias_symbol().clone());
            let new_alias_type_arguments = if alias_symbol.is_some() {
                alias_type_arguments.map(ToOwned::to_owned)
            } else {
                self.instantiate_types(type_.maybe_alias_type_arguments().as_deref(), mapper)
            };
            let type_as_indexed_access_type = type_.as_indexed_access_type();
            return self.get_indexed_access_type(
                &self.instantiate_type(&type_as_indexed_access_type.object_type, Some(mapper)),
                &self.instantiate_type(&type_as_indexed_access_type.index_type, Some(mapper)),
                Some(type_as_indexed_access_type.access_flags),
                Option::<&Node>::None,
                new_alias_symbol,
                new_alias_type_arguments.as_deref(),
            );
        }
        if flags.intersects(TypeFlags::Conditional) {
            return self.get_conditional_type_instantiation(
                type_,
                &self.combine_type_mappers(
                    type_.as_conditional_type().mapper.clone(),
                    mapper.clone(),
                ),
                alias_symbol,
                alias_type_arguments,
            );
        }
        if flags.intersects(TypeFlags::Substitution) {
            let type_as_substitution_type = type_.as_substitution_type();
            let maybe_variable =
                self.instantiate_type(&type_as_substitution_type.base_type, Some(mapper));
            if maybe_variable.flags().intersects(TypeFlags::TypeVariable) {
                return self.get_substitution_type(
                    &maybe_variable,
                    &self.instantiate_type(&type_as_substitution_type.substitute, Some(mapper)),
                );
            } else {
                let sub =
                    self.instantiate_type(&type_as_substitution_type.substitute, Some(mapper));
                if sub.flags().intersects(TypeFlags::AnyOrUnknown)
                    || self.is_type_assignable_to(
                        &self.get_restrictive_instantiation(&maybe_variable),
                        &self.get_restrictive_instantiation(&sub),
                    )
                {
                    return maybe_variable;
                }
                return sub;
            }
        }
        type_.type_wrapper()
    }

    pub(super) fn instantiate_reverse_mapped_type(
        &self,
        type_: &Type, /*ReverseMappedType*/
        mapper: &TypeMapper,
    ) -> Rc<Type> {
        let type_as_reverse_mapped_type = type_.as_reverse_mapped_type();
        let inner_mapped_type =
            self.instantiate_type(&type_as_reverse_mapped_type.mapped_type, Some(mapper));
        if !get_object_flags(&inner_mapped_type).intersects(ObjectFlags::Mapped) {
            return type_.type_wrapper();
        }
        let inner_index_type =
            self.instantiate_type(&type_as_reverse_mapped_type.constraint_type, Some(mapper));
        if !inner_index_type.flags().intersects(TypeFlags::Index) {
            return type_.type_wrapper();
        }
        let instantiated = self.infer_type_for_homomorphic_mapped_type(
            &self.instantiate_type(&type_as_reverse_mapped_type.source, Some(mapper)),
            &inner_mapped_type,
            &inner_index_type,
        );
        if let Some(instantiated) = instantiated {
            return instantiated;
        }
        type_.type_wrapper()
    }

    pub(super) fn get_permissive_instantiation(&self, type_: &Type) -> Rc<Type> {
        if type_
            .flags()
            .intersects(TypeFlags::Primitive | TypeFlags::AnyOrUnknown | TypeFlags::Never)
        {
            type_.type_wrapper()
        } else {
            if type_.maybe_permissive_instantiation().is_none() {
                *type_.maybe_permissive_instantiation() =
                    Some(self.instantiate_type(type_, self.permissive_mapper.as_deref()));
            }
            type_.maybe_permissive_instantiation().clone().unwrap()
        }
    }

    pub(super) fn get_restrictive_instantiation(&self, type_: &Type) -> Rc<Type> {
        if type_
            .flags()
            .intersects(TypeFlags::Primitive | TypeFlags::AnyOrUnknown | TypeFlags::Never)
        {
            return type_.type_wrapper();
        }
        if let Some(type_restrictive_instantiation) =
            type_.maybe_restrictive_instantiation().clone()
        {
            return type_restrictive_instantiation;
        }
        let ret = self.instantiate_type(type_, self.restrictive_mapper.as_deref());
        *type_.maybe_restrictive_instantiation() = Some(ret.clone());
        *ret.maybe_restrictive_instantiation() = Some(ret.clone());
        ret
    }

    pub(super) fn instantiate_index_info(
        &self,
        info: &IndexInfo,
        mapper: &TypeMapper,
    ) -> Rc<IndexInfo> {
        Rc::new(self.create_index_info(
            info.key_type.clone(),
            self.instantiate_type(&info.type_, Some(mapper)),
            info.is_readonly,
            info.declaration.clone(),
        ))
    }

    pub(super) fn is_context_sensitive(
        &self,
        node: &Node, /*Expression | MethodDeclaration | ObjectLiteralElementLike | JsxAttributeLike | JsxChild*/
    ) -> bool {
        Debug_.assert(
            node.kind() != SyntaxKind::MethodDeclaration || is_object_literal_method(node),
            None,
        );
        match node.kind() {
            SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::FunctionDeclaration => {
                self.is_context_sensitive_function_like_declaration(node)
            }
            SyntaxKind::ObjectLiteralExpression => some(
                Some(&node.as_object_literal_expression().properties),
                Some(|property: &Rc<Node>| self.is_context_sensitive(property)),
            ),
            SyntaxKind::ArrayLiteralExpression => some(
                Some(&node.as_array_literal_expression().elements),
                Some(|element: &Rc<Node>| self.is_context_sensitive(element)),
            ),
            SyntaxKind::ConditionalExpression => {
                let node_as_conditional_expression = node.as_conditional_expression();
                self.is_context_sensitive(&node_as_conditional_expression.when_true)
                    || self.is_context_sensitive(&node_as_conditional_expression.when_false)
            }
            SyntaxKind::BinaryExpression => {
                let node_as_binary_expression = node.as_binary_expression();
                matches!(
                    node_as_binary_expression.operator_token.kind(),
                    SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
                ) && (self.is_context_sensitive(&node_as_binary_expression.left)
                    || self.is_context_sensitive(&node_as_binary_expression.right))
            }
            SyntaxKind::PropertyAssignment => {
                self.is_context_sensitive(&node.as_property_assignment().initializer)
            }
            SyntaxKind::ParenthesizedExpression => {
                self.is_context_sensitive(&node.as_parenthesized_expression().expression)
            }
            SyntaxKind::JsxAttributes => {
                some(
                    Some(&node.as_jsx_attributes().properties),
                    Some(|property: &Rc<Node>| self.is_context_sensitive(property)),
                ) || is_jsx_opening_element(&node.parent())
                    && some(
                        Some(&node.parent().parent().as_jsx_element().children),
                        Some(|child: &Rc<Node>| self.is_context_sensitive(child)),
                    )
            }
            SyntaxKind::JsxAttribute => {
                let initializer = node.as_jsx_attribute().initializer.as_ref();
                matches!(
                    initializer,
                    Some(initializer) if self.is_context_sensitive(initializer)
                )
            }
            SyntaxKind::JsxExpression => {
                let expression = node.as_jsx_expression().expression.as_ref();
                matches!(
                    expression,
                    Some(expression) if self.is_context_sensitive(expression)
                )
            }
            _ => false,
        }
    }

    pub(super) fn is_context_sensitive_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> bool {
        (!is_function_declaration(node)
            || is_in_js_file(Some(node))
                && self
                    .get_type_for_declaration_from_jsdoc_comment(node)
                    .is_some())
            && (has_context_sensitive_parameters(node)
                || self.has_context_sensitive_return_expression(node))
    }

    pub(super) fn has_context_sensitive_return_expression(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> bool {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        node_as_function_like_declaration
            .maybe_type_parameters()
            .is_none()
            && get_effective_return_type_node(node).is_none()
            && matches!(
                node_as_function_like_declaration.maybe_body(),
                Some(node_body) if node_body.kind() == SyntaxKind::Block && self.is_context_sensitive(&node_body)
            )
    }

    pub(super) fn is_context_sensitive_function_or_object_literal_method(
        &self,
        func: &Node,
    ) -> bool {
        (is_in_js_file(Some(func)) && is_function_declaration(func)
            || is_function_expression_or_arrow_function(func)
            || is_object_literal_method(func))
            && self.is_context_sensitive_function_like_declaration(func)
    }

    pub(super) fn get_type_without_signatures(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_);
            let resolved_as_resolved_type = resolved.as_resolved_type();
            if !resolved_as_resolved_type.construct_signatures().is_empty()
                || !resolved_as_resolved_type.call_signatures().is_empty()
            {
                let result = self.create_object_type(ObjectFlags::Anonymous, type_.maybe_symbol());
                result.resolve(
                    resolved_as_resolved_type.members(),
                    resolved_as_resolved_type.properties().clone(),
                    vec![],
                    vec![],
                    vec![],
                );
                return result.into();
            }
        } else if type_.flags().intersects(TypeFlags::Intersection) {
            return self.get_intersection_type(
                &map(
                    Some(type_.as_intersection_type().types()),
                    |type_: &Rc<Type>, _| self.get_type_without_signatures(type_),
                )
                .unwrap(),
                Option::<&Symbol>::None,
                None,
            );
        }
        type_.type_wrapper()
    }

    pub(super) fn is_type_identical_to(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, &self.identity_relation())
    }

    pub(super) fn compare_types_identical(&self, source: &Type, target: &Type) -> Ternary {
        if self.is_type_related_to(source, target, &self.identity_relation()) {
            Ternary::True
        } else {
            Ternary::False
        }
    }

    pub(super) fn compare_types_assignable(&self, source: &Type, target: &Type) -> Ternary {
        if self.is_type_related_to(source, target, &self.assignable_relation()) {
            Ternary::True
        } else {
            Ternary::False
        }
    }

    pub(super) fn compare_types_subtype_of(&self, source: &Type, target: &Type) -> Ternary {
        if self.is_type_related_to(source, target, &self.subtype_relation()) {
            Ternary::True
        } else {
            Ternary::False
        }
    }

    pub(super) fn is_type_subtype_of(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, &self.subtype_relation())
    }

    pub(super) fn is_type_assignable_to(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, &self.assignable_relation())
    }

    pub(super) fn is_type_derived_from(&self, source: &Type, target: &Type) -> bool {
        if source.flags().intersects(TypeFlags::Union) {
            every(source.as_union_type().types(), |t: &Rc<Type>, _| {
                self.is_type_derived_from(t, target)
            })
        } else if target.flags().intersects(TypeFlags::Union) {
            some(
                Some(target.as_union_type().types()),
                Some(|t: &Rc<Type>| self.is_type_derived_from(source, t)),
            )
        } else if source
            .flags()
            .intersects(TypeFlags::InstantiableNonPrimitive)
        {
            self.is_type_derived_from(
                &self
                    .get_base_constraint_of_type(source)
                    .unwrap_or_else(|| self.unknown_type()),
                target,
            )
        } else if ptr::eq(target, &*self.global_object_type()) {
            source
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::NonPrimitive)
        } else if ptr::eq(target, &*self.global_function_type()) {
            source.flags().intersects(TypeFlags::Object) && self.is_function_object_type(source)
        } else {
            self.has_base_type(source, Some(self.get_target_type(target)))
                || self.is_array_type(target)
                    && !self.is_readonly_array_type(target)
                    && self.is_type_derived_from(source, &self.global_readonly_array_type())
        }
    }

    pub(super) fn is_type_comparable_to(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, &self.comparable_relation())
    }

    pub(super) fn are_types_comparable(&self, type1: &Type, type2: &Type) -> bool {
        self.is_type_comparable_to(type1, type2) || self.is_type_comparable_to(type2, type1)
    }

    pub(super) fn check_type_assignable_to<
        TErrorNode: Borrow<Node>,
        TExpr: Borrow<Node>,
        TContainingMessageChain: CheckTypeContainingMessageChain,
    >(
        &self,
        source: &Type,
        target: &Type,
        error_node: Option<TErrorNode>,
        head_message: Option<&'static DiagnosticMessage>,
        containing_message_chain: Option<TContainingMessageChain>,
        error_output_object: Option<&dyn CheckTypeErrorOutputContainer>,
    ) -> bool {
        self.check_type_related_to(
            source,
            target,
            &self.assignable_relation(),
            error_node,
            head_message,
            containing_message_chain,
            error_output_object,
        )
    }
}
