#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    append, filter, is_optional_type_node, is_rest_type_node, is_tuple_type_node, length, map,
    AccessFlags, ConditionalRoot, ConditionalType, Diagnostics, InferenceFlags, InferencePriority,
    MappedType, Node, NodeInterface, ObjectFlags, Signature, Symbol, SymbolFlags, SymbolInterface,
    Ternary, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
};

impl TypeChecker {
    pub(super) fn get_type_from_indexed_access_type_node(
        &self,
        node: &Node, /*IndexedAccessTypeNode*/
    ) -> Rc<Type> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            let node_as_indexed_access_type_node = node.as_indexed_access_type_node();
            let object_type =
                self.get_type_from_type_node_(&node_as_indexed_access_type_node.object_type);
            let index_type =
                self.get_type_from_type_node_(&node_as_indexed_access_type_node.index_type);
            let potential_alias = self.get_alias_symbol_for_type_node(node);
            let resolved = self.get_indexed_access_type(
                &object_type,
                &index_type,
                Some(AccessFlags::None),
                Some(node),
                potential_alias.as_deref(),
                self.get_type_arguments_for_alias_symbol(potential_alias.as_deref())
                    .as_deref(),
            );
            links.borrow_mut().resolved_type = Some(
                if resolved.flags().intersects(TypeFlags::IndexedAccess) && {
                    let resolved_as_indexed_access_type = resolved.as_indexed_access_type();
                    Rc::ptr_eq(&resolved_as_indexed_access_type.object_type, &object_type)
                        && Rc::ptr_eq(&resolved_as_indexed_access_type.index_type, &index_type)
                } {
                    self.get_conditional_flow_type_of_type(&resolved, node)
                } else {
                    resolved
                },
            );
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn get_type_from_mapped_type_node(
        &self,
        node: &Node, /*MappedTypeNode*/
    ) -> Rc<Type> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            let type_ = self.create_object_type(ObjectFlags::Mapped, node.maybe_symbol());
            let type_: Rc<Type> = MappedType::new(type_, node.node_wrapper()).into();
            let alias_symbol = self.get_alias_symbol_for_type_node(node);
            *type_.maybe_alias_symbol() = alias_symbol.clone();
            *type_.maybe_alias_type_arguments() =
                self.get_type_arguments_for_alias_symbol(alias_symbol);
            links.borrow_mut().resolved_type = Some(type_.clone());
            self.get_constraint_type_from_mapped_type(&type_);
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn get_actual_type_variable(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Substitution) {
            return type_.as_substitution_type().base_type.clone();
        }
        if type_.flags().intersects(TypeFlags::IndexedAccess) {
            let type_as_indexed_access_type = type_.as_indexed_access_type();
            if type_as_indexed_access_type
                .object_type
                .flags()
                .intersects(TypeFlags::Substitution)
                || type_as_indexed_access_type
                    .index_type
                    .flags()
                    .intersects(TypeFlags::Substitution)
            {
                return self.get_indexed_access_type(
                    &self.get_actual_type_variable(&type_as_indexed_access_type.object_type),
                    &self.get_actual_type_variable(&type_as_indexed_access_type.index_type),
                    None,
                    Option::<&Node>::None,
                    Option::<&Symbol>::None,
                    None,
                );
            }
        }
        type_.type_wrapper()
    }

    pub(super) fn is_typical_nondistributive_conditional(&self, root: &ConditionalRoot) -> bool {
        !root.is_distributive
            && self.is_singleton_tuple_type(&root.node.as_conditional_type_node().check_type)
            && self.is_singleton_tuple_type(&root.node.as_conditional_type_node().extends_type)
    }

    pub(super) fn is_singleton_tuple_type(&self, node: &Node /*TypeNode*/) -> bool {
        is_tuple_type_node(node) && {
            let node_as_tuple_type_node = node.as_tuple_type_node();
            length(Some(&*node_as_tuple_type_node.elements)) == 1
                && !is_optional_type_node(&node_as_tuple_type_node.elements[0])
                && !is_rest_type_node(&node_as_tuple_type_node.elements[0])
        }
    }

    pub(super) fn unwrap_nondistributive_conditional_tuple(
        &self,
        root: &ConditionalRoot,
        type_: &Type,
    ) -> Rc<Type> {
        if self.is_typical_nondistributive_conditional(root) && self.is_tuple_type(type_) {
            self.get_type_arguments(type_)[0].clone()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_conditional_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        root: &ConditionalRoot,
        mut mapper: Option<TypeMapper>,
        alias_symbol: Option<TAliasSymbol>,
        mut alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let result: Rc<Type>;
        let mut extra_types: Option<Vec<Rc<Type>>> = None;
        let mut tail_count = 0;
        let mut root = root.clone(); // TODO: would it make more sense to wrap these in Rc<ConditionalRoot>?
        let mut alias_symbol =
            alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
        loop {
            if tail_count == 1000 {
                self.error(
                    self.maybe_current_node(),
                    &Diagnostics::Type_instantiation_is_excessively_deep_and_possibly_infinite,
                    None,
                );
                result = self.error_type();
                break;
            }
            let is_unwrapped = self.is_typical_nondistributive_conditional(&root);
            let check_type = self
                .instantiate_type(
                    Some(self.unwrap_nondistributive_conditional_tuple(
                        &root,
                        &self.get_actual_type_variable(&root.check_type),
                    )),
                    mapper.as_ref(),
                )
                .unwrap();
            let check_type_instantiable = self.is_generic_type(&check_type);
            let extends_type = self
                .instantiate_type(
                    Some(self.unwrap_nondistributive_conditional_tuple(&root, &root.extends_type)),
                    mapper.as_ref(),
                )
                .unwrap();
            if Rc::ptr_eq(&check_type, &self.wildcard_type())
                || Rc::ptr_eq(&extends_type, &self.wildcard_type())
            {
                return self.wildcard_type();
            }
            let mut combined_mapper: Option<TypeMapper> = None;
            if let Some(root_infer_type_parameters) = root.infer_type_parameters.as_ref() {
                let context = self.create_inference_context(
                    root_infer_type_parameters,
                    Option::<&Signature>::None,
                    InferenceFlags::None,
                    Option::<fn(&Type, &Type, Option<bool>) -> Ternary>::None,
                );
                if !check_type_instantiable {
                    self.infer_types(
                        &context.inferences,
                        &check_type,
                        &extends_type,
                        Some(InferencePriority::NoConstraints | InferencePriority::AlwaysStrict),
                        None,
                    );
                }
                combined_mapper = Some(if let Some(mapper) = mapper.as_ref() {
                    self.combine_type_mappers(Some(context.mapper.clone()), mapper.clone())
                } else {
                    context.mapper.clone()
                });
            }
            let inferred_extends_type = if let Some(combined_mapper) = combined_mapper.as_ref() {
                self.instantiate_type(
                    Some(self.unwrap_nondistributive_conditional_tuple(&root, &root.extends_type)),
                    Some(combined_mapper),
                )
                .unwrap()
            } else {
                extends_type.clone()
            };
            if !check_type_instantiable && !self.is_generic_type(&inferred_extends_type) {
                if !inferred_extends_type
                    .flags()
                    .intersects(TypeFlags::AnyOrUnknown)
                    && (check_type.flags().intersects(TypeFlags::Any) && !is_unwrapped
                        || !self.is_type_assignable_to(
                            &self.get_permissive_instantiation(&check_type),
                            &self.get_permissive_instantiation(&inferred_extends_type),
                        ))
                {
                    if check_type.flags().intersects(TypeFlags::Any) && !is_unwrapped {
                        if extra_types.is_none() {
                            extra_types = Some(vec![]);
                        }
                        extra_types.as_mut().unwrap().push(
                            self.instantiate_type(
                                Some(self.get_type_from_type_node_(
                                    &root.node.as_conditional_type_node().true_type,
                                )),
                                combined_mapper.as_ref().or_else(|| mapper.as_ref()),
                            )
                            .unwrap(),
                        );
                    }
                    let false_type = self
                        .get_type_from_type_node_(&root.node.as_conditional_type_node().false_type);
                    if false_type.flags().intersects(TypeFlags::Conditional) {
                        let new_root = &false_type.as_conditional_type().root;
                        if Rc::ptr_eq(&new_root.node.parent(), &root.node)
                            && (!new_root.is_distributive
                                || Rc::ptr_eq(&new_root.check_type, &root.check_type))
                        {
                            root = new_root.clone();
                            continue;
                        }
                        let mapper_ref = mapper.clone();
                        if self.can_tail_recurse(
                            &mut root,
                            &mut mapper,
                            &mut alias_symbol,
                            &mut alias_type_arguments,
                            &mut tail_count,
                            &false_type,
                            mapper_ref.as_ref(),
                        ) {
                            continue;
                        }
                    }
                    result = self
                        .instantiate_type(Some(false_type), mapper.as_ref())
                        .unwrap();
                    break;
                }
                if inferred_extends_type
                    .flags()
                    .intersects(TypeFlags::AnyOrUnknown)
                    || self.is_type_assignable_to(
                        &self.get_restrictive_instantiation(&check_type),
                        &self.get_restrictive_instantiation(&inferred_extends_type),
                    )
                {
                    let true_type = self
                        .get_type_from_type_node_(&root.node.as_conditional_type_node().true_type);
                    let mapper_ref = mapper.clone();
                    let true_mapper = combined_mapper.as_ref().or_else(|| mapper_ref.as_ref());
                    if self.can_tail_recurse(
                        &mut root,
                        &mut mapper,
                        &mut alias_symbol,
                        &mut alias_type_arguments,
                        &mut tail_count,
                        &true_type,
                        true_mapper,
                    ) {
                        continue;
                    }
                    result = self.instantiate_type(Some(true_type), true_mapper).unwrap();
                    break;
                }
            }
            let result_base = self.create_type(TypeFlags::Conditional);
            result = ConditionalType::new(
                result_base,
                root.clone(),
                self.instantiate_type(Some(&*root.check_type), mapper.as_ref())
                    .unwrap(),
                self.instantiate_type(Some(&*root.extends_type), mapper.as_ref())
                    .unwrap(),
                mapper.clone(),
                combined_mapper,
            )
            .into();
            *result.maybe_alias_symbol() =
                alias_symbol.clone().or_else(|| root.alias_symbol.clone());
            *result.maybe_alias_type_arguments() = if alias_symbol.is_some() {
                alias_type_arguments.map(ToOwned::to_owned)
            } else {
                self.instantiate_types(
                    root.alias_type_arguments.as_deref(),
                    mapper.as_ref().unwrap(),
                )
            };
            break;
        }
        if let Some(mut extra_types) = extra_types {
            append(&mut extra_types, Some(result));
            self.get_union_type(
                extra_types,
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )
        } else {
            result
        }
    }

    pub(super) fn can_tail_recurse(
        &self,
        root: &mut ConditionalRoot,
        mapper: &mut Option<TypeMapper>,
        alias_symbol: &mut Option<Rc<Symbol>>,
        alias_type_arguments: &mut Option<&[Rc<Type>]>,
        tail_count: &mut usize,
        new_type: &Type,
        new_mapper: Option<&TypeMapper>,
    ) -> bool {
        if new_type.flags().intersects(TypeFlags::Conditional) {
            if let Some(new_mapper) = new_mapper {
                let new_type_as_conditional_type = new_type.as_conditional_type();
                let new_root = &new_type_as_conditional_type.root;
                if let Some(new_root_outer_type_parameters) =
                    new_root.outer_type_parameters.as_ref()
                {
                    let type_param_mapper = self.combine_type_mappers(
                        new_type_as_conditional_type.mapper.clone(),
                        new_mapper.clone(),
                    );
                    let type_arguments =
                        map(Some(new_root_outer_type_parameters), |t: &Rc<Type>, _| {
                            self.get_mapped_type(t, &type_param_mapper)
                        })
                        .unwrap();
                    let new_root_mapper = self.create_type_mapper(
                        new_root_outer_type_parameters.clone(),
                        Some(type_arguments),
                    );
                    let new_check_type = if new_root.is_distributive {
                        Some(self.get_mapped_type(&new_root.check_type, &new_root_mapper))
                    } else {
                        None
                    };
                    if match new_check_type.as_ref() {
                        None => true,
                        Some(new_check_type) => {
                            Rc::ptr_eq(new_check_type, &new_root.check_type)
                                || !new_check_type
                                    .flags()
                                    .intersects(TypeFlags::Union | TypeFlags::Never)
                        }
                    } {
                        *root = new_root.clone();
                        *mapper = Some(new_root_mapper);
                        *alias_symbol = None;
                        *alias_type_arguments = None;
                        if new_root.alias_symbol.is_some() {
                            *tail_count += 1;
                        }
                        return true;
                    }
                }
            }
        }
        false
    }

    pub(super) fn get_true_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        let type_as_conditional_type = type_.as_conditional_type();
        if type_as_conditional_type
            .maybe_resolved_true_type()
            .is_none()
        {
            *type_as_conditional_type.maybe_resolved_true_type() = Some(
                self.instantiate_type(
                    Some(
                        self.get_type_from_type_node_(
                            &type_as_conditional_type
                                .root
                                .node
                                .as_conditional_type_node()
                                .true_type,
                        ),
                    ),
                    type_as_conditional_type.mapper.as_ref(),
                )
                .unwrap(),
            );
        }
        type_as_conditional_type
            .maybe_resolved_true_type()
            .clone()
            .unwrap()
    }

    pub(super) fn get_false_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        let type_as_conditional_type = type_.as_conditional_type();
        if type_as_conditional_type
            .maybe_resolved_false_type()
            .is_none()
        {
            *type_as_conditional_type.maybe_resolved_false_type() = Some(
                self.instantiate_type(
                    Some(
                        self.get_type_from_type_node_(
                            &type_as_conditional_type
                                .root
                                .node
                                .as_conditional_type_node()
                                .false_type,
                        ),
                    ),
                    type_as_conditional_type.mapper.as_ref(),
                )
                .unwrap(),
            );
        }
        type_as_conditional_type
            .maybe_resolved_false_type()
            .clone()
            .unwrap()
    }

    pub(super) fn get_inferred_true_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        let type_as_conditional_type = type_.as_conditional_type();
        if type_as_conditional_type
            .maybe_resolved_true_type()
            .is_none()
        {
            *type_as_conditional_type.maybe_resolved_true_type() = Some(
                if let Some(type_combined_mapper) =
                    type_as_conditional_type.combined_mapper.as_ref()
                {
                    self.instantiate_type(
                        Some(
                            self.get_type_from_type_node_(
                                &type_as_conditional_type
                                    .root
                                    .node
                                    .as_conditional_type_node()
                                    .true_type,
                            ),
                        ),
                        Some(type_combined_mapper),
                    )
                    .unwrap()
                } else {
                    self.get_true_type_from_conditional_type(type_)
                },
            );
        }
        type_as_conditional_type
            .maybe_resolved_true_type()
            .clone()
            .unwrap()
    }

    pub(super) fn get_infer_type_parameters(
        &self,
        node: &Node, /*ConditionalTypeNode*/
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        let mut result: Option<Vec<Rc<Type>>> = None;
        if let Some(node_locals) = node.maybe_locals().clone() {
            (*node_locals)
                .borrow()
                .values()
                .for_each(|symbol: &Rc<Symbol>| {
                    if symbol.flags().intersects(SymbolFlags::TypeParameter) {
                        if result.is_none() {
                            result = Some(vec![]);
                        }
                        append(
                            result.as_mut().unwrap(),
                            Some(self.get_declared_type_of_symbol(symbol)),
                        );
                    }
                });
        }
        result
    }

    pub(super) fn is_distribution_dependent(&self, root: &ConditionalRoot) -> bool {
        root.is_distributive
            && (self.is_type_parameter_possibly_referenced(
                &root.check_type,
                &root.node.as_conditional_type_node().true_type,
            ) || self.is_type_parameter_possibly_referenced(
                &root.check_type,
                &root.node.as_conditional_type_node().false_type,
            ))
    }

    pub(super) fn get_type_from_conditional_type_node(
        &self,
        node: &Node, /*ConditionalTypeNode*/
    ) -> Rc<Type> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            let node_as_conditional_type_node = node.as_conditional_type_node();
            let check_type =
                self.get_type_from_type_node_(&node_as_conditional_type_node.check_type);
            let alias_symbol = self.get_alias_symbol_for_type_node(node);
            let alias_type_arguments =
                self.get_type_arguments_for_alias_symbol(alias_symbol.as_deref());
            let all_outer_type_parameters = self.get_outer_type_parameters(node, Some(true));
            let outer_type_parameters = if alias_type_arguments.is_some() {
                all_outer_type_parameters
            } else {
                filter(all_outer_type_parameters.as_deref(), |tp: &Rc<Type>| {
                    self.is_type_parameter_possibly_referenced(tp, node)
                })
            };
            let root = ConditionalRoot::new(
                node.node_wrapper(),
                check_type.clone(),
                self.get_type_from_type_node_(&node_as_conditional_type_node.extends_type),
                check_type.flags().intersects(TypeFlags::TypeParameter),
                self.get_infer_type_parameters(node),
                outer_type_parameters.clone(),
                alias_symbol,
                alias_type_arguments,
            );
            let resolved_type =
                self.get_conditional_type(&root, None, Option::<&Symbol>::None, None);
            links.borrow_mut().resolved_type = Some(resolved_type.clone());
            if let Some(outer_type_parameters) = outer_type_parameters {
                let mut instantiations: HashMap<String, Rc<Type>> = HashMap::new();
                instantiations.insert(
                    self.get_type_list_id(Some(&outer_type_parameters)),
                    resolved_type,
                );
                *root.maybe_instantiations() = Some(instantiations);
            }
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn get_alias_symbol_for_type_node(&self, node: &Node) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_arguments_for_alias_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }
}
