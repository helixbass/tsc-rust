#![allow(non_upper_case_globals)]

use gc::{Gc, GcCell};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::{
    HasTypeArgumentsInterface, __String, append, concatenate, create_symbol_table,
    declaration_name_to_string, every, find, get_declaration_modifier_flags_from_symbol,
    is_identifier, is_jsdoc_type_expression, is_jsdoc_type_literal, is_literal_import_type_node,
    is_optional_type_node, is_parenthesized_type_node, is_rest_type_node, is_tuple_type_node,
    is_type_alias, is_type_operator_node, length, map, maybe_concatenate, maybe_filter,
    node_is_missing, same_map, AccessFlags, CheckFlags, ConditionalRoot, ConditionalType,
    Diagnostics, IndexInfo, InferenceFlags, InferencePriority, MappedType, ModifierFlags, Node,
    NodeFlags, NodeInterface, NodeLinks, ObjectFlags, ObjectFlagsTypeInterface, Signature, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, Ternary, TransientSymbolInterface, Type, TypeChecker,
    TypeFlags, TypeInterface, TypeMapper, UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub(super) fn get_type_from_indexed_access_type_node(
        &self,
        node: &Node, /*IndexedAccessTypeNode*/
    ) -> Gc<Type> {
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
                    Gc::ptr_eq(&resolved_as_indexed_access_type.object_type, &object_type)
                        && Gc::ptr_eq(&resolved_as_indexed_access_type.index_type, &index_type)
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
    ) -> Gc<Type> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            let type_ = self.create_object_type(ObjectFlags::Mapped, node.maybe_symbol());
            let type_: Gc<Type> = MappedType::new(type_, node.node_wrapper()).into();
            let alias_symbol = self.get_alias_symbol_for_type_node(node);
            *type_.maybe_alias_symbol_mut() = alias_symbol.clone();
            *type_.maybe_alias_type_arguments_mut() =
                self.get_type_arguments_for_alias_symbol(alias_symbol);
            links.borrow_mut().resolved_type = Some(type_.clone());
            self.get_constraint_type_from_mapped_type(&type_);
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn get_actual_type_variable(&self, type_: &Type) -> Gc<Type> {
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

    pub(super) fn is_typical_nondistributive_conditional(
        &self,
        root: Gc<GcCell<ConditionalRoot>>,
    ) -> bool {
        !(*root).borrow().is_distributive
            && self.is_singleton_tuple_type(
                &(*root)
                    .borrow()
                    .node
                    .clone()
                    .as_conditional_type_node()
                    .check_type,
            )
            && self.is_singleton_tuple_type(
                &(*root)
                    .borrow()
                    .node
                    .clone()
                    .as_conditional_type_node()
                    .extends_type,
            )
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
        root: Gc<GcCell<ConditionalRoot>>,
        type_: &Type,
    ) -> Gc<Type> {
        if self.is_typical_nondistributive_conditional(root) && self.is_tuple_type(type_) {
            self.get_type_arguments(type_)[0].clone()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_conditional_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        root: Gc<GcCell<ConditionalRoot>>,
        mut mapper: Option<Gc<TypeMapper>>,
        alias_symbol: Option<TAliasSymbol>,
        mut alias_type_arguments: Option<&[Gc<Type>]>,
    ) -> Gc<Type> {
        let result: Gc<Type>;
        let mut extra_types: Option<Vec<Gc<Type>>> = None;
        let mut tail_count = 0;
        let mut root = root.clone();
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
            let is_unwrapped = self.is_typical_nondistributive_conditional(root.clone());
            let check_type = self.instantiate_type(
                &self.unwrap_nondistributive_conditional_tuple(
                    root.clone(),
                    &self.get_actual_type_variable(&(*root).borrow().check_type.clone()),
                ),
                mapper.clone(),
            );
            let check_type_instantiable = self.is_generic_type(&check_type);
            let extends_type = self.instantiate_type(
                &self.unwrap_nondistributive_conditional_tuple(
                    root.clone(),
                    &(*root).borrow().extends_type.clone(),
                ),
                mapper.clone(),
            );
            if Gc::ptr_eq(&check_type, &self.wildcard_type())
                || Gc::ptr_eq(&extends_type, &self.wildcard_type())
            {
                return self.wildcard_type();
            }
            let mut combined_mapper: Option<Gc<TypeMapper>> = None;
            if let Some(root_infer_type_parameters) =
                (*root).borrow().infer_type_parameters.clone().as_ref()
            {
                let context = self.create_inference_context(
                    root_infer_type_parameters,
                    None,
                    InferenceFlags::None,
                    None,
                );
                if !check_type_instantiable {
                    self.infer_types(
                        &context.inferences(),
                        &check_type,
                        &extends_type,
                        Some(InferencePriority::NoConstraints | InferencePriority::AlwaysStrict),
                        None,
                    );
                }
                combined_mapper = Some(if let Some(mapper) = mapper.as_ref() {
                    self.combine_type_mappers(Some(context.mapper().clone()), mapper.clone())
                } else {
                    context.mapper().clone()
                });
            }
            let inferred_extends_type = if let Some(combined_mapper) = combined_mapper.as_ref() {
                self.instantiate_type(
                    &self.unwrap_nondistributive_conditional_tuple(
                        root.clone(),
                        &(*root).borrow().extends_type.clone(),
                    ),
                    Some(combined_mapper.clone()),
                )
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
                                &self.get_type_from_type_node_(
                                    &(*root)
                                        .borrow()
                                        .node
                                        .clone()
                                        .as_conditional_type_node()
                                        .true_type,
                                ),
                                combined_mapper.clone().or_else(|| mapper.clone()),
                            ),
                        );
                    }
                    let false_type = self.get_type_from_type_node_(
                        &(*root)
                            .borrow()
                            .node
                            .clone()
                            .as_conditional_type_node()
                            .false_type,
                    );
                    if false_type.flags().intersects(TypeFlags::Conditional) {
                        let new_root = false_type.as_conditional_type().root.clone();
                        if Gc::ptr_eq(&(*new_root).borrow().node.parent(), &(*root).borrow().node)
                            && (!(*new_root).borrow().is_distributive
                                || Gc::ptr_eq(
                                    &(*new_root).borrow().check_type,
                                    &(*root).borrow().check_type,
                                ))
                        {
                            root = new_root.clone();
                            continue;
                        }
                        let mapper_clone = mapper.clone();
                        if self.can_tail_recurse(
                            &mut root,
                            &mut mapper,
                            &mut alias_symbol,
                            &mut alias_type_arguments,
                            &mut tail_count,
                            &false_type,
                            mapper_clone,
                        ) {
                            continue;
                        }
                    }
                    result = self.instantiate_type(&false_type, mapper.clone());
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
                    let true_type = self.get_type_from_type_node_(
                        &(*root)
                            .borrow()
                            .node
                            .clone()
                            .as_conditional_type_node()
                            .true_type,
                    );
                    let true_mapper = combined_mapper.clone().or_else(|| mapper.clone());
                    if self.can_tail_recurse(
                        &mut root,
                        &mut mapper,
                        &mut alias_symbol,
                        &mut alias_type_arguments,
                        &mut tail_count,
                        &true_type,
                        true_mapper.clone(),
                    ) {
                        continue;
                    }
                    result = self.instantiate_type(&true_type, true_mapper);
                    break;
                }
            }
            let result_base = self.create_type(TypeFlags::Conditional);
            result = ConditionalType::new(
                result_base,
                root.clone(),
                self.instantiate_type(&(*root).borrow().check_type.clone(), mapper.clone()),
                self.instantiate_type(&(*root).borrow().extends_type.clone(), mapper.clone()),
                mapper.clone(),
                combined_mapper,
            )
            .into();
            *result.maybe_alias_symbol_mut() = alias_symbol
                .clone()
                .or_else(|| (*root).borrow().alias_symbol.clone());
            *result.maybe_alias_type_arguments_mut() = if alias_symbol.is_some() {
                alias_type_arguments.map(ToOwned::to_owned)
            } else {
                self.instantiate_types(
                    (*root).borrow().alias_type_arguments.clone().as_deref(),
                    mapper.clone(),
                )
            };
            break;
        }
        if let Some(mut extra_types) = extra_types {
            append(&mut extra_types, Some(result));
            self.get_union_type(
                &extra_types,
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
        root: &mut Gc<GcCell<ConditionalRoot>>,
        mapper: &mut Option<Gc<TypeMapper>>,
        alias_symbol: &mut Option<Gc<Symbol>>,
        alias_type_arguments: &mut Option<&[Gc<Type>]>,
        tail_count: &mut usize,
        new_type: &Type,
        new_mapper: Option<Gc<TypeMapper>>,
    ) -> bool {
        if new_type.flags().intersects(TypeFlags::Conditional) {
            if let Some(new_mapper) = new_mapper {
                let new_type_as_conditional_type = new_type.as_conditional_type();
                let new_root = new_type_as_conditional_type.root.clone();
                let new_root_outer_type_parameters =
                    (*new_root).borrow().outer_type_parameters.clone();
                if let Some(new_root_outer_type_parameters) =
                    new_root_outer_type_parameters.as_ref()
                {
                    let type_param_mapper = self.combine_type_mappers(
                        new_type_as_conditional_type.mapper.clone(),
                        new_mapper,
                    );
                    let type_arguments = map(new_root_outer_type_parameters, |t: &Gc<Type>, _| {
                        self.get_mapped_type(t, &type_param_mapper)
                    });
                    let new_root_mapper = Gc::new(self.create_type_mapper(
                        new_root_outer_type_parameters.clone(),
                        Some(type_arguments),
                    ));
                    let new_check_type = if (*new_root).borrow().is_distributive {
                        Some(self.get_mapped_type(
                            &(*new_root).borrow().clone().check_type,
                            &new_root_mapper,
                        ))
                    } else {
                        None
                    };
                    if match new_check_type.as_ref() {
                        None => true,
                        Some(new_check_type) => {
                            Gc::ptr_eq(new_check_type, &(*new_root).borrow().check_type)
                                || !new_check_type
                                    .flags()
                                    .intersects(TypeFlags::Union | TypeFlags::Never)
                        }
                    } {
                        *root = new_root.clone();
                        *mapper = Some(new_root_mapper);
                        *alias_symbol = None;
                        *alias_type_arguments = None;
                        if (*new_root).borrow().alias_symbol.is_some() {
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
    ) -> Gc<Type> {
        let type_as_conditional_type = type_.as_conditional_type();
        if type_as_conditional_type
            .maybe_resolved_true_type()
            .is_none()
        {
            *type_as_conditional_type.maybe_resolved_true_type() = Some(
                self.instantiate_type(
                    &self.get_type_from_type_node_(
                        &(*type_as_conditional_type.root)
                            .borrow()
                            .node
                            .clone()
                            .as_conditional_type_node()
                            .true_type,
                    ),
                    type_as_conditional_type.mapper.clone(),
                ),
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
    ) -> Gc<Type> {
        let type_as_conditional_type = type_.as_conditional_type();
        if type_as_conditional_type
            .maybe_resolved_false_type()
            .is_none()
        {
            *type_as_conditional_type.maybe_resolved_false_type() = Some(
                self.instantiate_type(
                    &self.get_type_from_type_node_(
                        &(*type_as_conditional_type.root)
                            .borrow()
                            .node
                            .clone()
                            .as_conditional_type_node()
                            .false_type,
                    ),
                    type_as_conditional_type.mapper.clone(),
                ),
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
    ) -> Gc<Type> {
        let type_as_conditional_type = type_.as_conditional_type();
        if type_as_conditional_type
            .maybe_resolved_inferred_true_type()
            .is_none()
        {
            *type_as_conditional_type.maybe_resolved_inferred_true_type() = Some(
                if let Some(type_combined_mapper) = type_as_conditional_type.combined_mapper.clone()
                {
                    self.instantiate_type(
                        &self.get_type_from_type_node_(
                            &(*type_as_conditional_type.root)
                                .borrow()
                                .node
                                .clone()
                                .as_conditional_type_node()
                                .true_type,
                        ),
                        Some(type_combined_mapper),
                    )
                } else {
                    self.get_true_type_from_conditional_type(type_)
                },
            );
        }
        type_as_conditional_type
            .maybe_resolved_inferred_true_type()
            .clone()
            .unwrap()
    }

    pub(super) fn get_infer_type_parameters(
        &self,
        node: &Node, /*ConditionalTypeNode*/
    ) -> Option<Vec<Gc<Type /*TypeParameter*/>>> {
        let mut result: Option<Vec<Gc<Type>>> = None;
        if let Some(node_locals) = node.maybe_locals().clone() {
            (*node_locals)
                .borrow()
                .values()
                .for_each(|symbol: &Gc<Symbol>| {
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
    ) -> Gc<Type> {
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
                maybe_filter(all_outer_type_parameters.as_deref(), |tp: &Gc<Type>| {
                    self.is_type_parameter_possibly_referenced(tp, node)
                })
            };
            let root = Gc::new(GcCell::new(ConditionalRoot::new(
                node.node_wrapper(),
                check_type.clone(),
                self.get_type_from_type_node_(&node_as_conditional_type_node.extends_type),
                check_type.flags().intersects(TypeFlags::TypeParameter),
                self.get_infer_type_parameters(node),
                outer_type_parameters.clone(),
                alias_symbol,
                alias_type_arguments,
            )));
            let resolved_type =
                self.get_conditional_type(root.clone(), None, Option::<&Symbol>::None, None);
            links.borrow_mut().resolved_type = Some(resolved_type.clone());
            if let Some(outer_type_parameters) = outer_type_parameters {
                let mut instantiations: HashMap<String, Gc<Type>> = HashMap::new();
                instantiations.insert(
                    self.get_type_list_id(Some(&outer_type_parameters)),
                    resolved_type,
                );
                *root.borrow_mut().maybe_instantiations() = Some(instantiations);
            }
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn get_type_from_infer_type_node(
        &self,
        node: &Node, /*InferTypeNode*/
    ) -> Gc<Type> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            links.borrow_mut().resolved_type = Some(
                self.get_declared_type_of_type_parameter(
                    &self
                        .get_symbol_of_node(&node.as_infer_type_node().type_parameter)
                        .unwrap(),
                ),
            );
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn get_identifier_chain(
        &self,
        node: &Node, /*EntityName*/
    ) -> Vec<Gc<Node /*Identifier*/>> {
        if is_identifier(node) {
            vec![node.node_wrapper()]
        } else {
            let node_as_qualified_name = node.as_qualified_name();
            let mut ret = self.get_identifier_chain(&node_as_qualified_name.left);
            append(&mut ret, Some(node_as_qualified_name.right.clone()));
            ret
        }
    }

    pub(super) fn get_type_from_import_type_node(
        &self,
        node: &Node, /*ImportTypeNode*/
    ) -> Gc<Type> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            let node_as_import_type_node = node.as_import_type_node();
            if node_as_import_type_node.is_type_of()
                && node_as_import_type_node.maybe_type_arguments().is_some()
            {
                self.error(
                    Some(node),
                    &Diagnostics::Type_arguments_cannot_be_used_here,
                    None,
                );
                let mut links = links.borrow_mut();
                links.resolved_symbol = Some(self.unknown_symbol());
                let ret = self.error_type();
                links.resolved_type = Some(ret.clone());
                return ret;
            }
            if !is_literal_import_type_node(node) {
                self.error(
                    Some(&*node_as_import_type_node.argument),
                    &Diagnostics::String_literal_expected,
                    None,
                );
                let mut links = links.borrow_mut();
                links.resolved_symbol = Some(self.unknown_symbol());
                let ret = self.error_type();
                links.resolved_type = Some(ret.clone());
                return ret;
            }
            let target_meaning = if node_as_import_type_node.is_type_of() {
                SymbolFlags::Value
            } else if node.flags().intersects(NodeFlags::JSDoc) {
                SymbolFlags::Value | SymbolFlags::Type
            } else {
                SymbolFlags::Type
            };
            let inner_module_symbol = self.resolve_external_module_name_(
                node,
                &node_as_import_type_node
                    .argument
                    .as_literal_type_node()
                    .literal,
                None,
            );
            if inner_module_symbol.is_none() {
                let mut links = links.borrow_mut();
                links.resolved_symbol = Some(self.unknown_symbol());
                let ret = self.error_type();
                links.resolved_type = Some(ret.clone());
                return ret;
            }
            let inner_module_symbol = inner_module_symbol.unwrap();
            let module_symbol = self
                .resolve_external_module_symbol(Some(&*inner_module_symbol), Some(false))
                .unwrap();
            if !node_is_missing(node_as_import_type_node.qualifier.as_deref()) {
                let mut name_stack: Vec<Gc<Node /*Identifier*/>> =
                    self.get_identifier_chain(node_as_import_type_node.qualifier.as_ref().unwrap());
                let mut current_namespace = module_symbol.clone();
                let mut current: Option<Gc<Node /*Identifier*/>>;
                while {
                    current = if name_stack.is_empty() {
                        None
                    } else {
                        Some(name_stack.remove(0))
                    };
                    current.is_some()
                } {
                    let current = current.unwrap();
                    let meaning = if !name_stack.is_empty() {
                        SymbolFlags::Namespace
                    } else {
                        target_meaning
                    };
                    let merged_resolved_symbol = self
                        .get_merged_symbol(self.resolve_symbol(Some(&*current_namespace), None))
                        .unwrap();
                    let next = if node_as_import_type_node.is_type_of() {
                        self.get_property_of_type_(
                            &self.get_type_of_symbol(&merged_resolved_symbol),
                            &current.as_identifier().escaped_text,
                            None,
                        )
                    } else {
                        self.get_symbol(
                            &(*self.get_exports_of_symbol(&merged_resolved_symbol)).borrow(),
                            &current.as_identifier().escaped_text,
                            meaning,
                        )
                    };
                    if next.is_none() {
                        self.error(
                            Some(&*current),
                            &Diagnostics::Namespace_0_has_no_exported_member_1,
                            Some(vec![
                                self.get_fully_qualified_name(
                                    &current_namespace,
                                    Option::<&Node>::None,
                                ),
                                declaration_name_to_string(Some(&*current)).into_owned(),
                            ]),
                        );
                        let ret = self.error_type();
                        links.borrow_mut().resolved_type = Some(ret.clone());
                        return ret;
                    }
                    let next = next.unwrap();
                    self.get_node_links(&current).borrow_mut().resolved_symbol = Some(next.clone());
                    self.get_node_links(&current.parent())
                        .borrow_mut()
                        .resolved_symbol = Some(next.clone());
                    current_namespace = next;
                }
                links.borrow_mut().resolved_type = Some(self.resolve_import_symbol_type(
                    node,
                    &links,
                    &current_namespace,
                    target_meaning,
                ));
            } else {
                if module_symbol.flags().intersects(target_meaning) {
                    links.borrow_mut().resolved_type = Some(self.resolve_import_symbol_type(
                        node,
                        &links,
                        &module_symbol,
                        target_meaning,
                    ));
                } else {
                    let error_message = if target_meaning == SymbolFlags::Value {
                        &*Diagnostics::Module_0_does_not_refer_to_a_value_but_is_used_as_a_value_here
                    } else {
                        &*Diagnostics::Module_0_does_not_refer_to_a_type_but_is_used_as_a_type_here_Did_you_mean_typeof_import_0
                    };

                    self.error(
                        Some(node),
                        error_message,
                        Some(vec![node_as_import_type_node
                            .argument
                            .as_literal_type_node()
                            .literal
                            .as_literal_like_node()
                            .text()
                            .clone()]),
                    );

                    let mut links = links.borrow_mut();
                    links.resolved_symbol = Some(self.unknown_symbol());
                    links.resolved_type = Some(self.error_type());
                }
            }
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn resolve_import_symbol_type(
        &self,
        node: &Node, /*ImportTypeNode*/
        links: &GcCell<NodeLinks>,
        symbol: &Symbol,
        meaning: SymbolFlags,
    ) -> Gc<Type> {
        let resolved_symbol = self.resolve_symbol(Some(symbol), None).unwrap();
        links.borrow_mut().resolved_symbol = Some(resolved_symbol.clone());
        if meaning == SymbolFlags::Value {
            self.get_type_of_symbol(symbol)
        } else {
            self.get_type_reference_type(node, &resolved_symbol)
        }
    }

    pub(super) fn get_type_from_type_literal_or_function_or_constructor_type_node(
        &self,
        node: &Node, /*TypeNode*/
    ) -> Gc<Type> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            let alias_symbol = self.get_alias_symbol_for_type_node(node);
            if (*self.get_members_of_symbol(&node.symbol()))
                .borrow()
                .is_empty()
                && alias_symbol.is_none()
            {
                links.borrow_mut().resolved_type = Some(self.empty_type_literal_type());
            } else {
                let mut type_: Gc<Type> = self
                    .create_object_type(ObjectFlags::Anonymous, node.maybe_symbol())
                    .into();
                *type_.maybe_alias_symbol_mut() = alias_symbol.clone();
                *type_.maybe_alias_type_arguments_mut() =
                    self.get_type_arguments_for_alias_symbol(alias_symbol.as_deref());
                if is_jsdoc_type_literal(node) && node.as_jsdoc_type_literal().is_array_type {
                    type_ = self.create_array_type(&type_, None);
                }
                links.borrow_mut().resolved_type = Some(type_);
            }
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn get_alias_symbol_for_type_node(&self, node: &Node) -> Option<Gc<Symbol>> {
        let mut host = node.parent();
        while is_parenthesized_type_node(&host)
            || is_jsdoc_type_expression(&host)
            || is_type_operator_node(&host)
                && host.as_type_operator_node().operator == SyntaxKind::ReadonlyKeyword
        {
            host = host.parent();
        }
        if is_type_alias(&host) {
            self.get_symbol_of_node(&host)
        } else {
            None
        }
    }

    pub(super) fn get_type_arguments_for_alias_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
    ) -> Option<Vec<Gc<Type>>> {
        symbol.and_then(|symbol| {
            self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol.borrow())
        })
    }

    pub(super) fn is_non_generic_object_type(&self, type_: &Type) -> bool {
        type_.flags().intersects(TypeFlags::Object) && !self.is_generic_mapped_type(type_)
    }

    pub(super) fn is_empty_object_type_or_spreads_into_empty_object(&self, type_: &Type) -> bool {
        self.is_empty_object_type(type_)
            || type_.flags().intersects(
                TypeFlags::Null
                    | TypeFlags::Undefined
                    | TypeFlags::BooleanLike
                    | TypeFlags::NumberLike
                    | TypeFlags::BigIntLike
                    | TypeFlags::StringLike
                    | TypeFlags::EnumLike
                    | TypeFlags::NonPrimitive
                    | TypeFlags::Index,
            )
    }

    pub(super) fn try_merge_union_of_object_type_and_empty_object(
        &self,
        type_: &Type,
        readonly: bool,
    ) -> Gc<Type> {
        if !type_.flags().intersects(TypeFlags::Union) {
            return type_.type_wrapper();
        }
        let type_as_union_type = type_.as_union_type();
        if every(type_as_union_type.types(), |type_: &Gc<Type>, _| {
            self.is_empty_object_type_or_spreads_into_empty_object(type_)
        }) {
            return find(type_as_union_type.types(), |type_: &Gc<Type>, _| {
                self.is_empty_object_type(type_)
            })
            .map(Clone::clone)
            .unwrap_or_else(|| self.empty_object_type());
        }
        let first_type = find(type_as_union_type.types(), |type_: &Gc<Type>, _| {
            !self.is_empty_object_type_or_spreads_into_empty_object(type_)
        })
        .map(Clone::clone);
        if first_type.is_none() {
            return type_.type_wrapper();
        }
        let first_type = first_type.unwrap();
        let second_type = find(type_as_union_type.types(), |t: &Gc<Type>, _| {
            !Gc::ptr_eq(t, &first_type)
                && !self.is_empty_object_type_or_spreads_into_empty_object(type_)
        })
        .map(Clone::clone);
        if second_type.is_some() {
            return type_.type_wrapper();
        }
        self.get_anonymous_partial_type(readonly, &first_type)
    }

    pub(super) fn get_anonymous_partial_type(&self, readonly: bool, type_: &Type) -> Gc<Type> {
        let mut members = create_symbol_table(None);
        for prop in self.get_properties_of_type(type_) {
            if get_declaration_modifier_flags_from_symbol(&prop, None)
                .intersects(ModifierFlags::Private | ModifierFlags::Protected)
            {
            } else if self.is_spreadable_property(&prop) {
                let is_setonly_accessor = prop.flags().intersects(SymbolFlags::SetAccessor)
                    && !prop.flags().intersects(SymbolFlags::GetAccessor);
                let flags = SymbolFlags::Property | SymbolFlags::Optional;
                let result: Gc<Symbol> = self
                    .create_symbol(
                        flags,
                        prop.escaped_name().to_owned(),
                        Some(
                            self.get_is_late_check_flag(&prop)
                                | if readonly {
                                    CheckFlags::Readonly
                                } else {
                                    CheckFlags::None
                                },
                        ),
                    )
                    .into();
                let result_links = result.as_transient_symbol().symbol_links();
                let mut result_links = result_links.borrow_mut();
                result_links.type_ = Some(if is_setonly_accessor {
                    self.undefined_type()
                } else {
                    self.add_optionality(&self.get_type_of_symbol(&prop), Some(true), None)
                });
                let prop_declarations = prop.maybe_declarations();
                if let Some(prop_declarations) = prop_declarations.as_ref() {
                    result.set_declarations(prop_declarations.clone());
                }
                result_links.name_type = (*self.get_symbol_links(&prop)).borrow().name_type.clone();
                result_links.synthetic_origin = Some(prop.clone());
                members.insert(prop.escaped_name().to_owned(), result);
            }
        }
        let spread = self.create_anonymous_type(
            type_.maybe_symbol(),
            Gc::new(GcCell::new(members)),
            vec![],
            vec![],
            self.get_index_infos_of_type(type_),
        );
        let spread_as_object_type = spread.as_object_type();
        spread_as_object_type.set_object_flags(
            spread_as_object_type.object_flags()
                | ObjectFlags::ObjectLiteral
                | ObjectFlags::ContainsObjectOrArrayLiteral,
        );
        spread
    }

    pub(super) fn get_spread_type<TSymbol: Borrow<Symbol>>(
        &self,
        left: &Type,
        right: &Type,
        symbol: Option<TSymbol>,
        object_flags: ObjectFlags,
        readonly: bool,
    ) -> Gc<Type> {
        if left.flags().intersects(TypeFlags::Any) || right.flags().intersects(TypeFlags::Any) {
            return self.any_type();
        }
        if left.flags().intersects(TypeFlags::Unknown)
            || right.flags().intersects(TypeFlags::Unknown)
        {
            return self.unknown_type();
        }
        if left.flags().intersects(TypeFlags::Never) {
            return right.type_wrapper();
        }
        if right.flags().intersects(TypeFlags::Never) {
            return left.type_wrapper();
        }
        let left = self.try_merge_union_of_object_type_and_empty_object(left, readonly);
        let symbol = symbol.map(|symbol| symbol.borrow().symbol_wrapper());
        if left.flags().intersects(TypeFlags::Union) {
            return if self.check_cross_product_union(&[left.clone(), right.type_wrapper()]) {
                self.map_type(
                    &left,
                    &mut |t| {
                        Some(self.get_spread_type(
                            t,
                            right,
                            symbol.as_deref(),
                            object_flags,
                            readonly,
                        ))
                    },
                    None,
                )
                .unwrap()
            } else {
                self.error_type()
            };
        }
        let right = self.try_merge_union_of_object_type_and_empty_object(right, readonly);
        if right.flags().intersects(TypeFlags::Union) {
            return if self.check_cross_product_union(&[left.clone(), right.clone()]) {
                self.map_type(
                    &right,
                    &mut |t| {
                        Some(self.get_spread_type(
                            &left,
                            t,
                            symbol.as_deref(),
                            object_flags,
                            readonly,
                        ))
                    },
                    None,
                )
                .unwrap()
            } else {
                self.error_type()
            };
        }
        if right.flags().intersects(
            TypeFlags::BooleanLike
                | TypeFlags::NumberLike
                | TypeFlags::BigIntLike
                | TypeFlags::StringLike
                | TypeFlags::EnumLike
                | TypeFlags::NonPrimitive
                | TypeFlags::Index,
        ) {
            return left;
        }

        if self.is_generic_object_type(&left) || self.is_generic_object_type(&right) {
            if self.is_empty_object_type(&left) {
                return right;
            }
            if left.flags().intersects(TypeFlags::Intersection) {
                let types = left.as_intersection_type().types();
                let last_left = &types[types.len() - 1];
                if self.is_non_generic_object_type(last_left)
                    && self.is_non_generic_object_type(&right)
                {
                    return self.get_intersection_type(
                        &concatenate(
                            types[0..types.len() - 1].to_owned(),
                            vec![self.get_spread_type(
                                last_left,
                                &right,
                                symbol.as_deref(),
                                object_flags,
                                readonly,
                            )],
                        ),
                        Option::<&Symbol>::None,
                        None,
                    );
                }
            }
            return self.get_intersection_type(&vec![left, right], Option::<&Symbol>::None, None);
        }

        let mut members = create_symbol_table(None);
        let mut skipped_private_members: HashSet<__String> = HashSet::new();
        let index_infos = if Gc::ptr_eq(&left, &self.empty_object_type()) {
            self.get_index_infos_of_type(&right)
        } else {
            self.get_union_index_infos(&[left.clone(), right.clone()])
        };

        for right_prop in self.get_properties_of_type(&right) {
            if get_declaration_modifier_flags_from_symbol(&right_prop, None)
                .intersects(ModifierFlags::Private | ModifierFlags::Protected)
            {
                skipped_private_members.insert(right_prop.escaped_name().to_owned());
            } else if self.is_spreadable_property(&right_prop) {
                members.insert(
                    right_prop.escaped_name().to_owned(),
                    self.get_spread_symbol(&right_prop, readonly),
                );
            }
        }

        for left_prop in self.get_properties_of_type(&left) {
            if skipped_private_members.contains(left_prop.escaped_name())
                || !self.is_spreadable_property(&left_prop)
            {
                continue;
            }
            if members.contains_key(left_prop.escaped_name()) {
                let right_prop = members.get(left_prop.escaped_name()).unwrap();
                let right_type = self.get_type_of_symbol(right_prop);
                if right_prop.flags().intersects(SymbolFlags::Optional) {
                    let declarations = maybe_concatenate(
                        left_prop.maybe_declarations().clone(),
                        right_prop.maybe_declarations().clone(),
                    );
                    let flags = SymbolFlags::Property | (left_prop.flags() & SymbolFlags::Optional);
                    let result: Gc<Symbol> = self
                        .create_symbol(flags, left_prop.escaped_name().to_owned(), None)
                        .into();
                    let result_links = result.as_transient_symbol().symbol_links();
                    let mut result_links = result_links.borrow_mut();
                    result_links.type_ = Some(self.get_union_type(
                        &[
                            self.get_type_of_symbol(&left_prop),
                            self.remove_missing_or_undefined_type(&right_type),
                        ],
                        None,
                        Option::<&Symbol>::None,
                        None,
                        Option::<&Type>::None,
                    ));
                    result_links.left_spread = Some(left_prop.clone());
                    result_links.right_spread = Some(right_prop.clone());
                    if let Some(declarations) = declarations {
                        result.set_declarations(declarations);
                    }
                    result_links.name_type = (*self.get_symbol_links(&left_prop))
                        .borrow()
                        .name_type
                        .clone();
                    members.insert(left_prop.escaped_name().to_owned(), result);
                }
            } else {
                members.insert(
                    left_prop.escaped_name().to_owned(),
                    self.get_spread_symbol(&left_prop, readonly),
                );
            }
        }

        let spread = self.create_anonymous_type(
            symbol.as_deref(),
            Gc::new(GcCell::new(members)),
            vec![],
            vec![],
            same_map(&index_infos, |info: &Gc<IndexInfo>, _| {
                self.get_index_info_with_readonly(info, readonly)
            }),
        );
        let spread_as_object_type = spread.as_object_type();
        spread_as_object_type.set_object_flags(
            spread_as_object_type.object_flags()
                | ObjectFlags::ObjectLiteral
                | ObjectFlags::ContainsObjectOrArrayLiteral
                | ObjectFlags::ContainsSpread
                | object_flags,
        );
        spread
    }
}
