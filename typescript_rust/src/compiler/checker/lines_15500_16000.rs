use std::{
    collections::{HashMap, HashSet},
    io,
};

use id_arena::Id;

use crate::{
    try_every, HasTypeArgumentsInterface, __String, append, concatenate, create_symbol_table,
    declaration_name_to_string, get_declaration_modifier_flags_from_symbol, is_identifier,
    is_jsdoc_type_expression, is_jsdoc_type_literal, is_literal_import_type_node,
    is_optional_type_node, is_parenthesized_type_node, is_rest_type_node, is_tuple_type_node,
    is_type_alias, is_type_operator_node, length, maybe_concatenate, node_is_missing, released,
    same_map, try_find, try_map, try_maybe_filter, AccessFlags, CheckFlags, ConditionalRoot,
    ConditionalType, Diagnostics, HasArena, InArena, IndexInfo, InferenceFlags, InferencePriority,
    MappedType, ModifierFlags, Node, NodeFlags, NodeInterface, NodeLinks, ObjectFlags,
    ObjectFlagsTypeInterface, OptionInArena, OptionTry, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub(super) fn get_type_from_indexed_access_type_node(
        &self,
        node: Id<Node>, /*IndexedAccessTypeNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            let object_type = self.get_type_from_type_node_(
                node.ref_(self).as_indexed_access_type_node().object_type,
            )?;
            let index_type = self.get_type_from_type_node_(
                node.ref_(self).as_indexed_access_type_node().index_type,
            )?;
            let potential_alias = self.get_alias_symbol_for_type_node(node)?;
            let resolved = self.get_indexed_access_type(
                object_type,
                index_type,
                Some(AccessFlags::None),
                Some(node),
                potential_alias,
                self.get_type_arguments_for_alias_symbol(potential_alias)?
                    .as_deref(),
            )?;
            links.ref_mut(self).resolved_type = Some(
                if resolved
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::IndexedAccess)
                    && {
                        resolved.ref_(self).as_indexed_access_type().object_type == object_type
                            && resolved.ref_(self).as_indexed_access_type().index_type == index_type
                    }
                {
                    self.get_conditional_flow_type_of_type(resolved, node)?
                } else {
                    resolved
                },
            );
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_type_from_mapped_type_node(
        &self,
        node: Id<Node>, /*MappedTypeNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            let type_ =
                self.create_object_type(ObjectFlags::Mapped, node.ref_(self).maybe_symbol());
            let type_ = self.alloc_type(MappedType::new(type_, node).into());
            let alias_symbol = self.get_alias_symbol_for_type_node(node)?;
            type_.ref_(self).set_alias_symbol(alias_symbol);
            let alias_type_arguments = self.get_type_arguments_for_alias_symbol(alias_symbol)?;
            type_
                .ref_(self)
                .set_alias_type_arguments(alias_type_arguments);
            links.ref_mut(self).resolved_type = Some(type_.clone());
            self.get_constraint_type_from_mapped_type(type_)?;
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_actual_type_variable(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        if type_.ref_(self).flags().intersects(TypeFlags::Substitution) {
            return Ok(type_.ref_(self).as_substitution_type().base_type.clone());
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::IndexedAccess)
        {
            if type_
                .ref_(self)
                .as_indexed_access_type()
                .object_type
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Substitution)
                || type_
                    .ref_(self)
                    .as_indexed_access_type()
                    .index_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::Substitution)
            {
                return self.get_indexed_access_type(
                    self.get_actual_type_variable(released!(
                        type_.ref_(self).as_indexed_access_type().object_type
                    ))?,
                    self.get_actual_type_variable(released!(
                        type_.ref_(self).as_indexed_access_type().index_type
                    ))?,
                    None,
                    Option::<Id<Node>>::None,
                    Option::<Id<Symbol>>::None,
                    None,
                );
            }
        }
        Ok(type_)
    }

    pub(super) fn is_typical_nondistributive_conditional(&self, root: Id<ConditionalRoot>) -> bool {
        !root.ref_(self).is_distributive
            && self.is_singleton_tuple_type(
                root.ref_(self)
                    .node
                    .ref_(self)
                    .as_conditional_type_node()
                    .check_type,
            )
            && self.is_singleton_tuple_type(
                root.ref_(self)
                    .node
                    .ref_(self)
                    .as_conditional_type_node()
                    .extends_type,
            )
    }

    pub(super) fn is_singleton_tuple_type(&self, node: Id<Node> /*TypeNode*/) -> bool {
        is_tuple_type_node(&node.ref_(self)) && {
            let node_ref = node.ref_(self);
            let node_as_tuple_type_node = node_ref.as_tuple_type_node();
            length(Some(&*node_as_tuple_type_node.elements.ref_(self))) == 1
                && !is_optional_type_node(
                    &node_as_tuple_type_node.elements.ref_(self)[0].ref_(self),
                )
                && !is_rest_type_node(&node_as_tuple_type_node.elements.ref_(self)[0].ref_(self))
        }
    }

    pub(super) fn unwrap_nondistributive_conditional_tuple(
        &self,
        root: Id<ConditionalRoot>,
        type_: Id<Type>,
    ) -> io::Result<Id<Type>> {
        Ok(
            if self.is_typical_nondistributive_conditional(root) && self.is_tuple_type(type_) {
                self.get_type_arguments(type_)?[0].clone()
            } else {
                type_
            },
        )
    }

    pub(super) fn get_conditional_type(
        &self,
        root: Id<ConditionalRoot>,
        mut mapper: Option<Id<TypeMapper>>,
        mut alias_symbol: Option<Id<Symbol>>,
        mut alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type>> {
        let result: Id<Type>;
        let mut extra_types: Option<Vec<Id<Type>>> = None;
        let mut tail_count = 0;
        let mut root = root.clone();
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
                self.unwrap_nondistributive_conditional_tuple(
                    root.clone(),
                    self.get_actual_type_variable(released!(root.ref_(self).check_type))?,
                )?,
                mapper.clone(),
            )?;
            let check_type_instantiable = self.is_generic_type(check_type)?;
            let extends_type = self.instantiate_type(
                self.unwrap_nondistributive_conditional_tuple(
                    root.clone(),
                    root.ref_(self).extends_type.clone(),
                )?,
                mapper.clone(),
            )?;
            if check_type == self.wildcard_type() || extends_type == self.wildcard_type() {
                return Ok(self.wildcard_type());
            }
            let mut combined_mapper: Option<Id<TypeMapper>> = None;
            if let Some(root_infer_type_parameters) =
                released!(root.ref_(self).infer_type_parameters.clone()).as_ref()
            {
                let context = self.create_inference_context(
                    root_infer_type_parameters,
                    None,
                    InferenceFlags::None,
                    None,
                );
                if !check_type_instantiable {
                    self.infer_types(
                        &released!(context.ref_(self).inferences().clone()),
                        check_type,
                        extends_type,
                        Some(InferencePriority::NoConstraints | InferencePriority::AlwaysStrict),
                        None,
                    )?;
                }
                combined_mapper = Some(if let Some(mapper) = mapper.as_ref() {
                    self.combine_type_mappers(
                        Some(context.ref_(self).mapper().clone()),
                        mapper.clone(),
                    )
                } else {
                    context.ref_(self).mapper().clone()
                });
            }
            let inferred_extends_type = if let Some(combined_mapper) = combined_mapper.as_ref() {
                self.instantiate_type(
                    self.unwrap_nondistributive_conditional_tuple(
                        root.clone(),
                        root.ref_(self).extends_type.clone(),
                    )?,
                    Some(combined_mapper.clone()),
                )?
            } else {
                extends_type.clone()
            };
            if !check_type_instantiable && !self.is_generic_type(inferred_extends_type)? {
                if !inferred_extends_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::AnyOrUnknown)
                    && (check_type.ref_(self).flags().intersects(TypeFlags::Any) && !is_unwrapped
                        || !self.is_type_assignable_to(
                            self.get_permissive_instantiation(check_type)?,
                            self.get_permissive_instantiation(inferred_extends_type)?,
                        )?)
                {
                    if check_type.ref_(self).flags().intersects(TypeFlags::Any) && !is_unwrapped {
                        if extra_types.is_none() {
                            extra_types = Some(vec![]);
                        }
                        extra_types.as_mut().unwrap().push(self.instantiate_type(
                            self.get_type_from_type_node_(released!(root.ref_(self)
                                        .node
                                        .ref_(self)
                                        .as_conditional_type_node()
                                        .true_type))?,
                            combined_mapper.clone().or_else(|| mapper.clone()),
                        )?);
                    }
                    let false_type = self.get_type_from_type_node_(released!(
                        root.ref_(self)
                            .node
                            .ref_(self)
                            .as_conditional_type_node()
                            .false_type
                    ))?;
                    if false_type
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::Conditional)
                    {
                        let new_root = false_type.ref_(self).as_conditional_type().root.clone();
                        if new_root.ref_(self).node.ref_(self).parent() == root.ref_(self).node
                            && (!new_root.ref_(self).is_distributive
                                || new_root.ref_(self).check_type == root.ref_(self).check_type)
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
                            false_type,
                            mapper_clone,
                        )? {
                            continue;
                        }
                    }
                    result = self.instantiate_type(false_type, mapper.clone())?;
                    break;
                }
                if inferred_extends_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::AnyOrUnknown)
                    || self.is_type_assignable_to(
                        self.get_restrictive_instantiation(check_type)?,
                        self.get_restrictive_instantiation(inferred_extends_type)?,
                    )?
                {
                    let true_type = self.get_type_from_type_node_(released!(
                        root.ref_(self)
                            .node
                            .ref_(self)
                            .as_conditional_type_node()
                            .true_type
                    ))?;
                    let true_mapper = combined_mapper.clone().or_else(|| mapper.clone());
                    if self.can_tail_recurse(
                        &mut root,
                        &mut mapper,
                        &mut alias_symbol,
                        &mut alias_type_arguments,
                        &mut tail_count,
                        true_type,
                        true_mapper.clone(),
                    )? {
                        continue;
                    }
                    result = self.instantiate_type(true_type, true_mapper)?;
                    break;
                }
            }
            let result_base = self.create_type(TypeFlags::Conditional);
            result = self.alloc_type(
                ConditionalType::new(
                    result_base,
                    root.clone(),
                    self.instantiate_type(root.ref_(self).check_type.clone(), mapper.clone())?,
                    self.instantiate_type(root.ref_(self).extends_type.clone(), mapper.clone())?,
                    mapper.clone(),
                    combined_mapper,
                )
                .into(),
            );
            result
                .ref_(self)
                .set_alias_symbol(alias_symbol.or_else(|| root.ref_(self).alias_symbol));
            result
                .ref_(self)
                .set_alias_type_arguments(if alias_symbol.is_some() {
                    alias_type_arguments.map(ToOwned::to_owned)
                } else {
                    self.instantiate_types(
                        root.ref_(self).alias_type_arguments.clone().as_deref(),
                        mapper.clone(),
                    )?
                });
            break;
        }
        Ok(if let Some(mut extra_types) = extra_types {
            append(&mut extra_types, Some(result));
            self.get_union_type(&extra_types, None, Option::<Id<Symbol>>::None, None, None)?
        } else {
            result
        })
    }

    pub(super) fn can_tail_recurse(
        &self,
        root: &mut Id<ConditionalRoot>,
        mapper: &mut Option<Id<TypeMapper>>,
        alias_symbol: &mut Option<Id<Symbol>>,
        alias_type_arguments: &mut Option<&[Id<Type>]>,
        tail_count: &mut usize,
        new_type: Id<Type>,
        new_mapper: Option<Id<TypeMapper>>,
    ) -> io::Result<bool> {
        if new_type
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Conditional)
        {
            if let Some(new_mapper) = new_mapper {
                let new_root = new_type.ref_(self).as_conditional_type().root.clone();
                let new_root_outer_type_parameters =
                    new_root.ref_(self).outer_type_parameters.clone();
                if let Some(new_root_outer_type_parameters) =
                    new_root_outer_type_parameters.as_ref()
                {
                    let type_param_mapper = self.combine_type_mappers(
                        new_type.ref_(self).as_conditional_type().mapper.clone(),
                        new_mapper,
                    );
                    let type_arguments =
                        try_map(new_root_outer_type_parameters, |&t: &Id<Type>, _| {
                            self.get_mapped_type(t, type_param_mapper)
                        })?;
                    let new_root_mapper = self.create_type_mapper(
                        new_root_outer_type_parameters.clone(),
                        Some(type_arguments),
                    );
                    let new_check_type = if new_root.ref_(self).is_distributive {
                        Some(self.get_mapped_type(
                            new_root.ref_(self).clone().check_type,
                            new_root_mapper,
                        )?)
                    } else {
                        None
                    };
                    if match new_check_type {
                        None => true,
                        Some(new_check_type) => {
                            new_check_type == new_root.ref_(self).check_type
                                || !new_check_type
                                    .ref_(self)
                                    .flags()
                                    .intersects(TypeFlags::Union | TypeFlags::Never)
                        }
                    } {
                        *root = new_root.clone();
                        *mapper = Some(new_root_mapper);
                        *alias_symbol = None;
                        *alias_type_arguments = None;
                        if new_root.ref_(self).alias_symbol.is_some() {
                            *tail_count += 1;
                        }
                        return Ok(true);
                    }
                }
            }
        }
        Ok(false)
    }

    pub(super) fn get_true_type_from_conditional_type(
        &self,
        type_: Id<Type>, /*ConditionalType*/
    ) -> io::Result<Id<Type>> {
        if type_
            .ref_(self)
            .as_conditional_type()
            .maybe_resolved_true_type()
            .is_none()
        {
            let resolved_true_type = self.instantiate_type(
                self.get_type_from_type_node_(released!(
                    type_
                        .ref_(self)
                        .as_conditional_type()
                        .root
                        .ref_(self)
                        .node
                        .ref_(self)
                        .as_conditional_type_node()
                        .true_type
                ))?,
                released!(type_.ref_(self).as_conditional_type().mapper),
            )?;
            type_
                .ref_(self)
                .as_conditional_type()
                .set_resolved_true_type(Some(resolved_true_type));
        }
        Ok(type_
            .ref_(self)
            .as_conditional_type()
            .maybe_resolved_true_type()
            .unwrap())
    }

    pub(super) fn get_false_type_from_conditional_type(
        &self,
        type_: Id<Type>, /*ConditionalType*/
    ) -> io::Result<Id<Type>> {
        if type_
            .ref_(self)
            .as_conditional_type()
            .maybe_resolved_false_type()
            .is_none()
        {
            let resolved_false_type = self.instantiate_type(
                self.get_type_from_type_node_(released!(
                    type_
                        .ref_(self)
                        .as_conditional_type()
                        .root
                        .ref_(self)
                        .node
                        .ref_(self)
                        .as_conditional_type_node()
                        .false_type
                ))?,
                {
                    let mapper = type_.ref_(self).as_conditional_type().mapper.clone();
                    mapper
                },
            )?;
            type_
                .ref_(self)
                .as_conditional_type()
                .set_resolved_false_type(Some(resolved_false_type));
        }
        Ok(type_
            .ref_(self)
            .as_conditional_type()
            .maybe_resolved_false_type()
            .unwrap())
    }

    pub(super) fn get_inferred_true_type_from_conditional_type(
        &self,
        type_: Id<Type>, /*ConditionalType*/
    ) -> io::Result<Id<Type>> {
        if type_
            .ref_(self)
            .as_conditional_type()
            .maybe_resolved_inferred_true_type()
            .is_none()
        {
            let resolved_inferred_true_type = if let Some(type_combined_mapper) =
                released!(type_.ref_(self).as_conditional_type().combined_mapper)
            {
                self.instantiate_type(
                    self.get_type_from_type_node_(released!(
                        type_
                            .ref_(self)
                            .as_conditional_type()
                            .root
                            .ref_(self)
                            .node
                            .ref_(self)
                            .as_conditional_type_node()
                            .true_type
                    ))?,
                    Some(type_combined_mapper),
                )?
            } else {
                self.get_true_type_from_conditional_type(type_)?
            };
            type_
                .ref_(self)
                .as_conditional_type()
                .set_resolved_inferred_true_type(Some(resolved_inferred_true_type));
        }
        Ok(type_
            .ref_(self)
            .as_conditional_type()
            .maybe_resolved_inferred_true_type()
            .unwrap())
    }

    pub(super) fn get_infer_type_parameters(
        &self,
        node: Id<Node>, /*ConditionalTypeNode*/
    ) -> io::Result<Option<Vec<Id<Type /*TypeParameter*/>>>> {
        let mut result: Option<Vec<Id<Type>>> = None;
        if let Some(node_locals) = node.ref_(self).maybe_locals().clone() {
            node_locals.ref_(self).values().try_for_each(
                |&symbol: &Id<Symbol>| -> io::Result<_> {
                    if symbol
                        .ref_(self)
                        .flags()
                        .intersects(SymbolFlags::TypeParameter)
                    {
                        if result.is_none() {
                            result = Some(vec![]);
                        }
                        append(
                            result.as_mut().unwrap(),
                            Some(self.get_declared_type_of_symbol(symbol)?),
                        );
                    }

                    Ok(())
                },
            )?;
        }
        Ok(result)
    }

    pub(super) fn is_distribution_dependent(&self, root: &ConditionalRoot) -> io::Result<bool> {
        Ok(root.is_distributive
            && (self.is_type_parameter_possibly_referenced(
                root.check_type,
                root.node.ref_(self).as_conditional_type_node().true_type,
            )? || self.is_type_parameter_possibly_referenced(
                root.check_type,
                root.node.ref_(self).as_conditional_type_node().false_type,
            )?))
    }

    pub(super) fn get_type_from_conditional_type_node(
        &self,
        node: Id<Node>, /*ConditionalTypeNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            let node_ref = node.ref_(self);
            let node_as_conditional_type_node = node_ref.as_conditional_type_node();
            let check_type =
                self.get_type_from_type_node_(node_as_conditional_type_node.check_type)?;
            let alias_symbol = self.get_alias_symbol_for_type_node(node)?;
            let alias_type_arguments = self.get_type_arguments_for_alias_symbol(alias_symbol)?;
            let all_outer_type_parameters = self.get_outer_type_parameters(node, Some(true))?;
            let outer_type_parameters = if alias_type_arguments.is_some() {
                all_outer_type_parameters
            } else {
                try_maybe_filter(all_outer_type_parameters.as_deref(), |&tp: &Id<Type>| {
                    self.is_type_parameter_possibly_referenced(tp, node)
                })
                .transpose()?
            };
            let root = self.alloc_conditional_root(ConditionalRoot::new(
                node,
                check_type.clone(),
                self.get_type_from_type_node_(node_as_conditional_type_node.extends_type)?,
                {
                    let intersects = check_type
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::TypeParameter);
                    intersects
                },
                self.get_infer_type_parameters(node)?,
                outer_type_parameters.clone(),
                alias_symbol,
                alias_type_arguments,
            ));
            let resolved_type =
                self.get_conditional_type(root.clone(), None, Option::<Id<Symbol>>::None, None)?;
            links.ref_mut(self).resolved_type = Some(resolved_type.clone());
            if let Some(outer_type_parameters) = outer_type_parameters {
                let mut instantiations: HashMap<String, Id<Type>> = HashMap::new();
                instantiations.insert(
                    self.get_type_list_id(Some(&outer_type_parameters)),
                    resolved_type,
                );
                *root.ref_mut(self).maybe_instantiations() = Some(instantiations);
            }
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_type_from_infer_type_node(
        &self,
        node: Id<Node>, /*InferTypeNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            links.ref_mut(self).resolved_type = Some(
                self.get_declared_type_of_type_parameter(
                    self.get_symbol_of_node(node.ref_(self).as_infer_type_node().type_parameter)?
                        .unwrap(),
                ),
            );
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_identifier_chain(
        &self,
        node: Id<Node>, /*EntityName*/
    ) -> Vec<Id<Node /*Identifier*/>> {
        if is_identifier(&node.ref_(self)) {
            vec![node]
        } else {
            let node_ref = node.ref_(self);
            let node_as_qualified_name = node_ref.as_qualified_name();
            let mut ret = self.get_identifier_chain(node_as_qualified_name.left);
            append(&mut ret, Some(node_as_qualified_name.right.clone()));
            ret
        }
    }

    pub(super) fn get_type_from_import_type_node(
        &self,
        node: Id<Node>, /*ImportTypeNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            if node.ref_(self).as_import_type_node().is_type_of()
                && node
                    .ref_(self)
                    .as_import_type_node()
                    .maybe_type_arguments()
                    .is_some()
            {
                self.error(
                    Some(node),
                    &Diagnostics::Type_arguments_cannot_be_used_here,
                    None,
                );
                let mut links = links.ref_mut(self);
                links.resolved_symbol = Some(self.unknown_symbol());
                let ret = self.error_type();
                links.resolved_type = Some(ret.clone());
                return Ok(ret);
            }
            if !is_literal_import_type_node(node, self) {
                self.error(
                    Some(node.ref_(self).as_import_type_node().argument),
                    &Diagnostics::String_literal_expected,
                    None,
                );
                let mut links = links.ref_mut(self);
                links.resolved_symbol = Some(self.unknown_symbol());
                let ret = self.error_type();
                links.resolved_type = Some(ret.clone());
                return Ok(ret);
            }
            let target_meaning = if node.ref_(self).as_import_type_node().is_type_of() {
                SymbolFlags::Value
            } else if node.ref_(self).flags().intersects(NodeFlags::JSDoc) {
                SymbolFlags::Value | SymbolFlags::Type
            } else {
                SymbolFlags::Type
            };
            let inner_module_symbol = self.resolve_external_module_name_(
                node,
                node.ref_(self)
                    .as_import_type_node()
                    .argument
                    .ref_(self)
                    .as_literal_type_node()
                    .literal,
                None,
            )?;
            if inner_module_symbol.is_none() {
                let mut links = links.ref_mut(self);
                links.resolved_symbol = Some(self.unknown_symbol());
                let ret = self.error_type();
                links.resolved_type = Some(ret.clone());
                return Ok(ret);
            }
            let inner_module_symbol = inner_module_symbol.unwrap();
            let module_symbol = self
                .resolve_external_module_symbol(Some(inner_module_symbol), Some(false))?
                .unwrap();
            if !node_is_missing(
                node.ref_(self)
                    .as_import_type_node()
                    .qualifier
                    .refed(self)
                    .as_deref(),
            ) {
                let mut name_stack: Vec<Id<Node /*Identifier*/>> = self
                    .get_identifier_chain(node.ref_(self).as_import_type_node().qualifier.unwrap());
                let mut current_namespace = module_symbol.clone();
                let mut current: Option<Id<Node /*Identifier*/>>;
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
                        .get_merged_symbol(self.resolve_symbol(Some(current_namespace), None)?)
                        .unwrap();
                    let next = if node.ref_(self).as_import_type_node().is_type_of() {
                        self.get_property_of_type_(
                            self.get_type_of_symbol(merged_resolved_symbol)?,
                            &current.ref_(self).as_identifier().escaped_text,
                            None,
                        )?
                    } else {
                        self.get_symbol(
                            self.get_exports_of_symbol(merged_resolved_symbol)?,
                            &current.ref_(self).as_identifier().escaped_text,
                            meaning,
                        )?
                    };
                    if next.is_none() {
                        self.error(
                            Some(current),
                            &Diagnostics::Namespace_0_has_no_exported_member_1,
                            Some(vec![
                                self.get_fully_qualified_name(
                                    current_namespace,
                                    Option::<Id<Node>>::None,
                                )?,
                                declaration_name_to_string(Some(current), self).into_owned(),
                            ]),
                        );
                        let ret = self.error_type();
                        links.ref_mut(self).resolved_type = Some(ret.clone());
                        return Ok(ret);
                    }
                    let next = next.unwrap();
                    self.get_node_links(current).ref_mut(self).resolved_symbol = Some(next.clone());
                    self.get_node_links(current.ref_(self).parent())
                        .ref_mut(self)
                        .resolved_symbol = Some(next.clone());
                    current_namespace = next;
                }
                links.ref_mut(self).resolved_type = Some(self.resolve_import_symbol_type(
                    node,
                    links,
                    current_namespace,
                    target_meaning,
                )?);
            } else {
                if module_symbol.ref_(self).flags().intersects(target_meaning) {
                    links.ref_mut(self).resolved_type = Some(self.resolve_import_symbol_type(
                        node,
                        links,
                        module_symbol,
                        target_meaning,
                    )?);
                } else {
                    let error_message = if target_meaning == SymbolFlags::Value {
                        &*Diagnostics::Module_0_does_not_refer_to_a_value_but_is_used_as_a_value_here
                    } else {
                        &*Diagnostics::Module_0_does_not_refer_to_a_type_but_is_used_as_a_type_here_Did_you_mean_typeof_import_0
                    };

                    self.error(
                        Some(node),
                        error_message,
                        Some(vec![node
                            .ref_(self)
                            .as_import_type_node()
                            .argument
                            .ref_(self)
                            .as_literal_type_node()
                            .literal
                            .ref_(self)
                            .as_literal_like_node()
                            .text()
                            .clone()]),
                    );

                    let mut links = links.ref_mut(self);
                    links.resolved_symbol = Some(self.unknown_symbol());
                    links.resolved_type = Some(self.error_type());
                }
            }
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn resolve_import_symbol_type(
        &self,
        node: Id<Node>, /*ImportTypeNode*/
        links: Id<NodeLinks>,
        symbol: Id<Symbol>,
        meaning: SymbolFlags,
    ) -> io::Result<Id<Type>> {
        let resolved_symbol = self.resolve_symbol(Some(symbol), None)?.unwrap();
        links.ref_mut(self).resolved_symbol = Some(resolved_symbol.clone());
        Ok(if meaning == SymbolFlags::Value {
            self.get_type_of_symbol(symbol)?
        } else {
            self.get_type_reference_type(node, resolved_symbol)?
        })
    }

    pub(super) fn get_type_from_type_literal_or_function_or_constructor_type_node(
        &self,
        node: Id<Node>, /*TypeNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            let alias_symbol = self.get_alias_symbol_for_type_node(node)?;
            if self
                .get_members_of_symbol(node.ref_(self).symbol())?
                .ref_(self)
                .is_empty()
                && alias_symbol.is_none()
            {
                links.ref_mut(self).resolved_type = Some(self.empty_type_literal_type());
            } else {
                let mut type_ = self.alloc_type(
                    self.create_object_type(ObjectFlags::Anonymous, node.ref_(self).maybe_symbol())
                        .into(),
                );
                type_.ref_(self).set_alias_symbol(alias_symbol);
                let alias_type_arguments =
                    self.get_type_arguments_for_alias_symbol(alias_symbol)?;
                type_
                    .ref_(self)
                    .set_alias_type_arguments(alias_type_arguments);
                if is_jsdoc_type_literal(&node.ref_(self))
                    && node.ref_(self).as_jsdoc_type_literal().is_array_type
                {
                    type_ = self.create_array_type(type_, None);
                }
                links.ref_mut(self).resolved_type = Some(type_);
            }
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_alias_symbol_for_type_node(
        &self,
        node: Id<Node>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let mut host = node.ref_(self).parent();
        while is_parenthesized_type_node(&host.ref_(self))
            || is_jsdoc_type_expression(&host.ref_(self))
            || is_type_operator_node(&host.ref_(self))
                && host.ref_(self).as_type_operator_node().operator == SyntaxKind::ReadonlyKeyword
        {
            host = host.ref_(self).parent();
        }
        Ok(if is_type_alias(&host.ref_(self)) {
            self.get_symbol_of_node(host)?
        } else {
            None
        })
    }

    pub(super) fn get_type_arguments_for_alias_symbol(
        &self,
        symbol: Option<Id<Symbol>>,
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        symbol.try_and_then(|symbol| {
            self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol)
        })
    }

    pub(super) fn is_non_generic_object_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(type_.ref_(self).flags().intersects(TypeFlags::Object)
            && !self.is_generic_mapped_type(type_)?)
    }

    pub(super) fn is_empty_object_type_or_spreads_into_empty_object(
        &self,
        type_: Id<Type>,
    ) -> io::Result<bool> {
        Ok(self.is_empty_object_type(type_)?
            || type_.ref_(self).flags().intersects(
                TypeFlags::Null
                    | TypeFlags::Undefined
                    | TypeFlags::BooleanLike
                    | TypeFlags::NumberLike
                    | TypeFlags::BigIntLike
                    | TypeFlags::StringLike
                    | TypeFlags::EnumLike
                    | TypeFlags::NonPrimitive
                    | TypeFlags::Index,
            ))
    }

    pub(super) fn try_merge_union_of_object_type_and_empty_object(
        &self,
        type_: Id<Type>,
        readonly: bool,
    ) -> io::Result<Id<Type>> {
        if !type_.ref_(self).flags().intersects(TypeFlags::Union) {
            return Ok(type_);
        }
        if try_every(
            type_.ref_(self).as_union_type().types(),
            |&type_: &Id<Type>, _| self.is_empty_object_type_or_spreads_into_empty_object(type_),
        )? {
            return Ok(try_find(
                type_.ref_(self).as_union_type().types(),
                |&type_: &Id<Type>, _| self.is_empty_object_type(type_),
            )?
            .cloned()
            .unwrap_or_else(|| self.empty_object_type()));
        }
        let first_type = try_find(
            type_.ref_(self).as_union_type().types(),
            |&type_: &Id<Type>, _| -> io::Result<_> {
                Ok(!self.is_empty_object_type_or_spreads_into_empty_object(type_)?)
            },
        )?
        .cloned();
        if first_type.is_none() {
            return Ok(type_);
        }
        let first_type = first_type.unwrap();
        let second_type = try_find(
            type_.ref_(self).as_union_type().types(),
            |&t: &Id<Type>, _| -> io::Result<_> {
                Ok(t != first_type
                    && !self.is_empty_object_type_or_spreads_into_empty_object(type_)?)
            },
        )?
        .cloned();
        if second_type.is_some() {
            return Ok(type_);
        }
        self.get_anonymous_partial_type(readonly, first_type)
    }

    pub(super) fn get_anonymous_partial_type(
        &self,
        readonly: bool,
        type_: Id<Type>,
    ) -> io::Result<Id<Type>> {
        let mut members = create_symbol_table(Option::<&[Id<Symbol>]>::None, self);
        for prop in self.get_properties_of_type(type_)? {
            if get_declaration_modifier_flags_from_symbol(prop, None, self)
                .intersects(ModifierFlags::Private | ModifierFlags::Protected)
            {
            } else if self.is_spreadable_property(prop) {
                let is_setonly_accessor =
                    prop.ref_(self).flags().intersects(SymbolFlags::SetAccessor)
                        && !prop.ref_(self).flags().intersects(SymbolFlags::GetAccessor);
                let flags = SymbolFlags::Property | SymbolFlags::Optional;
                let result = self.alloc_symbol(
                    self.create_symbol(
                        flags,
                        released!(prop.ref_(self).escaped_name().to_owned()),
                        Some(
                            self.get_is_late_check_flag(prop)
                                | if readonly {
                                    CheckFlags::Readonly
                                } else {
                                    CheckFlags::None
                                },
                        ),
                    )
                    .into(),
                );
                let result_links = result.ref_(self).as_transient_symbol().symbol_links();
                result_links.ref_mut(self).type_ = Some(if is_setonly_accessor {
                    self.undefined_type()
                } else {
                    self.add_optionality(self.get_type_of_symbol(prop)?, Some(true), None)?
                });
                let prop_ref = prop.ref_(self);
                let prop_declarations = prop_ref.maybe_declarations();
                if let Some(prop_declarations) = prop_declarations.as_ref() {
                    result
                        .ref_(self)
                        .set_declarations(prop_declarations.clone());
                }
                result_links.ref_mut(self).name_type =
                    released!(self.get_symbol_links(prop).ref_(self).name_type);
                result_links.ref_mut(self).synthetic_origin = Some(prop.clone());
                members.insert(prop.ref_(self).escaped_name().to_owned(), result);
            }
        }
        let spread = self.create_anonymous_type(
            released!(type_.ref_(self).maybe_symbol()),
            self.alloc_symbol_table(members),
            vec![],
            vec![],
            self.get_index_infos_of_type(type_)?,
        )?;
        spread.ref_(self).as_object_type().set_object_flags(
            spread.ref_(self).as_object_type().object_flags()
                | ObjectFlags::ObjectLiteral
                | ObjectFlags::ContainsObjectOrArrayLiteral,
        );
        Ok(spread)
    }

    pub(super) fn get_spread_type(
        &self,
        left: Id<Type>,
        right: Id<Type>,
        symbol: Option<Id<Symbol>>,
        object_flags: ObjectFlags,
        readonly: bool,
    ) -> io::Result<Id<Type>> {
        if left.ref_(self).flags().intersects(TypeFlags::Any)
            || right.ref_(self).flags().intersects(TypeFlags::Any)
        {
            return Ok(self.any_type());
        }
        if left.ref_(self).flags().intersects(TypeFlags::Unknown)
            || right.ref_(self).flags().intersects(TypeFlags::Unknown)
        {
            return Ok(self.unknown_type());
        }
        if left.ref_(self).flags().intersects(TypeFlags::Never) {
            return Ok(right);
        }
        if right.ref_(self).flags().intersects(TypeFlags::Never) {
            return Ok(left);
        }
        let left = self.try_merge_union_of_object_type_and_empty_object(left, readonly)?;
        if left.ref_(self).flags().intersects(TypeFlags::Union) {
            return Ok(if self.check_cross_product_union(&[left.clone(), right]) {
                self.try_map_type(
                    left,
                    &mut |t| {
                        Ok(Some(self.get_spread_type(
                            t,
                            right,
                            symbol,
                            object_flags,
                            readonly,
                        )?))
                    },
                    None,
                )?
                .unwrap()
            } else {
                self.error_type()
            });
        }
        let right = self.try_merge_union_of_object_type_and_empty_object(right, readonly)?;
        if right.ref_(self).flags().intersects(TypeFlags::Union) {
            return Ok(
                if self.check_cross_product_union(&[left.clone(), right.clone()]) {
                    self.try_map_type(
                        right,
                        &mut |t| {
                            Ok(Some(self.get_spread_type(
                                left,
                                t,
                                symbol,
                                object_flags,
                                readonly,
                            )?))
                        },
                        None,
                    )?
                    .unwrap()
                } else {
                    self.error_type()
                },
            );
        }
        if right.ref_(self).flags().intersects(
            TypeFlags::BooleanLike
                | TypeFlags::NumberLike
                | TypeFlags::BigIntLike
                | TypeFlags::StringLike
                | TypeFlags::EnumLike
                | TypeFlags::NonPrimitive
                | TypeFlags::Index,
        ) {
            return Ok(left);
        }

        if self.is_generic_object_type(left)? || self.is_generic_object_type(right)? {
            if self.is_empty_object_type(left)? {
                return Ok(right);
            }
            if left.ref_(self).flags().intersects(TypeFlags::Intersection) {
                let types = left.ref_(self).as_intersection_type().types().to_owned();
                let last_left = types[types.len() - 1];
                if self.is_non_generic_object_type(last_left)?
                    && self.is_non_generic_object_type(right)?
                {
                    return self.get_intersection_type(
                        &concatenate(
                            types[0..types.len() - 1].to_owned(),
                            vec![self.get_spread_type(
                                last_left,
                                right,
                                symbol,
                                object_flags,
                                readonly,
                            )?],
                        ),
                        Option::<Id<Symbol>>::None,
                        None,
                    );
                }
            }
            return self.get_intersection_type(
                &vec![left, right],
                Option::<Id<Symbol>>::None,
                None,
            );
        }

        let mut members = create_symbol_table(Option::<&[Id<Symbol>]>::None, self);
        let mut skipped_private_members: HashSet<__String> = HashSet::new();
        let index_infos = if left == self.empty_object_type() {
            self.get_index_infos_of_type(right)?
        } else {
            self.get_union_index_infos(&[left.clone(), right.clone()])?
        };

        for right_prop in self.get_properties_of_type(right)? {
            if get_declaration_modifier_flags_from_symbol(right_prop, None, self)
                .intersects(ModifierFlags::Private | ModifierFlags::Protected)
            {
                skipped_private_members.insert(right_prop.ref_(self).escaped_name().to_owned());
            } else if self.is_spreadable_property(right_prop) {
                members.insert(
                    released!(right_prop.ref_(self).escaped_name().to_owned()),
                    self.get_spread_symbol(right_prop, readonly)?,
                );
            }
        }

        for left_prop in self.get_properties_of_type(left)? {
            if skipped_private_members.contains(left_prop.ref_(self).escaped_name())
                || !self.is_spreadable_property(left_prop)
            {
                continue;
            }
            if members.contains_key(left_prop.ref_(self).escaped_name()) {
                let right_prop = *members.get(left_prop.ref_(self).escaped_name()).unwrap();
                let right_type = self.get_type_of_symbol(right_prop)?;
                if right_prop
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::Optional)
                {
                    let declarations = maybe_concatenate(
                        left_prop.ref_(self).maybe_declarations().clone(),
                        right_prop.ref_(self).maybe_declarations().clone(),
                    );
                    let flags = SymbolFlags::Property
                        | (left_prop.ref_(self).flags() & SymbolFlags::Optional);
                    let result = self.alloc_symbol(
                        self.create_symbol(
                            flags,
                            released!(left_prop.ref_(self).escaped_name().to_owned()),
                            None,
                        )
                        .into(),
                    );
                    let result_links = result.ref_(self).as_transient_symbol().symbol_links();
                    result_links.ref_mut(self).type_ = Some(self.get_union_type(
                        &[
                            self.get_type_of_symbol(left_prop)?,
                            self.remove_missing_or_undefined_type(right_type)?,
                        ],
                        None,
                        Option::<Id<Symbol>>::None,
                        None,
                        None,
                    )?);
                    result_links.ref_mut(self).left_spread = Some(left_prop.clone());
                    result_links.ref_mut(self).right_spread = Some(right_prop.clone());
                    if let Some(declarations) = declarations {
                        result.ref_(self).set_declarations(declarations);
                    }
                    result_links.ref_mut(self).name_type =
                        released!(self.get_symbol_links(left_prop).ref_(self).name_type);
                    members.insert(left_prop.ref_(self).escaped_name().to_owned(), result);
                }
            } else {
                members.insert(
                    released!(left_prop.ref_(self).escaped_name().to_owned()),
                    self.get_spread_symbol(left_prop, readonly)?,
                );
            }
        }

        let spread = self.create_anonymous_type(
            symbol,
            self.alloc_symbol_table(members),
            vec![],
            vec![],
            same_map(&index_infos, |&info: &Id<IndexInfo>, _| {
                self.get_index_info_with_readonly(info, readonly)
            }),
        )?;
        spread.ref_(self).as_object_type().set_object_flags(
            spread.ref_(self).as_object_type().object_flags()
                | ObjectFlags::ObjectLiteral
                | ObjectFlags::ContainsObjectOrArrayLiteral
                | ObjectFlags::ContainsSpread
                | object_flags,
        );
        Ok(spread)
    }
}
