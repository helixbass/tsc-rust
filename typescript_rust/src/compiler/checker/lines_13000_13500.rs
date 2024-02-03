use std::{
    borrow::Cow,
    convert::{TryFrom, TryInto},
    io, ptr,
};

use id_arena::Id;

use super::{anon, get_symbol_id, intrinsic_type_kinds};
use crate::{
    append, concatenate, declaration_name_to_string, get_check_flags, get_containing_function,
    get_declaration_of_kind, get_effective_container_for_jsdoc_template_tag, get_object_flags,
    index_of_eq, is_entity_name_expression, is_expression_with_type_arguments, is_identifier,
    is_in_js_file, is_jsdoc_augments_tag, is_jsdoc_index_signature, is_jsdoc_template_tag,
    is_statement, is_type_alias, length, maybe_concatenate, skip_parentheses, try_map,
    walk_up_parenthesized_types_and_get_parent_and_child, AsDoubleDeref, BaseObjectType,
    CheckFlags, Diagnostics, GetOrInsertDefault, HasArena, HasTypeArgumentsInterface, InArena,
    InterfaceTypeInterface, Node, NodeFlags, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface,
    OptionInArena, OptionTry, SubstitutionType, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeFormatFlags, TypeId, TypeInterface,
    TypeMapper, TypeReference, TypeReferenceInterface, TypeSystemPropertyName,
};

impl TypeChecker {
    pub(super) fn get_inferred_type_parameter_constraint(
        &self,
        type_parameter: Id<Type>, /*TypeParameter*/
    ) -> io::Result<Option<Id<Type>>> {
        let mut inferences: Option<Vec<Id<Type>>> = None;
        if let Some(type_parameter_symbol) = type_parameter.ref_(self).maybe_symbol() {
            if let Some(type_parameter_symbol_declarations) = type_parameter_symbol
                .ref_(self)
                .maybe_declarations()
                .as_deref()
            {
                for declaration in type_parameter_symbol_declarations {
                    if declaration.ref_(self).parent().ref_(self).kind() == SyntaxKind::InferType {
                        let (child_type_parameter, grand_parent) =
                            walk_up_parenthesized_types_and_get_parent_and_child(
                                declaration.ref_(self).parent().ref_(self).parent(),
                                self,
                            );
                        let grand_parent = grand_parent.unwrap();
                        let child_type_parameter =
                            child_type_parameter.unwrap_or_else(|| declaration.ref_(self).parent());
                        if grand_parent.ref_(self).kind() == SyntaxKind::TypeReference {
                            let type_reference = grand_parent;
                            let type_parameters =
                                self.get_type_parameters_for_type_reference(type_reference)?;
                            if let Some(type_parameters) = type_parameters {
                                let index: usize = index_of_eq(
                                    &type_reference
                                        .ref_(self)
                                        .as_type_reference_node()
                                        .maybe_type_arguments()
                                        .unwrap()
                                        .ref_(self),
                                    &child_type_parameter,
                                )
                                .try_into()
                                .unwrap();
                                if index < type_parameters.len() {
                                    let declared_constraint = self
                                        .get_constraint_of_type_parameter(type_parameters[index])?;
                                    if let Some(declared_constraint) = declared_constraint {
                                        let mapper = self.create_type_mapper(
                                            type_parameters.clone(),
                                            Some(self.get_effective_type_arguments(
                                                type_reference,
                                                Some(&type_parameters),
                                            )?),
                                        );
                                        let constraint = self
                                            .instantiate_type(declared_constraint, Some(mapper))?;
                                        if constraint != type_parameter {
                                            if inferences.is_none() {
                                                inferences = Some(vec![]);
                                            }
                                            append(inferences.as_mut().unwrap(), Some(constraint));
                                        }
                                    }
                                }
                            }
                        } else if grand_parent.ref_(self).kind() == SyntaxKind::Parameter
                            && grand_parent
                                .ref_(self)
                                .as_parameter_declaration()
                                .dot_dot_dot_token
                                .is_some()
                            || grand_parent.ref_(self).kind() == SyntaxKind::RestType
                            || grand_parent.ref_(self).kind() == SyntaxKind::NamedTupleMember
                                && grand_parent
                                    .ref_(self)
                                    .as_named_tuple_member()
                                    .dot_dot_dot_token
                                    .is_some()
                        {
                            if inferences.is_none() {
                                inferences = Some(vec![]);
                            }
                            append(
                                inferences.as_mut().unwrap(),
                                Some(self.create_array_type(self.unknown_type(), None)),
                            );
                        } else if grand_parent.ref_(self).kind()
                            == SyntaxKind::TemplateLiteralTypeSpan
                        {
                            if inferences.is_none() {
                                inferences = Some(vec![]);
                            }
                            append(inferences.as_mut().unwrap(), Some(self.string_type()));
                        } else if grand_parent.ref_(self).kind() == SyntaxKind::TypeParameter
                            && grand_parent.ref_(self).parent().ref_(self).kind()
                                == SyntaxKind::MappedType
                        {
                            if inferences.is_none() {
                                inferences = Some(vec![]);
                            }
                            append(
                                inferences.as_mut().unwrap(),
                                Some(self.keyof_constraint_type()),
                            );
                        } else if grand_parent.ref_(self).kind() == SyntaxKind::MappedType && {
                            let grand_parent_ref = grand_parent.ref_(self);
                            let grand_parent_as_mapped_type_node =
                                grand_parent_ref.as_mapped_type_node();
                            grand_parent_as_mapped_type_node.type_.is_some()
                                && skip_parentheses(
                                    grand_parent_as_mapped_type_node.type_.unwrap(),
                                    None,
                                    self,
                                ) == declaration.ref_(self).parent()
                                && grand_parent.ref_(self).parent().ref_(self).kind()
                                    == SyntaxKind::ConditionalType
                                && {
                                    let grand_parent_parent = grand_parent.ref_(self).parent();
                                    let grand_parent_parent_ref = grand_parent_parent.ref_(self);
                                    let grand_parent_parent_as_conditional_type_node =
                                        grand_parent_parent_ref.as_conditional_type_node();
                                    grand_parent_parent_as_conditional_type_node.extends_type
                                        == grand_parent
                                        && grand_parent_parent_as_conditional_type_node
                                            .check_type
                                            .ref_(self)
                                            .kind()
                                            == SyntaxKind::MappedType
                                        && grand_parent_parent_as_conditional_type_node
                                            .check_type
                                            .ref_(self)
                                            .as_mapped_type_node()
                                            .type_
                                            .is_some()
                                }
                        } {
                            let check_mapped_type = grand_parent
                                .ref_(self)
                                .parent()
                                .ref_(self)
                                .as_conditional_type_node()
                                .check_type;
                            let check_mapped_type_ref = check_mapped_type.ref_(self);
                            let check_mapped_type = check_mapped_type_ref.as_mapped_type_node();
                            let node_type =
                                self.get_type_from_type_node_(check_mapped_type.type_.unwrap())?;
                            append(
                                inferences.get_or_insert_default_(),
                                Some(
                                    self.instantiate_type(
                                        node_type,
                                        Some(
                                            self.make_unary_type_mapper(
                                                self.get_declared_type_of_type_parameter(
                                                    self.get_symbol_of_node(
                                                        check_mapped_type.type_parameter,
                                                    )?
                                                    .unwrap(),
                                                ),
                                                if let Some(
                                                    check_mapped_type_type_parameter_constraint,
                                                ) = check_mapped_type
                                                    .type_parameter
                                                    .ref_(self)
                                                    .as_type_parameter_declaration()
                                                    .constraint
                                                {
                                                    self.get_type_from_type_node_(
                                                        check_mapped_type_type_parameter_constraint,
                                                    )?
                                                } else {
                                                    self.keyof_constraint_type()
                                                },
                                            ),
                                        ),
                                    )?,
                                ),
                            );
                        }
                    }
                }
            }
        }
        inferences.try_map(|inferences| {
            self.get_intersection_type(&inferences, Option::<Id<Symbol>>::None, None)
        })
    }

    pub(super) fn get_constraint_from_type_parameter(
        &self,
        type_parameter: Id<Type>, /*TypeParameter*/
    ) -> io::Result<Option<Id<Type>>> {
        if type_parameter
            .ref_(self)
            .as_type_parameter()
            .maybe_constraint()
            .is_none()
        {
            if let Some(type_parameter_target) = {
                let type_parameter_target = type_parameter.ref_(self).as_type_parameter().target;
                type_parameter_target
            } {
                let target_constraint =
                    self.get_constraint_of_type_parameter(type_parameter_target)?;
                let constraint = match target_constraint {
                    Some(target_constraint) => self.instantiate_type(target_constraint, {
                        let mapper = type_parameter.ref_(self).as_type_parameter().maybe_mapper();
                        mapper
                    })?,
                    None => self.no_constraint_type(),
                };
                type_parameter
                    .ref_(self)
                    .as_type_parameter()
                    .set_constraint(Some(constraint));
            } else {
                let constraint_declaration = self.get_constraint_declaration(type_parameter);
                match constraint_declaration {
                    None => {
                        type_parameter
                            .ref_(self)
                            .as_type_parameter()
                            .set_constraint(Some(
                                self.get_inferred_type_parameter_constraint(type_parameter)?
                                    .unwrap_or_else(|| self.no_constraint_type()),
                            ));
                    }
                    Some(constraint_declaration) => {
                        let mut type_ = self.get_type_from_type_node_(constraint_declaration)?;
                        if type_.ref_(self).flags().intersects(TypeFlags::Any)
                            && !self.is_error_type(type_)
                        {
                            type_ = if constraint_declaration
                                .ref_(self)
                                .parent()
                                .ref_(self)
                                .parent()
                                .ref_(self)
                                .kind()
                                == SyntaxKind::MappedType
                            {
                                self.keyof_constraint_type()
                            } else {
                                self.unknown_type()
                            };
                        }
                        type_parameter
                            .ref_(self)
                            .as_type_parameter()
                            .set_constraint(Some(type_));
                    }
                }
            }
        }
        Ok(type_parameter
            .ref_(self)
            .as_type_parameter()
            .maybe_constraint()
            .and_then(|type_parameter_constraint| {
                if type_parameter_constraint == self.no_constraint_type() {
                    None
                } else {
                    Some(type_parameter_constraint)
                }
            }))
    }

    pub(super) fn get_parent_symbol_of_type_parameter(
        &self,
        type_parameter: Id<Type>, /*TypeParameter*/
    ) -> io::Result<Option<Id<Symbol>>> {
        let tp = get_declaration_of_kind(
            type_parameter.ref_(self).symbol(),
            SyntaxKind::TypeParameter,
            self,
        )
        .unwrap();
        let host = if is_jsdoc_template_tag(&tp.ref_(self).parent().ref_(self)) {
            get_effective_container_for_jsdoc_template_tag(tp.ref_(self).parent(), self)
        } else {
            Some(tp.ref_(self).parent())
        };
        host.try_and_then(|host| self.get_symbol_of_node(host))
    }

    pub(super) fn get_type_list_id(&self, types: Option<&[Id<Type>]>) -> String {
        let mut result = "".to_owned();
        if let Some(types) = types {
            let length = types.len();
            let mut i = 0;
            while i < length {
                let start_id = types[i].ref_(self).id();
                let mut count = 1;
                while i + count < length
                    && types[i + count].ref_(self).id()
                        == start_id + TypeId::try_from(count).unwrap()
                {
                    count += 1;
                }
                if !result.is_empty() {
                    result.push_str(",");
                }
                result.push_str(&format!("{}", start_id));
                if count > 1 {
                    result.push_str(&format!(":{}", count));
                }
                i += count;
            }
        }
        result
    }

    pub(super) fn get_alias_id(
        &self,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> String {
        if let Some(alias_symbol) = alias_symbol {
            format!(
                "@{}{}",
                get_symbol_id(&alias_symbol.ref_(self)),
                if let Some(alias_type_arguments) = alias_type_arguments {
                    format!(":{}", self.get_type_list_id(Some(alias_type_arguments)))
                } else {
                    "".to_owned()
                }
            )
        } else {
            "".to_owned()
        }
    }

    pub(super) fn get_propagating_flags_of_types(
        &self,
        types: &[Id<Type>],
        exclude_kinds: TypeFlags,
    ) -> ObjectFlags {
        let mut result = ObjectFlags::None;
        for &type_ in types {
            if !type_.ref_(self).flags().intersects(exclude_kinds) {
                result |= get_object_flags(&type_.ref_(self));
            }
        }
        result & ObjectFlags::PropagatingFlags
    }

    pub(super) fn create_type_reference(
        &self,
        target: Id<Type>, /*GenericType*/
        type_arguments: Option<Vec<Id<Type>>>,
    ) -> Id<Type /*TypeReference*/> {
        let id = self.get_type_list_id(type_arguments.as_deref());
        let type_ = target
            .ref_(self)
            .as_generic_type()
            .instantiations()
            .get(&id)
            .map(Clone::clone);
        if type_.is_none() {
            let type_ =
                self.create_object_type(ObjectFlags::Reference, target.ref_(self).maybe_symbol());
            type_.set_object_flags(
                type_.object_flags()
                    | if let Some(type_arguments) = type_arguments.as_ref() {
                        self.get_propagating_flags_of_types(type_arguments, TypeFlags::None)
                    } else {
                        ObjectFlags::None
                    },
            );
            let type_ = self.alloc_type(TypeReference::new(type_, target, type_arguments).into());
            target
                .ref_(self)
                .as_generic_type()
                .instantiations()
                .insert(id, type_.clone());
            return type_;
        }
        type_.unwrap()
    }

    pub(super) fn clone_type_reference(&self, source: Id<Type> /*TypeReference*/) -> Id<Type> {
        let type_ = self.create_type(source.ref_(self).flags());
        type_.set_symbol(source.ref_(self).maybe_symbol());
        let type_ = BaseObjectType::new(
            type_,
            source
                .ref_(self)
                .as_type_reference_interface()
                .object_flags(),
        );
        let type_ = self.alloc_type(
            TypeReference::new(
                type_,
                {
                    let target = source.ref_(self).as_type_reference_interface().target();
                    target
                },
                {
                    let resolved_type_arguments = source
                        .ref_(self)
                        .as_type_reference_interface()
                        .maybe_resolved_type_arguments()
                        .clone();
                    resolved_type_arguments
                },
            )
            .into(),
        );
        type_
    }

    pub(super) fn create_deferred_type_reference(
        &self,
        target: Id<Type>, /*GenericType*/
        node: Id<Node>,   /*TypeReferenceNode | ArrayTypeNode | TupleTypeNode*/
        mapper: Option<Id<TypeMapper>>,
        mut alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type /*DeferredTypeReference*/>> {
        let mut alias_type_arguments = alias_type_arguments.map(ToOwned::to_owned);
        if alias_symbol.is_none() {
            alias_symbol = self.get_alias_symbol_for_type_node(node)?;
            let local_alias_type_arguments =
                self.get_type_arguments_for_alias_symbol(alias_symbol)?;
            alias_type_arguments = if let Some(mapper) = mapper.as_ref() {
                self.instantiate_types(local_alias_type_arguments.as_deref(), Some(mapper.clone()))?
            } else {
                local_alias_type_arguments
            };
        }
        let mut type_ =
            self.create_object_type(ObjectFlags::Reference, target.ref_(self).maybe_symbol());
        type_.mapper = mapper;
        let type_ = self.alloc_type(TypeReference::new(type_, target, None).into());
        type_.ref_(self).as_type_reference().set_node(Some(node));
        type_.ref_(self).set_alias_symbol(alias_symbol);
        type_
            .ref_(self)
            .set_alias_type_arguments(alias_type_arguments);
        Ok(type_)
    }

    pub(super) fn get_type_arguments(
        &self,
        type_: Id<Type>, /*TypeReference*/
    ) -> io::Result<Vec<Id<Type>>> {
        if type_
            .ref_(self)
            .as_type_reference_interface()
            .maybe_resolved_type_arguments()
            .is_none()
        {
            if !self
                .push_type_resolution(&type_.into(), TypeSystemPropertyName::ResolvedTypeArguments)
            {
                return Ok(type_
                    .ref_(self)
                    .as_type_reference_interface()
                    .target()
                    .ref_(self)
                    .as_interface_type()
                    .maybe_local_type_parameters()
                    .map_or_else(
                        || vec![],
                        |local_type_parameters| {
                            local_type_parameters
                                .iter()
                                .map(|_| self.error_type())
                                .collect()
                        },
                    ));
            }
            let node = type_
                .ref_(self)
                .as_type_reference_interface()
                .maybe_node()
                .clone();
            let type_arguments: Vec<Id<Type>> = if node.is_none() {
                vec![]
            } else {
                let node = node.unwrap();
                if node.ref_(self).kind() == SyntaxKind::TypeReference {
                    let target = type_.ref_(self).as_type_reference_interface().target();
                    concatenate(
                        {
                            let outer_type_parameters = target
                                .ref_(self)
                                .as_interface_type()
                                .maybe_outer_type_parameters()
                                .map_or_else(|| vec![], ToOwned::to_owned);
                            outer_type_parameters
                        },
                        self.get_effective_type_arguments(
                            node,
                            {
                                let local_type_parameters = target
                                    .ref_(self)
                                    .as_interface_type()
                                    .maybe_local_type_parameters()
                                    .map(ToOwned::to_owned);
                                local_type_parameters
                            }
                            .as_deref(),
                        )?,
                    )
                } else if node.ref_(self).kind() == SyntaxKind::ArrayType {
                    vec![self.get_type_from_type_node_(
                        node.ref_(self).as_array_type_node().element_type,
                    )?]
                } else {
                    try_map(
                        &*node.ref_(self).as_tuple_type_node().elements.ref_(self),
                        |&element: &Id<Node>, _| self.get_type_from_type_node_(element),
                    )?
                }
            };
            if self.pop_type_resolution() {
                *type_
                    .ref_(self)
                    .as_type_reference_interface()
                    .maybe_resolved_type_arguments_mut() = if let Some(type_mapper) = {
                    let mapper = type_
                        .ref_(self)
                        .as_type_reference_interface()
                        .maybe_mapper();
                    mapper
                } {
                    self.instantiate_types(Some(&type_arguments), Some(type_mapper))?
                } else {
                    Some(type_arguments)
                };
            } else {
                *type_
                    .ref_(self)
                    .as_type_reference_interface()
                    .maybe_resolved_type_arguments_mut() = Some(
                    type_
                        .ref_(self)
                        .as_type_reference_interface()
                        .target()
                        .ref_(self)
                        .as_interface_type()
                        .maybe_local_type_parameters()
                        .map_or_else(
                            || vec![],
                            |local_type_parameters| {
                                local_type_parameters
                                    .iter()
                                    .map(|_| self.error_type())
                                    .collect()
                            },
                        ),
                );
                self.error(
                    type_
                        .ref_(self)
                        .as_type_reference_interface()
                        .maybe_node()
                        .clone()
                        .or_else(|| self.maybe_current_node()),
                    if type_
                        .ref_(self)
                        .as_type_reference_interface()
                        .target()
                        .ref_(self)
                        .maybe_symbol()
                        .is_some()
                    {
                        &Diagnostics::Type_arguments_for_0_circularly_reference_themselves
                    } else {
                        &Diagnostics::Tuple_type_arguments_circularly_reference_themselves
                    },
                    if let Some(type_target_symbol) = type_
                        .ref_(self)
                        .as_type_reference_interface()
                        .target()
                        .ref_(self)
                        .maybe_symbol()
                    {
                        Some(vec![self.symbol_to_string_(
                            type_target_symbol,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                            None,
                        )?])
                    } else {
                        None
                    },
                );
            }
        }
        Ok(type_
            .ref_(self)
            .as_type_reference_interface()
            .maybe_resolved_type_arguments()
            .clone()
            .unwrap())
    }

    pub(super) fn get_type_reference_arity(&self, type_: Id<Type> /*TypeReference*/) -> usize {
        length(
            type_
                .ref_(self)
                .as_type_reference_interface()
                .target()
                .ref_(self)
                .as_interface_type()
                .maybe_type_parameters(),
        )
    }

    pub(super) fn get_type_from_class_or_interface_reference(
        &self,
        node: Id<Node>, /*NodeWithTypeArguments*/
        symbol: Id<Symbol>,
    ) -> io::Result<Id<Type>> {
        let type_ =
            self.get_declared_type_of_symbol(self.get_merged_symbol(Some(symbol)).unwrap())?;
        let type_parameters = type_
            .ref_(self)
            .as_interface_type()
            .maybe_local_type_parameters()
            .map(ToOwned::to_owned);
        if let Some(type_parameters) = type_parameters.as_deref() {
            let num_type_arguments = length(
                node.ref_(self)
                    .as_has_type_arguments()
                    .maybe_type_arguments()
                    .refed(self)
                    .as_double_deref(),
            );
            let min_type_argument_count = self.get_min_type_argument_count(Some(type_parameters));
            let is_js = is_in_js_file(Some(&node.ref_(self)));
            let is_js_implicit_any = !self.no_implicit_any && is_js;
            if !is_js_implicit_any
                && (num_type_arguments < min_type_argument_count
                    || num_type_arguments > type_parameters.len())
            {
                let missing_augments_tag = is_js
                    && is_expression_with_type_arguments(&node.ref_(self))
                    && !is_jsdoc_augments_tag(&node.ref_(self).parent().ref_(self));
                let diag = if min_type_argument_count == type_parameters.len() {
                    if missing_augments_tag {
                        &*Diagnostics::Expected_0_type_arguments_provide_these_with_an_extends_tag
                    } else {
                        &*Diagnostics::Generic_type_0_requires_1_type_argument_s
                    }
                } else {
                    if missing_augments_tag {
                        &*Diagnostics::Expected_0_1_type_arguments_provide_these_with_an_extends_tag
                    } else {
                        &*Diagnostics::Generic_type_0_requires_between_1_and_2_type_arguments
                    }
                };

                let type_str = self.type_to_string_(
                    type_,
                    Option::<Id<Node>>::None,
                    Some(TypeFormatFlags::WriteArrayAsGenericType),
                    None,
                )?;
                self.error(
                    Some(node),
                    diag,
                    Some(vec![
                        type_str,
                        min_type_argument_count.to_string(),
                        type_parameters.len().to_string(),
                    ]),
                );
                if !is_js {
                    return Ok(self.error_type());
                }
            }
            if node.ref_(self).kind() == SyntaxKind::TypeReference
                && self.is_deferred_type_reference_node(
                    node,
                    Some(
                        length(
                            node.ref_(self)
                                .as_has_type_arguments()
                                .maybe_type_arguments()
                                .refed(self)
                                .as_double_deref(),
                        ) != type_parameters.len(),
                    ),
                )?
            {
                return self.create_deferred_type_reference(
                    type_,
                    node,
                    None,
                    Option::<Id<Symbol>>::None,
                    None,
                );
            }
            let type_arguments = maybe_concatenate(
                {
                    let outer_type_parameters = type_
                        .ref_(self)
                        .as_interface_type()
                        .maybe_outer_type_parameters()
                        .map(ToOwned::to_owned);
                    outer_type_parameters
                },
                self.fill_missing_type_arguments(
                    self.type_arguments_from_type_reference_node(node)?,
                    Some(type_parameters),
                    min_type_argument_count,
                    is_js,
                )?,
            );
            return Ok(self.create_type_reference(type_, type_arguments));
        }
        Ok(if self.check_no_type_arguments(node, Some(symbol))? {
            type_
        } else {
            self.error_type()
        })
    }

    pub(super) fn get_type_alias_instantiation(
        &self,
        symbol: Id<Symbol>,
        type_arguments: Option<&[Id<Type>]>,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type>> {
        let type_ = self.get_declared_type_of_symbol(symbol)?;
        if type_ == self.intrinsic_marker_type()
            && intrinsic_type_kinds.contains_key(symbol.ref_(self).escaped_name())
            && matches!(type_arguments, Some(type_arguments) if type_arguments.len() == 1)
        {
            return self.get_string_mapping_type(symbol, type_arguments.unwrap()[0]);
        }
        let links = self.get_symbol_links(symbol);
        let type_parameters = (*links.ref_(self))
            .borrow()
            .type_parameters
            .clone()
            .unwrap();
        let id = format!(
            "{}{}",
            self.get_type_list_id(type_arguments),
            self.get_alias_id(alias_symbol, alias_type_arguments)
        );
        let mut instantiation = (*links.ref_(self))
            .borrow()
            .instantiations
            .as_ref()
            .unwrap()
            .get(&id)
            .map(Clone::clone);
        if instantiation.is_none() {
            instantiation = Some(
                self.instantiate_type_with_alias(
                    type_,
                    self.create_type_mapper(
                        type_parameters.clone(),
                        self.fill_missing_type_arguments(
                            type_arguments.map(ToOwned::to_owned),
                            Some(&type_parameters),
                            self.get_min_type_argument_count(Some(&type_parameters)),
                            is_in_js_file(
                                symbol
                                    .ref_(self)
                                    .maybe_value_declaration()
                                    .refed(self)
                                    .as_deref(),
                            ),
                        )?,
                    ),
                    alias_symbol,
                    alias_type_arguments,
                )?,
            );
            links
                .ref_mut(self)
                .instantiations
                .as_mut()
                .unwrap()
                .insert(id, instantiation.clone().unwrap());
        }
        Ok(instantiation.unwrap())
    }

    pub(super) fn get_type_from_type_alias_reference(
        &self,
        node: Id<Node>, /*NodeWithTypeArguments*/
        symbol: Id<Symbol>,
    ) -> io::Result<Id<Type>> {
        if get_check_flags(&symbol.ref_(self)).intersects(CheckFlags::Unresolved) {
            let type_arguments = self.type_arguments_from_type_reference_node(node)?;
            let id = self.get_alias_id(Some(symbol), type_arguments.as_deref());
            let mut error_type = self.error_types().get(&id).map(Clone::clone);
            if error_type.is_none() {
                error_type = Some(
                    self.alloc_type(
                        self.create_intrinsic_type(TypeFlags::Any, "error", None)
                            .into(),
                    ),
                );
                let error_type = error_type.unwrap();
                error_type.ref_(self).set_alias_symbol(Some(symbol));
                error_type
                    .ref_(self)
                    .set_alias_type_arguments(type_arguments);
                self.error_types().insert(id, error_type.clone());
            }
            return Ok(error_type.unwrap());
        }
        let type_ = self.get_declared_type_of_symbol(symbol)?;
        let type_parameters = (*self.get_symbol_links(symbol).ref_(self))
            .borrow()
            .type_parameters
            .clone();
        if let Some(type_parameters) = type_parameters {
            let num_type_arguments = length(
                node.ref_(self)
                    .as_has_type_arguments()
                    .maybe_type_arguments()
                    .refed(self)
                    .as_double_deref(),
            );
            let min_type_argument_count = self.get_min_type_argument_count(Some(&type_parameters));
            if num_type_arguments < min_type_argument_count
                || num_type_arguments > type_parameters.len()
            {
                self.error(
                    Some(node),
                    if min_type_argument_count == type_parameters.len() {
                        &Diagnostics::Generic_type_0_requires_1_type_argument_s
                    } else {
                        &Diagnostics::Generic_type_0_requires_between_1_and_2_type_arguments
                    },
                    Some(vec![
                        self.symbol_to_string_(symbol, Option::<Id<Node>>::None, None, None, None)?,
                        min_type_argument_count.to_string(),
                        type_parameters.len().to_string(),
                    ]),
                );
                return Ok(self.error_type());
            }
            let alias_symbol = self.get_alias_symbol_for_type_node(node)?;
            let new_alias_symbol = alias_symbol.filter(|&alias_symbol| {
                self.is_local_type_alias(symbol) || !self.is_local_type_alias(alias_symbol)
            });
            return self.get_type_alias_instantiation(
                symbol,
                self.type_arguments_from_type_reference_node(node)?
                    .as_deref(),
                new_alias_symbol,
                self.get_type_arguments_for_alias_symbol(new_alias_symbol)?
                    .as_deref(),
            );
        }
        Ok(if self.check_no_type_arguments(node, Some(symbol))? {
            type_
        } else {
            self.error_type()
        })
    }

    pub(super) fn is_local_type_alias(&self, symbol: Id<Symbol>) -> bool {
        let symbol_ref = symbol.ref_(self);
        let symbol_declarations = symbol_ref.maybe_declarations();
        let declaration = symbol_declarations.as_ref().and_then(|declarations| {
            declarations
                .iter()
                .find(|declaration: &&Id<Node>| is_type_alias(&declaration.ref_(self)))
                .map(Clone::clone)
        });
        declaration
            .and_then(|declaration| get_containing_function(declaration, self))
            .is_some()
    }

    pub(super) fn get_type_reference_name(
        &self,
        node: Id<Node>, /*TypeReferenceType*/
    ) -> Option<Id<Node /*EntityNameOrEntityNameExpression*/>> {
        match node.ref_(self).kind() {
            SyntaxKind::TypeReference => {
                return Some(node.ref_(self).as_type_reference_node().type_name);
            }
            SyntaxKind::ExpressionWithTypeArguments => {
                let expr = node
                    .ref_(self)
                    .as_expression_with_type_arguments()
                    .expression;
                if is_entity_name_expression(expr, self) {
                    return Some(expr);
                }
            }
            _ => (),
        }
        None
    }

    pub(super) fn get_symbol_path(&self, symbol: Id<Symbol>) -> String {
        match symbol.ref_(self).maybe_parent() {
            Some(symbol_parent) => format!(
                "{}.{}",
                self.get_symbol_path(symbol_parent),
                symbol.ref_(self).escaped_name()
            ),
            None => symbol.ref_(self).escaped_name().to_owned(),
        }
    }

    pub(super) fn get_unresolved_symbol_for_entity_name(
        &self,
        name: Id<Node>, /*EntityNameOrEntityNameExpression*/
    ) -> Id<Symbol /*TransientSymbol*/> {
        let identifier = if name.ref_(self).kind() == SyntaxKind::QualifiedName {
            name.ref_(self).as_qualified_name().right
        } else if name.ref_(self).kind() == SyntaxKind::PropertyAccessExpression {
            name.ref_(self).as_property_access_expression().name
        } else {
            name
        };
        let identifier_ref = identifier.ref_(self);
        let text = &identifier_ref.as_identifier().escaped_text;
        if &**text != "" {
            let parent_symbol = if name.ref_(self).kind() == SyntaxKind::QualifiedName {
                Some(self.get_unresolved_symbol_for_entity_name(
                    name.ref_(self).as_qualified_name().left,
                ))
            } else if name.ref_(self).kind() == SyntaxKind::PropertyAccessExpression {
                Some(self.get_unresolved_symbol_for_entity_name(
                    name.ref_(self).as_property_access_expression().expression,
                ))
            } else {
                None
            };
            let path = if let Some(parent_symbol) = parent_symbol {
                format!("{}.{}", self.get_symbol_path(parent_symbol), &**text)
            } else {
                (&**text).to_owned()
            };
            let mut result = self.unresolved_symbols().get(&path).map(Clone::clone);
            if result.is_none() {
                result = Some(
                    self.alloc_symbol(
                        self.create_symbol(
                            SymbolFlags::TypeAlias,
                            text.clone(),
                            Some(CheckFlags::Unresolved),
                        )
                        .into(),
                    ),
                );
                let result = result.unwrap();
                self.unresolved_symbols().insert(path, result.clone());
                result.ref_(self).set_parent(parent_symbol);
                result
                    .ref_(self)
                    .as_transient_symbol()
                    .symbol_links()
                    .ref_mut(self)
                    .declared_type = Some(self.unresolved_type());
            }
            return result.unwrap();
        }
        return self.unknown_symbol();
    }

    pub(super) fn resolve_type_reference_name(
        &self,
        type_reference: Id<Node>, /*TypeReferenceType*/
        meaning: SymbolFlags,
        ignore_errors: Option<bool>,
    ) -> io::Result<Id<Symbol>> {
        let ignore_errors = ignore_errors.unwrap_or(false);
        let name = self.get_type_reference_name(type_reference);
        let name = match name {
            Some(name) => name,
            None => {
                return Ok(self.unknown_symbol());
            }
        };
        let symbol = self.resolve_entity_name(
            name,
            meaning,
            Some(ignore_errors),
            None,
            Option::<Id<Node>>::None,
        )?;
        Ok(
            if symbol.is_some() && symbol.unwrap() != self.unknown_symbol() {
                symbol.unwrap()
            } else if ignore_errors {
                self.unknown_symbol()
            } else {
                self.get_unresolved_symbol_for_entity_name(name)
            },
        )
    }

    pub(super) fn get_type_reference_type(
        &self,
        node: Id<Node>,
        symbol: Id<Symbol>,
    ) -> io::Result<Id<Type>> {
        if symbol == self.unknown_symbol() {
            return Ok(self.error_type());
        }
        let symbol = self.get_expando_symbol(symbol)?.unwrap_or_else(|| symbol);
        if symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return self.get_type_from_class_or_interface_reference(node, symbol);
        }
        if symbol.ref_(self).flags().intersects(SymbolFlags::TypeAlias) {
            return self.get_type_from_type_alias_reference(node, symbol);
        }
        let res = self.try_get_declared_type_of_symbol(symbol)?;
        if let Some(res) = res {
            return Ok(if self.check_no_type_arguments(node, Some(symbol))? {
                self.get_regular_type_of_literal_type(res)
            } else {
                self.error_type()
            });
        }
        if symbol.ref_(self).flags().intersects(SymbolFlags::Value)
            && self.is_jsdoc_type_reference(node)
        {
            let jsdoc_type = self.get_type_from_jsdoc_value_reference(node, symbol)?;
            if let Some(jsdoc_type) = jsdoc_type {
                return Ok(jsdoc_type);
            } else {
                self.resolve_type_reference_name(node, SymbolFlags::Type, None)?;
                return self.get_type_of_symbol(symbol);
            }
        }
        Ok(self.error_type())
    }

    pub(super) fn get_type_from_jsdoc_value_reference(
        &self,
        node: Id<Node>, /*NodeWithTypeArguments*/
        symbol: Id<Symbol>,
    ) -> io::Result<Option<Id<Type>>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_jsdoc_type.is_none() {
            let value_type = self.get_type_of_symbol(symbol)?;
            let mut type_type = value_type.clone();
            if symbol.ref_(self).maybe_value_declaration().is_some() {
                let is_import_type_with_qualifier = node.ref_(self).kind()
                    == SyntaxKind::ImportType
                    && node.ref_(self).as_import_type_node().qualifier.is_some();
                if let Some(value_type_symbol) =
                    value_type
                        .ref_(self)
                        .maybe_symbol()
                        .filter(|&value_type_symbol| {
                            value_type_symbol != symbol && is_import_type_with_qualifier
                        })
                {
                    type_type = self.get_type_reference_type(node, value_type_symbol)?;
                }
            }
            links.ref_mut(self).resolved_jsdoc_type = Some(type_type);
        }
        let ret = links.ref_(self).resolved_jsdoc_type.clone();
        Ok(ret)
    }

    pub(super) fn get_substitution_type(
        &self,
        base_type: Id<Type>,
        substitute: Id<Type>,
    ) -> Id<Type> {
        if substitute
            .ref_(self)
            .flags()
            .intersects(TypeFlags::AnyOrUnknown)
            || substitute == base_type
        {
            return base_type;
        }
        let id = format!(
            "{}>{}",
            self.get_type_id(base_type),
            self.get_type_id(substitute)
        );
        let cached = self.substitution_types().get(&id).map(Clone::clone);
        if let Some(cached) = cached {
            return cached;
        }
        let result = self.create_type(TypeFlags::Substitution);
        let result = self.alloc_type(SubstitutionType::new(result, base_type, substitute).into());
        self.substitution_types().insert(id, result.clone());
        result
    }

    pub(super) fn is_unary_tuple_type_node(&self, node: Id<Node> /*TypeNode*/) -> bool {
        node.ref_(self).kind() == SyntaxKind::TupleType
            && node
                .ref_(self)
                .as_tuple_type_node()
                .elements
                .ref_(self)
                .len()
                == 1
    }

    pub(super) fn get_implied_constraint(
        &self,
        type_: Id<Type>,
        check_node: Id<Node>,   /*TypeNode*/
        extends_node: Id<Node>, /*TypeNode*/
    ) -> io::Result<Option<Id<Type>>> {
        Ok(
            if self.is_unary_tuple_type_node(check_node)
                && self.is_unary_tuple_type_node(extends_node)
            {
                self.get_implied_constraint(
                    type_,
                    check_node
                        .ref_(self)
                        .as_tuple_type_node()
                        .elements
                        .ref_(self)[0],
                    extends_node
                        .ref_(self)
                        .as_tuple_type_node()
                        .elements
                        .ref_(self)[0],
                )?
            } else if self.get_actual_type_variable(self.get_type_from_type_node_(check_node)?)?
                == type_
            {
                Some(self.get_type_from_type_node_(extends_node)?)
            } else {
                None
            },
        )
    }

    pub(super) fn get_conditional_flow_type_of_type(
        &self,
        type_: Id<Type>,
        mut node: Id<Node>,
    ) -> io::Result<Id<Type>> {
        let mut constraints: Option<Vec<Id<Type>>> = None;
        let mut covariant = true;
        while
        /*node &&*/
        !is_statement(node, self) && node.ref_(self).kind() != SyntaxKind::JSDocComment {
            let parent = node.ref_(self).parent();
            if parent.ref_(self).kind() == SyntaxKind::Parameter {
                covariant = !covariant;
            }
            if (covariant || type_.ref_(self).flags().intersects(TypeFlags::TypeVariable))
                && parent.ref_(self).kind() == SyntaxKind::ConditionalType
                && node == parent.ref_(self).as_conditional_type_node().true_type
            {
                let parent_ref = parent.ref_(self);
                let parent_as_conditional_type_node = parent_ref.as_conditional_type_node();
                let constraint = self.get_implied_constraint(
                    type_,
                    parent_as_conditional_type_node.check_type,
                    parent_as_conditional_type_node.extends_type,
                )?;
                if let Some(constraint) = constraint {
                    if constraints.is_none() {
                        constraints = Some(vec![]);
                    }
                    constraints.as_mut().unwrap().push(constraint);
                }
            }
            node = parent;
        }
        Ok(if let Some(mut constraints) = constraints {
            constraints.push(type_);
            self.get_substitution_type(
                type_,
                self.get_intersection_type(&constraints, Option::<Id<Symbol>>::None, None)?,
            )
        } else {
            type_
        })
    }

    pub(super) fn is_jsdoc_type_reference(&self, node: Id<Node>) -> bool {
        node.ref_(self).flags().intersects(NodeFlags::JSDoc)
            && matches!(
                node.ref_(self).kind(),
                SyntaxKind::TypeReference | SyntaxKind::ImportType
            )
    }

    pub(super) fn check_no_type_arguments(
        &self,
        node: Id<Node>, /*NodeWithTypeArguments*/
        symbol: Option<Id<Symbol>>,
    ) -> io::Result<bool> {
        if node
            .ref_(self)
            .as_has_type_arguments()
            .maybe_type_arguments()
            .is_some()
        {
            self.error(
                Some(node),
                &Diagnostics::Type_0_is_not_generic,
                Some(vec![if let Some(symbol) = symbol {
                    self.symbol_to_string_(symbol, Option::<Id<Node>>::None, None, None, None)?
                } else if node.ref_(self).kind() == SyntaxKind::TypeReference
                /*&& node.as_type_reference_node().type_name.is_some()*/
                {
                    declaration_name_to_string(
                        Some(node.ref_(self).as_type_reference_node().type_name),
                        self,
                    )
                    .into_owned()
                } else {
                    anon.to_owned()
                }]),
            );
            return Ok(false);
        }
        Ok(true)
    }

    pub(super) fn get_intended_type_from_jsdoc_type_reference(
        &self,
        node: Id<Node>, /*TypeReferenceNode*/
    ) -> io::Result<Option<Id<Type>>> {
        let node_ref = node.ref_(self);
        let node_as_type_reference_node = node_ref.as_type_reference_node();
        if is_identifier(&node_as_type_reference_node.type_name.ref_(self)) {
            let type_args = node_as_type_reference_node.maybe_type_arguments();
            let type_args = type_args.as_ref();
            match &*node_as_type_reference_node
                .type_name
                .ref_(self)
                .as_identifier()
                .escaped_text
            {
                "String" => {
                    self.check_no_type_arguments(node, Option::<Id<Symbol>>::None)?;
                    return Ok(Some(self.string_type()));
                }
                "Number" => {
                    self.check_no_type_arguments(node, Option::<Id<Symbol>>::None)?;
                    return Ok(Some(self.number_type()));
                }
                "Boolean" => {
                    self.check_no_type_arguments(node, Option::<Id<Symbol>>::None)?;
                    return Ok(Some(self.boolean_type()));
                }
                "Void" => {
                    self.check_no_type_arguments(node, Option::<Id<Symbol>>::None)?;
                    return Ok(Some(self.void_type()));
                }
                "Undefined" => {
                    self.check_no_type_arguments(node, Option::<Id<Symbol>>::None)?;
                    return Ok(Some(self.undefined_type()));
                }
                "Null" => {
                    self.check_no_type_arguments(node, Option::<Id<Symbol>>::None)?;
                    return Ok(Some(self.null_type()));
                }
                "Function" | "function" => {
                    self.check_no_type_arguments(node, Option::<Id<Symbol>>::None)?;
                    return Ok(Some(self.global_function_type()));
                }
                "array" => {
                    return Ok(
                        if match type_args {
                            None => true,
                            Some(type_args) => type_args.ref_(self).is_empty(),
                        } && !self.no_implicit_any
                        {
                            Some(self.any_array_type())
                        } else {
                            None
                        },
                    );
                }
                "promise" => {
                    return Ok(
                        if match type_args {
                            None => true,
                            Some(type_args) => type_args.ref_(self).is_empty(),
                        } && !self.no_implicit_any
                        {
                            Some(self.create_promise_type(self.any_type())?)
                        } else {
                            None
                        },
                    );
                }
                "Object" => {
                    if let Some(type_args) = type_args {
                        if type_args.ref_(self).len() == 2 {
                            if is_jsdoc_index_signature(node, self) {
                                let indexed =
                                    self.get_type_from_type_node_(type_args.ref_(self)[0])?;
                                let target =
                                    self.get_type_from_type_node_(type_args.ref_(self)[1])?;
                                let index_info = if indexed == self.string_type()
                                    || indexed == self.number_type()
                                {
                                    vec![self.alloc_index_info(
                                        self.create_index_info(indexed, target, false, None),
                                    )]
                                } else {
                                    vec![]
                                };
                                return Ok(Some(self.create_anonymous_type(
                                    Option::<Id<Symbol>>::None,
                                    self.empty_symbols(),
                                    vec![],
                                    vec![],
                                    index_info,
                                )?));
                            }
                            return Ok(Some(self.any_type()));
                        }
                    }
                    self.check_no_type_arguments(node, Option::<Id<Symbol>>::None)?;
                    return Ok(if !self.no_implicit_any {
                        Some(self.any_type())
                    } else {
                        None
                    });
                }
                _ => (),
            }
        }
        Ok(None)
    }
}
