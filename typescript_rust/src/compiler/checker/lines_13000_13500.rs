use std::{
    borrow::{Borrow, Cow},
    convert::{TryFrom, TryInto},
    io, ptr,
};

use gc::Gc;
use id_arena::Id;

use super::{anon, get_symbol_id, intrinsic_type_kinds};
use crate::{
    append, concatenate, declaration_name_to_string, get_check_flags, get_containing_function,
    get_declaration_of_kind, get_effective_container_for_jsdoc_template_tag, get_object_flags,
    index_of_gc, is_entity_name_expression, is_expression_with_type_arguments, is_identifier,
    is_in_js_file, is_jsdoc_augments_tag, is_jsdoc_index_signature, is_jsdoc_template_tag,
    is_statement, is_type_alias, length, maybe_concatenate, skip_parentheses, try_map,
    walk_up_parenthesized_types_and_get_parent_and_child, AsDoubleDeref, BaseObjectType,
    CheckFlags, Diagnostics, GetOrInsertDefault, HasTypeArgumentsInterface, InterfaceTypeInterface,
    Node, NodeFlags, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, OptionTry,
    SubstitutionType, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, TransientSymbolInterface,
    Type, TypeChecker, TypeFlags, TypeFormatFlags, TypeId, TypeInterface, TypeMapper,
    TypeReference, TypeReferenceInterface, TypeSystemPropertyName,
};

impl TypeChecker {
    pub(super) fn get_inferred_type_parameter_constraint(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> io::Result<Option<Id<Type>>> {
        let mut inferences: Option<Vec<Id<Type>>> = None;
        if let Some(type_parameter_symbol) = type_parameter.maybe_symbol() {
            if let Some(type_parameter_symbol_declarations) =
                type_parameter_symbol.maybe_declarations().as_deref()
            {
                for declaration in type_parameter_symbol_declarations {
                    if declaration.parent().kind() == SyntaxKind::InferType {
                        let (child_type_parameter, grand_parent) =
                            walk_up_parenthesized_types_and_get_parent_and_child(
                                &declaration.parent().parent(),
                            );
                        let grand_parent = grand_parent.unwrap();
                        let child_type_parameter =
                            child_type_parameter.unwrap_or_else(|| declaration.parent());
                        if grand_parent.kind() == SyntaxKind::TypeReference {
                            let type_reference = &grand_parent;
                            let type_parameters =
                                self.get_type_parameters_for_type_reference(type_reference)?;
                            if let Some(type_parameters) = type_parameters {
                                let index: usize = index_of_gc(
                                    type_reference
                                        .as_type_reference_node()
                                        .maybe_type_arguments()
                                        .as_deref()
                                        .unwrap(),
                                    &child_type_parameter,
                                )
                                .try_into()
                                .unwrap();
                                if index < type_parameters.len() {
                                    let declared_constraint = self
                                        .get_constraint_of_type_parameter(
                                            &type_parameters[index],
                                        )?;
                                    if let Some(declared_constraint) = declared_constraint {
                                        let mapper = Gc::new(self.create_type_mapper(
                                            type_parameters.clone(),
                                            Some(self.get_effective_type_arguments(
                                                type_reference,
                                                Some(&type_parameters),
                                            )?),
                                        ));
                                        let constraint = self
                                            .instantiate_type(&declared_constraint, Some(mapper))?;
                                        if !ptr::eq(&*constraint, type_parameter) {
                                            if inferences.is_none() {
                                                inferences = Some(vec![]);
                                            }
                                            append(inferences.as_mut().unwrap(), Some(constraint));
                                        }
                                    }
                                }
                            }
                        } else if grand_parent.kind() == SyntaxKind::Parameter
                            && grand_parent
                                .as_parameter_declaration()
                                .dot_dot_dot_token
                                .is_some()
                            || grand_parent.kind() == SyntaxKind::RestType
                            || grand_parent.kind() == SyntaxKind::NamedTupleMember
                                && grand_parent
                                    .as_named_tuple_member()
                                    .dot_dot_dot_token
                                    .is_some()
                        {
                            if inferences.is_none() {
                                inferences = Some(vec![]);
                            }
                            append(
                                inferences.as_mut().unwrap(),
                                Some(self.create_array_type(&self.unknown_type(), None)),
                            );
                        } else if grand_parent.kind() == SyntaxKind::TemplateLiteralTypeSpan {
                            if inferences.is_none() {
                                inferences = Some(vec![]);
                            }
                            append(inferences.as_mut().unwrap(), Some(self.string_type()));
                        } else if grand_parent.kind() == SyntaxKind::TypeParameter
                            && grand_parent.parent().kind() == SyntaxKind::MappedType
                        {
                            if inferences.is_none() {
                                inferences = Some(vec![]);
                            }
                            append(
                                inferences.as_mut().unwrap(),
                                Some(self.keyof_constraint_type()),
                            );
                        } else if grand_parent.kind() == SyntaxKind::MappedType && {
                            let grand_parent_as_mapped_type_node =
                                grand_parent.as_mapped_type_node();
                            grand_parent_as_mapped_type_node.type_.is_some()
                                && Gc::ptr_eq(
                                    &skip_parentheses(
                                        grand_parent_as_mapped_type_node.type_.as_ref().unwrap(),
                                        None,
                                    ),
                                    &declaration.parent(),
                                )
                                && grand_parent.parent().kind() == SyntaxKind::ConditionalType
                                && {
                                    let grand_parent_parent = grand_parent.parent();
                                    let grand_parent_parent_as_conditional_type_node =
                                        grand_parent_parent.as_conditional_type_node();
                                    Gc::ptr_eq(
                                        &grand_parent_parent_as_conditional_type_node.extends_type,
                                        &grand_parent,
                                    ) && grand_parent_parent_as_conditional_type_node
                                        .check_type
                                        .kind()
                                        == SyntaxKind::MappedType
                                        && grand_parent_parent_as_conditional_type_node
                                            .check_type
                                            .as_mapped_type_node()
                                            .type_
                                            .is_some()
                                }
                        } {
                            let check_mapped_type = grand_parent
                                .parent()
                                .as_conditional_type_node()
                                .check_type
                                .clone();
                            let check_mapped_type = check_mapped_type.as_mapped_type_node();
                            let node_type = self.get_type_from_type_node_(
                                check_mapped_type.type_.as_ref().unwrap(),
                            )?;
                            append(
                                inferences.get_or_insert_default_(),
                                Some(
                                    self.instantiate_type(
                                        &node_type,
                                        Some(Gc::new(
                                            self.make_unary_type_mapper(
                                                &self.get_declared_type_of_type_parameter(
                                                    &self
                                                        .get_symbol_of_node(
                                                            &check_mapped_type.type_parameter,
                                                        )?
                                                        .unwrap(),
                                                ),
                                                &*if let Some(
                                                    check_mapped_type_type_parameter_constraint,
                                                ) = check_mapped_type
                                                    .type_parameter
                                                    .as_type_parameter_declaration()
                                                    .constraint
                                                    .as_ref()
                                                {
                                                    self.get_type_from_type_node_(
                                                        check_mapped_type_type_parameter_constraint,
                                                    )?
                                                } else {
                                                    self.keyof_constraint_type()
                                                },
                                            ),
                                        )),
                                    )?,
                                ),
                            );
                        }
                    }
                }
            }
        }
        inferences.try_map(|inferences| {
            self.get_intersection_type(&inferences, Option::<&Symbol>::None, None)
        })
    }

    pub(super) fn get_constraint_from_type_parameter(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> io::Result<Option<Id<Type>>> {
        let type_parameter_as_type_parameter = type_parameter.as_type_parameter();
        if type_parameter_as_type_parameter
            .maybe_constraint()
            .is_none()
        {
            if let Some(type_parameter_target) = type_parameter_as_type_parameter.target.as_ref() {
                let target_constraint =
                    self.get_constraint_of_type_parameter(type_parameter_target)?;
                type_parameter_as_type_parameter.set_constraint(match target_constraint {
                    Some(target_constraint) => self.instantiate_type(
                        &target_constraint,
                        type_parameter_as_type_parameter.maybe_mapper(),
                    )?,
                    None => self.no_constraint_type(),
                });
            } else {
                let constraint_declaration = self.get_constraint_declaration(type_parameter);
                match constraint_declaration {
                    None => {
                        type_parameter_as_type_parameter.set_constraint(
                            self.get_inferred_type_parameter_constraint(type_parameter)?
                                .unwrap_or_else(|| self.no_constraint_type()),
                        );
                    }
                    Some(constraint_declaration) => {
                        let mut type_ = self.get_type_from_type_node_(&constraint_declaration)?;
                        if type_.flags().intersects(TypeFlags::Any) && !self.is_error_type(&type_) {
                            type_ = if constraint_declaration.parent().parent().kind()
                                == SyntaxKind::MappedType
                            {
                                self.keyof_constraint_type()
                            } else {
                                self.unknown_type()
                            };
                        }
                        type_parameter_as_type_parameter.set_constraint(type_);
                    }
                }
            }
        }
        Ok(type_parameter_as_type_parameter
            .maybe_constraint()
            .and_then(|type_parameter_constraint| {
                if Gc::ptr_eq(&type_parameter_constraint, &self.no_constraint_type()) {
                    None
                } else {
                    Some(type_parameter_constraint)
                }
            }))
    }

    pub(super) fn get_parent_symbol_of_type_parameter(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> io::Result<Option<Gc<Symbol>>> {
        let tp =
            get_declaration_of_kind(&type_parameter.symbol(), SyntaxKind::TypeParameter).unwrap();
        let host = if is_jsdoc_template_tag(&tp.parent()) {
            get_effective_container_for_jsdoc_template_tag(&tp.parent())
        } else {
            Some(tp.parent())
        };
        host.try_and_then(|host| self.get_symbol_of_node(&host))
    }

    pub(super) fn get_type_list_id(&self, types: Option<&[Id<Type>]>) -> String {
        let mut result = "".to_owned();
        if let Some(types) = types {
            let length = types.len();
            let mut i = 0;
            while i < length {
                let start_id = types[i].id();
                let mut count = 1;
                while i + count < length
                    && types[i + count].id() == start_id + TypeId::try_from(count).unwrap()
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
        alias_symbol: Option<impl Borrow<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> String {
        if let Some(alias_symbol) = alias_symbol {
            let alias_symbol = alias_symbol.borrow();
            format!(
                "@{}{}",
                get_symbol_id(alias_symbol),
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
        for type_ in types {
            if !type_.flags().intersects(exclude_kinds) {
                result |= get_object_flags(&type_);
            }
        }
        result & ObjectFlags::PropagatingFlags
    }

    pub(super) fn create_type_reference(
        &self,
        target: &Type, /*GenericType*/
        type_arguments: Option<Vec<Id<Type>>>,
    ) -> Id<Type /*TypeReference*/> {
        let id = self.get_type_list_id(type_arguments.as_deref());
        let type_ = target
            .as_generic_type()
            .instantiations()
            .get(&id)
            .map(Clone::clone);
        if type_.is_none() {
            let type_ = self.create_object_type(ObjectFlags::Reference, target.maybe_symbol());
            type_.set_object_flags(
                type_.object_flags()
                    | if let Some(type_arguments) = type_arguments.as_ref() {
                        self.get_propagating_flags_of_types(type_arguments, TypeFlags::None)
                    } else {
                        ObjectFlags::None
                    },
            );
            let type_: Id<Type> =
                TypeReference::new(type_, target.type_wrapper(), type_arguments).into();
            target
                .as_generic_type()
                .instantiations()
                .insert(id, type_.clone());
            return type_;
        }
        type_.unwrap()
    }

    pub(super) fn clone_type_reference(&self, source: &Type /*TypeReference*/) -> Id<Type> {
        let type_ = self.create_type(source.flags());
        type_.set_symbol(source.maybe_symbol());
        let source_as_type_reference = source.as_type_reference_interface();
        let type_ = BaseObjectType::new(type_, source_as_type_reference.object_flags());
        let type_: Id<Type> = TypeReference::new(
            type_,
            source_as_type_reference.target(),
            source_as_type_reference
                .maybe_resolved_type_arguments()
                .clone(),
        )
        .into();
        type_
    }

    pub(super) fn create_deferred_type_reference(
        &self,
        target: &Type, /*GenericType*/
        node: &Node,   /*TypeReferenceNode | ArrayTypeNode | TupleTypeNode*/
        mapper: Option<Gc<TypeMapper>>,
        alias_symbol: Option<impl Borrow<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type /*DeferredTypeReference*/>> {
        let mut alias_symbol =
            alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
        let mut alias_type_arguments = alias_type_arguments.map(ToOwned::to_owned);
        if alias_symbol.is_none() {
            alias_symbol = self.get_alias_symbol_for_type_node(node)?;
            let local_alias_type_arguments =
                self.get_type_arguments_for_alias_symbol(alias_symbol.as_deref())?;
            alias_type_arguments = if let Some(mapper) = mapper.as_ref() {
                self.instantiate_types(local_alias_type_arguments.as_deref(), Some(mapper.clone()))?
            } else {
                local_alias_type_arguments
            };
        }
        let mut type_ = self.create_object_type(ObjectFlags::Reference, target.maybe_symbol());
        type_.mapper = mapper;
        let type_: Id<Type> = TypeReference::new(type_, target.type_wrapper(), None).into();
        *type_.as_type_reference().maybe_node_mut() = Some(node.node_wrapper());
        *type_.maybe_alias_symbol_mut() = alias_symbol;
        *type_.maybe_alias_type_arguments_mut() = alias_type_arguments;
        Ok(type_)
    }

    pub(super) fn get_type_arguments(
        &self,
        type_: &Type, /*TypeReference*/
    ) -> io::Result<Vec<Id<Type>>> {
        let type_as_type_reference = type_.as_type_reference_interface();
        if type_as_type_reference
            .maybe_resolved_type_arguments()
            .is_none()
        {
            if !self.push_type_resolution(
                &type_.type_wrapper().into(),
                TypeSystemPropertyName::ResolvedTypeArguments,
            ) {
                return Ok(type_as_type_reference
                    .target()
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
            let node = type_as_type_reference.maybe_node().clone();
            let type_arguments: Vec<Id<Type>> = if node.is_none() {
                vec![]
            } else {
                let node = node.unwrap();
                if node.kind() == SyntaxKind::TypeReference {
                    let target = type_as_type_reference.target();
                    let target_as_interface_type = target.as_interface_type();
                    concatenate(
                        target_as_interface_type
                            .maybe_outer_type_parameters()
                            .map_or_else(|| vec![], ToOwned::to_owned),
                        self.get_effective_type_arguments(
                            &node,
                            target_as_interface_type.maybe_local_type_parameters(),
                        )?,
                    )
                } else if node.kind() == SyntaxKind::ArrayType {
                    vec![self.get_type_from_type_node_(&node.as_array_type_node().element_type)?]
                } else {
                    try_map(
                        &node.as_tuple_type_node().elements,
                        |element: &Gc<Node>, _| self.get_type_from_type_node_(element),
                    )?
                }
            };
            if self.pop_type_resolution() {
                *type_as_type_reference.maybe_resolved_type_arguments_mut() =
                    if let Some(type_mapper) = type_as_type_reference.maybe_mapper() {
                        self.instantiate_types(Some(&type_arguments), Some(type_mapper))?
                    } else {
                        Some(type_arguments)
                    };
            } else {
                *type_as_type_reference.maybe_resolved_type_arguments_mut() = Some(
                    type_as_type_reference
                        .target()
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
                    type_as_type_reference
                        .maybe_node()
                        .clone()
                        .or_else(|| self.maybe_current_node()),
                    if type_as_type_reference.target().maybe_symbol().is_some() {
                        &Diagnostics::Type_arguments_for_0_circularly_reference_themselves
                    } else {
                        &Diagnostics::Tuple_type_arguments_circularly_reference_themselves
                    },
                    if let Some(type_target_symbol) = type_as_type_reference.target().maybe_symbol()
                    {
                        Some(vec![self.symbol_to_string_(
                            &type_target_symbol,
                            Option::<&Node>::None,
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
        Ok(type_as_type_reference
            .maybe_resolved_type_arguments()
            .clone()
            .unwrap())
    }

    pub(super) fn get_type_reference_arity(&self, type_: &Type /*TypeReference*/) -> usize {
        length(
            type_
                .as_type_reference_interface()
                .target()
                .as_interface_type()
                .maybe_type_parameters(),
        )
    }

    pub(super) fn get_type_from_class_or_interface_reference(
        &self,
        node: &Node, /*NodeWithTypeArguments*/
        symbol: &Symbol,
    ) -> io::Result<Id<Type>> {
        let type_ =
            self.get_declared_type_of_symbol(&self.get_merged_symbol(Some(symbol)).unwrap())?;
        let type_as_interface_type = type_.as_interface_type();
        let type_parameters = type_as_interface_type.maybe_local_type_parameters();
        if let Some(type_parameters) = type_parameters {
            let num_type_arguments = length(
                node.as_has_type_arguments()
                    .maybe_type_arguments()
                    .as_double_deref(),
            );
            let min_type_argument_count = self.get_min_type_argument_count(Some(type_parameters));
            let is_js = is_in_js_file(Some(node));
            let is_js_implicit_any = !self.no_implicit_any && is_js;
            if !is_js_implicit_any
                && (num_type_arguments < min_type_argument_count
                    || num_type_arguments > type_parameters.len())
            {
                let missing_augments_tag = is_js
                    && is_expression_with_type_arguments(node)
                    && !is_jsdoc_augments_tag(&node.parent());
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
                    &type_,
                    Option::<&Node>::None,
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
            if node.kind() == SyntaxKind::TypeReference
                && self.is_deferred_type_reference_node(
                    node,
                    Some(
                        length(
                            node.as_has_type_arguments()
                                .maybe_type_arguments()
                                .as_double_deref(),
                        ) != type_parameters.len(),
                    ),
                )?
            {
                return self.create_deferred_type_reference(
                    &type_,
                    node,
                    None,
                    Option::<&Symbol>::None,
                    None,
                );
            }
            let type_arguments = maybe_concatenate(
                type_as_interface_type
                    .maybe_outer_type_parameters()
                    .map(ToOwned::to_owned),
                self.fill_missing_type_arguments(
                    self.type_arguments_from_type_reference_node(node)?,
                    Some(type_parameters),
                    min_type_argument_count,
                    is_js,
                )?,
            );
            return Ok(self.create_type_reference(&type_, type_arguments));
        }
        Ok(if self.check_no_type_arguments(node, Some(symbol))? {
            type_
        } else {
            self.error_type()
        })
    }

    pub(super) fn get_type_alias_instantiation(
        &self,
        symbol: &Symbol,
        type_arguments: Option<&[Id<Type>]>,
        alias_symbol: Option<impl Borrow<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type>> {
        let type_ = self.get_declared_type_of_symbol(symbol)?;
        if Gc::ptr_eq(&type_, &self.intrinsic_marker_type())
            && intrinsic_type_kinds.contains_key(symbol.escaped_name())
            && matches!(type_arguments, Some(type_arguments) if type_arguments.len() == 1)
        {
            return self.get_string_mapping_type(symbol, &type_arguments.unwrap()[0]);
        }
        let links = self.get_symbol_links(symbol);
        let type_parameters = (*links).borrow().type_parameters.clone().unwrap();
        let alias_symbol = alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
        let id = format!(
            "{}{}",
            self.get_type_list_id(type_arguments),
            self.get_alias_id(alias_symbol.as_deref(), alias_type_arguments)
        );
        let mut instantiation = (*links)
            .borrow()
            .instantiations
            .as_ref()
            .unwrap()
            .get(&id)
            .map(Clone::clone);
        if instantiation.is_none() {
            instantiation = Some(self.instantiate_type_with_alias(
                &type_,
                Gc::new(self.create_type_mapper(
                    type_parameters.clone(),
                    self.fill_missing_type_arguments(
                        type_arguments.map(ToOwned::to_owned),
                        Some(&type_parameters),
                        self.get_min_type_argument_count(Some(&type_parameters)),
                        is_in_js_file(symbol.maybe_value_declaration()),
                    )?,
                )),
                alias_symbol.as_deref(),
                alias_type_arguments,
            )?);
            links
                .borrow_mut()
                .instantiations
                .as_mut()
                .unwrap()
                .insert(id, instantiation.clone().unwrap());
        }
        Ok(instantiation.unwrap())
    }

    pub(super) fn get_type_from_type_alias_reference(
        &self,
        node: &Node, /*NodeWithTypeArguments*/
        symbol: &Symbol,
    ) -> io::Result<Id<Type>> {
        if get_check_flags(symbol).intersects(CheckFlags::Unresolved) {
            let type_arguments = self.type_arguments_from_type_reference_node(node)?;
            let id = self.get_alias_id(Some(symbol), type_arguments.as_deref());
            let mut error_type = self.error_types().get(&id).map(Clone::clone);
            if error_type.is_none() {
                error_type = Some(
                    self.create_intrinsic_type(TypeFlags::Any, "error", None)
                        .into(),
                );
                let error_type = error_type.as_ref().unwrap();
                *error_type.maybe_alias_symbol_mut() = Some(symbol.symbol_wrapper());
                *error_type.maybe_alias_type_arguments_mut() = type_arguments;
                self.error_types().insert(id, error_type.clone());
            }
            return Ok(error_type.unwrap());
        }
        let type_ = self.get_declared_type_of_symbol(symbol)?;
        let type_parameters = (*self.get_symbol_links(symbol))
            .borrow()
            .type_parameters
            .clone();
        if let Some(type_parameters) = type_parameters {
            let num_type_arguments = length(
                node.as_has_type_arguments()
                    .maybe_type_arguments()
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
                        self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)?,
                        min_type_argument_count.to_string(),
                        type_parameters.len().to_string(),
                    ]),
                );
                return Ok(self.error_type());
            }
            let alias_symbol = self.get_alias_symbol_for_type_node(node)?;
            let new_alias_symbol = alias_symbol.filter(|alias_symbol| {
                self.is_local_type_alias(symbol) || !self.is_local_type_alias(alias_symbol)
            });
            return self.get_type_alias_instantiation(
                symbol,
                self.type_arguments_from_type_reference_node(node)?
                    .as_deref(),
                new_alias_symbol.as_deref(),
                self.get_type_arguments_for_alias_symbol(new_alias_symbol.as_deref())?
                    .as_deref(),
            );
        }
        Ok(if self.check_no_type_arguments(node, Some(symbol))? {
            type_
        } else {
            self.error_type()
        })
    }

    pub(super) fn is_local_type_alias(&self, symbol: &Symbol) -> bool {
        let symbol_declarations = symbol.maybe_declarations();
        let declaration = symbol_declarations.as_ref().and_then(|declarations| {
            declarations
                .iter()
                .find(|declaration: &&Gc<Node>| is_type_alias(declaration))
                .map(Clone::clone)
        });
        declaration
            .and_then(|declaration| get_containing_function(&declaration))
            .is_some()
    }

    pub(super) fn get_type_reference_name(
        &self,
        node: &Node, /*TypeReferenceType*/
    ) -> Option<Gc<Node /*EntityNameOrEntityNameExpression*/>> {
        match node.kind() {
            SyntaxKind::TypeReference => {
                return Some(node.as_type_reference_node().type_name.clone());
            }
            SyntaxKind::ExpressionWithTypeArguments => {
                let expr = &node.as_expression_with_type_arguments().expression;
                if is_entity_name_expression(expr) {
                    return Some(expr.clone());
                }
            }
            _ => (),
        }
        None
    }

    pub(super) fn get_symbol_path<'symbol>(&self, symbol: &'symbol Symbol) -> Cow<'symbol, str> {
        match symbol.maybe_parent() {
            Some(symbol_parent) => format!(
                "{}.{}",
                self.get_symbol_path(&symbol_parent),
                symbol.escaped_name()
            )
            .into(),
            None => symbol.escaped_name().into(),
        }
    }

    pub(super) fn get_unresolved_symbol_for_entity_name(
        &self,
        name: &Node, /*EntityNameOrEntityNameExpression*/
    ) -> Gc<Symbol /*TransientSymbol*/> {
        let identifier = if name.kind() == SyntaxKind::QualifiedName {
            name.as_qualified_name().right.clone()
        } else if name.kind() == SyntaxKind::PropertyAccessExpression {
            name.as_property_access_expression().name.clone()
        } else {
            name.node_wrapper()
        };
        let text = &identifier.as_identifier().escaped_text;
        if &**text != "" {
            let parent_symbol = if name.kind() == SyntaxKind::QualifiedName {
                Some(self.get_unresolved_symbol_for_entity_name(&name.as_qualified_name().left))
            } else if name.kind() == SyntaxKind::PropertyAccessExpression {
                Some(self.get_unresolved_symbol_for_entity_name(
                    &name.as_property_access_expression().expression,
                ))
            } else {
                None
            };
            let path = if let Some(parent_symbol) = parent_symbol.as_ref() {
                format!("{}.{}", self.get_symbol_path(parent_symbol), &**text)
            } else {
                (&**text).to_owned()
            };
            let mut result = self.unresolved_symbols().get(&path).map(Clone::clone);
            if result.is_none() {
                result = Some(
                    self.create_symbol(
                        SymbolFlags::TypeAlias,
                        text.clone(),
                        Some(CheckFlags::Unresolved),
                    )
                    .into(),
                );
                let result = result.as_ref().unwrap();
                self.unresolved_symbols().insert(path, result.clone());
                result.set_parent(parent_symbol);
                result
                    .as_transient_symbol()
                    .symbol_links()
                    .borrow_mut()
                    .declared_type = Some(self.unresolved_type());
            }
            return result.unwrap();
        }
        return self.unknown_symbol();
    }

    pub(super) fn resolve_type_reference_name(
        &self,
        type_reference: &Node, /*TypeReferenceType*/
        meaning: SymbolFlags,
        ignore_errors: Option<bool>,
    ) -> io::Result<Gc<Symbol>> {
        let ignore_errors = ignore_errors.unwrap_or(false);
        let name = self.get_type_reference_name(type_reference);
        let name = match name {
            Some(name) => name,
            None => {
                return Ok(self.unknown_symbol());
            }
        };
        let symbol = self.resolve_entity_name(
            &*name,
            meaning,
            Some(ignore_errors),
            None,
            Option::<&Node>::None,
        )?;
        Ok(
            if symbol.is_some() && !Gc::ptr_eq(symbol.as_ref().unwrap(), &self.unknown_symbol()) {
                symbol.unwrap()
            } else if ignore_errors {
                self.unknown_symbol()
            } else {
                self.get_unresolved_symbol_for_entity_name(&name)
            },
        )
    }

    pub(super) fn get_type_reference_type(
        &self,
        node: &Node,
        symbol: &Symbol,
    ) -> io::Result<Id<Type>> {
        if ptr::eq(symbol, &*self.unknown_symbol()) {
            return Ok(self.error_type());
        }
        let symbol = self
            .get_expando_symbol(symbol)?
            .unwrap_or_else(|| symbol.symbol_wrapper());
        if symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return self.get_type_from_class_or_interface_reference(node, &symbol);
        }
        if symbol.flags().intersects(SymbolFlags::TypeAlias) {
            return self.get_type_from_type_alias_reference(node, &symbol);
        }
        let res = self.try_get_declared_type_of_symbol(&symbol)?;
        if let Some(res) = res {
            return Ok(if self.check_no_type_arguments(node, Some(&*symbol))? {
                self.get_regular_type_of_literal_type(&res)
            } else {
                self.error_type()
            });
        }
        if symbol.flags().intersects(SymbolFlags::Value) && self.is_jsdoc_type_reference(node) {
            let jsdoc_type = self.get_type_from_jsdoc_value_reference(node, &symbol)?;
            if let Some(jsdoc_type) = jsdoc_type {
                return Ok(jsdoc_type);
            } else {
                self.resolve_type_reference_name(node, SymbolFlags::Type, None)?;
                return self.get_type_of_symbol(&symbol);
            }
        }
        Ok(self.error_type())
    }

    pub(super) fn get_type_from_jsdoc_value_reference(
        &self,
        node: &Node, /*NodeWithTypeArguments*/
        symbol: &Symbol,
    ) -> io::Result<Option<Id<Type>>> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_jsdoc_type.is_none() {
            let value_type = self.get_type_of_symbol(symbol)?;
            let mut type_type = value_type.clone();
            if symbol.maybe_value_declaration().is_some() {
                let is_import_type_with_qualifier = node.kind() == SyntaxKind::ImportType
                    && node.as_import_type_node().qualifier.is_some();
                if let Some(value_type_symbol) =
                    value_type.maybe_symbol().filter(|value_type_symbol| {
                        !ptr::eq(&**value_type_symbol, symbol) && is_import_type_with_qualifier
                    })
                {
                    type_type = self.get_type_reference_type(node, &value_type_symbol)?;
                }
            }
            links.borrow_mut().resolved_jsdoc_type = Some(type_type);
        }
        let ret = (*links).borrow().resolved_jsdoc_type.clone();
        Ok(ret)
    }

    pub(super) fn get_substitution_type(&self, base_type: &Type, substitute: &Type) -> Id<Type> {
        if substitute.flags().intersects(TypeFlags::AnyOrUnknown) || ptr::eq(substitute, base_type)
        {
            return base_type.type_wrapper();
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
        let result: Id<Type> =
            SubstitutionType::new(result, base_type.type_wrapper(), substitute.type_wrapper())
                .into();
        self.substitution_types().insert(id, result.clone());
        result
    }

    pub(super) fn is_unary_tuple_type_node(&self, node: &Node /*TypeNode*/) -> bool {
        node.kind() == SyntaxKind::TupleType && node.as_tuple_type_node().elements.len() == 1
    }

    pub(super) fn get_implied_constraint(
        &self,
        type_: &Type,
        check_node: &Node,   /*TypeNode*/
        extends_node: &Node, /*TypeNode*/
    ) -> io::Result<Option<Id<Type>>> {
        Ok(
            if self.is_unary_tuple_type_node(check_node)
                && self.is_unary_tuple_type_node(extends_node)
            {
                self.get_implied_constraint(
                    type_,
                    &check_node.as_tuple_type_node().elements[0],
                    &extends_node.as_tuple_type_node().elements[0],
                )?
            } else if ptr::eq(
                &*self.get_actual_type_variable(&*self.get_type_from_type_node_(check_node)?)?,
                type_,
            ) {
                Some(self.get_type_from_type_node_(extends_node)?)
            } else {
                None
            },
        )
    }

    pub(super) fn get_conditional_flow_type_of_type(
        &self,
        type_: &Type,
        node: &Node,
    ) -> io::Result<Id<Type>> {
        let mut constraints: Option<Vec<Id<Type>>> = None;
        let mut covariant = true;
        let mut node = node.node_wrapper();
        while
        /*node &&*/
        !is_statement(&node) && node.kind() != SyntaxKind::JSDocComment {
            let parent = node.parent();
            if parent.kind() == SyntaxKind::Parameter {
                covariant = !covariant;
            }
            if (covariant || type_.flags().intersects(TypeFlags::TypeVariable))
                && parent.kind() == SyntaxKind::ConditionalType
                && Gc::ptr_eq(&node, &parent.as_conditional_type_node().true_type)
            {
                let parent_as_conditional_type_node = parent.as_conditional_type_node();
                let constraint = self.get_implied_constraint(
                    type_,
                    &parent_as_conditional_type_node.check_type,
                    &parent_as_conditional_type_node.extends_type,
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
            constraints.push(type_.type_wrapper());
            self.get_substitution_type(
                type_,
                &*self.get_intersection_type(&constraints, Option::<&Symbol>::None, None)?,
            )
        } else {
            type_.type_wrapper()
        })
    }

    pub(super) fn is_jsdoc_type_reference(&self, node: &Node) -> bool {
        node.flags().intersects(NodeFlags::JSDoc)
            && matches!(
                node.kind(),
                SyntaxKind::TypeReference | SyntaxKind::ImportType
            )
    }

    pub(super) fn check_no_type_arguments(
        &self,
        node: &Node, /*NodeWithTypeArguments*/
        symbol: Option<impl Borrow<Symbol>>,
    ) -> io::Result<bool> {
        if node
            .as_has_type_arguments()
            .maybe_type_arguments()
            .is_some()
        {
            self.error(
                Some(node),
                &Diagnostics::Type_0_is_not_generic,
                Some(vec![if let Some(symbol) = symbol {
                    let symbol = symbol.borrow();
                    self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)?
                } else if node.kind() == SyntaxKind::TypeReference
                /*&& node.as_type_reference_node().type_name.is_some()*/
                {
                    declaration_name_to_string(Some(&*node.as_type_reference_node().type_name))
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
        node: &Node, /*TypeReferenceNode*/
    ) -> io::Result<Option<Id<Type>>> {
        let node_as_type_reference_node = node.as_type_reference_node();
        if is_identifier(&node_as_type_reference_node.type_name) {
            let type_args = node_as_type_reference_node.maybe_type_arguments();
            let type_args = type_args.as_ref();
            match &*node_as_type_reference_node
                .type_name
                .as_identifier()
                .escaped_text
            {
                "String" => {
                    self.check_no_type_arguments(node, Option::<&Symbol>::None)?;
                    return Ok(Some(self.string_type()));
                }
                "Number" => {
                    self.check_no_type_arguments(node, Option::<&Symbol>::None)?;
                    return Ok(Some(self.number_type()));
                }
                "Boolean" => {
                    self.check_no_type_arguments(node, Option::<&Symbol>::None)?;
                    return Ok(Some(self.boolean_type()));
                }
                "Void" => {
                    self.check_no_type_arguments(node, Option::<&Symbol>::None)?;
                    return Ok(Some(self.void_type()));
                }
                "Undefined" => {
                    self.check_no_type_arguments(node, Option::<&Symbol>::None)?;
                    return Ok(Some(self.undefined_type()));
                }
                "Null" => {
                    self.check_no_type_arguments(node, Option::<&Symbol>::None)?;
                    return Ok(Some(self.null_type()));
                }
                "Function" | "function" => {
                    self.check_no_type_arguments(node, Option::<&Symbol>::None)?;
                    return Ok(Some(self.global_function_type()));
                }
                "array" => {
                    return Ok(
                        if match type_args {
                            None => true,
                            Some(type_args) => type_args.is_empty(),
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
                            Some(type_args) => type_args.is_empty(),
                        } && !self.no_implicit_any
                        {
                            Some(self.create_promise_type(&self.any_type())?)
                        } else {
                            None
                        },
                    );
                }
                "Object" => {
                    if let Some(type_args) = type_args {
                        if type_args.len() == 2 {
                            if is_jsdoc_index_signature(node) {
                                let indexed = self.get_type_from_type_node_(&type_args[0])?;
                                let target = self.get_type_from_type_node_(&type_args[1])?;
                                let index_info = if Gc::ptr_eq(&indexed, &self.string_type())
                                    || Gc::ptr_eq(&indexed, &self.number_type())
                                {
                                    vec![Gc::new(
                                        self.create_index_info(indexed, target, false, None),
                                    )]
                                } else {
                                    vec![]
                                };
                                return Ok(Some(self.create_anonymous_type(
                                    Option::<&Symbol>::None,
                                    self.empty_symbols(),
                                    vec![],
                                    vec![],
                                    index_info,
                                )?));
                            }
                            return Ok(Some(self.any_type()));
                        }
                    }
                    self.check_no_type_arguments(node, Option::<&Symbol>::None)?;
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
