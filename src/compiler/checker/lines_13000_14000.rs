#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use crate::{
    append, index_of_rc, skip_parentheses, walk_up_parenthesized_types_and_get_parent_and_child,
    ElementFlags, InterfaceTypeInterface, __String, concatenate, get_object_flags, map,
    DiagnosticMessage, Diagnostics, Node, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface,
    Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeReference,
};

impl TypeChecker {
    pub(super) fn get_inferred_type_parameter_constraint(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> Option<Rc<Type>> {
        let mut inferences: Option<Vec<Rc<Type>>> = None;
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
                                self.get_type_parameters_for_type_reference(type_reference);
                            if let Some(type_parameters) = type_parameters {
                                let index: usize = index_of_rc(
                                    type_reference
                                        .as_type_reference_node()
                                        .type_arguments
                                        .as_deref()
                                        .unwrap(),
                                    &child_type_parameter,
                                )
                                .try_into()
                                .unwrap();
                                if index < type_parameters.len() {
                                    let declared_constraint = self
                                        .get_constraint_of_type_parameter(&type_parameters[index]);
                                    if let Some(declared_constraint) = declared_constraint {
                                        let mapper = self.create_type_mapper(
                                            type_parameters.clone(),
                                            Some(self.get_effective_type_arguments(
                                                type_reference,
                                                &type_parameters,
                                            )),
                                        );
                                        let constraint = self
                                            .instantiate_type(
                                                Some(&*declared_constraint),
                                                Some(&mapper),
                                            )
                                            .unwrap();
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
                                && Rc::ptr_eq(
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
                                    Rc::ptr_eq(
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
                            );
                            if inferences.is_none() {
                                inferences = Some(vec![]);
                            }
                            append(
                                inferences.as_mut().unwrap(),
                                self.instantiate_type(
                                    Some(node_type),
                                    Some(
                                        &self.make_unary_type_mapper(
                                            &self.get_declared_type_of_type_parameter(
                                                &self
                                                    .get_symbol_of_node(
                                                        &check_mapped_type.type_parameter,
                                                    )
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
                                                )
                                            } else {
                                                self.keyof_constraint_type()
                                            },
                                        ),
                                    ),
                                ),
                            );
                        }
                    }
                }
            }
        }
        inferences.map(|inferences| {
            self.get_intersection_type(&inferences, Option::<&Symbol>::None, None)
        })
    }

    pub(super) fn get_constraint_from_type_parameter(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> Option<Rc<Type>> {
        let type_parameter_as_type_parameter = type_parameter.as_type_parameter();
        if type_parameter_as_type_parameter
            .maybe_constraint()
            .is_none()
        {
            if let Some(type_parameter_target) = type_parameter_as_type_parameter.target.as_ref() {
                let target_constraint =
                    self.get_constraint_of_type_parameter(type_parameter_target);
                type_parameter_as_type_parameter.set_constraint(match target_constraint {
                    Some(target_constraint) => self
                        .instantiate_type(
                            Some(target_constraint),
                            type_parameter_as_type_parameter.maybe_mapper().as_ref(),
                        )
                        .unwrap(),
                    None => self.no_constraint_type(),
                });
            } else {
                let constraint_declaration = self.get_constraint_declaration(type_parameter);
                match constraint_declaration {
                    None => {
                        type_parameter_as_type_parameter.set_constraint(
                            self.get_inferred_type_parameter_constraint(type_parameter)
                                .unwrap_or_else(|| self.no_constraint_type()),
                        );
                    }
                    Some(constraint_declaration) => {
                        let mut type_ = self.get_type_from_type_node_(&constraint_declaration);
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
        type_parameter_as_type_parameter
            .maybe_constraint()
            .and_then(|type_parameter_constraint| {
                if Rc::ptr_eq(&type_parameter_constraint, &self.no_constraint_type()) {
                    None
                } else {
                    Some(type_parameter_constraint)
                }
            })
    }

    pub(super) fn get_parent_symbol_of_type_parameter(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_list_id(&self, types: Option<&[Rc<Type>]>) -> String {
        unimplemented!()
    }

    pub(super) fn get_propagating_flags_of_types(
        &self,
        types: &[Rc<Type>],
        exclude_kinds: TypeFlags,
    ) -> ObjectFlags {
        let mut result = ObjectFlags::None;
        for type_ in types {
            if !type_.flags().intersects(exclude_kinds) {
                result |= get_object_flags(&*type_);
            }
        }
        result & ObjectFlags::PropagatingFlags
    }

    pub(super) fn create_type_reference(
        &self,
        target: &Type, /*GenericType*/
        type_arguments: Option<Vec<Rc<Type>>>,
    ) -> TypeReference {
        let type_ = self.create_object_type(ObjectFlags::Reference, Some(target.symbol()));
        type_.set_object_flags(
            type_.object_flags()
                | if let Some(type_arguments) = type_arguments.as_ref() {
                    self.get_propagating_flags_of_types(type_arguments, TypeFlags::None)
                } else {
                    ObjectFlags::None
                },
        );
        let type_ = TypeReference::new(type_, target.type_wrapper(), type_arguments);
        type_
    }

    pub(super) fn clone_type_reference(&self, source: &Type /*TypeReference*/) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_arguments(&self, type_: &Type /*TypeReference*/) -> Vec<Rc<Type>> {
        let type_as_type_reference = type_.as_type_reference();
        let mut resolved_type_arguments =
            type_as_type_reference.resolved_type_arguments.borrow_mut();
        if resolved_type_arguments.is_none() {
            let node = type_as_type_reference.node.borrow();
            let type_arguments = match &*node {
                None => vec![],
                Some(node) => match &**node {
                    Node::TypeReferenceNode(type_reference_node) => {
                        let target_as_interface_type =
                            type_as_type_reference.target.as_interface_type();
                        concatenate(
                            target_as_interface_type
                                .maybe_outer_type_parameters()
                                .map_or_else(
                                    || vec![],
                                    |outer_type_parameters| outer_type_parameters.to_owned(),
                                ),
                            self.get_effective_type_arguments(
                                node,
                                target_as_interface_type
                                    .maybe_local_type_parameters()
                                    .unwrap(),
                            ),
                        )
                    }
                    Node::ArrayTypeNode(array_type_node) => unimplemented!(),
                    _ => unimplemented!(),
                },
            };
            if true {
                *resolved_type_arguments = if false {
                    unimplemented!()
                } else {
                    Some(type_arguments)
                };
            } else {
                unimplemented!()
            }
        }
        (*resolved_type_arguments).clone().unwrap()
    }

    pub(super) fn get_type_reference_arity(&self, type_: &Type /*TypeReference*/) -> usize {
        unimplemented!()
    }

    pub(super) fn get_type_from_class_or_interface_reference(
        &self,
        node: &Node,
        symbol: &Symbol,
    ) -> Rc<Type> {
        let type_ =
            self.get_declared_type_of_symbol(&self.get_merged_symbol(Some(symbol)).unwrap());
        let type_as_interface_type = type_.as_interface_type();
        let type_parameters = type_as_interface_type.maybe_type_parameters();
        if let Some(type_parameters) = type_parameters {
            let type_arguments = concatenate(
                type_as_interface_type
                    .maybe_outer_type_parameters()
                    .map_or_else(
                        || vec![],
                        |outer_type_parameters| outer_type_parameters.to_owned(),
                    ),
                self.fill_missing_type_arguments(
                    self.type_arguments_from_type_reference_node(&*node.node_wrapper()),
                    Some(type_parameters),
                    0, // TODO: this is wrong
                    false,
                )
                .unwrap_or_else(|| vec![]),
            );
            return self
                .create_type_reference(&type_, Some(type_arguments))
                .into();
        }
        if self.check_no_type_arguments(node, symbol) {
            type_
        } else {
            unimplemented!()
        }
    }

    pub(super) fn get_type_alias_instantiation<TAliasSymbol: Borrow<Symbol>>(
        &self,
        symbol: &Symbol,
        type_arguments: Option<&[Rc<Type>]>,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_reference_name(
        &self,
        node: &Node, /*TypeReferenceNode*/
    ) -> Option<Rc<Node /*EntityNameOrEntityNameExpression*/>> {
        match node.kind() {
            SyntaxKind::TypeReference => {
                return Some(node.as_type_reference_node().type_name.clone());
            }
            SyntaxKind::ExpressionWithTypeArguments => unimplemented!(),
            _ => (),
        }
        None
    }

    pub(super) fn resolve_type_reference_name(
        &self,
        type_reference: &Node, /*TypeReferenceNode*/
        meaning: SymbolFlags,
        ignore_errors: Option<bool>,
    ) -> Rc<Symbol> {
        let ignore_errors = ignore_errors.unwrap_or(false);
        let name = self.get_type_reference_name(type_reference);
        let name = match name {
            Some(name) => name,
            None => {
                return self.unknown_symbol();
            }
        };
        let symbol = self.resolve_entity_name(
            &*name,
            meaning,
            Some(ignore_errors),
            None,
            Option::<&Node>::None,
        );
        if symbol.is_some() && !Rc::ptr_eq(symbol.as_ref().unwrap(), &self.unknown_symbol()) {
            symbol.unwrap()
        } else if ignore_errors {
            self.unknown_symbol()
        } else {
            unimplemented!()
        }
    }

    pub(super) fn get_type_reference_type(&self, node: &Node, symbol: &Symbol) -> Rc<Type> {
        if ptr::eq(symbol, Rc::as_ptr(&self.unknown_symbol())) {
            unimplemented!()
        }
        if symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return self.get_type_from_class_or_interface_reference(node, symbol);
        }
        let res = self.try_get_declared_type_of_symbol(symbol);
        if let Some(res) = res {
            return if self.check_no_type_arguments(node, symbol) {
                self.get_regular_type_of_literal_type(&res)
            } else {
                unimplemented!()
            };
        }
        unimplemented!()
    }

    pub(super) fn get_conditional_flow_type_of_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn is_jsdoc_type_reference(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn check_no_type_arguments(
        &self,
        node: &Node, /*NodeWithTypeArguments*/
        symbol: &Symbol,
    ) -> bool {
        if let Some(type_arguments) = node.as_has_type_arguments().maybe_type_arguments() {
            unimplemented!()
        }
        true
    }

    pub(super) fn get_type_from_type_reference(
        &self,
        node: &Node, /*TypeReferenceNode*/
    ) -> Rc<Type> {
        let mut symbol: Option<Rc<Symbol>> = None;
        let mut type_: Option<Rc<Type>> = None;
        let meaning = SymbolFlags::Type;
        if type_.is_none() {
            symbol = Some(self.resolve_type_reference_name(node, meaning, None));
            type_ = Some(self.get_type_reference_type(node, &symbol.unwrap()));
        }
        let type_ = type_.unwrap();
        type_
    }

    pub(super) fn type_arguments_from_type_reference_node(
        &self,
        node: &Node, /*NodeWithTypeArguments*/
    ) -> Option<Vec<Rc<Type>>> {
        map(
            node.as_has_type_arguments().maybe_type_arguments(),
            |type_argument, _| self.get_type_from_type_node_(&**type_argument),
        )
    }

    pub(super) fn get_type_of_global_symbol<TSymbolRef: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbolRef>,
    ) -> Rc<Type /*ObjectType*/> {
        unimplemented!()
    }

    pub(super) fn get_global_value_symbol(
        &self,
        name: &__String,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
        self.get_global_symbol(
            name,
            SymbolFlags::Value,
            if report_errors {
                Some(&Diagnostics::Cannot_find_global_value_0)
            } else {
                None
            },
        )
    }

    pub(super) fn get_global_type_symbol(
        &self,
        name: &__String,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
        self.get_global_symbol(
            name,
            SymbolFlags::Type,
            if report_errors {
                Some(&Diagnostics::Cannot_find_global_type_0)
            } else {
                None
            },
        )
    }

    pub(super) fn get_global_symbol(
        &self,
        name: &__String,
        meaning: SymbolFlags,
        diagnostic: Option<&DiagnosticMessage>,
    ) -> Option<Rc<Symbol>> {
        self.resolve_name_(
            Option::<&Node>::None,
            name,
            meaning,
            diagnostic,
            Some(name.clone()),
            false,
            None,
        )
    }

    pub(super) fn get_global_type(
        &self,
        name: &__String,
        arity: usize,
        report_errors: bool,
    ) -> Option<Rc<Type>> {
        let symbol = self.get_global_type_symbol(name, report_errors);
        if true {
            Some(self.get_type_of_global_symbol(symbol))
        } else {
            None
        }
    }

    pub(super) fn get_global_es_symbol_constructor_type_symbol(
        &self,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_global_es_symbol_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_promise_type(&self, report_errors: bool) -> Rc<Type /*GenericType*/> {
        let mut deferred_global_promise_type_ref = self.deferred_global_promise_type.borrow_mut();
        if let Some(deferred_global_promise_type) = deferred_global_promise_type_ref.as_ref() {
            return deferred_global_promise_type.clone();
        }
        *deferred_global_promise_type_ref =
            self.get_global_type(&__String::new("Promise".to_string()), 1, report_errors);
        deferred_global_promise_type_ref.as_ref().map_or_else(
            || self.empty_generic_type(),
            |deferred_global_promise_type| deferred_global_promise_type.clone(),
        )
    }

    pub(super) fn get_global_promise_like_type(
        &self,
        report_errors: bool,
    ) -> Rc<Type /*GenericType*/> {
        unimplemented!()
    }

    pub(super) fn get_global_promise_constructor_symbol(
        &self,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
        let mut deferred_global_promise_constructor_symbol_ref =
            self.deferred_global_promise_constructor_symbol.borrow_mut();
        if let Some(deferred_global_promise_constructor_symbol) =
            deferred_global_promise_constructor_symbol_ref.as_ref()
        {
            return Some(deferred_global_promise_constructor_symbol.clone());
        }
        *deferred_global_promise_constructor_symbol_ref =
            self.get_global_value_symbol(&__String::new("Promise".to_string()), report_errors);
        deferred_global_promise_constructor_symbol_ref.as_ref().map(
            |deferred_global_promise_constructor_symbol| {
                deferred_global_promise_constructor_symbol.clone()
            },
        )
    }

    pub(super) fn get_global_async_iterable_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_async_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_async_iterable_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_async_generator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_iterable_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_iterable_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_generator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_omit_symbol(&self) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_global_big_int_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_iterable_type(&self, iterated_type: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_array_type(
        &self,
        element_type: &Type,
        readonly: Option<bool>,
    ) -> Rc<Type /*ObjectType*/> {
        unimplemented!()
    }

    pub(super) fn get_array_or_tuple_target_type(
        &self,
        node: &Node, /*ArrayTypeNode*/
    ) -> Rc<Type /*GenericType*/> {
        let element_type = self.get_array_element_type_node(node);
        if let Some(element_type) = element_type {
            return self.global_array_type();
        }
        unimplemented!()
    }

    pub(super) fn get_type_from_array_or_tuple_type_node(
        &self,
        node: &Node, /*ArrayTypeNode*/
    ) -> Rc<Type> {
        let node_as_array_type_node = node.as_array_type_node();
        let target = self.get_array_or_tuple_target_type(node);
        if false {
            unimplemented!()
        } else if false {
            unimplemented!()
        } else {
            let element_types =
                vec![self.get_type_from_type_node_(&*node_as_array_type_node.element_type)];
            return self.create_normalized_type_reference(&target, Some(element_types));
        }
    }

    pub(super) fn create_tuple_type(
        &self,
        element_types: &[Rc<Type>],
        element_flags: Option<&[ElementFlags]>,
        readonly: Option<bool>,
        named_member_declarations: Option<&[Rc<Node /*NamedTupleMember | ParameterDeclaration*/>]>,
    ) -> Rc<Type> {
        let readonly = readonly.unwrap_or(false);
        unimplemented!()
    }
}
