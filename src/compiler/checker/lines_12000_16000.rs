#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::convert::TryFrom;
use std::ptr;
use std::rc::Rc;

use crate::{
    filter, get_effective_constraint_of_type_parameter, get_effective_return_type_node,
    get_effective_type_parameter_declarations, is_binding_pattern, is_type_parameter_declaration,
    map_defined, maybe_append_if_unique_rc, node_is_missing, AccessFlags, IndexInfo,
    InterfaceTypeInterface, Signature, SignatureFlags, SignatureKind, SymbolTable, TypePredicate,
    TypePredicateKind, UnionType, __String, binary_search_copy_key, compare_values, concatenate,
    get_name_of_declaration, get_object_flags, map, unescape_leading_underscores,
    BaseUnionOrIntersectionType, DiagnosticMessage, Diagnostics, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeId, TypeInterface, TypeReference, UnionReduction,
};

impl TypeChecker {
    pub(super) fn get_apparent_type(&self, type_: &Type) -> Rc<Type> {
        let t = if type_.flags().intersects(TypeFlags::Instantiable) {
            unimplemented!()
        } else {
            type_.type_wrapper()
        };
        if false {
            unimplemented!()
        } else {
            t
        }
    }

    pub(super) fn get_reduced_apparent_type(&self, type_: &Type) -> Rc<Type> {
        self.get_reduced_type(&self.get_apparent_type(&self.get_reduced_type(type_)))
    }

    pub(super) fn get_reduced_type(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn get_property_of_type_(
        &self,
        type_: &Type,
        name: &__String,
        skip_object_function_property_augment: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(&type_);
            let symbol = (*resolved.as_resolved_type().members())
                .borrow()
                .get(name)
                .map(Clone::clone);
            if let Some(symbol) = symbol {
                if self.symbol_is_value(&symbol) {
                    return Some(symbol);
                }
            }
            return /*self.get_property_of_object_type(self.global_object_type(), name)*/ None;
        }
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            unimplemented!()
        }
        None
    }

    pub(super) fn get_signatures_of_structured_type(
        &self,
        type_: &Type,
        kind: SignatureKind,
    ) -> Vec<Rc<Signature>> {
        if type_.flags().intersects(TypeFlags::StructuredType) {
            let resolved = self.resolve_structured_type_members(type_);
            let resolved = resolved.as_resolved_type();
            return if kind == SignatureKind::Call {
                resolved.call_signatures().clone()
            } else {
                resolved.construct_signatures().clone()
            };
        }
        vec![]
    }

    pub(super) fn get_signatures_of_type(
        &self,
        type_: &Type,
        kind: SignatureKind,
    ) -> Vec<Rc<Signature>> {
        self.get_signatures_of_structured_type(&self.get_reduced_apparent_type(type_), kind)
    }

    pub(super) fn get_index_infos_of_type(&self, type_: &Type) -> Vec<Rc<IndexInfo>> {
        unimplemented!()
    }

    pub(super) fn get_index_info_of_type_(
        &self,
        type_: &Type,
        key_type: &Type,
    ) -> Option<Rc<IndexInfo>> {
        unimplemented!()
    }

    pub(super) fn get_index_type_of_type_(
        &self,
        type_: &Type,
        key_type: &Type,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_applicable_index_info_for_name(
        &self,
        type_: &Type,
        name: &__String,
    ) -> Option<Rc<IndexInfo>> {
        unimplemented!()
    }

    pub(super) fn get_type_parameters_from_declaration(
        &self,
        declaration: &Node, /*DeclarationWithTypeParameters*/
    ) -> Option<Vec<Rc<Type /*<TypeParameter>*/>>> {
        let mut result: Option<Vec<Rc<Type>>> = None;
        for node in get_effective_type_parameter_declarations(declaration) {
            result = Some(maybe_append_if_unique_rc(
                result,
                &self.get_declared_type_of_type_parameter(&node.symbol()),
            ));
        }
        result
    }

    pub(super) fn symbols_to_array(&self, symbols: &SymbolTable) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn try_find_ambient_module_(
        &self,
        module_name: &str,
        with_augmentations: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_optional_parameter_(
        &self,
        node: &Node, /*ParameterDeclaration | JSDocParameterTag | JSDocPropertyTag*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn create_type_predicate(
        &self,
        kind: TypePredicateKind,
        parameter_name: Option<String>,
        parameter_index: Option<usize>,
        type_: Option<Rc<Type>>,
    ) -> TypePredicate {
        TypePredicate {
            kind,
            parameter_name,
            parameter_index,
            type_,
        }
    }

    pub(super) fn fill_missing_type_arguments(
        &self,
        type_arguments: Option<Vec<Rc<Type>>>,
        type_parameters: Option<&[Rc<Type /*TypeParameter*/>]>,
    ) -> Option<Vec<Rc<Type>>> {
        type_arguments.map(|vec| vec.clone())
    }

    pub(super) fn get_signature_from_declaration_(
        &self,
        declaration: &Node, /*SignatureDeclaration | JSDocSignature*/
    ) -> Rc<Signature> {
        let links = self.get_node_links(declaration);
        if (*links).borrow().resolved_signature.is_none() {
            let mut parameters: Vec<Rc<Symbol>> = vec![];
            let mut flags = SignatureFlags::None;
            let mut min_argument_count = 0;
            let mut this_parameter: Option<Rc<Symbol>> = None;
            let mut has_this_parameter = false;

            let declaration_as_signature_declaration = declaration.as_signature_declaration();
            for (i, param) in declaration_as_signature_declaration
                .parameters()
                .iter()
                .enumerate()
            {
                let mut param_symbol = param.symbol();
                let type_ = if false {
                    unimplemented!()
                } else {
                    param.as_has_type().maybe_type()
                };
                if
                /*paramSymbol &&*/
                param_symbol.flags().intersects(SymbolFlags::Property)
                    && !is_binding_pattern(param.as_named_declaration().maybe_name())
                {
                    let resolved_symbol = self.resolve_name_(
                        Some(&**param),
                        param_symbol.escaped_name(),
                        SymbolFlags::Value,
                        None,
                        Option::<Rc<Node>>::None,
                        false,
                        None,
                    );
                    param_symbol = resolved_symbol.unwrap();
                }
                if i == 0 && false {
                    unimplemented!()
                } else {
                    parameters.push(param_symbol);
                }

                if matches!(type_, Some(type_) if type_.kind() == SyntaxKind::LiteralType) {
                    flags |= SignatureFlags::HasLiteralTypes;
                }

                let is_optional_parameter = false;
                if !is_optional_parameter {
                    min_argument_count = parameters.len();
                }
            }

            let class_type: Option<Rc<Type>> = if false { unimplemented!() } else { None };
            let type_parameters = match class_type {
                Some(class_type) => unimplemented!(),
                None => self.get_type_parameters_from_declaration(declaration),
            };
            let resolved_signature = Rc::new(self.create_signature(
                Some(declaration.node_wrapper()),
                type_parameters,
                this_parameter,
                parameters,
                None,
                None,
                min_argument_count,
                flags,
            ));
            links.borrow_mut().resolved_signature = Some(resolved_signature);
        }
        let links = (*links).borrow();
        links.resolved_signature.clone().unwrap()
    }

    pub(super) fn get_signature_of_type_tag(
        &self,
        node: &Node, /*SignatureDeclaration | JSDocSignature*/
    ) -> Option<Rc<Signature>> {
        if !false {
            return None;
        }
        unimplemented!()
    }

    pub(super) fn get_return_type_of_type_tag(
        &self,
        node: &Node, /*SignatureDeclaration | JSDocSignature*/
    ) -> Option<Rc<Type>> {
        let signature = self.get_signature_of_type_tag(node);
        signature.map(|signature| self.get_return_type_of_signature(&signature))
    }

    pub(super) fn resolve_external_module_type_by_literal(
        &self,
        name: &Node, /*StringLiteral*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_this_type_of_signature(&self, signature: &Signature) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_predicate_of_signature(
        &self,
        signature: &Signature,
    ) -> Option<Rc<TypePredicate>> {
        unimplemented!()
    }

    pub(super) fn get_return_type_of_signature(&self, signature: &Signature) -> Rc<Type> {
        if signature.resolved_return_type.borrow().is_none() {
            let mut type_: Rc<Type> = if false {
                unimplemented!()
            } else {
                let signature_declaration = signature.declaration.as_ref().unwrap();
                self.get_return_type_from_annotation(signature_declaration)
                    .unwrap_or_else(|| {
                        if node_is_missing(
                            signature_declaration
                                .maybe_as_function_like_declaration()
                                .and_then(|function_like_declaration| {
                                    function_like_declaration.maybe_body()
                                }),
                        ) {
                            self.any_type()
                        } else {
                            self.get_return_type_from_body(signature_declaration, None)
                        }
                    })
            };
            if signature.flags.intersects(SignatureFlags::IsInnerCallChain) {
                type_ = self.add_optional_type_marker(&type_);
            } else if signature.flags.intersects(SignatureFlags::IsOuterCallChain) {
                type_ = self.get_optional_type_(&type_, None);
            }
            *signature.resolved_return_type.borrow_mut() = Some(type_);
        }
        signature.resolved_return_type.borrow().clone().unwrap()
    }

    pub(super) fn get_return_type_from_annotation(
        &self,
        declaration: &Node, /*SignatureDeclaration | JSDocSignature*/
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_return_type_node(declaration);
        if let Some(type_node) = type_node {
            return Some(self.get_type_from_type_node_(&type_node));
        }
        self.get_return_type_of_type_tag(declaration)
    }

    pub(super) fn get_or_create_type_from_signature(
        &self,
        signature: &Signature,
    ) -> Rc<Type /*ObjectType*/> {
        unimplemented!()
    }

    pub(super) fn get_index_symbol_from_symbol_table(
        &self,
        symbol_table: &SymbolTable,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn create_index_info(
        &self,
        key_type: Rc<Type>,
        type_: Rc<Type>,
        is_readonly: bool,
        declaration: Option<Rc<Node /*IndexSignatureDeclaration*/>>,
    ) -> IndexInfo {
        IndexInfo {
            key_type,
            type_,
            is_readonly,
            declaration,
        }
    }

    pub(super) fn get_constraint_declaration(
        &self,
        type_: &Type, /*TypeParameter*/
    ) -> Option<Rc<Node /*TypeNode*/>> {
        map_defined(
            filter(
                type_
                    .maybe_symbol()
                    .and_then(|symbol| symbol.maybe_declarations().clone())
                    .as_deref(),
                |node: &Rc<Node>| is_type_parameter_declaration(node),
            ),
            |node, _| get_effective_constraint_of_type_parameter(&node),
        )
        .get(0)
        .map(Clone::clone)
    }

    pub(super) fn get_inferred_type_parameter_constraint(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
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

    pub(super) fn create_normalized_type_reference(
        &self,
        target: &Type, /*GenericType*/
        type_arguments: Option<Vec<Rc<Type>>>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn slice_tuple_type(
        &self,
        type_: &Type, /*TupleTypeReference*/
        index: usize,
        end_skip_count: Option<usize>,
    ) -> Rc<Type> {
        let end_skip_count = end_skip_count.unwrap_or(0);
        unimplemented!()
    }

    pub(super) fn get_type_id(&self, type_: &Type) -> TypeId {
        type_.id()
    }

    pub(super) fn contains_type(&self, types: &[Rc<Type>], type_: &Type) -> bool {
        binary_search_copy_key(
            types,
            &type_.type_wrapper(),
            |t, _| Some(self.get_type_id(t)),
            compare_values,
            None,
        ) >= 0
    }

    pub(super) fn add_type_to_union(
        &self,
        type_set: &mut Vec<Rc<Type>>,
        mut includes: TypeFlags,
        type_: &Type,
    ) -> TypeFlags {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::Union) {
            return self.add_types_to_union(
                type_set,
                includes
                    | if self.is_named_union_type(type_) {
                        TypeFlags::Union
                    } else {
                        TypeFlags::None
                    },
                type_.as_union_or_intersection_type_interface().types(),
            );
        }
        if !flags.intersects(TypeFlags::Never) {
            includes |= flags & TypeFlags::IncludesMask;
            if flags.intersects(TypeFlags::Instantiable) {
                includes |= TypeFlags::IncludesInstantiable;
            }
            if false {
                unimplemented!()
            } else {
                let len = type_set.len();
                let index: isize = if len > 0 && type_.id() > type_set[len - 1].id() {
                    !isize::try_from(len).unwrap()
                } else {
                    binary_search_copy_key(
                        type_set,
                        &type_.type_wrapper(),
                        |type_, _| Some(self.get_type_id(type_)),
                        compare_values,
                        None,
                    )
                };
                if index < 0 {
                    type_set.insert(usize::try_from(!index).unwrap(), type_.type_wrapper());
                }
            }
        }
        includes
    }

    pub(super) fn add_types_to_union(
        &self,
        type_set: &mut Vec<Rc<Type>>,
        mut includes: TypeFlags,
        types: &[Rc<Type>],
    ) -> TypeFlags {
        for type_ in types {
            includes = self.add_type_to_union(type_set, includes, &type_);
        }
        includes
    }

    pub(super) fn is_named_union_type(&self, type_: &Type) -> bool {
        false
    }

    pub(super) fn get_union_type(
        &self,
        types: Vec<Rc<Type>>,
        union_reduction: Option<UnionReduction>,
    ) -> Rc<Type> {
        let union_reduction = union_reduction.unwrap_or(UnionReduction::Literal);
        if types.is_empty() {
            return self.never_type();
        }
        if types.len() == 1 {
            return types[0].clone();
        }
        let mut type_set: Vec<Rc<Type>> = vec![];
        let includes = self.add_types_to_union(&mut type_set, TypeFlags::None, &types);
        if union_reduction != UnionReduction::None {}
        let object_flags = (if includes.intersects(TypeFlags::NotPrimitiveUnion) {
            ObjectFlags::None
        } else {
            ObjectFlags::PrimitiveUnion
        }) | (if includes.intersects(TypeFlags::Intersection) {
            ObjectFlags::ContainsIntersections
        } else {
            ObjectFlags::None
        });
        self.get_union_type_from_sorted_list(type_set, object_flags)
    }

    pub(super) fn get_union_type_from_sorted_list(
        &self,
        types: Vec<Rc<Type>>,
        object_flags: ObjectFlags,
    ) -> Rc<Type> {
        let mut type_: Option<Rc<Type>> = None;
        if type_.is_none() {
            let is_boolean = types.len() == 2
                && types[0].flags().intersects(TypeFlags::BooleanLiteral)
                && types[1].flags().intersects(TypeFlags::BooleanLiteral);
            let base_type = self.create_type(if is_boolean {
                TypeFlags::Union | TypeFlags::Boolean
            } else {
                TypeFlags::Union
            });
            let object_flags_to_set =
                object_flags | self.get_propagating_flags_of_types(&types, TypeFlags::Nullable);
            type_ = Some(
                UnionType::new(BaseUnionOrIntersectionType::new(
                    base_type,
                    types,
                    object_flags_to_set,
                ))
                .into(),
            );
            // TODO: also treat union type as intrinsic type with intrinsic_name = "boolean" if
            // is_boolean - should expose maybe_intrinsic_name on UnionType or something?
        }
        type_.unwrap()
    }

    pub(super) fn get_type_from_union_type_node(
        &self,
        node: &Node, /*UnionTypeNode*/
    ) -> Rc<Type> {
        let node_as_union_type_node = node.as_union_type_node();
        let links = self.get_node_links(node);
        let mut links_ref = links.borrow_mut();
        if links_ref.resolved_type.is_none() {
            // let alias_symbol = self.get_alias_symbol_for_type_node(node);
            links_ref.resolved_type = Some(
                self.get_union_type(
                    map(Some(&node_as_union_type_node.types), |type_, _| {
                        self.get_type_from_type_node_(type_)
                    })
                    .unwrap(),
                    Some(UnionReduction::Literal),
                ),
            );
        }
        links_ref.resolved_type.clone().unwrap()
    }

    pub(super) fn get_intersection_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        types: &[Rc<Type>],
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_literal_type_from_property_name(
        &self,
        name: &Node, /*PropertyName*/
    ) -> Rc<Type> {
        if let Node::Identifier(identifier) = name {
            self.get_string_literal_type(&unescape_leading_underscores(&identifier.escaped_text))
        } else {
            unimplemented!()
        }
    }

    pub(super) fn get_literal_type_from_property(
        &self,
        prop: &Symbol,
        include: TypeFlags,
        include_non_public: Option<bool>,
    ) -> Rc<Type> {
        let include_non_public = include_non_public.unwrap_or(false);
        if include_non_public || true {
            let mut type_ = None;
            if type_.is_none() {
                let name = prop
                    .maybe_value_declaration()
                    .and_then(|value_declaration| {
                        get_name_of_declaration(Some(&*value_declaration))
                    });
                type_ = if false {
                    unimplemented!()
                } else if let Some(name) = name {
                    Some(self.get_literal_type_from_property_name(&*name))
                } else {
                    unimplemented!()
                }
            }
            if let Some(type_) = type_ {
                if type_.flags().intersects(include) {
                    return type_;
                }
            }
        }
        unimplemented!()
    }

    pub(super) fn get_property_name_from_index(&self, index_type: &Type) -> Option<__String> {
        if self.is_type_usable_as_property_name(index_type) {
            Some(self.get_property_name_from_type(index_type))
        } else {
            unimplemented!()
        }
    }

    pub(super) fn get_property_type_for_index_type(
        &self,
        original_object_type: &Type,
        object_type: &Type,
        index_type: &Type,
        full_index_type: &Type,
    ) -> Option<Rc<Type>> {
        let prop_name = if false {
            unimplemented!()
        } else {
            self.get_property_name_from_index(index_type)
        };
        if let Some(prop_name) = prop_name {
            let prop = self.get_property_of_type_(object_type, &prop_name, None);
            if let Some(prop) = prop {
                let prop_type = self.get_type_of_symbol(&*prop);
                return if false {
                    unimplemented!()
                } else {
                    Some(prop_type)
                };
            }
        }
        None
    }

    pub(super) fn is_generic_object_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_generic_index_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_this_type_parameter(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_indexed_access_type<
        TAccessNode: Borrow<Node>,
        TAliasSymbol: Borrow<Symbol>,
    >(
        &self,
        object_type: &Type,
        index_type: &Type,
        access_flags: Option<AccessFlags>,
        access_node: Option<
            TAccessNode, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let access_flags = access_flags.unwrap_or(AccessFlags::None);
        unimplemented!()
    }

    pub(super) fn get_indexed_access_type_or_undefined<
        TAccessNode: Borrow<Node>,
        TAliasSymbol: Borrow<Symbol>,
    >(
        &self,
        object_type: &Type,
        index_type: &Type,
        access_flags: Option<AccessFlags>,
        access_node: Option<
            TAccessNode, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Option<Rc<Type>> {
        let access_flags = access_flags.unwrap_or(AccessFlags::None);
        let apparent_object_type = self.get_reduced_apparent_type(object_type);
        self.get_property_type_for_index_type(
            object_type,
            &apparent_object_type,
            index_type,
            index_type,
        )
    }

    pub(super) fn get_true_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_false_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        unimplemented!()
    }
}
