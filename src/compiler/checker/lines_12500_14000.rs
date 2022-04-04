#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use crate::{
    filter, first_defined, for_each_child_bool, get_declaration_of_kind,
    get_effective_constraint_of_type_parameter, get_effective_return_type_node,
    get_immediately_invoked_function_expression, get_jsdoc_parameter_tags, get_jsdoc_tags,
    get_jsdoc_type, get_jsdoc_type_tag, has_jsdoc_parameter_tags, has_rest_parameter,
    has_syntactic_modifier, is_binding_pattern, is_constructor_declaration,
    is_constructor_type_node, is_function_like, is_function_like_declaration, is_in_js_file,
    is_jsdoc_construct_signature, is_jsdoc_parameter_tag, is_jsdoc_signature,
    is_jsdoc_variadic_type, is_part_of_type_node, is_rest_parameter, is_type_parameter_declaration,
    is_type_predicate_node, is_value_signature_declaration, last_or_undefined, length, map_defined,
    node_is_missing, node_starts_new_lexical_environment, CheckFlags, Debug_, ElementFlags,
    HasInitializerInterface, IndexInfo, InterfaceTypeInterface, InternalSymbolName, ModifierFlags,
    NodeArray, NodeCheckFlags, ReadonlyTextRange, Signature, SignatureFlags, SymbolTable,
    TransientSymbolInterface, TypePredicate, __String, concatenate, get_object_flags, map,
    DiagnosticMessage, Diagnostics, Node, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface,
    Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeReference,
};

impl TypeChecker {
    pub(super) fn fill_missing_type_arguments(
        &self,
        type_arguments: Option<Vec<Rc<Type>>>,
        type_parameters: Option<&[Rc<Type /*TypeParameter*/>]>,
        min_type_argument_count: usize,
        is_java_script_implicit_any: bool,
    ) -> Option<Vec<Rc<Type>>> {
        let num_type_parameters = length(type_parameters);
        if num_type_parameters == 0 {
            return Some(vec![]);
        }
        let type_parameters = type_parameters.unwrap();
        let num_type_arguments = length(type_arguments.as_deref());
        if is_java_script_implicit_any
            || num_type_arguments >= min_type_argument_count
                && num_type_arguments <= num_type_parameters
        {
            let mut result = type_arguments
                .as_ref()
                .map_or_else(|| vec![], |type_arguments| type_arguments.clone());
            for i in num_type_arguments..num_type_parameters {
                result[i] = self.error_type();
            }
            let base_default_type =
                self.get_default_type_argument_type(is_java_script_implicit_any);
            for i in num_type_arguments..num_type_parameters {
                let mut default_type = self.get_default_from_type_parameter_(&type_parameters[i]);
                if is_java_script_implicit_any
                    && matches!(
                        default_type.as_ref(),
                        Some(default_type) if self.is_type_identical_to(default_type, &self.unknown_type()) || self.is_type_identical_to(default_type, &self.empty_object_type())
                    )
                {
                    default_type = Some(self.any_type());
                }
                result[i] =
                    if let Some(default_type) = default_type {
                        self.instantiate_type(
                            Some(default_type),
                            Some(&self.create_type_mapper(
                                type_parameters.to_owned(),
                                Some(result.clone()),
                            )),
                        )
                        .unwrap()
                    } else {
                        base_default_type.clone()
                    };
            }
            result.truncate(type_parameters.len());
            return Some(result);
        }
        type_arguments.map(|type_arguments| type_arguments.clone())
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
            let iife = get_immediately_invoked_function_expression(declaration);
            let is_js_construct_signature = is_jsdoc_construct_signature(declaration);
            let is_untyped_signature_in_js_file = iife.is_none()
                && is_in_js_file(Some(declaration))
                && is_value_signature_declaration(declaration)
                && !has_jsdoc_parameter_tags(declaration)
                && get_jsdoc_type(declaration).is_none();
            if is_untyped_signature_in_js_file {
                flags |= SignatureFlags::IsUntypedSignatureInJSFile;
            }

            let declaration_as_signature_declaration = declaration.as_signature_declaration();
            for (i, param) in declaration_as_signature_declaration
                .parameters()
                .iter()
                .enumerate()
                .skip(if is_js_construct_signature { 1 } else { 0 })
            {
                let mut param_symbol = param.maybe_symbol();
                let type_ = if is_jsdoc_parameter_tag(param) {
                    param
                        .as_jsdoc_property_like_tag()
                        .type_expression
                        .as_ref()
                        .map(|type_expression| {
                            type_expression.as_jsdoc_type_expression().type_.clone()
                        })
                } else {
                    param.as_has_type().maybe_type()
                };
                if matches!(
                    param_symbol.as_ref(),
                    Some(param_symbol) if param_symbol.flags().intersects(SymbolFlags::Property)
                ) && !is_binding_pattern(param.as_named_declaration().maybe_name())
                {
                    let resolved_symbol = self.resolve_name_(
                        Some(&**param),
                        param_symbol.as_ref().unwrap().escaped_name(),
                        SymbolFlags::Value,
                        None,
                        Option::<Rc<Node>>::None,
                        false,
                        None,
                    );
                    param_symbol = resolved_symbol;
                }
                let param_symbol = param_symbol.unwrap();
                if i == 0 && param_symbol.escaped_name() == &InternalSymbolName::This() {
                    has_this_parameter = true;
                    this_parameter = param.maybe_symbol();
                } else {
                    parameters.push(param_symbol);
                }

                if matches!(type_.as_ref(), Some(type_) if type_.kind() == SyntaxKind::LiteralType)
                {
                    flags |= SignatureFlags::HasLiteralTypes;
                }

                let is_optional_parameter = self.is_optional_jsdoc_property_like_tag(param)
                    || {
                        let param_as_parameter_declaration = param.as_parameter_declaration();
                        param_as_parameter_declaration.maybe_initializer().is_some()
                            || param_as_parameter_declaration.question_token.is_some()
                            || is_rest_parameter(param)
                            || matches!(
                                iife.as_ref(),
                                Some(iife) if parameters.len() > iife.as_call_expression().arguments.len()
                            ) && type_.is_none()
                            || self.is_jsdoc_optional_parameter(param)
                    };
                if !is_optional_parameter {
                    min_argument_count = parameters.len();
                }
            }

            if matches!(
                declaration.kind(),
                SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
            ) && self.has_bindable_name(declaration)
                && (!has_this_parameter || this_parameter.is_none())
            {
                let other_kind = if declaration.kind() == SyntaxKind::GetAccessor {
                    SyntaxKind::SetAccessor
                } else {
                    SyntaxKind::GetAccessor
                };
                let other = get_declaration_of_kind(
                    &self.get_symbol_of_node(declaration).unwrap(),
                    other_kind,
                );
                if let Some(other) = other {
                    this_parameter = self.get_annotated_accessor_this_parameter(&other);
                }
            }

            let class_type: Option<Rc<Type>> = if declaration.kind() == SyntaxKind::Constructor {
                Some(
                    self.get_declared_type_of_class_or_interface(
                        &self
                            .get_merged_symbol(declaration.parent().maybe_symbol())
                            .unwrap(),
                    ),
                )
            } else {
                None
            };
            let type_parameters = match class_type {
                Some(class_type) => class_type
                    .as_interface_type()
                    .maybe_local_type_parameters()
                    .map(ToOwned::to_owned),
                None => self.get_type_parameters_from_declaration(declaration),
            };
            if has_rest_parameter(declaration)
                || is_in_js_file(Some(declaration))
                    && self.maybe_add_js_synthetic_rest_parameter(declaration, &mut parameters)
            {
                flags |= SignatureFlags::HasRestParameter;
            }
            if is_constructor_type_node(declaration)
                && has_syntactic_modifier(declaration, ModifierFlags::Abstract)
                || is_constructor_declaration(declaration)
                    && has_syntactic_modifier(&declaration.parent(), ModifierFlags::Abstract)
            {
                flags |= SignatureFlags::Abstract;
            }
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

    pub(super) fn maybe_add_js_synthetic_rest_parameter(
        &self,
        declaration: &Node, /*SignatureDeclaration | JSDocSignature*/
        parameters: &mut Vec<Rc<Symbol>>,
    ) -> bool {
        if is_jsdoc_signature(declaration) || !self.contains_arguments_reference(declaration) {
            return false;
        }
        let last_param = last_or_undefined(declaration.as_signature_declaration().parameters());
        let last_param_tags = if let Some(last_param) = last_param {
            get_jsdoc_parameter_tags(last_param)
        } else {
            get_jsdoc_tags(declaration)
                .into_iter()
                .filter(|tag: &Rc<Node>| is_jsdoc_parameter_tag(tag))
                .collect()
        };
        let last_param_variadic_type = first_defined(&last_param_tags, |p: &Rc<Node>, _| {
            p.as_jsdoc_property_like_tag()
                .type_expression
                .as_ref()
                .filter(|type_expression| {
                    is_jsdoc_variadic_type(&type_expression.as_jsdoc_type_expression().type_)
                })
                .map(|type_expression| type_expression.as_jsdoc_type_expression().type_.clone())
        });
        let synthetic_args_symbol: Rc<Symbol> = self
            .create_symbol(
                SymbolFlags::Variable,
                __String::new("args".to_owned()),
                Some(CheckFlags::RestParameter),
            )
            .into();
        synthetic_args_symbol
            .as_transient_symbol()
            .symbol_links()
            .borrow_mut()
            .type_ = Some(
            if let Some(last_param_variadic_type) = last_param_variadic_type.as_ref() {
                self.create_array_type(
                    &self.get_type_from_type_node_(
                        last_param_variadic_type
                            .as_base_jsdoc_unary_type()
                            .type_
                            .as_ref()
                            .unwrap(),
                    ),
                    None,
                )
            } else {
                self.any_array_type()
            },
        );
        if let Some(last_param_variadic_type) = last_param_variadic_type {
            parameters.pop();
        };
        parameters.push(synthetic_args_symbol);
        true
    }

    pub(super) fn get_signature_of_type_tag(
        &self,
        node: &Node, /*SignatureDeclaration | JSDocSignature*/
    ) -> Option<Rc<Signature>> {
        if !(is_in_js_file(Some(node)) && is_function_like_declaration(node)) {
            return None;
        }
        let type_tag = get_jsdoc_type_tag(node);
        type_tag
            .and_then(|type_tag| {
                type_tag
                    .as_base_jsdoc_type_like_tag()
                    .type_expression
                    .clone()
            })
            .and_then(|type_expression| {
                self.get_single_call_signature(&self.get_type_from_type_node_(&type_expression))
            })
    }

    pub(super) fn get_return_type_of_type_tag(
        &self,
        node: &Node, /*SignatureDeclaration | JSDocSignature*/
    ) -> Option<Rc<Type>> {
        let signature = self.get_signature_of_type_tag(node);
        signature.map(|signature| self.get_return_type_of_signature(&signature))
    }

    pub(super) fn contains_arguments_reference(
        &self,
        declaration: &Node, /*SignatureDeclaration*/
    ) -> bool {
        let links = self.get_node_links(declaration);
        if (*links).borrow().contains_arguments_reference.is_none() {
            let contains_arguments_reference = if (*links)
                .borrow()
                .flags
                .intersects(NodeCheckFlags::CaptureArguments)
            {
                true
            } else {
                self.contains_arguments_reference_traverse(
                    declaration.as_function_like_declaration().maybe_body(),
                )
            };
            links.borrow_mut().contains_arguments_reference = Some(contains_arguments_reference);
        }
        let ret = (*links).borrow().contains_arguments_reference.unwrap();
        ret
    }

    pub(super) fn contains_arguments_reference_traverse<TNode: Borrow<Node>>(
        &self,
        node: Option<TNode>,
    ) -> bool {
        if node.is_none() {
            return false;
        }
        let node = node.unwrap();
        let node = node.borrow();
        match node.kind() {
            SyntaxKind::Identifier => {
                &node.as_identifier().escaped_text == self.arguments_symbol().escaped_name()
                    && matches!(
                        self.get_referenced_value_symbol(node, None),
                        Some(symbol) if Rc::ptr_eq(&symbol, &self.arguments_symbol())
                    )
            }

            SyntaxKind::PropertyDeclaration
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor => {
                let name = node.as_named_declaration().name();
                name.kind() == SyntaxKind::ComputedPropertyName
                    && self.contains_arguments_reference_traverse(Some(name))
            }

            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => self
                .contains_arguments_reference_traverse(Some(node.as_has_expression().expression())),

            _ => {
                !node_starts_new_lexical_environment(node)
                    && !is_part_of_type_node(node)
                    && for_each_child_bool(
                        node,
                        |child| self.contains_arguments_reference_traverse(Some(child)),
                        Option::<fn(&NodeArray) -> bool>::None,
                    )
            }
        }
    }

    pub(super) fn get_signatures_of_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
    ) -> Vec<Rc<Signature>> {
        if symbol.is_none() {
            return vec![];
        }
        let symbol = symbol.unwrap();
        let symbol = symbol.borrow();
        let symbol_declarations = symbol.maybe_declarations();
        if symbol_declarations.is_none() {
            return vec![];
        }
        let symbol_declarations = symbol_declarations.as_ref().unwrap();
        let mut result: Vec<Rc<Signature>> = vec![];
        for (i, decl) in symbol_declarations.iter().enumerate() {
            if !is_function_like(Some(&**decl)) {
                continue;
            }
            if i > 0 && decl.as_function_like_declaration().maybe_body().is_some() {
                let previous = &symbol_declarations[i - 1];
                if Rc::ptr_eq(&decl.parent(), &previous.parent())
                    && decl.kind() == previous.kind()
                    && decl.pos() == previous.end()
                {
                    continue;
                }
            }
            result.push(self.get_signature_from_declaration_(decl));
        }
        result
    }

    pub(super) fn resolve_external_module_type_by_literal(
        &self,
        name: &Node, /*StringLiteral*/
    ) -> Rc<Type> {
        let module_sym = self.resolve_external_module_name_(name, name, None);
        if let Some(module_sym) = module_sym {
            let resolved_module_symbol =
                self.resolve_external_module_symbol(Some(module_sym), None);
            if let Some(resolved_module_symbol) = resolved_module_symbol {
                return self.get_type_of_symbol(&resolved_module_symbol);
            }
        }

        self.any_type()
    }

    pub(super) fn get_this_type_of_signature(&self, signature: &Signature) -> Option<Rc<Type>> {
        signature
            .this_parameter
            .as_ref()
            .map(|this_parameter| self.get_type_of_symbol(this_parameter))
    }

    pub(super) fn get_type_predicate_of_signature(
        &self,
        signature: &Signature,
    ) -> Option<Rc<TypePredicate>> {
        if signature.maybe_resolved_type_predicate().is_none() {
            if let Some(signature_target) = signature.target.as_ref() {
                let target_type_predicate = self.get_type_predicate_of_signature(signature_target);
                *signature.maybe_resolved_type_predicate() =
                    Some(if let Some(target_type_predicate) = target_type_predicate {
                        Rc::new(self.instantiate_type_predicate(
                            &target_type_predicate,
                            signature.mapper.as_ref().unwrap(),
                        ))
                    } else {
                        self.no_type_predicate()
                    });
            } else if let Some(signature_composite_signatures) =
                signature.composite_signatures.as_ref()
            {
                *signature.maybe_resolved_type_predicate() = Some(
                    self.get_union_or_intersection_type_predicate(
                        signature_composite_signatures,
                        signature.composite_kind,
                    )
                    .map_or_else(|| self.no_type_predicate(), Rc::new),
                );
            } else {
                let type_ = signature
                    .declaration
                    .as_ref()
                    .and_then(|declaration| get_effective_return_type_node(declaration));
                let mut jsdoc_predicate: Option<Rc<TypePredicate>> = None;
                if type_.is_none() && is_in_js_file(signature.declaration.as_deref()) {
                    let jsdoc_signature =
                        self.get_signature_of_type_tag(signature.declaration.as_ref().unwrap());
                    if let Some(jsdoc_signature) = jsdoc_signature
                        .filter(|jsdoc_signature| !ptr::eq(signature, &**jsdoc_signature))
                    {
                        jsdoc_predicate = self.get_type_predicate_of_signature(&jsdoc_signature);
                    }
                }
                *signature.maybe_resolved_type_predicate() = Some(
                    if let Some(type_) = type_.filter(|type_| is_type_predicate_node(type_)) {
                        Rc::new(
                            self.create_type_predicate_from_type_predicate_node(&type_, signature),
                        )
                    } else {
                        jsdoc_predicate.unwrap_or_else(|| self.no_type_predicate())
                    },
                );
            }
            Debug_.assert(signature.maybe_resolved_type_predicate().is_some(), None);
        }
        if matches!(
            signature.maybe_resolved_type_predicate().as_ref(),
            Some(resolved_type_predicate) if Rc::ptr_eq(resolved_type_predicate, &self.no_type_predicate())
        ) {
            None
        } else {
            signature.maybe_resolved_type_predicate().clone()
        }
    }

    pub(super) fn create_type_predicate_from_type_predicate_node(
        &self,
        node: &Node, /*TypePredicateNode*/
        signature: &Signature,
    ) -> TypePredicate {
        unimplemented!()
    }

    pub(super) fn get_return_type_of_signature(&self, signature: &Signature) -> Rc<Type> {
        if signature.maybe_resolved_return_type().is_none() {
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
            *signature.maybe_resolved_return_type() = Some(type_);
        }
        signature.maybe_resolved_return_type().clone().unwrap()
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

    pub(super) fn get_signature_instantiation(
        &self,
        signature: &Signature,
        type_arguments: Option<&[Rc<Type>]>,
        is_javascript: bool,
        inferred_type_parameters: Option<&[Rc<Type /*TypeParameter*/>]>,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn create_signature_instantiation(
        &self,
        signature: &Signature,
        type_arguments: Option<&[Rc<Type>]>,
    ) -> Signature {
        unimplemented!()
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

    pub(super) fn get_index_infos_of_symbol(&self, symbol: &Symbol) -> Vec<Rc<IndexInfo>> {
        unimplemented!()
    }

    pub(super) fn get_index_infos_of_index_symbol(
        &self,
        index_symbol: &Symbol,
    ) -> Vec<Rc<IndexInfo>> {
        unimplemented!()
    }

    pub(super) fn is_valid_index_key_type(&self, type_: &Type) -> bool {
        unimplemented!()
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
