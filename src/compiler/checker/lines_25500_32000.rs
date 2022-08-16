#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, IterationTypeKind, IterationUse,
    MinArgumentCountFlags, ResolveNameNameArg, TypeFacts, WideningKind,
};
use crate::{
    filter, find_ancestor, for_each, get_assignment_declaration_kind, get_containing_function,
    get_immediately_invoked_function_expression, get_source_file_of_node, get_this_parameter,
    index_of_node, is_access_expression, is_binding_element, is_binding_pattern, is_class_like,
    is_computed_non_literal_name, is_defaulted_expando_initializer, is_expression,
    is_function_expression_or_arrow_function, is_function_like, is_identifier, is_import_call,
    is_in_js_file, is_jsx_opening_like_element, is_object_literal_method, is_parameter,
    is_private_identifier, is_property_access_expression, is_static, last_or_undefined,
    walk_up_parenthesized_expressions, AccessFlags, AssignmentDeclarationKind, ContextFlags,
    Debug_, Diagnostics, FunctionFlags, NamedDeclarationInterface, NodeFlags, Number, Signature,
    SignatureFlags, SignatureKind, StringOrRcNode, SymbolFlags, Ternary, UnionReduction, __String,
    create_symbol_table, get_effective_type_annotation_node, get_function_flags, get_object_flags,
    has_initializer, is_object_literal_expression, HasInitializerInterface, InferenceContext, Node,
    NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, Symbol, SymbolInterface, SyntaxKind,
    Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn is_legal_usage_of_super_expression<TContainer: Borrow<Node>>(
        &self,
        is_call_expression: bool,
        container: Option<TContainer>,
    ) -> bool {
        if container.is_none() {
            return false;
        }
        let container = container.unwrap();
        let container = container.borrow();

        if is_call_expression {
            return container.kind() == SyntaxKind::Constructor;
        } else {
            if is_class_like(&container.parent())
                || container.parent().kind() == SyntaxKind::ObjectLiteralExpression
            {
                if is_static(container) {
                    return matches!(
                        container.kind(),
                        SyntaxKind::MethodDeclaration
                            | SyntaxKind::MethodSignature
                            | SyntaxKind::GetAccessor
                            | SyntaxKind::SetAccessor
                            | SyntaxKind::PropertyDeclaration
                            | SyntaxKind::ClassStaticBlockDeclaration
                    );
                } else {
                    return matches!(
                        container.kind(),
                        SyntaxKind::MethodDeclaration
                            | SyntaxKind::MethodSignature
                            | SyntaxKind::GetAccessor
                            | SyntaxKind::SetAccessor
                            | SyntaxKind::PropertyDeclaration
                            | SyntaxKind::PropertySignature
                            | SyntaxKind::Constructor
                    );
                }
            }
        }

        false
    }

    pub(super) fn get_containing_object_literal(
        &self,
        func: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Node /*ObjectLiteralExpression*/>> {
        if matches!(
            func.kind(),
            SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
        ) && func.parent().kind() == SyntaxKind::ObjectLiteralExpression
        {
            Some(func.parent())
        } else if func.kind() == SyntaxKind::FunctionExpression
            && func.parent().kind() == SyntaxKind::PropertyAssignment
        {
            Some(func.parent().parent())
        } else {
            None
        }
    }

    pub(super) fn get_this_type_argument(&self, type_: &Type) -> Option<Rc<Type>> {
        if get_object_flags(type_).intersects(ObjectFlags::Reference)
            && Rc::ptr_eq(&type_.as_type_reference().target, &self.global_this_type())
        {
            self.get_type_arguments(type_).get(0).cloned()
        } else {
            None
        }
    }

    pub(super) fn get_this_type_from_contextual_type(&self, type_: &Type) -> Option<Rc<Type>> {
        self.map_type(
            type_,
            &mut |t: &Type| {
                if t.flags().intersects(TypeFlags::Intersection) {
                    for_each(
                        t.as_union_or_intersection_type_interface().types(),
                        |type_: &Rc<Type>, _| self.get_this_type_argument(type_),
                    )
                } else {
                    self.get_this_type_argument(t)
                }
            },
            None,
        )
    }

    pub(super) fn get_contextual_this_parameter_type(
        &self,
        func: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Type>> {
        if func.kind() == SyntaxKind::ArrowFunction {
            return None;
        }
        if self.is_context_sensitive_function_or_object_literal_method(func) {
            let contextual_signature = self.get_contextual_signature(func);
            if let Some(contextual_signature) = contextual_signature.as_ref() {
                let this_parameter = contextual_signature.this_parameter.as_ref();
                if let Some(this_parameter) = this_parameter {
                    return Some(self.get_type_of_symbol(this_parameter));
                }
            }
        }
        let in_js = is_in_js_file(Some(func));
        if self.no_implicit_this || in_js {
            let containing_literal = self.get_containing_object_literal(func);
            if let Some(containing_literal) = containing_literal.as_ref() {
                let contextual_type =
                    self.get_apparent_type_of_contextual_type(containing_literal, None);
                let mut literal = containing_literal.clone();
                let mut type_ = contextual_type.clone();
                while let Some(type_present) = type_.as_ref() {
                    let this_type = self.get_this_type_from_contextual_type(type_present);
                    if let Some(this_type) = this_type.as_ref() {
                        return Some(
                            self.instantiate_type(
                                this_type,
                                self.get_mapper_from_context(
                                    self.get_inference_context(containing_literal).as_deref(),
                                )
                                .as_ref(),
                            ),
                        );
                    }
                    if literal.parent().kind() != SyntaxKind::PropertyAssignment {
                        break;
                    }
                    literal = literal.parent().parent();
                    type_ = self.get_apparent_type_of_contextual_type(&literal, None);
                }
                return Some(self.get_widened_type(&*if let Some(contextual_type) =
                    contextual_type.as_ref()
                {
                    self.get_non_nullable_type(contextual_type)
                } else {
                    self.check_expression_cached(containing_literal, None)
                }));
            }
            let parent = walk_up_parenthesized_expressions(&func.parent()).unwrap();
            if parent.kind() == SyntaxKind::BinaryExpression {
                let parent_as_binary_expression = parent.as_binary_expression();
                if parent_as_binary_expression.operator_token.kind() == SyntaxKind::EqualsToken {
                    let target = &parent_as_binary_expression.left;
                    if is_access_expression(target) {
                        let expression = target.as_has_expression().expression();
                        if in_js && is_identifier(&expression) {
                            let source_file = get_source_file_of_node(Some(&*parent)).unwrap();
                            if source_file
                                .as_source_file()
                                .maybe_common_js_module_indicator()
                                .is_some()
                                && matches!(
                                    source_file.maybe_symbol().as_ref(),
                                    Some(source_file_symbol) if Rc::ptr_eq(
                                        &self.get_resolved_symbol(&expression),
                                        source_file_symbol,
                                    )
                                )
                            {
                                return None;
                            }
                        }

                        return Some(
                            self.get_widened_type(&self.check_expression_cached(&expression, None)),
                        );
                    }
                }
            }
        }
        None
    }

    pub(super) fn get_contextually_typed_parameter_type(
        &self,
        parameter: &Node, /*ParameterDeclaration*/
    ) -> Option<Rc<Type>> {
        let func = parameter.parent();
        if !self.is_context_sensitive_function_or_object_literal_method(&func) {
            return None;
        }
        let iife = get_immediately_invoked_function_expression(&func);
        let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
        let func_as_function_like_declaration = func.as_function_like_declaration();
        if let Some(iife) = iife.as_ref()
        /*&& iife.arguments*/
        {
            let args = self.get_effective_call_arguments(iife);
            let index_of_parameter = func_as_function_like_declaration
                .parameters()
                .into_iter()
                .position(|param| ptr::eq(&**param, parameter))
                .unwrap();
            if parameter_as_parameter_declaration
                .dot_dot_dot_token
                .is_some()
            {
                return Some(self.get_spread_argument_type(
                    &args,
                    index_of_parameter,
                    args.len(),
                    &self.any_type(),
                    Option::<&InferenceContext>::None,
                    CheckMode::Normal,
                ));
            }
            let links = self.get_node_links(iife);
            let cached = (*links).borrow().resolved_signature.clone();
            links.borrow_mut().resolved_signature = Some(self.any_signature());
            let type_ = if index_of_parameter < args.len() {
                Some(self.get_widened_literal_type(&self.check_expression(
                    &args[index_of_parameter],
                    None,
                    None,
                )))
            } else if parameter_as_parameter_declaration
                .maybe_initializer()
                .is_some()
            {
                None
            } else {
                Some(self.undefined_widening_type())
            };
            links.borrow_mut().resolved_signature = cached;
            return type_;
        }
        let contextual_signature = self.get_contextual_signature(&func);
        if let Some(contextual_signature) = contextual_signature.as_ref() {
            let index = func_as_function_like_declaration
                .parameters()
                .into_iter()
                .position(|param| ptr::eq(&**param, parameter))
                .unwrap()
                - if get_this_parameter(&func).is_some() {
                    1
                } else {
                    0
                };
            return if parameter_as_parameter_declaration
                .dot_dot_dot_token
                .is_some()
                && matches!(
                    last_or_undefined(func_as_function_like_declaration.parameters()),
                    Some(last) if ptr::eq(&**last, parameter)
                ) {
                Some(self.get_rest_type_at_position(contextual_signature, index))
            } else {
                self.try_get_type_at_position(contextual_signature, index)
            };
        }
        None
    }

    pub(super) fn get_contextual_type_for_variable_like_declaration(
        &self,
        declaration: &Node,
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(declaration);
        if let Some(type_node) = type_node.as_ref() {
            return Some(self.get_type_from_type_node_(type_node));
        }
        match declaration.kind() {
            SyntaxKind::Parameter => {
                return self.get_contextually_typed_parameter_type(declaration);
            }
            SyntaxKind::BindingElement => {
                return self.get_contextual_type_for_binding_element(declaration);
            }
            SyntaxKind::PropertyDeclaration => {
                if is_static(declaration) {
                    return self.get_contextual_type_for_static_property_declaration(declaration);
                }
            }
            _ => (),
        }
        None
    }

    pub(super) fn get_contextual_type_for_binding_element(
        &self,
        declaration: &Node, /*BindingElement*/
    ) -> Option<Rc<Type>> {
        let parent = declaration.parent().parent();
        let declaration_as_binding_element = declaration.as_binding_element();
        let name = declaration_as_binding_element
            .property_name
            .clone()
            .unwrap_or_else(|| declaration_as_binding_element.name());
        let parent_type = self
            .get_contextual_type_for_variable_like_declaration(&parent)
            .or_else(|| {
                if parent.kind() != SyntaxKind::BindingElement
                    && parent.as_has_initializer().maybe_initializer().is_some()
                {
                    Some(self.check_declaration_initializer(&parent, Option::<&Type>::None))
                } else {
                    None
                }
            });
        if parent_type.is_none()
            || is_binding_pattern(Some(&*name))
            || is_computed_non_literal_name(&name)
        {
            return None;
        }
        let parent_type = parent_type.unwrap();
        if parent.as_named_declaration().name().kind() == SyntaxKind::ArrayBindingPattern {
            let index = index_of_node(
                &declaration.parent().as_has_elements().elements(),
                declaration,
            );
            if index < 0 {
                return None;
            }
            let index: usize = index.try_into().unwrap();
            return self.get_contextual_type_for_element_expression(Some(&*parent_type), index);
        }
        let name_type = self.get_literal_type_from_property_name(&name);
        if self.is_type_usable_as_property_name(&name_type) {
            let text = self.get_property_name_from_type(&name_type);
            return self.get_type_of_property_of_type_(&parent_type, &text);
        }
        None
    }

    pub(super) fn get_contextual_type_for_static_property_declaration(
        &self,
        declaration: &Node, /*PropertyDeclaration*/
    ) -> Option<Rc<Type>> {
        let parent_type = if is_expression(&declaration.parent()) {
            self.get_contextual_type_(&declaration.parent(), None)
        } else {
            None
        }?;
        self.get_type_of_property_of_contextual_type(
            &parent_type,
            self.get_symbol_of_node(declaration).unwrap().escaped_name(),
        )
    }

    pub(super) fn get_contextual_type_for_initializer_expression(
        &self,
        node: &Node,
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let declaration = node.parent();
        let declaration_as_variable_declaration = declaration.as_variable_declaration();
        if has_initializer(&declaration)
            && matches!(
                declaration_as_variable_declaration.maybe_initializer().as_deref(),
                Some(declaration_initializer) if ptr::eq(
                    node,
                    declaration_initializer
                )
            )
        {
            let result = self.get_contextual_type_for_variable_like_declaration(&declaration);
            if result.is_some() {
                return result;
            }
            if !matches!(
                context_flags,
                Some(context_flags) if context_flags.intersects(ContextFlags::SkipBindingPatterns)
            ) && is_binding_pattern(declaration_as_variable_declaration.maybe_name())
            {
                return Some(self.get_type_from_binding_pattern(
                    &declaration_as_variable_declaration.name(),
                    Some(true),
                    Some(false),
                ));
            }
        }
        None
    }

    pub(super) fn get_contextual_type_for_return_expression(
        &self,
        node: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        let func = get_containing_function(node);
        if let Some(func) = func.as_ref() {
            let contextual_return_type = self.get_contextual_return_type(func);
            if let Some(mut contextual_return_type) = contextual_return_type {
                let function_flags = get_function_flags(Some(&**func));
                if function_flags.intersects(FunctionFlags::Generator) {
                    let use_ = if function_flags.intersects(FunctionFlags::Async) {
                        IterationUse::AsyncGeneratorReturnType
                    } else {
                        IterationUse::GeneratorReturnType
                    };
                    let iteration_types = self.get_iteration_types_of_iterable(
                        &contextual_return_type,
                        use_,
                        Option::<&Node>::None,
                    )?;
                    contextual_return_type = iteration_types.return_type();
                }

                if function_flags.intersects(FunctionFlags::Async) {
                    let contextual_awaited_type = self.map_type(
                        &contextual_return_type,
                        &mut |type_: &Type| {
                            self.get_awaited_type_no_alias(type_, Option::<&Node>::None, None, None)
                        },
                        None,
                    );
                    return contextual_awaited_type
                        .as_ref()
                        .map(|contextual_awaited_type| {
                            self.get_union_type(
                                vec![
                                    contextual_awaited_type.clone(),
                                    self.create_promise_like_type(contextual_awaited_type),
                                ],
                                None,
                                Option::<&Symbol>::None,
                                None,
                                Option::<&Type>::None,
                            )
                        });
                }

                return Some(contextual_return_type);
            }
        }
        None
    }

    pub(super) fn get_contextual_type_for_await_operand(
        &self,
        node: &Node, /*AwaitExpression*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let contextual_type = self.get_contextual_type_(node, context_flags);
        if let Some(contextual_type) = contextual_type.as_ref() {
            let contextual_awaited_type =
                self.get_awaited_type_no_alias(contextual_type, Option::<&Node>::None, None, None);
            return contextual_awaited_type
                .as_ref()
                .map(|contextual_awaited_type| {
                    self.get_union_type(
                        vec![
                            contextual_awaited_type.clone(),
                            self.create_promise_like_type(contextual_awaited_type),
                        ],
                        None,
                        Option::<&Symbol>::None,
                        None,
                        Option::<&Type>::None,
                    )
                });
        }
        None
    }

    pub(super) fn get_contextual_type_for_yield_operand(
        &self,
        node: &Node, /*YieldExpression*/
    ) -> Option<Rc<Type>> {
        let func = get_containing_function(node);
        if let Some(func) = func.as_ref() {
            let function_flags = get_function_flags(Some(&**func));
            let contextual_return_type = self.get_contextual_return_type(func);
            if let Some(contextual_return_type) = contextual_return_type.as_ref() {
                return if node.as_yield_expression().asterisk_token.is_some() {
                    Some(contextual_return_type.clone())
                } else {
                    self.get_iteration_type_of_generator_function_return_type(
                        IterationTypeKind::Yield,
                        contextual_return_type,
                        function_flags.intersects(FunctionFlags::Async),
                    )
                };
            }
        }

        None
    }

    pub(super) fn is_in_parameter_initializer_before_containing_function(
        &self,
        node: &Node,
    ) -> bool {
        let mut in_binding_initializer = false;
        let mut node = node.node_wrapper();
        while let Some(node_parent) = node
            .maybe_parent()
            .as_ref()
            .filter(|node_parent| !is_function_like(Some(&***node_parent)))
        {
            if is_parameter(node_parent)
                && (in_binding_initializer
                    || matches!(
                        node_parent.as_has_initializer().maybe_initializer().as_ref(),
                        Some(node_parent_initializer) if Rc::ptr_eq(
                            node_parent_initializer,
                            &node
                        )
                    ))
            {
                return true;
            }
            if is_binding_element(node_parent)
                && matches!(
                    node_parent.as_has_initializer().maybe_initializer().as_ref(),
                    Some(node_parent_initializer) if Rc::ptr_eq(
                        node_parent_initializer,
                        &node
                    )
                )
            {
                in_binding_initializer = true;
            }

            node = node_parent.clone();
        }

        false
    }

    pub(super) fn get_contextual_iteration_type(
        &self,
        kind: IterationTypeKind,
        function_decl: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Type>> {
        let is_async = get_function_flags(Some(function_decl)).intersects(FunctionFlags::Async);
        let contextual_return_type = self.get_contextual_return_type(function_decl);
        if let Some(contextual_return_type) = contextual_return_type.as_ref() {
            return self.get_iteration_type_of_generator_function_return_type(
                kind,
                contextual_return_type,
                is_async,
            );
        }

        None
    }

    pub(super) fn get_contextual_return_type(
        &self,
        function_decl: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Type>> {
        let return_type = self.get_return_type_from_annotation(function_decl);
        if return_type.is_some() {
            return return_type;
        }
        let signature = self.get_contextual_signature_for_function_like_declaration(function_decl);
        if let Some(signature) = signature
            .as_ref()
            .filter(|signature| !self.is_resolving_return_type_of_signature((*signature).clone()))
        {
            return Some(self.get_return_type_of_signature(signature.clone()));
        }
        let iife = get_immediately_invoked_function_expression(function_decl);
        if let Some(iife) = iife.as_ref() {
            return self.get_contextual_type_(iife, None);
        }
        None
    }

    pub(super) fn get_contextual_type_for_argument(
        &self,
        call_target: &Node, /*CallLikeExpression*/
        arg: &Node,         /*Expression*/
    ) -> Option<Rc<Type>> {
        let args = self.get_effective_call_arguments(call_target);
        let arg_index = args.iter().position(|argument| ptr::eq(&**argument, arg));
        arg_index.map(|arg_index| {
            self.get_contextual_type_for_argument_at_index_(call_target, arg_index)
        })
    }

    pub(super) fn get_contextual_type_for_argument_at_index_(
        &self,
        call_target: &Node, /*CallLikeExpression*/
        arg_index: usize,
    ) -> Rc<Type> {
        if is_import_call(call_target) {
            return if arg_index == 0 {
                self.string_type()
            } else if arg_index == 1 {
                self.get_global_import_call_options_type(false)
            } else {
                self.any_type()
            };
        }

        let signature = if matches!(
            (*self.get_node_links(call_target)).borrow().resolved_signature.as_ref(),
            Some(resolved_signature) if Rc::ptr_eq(
                resolved_signature,
                &self.resolving_signature()
            )
        ) {
            self.resolving_signature()
        } else {
            self.get_resolved_signature_(call_target, None, None)
        };

        if is_jsx_opening_like_element(call_target) && arg_index == 0 {
            return self.get_effective_first_argument_for_jsx_signature(&signature, call_target);
        }
        let rest_index = signature.parameters().len() - 1;
        if signature_has_rest_parameter(&signature) && arg_index >= rest_index {
            self.get_indexed_access_type(
                &self.get_type_of_symbol(&signature.parameters()[rest_index]),
                &self.get_number_literal_type(Number::new((arg_index - rest_index) as f64)),
                Some(AccessFlags::Contextual),
                Option::<&Node>::None,
                Option::<&Symbol>::None,
                None,
            )
        } else {
            self.get_type_at_position(&signature, arg_index)
        }
    }

    pub(super) fn get_contextual_type_for_substitution_expression(
        &self,
        template: &Node,                /*TemplateExpression*/
        substitution_expression: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        if template.parent().kind() == SyntaxKind::TaggedTemplateExpression {
            return self
                .get_contextual_type_for_argument(&template.parent(), substitution_expression);
        }

        None
    }

    pub(super) fn get_contextual_type_for_binary_operand(
        &self,
        node: &Node, /*Expression*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let binary_expression = node.parent();
        let binary_expression_as_binary_expression = binary_expression.as_binary_expression();
        let left = &binary_expression_as_binary_expression.left;
        let operator_token = &binary_expression_as_binary_expression.operator_token;
        let right = &binary_expression_as_binary_expression.right;
        match operator_token.kind() {
            SyntaxKind::EqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::BarBarEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken => {
                if ptr::eq(node, &**right) {
                    self.get_contextual_type_for_assignment_declaration(&binary_expression)
                } else {
                    None
                }
            }
            SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken => {
                let type_ = self.get_contextual_type_(&binary_expression, context_flags);
                if ptr::eq(node, &**right)
                    && match type_.as_ref() {
                        Some(type_) => type_.maybe_pattern().is_some(),
                        None => !is_defaulted_expando_initializer(&binary_expression),
                    }
                {
                    Some(self.get_type_of_expression(left))
                } else {
                    type_
                }
            }
            SyntaxKind::AmpersandAmpersandToken | SyntaxKind::CommaToken => {
                if ptr::eq(node, &**right) {
                    self.get_contextual_type_(&binary_expression, context_flags)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub(super) fn get_symbol_for_expression(
        &self,
        e: &Node, /*Expression*/
    ) -> Option<Rc<Symbol>> {
        if e.maybe_symbol().is_some() {
            return e.maybe_symbol();
        }
        if is_identifier(e) {
            return Some(self.get_resolved_symbol(e));
        }
        if is_property_access_expression(e) {
            let e_as_property_access_expression = e.as_property_access_expression();
            let lhs_type = self.get_type_of_expression(&e_as_property_access_expression.expression);
            return if is_private_identifier(&e_as_property_access_expression.name) {
                self.try_get_private_identifier_property_of_type(
                    &lhs_type,
                    &e_as_property_access_expression.name,
                )
            } else {
                self.get_property_of_type_(
                    &lhs_type,
                    &e_as_property_access_expression
                        .name
                        .as_identifier()
                        .escaped_text,
                    None,
                )
            };
        }
        None
    }

    pub(super) fn try_get_private_identifier_property_of_type(
        &self,
        type_: &Type,
        id: &Node, /*PrivateIdentifier*/
    ) -> Option<Rc<Symbol>> {
        let lexically_scoped_symbol = self.lookup_symbol_for_private_identifier_declaration(
            &id.as_private_identifier().escaped_text,
            id,
        );
        lexically_scoped_symbol
            .as_ref()
            .and_then(|lexically_scoped_symbol| {
                self.get_private_identifier_property_of_type_(type_, lexically_scoped_symbol)
            })
    }

    pub(super) fn get_contextual_type_for_assignment_declaration(
        &self,
        binary_expression: &Node, /*BinaryExpression*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn is_possibly_aliased_this_property(
        &self,
        declaration: &Node, /*BinaryExpression*/
        kind: Option<AssignmentDeclarationKind>,
    ) -> bool {
        let kind = kind.unwrap_or_else(|| get_assignment_declaration_kind(declaration));
        unimplemented!()
    }

    pub(super) fn get_type_of_property_of_contextual_type(
        &self,
        type_: &Type,
        name: &__String,
    ) -> Option<Rc<Type>> {
        self.map_type(
            type_,
            &mut |t| {
                if false {
                    unimplemented!()
                } else if t.flags().intersects(TypeFlags::StructuredType) {
                    let prop = self.get_property_of_type_(t, name, None);
                    if let Some(prop) = prop {
                        return if false {
                            None
                        } else {
                            Some(self.get_type_of_symbol(&*prop))
                        };
                    }
                    return if let Some(found) = Option::<()>::None /*self.find_applicable_index_info(self.get_index_infos_of_structured_type(t), self.get_string_literal_type(unescape_leading_underscores(name)))*/ {
                        unimplemented!()
                    } else {
                        None
                    };
                }
                None
            },
            Some(true),
        )
    }

    pub(super) fn get_contextual_type_for_object_literal_element_(
        &self,
        element: &Node, /*PropertyAssignment*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let object_literal = element.parent();
        // let property_assignment_type = if is_property_assignment(element) {
        // } else {
        //     None
        // };
        // if property_assignment_type.is_some() {
        //     return property_assignment_type;
        // }
        let type_ = self.get_apparent_type_of_contextual_type(&object_literal, context_flags);
        if let Some(type_) = type_ {
            if self.has_bindable_name(element) {
                return self.get_type_of_property_of_contextual_type(
                    &type_,
                    self.get_symbol_of_node(element).unwrap().escaped_name(),
                );
            }
            unimplemented!()
        }
        None
    }

    pub(super) fn get_contextual_type_for_element_expression<TArrayContextualType: Borrow<Type>>(
        &self,
        array_contextual_type: Option<TArrayContextualType>,
        index: usize,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_contextual_type_for_jsx_attribute_(
        &self,
        attribute: &Node, /*JsxAttribute | JsxSpreadAttribute*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn is_possibly_discriminant_value(&self, node: &Node /*Expression*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_apparent_type_of_contextual_type(
        &self,
        node: &Node, /*Expression | MethodDeclaration*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let contextual_type = if false {
            unimplemented!()
        } else {
            self.get_contextual_type_(node, context_flags)
        };
        let instantiated_type =
            self.instantiate_contextual_type(contextual_type, node, context_flags);
        if let Some(instantiated_type) = instantiated_type {
            if !(matches!(context_flags, Some(context_flags) if context_flags.intersects(ContextFlags::NoConstraints))
                && instantiated_type
                    .flags()
                    .intersects(TypeFlags::TypeVariable))
            {
                let apparent_type = self
                    .map_type(
                        &instantiated_type,
                        &mut |type_| Some(self.get_apparent_type(type_)),
                        Some(true),
                    )
                    .unwrap();
                return if apparent_type.flags().intersects(TypeFlags::Union)
                    && is_object_literal_expression(node)
                {
                    unimplemented!()
                } else if false {
                    unimplemented!()
                } else {
                    Some(apparent_type)
                };
            }
        }
        None
    }

    pub(super) fn instantiate_contextual_type<TTypeRef: Borrow<Type>, TNode: NodeInterface>(
        &self,
        contextual_type: Option<TTypeRef>,
        node: &TNode,
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        if false {
            unimplemented!()
        }
        contextual_type.map(|contextual_type| contextual_type.borrow().type_wrapper())
    }

    pub(super) fn get_contextual_type_(
        &self,
        node: &Node, /*Expression*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let parent = node.parent();
        match &*parent {
            Node::VariableDeclaration(_) => {
                self.get_contextual_type_for_initializer_expression(node, context_flags)
            }
            Node::PropertyAssignment(_) => {
                self.get_contextual_type_for_object_literal_element_(node, context_flags)
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn get_inference_context(&self, node: &Node) -> Option<Rc<InferenceContext>> {
        let ancestor = find_ancestor(Some(node), |n: &Node| n.maybe_inference_context().is_some());
        ancestor.map(|ancestor| ancestor.maybe_inference_context().clone().unwrap())
    }

    pub(super) fn get_effective_first_argument_for_jsx_signature(
        &self,
        signature: &Signature,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_intersected_signatures(
        &self,
        signatures: &[Rc<Signature>],
    ) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn get_contextual_call_signature(
        &self,
        type_: &Type,
        node: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Signature>> {
        let signatures = self.get_signatures_of_type(type_, SignatureKind::Call);
        let applicable_by_arity = filter(&signatures, |s| !self.is_arity_smaller(s, node));
        if applicable_by_arity.len() == 1 {
            Some(applicable_by_arity[0].clone())
        } else {
            self.get_intersected_signatures(&applicable_by_arity)
        }
    }

    pub(super) fn is_arity_smaller(
        &self,
        signature: &Signature,
        target: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_contextual_signature_for_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> Option<Rc<Signature>> {
        if is_function_expression_or_arrow_function(node) || is_object_literal_method(node) {
            self.get_contextual_signature(node)
        } else {
            None
        }
    }

    pub(super) fn get_contextual_signature(
        &self,
        node: &Node, /*FunctionExpression | ArrowFunction | MethodDeclaration*/
    ) -> Option<Rc<Signature>> {
        Debug_.assert(
            node.kind() != SyntaxKind::MethodDeclaration || is_object_literal_method(node),
            None,
        );
        let type_tag_signature = self.get_signature_of_type_tag(node);
        if type_tag_signature.is_some() {
            return type_tag_signature;
        }
        let type_ = self.get_apparent_type_of_contextual_type(node, Some(ContextFlags::Signature));
        if type_.is_none() {
            return None;
        }
        let type_ = type_.unwrap();
        if !type_.flags().intersects(TypeFlags::Union) {
            return self.get_contextual_call_signature(&type_, node);
        }
        let mut signature_list: Option<Vec<Rc<Signature>>> = None;
        let types = type_.as_union_or_intersection_type_interface().types();
        for current in types {
            let signature = self.get_contextual_call_signature(current, node);
            if let Some(signature) = signature {
                match signature_list.as_mut() {
                    None => {
                        signature_list = Some(vec![signature]);
                    }
                    Some(signature_list) => {
                        if self.compare_signatures_identical(
                            signature_list[0].clone(),
                            signature.clone(),
                            false,
                            true,
                            true,
                            |a, b| self.compare_types_identical(a, b),
                        ) == Ternary::False
                        {
                            return None;
                        } else {
                            signature_list.push(signature);
                        }
                    }
                }
            }
        }

        signature_list.map(|signature_list| {
            if signature_list.len() == 1 {
                signature_list[0].clone()
            } else {
                Rc::new(self.create_union_signature(&signature_list[0].clone(), signature_list))
            }
        })
    }

    pub(super) fn has_default_value(
        &self,
        node: &Node, /*BindingElement | Expression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_array_literal(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_numeric_literal_name(&self, name: &str) -> bool {
        unimplemented!()
    }

    pub(super) fn check_computed_property_name(
        &self,
        node: &Node, /*ComputedPropertyName*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_object_literal(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) -> Rc<Type> {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        let mut properties_table = create_symbol_table(None);
        let mut properties_array: Vec<Rc<Symbol>> = vec![];

        let object_flags = self.fresh_object_literal_flag;

        for member_decl in &node_as_object_literal_expression.properties {
            let member = self.get_symbol_of_node(&**member_decl).unwrap();
            if member_decl.kind() == SyntaxKind::PropertyAssignment {
            } else {
                unimplemented!()
            }

            if false {
                unimplemented!()
            } else {
                properties_table.insert(member.escaped_name().clone(), member.clone());
            }
            properties_array.push(member);
        }

        let create_object_literal_type = || {
            let result = self.create_anonymous_type(
                Some(node.symbol()),
                Rc::new(RefCell::new(properties_table)),
                vec![],
                vec![],
                vec![], // TODO: this is wrong
            );
            result.set_object_flags(
                result.object_flags()
                    | object_flags
                    | ObjectFlags::ObjectLiteral
                    | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
            result.into()
        };

        create_object_literal_type()
    }

    pub(super) fn is_valid_spread_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_hyphenated_jsx_name(&self, name: &str) -> bool {
        unimplemented!()
    }

    pub(super) fn check_jsx_attribute(
        &self,
        node: &Node, /*JsxAttribute*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_jsx_children(
        &self,
        node: &Node, /*JsxElement | JsxFragment*/
        check_mode: Option<CheckMode>,
    ) -> Vec<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_type<TLocation: Borrow<Node>>(
        &self,
        name: &__String,
        location: Option<TLocation>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_jsx_namespace_at<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_children_property_name(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<__String> {
        unimplemented!()
    }

    pub(super) fn is_known_property(
        &self,
        target_type: &Type,
        name: &__String,
        is_comparing_jsx_attributes: bool,
    ) -> bool {
        if target_type.flags().intersects(TypeFlags::Object) {
            if self
                .get_property_of_object_type(target_type, name)
                .is_some()
                || false
            {
                return true;
            }
        } else if target_type
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
            && self.is_excess_property_check_target(target_type)
        {
            unimplemented!()
        }
        false
    }

    pub(super) fn is_excess_property_check_target(&self, type_: &Type) -> bool {
        (type_.flags().intersects(TypeFlags::Object)
            && !(get_object_flags(type_)
                .intersects(ObjectFlags::ObjectLiteralPatternWithComputedProperties)))
            || type_.flags().intersects(TypeFlags::NonPrimitive)
            || (type_.flags().intersects(TypeFlags::Union) && unimplemented!())
            || (type_.flags().intersects(TypeFlags::Intersection) && unimplemented!())
    }

    pub(super) fn get_declaration_node_flags_from_symbol(&self, s: &Symbol) -> NodeFlags {
        unimplemented!()
    }

    pub(super) fn is_prototype_property(&self, symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn check_non_null_expression(
        &self,
        node: &Node, /*Expression | QualifiedName*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_nullable_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_non_nullable_type_if_needed(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_non_null_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn lookup_symbol_for_private_identifier_declaration(
        &self,
        prop_name: &__String,
        location: &Node,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_symbol_for_private_identifier_expression(
        &self,
        priv_id: &Node, /*PrivateIdentifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_private_identifier_property_of_type_(
        &self,
        left_type: &Type,
        lexically_scoped_identifier: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_this_property_access_in_constructor(
        &self,
        node: &Node, /*ElementAccessExpression | PropertyAccessExpression | QualifiedName*/
        prop: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_unchecked_js_suggestion<TNode: Borrow<Node>, TSuggestion: Borrow<Symbol>>(
        &self,
        node: Option<TNode>,
        suggestion: Option<TSuggestion>,
        exclude_classes: bool,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_in_property_initializer_or_class_static_block(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn type_has_static_property(
        &self,
        prop_name: &__String,
        containing_type: &Type,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_suggested_lib_for_nonexistent_name(
        &self,
        name: ResolveNameNameArg,
    ) -> Option<String> {
        // unimplemented!()
        None
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_jsx_attribute<
        TName: Into<StringOrRcNode>,
    >(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_suggestion_for_nonexistent_property<TName: Into<StringOrRcNode>>(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        // unimplemented!()
        None
    }

    pub(super) fn get_suggestion_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_module(
        &self,
        name: &Node, /*Identifier*/
        target_module: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_suggestion_for_nonexistent_index_signature(
        &self,
        object_type: &Type,
        name: &Node, /*ElementAccessExpression*/
        keyed_type: &Type,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_type_for_nonexistent_string_literal_type(
        &self,
        source: &Type, /*StringLiteralType*/
        target: &Type, /*UnionType*/
    ) -> Option<Rc<Type /*StringLiteralType*/>> {
        unimplemented!()
    }

    pub(super) fn mark_property_as_referenced<TNodeForCheckWriteOnly: Borrow<Node>>(
        &self,
        prop: &Symbol,
        node_for_check_write_only: Option<TNodeForCheckWriteOnly>,
        is_self_type_access: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn is_self_type_access<TParent: Borrow<Symbol>>(
        &self,
        name: &Node, /*Expression | QualifiedName*/
        parent: Option<TParent>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_valid_property_access_(
        &self,
        node: &Node, /*PropertyAccessExpression | QualifiedName | ImportTypeNode*/
        property_name: &__String,
    ) -> bool {
        unimplemented!()
    }

    pub fn is_valid_property_access_for_completions_(
        &self,
        node_in: &Node, /*PropertyAccessExpression | ImportTypeNode | QualifiedName*/
        type_: &Type,
        property: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn accepts_void(&self, t: &Type) -> bool {
        t.flags().intersects(TypeFlags::Void)
    }

    pub(super) fn get_single_call_signature(&self, type_: &Type) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn get_single_call_or_construct_signature(
        &self,
        type_: &Type,
    ) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn instantiate_signature_in_context_of<
        TCompareTypes: FnMut(&Type, &Type, Option<bool>) -> Ternary,
    >(
        &self,
        signature: &Signature,
        contextual_signature: &Signature,
        inference_context: Option<&InferenceContext>,
        compare_types: Option<&mut TCompareTypes>,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn get_resolved_signature_(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates_out_array: Option<&[Rc<Signature>]>,
        check_mode: Option<CheckMode>,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn get_spread_argument_type<TContext: Borrow<InferenceContext>>(
        &self,
        args: &[Rc<Node /*Expression*/>],
        index: usize,
        arg_count: usize,
        rest_type: &Type,
        context: Option<TContext>,
        check_mode: CheckMode,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_effective_call_arguments(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> Vec<Rc<Node /*Expression*/>> {
        unimplemented!()
    }

    pub(super) fn is_js_constructor<TNode: Borrow<Node>>(&self, node: Option<TNode>) -> bool {
        unimplemented!()
    }

    pub(super) fn merge_js_symbols<TSource: Borrow<Symbol>>(
        &self,
        target: &Symbol,
        source: Option<TSource>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_assigned_class_symbol(
        &self,
        decl: &Node, /*Declaration*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_symbol_of_expando(
        &self,
        node: &Node,
        allow_declaration: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_with_synthetic_default_only(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_with_synthetic_default_import_type(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_common_js_require(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_type_of_parameter(&self, symbol: &Symbol) -> Rc<Type> {
        let type_ = self.get_type_of_symbol(symbol);
        if self.strict_null_checks {
            let declaration = symbol.maybe_value_declaration();
            if matches!(declaration.as_ref(), Some(declaration) if has_initializer(&declaration)) {
                return self.get_optional_type_(&type_, None);
            }
        }
        type_
    }

    pub(super) fn get_tuple_element_label(
        &self,
        d: &Node, /*ParameterDeclaration | NamedTupleMember*/
    ) -> __String {
        unimplemented!()
    }

    pub(super) fn get_parameter_name_at_position<TOverrideRestType: Borrow<Type>>(
        &self,
        signature: &Signature,
        pos: usize,
        override_rest_type: Option<TOverrideRestType>,
    ) -> __String {
        unimplemented!()
    }

    pub(super) fn get_type_at_position(&self, signature: &Signature, pos: usize) -> Rc<Type> {
        self.try_get_type_at_position(signature, pos)
            .unwrap_or_else(|| self.any_type())
    }

    pub(super) fn try_get_type_at_position(
        &self,
        signature: &Signature,
        pos: usize,
    ) -> Option<Rc<Type>> {
        let param_count = signature.parameters().len()
            - if signature_has_rest_parameter(signature) {
                1
            } else {
                0
            };
        if pos < param_count {
            return Some(self.get_type_of_parameter(&signature.parameters()[pos]));
        }
        if signature_has_rest_parameter(signature) {
            let rest_type = self.get_type_of_symbol(&signature.parameters()[param_count]);
            let index = pos - param_count;
            unimplemented!()
        }
        None
    }

    pub(super) fn get_rest_type_at_position(&self, signature: &Signature, pos: usize) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_parameter_count(&self, signature: &Signature) -> usize {
        let length = signature.parameters().len();
        if signature_has_rest_parameter(signature) {
            let rest_type = self.get_type_of_symbol(&signature.parameters()[length - 1]);
            if self.is_tuple_type(&rest_type) {
                unimplemented!()
            }
        }
        length
    }

    pub(super) fn get_min_argument_count(
        &self,
        signature: &Signature,
        flags: Option<MinArgumentCountFlags>,
    ) -> usize {
        let strong_arity_for_untyped_js = match flags {
            None => false,
            Some(flags) => flags.intersects(MinArgumentCountFlags::StrongArityForUntypedJS),
        };
        let void_is_non_optional = match flags {
            None => false,
            Some(flags) => flags.intersects(MinArgumentCountFlags::VoidIsNonOptional),
        };
        if void_is_non_optional || signature.maybe_resolved_min_argument_count().is_none() {
            let mut min_argument_count = None;
            if signature_has_rest_parameter(signature) {
                let rest_type = self
                    .get_type_of_symbol(&signature.parameters()[signature.parameters().len() - 1]);
                if self.is_tuple_type(&rest_type) {
                    unimplemented!()
                }
            }
            if min_argument_count.is_none() {
                if !strong_arity_for_untyped_js
                    && signature
                        .flags
                        .intersects(SignatureFlags::IsUntypedSignatureInJSFile)
                {
                    return 0;
                }
                min_argument_count = Some(signature.min_argument_count());
            }
            let mut min_argument_count = min_argument_count.unwrap();
            if void_is_non_optional {
                return min_argument_count;
            }
            let mut i = min_argument_count - 1;
            while i >= 0 {
                let type_ = self.get_type_at_position(signature, i);
                if self
                    .filter_type(&type_, |type_| self.accepts_void(type_))
                    .flags()
                    .intersects(TypeFlags::Never)
                {
                    break;
                }
                min_argument_count = i;
                i -= 1;
            }
            signature.set_resolved_min_argument_count(min_argument_count);
        }
        signature.resolved_min_argument_count()
    }

    pub(super) fn has_effective_rest_parameter(&self, signature: &Signature) -> bool {
        if signature_has_rest_parameter(signature) {
            let rest_type =
                self.get_type_of_symbol(&signature.parameters()[signature.parameters().len() - 1]);
            return !self.is_tuple_type(&rest_type) || unimplemented!();
        }
        false
    }

    pub(super) fn get_effective_rest_type(&self, signature: &Signature) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_non_array_rest_type(&self, signature: &Signature) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_first_parameter_of_signature(
        &self,
        signature: &Signature,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_promise_type(&self, promised_type: &Type) -> Rc<Type> {
        let global_promise_type = self.get_global_promise_type(true);
        if !Rc::ptr_eq(&global_promise_type, &self.empty_generic_type()) {
            let promised_type = self
                .get_awaited_type_no_alias(
                    &self.unwrap_awaited_type(promised_type),
                    Option::<&Node>::None,
                    None,
                    None,
                )
                .unwrap_or_else(|| self.unknown_type());
            return self.create_type_reference(&global_promise_type, Some(vec![promised_type]));
        }

        self.unknown_type()
    }

    pub(super) fn create_promise_like_type(&self, promised_type: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_promise_return_type(
        &self,
        func: &Node, /*FunctionLikeDeclaration | ImportCall*/
        promised_type: &Type,
    ) -> Rc<Type> {
        let promise_type = self.create_promise_type(promised_type);
        if Rc::ptr_eq(&promise_type, &self.unknown_type()) {
            self.error(
                Some(func),
                if is_import_call(func) {
                    &Diagnostics::A_dynamic_import_call_returns_a_Promise_Make_sure_you_have_a_declaration_for_Promise_or_include_ES2015_in_your_lib_option
                } else {
                    &Diagnostics::An_async_function_or_method_must_return_a_Promise_Make_sure_you_have_a_declaration_for_Promise_or_include_ES2015_in_your_lib_option
                },
                None
            );
            return self.error_type();
        } else if self.get_global_promise_constructor_symbol(true).is_none() {
            self.error(
                Some(func),
                if is_import_call(func) {
                    &Diagnostics::A_dynamic_import_call_in_ES5_SlashES3_requires_the_Promise_constructor_Make_sure_you_have_a_declaration_for_the_Promise_constructor_or_include_ES2015_in_your_lib_option
                } else {
                    &Diagnostics::An_async_function_or_method_in_ES5_SlashES3_requires_the_Promise_constructor_Make_sure_you_have_a_declaration_for_the_Promise_constructor_or_include_ES2015_in_your_lib_option
                },
                None
            );
        }

        promise_type
    }

    pub(super) fn get_return_type_from_body(
        &self,
        func: &Node, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let func_as_function_like_declaration = func.as_function_like_declaration();
        if func_as_function_like_declaration.maybe_body().is_none() {
            return self.error_type();
        }
        let func_body = func_as_function_like_declaration.maybe_body().unwrap();

        let function_flags = get_function_flags(Some(func));
        let is_async = function_flags.intersects(FunctionFlags::Async);
        let is_generator = function_flags.intersects(FunctionFlags::Generator);

        let mut return_type: Option<Rc<Type>> = None;
        let mut yield_type: Option<Rc<Type>> = None;
        let mut next_type: Option<Rc<Type>> = None;
        let fallback_return_type = self.void_type();
        if func_body.kind() != SyntaxKind::Block {
            return_type = Some(self.check_expression_cached(
                &func_body,
                check_mode.map(|check_mode| check_mode & !CheckMode::SkipGenericFunctions),
            ));
            if is_async {
                unimplemented!()
            }
        } else if is_generator {
            unimplemented!()
        } else {
            let types = self.check_and_aggregate_return_expression_types(func, check_mode);
            if types.is_none() {
                return if function_flags.intersects(FunctionFlags::Async) {
                    self.create_promise_return_type(func, &self.never_type())
                } else {
                    self.never_type()
                };
            }
            let types = types.unwrap();
            if types.is_empty() {
                return if function_flags.intersects(FunctionFlags::Async) {
                    self.create_promise_return_type(func, &self.void_type())
                } else {
                    self.void_type()
                };
            }

            return_type = Some(self.get_union_type(
                types,
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            ));
        }

        if return_type.is_some() || yield_type.is_some() || next_type.is_some() {
            if let Some(yield_type) = yield_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    yield_type,
                    Some(WideningKind::GeneratorYield),
                );
            }
            if let Some(return_type) = return_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    return_type,
                    Some(WideningKind::FunctionReturn),
                );
            }
            if let Some(next_type) = next_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    next_type,
                    Some(WideningKind::GeneratorNext),
                );
            }

            if matches!(return_type.as_ref(), Some(return_type) if self.is_unit_type(return_type))
                || matches!(yield_type.as_ref(), Some(yield_type) if self.is_unit_type(yield_type))
                || matches!(next_type.as_ref(), Some(next_type) if self.is_unit_type(next_type))
            {
                unimplemented!()
            }

            if let Some(yield_type_present) = yield_type {
                yield_type = Some(self.get_widened_type(&yield_type_present));
            }
            if let Some(return_type_present) = return_type {
                return_type = Some(self.get_widened_type(&return_type_present));
            }
            if let Some(next_type_present) = next_type {
                next_type = Some(self.get_widened_type(&next_type_present));
            }
        }

        if is_generator {
            unimplemented!()
        } else {
            if is_async {
                self.create_promise_type(&return_type.unwrap_or(fallback_return_type))
            } else {
                return_type.unwrap_or(fallback_return_type)
            }
        }
    }

    pub(super) fn get_facts_from_typeof_switch(
        &self,
        start: usize,
        end: usize,
        witnesses: &[String],
        has_default: bool,
    ) -> TypeFacts {
        unimplemented!()
    }

    pub(super) fn is_exhaustive_switch_statement(
        &self,
        node: &Node, /*SwitchStatement*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_and_aggregate_return_expression_types(
        &self,
        func: &Node, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }
}
