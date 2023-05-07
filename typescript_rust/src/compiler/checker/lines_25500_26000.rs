use gc::Gc;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;
use std::{borrow::Borrow, io};

use super::{signature_has_rest_parameter, CheckMode, IterationTypeKind, IterationUse};
use crate::{
    for_each, get_containing_function, get_effective_type_annotation_node, get_function_flags,
    get_immediately_invoked_function_expression, get_object_flags, get_source_file_of_node,
    get_this_parameter, has_initializer, index_of_node, is_access_expression, is_binding_element,
    is_binding_pattern, is_computed_non_literal_name, is_defaulted_expando_initializer,
    is_expression, is_function_like, is_identifier, is_import_call, is_in_js_file,
    is_jsx_opening_like_element, is_parameter, is_private_identifier,
    is_property_access_expression, is_static, last_or_undefined, maybe_is_class_like,
    return_ok_none_if_none, walk_up_parenthesized_expressions, AccessFlags, ContextFlags,
    FunctionFlags, HasInitializerInterface, InferenceContext, NamedDeclarationInterface, Node,
    NodeInterface, Number, ObjectFlags, OptionTry, Symbol, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface,
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
            if maybe_is_class_like(container.maybe_parent())
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
    ) -> Option<Gc<Node /*ObjectLiteralExpression*/>> {
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

    pub(super) fn get_this_type_argument(&self, type_: &Type) -> Option<Gc<Type>> {
        if get_object_flags(type_).intersects(ObjectFlags::Reference)
            && Gc::ptr_eq(&type_.as_type_reference().target, &self.global_this_type())
        {
            self.get_type_arguments(type_).get(0).cloned()
        } else {
            None
        }
    }

    pub(super) fn get_this_type_from_contextual_type(&self, type_: &Type) -> Option<Gc<Type>> {
        self.map_type(
            type_,
            &mut |t: &Type| {
                if t.flags().intersects(TypeFlags::Intersection) {
                    for_each(
                        t.as_union_or_intersection_type_interface().types(),
                        |type_: &Gc<Type>, _| self.get_this_type_argument(type_),
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
    ) -> io::Result<Option<Gc<Type>>> {
        if func.kind() == SyntaxKind::ArrowFunction {
            return Ok(None);
        }
        if self.is_context_sensitive_function_or_object_literal_method(func)? {
            let contextual_signature = self.get_contextual_signature(func);
            if let Some(contextual_signature) = contextual_signature.as_ref() {
                let this_parameter = contextual_signature.maybe_this_parameter();
                if let Some(this_parameter) = this_parameter.as_ref() {
                    return Ok(Some(self.get_type_of_symbol(this_parameter)?));
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
                        return Ok(Some(self.instantiate_type(
                            this_type,
                            self.get_mapper_from_context(
                                self.get_inference_context(containing_literal).as_deref(),
                            ),
                        )?));
                    }
                    if literal.parent().kind() != SyntaxKind::PropertyAssignment {
                        break;
                    }
                    literal = literal.parent().parent();
                    type_ = self.get_apparent_type_of_contextual_type(&literal, None);
                }
                return Ok(Some(self.get_widened_type(
                    &*if let Some(contextual_type) = contextual_type.as_ref() {
                        self.get_non_nullable_type(contextual_type)?
                    } else {
                        self.check_expression_cached(containing_literal, None)?
                    },
                )));
            }
            let parent = walk_up_parenthesized_expressions(&func.parent()).unwrap();
            if parent.kind() == SyntaxKind::BinaryExpression {
                let parent_as_binary_expression = parent.as_binary_expression();
                if parent_as_binary_expression.operator_token.kind() == SyntaxKind::EqualsToken {
                    let target = &parent_as_binary_expression.left;
                    if is_access_expression(target) {
                        let expression = target.as_has_expression().expression();
                        if in_js && is_identifier(&expression) {
                            let source_file = get_source_file_of_node(&parent);
                            if source_file
                                .as_source_file()
                                .maybe_common_js_module_indicator()
                                .is_some()
                                && matches!(
                                    source_file.maybe_symbol().as_ref(),
                                    Some(source_file_symbol) if Gc::ptr_eq(
                                        &self.get_resolved_symbol(&expression)?,
                                        source_file_symbol,
                                    )
                                )
                            {
                                return Ok(None);
                            }
                        }

                        return Ok(Some(self.get_widened_type(
                            &*self.check_expression_cached(&expression, None)?,
                        )));
                    }
                }
            }
        }
        Ok(None)
    }

    pub(super) fn get_contextually_typed_parameter_type(
        &self,
        parameter: &Node, /*ParameterDeclaration*/
    ) -> io::Result<Option<Gc<Type>>> {
        let func = parameter.parent();
        if !self.is_context_sensitive_function_or_object_literal_method(&func)? {
            return Ok(None);
        }
        let iife = get_immediately_invoked_function_expression(&func);
        let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
        let func_as_function_like_declaration = func.as_function_like_declaration();
        if let Some(iife) = iife.as_ref()
        /*&& iife.arguments*/
        {
            let args = self.get_effective_call_arguments(iife)?;
            let index_of_parameter = func_as_function_like_declaration
                .parameters()
                .into_iter()
                .position(|param| ptr::eq(&**param, parameter))
                .unwrap();
            if parameter_as_parameter_declaration
                .dot_dot_dot_token
                .is_some()
            {
                return Ok(Some(self.get_spread_argument_type(
                    &args,
                    index_of_parameter,
                    args.len(),
                    &self.any_type(),
                    None,
                    CheckMode::Normal,
                )?));
            }
            let links = self.get_node_links(iife);
            let cached = (*links).borrow().resolved_signature.clone();
            links.borrow_mut().resolved_signature = Some(self.any_signature());
            let type_ = if index_of_parameter < args.len() {
                Some(self.get_widened_literal_type(&*self.check_expression(
                    &args[index_of_parameter],
                    None,
                    None,
                )?)?)
            } else if parameter_as_parameter_declaration
                .maybe_initializer()
                .is_some()
            {
                None
            } else {
                Some(self.undefined_widening_type())
            };
            links.borrow_mut().resolved_signature = cached;
            return Ok(type_);
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
            return Ok(
                if parameter_as_parameter_declaration
                    .dot_dot_dot_token
                    .is_some()
                    && matches!(
                        last_or_undefined(&func_as_function_like_declaration.parameters()),
                        Some(last) if ptr::eq(&**last, parameter)
                    )
                {
                    Some(self.get_rest_type_at_position(contextual_signature, index))
                } else {
                    self.try_get_type_at_position(contextual_signature, index)
                },
            );
        }
        Ok(None)
    }

    pub(super) fn get_contextual_type_for_variable_like_declaration(
        &self,
        declaration: &Node,
    ) -> io::Result<Option<Gc<Type>>> {
        let type_node = get_effective_type_annotation_node(declaration);
        if let Some(type_node) = type_node.as_ref() {
            return Ok(Some(self.get_type_from_type_node_(type_node)?));
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
        Ok(None)
    }

    pub(super) fn get_contextual_type_for_binding_element(
        &self,
        declaration: &Node, /*BindingElement*/
    ) -> io::Result<Option<Gc<Type>>> {
        let parent = declaration.parent().parent();
        let declaration_as_binding_element = declaration.as_binding_element();
        let name = declaration_as_binding_element
            .property_name
            .clone()
            .unwrap_or_else(|| declaration_as_binding_element.name());
        let parent_type = self
            .get_contextual_type_for_variable_like_declaration(&parent)?
            .try_or_else(|| -> io::Result<_> {
                Ok(
                    if parent.kind() != SyntaxKind::BindingElement
                        && parent.as_has_initializer().maybe_initializer().is_some()
                    {
                        Some(self.check_declaration_initializer(&parent, Option::<&Type>::None)?)
                    } else {
                        None
                    },
                )
            })?;
        if parent_type.is_none()
            || is_binding_pattern(Some(&*name))
            || is_computed_non_literal_name(&name)
        {
            return Ok(None);
        }
        let parent_type = parent_type.unwrap();
        if parent.as_named_declaration().name().kind() == SyntaxKind::ArrayBindingPattern {
            let index = index_of_node(
                &declaration.parent().as_has_elements().elements(),
                declaration,
            );
            if index < 0 {
                return Ok(None);
            }
            let index: usize = index.try_into().unwrap();
            return self.get_contextual_type_for_element_expression(Some(&*parent_type), index);
        }
        let name_type = self.get_literal_type_from_property_name(&name)?;
        if self.is_type_usable_as_property_name(&name_type) {
            let text = self.get_property_name_from_type(&name_type);
            return self.get_type_of_property_of_type_(&parent_type, &text);
        }
        Ok(None)
    }

    pub(super) fn get_contextual_type_for_static_property_declaration(
        &self,
        declaration: &Node, /*PropertyDeclaration*/
    ) -> io::Result<Option<Gc<Type>>> {
        let parent_type = return_ok_none_if_none!(if is_expression(&declaration.parent()) {
            self.get_contextual_type_(&declaration.parent(), None)?
        } else {
            None
        });
        self.get_type_of_property_of_contextual_type(
            &parent_type,
            self.get_symbol_of_node(declaration).unwrap().escaped_name(),
        )
    }

    pub(super) fn get_contextual_type_for_initializer_expression(
        &self,
        node: &Node,
        context_flags: Option<ContextFlags>,
    ) -> io::Result<Option<Gc<Type>>> {
        let declaration = node.parent();
        if has_initializer(&declaration)
            && matches!(
                declaration.as_has_initializer().maybe_initializer().as_deref(),
                Some(declaration_initializer) if ptr::eq(
                    node,
                    declaration_initializer
                )
            )
        {
            let result = self.get_contextual_type_for_variable_like_declaration(&declaration)?;
            if result.is_some() {
                return Ok(result);
            }
            if !matches!(
                context_flags,
                Some(context_flags) if context_flags.intersects(ContextFlags::SkipBindingPatterns)
            ) && is_binding_pattern(declaration.as_named_declaration().maybe_name())
            {
                return Ok(Some(self.get_type_from_binding_pattern(
                    &declaration.as_named_declaration().name(),
                    Some(true),
                    Some(false),
                )?));
            }
        }
        Ok(None)
    }

    pub(super) fn get_contextual_type_for_return_expression(
        &self,
        node: &Node, /*Expression*/
    ) -> io::Result<Option<Gc<Type>>> {
        let func = get_containing_function(node);
        if let Some(func) = func.as_ref() {
            let contextual_return_type = self.get_contextual_return_type(func)?;
            if let Some(mut contextual_return_type) = contextual_return_type {
                let function_flags = get_function_flags(Some(&**func));
                if function_flags.intersects(FunctionFlags::Generator) {
                    let use_ = if function_flags.intersects(FunctionFlags::Async) {
                        IterationUse::AsyncGeneratorReturnType
                    } else {
                        IterationUse::GeneratorReturnType
                    };
                    let iteration_types = return_ok_none_if_none!(self
                        .get_iteration_types_of_iterable(
                            &contextual_return_type,
                            use_,
                            Option::<&Node>::None,
                        ));
                    contextual_return_type = iteration_types.return_type();
                }

                if function_flags.intersects(FunctionFlags::Async) {
                    let contextual_awaited_type = self.try_map_type(
                        &contextual_return_type,
                        &mut |type_: &Type| {
                            self.get_awaited_type_no_alias(type_, Option::<&Node>::None, None, None)
                        },
                        None,
                    )?;
                    return contextual_awaited_type
                        .as_ref()
                        .try_map(|contextual_awaited_type| {
                            self.get_union_type(
                                &[
                                    contextual_awaited_type.clone(),
                                    self.create_promise_like_type(contextual_awaited_type)?,
                                ],
                                None,
                                Option::<&Symbol>::None,
                                None,
                                Option::<&Type>::None,
                            )
                        });
                }

                return Ok(Some(contextual_return_type));
            }
        }
        Ok(None)
    }

    pub(super) fn get_contextual_type_for_await_operand(
        &self,
        node: &Node, /*AwaitExpression*/
        context_flags: Option<ContextFlags>,
    ) -> io::Result<Option<Gc<Type>>> {
        let contextual_type = self.get_contextual_type_(node, context_flags)?;
        if let Some(contextual_type) = contextual_type.as_ref() {
            let contextual_awaited_type =
                self.get_awaited_type_no_alias(contextual_type, Option::<&Node>::None, None, None)?;
            return contextual_awaited_type
                .as_ref()
                .try_map(|contextual_awaited_type| {
                    self.get_union_type(
                        &[
                            contextual_awaited_type.clone(),
                            self.create_promise_like_type(contextual_awaited_type)?,
                        ],
                        None,
                        Option::<&Symbol>::None,
                        None,
                        Option::<&Type>::None,
                    )
                });
        }
        Ok(None)
    }

    pub(super) fn get_contextual_type_for_yield_operand(
        &self,
        node: &Node, /*YieldExpression*/
    ) -> io::Result<Option<Gc<Type>>> {
        let func = get_containing_function(node);
        if let Some(func) = func.as_ref() {
            let function_flags = get_function_flags(Some(&**func));
            let contextual_return_type = self.get_contextual_return_type(func)?;
            if let Some(contextual_return_type) = contextual_return_type.as_ref() {
                return Ok(if node.as_yield_expression().asterisk_token.is_some() {
                    Some(contextual_return_type.clone())
                } else {
                    self.get_iteration_type_of_generator_function_return_type(
                        IterationTypeKind::Yield,
                        contextual_return_type,
                        function_flags.intersects(FunctionFlags::Async),
                    )
                });
            }
        }

        Ok(None)
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
                        Some(node_parent_initializer) if Gc::ptr_eq(
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
                    Some(node_parent_initializer) if Gc::ptr_eq(
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
    ) -> io::Result<Option<Gc<Type>>> {
        let is_async = get_function_flags(Some(function_decl)).intersects(FunctionFlags::Async);
        let contextual_return_type = self.get_contextual_return_type(function_decl)?;
        if let Some(contextual_return_type) = contextual_return_type.as_ref() {
            return Ok(self.get_iteration_type_of_generator_function_return_type(
                kind,
                contextual_return_type,
                is_async,
            ));
        }

        Ok(None)
    }

    pub(super) fn get_contextual_return_type(
        &self,
        function_decl: &Node, /*SignatureDeclaration*/
    ) -> io::Result<Option<Gc<Type>>> {
        let return_type = self.get_return_type_from_annotation(function_decl)?;
        if return_type.is_some() {
            return Ok(return_type);
        }
        let signature = self.get_contextual_signature_for_function_like_declaration(function_decl);
        if let Some(signature) = signature
            .as_ref()
            .filter(|signature| !self.is_resolving_return_type_of_signature((*signature).clone()))
        {
            return Ok(Some(self.get_return_type_of_signature(signature.clone())?));
        }
        let iife = get_immediately_invoked_function_expression(function_decl);
        if let Some(iife) = iife.as_ref() {
            return self.get_contextual_type_(iife, None);
        }
        Ok(None)
    }

    pub(super) fn get_contextual_type_for_argument(
        &self,
        call_target: &Node, /*CallLikeExpression*/
        arg: &Node,         /*Expression*/
    ) -> io::Result<Option<Gc<Type>>> {
        let args = self.get_effective_call_arguments(call_target)?;
        let arg_index = args.iter().position(|argument| ptr::eq(&**argument, arg));
        arg_index.try_map(|arg_index| {
            self.get_contextual_type_for_argument_at_index_(call_target, arg_index)
        })
    }

    pub(super) fn get_contextual_type_for_argument_at_index_(
        &self,
        call_target: &Node, /*CallLikeExpression*/
        arg_index: usize,
    ) -> io::Result<Gc<Type>> {
        if is_import_call(call_target) {
            return Ok(if arg_index == 0 {
                self.string_type()
            } else if arg_index == 1 {
                self.get_global_import_call_options_type(false)?
            } else {
                self.any_type()
            });
        }

        let signature = if matches!(
            (*self.get_node_links(call_target)).borrow().resolved_signature.as_ref(),
            Some(resolved_signature) if Gc::ptr_eq(
                resolved_signature,
                &self.resolving_signature()
            )
        ) {
            self.resolving_signature()
        } else {
            self.get_resolved_signature_(call_target, None, None)
        };

        if is_jsx_opening_like_element(call_target) && arg_index == 0 {
            return Ok(
                self.get_effective_first_argument_for_jsx_signature(signature.clone(), call_target)
            );
        }
        let rest_index = TryInto::<isize>::try_into(signature.parameters().len()).unwrap() - 1;
        Ok(
            if signature_has_rest_parameter(&signature)
                && TryInto::<isize>::try_into(arg_index).unwrap() >= rest_index
            {
                let rest_index: usize = rest_index.try_into().unwrap();
                self.get_indexed_access_type(
                    &*self.get_type_of_symbol(&signature.parameters()[rest_index])?,
                    &self.get_number_literal_type(Number::new((arg_index - rest_index) as f64)),
                    Some(AccessFlags::Contextual),
                    Option::<&Node>::None,
                    Option::<&Symbol>::None,
                    None,
                )
            } else {
                self.get_type_at_position(&signature, arg_index)
            },
        )
    }

    pub(super) fn get_contextual_type_for_substitution_expression(
        &self,
        template: &Node,                /*TemplateExpression*/
        substitution_expression: &Node, /*Expression*/
    ) -> io::Result<Option<Gc<Type>>> {
        if template.parent().kind() == SyntaxKind::TaggedTemplateExpression {
            return self
                .get_contextual_type_for_argument(&template.parent(), substitution_expression);
        }

        Ok(None)
    }

    pub(super) fn get_contextual_type_for_binary_operand(
        &self,
        node: &Node, /*Expression*/
        context_flags: Option<ContextFlags>,
    ) -> io::Result<Option<Gc<Type>>> {
        let binary_expression = node.parent();
        let binary_expression_as_binary_expression = binary_expression.as_binary_expression();
        let left = &binary_expression_as_binary_expression.left;
        let operator_token = &binary_expression_as_binary_expression.operator_token;
        let right = &binary_expression_as_binary_expression.right;
        Ok(match operator_token.kind() {
            SyntaxKind::EqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::BarBarEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken => {
                if ptr::eq(node, &**right) {
                    self.get_contextual_type_for_assignment_declaration(&binary_expression)?
                } else {
                    None
                }
            }
            SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken => {
                let type_ = self.get_contextual_type_(&binary_expression, context_flags)?;
                if ptr::eq(node, &**right)
                    && match type_.as_ref() {
                        Some(type_) => type_.maybe_pattern().is_some(),
                        None => !is_defaulted_expando_initializer(&binary_expression),
                    }
                {
                    Some(self.get_type_of_expression(left)?)
                } else {
                    type_
                }
            }
            SyntaxKind::AmpersandAmpersandToken | SyntaxKind::CommaToken => {
                if ptr::eq(node, &**right) {
                    self.get_contextual_type_(&binary_expression, context_flags)?
                } else {
                    None
                }
            }
            _ => None,
        })
    }

    pub(super) fn get_symbol_for_expression(
        &self,
        e: &Node, /*Expression*/
    ) -> io::Result<Option<Gc<Symbol>>> {
        if e.maybe_symbol().is_some() {
            return Ok(e.maybe_symbol());
        }
        if is_identifier(e) {
            return Ok(Some(self.get_resolved_symbol(e)?));
        }
        if is_property_access_expression(e) {
            let e_as_property_access_expression = e.as_property_access_expression();
            let lhs_type =
                self.get_type_of_expression(&e_as_property_access_expression.expression)?;
            return Ok(
                if is_private_identifier(&e_as_property_access_expression.name) {
                    self.try_get_private_identifier_property_of_type(
                        &lhs_type,
                        &e_as_property_access_expression.name,
                    )?
                } else {
                    self.get_property_of_type_(
                        &lhs_type,
                        &e_as_property_access_expression
                            .name
                            .as_identifier()
                            .escaped_text,
                        None,
                    )?
                },
            );
        }
        Ok(None)
    }

    pub(super) fn try_get_private_identifier_property_of_type(
        &self,
        type_: &Type,
        id: &Node, /*PrivateIdentifier*/
    ) -> io::Result<Option<Gc<Symbol>>> {
        let lexically_scoped_symbol = self.lookup_symbol_for_private_identifier_declaration(
            &id.as_private_identifier().escaped_text,
            id,
        );
        lexically_scoped_symbol
            .as_ref()
            .try_and_then(|lexically_scoped_symbol| {
                self.get_private_identifier_property_of_type_(type_, lexically_scoped_symbol)
            })
    }
}
