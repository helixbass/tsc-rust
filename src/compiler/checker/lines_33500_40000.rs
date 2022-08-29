#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{get_node_id, CheckMode, IterationTypeKind, IterationUse, UnusedKind};
use crate::{
    append, has_jsdoc_nodes, is_call_chain, is_call_expression, is_require_call, set_node_flags,
    text_char_at_index, CharacterCodes, Debug_, Number, SymbolFlags, __String, concatenate, every,
    for_each, get_combined_node_flags, get_containing_function_or_class_static_block,
    get_effective_initializer, get_function_flags, get_jsdoc_type_assertion_type,
    is_array_literal_expression, is_assertion_expression, is_binding_element,
    is_const_type_reference, is_declaration_readonly, is_function_or_module_block, is_in_js_file,
    is_jsdoc_type_assertion, is_omitted_expression, is_parameter, is_parenthesized_expression,
    is_private_identifier, is_property_assignment, is_shorthand_property_assignment,
    is_spread_element, is_template_span, map, maybe_for_each, parse_pseudo_big_int,
    skip_parentheses, some, ContextFlags, Diagnostic, DiagnosticMessage, Diagnostics, ElementFlags,
    FunctionFlags, HasTypeParametersInterface, InferenceContext, InferenceFlags, InferenceInfo,
    InferencePriority, IterationTypes, IterationTypesResolver, LiteralLikeNodeInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeFlags, NodeInterface, PseudoBigInt,
    SignatureKind, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface, UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub(super) fn is_type_assertion(&self, node: &Node /*Expression*/) -> bool {
        let node = skip_parentheses(node, Some(true));
        matches!(
            node.kind(),
            SyntaxKind::TypeAssertionExpression | SyntaxKind::AsExpression
        ) || is_jsdoc_type_assertion(&node)
    }

    pub(super) fn check_declaration_initializer<TType: Borrow<Type>>(
        &self,
        declaration: &Node, /*HasExpressionInitializer*/
        contextual_type: Option<TType>,
    ) -> Rc<Type> {
        let initializer = get_effective_initializer(declaration).unwrap();
        let type_ = self
            .get_quick_type_of_expression(&initializer)
            .unwrap_or_else(|| {
                if let Some(contextual_type) = contextual_type {
                    self.check_expression_with_contextual_type(
                        &initializer,
                        contextual_type.borrow(),
                        None,
                        CheckMode::Normal,
                    )
                } else {
                    self.check_expression_cached(&initializer, None)
                }
            });
        if is_parameter(declaration)
            && declaration.as_parameter_declaration().name().kind()
                == SyntaxKind::ArrayBindingPattern
            && self.is_tuple_type(&type_)
            && !type_
                .as_type_reference()
                .target
                .as_tuple_type()
                .has_rest_element
            && self.get_type_reference_arity(&type_)
                < declaration
                    .as_parameter_declaration()
                    .name()
                    .as_array_binding_pattern()
                    .elements
                    .len()
        {
            self.pad_tuple_type(&type_, &declaration.as_parameter_declaration().name())
        } else {
            type_
        }
    }

    pub(super) fn pad_tuple_type(
        &self,
        type_: &Type,   /*TupleTypeReference*/
        pattern: &Node, /*ArrayBindingPattern*/
    ) -> Rc<Type> {
        let pattern_elements = &pattern.as_array_binding_pattern().elements;
        let mut element_types = self.get_type_arguments(type_);
        let type_target_as_tuple_type = type_.as_type_reference().target.as_tuple_type();
        let mut element_flags = type_target_as_tuple_type.element_flags.clone();
        for i in self.get_type_reference_arity(type_)..pattern_elements.len() {
            let e = &pattern_elements[i];
            if i < pattern_elements.len() - 1
                || !(e.kind() == SyntaxKind::BindingElement
                    && e.as_binding_element().dot_dot_dot_token.is_some())
            {
                element_types.push(if !is_omitted_expression(e) && self.has_default_value(e) {
                    self.get_type_from_binding_element(e, Some(false), Some(false))
                } else {
                    self.any_type()
                });
                element_flags.push(ElementFlags::Optional);
                if !is_omitted_expression(e) && !self.has_default_value(e) {
                    self.report_implicit_any(e, &self.any_type(), None);
                }
            }
        }
        self.create_tuple_type(
            &element_types,
            Some(&element_flags),
            Some(type_target_as_tuple_type.readonly),
            None,
        )
    }

    pub(super) fn widen_type_inferred_from_initializer(
        &self,
        declaration: &Node, /*HasExpressionInitializer*/
        type_: &Type,
    ) -> Rc<Type> {
        let widened = if get_combined_node_flags(declaration).intersects(NodeFlags::Const)
            || is_declaration_readonly(declaration)
        {
            type_.type_wrapper()
        } else {
            self.get_widened_literal_type(type_)
        };
        if is_in_js_file(Some(declaration)) {
            if self.is_empty_literal_type(&widened) {
                self.report_implicit_any(declaration, &self.any_type(), None);
                return self.any_type();
            } else if self.is_empty_array_literal_type(&widened) {
                self.report_implicit_any(declaration, &self.any_array_type(), None);
                return self.any_array_type();
            }
        }
        widened
    }

    pub(super) fn is_literal_of_contextual_type<TContextualType: Borrow<Type>>(
        &self,
        candidate_type: &Type,
        contextual_type: Option<TContextualType>,
    ) -> bool {
        if let Some(contextual_type) = contextual_type {
            let contextual_type = contextual_type.borrow();
            if contextual_type
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
            {
                let types = contextual_type.as_union_type().types();
                return some(
                    Some(types),
                    Some(|t: &Rc<Type>| {
                        self.is_literal_of_contextual_type(candidate_type, Some(&**t))
                    }),
                );
            }
            if contextual_type
                .flags()
                .intersects(TypeFlags::InstantiableNonPrimitive)
            {
                let constraint = self
                    .get_base_constraint_of_type(contextual_type)
                    .unwrap_or_else(|| self.unknown_type());
                return self.maybe_type_of_kind(&constraint, TypeFlags::String)
                    && self.maybe_type_of_kind(candidate_type, TypeFlags::StringLiteral)
                    || self.maybe_type_of_kind(&constraint, TypeFlags::Number)
                        && self.maybe_type_of_kind(candidate_type, TypeFlags::NumberLiteral)
                    || self.maybe_type_of_kind(&constraint, TypeFlags::BigInt)
                        && self.maybe_type_of_kind(candidate_type, TypeFlags::BigIntLiteral)
                    || self.maybe_type_of_kind(&constraint, TypeFlags::ESSymbol)
                        && self.maybe_type_of_kind(candidate_type, TypeFlags::UniqueESSymbol)
                    || self.is_literal_of_contextual_type(candidate_type, Some(constraint));
            }
            return contextual_type.flags().intersects(
                TypeFlags::StringLiteral
                    | TypeFlags::Index
                    | TypeFlags::TemplateLiteral
                    | TypeFlags::StringMapping,
            ) && self.maybe_type_of_kind(candidate_type, TypeFlags::StringLiteral)
                || contextual_type.flags().intersects(TypeFlags::NumberLiteral)
                    && self.maybe_type_of_kind(candidate_type, TypeFlags::NumberLiteral)
                || contextual_type.flags().intersects(TypeFlags::BigIntLiteral)
                    && self.maybe_type_of_kind(candidate_type, TypeFlags::BigIntLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::BooleanLiteral)
                    && self.maybe_type_of_kind(candidate_type, TypeFlags::BooleanLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::UniqueESSymbol)
                    && self.maybe_type_of_kind(candidate_type, TypeFlags::UniqueESSymbol);
        }
        false
    }

    pub(super) fn is_const_context(&self, node: &Node /*Expression*/) -> bool {
        let parent = node.parent();
        is_assertion_expression(&parent)
            && is_const_type_reference(&parent.as_has_type().maybe_type().unwrap())
            || is_jsdoc_type_assertion(&parent)
                && is_const_type_reference(&get_jsdoc_type_assertion_type(&parent))
            || (is_parenthesized_expression(&parent)
                || is_array_literal_expression(&parent)
                || is_spread_element(&parent))
                && self.is_const_context(&parent)
            || (is_property_assignment(&parent)
                || is_shorthand_property_assignment(&parent)
                || is_template_span(&parent))
                && self.is_const_context(&parent.parent())
    }

    pub(super) fn check_expression_for_mutable_location<TContextualType: Borrow<Type>>(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
        contextual_type: Option<TContextualType>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        let type_ = self.check_expression(node, check_mode, force_tuple);
        if self.is_const_context(node) {
            self.get_regular_type_of_literal_type(&type_)
        } else if self.is_type_assertion(node) {
            type_.type_wrapper()
        } else {
            self.get_widened_literal_like_type_for_contextual_type(
                &type_,
                self.instantiate_contextual_type(
                    if contextual_type.is_none() {
                        self.get_contextual_type_(node, None)
                    } else {
                        Some(contextual_type.unwrap().borrow().type_wrapper())
                    },
                    node,
                    None,
                ),
            )
        }
    }

    pub(super) fn check_property_assignment(
        &self,
        node: &Node, /*PropertyAssignment*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let node_as_property_assignment = node.as_property_assignment();
        if node_as_property_assignment.name().kind() == SyntaxKind::ComputedPropertyName {
            self.check_computed_property_name(&node_as_property_assignment.name());
        }
        self.check_expression_for_mutable_location(
            &node_as_property_assignment.initializer,
            check_mode,
            Option::<&Type>::None,
            None,
        )
    }

    pub(super) fn check_object_literal_method(
        &self,
        node: &Node, /*MethodDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_grammar_method(node);

        let node_as_method_declaration = node.as_method_declaration();
        if node_as_method_declaration.name().kind() == SyntaxKind::ComputedPropertyName {
            self.check_computed_property_name(&node_as_method_declaration.name());
        }

        let uninstantiated_type =
            self.check_function_expression_or_object_literal_method(node, check_mode);
        self.instantiate_type_with_single_generic_call_signature(
            node,
            &uninstantiated_type,
            check_mode,
        )
    }

    pub(super) fn instantiate_type_with_single_generic_call_signature(
        &self,
        node: &Node, /*Expression | MethodDeclaration | QualifiedName*/
        type_: &Type,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        if let Some(check_mode) = check_mode.filter(|check_mode| {
            check_mode.intersects(CheckMode::Inferential | CheckMode::SkipGenericFunctions)
        }) {
            let call_signature = self.get_single_signature(type_, SignatureKind::Call, true);
            let construct_signature =
                self.get_single_signature(type_, SignatureKind::Construct, true);
            let signature = call_signature
                .clone()
                .or_else(|| construct_signature.clone());
            if let Some(signature) = signature
                .as_ref()
                .filter(|signature| signature.maybe_type_parameters().is_some())
            {
                let contextual_type = self
                    .get_apparent_type_of_contextual_type(node, Some(ContextFlags::NoConstraints));
                if let Some(contextual_type) = contextual_type.as_ref() {
                    let contextual_signature = self.get_single_signature(
                        &self.get_non_nullable_type(contextual_type),
                        if call_signature.is_some() {
                            SignatureKind::Call
                        } else {
                            SignatureKind::Construct
                        },
                        false,
                    );
                    if let Some(contextual_signature) =
                        contextual_signature
                            .as_ref()
                            .filter(|contextual_signature| {
                                contextual_signature.maybe_type_parameters().is_none()
                            })
                    {
                        if check_mode.intersects(CheckMode::SkipGenericFunctions) {
                            self.skipped_generic_function(node, check_mode);
                            return self.any_function_type();
                        }
                        let context = self.get_inference_context(node).unwrap();
                        let return_type = context.signature.as_ref().map(|context_signature| {
                            self.get_return_type_of_signature(context_signature.clone())
                        });
                        let return_signature = return_type.as_ref().and_then(|return_type| {
                            self.get_single_call_or_construct_signature(return_type)
                        });
                        if let Some(return_signature) =
                            return_signature.as_ref().filter(|return_signature| {
                                return_signature.maybe_type_parameters().is_none()
                                    && !every(
                                        &context.inferences(),
                                        |inference: &Rc<InferenceInfo>, _| {
                                            self.has_inference_candidates(inference)
                                        },
                                    )
                            })
                        {
                            let unique_type_parameters = self.get_unique_type_parameters(
                                &context,
                                signature.maybe_type_parameters().as_ref().unwrap(),
                            );
                            let instantiated_signature = self
                                .get_signature_instantiation_without_filling_in_type_arguments(
                                    signature.clone(),
                                    Some(&unique_type_parameters),
                                );
                            let inferences =
                                map(&*context.inferences(), |info: &Rc<InferenceInfo>, _| {
                                    Rc::new(self.create_inference_info(&info.type_parameter))
                                });
                            self.apply_to_parameter_types(
                                &instantiated_signature,
                                contextual_signature,
                                |source: &Type, target: &Type| {
                                    self.infer_types(
                                        &inferences,
                                        source,
                                        target,
                                        Some(InferencePriority::None),
                                        Some(true),
                                    );
                                },
                            );
                            if some(
                                Some(&inferences),
                                Some(|inference: &Rc<InferenceInfo>| {
                                    self.has_inference_candidates(inference)
                                }),
                            ) {
                                self.apply_to_return_types(
                                    instantiated_signature.clone(),
                                    contextual_signature.clone(),
                                    |source: &Type, target: &Type| {
                                        self.infer_types(&inferences, source, target, None, None);
                                    },
                                );
                                if !self
                                    .has_overlapping_inferences(&context.inferences(), &inferences)
                                {
                                    self.merge_inferences(
                                        &mut context.inferences_mut(),
                                        &inferences,
                                    );
                                    {
                                        let mut context_inferred_type_parameters =
                                            context.maybe_inferred_type_parameters_mut();
                                        *context_inferred_type_parameters = Some(concatenate(
                                            context_inferred_type_parameters
                                                .clone()
                                                .unwrap_or_else(|| vec![]),
                                            unique_type_parameters,
                                        ));
                                    }
                                    return self
                                        .get_or_create_type_from_signature(instantiated_signature);
                                }
                            }
                        }
                        return self.get_or_create_type_from_signature(
                            self.instantiate_signature_in_context_of(
                                signature.clone(),
                                contextual_signature.clone(),
                                Some(&context),
                                None,
                            ),
                        );
                    }
                }
            }
        }
        type_.type_wrapper()
    }

    pub(super) fn skipped_generic_function(&self, node: &Node, check_mode: CheckMode) {
        if check_mode.intersects(CheckMode::Inferential) {
            let context = self.get_inference_context(node).unwrap();
            context.set_flags(context.flags() | InferenceFlags::SkippedGenericFunction);
        }
    }

    pub(super) fn has_inference_candidates(&self, info: &InferenceInfo) -> bool {
        info.maybe_candidates().is_some() || info.maybe_contra_candidates().is_some()
    }

    pub(super) fn has_overlapping_inferences(
        &self,
        a: &[Rc<InferenceInfo>],
        b: &[Rc<InferenceInfo>],
    ) -> bool {
        for i in 0..a.len() {
            if self.has_inference_candidates(&a[i]) && self.has_inference_candidates(&b[i]) {
                return true;
            }
        }
        false
    }

    pub(super) fn merge_inferences(
        &self,
        target: &mut Vec<Rc<InferenceInfo>>,
        source: &[Rc<InferenceInfo>],
    ) {
        for i in 0..target.len() {
            if !self.has_inference_candidates(&target[i])
                && self.has_inference_candidates(&source[i])
            {
                target[i] = source[i].clone();
            }
        }
    }

    pub(super) fn get_unique_type_parameters(
        &self,
        context: &InferenceContext,
        type_parameters: &[Rc<Type /*TypeParameter*/>],
    ) -> Vec<Rc<Type /*TypeParameter*/>> {
        let mut result: Vec<Rc<Type /*TypeParameter*/>> = vec![];
        let mut old_type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>> = None;
        let mut new_type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>> = None;
        for tp in type_parameters {
            let tp_symbol = tp.symbol();
            let name = tp_symbol.escaped_name();
            if self.has_type_parameter_by_name(
                context.maybe_inferred_type_parameters().as_deref(),
                name,
            ) || self.has_type_parameter_by_name(Some(&result), name)
            {
                let new_name = self.get_unique_type_parameter_name(
                    &concatenate(
                        context
                            .maybe_inferred_type_parameters()
                            .clone()
                            .unwrap_or_else(|| vec![]),
                        result.clone(),
                    ),
                    name,
                );
                let symbol: Rc<Symbol> = self
                    .create_symbol(SymbolFlags::TypeParameter, new_name, None)
                    .into();
                let mut new_type_parameter = self.create_type_parameter(Some(&*symbol));
                new_type_parameter.target = Some(tp.clone());
                let new_type_parameter: Rc<Type> = new_type_parameter.into();
                if old_type_parameters.is_none() {
                    old_type_parameters = Some(vec![]);
                }
                append(old_type_parameters.as_mut().unwrap(), Some(tp.clone()));
                if new_type_parameters.is_none() {
                    new_type_parameters = Some(vec![]);
                }
                append(
                    new_type_parameters.as_mut().unwrap(),
                    Some(new_type_parameter.clone()),
                );
                result.push(new_type_parameter);
            } else {
                result.push(tp.clone());
            }
        }
        if let Some(new_type_parameters) = new_type_parameters.as_ref() {
            let mapper = self.create_type_mapper(
                old_type_parameters.unwrap(),
                Some(new_type_parameters.clone()),
            );
            for tp in new_type_parameters {
                tp.as_type_parameter().set_mapper(mapper.clone());
            }
        }
        result
    }

    pub(super) fn has_type_parameter_by_name(
        &self,
        type_parameters: Option<&[Rc<Type /*TypeParameter*/>]>,
        name: &__String,
    ) -> bool {
        some(
            type_parameters,
            Some(|tp: &Rc<Type>| tp.symbol().escaped_name() == name),
        )
    }

    pub(super) fn get_unique_type_parameter_name(
        &self,
        type_parameters: &[Rc<Type /*TypeParameter*/>],
        base_name: &__String,
    ) -> __String {
        let base_name_as_chars = base_name.chars().collect::<Vec<_>>();
        let mut len = base_name_as_chars.len();
        while len > 1
            && text_char_at_index(&base_name_as_chars, len - 1) >= CharacterCodes::_0
            && text_char_at_index(&base_name_as_chars, len - 1) <= CharacterCodes::_9
        {
            len -= 1;
        }
        let s: String = base_name_as_chars[0..len].into_iter().collect();
        let mut index = 1;
        loop {
            let augmented_name = __String::new(format!("{}{}", s, index));
            if !self.has_type_parameter_by_name(Some(type_parameters), &augmented_name) {
                return augmented_name;
            }
            index += 1;
        }
    }

    pub(super) fn get_return_type_of_single_non_generic_call_signature(
        &self,
        func_type: &Type,
    ) -> Option<Rc<Type>> {
        let signature = self.get_single_call_signature(func_type);
        signature
            .filter(|signature| signature.maybe_type_parameters().is_none())
            .map(|signature| self.get_return_type_of_signature(signature))
    }

    pub(super) fn get_return_type_of_single_non_generic_signature_of_call_chain(
        &self,
        expr: &Node, /*CallChain*/
    ) -> Option<Rc<Type>> {
        let expr_as_call_expression = expr.as_call_expression();
        let func_type = self.check_expression(&expr_as_call_expression.expression, None, None);
        let non_optional_type =
            self.get_optional_expression_type(&func_type, &expr_as_call_expression.expression);
        let return_type = self.get_return_type_of_single_non_generic_call_signature(&func_type);
        return_type.as_ref().map(|return_type| {
            self.propagate_optional_type_marker(
                return_type,
                expr,
                !Rc::ptr_eq(&non_optional_type, &func_type),
            )
        })
    }

    pub(super) fn get_type_of_expression(&self, node: &Node /*Expression*/) -> Rc<Type> {
        let quick_type = self.get_quick_type_of_expression(node);
        if let Some(quick_type) = quick_type {
            return quick_type;
        }
        if node.flags().intersects(NodeFlags::TypeCached) {
            if let Some(flow_type_cache) = self.maybe_flow_type_cache().as_ref() {
                let cached_type = flow_type_cache.get(&get_node_id(node));
                if let Some(cached_type) = cached_type {
                    return cached_type.clone();
                }
            }
        }
        let start_invocation_count = self.flow_invocation_count();
        let type_ = self.check_expression(node, None, None);
        if self.flow_invocation_count() != start_invocation_count {
            let mut flow_type_cache = self.maybe_flow_type_cache();
            if flow_type_cache.is_none() {
                *flow_type_cache = Some(HashMap::new());
            }
            let cache = flow_type_cache.as_mut().unwrap();
            cache.insert(get_node_id(node), type_.clone());
            set_node_flags(Some(node), node.flags() | NodeFlags::TypeCached);
        }
        type_
    }

    pub(super) fn get_quick_type_of_expression(
        &self,
        node: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        let mut expr = skip_parentheses(node, Some(true));
        if is_jsdoc_type_assertion(&expr) {
            let type_ = get_jsdoc_type_assertion_type(&expr);
            if !is_const_type_reference(&type_) {
                return Some(self.get_type_from_type_node_(&type_));
            }
        }
        expr = skip_parentheses(node, None);
        if is_call_expression(node)
            && expr.as_call_expression().expression.kind() != SyntaxKind::SuperKeyword
            && !is_require_call(&expr, true)
            && !self.is_symbol_or_symbol_for_call(&expr)
        {
            let type_ = if is_call_chain(&expr) {
                self.get_return_type_of_single_non_generic_signature_of_call_chain(&expr)
            } else {
                self.get_return_type_of_single_non_generic_call_signature(
                    &self.check_non_null_expression(&expr.as_call_expression().expression),
                )
            };
            if type_.is_some() {
                return type_;
            }
        } else if is_assertion_expression(&expr)
            && !is_const_type_reference(&expr.as_has_type().maybe_type().unwrap())
        {
            return Some(self.get_type_from_type_node_(&expr.as_has_type().maybe_type().unwrap()));
        } else if matches!(
            node.kind(),
            SyntaxKind::NumericLiteral
                | SyntaxKind::StringLiteral
                | SyntaxKind::TrueKeyword
                | SyntaxKind::FalseKeyword
        ) {
            return Some(self.check_expression(node, None, None));
        }
        None
    }

    pub(super) fn get_context_free_type_of_expression(
        &self,
        node: &Node, /*Expression*/
    ) -> Rc<Type> {
        let links = self.get_node_links(node);
        if let Some(links_context_free_type) = (*links).borrow().context_free_type.clone() {
            return links_context_free_type;
        }
        let save_contextual_type = node.maybe_contextual_type().clone();
        *node.maybe_contextual_type() = Some(self.any_type());
        // try {
        let type_ = self.check_expression(node, Some(CheckMode::SkipContextSensitive), None);
        links.borrow_mut().context_free_type = Some(type_.clone());
        // }
        // finally {
        *node.maybe_contextual_type() = save_contextual_type;
        // }
        type_
    }

    pub(super) fn check_expression(
        &self,
        node: &Node, /*Expression | QualifiedName*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        // tracing?.push(tracing.Phase.Check, "checkExpression", { kind: node.kind, pos: node.pos, end: node.end });
        let save_current_node = self.maybe_current_node();
        self.set_current_node(Some(node.node_wrapper()));
        self.set_instantiation_count(0);
        let uninstantiated_type = self.check_expression_worker(node, check_mode, force_tuple);
        let type_ = self.instantiate_type_with_single_generic_call_signature(
            node,
            &uninstantiated_type,
            check_mode,
        );
        if self.is_const_enum_object_type(&type_) {
            self.check_const_enum_access(node, &type_);
        }
        self.set_current_node(save_current_node);
        // tracing?.pop();
        type_
    }

    pub(super) fn check_const_enum_access(
        &self,
        node: &Node, /*Expression | QualifiedName*/
        type_: &Type,
    ) {
        let ok = (node.parent().kind() == SyntaxKind::PropertyAccessExpression
            && ptr::eq(
                &*node.parent().as_property_access_expression().expression,
                node,
            ))
            || (node.parent().kind() == SyntaxKind::ElementAccessExpression
                && ptr::eq(
                    &*node.parent().as_element_access_expression().expression,
                    node,
                ))
            || (matches!(
                node.kind(),
                SyntaxKind::Identifier | SyntaxKind::QualifiedName
            ) && self.is_in_right_side_of_import_or_export_assignment(node)
                || node.parent().kind() == SyntaxKind::TypeQuery
                    && ptr::eq(&*node.parent().as_type_query_node().expr_name, node))
            || node.parent().kind() == SyntaxKind::ExportSpecifier;

        if !ok {
            self.error(
                Some(node),
                &Diagnostics::const_enums_can_only_be_used_in_property_or_index_access_expressions_or_the_right_hand_side_of_an_import_declaration_or_export_assignment_or_type_query,
                None,
            );
        }

        if self.compiler_options.isolated_modules == Some(true) {
            Debug_.assert(
                type_.symbol().flags().intersects(SymbolFlags::ConstEnum),
                None,
            );
            let const_enum_declaration = type_.symbol().maybe_value_declaration().unwrap();
            if const_enum_declaration
                .flags()
                .intersects(NodeFlags::Ambient)
            {
                self.error(
                    Some(node),
                    &Diagnostics::Cannot_access_ambient_const_enums_when_the_isolatedModules_flag_is_provided,
                    None,
                );
            }
        }
    }

    pub(super) fn check_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let node_as_parenthesized_expression = node.as_parenthesized_expression();
        if has_jsdoc_nodes(node) && is_jsdoc_type_assertion(node) {
            let type_ = get_jsdoc_type_assertion_type(node);
            return self.check_assertion_worker(
                &type_,
                &type_,
                &node_as_parenthesized_expression.expression,
                check_mode,
            );
        }
        self.check_expression(
            &node_as_parenthesized_expression.expression,
            check_mode,
            None,
        )
    }

    pub(super) fn check_expression_worker(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        let kind = node.kind();
        if let Some(cancellation_token) = self.maybe_cancellation_token() {
            match kind {
                SyntaxKind::ClassExpression
                | SyntaxKind::FunctionExpression
                | SyntaxKind::ArrowFunction => {
                    cancellation_token.throw_if_cancellation_requested();
                }
                _ => (),
            }
        }
        match kind {
            SyntaxKind::Identifier => self.check_identifier(node, check_mode),
            SyntaxKind::PrivateIdentifier => self.check_private_identifier_expression(node),
            SyntaxKind::ThisKeyword => self.check_this_expression(node),
            SyntaxKind::SuperKeyword => self.check_super_expression(node),
            SyntaxKind::NullKeyword => self.null_widening_type(),
            SyntaxKind::NoSubstitutionTemplateLiteral | SyntaxKind::StringLiteral => self
                .get_fresh_type_of_literal_type(
                    &self.get_string_literal_type(&node.as_literal_like_node().text()),
                ),
            SyntaxKind::NumericLiteral => {
                self.check_grammar_numeric_literal(node);
                self.get_fresh_type_of_literal_type(&self.get_number_literal_type(Number::new(
                    node.as_literal_like_node().text().parse::<f64>().unwrap(),
                )))
            }
            SyntaxKind::BigIntLiteral => {
                self.check_grammar_big_int_literal(node);
                self.get_fresh_type_of_literal_type(&self.get_big_int_literal_type(
                    PseudoBigInt::new(
                        false,
                        parse_pseudo_big_int(&node.as_literal_like_node().text()),
                    ),
                ))
            }
            SyntaxKind::TrueKeyword => self.true_type(),
            SyntaxKind::FalseKeyword => self.false_type(),
            SyntaxKind::TemplateExpression => self.check_template_expression(node),
            SyntaxKind::RegularExpressionLiteral => self.global_reg_exp_type(),
            SyntaxKind::ArrayLiteralExpression => {
                self.check_array_literal(node, check_mode, force_tuple)
            }
            SyntaxKind::ObjectLiteralExpression => self.check_object_literal(node, check_mode),
            SyntaxKind::PropertyAccessExpression => {
                self.check_property_access_expression(node, check_mode)
            }
            SyntaxKind::QualifiedName => self.check_qualified_name(node, check_mode),
            SyntaxKind::ElementAccessExpression => self.check_indexed_access(node, check_mode),
            SyntaxKind::CallExpression => {
                if node.as_call_expression().expression.kind() == SyntaxKind::ImportKeyword {
                    return self.check_import_call_expression(node);
                }
                self.check_call_expression(node, check_mode)
            }
            SyntaxKind::NewExpression => self.check_call_expression(node, check_mode),
            SyntaxKind::TaggedTemplateExpression => self.check_tagged_template_expression(node),
            SyntaxKind::ParenthesizedExpression => {
                self.check_parenthesized_expression(node, check_mode)
            }
            SyntaxKind::ClassExpression => self.check_class_expression(node),
            SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction => {
                self.check_function_expression_or_object_literal_method(node, check_mode)
            }
            SyntaxKind::TypeOfExpression => self.check_type_of_expression(node),
            SyntaxKind::TypeAssertionExpression | SyntaxKind::AsExpression => {
                self.check_assertion(node)
            }
            SyntaxKind::NonNullExpression => self.check_non_null_assertion(node),
            SyntaxKind::MetaProperty => self.check_meta_property(node),
            SyntaxKind::DeleteExpression => self.check_delete_expression(node),
            SyntaxKind::VoidExpression => self.check_void_expression(node),
            SyntaxKind::AwaitExpression => self.check_await_expression(node),
            SyntaxKind::PrefixUnaryExpression => self.check_prefix_unary_expression(node),
            SyntaxKind::PostfixUnaryExpression => self.check_postfix_unary_expression(node),
            SyntaxKind::BinaryExpression => self.check_binary_expression().call(node, check_mode),
            SyntaxKind::ConditionalExpression => {
                self.check_conditional_expression(node, check_mode)
            }
            SyntaxKind::SpreadElement => self.check_spread_expression(node, check_mode),
            SyntaxKind::OmittedExpression => self.undefined_widening_type(),
            SyntaxKind::YieldExpression => self.check_yield_expression(node),
            SyntaxKind::SyntheticExpression => self.check_synthetic_expression(node),
            SyntaxKind::JsxExpression => self.check_jsx_expression(node, check_mode),
            SyntaxKind::JsxElement => self.check_jsx_element(node, check_mode),
            SyntaxKind::JsxSelfClosingElement => {
                self.check_jsx_self_closing_element(node, check_mode)
            }
            SyntaxKind::JsxFragment => self.check_jsx_fragment(node),
            SyntaxKind::JsxAttributes => self.check_jsx_attributes(node, check_mode),
            SyntaxKind::JsxOpeningElement => {
                Debug_.fail(Some("Shouldn't ever directly check a JsxOpeningElement"))
            }
            _ => self.error_type(),
        }
    }

    pub(super) fn check_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        // TODO
    }

    pub(super) fn check_signature_declaration(&self, node: &Node /*SignatureDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn check_property_declaration(&self, node: &Node /*PropertySignature*/) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_property_signature(&self, node: &Node /*PropertySignature*/) {
        if is_private_identifier(&*node.as_property_signature().name()) {
            self.error(
                Some(node.node_wrapper()),
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                None,
            );
        }
        self.check_property_declaration(node)
    }

    pub(super) fn get_effective_type_arguments(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
        type_parameters: &[Rc<Type /*TypeParameter*/>],
    ) -> Vec<Rc<Type>> {
        self.fill_missing_type_arguments(
            Some(map(
                node.as_has_type_arguments().maybe_type_arguments().unwrap(),
                |type_argument, _| self.get_type_from_type_node_(type_argument),
            )),
            Some(type_parameters),
            0, // TODO: this is wrong
            false,
        )
        .unwrap()
    }

    pub(super) fn get_type_parameters_for_type_reference(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }

    pub(super) fn check_type_reference_node(&self, node: &Node /*TypeReferenceNode*/) {
        maybe_for_each(
            node.as_type_reference_node().type_arguments.as_ref(),
            |type_argument, _| {
                self.check_source_element(Some(&**type_argument));
                Option::<()>::None
            },
        );
        let type_ = self.get_type_from_type_reference(node);
    }

    pub(super) fn get_type_argument_constraint_(
        &self,
        node: &Node, /*TypeNode*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn check_array_type(&self, node: &Node /*ArrayTypeNode*/) {
        self.check_source_element(Some(&*node.as_array_type_node().element_type));
    }

    pub(super) fn check_union_or_intersection_type(
        &self,
        node: &Node, /*UnionOrIntersectionTypeNode*/
    ) {
        for_each(
            node.as_union_or_intersection_type_node().types(),
            |type_, _| {
                self.check_source_element(Some(&**type_));
                Option::<()>::None
            },
        );
        self.get_type_from_type_node_(node);
    }

    pub(super) fn check_indexed_access_index_type(
        &self,
        type_: &Type,
        access_node: &Node, /*IndexedAccessTypeNode | ElementAccessExpression*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_private_within_ambient(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_awaited_type_of_promise<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_promised_type_of_promise<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn check_awaited_type(
        &self,
        type_: &Type,
        with_alias: bool,
        error_node: &Node,
        diagnostic_message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_awaited_type_instantiation(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Conditional) {
            unimplemented!()
        }
        false
    }

    pub(super) fn unwrap_awaited_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.unwrap_awaited_type(type_)),
                None,
            )
            .unwrap()
        } else if self.is_awaited_type_instantiation(type_) {
            unimplemented!()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_awaited_type_<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_awaited_type_no_alias<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        if self.is_type_any(Some(type_)) {
            return Some(type_.type_wrapper());
        }

        if self.is_awaited_type_instantiation(type_) {
            return Some(type_.type_wrapper());
        }

        unimplemented!()
    }

    pub(super) fn check_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if self.produce_diagnostics {
            self.check_function_or_method_declaration(node);
        }
    }

    pub(super) fn check_function_or_method_declaration(
        &self,
        node: &Node, /*FunctionDeclaration | MethodDeclaration | MethodSignature*/
    ) {
        // self.check_decorators(node);
        // self.check_signature_declaration(node);
    }

    pub(super) fn check_unused_identifiers<
        TAddDiagnostic: FnMut(&Node, UnusedKind, Rc<Diagnostic>),
    >(
        &self,
        potentially_unused_identifiers: &[Rc<Node /*PotentiallyUnusedIdentifier*/>],
        add_diagnostic: TAddDiagnostic, /*AddUnusedDiagnostic*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_block(&self, node: &Node /*Block*/) {
        let node_as_block = node.as_block();
        if is_function_or_module_block(node) {
            for_each(&node_as_block.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        } else {
            for_each(&node_as_block.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        }
    }

    pub(super) fn check_collisions_for_declaration_name<TName: Borrow<Node>>(
        &self,
        node: &Node,
        name: Option<TName /*Identifier*/>,
    ) {
        unimplemented!()
    }

    pub(super) fn convert_auto_to_any(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn check_variable_like_declaration(&self, node: &Node) {
        let node_as_variable_like_declaration = node.as_variable_like_declaration();
        if !is_binding_element(node) {
            self.check_source_element(node_as_variable_like_declaration.maybe_type());
        }

        let symbol = self.get_symbol_of_node(node).unwrap();

        let type_ = self.convert_auto_to_any(&self.get_type_of_symbol(&*symbol));
        let value_declaration = symbol.maybe_value_declaration();
        if value_declaration.is_some() && ptr::eq(node, &*value_declaration.unwrap()) {
            let initializer = get_effective_initializer(node);
            if let Some(initializer) = initializer {
                if true {
                    let initializer_type = self.check_expression_cached(&initializer, None);
                    self.check_type_assignable_to_and_optionally_elaborate(
                        &initializer_type,
                        &type_,
                        Some(node),
                        Some(&*initializer),
                        None,
                        None,
                    );
                }
            }
        } else {
            unimplemented!()
        }
    }

    pub(super) fn error_next_variable_or_property_declaration_must_have_same_type<
        TFirstDeclaration: Borrow<Node>,
    >(
        &self,
        first_declaration: Option<TFirstDeclaration /*Declaration*/>,
        first_type: &Type,
        next_declaration: &Node, /*Declaration*/
        next_type: &Type,
    ) {
        unimplemented!()
    }

    pub(super) fn check_variable_declaration(&self, node: &Node /*VariableDeclaration*/) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_variable_statement(&self, node: &Node /*VariableStatement*/) {
        for_each(
            &node
                .as_variable_statement()
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
            |declaration, _| Some(self.check_source_element(Some(&**declaration))),
        );
    }

    pub(super) fn check_expression_statement(&self, node: &Node /*ExpressionStatement*/) {
        let expression = &node.as_expression_statement().expression;
        self.check_expression(expression, None, None);
    }

    pub(super) fn check_if_statement(&self, node: &Node /*IfStatement*/) {
        let node_as_if_statement = node.as_if_statement();
        let type_ = self.check_truthiness_expression(&node_as_if_statement.expression, None);
        self.check_source_element(Some(&*node_as_if_statement.then_statement));

        if node_as_if_statement.then_statement.kind() == SyntaxKind::EmptyStatement {
            self.error(
                Some(&*node_as_if_statement.then_statement),
                &Diagnostics::The_body_of_an_if_statement_cannot_be_the_empty_statement,
                None,
            );
        }

        self.check_source_element(node_as_if_statement.else_statement.clone());
    }

    pub(super) fn check_testing_known_truthy_callable_or_awaitable_type<TBody: Borrow<Node>>(
        &self,
        cond_expr: &Node, /*Expression*/
        type_: &Type,
        body: Option<TBody /*Statement | Expression*/>,
    ) {
        unimplemented!()
    }

    pub(super) fn check_truthiness_of_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Void) {
            self.error(
                Some(node),
                &Diagnostics::An_expression_of_type_void_cannot_be_tested_for_truthiness,
                None,
            );
        }

        type_.type_wrapper()
    }

    pub(super) fn check_truthiness_expression(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_truthiness_of_type(&self.check_expression(node, check_mode, None), node)
    }

    pub(super) fn check_right_hand_side_of_for_of(
        &self,
        statement: &Node, /*ForOfStatement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
        check_assignability: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_iteration_type_of_iterable<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        type_kind: IterationTypeKind,
        input_type: &Type,
        error_node: Option<TErrorNode>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn create_iteration_types(
        &self,
        yield_type: Option<Rc<Type>>,
        return_type: Option<Rc<Type>>,
        next_type: Option<Rc<Type>>,
    ) -> IterationTypes {
        let yield_type = yield_type.unwrap_or_else(|| self.never_type());
        let return_type = return_type.unwrap_or_else(|| self.never_type());
        let next_type = next_type.unwrap_or_else(|| self.unknown_type());
        IterationTypes::new(yield_type, return_type, next_type)
    }

    pub(super) fn get_iteration_types_of_iterable<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        use_: IterationUse,
        error_node: Option<TErrorNode>,
    ) -> Option<IterationTypes> {
        unimplemented!()
    }

    pub(super) fn get_iteration_types_of_global_iterable_type(
        &self,
        global_type: &Type,
        resolver: &IterationTypesResolver,
    ) -> IterationTypes {
        unimplemented!()
    }

    pub(super) fn get_iteration_type_of_generator_function_return_type(
        &self,
        kind: IterationTypeKind,
        return_type: &Type,
        is_async_generator: bool,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_iteration_types_of_generator_function_return_type(
        &self,
        type_: &Type,
        is_async_generator: bool,
    ) -> Option<IterationTypes> {
        unimplemented!()
    }

    pub(super) fn unwrap_return_type(
        &self,
        return_type: &Type,
        function_flags: FunctionFlags,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_unwrapped_return_type_void_or_any(
        &self,
        func: &Node, /*SignatureDeclaration*/
        return_type: &Type,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_return_statement(&self, node: &Node /*ReturnStatement*/) {
        let container = get_containing_function_or_class_static_block(node);

        if container.is_none() {
            unimplemented!()
        }
        let container = container.unwrap();

        let signature = self.get_signature_from_declaration_(&container);
        let return_type = self.get_return_type_of_signature(signature);
        let function_flags = get_function_flags(Some(&*container));
        let node_as_return_statement = node.as_return_statement();
        if self.strict_null_checks
            || node_as_return_statement.expression.is_some()
            || return_type.flags().intersects(TypeFlags::Never)
        {
            let expr_type = match node_as_return_statement.expression.as_ref() {
                Some(expression) => self.check_expression_cached(&expression, None),
                None => self.undefined_type(),
            };
            if false {
                unimplemented!()
            } else if self.get_return_type_from_annotation(&container).is_some() {
                let unwrapped_return_type = self
                    .unwrap_return_type(&return_type, function_flags)/*.unwrap_or(return_type)*/;
                let unwrapped_expr_type = if function_flags.intersects(FunctionFlags::Async) {
                    self.check_awaited_type(&expr_type, false, node, &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member, None)
                } else {
                    expr_type
                };
                // if unwrappedReturnType {
                self.check_type_assignable_to_and_optionally_elaborate(
                    &unwrapped_expr_type,
                    &unwrapped_return_type,
                    Some(node),
                    node_as_return_statement.expression.clone(),
                    None,
                    None,
                );
                // }
            }
        }
    }

    pub(super) fn check_type_parameters(
        &self,
        type_parameter_declarations: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
    ) {
        if let Some(type_parameter_declarations) = type_parameter_declarations {
            for node in type_parameter_declarations {
                self.check_type_parameter(&node);
            }
        }
    }

    pub(super) fn check_class_expression(&self, node: &Node /*ClassExpression*/) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_target_symbol(&self, s: &Symbol) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn is_property_without_initializer(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_property_initialized_in_static_blocks(
        &self,
        prop_name: &Node, /*Identifier | PrivateIdentifier*/
        prop_type: &Type,
        static_blocks: &[Rc<Node /*ClassStaticBlockDeclaration*/>],
        start_pos: isize,
        end_pos: isize,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_interface_declaration(&self, node: &Node /*InterfaceDeclaration*/) {
        let node_as_interface_declaration = node.as_interface_declaration();
        self.check_type_parameters(
            node_as_interface_declaration
                .maybe_type_parameters()
                .as_ref(),
        );
        for_each(&node_as_interface_declaration.members, |member, _| {
            self.check_source_element(Some(&**member));
            Option::<()>::None
        });
    }

    pub(super) fn check_type_alias_declaration(&self, node: &Node /*TypeAliasDeclaration*/) {
        let node_as_type_alias_declaration = node.as_type_alias_declaration();
        self.check_type_parameters(
            node_as_type_alias_declaration
                .maybe_type_parameters()
                .as_ref(),
        );
        if false {
            unimplemented!()
        } else {
            self.check_source_element(Some(&*node_as_type_alias_declaration.type_));
        }
    }
}
