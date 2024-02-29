use std::{collections::HashMap, io};

use id_arena::Id;

use super::{get_node_id, CheckMode};
use crate::{
    append, has_jsdoc_nodes, is_call_chain, is_call_expression, is_require_call, set_node_flags,
    text_char_at_index, CharacterCodes, Debug_, Number, SymbolFlags, __String, concatenate, every,
    get_combined_node_flags, get_effective_initializer, get_jsdoc_type_assertion_type,
    is_array_literal_expression, is_assertion_expression, is_const_type_reference,
    is_declaration_readonly, is_in_js_file, is_jsdoc_type_assertion, is_omitted_expression,
    is_parameter, is_parenthesized_expression, is_property_assignment,
    is_shorthand_property_assignment, is_spread_element, is_template_span, map,
    parse_pseudo_big_int, released, skip_parentheses, some, try_some, ContextFlags, Diagnostics,
    ElementFlags, HasArena, InArena, InferenceContext, InferenceFlags, InferenceInfo,
    InferencePriority, Matches, NamedDeclarationInterface, Node, NodeFlags, NodeInterface,
    OptionTry, PseudoBigInt, SignatureKind, SymbolInterface, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn is_type_assertion(&self, node: Id<Node> /*Expression*/) -> bool {
        let node = skip_parentheses(node, Some(true), self);
        matches!(
            node.ref_(self).kind(),
            SyntaxKind::TypeAssertionExpression | SyntaxKind::AsExpression
        ) || is_jsdoc_type_assertion(node, self)
    }

    pub(super) fn check_declaration_initializer(
        &self,
        declaration: Id<Node>, /*HasExpressionInitializer*/
        contextual_type: Option<Id<Type>>,
    ) -> io::Result<Id<Type>> {
        let initializer = get_effective_initializer(declaration, self).unwrap();
        let type_ = self
            .get_quick_type_of_expression(initializer)?
            .try_unwrap_or_else(|| -> io::Result<_> {
                Ok(if let Some(contextual_type) = contextual_type {
                    self.check_expression_with_contextual_type(
                        initializer,
                        contextual_type,
                        None,
                        CheckMode::Normal,
                    )?
                } else {
                    self.check_expression_cached(initializer, None)?
                })
            })?;
        Ok(
            if is_parameter(&declaration.ref_(self))
                && declaration
                    .ref_(self)
                    .as_parameter_declaration()
                    .name()
                    .ref_(self)
                    .kind()
                    == SyntaxKind::ArrayBindingPattern
                && self.is_tuple_type(type_)
                && !type_
                    .ref_(self)
                    .as_type_reference()
                    .target
                    .ref_(self)
                    .as_tuple_type()
                    .has_rest_element
                && self.get_type_reference_arity(type_)
                    < declaration
                        .ref_(self)
                        .as_parameter_declaration()
                        .name()
                        .ref_(self)
                        .as_array_binding_pattern()
                        .elements
                        .ref_(self)
                        .len()
            {
                self.pad_tuple_type(
                    type_,
                    declaration.ref_(self).as_parameter_declaration().name(),
                )?
            } else {
                type_
            },
        )
    }

    pub(super) fn pad_tuple_type(
        &self,
        type_: Id<Type>,   /*TupleTypeReference*/
        pattern: Id<Node>, /*ArrayBindingPattern*/
    ) -> io::Result<Id<Type>> {
        let pattern_ref = pattern.ref_(self);
        let pattern_elements = &pattern_ref.as_array_binding_pattern().elements;
        let mut element_types = self.get_type_arguments(type_)?;
        let type_target = type_.ref_(self).as_type_reference().target;
        let mut element_flags = type_target.ref_(self).as_tuple_type().element_flags.clone();
        for i in self.get_type_reference_arity(type_)..pattern_elements.ref_(self).len() {
            let e = pattern_elements.ref_(self)[i];
            if i < pattern_elements.ref_(self).len() - 1
                || !(e.ref_(self).kind() == SyntaxKind::BindingElement
                    && e.ref_(self)
                        .as_binding_element()
                        .dot_dot_dot_token
                        .is_some())
            {
                element_types.push(
                    if !is_omitted_expression(&e.ref_(self)) && self.has_default_value(e) {
                        self.get_type_from_binding_element(e, Some(false), Some(false))?
                    } else {
                        self.any_type()
                    },
                );
                element_flags.push(ElementFlags::Optional);
                if !is_omitted_expression(&e.ref_(self)) && !self.has_default_value(e) {
                    self.report_implicit_any(e, self.any_type(), None)?;
                }
            }
        }
        self.create_tuple_type(
            &element_types,
            Some(&element_flags),
            Some(type_target.ref_(self).as_tuple_type().readonly),
            None,
        )
    }

    pub(super) fn widen_type_inferred_from_initializer(
        &self,
        declaration: Id<Node>, /*HasExpressionInitializer*/
        type_: Id<Type>,
    ) -> io::Result<Id<Type>> {
        let widened = if get_combined_node_flags(declaration, self).intersects(NodeFlags::Const)
            || is_declaration_readonly(declaration, self)
        {
            type_
        } else {
            self.get_widened_literal_type(type_)?
        };
        if is_in_js_file(Some(&declaration.ref_(self))) {
            if self.is_empty_literal_type(widened) {
                self.report_implicit_any(declaration, self.any_type(), None)?;
                return Ok(self.any_type());
            } else if self.is_empty_array_literal_type(widened)? {
                self.report_implicit_any(declaration, self.any_array_type(), None)?;
                return Ok(self.any_array_type());
            }
        }
        Ok(widened)
    }

    pub(super) fn is_literal_of_contextual_type(
        &self,
        candidate_type: Id<Type>,
        contextual_type: Option<Id<Type>>,
    ) -> io::Result<bool> {
        if let Some(contextual_type) = contextual_type {
            if contextual_type
                .ref_(self)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
            {
                let types = contextual_type
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types()
                    .to_owned();
                return try_some(
                    Some(&types),
                    Some(|&t: &Id<Type>| {
                        self.is_literal_of_contextual_type(candidate_type, Some(t))
                    }),
                );
            }
            if contextual_type
                .ref_(self)
                .flags()
                .intersects(TypeFlags::InstantiableNonPrimitive)
            {
                let constraint = self
                    .get_base_constraint_of_type(contextual_type)?
                    .unwrap_or_else(|| self.unknown_type());
                return Ok(self.maybe_type_of_kind(constraint, TypeFlags::String)
                    && self.maybe_type_of_kind(candidate_type, TypeFlags::StringLiteral)
                    || self.maybe_type_of_kind(constraint, TypeFlags::Number)
                        && self.maybe_type_of_kind(candidate_type, TypeFlags::NumberLiteral)
                    || self.maybe_type_of_kind(constraint, TypeFlags::BigInt)
                        && self.maybe_type_of_kind(candidate_type, TypeFlags::BigIntLiteral)
                    || self.maybe_type_of_kind(constraint, TypeFlags::ESSymbol)
                        && self.maybe_type_of_kind(candidate_type, TypeFlags::UniqueESSymbol)
                    || self.is_literal_of_contextual_type(candidate_type, Some(constraint))?);
            }
            return Ok(contextual_type.ref_(self).flags().intersects(
                TypeFlags::StringLiteral
                    | TypeFlags::Index
                    | TypeFlags::TemplateLiteral
                    | TypeFlags::StringMapping,
            ) && self.maybe_type_of_kind(candidate_type, TypeFlags::StringLiteral)
                || contextual_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::NumberLiteral)
                    && self.maybe_type_of_kind(candidate_type, TypeFlags::NumberLiteral)
                || contextual_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::BigIntLiteral)
                    && self.maybe_type_of_kind(candidate_type, TypeFlags::BigIntLiteral)
                || contextual_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::BooleanLiteral)
                    && self.maybe_type_of_kind(candidate_type, TypeFlags::BooleanLiteral)
                || contextual_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::UniqueESSymbol)
                    && self.maybe_type_of_kind(candidate_type, TypeFlags::UniqueESSymbol));
        }
        Ok(false)
    }

    pub(super) fn is_const_context(&self, node: Id<Node> /*Expression*/) -> bool {
        let parent = node.ref_(self).parent();
        is_assertion_expression(&parent.ref_(self))
            && is_const_type_reference(parent.ref_(self).as_has_type().maybe_type().unwrap(), self)
            || is_jsdoc_type_assertion(parent, self)
                && is_const_type_reference(get_jsdoc_type_assertion_type(parent, self), self)
            || (is_parenthesized_expression(&parent.ref_(self))
                || is_array_literal_expression(&parent.ref_(self))
                || is_spread_element(&parent.ref_(self)))
                && self.is_const_context(parent)
            || (is_property_assignment(&parent.ref_(self))
                || is_shorthand_property_assignment(&parent.ref_(self))
                || is_template_span(&parent.ref_(self)))
                && self.is_const_context(parent.ref_(self).parent())
    }

    pub(super) fn check_expression_for_mutable_location(
        &self,
        node: Id<Node>, /*Expression*/
        check_mode: Option<CheckMode>,
        contextual_type: Option<Id<Type>>,
        force_tuple: Option<bool>,
    ) -> io::Result<Id<Type>> {
        let type_ = self.check_expression(node, check_mode, force_tuple)?;
        Ok(if self.is_const_context(node) {
            self.get_regular_type_of_literal_type(type_)
        } else if self.is_type_assertion(node) {
            type_
        } else {
            self.get_widened_literal_like_type_for_contextual_type(
                type_,
                self.instantiate_contextual_type(
                    if contextual_type.is_none() {
                        self.get_contextual_type_(node, None)?
                    } else {
                        Some(contextual_type.unwrap())
                    },
                    node,
                    None,
                )?,
            )?
        })
    }

    pub(super) fn check_property_assignment(
        &self,
        node: Id<Node>, /*PropertyAssignment*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_property_assignment = node_ref.as_property_assignment();
        if node_as_property_assignment.name().ref_(self).kind() == SyntaxKind::ComputedPropertyName
        {
            self.check_computed_property_name(node_as_property_assignment.name())?;
        }
        self.check_expression_for_mutable_location(
            node_as_property_assignment.initializer,
            check_mode,
            None,
            None,
        )
    }

    pub(super) fn check_object_literal_method(
        &self,
        node: Id<Node>, /*MethodDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        self.check_grammar_method(node)?;

        let node_ref = node.ref_(self);
        let node_as_method_declaration = node_ref.as_method_declaration();
        if node_as_method_declaration.name().ref_(self).kind() == SyntaxKind::ComputedPropertyName {
            self.check_computed_property_name(node_as_method_declaration.name())?;
        }

        let uninstantiated_type =
            self.check_function_expression_or_object_literal_method(node, check_mode)?;
        self.instantiate_type_with_single_generic_call_signature(
            node,
            uninstantiated_type,
            check_mode,
        )
    }

    pub(super) fn instantiate_type_with_single_generic_call_signature(
        &self,
        node: Id<Node>, /*Expression | MethodDeclaration | QualifiedName*/
        type_: Id<Type>,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        if let Some(check_mode) = check_mode.filter(|check_mode| {
            check_mode.intersects(CheckMode::Inferential | CheckMode::SkipGenericFunctions)
        }) {
            let call_signature = self.get_single_signature(type_, SignatureKind::Call, true)?;
            let construct_signature =
                self.get_single_signature(type_, SignatureKind::Construct, true)?;
            let signature = call_signature
                .clone()
                .or_else(|| construct_signature.clone());
            if let Some(signature) =
                signature.filter(|signature| signature.ref_(self).maybe_type_parameters().is_some())
            {
                let contextual_type = self.get_apparent_type_of_contextual_type(
                    node,
                    Some(ContextFlags::NoConstraints),
                )?;
                if let Some(contextual_type) = contextual_type {
                    let contextual_signature = self.get_single_signature(
                        self.get_non_nullable_type(contextual_type)?,
                        if call_signature.is_some() {
                            SignatureKind::Call
                        } else {
                            SignatureKind::Construct
                        },
                        false,
                    )?;
                    if let Some(contextual_signature) =
                        contextual_signature.filter(|contextual_signature| {
                            contextual_signature
                                .ref_(self)
                                .maybe_type_parameters()
                                .is_none()
                        })
                    {
                        if check_mode.intersects(CheckMode::SkipGenericFunctions) {
                            self.skipped_generic_function(node, check_mode);
                            return Ok(self.any_function_type());
                        }
                        let context = self.get_inference_context(node).unwrap();
                        let return_type =
                            context.ref_(self).signature.try_map(|context_signature| {
                                self.get_return_type_of_signature(context_signature.clone())
                            })?;
                        let return_signature = return_type.try_and_then(|return_type| {
                            self.get_single_call_or_construct_signature(return_type)
                        })?;
                        if return_signature.matches(|return_signature| {
                            return_signature
                                .ref_(self)
                                .maybe_type_parameters()
                                .is_none()
                                && !every(
                                    &context.ref_(self).inferences(),
                                    |inference: &Id<InferenceInfo>, _| {
                                        self.has_inference_candidates(&inference.ref_(self))
                                    },
                                )
                        }) {
                            let unique_type_parameters = self.get_unique_type_parameters(
                                context,
                                signature
                                    .ref_(self)
                                    .maybe_type_parameters()
                                    .as_ref()
                                    .unwrap(),
                            );
                            let instantiated_signature = self
                                .get_signature_instantiation_without_filling_in_type_arguments(
                                    signature.clone(),
                                    Some(&unique_type_parameters),
                                )?;
                            let inferences = map(
                                &*context.ref_(self).inferences(),
                                |info: &Id<InferenceInfo>, _| {
                                    self.alloc_inference_info(
                                        self.create_inference_info(info.ref_(self).type_parameter),
                                    )
                                },
                            );
                            self.apply_to_parameter_types(
                                instantiated_signature,
                                contextual_signature,
                                |source: Id<Type>, target: Id<Type>| {
                                    self.infer_types(
                                        &inferences,
                                        source,
                                        target,
                                        Some(InferencePriority::None),
                                        Some(true),
                                    )
                                },
                            )?;
                            if some(
                                Some(&inferences),
                                Some(|inference: &Id<InferenceInfo>| {
                                    self.has_inference_candidates(&inference.ref_(self))
                                }),
                            ) {
                                self.apply_to_return_types(
                                    instantiated_signature.clone(),
                                    contextual_signature.clone(),
                                    |source: Id<Type>, target: Id<Type>| {
                                        self.infer_types(&inferences, source, target, None, None)?;

                                        Ok(())
                                    },
                                )?;
                                if !self.has_overlapping_inferences(
                                    &context.ref_(self).inferences(),
                                    &inferences,
                                ) {
                                    self.merge_inferences(
                                        &mut context.ref_(self).inferences_mut(),
                                        &inferences,
                                    );
                                    {
                                        let context_ref = context.ref_(self);
                                        let mut context_inferred_type_parameters =
                                            context_ref.maybe_inferred_type_parameters_mut();
                                        *context_inferred_type_parameters = Some(concatenate(
                                            context_inferred_type_parameters
                                                .clone()
                                                .unwrap_or_else(|| vec![]),
                                            unique_type_parameters,
                                        ));
                                    }
                                    return Ok(self.get_or_create_type_from_signature(
                                        instantiated_signature,
                                    ));
                                }
                            }
                        }
                        return Ok(self.get_or_create_type_from_signature(
                            self.instantiate_signature_in_context_of(
                                signature.clone(),
                                contextual_signature.clone(),
                                Some(context),
                                None,
                            )?,
                        ));
                    }
                }
            }
        }
        Ok(type_)
    }

    pub(super) fn skipped_generic_function(&self, node: Id<Node>, check_mode: CheckMode) {
        if check_mode.intersects(CheckMode::Inferential) {
            let context = self.get_inference_context(node).unwrap();
            context
                .ref_(self)
                .set_flags(context.ref_(self).flags() | InferenceFlags::SkippedGenericFunction);
        }
    }

    pub(super) fn has_inference_candidates(&self, info: &InferenceInfo) -> bool {
        info.maybe_candidates().is_some() || info.maybe_contra_candidates().is_some()
    }

    pub(super) fn has_overlapping_inferences(
        &self,
        a: &[Id<InferenceInfo>],
        b: &[Id<InferenceInfo>],
    ) -> bool {
        for i in 0..a.len() {
            if self.has_inference_candidates(&a[i].ref_(self))
                && self.has_inference_candidates(&b[i].ref_(self))
            {
                return true;
            }
        }
        false
    }

    pub(super) fn merge_inferences(
        &self,
        target: &mut Vec<Id<InferenceInfo>>,
        source: &[Id<InferenceInfo>],
    ) {
        for i in 0..target.len() {
            if !self.has_inference_candidates(&target[i].ref_(self))
                && self.has_inference_candidates(&source[i].ref_(self))
            {
                target[i] = source[i].clone();
            }
        }
    }

    pub(super) fn get_unique_type_parameters(
        &self,
        context: Id<InferenceContext>,
        type_parameters: &[Id<Type /*TypeParameter*/>],
    ) -> Vec<Id<Type /*TypeParameter*/>> {
        let mut result: Vec<Id<Type /*TypeParameter*/>> = vec![];
        let mut old_type_parameters: Option<Vec<Id<Type /*TypeParameter*/>>> = None;
        let mut new_type_parameters: Option<Vec<Id<Type /*TypeParameter*/>>> = None;
        for &tp in type_parameters {
            let tp_symbol = tp.ref_(self).symbol();
            let tp_symbol_ref = tp_symbol.ref_(self);
            let name = tp_symbol_ref.escaped_name();
            if self.has_type_parameter_by_name(
                context
                    .ref_(self)
                    .maybe_inferred_type_parameters()
                    .as_deref(),
                name,
            ) || self.has_type_parameter_by_name(Some(&result), name)
            {
                let new_name = self.get_unique_type_parameter_name(
                    &concatenate(
                        context
                            .ref_(self)
                            .maybe_inferred_type_parameters()
                            .clone()
                            .unwrap_or_else(|| vec![]),
                        result.clone(),
                    ),
                    name,
                );
                let symbol = self.alloc_symbol(
                    self.create_symbol(SymbolFlags::TypeParameter, new_name, None)
                        .into(),
                );
                let mut new_type_parameter = self.create_type_parameter(Some(symbol));
                new_type_parameter.target = Some(tp.clone());
                let new_type_parameter: Id<Type> = self.alloc_type(new_type_parameter.into());
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
            for &tp in new_type_parameters {
                tp.ref_(self).as_type_parameter().set_mapper(mapper.clone());
            }
        }
        result
    }

    pub(super) fn has_type_parameter_by_name(
        &self,
        type_parameters: Option<&[Id<Type /*TypeParameter*/>]>,
        name: &str, /*__String*/
    ) -> bool {
        some(
            type_parameters,
            Some(|&tp: &Id<Type>| tp.ref_(self).symbol().ref_(self).escaped_name() == name),
        )
    }

    pub(super) fn get_unique_type_parameter_name(
        &self,
        type_parameters: &[Id<Type /*TypeParameter*/>],
        base_name: &str, /*__String*/
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
            let augmented_name = format!("{}{}", s, index);
            if !self.has_type_parameter_by_name(Some(type_parameters), &augmented_name) {
                return augmented_name;
            }
            index += 1;
        }
    }

    pub(super) fn get_return_type_of_single_non_generic_call_signature(
        &self,
        func_type: Id<Type>,
    ) -> io::Result<Option<Id<Type>>> {
        let signature = self.get_single_call_signature(func_type)?;
        signature
            .filter(|signature| signature.ref_(self).maybe_type_parameters().is_none())
            .try_map(|signature| self.get_return_type_of_signature(signature))
    }

    pub(super) fn get_return_type_of_single_non_generic_signature_of_call_chain(
        &self,
        expr: Id<Node>, /*CallChain*/
    ) -> io::Result<Option<Id<Type>>> {
        let expr_ref = expr.ref_(self);
        let expr_as_call_expression = expr_ref.as_call_expression();
        let func_type = self.check_expression(expr_as_call_expression.expression, None, None)?;
        let non_optional_type =
            self.get_optional_expression_type(func_type, expr_as_call_expression.expression)?;
        let return_type = self.get_return_type_of_single_non_generic_call_signature(func_type)?;
        return_type.try_map(|return_type| {
            self.propagate_optional_type_marker(return_type, expr, non_optional_type != func_type)
        })
    }

    pub(super) fn get_type_of_expression(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> io::Result<Id<Type>> {
        let quick_type = self.get_quick_type_of_expression(node)?;
        if let Some(quick_type) = quick_type {
            return Ok(quick_type);
        }
        if node.ref_(self).flags().intersects(NodeFlags::TypeCached) {
            if let Some(flow_type_cache) = self.maybe_flow_type_cache().as_ref() {
                let cached_type = flow_type_cache.get(&get_node_id(&node.ref_(self)));
                if let Some(cached_type) = cached_type {
                    return Ok(cached_type.clone());
                }
            }
        }
        let start_invocation_count = self.flow_invocation_count();
        let type_ = self.check_expression(node, None, None)?;
        if self.flow_invocation_count() != start_invocation_count {
            let mut flow_type_cache = self.maybe_flow_type_cache();
            if flow_type_cache.is_none() {
                *flow_type_cache = Some(HashMap::new());
            }
            let cache = flow_type_cache.as_mut().unwrap();
            cache.insert(get_node_id(&node.ref_(self)), type_.clone());
            set_node_flags(
                Some(&node.ref_(self)),
                node.ref_(self).flags() | NodeFlags::TypeCached,
            );
        }
        Ok(type_)
    }

    pub(super) fn get_quick_type_of_expression(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> io::Result<Option<Id<Type>>> {
        let mut expr = skip_parentheses(node, Some(true), self);
        if is_jsdoc_type_assertion(expr, self) {
            let type_ = get_jsdoc_type_assertion_type(expr, self);
            if !is_const_type_reference(type_, self) {
                return Ok(Some(self.get_type_from_type_node_(type_)?));
            }
        }
        expr = skip_parentheses(node, None, self);
        if is_call_expression(&node.ref_(self))
            && expr
                .ref_(self)
                .as_call_expression()
                .expression
                .ref_(self)
                .kind()
                != SyntaxKind::SuperKeyword
            && !is_require_call(expr, true, self)
            && !self.is_symbol_or_symbol_for_call(expr)?
        {
            let type_ = if is_call_chain(&expr.ref_(self)) {
                self.get_return_type_of_single_non_generic_signature_of_call_chain(expr)?
            } else {
                self.get_return_type_of_single_non_generic_call_signature(
                    self.check_non_null_expression(
                        expr.ref_(self).as_call_expression().expression,
                    )?,
                )?
            };
            if type_.is_some() {
                return Ok(type_);
            }
        } else if is_assertion_expression(&expr.ref_(self))
            && !is_const_type_reference(expr.ref_(self).as_has_type().maybe_type().unwrap(), self)
        {
            return Ok(Some(self.get_type_from_type_node_(
                expr.ref_(self).as_has_type().maybe_type().unwrap(),
            )?));
        } else if matches!(
            node.ref_(self).kind(),
            SyntaxKind::NumericLiteral
                | SyntaxKind::StringLiteral
                | SyntaxKind::TrueKeyword
                | SyntaxKind::FalseKeyword
        ) {
            return Ok(Some(self.check_expression(node, None, None)?));
        }
        Ok(None)
    }

    pub(super) fn get_context_free_type_of_expression(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if let Some(links_context_free_type) = links.ref_(self).context_free_type.clone() {
            return Ok(links_context_free_type);
        }
        let save_contextual_type = node.ref_(self).maybe_contextual_type();
        node.ref_(self).set_contextual_type(Some(self.any_type()));
        // try {
        let type_ = self.check_expression(node, Some(CheckMode::SkipContextSensitive), None)?;
        links.ref_mut(self).context_free_type = Some(type_.clone());
        // }
        // finally {
        node.ref_(self).set_contextual_type(save_contextual_type);
        // }
        Ok(type_)
    }

    pub(super) fn check_expression(
        &self,
        node: Id<Node>, /*Expression | QualifiedName*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> io::Result<Id<Type>> {
        // tracing?.push(tracing.Phase.Check, "checkExpression", { kind: node.kind, pos: node.pos, end: node.end });
        let save_current_node = self.maybe_current_node();
        self.set_current_node(Some(node));
        self.set_instantiation_count(0);
        let uninstantiated_type = self.check_expression_worker(node, check_mode, force_tuple)?;
        let type_ = self.instantiate_type_with_single_generic_call_signature(
            node,
            uninstantiated_type,
            check_mode,
        )?;
        if self.is_const_enum_object_type(type_) {
            self.check_const_enum_access(node, type_);
        }
        self.set_current_node(save_current_node);
        // tracing?.pop();
        Ok(type_)
    }

    pub(super) fn check_const_enum_access(
        &self,
        node: Id<Node>, /*Expression | QualifiedName*/
        type_: Id<Type>,
    ) {
        let ok = (node.ref_(self).parent().ref_(self).kind()
            == SyntaxKind::PropertyAccessExpression
            && node
                .ref_(self)
                .parent()
                .ref_(self)
                .as_property_access_expression()
                .expression
                == node)
            || (node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ElementAccessExpression
                && node
                    .ref_(self)
                    .parent()
                    .ref_(self)
                    .as_element_access_expression()
                    .expression
                    == node)
            || (matches!(
                node.ref_(self).kind(),
                SyntaxKind::Identifier | SyntaxKind::QualifiedName
            ) && self.is_in_right_side_of_import_or_export_assignment(node)
                || node.ref_(self).parent().ref_(self).kind() == SyntaxKind::TypeQuery
                    && node
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .as_type_query_node()
                        .expr_name
                        == node)
            || node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ExportSpecifier;

        if !ok {
            self.error(
                Some(node),
                &Diagnostics::const_enums_can_only_be_used_in_property_or_index_access_expressions_or_the_right_hand_side_of_an_import_declaration_or_export_assignment_or_type_query,
                None,
            );
        }

        if self.compiler_options.ref_(self).isolated_modules == Some(true) {
            Debug_.assert(
                type_
                    .ref_(self)
                    .symbol()
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::ConstEnum),
                None,
            );
            let const_enum_declaration = type_
                .ref_(self)
                .symbol()
                .ref_(self)
                .maybe_value_declaration()
                .unwrap();
            if const_enum_declaration
                .ref_(self)
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
        node: Id<Node>, /*ParenthesizedExpression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        if has_jsdoc_nodes(&node.ref_(self)) && is_jsdoc_type_assertion(node, self) {
            let type_ = get_jsdoc_type_assertion_type(node, self);
            return self.check_assertion_worker(
                type_,
                type_,
                node.ref_(self).as_parenthesized_expression().expression,
                check_mode,
            );
        }
        self.check_expression(
            released!(node.ref_(self).as_parenthesized_expression().expression),
            check_mode,
            None,
        )
    }

    pub(super) fn check_expression_worker(
        &self,
        node: Id<Node>, /*Expression*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> io::Result<Id<Type>> {
        let kind = node.ref_(self).kind();
        if let Some(cancellation_token) = self.maybe_cancellation_token() {
            match kind {
                SyntaxKind::ClassExpression
                | SyntaxKind::FunctionExpression
                | SyntaxKind::ArrowFunction => {
                    cancellation_token
                        .ref_(self)
                        .throw_if_cancellation_requested();
                }
                _ => (),
            }
        }
        Ok(match kind {
            SyntaxKind::Identifier => self.check_identifier(node, check_mode)?,
            SyntaxKind::PrivateIdentifier => self.check_private_identifier_expression(node),
            SyntaxKind::ThisKeyword => self.check_this_expression(node)?,
            SyntaxKind::SuperKeyword => self.check_super_expression(node)?,
            SyntaxKind::NullKeyword => self.null_widening_type(),
            SyntaxKind::NoSubstitutionTemplateLiteral | SyntaxKind::StringLiteral => self
                .get_fresh_type_of_literal_type(
                    self.get_string_literal_type(&node.ref_(self).as_literal_like_node().text()),
                ),
            SyntaxKind::NumericLiteral => {
                self.check_grammar_numeric_literal(node);
                self.get_fresh_type_of_literal_type(
                    self.get_number_literal_type(Number::new(
                        node.ref_(self)
                            .as_literal_like_node()
                            .text()
                            .parse::<f64>()
                            .unwrap(),
                    )),
                )
            }
            SyntaxKind::BigIntLiteral => {
                self.check_grammar_big_int_literal(node);
                self.get_fresh_type_of_literal_type(self.get_big_int_literal_type(
                    PseudoBigInt::new(
                        false,
                        parse_pseudo_big_int(&node.ref_(self).as_literal_like_node().text()),
                    ),
                ))
            }
            SyntaxKind::TrueKeyword => self.true_type(),
            SyntaxKind::FalseKeyword => self.false_type(),
            SyntaxKind::TemplateExpression => self.check_template_expression(node)?,
            SyntaxKind::RegularExpressionLiteral => self.global_reg_exp_type(),
            SyntaxKind::ArrayLiteralExpression => {
                self.check_array_literal(node, check_mode, force_tuple)?
            }
            SyntaxKind::ObjectLiteralExpression => self.check_object_literal(node, check_mode)?,
            SyntaxKind::PropertyAccessExpression => {
                self.check_property_access_expression(node, check_mode)?
            }
            SyntaxKind::QualifiedName => self.check_qualified_name(node, check_mode)?,
            SyntaxKind::ElementAccessExpression => self.check_indexed_access(node, check_mode)?,
            SyntaxKind::CallExpression => {
                if node
                    .ref_(self)
                    .as_call_expression()
                    .expression
                    .ref_(self)
                    .kind()
                    == SyntaxKind::ImportKeyword
                {
                    return self.check_import_call_expression(node);
                }
                self.check_call_expression(node, check_mode)?
            }
            SyntaxKind::NewExpression => self.check_call_expression(node, check_mode)?,
            SyntaxKind::TaggedTemplateExpression => self.check_tagged_template_expression(node)?,
            SyntaxKind::ParenthesizedExpression => {
                self.check_parenthesized_expression(node, check_mode)?
            }
            SyntaxKind::ClassExpression => self.check_class_expression(node)?,
            SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction => {
                self.check_function_expression_or_object_literal_method(node, check_mode)?
            }
            SyntaxKind::TypeOfExpression => self.check_type_of_expression(node)?,
            SyntaxKind::TypeAssertionExpression | SyntaxKind::AsExpression => {
                self.check_assertion(node)?
            }
            SyntaxKind::NonNullExpression => self.check_non_null_assertion(node)?,
            SyntaxKind::MetaProperty => self.check_meta_property(node)?,
            SyntaxKind::DeleteExpression => self.check_delete_expression(node)?,
            SyntaxKind::VoidExpression => self.check_void_expression(node)?,
            SyntaxKind::AwaitExpression => self.check_await_expression(node)?,
            SyntaxKind::PrefixUnaryExpression => self.check_prefix_unary_expression(node)?,
            SyntaxKind::PostfixUnaryExpression => self.check_postfix_unary_expression(node)?,
            SyntaxKind::BinaryExpression => self
                .check_binary_expression()
                .ref_(self)
                .call(node, check_mode)?,
            SyntaxKind::ConditionalExpression => {
                self.check_conditional_expression(node, check_mode)?
            }
            SyntaxKind::SpreadElement => self.check_spread_expression(node, check_mode)?,
            SyntaxKind::OmittedExpression => self.undefined_widening_type(),
            SyntaxKind::YieldExpression => self.check_yield_expression(node)?,
            SyntaxKind::SyntheticExpression => self.check_synthetic_expression(node)?,
            SyntaxKind::JsxExpression => self.check_jsx_expression(node, check_mode)?,
            SyntaxKind::JsxElement => self.check_jsx_element(node, check_mode)?,
            SyntaxKind::JsxSelfClosingElement => {
                self.check_jsx_self_closing_element(node, check_mode)?
            }
            SyntaxKind::JsxFragment => self.check_jsx_fragment(node)?,
            SyntaxKind::JsxAttributes => self.check_jsx_attributes(node, check_mode)?,
            SyntaxKind::JsxOpeningElement => {
                Debug_.fail(Some("Shouldn't ever directly check a JsxOpeningElement"))
            }
            _ => self.error_type(),
        })
    }
}
