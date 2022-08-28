#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, IterationTypeKind, IterationUse, UnusedKind};
use crate::{
    are_option_rcs_equal, expression_result_is_unused, for_each, get_assigned_expando_initializer,
    get_combined_node_flags, get_containing_function,
    get_containing_function_or_class_static_block, get_effective_initializer, get_function_flags,
    is_assignment_operator, is_binding_element, is_element_access_expression,
    is_function_or_module_block, is_identifier, is_jsdoc_typedef_tag, is_jsx_self_closing_element,
    is_object_literal_expression, is_parenthesized_expression, is_private_identifier,
    is_property_access_expression, map, maybe_for_each, parse_pseudo_big_int, token_to_string,
    unescape_leading_underscores, AssignmentDeclarationKind, Diagnostic, DiagnosticMessage,
    Diagnostics, ExternalEmitHelpers, FunctionFlags, HasTypeParametersInterface, InferenceContext,
    InferenceInfo, IterationTypes, IterationTypesResolver, LiteralLikeNodeInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeFlags, NodeInterface, PseudoBigInt,
    ScriptTarget, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface, UnionOrIntersectionTypeInterface, UnionReduction,
};

impl TypeChecker {
    pub(super) fn both_are_big_int_like(&self, left: &Type, right: &Type) -> bool {
        self.is_type_assignable_to_kind(left, TypeFlags::BigIntLike, None)
            && self.is_type_assignable_to_kind(right, TypeFlags::BigIntLike, None)
    }

    pub(super) fn check_assignment_declaration(
        &self,
        kind: AssignmentDeclarationKind,
        right_type: &Type,
    ) {
        if kind == AssignmentDeclarationKind::ModuleExports {
            for prop in &self.get_properties_of_object_type(right_type) {
                let prop_type = self.get_type_of_symbol(prop);
                if matches!(
                    prop_type.maybe_symbol().as_ref(),
                    Some(prop_type_symbol) if prop_type_symbol.flags().intersects(SymbolFlags::Class)
                ) {
                    let name = prop.escaped_name();
                    let symbol = self.resolve_name_(
                        prop.maybe_value_declaration(),
                        name,
                        SymbolFlags::Type,
                        None,
                        Some(name.clone()),
                        false,
                        None,
                    );
                    if matches!(
                        symbol.as_ref().and_then(|symbol| symbol.maybe_declarations().clone()).as_ref(),
                        Some(symbol_declarations) if symbol_declarations.iter().any(|symbol_declaration| is_jsdoc_typedef_tag(symbol_declaration))
                    ) {
                        self.add_duplicate_declaration_errors_for_symbols(
                            symbol.as_ref().unwrap(),
                            &Diagnostics::Duplicate_identifier_0,
                            &unescape_leading_underscores(name),
                            prop,
                        );
                        self.add_duplicate_declaration_errors_for_symbols(
                            prop,
                            &Diagnostics::Duplicate_identifier_0,
                            &unescape_leading_underscores(name),
                            symbol.as_ref().unwrap(),
                        );
                    }
                }
            }
        }
    }

    pub(super) fn is_eval_node(&self, node: &Node /*Expression*/) -> bool {
        node.kind() == SyntaxKind::Identifier && node.as_identifier().escaped_text.eq_str("eval")
    }

    pub(super) fn check_for_disallowed_es_symbol_operand(
        &self,
        left_type: &Type,
        right_type: &Type,
        left: &Node,
        right: &Node,
        operator: SyntaxKind,
    ) -> bool {
        let offending_symbol_operand =
            if self.maybe_type_of_kind(left_type, TypeFlags::ESSymbolLike) {
                Some(left.node_wrapper())
            } else if self.maybe_type_of_kind(right_type, TypeFlags::ESSymbolLike) {
                Some(right.node_wrapper())
            } else {
                None
            };

        if let Some(offending_symbol_operand) = offending_symbol_operand.as_ref() {
            self.error(
                Some(&**offending_symbol_operand),
                &Diagnostics::The_0_operator_cannot_be_applied_to_type_symbol,
                Some(vec![token_to_string(operator).unwrap().to_owned()]),
            );
            return false;
        }

        true
    }

    pub(super) fn get_suggested_boolean_operator(
        &self,
        operator: SyntaxKind,
    ) -> Option<SyntaxKind> {
        match operator {
            SyntaxKind::BarToken | SyntaxKind::BarEqualsToken => Some(SyntaxKind::BarBarToken),
            SyntaxKind::CaretToken | SyntaxKind::CaretEqualsToken => {
                Some(SyntaxKind::ExclamationEqualsEqualsToken)
            }
            SyntaxKind::AmpersandToken | SyntaxKind::AmpersandEqualsToken => {
                Some(SyntaxKind::AmpersandAmpersandToken)
            }
            _ => None,
        }
    }

    pub(super) fn check_assignment_operator(
        &self,
        operator: SyntaxKind,
        left: &Node,
        left_type: &Type,
        right: &Node,
        value_type: &Type,
    ) {
        if self.produce_diagnostics && is_assignment_operator(operator) {
            if self.check_reference_expression(
                left,
                &Diagnostics::The_left_hand_side_of_an_assignment_expression_must_be_a_variable_or_a_property_access,
                &Diagnostics::The_left_hand_side_of_an_assignment_expression_may_not_be_an_optional_property_access,
            ) && (
                !is_identifier(left) ||
                unescape_leading_underscores(&left.as_identifier().escaped_text) != "exports"
            ) {
                let mut head_message: Option<&'static DiagnosticMessage> = None;
                if self.exact_optional_property_types == Some(true) && is_property_access_expression(left) && self.maybe_type_of_kind(
                    value_type,
                    TypeFlags::Undefined
                ) {
                    let left_as_property_access_expression = left.as_property_access_expression();
                    let target = self.get_type_of_property_of_type_(
                        &self.get_type_of_expression(&left_as_property_access_expression.expression),
                        &left_as_property_access_expression.name.as_member_name().escaped_text()
                    );
                    if self.is_exact_optional_property_mismatch(
                        Some(value_type),
                        target
                    ) {
                        head_message = Some(&Diagnostics::Type_0_is_not_assignable_to_type_1_with_exactOptionalPropertyTypes_Colon_true_Consider_adding_undefined_to_the_type_of_the_target);
                    }
                }
                self.check_type_assignable_to_and_optionally_elaborate(
                    value_type,
                    left_type,
                    Some(left),
                    Some(right),
                    head_message,
                    None,
                );
            }
        }
    }

    pub(super) fn is_assignment_declaration(
        &self,
        left: &Node,
        right: &Node,
        kind: AssignmentDeclarationKind,
    ) -> bool {
        match kind {
            AssignmentDeclarationKind::ModuleExports => true,
            AssignmentDeclarationKind::ExportsProperty
            | AssignmentDeclarationKind::Property
            | AssignmentDeclarationKind::Prototype
            | AssignmentDeclarationKind::PrototypeProperty
            | AssignmentDeclarationKind::ThisProperty => {
                let symbol = self.get_symbol_of_node(left);
                let init = get_assigned_expando_initializer(Some(right));
                matches!(
                    init.as_ref(),
                    Some(init) if is_object_literal_expression(init)
                ) && matches!(
                    symbol.as_ref().and_then(|symbol| symbol.maybe_exports().clone()),
                    Some(symbol_exports) if !(*symbol_exports).borrow().is_empty()
                )
            }
            _ => false,
        }
    }

    pub(super) fn report_operator_error_unless<
        TErrorNode: Borrow<Node>,
        TTypesAreCompatible: FnMut(&Type, &Type) -> bool,
    >(
        &self,
        left_type: &Type,
        right_type: &Type,
        operator_token: &Node,
        error_node: Option<TErrorNode>,
        mut types_are_compatible: TTypesAreCompatible,
    ) -> bool {
        if !types_are_compatible(left_type, right_type) {
            self.report_operator_error(
                error_node,
                operator_token,
                left_type,
                right_type,
                Some(types_are_compatible),
            );
            return true;
        }
        false
    }

    pub(super) fn report_operator_error<
        TErrorNode: Borrow<Node>,
        TIsRelated: FnMut(&Type, &Type) -> bool,
    >(
        &self,
        error_node: Option<TErrorNode>,
        operator_token: &Node,
        left_type: &Type,
        right_type: &Type,
        mut is_related: Option<TIsRelated>,
    ) {
        let mut would_work_with_await = false;
        let err_node = error_node.map_or_else(
            || operator_token.node_wrapper(),
            |error_node| error_node.borrow().node_wrapper(),
        );
        if let Some(is_related) = is_related.as_mut() {
            let awaited_left_type =
                self.get_awaited_type_no_alias(left_type, Option::<&Node>::None, None, None);
            let awaited_right_type =
                self.get_awaited_type_no_alias(right_type, Option::<&Node>::None, None, None);
            would_work_with_await = !(are_option_rcs_equal(
                awaited_left_type.as_ref(),
                Some(&left_type.type_wrapper()),
            ) && are_option_rcs_equal(
                awaited_right_type.as_ref(),
                Some(&right_type.type_wrapper()),
            )) && awaited_left_type.is_some()
                && awaited_right_type.is_some()
                && is_related(
                    awaited_left_type.as_ref().unwrap(),
                    awaited_right_type.as_ref().unwrap(),
                );
        }

        let mut effective_left = left_type.type_wrapper();
        let mut effective_right = right_type.type_wrapper();
        if !would_work_with_await {
            if let Some(is_related) = is_related.as_mut() {
                (effective_left, effective_right) =
                    self.get_base_types_if_unrelated(left_type, right_type, is_related);
            }
        }
        let (left_str, right_str) =
            self.get_type_names_for_error_display(&effective_left, &effective_right);
        if self
            .try_give_better_primary_error(
                operator_token,
                &err_node,
                would_work_with_await,
                &left_str,
                &right_str,
            )
            .is_none()
        {
            self.error_and_maybe_suggest_await(
                &err_node,
                would_work_with_await,
                &Diagnostics::Operator_0_cannot_be_applied_to_types_1_and_2,
                Some(vec![
                    token_to_string(operator_token.kind()).unwrap().to_owned(),
                    left_str,
                    right_str,
                ]),
            );
        }
    }

    pub(super) fn try_give_better_primary_error(
        &self,
        operator_token: &Node,
        err_node: &Node,
        maybe_missing_await: bool,
        left_str: &str,
        right_str: &str,
    ) -> Option<Rc<Diagnostic>> {
        let mut type_name: Option<&'static str> = None;
        match operator_token.kind() {
            SyntaxKind::EqualsEqualsEqualsToken | SyntaxKind::EqualsEqualsToken => {
                type_name = Some("false");
            }
            SyntaxKind::ExclamationEqualsEqualsToken | SyntaxKind::ExclamationEqualsToken => {
                type_name = Some("true");
            }
            _ => (),
        }

        if let Some(type_name) = type_name {
            return Some(self.error_and_maybe_suggest_await(
                err_node,
                maybe_missing_await,
                &Diagnostics::This_condition_will_always_return_0_since_the_types_1_and_2_have_no_overlap,
                Some(vec![
                    type_name.to_owned(),
                    left_str.to_owned(),
                    right_str.to_owned(),
                ])
            ));
        }

        None
    }

    pub(super) fn get_base_types_if_unrelated<TIsRelated: FnMut(&Type, &Type) -> bool>(
        &self,
        left_type: &Type,
        right_type: &Type,
        is_related: &mut TIsRelated,
    ) -> (Rc<Type>, Rc<Type>) {
        let mut effective_left = left_type.type_wrapper();
        let mut effective_right = right_type.type_wrapper();
        let left_base = self.get_base_type_of_literal_type(left_type);
        let right_base = self.get_base_type_of_literal_type(right_type);
        if !is_related(&left_base, &right_base) {
            effective_left = left_base;
            effective_right = right_base;
        }
        (effective_left, effective_right)
    }

    pub(super) fn check_yield_expression(&self, node: &Node /*YieldExpression*/) -> Rc<Type> {
        if self.produce_diagnostics {
            if !node.flags().intersects(NodeFlags::YieldContext) {
                self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::A_yield_expression_is_only_allowed_in_a_generator_body,
                    None,
                );
            }

            if self.is_in_parameter_initializer_before_containing_function(node) {
                self.error(
                    Some(node),
                    &Diagnostics::yield_expressions_cannot_be_used_in_a_parameter_initializer,
                    None,
                );
            }
        }

        let func = get_containing_function(node);
        if func.is_none() {
            return self.any_type();
        }
        let func = func.unwrap();
        let function_flags = get_function_flags(Some(&*func));

        if !function_flags.intersects(FunctionFlags::Generator) {
            return self.any_type();
        }

        let is_async = function_flags.intersects(FunctionFlags::Async);
        let node_as_yield_expression = node.as_yield_expression();
        if node_as_yield_expression.asterisk_token.is_some() {
            if is_async && self.language_version < ScriptTarget::ESNext {
                self.check_external_emit_helpers(node, ExternalEmitHelpers::AsyncDelegatorIncludes);
            }

            if !is_async
                && self.language_version < ScriptTarget::ES2015
                && self.compiler_options.downlevel_iteration == Some(true)
            {
                self.check_external_emit_helpers(node, ExternalEmitHelpers::Values);
            }
        }

        let return_type = self.get_return_type_from_annotation(&func);
        let iteration_types = return_type.as_ref().and_then(|return_type| {
            self.get_iteration_types_of_generator_function_return_type(return_type, is_async)
        });
        let signature_yield_type = iteration_types
            .as_ref()
            .map(|iteration_types| iteration_types.yield_type())
            .unwrap_or_else(|| self.any_type());
        let signature_next_type = iteration_types
            .as_ref()
            .map(|iteration_types| iteration_types.next_type())
            .unwrap_or_else(|| self.any_type());
        let resolved_signature_next_type = if is_async {
            self.get_awaited_type_(&signature_next_type, Option::<&Node>::None, None, None)
                .unwrap_or_else(|| self.any_type())
        } else {
            signature_next_type
        };
        let yield_expression_type =
            if let Some(node_expression) = node_as_yield_expression.expression.as_ref() {
                self.check_expression(node_expression, None, None)
            } else {
                self.undefined_widening_type()
            };
        let yielded_type = self.get_yielded_type_of_yield_expression(
            node,
            &yield_expression_type,
            &resolved_signature_next_type,
            is_async,
        );
        if return_type.is_some() {
            if let Some(yielded_type) = yielded_type.as_ref() {
                self.check_type_assignable_to_and_optionally_elaborate(
                    yielded_type,
                    &signature_yield_type,
                    Some(
                        node_as_yield_expression
                            .expression
                            .clone()
                            .unwrap_or_else(|| node.node_wrapper()),
                    ),
                    node_as_yield_expression.expression.as_deref(),
                    None,
                    None,
                );
            }
        }

        if node_as_yield_expression.asterisk_token.is_some() {
            let use_ = if is_async {
                IterationUse::AsyncYieldStar
            } else {
                IterationUse::YieldStar
            };
            return self
                .get_iteration_type_of_iterable(
                    use_,
                    IterationTypeKind::Return,
                    &yield_expression_type,
                    node_as_yield_expression.expression.as_deref(),
                )
                .unwrap_or_else(|| self.any_type());
        } else if let Some(return_type) = return_type.as_ref() {
            return self
                .get_iteration_type_of_generator_function_return_type(
                    IterationTypeKind::Next,
                    return_type,
                    is_async,
                )
                .unwrap_or_else(|| self.any_type());
        }
        let mut type_ = self.get_contextual_iteration_type(IterationTypeKind::Next, &func);
        if type_.is_none() {
            type_ = Some(self.any_type());
            if self.produce_diagnostics
                && self.no_implicit_any
                && !expression_result_is_unused(node)
            {
                let contextual_type = self.get_contextual_type_(node, None);
                if match contextual_type.as_ref() {
                    None => true,
                    Some(contextual_type) => self.is_type_any(Some(&**contextual_type)),
                } {
                    self.error(
                        Some(node),
                        &Diagnostics::yield_expression_implicitly_results_in_an_any_type_because_its_containing_generator_lacks_a_return_type_annotation,
                        None,
                    );
                }
            }
        }
        type_.unwrap()
    }

    pub(super) fn check_conditional_expression(
        &self,
        node: &Node, /*ConditionalExpression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let node_as_conditional_expression = node.as_conditional_expression();
        let type_ =
            self.check_truthiness_expression(&node_as_conditional_expression.condition, None);
        self.check_testing_known_truthy_callable_or_awaitable_type(
            &node_as_conditional_expression.condition,
            &type_,
            Some(&*node_as_conditional_expression.when_true),
        );
        let type1 =
            self.check_expression(&node_as_conditional_expression.when_true, check_mode, None);
        let type2 =
            self.check_expression(&node_as_conditional_expression.when_false, check_mode, None);
        self.get_union_type(
            vec![type1, type2],
            Some(UnionReduction::Subtype),
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        )
    }

    pub(super) fn is_template_literal_context(&self, node: &Node) -> bool {
        let parent = node.parent();
        is_parenthesized_expression(node) && self.is_template_literal_context(&parent)
            || is_element_access_expression(&parent)
                && ptr::eq(
                    &*parent.as_element_access_expression().argument_expression,
                    node,
                )
    }

    pub(super) fn check_template_expression(
        &self,
        node: &Node, /*TemplateExpression*/
    ) -> Rc<Type> {
        let node_as_template_expression = node.as_template_expression();
        let mut texts = vec![node_as_template_expression
            .head
            .as_literal_like_node()
            .text()];
        let mut types = vec![];
        for span in node_as_template_expression.template_spans.iter() {
            let span = span.as_template_span();
            let type_ = self.check_expression(&span.expression, None, None);
            if self.maybe_type_of_kind(&type_, TypeFlags::ESSymbolLike) {
                self.error(
                    Some(&*span.expression),
                    &Diagnostics::Implicit_conversion_of_a_symbol_to_a_string_will_fail_at_runtime_Consider_wrapping_this_expression_in_String,
                    None,
                );
            }
            texts.push(span.literal.as_literal_like_node().text());
            types.push(
                if self.is_type_assignable_to(&type_, &self.template_constraint_type()) {
                    type_
                } else {
                    self.string_type()
                },
            );
        }
        if self.is_const_context(node)
            || self.is_template_literal_context(node)
            || self.some_type(
                &self
                    .get_contextual_type_(node, None)
                    .unwrap_or_else(|| self.unknown_type()),
                |type_: &Type| self.is_template_literal_contextual_type(type_),
            )
        {
            self.get_template_literal_type(
                &texts
                    .into_iter()
                    .map(|text| text.clone())
                    .collect::<Vec<_>>(),
                &types,
            )
        } else {
            self.string_type()
        }
    }

    pub(super) fn is_template_literal_contextual_type(&self, type_: &Type) -> bool {
        type_
            .flags()
            .intersects(TypeFlags::StringLiteral | TypeFlags::TemplateLiteral)
            || type_
                .flags()
                .intersects(TypeFlags::InstantiableNonPrimitive)
                && self.maybe_type_of_kind(
                    &self
                        .get_base_constraint_of_type(type_)
                        .unwrap_or_else(|| self.unknown_type()),
                    TypeFlags::StringLike,
                )
    }

    pub(super) fn get_context_node(&self, node: &Node /*Expression*/) -> Rc<Node> {
        if node.kind() == SyntaxKind::JsxAttributes && !is_jsx_self_closing_element(&node.parent())
        {
            return node.parent().parent();
        }
        node.node_wrapper()
    }

    pub(super) fn check_expression_with_contextual_type(
        &self,
        node: &Node, /*Expression*/
        contextual_type: &Type,
        inference_context: Option<Rc<InferenceContext>>,
        check_mode: CheckMode,
    ) -> Rc<Type> {
        let context = self.get_context_node(node);
        let save_contextual_type = context.maybe_contextual_type().clone();
        let save_inference_context = context.maybe_inference_context().clone();
        // try {
        *context.maybe_contextual_type() = Some(contextual_type.type_wrapper());
        *context.maybe_inference_context() = inference_context.clone();
        let type_ = self.check_expression(
            node,
            Some(
                check_mode
                    | CheckMode::Contextual
                    | if inference_context.is_some() {
                        CheckMode::Inferential
                    } else {
                        CheckMode::Normal
                    },
            ),
            None,
        );
        let result = if self.maybe_type_of_kind(&type_, TypeFlags::Literal)
            && self.is_literal_of_contextual_type(
                &type_,
                self.instantiate_contextual_type(Some(contextual_type), node, None),
            ) {
            self.get_regular_type_of_literal_type(&type_)
        } else {
            type_
        };
        // }
        // finally {
        *context.maybe_contextual_type() = save_contextual_type;
        *context.maybe_inference_context() = save_inference_context;
        // }
        result
    }

    pub(super) fn check_expression_cached(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let links = self.get_node_links(node);
        let links_resolved_type_is_none = (*links).borrow().resolved_type.is_none();
        if links_resolved_type_is_none {
            if let Some(check_mode) = check_mode {
                if check_mode != CheckMode::Normal {
                    return self.check_expression(node, Some(check_mode), None);
                }
            }
            let save_flow_loop_start = self.flow_loop_start();
            let save_flow_type_cache = self.maybe_flow_type_cache().take();
            self.set_flow_loop_start(self.flow_loop_count());
            let resolved_type = self.check_expression(node, check_mode, None);
            links.borrow_mut().resolved_type = Some(resolved_type);
            *self.maybe_flow_type_cache() = save_flow_type_cache;
            self.set_flow_loop_start(save_flow_loop_start);
        }
        let resolved_type = (*links).borrow().resolved_type.clone().unwrap();
        resolved_type
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
                    unimplemented!()
                } else {
                    self.check_expression_cached(&initializer, None)
                }
            });
        if false {
            unimplemented!()
        } else {
            type_
        }
    }

    pub(super) fn widen_type_inferred_from_initializer(
        &self,
        declaration: &Node, /*HasExpressionInitializer*/
        type_: &Type,
    ) -> Rc<Type> {
        let widened = if get_combined_node_flags(declaration).intersects(NodeFlags::Const) {
            type_.type_wrapper()
        } else {
            self.get_widened_literal_type(type_)
        };
        widened
    }

    pub(super) fn is_literal_of_contextual_type<TTypeRef: Borrow<Type>>(
        &self,
        candidate_type: &Type,
        contextual_type: Option<TTypeRef>,
    ) -> bool {
        if let Some(contextual_type) = contextual_type {
            let contextual_type = contextual_type.borrow();
            if let Type::UnionOrIntersectionType(union_or_intersection_type) = contextual_type {
                let types = union_or_intersection_type.types();
                // return some(
                //     types,
                //     Some(Box::new(|t| {
                //         self.is_literal_of_contextual_type(candidate_type, Some(t.clone()))
                //     })),
                // );
                return types
                    .iter()
                    .any(|t| self.is_literal_of_contextual_type(candidate_type, Some(&**t)));
            }
            return contextual_type.flags().intersects(
                TypeFlags::StringLiteral
                    | TypeFlags::Index
                    | TypeFlags::TemplateLiteral
                    | TypeFlags::StringMapping,
            ) && self.maybe_type_of_kind(&*candidate_type, TypeFlags::StringLiteral)
                || contextual_type.flags().intersects(TypeFlags::NumberLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::NumberLiteral)
                || contextual_type.flags().intersects(TypeFlags::BigIntLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::BigIntLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::BooleanLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::BooleanLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::UniqueESSymbol)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::UniqueESSymbol);
        }
        false
    }

    pub(super) fn is_const_context(&self, node: &Node /*Expression*/) -> bool {
        unimplemented!()
    }

    pub(super) fn check_expression_for_mutable_location<TContextualType: Borrow<Type>>(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
        contextual_type: Option<TContextualType>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        let type_ = self.check_expression(node, check_mode, force_tuple);
        if false {
            unimplemented!()
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
        self.check_expression_for_mutable_location(
            &node.as_property_assignment().initializer,
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
        unimplemented!()
    }

    pub(super) fn skipped_generic_function(&self, node: &Node, check_mode: CheckMode) {
        unimplemented!()
    }

    pub(super) fn has_inference_candidates(&self, info: &InferenceInfo) -> bool {
        unimplemented!()
    }

    pub(super) fn get_type_of_expression(&self, node: &Node /*Expression*/) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_quick_type_of_expression(
        &self,
        node: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        let expr = node;
        if false {
            unimplemented!()
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
        unimplemented!()
    }

    pub(super) fn check_expression(
        &self,
        node: &Node, /*Expression | QualifiedName*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        self.check_expression_worker(node, check_mode)
    }

    pub(super) fn check_expression_worker(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        match node {
            Node::Identifier(_) => self.check_identifier(node, check_mode),
            Node::BaseNode(node_as_base_node) => match node_as_base_node.kind() {
                SyntaxKind::TrueKeyword => self.true_type(),
                _ => unimplemented!(),
            },
            Node::ObjectLiteralExpression(_) => self.check_object_literal(node, check_mode),
            Node::PrefixUnaryExpression(_) => self.check_prefix_unary_expression(node),
            // Node::Expression(Expression::BinaryExpression(_)) => {
            //     return self.check_binary_expression(node);
            // }
            Node::TemplateLiteralLikeNode(template_literal_like_node) => {
                let type_: Rc<Type> =
                    self.get_string_literal_type(&*template_literal_like_node.text());
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::StringLiteral(string_literal) => {
                let type_: Rc<Type> = self.get_string_literal_type(&*string_literal.text());
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::NumericLiteral(numeric_literal) => {
                self.check_grammar_numeric_literal(node);
                let type_: Rc<Type> =
                    self.get_number_literal_type(numeric_literal.text().as_str().into());
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::BigIntLiteral(big_int_literal) => {
                let type_: Rc<Type> = self
                    .get_big_int_literal_type(PseudoBigInt::new(
                        false,
                        parse_pseudo_big_int(&*big_int_literal.text()),
                    ))
                    .into();
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::TemplateExpression(_) => self.check_template_expression(node),
            _ => unimplemented!("{:#?}", node),
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
