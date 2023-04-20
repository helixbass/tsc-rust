#![allow(non_upper_case_globals)]

use gc::Gc;
use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, IterationTypeKind, IterationUse};
use crate::{
    are_option_gcs_equal, are_option_rcs_equal, expression_result_is_unused,
    get_assigned_expando_initializer, get_containing_function, get_function_flags,
    is_assignment_operator, is_element_access_expression, is_identifier, is_jsdoc_typedef_tag,
    is_jsx_self_closing_element, is_object_literal_expression, is_parenthesized_expression,
    is_property_access_expression, token_to_string, unescape_leading_underscores,
    AssignmentDeclarationKind, Diagnostic, DiagnosticMessage, Diagnostics, ExternalEmitHelpers,
    FunctionFlags, InferenceContext, Node, NodeFlags, NodeInterface, ScriptTarget, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    UnionReduction,
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
                        Some(name),
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
        node.kind() == SyntaxKind::Identifier && node.as_identifier().escaped_text == "eval"
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
            would_work_with_await = !(are_option_gcs_equal(
                awaited_left_type.as_ref(),
                Some(&left_type.type_wrapper()),
            ) && are_option_gcs_equal(
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
    ) -> Option<Gc<Diagnostic>> {
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
    ) -> (Gc<Type>, Gc<Type>) {
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

    pub(super) fn check_yield_expression(&self, node: &Node /*YieldExpression*/) -> Gc<Type> {
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
    ) -> Gc<Type> {
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
            &[type1, type2],
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
    ) -> Gc<Type> {
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

    pub(super) fn get_context_node(&self, node: &Node /*Expression*/) -> Gc<Node> {
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
        inference_context: Option<Gc<InferenceContext>>,
        check_mode: CheckMode,
    ) -> Gc<Type> {
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
    ) -> Gc<Type> {
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
}
