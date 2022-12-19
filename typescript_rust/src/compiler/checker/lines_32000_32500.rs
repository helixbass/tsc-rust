#![allow(non_upper_case_globals)]

use gc::Gc;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, TypeFacts};
use crate::{
    every, get_containing_class, get_object_flags, token_to_string, ExternalEmitHelpers, Number,
    __String, add_related_info, are_option_rcs_equal, create_diagnostic_for_node,
    create_file_diagnostic, first_or_undefined, get_check_flags, get_containing_function,
    get_containing_function_or_class_static_block, get_declaration_modifier_flags_from_symbol,
    get_effective_return_type_node, get_function_flags, get_source_file_of_node,
    get_span_of_token_at_position, has_context_sensitive_parameters, is_access_expression,
    is_binary_expression, is_bindable_object_define_property_call, is_call_expression,
    is_class_static_block_declaration, is_effective_external_module, is_function_expression,
    is_in_top_level_context, is_object_literal_method, is_private_identifier,
    is_property_access_expression, is_property_assignment, parse_pseudo_big_int,
    skip_outer_expressions, skip_parentheses, some, AssignmentKind, CheckFlags, Debug_, Diagnostic,
    DiagnosticMessage, DiagnosticRelatedInformation, Diagnostics, FunctionFlags,
    LiteralLikeNodeInterface, ModifierFlags, ModuleKind, Node, NodeCheckFlags, NodeFlags,
    NodeInterface, ObjectFlags, OuterExpressionKinds, PseudoBigInt, ReadonlyTextRange,
    ScriptTarget, SignatureFlags, SignatureKind, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    TextSpan, Type, TypeChecker, TypeFlags, TypeInterface, UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub(super) fn check_function_expression_or_object_literal_method(
        &self,
        node: &Node, /*FunctionExpression | ArrowFunction | MethodDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> Gc<Type> {
        Debug_.assert(
            node.kind() != SyntaxKind::MethodDeclaration || is_object_literal_method(node),
            None,
        );
        self.check_node_deferred(node);

        if is_function_expression(node) {
            self.check_collisions_for_declaration_name(
                node,
                node.as_named_declaration().maybe_name(),
            );
        }

        if matches!(
            check_mode,
            Some(check_mode) if check_mode.intersects(CheckMode::SkipContextSensitive)
        ) && self.is_context_sensitive(node)
        {
            if get_effective_return_type_node(node).is_none()
                && !has_context_sensitive_parameters(node)
            {
                let contextual_signature = self.get_contextual_signature(node);
                if matches!(
                    contextual_signature.as_ref(),
                    Some(contextual_signature) if self.could_contain_type_variables(&self.get_return_type_of_signature(contextual_signature.clone()))
                ) {
                    let links = self.get_node_links(node);
                    if let Some(links_context_free_type) =
                        (*links).borrow().context_free_type.clone()
                    {
                        return links_context_free_type;
                    }
                    let return_type = self.get_return_type_from_body(node, check_mode);
                    let return_only_signature = Rc::new(self.create_signature(
                        None,
                        None,
                        None,
                        vec![],
                        Some(return_type),
                        None,
                        0,
                        SignatureFlags::None,
                    ));
                    let return_only_type = self.create_anonymous_type(
                        node.maybe_symbol(),
                        self.empty_symbols(),
                        vec![return_only_signature],
                        vec![],
                        vec![],
                    );
                    let return_only_type_as_object_flags_type =
                        return_only_type.as_object_flags_type();
                    return_only_type_as_object_flags_type.set_object_flags(
                        return_only_type_as_object_flags_type.object_flags()
                            | ObjectFlags::NonInferrableType,
                    );
                    links.borrow_mut().context_free_type = Some(return_only_type.clone());
                    return return_only_type;
                }
            }
            return self.any_function_type();
        }

        let has_grammar_error = self.check_grammar_function_like_declaration(node);
        if !has_grammar_error && node.kind() == SyntaxKind::FunctionExpression {
            self.check_grammar_for_generator(node);
        }

        self.contextually_check_function_expression_or_object_literal_method(node, check_mode);

        self.get_type_of_symbol(&self.get_symbol_of_node(node).unwrap())
    }

    pub(super) fn contextually_check_function_expression_or_object_literal_method(
        &self,
        node: &Node, /*FunctionExpression | ArrowFunction | MethodDeclaration*/
        check_mode: Option<CheckMode>,
    ) {
        let links = self.get_node_links(node);
        if !(*links)
            .borrow()
            .flags
            .intersects(NodeCheckFlags::ContextChecked)
        {
            let contextual_signature = self.get_contextual_signature(node);
            if !(*links)
                .borrow()
                .flags
                .intersects(NodeCheckFlags::ContextChecked)
            {
                links.borrow_mut().flags |= NodeCheckFlags::ContextChecked;
                let signatures_of_type = self.get_signatures_of_type(
                    &self.get_type_of_symbol(&self.get_symbol_of_node(node).unwrap()),
                    SignatureKind::Call,
                );
                let signature = first_or_undefined(&signatures_of_type);
                if signature.is_none() {
                    return;
                }
                let signature = signature.unwrap();
                if self.is_context_sensitive(node) {
                    if let Some(contextual_signature) = contextual_signature.as_ref() {
                        let inference_context = self.get_inference_context(node);
                        if matches!(
                            check_mode,
                            Some(check_mode) if check_mode.intersects(CheckMode::Inferential)
                        ) {
                            self.infer_from_annotated_parameters(
                                signature,
                                contextual_signature.clone(),
                                inference_context.as_ref().unwrap(),
                            );
                        }
                        let instantiated_contextual_signature =
                            if let Some(inference_context) = inference_context.as_ref() {
                                Rc::new(self.instantiate_signature(
                                    contextual_signature.clone(),
                                    inference_context.mapper(),
                                    None,
                                ))
                            } else {
                                contextual_signature.clone()
                            };
                        self.assign_contextual_parameter_types(
                            signature,
                            &instantiated_contextual_signature,
                        );
                    } else {
                        self.assign_non_contextual_parameter_types(signature);
                    }
                }
                if contextual_signature.is_some()
                    && self.get_return_type_from_annotation(node).is_none()
                    && signature.maybe_resolved_return_type().is_none()
                {
                    let return_type = self.get_return_type_from_body(node, check_mode);
                    if signature.maybe_resolved_return_type().is_none() {
                        *signature.maybe_resolved_return_type_mut() = Some(return_type);
                    }
                }
                self.check_signature_declaration(node);
            }
        }
    }

    pub(super) fn check_function_expression_or_object_literal_method_deferred(
        &self,
        node: &Node, /*ArrowFunction | FunctionExpression | MethodDeclaration*/
    ) {
        Debug_.assert(
            node.kind() != SyntaxKind::MethodDeclaration || is_object_literal_method(node),
            None,
        );

        let function_flags = get_function_flags(Some(node));
        let return_type = self.get_return_type_from_annotation(node);
        self.check_all_code_paths_in_non_void_function_return_or_throw(
            node,
            return_type.as_deref(),
        );

        if let Some(node_body) = node.as_function_like_declaration().maybe_body().as_ref() {
            if get_effective_return_type_node(node).is_none() {
                self.get_return_type_of_signature(self.get_signature_from_declaration_(node));
            }

            if node_body.kind() == SyntaxKind::Block {
                self.check_source_element(Some(&**node_body));
            } else {
                let expr_type = self.check_expression(node_body, None, None);
                let return_or_promised_type = return_type
                    .as_ref()
                    .map(|return_type| self.unwrap_return_type(return_type, function_flags));
                if let Some(return_or_promised_type) = return_or_promised_type.as_ref() {
                    if function_flags & FunctionFlags::AsyncGenerator == FunctionFlags::Async {
                        let awaited_type = self.check_awaited_type(
                            &expr_type,
                            false,
                            node_body,
                            &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member,
                            None,
                        );
                        self.check_type_assignable_to_and_optionally_elaborate(
                            &awaited_type,
                            return_or_promised_type,
                            Some(&**node_body),
                            Some(&**node_body),
                            None,
                            None,
                        );
                    } else {
                        self.check_type_assignable_to_and_optionally_elaborate(
                            &expr_type,
                            return_or_promised_type,
                            Some(&**node_body),
                            Some(&**node_body),
                            None,
                            None,
                        );
                    }
                }
            }
        }
    }

    pub(super) fn check_arithmetic_operand_type(
        &self,
        operand: &Node, /*Expression*/
        type_: &Type,
        diagnostic: &DiagnosticMessage,
        is_await_valid: Option<bool>,
    ) -> bool {
        let is_await_valid = is_await_valid.unwrap_or(false);
        if !self.is_type_assignable_to(type_, &self.number_or_big_int_type()) {
            let awaited_type = if is_await_valid {
                self.get_awaited_type_of_promise(type_, Option::<&Node>::None, None, None)
            } else {
                None
            };
            self.error_and_maybe_suggest_await(
                operand,
                matches!(
                    awaited_type.as_ref(),
                    Some(awaited_type) if self.is_type_assignable_to(awaited_type, &self.number_or_big_int_type())
                ),
                diagnostic,
                None,
            );
            return false;
        }
        true
    }

    pub(super) fn is_readonly_assignment_declaration(&self, d: &Node /*Declaration*/) -> bool {
        if !is_call_expression(d) {
            return false;
        }
        if !is_bindable_object_define_property_call(d) {
            return false;
        }
        let d_as_call_expression = d.as_call_expression();
        let object_lit_type =
            self.check_expression_cached(&d_as_call_expression.arguments[2], None);
        let value_type = self.get_type_of_property_of_type_(&object_lit_type, "value");
        if let Some(value_type) = value_type.as_ref() {
            let writable_prop = self.get_property_of_type_(&object_lit_type, "writable", None);
            let writable_type = writable_prop
                .as_ref()
                .map(|writable_prop| self.get_type_of_symbol(writable_prop));
            if match writable_type.as_ref() {
                None => true,
                Some(writable_type) => {
                    Rc::ptr_eq(writable_type, &self.false_type())
                        || Rc::ptr_eq(writable_type, &self.regular_false_type())
                }
            } {
                return true;
            }
            if let Some(ref writable_prop_value_declaration) = writable_prop
                .as_ref()
                .and_then(|writable_prop| writable_prop.maybe_value_declaration())
                .filter(|writable_prop_value_declaration| {
                    is_property_assignment(writable_prop_value_declaration)
                })
            {
                let initializer = &writable_prop_value_declaration
                    .as_property_assignment()
                    .initializer;
                let raw_original_type = self.check_expression(initializer, None, None);
                if Rc::ptr_eq(&raw_original_type, &self.false_type())
                    || Rc::ptr_eq(&raw_original_type, &self.regular_false_type())
                {
                    return true;
                }
            }
            return false;
        }
        let set_prop = self.get_property_of_type_(&object_lit_type, "set", None);
        set_prop.is_none()
    }

    pub(super) fn is_readonly_symbol(&self, symbol: &Symbol) -> bool {
        get_check_flags(symbol).intersects(CheckFlags::Readonly)
            || symbol.flags().intersects(SymbolFlags::Property)
                && get_declaration_modifier_flags_from_symbol(symbol, None)
                    .intersects(ModifierFlags::Readonly)
            || symbol.flags().intersects(SymbolFlags::Variable)
                && self
                    .get_declaration_node_flags_from_symbol(symbol)
                    .intersects(NodeFlags::Const)
            || symbol.flags().intersects(SymbolFlags::Accessor)
                && !symbol.flags().intersects(SymbolFlags::SetAccessor)
            || symbol.flags().intersects(SymbolFlags::EnumMember)
            || some(
                symbol.maybe_declarations().as_deref(),
                Some(|declaration: &Gc<Node>| self.is_readonly_assignment_declaration(declaration)),
            )
    }

    pub(super) fn is_assignment_to_readonly_entity(
        &self,
        expr: &Node, /*Expression*/
        symbol: &Symbol,
        assignment_kind: AssignmentKind,
    ) -> bool {
        if assignment_kind == AssignmentKind::None {
            return false;
        }
        if self.is_readonly_symbol(symbol) {
            if symbol.flags().intersects(SymbolFlags::Property)
                && is_access_expression(expr)
                && expr.as_has_expression().expression().kind() == SyntaxKind::ThisKeyword
            {
                let ctor = get_containing_function(expr);
                if !matches!(
                    ctor.as_ref(),
                    Some(ctor) if ctor.kind() == SyntaxKind::Constructor || self.is_js_constructor(Some(&**ctor))
                ) {
                    return true;
                }
                let ctor = ctor.unwrap();
                if let Some(symbol_value_declaration) = symbol.maybe_value_declaration().as_ref() {
                    let is_assignment_declaration = is_binary_expression(symbol_value_declaration);
                    let is_local_property_declaration = are_option_rcs_equal(
                        ctor.maybe_parent().as_ref(),
                        symbol_value_declaration.maybe_parent().as_ref(),
                    );
                    let is_local_parameter_property = matches!(
                        symbol_value_declaration.maybe_parent().as_ref(),
                        Some(symbol_value_declaration_parent) if Rc::ptr_eq(
                            &ctor,
                            symbol_value_declaration_parent
                        )
                    );
                    let is_local_this_property_assignment = is_assignment_declaration
                        && are_option_rcs_equal(
                            symbol
                                .maybe_parent()
                                .and_then(|symbol_parent| symbol_parent.maybe_value_declaration())
                                .as_ref(),
                            ctor.maybe_parent().as_ref(),
                        );
                    let is_local_this_property_assignment_constructor_function =
                        is_assignment_declaration
                            && matches!(
                                symbol.maybe_parent().and_then(|symbol_parent| symbol_parent.maybe_value_declaration()).as_ref(),
                                Some(symbol_parent_value_declaration) if Rc::ptr_eq(
                                    symbol_parent_value_declaration,
                                    &ctor,
                                )
                            );
                    let is_writeable_symbol = is_local_property_declaration
                        || is_local_parameter_property
                        || is_local_this_property_assignment
                        || is_local_this_property_assignment_constructor_function;
                    return !is_writeable_symbol;
                }
            }
            return true;
        }
        if is_access_expression(expr) {
            let node = skip_parentheses(&expr.as_has_expression().expression(), None);
            if node.kind() == SyntaxKind::Identifier {
                let symbol = (*self.get_node_links(&node))
                    .borrow()
                    .resolved_symbol
                    .clone()
                    .unwrap();
                if symbol.flags().intersects(SymbolFlags::Alias) {
                    let declaration = self.get_declaration_of_alias_symbol(&symbol);
                    return matches!(
                        declaration.as_ref(),
                        Some(declaration) if declaration.kind() == SyntaxKind::NamespaceImport
                    );
                }
            }
        }
        false
    }

    pub(super) fn check_reference_expression(
        &self,
        expr: &Node, /*Expression*/
        invalid_reference_message: &'static DiagnosticMessage,
        invalid_optional_chain_message: &'static DiagnosticMessage,
    ) -> bool {
        let node = skip_outer_expressions(
            expr,
            Some(OuterExpressionKinds::Assertions | OuterExpressionKinds::Parentheses),
        );
        if node.kind() != SyntaxKind::Identifier && !is_access_expression(&node) {
            self.error(Some(expr), invalid_reference_message, None);
            return false;
        }
        if node.flags().intersects(NodeFlags::OptionalChain) {
            self.error(Some(expr), invalid_optional_chain_message, None);
            return false;
        }
        true
    }

    pub(super) fn check_delete_expression(
        &self,
        node: &Node, /*DeleteExpression*/
    ) -> Gc<Type> {
        let node_as_delete_expression = node.as_delete_expression();
        self.check_expression(&node_as_delete_expression.expression, None, None);
        let expr = skip_parentheses(&node_as_delete_expression.expression, None);
        if !is_access_expression(&expr) {
            self.error(
                Some(&*expr),
                &Diagnostics::The_operand_of_a_delete_operator_must_be_a_property_reference,
                None,
            );
            return self.boolean_type();
        }
        if is_property_access_expression(&expr)
            && is_private_identifier(&expr.as_property_access_expression().name)
        {
            self.error(
                Some(&*expr),
                &Diagnostics::The_operand_of_a_delete_operator_cannot_be_a_private_identifier,
                None,
            );
        }
        let links = self.get_node_links(&expr);
        let resolved_symbol = (*links).borrow().resolved_symbol.clone();
        let symbol = self.get_export_symbol_of_value_symbol_if_exported(resolved_symbol);
        if let Some(symbol) = symbol.as_ref() {
            if self.is_readonly_symbol(symbol) {
                self.error(
                    Some(&*expr),
                    &Diagnostics::The_operand_of_a_delete_operator_cannot_be_a_read_only_property,
                    None,
                );
            }
            self.check_delete_expression_must_be_optional(&expr, symbol);
        }
        self.boolean_type()
    }

    pub(super) fn check_delete_expression_must_be_optional(
        &self,
        expr: &Node, /*AccessExpression*/
        symbol: &Symbol,
    ) {
        let type_ = self.get_type_of_symbol(symbol);
        if self.strict_null_checks
            && !type_.flags().intersects(TypeFlags::Any | TypeFlags::Never)
            && !if self.exact_optional_property_types == Some(true) {
                symbol.flags().intersects(SymbolFlags::Optional)
            } else {
                self.get_falsy_flags(&type_)
                    .intersects(TypeFlags::Undefined)
            }
        {
            self.error(
                Some(expr),
                &Diagnostics::The_operand_of_a_delete_operator_must_be_optional,
                None,
            );
        }
    }

    pub(super) fn check_type_of_expression(
        &self,
        node: &Node, /*TypeOfExpression*/
    ) -> Gc<Type> {
        self.check_expression(&node.as_type_of_expression().expression, None, None);
        self.typeof_type()
    }

    pub(super) fn check_void_expression(&self, node: &Node /*VoidExpression*/) -> Gc<Type> {
        self.check_expression(&node.as_void_expression().expression, None, None);
        self.undefined_widening_type()
    }

    pub(super) fn check_await_expression(&self, node: &Node /*AwaitExpression*/) -> Gc<Type> {
        if self.produce_diagnostics {
            let container = get_containing_function_or_class_static_block(node);
            if matches!(
                container.as_ref(),
                Some(container) if is_class_static_block_declaration(container)
            ) {
                self.error(
                    Some(node),
                    &Diagnostics::Await_expression_cannot_be_used_inside_a_class_static_block,
                    None,
                );
            } else if !node.flags().intersects(NodeFlags::AwaitContext) {
                if is_in_top_level_context(node) {
                    let source_file = get_source_file_of_node(Some(node)).unwrap();
                    if !self.has_parse_diagnostics(&source_file) {
                        let mut span: Option<TextSpan> = None;
                        if !is_effective_external_module(&source_file, &self.compiler_options) {
                            if span.is_none() {
                                span = Some(get_span_of_token_at_position(
                                    &source_file,
                                    node.pos().try_into().unwrap(),
                                ));
                            }
                            let diagnostic: Gc<Diagnostic> = Rc::new(
                                create_file_diagnostic(
                                    &source_file,
                                    span.unwrap().start,
                                    span.unwrap().length,
                                    &Diagnostics::await_expressions_are_only_allowed_at_the_top_level_of_a_file_when_that_file_is_a_module_but_this_file_has_no_imports_or_exports_Consider_adding_an_empty_export_to_make_this_file_a_module,
                                    None,
                                ).into()
                            );
                            self.diagnostics().add(diagnostic);
                        }
                        if !matches!(
                            self.module_kind,
                            ModuleKind::ES2022 | ModuleKind::ESNext | ModuleKind::System
                        ) && !(self.module_kind == ModuleKind::NodeNext
                            && get_source_file_of_node(Some(node))
                                .unwrap()
                                .as_source_file()
                                .maybe_implied_node_format()
                                == Some(ModuleKind::ESNext))
                            || self.language_version < ScriptTarget::ES2017
                        {
                            span = Some(get_span_of_token_at_position(
                                &source_file,
                                node.pos().try_into().unwrap(),
                            ));
                            let diagnostic: Gc<Diagnostic> = Rc::new(
                                create_file_diagnostic(
                                    &source_file,
                                    span.unwrap().start,
                                    span.unwrap().length,
                                    &Diagnostics::Top_level_await_expressions_are_only_allowed_when_the_module_option_is_set_to_es2022_esnext_system_or_nodenext_and_the_target_option_is_set_to_es2017_or_higher,
                                    None,
                                ).into()
                            );
                            self.diagnostics().add(diagnostic);
                        }
                    }
                } else {
                    let source_file = get_source_file_of_node(Some(node)).unwrap();
                    if !self.has_parse_diagnostics(&source_file) {
                        let span = get_span_of_token_at_position(
                            &source_file,
                            node.pos().try_into().unwrap(),
                        );
                        let diagnostic: Gc<Diagnostic> = Rc::new(
                            create_file_diagnostic(
                                &source_file,
                                span.start,
                                span.length,
                                &Diagnostics::await_expressions_are_only_allowed_within_async_functions_and_at_the_top_levels_of_modules,
                                None,
                            ).into()
                        );
                        if let Some(container) = container.as_ref().filter(|container| {
                            container.kind() != SyntaxKind::Constructor
                                && !get_function_flags(Some(&***container))
                                    .intersects(FunctionFlags::Async)
                        }) {
                            let related_info: Rc<DiagnosticRelatedInformation> = Rc::new(
                                create_diagnostic_for_node(
                                    container,
                                    &Diagnostics::Did_you_mean_to_mark_this_function_as_async,
                                    None,
                                )
                                .into(),
                            );
                            add_related_info(&diagnostic, vec![related_info]);
                        }
                        self.diagnostics().add(diagnostic);
                    }
                }
            }

            if self.is_in_parameter_initializer_before_containing_function(node) {
                self.error(
                    Some(node),
                    &Diagnostics::await_expressions_cannot_be_used_in_a_parameter_initializer,
                    None,
                );
            }
        }

        let operand_type =
            self.check_expression(&node.as_await_expression().expression, None, None);
        let awaited_type = self.check_awaited_type(
            &operand_type,
            true,
            node,
            &Diagnostics::Type_of_await_operand_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member,
            None,
        );
        if Rc::ptr_eq(&awaited_type, &operand_type)
            && !self.is_error_type(&awaited_type)
            && !operand_type.flags().intersects(TypeFlags::AnyOrUnknown)
        {
            self.add_error_or_suggestion(
                false,
                Rc::new(
                    create_diagnostic_for_node(
                        node,
                        &Diagnostics::await_has_no_effect_on_the_type_of_this_expression,
                        None,
                    )
                    .into(),
                ),
            );
        }
        awaited_type
    }

    pub(super) fn check_prefix_unary_expression(
        &self,
        node: &Node, /*PrefixUnaryExpression*/
    ) -> Gc<Type> {
        let node_as_prefix_unary_expression = node.as_prefix_unary_expression();
        let operand_type =
            self.check_expression(&node_as_prefix_unary_expression.operand, None, None);
        if Rc::ptr_eq(&operand_type, &self.silent_never_type()) {
            return self.silent_never_type();
        }
        match node_as_prefix_unary_expression.operand.kind() {
            SyntaxKind::NumericLiteral => match node_as_prefix_unary_expression.operator {
                SyntaxKind::MinusToken => {
                    return self.get_fresh_type_of_literal_type(
                        &self.get_number_literal_type(Number::new(
                            node_as_prefix_unary_expression
                                .operand
                                .as_numeric_literal()
                                .text()
                                .parse::<f64>()
                                .unwrap()
                                * -1.0,
                        )),
                    );
                }
                SyntaxKind::PlusToken => {
                    return self.get_fresh_type_of_literal_type(
                        &self.get_number_literal_type(Number::new(
                            node_as_prefix_unary_expression
                                .operand
                                .as_numeric_literal()
                                .text()
                                .parse::<f64>()
                                .unwrap(),
                        )),
                    );
                }
                _ => (),
            },
            SyntaxKind::BigIntLiteral => {
                if node_as_prefix_unary_expression.operator == SyntaxKind::MinusToken {
                    return self.get_fresh_type_of_literal_type(
                        &self.get_big_int_literal_type(PseudoBigInt::new(
                            true,
                            parse_pseudo_big_int(
                                &node_as_prefix_unary_expression
                                    .operand
                                    .as_big_int_literal()
                                    .text(),
                            ),
                        )),
                    );
                }
            }
            _ => (),
        }
        match node_as_prefix_unary_expression.operator {
            SyntaxKind::PlusToken | SyntaxKind::MinusToken | SyntaxKind::TildeToken => {
                self.check_non_null_type(&operand_type, &node_as_prefix_unary_expression.operand);
                if self.maybe_type_of_kind(&operand_type, TypeFlags::ESSymbolLike) {
                    self.error(
                        Some(&*node_as_prefix_unary_expression.operand),
                        &Diagnostics::The_0_operator_cannot_be_applied_to_type_symbol,
                        Some(vec![token_to_string(
                            node_as_prefix_unary_expression.operator,
                        )
                        .unwrap()
                        .to_owned()]),
                    );
                }
                if node_as_prefix_unary_expression.operator == SyntaxKind::PlusToken {
                    if self.maybe_type_of_kind(&operand_type, TypeFlags::BigIntLike) {
                        self.error(
                            Some(&*node_as_prefix_unary_expression.operand),
                            &Diagnostics::Operator_0_cannot_be_applied_to_type_1,
                            Some(vec![
                                token_to_string(node_as_prefix_unary_expression.operator)
                                    .unwrap()
                                    .to_owned(),
                                self.type_to_string_(
                                    &self.get_base_type_of_literal_type(&operand_type),
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                ),
                            ]),
                        );
                    }
                    return self.number_type();
                }
                self.get_unary_result_type(&operand_type)
            }
            SyntaxKind::ExclamationToken => {
                self.check_truthiness_expression(&node_as_prefix_unary_expression.operand, None);
                let facts = self.get_type_facts(&operand_type, None)
                    & (TypeFacts::Truthy | TypeFacts::Falsy);
                match facts {
                    TypeFacts::Truthy => self.false_type(),
                    TypeFacts::Falsy => self.true_type(),
                    _ => self.boolean_type(),
                }
            }
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken => {
                let ok = self.check_arithmetic_operand_type(&node_as_prefix_unary_expression.operand, &self.check_non_null_type(&operand_type, &node_as_prefix_unary_expression.operand), &Diagnostics::An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type, None);
                if ok {
                    self.check_reference_expression(
                        &node_as_prefix_unary_expression.operand,
                        &Diagnostics::The_operand_of_an_increment_or_decrement_operator_must_be_a_variable_or_a_property_access,
                        &Diagnostics::The_operand_of_an_increment_or_decrement_operator_may_not_be_an_optional_property_access,
                    );
                }
                self.get_unary_result_type(&operand_type)
            }
            _ => self.error_type(),
        }
    }

    pub(super) fn check_postfix_unary_expression(
        &self,
        node: &Node, /*PostfixUnaryExpression*/
    ) -> Gc<Type> {
        let node_as_postfix_unary_expression = node.as_postfix_unary_expression();
        let operand_type =
            self.check_expression(&node_as_postfix_unary_expression.operand, None, None);
        if Rc::ptr_eq(&operand_type, &self.silent_never_type()) {
            return self.silent_never_type();
        }
        let ok = self.check_arithmetic_operand_type(
            &node_as_postfix_unary_expression.operand,
            &self.check_non_null_type(&operand_type, &node_as_postfix_unary_expression.operand),
            &Diagnostics::An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type,
            None,
        );
        if ok {
            self.check_reference_expression(
                &node_as_postfix_unary_expression.operand,
                &Diagnostics::The_operand_of_an_increment_or_decrement_operator_must_be_a_variable_or_a_property_access,
                &Diagnostics::The_operand_of_an_increment_or_decrement_operator_may_not_be_an_optional_property_access,
            );
        }
        self.get_unary_result_type(&operand_type)
    }

    pub(super) fn get_unary_result_type(&self, operand_type: &Type) -> Gc<Type> {
        if self.maybe_type_of_kind(operand_type, TypeFlags::BigIntLike) {
            return if self.is_type_assignable_to_kind(operand_type, TypeFlags::AnyOrUnknown, None)
                || self.maybe_type_of_kind(operand_type, TypeFlags::NumberLike)
            {
                self.number_or_big_int_type()
            } else {
                self.bigint_type()
            };
        }
        self.number_type()
    }

    pub(super) fn maybe_type_of_kind(&self, type_: &Type, kind: TypeFlags) -> bool {
        if type_.flags().intersects(kind) {
            return true;
        }
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            let types = type_.as_union_or_intersection_type_interface().types();
            for t in types {
                if self.maybe_type_of_kind(t, kind) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn is_type_assignable_to_kind(
        &self,
        source: &Type,
        kind: TypeFlags,
        strict: Option<bool>,
    ) -> bool {
        if source.flags().intersects(kind) {
            return true;
        }
        if strict == Some(true)
            && source.flags().intersects(
                TypeFlags::AnyOrUnknown | TypeFlags::Void | TypeFlags::Undefined | TypeFlags::Null,
            )
        {
            return false;
        }
        kind.intersects(TypeFlags::NumberLike)
            && self.is_type_assignable_to(source, &self.number_type())
            || kind.intersects(TypeFlags::BigIntLike)
                && self.is_type_assignable_to(source, &self.bigint_type())
            || kind.intersects(TypeFlags::StringLike)
                && self.is_type_assignable_to(source, &self.string_type())
            || kind.intersects(TypeFlags::BooleanLike)
                && self.is_type_assignable_to(source, &self.boolean_type())
            || kind.intersects(TypeFlags::Void)
                && self.is_type_assignable_to(source, &self.void_type())
            || kind.intersects(TypeFlags::Never)
                && self.is_type_assignable_to(source, &self.never_type())
            || kind.intersects(TypeFlags::Null)
                && self.is_type_assignable_to(source, &self.null_type())
            || kind.intersects(TypeFlags::Undefined)
                && self.is_type_assignable_to(source, &self.undefined_type())
            || kind.intersects(TypeFlags::ESSymbol)
                && self.is_type_assignable_to(source, &self.es_symbol_type())
            || kind.intersects(TypeFlags::NonPrimitive)
                && self.is_type_assignable_to(source, &self.non_primitive_type())
    }

    pub(super) fn all_types_assignable_to_kind(
        &self,
        source: &Type,
        kind: TypeFlags,
        strict: Option<bool>,
    ) -> bool {
        if source.flags().intersects(TypeFlags::Union) {
            every(source.as_union_type().types(), |sub_type: &Gc<Type>, _| {
                self.all_types_assignable_to_kind(sub_type, kind, strict)
            })
        } else {
            self.is_type_assignable_to_kind(source, kind, strict)
        }
    }

    pub(super) fn is_const_enum_object_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::Anonymous)
            && matches!(
                type_.maybe_symbol().as_ref(),
                Some(type_symbol) if self.is_const_enum_symbol(type_symbol)
            )
    }

    pub(super) fn is_const_enum_symbol(&self, symbol: &Symbol) -> bool {
        symbol.flags().intersects(SymbolFlags::ConstEnum)
    }

    pub(super) fn check_instance_of_expression(
        &self,
        left: &Node,  /*Expression*/
        right: &Node, /*Expression*/
        left_type: &Type,
        right_type: &Type,
    ) -> Gc<Type> {
        if ptr::eq(left_type, &*self.silent_never_type())
            || ptr::eq(right_type, &*self.silent_never_type())
        {
            return self.silent_never_type();
        }
        if !self.is_type_any(Some(left_type))
            && self.all_types_assignable_to_kind(left_type, TypeFlags::Primitive, None)
        {
            self.error(
                Some(left),
                &Diagnostics::The_left_hand_side_of_an_instanceof_expression_must_be_of_type_any_an_object_type_or_a_type_parameter,
                None,
            );
        }
        if !(self.is_type_any(Some(right_type))
            || self.type_has_call_or_construct_signatures(right_type)
            || self.is_type_subtype_of(right_type, &self.global_function_type()))
        {
            self.error(
                Some(right),
                &Diagnostics::The_right_hand_side_of_an_instanceof_expression_must_be_of_type_any_or_of_a_type_assignable_to_the_Function_interface_type,
                None,
            );
        }
        self.boolean_type()
    }

    pub(super) fn check_in_expression(
        &self,
        left: &Node,  /*Expression*/
        right: &Node, /*Expression*/
        left_type: &Type,
        right_type: &Type,
    ) -> Gc<Type> {
        if ptr::eq(left_type, &*self.silent_never_type())
            || ptr::eq(right_type, &*self.silent_never_type())
        {
            return self.silent_never_type();
        }
        let mut left_type = left_type.type_wrapper();
        if is_private_identifier(left) {
            if self.language_version < ScriptTarget::ESNext {
                self.check_external_emit_helpers(left, ExternalEmitHelpers::ClassPrivateFieldIn);
            }
            if (*self.get_node_links(left))
                .borrow()
                .resolved_symbol
                .is_some()
                && get_containing_class(left).is_some()
            {
                let is_unchecked_js =
                    self.is_unchecked_js_suggestion(Some(left), right_type.maybe_symbol(), true);
                self.report_nonexistent_property(left, right_type, is_unchecked_js);
            }
        } else {
            left_type = self.check_non_null_type(&left_type, left);
            if !(self.all_types_assignable_to_kind(
                &left_type,
                TypeFlags::StringLike | TypeFlags::NumberLike | TypeFlags::ESSymbolLike,
                None,
            ) || self.is_type_assignable_to_kind(
                &left_type,
                TypeFlags::Index
                    | TypeFlags::TemplateLiteral
                    | TypeFlags::StringMapping
                    | TypeFlags::TypeParameter,
                None,
            )) {
                self.error(
                    Some(left),
                    &Diagnostics::The_left_hand_side_of_an_in_expression_must_be_a_private_identifier_or_of_type_any_string_number_or_symbol,
                    None
                );
            }
        }
        let right_type = self.check_non_null_type(right_type, right);
        let right_type_constraint = self.get_constraint_of_type(&right_type);
        if !self.all_types_assignable_to_kind(
            &right_type,
            TypeFlags::NonPrimitive | TypeFlags::InstantiableNonPrimitive,
            None,
        ) || matches!(
            right_type_constraint.as_ref(),
            Some(right_type_constraint) if self.is_type_assignable_to_kind(
                &right_type,
                TypeFlags::UnionOrIntersection,
                None,
            ) &&
                !self.all_types_assignable_to_kind(
                    right_type_constraint,
                    TypeFlags::NonPrimitive | TypeFlags::InstantiableNonPrimitive,
                    None,
                ) ||
                !self.maybe_type_of_kind(right_type_constraint, TypeFlags::NonPrimitive | TypeFlags::InstantiableNonPrimitive | TypeFlags::Object)
        ) {
            self.error(
                Some(right),
                &Diagnostics::The_right_hand_side_of_an_in_expression_must_not_be_a_primitive,
                None,
            );
        }
        self.boolean_type()
    }
}
