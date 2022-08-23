#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, GetDiagnosticSpanForCallNodeReturn,
    InvocationErrorDetails, JsxNames, MinArgumentCountFlags, TypeFacts, WideningKind,
};
use crate::{
    add_related_info, chain_diagnostic_messages, create_diagnostic_for_node,
    create_diagnostic_for_node_array, create_diagnostic_for_node_from_message_chain, every,
    factory, get_source_file_of_node, get_text_of_node, is_array_literal_expression,
    is_call_expression, is_import_call, length, maybe_for_each, synthetic_factory, Debug_,
    Diagnostic, DiagnosticMessage, DiagnosticRelatedInformation,
    DiagnosticRelatedInformationInterface, Diagnostics, FunctionFlags, NodeArray, Signature,
    SignatureFlags, SignatureKind, SymbolFlags, TransientSymbolInterface, UnionReduction, __String,
    get_function_flags, has_initializer, Node, NodeInterface, Symbol, SymbolInterface, SyntaxKind,
    Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn invocation_error(
        &self,
        error_target: &Node,
        apparent_type: &Type,
        kind: SignatureKind,
        related_information: Option<Rc<DiagnosticRelatedInformation>>,
    ) {
        let InvocationErrorDetails {
            message_chain,
            related_message: related_info,
        } = self.invocation_error_details(error_target, apparent_type, kind);
        let diagnostic: Rc<Diagnostic> = Rc::new(
            create_diagnostic_for_node_from_message_chain(error_target, message_chain, None).into(),
        );
        if let Some(related_info) = related_info {
            add_related_info(
                &diagnostic,
                vec![Rc::new(
                    create_diagnostic_for_node(error_target, related_info, None).into(),
                )],
            );
        }
        if is_call_expression(&error_target.parent()) {
            let GetDiagnosticSpanForCallNodeReturn { start, length, .. } =
                self.get_diagnostic_span_for_call_node(&error_target.parent(), Some(true));
            diagnostic.set_start(Some(start));
            diagnostic.set_length(Some(length));
        }
        self.diagnostics().add(diagnostic.clone());
        self.invocation_error_recovery(
            apparent_type,
            kind,
            &*if let Some(related_information) = related_information {
                add_related_info(&diagnostic, vec![related_information]);
                diagnostic
            } else {
                diagnostic
            },
        )
    }

    pub(super) fn invocation_error_recovery(
        &self,
        apparent_type: &Type,
        kind: SignatureKind,
        diagnostic: &Diagnostic,
    ) {
        let apparent_type_symbol = apparent_type.maybe_symbol();
        if apparent_type_symbol.is_none() {
            return;
        }
        let apparent_type_symbol = apparent_type_symbol.unwrap();
        let import_node = (*self.get_symbol_links(&apparent_type_symbol))
            .borrow()
            .originating_import
            .clone();
        if let Some(import_node) = import_node
            .as_ref()
            .filter(|import_node| !is_import_call(import_node))
        {
            let sigs = self.get_signatures_of_type(
                &self.get_type_of_symbol(
                    &(*self.get_symbol_links(&apparent_type_symbol))
                        .borrow()
                        .target
                        .clone()
                        .unwrap(),
                ),
                kind,
            );
            if
            /* !sigs ||*/
            sigs.is_empty() {
                return;
            }

            add_related_info(
                diagnostic,
                vec![
                    Rc::new(
                        create_diagnostic_for_node(
                            import_node,
                            &Diagnostics::Type_originates_at_this_import_A_namespace_style_import_cannot_be_called_or_constructed_and_will_cause_a_failure_at_runtime_Consider_using_a_default_import_or_import_require_here_instead,
                            None,
                        ).into()
                    )
                ]
            );
        }
    }

    pub(super) fn resolve_tagged_template_expression(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
        candidates_out_array: Option<&mut Vec<Rc<Signature>>>,
        check_mode: CheckMode,
    ) -> Rc<Signature> {
        let node_as_tagged_template_expression = node.as_tagged_template_expression();
        let tag_type = self.check_expression(&node_as_tagged_template_expression.tag, None, None);
        let apparent_type = self.get_apparent_type(&tag_type);

        if self.is_error_type(&apparent_type) {
            return self.resolve_error_call(node);
        }

        let call_signatures = self.get_signatures_of_type(&apparent_type, SignatureKind::Call);
        let num_construct_signatures = self
            .get_signatures_of_type(&apparent_type, SignatureKind::Construct)
            .len();

        if self.is_untyped_function_call(
            &tag_type,
            &apparent_type,
            call_signatures.len(),
            num_construct_signatures,
        ) {
            return self.resolve_untyped_call(node);
        }

        if call_signatures.is_empty() {
            if is_array_literal_expression(&node.parent()) {
                let diagnostic: Rc<Diagnostic> = Rc::new(
                    create_diagnostic_for_node(
                        &node_as_tagged_template_expression.tag,
                        &Diagnostics::It_is_likely_that_you_are_missing_a_comma_to_separate_these_two_template_expressions_They_form_a_tagged_template_expression_which_cannot_be_invoked,
                        None,
                    ).into()
                );
                self.diagnostics().add(diagnostic);
                return self.resolve_error_call(node);
            }

            self.invocation_error(
                &node_as_tagged_template_expression.tag,
                &apparent_type,
                SignatureKind::Call,
                None,
            );
            return self.resolve_error_call(node);
        }

        self.resolve_call(
            node,
            &call_signatures,
            candidates_out_array,
            check_mode,
            SignatureFlags::None,
            None,
        )
    }

    pub(super) fn get_diagnostic_head_message_for_decorator_resolution(
        &self,
        node: &Node, /*Decorator*/
    ) -> &'static DiagnosticMessage {
        match node.parent().kind() {
            SyntaxKind::ClassDeclaration |
            SyntaxKind::ClassExpression => &*Diagnostics::Unable_to_resolve_signature_of_class_decorator_when_called_as_an_expression,

            SyntaxKind::Parameter => &*Diagnostics::Unable_to_resolve_signature_of_parameter_decorator_when_called_as_an_expression,

            SyntaxKind::PropertyDeclaration => &*Diagnostics::Unable_to_resolve_signature_of_property_decorator_when_called_as_an_expression,

            SyntaxKind::MethodDeclaration |
            SyntaxKind::GetAccessor |
            SyntaxKind::SetAccessor => &*Diagnostics::Unable_to_resolve_signature_of_method_decorator_when_called_as_an_expression,

            _ => Debug_.fail(None)
        }
    }

    pub(super) fn resolve_decorator(
        &self,
        node: &Node, /*Decorator*/
        candidates_out_array: Option<&mut Vec<Rc<Signature>>>,
        check_mode: CheckMode,
    ) -> Rc<Signature> {
        let node_as_decorator = node.as_decorator();
        let func_type = self.check_expression(&node_as_decorator.expression, None, None);
        let apparent_type = self.get_apparent_type(&func_type);
        if self.is_error_type(&apparent_type) {
            return self.resolve_error_call(node);
        }

        let call_signatures = self.get_signatures_of_type(&apparent_type, SignatureKind::Call);
        let num_construct_signatures = self
            .get_signatures_of_type(&apparent_type, SignatureKind::Construct)
            .len();
        if self.is_untyped_function_call(
            &func_type,
            &apparent_type,
            call_signatures.len(),
            num_construct_signatures,
        ) {
            return self.resolve_untyped_call(node);
        }

        if self.is_potentially_uncalled_decorator(node, &call_signatures) {
            let node_str =
                get_text_of_node(&node_as_decorator.expression, Some(false)).into_owned();
            self.error(
                Some(node),
                &Diagnostics::_0_accepts_too_few_arguments_to_be_used_as_a_decorator_here_Did_you_mean_to_call_it_first_and_write_0,
                Some(vec![
                    node_str
                ])
            );
            return self.resolve_error_call(node);
        }

        let head_message = self.get_diagnostic_head_message_for_decorator_resolution(node);
        if call_signatures.is_empty() {
            let error_details = self.invocation_error_details(
                &node_as_decorator.expression,
                &apparent_type,
                SignatureKind::Call,
            );
            let InvocationErrorDetails {
                message_chain: error_details_message_chain,
                related_message: error_details_related_message,
            } = error_details;
            let message_chain =
                chain_diagnostic_messages(Some(error_details_message_chain), head_message, None);
            let diag: Rc<Diagnostic> = Rc::new(
                create_diagnostic_for_node_from_message_chain(
                    &node_as_decorator.expression,
                    message_chain,
                    None,
                )
                .into(),
            );
            if let Some(error_details_related_message) = error_details_related_message {
                add_related_info(
                    &diag,
                    vec![Rc::new(
                        create_diagnostic_for_node(
                            &node_as_decorator.expression,
                            error_details_related_message,
                            None,
                        )
                        .into(),
                    )],
                );
            }
            self.diagnostics().add(diag.clone());
            self.invocation_error_recovery(&apparent_type, SignatureKind::Call, &diag);
            return self.resolve_error_call(node);
        }

        self.resolve_call(
            node,
            &call_signatures,
            candidates_out_array,
            check_mode,
            SignatureFlags::None,
            Some(head_message),
        )
    }

    pub(super) fn create_signature_for_jsx_intrinsic(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
        result: &Type,
    ) -> Rc<Signature> {
        let namespace = self.get_jsx_namespace_at(Some(node));
        let exports = namespace
            .as_ref()
            .map(|namespace| self.get_exports_of_symbol(namespace));
        let type_symbol = exports.as_ref().and_then(|exports| {
            self.get_symbol(&(**exports).borrow(), &JsxNames::Element, SymbolFlags::Type)
        });
        let return_node = type_symbol.as_ref().and_then(|type_symbol| {
            self.node_builder.symbol_to_entity_name(
                self,
                type_symbol,
                Some(SymbolFlags::Type),
                Some(node),
                None,
                None,
            )
        });
        let declaration: Rc<Node> = factory.with(|factory_| {
            synthetic_factory.with(|synthetic_factory_| {
                factory_
                    .create_function_type_node(
                        synthetic_factory_,
                        Option::<NodeArray>::None,
                        vec![factory_
                            .create_parameter_declaration(
                                synthetic_factory_,
                                Option::<NodeArray>::None,
                                Option::<NodeArray>::None,
                                None,
                                Some("props".to_owned()),
                                None,
                                self.node_builder.type_to_type_node(
                                    self,
                                    result,
                                    Some(node),
                                    None,
                                    None,
                                ),
                                None,
                            )
                            .into()],
                        Some(if let Some(return_node) = return_node {
                            factory_
                                .create_type_reference_node(
                                    synthetic_factory_,
                                    return_node,
                                    Option::<NodeArray>::None,
                                )
                                .into()
                        } else {
                            factory_
                                .create_keyword_type_node(
                                    synthetic_factory_,
                                    SyntaxKind::AnyKeyword,
                                )
                                .into()
                        }),
                    )
                    .into()
            })
        });
        let parameter_symbol: Rc<Symbol> = self
            .create_symbol(
                SymbolFlags::FunctionScopedVariable,
                __String::new("props".to_owned()),
                None,
            )
            .into();
        parameter_symbol
            .as_transient_symbol()
            .symbol_links()
            .borrow_mut()
            .type_ = Some(result.type_wrapper());
        Rc::new(self.create_signature(
            Some(declaration),
            None,
            None,
            vec![parameter_symbol],
            Some(if let Some(type_symbol) = type_symbol.as_ref() {
                self.get_declared_type_of_symbol(type_symbol)
            } else {
                self.error_type()
            }),
            None,
            1,
            SignatureFlags::None,
        ))
    }

    pub(super) fn resolve_jsx_opening_like_element(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
        candidates_out_array: Option<&mut Vec<Rc<Signature>>>,
        check_mode: CheckMode,
    ) -> Rc<Signature> {
        let node_as_jsx_opening_like_element = node.as_jsx_opening_like_element();
        if self.is_jsx_intrinsic_identifier(&node_as_jsx_opening_like_element.tag_name()) {
            let result = self.get_intrinsic_attributes_type_from_jsx_opening_like_element(node);
            let fake_signature = self.create_signature_for_jsx_intrinsic(node, &result);
            self.check_type_assignable_to_and_optionally_elaborate(
                &self.check_expression_with_contextual_type(
                    &node_as_jsx_opening_like_element.attributes(),
                    &self.get_effective_first_argument_for_jsx_signature(
                        fake_signature.clone(),
                        node,
                    ),
                    None,
                    Some(CheckMode::Normal),
                ),
                &result,
                Some(node_as_jsx_opening_like_element.tag_name()),
                Some(node_as_jsx_opening_like_element.attributes()),
                None,
                None,
            );
            if length(
                node_as_jsx_opening_like_element
                    .maybe_type_arguments()
                    .map(|type_arguments| &**type_arguments),
            ) > 0
            {
                maybe_for_each(
                    node_as_jsx_opening_like_element.maybe_type_arguments(),
                    |type_argument: &Rc<Node>, _| -> Option<()> {
                        self.check_source_element(Some(&**type_argument));
                        None
                    },
                );
                self.diagnostics().add(Rc::new(
                    create_diagnostic_for_node_array(
                        &get_source_file_of_node(Some(node)).unwrap(),
                        node_as_jsx_opening_like_element
                            .maybe_type_arguments()
                            .unwrap(),
                        &Diagnostics::Expected_0_type_arguments_but_got_1,
                        Some(vec![
                            0usize.to_string(),
                            length(
                                node_as_jsx_opening_like_element
                                    .maybe_type_arguments()
                                    .map(|type_arguments| &**type_arguments),
                            )
                            .to_string(),
                        ]),
                    )
                    .into(),
                ));
            }
            return fake_signature;
        }
        let expr_types =
            self.check_expression(&node_as_jsx_opening_like_element.tag_name(), None, None);
        let apparent_type = self.get_apparent_type(&expr_types);
        if self.is_error_type(&apparent_type) {
            return self.resolve_error_call(node);
        }

        let signatures = self.get_uninstantiated_jsx_signatures_of_type(&expr_types, node);
        if self.is_untyped_function_call(&expr_types, &apparent_type, signatures.len(), 0) {
            return self.resolve_untyped_call(node);
        }

        if signatures.is_empty() {
            self.error(
                Some(node_as_jsx_opening_like_element.tag_name()),
                &Diagnostics::JSX_element_type_0_does_not_have_any_construct_or_call_signatures,
                Some(vec![get_text_of_node(
                    &node_as_jsx_opening_like_element.tag_name(),
                    None,
                )
                .into_owned()]),
            );
            return self.resolve_error_call(node);
        }

        self.resolve_call(
            node,
            &signatures,
            candidates_out_array,
            check_mode,
            SignatureFlags::None,
            None,
        )
    }

    pub(super) fn is_potentially_uncalled_decorator(
        &self,
        decorator: &Node, /*Decorator*/
        signatures: &[Rc<Signature>],
    ) -> bool {
        !signatures.is_empty()
            && every(signatures, |signature: &Rc<Signature>, _| {
                signature.min_argument_count() == 0
                    && !signature_has_rest_parameter(signature)
                    && signature.parameters().len()
                        < self.get_decorator_argument_count(decorator, signature)
            })
    }

    pub(super) fn resolve_signature(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates_out_array: Option<&mut Vec<Rc<Signature>>>,
        check_mode: CheckMode,
    ) -> Rc<Signature> {
        match node.kind() {
            SyntaxKind::CallExpression => {
                self.resolve_call_expression(node, candidates_out_array, check_mode)
            }
            SyntaxKind::NewExpression => {
                self.resolve_new_expression(node, candidates_out_array, check_mode)
            }
            SyntaxKind::TaggedTemplateExpression => {
                self.resolve_tagged_template_expression(node, candidates_out_array, check_mode)
            }
            SyntaxKind::Decorator => self.resolve_decorator(node, candidates_out_array, check_mode),
            SyntaxKind::JsxOpeningElement | SyntaxKind::JsxSelfClosingElement => {
                self.resolve_jsx_opening_like_element(node, candidates_out_array, check_mode)
            }
            _ => Debug_.assert_never(
                node,
                Some("Branch in 'resolveSignature' should be unreachable."),
            ),
        }
    }

    pub(super) fn get_resolved_signature_(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates_out_array: Option<&[Rc<Signature>]>,
        check_mode: Option<CheckMode>,
    ) -> Rc<Signature> {
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

    pub(super) fn check_deprecated_signature(
        &self,
        signature: &Signature,
        node: &Node, /*CallLikeExpression*/
    ) {
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

    pub(super) fn get_type_of_first_parameter_of_signature_with_fallback(
        &self,
        signature: &Signature,
        fallback_type: &Type,
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
