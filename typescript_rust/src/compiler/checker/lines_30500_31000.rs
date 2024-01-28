use std::{borrow::Borrow, collections::HashMap, io, ptr};

use gc::{Gc, GcCell};
use id_arena::Id;

use super::{
    signature_has_rest_parameter, CheckMode, GetDiagnosticSpanForCallNodeReturn,
    InvocationErrorDetails, JsxNames,
};
use crate::{
    add_related_info, chain_diagnostic_messages, create_diagnostic_for_node,
    create_diagnostic_for_node_array, create_diagnostic_for_node_from_message_chain,
    create_symbol_table, every, get_expando_initializer, get_factory,
    get_initializer_of_binary_expression, get_invoked_expression, get_jsdoc_class_tag,
    get_source_file_of_node, get_symbol_id, get_text_of_node, is_array_literal_expression,
    is_binary_expression, is_bindable_static_name_expression, is_call_expression, is_dotted_name,
    is_function_declaration, is_function_expression, is_function_like_declaration, is_import_call,
    is_in_js_file, is_jsdoc_construct_signature, is_object_literal_expression, is_prototype_access,
    is_qualified_name, is_same_entity_name, is_transient_symbol, is_var_const,
    is_variable_declaration, length, return_ok_default_if_none, skip_parentheses,
    try_get_property_access_or_identifier_to_string, try_maybe_for_each,
    walk_up_parenthesized_expressions, AsDoubleDeref, Debug_, Diagnostic, DiagnosticMessage,
    DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface, Diagnostics, HasArena,
    HasInitializerInterface, InArena, Matches, NamedDeclarationInterface, Node, NodeArray,
    NodeFlags, NodeInterface, ObjectFlags, OptionTry, Signature, SignatureFlags, SignatureKind,
    Symbol, SymbolFlags, SymbolInterface, SyntaxKind, TransientSymbolInterface, Type, TypeChecker,
    TypeFlags, TypeInterface,
    OptionInArena,
};

impl TypeChecker {
    pub(super) fn invocation_error(
        &self,
        error_target: Id<Node>,
        apparent_type: Id<Type>,
        kind: SignatureKind,
        related_information: Option<Id<DiagnosticRelatedInformation>>,
    ) -> io::Result<()> {
        let InvocationErrorDetails {
            message_chain,
            related_message: related_info,
        } = self.invocation_error_details(error_target, apparent_type, kind)?;
        let diagnostic: Id<Diagnostic> = self.alloc_diagnostic(
            create_diagnostic_for_node_from_message_chain(error_target, message_chain, None, self).into(),
        );
        if let Some(related_info) = related_info {
            add_related_info(
                &diagnostic.ref_(self),
                vec![self.alloc_diagnostic_related_information(
                    create_diagnostic_for_node(error_target, related_info, None, self).into(),
                )],
            );
        }
        if is_call_expression(&error_target.ref_(self).parent().ref_(self)) {
            let GetDiagnosticSpanForCallNodeReturn { start, length, .. } =
                self.get_diagnostic_span_for_call_node(error_target.ref_(self).parent(), Some(true));
            diagnostic.ref_(self).set_start(Some(start));
            diagnostic.ref_(self).set_length(Some(length));
        }
        self.diagnostics().add(diagnostic.clone());
        self.invocation_error_recovery(
            apparent_type,
            kind,
            &if let Some(related_information) = related_information {
                add_related_info(&diagnostic.ref_(self), vec![related_information]);
                diagnostic
            } else {
                diagnostic
            }.ref_(self),
        )?;

        Ok(())
    }

    pub(super) fn invocation_error_recovery(
        &self,
        apparent_type: Id<Type>,
        kind: SignatureKind,
        diagnostic: &Diagnostic,
    ) -> io::Result<()> {
        let apparent_type_symbol = apparent_type.ref_(self).maybe_symbol();
        if apparent_type_symbol.is_none() {
            return Ok(());
        }
        let apparent_type_symbol = apparent_type_symbol.unwrap();
        let import_node = (*self.get_symbol_links(apparent_type_symbol).ref_(self))
            .borrow()
            .originating_import
            .clone();
        if let Some(import_node) = import_node
            .filter(|&import_node| !is_import_call(import_node, self))
        {
            let sigs = self.get_signatures_of_type(
                self.get_type_of_symbol(
                    (*self.get_symbol_links(apparent_type_symbol).ref_(self))
                        .borrow()
                        .target
                        .clone()
                        .unwrap(),
                )?,
                kind,
            )?;
            if
            /* !sigs ||*/
            sigs.is_empty() {
                return Ok(());
            }

            add_related_info(
                diagnostic,
                vec![
                    self.alloc_diagnostic_related_information(
                        create_diagnostic_for_node(
                            import_node,
                            &Diagnostics::Type_originates_at_this_import_A_namespace_style_import_cannot_be_called_or_constructed_and_will_cause_a_failure_at_runtime_Consider_using_a_default_import_or_import_require_here_instead,
                            None,
                            self,
                        ).into()
                    )
                ]
            );
        }

        Ok(())
    }

    pub(super) fn resolve_tagged_template_expression(
        &self,
        node: Id<Node>, /*TaggedTemplateExpression*/
        candidates_out_array: Option<&mut Vec<Id<Signature>>>,
        check_mode: CheckMode,
    ) -> io::Result<Id<Signature>> {
        let node_ref = node.ref_(self);
        let node_as_tagged_template_expression = node_ref.as_tagged_template_expression();
        let tag_type =
            self.check_expression(node_as_tagged_template_expression.tag, None, None)?;
        let apparent_type = self.get_apparent_type(tag_type)?;

        if self.is_error_type(apparent_type) {
            return self.resolve_error_call(node);
        }

        let call_signatures = self.get_signatures_of_type(apparent_type, SignatureKind::Call)?;
        let num_construct_signatures = self
            .get_signatures_of_type(apparent_type, SignatureKind::Construct)?
            .len();

        if self.is_untyped_function_call(
            tag_type,
            apparent_type,
            call_signatures.len(),
            num_construct_signatures,
        )? {
            return self.resolve_untyped_call(node);
        }

        if call_signatures.is_empty() {
            if is_array_literal_expression(&node.ref_(self).parent().ref_(self)) {
                let diagnostic: Id<Diagnostic> = self.alloc_diagnostic(
                    create_diagnostic_for_node(
                        node_as_tagged_template_expression.tag,
                        &Diagnostics::It_is_likely_that_you_are_missing_a_comma_to_separate_these_two_template_expressions_They_form_a_tagged_template_expression_which_cannot_be_invoked,
                        None,
                        self,
                    ).into()
                );
                self.diagnostics().add(diagnostic);
                return self.resolve_error_call(node);
            }

            self.invocation_error(
                node_as_tagged_template_expression.tag,
                apparent_type,
                SignatureKind::Call,
                None,
            )?;
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
        node: Id<Node>, /*Decorator*/
    ) -> &'static DiagnosticMessage {
        match node.ref_(self).parent().ref_(self).kind() {
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
        node: Id<Node>, /*Decorator*/
        candidates_out_array: Option<&mut Vec<Id<Signature>>>,
        check_mode: CheckMode,
    ) -> io::Result<Id<Signature>> {
        let node_ref = node.ref_(self);
        let node_as_decorator = node_ref.as_decorator();
        let func_type = self.check_expression(node_as_decorator.expression, None, None)?;
        let apparent_type = self.get_apparent_type(func_type)?;
        if self.is_error_type(apparent_type) {
            return self.resolve_error_call(node);
        }

        let call_signatures = self.get_signatures_of_type(apparent_type, SignatureKind::Call)?;
        let num_construct_signatures = self
            .get_signatures_of_type(apparent_type, SignatureKind::Construct)?
            .len();
        if self.is_untyped_function_call(
            func_type,
            apparent_type,
            call_signatures.len(),
            num_construct_signatures,
        )? {
            return self.resolve_untyped_call(node);
        }

        if self.is_potentially_uncalled_decorator(node, &call_signatures) {
            let node_str =
                get_text_of_node(node_as_decorator.expression, Some(false), self).into_owned();
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
                node_as_decorator.expression,
                apparent_type,
                SignatureKind::Call,
            )?;
            let InvocationErrorDetails {
                message_chain: error_details_message_chain,
                related_message: error_details_related_message,
            } = error_details;
            let message_chain =
                chain_diagnostic_messages(Some(error_details_message_chain), head_message, None);
            let diag: Id<Diagnostic> = self.alloc_diagnostic(
                create_diagnostic_for_node_from_message_chain(
                    node_as_decorator.expression,
                    message_chain,
                    None,
                    self,
                )
                .into(),
            );
            if let Some(error_details_related_message) = error_details_related_message {
                add_related_info(
                    &diag.ref_(self),
                    vec![self.alloc_diagnostic_related_information(
                        create_diagnostic_for_node(
                            node_as_decorator.expression,
                            error_details_related_message,
                            None,
                            self,
                        )
                        .into(),
                    )],
                );
            }
            self.diagnostics().add(diag.clone());
            self.invocation_error_recovery(apparent_type, SignatureKind::Call, &diag.ref_(self))?;
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
        node: Id<Node>, /*JsxOpeningLikeElement*/
        result: Id<Type>,
    ) -> io::Result<Id<Signature>> {
        let namespace = self.get_jsx_namespace_at(Some(node))?;
        let exports = namespace.try_map(|namespace| self.get_exports_of_symbol(namespace))?;
        let type_symbol = exports.as_ref().try_and_then(|exports| {
            self.get_symbol(&exports.ref_(self), &JsxNames::Element, SymbolFlags::Type)
        })?;
        let return_node = type_symbol.try_and_then(|type_symbol| {
            self.node_builder().symbol_to_entity_name(
                type_symbol,
                Some(SymbolFlags::Type),
                Some(node),
                None,
                None,
            )
        })?;
        let declaration = get_factory(self).create_function_type_node(
            Option::<Id<NodeArray>>::None,
            vec![get_factory(self).create_parameter_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                None,
                Some("props"),
                None,
                self.node_builder()
                    .type_to_type_node(result, Some(node), None, None)?,
                None,
            )],
            Some(if let Some(return_node) = return_node {
                get_factory(self).create_type_reference_node(return_node, Option::<Id<NodeArray>>::None)
            } else {
                get_factory(self).create_keyword_type_node(SyntaxKind::AnyKeyword)
            }),
        );
        let parameter_symbol = self.alloc_symbol(
            self.create_symbol(
                SymbolFlags::FunctionScopedVariable,
                "props".to_owned(),
                None,
            )
            .into(),
        );
        parameter_symbol
            .ref_(self)
            .as_transient_symbol()
            .symbol_links()
            .ref_(self).borrow_mut()
            .type_ = Some(result);
        Ok(self.alloc_signature(self.create_signature(
            Some(declaration),
            None,
            None,
            vec![parameter_symbol],
            Some(if let Some(type_symbol) = type_symbol {
                self.get_declared_type_of_symbol(type_symbol)?
            } else {
                self.error_type()
            }),
            None,
            1,
            SignatureFlags::None,
        )))
    }

    pub(super) fn resolve_jsx_opening_like_element(
        &self,
        node: Id<Node>, /*JsxOpeningLikeElement*/
        candidates_out_array: Option<&mut Vec<Id<Signature>>>,
        check_mode: CheckMode,
    ) -> io::Result<Id<Signature>> {
        let node_ref = node.ref_(self);
        let node_as_jsx_opening_like_element = node_ref.as_jsx_opening_like_element();
        if self.is_jsx_intrinsic_identifier(node_as_jsx_opening_like_element.tag_name()) {
            let result = self.get_intrinsic_attributes_type_from_jsx_opening_like_element(node)?;
            let fake_signature = self.create_signature_for_jsx_intrinsic(node, result)?;
            self.check_type_assignable_to_and_optionally_elaborate(
                self.check_expression_with_contextual_type(
                    node_as_jsx_opening_like_element.attributes(),
                    self.get_effective_first_argument_for_jsx_signature(
                        fake_signature.clone(),
                        node,
                    )?,
                    None,
                    CheckMode::Normal,
                )?,
                result,
                Some(node_as_jsx_opening_like_element.tag_name()),
                Some(node_as_jsx_opening_like_element.attributes()),
                None,
                None,
            )?;
            if length(
                node_as_jsx_opening_like_element
                    .maybe_type_arguments()
                    .refed(self).as_double_deref(),
            ) > 0
            {
                try_maybe_for_each(
                    node_as_jsx_opening_like_element
                        .maybe_type_arguments()
                        .refed(self).as_deref(),
                    |&type_argument: &Id<Node>, _| -> io::Result<Option<()>> {
                        self.check_source_element(Some(type_argument))?;
                        Ok(None)
                    },
                )?;
                self.diagnostics().add(self.alloc_diagnostic(
                    create_diagnostic_for_node_array(
                        &get_source_file_of_node(node, self).ref_(self),
                        &node_as_jsx_opening_like_element
                            .maybe_type_arguments()
                            .unwrap().ref_(self),
                        &Diagnostics::Expected_0_type_arguments_but_got_1,
                        Some(vec![
                            0usize.to_string(),
                            length(
                                node_as_jsx_opening_like_element
                                    .maybe_type_arguments()
                                    .refed(self).as_double_deref(),
                            )
                            .to_string(),
                        ]),
                    )
                    .into(),
                ));
            }
            return Ok(fake_signature);
        }
        let expr_types =
            self.check_expression(node_as_jsx_opening_like_element.tag_name(), None, None)?;
        let apparent_type = self.get_apparent_type(expr_types)?;
        if self.is_error_type(apparent_type) {
            return self.resolve_error_call(node);
        }

        let signatures = self.get_uninstantiated_jsx_signatures_of_type(expr_types, node)?;
        if self.is_untyped_function_call(expr_types, apparent_type, signatures.len(), 0)? {
            return self.resolve_untyped_call(node);
        }

        if signatures.is_empty() {
            self.error(
                Some(node_as_jsx_opening_like_element.tag_name()),
                &Diagnostics::JSX_element_type_0_does_not_have_any_construct_or_call_signatures,
                Some(vec![get_text_of_node(
                    node_as_jsx_opening_like_element.tag_name(),
                    None,
                    self,
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
        decorator: Id<Node>, /*Decorator*/
        signatures: &[Id<Signature>],
    ) -> bool {
        !signatures.is_empty()
            && every(signatures, |&signature: &Id<Signature>, _| {
                signature.ref_(self).min_argument_count() == 0
                    && !signature_has_rest_parameter(&signature.ref_(self))
                    && signature.ref_(self).parameters().len()
                        < self.get_decorator_argument_count(decorator, signature)
            })
    }

    pub(super) fn resolve_signature(
        &self,
        node: Id<Node>, /*CallLikeExpression*/
        candidates_out_array: Option<&mut Vec<Id<Signature>>>,
        check_mode: CheckMode,
    ) -> io::Result<Id<Signature>> {
        Ok(match node.ref_(self).kind() {
            SyntaxKind::CallExpression => {
                self.resolve_call_expression(node, candidates_out_array, check_mode)?
            }
            SyntaxKind::NewExpression => {
                self.resolve_new_expression(node, candidates_out_array, check_mode)?
            }
            SyntaxKind::TaggedTemplateExpression => {
                self.resolve_tagged_template_expression(node, candidates_out_array, check_mode)?
            }
            SyntaxKind::Decorator => {
                self.resolve_decorator(node, candidates_out_array, check_mode)?
            }
            SyntaxKind::JsxOpeningElement | SyntaxKind::JsxSelfClosingElement => {
                self.resolve_jsx_opening_like_element(node, candidates_out_array, check_mode)?
            }
            _ => Debug_.assert_never(
                node,
                Some("Branch in 'resolveSignature' should be unreachable."),
            ),
        })
    }

    pub(super) fn get_resolved_signature_(
        &self,
        node: Id<Node>, /*CallLikeExpression*/
        candidates_out_array: Option<&mut Vec<Id<Signature>>>,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Signature>> {
        let links = self.get_node_links(node);
        let cached = (*links).borrow().resolved_signature.clone();
        if let Some(cached) = cached.filter(|&cached| {
            cached != self.resolving_signature() && candidates_out_array.is_none()
        }) {
            return Ok(cached.clone());
        }
        links.borrow_mut().resolved_signature = Some(self.resolving_signature());
        let result = self.resolve_signature(
            node,
            candidates_out_array,
            check_mode.unwrap_or(CheckMode::Normal),
        )?;
        if result != self.resolving_signature() {
            links.borrow_mut().resolved_signature =
                if self.flow_loop_start() == self.flow_loop_count() {
                    Some(result.clone())
                } else {
                    cached
                }
        }
        Ok(result)
    }

    pub(super) fn is_js_constructor(&self, node: Option<Id<Node>>) -> io::Result<bool> {
        let Some(node) = node else {
            return Ok(false);
        };
        if !is_in_js_file(Some(&node.ref_(self))) {
            return Ok(false);
        }
        let func = if is_function_declaration(&node.ref_(self)) || is_function_expression(&node.ref_(self)) {
            Some(node)
        } else if is_variable_declaration(&node.ref_(self))
            && matches!(
                node.ref_(self).as_variable_declaration().maybe_initializer(),
                Some(node_initializer) if is_function_expression(&node_initializer.ref_(self))
            )
        {
            node.ref_(self).as_variable_declaration().maybe_initializer()
        } else {
            None
        };
        if let Some(func) = func {
            if get_jsdoc_class_tag(node, self).is_some() {
                return Ok(true);
            }

            let symbol = self.get_symbol_of_node(func)?;
            return Ok(matches!(
                symbol.and_then(|symbol| symbol.ref_(self).maybe_members().clone()).as_ref(),
                Some(symbol_members) if !symbol_members.ref_(self).is_empty(),
            ));
        }
        Ok(false)
    }

    pub(super) fn merge_js_symbols(
        &self,
        target: Id<Symbol>,
        source: Option<Id<Symbol>>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let source = return_ok_default_if_none!(source);
        let links = self.get_symbol_links(source);
        if !matches!(
            (*links.ref_(self)).borrow().inferred_class_symbol.as_ref(),
            Some(links_inferred_class_symbol) if links_inferred_class_symbol.contains_key(&get_symbol_id(&target.ref_(self)))
        ) {
            let inferred = if is_transient_symbol(&target.ref_(self)) {
                target
            } else {
                self.clone_symbol(target)
            };
            {
                let inferred_ref = inferred.ref_(self);
                let mut inferred_exports = inferred_ref.maybe_exports_mut();
                if inferred_exports.is_none() {
                    *inferred_exports = Some(self.alloc_symbol_table(create_symbol_table(
                        self.arena(),
                        Option::<&[Id<Symbol>]>::None,
                    )));
                }
            }
            {
                let inferred_ref = inferred.ref_(self);
                let mut inferred_members = inferred_ref.maybe_members_mut();
                if inferred_members.is_none() {
                    *inferred_members = Some(self.alloc_symbol_table(create_symbol_table(
                        self.arena(),
                        Option::<&[Id<Symbol>]>::None,
                    )));
                }
            }
            inferred.ref_(self).set_flags(
                inferred.ref_(self).flags() | (source.ref_(self).flags() & SymbolFlags::Class),
            );
            if matches!(
                source.ref_(self).maybe_exports().as_ref(),
                Some(source_exports) if !source_exports.ref_(self).is_empty()
            ) {
                self.merge_symbol_table(
                    inferred.ref_(self).maybe_exports().unwrap(),
                    &source.ref_(self).maybe_exports().unwrap().ref_(self),
                    None,
                )?;
            }
            if matches!(
                source.ref_(self).maybe_members().as_ref(),
                Some(source_members) if !source_members.ref_(self).is_empty()
            ) {
                self.merge_symbol_table(
                    inferred.ref_(self).maybe_members().unwrap(),
                    &source.ref_(self).maybe_members().unwrap().ref_(self),
                    None,
                )?;
            }
            {
                let links_ref = links.ref_(self);
                let mut links = links_ref.borrow_mut();
                if links.inferred_class_symbol.is_none() {
                    links.inferred_class_symbol = Some(HashMap::new());
                }
                links
                    .inferred_class_symbol
                    .as_mut()
                    .unwrap()
                    .insert(get_symbol_id(&inferred.ref_(self)), inferred.clone());
            }
            return Ok(Some(inferred));
        }
        let ret = (*links.ref_(self))
            .borrow()
            .inferred_class_symbol
            .as_ref()
            .unwrap()
            .get(&get_symbol_id(&target.ref_(self)))
            .cloned();
        Ok(ret)
    }

    pub(super) fn get_assigned_class_symbol(
        &self,
        decl: Id<Node>, /*Declaration*/
    ) -> io::Result<Option<Id<Symbol>>> {
        let assignment_symbol = /*decl &&*/ self.get_symbol_of_expando(decl, true)?;
        let prototype = assignment_symbol
            .and_then(|assignment_symbol| assignment_symbol.ref_(self).maybe_exports().clone())
            .and_then(|assignment_symbol_exports| {
                assignment_symbol_exports
                    .ref_(self)
                    .get("prototype")
                    .cloned()
            });
        let init = prototype
            .and_then(|prototype| prototype.ref_(self).maybe_value_declaration())
            .and_then(|prototype_value_declaration| {
                self.get_assigned_js_prototype(prototype_value_declaration)
            });
        init.try_and_then(|init| self.get_symbol_of_node(init))
    }

    pub(super) fn get_symbol_of_expando(
        &self,
        node: Id<Node>,
        allow_declaration: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        if node.ref_(self).maybe_parent().is_none() {
            return Ok(None);
        }
        let mut name: Option<Id<Node /*Expression | BindingName*/>> = None;
        let mut decl: Option<Id<Node>> = None;
        if is_variable_declaration(&node.ref_(self).parent().ref_(self))
            && node.ref_(self).parent().ref_(self).as_variable_declaration().maybe_initializer() == Some(node)
        {
            if !is_in_js_file(Some(&node.ref_(self)))
                && !(is_var_const(node.ref_(self).parent(), self) && is_function_like_declaration(&node.ref_(self)))
            {
                return Ok(None);
            }
            name = node.ref_(self).parent().ref_(self).as_variable_declaration().maybe_name();
            decl = Some(node.ref_(self).parent());
        } else if is_binary_expression(&node.ref_(self).parent().ref_(self)) {
            let parent_node = node.ref_(self).parent();
            let parent_node_ref = parent_node.ref_(self);
            let parent_node_as_binary_expression = parent_node_ref.as_binary_expression();
            let parent_node_operator = node.ref_(self).parent().ref_(self).as_binary_expression().operator_token.ref_(self).kind();
            if parent_node_operator == SyntaxKind::EqualsToken
                && (allow_declaration || parent_node_as_binary_expression.right == node)
            {
                name = Some(parent_node_as_binary_expression.left.clone());
                decl = name.clone();
            } else if matches!(
                parent_node_operator,
                SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
            ) {
                if is_variable_declaration(&parent_node.ref_(self).parent().ref_(self))
                    && parent_node.ref_(self).parent().ref_(self).as_variable_declaration().maybe_initializer() == Some(parent_node)
                {
                    name = parent_node.ref_(self).parent().ref_(self).as_variable_declaration().maybe_name();
                    decl = Some(parent_node.ref_(self).parent());
                } else if is_binary_expression(&parent_node.ref_(self).parent().ref_(self))
                    && parent_node
                        .ref_(self).parent()
                        .ref_(self).as_binary_expression()
                        .operator_token
                        .ref_(self).kind()
                        == SyntaxKind::EqualsToken
                    && (allow_declaration
                        || parent_node.ref_(self).parent().ref_(self).as_binary_expression().right == parent_node)
                {
                    name = Some(parent_node.ref_(self).parent().ref_(self).as_binary_expression().left);
                    decl = name;
                }

                if match name {
                    None => true,
                    Some(name) => {
                        !is_bindable_static_name_expression(name, None, self)
                            || !is_same_entity_name(name, parent_node.ref_(self).as_binary_expression().left, self)
                    }
                } {
                    return Ok(None);
                }
            }
        } else if allow_declaration && is_function_declaration(&node.ref_(self)) {
            name = node.ref_(self).as_named_declaration().maybe_name();
            decl = Some(node);
        }

        let decl = return_ok_default_if_none!(decl);
        let name = return_ok_default_if_none!(name);
        if !allow_declaration && get_expando_initializer(node, is_prototype_access(name, self), self).is_none()
        {
            return Ok(None);
        }
        self.get_symbol_of_node(decl)
    }

    pub(super) fn get_assigned_js_prototype(&self, node: Id<Node>) -> Option<Id<Node>> {
        if node.ref_(self).maybe_parent().is_none() {
            return None;
        }
        let mut parent = node.ref_(self).maybe_parent();
        while let Some(parent_present) = parent
            .filter(|parent| parent.ref_(self).kind() == SyntaxKind::PropertyAccessExpression)
        {
            parent = parent_present.ref_(self).maybe_parent();
        }
        if let Some(parent) = parent.filter(|parent| {
            is_binary_expression(&parent.ref_(self)) && {
                let parent_ref = parent.ref_(self);
                let parent_as_binary_expression = parent_ref.as_binary_expression();
                is_prototype_access(parent_as_binary_expression.left, self)
                    && parent_as_binary_expression.operator_token.ref_(self).kind() == SyntaxKind::EqualsToken
            }
        }) {
            let right = get_initializer_of_binary_expression(parent, self);
            return if is_object_literal_expression(&right.ref_(self)) {
                Some(right)
            } else {
                None
            };
        }
        None
    }

    pub(super) fn check_call_expression(
        &self,
        node: Id<Node>, /*CallExpression |} NewExpression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        if !self.check_grammar_type_arguments(
            node,
            node.ref_(self).as_has_type_arguments()
                .maybe_type_arguments(),
        ) {
            self.check_grammar_arguments(node.ref_(self).as_has_arguments().maybe_arguments());
        }

        let signature = self.get_resolved_signature_(node, None, check_mode)?;
        if signature == self.resolving_signature() {
            return Ok(self.non_inferrable_type());
        }

        self.check_deprecated_signature(signature.clone(), node)?;

        if node.ref_(self).as_has_expression().expression().ref_(self).kind() == SyntaxKind::SuperKeyword {
            return Ok(self.void_type());
        }

        if node.ref_(self).kind() == SyntaxKind::NewExpression {
            let declaration = signature.ref_(self).declaration;

            if declaration.try_matches(|declaration| -> io::Result<_> {
                Ok(!matches!(
                    declaration.ref_(self).kind(),
                    SyntaxKind::Constructor
                        | SyntaxKind::ConstructSignature
                        | SyntaxKind::ConstructorType
                ) && !is_jsdoc_construct_signature(declaration, self)
                    && !self.is_js_constructor(Some(declaration))?)
            })? {
                if self.no_implicit_any {
                    self.error(
                        Some(node),
                        &Diagnostics::new_expression_whose_target_lacks_a_construct_signature_implicitly_has_an_any_type,
                        None,
                    );
                }
                return Ok(self.any_type());
            }
        }

        if is_in_js_file(Some(&node.ref_(self))) && self.is_common_js_require(node)? {
            return self.resolve_external_module_type_by_literal(
                node.ref_(self).as_has_arguments().maybe_arguments().unwrap().ref_(self)[0],
            );
        }

        let return_type = self.get_return_type_of_signature(signature.clone())?;
        if return_type
            .ref_(self)
            .flags()
            .intersects(TypeFlags::ESSymbolLike)
            && self.is_symbol_or_symbol_for_call(node)?
        {
            return self.get_es_symbol_like_type_for_node(
                walk_up_parenthesized_expressions(node.ref_(self).parent(), self).unwrap(),
            );
        }
        if node.ref_(self).kind() == SyntaxKind::CallExpression
            && node.ref_(self).as_call_expression().question_dot_token.is_none()
            && node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ExpressionStatement
            && return_type.ref_(self).flags().intersects(TypeFlags::Void)
            && self.get_type_predicate_of_signature(signature)?.is_some()
        {
            let node_ref = node.ref_(self);
            let node_as_call_expression = node_ref.as_call_expression();
            if !is_dotted_name(node_as_call_expression.expression, self) {
                self.error(
                    Some(node_as_call_expression.expression),
                    &Diagnostics::Assertions_require_the_call_target_to_be_an_identifier_or_qualified_name,
                    None,
                );
            } else if self.get_effects_signature(node)?.is_none() {
                let diagnostic = self.error(
                    Some(node_as_call_expression.expression),
                    &Diagnostics::Assertions_require_every_name_in_the_call_target_to_be_declared_with_an_explicit_type_annotation,
                    None,
                );
                self.get_type_of_dotted_name(
                    node_as_call_expression.expression,
                    Some(&diagnostic.ref_(self)),
                )?;
            }
        }

        if is_in_js_file(Some(&node.ref_(self))) {
            let js_symbol = self.get_symbol_of_expando(node, false)?;
            if let Some(js_symbol_exports) = js_symbol
                .and_then(|js_symbol| js_symbol.ref_(self).maybe_exports().clone())
                .filter(|js_symbol_exports| !js_symbol_exports.ref_(self).is_empty())
            {
                let js_assignment_type = self.create_anonymous_type(
                    js_symbol,
                    js_symbol_exports,
                    vec![],
                    vec![],
                    vec![],
                )?;
                {
                    let js_assignment_type = js_assignment_type.ref_(self);
                    let js_assignment_type_as_object_flags_type =
                        js_assignment_type.as_object_flags_type();
                    js_assignment_type_as_object_flags_type.set_object_flags(
                        js_assignment_type_as_object_flags_type.object_flags()
                            | ObjectFlags::JSLiteral,
                    );
                }
                return self.get_intersection_type(
                    &[return_type, js_assignment_type],
                    Option::<Id<Symbol>>::None,
                    None,
                );
            }
        }

        Ok(return_type)
    }

    pub(super) fn check_deprecated_signature(
        &self,
        signature: Id<Signature>,
        node: Id<Node>, /*CallLikeExpression*/
    ) -> io::Result<()> {
        if let Some(signature_declaration) =
            signature
                .ref_(self).declaration
                .filter(|signature_declaration| {
                    signature_declaration
                        .ref_(self).flags()
                        .intersects(NodeFlags::Deprecated)
                })
        {
            let suggestion_node = self.get_deprecated_suggestion_node(node);
            let invoked_expression = get_invoked_expression(&node.ref_(self));
            let name = try_get_property_access_or_identifier_to_string(invoked_expression, self);
            self.add_deprecated_suggestion_with_signature(
                suggestion_node,
                signature_declaration,
                name.as_deref(),
                &self.signature_to_string_(
                    signature.clone(),
                    Option::<Id<Node>>::None,
                    None,
                    None,
                    None,
                )?,
            );
        }

        Ok(())
    }

    pub(super) fn get_deprecated_suggestion_node(&self, node: Id<Node>) -> Id<Node> {
        let node = skip_parentheses(node, None, self);
        match node.ref_(self).kind() {
            SyntaxKind::CallExpression | SyntaxKind::Decorator | SyntaxKind::NewExpression => {
                self.get_deprecated_suggestion_node(node.ref_(self).as_has_expression().expression())
            }
            SyntaxKind::TaggedTemplateExpression => {
                self.get_deprecated_suggestion_node(node.ref_(self).as_tagged_template_expression().tag)
            }
            SyntaxKind::JsxOpeningElement | SyntaxKind::JsxSelfClosingElement => {
                self.get_deprecated_suggestion_node(node.ref_(self).as_jsx_opening_like_element().tag_name())
            }
            SyntaxKind::ElementAccessExpression => self.get_deprecated_suggestion_node(
                node.ref_(self).as_element_access_expression().argument_expression,
            ),
            SyntaxKind::PropertyAccessExpression => {
                self.get_deprecated_suggestion_node(node.ref_(self).as_property_access_expression().name)
            }
            SyntaxKind::TypeReference => {
                let type_reference = node;
                let type_reference_ref = type_reference.ref_(self);
                let type_reference_as_type_reference_node = type_reference_ref.as_type_reference_node();
                if is_qualified_name(&type_reference_as_type_reference_node.type_name.ref_(self)) {
                    type_reference_as_type_reference_node
                        .type_name
                        .ref_(self).as_qualified_name()
                        .right
                } else {
                    type_reference
                }
            }
            _ => node,
        }
    }
}
