use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    cmp,
    collections::HashMap,
    io,
    rc::Rc,
};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;
use local_macros::enum_unwrapped;

use super::{CheckMode, CheckTypeContainingMessageChain, CheckTypeErrorOutputContainer};
use crate::{
    add_related_info, are_option_gcs_equal, chain_diagnostic_messages,
    chain_diagnostic_messages_multiple, create_diagnostic_for_node,
    create_diagnostic_for_node_array, create_diagnostic_for_node_from_message_chain,
    create_file_diagnostic, every, factory, filter, find, first, flat_map, flatten, for_each,
    get_error_span_for_node, get_first_identifier, get_source_file_of_node, id_text,
    is_access_expression, is_binding_pattern, is_call_expression,
    is_function_expression_or_arrow_function, is_function_like_declaration, is_identifier,
    is_in_js_file, is_jsx_opening_element, is_jsx_opening_like_element, is_new_expression,
    is_parameter, is_property_access_expression, is_rest_parameter, last, length, map,
    node_is_present, parse_node_factory, return_ok_default_if_none, set_parent, set_text_range,
    set_text_range_pos_end, skip_outer_expressions, some, try_maybe_for_each, try_some,
    BaseDiagnostic, BaseDiagnosticRelatedInformation, Debug_, Diagnostic, DiagnosticInterface,
    DiagnosticMessage, DiagnosticMessageChain, DiagnosticMessageText, DiagnosticRelatedInformation,
    DiagnosticRelatedInformationInterface, Diagnostics, ElementFlags, InferenceContext,
    InferenceFlags, Node, NodeArray, NodeInterface, ReadonlyTextRange, RelationComparisonResult,
    ScriptTarget, Signature, SignatureFlags, SymbolFlags, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypeInterface, UsizeOrNegativeInfinity,
};

impl TypeChecker {
    pub(super) fn maybe_add_missing_await_info(
        &self,
        report_errors: bool,
        error_output_container: Gc<Box<dyn CheckTypeErrorOutputContainer>>,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        error_node: Option<impl Borrow<Node>>,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<()> {
        if let Some(error_node) = error_node {
            let error_node = error_node.borrow();
            if report_errors && error_output_container.errors_len() > 0 {
                if self
                    .get_awaited_type_of_promise(target, Option::<&Node>::None, None, None)?
                    .is_some()
                {
                    return Ok(());
                }
                let awaited_type_of_source =
                    self.get_awaited_type_of_promise(source, Option::<&Node>::None, None, None)?;
                if matches!(
                    awaited_type_of_source,
                    Some(awaited_type_of_source) if self.is_type_related_to(
                        awaited_type_of_source,
                        target,
                        relation.clone()
                    )?
                ) {
                    add_related_info(
                        &error_output_container.get_error(0).unwrap(),
                        vec![Gc::new(
                            create_diagnostic_for_node(
                                error_node,
                                &Diagnostics::Did_you_forget_to_use_await,
                                None,
                            )
                            .into(),
                        )],
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn get_this_argument_of_call(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> Option<Gc<Node /*LeftHandSideExpression*/>> {
        let expression = if node.kind() == SyntaxKind::CallExpression {
            Some(node.as_call_expression().expression.clone())
        } else if node.kind() == SyntaxKind::TaggedTemplateExpression {
            Some(node.as_tagged_template_expression().tag.clone())
        } else {
            None
        };
        if let Some(expression) = expression.as_ref() {
            let callee = skip_outer_expressions(expression, None);
            if is_access_expression(&callee) {
                return callee.as_has_expression().maybe_expression();
            }
        }
        None
    }

    pub(super) fn create_synthetic_expression<TTupleNameSource: Borrow<Node>>(
        &self,
        parent: &Node,
        type_: Id<Type>,
        is_spread: Option<bool>,
        tuple_name_source: Option<
            TTupleNameSource, /*ParameterDeclaration | NamedTupleMember*/
        >,
    ) -> Gc<Node> {
        let result = parse_node_factory.with(|parse_node_factory_| {
            parse_node_factory_.create_synthetic_expression(
                type_,
                is_spread,
                tuple_name_source
                    .map(|tuple_name_source| tuple_name_source.borrow().node_wrapper()),
            )
        });
        set_text_range(&*result, Some(parent));
        set_parent(&result, Some(parent));
        result
    }

    pub(super) fn get_effective_call_arguments(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> io::Result<Vec<Gc<Node /*Expression*/>>> {
        if node.kind() == SyntaxKind::TaggedTemplateExpression {
            let template = &node.as_tagged_template_expression().template;
            let mut args: Vec<Gc<Node /*Expression*/>> = vec![self.create_synthetic_expression(
                template,
                self.get_global_template_strings_array_type()?,
                None,
                Option::<&Node>::None,
            )];
            if template.kind() == SyntaxKind::TemplateExpression {
                for_each(
                    &template.as_template_expression().template_spans,
                    |span: &Gc<Node>, _| -> Option<()> {
                        args.push(span.as_template_span().expression.clone());
                        None
                    },
                );
            }
            return Ok(args);
        }
        if node.kind() == SyntaxKind::Decorator {
            return self.get_effective_decorator_arguments(node);
        }
        if is_jsx_opening_like_element(node) {
            return Ok(
                if !node
                    .as_jsx_opening_like_element()
                    .attributes()
                    .as_jsx_attributes()
                    .properties
                    .is_empty()
                    || is_jsx_opening_element(node)
                        && !node.parent().as_jsx_element().children.is_empty()
                {
                    vec![node.as_jsx_opening_like_element().attributes()]
                } else {
                    vec![]
                },
            );
        }
        let args = node
            .as_has_arguments()
            .maybe_arguments()
            .map_or_else(|| vec![], |arguments| arguments.to_vec());
        let spread_index = self.get_spread_argument_index(&args);
        if let Some(spread_index) = spread_index {
            let mut effective_args = args[0..spread_index].to_owned();
            for i in spread_index..args.len() {
                let arg = &args[i];
                let spread_type = if arg.kind() == SyntaxKind::SpreadElement {
                    Some(if self.flow_loop_count() > 0 {
                        self.check_expression(&arg.as_spread_element().expression, None, None)?
                    } else {
                        self.check_expression_cached(&arg.as_spread_element().expression, None)?
                    })
                } else {
                    None
                };
                if let Some(spread_type) =
                    spread_type.filter(|&spread_type| self.is_tuple_type(spread_type))
                {
                    let spread_type_target = self.type_(spread_type).as_type_reference().target;
                    for_each(
                        &self.get_type_arguments(spread_type)?,
                        |&t: &Id<Type>, i| -> Option<()> {
                            let flags =
                                self.type_(spread_type_target).as_tuple_type().element_flags[i];
                            let synthetic_arg = self.create_synthetic_expression(
                                arg,
                                if flags.intersects(ElementFlags::Rest) {
                                    self.create_array_type(t, None)
                                } else {
                                    t
                                },
                                Some(flags.intersects(ElementFlags::Variable)),
                                self.type_(spread_type_target)
                                    .as_tuple_type()
                                    .labeled_element_declarations
                                    .as_ref()
                                    .and_then(|spread_type_target_labeled_element_declarations| {
                                        spread_type_target_labeled_element_declarations
                                            .get(i)
                                            .cloned()
                                    }),
                            );
                            effective_args.push(synthetic_arg);
                            None
                        },
                    );
                } else {
                    effective_args.push(arg.clone());
                }
            }
            return Ok(effective_args);
        }
        Ok(args)
    }

    pub(super) fn get_effective_decorator_arguments(
        &self,
        node: &Node, /*Decorator*/
    ) -> io::Result<Vec<Gc<Node /*Expression*/>>> {
        let parent = node.parent();
        let expr = &node.as_decorator().expression;
        Ok(match parent.kind() {
            SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => {
                vec![self.create_synthetic_expression(
                    expr,
                    self.get_type_of_symbol(self.get_symbol_of_node(&parent)?.unwrap())?,
                    None,
                    Option::<&Node>::None,
                )]
            }
            SyntaxKind::Parameter => {
                let func = parent.parent();
                vec![
                    self.create_synthetic_expression(
                        expr,
                        if parent.parent().kind() == SyntaxKind::Constructor {
                            self.get_type_of_symbol(self.get_symbol_of_node(&func)?.unwrap())?
                        } else {
                            self.error_type()
                        },
                        None,
                        Option::<&Node>::None,
                    ),
                    self.create_synthetic_expression(
                        expr,
                        self.any_type(),
                        None,
                        Option::<&Node>::None,
                    ),
                    self.create_synthetic_expression(
                        expr,
                        self.number_type(),
                        None,
                        Option::<&Node>::None,
                    ),
                ]
            }
            SyntaxKind::PropertyDeclaration
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor => {
                let has_prop_desc = parent.kind() != SyntaxKind::PropertyDeclaration
                    && self.language_version != ScriptTarget::ES3;
                vec![
                    self.create_synthetic_expression(
                        expr,
                        self.get_parent_type_of_class_element(&parent)?,
                        None,
                        Option::<&Node>::None,
                    ),
                    self.create_synthetic_expression(
                        expr,
                        self.get_class_element_property_key_type(&parent)?,
                        None,
                        Option::<&Node>::None,
                    ),
                    self.create_synthetic_expression(
                        expr,
                        if has_prop_desc {
                            self.create_typed_property_descriptor_type(
                                self.get_type_of_node(&parent)?,
                            )?
                        } else {
                            self.any_type()
                        },
                        None,
                        Option::<&Node>::None,
                    ),
                ]
            }
            _ => Debug_.fail(None),
        })
    }

    pub(super) fn get_decorator_argument_count(
        &self,
        node: &Node, /*Decorator*/
        signature: &Signature,
    ) -> usize {
        match node.parent().kind() {
            SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => 1,
            SyntaxKind::PropertyDeclaration => 2,
            SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                if self.language_version == ScriptTarget::ES3 || signature.parameters().len() <= 2 {
                    2
                } else {
                    3
                }
            }
            SyntaxKind::Parameter => 3,
            _ => Debug_.fail(None),
        }
    }

    pub(super) fn get_diagnostic_span_for_call_node(
        &self,
        node: &Node, /*CallExpression*/
        do_not_include_arguments: Option<bool>,
    ) -> GetDiagnosticSpanForCallNodeReturn {
        let start: isize;
        let length: isize;
        let source_file = get_source_file_of_node(node);

        let node_as_call_expression = node.as_call_expression();
        if is_property_access_expression(&node_as_call_expression.expression) {
            let name_span = get_error_span_for_node(
                &source_file,
                &node_as_call_expression
                    .expression
                    .as_property_access_expression()
                    .name,
            );
            start = name_span.start;
            length = if do_not_include_arguments == Some(true) {
                name_span.length
            } else {
                node.end() - start
            };
        } else {
            let expression_span =
                get_error_span_for_node(&source_file, &node_as_call_expression.expression);
            start = expression_span.start;
            length = if do_not_include_arguments == Some(true) {
                expression_span.length
            } else {
                node.end() - start
            };
        }
        GetDiagnosticSpanForCallNodeReturn {
            start,
            length,
            source_file,
        }
    }

    pub(super) fn get_diagnostic_for_call_node(
        &self,
        node: &Node, /*CallLikeExpression*/
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Gc<Diagnostic /*DiagnosticWithLocation*/> {
        if is_call_expression(node) {
            let GetDiagnosticSpanForCallNodeReturn {
                source_file,
                start,
                length,
            } = self.get_diagnostic_span_for_call_node(node, None);
            Gc::new(create_file_diagnostic(&source_file, start, length, message, args).into())
        } else {
            Gc::new(create_diagnostic_for_node(node, message, args).into())
        }
    }

    pub(super) fn is_promise_resolve_arity_error(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> io::Result<bool> {
        if !is_call_expression(node) {
            return Ok(false);
        }
        let node_as_call_expression = node.as_call_expression();
        if !is_identifier(&node_as_call_expression.expression) {
            return Ok(false);
        }

        let symbol = self.resolve_name_(
            Some(&*node_as_call_expression.expression),
            &node_as_call_expression
                .expression
                .as_identifier()
                .escaped_text,
            SymbolFlags::Value,
            None,
            Option::<Gc<Node>>::None,
            false,
            None,
        )?;
        let decl = symbol
            .and_then(|symbol| self.symbol(symbol).maybe_value_declaration());
        if decl.is_none() {
            return Ok(false);
        }
        let decl = decl.unwrap();
        if !is_parameter(&decl)
            || !is_function_expression_or_arrow_function(&decl.parent())
            || !is_new_expression(&decl.parent().parent())
            || is_identifier(&decl.parent().parent().as_new_expression().expression)
        {
            return Ok(false);
        }

        let global_promise_symbol =
            return_ok_default_if_none!(self.get_global_promise_constructor_symbol(false)?);

        let constructor_symbol = self.get_symbol_at_location_(
            &decl.parent().parent().as_new_expression().expression,
            Some(true),
        )?;
        Ok(matches!(
            constructor_symbol,
            Some(constructor_symbol) if constructor_symbol == global_promise_symbol
        ))
    }

    pub(super) fn get_argument_arity_error(
        &self,
        node: &Node, /*CallLikeExpression*/
        signatures: &[Gc<Signature>],
        args: &[Gc<Node /*Expression*/>],
    ) -> io::Result<Gc<Diagnostic>> {
        let spread_index = self.get_spread_argument_index(args);
        if let Some(spread_index) = spread_index {
            return Ok(Gc::new(
                create_diagnostic_for_node(
                    &args[spread_index],
                    &Diagnostics::A_spread_argument_must_either_have_a_tuple_type_or_be_passed_to_a_rest_parameter,
                    None,
                ).into()
            ));
        }
        let mut min = usize::MAX;
        let mut max = UsizeOrNegativeInfinity::NegativeInfinity;
        let mut max_below = UsizeOrNegativeInfinity::NegativeInfinity;
        let mut min_above = usize::MAX;

        let mut closest_signature: Option<Gc<Signature>> = None;
        for sig in signatures {
            let min_parameter = self.get_min_argument_count(sig, None)?;
            let max_parameter = self.get_parameter_count(sig)?;
            if min_parameter < min {
                min = min_parameter;
                closest_signature = Some(sig.clone());
            }
            max = UsizeOrNegativeInfinity::Usize(match max {
                UsizeOrNegativeInfinity::NegativeInfinity => max_parameter,
                UsizeOrNegativeInfinity::Usize(max) => cmp::max(max, max_parameter),
            });
            if min_parameter < args.len()
                && match max_below {
                    UsizeOrNegativeInfinity::NegativeInfinity => true,
                    UsizeOrNegativeInfinity::Usize(max_below) => min_parameter > max_below,
                }
            {
                max_below = UsizeOrNegativeInfinity::Usize(min_parameter);
            }
            if args.len() < max_parameter && max_parameter < min_above {
                min_above = max_parameter;
            }
        }
        let has_rest_parameter = try_some(
            Some(signatures),
            Some(|signature: &Gc<Signature>| self.has_effective_rest_parameter(signature)),
        )?;
        let parameter_range = if has_rest_parameter {
            min.to_string()
        } else if match max {
            UsizeOrNegativeInfinity::NegativeInfinity => false,
            UsizeOrNegativeInfinity::Usize(max) => min < max,
        } {
            format!(
                "{}-{}",
                min,
                enum_unwrapped!(max, [UsizeOrNegativeInfinity, Usize])
            )
        } else {
            min.to_string()
        };
        let error = if has_rest_parameter {
            &*Diagnostics::Expected_at_least_0_arguments_but_got_1
        } else if parameter_range == "1"
            && args.is_empty()
            && self.is_promise_resolve_arity_error(node)?
        {
            &*Diagnostics::Expected_0_arguments_but_got_1_Did_you_forget_to_include_void_in_your_type_argument_to_Promise
        } else {
            &*Diagnostics::Expected_0_arguments_but_got_1
        };
        Ok(
            if min < args.len()
                && match max {
                    UsizeOrNegativeInfinity::NegativeInfinity => false,
                    UsizeOrNegativeInfinity::Usize(max) => args.len() < max,
                }
            {
                self.get_diagnostic_for_call_node(
                node,
                &Diagnostics::No_overload_expects_0_arguments_but_overloads_do_exist_that_expect_either_1_or_2_arguments,
                Some(vec![
                    args.len().to_string(),
                    enum_unwrapped!(max_below, [UsizeOrNegativeInfinity, Usize]).to_string(),
                    min_above.to_string()
                ])
            )
            } else if args.len() < min {
                let diagnostic = self.get_diagnostic_for_call_node(
                    node,
                    error,
                    Some(vec![parameter_range, args.len().to_string()]),
                );
                let parameter = closest_signature
                    .as_ref()
                    .and_then(|closest_signature| closest_signature.declaration.as_ref())
                    .and_then(|closest_signature_declaration| {
                        closest_signature_declaration
                            .as_signature_declaration()
                            .parameters()
                            .get(
                                if closest_signature
                                    .as_ref()
                                    .unwrap()
                                    .maybe_this_parameter()
                                    .is_some()
                                {
                                    args.len() + 1
                                } else {
                                    args.len()
                                },
                            )
                            .cloned()
                    });
                if let Some(parameter) = parameter.as_ref() {
                    let parameter_error = create_diagnostic_for_node(
                        parameter,
                        if is_binding_pattern(parameter.as_named_declaration().maybe_name()) {
                            &*Diagnostics::An_argument_matching_this_binding_pattern_was_not_provided
                        } else if is_rest_parameter(parameter) {
                            &*Diagnostics::Arguments_for_the_rest_parameter_0_were_not_provided
                        } else {
                            &*Diagnostics::An_argument_for_0_was_not_provided
                        },
                        if parameter.as_named_declaration().maybe_name().is_none() {
                            Some(vec![args.len().to_string()])
                        } else if !is_binding_pattern(parameter.as_named_declaration().maybe_name())
                        {
                            Some(vec![id_text(&get_first_identifier(
                                &parameter.as_named_declaration().name(),
                            ))
                            .to_owned()])
                        } else {
                            None
                        },
                    );
                    add_related_info(&diagnostic, vec![Gc::new(parameter_error.into())]);
                }
                diagnostic
            } else {
                let error_span = factory.with(|factory_| {
                    factory_.create_node_array(
                        Some(match max {
                            UsizeOrNegativeInfinity::NegativeInfinity => args.to_owned(),
                            UsizeOrNegativeInfinity::Usize(max) => args[max..].to_owned(),
                        }),
                        None,
                    )
                });
                let pos = first(&error_span).pos();
                let mut end = last(&error_span).end();
                if end == pos {
                    end += 1;
                }
                set_text_range_pos_end(&*error_span, pos, end);
                Gc::new(
                    create_diagnostic_for_node_array(
                        &get_source_file_of_node(node),
                        &error_span,
                        error,
                        Some(vec![parameter_range, args.len().to_string()]),
                    )
                    .into(),
                )
            },
        )
    }

    pub(super) fn get_type_argument_arity_error(
        &self,
        node: &Node, /*CallLikeExpression*/
        signatures: &[Gc<Signature>],
        type_arguments: &NodeArray, /*<TypeNode>*/
    ) -> Gc<Diagnostic> {
        let arg_count = type_arguments.len();
        if signatures.len() == 1 {
            let sig = &signatures[0];
            let min = self.get_min_type_argument_count(sig.maybe_type_parameters().as_deref());
            let max = length(sig.maybe_type_parameters().as_deref());
            return Gc::new(
                create_diagnostic_for_node_array(
                    &get_source_file_of_node(node),
                    type_arguments,
                    &Diagnostics::Expected_0_type_arguments_but_got_1,
                    Some(vec![
                        if min < max {
                            format!("{}-{}", min, max)
                        } else {
                            min.to_string()
                        },
                        arg_count.to_string(),
                    ]),
                )
                .into(),
            );
        }
        let mut below_arg_count = UsizeOrNegativeInfinity::NegativeInfinity;
        let mut above_arg_count = usize::MAX;
        for sig in signatures {
            let min = self.get_min_type_argument_count(sig.maybe_type_parameters().as_deref());
            let max = length(sig.maybe_type_parameters().as_deref());
            if min > arg_count {
                above_arg_count = cmp::min(above_arg_count, min);
            } else if max < arg_count {
                below_arg_count = UsizeOrNegativeInfinity::Usize(match below_arg_count {
                    UsizeOrNegativeInfinity::NegativeInfinity => max,
                    UsizeOrNegativeInfinity::Usize(below_arg_count) => {
                        cmp::max(below_arg_count, max)
                    }
                });
            }
        }
        if let UsizeOrNegativeInfinity::Usize(below_arg_count) = below_arg_count {
            if above_arg_count != usize::MAX {
                return Gc::new(
                    create_diagnostic_for_node_array(
                        &get_source_file_of_node(node),
                        type_arguments,
                        &Diagnostics::No_overload_expects_0_type_arguments_but_overloads_do_exist_that_expect_either_1_or_2_type_arguments,
                        Some(vec![
                            arg_count.to_string(),
                            below_arg_count.to_string(),
                            above_arg_count.to_string(),
                        ])
                    ).into()
                );
            }
        }
        Gc::new(
            create_diagnostic_for_node_array(
                &get_source_file_of_node(node),
                type_arguments,
                &Diagnostics::Expected_0_type_arguments_but_got_1,
                Some(vec![
                    match below_arg_count {
                        UsizeOrNegativeInfinity::NegativeInfinity => above_arg_count,
                        UsizeOrNegativeInfinity::Usize(below_arg_count) => below_arg_count,
                    }
                    .to_string(),
                    arg_count.to_string(),
                ]),
            )
            .into(),
        )
    }

    pub(super) fn resolve_call(
        &self,
        node: &Node, /*CallLikeExpression*/
        signatures: &[Gc<Signature>],
        candidates_out_array: Option<&mut Vec<Gc<Signature>>>,
        check_mode: CheckMode,
        call_chain_flags: SignatureFlags,
        fallback_error: Option<&'static DiagnosticMessage>,
    ) -> io::Result<Gc<Signature>> {
        let is_tagged_template = node.kind() == SyntaxKind::TaggedTemplateExpression;
        let is_decorator = node.kind() == SyntaxKind::Decorator;
        let is_jsx_opening_or_self_closing_element = is_jsx_opening_like_element(node);
        let report_errors = candidates_out_array.is_none() && self.produce_diagnostics;

        let mut type_arguments: Option<Gc<NodeArray> /*<TypeNode>*/> = None;

        if !is_decorator {
            type_arguments = node.as_has_type_arguments().maybe_type_arguments().clone();

            if is_tagged_template
                || is_jsx_opening_or_self_closing_element
                || node.as_has_expression().expression().kind() != SyntaxKind::SuperKeyword
            {
                try_maybe_for_each(
                    type_arguments.as_ref(),
                    |type_argument: &Gc<Node>, _| -> io::Result<Option<()>> {
                        self.check_source_element(Some(&**type_argument))?;
                        Ok(None)
                    },
                )?;
            }
        }

        let mut candidates_default = vec![];
        let candidates_out_array_is_some = candidates_out_array.is_some();
        let candidates = candidates_out_array.unwrap_or(&mut candidates_default);
        self.reorder_candidates(signatures, candidates, call_chain_flags)?;
        if candidates.is_empty() {
            if report_errors {
                self.diagnostics().add(self.get_diagnostic_for_call_node(
                    node,
                    &Diagnostics::Call_target_does_not_contain_any_signatures,
                    None,
                ));
            }
            return self.resolve_error_call(node);
        }

        let args = self.get_effective_call_arguments(node)?;

        let is_single_non_generic_candidate =
            candidates.len() == 1 && candidates[0].maybe_type_parameters().is_none();
        let mut arg_check_mode = if !is_decorator
            && !is_single_non_generic_candidate
            && try_some(
                Some(&*args),
                Some(|arg: &Gc<Node>| self.is_context_sensitive(arg)),
            )? {
            CheckMode::SkipContextSensitive
        } else {
            CheckMode::Normal
        };

        let mut candidates_for_argument_error: Option<Vec<Gc<Signature>>> = None;
        let mut candidate_for_argument_arity_error: Option<Gc<Signature>> = None;
        let mut candidate_for_type_argument_error: Option<Gc<Signature>> = None;
        let mut result: Option<Gc<Signature>> = None;

        let signature_help_trailing_comma = check_mode.intersects(CheckMode::IsForSignatureHelp)
            && node.kind() == SyntaxKind::CallExpression
            && node.as_call_expression().arguments.has_trailing_comma;

        if candidates.len() > 1 {
            result = self.choose_overload(
                &mut candidates_for_argument_error,
                &mut candidate_for_argument_arity_error,
                &mut candidate_for_type_argument_error,
                type_arguments.as_deref(),
                node,
                &args,
                &mut arg_check_mode,
                candidates,
                self.subtype_relation.clone(),
                is_single_non_generic_candidate,
                Some(signature_help_trailing_comma),
            )?;
        }
        if result.is_none() {
            result = self.choose_overload(
                &mut candidates_for_argument_error,
                &mut candidate_for_argument_arity_error,
                &mut candidate_for_type_argument_error,
                type_arguments.as_deref(),
                node,
                &args,
                &mut arg_check_mode,
                candidates,
                self.assignable_relation.clone(),
                is_single_non_generic_candidate,
                Some(signature_help_trailing_comma),
            )?;
        }
        if let Some(result) = result {
            return Ok(result);
        }

        if report_errors {
            if let Some(candidates_for_argument_error_present) =
                candidates_for_argument_error.clone()
            {
                if candidates_for_argument_error_present.len() == 1
                    || candidates_for_argument_error_present.len() > 3
                {
                    let last = candidates_for_argument_error_present
                        [candidates_for_argument_error_present.len() - 1]
                        .clone();
                    let mut chain: Option<DiagnosticMessageChain> = None;
                    if candidates_for_argument_error_present.len() > 3 {
                        chain = Some(chain_diagnostic_messages(
                            chain,
                            &Diagnostics::The_last_overload_gave_the_following_error,
                            None,
                        ));
                        chain = Some(chain_diagnostic_messages(
                            chain,
                            &Diagnostics::No_overload_matches_this_call,
                            None,
                        ));
                    }
                    let diags = self.get_signature_applicability_error(
                        node,
                        &args,
                        last.clone(),
                        self.assignable_relation.clone(),
                        CheckMode::Normal,
                        true,
                        Some(Gc::new(Box::new(ResolveCallContainingMessageChain::new(
                            chain.map(|chain| Rc::new(RefCell::new(chain))),
                        )))),
                    )?;
                    if let Some(diags) = diags.as_ref() {
                        for d in diags {
                            if let Some(last_declaration) = last.declaration.as_ref() {
                                if candidates_for_argument_error_present.len() > 3 {
                                    add_related_info(
                                        d,
                                        vec![Gc::new(
                                            create_diagnostic_for_node(
                                                last_declaration,
                                                &Diagnostics::The_last_overload_is_declared_here,
                                                None,
                                            )
                                            .into(),
                                        )],
                                    );
                                }
                            }
                            self.add_implementation_success_elaboration(
                                &mut candidates_for_argument_error,
                                &mut candidate_for_argument_arity_error,
                                &mut candidate_for_type_argument_error,
                                type_arguments.as_deref(),
                                node,
                                &args,
                                &mut arg_check_mode,
                                &last,
                                d,
                            )?;
                            self.diagnostics().add(d.clone());
                        }
                    } else {
                        Debug_.fail(Some("No error for last overload signature"));
                    }
                } else {
                    let mut all_diagnostics: Vec<
                        Vec<Gc<Diagnostic /*DiagnosticRelatedInformation*/>>,
                    > = vec![];
                    let mut max = 0;
                    let mut min = usize::MAX;
                    let mut min_index = 0;
                    let i: Rc<Cell<usize>> = Rc::new(Cell::new(0));
                    for c in &candidates_for_argument_error_present {
                        let chain = ResolveCallOverloadContainingMessageChain::new(
                            self.rc_wrapper(),
                            i.clone(),
                            candidates.len(),
                            c.clone(),
                        );
                        let diags = self.get_signature_applicability_error(
                            node,
                            &args,
                            c.clone(),
                            self.assignable_relation.clone(),
                            CheckMode::Normal,
                            true,
                            Some(Gc::new(Box::new(chain))),
                        )?;
                        if let Some(diags) = diags {
                            if diags.len() <= min {
                                min = diags.len();
                                min_index = i.get();
                            }
                            max = cmp::max(max, diags.len());
                            all_diagnostics.push(diags);
                        } else {
                            Debug_.fail(Some("No error for 3 or fewer overload signatures"));
                        }
                        i.set(i.get() + 1);
                    }

                    let diags = if max > 1 {
                        all_diagnostics[min_index].clone()
                    } else {
                        flatten(&all_diagnostics)
                    };
                    Debug_.assert(
                        !diags.is_empty(),
                        Some("No errors reported for 3 or fewer overload signatures"),
                    );
                    let chain = chain_diagnostic_messages_multiple(
                        map(&diags, |d: &Gc<Diagnostic>, _| match d.message_text() {
                            DiagnosticMessageText::String(d_message_text) => {
                                DiagnosticMessageChain::new(
                                    d_message_text.clone(),
                                    d.category(),
                                    d.code(),
                                    None,
                                )
                            }
                            DiagnosticMessageText::DiagnosticMessageChain(d_message_text) => {
                                d_message_text.clone()
                            }
                        }),
                        &Diagnostics::No_overload_matches_this_call,
                        None,
                    );
                    let related: Vec<Gc<DiagnosticRelatedInformation>> =
                        flat_map(Some(&*diags), |d: &Gc<Diagnostic>, _| {
                            d.maybe_related_information()
                                .clone()
                                .unwrap_or_else(|| vec![])
                        });
                    let diag: Gc<Diagnostic>;
                    if every(&diags, |d: &Gc<Diagnostic>, _| {
                        d.start() == diags[0].start()
                            && d.length() == diags[0].length()
                            && are_option_gcs_equal(
                                d.maybe_file().as_ref(),
                                diags[0].maybe_file().as_ref(),
                            )
                    }) {
                        diag = Gc::new(
                            BaseDiagnostic::new(
                                BaseDiagnosticRelatedInformation::new(
                                    chain.category,
                                    chain.code,
                                    diags[0].maybe_file(),
                                    Some(diags[0].start()),
                                    Some(diags[0].length()),
                                    chain,
                                ),
                                Some(related),
                            )
                            .into(),
                        );
                    } else {
                        diag = Gc::new(
                            create_diagnostic_for_node_from_message_chain(
                                node,
                                chain,
                                Some(related),
                            )
                            .into(),
                        );
                    }
                    self.add_implementation_success_elaboration(
                        &mut candidates_for_argument_error,
                        &mut candidate_for_argument_arity_error,
                        &mut candidate_for_type_argument_error,
                        type_arguments.as_deref(),
                        node,
                        &args,
                        &mut arg_check_mode,
                        &candidates_for_argument_error_present[0],
                        &diag,
                    )?;
                    self.diagnostics().add(diag);
                }
            } else if let Some(candidate_for_argument_arity_error) =
                candidate_for_argument_arity_error
            {
                self.diagnostics().add(self.get_argument_arity_error(
                    node,
                    &[candidate_for_argument_arity_error],
                    &args,
                )?);
            } else if let Some(candidate_for_type_argument_error) =
                candidate_for_type_argument_error.as_ref()
            {
                self.check_type_arguments(
                    candidate_for_type_argument_error,
                    node.as_has_type_arguments()
                        .maybe_type_arguments()
                        .as_ref()
                        .unwrap(),
                    true,
                    fallback_error,
                )?;
            } else {
                let signatures_with_correct_type_argument_arity =
                    filter(signatures, |s: &Gc<Signature>| {
                        self.has_correct_type_argument_arity(s, type_arguments.as_deref())
                    });
                if signatures_with_correct_type_argument_arity.is_empty() {
                    self.diagnostics().add(self.get_type_argument_arity_error(
                        node,
                        signatures,
                        type_arguments.as_ref().unwrap(),
                    ));
                } else if !is_decorator {
                    self.diagnostics().add(self.get_argument_arity_error(
                        node,
                        &signatures_with_correct_type_argument_arity,
                        &args,
                    )?);
                } else if let Some(fallback_error) = fallback_error {
                    self.diagnostics().add(self.get_diagnostic_for_call_node(
                        node,
                        fallback_error,
                        None,
                    ));
                }
            }
        }

        self.get_candidate_for_overload_failure(
            node,
            candidates,
            &args,
            candidates_out_array_is_some,
        )
    }

    pub(super) fn add_implementation_success_elaboration(
        &self,
        candidates_for_argument_error: &mut Option<Vec<Gc<Signature>>>,
        candidate_for_argument_arity_error: &mut Option<Gc<Signature>>,
        candidate_for_type_argument_error: &mut Option<Gc<Signature>>,
        type_arguments: Option<&NodeArray>,
        node: &Node,
        args: &[Gc<Node>],
        arg_check_mode: &mut CheckMode,
        failed: &Signature,
        diagnostic: &Diagnostic,
    ) -> io::Result<()> {
        let old_candidates_for_argument_error = candidates_for_argument_error.clone();
        let old_candidate_for_argument_arity_error = candidate_for_argument_arity_error.clone();
        let old_candidate_for_type_argument_error = candidate_for_type_argument_error.clone();

        let failed_signature_declarations = failed
            .declaration
            .as_ref()
            .and_then(|failed_declaration| failed_declaration.maybe_symbol())
            .and_then(|failed_declaration_symbol| {
                self.symbol(failed_declaration_symbol)
                    .maybe_declarations()
                    .clone()
            })
            .unwrap_or_else(|| vec![]);
        let is_overload = failed_signature_declarations.len() > 1;
        let impl_decl = if is_overload {
            find(&failed_signature_declarations, |d: &Gc<Node>, _| {
                is_function_like_declaration(d)
                    && node_is_present(d.as_function_like_declaration().maybe_body())
            })
        } else {
            None
        };
        if let Some(impl_decl) = impl_decl {
            let candidate = self.get_signature_from_declaration_(impl_decl)?;
            let is_single_non_generic_candidate = candidate.maybe_type_parameters().is_none();
            let mut candidates = vec![candidate];
            if self
                .choose_overload(
                    candidates_for_argument_error,
                    candidate_for_argument_arity_error,
                    candidate_for_type_argument_error,
                    type_arguments,
                    node,
                    args,
                    arg_check_mode,
                    &mut candidates,
                    self.assignable_relation.clone(),
                    is_single_non_generic_candidate,
                    None,
                )?
                .is_some()
            {
                add_related_info(
                    diagnostic,
                    vec![
                        Gc::new(
                            create_diagnostic_for_node(
                                impl_decl,
                                &Diagnostics::The_call_would_have_succeeded_against_this_implementation_but_implementation_signatures_of_overloads_are_not_externally_visible,
                                None,
                            ).into()
                        )
                    ]
                );
            }
        }

        *candidates_for_argument_error = old_candidates_for_argument_error;
        *candidate_for_argument_arity_error = old_candidate_for_argument_arity_error;
        *candidate_for_type_argument_error = old_candidate_for_type_argument_error;

        Ok(())
    }

    pub(super) fn choose_overload(
        &self,
        candidates_for_argument_error: &mut Option<Vec<Gc<Signature>>>,
        candidate_for_argument_arity_error: &mut Option<Gc<Signature>>,
        candidate_for_type_argument_error: &mut Option<Gc<Signature>>,
        type_arguments: Option<&NodeArray>,
        node: &Node,
        args: &[Gc<Node>],
        arg_check_mode: &mut CheckMode,
        candidates: &mut Vec<Gc<Signature>>,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        is_single_non_generic_candidate: bool,
        signature_help_trailing_comma: Option<bool>,
    ) -> io::Result<Option<Gc<Signature>>> {
        let signature_help_trailing_comma = signature_help_trailing_comma.unwrap_or(false);
        *candidates_for_argument_error = None;
        *candidate_for_argument_arity_error = None;
        *candidate_for_type_argument_error = None;

        if is_single_non_generic_candidate {
            let candidate = &candidates[0];
            if some(
                type_arguments.map(|type_arguments| &**type_arguments),
                Option::<fn(&Gc<Node>) -> bool>::None,
            ) || !self.has_correct_arity(
                node,
                args,
                candidate,
                Some(signature_help_trailing_comma),
            )? {
                return Ok(None);
            }
            if self
                .get_signature_applicability_error(
                    node,
                    args,
                    candidate.clone(),
                    relation.clone(),
                    CheckMode::Normal,
                    false,
                    None,
                )?
                .is_some()
            {
                *candidates_for_argument_error = Some(vec![candidate.clone()]);
                return Ok(None);
            }
            return Ok(Some(candidate.clone()));
        }

        for candidate_index in 0..candidates.len() {
            let candidate = &candidates[candidate_index];
            if !self.has_correct_type_argument_arity(candidate, type_arguments)
                || !self.has_correct_arity(
                    node,
                    args,
                    candidate,
                    Some(signature_help_trailing_comma),
                )?
            {
                continue;
            }

            let mut check_candidate: Gc<Signature>;
            let mut inference_context: Option<Gc<InferenceContext>> = None;

            if let Some(ref candidate_type_parameters) = candidate.maybe_type_parameters().clone() {
                let type_argument_types: Option<Vec<Id<Type>>>;
                if some(
                    type_arguments.map(|type_arguments| &**type_arguments),
                    Option::<fn(&Gc<Node>) -> bool>::None,
                ) {
                    type_argument_types =
                        self.check_type_arguments(candidate, type_arguments.unwrap(), false, None)?;
                    if type_argument_types.is_none() {
                        *candidate_for_type_argument_error = Some(candidate.clone());
                        continue;
                    }
                } else {
                    inference_context = Some(self.create_inference_context(
                        candidate_type_parameters,
                        Some(candidate.clone()),
                        if is_in_js_file(Some(node)) {
                            InferenceFlags::AnyDefault
                        } else {
                            InferenceFlags::None
                        },
                        None,
                    ));
                    type_argument_types = Some(self.infer_type_arguments(
                        node,
                        candidate.clone(),
                        args,
                        *arg_check_mode | CheckMode::SkipGenericFunctions,
                        inference_context.clone().unwrap(),
                    )?);
                    *arg_check_mode |= if inference_context
                        .as_ref()
                        .unwrap()
                        .flags()
                        .intersects(InferenceFlags::SkippedGenericFunction)
                    {
                        CheckMode::SkipGenericFunctions
                    } else {
                        CheckMode::Normal
                    };
                }
                check_candidate = self.get_signature_instantiation(
                    candidate.clone(),
                    type_argument_types.as_deref(),
                    is_in_js_file(candidate.declaration.as_deref()),
                    inference_context
                        .as_ref()
                        .and_then(|inference_context| {
                            inference_context.maybe_inferred_type_parameters().clone()
                        })
                        .as_deref(),
                )?;
                if self.get_non_array_rest_type(candidate)?.is_some()
                    && !self.has_correct_arity(
                        node,
                        args,
                        &check_candidate,
                        Some(signature_help_trailing_comma),
                    )?
                {
                    *candidate_for_argument_arity_error = Some(check_candidate.clone());
                    continue;
                }
            } else {
                check_candidate = candidate.clone();
            }
            if self
                .get_signature_applicability_error(
                    node,
                    args,
                    check_candidate.clone(),
                    relation.clone(),
                    *arg_check_mode,
                    false,
                    None,
                )?
                .is_some()
            {
                if candidates_for_argument_error.is_none() {
                    *candidates_for_argument_error = Some(vec![]);
                }
                candidates_for_argument_error
                    .as_mut()
                    .unwrap()
                    .push(check_candidate.clone());
                continue;
            }
            if *arg_check_mode != CheckMode::Normal {
                *arg_check_mode = CheckMode::Normal;
                if let Some(inference_context) = inference_context.as_ref() {
                    let type_argument_types = self.infer_type_arguments(
                        node,
                        candidate.clone(),
                        args,
                        *arg_check_mode,
                        inference_context.clone(),
                    )?;
                    check_candidate = self.get_signature_instantiation(
                        candidate.clone(),
                        Some(&type_argument_types),
                        is_in_js_file(candidate.declaration.as_deref()),
                        /*inferenceContext &&*/
                        inference_context
                            .maybe_inferred_type_parameters()
                            .as_deref(),
                    )?;
                    if self.get_non_array_rest_type(candidate)?.is_some()
                        && !self.has_correct_arity(
                            node,
                            args,
                            &check_candidate,
                            Some(signature_help_trailing_comma),
                        )?
                    {
                        *candidate_for_argument_arity_error = Some(check_candidate.clone());
                        continue;
                    }
                }
                if self
                    .get_signature_applicability_error(
                        node,
                        args,
                        check_candidate.clone(),
                        relation.clone(),
                        *arg_check_mode,
                        false,
                        None,
                    )?
                    .is_some()
                {
                    if candidates_for_argument_error.is_none() {
                        *candidates_for_argument_error = Some(vec![]);
                    }
                    candidates_for_argument_error
                        .as_mut()
                        .unwrap()
                        .push(check_candidate.clone());
                    continue;
                }
            }
            candidates[candidate_index] = check_candidate.clone();
            return Ok(Some(check_candidate));
        }

        Ok(None)
    }
}

pub(super) struct GetDiagnosticSpanForCallNodeReturn {
    pub start: isize,
    pub length: isize,
    pub source_file: Gc<Node /*SourceFile*/>,
}

#[derive(Trace, Finalize)]
pub(super) struct ResolveCallContainingMessageChain {
    #[unsafe_ignore_trace]
    chain: Option<Rc<RefCell<DiagnosticMessageChain>>>,
}

impl ResolveCallContainingMessageChain {
    pub fn new(chain: Option<Rc<RefCell<DiagnosticMessageChain>>>) -> Self {
        Self { chain }
    }
}

impl CheckTypeContainingMessageChain for ResolveCallContainingMessageChain {
    fn get(&self) -> io::Result<Option<Rc<RefCell<DiagnosticMessageChain>>>> {
        Ok(self.chain.clone())
    }
}

#[derive(Trace, Finalize)]
struct ResolveCallOverloadContainingMessageChain {
    type_checker: Gc<TypeChecker>,
    #[unsafe_ignore_trace]
    i: Rc<Cell<usize>>,
    candidates_len: usize,
    c: Gc<Signature>,
}

impl ResolveCallOverloadContainingMessageChain {
    pub fn new(
        type_checker: Gc<TypeChecker>,
        i: Rc<Cell<usize>>,
        candidates_len: usize,
        c: Gc<Signature>,
    ) -> Self {
        Self {
            type_checker,
            i,
            candidates_len,
            c,
        }
    }
}

impl CheckTypeContainingMessageChain for ResolveCallOverloadContainingMessageChain {
    fn get(&self) -> io::Result<Option<Rc<RefCell<DiagnosticMessageChain>>>> {
        Ok(Some(Rc::new(RefCell::new(chain_diagnostic_messages(
            None,
            &Diagnostics::Overload_0_of_1_2_gave_the_following_error,
            Some(vec![
                (self.i.get() + 1).to_string(),
                self.candidates_len.to_string(),
                self.type_checker.signature_to_string_(
                    self.c.clone(),
                    Option::<&Node>::None,
                    None,
                    None,
                    None,
                )?,
            ]),
        )))))
    }
}
