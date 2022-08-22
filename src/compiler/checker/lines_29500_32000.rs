#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::cmp;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, CheckTypeContainingMessageChain,
    CheckTypeErrorOutputContainer, MinArgumentCountFlags, TypeFacts, WideningKind,
};
use crate::{
    add_related_info, are_option_rcs_equal, chain_diagnostic_messages,
    chain_diagnostic_messages_multiple, create_diagnostic_for_node,
    create_diagnostic_for_node_array, create_diagnostic_for_node_from_message_chain,
    create_file_diagnostic, every, factory, filter, first, flat_map, flatten, for_each,
    get_error_span_for_node, get_first_identifier, get_source_file_of_node, id_text,
    is_access_expression, is_binding_pattern, is_call_expression,
    is_function_expression_or_arrow_function, is_identifier, is_import_call,
    is_jsx_opening_element, is_jsx_opening_like_element, is_new_expression, is_parameter,
    is_property_access_expression, is_rest_parameter, last, length, map, maybe_for_each,
    parse_base_node_factory, parse_node_factory, set_parent, set_text_range,
    set_text_range_pos_end, skip_outer_expressions, some, BaseDiagnostic,
    BaseDiagnosticRelatedInformation, Debug_, Diagnostic, DiagnosticInterface, DiagnosticMessage,
    DiagnosticMessageChain, DiagnosticMessageText, DiagnosticRelatedInformation,
    DiagnosticRelatedInformationInterface, Diagnostics, ElementFlags, FunctionFlags, NodeArray,
    ReadonlyTextRange, RelationComparisonResult, ScriptTarget, Signature, SignatureFlags,
    SymbolFlags, UnionReduction, UsizeOrNegativeInfinity, __String, get_function_flags,
    has_initializer, Node, NodeInterface, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeInterface,
};
use local_macros::enum_unwrapped;

impl TypeChecker {
    pub(super) fn maybe_add_missing_await_info<TErrorNode: Borrow<Node>>(
        &self,
        report_errors: bool,
        error_output_container: Rc<dyn CheckTypeErrorOutputContainer>,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        error_node: Option<TErrorNode>,
        source: &Type,
        target: &Type,
    ) {
        if let Some(error_node) = error_node {
            let error_node = error_node.borrow();
            if report_errors && error_output_container.errors_len() > 0 {
                if self
                    .get_awaited_type_of_promise(target, Option::<&Node>::None, None, None)
                    .is_some()
                {
                    return;
                }
                let awaited_type_of_source =
                    self.get_awaited_type_of_promise(source, Option::<&Node>::None, None, None);
                if matches!(
                    awaited_type_of_source.as_ref(),
                    Some(awaited_type_of_source) if self.is_type_related_to(
                        awaited_type_of_source,
                        target,
                        relation.clone()
                    )
                ) {
                    add_related_info(
                        &error_output_container.get_error(0).unwrap(),
                        vec![Rc::new(
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
    }

    pub(super) fn get_this_argument_of_call(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> Option<Rc<Node /*LeftHandSideExpression*/>> {
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
        type_: &Type,
        is_spread: Option<bool>,
        tuple_name_source: Option<
            TTupleNameSource, /*ParameterDeclaration | NamedTupleMember*/
        >,
    ) -> Rc<Node> {
        let result: Rc<Node> = parse_node_factory.with(|parse_node_factory_| {
            parse_base_node_factory.with(|parse_base_node_factory_| {
                parse_node_factory_
                    .create_synthetic_expression(
                        parse_base_node_factory_,
                        type_.type_wrapper(),
                        is_spread,
                        tuple_name_source
                            .map(|tuple_name_source| tuple_name_source.borrow().node_wrapper()),
                    )
                    .into()
            })
        });
        set_text_range(&*result, Some(parent));
        set_parent(&result, Some(parent));
        result
    }

    pub(super) fn get_effective_call_arguments(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> Vec<Rc<Node /*Expression*/>> {
        if node.kind() == SyntaxKind::TaggedTemplateExpression {
            let template = &node.as_tagged_template_expression().template;
            let mut args: Vec<Rc<Node /*Expression*/>> = vec![self.create_synthetic_expression(
                template,
                &self.get_global_template_strings_array_type(),
                None,
                Option::<&Node>::None,
            )];
            if template.kind() == SyntaxKind::TemplateExpression {
                for_each(
                    &template.as_template_expression().template_spans,
                    |span: &Rc<Node>, _| -> Option<()> {
                        args.push(span.as_template_span().expression.clone());
                        None
                    },
                );
            }
            return args;
        }
        if node.kind() == SyntaxKind::Decorator {
            return self.get_effective_decorator_arguments(node);
        }
        if is_jsx_opening_like_element(node) {
            return if !node
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
            };
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
                        self.check_expression(&arg.as_spread_element().expression, None, None)
                    } else {
                        self.check_expression_cached(&arg.as_spread_element().expression, None)
                    })
                } else {
                    None
                };
                if let Some(spread_type) = spread_type
                    .as_ref()
                    .filter(|spread_type| self.is_tuple_type(spread_type))
                {
                    let spread_type_target_as_tuple_type =
                        spread_type.as_type_reference().target.as_tuple_type();
                    for_each(
                        &self.get_type_arguments(spread_type),
                        |t: &Rc<Type>, i| -> Option<()> {
                            let flags = spread_type_target_as_tuple_type.element_flags[i];
                            let synthetic_arg = self.create_synthetic_expression(
                                arg,
                                &*if flags.intersects(ElementFlags::Rest) {
                                    self.create_array_type(t, None)
                                } else {
                                    t.clone()
                                },
                                Some(flags.intersects(ElementFlags::Variable)),
                                spread_type_target_as_tuple_type
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
            return effective_args;
        }
        args
    }

    pub(super) fn get_effective_decorator_arguments(
        &self,
        node: &Node, /*Decorator*/
    ) -> Vec<Rc<Node /*Expression*/>> {
        let parent = node.parent();
        let expr = &node.as_decorator().expression;
        match parent.kind() {
            SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => {
                vec![self.create_synthetic_expression(
                    expr,
                    &self.get_type_of_symbol(&self.get_symbol_of_node(&parent).unwrap()),
                    None,
                    Option::<&Node>::None,
                )]
            }
            SyntaxKind::Parameter => {
                let func = parent.parent();
                vec![
                    self.create_synthetic_expression(
                        expr,
                        &*if parent.parent().kind() == SyntaxKind::Constructor {
                            self.get_type_of_symbol(&self.get_symbol_of_node(&func).unwrap())
                        } else {
                            self.error_type()
                        },
                        None,
                        Option::<&Node>::None,
                    ),
                    self.create_synthetic_expression(
                        expr,
                        &self.any_type(),
                        None,
                        Option::<&Node>::None,
                    ),
                    self.create_synthetic_expression(
                        expr,
                        &self.number_type(),
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
                        &self.get_parent_type_of_class_element(&parent),
                        None,
                        Option::<&Node>::None,
                    ),
                    self.create_synthetic_expression(
                        expr,
                        &self.get_class_element_property_key_type(&parent),
                        None,
                        Option::<&Node>::None,
                    ),
                    self.create_synthetic_expression(
                        expr,
                        &*if has_prop_desc {
                            self.create_typed_property_descriptor_type(
                                &self.get_type_of_node(&parent),
                            )
                        } else {
                            self.any_type()
                        },
                        None,
                        Option::<&Node>::None,
                    ),
                ]
            }
            _ => Debug_.fail(None),
        }
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
        let source_file = get_source_file_of_node(Some(node)).unwrap();

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
    ) -> Rc<Diagnostic /*DiagnosticWithLocation*/> {
        if is_call_expression(node) {
            let GetDiagnosticSpanForCallNodeReturn {
                source_file,
                start,
                length,
            } = self.get_diagnostic_span_for_call_node(node, None);
            Rc::new(create_file_diagnostic(&source_file, start, length, message, args).into())
        } else {
            Rc::new(create_diagnostic_for_node(node, message, args).into())
        }
    }

    pub(super) fn is_promise_resolve_arity_error(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> bool {
        if !is_call_expression(node) {
            return false;
        }
        let node_as_call_expression = node.as_call_expression();
        if !is_identifier(&node_as_call_expression.expression) {
            return false;
        }

        let symbol = self.resolve_name_(
            Some(&*node_as_call_expression.expression),
            &node_as_call_expression
                .expression
                .as_identifier()
                .escaped_text,
            SymbolFlags::Value,
            None,
            Option::<Rc<Node>>::None,
            false,
            None,
        );
        let decl = symbol
            .as_ref()
            .and_then(|symbol| symbol.maybe_value_declaration());
        if decl.is_none() {
            return false;
        }
        let decl = decl.unwrap();
        if !is_parameter(&decl)
            || !is_function_expression_or_arrow_function(&decl.parent())
            || !is_new_expression(&decl.parent().parent())
            || is_identifier(&decl.parent().parent().as_new_expression().expression)
        {
            return false;
        }

        let global_promise_symbol = self.get_global_promise_constructor_symbol(false);
        if global_promise_symbol.is_none() {
            return false;
        }
        let global_promise_symbol = global_promise_symbol.unwrap();

        let constructor_symbol = self.get_symbol_at_location_(
            &decl.parent().parent().as_new_expression().expression,
            Some(true),
        );
        matches!(
            constructor_symbol.as_ref(),
            Some(constructor_symbol) if Rc::ptr_eq(
                constructor_symbol,
                &global_promise_symbol
            )
        )
    }

    pub(super) fn get_argument_arity_error(
        &self,
        node: &Node, /*CallLikeExpression*/
        signatures: &[Rc<Signature>],
        args: &[Rc<Node /*Expression*/>],
    ) -> Rc<Diagnostic> {
        let spread_index = self.get_spread_argument_index(args);
        if let Some(spread_index) = spread_index {
            return Rc::new(
                create_diagnostic_for_node(
                    &args[spread_index],
                    &Diagnostics::A_spread_argument_must_either_have_a_tuple_type_or_be_passed_to_a_rest_parameter,
                    None,
                ).into()
            );
        }
        let mut min = usize::MAX;
        let mut max = UsizeOrNegativeInfinity::NegativeInfinity;
        let mut max_below = UsizeOrNegativeInfinity::NegativeInfinity;
        let mut min_above = usize::MAX;

        let mut closest_signature: Option<Rc<Signature>> = None;
        for sig in signatures {
            let min_parameter = self.get_min_argument_count(sig, None);
            let max_parameter = self.get_parameter_count(sig);
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
        let has_rest_parameter = some(
            Some(signatures),
            Some(|signature: &Rc<Signature>| self.has_effective_rest_parameter(signature)),
        );
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
            && self.is_promise_resolve_arity_error(node)
        {
            &*Diagnostics::Expected_0_arguments_but_got_1_Did_you_forget_to_include_void_in_your_type_argument_to_Promise
        } else {
            &*Diagnostics::Expected_0_arguments_but_got_1
        };
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
                        .as_has_arguments()
                        .maybe_arguments()
                })
                .and_then(|closest_signature_declaration_arguments| {
                    closest_signature_declaration_arguments
                        .get(
                            if closest_signature.as_ref().unwrap().this_parameter.is_some() {
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
                    } else if !is_binding_pattern(parameter.as_named_declaration().maybe_name()) {
                        Some(vec![id_text(&get_first_identifier(
                            &parameter.as_named_declaration().name(),
                        ))])
                    } else {
                        None
                    },
                );
                add_related_info(&diagnostic, vec![Rc::new(parameter_error.into())]);
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
            set_text_range_pos_end(&error_span, pos, end);
            Rc::new(
                create_diagnostic_for_node_array(
                    &get_source_file_of_node(Some(node)).unwrap(),
                    &error_span,
                    error,
                    Some(vec![parameter_range, args.len().to_string()]),
                )
                .into(),
            )
        }
    }

    pub(super) fn get_type_argument_arity_error(
        &self,
        node: &Node, /*CallLikeExpression*/
        signatures: &[Rc<Signature>],
        type_arguments: &NodeArray, /*<TypeNode>*/
    ) -> Rc<Diagnostic> {
        let arg_count = type_arguments.len();
        if signatures.len() == 1 {
            let sig = &signatures[0];
            let min = self.get_min_type_argument_count(sig.type_parameters.as_deref());
            let max = length(sig.type_parameters.as_deref());
            return Rc::new(
                create_diagnostic_for_node_array(
                    &get_source_file_of_node(Some(node)).unwrap(),
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
            let min = self.get_min_type_argument_count(sig.type_parameters.as_deref());
            let max = length(sig.type_parameters.as_deref());
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
                return Rc::new(
                    create_diagnostic_for_node_array(
                        &get_source_file_of_node(Some(node)).unwrap(),
                        type_arguments,
                        &Diagnostics::No_overload_expects_0_arguments_but_overloads_do_exist_that_expect_either_1_or_2_arguments,
                        Some(vec![
                            arg_count.to_string(),
                            below_arg_count.to_string(),
                            above_arg_count.to_string(),
                        ])
                    ).into()
                );
            }
        }
        Rc::new(
            create_diagnostic_for_node_array(
                &get_source_file_of_node(Some(node)).unwrap(),
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
        signatures: &[Rc<Signature>],
        candidates_out_array: Option<&mut Vec<Rc<Signature>>>,
        check_mode: CheckMode,
        call_chain_flags: SignatureFlags,
        fallback_error: Option<&'static DiagnosticMessage>,
    ) -> Rc<Signature> {
        let is_tagged_template = node.kind() == SyntaxKind::TaggedTemplateExpression;
        let is_decorator = node.kind() == SyntaxKind::Decorator;
        let is_jsx_opening_or_self_closing_element = is_jsx_opening_like_element(node);
        let report_errors = candidates_out_array.is_none() && self.produce_diagnostics;

        let mut type_arguments: Option<&NodeArray /*<TypeNode>*/> = None;

        if !is_decorator {
            type_arguments = node.as_has_type_arguments().maybe_type_arguments();

            if is_tagged_template
                || is_jsx_opening_or_self_closing_element
                || node.as_has_expression().expression().kind() != SyntaxKind::SuperKeyword
            {
                maybe_for_each(
                    type_arguments,
                    |type_argument: &Rc<Node>, _| -> Option<()> {
                        self.check_source_element(Some(&**type_argument));
                        None
                    },
                );
            }
        }

        let mut candidates_default = vec![];
        let candidates_out_array_is_some = candidates_out_array.is_some();
        let candidates = candidates_out_array.unwrap_or(&mut candidates_default);
        self.reorder_candidates(signatures, candidates, call_chain_flags);
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

        let args = self.get_effective_call_arguments(node);

        let is_single_non_generic_candidate =
            candidates.len() == 1 && candidates[0].type_parameters.is_none();
        let arg_check_mode = if !is_decorator
            && !is_single_non_generic_candidate
            && some(
                Some(&*args),
                Some(|arg: &Rc<Node>| self.is_context_sensitive(arg)),
            ) {
            CheckMode::SkipContextSensitive
        } else {
            CheckMode::Normal
        };

        let mut candidates_for_argument_error: Option<Vec<Rc<Signature>>> = None;
        let mut candidate_for_argument_arity_error: Option<Rc<Signature>> = None;
        let mut candidate_for_type_argument_error: Option<Rc<Signature>> = None;
        let mut result: Option<Rc<Signature>> = None;

        let signature_help_trailing_comma = check_mode.intersects(CheckMode::IsForSignatureHelp)
            && node.kind() == SyntaxKind::CallExpression
            && node.as_call_expression().arguments.has_trailing_comma;

        if candidates.len() > 1 {
            result = self.choose_overload(
                &candidates,
                self.subtype_relation.clone(),
                is_single_non_generic_candidate,
                Some(signature_help_trailing_comma),
            );
        }
        if result.is_none() {
            result = self.choose_overload(
                &candidates,
                self.assignable_relation.clone(),
                is_single_non_generic_candidate,
                Some(signature_help_trailing_comma),
            );
        }
        if let Some(result) = result {
            return result;
        }

        if report_errors {
            if let Some(candidates_for_argument_error) = candidates_for_argument_error.as_ref() {
                if candidates_for_argument_error.len() == 1
                    || candidates_for_argument_error.len() > 3
                {
                    let last =
                        &candidates_for_argument_error[candidates_for_argument_error.len() - 1];
                    let mut chain: Option<DiagnosticMessageChain> = None;
                    if candidates_for_argument_error.len() > 3 {
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
                        Some(Rc::new(ResolveCallContainingMessageChain::new(
                            chain.map(|chain| Rc::new(RefCell::new(chain))),
                        ))),
                    );
                    if let Some(diags) = diags.as_ref() {
                        for d in diags {
                            if let Some(last_declaration) = last.declaration.as_ref() {
                                if candidates_for_argument_error.len() > 3 {
                                    add_related_info(
                                        d,
                                        vec![Rc::new(
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
                            self.add_implementation_success_elaboration(last, d.clone());
                            self.diagnostics().add(d.clone());
                        }
                    } else {
                        Debug_.fail(Some("No error for last overload signature"));
                    }
                } else {
                    let mut all_diagnostics: Vec<
                        Vec<Rc<Diagnostic /*DiagnosticRelatedInformation*/>>,
                    > = vec![];
                    let mut max = 0;
                    let mut min = usize::MAX;
                    let mut min_index = 0;
                    let i: Rc<Cell<usize>> = Rc::new(Cell::new(0));
                    for c in candidates_for_argument_error {
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
                            Some(Rc::new(chain)),
                        );
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
                        map(&diags, |d: &Rc<Diagnostic>, _| match d.message_text() {
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
                    let related: Vec<Rc<DiagnosticRelatedInformation>> =
                        flat_map(Some(&*diags), |d: &Rc<Diagnostic>, _| {
                            d.related_information().clone().unwrap_or_else(|| vec![])
                        });
                    let diag: Rc<Diagnostic>;
                    if every(&diags, |d: &Rc<Diagnostic>, _| {
                        d.start() == diags[0].start()
                            && d.length() == diags[0].length()
                            && are_option_rcs_equal(
                                d.maybe_file().as_ref(),
                                diags[0].maybe_file().as_ref(),
                            )
                    }) {
                        diag = Rc::new(
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
                        diag = Rc::new(
                            create_diagnostic_for_node_from_message_chain(
                                node,
                                chain,
                                Some(related),
                            )
                            .into(),
                        );
                    }
                    self.add_implementation_success_elaboration(
                        &candidates_for_argument_error[0],
                        diag.clone(),
                    );
                    self.diagnostics().add(diag);
                }
            } else if let Some(candidate_for_argument_arity_error) =
                candidate_for_argument_arity_error
            {
                self.diagnostics().add(self.get_argument_arity_error(
                    node,
                    &[candidate_for_argument_arity_error],
                    &args,
                ));
            } else if let Some(candidate_for_type_argument_error) =
                candidate_for_type_argument_error.as_ref()
            {
                self.check_type_arguments(
                    candidate_for_type_argument_error,
                    node.as_has_type_arguments().maybe_type_arguments().unwrap(),
                    true,
                    fallback_error,
                );
            } else {
                let signatures_with_correct_type_argument_arity =
                    filter(signatures, |s: &Rc<Signature>| {
                        self.has_correct_type_argument_arity(s, type_arguments)
                    });
                if signatures_with_correct_type_argument_arity.is_empty() {
                    self.diagnostics().add(self.get_type_argument_arity_error(
                        node,
                        signatures,
                        type_arguments.unwrap(),
                    ));
                } else if !is_decorator {
                    self.diagnostics().add(self.get_argument_arity_error(
                        node,
                        &signatures_with_correct_type_argument_arity,
                        &args,
                    ));
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
        failed: &Signature,
        diagnostic: Rc<Diagnostic>,
    ) {
        unimplemented!()
    }

    pub(super) fn choose_overload(
        &self,
        candidates: &[Rc<Signature>],
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        is_single_non_generic_candidate: bool,
        signature_help_trailing_comma: Option<bool>,
    ) -> Option<Rc<Signature>> {
        let signature_help_trailing_comma = signature_help_trailing_comma.unwrap_or(false);
        unimplemented!()
    }

    pub(super) fn get_candidate_for_overload_failure(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates: &[Rc<Signature>],
        args: &[Rc<Node /*Expression*/>],
        has_candidates_out_array: bool,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn create_signature_for_jsx_intrinsic(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
        result: &Type,
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

pub(super) struct GetDiagnosticSpanForCallNodeReturn {
    pub start: isize,
    pub length: isize,
    pub source_file: Rc<Node /*SourceFile*/>,
}

struct ResolveCallContainingMessageChain {
    chain: Option<Rc<RefCell<DiagnosticMessageChain>>>,
}

impl ResolveCallContainingMessageChain {
    pub fn new(chain: Option<Rc<RefCell<DiagnosticMessageChain>>>) -> Self {
        Self { chain }
    }
}

impl CheckTypeContainingMessageChain for ResolveCallContainingMessageChain {
    fn get(&self) -> Option<Rc<RefCell<DiagnosticMessageChain>>> {
        self.chain.clone()
    }
}

struct ResolveCallOverloadContainingMessageChain {
    type_checker: Rc<TypeChecker>,
    i: Rc<Cell<usize>>,
    candidates_len: usize,
    c: Rc<Signature>,
}

impl ResolveCallOverloadContainingMessageChain {
    pub fn new(
        type_checker: Rc<TypeChecker>,
        i: Rc<Cell<usize>>,
        candidates_len: usize,
        c: Rc<Signature>,
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
    fn get(&self) -> Option<Rc<RefCell<DiagnosticMessageChain>>> {
        Some(Rc::new(RefCell::new(chain_diagnostic_messages(
            None,
            &Diagnostics::Overload_0_of_1_2_gave_the_following_error,
            Some(vec![
                (self.i.get() + 1).to_string(),
                self.candidates_len.to_string(),
                self.type_checker.signature_to_string_(
                    &self.c,
                    Option::<&Node>::None,
                    None,
                    None,
                    None,
                ),
            ]),
        ))))
    }
}
