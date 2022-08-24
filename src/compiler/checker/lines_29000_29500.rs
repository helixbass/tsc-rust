#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    CheckMode, CheckTypeContainingMessageChain, CheckTypeErrorOutputContainer,
    CheckTypeErrorOutputContainerConcrete, IterationUse,
};
use crate::{
    add_related_info, chain_diagnostic_messages, create_diagnostic_for_node, entity_name_to_string,
    find, find_index, is_in_js_file, is_jsx_opening_element, is_jsx_opening_like_element,
    is_jsx_self_closing_element, is_optional_chain, is_optional_chain_root, last, length, map,
    maybe_every, node_is_missing, set_text_range_pos_end, some, AccessFlags, ContextFlags, Debug_,
    Diagnostic, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, ElementFlags,
    InferenceContext, InferenceFlags, InferenceInfo, InferencePriority, JsxReferenceKind, Node,
    NodeArray, NodeInterface, Number, ReadonlyTextRange, RelationComparisonResult, Signature,
    SignatureKind, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker,
    TypeComparer, TypeFlags, TypeInterface, TypeMapper,
};

impl TypeChecker {
    pub(super) fn is_spread_argument<TArg: Borrow<Node>>(&self, arg: Option<TArg>) -> bool {
        if arg.is_none() {
            return false;
        }
        let arg = arg.unwrap();
        let arg = arg.borrow();
        arg.kind() == SyntaxKind::SpreadElement
            || arg.kind() == SyntaxKind::SyntheticExpression
                && arg.as_synthetic_expression().is_spread
    }

    pub(super) fn get_spread_argument_index(
        &self,
        args: &[Rc<Node /*Expression*/>],
    ) -> Option<usize> {
        find_index(
            args,
            |arg: &Rc<Node>, _| self.is_spread_argument(Some(&**arg)),
            None,
        )
    }

    pub(super) fn accepts_void(&self, t: &Type) -> bool {
        t.flags().intersects(TypeFlags::Void)
    }

    pub(super) fn accepts_void_undefined_unknown_or_any(&self, t: &Type) -> bool {
        t.flags().intersects(
            TypeFlags::Void | TypeFlags::Undefined | TypeFlags::Unknown | TypeFlags::Any,
        )
    }

    pub(super) fn has_correct_arity(
        &self,
        node: &Node, /*CallLikeExpression*/
        args: &[Rc<Node /*Expression*/>],
        signature: &Signature,
        signature_help_trailing_comma: Option<bool>,
    ) -> bool {
        let signature_help_trailing_comma = signature_help_trailing_comma.unwrap_or(false);
        let arg_count: usize;
        let mut call_is_incomplete = false;
        let mut effective_parameter_count = self.get_parameter_count(signature);
        let mut effective_minimum_arguments = self.get_min_argument_count(signature, None);

        if node.kind() == SyntaxKind::TaggedTemplateExpression {
            arg_count = args.len();
            let node_as_tagged_template_expression = node.as_tagged_template_expression();
            if node_as_tagged_template_expression.template.kind() == SyntaxKind::TemplateExpression
            {
                let last_span = last(
                    &node_as_tagged_template_expression
                        .template
                        .as_template_expression()
                        .template_spans,
                );
                let last_span_as_template_span = last_span.as_template_span();
                call_is_incomplete = node_is_missing(Some(&*last_span_as_template_span.literal))
                    || last_span_as_template_span
                        .literal
                        .as_literal_like_node()
                        .is_unterminated()
                        == Some(true);
            } else {
                let template_literal = &node_as_tagged_template_expression.template;
                Debug_.assert(
                    template_literal.kind() == SyntaxKind::NoSubstitutionTemplateLiteral,
                    None,
                );
                call_is_incomplete =
                    template_literal.as_literal_like_node().is_unterminated() == Some(true);
            }
        } else if node.kind() == SyntaxKind::Decorator {
            arg_count = self.get_decorator_argument_count(node, signature);
        } else if is_jsx_opening_like_element(node) {
            call_is_incomplete =
                node.as_jsx_opening_like_element().attributes().end() == node.end();
            if call_is_incomplete {
                return true;
            }
            arg_count = if effective_minimum_arguments == 0 {
                args.len()
            } else {
                1
            };
            effective_parameter_count = if args.is_empty() {
                effective_parameter_count
            } else {
                1
            };
            effective_minimum_arguments = cmp::min(effective_minimum_arguments, 1);
        } else if node.as_has_arguments().maybe_arguments().is_none() {
            Debug_.assert(node.kind() == SyntaxKind::NewExpression, None);
            return self.get_min_argument_count(signature, None) == 0;
        } else {
            arg_count = if signature_help_trailing_comma {
                args.len() + 1
            } else {
                args.len()
            };

            call_is_incomplete =
                node.as_has_arguments().maybe_arguments().unwrap().end() == node.end();

            let spread_arg_index = self.get_spread_argument_index(args);
            if let Some(spread_arg_index) = spread_arg_index {
                return spread_arg_index >= self.get_min_argument_count(signature, None)
                    && (self.has_effective_rest_parameter(signature)
                        || spread_arg_index < self.get_parameter_count(signature));
            }
        }

        if !self.has_effective_rest_parameter(signature) && arg_count > effective_parameter_count {
            return false;
        }

        if call_is_incomplete || arg_count >= effective_minimum_arguments {
            return true;
        }
        for i in arg_count..effective_minimum_arguments {
            let type_ = self.get_type_at_position(signature, i);
            if self
                .filter_type(&type_, |type_: &Type| {
                    if is_in_js_file(Some(node)) && !self.strict_null_checks {
                        self.accepts_void_undefined_unknown_or_any(type_)
                    } else {
                        self.accepts_void(type_)
                    }
                })
                .flags()
                .intersects(TypeFlags::Never)
            {
                return false;
            }
        }
        true
    }

    pub(super) fn has_correct_type_argument_arity(
        &self,
        signature: &Signature,
        type_arguments: Option<&NodeArray /*<TypeNode>*/>,
    ) -> bool {
        let num_type_parameters = length(signature.maybe_type_parameters().as_deref());
        let min_type_argument_count =
            self.get_min_type_argument_count(signature.maybe_type_parameters().as_deref());
        !some(
            type_arguments.map(|type_arguments| &**type_arguments),
            Option::<fn(&Rc<Node>) -> bool>::None,
        ) || {
            let type_arguments = type_arguments.unwrap();
            type_arguments.len() >= min_type_argument_count
                && type_arguments.len() <= num_type_parameters
        }
    }

    pub(super) fn get_single_call_signature(&self, type_: &Type) -> Option<Rc<Signature>> {
        self.get_single_signature(type_, SignatureKind::Call, false)
    }

    pub(super) fn get_single_call_or_construct_signature(
        &self,
        type_: &Type,
    ) -> Option<Rc<Signature>> {
        self.get_single_signature(type_, SignatureKind::Call, false)
            .or_else(|| self.get_single_signature(type_, SignatureKind::Construct, false))
    }

    pub(super) fn get_single_signature(
        &self,
        type_: &Type,
        kind: SignatureKind,
        allow_members: bool,
    ) -> Option<Rc<Signature>> {
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_);
            let resolved_as_resolved_type = resolved.as_resolved_type();
            if allow_members
                || resolved_as_resolved_type.properties().is_empty()
                    && resolved_as_resolved_type.index_infos().is_empty()
            {
                if kind == SignatureKind::Call
                    && resolved_as_resolved_type.call_signatures().len() == 1
                    && resolved_as_resolved_type.construct_signatures().is_empty()
                {
                    return Some(resolved_as_resolved_type.call_signatures()[0].clone());
                }
                if kind == SignatureKind::Construct
                    && resolved_as_resolved_type.construct_signatures().len() == 1
                    && resolved_as_resolved_type.call_signatures().is_empty()
                {
                    return Some(resolved_as_resolved_type.construct_signatures()[0].clone());
                }
            }
        }
        None
    }

    pub(super) fn instantiate_signature_in_context_of(
        &self,
        signature: Rc<Signature>,
        contextual_signature: Rc<Signature>,
        inference_context: Option<&InferenceContext>,
        compare_types: Option<Rc<dyn TypeComparer>>,
    ) -> Rc<Signature> {
        let context = self.create_inference_context(
            &signature.maybe_type_parameters().clone().unwrap(),
            Some(signature.clone()),
            InferenceFlags::None,
            compare_types,
        );
        let rest_type = self.get_effective_rest_type(&contextual_signature);
        let mapper = inference_context.map(|inference_context| {
            if matches!(
                rest_type.as_ref(),
                Some(rest_type) if rest_type.flags().intersects(TypeFlags::TypeParameter)
            ) {
                inference_context.non_fixing_mapper().clone()
            } else {
                inference_context.mapper().clone()
            }
        });
        let source_signature = if let Some(mapper) = mapper.as_ref() {
            Rc::new(self.instantiate_signature(contextual_signature.clone(), mapper, None))
        } else {
            contextual_signature.clone()
        };
        self.apply_to_parameter_types(
            &source_signature,
            &signature,
            |source: &Type, target: &Type| {
                self.infer_types(&context.inferences, source, target, None, None);
            },
        );
        if inference_context.is_none() {
            self.apply_to_return_types(
                contextual_signature.clone(),
                signature.clone(),
                |source: &Type, target: &Type| {
                    self.infer_types(
                        &context.inferences,
                        source,
                        target,
                        Some(InferencePriority::ReturnType),
                        None,
                    );
                },
            );
        }
        self.get_signature_instantiation(
            signature.clone(),
            Some(&self.get_inferred_types(&context)),
            is_in_js_file(contextual_signature.declaration.as_deref()),
            None,
        )
    }

    pub(super) fn infer_jsx_type_arguments(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
        signature: Rc<Signature>,
        check_mode: CheckMode,
        context: &InferenceContext,
    ) -> Vec<Rc<Type>> {
        let param_type =
            self.get_effective_first_argument_for_jsx_signature(signature.clone(), node);
        let check_attr_type = self.check_expression_with_contextual_type(
            &node.as_jsx_opening_like_element().attributes(),
            &param_type,
            Some(context),
            Some(check_mode),
        );
        self.infer_types(
            &context.inferences,
            &check_attr_type,
            &param_type,
            None,
            None,
        );
        self.get_inferred_types(context)
    }

    pub(super) fn get_this_argument_type<TThisArgumentNode: Borrow<Node>>(
        &self,
        this_argument_node: Option<TThisArgumentNode /*LeftHandSideExpression*/>,
    ) -> Rc<Type> {
        if this_argument_node.is_none() {
            return self.void_type();
        }
        let this_argument_node = this_argument_node.unwrap();
        let this_argument_node = this_argument_node.borrow();
        let this_argument_type = self.check_expression(this_argument_node, None, None);
        if is_optional_chain_root(&this_argument_node.parent()) {
            self.get_non_nullable_type(&this_argument_type)
        } else if is_optional_chain(&this_argument_node.parent()) {
            self.remove_optional_type_marker(&this_argument_type)
        } else {
            this_argument_type
        }
    }

    pub(super) fn infer_type_arguments(
        &self,
        node: &Node, /*CallLikeExpression*/
        signature: Rc<Signature>,
        args: &[Rc<Node /*Expression*/>],
        check_mode: CheckMode,
        context: &InferenceContext,
    ) -> Vec<Rc<Type>> {
        if is_jsx_opening_like_element(node) {
            return self.infer_jsx_type_arguments(node, signature, check_mode, context);
        }

        if node.kind() != SyntaxKind::Decorator {
            let contextual_type = self.get_contextual_type_(
                node,
                Some(
                    if maybe_every(
                        signature.maybe_type_parameters().as_deref(),
                        |p: &Rc<Type>, _| self.get_default_from_type_parameter_(p).is_some(),
                    ) {
                        ContextFlags::SkipBindingPatterns
                    } else {
                        ContextFlags::None
                    },
                ),
            );
            if let Some(contextual_type) = contextual_type.as_ref() {
                let outer_context = self.get_inference_context(node);
                let outer_mapper = self.get_mapper_from_context(
                    self.clone_inference_context(
                        outer_context.as_deref(),
                        Some(InferenceFlags::NoDefault),
                    )
                    .as_deref(),
                );
                let instantiated_type =
                    self.instantiate_type(contextual_type, outer_mapper.as_ref());
                let contextual_signature = self.get_single_call_signature(&instantiated_type);
                let inference_source_type = if let Some(ref contextual_signature_type_parameters) =
                    contextual_signature
                        .as_ref()
                        .and_then(|contextual_signature| {
                            contextual_signature.maybe_type_parameters().clone()
                        }) {
                    self.get_or_create_type_from_signature(
                        self.get_signature_instantiation_without_filling_in_type_arguments(
                            contextual_signature.clone().unwrap(),
                            Some(contextual_signature_type_parameters),
                        ),
                    )
                } else {
                    instantiated_type.clone()
                };
                let inference_target_type = self.get_return_type_of_signature(signature.clone());
                self.infer_types(
                    &context.inferences,
                    &inference_source_type,
                    &inference_target_type,
                    Some(InferencePriority::ReturnType),
                    None,
                );
                let return_context = self.create_inference_context(
                    &signature.maybe_type_parameters().clone().unwrap(),
                    Some(signature.clone()),
                    context.flags,
                    None,
                );
                let return_source_type = self.instantiate_type(
                    contextual_type,
                    if let Some(outer_context) = outer_context.as_ref() {
                        outer_context.maybe_return_mapper().clone()
                    } else {
                        None
                    }
                    .as_ref(),
                );
                self.infer_types(
                    &return_context.inferences,
                    &return_source_type,
                    &inference_target_type,
                    None,
                    None,
                );
                *context.maybe_return_mapper() = if some(
                    Some(&return_context.inferences),
                    Some(|inference: &Rc<InferenceInfo>| self.has_inference_candidates(inference)),
                ) {
                    self.get_mapper_from_context(
                        self.clone_inferred_part_of_context(&return_context)
                            .as_deref(),
                    )
                } else {
                    None
                };
            }
        }

        let rest_type = self.get_non_array_rest_type(&signature);
        let arg_count = if rest_type.is_some() {
            cmp::min(self.get_parameter_count(&signature) - 1, args.len())
        } else {
            args.len()
        };
        if let Some(rest_type) = rest_type
            .as_ref()
            .filter(|rest_type| rest_type.flags().intersects(TypeFlags::TypeParameter))
        {
            let info = find(&context.inferences, |info: &Rc<InferenceInfo>, _| {
                Rc::ptr_eq(&info.type_parameter, rest_type)
            });
            if let Some(info) = info {
                info.set_implied_arity(
                    if find_index(
                        args,
                        |arg: &Rc<Node>, _| self.is_spread_argument(Some(&**arg)),
                        None,
                    )
                    .is_none()
                    {
                        Some(args.len() - arg_count)
                    } else {
                        None
                    },
                );
            }
        }

        let this_type = self.get_this_type_of_signature(&signature);
        if let Some(this_type) = this_type.as_ref() {
            let this_argument_node = self.get_this_argument_of_call(node);
            self.infer_types(
                &context.inferences,
                &self.get_this_argument_type(this_argument_node.as_deref()),
                this_type,
                None,
                None,
            );
        }

        for i in 0..arg_count {
            let arg = &args[i];
            if arg.kind() != SyntaxKind::OmittedExpression {
                let param_type = self.get_type_at_position(&signature, i);
                let arg_type = self.check_expression_with_contextual_type(
                    arg,
                    &param_type,
                    Some(context),
                    Some(check_mode),
                );
                self.infer_types(&context.inferences, &arg_type, &param_type, None, None);
            }
        }

        if let Some(rest_type) = rest_type.as_ref() {
            let spread_type = self.get_spread_argument_type(
                args,
                arg_count,
                args.len(),
                rest_type,
                Some(context),
                check_mode,
            );
            self.infer_types(&context.inferences, &spread_type, rest_type, None, None);
        }

        self.get_inferred_types(context)
    }

    pub(super) fn get_mutable_array_or_tuple_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_: &Type| Some(self.get_mutable_array_or_tuple_type(type_)),
                None,
            )
            .unwrap()
        } else if type_.flags().intersects(TypeFlags::Any)
            || self.is_mutable_array_or_tuple(
                &*self
                    .get_base_constraint_of_type(type_)
                    .unwrap_or_else(|| type_.type_wrapper()),
            )
        {
            type_.type_wrapper()
        } else if self.is_tuple_type(type_) {
            self.create_tuple_type(
                &self.get_type_arguments(type_),
                Some(
                    &type_
                        .as_type_reference()
                        .target
                        .as_tuple_type()
                        .element_flags,
                ),
                Some(false),
                type_
                    .as_type_reference()
                    .target
                    .as_tuple_type()
                    .labeled_element_declarations
                    .as_deref(),
            )
        } else {
            self.create_tuple_type(
                &[type_.type_wrapper()],
                Some(&[ElementFlags::Variadic]),
                None,
                None,
            )
        }
    }

    pub(super) fn get_spread_argument_type(
        &self,
        args: &[Rc<Node /*Expression*/>],
        index: usize,
        arg_count: usize,
        rest_type: &Type,
        context: Option<&InferenceContext>,
        check_mode: CheckMode,
    ) -> Rc<Type> {
        if index >= arg_count - 1 {
            let arg = &args[arg_count - 1];
            if self.is_spread_argument(Some(&**arg)) {
                return self.get_mutable_array_or_tuple_type(&*if arg.kind()
                    == SyntaxKind::SyntheticExpression
                {
                    arg.as_synthetic_expression().type_.clone()
                } else {
                    self.check_expression_with_contextual_type(
                        &arg.as_spread_element().expression,
                        rest_type,
                        context,
                        Some(check_mode),
                    )
                });
            }
        }
        let mut types: Vec<Rc<Type>> = vec![];
        let mut flags: Vec<ElementFlags> = vec![];
        let mut names: Vec<Rc<Node>> = vec![];
        for i in index..arg_count {
            let arg = &args[i];
            if self.is_spread_argument(Some(&**arg)) {
                let spread_type = if arg.kind() == SyntaxKind::SyntheticExpression {
                    arg.as_synthetic_expression().type_.clone()
                } else {
                    self.check_expression(&arg.as_spread_element().expression, None, None)
                };
                if self.is_array_like_type(&spread_type) {
                    types.push(spread_type);
                    flags.push(ElementFlags::Variadic);
                } else {
                    types.push(self.check_iterated_type_or_element_type(
                        IterationUse::Spread,
                        &spread_type,
                        &self.undefined_type(),
                        Some(if arg.kind() == SyntaxKind::SpreadElement {
                            arg.as_spread_element().expression.clone()
                        } else {
                            arg.clone()
                        }),
                    ));
                    flags.push(ElementFlags::Rest);
                }
            } else {
                let contextual_type = self.get_indexed_access_type(
                    rest_type,
                    &self.get_number_literal_type(Number::new((i - index) as f64)),
                    Some(AccessFlags::Contextual),
                    Option::<&Node>::None,
                    Option::<&Symbol>::None,
                    None,
                );
                let arg_type = self.check_expression_with_contextual_type(
                    arg,
                    &contextual_type,
                    context,
                    Some(check_mode),
                );
                let has_primitive_contextual_type = self.maybe_type_of_kind(
                    &contextual_type,
                    TypeFlags::Primitive
                        | TypeFlags::Index
                        | TypeFlags::TemplateLiteral
                        | TypeFlags::StringMapping,
                );
                types.push(if has_primitive_contextual_type {
                    self.get_regular_type_of_literal_type(&arg_type)
                } else {
                    self.get_widened_literal_type(&arg_type)
                });
                flags.push(ElementFlags::Required);
            }
            if arg.kind() == SyntaxKind::SyntheticExpression {
                if let Some(arg_tuple_name_source) =
                    arg.as_synthetic_expression().tuple_name_source.as_ref()
                {
                    names.push(arg_tuple_name_source.clone());
                }
            }
        }
        self.create_tuple_type(
            &types,
            Some(&flags),
            Some(false),
            if length(Some(&names)) == length(Some(&types)) {
                Some(&*names)
            } else {
                None
            },
        )
    }

    pub(super) fn check_type_arguments(
        &self,
        signature: &Signature,
        type_argument_nodes: &[Rc<Node /*TypeNode*/>],
        report_errors: bool,
        head_message: Option<&'static DiagnosticMessage>,
    ) -> Option<Vec<Rc<Type>>> {
        let is_javascript = is_in_js_file(signature.declaration.as_deref());
        let type_parameters = signature.maybe_type_parameters().clone().unwrap();
        let type_argument_types = self
            .fill_missing_type_arguments(
                Some(map(type_argument_nodes, |node: &Rc<Node>, _| {
                    self.get_type_from_type_node_(node)
                })),
                Some(&type_parameters),
                self.get_min_type_argument_count(Some(&type_parameters)),
                is_javascript,
            )
            .unwrap();
        let mut mapper: Option<TypeMapper> = None;
        for i in 0..type_argument_nodes.len() {
            Debug_.assert(
                type_parameters.get(i).is_some(),
                Some("Should not call checkTypeArguments with too many type arguments"),
            );
            let constraint = self.get_constraint_of_type_parameter(&type_parameters[i]);
            if let Some(constraint) = constraint.as_ref() {
                let error_info: Option<Rc<dyn CheckTypeContainingMessageChain>> =
                    if report_errors && head_message.is_some() {
                        Some(Rc::new(CheckTypeArgumentsErrorInfo))
                    } else {
                        None
                    };
                let type_argument_head_message =
                    head_message.unwrap_or(&*Diagnostics::Type_0_does_not_satisfy_the_constraint_1);
                if mapper.is_none() {
                    mapper = Some(self.create_type_mapper(
                        type_parameters.clone(),
                        Some(type_argument_types.clone()),
                    ));
                }
                let type_argument = &type_argument_types[i];
                if !self.check_type_assignable_to(
                    type_argument,
                    &self.get_type_with_this_argument(
                        &self.instantiate_type(constraint, mapper.as_ref()),
                        Some(&**type_argument),
                        None,
                    ),
                    if report_errors {
                        Some(&*type_argument_nodes[i])
                    } else {
                        None
                    },
                    Some(type_argument_head_message),
                    error_info.clone(),
                    None,
                ) {
                    return None;
                }
            }
        }
        Some(type_argument_types)
    }

    pub(super) fn get_jsx_reference_kind(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> JsxReferenceKind {
        let node_as_jsx_opening_like_element = node.as_jsx_opening_like_element();
        if self.is_jsx_intrinsic_identifier(&node_as_jsx_opening_like_element.tag_name()) {
            return JsxReferenceKind::Mixed;
        }
        let tag_type = self.get_apparent_type(&self.check_expression(
            &node_as_jsx_opening_like_element.tag_name(),
            None,
            None,
        ));
        if length(Some(
            &self.get_signatures_of_type(&tag_type, SignatureKind::Construct),
        )) > 0
        {
            return JsxReferenceKind::Component;
        }
        if length(Some(
            &self.get_signatures_of_type(&tag_type, SignatureKind::Call),
        )) > 0
        {
            return JsxReferenceKind::Function;
        }
        JsxReferenceKind::Mixed
    }

    pub(super) fn check_applicable_signature_for_jsx_opening_like_element(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
        signature: Rc<Signature>,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        check_mode: CheckMode,
        report_errors: bool,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
        error_output_container: Rc<dyn CheckTypeErrorOutputContainer>,
    ) -> bool {
        let param_type =
            self.get_effective_first_argument_for_jsx_signature(signature.clone(), node);
        let node_as_jsx_opening_like_element = node.as_jsx_opening_like_element();
        let attributes_type = self.check_expression_with_contextual_type(
            &node_as_jsx_opening_like_element.attributes(),
            &param_type,
            None,
            Some(check_mode),
        );
        self.check_tag_name_does_not_expect_too_many_arguments(
            node,
            report_errors,
            error_output_container.clone(),
        ) && self.check_type_related_to_and_optionally_elaborate(
            &attributes_type,
            &param_type,
            relation,
            if report_errors {
                Some(node_as_jsx_opening_like_element.tag_name())
            } else {
                None
            },
            Some(node_as_jsx_opening_like_element.attributes()),
            None,
            containing_message_chain,
            Some(error_output_container),
        )
    }

    pub(super) fn check_tag_name_does_not_expect_too_many_arguments(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
        report_errors: bool,
        error_output_container: Rc<dyn CheckTypeErrorOutputContainer>,
    ) -> bool {
        if self
            .get_jsx_namespace_container_for_implicit_import(Some(node))
            .is_some()
        {
            return true;
        }
        let node_as_jsx_opening_like_element = node.as_jsx_opening_like_element();
        let tag_type = if is_jsx_opening_element(node)
            || is_jsx_self_closing_element(node)
                && !self.is_jsx_intrinsic_identifier(&node_as_jsx_opening_like_element.tag_name())
        {
            Some(self.check_expression(&node_as_jsx_opening_like_element.tag_name(), None, None))
        } else {
            None
        };
        if tag_type.is_none() {
            return true;
        }
        let tag_type = tag_type.unwrap();
        let tag_call_signatures = self.get_signatures_of_type(&tag_type, SignatureKind::Call);
        if length(Some(&*tag_call_signatures)) == 0 {
            return true;
        }
        let factory = self.get_jsx_factory_entity(node);
        if factory.is_none() {
            return true;
        }
        let factory = factory.unwrap();
        let factory_symbol = self.resolve_entity_name(
            &factory,
            SymbolFlags::Value,
            Some(true),
            Some(false),
            Some(node),
        );
        if factory_symbol.is_none() {
            return true;
        }
        let factory_symbol = factory_symbol.unwrap();

        let factory_type = self.get_type_of_symbol(&factory_symbol);
        let call_signatures = self.get_signatures_of_type(&factory_type, SignatureKind::Call);
        if length(Some(&*call_signatures)) == 0 {
            return true;
        }

        let mut has_first_param_signatures = false;
        let mut max_param_count = 0;
        for sig in &call_signatures {
            let firstparam = self.get_type_at_position(sig, 0);
            let signatures_of_param = self.get_signatures_of_type(&firstparam, SignatureKind::Call);
            if length(Some(&*signatures_of_param)) == 0 {
                continue;
            }
            for param_sig in &signatures_of_param {
                has_first_param_signatures = true;
                if self.has_effective_rest_parameter(param_sig) {
                    return true;
                }
                let param_count = self.get_parameter_count(param_sig);
                if param_count > max_param_count {
                    max_param_count = param_count;
                }
            }
        }
        if !has_first_param_signatures {
            return true;
        }
        let mut absolute_min_arg_count = usize::MAX;
        for tag_sig in &tag_call_signatures {
            let tag_required_arg_count = self.get_min_argument_count(tag_sig, None);
            if tag_required_arg_count < absolute_min_arg_count {
                absolute_min_arg_count = tag_required_arg_count;
            }
        }
        if absolute_min_arg_count <= max_param_count {
            return true;
        }

        if report_errors {
            let diag: Rc<Diagnostic> = Rc::new(
                create_diagnostic_for_node(
                    &node_as_jsx_opening_like_element.tag_name(),
                    &Diagnostics::Tag_0_expects_at_least_1_arguments_but_the_JSX_factory_2_provides_at_most_3,
                    Some(vec![
                        entity_name_to_string(
                            &node_as_jsx_opening_like_element.tag_name()
                        ).into_owned(),
                        absolute_min_arg_count.to_string(),
                        entity_name_to_string(&factory).into_owned(),
                        max_param_count.to_string(),
                    ])
                ).into()
            );
            let tag_name_declaration = self
                .get_symbol_at_location_(&node_as_jsx_opening_like_element.tag_name(), None)
                .and_then(|symbol| symbol.maybe_value_declaration());
            if let Some(tag_name_declaration) = tag_name_declaration.as_ref() {
                add_related_info(
                    &diag,
                    vec![Rc::new(
                        create_diagnostic_for_node(
                            tag_name_declaration,
                            &Diagnostics::_0_is_declared_here,
                            Some(vec![entity_name_to_string(
                                &node_as_jsx_opening_like_element.tag_name(),
                            )
                            .into_owned()]),
                        )
                        .into(),
                    )],
                );
            }
            if
            /*errorOutputContainer &&*/
            error_output_container.skip_logging() == Some(true) {
                error_output_container.push_error(diag.clone());
            }
            if error_output_container.skip_logging() != Some(true) {
                self.diagnostics().add(diag);
            }
        }
        false
    }

    pub(super) fn get_signature_applicability_error(
        &self,
        node: &Node, /*CallLikeExpression*/
        args: &[Rc<Node /*Expression*/>],
        signature: Rc<Signature>,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        check_mode: CheckMode,
        report_errors: bool,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
    ) -> Option<Vec<Rc<Diagnostic>>> {
        let error_output_container: Rc<dyn CheckTypeErrorOutputContainer> =
            Rc::new(CheckTypeErrorOutputContainerConcrete::new(Some(true)));
        if is_jsx_opening_like_element(node) {
            if !self.check_applicable_signature_for_jsx_opening_like_element(
                node,
                signature.clone(),
                relation.clone(),
                check_mode,
                report_errors,
                containing_message_chain.clone(),
                error_output_container.clone(),
            ) {
                Debug_.assert(
                    !report_errors || error_output_container.errors_len() > 0,
                    Some("jsx should have errors when reporting errors"),
                );
                return Some(error_output_container.errors()) /*|| emptyArray*/;
            }
            return None;
        }
        let this_type = self.get_this_type_of_signature(&signature);
        if let Some(this_type) = this_type.as_ref().filter(|this_type| {
            !Rc::ptr_eq(this_type, &self.void_type()) && node.kind() != SyntaxKind::NewExpression
        }) {
            let this_argument_node = self.get_this_argument_of_call(node);
            let this_argument_type = self.get_this_argument_type(this_argument_node.as_deref());
            let error_node = if report_errors {
                Some(
                    this_argument_node
                        .clone()
                        .unwrap_or_else(|| node.node_wrapper()),
                )
            } else {
                None
            };
            let head_message = &Diagnostics::The_this_context_of_type_0_is_not_assignable_to_method_s_this_of_type_1;
            if !self.check_type_related_to(
                &this_argument_type,
                this_type,
                relation.clone(),
                error_node,
                Some(Cow::Borrowed(head_message)),
                containing_message_chain.clone(),
                Some(error_output_container.clone()),
            ) {
                Debug_.assert(
                    !report_errors || error_output_container.errors_len() > 0,
                    Some("this parameter should have errors when reporting errors"),
                );
                return Some(error_output_container.errors()) /*|| emptyArray*/;
            }
        }
        let head_message =
            &Diagnostics::Argument_of_type_0_is_not_assignable_to_parameter_of_type_1;
        let rest_type = self.get_non_array_rest_type(&signature);
        let arg_count = if rest_type.is_some() {
            cmp::min(self.get_parameter_count(&signature) - 1, args.len())
        } else {
            args.len()
        };
        for i in 0..arg_count {
            let arg = &args[i];
            if arg.kind() != SyntaxKind::OmittedExpression {
                let param_type = self.get_type_at_position(&signature, i);
                let arg_type = self.check_expression_with_contextual_type(
                    arg,
                    &param_type,
                    None,
                    Some(check_mode),
                );
                let check_arg_type = if check_mode.intersects(CheckMode::SkipContextSensitive) {
                    self.get_regular_type_of_object_literal(&arg_type)
                } else {
                    arg_type.clone()
                };
                if !self.check_type_related_to_and_optionally_elaborate(
                    &check_arg_type,
                    &param_type,
                    relation.clone(),
                    if report_errors {
                        Some(arg.clone())
                    } else {
                        None
                    },
                    Some(arg.clone()),
                    Some(head_message),
                    containing_message_chain.clone(),
                    Some(error_output_container.clone()),
                ) {
                    Debug_.assert(
                        !report_errors || error_output_container.errors_len() > 0,
                        Some("parameter should have errors when reporting errors"),
                    );
                    self.maybe_add_missing_await_info(
                        report_errors,
                        error_output_container.clone(),
                        relation.clone(),
                        Some(&**arg),
                        &check_arg_type,
                        &param_type,
                    );
                    return Some(error_output_container.errors()) /*|| emptyArray*/;
                }
            }
        }
        if let Some(rest_type) = rest_type.as_ref() {
            let spread_type = self.get_spread_argument_type(
                args,
                arg_count,
                args.len(),
                rest_type,
                None,
                check_mode,
            );
            let rest_arg_count = args.len() - arg_count;
            let error_node = if !report_errors {
                None
            } else if rest_arg_count == 0 {
                Some(node.node_wrapper())
            } else if rest_arg_count == 1 {
                Some(args[arg_count].clone())
            } else {
                let error_node = self.create_synthetic_expression(
                    node,
                    &spread_type,
                    None,
                    Option::<&Node>::None,
                );
                set_text_range_pos_end(
                    &*error_node,
                    args[arg_count].pos(),
                    args[args.len() - 1].end(),
                );
                Some(error_node)
            };
            if !self.check_type_related_to(
                &spread_type,
                rest_type,
                relation.clone(),
                error_node.clone(),
                Some(Cow::Borrowed(head_message)),
                None,
                Some(error_output_container.clone()),
            ) {
                Debug_.assert(
                    !report_errors || error_output_container.errors_len() > 0,
                    Some("rest parameter should have errors when reporting errors"),
                );
                self.maybe_add_missing_await_info(
                    report_errors,
                    error_output_container.clone(),
                    relation.clone(),
                    error_node,
                    &spread_type,
                    rest_type,
                );
                return Some(error_output_container.errors()) /*|| emptyArray*/;
            }
        }
        None
    }
}

struct CheckTypeArgumentsErrorInfo;

impl CheckTypeContainingMessageChain for CheckTypeArgumentsErrorInfo {
    fn get(&self) -> Option<Rc<RefCell<DiagnosticMessageChain>>> {
        Some(Rc::new(RefCell::new(chain_diagnostic_messages(
            None,
            &Diagnostics::Type_0_does_not_satisfy_the_constraint_1,
            None,
        ))))
    }
}
