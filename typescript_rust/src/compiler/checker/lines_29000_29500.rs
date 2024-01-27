use std::{
    borrow::{Borrow, Cow},
    cell::RefCell,
    cmp,
    collections::HashMap,
    io,
    rc::Rc,
};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use super::{
    CheckMode, CheckTypeContainingMessageChain, CheckTypeErrorOutputContainer,
    CheckTypeErrorOutputContainerConcrete, IterationUse,
};
use crate::{
    add_related_info, chain_diagnostic_messages, create_diagnostic_for_node, entity_name_to_string,
    find, find_index, is_in_js_file, is_jsx_opening_element, is_jsx_opening_like_element,
    is_jsx_self_closing_element, is_optional_chain, is_optional_chain_root, last, length,
    node_is_missing, set_text_range_pos_end, some, try_map, try_maybe_every, AccessFlags,
    ContextFlags, Debug_, Diagnostic, DiagnosticMessage, DiagnosticMessageChain, Diagnostics,
    ElementFlags, HasArena, InArena, InferenceContext, InferenceFlags, InferenceInfo,
    InferencePriority, JsxReferenceKind, Node, NodeArray, NodeInterface, Number, OptionTry,
    ReadonlyTextRange, RelationComparisonResult, Signature, SignatureKind, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeComparer, TypeFlags, TypeInterface,
    TypeMapper,
    OptionInArena,
};

impl TypeChecker {
    pub(super) fn is_spread_argument(&self, arg: Option<Id<Node>>) -> bool {
        let Some(arg) = arg else {
            return false;
        };
        arg.ref_(self).kind() == SyntaxKind::SpreadElement
            || arg.ref_(self).kind() == SyntaxKind::SyntheticExpression
                && arg.ref_(self).as_synthetic_expression().is_spread
    }

    pub(super) fn get_spread_argument_index(
        &self,
        args: &[Id<Node /*Expression*/>],
    ) -> Option<usize> {
        find_index(
            args,
            |&arg: &Id<Node>, _| self.is_spread_argument(Some(arg)),
            None,
        )
    }

    pub(super) fn accepts_void(&self, t: Id<Type>) -> bool {
        t.ref_(self).flags().intersects(TypeFlags::Void)
    }

    pub(super) fn accepts_void_undefined_unknown_or_any(&self, t: Id<Type>) -> bool {
        t.ref_(self).flags().intersects(
            TypeFlags::Void | TypeFlags::Undefined | TypeFlags::Unknown | TypeFlags::Any,
        )
    }

    pub(super) fn has_correct_arity(
        &self,
        node: Id<Node>, /*CallLikeExpression*/
        args: &[Id<Node /*Expression*/>],
        signature: Id<Signature>,
        signature_help_trailing_comma: Option<bool>,
    ) -> io::Result<bool> {
        let signature_help_trailing_comma = signature_help_trailing_comma.unwrap_or(false);
        let arg_count: usize;
        let mut call_is_incomplete = false;
        let mut effective_parameter_count = self.get_parameter_count(signature)?;
        let mut effective_minimum_arguments = self.get_min_argument_count(signature, None)?;

        if node.ref_(self).kind() == SyntaxKind::TaggedTemplateExpression {
            arg_count = args.len();
            let node_ref = node.ref_(self);
            let node_as_tagged_template_expression = node_ref.as_tagged_template_expression();
            if node_as_tagged_template_expression.template.ref_(self).kind() == SyntaxKind::TemplateExpression
            {
                let last_span = *last(
                    &node_as_tagged_template_expression
                        .template
                        .ref_(self).as_template_expression()
                        .template_spans,
                );
                let last_span_ref = last_span.ref_(self);
                let last_span_as_template_span = last_span_ref.as_template_span();
                call_is_incomplete = node_is_missing(Some(&last_span_as_template_span.literal.ref_(self)))
                    || last_span_as_template_span
                        .literal
                        .ref_(self).as_literal_like_node()
                        .is_unterminated()
                        == Some(true);
            } else {
                let template_literal = &node_as_tagged_template_expression.template;
                Debug_.assert(
                    template_literal.ref_(self).kind() == SyntaxKind::NoSubstitutionTemplateLiteral,
                    None,
                );
                call_is_incomplete =
                    template_literal.ref_(self).as_literal_like_node().is_unterminated() == Some(true);
            }
        } else if node.ref_(self).kind() == SyntaxKind::Decorator {
            arg_count = self.get_decorator_argument_count(node, signature);
        } else if is_jsx_opening_like_element(&node.ref_(self)) {
            call_is_incomplete =
                node.ref_(self).as_jsx_opening_like_element().attributes().ref_(self).end() == node.ref_(self).end();
            if call_is_incomplete {
                return Ok(true);
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
        } else if node.ref_(self).as_has_arguments().maybe_arguments().is_none() {
            Debug_.assert(node.ref_(self).kind() == SyntaxKind::NewExpression, None);
            return Ok(self.get_min_argument_count(signature, None)? == 0);
        } else {
            arg_count = if signature_help_trailing_comma {
                args.len() + 1
            } else {
                args.len()
            };

            call_is_incomplete =
                node.ref_(self).as_has_arguments().maybe_arguments().unwrap().end() == node.ref_(self).end();

            let spread_arg_index = self.get_spread_argument_index(args);
            if let Some(spread_arg_index) = spread_arg_index {
                return Ok(
                    spread_arg_index >= self.get_min_argument_count(signature, None)?
                        && (self.has_effective_rest_parameter(signature)?
                            || spread_arg_index < self.get_parameter_count(signature)?),
                );
            }
        }

        if !self.has_effective_rest_parameter(signature)? && arg_count > effective_parameter_count {
            return Ok(false);
        }

        if call_is_incomplete || arg_count >= effective_minimum_arguments {
            return Ok(true);
        }
        for i in arg_count..effective_minimum_arguments {
            let type_ = self.get_type_at_position(signature, i)?;
            if self
                .filter_type(type_, |type_: Id<Type>| {
                    if is_in_js_file(Some(&node.ref_(self))) && !self.strict_null_checks {
                        self.accepts_void_undefined_unknown_or_any(type_)
                    } else {
                        self.accepts_void(type_)
                    }
                })
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Never)
            {
                return Ok(false);
            }
        }
        Ok(true)
    }

    pub(super) fn has_correct_type_argument_arity(
        &self,
        signature: Id<Signature>,
        type_arguments: Option<Id<NodeArray> /*<TypeNode>*/>,
    ) -> bool {
        let num_type_parameters = length(signature.ref_(self).maybe_type_parameters().as_deref());
        let min_type_argument_count =
            self.get_min_type_argument_count(signature.ref_(self).maybe_type_parameters().as_deref());
        !some(
            type_arguments.map(|type_arguments| &**type_arguments),
            Option::<fn(&Id<Node>) -> bool>::None,
        ) || {
            let type_arguments = type_arguments.unwrap();
            type_arguments.len() >= min_type_argument_count
                && type_arguments.len() <= num_type_parameters
        }
    }

    pub(super) fn get_single_call_signature(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Option<Id<Signature>>> {
        self.get_single_signature(type_, SignatureKind::Call, false)
    }

    pub(super) fn get_single_call_or_construct_signature(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Option<Id<Signature>>> {
        self.get_single_signature(type_, SignatureKind::Call, false)?
            .try_or_else(|| self.get_single_signature(type_, SignatureKind::Construct, false))
    }

    pub(super) fn get_single_signature(
        &self,
        type_: Id<Type>,
        kind: SignatureKind,
        allow_members: bool,
    ) -> io::Result<Option<Id<Signature>>> {
        if type_.ref_(self).flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_)?;
            if allow_members
                || resolved
                    .ref_(self)
                    .as_resolved_type()
                    .properties()
                    .is_empty()
                    && resolved
                        .ref_(self)
                        .as_resolved_type()
                        .index_infos()
                        .is_empty()
            {
                if kind == SignatureKind::Call
                    && resolved
                        .ref_(self)
                        .as_resolved_type()
                        .call_signatures()
                        .len()
                        == 1
                    && resolved
                        .ref_(self)
                        .as_resolved_type()
                        .construct_signatures()
                        .is_empty()
                {
                    return Ok(Some(
                        resolved.ref_(self).as_resolved_type().call_signatures()[0].clone(),
                    ));
                }
                if kind == SignatureKind::Construct
                    && resolved
                        .ref_(self)
                        .as_resolved_type()
                        .construct_signatures()
                        .len()
                        == 1
                    && resolved
                        .ref_(self)
                        .as_resolved_type()
                        .call_signatures()
                        .is_empty()
                {
                    return Ok(Some(
                        resolved
                            .ref_(self)
                            .as_resolved_type()
                            .construct_signatures()[0]
                            .clone(),
                    ));
                }
            }
        }
        Ok(None)
    }

    pub(super) fn instantiate_signature_in_context_of(
        &self,
        signature: Id<Signature>,
        contextual_signature: Id<Signature>,
        inference_context: Option<&InferenceContext>,
        compare_types: Option<Gc<Box<dyn TypeComparer>>>,
    ) -> io::Result<Id<Signature>> {
        let context = self.create_inference_context(
            &signature.ref_(self).maybe_type_parameters().clone().unwrap(),
            Some(signature.clone()),
            InferenceFlags::None,
            compare_types,
        );
        let rest_type = self.get_effective_rest_type(contextual_signature)?;
        let mapper = inference_context.map(|inference_context| {
            if matches!(
                rest_type,
                Some(rest_type) if rest_type.ref_(self).flags().intersects(TypeFlags::TypeParameter)
            ) {
                inference_context.non_fixing_mapper().clone()
            } else {
                inference_context.mapper().clone()
            }
        });
        let source_signature = if let Some(mapper) = mapper {
            self.alloc_signature(self.instantiate_signature(contextual_signature.clone(), mapper, None)?)
        } else {
            contextual_signature.clone()
        };
        self.apply_to_parameter_types(
            source_signature,
            signature,
            |source: Id<Type>, target: Id<Type>| {
                self.infer_types(&context.inferences(), source, target, None, None)
            },
        )?;
        if inference_context.is_none() {
            self.apply_to_return_types(
                contextual_signature.clone(),
                signature.clone(),
                |source: Id<Type>, target: Id<Type>| {
                    self.infer_types(
                        &context.inferences(),
                        source,
                        target,
                        Some(InferencePriority::ReturnType),
                        None,
                    )?;

                    Ok(())
                },
            )?;
        }
        self.get_signature_instantiation(
            signature.clone(),
            Some(&*self.get_inferred_types(&context)?),
            is_in_js_file(contextual_signature.ref_(self).declaration.refed(self).as_deref()),
            None,
        )
    }

    pub(super) fn infer_jsx_type_arguments(
        &self,
        node: Id<Node>, /*JsxOpeningLikeElement*/
        signature: Id<Signature>,
        check_mode: CheckMode,
        context: Gc<InferenceContext>,
    ) -> io::Result<Vec<Id<Type>>> {
        let param_type =
            self.get_effective_first_argument_for_jsx_signature(signature.clone(), node)?;
        let check_attr_type = self.check_expression_with_contextual_type(
            node.ref_(self).as_jsx_opening_like_element().attributes(),
            param_type,
            Some(context.clone()),
            check_mode,
        )?;
        self.infer_types(
            &context.inferences(),
            check_attr_type,
            param_type,
            None,
            None,
        )?;
        self.get_inferred_types(&context)
    }

    pub(super) fn get_this_argument_type(
        &self,
        this_argument_node: Option<Id<Node> /*LeftHandSideExpression*/>,
    ) -> io::Result<Id<Type>> {
        let Some(this_argument_node) = this_argument_node else {
            return Ok(self.void_type());
        };
        let this_argument_type = self.check_expression(this_argument_node, None, None)?;
        Ok(if is_optional_chain_root(&this_argument_node.ref_(self).parent().ref_(self)) {
            self.get_non_nullable_type(this_argument_type)?
        } else if is_optional_chain(&this_argument_node.ref_(self).parent().ref_(self)) {
            self.remove_optional_type_marker(this_argument_type)
        } else {
            this_argument_type
        })
    }

    pub(super) fn infer_type_arguments(
        &self,
        node: Id<Node>, /*CallLikeExpression*/
        signature: Id<Signature>,
        args: &[Id<Node /*Expression*/>],
        check_mode: CheckMode,
        context: Gc<InferenceContext>,
    ) -> io::Result<Vec<Id<Type>>> {
        if is_jsx_opening_like_element(&node.ref_(self)) {
            return self.infer_jsx_type_arguments(node, signature, check_mode, context);
        }

        if node.ref_(self).kind() != SyntaxKind::Decorator {
            let contextual_type = self.get_contextual_type_(
                node,
                Some(
                    if try_maybe_every(
                        signature.ref_(self).maybe_type_parameters().as_deref(),
                        |&p: &Id<Type>, _| -> io::Result<_> {
                            Ok(self.get_default_from_type_parameter_(p)?.is_some())
                        },
                    )? {
                        ContextFlags::SkipBindingPatterns
                    } else {
                        ContextFlags::None
                    },
                ),
            )?;
            if let Some(contextual_type) = contextual_type {
                let outer_context = self.get_inference_context(node);
                let outer_mapper = self.get_mapper_from_context(
                    self.clone_inference_context(
                        outer_context.as_deref(),
                        Some(InferenceFlags::NoDefault),
                    )
                    .as_deref(),
                );
                let instantiated_type = self.instantiate_type(contextual_type, outer_mapper)?;
                let contextual_signature = self.get_single_call_signature(instantiated_type)?;
                let inference_source_type = if let Some(ref contextual_signature_type_parameters) =
                    contextual_signature
                        .and_then(|contextual_signature| {
                            contextual_signature.ref_(self).maybe_type_parameters().clone()
                        }) {
                    self.get_or_create_type_from_signature(
                        self.get_signature_instantiation_without_filling_in_type_arguments(
                            contextual_signature.clone().unwrap(),
                            Some(contextual_signature_type_parameters),
                        )?,
                    )
                } else {
                    instantiated_type.clone()
                };
                let inference_target_type = self.get_return_type_of_signature(signature.clone())?;
                self.infer_types(
                    &context.inferences(),
                    inference_source_type,
                    inference_target_type,
                    Some(InferencePriority::ReturnType),
                    None,
                )?;
                let return_context = self.create_inference_context(
                    &signature.ref_(self).maybe_type_parameters().clone().unwrap(),
                    Some(signature.clone()),
                    context.flags(),
                    None,
                );
                let return_source_type = self.instantiate_type(
                    contextual_type,
                    if let Some(outer_context) = outer_context.as_ref() {
                        outer_context.maybe_return_mapper()
                    } else {
                        None
                    },
                )?;
                self.infer_types(
                    &return_context.inferences(),
                    return_source_type,
                    inference_target_type,
                    None,
                    None,
                )?;
                context.set_return_mapper(
                    if some(
                        Some(&**return_context.inferences()),
                        Some(|inference: &Gc<InferenceInfo>| {
                            self.has_inference_candidates(inference)
                        }),
                    ) {
                        self.get_mapper_from_context(
                            self.clone_inferred_part_of_context(&return_context)
                                .as_deref(),
                        )
                    } else {
                        None
                    },
                );
            }
        }

        let rest_type = self.get_non_array_rest_type(signature)?;
        let arg_count = if rest_type.is_some() {
            cmp::min(self.get_parameter_count(signature)? - 1, args.len())
        } else {
            args.len()
        };
        if let Some(rest_type) = rest_type.filter(|&rest_type| {
            rest_type
                .ref_(self)
                .flags()
                .intersects(TypeFlags::TypeParameter)
        }) {
            let info = find(&context.inferences(), |info: &Gc<InferenceInfo>, _| {
                info.type_parameter == rest_type
            })
            .cloned();
            if let Some(info) = info.as_ref() {
                info.set_implied_arity(
                    if find_index(
                        args,
                        |&arg: &Id<Node>, _| self.is_spread_argument(Some(arg)),
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

        let this_type = self.get_this_type_of_signature(signature)?;
        if let Some(this_type) = this_type {
            let this_argument_node = self.get_this_argument_of_call(node);
            self.infer_types(
                &context.inferences(),
                self.get_this_argument_type(this_argument_node)?,
                this_type,
                None,
                None,
            )?;
        }

        for i in 0..arg_count {
            let arg = args[i];
            if arg.ref_(self).kind() != SyntaxKind::OmittedExpression {
                let param_type = self.get_type_at_position(signature, i)?;
                let arg_type = self.check_expression_with_contextual_type(
                    arg,
                    param_type,
                    Some(context.clone()),
                    check_mode,
                )?;
                self.infer_types(&context.inferences(), arg_type, param_type, None, None)?;
            }
        }

        if let Some(rest_type) = rest_type {
            let spread_type = self.get_spread_argument_type(
                args,
                arg_count,
                args.len(),
                rest_type,
                Some(context.clone()),
                check_mode,
            )?;
            self.infer_types(&context.inferences(), spread_type, rest_type, None, None)?;
        }

        self.get_inferred_types(&context)
    }

    pub(super) fn get_mutable_array_or_tuple_type(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        Ok(if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            self.try_map_type(
                type_,
                &mut |type_: Id<Type>| Ok(Some(self.get_mutable_array_or_tuple_type(type_)?)),
                None,
            )?
            .unwrap()
        } else if type_.ref_(self).flags().intersects(TypeFlags::Any)
            || self.is_mutable_array_or_tuple(
                self.get_base_constraint_of_type(type_)?
                    .unwrap_or_else(|| type_),
            )
        {
            type_
        } else if self.is_tuple_type(type_) {
            self.create_tuple_type(
                &*self.get_type_arguments(type_)?,
                Some(
                    &type_
                        .ref_(self)
                        .as_type_reference()
                        .target
                        .ref_(self)
                        .as_tuple_type()
                        .element_flags,
                ),
                Some(false),
                type_
                    .ref_(self)
                    .as_type_reference()
                    .target
                    .ref_(self)
                    .as_tuple_type()
                    .labeled_element_declarations
                    .as_deref(),
            )?
        } else {
            self.create_tuple_type(&[type_], Some(&[ElementFlags::Variadic]), None, None)?
        })
    }

    pub(super) fn get_spread_argument_type(
        &self,
        args: &[Id<Node /*Expression*/>],
        index: usize,
        arg_count: usize,
        rest_type: Id<Type>,
        context: Option<Gc<InferenceContext>>,
        check_mode: CheckMode,
    ) -> io::Result<Id<Type>> {
        if index >= arg_count - 1 {
            let arg = args[arg_count - 1];
            if self.is_spread_argument(Some(arg)) {
                return self.get_mutable_array_or_tuple_type(
                    if arg.ref_(self).kind() == SyntaxKind::SyntheticExpression {
                        arg.ref_(self).as_synthetic_expression().type_
                    } else {
                        self.check_expression_with_contextual_type(
                            arg.ref_(self).as_spread_element().expression,
                            rest_type,
                            context.clone(),
                            check_mode,
                        )?
                    },
                );
            }
        }
        let mut types: Vec<Id<Type>> = vec![];
        let mut flags: Vec<ElementFlags> = vec![];
        let mut names: Vec<Id<Node>> = vec![];
        for i in index..arg_count {
            let arg = args[i];
            if self.is_spread_argument(Some(arg)) {
                let spread_type = if arg.ref_(self).kind() == SyntaxKind::SyntheticExpression {
                    arg.ref_(self).as_synthetic_expression().type_
                } else {
                    self.check_expression(arg.ref_(self).as_spread_element().expression, None, None)?
                };
                if self.is_array_like_type(spread_type)? {
                    types.push(spread_type);
                    flags.push(ElementFlags::Variadic);
                } else {
                    types.push(self.check_iterated_type_or_element_type(
                        IterationUse::Spread,
                        spread_type,
                        self.undefined_type(),
                        Some(if arg.ref_(self).kind() == SyntaxKind::SpreadElement {
                            arg.ref_(self).as_spread_element().expression
                        } else {
                            arg
                        }),
                    )?);
                    flags.push(ElementFlags::Rest);
                }
            } else {
                let contextual_type = self.get_indexed_access_type(
                    rest_type,
                    self.get_number_literal_type(Number::new((i - index) as f64)),
                    Some(AccessFlags::Contextual),
                    Option::<Id<Node>>::None,
                    Option::<Id<Symbol>>::None,
                    None,
                )?;
                let arg_type = self.check_expression_with_contextual_type(
                    arg,
                    contextual_type,
                    context.clone(),
                    check_mode,
                )?;
                let has_primitive_contextual_type = self.maybe_type_of_kind(
                    contextual_type,
                    TypeFlags::Primitive
                        | TypeFlags::Index
                        | TypeFlags::TemplateLiteral
                        | TypeFlags::StringMapping,
                );
                types.push(if has_primitive_contextual_type {
                    self.get_regular_type_of_literal_type(arg_type)
                } else {
                    self.get_widened_literal_type(arg_type)?
                });
                flags.push(ElementFlags::Required);
            }
            if arg.ref_(self).kind() == SyntaxKind::SyntheticExpression {
                if let Some(arg_tuple_name_source) =
                    arg.ref_(self).as_synthetic_expression().tuple_name_source
                {
                    names.push(arg_tuple_name_source);
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
        signature: Id<Signature>,
        type_argument_nodes: &[Id<Node /*TypeNode*/>],
        report_errors: bool,
        head_message: Option<&'static DiagnosticMessage>,
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        let is_javascript = is_in_js_file(signature.ref_(self).declaration.refed(self).as_deref());
        let type_parameters = signature.ref_(self).maybe_type_parameters().clone().unwrap();
        let type_argument_types = self
            .fill_missing_type_arguments(
                Some(try_map(type_argument_nodes, |&node: &Id<Node>, _| {
                    self.get_type_from_type_node_(node)
                })?),
                Some(&type_parameters),
                self.get_min_type_argument_count(Some(&type_parameters)),
                is_javascript,
            )?
            .unwrap();
        let mut mapper: Option<Id<TypeMapper>> = None;
        for i in 0..type_argument_nodes.len() {
            Debug_.assert(
                type_parameters.get(i).is_some(),
                Some("Should not call checkTypeArguments with too many type arguments"),
            );
            let constraint = self.get_constraint_of_type_parameter(type_parameters[i])?;
            if let Some(constraint) = constraint {
                let error_info: Option<Gc<Box<dyn CheckTypeContainingMessageChain>>> =
                    if report_errors && head_message.is_some() {
                        Some(Gc::new(Box::new(CheckTypeArgumentsErrorInfo)))
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
                let type_argument = type_argument_types[i];
                if !self.check_type_assignable_to(
                    type_argument,
                    self.get_type_with_this_argument(
                        self.instantiate_type(constraint, mapper.clone())?,
                        Some(type_argument),
                        None,
                    )?,
                    if report_errors {
                        Some(type_argument_nodes[i])
                    } else {
                        None
                    },
                    Some(type_argument_head_message),
                    error_info.clone(),
                    None,
                )? {
                    return Ok(None);
                }
            }
        }
        Ok(Some(type_argument_types))
    }

    pub(super) fn get_jsx_reference_kind(
        &self,
        node: Id<Node>, /*JsxOpeningLikeElement*/
    ) -> io::Result<JsxReferenceKind> {
        let node_ref = node.ref_(self);
        let node_as_jsx_opening_like_element = node_ref.as_jsx_opening_like_element();
        if self.is_jsx_intrinsic_identifier(node_as_jsx_opening_like_element.tag_name()) {
            return Ok(JsxReferenceKind::Mixed);
        }
        let tag_type = self.get_apparent_type(self.check_expression(
            node_as_jsx_opening_like_element.tag_name(),
            None,
            None,
        )?)?;
        if length(Some(
            &self.get_signatures_of_type(tag_type, SignatureKind::Construct)?,
        )) > 0
        {
            return Ok(JsxReferenceKind::Component);
        }
        if length(Some(
            &self.get_signatures_of_type(tag_type, SignatureKind::Call)?,
        )) > 0
        {
            return Ok(JsxReferenceKind::Function);
        }
        Ok(JsxReferenceKind::Mixed)
    }

    pub(super) fn check_applicable_signature_for_jsx_opening_like_element(
        &self,
        node: Id<Node>, /*JsxOpeningLikeElement*/
        signature: Id<Signature>,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        check_mode: CheckMode,
        report_errors: bool,
        containing_message_chain: Option<Gc<Box<dyn CheckTypeContainingMessageChain>>>,
        error_output_container: Gc<Box<dyn CheckTypeErrorOutputContainer>>,
    ) -> io::Result<bool> {
        let param_type =
            self.get_effective_first_argument_for_jsx_signature(signature.clone(), node)?;
        let node_ref = node.ref_(self);
        let node_as_jsx_opening_like_element = node_ref.as_jsx_opening_like_element();
        let attributes_type = self.check_expression_with_contextual_type(
            node_as_jsx_opening_like_element.attributes(),
            param_type,
            None,
            check_mode,
        )?;
        Ok(self.check_tag_name_does_not_expect_too_many_arguments(
            node,
            report_errors,
            error_output_container.clone(),
        )? && self.check_type_related_to_and_optionally_elaborate(
            attributes_type,
            param_type,
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
        )?)
    }

    pub(super) fn check_tag_name_does_not_expect_too_many_arguments(
        &self,
        node: Id<Node>, /*JsxOpeningLikeElement*/
        report_errors: bool,
        error_output_container: Gc<Box<dyn CheckTypeErrorOutputContainer>>,
    ) -> io::Result<bool> {
        if self
            .get_jsx_namespace_container_for_implicit_import(Some(node))?
            .is_some()
        {
            return Ok(true);
        }
        let node_ref = node.ref_(self);
        let node_as_jsx_opening_like_element = node_ref.as_jsx_opening_like_element();
        let Some(tag_type) = (if is_jsx_opening_element(&node.ref_(self))
            || is_jsx_self_closing_element(&node.ref_(self))
                && !self.is_jsx_intrinsic_identifier(node_as_jsx_opening_like_element.tag_name())
        {
            Some(self.check_expression(node_as_jsx_opening_like_element.tag_name(), None, None)?)
        } else {
            None
        }) else {
            return Ok(true);
        };
        let tag_call_signatures = self.get_signatures_of_type(tag_type, SignatureKind::Call)?;
        if length(Some(&*tag_call_signatures)) == 0 {
            return Ok(true);
        }
        let Some(factory) = self.get_jsx_factory_entity(node) else {
            return Ok(true);
        };
        let Some(factory_symbol) = self.resolve_entity_name(
            factory,
            SymbolFlags::Value,
            Some(true),
            Some(false),
            Some(node),
        )? else {
            return Ok(true);
        };

        let factory_type = self.get_type_of_symbol(factory_symbol)?;
        let call_signatures = self.get_signatures_of_type(factory_type, SignatureKind::Call)?;
        if length(Some(&*call_signatures)) == 0 {
            return Ok(true);
        }

        let mut has_first_param_signatures = false;
        let mut max_param_count = 0;
        for &sig in &call_signatures {
            let firstparam = self.get_type_at_position(sig, 0)?;
            let signatures_of_param =
                self.get_signatures_of_type(firstparam, SignatureKind::Call)?;
            if length(Some(&*signatures_of_param)) == 0 {
                continue;
            }
            for &param_sig in &signatures_of_param {
                has_first_param_signatures = true;
                if self.has_effective_rest_parameter(param_sig)? {
                    return Ok(true);
                }
                let param_count = self.get_parameter_count(param_sig)?;
                if param_count > max_param_count {
                    max_param_count = param_count;
                }
            }
        }
        if !has_first_param_signatures {
            return Ok(true);
        }
        let mut absolute_min_arg_count = usize::MAX;
        for &tag_sig in &tag_call_signatures {
            let tag_required_arg_count = self.get_min_argument_count(tag_sig, None)?;
            if tag_required_arg_count < absolute_min_arg_count {
                absolute_min_arg_count = tag_required_arg_count;
            }
        }
        if absolute_min_arg_count <= max_param_count {
            return Ok(true);
        }

        if report_errors {
            let diag: Id<Diagnostic> = self.alloc_diagnostic(
                create_diagnostic_for_node(
                    node_as_jsx_opening_like_element.tag_name(),
                    &Diagnostics::Tag_0_expects_at_least_1_arguments_but_the_JSX_factory_2_provides_at_most_3,
                    Some(vec![
                        entity_name_to_string(
                            node_as_jsx_opening_like_element.tag_name(),
                            self,
                        ).into_owned(),
                        absolute_min_arg_count.to_string(),
                        entity_name_to_string(factory, self).into_owned(),
                        max_param_count.to_string(),
                    ]),
                    self,
                ).into()
            );
            let tag_name_declaration = self
                .get_symbol_at_location_(node_as_jsx_opening_like_element.tag_name(), None)?
                .and_then(|symbol| symbol.ref_(self).maybe_value_declaration());
            if let Some(tag_name_declaration) = tag_name_declaration {
                add_related_info(
                    &diag.ref_(self),
                    vec![self.alloc_diagnostic_related_information(
                        create_diagnostic_for_node(
                            tag_name_declaration,
                            &Diagnostics::_0_is_declared_here,
                            Some(vec![entity_name_to_string(
                                node_as_jsx_opening_like_element.tag_name(),
                                self,
                            )
                            .into_owned()]),
                            self,
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
        Ok(false)
    }

    pub(super) fn get_signature_applicability_error(
        &self,
        node: Id<Node>, /*CallLikeExpression*/
        args: &[Id<Node /*Expression*/>],
        signature: Id<Signature>,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        check_mode: CheckMode,
        report_errors: bool,
        containing_message_chain: Option<Gc<Box<dyn CheckTypeContainingMessageChain>>>,
    ) -> io::Result<Option<Vec<Id<Diagnostic>>>> {
        let error_output_container: Gc<Box<dyn CheckTypeErrorOutputContainer>> = Gc::new(Box::new(
            CheckTypeErrorOutputContainerConcrete::new(Some(true)),
        ));
        if is_jsx_opening_like_element(&node.ref_(self)) {
            if !self.check_applicable_signature_for_jsx_opening_like_element(
                node,
                signature.clone(),
                relation.clone(),
                check_mode,
                report_errors,
                containing_message_chain.clone(),
                error_output_container.clone(),
            )? {
                Debug_.assert(
                    !report_errors || error_output_container.errors_len() > 0,
                    Some("jsx should have errors when reporting errors"),
                );
                return Ok(Some(error_output_container.errors())) /*|| emptyArray*/;
            }
            return Ok(None);
        }
        let this_type = self.get_this_type_of_signature(signature)?;
        if let Some(this_type) = this_type.filter(|&this_type| {
            this_type != self.void_type() && node.ref_(self).kind() != SyntaxKind::NewExpression
        }) {
            let this_argument_node = self.get_this_argument_of_call(node);
            let this_argument_type = self.get_this_argument_type(this_argument_node)?;
            let error_node = if report_errors {
                Some(
                    this_argument_node
                        .unwrap_or(node),
                )
            } else {
                None
            };
            let head_message = &Diagnostics::The_this_context_of_type_0_is_not_assignable_to_method_s_this_of_type_1;
            if !self.check_type_related_to(
                this_argument_type,
                this_type,
                relation.clone(),
                error_node,
                Some(Cow::Borrowed(head_message)),
                containing_message_chain.clone(),
                Some(error_output_container.clone()),
            )? {
                Debug_.assert(
                    !report_errors || error_output_container.errors_len() > 0,
                    Some("this parameter should have errors when reporting errors"),
                );
                return Ok(Some(error_output_container.errors())) /*|| emptyArray*/;
            }
        }
        let head_message =
            &Diagnostics::Argument_of_type_0_is_not_assignable_to_parameter_of_type_1;
        let rest_type = self.get_non_array_rest_type(signature)?;
        let arg_count = if rest_type.is_some() {
            cmp::min(self.get_parameter_count(signature)? - 1, args.len())
        } else {
            args.len()
        };
        for i in 0..arg_count {
            let arg = args[i];
            if arg.ref_(self).kind() != SyntaxKind::OmittedExpression {
                let param_type = self.get_type_at_position(signature, i)?;
                let arg_type =
                    self.check_expression_with_contextual_type(arg, param_type, None, check_mode)?;
                let check_arg_type = if check_mode.intersects(CheckMode::SkipContextSensitive) {
                    self.get_regular_type_of_object_literal(arg_type)?
                } else {
                    arg_type.clone()
                };
                if !self.check_type_related_to_and_optionally_elaborate(
                    check_arg_type,
                    param_type,
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
                )? {
                    Debug_.assert(
                        !report_errors || error_output_container.errors_len() > 0,
                        Some("parameter should have errors when reporting errors"),
                    );
                    self.maybe_add_missing_await_info(
                        report_errors,
                        error_output_container.clone(),
                        relation.clone(),
                        Some(arg),
                        check_arg_type,
                        param_type,
                    )?;
                    return Ok(Some(error_output_container.errors())) /*|| emptyArray*/;
                }
            }
        }
        if let Some(rest_type) = rest_type {
            let spread_type = self.get_spread_argument_type(
                args,
                arg_count,
                args.len(),
                rest_type,
                None,
                check_mode,
            )?;
            let rest_arg_count = args.len() - arg_count;
            let error_node = if !report_errors {
                None
            } else if rest_arg_count == 0 {
                Some(node)
            } else if rest_arg_count == 1 {
                Some(args[arg_count])
            } else {
                let error_node = self.create_synthetic_expression(
                    node,
                    spread_type,
                    None,
                    Option::<Id<Node>>::None,
                );
                set_text_range_pos_end(
                    &*error_node.ref_(self),
                    args[arg_count].ref_(self).pos(),
                    args[args.len() - 1].ref_(self).end(),
                );
                Some(error_node)
            };
            if !self.check_type_related_to(
                spread_type,
                rest_type,
                relation.clone(),
                error_node.clone(),
                Some(Cow::Borrowed(head_message)),
                None,
                Some(error_output_container.clone()),
            )? {
                Debug_.assert(
                    !report_errors || error_output_container.errors_len() > 0,
                    Some("rest parameter should have errors when reporting errors"),
                );
                self.maybe_add_missing_await_info(
                    report_errors,
                    error_output_container.clone(),
                    relation.clone(),
                    error_node,
                    spread_type,
                    rest_type,
                )?;
                return Ok(Some(error_output_container.errors())) /*|| emptyArray*/;
            }
        }
        Ok(None)
    }
}

#[derive(Trace, Finalize)]
struct CheckTypeArgumentsErrorInfo;

impl CheckTypeContainingMessageChain for CheckTypeArgumentsErrorInfo {
    fn get(&self) -> io::Result<Option<Rc<RefCell<DiagnosticMessageChain>>>> {
        Ok(Some(Rc::new(RefCell::new(chain_diagnostic_messages(
            None,
            &Diagnostics::Type_0_does_not_satisfy_the_constraint_1,
            None,
        )))))
    }
}
