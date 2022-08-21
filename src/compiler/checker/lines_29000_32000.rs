#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cmp;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, MinArgumentCountFlags, TypeFacts, WideningKind,
};
use crate::{
    find_index, is_import_call, is_in_js_file, is_jsx_opening_like_element, is_optional_chain,
    is_optional_chain_root, last, length, node_is_missing, some, Debug_, Diagnostics,
    FunctionFlags, InferenceFlags, InferencePriority, JsxReferenceKind, ReadonlyTextRange,
    Signature, SignatureFlags, SignatureKind, TypeComparer, UnionReduction, __String,
    get_function_flags, has_initializer, InferenceContext, Node, NodeInterface, Symbol,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
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
        type_arguments: Option<&[Rc<Node>] /*NodeArray<TypeNode>*/>,
    ) -> bool {
        let num_type_parameters = length(signature.type_parameters.as_deref());
        let min_type_argument_count =
            self.get_min_type_argument_count(signature.type_parameters.as_deref());
        !some(type_arguments, Option::<fn(&Rc<Node>) -> bool>::None) || {
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
            signature.type_parameters.as_deref().unwrap(),
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

    pub(super) fn get_spread_argument_type<TContext: Borrow<InferenceContext>>(
        &self,
        args: &[Rc<Node /*Expression*/>],
        index: usize,
        arg_count: usize,
        rest_type: &Type,
        context: Option<TContext>,
        check_mode: CheckMode,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_jsx_reference_kind(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> JsxReferenceKind {
        unimplemented!()
    }

    pub(super) fn get_effective_call_arguments(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> Vec<Rc<Node /*Expression*/>> {
        unimplemented!()
    }

    pub(super) fn get_decorator_argument_count(
        &self,
        node: &Node, /*Decorator*/
        signature: &Signature,
    ) -> usize {
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
