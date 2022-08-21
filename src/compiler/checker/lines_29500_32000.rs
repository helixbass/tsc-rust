#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, CheckTypeErrorOutputContainer, MinArgumentCountFlags,
    TypeFacts, WideningKind,
};
use crate::{
    add_related_info, create_diagnostic_for_node, for_each, get_error_span_for_node,
    get_source_file_of_node, is_access_expression, is_import_call, is_jsx_opening_element,
    is_jsx_opening_like_element, is_property_access_expression, parse_base_node_factory,
    parse_node_factory, set_parent, set_text_range, skip_outer_expressions, Debug_, Diagnostics,
    ElementFlags, FunctionFlags, ReadonlyTextRange, RelationComparisonResult, ScriptTarget,
    Signature, SignatureFlags, UnionReduction, __String, get_function_flags, has_initializer, Node,
    NodeInterface, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface,
};

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
