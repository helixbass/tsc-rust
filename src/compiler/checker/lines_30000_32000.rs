#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use super::{
    signature_has_literal_types, signature_has_rest_parameter, CheckMode, MinArgumentCountFlags,
    TypeFacts, WideningKind,
};
use crate::{
    first, is_import_call, is_in_js_file, last, map_defined, min_and_max, Debug_, Diagnostics,
    FunctionFlags, MinAndMax, Signature, SignatureFlags, UnionReduction, __String,
    get_function_flags, has_initializer, Node, NodeInterface, Symbol, SymbolInterface, SyntaxKind,
    Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_candidate_for_overload_failure(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates: &mut Vec<Rc<Signature>>,
        args: &[Rc<Node /*Expression*/>],
        has_candidates_out_array: bool,
    ) -> Rc<Signature> {
        Debug_.assert(!candidates.is_empty(), None);
        self.check_node_deferred(node);
        if has_candidates_out_array
            || candidates.len() == 1
            || candidates.into_iter().any(|c| c.type_parameters.is_some())
        {
            self.pick_longest_candidate_signature(node, candidates, args)
        } else {
            self.create_union_of_signatures_for_overload_failure(candidates)
        }
    }

    pub(super) fn create_union_of_signatures_for_overload_failure(
        &self,
        candidates: &[Rc<Signature>],
    ) -> Rc<Signature> {
        let this_parameters = map_defined(Some(candidates), |c: &Rc<Signature>, _| {
            c.this_parameter.clone()
        });
        let mut this_parameter: Option<Rc<Symbol>> = None;
        if !this_parameters.is_empty() {
            this_parameter = Some(
                self.create_combined_symbol_from_types(
                    &this_parameters,
                    &this_parameters
                        .iter()
                        .map(|parameter: &Rc<Symbol>| self.get_type_of_parameter(parameter))
                        .collect::<Vec<_>>(),
                ),
            );
        }
        let MinAndMax {
            min: min_argument_count,
            max: max_non_rest_param,
        } = min_and_max(candidates, |candidate: &Rc<Signature>| {
            self.get_num_non_rest_parameters(candidate)
        });
        let mut parameters: Vec<Rc<Symbol>> = vec![];
        for i in 0..max_non_rest_param {
            let symbols = map_defined(Some(candidates), |s: &Rc<Signature>, _| {
                if signature_has_rest_parameter(s) {
                    if i < s.parameters().len() - 1 {
                        Some(s.parameters()[i].clone())
                    } else {
                        Some(last(s.parameters()).clone())
                    }
                } else {
                    if i < s.parameters().len() {
                        Some(s.parameters()[i].clone())
                    } else {
                        None
                    }
                }
            });
            Debug_.assert(!symbols.is_empty(), None);
            parameters.push(self.create_combined_symbol_from_types(
                &symbols,
                &map_defined(Some(candidates), |candidate: &Rc<Signature>, _| {
                    self.try_get_type_at_position(candidate, i)
                }),
            ));
        }
        let rest_parameter_symbols = map_defined(Some(candidates), |c: &Rc<Signature>, _| {
            if signature_has_rest_parameter(c) {
                Some(last(c.parameters()).clone())
            } else {
                None
            }
        });
        let mut flags = SignatureFlags::None;
        if !rest_parameter_symbols.is_empty() {
            let type_ = self.create_array_type(
                &self.get_union_type(
                    map_defined(Some(candidates), |candidate: &Rc<Signature>, _| {
                        self.try_get_rest_type_of_signature(candidate)
                    }),
                    Some(UnionReduction::Subtype),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                ),
                None,
            );
            parameters.push(
                self.create_combined_symbol_for_overload_failure(&rest_parameter_symbols, &type_),
            );
            flags |= SignatureFlags::HasRestParameter;
        }
        if candidates
            .into_iter()
            .any(|candidate: &Rc<Signature>| signature_has_literal_types(candidate))
        {
            flags |= SignatureFlags::HasLiteralTypes;
        }
        Rc::new(
            self.create_signature(
                candidates[0].declaration.clone(),
                None,
                this_parameter,
                parameters,
                Some(
                    self.get_intersection_type(
                        &candidates
                            .into_iter()
                            .map(|candidate| self.get_return_type_of_signature(candidate.clone()))
                            .collect::<Vec<_>>(),
                        Option::<&Symbol>::None,
                        None,
                    ),
                ),
                None,
                min_argument_count,
                flags,
            ),
        )
    }

    pub(super) fn get_num_non_rest_parameters(&self, signature: &Signature) -> usize {
        let num_params = signature.parameters().len();
        if signature_has_rest_parameter(signature) {
            num_params - 1
        } else {
            num_params
        }
    }

    pub(super) fn create_combined_symbol_from_types(
        &self,
        sources: &[Rc<Symbol>],
        types: &[Rc<Type>],
    ) -> Rc<Symbol> {
        self.create_combined_symbol_for_overload_failure(
            sources,
            &self.get_union_type(
                types.to_owned(),
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            ),
        )
    }

    pub(super) fn create_combined_symbol_for_overload_failure(
        &self,
        sources: &[Rc<Symbol>],
        type_: &Type,
    ) -> Rc<Symbol> {
        self.create_symbol_with_type(&*first(sources), Some(type_))
    }

    pub(super) fn pick_longest_candidate_signature(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates: &mut Vec<Rc<Signature>>,
        args: &[Rc<Node /*Expression*/>],
    ) -> Rc<Signature> {
        let best_index = self.get_longest_candidate_index(
            candidates,
            match self.apparent_argument_count() {
                None => args.len(),
                Some(apparent_argument_count) => apparent_argument_count,
            },
        );
        let candidate = &candidates[best_index];
        let type_parameters = candidate.type_parameters.as_ref();
        if type_parameters.is_none() {
            return candidate.clone();
        }
        let type_parameters = type_parameters.unwrap();

        let type_argument_nodes = if self.call_like_expression_may_have_type_arguments(node) {
            node.as_has_type_arguments().maybe_type_arguments()
        } else {
            None
        };
        let instantiated = if let Some(type_argument_nodes) = type_argument_nodes {
            Rc::new(self.create_signature_instantiation(
                candidate.clone(),
                Some(&self.get_type_arguments_from_nodes(
                    type_argument_nodes,
                    type_parameters,
                    is_in_js_file(Some(node)),
                )),
            ))
        } else {
            self.infer_signature_instantiation_for_overload_failure(
                node,
                type_parameters,
                candidate,
                args,
            )
        };
        candidates[best_index] = instantiated.clone();
        instantiated
    }

    pub(super) fn get_type_arguments_from_nodes(
        &self,
        type_argument_nodes: &[Rc<Node /*TypeNode*/>],
        type_parameters: &[Rc<Type /*TypeParameter*/>],
        is_js: bool,
    ) -> Vec<Rc<Type>> {
        let mut type_arguments = type_argument_nodes
            .into_iter()
            .map(|type_argument_node| self.get_type_of_node(type_argument_node))
            .collect::<Vec<_>>();
        while type_arguments.len() > type_parameters.len() {
            type_arguments.pop();
        }
        while type_arguments.len() < type_parameters.len() {
            type_arguments.push(
                self.get_constraint_of_type_parameter(&type_parameters[type_arguments.len()])
                    .unwrap_or_else(|| self.get_default_type_argument_type(is_js)),
            );
        }
        type_arguments
    }

    pub(super) fn infer_signature_instantiation_for_overload_failure(
        &self,
        node: &Node, /*CallLikeExpression*/
        type_parameters: &[Rc<Type /*TypeParameter*/>],
        candidate: &Signature,
        args: &[Rc<Node /*Expression*/>],
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn get_longest_candidate_index(
        &self,
        candidates: &[Rc<Signature>],
        args_count: usize,
    ) -> usize {
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
