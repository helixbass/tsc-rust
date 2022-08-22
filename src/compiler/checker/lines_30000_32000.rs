#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::{
    signature_has_literal_types, signature_has_rest_parameter, CheckMode, MinArgumentCountFlags,
    TypeFacts, WideningKind,
};
use crate::{
    create_diagnostic_for_node, first, get_class_like_declaration_of_symbol, get_containing_class,
    get_effective_base_type_node, get_jsdoc_class_tag, get_object_flags, get_source_file_of_node,
    has_syntactic_modifier, is_call_chain, is_import_call, is_in_js_file, is_line_break,
    is_outermost_optional_chain, last, length, map_defined, min_and_max, skip_trivia,
    text_char_at_index, Debug_, DiagnosticRelatedInformation, Diagnostics, FunctionFlags,
    InferenceFlags, MinAndMax, ModifierFlags, ObjectFlags, ReadonlyTextRange, ScriptTarget,
    Signature, SignatureFlags, SignatureKind, SourceFileLike, UnionOrIntersectionTypeInterface,
    UnionReduction, __String, get_function_flags, has_initializer, Node, NodeInterface, Symbol,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
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
                candidate.clone(),
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
        candidate: Rc<Signature>,
        args: &[Rc<Node /*Expression*/>],
    ) -> Rc<Signature> {
        let inference_context = self.create_inference_context(
            type_parameters,
            Some(candidate.clone()),
            if is_in_js_file(Some(node)) {
                InferenceFlags::AnyDefault
            } else {
                InferenceFlags::None
            },
            None,
        );
        let type_argument_types = self.infer_type_arguments(
            node,
            candidate.clone(),
            args,
            CheckMode::SkipContextSensitive | CheckMode::SkipGenericFunctions,
            &inference_context,
        );
        Rc::new(self.create_signature_instantiation(candidate, Some(&type_argument_types)))
    }

    pub(super) fn get_longest_candidate_index(
        &self,
        candidates: &[Rc<Signature>],
        args_count: usize,
    ) -> usize {
        let mut max_params_index: Option<usize> = None;
        let mut max_params: Option<usize> = None;

        for i in 0..candidates.len() {
            let candidate = &candidates[i];
            let param_count = self.get_parameter_count(candidate);
            if self.has_effective_rest_parameter(candidate) || param_count >= args_count {
                return i;
            }
            if match max_params {
                None => true,
                Some(max_params) => param_count > max_params,
            } {
                max_params = Some(param_count);
                max_params_index = Some(i);
            }
        }

        max_params_index.unwrap()
    }

    pub(super) fn resolve_call_expression(
        &self,
        node: &Node, /*CallExpression*/
        candidates_out_array: Option<&mut Vec<Rc<Signature>>>,
        check_mode: CheckMode,
    ) -> Rc<Signature> {
        let node_as_call_expression = node.as_call_expression();
        if node_as_call_expression.expression.kind() == SyntaxKind::SuperKeyword {
            let super_type = self.check_super_expression(&node_as_call_expression.expression);
            if self.is_type_any(Some(&*super_type)) {
                for arg in &node_as_call_expression.arguments {
                    self.check_expression(arg, None, None);
                }
                return self.any_signature();
            }
            if !self.is_error_type(&super_type) {
                let base_type_node =
                    get_effective_base_type_node(&get_containing_class(node).unwrap());
                if let Some(base_type_node) = base_type_node.as_ref() {
                    let base_constructors = self.get_instantiated_constructors_for_type_arguments(
                        &super_type,
                        base_type_node
                            .as_expression_with_type_arguments()
                            .type_arguments
                            .as_deref(),
                        base_type_node,
                    );
                    return self.resolve_call(
                        node,
                        &base_constructors,
                        candidates_out_array,
                        check_mode,
                        SignatureFlags::None,
                        None,
                    );
                }
            }
            return self.resolve_untyped_call(node);
        }

        let call_chain_flags: SignatureFlags;
        let mut func_type = self.check_expression(&node_as_call_expression.expression, None, None);
        if is_call_chain(node) {
            let non_optional_type =
                self.get_optional_expression_type(&func_type, &node_as_call_expression.expression);
            call_chain_flags = if Rc::ptr_eq(&non_optional_type, &func_type) {
                SignatureFlags::None
            } else if is_outermost_optional_chain(node) {
                SignatureFlags::IsOuterCallChain
            } else {
                SignatureFlags::IsInnerCallChain
            };
            func_type = non_optional_type;
        } else {
            call_chain_flags = SignatureFlags::None;
        }

        func_type = self.check_non_null_type_with_reporter(
            &func_type,
            &node_as_call_expression.expression,
            |node: &Node, flags: TypeFlags| {
                self.report_cannot_invoke_possibly_null_or_undefined_error(node, flags)
            },
        );

        if Rc::ptr_eq(&func_type, &self.silent_never_type()) {
            return self.silent_never_signature();
        }

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
            if !self.is_error_type(&func_type) && node_as_call_expression.type_arguments.is_some() {
                self.error(
                    Some(node),
                    &Diagnostics::Untyped_function_calls_may_not_accept_type_arguments,
                    None,
                );
            }
            return self.resolve_untyped_call(node);
        }
        if call_signatures.is_empty() {
            if num_construct_signatures > 0 {
                self.error(
                    Some(node),
                    &Diagnostics::Value_of_type_0_is_not_callable_Did_you_mean_to_include_new,
                    Some(vec![self.type_to_string_(
                        &func_type,
                        Option::<&Node>::None,
                        None,
                        None,
                    )]),
                );
            } else {
                let mut related_information: Option<Rc<DiagnosticRelatedInformation>> = None;
                if node_as_call_expression.arguments.len() == 1 {
                    let source_file = get_source_file_of_node(Some(node)).unwrap();
                    let text = source_file.as_source_file().text_as_chars();
                    if is_line_break(text_char_at_index(
                        &text,
                        TryInto::<usize>::try_into(skip_trivia(
                            &text,
                            node_as_call_expression.expression.end(),
                            Some(true),
                            None,
                            None,
                        ))
                        .unwrap()
                            - 1,
                    )) {
                        related_information = Some(Rc::new(
                            create_diagnostic_for_node(
                                &node_as_call_expression.expression,
                                &Diagnostics::Are_you_missing_a_semicolon,
                                None,
                            )
                            .into(),
                        ));
                    }
                }
                self.invocation_error(
                    &node_as_call_expression.expression,
                    &apparent_type,
                    SignatureKind::Call,
                    related_information,
                );
            }
            return self.resolve_error_call(node);
        }
        if check_mode.intersects(CheckMode::SkipGenericFunctions)
            && node_as_call_expression.type_arguments.is_none()
            && call_signatures.iter().any(|call_signature| {
                self.is_generic_function_returning_function(call_signature.clone())
            })
        {
            self.skipped_generic_function(node, check_mode);
            return self.resolving_signature();
        }
        if call_signatures.iter().any(|sig| {
            is_in_js_file(sig.declaration.as_deref())
                && get_jsdoc_class_tag(sig.declaration.as_ref().unwrap()).is_some()
        }) {
            self.error(
                Some(node),
                &Diagnostics::Value_of_type_0_is_not_callable_Did_you_mean_to_include_new,
                Some(vec![self.type_to_string_(
                    &func_type,
                    Option::<&Node>::None,
                    None,
                    None,
                )]),
            );
            return self.resolve_error_call(node);
        }

        self.resolve_call(
            node,
            &call_signatures,
            candidates_out_array,
            check_mode,
            call_chain_flags,
            None,
        )
    }

    pub(super) fn is_generic_function_returning_function(&self, signature: Rc<Signature>) -> bool {
        signature.type_parameters.is_some()
            && self.is_function_type(&self.get_return_type_of_signature(signature))
    }

    pub(super) fn is_untyped_function_call(
        &self,
        func_type: &Type,
        apparent_func_type: &Type,
        num_call_signatures: usize,
        num_construct_signatures: usize,
    ) -> bool {
        self.is_type_any(Some(func_type))
            || self.is_type_any(Some(apparent_func_type))
                && func_type.flags().intersects(TypeFlags::TypeParameter)
            || num_call_signatures == 0
                && num_construct_signatures == 0
                && !apparent_func_type.flags().intersects(TypeFlags::Union)
                && !self
                    .get_reduced_type(apparent_func_type)
                    .flags()
                    .intersects(TypeFlags::Never)
                && self.is_type_assignable_to(func_type, &self.global_function_type())
    }

    pub(super) fn resolve_new_expression(
        &self,
        node: &Node, /*NewExpression*/
        candidates_out_array: Option<&mut Vec<Rc<Signature>>>,
        check_mode: CheckMode,
    ) -> Rc<Signature> {
        let node_as_new_expression = node.as_new_expression();
        if let Some(node_arguments) = node_as_new_expression.arguments.as_ref() {
            if self.language_version < ScriptTarget::ES5 {
                let spread_index = self.get_spread_argument_index(node_arguments);
                if let Some(spread_index) = spread_index {
                    self.error(
                        Some(&*node_arguments[spread_index]),
                        &Diagnostics::Spread_operator_in_new_expressions_is_only_available_when_targeting_ECMAScript_5_and_higher,
                        None,
                    );
                }
            }
        }

        let mut expression_type =
            self.check_non_null_expression(&node_as_new_expression.expression);
        if Rc::ptr_eq(&expression_type, &self.silent_never_type()) {
            return self.silent_never_signature();
        }

        expression_type = self.get_apparent_type(&expression_type);
        if self.is_error_type(&expression_type) {
            return self.resolve_error_call(node);
        }

        if self.is_type_any(Some(&*expression_type)) {
            if node_as_new_expression.type_arguments.is_some() {
                self.error(
                    Some(node),
                    &Diagnostics::Untyped_function_calls_may_not_accept_type_arguments,
                    None,
                );
            }
            return self.resolve_untyped_call(node);
        }

        let construct_signatures =
            self.get_signatures_of_type(&expression_type, SignatureKind::Construct);
        if !construct_signatures.is_empty() {
            if !self.is_constructor_accessible(node, &construct_signatures[0]) {
                return self.resolve_error_call(node);
            }
            if construct_signatures
                .iter()
                .any(|signature| signature.flags.intersects(SignatureFlags::Abstract))
            {
                self.error(
                    Some(node),
                    &Diagnostics::Cannot_create_an_instance_of_an_abstract_class,
                    None,
                );
                return self.resolve_error_call(node);
            }
            let value_decl =
                expression_type
                    .maybe_symbol()
                    .as_ref()
                    .and_then(|expression_type_symbol| {
                        get_class_like_declaration_of_symbol(expression_type_symbol)
                    });
            if let Some(value_decl) = value_decl
                .as_ref()
                .filter(|value_decl| has_syntactic_modifier(value_decl, ModifierFlags::Abstract))
            {
                self.error(
                    Some(node),
                    &Diagnostics::Cannot_create_an_instance_of_an_abstract_class,
                    None,
                );
                return self.resolve_error_call(node);
            }

            return self.resolve_call(
                node,
                &construct_signatures,
                candidates_out_array,
                check_mode,
                SignatureFlags::None,
                None,
            );
        }
        let call_signatures = self.get_signatures_of_type(&expression_type, SignatureKind::Call);
        if !call_signatures.is_empty() {
            let signature = self.resolve_call(
                node,
                &call_signatures,
                candidates_out_array,
                check_mode,
                SignatureFlags::None,
                None,
            );
            if !self.no_implicit_any {
                if matches!(
                    signature.declaration.as_ref(),
                    Some(signature_declaration) if !self.is_js_constructor(Some(&**signature_declaration)) &&
                        !Rc::ptr_eq(
                            &self.get_return_type_of_signature(signature.clone()),
                            &self.void_type()
                        )
                ) {
                    self.error(
                        Some(node),
                        &Diagnostics::Only_a_void_function_can_be_called_with_the_new_keyword,
                        None,
                    );
                }
                if matches!(
                    self.get_this_type_of_signature(&signature).as_ref(),
                    Some(this_type) if Rc::ptr_eq(
                        this_type,
                        &self.void_type()
                    )
                ) {
                    self.error(
                        Some(node),
                        &Diagnostics::A_function_that_is_called_with_the_new_keyword_cannot_have_a_this_type_that_is_void,
                        None,
                    );
                }
            }
            return signature;
        }

        self.invocation_error(
            &node_as_new_expression.expression,
            &expression_type,
            SignatureKind::Construct,
            None,
        );
        self.resolve_error_call(node)
    }

    pub(super) fn type_has_protected_accessible_base(
        &self,
        target: &Symbol,
        type_: &Type, /*InterfaceType*/
    ) -> bool {
        let base_types = self.get_base_types(type_);
        if length(Some(&base_types)) == 0 {
            return false;
        }
        let first_base = &base_types[0];
        if first_base.flags().intersects(TypeFlags::Intersection) {
            let types = first_base.as_intersection_type().types();
            let mixin_flags = self.find_mixins(types);
            let mut i = 0;
            for intersection_member in first_base.as_intersection_type().types() {
                if !mixin_flags[i] {
                    if get_object_flags(intersection_member)
                        .intersects(ObjectFlags::Class | ObjectFlags::Interface)
                    {
                        if matches!(
                            intersection_member.maybe_symbol().as_ref(),
                            Some(intersection_member_symbol) if ptr::eq(
                                &**intersection_member_symbol,
                                target
                            )
                        ) {
                            return true;
                        }
                        if self.type_has_protected_accessible_base(target, intersection_member) {
                            return true;
                        }
                    }
                }
                i += 1;
            }
            return false;
        }
        if matches!(
            first_base.maybe_symbol().as_ref(),
            Some(first_base_symbol) if ptr::eq(
                &**first_base_symbol,
                target
            )
        ) {
            return true;
        }
        self.type_has_protected_accessible_base(target, first_base)
    }

    pub(super) fn is_constructor_accessible(
        &self,
        node: &Node, /*NewExpression*/
        signature: &Signature,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn invocation_error(
        &self,
        error_target: &Node,
        apparent_type: &Type,
        kind: SignatureKind,
        related_information: Option<Rc<DiagnosticRelatedInformation>>,
    ) {
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
