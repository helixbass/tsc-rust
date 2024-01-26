use std::{convert::TryInto, io, ptr};

use gc::Gc;
use id_arena::Id;

use super::{signature_has_literal_types, signature_has_rest_parameter, CheckMode};
use crate::{
    chain_diagnostic_messages, create_diagnostic_for_node, first,
    get_class_like_declaration_of_symbol, get_containing_class, get_effective_base_type_node,
    get_jsdoc_class_tag, get_object_flags, get_selected_effective_modifier_flags,
    get_source_file_of_node, has_syntactic_modifier, is_call_chain, is_call_expression,
    is_in_js_file, is_line_break, is_outermost_optional_chain, last, length, map_defined,
    min_and_max, skip_trivia, text_char_at_index, try_map_defined, AsDoubleDeref, Debug_,
    DiagnosticMessage, DiagnosticMessageChain, DiagnosticRelatedInformation, Diagnostics, HasArena,
    HasTypeArgumentsInterface, InArena, InferenceFlags, IteratorExt, Matches, MinAndMax,
    ModifierFlags, Node, NodeInterface, ObjectFlags, ReadonlyTextRange, ScriptTarget, Signature,
    SignatureFlags, SignatureKind, SourceFileLike, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, UnionOrIntersectionTypeInterface,
    UnionReduction,
    OptionInArena,
};

impl TypeChecker {
    pub(super) fn get_candidate_for_overload_failure(
        &self,
        node: Id<Node>, /*CallLikeExpression*/
        candidates: &mut Vec<Id<Signature>>,
        args: &[Id<Node /*Expression*/>],
        has_candidates_out_array: bool,
    ) -> io::Result<Id<Signature>> {
        Debug_.assert(!candidates.is_empty(), None);
        self.check_node_deferred(node);
        Ok(
            if has_candidates_out_array
                || candidates.len() == 1
                || candidates
                    .into_iter()
                    .any(|c| c.ref_(self).maybe_type_parameters().is_some())
            {
                self.pick_longest_candidate_signature(node, candidates, args)?
            } else {
                self.create_union_of_signatures_for_overload_failure(candidates)?
            },
        )
    }

    pub(super) fn create_union_of_signatures_for_overload_failure(
        &self,
        candidates: &[Id<Signature>],
    ) -> io::Result<Id<Signature>> {
        let this_parameters = map_defined(Some(candidates), |c: &Id<Signature>, _| {
            c.ref_(self).maybe_this_parameter().clone()
        });
        let mut this_parameter: Option<Id<Symbol>> = None;
        if !this_parameters.is_empty() {
            this_parameter = Some(
                self.create_combined_symbol_from_types(
                    &this_parameters,
                    &this_parameters
                        .iter()
                        .map(|&parameter: &Id<Symbol>| self.get_type_of_parameter(parameter))
                        .collect::<Result<Vec<_>, _>>()?,
                )?,
            );
        }
        let MinAndMax {
            min: min_argument_count,
            max: max_non_rest_param,
        } = min_and_max(candidates, |&candidate: &Id<Signature>| {
            self.get_num_non_rest_parameters(candidate)
        });
        let mut parameters: Vec<Id<Symbol>> = vec![];
        for i in 0..max_non_rest_param {
            let symbols = map_defined(Some(candidates), |&s: &Id<Signature>, _| {
                if signature_has_rest_parameter(&s.ref_(self)) {
                    if i < s.ref_(self).parameters().len() - 1 {
                        Some(s.ref_(self).parameters()[i].clone())
                    } else {
                        Some(last(s.ref_(self).parameters()).clone())
                    }
                } else {
                    if i < s.ref_(self).parameters().len() {
                        Some(s.ref_(self).parameters()[i].clone())
                    } else {
                        None
                    }
                }
            });
            Debug_.assert(!symbols.is_empty(), None);
            parameters.push(self.create_combined_symbol_from_types(
                &symbols,
                &try_map_defined(Some(candidates), |&candidate: &Id<Signature>, _| {
                    self.try_get_type_at_position(candidate, i)
                })?,
            )?);
        }
        let rest_parameter_symbols = map_defined(Some(candidates), |&c: &Id<Signature>, _| {
            if signature_has_rest_parameter(&c.ref_(self)) {
                Some(last(c.ref_(self).parameters()).clone())
            } else {
                None
            }
        });
        let mut flags = SignatureFlags::None;
        if !rest_parameter_symbols.is_empty() {
            let type_ = self.create_array_type(
                self.get_union_type(
                    &try_map_defined(Some(candidates), |&candidate: &Id<Signature>, _| {
                        self.try_get_rest_type_of_signature(candidate)
                    })?,
                    Some(UnionReduction::Subtype),
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?,
                None,
            );
            parameters.push(
                self.create_combined_symbol_for_overload_failure(&rest_parameter_symbols, type_),
            );
            flags |= SignatureFlags::HasRestParameter;
        }
        if candidates
            .into_iter()
            .any(|&candidate: &Id<Signature>| signature_has_literal_types(&candidate.ref_(self)))
        {
            flags |= SignatureFlags::HasLiteralTypes;
        }
        Ok(self.alloc_signature(
            self.create_signature(
                candidates[0].ref_(self).declaration.clone(),
                None,
                this_parameter,
                parameters,
                Some(
                    self.get_intersection_type(
                        &candidates
                            .into_iter()
                            .map(|candidate| self.get_return_type_of_signature(candidate.clone()))
                            .collect::<Result<Vec<_>, _>>()?,
                        Option::<Id<Symbol>>::None,
                        None,
                    )?,
                ),
                None,
                min_argument_count,
                flags,
            ),
        ))
    }

    pub(super) fn get_num_non_rest_parameters(&self, signature: Id<Signature>) -> usize {
        let num_params = signature.ref_(self).parameters().len();
        if signature_has_rest_parameter(&signature.ref_(self)) {
            num_params - 1
        } else {
            num_params
        }
    }

    pub(super) fn create_combined_symbol_from_types(
        &self,
        sources: &[Id<Symbol>],
        types: &[Id<Type>],
    ) -> io::Result<Id<Symbol>> {
        Ok(self.create_combined_symbol_for_overload_failure(
            sources,
            self.get_union_type(
                types,
                Some(UnionReduction::Subtype),
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?,
        ))
    }

    pub(super) fn create_combined_symbol_for_overload_failure(
        &self,
        sources: &[Id<Symbol>],
        type_: Id<Type>,
    ) -> Id<Symbol> {
        self.create_symbol_with_type(*first(sources), Some(type_))
    }

    pub(super) fn pick_longest_candidate_signature(
        &self,
        node: Id<Node>, /*CallLikeExpression*/
        candidates: &mut Vec<Id<Signature>>,
        args: &[Id<Node /*Expression*/>],
    ) -> io::Result<Id<Signature>> {
        let best_index = self.get_longest_candidate_index(
            candidates,
            match self.apparent_argument_count() {
                None => args.len(),
                Some(apparent_argument_count) => apparent_argument_count,
            },
        )?;
        let candidate = &candidates[best_index];
        let type_parameters = candidate.ref_(self).maybe_type_parameters().clone();
        if type_parameters.is_none() {
            return Ok(candidate.clone());
        }
        let type_parameters = type_parameters.unwrap();
        let type_parameters = &type_parameters;

        let type_argument_nodes = if self.call_like_expression_may_have_type_arguments(node) {
            node.ref_(self).as_has_type_arguments().maybe_type_arguments().clone()
        } else {
            None
        };
        let instantiated = if let Some(type_argument_nodes) = type_argument_nodes.as_ref() {
            self.alloc_signature(self.create_signature_instantiation(
                candidate.clone(),
                Some(&*self.get_type_arguments_from_nodes(
                    type_argument_nodes,
                    type_parameters,
                    is_in_js_file(Some(&node.ref_(self))),
                )?),
            )?)
        } else {
            self.infer_signature_instantiation_for_overload_failure(
                node,
                type_parameters,
                candidate.clone(),
                args,
            )?
        };
        candidates[best_index] = instantiated.clone();
        Ok(instantiated)
    }

    pub(super) fn get_type_arguments_from_nodes(
        &self,
        type_argument_nodes: &[Id<Node /*TypeNode*/>],
        type_parameters: &[Id<Type /*TypeParameter*/>],
        is_js: bool,
    ) -> io::Result<Vec<Id<Type>>> {
        let mut type_arguments = type_argument_nodes
            .into_iter()
            .map(|&type_argument_node| self.get_type_of_node(type_argument_node))
            .collect::<Result<Vec<_>, _>>()?;
        while type_arguments.len() > type_parameters.len() {
            type_arguments.pop();
        }
        while type_arguments.len() < type_parameters.len() {
            type_arguments.push(
                self.get_constraint_of_type_parameter(type_parameters[type_arguments.len()])?
                    .unwrap_or_else(|| self.get_default_type_argument_type(is_js)),
            );
        }
        Ok(type_arguments)
    }

    pub(super) fn infer_signature_instantiation_for_overload_failure(
        &self,
        node: Id<Node>, /*CallLikeExpression*/
        type_parameters: &[Id<Type /*TypeParameter*/>],
        candidate: Id<Signature>,
        args: &[Id<Node /*Expression*/>],
    ) -> io::Result<Id<Signature>> {
        let inference_context = self.create_inference_context(
            type_parameters,
            Some(candidate.clone()),
            if is_in_js_file(Some(&node.ref_(self))) {
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
            inference_context,
        )?;
        Ok(self.alloc_signature(self.create_signature_instantiation(
            candidate,
            Some(&type_argument_types),
        )?))
    }

    pub(super) fn get_longest_candidate_index(
        &self,
        candidates: &[Id<Signature>],
        args_count: usize,
    ) -> io::Result<usize> {
        let mut max_params_index: Option<usize> = None;
        let mut max_params: Option<usize> = None;

        for i in 0..candidates.len() {
            let candidate = candidates[i];
            let param_count = self.get_parameter_count(candidate)?;
            if self.has_effective_rest_parameter(candidate)? || param_count >= args_count {
                return Ok(i);
            }
            if match max_params {
                None => true,
                Some(max_params) => param_count > max_params,
            } {
                max_params = Some(param_count);
                max_params_index = Some(i);
            }
        }

        Ok(max_params_index.unwrap())
    }

    pub(super) fn resolve_call_expression(
        &self,
        node: Id<Node>, /*CallExpression*/
        candidates_out_array: Option<&mut Vec<Id<Signature>>>,
        check_mode: CheckMode,
    ) -> io::Result<Id<Signature>> {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        if node_as_call_expression.expression.ref_(self).kind() == SyntaxKind::SuperKeyword {
            let super_type = self.check_super_expression(node_as_call_expression.expression)?;
            if self.is_type_any(Some(super_type)) {
                for &arg in &node_as_call_expression.arguments {
                    self.check_expression(arg, None, None)?;
                }
                return Ok(self.any_signature());
            }
            if !self.is_error_type(super_type) {
                let base_type_node =
                    get_effective_base_type_node(get_containing_class(node, self).unwrap(), self);
                if let Some(base_type_node) = base_type_node {
                    let base_constructors = self.get_instantiated_constructors_for_type_arguments(
                        super_type,
                        base_type_node
                            .ref_(self).as_expression_with_type_arguments()
                            .maybe_type_arguments()
                            .as_double_deref(),
                        base_type_node,
                    )?;
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
        let mut func_type =
            self.check_expression(node_as_call_expression.expression, None, None)?;
        if is_call_chain(&node.ref_(self)) {
            let non_optional_type =
                self.get_optional_expression_type(func_type, node_as_call_expression.expression)?;
            call_chain_flags = if non_optional_type == func_type {
                SignatureFlags::None
            } else if is_outermost_optional_chain(node, self) {
                SignatureFlags::IsOuterCallChain
            } else {
                SignatureFlags::IsInnerCallChain
            };
            func_type = non_optional_type;
        } else {
            call_chain_flags = SignatureFlags::None;
        }

        func_type = self.check_non_null_type_with_reporter(
            func_type,
            node_as_call_expression.expression,
            |node: Id<Node>, flags: TypeFlags| {
                self.report_cannot_invoke_possibly_null_or_undefined_error(node, flags)
            },
        )?;

        if func_type == self.silent_never_type() {
            return Ok(self.silent_never_signature());
        }

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
            if !self.is_error_type(func_type)
                && node_as_call_expression.maybe_type_arguments().is_some()
            {
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
                        func_type,
                        Option::<Id<Node>>::None,
                        None,
                        None,
                    )?]),
                );
            } else {
                let mut related_information: Option<Id<DiagnosticRelatedInformation>> = None;
                if node_as_call_expression.arguments.len() == 1 {
                    let source_file = get_source_file_of_node(node, self);
                    let source_file_ref = source_file.ref_(self);
                    let text = source_file_ref.as_source_file().text_as_chars();
                    if is_line_break(text_char_at_index(
                        &text,
                        TryInto::<usize>::try_into(skip_trivia(
                            &text,
                            node_as_call_expression.expression.ref_(self).end(),
                            Some(true),
                            None,
                            None,
                        ))
                        .unwrap()
                            - 1,
                    )) {
                        related_information = Some(self.alloc_diagnostic_related_information(
                            create_diagnostic_for_node(
                                node_as_call_expression.expression,
                                &Diagnostics::Are_you_missing_a_semicolon,
                                None,
                                self,
                            )
                            .into(),
                        ));
                    }
                }
                self.invocation_error(
                    node_as_call_expression.expression,
                    apparent_type,
                    SignatureKind::Call,
                    related_information,
                )?;
            }
            return self.resolve_error_call(node);
        }
        if check_mode.intersects(CheckMode::SkipGenericFunctions)
            && node_as_call_expression.maybe_type_arguments().is_none()
            && call_signatures.iter().try_any(|call_signature| {
                self.is_generic_function_returning_function(call_signature.clone())
            })?
        {
            self.skipped_generic_function(node, check_mode);
            return Ok(self.resolving_signature());
        }
        if call_signatures.iter().any(|sig| {
            is_in_js_file(sig.ref_(self).declaration.refed(self).as_deref())
                && get_jsdoc_class_tag(sig.ref_(self).declaration.unwrap(), self).is_some()
        }) {
            self.error(
                Some(node),
                &Diagnostics::Value_of_type_0_is_not_callable_Did_you_mean_to_include_new,
                Some(vec![self.type_to_string_(
                    func_type,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?]),
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

    pub(super) fn is_generic_function_returning_function(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<bool> {
        let signature_type_parameters_is_some = signature.ref_(self).maybe_type_parameters().is_some();
        Ok(signature_type_parameters_is_some
            && self.is_function_type(self.get_return_type_of_signature(signature)?)?)
    }

    pub(super) fn is_untyped_function_call(
        &self,
        func_type: Id<Type>,
        apparent_func_type: Id<Type>,
        num_call_signatures: usize,
        num_construct_signatures: usize,
    ) -> io::Result<bool> {
        Ok(self.is_type_any(Some(func_type))
            || self.is_type_any(Some(apparent_func_type))
                && func_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::TypeParameter)
            || num_call_signatures == 0
                && num_construct_signatures == 0
                && !apparent_func_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::Union)
                && !self
                    .get_reduced_type(apparent_func_type)?
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::Never)
                && self.is_type_assignable_to(func_type, self.global_function_type())?)
    }

    pub(super) fn resolve_new_expression(
        &self,
        node: Id<Node>, /*NewExpression*/
        candidates_out_array: Option<&mut Vec<Id<Signature>>>,
        check_mode: CheckMode,
    ) -> io::Result<Id<Signature>> {
        let node_ref = node.ref_(self);
        let node_as_new_expression = node_ref.as_new_expression();
        if let Some(node_arguments) = node_as_new_expression.arguments.as_ref() {
            if self.language_version < ScriptTarget::ES5 {
                let spread_index = self.get_spread_argument_index(node_arguments);
                if let Some(spread_index) = spread_index {
                    self.error(
                        Some(node_arguments[spread_index]),
                        &Diagnostics::Spread_operator_in_new_expressions_is_only_available_when_targeting_ECMAScript_5_and_higher,
                        None,
                    );
                }
            }
        }

        let mut expression_type =
            self.check_non_null_expression(node_as_new_expression.expression)?;
        if expression_type == self.silent_never_type() {
            return Ok(self.silent_never_signature());
        }

        expression_type = self.get_apparent_type(expression_type)?;
        if self.is_error_type(expression_type) {
            return self.resolve_error_call(node);
        }

        if self.is_type_any(Some(expression_type)) {
            if node_as_new_expression.maybe_type_arguments().is_some() {
                self.error(
                    Some(node),
                    &Diagnostics::Untyped_function_calls_may_not_accept_type_arguments,
                    None,
                );
            }
            return self.resolve_untyped_call(node);
        }

        let construct_signatures =
            self.get_signatures_of_type(expression_type, SignatureKind::Construct)?;
        if !construct_signatures.is_empty() {
            if !self.is_constructor_accessible(node, construct_signatures[0])? {
                return self.resolve_error_call(node);
            }
            if construct_signatures
                .iter()
                .any(|signature| signature.ref_(self).flags.intersects(SignatureFlags::Abstract))
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
                    .ref_(self)
                    .maybe_symbol()
                    .and_then(|expression_type_symbol| {
                        get_class_like_declaration_of_symbol(expression_type_symbol, self)
                    });
            if value_decl
                .matches(|value_decl| has_syntactic_modifier(value_decl, ModifierFlags::Abstract, self))
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
        let call_signatures = self.get_signatures_of_type(expression_type, SignatureKind::Call)?;
        if !call_signatures.is_empty() {
            let signature = self.resolve_call(
                node,
                &call_signatures,
                candidates_out_array,
                check_mode,
                SignatureFlags::None,
                None,
            )?;
            if !self.no_implicit_any {
                if matches!(
                    signature.ref_(self).declaration,
                    Some(signature_declaration) if !self.is_js_constructor(Some(signature_declaration))? &&
                        self.get_return_type_of_signature(signature.clone())? != self.void_type()
                ) {
                    self.error(
                        Some(node),
                        &Diagnostics::Only_a_void_function_can_be_called_with_the_new_keyword,
                        None,
                    );
                }
                if matches!(
                    self.get_this_type_of_signature(signature)?,
                    Some(this_type) if this_type == self.void_type()
                ) {
                    self.error(
                        Some(node),
                        &Diagnostics::A_function_that_is_called_with_the_new_keyword_cannot_have_a_this_type_that_is_void,
                        None,
                    );
                }
            }
            return Ok(signature);
        }

        self.invocation_error(
            node_as_new_expression.expression,
            expression_type,
            SignatureKind::Construct,
            None,
        )?;
        self.resolve_error_call(node)
    }

    pub(super) fn type_has_protected_accessible_base(
        &self,
        target: Id<Symbol>,
        type_: Id<Type>, /*InterfaceType*/
    ) -> io::Result<bool> {
        let base_types = self.get_base_types(type_)?;
        if length(Some(&base_types)) == 0 {
            return Ok(false);
        }
        let first_base = base_types[0];
        if first_base
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Intersection)
        {
            let first_base_ref = first_base.ref_(self);
            let types = first_base_ref.as_intersection_type().types();
            let mixin_flags = self.find_mixins(types)?;
            let mut i = 0;
            for intersection_member in first_base
                .ref_(self)
                .as_intersection_type()
                .types()
                .to_owned()
            {
                if !mixin_flags[i] {
                    if get_object_flags(&intersection_member.ref_(self))
                        .intersects(ObjectFlags::Class | ObjectFlags::Interface)
                    {
                        if matches!(
                            intersection_member.ref_(self).maybe_symbol(),
                            Some(intersection_member_symbol) if intersection_member_symbol == target
                        ) {
                            return Ok(true);
                        }
                        if self.type_has_protected_accessible_base(target, intersection_member)? {
                            return Ok(true);
                        }
                    }
                }
                i += 1;
            }
            return Ok(false);
        }
        if matches!(
            first_base.ref_(self).maybe_symbol(),
            Some(first_base_symbol) if first_base_symbol == target
        ) {
            return Ok(true);
        }
        self.type_has_protected_accessible_base(target, first_base)
    }

    pub(super) fn is_constructor_accessible(
        &self,
        node: Id<Node>, /*NewExpression*/
        signature: Id<Signature>,
    ) -> io::Result<bool> {
        if
        /* !signature ||*/
        signature.ref_(self).declaration.is_none() {
            return Ok(true);
        }

        let declaration = signature.ref_(self).declaration.unwrap();
        let modifiers = get_selected_effective_modifier_flags(
            declaration,
            ModifierFlags::NonPublicAccessibilityModifier,
            self,
        );

        if modifiers == ModifierFlags::None || declaration.ref_(self).kind() != SyntaxKind::Constructor {
            return Ok(true);
        }

        let declaring_class_declaration =
            get_class_like_declaration_of_symbol(declaration.ref_(self).parent().ref_(self).symbol(), self)
                .unwrap();
        let declaring_class = self.get_declared_type_of_symbol(declaration.ref_(self).parent().ref_(self).symbol())?;

        if !self.is_node_within_class(node, declaring_class_declaration) {
            let containing_class = get_containing_class(node, self);
            if let Some(containing_class) = containing_class {
                if modifiers.intersects(ModifierFlags::Protected) {
                    let containing_type = self.get_type_of_node(containing_class)?;
                    if self.type_has_protected_accessible_base(
                        declaration.ref_(self).parent().ref_(self).symbol(),
                        containing_type,
                    )? {
                        return Ok(true);
                    }
                }
            }
            if modifiers.intersects(ModifierFlags::Private) {
                self.error(
                    Some(node),
                    &Diagnostics::Constructor_of_class_0_is_private_and_only_accessible_within_the_class_declaration,
                    Some(vec![
                        self.type_to_string_(
                            declaring_class,
                            Option::<Id<Node>>::None,
                            None, None,
                        )?
                    ])
                );
            }
            if modifiers.intersects(ModifierFlags::Protected) {
                self.error(
                    Some(node),
                    &Diagnostics::Constructor_of_class_0_is_protected_and_only_accessible_within_the_class_declaration,
                    Some(vec![
                        self.type_to_string_(
                            declaring_class,
                            Option::<Id<Node>>::None,
                            None, None,
                        )?
                    ])
                );
            }
            return Ok(false);
        }

        Ok(true)
    }

    pub(super) fn invocation_error_details(
        &self,
        error_target: Id<Node>,
        apparent_type: Id<Type>,
        kind: SignatureKind,
    ) -> io::Result<InvocationErrorDetails> {
        let mut error_info: Option<DiagnosticMessageChain> = None;
        let is_call = kind == SignatureKind::Call;
        let awaited_type =
            self.get_awaited_type_(apparent_type, Option::<Id<Node>>::None, None, None)?;
        let maybe_missing_await = matches!(
            awaited_type,
            Some(awaited_type) if !self.get_signatures_of_type(awaited_type, kind)?.is_empty()
        );
        if apparent_type
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Union)
        {
            let types = apparent_type.ref_(self).as_union_type().types().to_owned();
            let mut has_signatures = false;
            for constituent in types {
                let signatures = self.get_signatures_of_type(constituent, kind)?;
                if !signatures.is_empty() {
                    has_signatures = true;
                    if error_info.is_some() {
                        break;
                    }
                } else {
                    if error_info.is_none() {
                        error_info = Some(chain_diagnostic_messages(
                            error_info,
                            if is_call {
                                &*Diagnostics::Type_0_has_no_call_signatures
                            } else {
                                &*Diagnostics::Type_0_has_no_construct_signatures
                            },
                            Some(vec![self.type_to_string_(
                                constituent,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                            )?]),
                        ));
                        error_info = Some(chain_diagnostic_messages(
                            error_info,
                            if is_call {
                                &*Diagnostics::Not_all_constituents_of_type_0_are_callable
                            } else {
                                &*Diagnostics::Not_all_constituents_of_type_0_are_constructable
                            },
                            Some(vec![self.type_to_string_(
                                apparent_type,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                            )?]),
                        ));
                    }
                    if has_signatures {
                        break;
                    }
                }
            }
            if !has_signatures {
                error_info = Some(chain_diagnostic_messages(
                    None,
                    if is_call {
                        &*Diagnostics::No_constituent_of_type_0_is_callable
                    } else {
                        &*Diagnostics::No_constituent_of_type_0_is_constructable
                    },
                    Some(vec![self.type_to_string_(
                        apparent_type,
                        Option::<Id<Node>>::None,
                        None,
                        None,
                    )?]),
                ));
            }
            if error_info.is_none() {
                error_info = Some(chain_diagnostic_messages(
                    error_info,
                    if is_call {
                        &*Diagnostics::Each_member_of_the_union_type_0_has_signatures_but_none_of_those_signatures_are_compatible_with_each_other
                    } else {
                        &*Diagnostics::Each_member_of_the_union_type_0_has_construct_signatures_but_none_of_those_signatures_are_compatible_with_each_other
                    },
                    Some(vec![self.type_to_string_(
                        apparent_type,
                        Option::<Id<Node>>::None,
                        None,
                        None,
                    )?]),
                ));
            }
        } else {
            error_info = Some(chain_diagnostic_messages(
                error_info,
                if is_call {
                    &*Diagnostics::Type_0_has_no_call_signatures
                } else {
                    &*Diagnostics::Type_0_has_no_construct_signatures
                },
                Some(vec![self.type_to_string_(
                    apparent_type,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?]),
            ));
        }

        let mut head_message = if is_call {
            &*Diagnostics::This_expression_is_not_callable
        } else {
            &*Diagnostics::This_expression_is_not_constructable
        };

        if is_call_expression(&error_target.ref_(self).parent().ref_(self))
            && error_target
                .ref_(self).parent()
                .ref_(self).as_call_expression()
                .arguments
                .is_empty()
        {
            let resolved_symbol = (*self.get_node_links(error_target))
                .borrow()
                .resolved_symbol
                .clone();
            if matches!(
                resolved_symbol,
                Some(resolved_symbol) if resolved_symbol.ref_(self).flags().intersects(SymbolFlags::GetAccessor)
            ) {
                head_message = &*Diagnostics::This_expression_is_not_callable_because_it_is_a_get_accessor_Did_you_mean_to_use_it_without;
            }
        }

        Ok(InvocationErrorDetails {
            message_chain: chain_diagnostic_messages(error_info, head_message, None),
            related_message: if maybe_missing_await {
                Some(&Diagnostics::Did_you_forget_to_use_await)
            } else {
                None
            },
        })
    }
}

pub(super) struct InvocationErrorDetails {
    pub message_chain: DiagnosticMessageChain,
    pub related_message: Option<&'static DiagnosticMessage>,
}
