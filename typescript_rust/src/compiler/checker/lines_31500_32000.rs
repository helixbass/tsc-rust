use std::{borrow::Borrow, io};

use gc::{Gc, GcCell};
use id_arena::Id;

use super::{
    signature_has_rest_parameter, typeof_eq_facts, typeof_ne_facts, CheckMode, IterationTypeKind,
    IterationUse, TypeFacts, WideningKind,
};
use crate::{
    are_option_gcs_equal, create_symbol_table, get_effective_return_type_node,
    get_effective_type_annotation_node, get_function_flags, is_import_call, is_omitted_expression,
    is_transient_symbol, last, node_is_missing, push_if_unique_eq, push_if_unique_gc, some,
    try_for_each_return_statement, try_for_each_yield_expression, CheckFlags, Diagnostics,
    FunctionFlags, HasArena, HasTypeInterface, InArena, InferenceContext,
    NamedDeclarationInterface, Node, NodeFlags, NodeInterface, OptionTry, Signature, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, TransientSymbolInterface, Type, TypeChecker,
    TypeFlags, TypeInterface, UnionReduction,
    OptionInArena,
};

impl TypeChecker {
    pub(super) fn get_type_of_first_parameter_of_signature_with_fallback(
        &self,
        signature: Id<Signature>,
        fallback_type: Id<Type>,
    ) -> io::Result<Id<Type>> {
        Ok(if !signature.ref_(self).parameters().is_empty() {
            self.get_type_at_position(signature, 0)?
        } else {
            fallback_type
        })
    }

    pub(super) fn infer_from_annotated_parameters(
        &self,
        signature: Id<Signature>,
        context: Id<Signature>,
        inference_context: &InferenceContext,
    ) -> io::Result<()> {
        let len = signature.ref_(self).parameters().len()
            - if signature_has_rest_parameter(&signature.ref_(self)) {
                1
            } else {
                0
            };
        for i in 0..len {
            let declaration = signature.ref_(self).parameters()[i]
                .ref_(self)
                .maybe_value_declaration()
                .unwrap();
            if declaration
                .ref_(self).as_parameter_declaration()
                .maybe_type()
                .is_some()
            {
                let type_node = get_effective_type_annotation_node(declaration, self);
                if let Some(type_node) = type_node {
                    self.infer_types(
                        &inference_context.inferences(),
                        self.get_type_from_type_node_(type_node)?,
                        self.get_type_at_position(context, i)?,
                        None,
                        None,
                    )?;
                }
            }
        }
        let rest_type = self.get_effective_rest_type(context)?;
        if let Some(rest_type) = rest_type.filter(|&rest_type| {
            rest_type
                .ref_(self)
                .flags()
                .intersects(TypeFlags::TypeParameter)
        }) {
            let instantiated_context = self.alloc_signature(self.instantiate_signature(
                context.clone(),
                inference_context.non_fixing_mapper(),
                None,
            )?);
            self.assign_contextual_parameter_types(signature, instantiated_context)?;
            let rest_pos = self.get_parameter_count(context)? - 1;
            self.infer_types(
                &inference_context.inferences(),
                self.get_rest_type_at_position(signature, rest_pos)?,
                rest_type,
                None,
                None,
            )?;
        }

        Ok(())
    }

    pub(super) fn assign_contextual_parameter_types(
        &self,
        signature: Id<Signature>,
        context: Id<Signature>,
    ) -> io::Result<()> {
        if let Some(context_type_parameters) = context.ref_(self).maybe_type_parameters().as_ref() {
            if signature.ref_(self).maybe_type_parameters().is_none() {
                *signature.ref_(self).maybe_type_parameters_mut() = Some(context_type_parameters.clone());
            } else {
                return Ok(());
            }
        }
        if let Some(context_this_parameter) = *context.ref_(self).maybe_this_parameter() {
            let parameter = signature.ref_(self).maybe_this_parameter().clone();
            if match parameter {
                None => true,
                Some(parameter) => matches!(
                    parameter.ref_(self).maybe_value_declaration(),
                    Some(parameter_value_declaration) if parameter_value_declaration.ref_(self).as_has_type().maybe_type().is_none()
                ),
            } {
                if parameter.is_none() {
                    *signature.ref_(self).maybe_this_parameter_mut() =
                        Some(self.create_symbol_with_type(context_this_parameter, None));
                }
                self.assign_parameter_type(
                    signature.ref_(self).maybe_this_parameter().unwrap(),
                    Some(self.get_type_of_symbol(context_this_parameter)?),
                )?;
            }
        }
        let len = signature.ref_(self).parameters().len()
            - if signature_has_rest_parameter(&signature.ref_(self)) {
                1
            } else {
                0
            };
        for i in 0..len {
            let parameter = signature.ref_(self).parameters()[i];
            if get_effective_type_annotation_node(
                parameter.ref_(self).maybe_value_declaration().unwrap(),
                self,
            )
            .is_none()
            {
                let contextual_parameter_type = self.try_get_type_at_position(context, i)?;
                self.assign_parameter_type(parameter, contextual_parameter_type)?;
            }
        }
        if signature_has_rest_parameter(&signature.ref_(self)) {
            let parameter = *last(signature.ref_(self).parameters());
            if is_transient_symbol(&parameter.ref_(self))
                || get_effective_type_annotation_node(
                    parameter.ref_(self).maybe_value_declaration().unwrap(),
                    self,
                )
                .is_none()
            {
                let contextual_parameter_type = self.get_rest_type_at_position(context, len)?;
                self.assign_parameter_type(parameter, Some(contextual_parameter_type))?;
            }
        }

        Ok(())
    }

    pub(super) fn assign_non_contextual_parameter_types(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<()> {
        if let Some(signature_this_parameter) = *signature.ref_(self).maybe_this_parameter() {
            self.assign_parameter_type(signature_this_parameter, None)?;
        }
        for &parameter in signature.ref_(self).parameters() {
            self.assign_parameter_type(parameter, None)?;
        }

        Ok(())
    }

    pub(super) fn assign_parameter_type(
        &self,
        parameter: Id<Symbol>,
        type_: Option<Id<Type>>,
    ) -> io::Result<()> {
        let links = self.get_symbol_links(parameter);
        if (*links.ref_(self)).borrow().type_.is_none() {
            let declaration = parameter.ref_(self).maybe_value_declaration().unwrap();
            links.ref_(self).borrow_mut().type_ = Some(type_.try_unwrap_or_else(|| {
                self.get_widened_type_for_variable_like_declaration(declaration, Some(true))
            })?);
            let declaration_name = declaration.ref_(self).as_named_declaration().name();
            if declaration_name.ref_(self).kind() != SyntaxKind::Identifier {
                if (*links.ref_(self)).borrow().type_.unwrap() == self.unknown_type() {
                    links.ref_(self).borrow_mut().type_ =
                        Some(self.get_type_from_binding_pattern(declaration_name, None, None)?);
                }
                self.assign_binding_element_types(declaration_name)?;
            }
        }

        Ok(())
    }

    pub(super) fn assign_binding_element_types(
        &self,
        pattern: Id<Node>, /*BindingPattern*/
    ) -> io::Result<()> {
        for &element in &*pattern.ref_(self).as_has_elements().elements().ref_(self) {
            if !is_omitted_expression(&element.ref_(self)) {
                let element_ref = element.ref_(self);
                let element_as_binding_element = element_ref.as_binding_element();
                if element_as_binding_element.name().ref_(self).kind() == SyntaxKind::Identifier {
                    self.get_symbol_links(self.get_symbol_of_node(element)?.unwrap())
                        .ref_(self).borrow_mut()
                        .type_ = self.get_type_for_binding_element(element)?;
                } else {
                    self.assign_binding_element_types(element_as_binding_element.name())?;
                }
            }
        }

        Ok(())
    }

    pub(super) fn create_promise_type(&self, promised_type: Id<Type>) -> io::Result<Id<Type>> {
        let global_promise_type = self.get_global_promise_type(true)?;
        if global_promise_type != self.empty_generic_type() {
            let promised_type = self
                .get_awaited_type_no_alias(
                    self.unwrap_awaited_type(promised_type)?,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?
                .unwrap_or_else(|| self.unknown_type());
            return Ok(self.create_type_reference(global_promise_type, Some(vec![promised_type])));
        }

        Ok(self.unknown_type())
    }

    pub(super) fn create_promise_like_type(&self, promised_type: Id<Type>) -> io::Result<Id<Type>> {
        let global_promise_like_type = self.get_global_promise_like_type(true)?;
        if global_promise_like_type != self.empty_generic_type() {
            let promised_type = self
                .get_awaited_type_no_alias(
                    self.unwrap_awaited_type(promised_type)?,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?
                .unwrap_or_else(|| self.unknown_type());
            return Ok(
                self.create_type_reference(global_promise_like_type, Some(vec![promised_type]))
            );
        }

        Ok(self.unknown_type())
    }

    pub(super) fn create_promise_return_type(
        &self,
        func: Id<Node>, /*FunctionLikeDeclaration | ImportCall*/
        promised_type: Id<Type>,
    ) -> io::Result<Id<Type>> {
        let promise_type = self.create_promise_type(promised_type)?;
        if promise_type == self.unknown_type() {
            self.error(
                Some(func),
                if is_import_call(func, self) {
                    &Diagnostics::A_dynamic_import_call_returns_a_Promise_Make_sure_you_have_a_declaration_for_Promise_or_include_ES2015_in_your_lib_option
                } else {
                    &Diagnostics::An_async_function_or_method_must_return_a_Promise_Make_sure_you_have_a_declaration_for_Promise_or_include_ES2015_in_your_lib_option
                },
                None
            );
            return Ok(self.error_type());
        } else if self.get_global_promise_constructor_symbol(true)?.is_none() {
            self.error(
                Some(func),
                if is_import_call(func, self) {
                    &Diagnostics::A_dynamic_import_call_in_ES5_SlashES3_requires_the_Promise_constructor_Make_sure_you_have_a_declaration_for_the_Promise_constructor_or_include_ES2015_in_your_lib_option
                } else {
                    &Diagnostics::An_async_function_or_method_in_ES5_SlashES3_requires_the_Promise_constructor_Make_sure_you_have_a_declaration_for_the_Promise_constructor_or_include_ES2015_in_your_lib_option
                },
                None
            );
        }

        Ok(promise_type)
    }

    pub(super) fn create_new_target_expression_type(
        &self,
        target_type: Id<Type>,
    ) -> io::Result<Id<Type>> {
        let symbol = self.alloc_symbol(
            self.create_symbol(SymbolFlags::None, "NewTargetExpression".to_owned(), None)
                .into(),
        );

        let target_property_symbol = self.alloc_symbol(
            self.create_symbol(
                SymbolFlags::Property,
                "target".to_owned(),
                Some(CheckFlags::Readonly),
            )
            .into(),
        );
        target_property_symbol
            .ref_(self)
            .set_parent(Some(symbol.clone()));
        target_property_symbol
            .ref_(self)
            .as_transient_symbol()
            .symbol_links()
            .ref_(self).borrow_mut()
            .type_ = Some(target_type);

        let members = Gc::new(GcCell::new(create_symbol_table(
            self.arena(),
            Some(&[target_property_symbol]),
        )));
        *symbol.ref_(self).maybe_members_mut() = Some(members.clone());
        self.create_anonymous_type(Some(symbol), members, vec![], vec![], vec![])
    }

    pub(super) fn get_return_type_from_body(
        &self,
        func: Id<Node>, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let func_ref = func.ref_(self);
        let func_as_function_like_declaration = func_ref.as_function_like_declaration();
        if func_as_function_like_declaration.maybe_body().is_none() {
            return Ok(self.error_type());
        }
        let func_body = func_as_function_like_declaration.maybe_body().unwrap();

        let function_flags = get_function_flags(Some(func), self);
        let is_async = function_flags.intersects(FunctionFlags::Async);
        let is_generator = function_flags.intersects(FunctionFlags::Generator);

        let mut return_type: Option<Id<Type>> = None;
        let mut yield_type: Option<Id<Type>> = None;
        let mut next_type: Option<Id<Type>> = None;
        let mut fallback_return_type = self.void_type();
        if func_body.ref_(self).kind() != SyntaxKind::Block {
            return_type = Some(self.check_expression_cached(
                func_body,
                check_mode.map(|check_mode| check_mode & !CheckMode::SkipGenericFunctions),
            )?);
            if is_async {
                return_type = Some(self.unwrap_awaited_type(
                    self.check_awaited_type(
                        return_type.unwrap(),
                        false,
                        func,
                        &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member,
                        None,
                    )?
                )?);
            }
        } else if is_generator {
            let return_types =
                self.check_and_aggregate_return_expression_types(func, check_mode)?;
            match return_types {
                None => {
                    fallback_return_type = self.never_type();
                }
                Some(return_types) => {
                    if !return_types.is_empty() {
                        return_type = Some(self.get_union_type(
                            &return_types,
                            Some(UnionReduction::Subtype),
                            Option::<Id<Symbol>>::None,
                            None,
                            None,
                        )?);
                    }
                }
            }
            let CheckAndAggregateYieldOperandTypesReturn {
                yield_types,
                next_types,
            } = self.check_and_aggregate_yield_operand_types(func, check_mode)?;
            yield_type = if some(Some(&yield_types), Option::<fn(&Id<Type>) -> bool>::None) {
                Some(self.get_union_type(
                    &yield_types,
                    Some(UnionReduction::Subtype),
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?)
            } else {
                None
            };
            next_type = if some(Some(&next_types), Option::<fn(&Id<Type>) -> bool>::None) {
                Some(self.get_intersection_type(&next_types, Option::<Id<Symbol>>::None, None)?)
            } else {
                None
            };
        } else {
            let types = self.check_and_aggregate_return_expression_types(func, check_mode)?;
            if types.is_none() {
                return Ok(if function_flags.intersects(FunctionFlags::Async) {
                    self.create_promise_return_type(func, self.never_type())?
                } else {
                    self.never_type()
                });
            }
            let types = types.unwrap();
            if types.is_empty() {
                return Ok(if function_flags.intersects(FunctionFlags::Async) {
                    self.create_promise_return_type(func, self.void_type())?
                } else {
                    self.void_type()
                });
            }

            return_type = Some(self.get_union_type(
                &types,
                Some(UnionReduction::Subtype),
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?);
        }

        if return_type.is_some() || yield_type.is_some() || next_type.is_some() {
            if let Some(yield_type) = yield_type {
                self.report_errors_from_widening(
                    func,
                    yield_type,
                    Some(WideningKind::GeneratorYield),
                )?;
            }
            if let Some(return_type) = return_type {
                self.report_errors_from_widening(
                    func,
                    return_type,
                    Some(WideningKind::FunctionReturn),
                )?;
            }
            if let Some(next_type) = next_type {
                self.report_errors_from_widening(
                    func,
                    next_type,
                    Some(WideningKind::GeneratorNext),
                )?;
            }

            if matches!(return_type, Some(return_type) if self.is_unit_type(return_type))
                || matches!(yield_type, Some(yield_type) if self.is_unit_type(yield_type))
                || matches!(next_type, Some(next_type) if self.is_unit_type(next_type))
            {
                let contextual_signature =
                    self.get_contextual_signature_for_function_like_declaration(func)?;
                let contextual_type =
                    contextual_signature.try_and_then(|contextual_signature| -> io::Result<_> {
                        Ok(
                            if contextual_signature == self.get_signature_from_declaration_(func)? {
                                if is_generator {
                                    None
                                } else {
                                    return_type.clone()
                                }
                            } else {
                                self.instantiate_contextual_type(
                                    Some(self.get_return_type_of_signature(contextual_signature)?),
                                    func,
                                    None,
                                )?
                            },
                        )
                    })?;
                if is_generator {
                    yield_type = self
                        .get_widened_literal_like_type_for_contextual_iteration_type_if_needed(
                            yield_type,
                            contextual_type,
                            IterationTypeKind::Yield,
                            is_async,
                        )?;
                    return_type = self
                        .get_widened_literal_like_type_for_contextual_iteration_type_if_needed(
                            return_type,
                            contextual_type,
                            IterationTypeKind::Return,
                            is_async,
                        )?;
                    next_type = self
                        .get_widened_literal_like_type_for_contextual_iteration_type_if_needed(
                            next_type,
                            contextual_type,
                            IterationTypeKind::Next,
                            is_async,
                        )?;
                } else {
                    return_type = self
                        .get_widened_literal_like_type_for_contextual_return_type_if_needed(
                            return_type,
                            contextual_type,
                            is_async,
                        )?;
                }
            }

            if let Some(yield_type_present) = yield_type {
                yield_type = Some(self.get_widened_type(yield_type_present)?);
            }
            if let Some(return_type_present) = return_type {
                return_type = Some(self.get_widened_type(return_type_present)?);
            }
            if let Some(next_type_present) = next_type {
                next_type = Some(self.get_widened_type(next_type_present)?);
            }
        }

        Ok(if is_generator {
            self.create_generator_return_type(
                yield_type.unwrap_or_else(|| self.never_type()),
                return_type.unwrap_or(fallback_return_type),
                next_type
                    .try_or_else(|| {
                        self.get_contextual_iteration_type(IterationTypeKind::Next, func)
                    })?
                    .unwrap_or_else(|| self.unknown_type()),
                is_async,
            )?
        } else {
            if is_async {
                self.create_promise_type(return_type.unwrap_or(fallback_return_type))?
            } else {
                return_type.unwrap_or(fallback_return_type)
            }
        })
    }

    pub(super) fn create_generator_return_type(
        &self,
        yield_type: Id<Type>,
        return_type: Id<Type>,
        next_type: Id<Type>,
        is_async_generator: bool,
    ) -> io::Result<Id<Type>> {
        let resolver = if is_async_generator {
            &self.async_iteration_types_resolver
        } else {
            &self.sync_iteration_types_resolver
        };
        let global_generator_type = (resolver.get_global_generator_type)(self, false)?;
        let yield_type = (resolver.resolve_iteration_type)(self, yield_type, None)?
            .unwrap_or_else(|| self.unknown_type());
        let return_type = (resolver.resolve_iteration_type)(self, return_type, None)?
            .unwrap_or_else(|| self.unknown_type());
        let next_type = (resolver.resolve_iteration_type)(self, next_type, None)?
            .unwrap_or_else(|| self.unknown_type());
        if global_generator_type == self.empty_generic_type() {
            let global_type = (resolver.get_global_iterable_iterator_type)(self, false)?;
            let iteration_types = if global_type != self.empty_generic_type() {
                Some(self.get_iteration_types_of_global_iterable_type(global_type, resolver)?)
            } else {
                None
            };
            let iterable_iterator_return_type =
                if let Some(iteration_types) = iteration_types.as_ref() {
                    iteration_types.ref_(self).return_type()
                } else {
                    self.any_type()
                };
            let iterable_iterator_next_type =
                if let Some(iteration_types) = iteration_types.as_ref() {
                    iteration_types.ref_(self).next_type()
                } else {
                    self.undefined_type()
                };
            if self.is_type_assignable_to(return_type, iterable_iterator_return_type)?
                && self.is_type_assignable_to(iterable_iterator_next_type, next_type)?
            {
                if global_type != self.empty_generic_type() {
                    return Ok(
                        self.create_type_from_generic_global_type(global_type, vec![yield_type])
                    );
                }

                (resolver.get_global_iterable_iterator_type)(self, true)?;
                return Ok(self.empty_object_type());
            }

            (resolver.get_global_generator_type)(self, true)?;
            return Ok(self.empty_object_type());
        }

        Ok(self.create_type_from_generic_global_type(
            global_generator_type,
            vec![yield_type, return_type, next_type],
        ))
    }

    pub(super) fn check_and_aggregate_yield_operand_types(
        &self,
        func: Id<Node>, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<CheckAndAggregateYieldOperandTypesReturn> {
        let mut yield_types: Vec<Id<Type>> = vec![];
        let mut next_types: Vec<Id<Type>> = vec![];
        let is_async = get_function_flags(Some(func), self).intersects(FunctionFlags::Async);
        try_for_each_yield_expression(
            func.ref_(self).as_function_like_declaration().maybe_body().unwrap(),
            |yield_expression: Id<Node>| -> io::Result<_> {
                let yield_expression_ref = yield_expression.ref_(self);
                let yield_expression_as_yield_expression = yield_expression_ref.as_yield_expression();
                let yield_expression_type = if let Some(yield_expression_expression) =
                    yield_expression_as_yield_expression.expression
                {
                    self.check_expression(yield_expression_expression, check_mode, None)?
                } else {
                    self.undefined_widening_type()
                };
                if let Some(ref yielded_type) = self.get_yielded_type_of_yield_expression(
                    yield_expression,
                    yield_expression_type,
                    self.any_type(),
                    is_async,
                )? {
                    push_if_unique_eq(&mut yield_types, yielded_type);
                }
                let next_type: Option<Id<Type>>;
                if yield_expression_as_yield_expression
                    .asterisk_token
                    .is_some()
                {
                    let iteration_types = self.get_iteration_types_of_iterable(
                        yield_expression_type,
                        if is_async {
                            IterationUse::AsyncYieldStar
                        } else {
                            IterationUse::YieldStar
                        },
                        yield_expression_as_yield_expression.expression,
                    )?;
                    next_type = iteration_types.map(|iteration_types| iteration_types.ref_(self).next_type());
                } else {
                    next_type = self.get_contextual_type_(yield_expression, None)?;
                }
                if let Some(next_type) = next_type.as_ref() {
                    push_if_unique_eq(&mut next_types, next_type);
                }

                Ok(())
            },
            self,
        )?;
        Ok(CheckAndAggregateYieldOperandTypesReturn {
            yield_types,
            next_types,
        })
    }

    pub(super) fn get_yielded_type_of_yield_expression(
        &self,
        node: Id<Node>, /*YieldExpression*/
        expression_type: Id<Type>,
        sent_type: Id<Type>,
        is_async: bool,
    ) -> io::Result<Option<Id<Type>>> {
        let node_ref = node.ref_(self);
        let node_as_yield_expression = node_ref.as_yield_expression();
        let error_node = node_as_yield_expression
            .expression
            .unwrap_or(node);
        let yielded_type = if node_as_yield_expression.asterisk_token.is_some() {
            self.check_iterated_type_or_element_type(
                if is_async {
                    IterationUse::AsyncYieldStar
                } else {
                    IterationUse::YieldStar
                },
                expression_type,
                sent_type,
                Some(error_node),
            )?
        } else {
            expression_type
        };
        Ok(if !is_async {
            Some(yielded_type)
        } else {
            self.get_awaited_type_(
                yielded_type,
                Some(error_node),
                Some(if node_as_yield_expression.asterisk_token.is_some() {
                    &*Diagnostics::Type_of_iterated_elements_of_a_yield_Asterisk_operand_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member
                } else {
                    &*Diagnostics::Type_of_yield_operand_in_an_async_generator_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member
                }),
                None,
            )?
        })
    }

    pub(super) fn get_facts_from_typeof_switch(
        &self,
        start: usize,
        end: usize,
        witnesses: &[String],
        has_default: bool,
    ) -> TypeFacts {
        let mut facts = TypeFacts::None;
        if has_default {
            for i in end..witnesses.len() {
                facts |= match typeof_ne_facts.get(&&*witnesses[i]).copied() {
                    None => TypeFacts::TypeofNEHostObject,
                    Some(fact) => {
                        if fact == TypeFacts::None {
                            TypeFacts::TypeofNEHostObject
                        } else {
                            fact
                        }
                    }
                };
            }
            for i in start..end {
                facts &= !typeof_ne_facts
                    .get(&&*witnesses[i])
                    .copied()
                    .unwrap_or(TypeFacts::None)
            }
            for i in 0..start {
                facts |= match typeof_ne_facts.get(&&*witnesses[i]).copied() {
                    None => TypeFacts::TypeofNEHostObject,
                    Some(fact) => {
                        if fact == TypeFacts::None {
                            TypeFacts::TypeofNEHostObject
                        } else {
                            fact
                        }
                    }
                };
            }
        } else {
            for i in start..end {
                facts |= match typeof_eq_facts.get(&&*witnesses[i]).copied() {
                    None => TypeFacts::TypeofEQHostObject,
                    Some(fact) => {
                        if fact == TypeFacts::None {
                            TypeFacts::TypeofEQHostObject
                        } else {
                            fact
                        }
                    }
                };
            }
            for i in 0..start {
                facts &= !typeof_eq_facts
                    .get(&&*witnesses[i])
                    .copied()
                    .unwrap_or(TypeFacts::None)
            }
        }
        facts
    }

    pub(super) fn is_exhaustive_switch_statement(
        &self,
        node: Id<Node>, /*SwitchStatement*/
    ) -> io::Result<bool> {
        let links = self.get_node_links(node);
        let links_is_exhaustive = (*links).borrow().is_exhaustive;
        if let Some(links_is_exhaustive) = links_is_exhaustive {
            return Ok(links_is_exhaustive);
        }
        let ret = self.compute_exhaustive_switch_statement(node)?;
        links.borrow_mut().is_exhaustive = Some(ret);
        Ok(ret)
    }

    pub(super) fn compute_exhaustive_switch_statement(
        &self,
        node: Id<Node>, /*SwitchStatement*/
    ) -> io::Result<bool> {
        let node_ref = node.ref_(self);
        let node_as_switch_statement = node_ref.as_switch_statement();
        if node_as_switch_statement.expression.ref_(self).kind() == SyntaxKind::TypeOfExpression {
            let operand_type = self.get_type_of_expression(
                node_as_switch_statement
                    .expression
                    .ref_(self).as_type_of_expression()
                    .expression,
            )?;
            let witnesses = self
                .get_switch_clause_type_of_witnesses(node, false)
                .into_iter()
                .map(Option::unwrap)
                .collect::<Vec<_>>();
            let not_equal_facts = self.get_facts_from_typeof_switch(0, 0, &witnesses, true);
            let type_ = self
                .get_base_constraint_of_type(operand_type)?
                .unwrap_or(operand_type);
            if type_.ref_(self).flags().intersects(TypeFlags::AnyOrUnknown) {
                return Ok(TypeFacts::AllTypeofNE & not_equal_facts == TypeFacts::AllTypeofNE);
            }
            return Ok(self
                .try_filter_type(type_, |t: Id<Type>| {
                    Ok(self.get_type_facts(t, None)? & not_equal_facts == not_equal_facts)
                })?
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Never));
        }
        let type_ = self.get_type_of_expression(node_as_switch_statement.expression)?;
        if !self.is_literal_type(type_) {
            return Ok(false);
        }
        let switch_types = self.get_switch_clause_types(node)?;
        if switch_types.is_empty()
            || some(
                Some(&switch_types),
                Some(|&switch_type: &Id<Type>| self.is_neither_unit_type_nor_never(switch_type)),
            )
        {
            return Ok(false);
        }
        Ok(self.each_type_contained_in(
            self.map_type(
                type_,
                &mut |type_: Id<Type>| Some(self.get_regular_type_of_literal_type(type_)),
                None,
            )
            .unwrap(),
            &switch_types,
        ))
    }

    pub(super) fn function_has_implicit_return(
        &self,
        func: Id<Node>, /*FunctionLikeDeclaration*/
    ) -> io::Result<bool> {
        Ok(matches!(
            func.ref_(self).as_function_like_declaration().maybe_end_flow_node().as_ref(),
            Some(func_end_flow_node) if self.is_reachable_flow_node(func_end_flow_node.clone())?
        ))
    }

    pub(super) fn check_and_aggregate_return_expression_types(
        &self,
        func: Id<Node>, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        let function_flags = get_function_flags(Some(func), self);
        let mut aggregated_types: Vec<Id<Type>> = vec![];
        let mut has_return_with_no_expression = self.function_has_implicit_return(func)?;
        let mut has_return_of_type_never = false;
        try_for_each_return_statement(
            func.ref_(self).as_function_like_declaration().maybe_body().unwrap(),
            |return_statement: Id<Node>| -> io::Result<_> {
                let expr = return_statement.ref_(self).as_return_statement().expression;
                if let Some(expr) = expr {
                    let mut type_ = self.check_expression_cached(
                        expr,
                        check_mode.map(|check_mode| check_mode & !CheckMode::SkipGenericFunctions),
                    )?;
                    if function_flags.intersects(FunctionFlags::Async) {
                        type_ = self.unwrap_awaited_type(
                            self.check_awaited_type(
                                type_,
                                false,
                                func,
                                &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member,
                                None,
                            )?
                        )?;
                    }
                    if type_.ref_(self).flags().intersects(TypeFlags::Never) {
                        has_return_of_type_never = true;
                    }
                    push_if_unique_eq(&mut aggregated_types, &type_);
                } else {
                    has_return_with_no_expression = true;
                }

                Ok(())
            },
            self,
        )?;
        if aggregated_types.is_empty()
            && !has_return_with_no_expression
            && (has_return_of_type_never || self.may_return_never(func))
        {
            return Ok(None);
        }
        if self.strict_null_checks
            && !aggregated_types.is_empty()
            && has_return_with_no_expression
            && !(self.is_js_constructor(Some(func))?
                && aggregated_types
                    .iter()
                    .any(|&t| t.ref_(self).maybe_symbol() == func.ref_(self).maybe_symbol()))
        {
            push_if_unique_eq(&mut aggregated_types, &self.undefined_type());
        }
        Ok(Some(aggregated_types))
    }

    pub(super) fn may_return_never(&self, func: Id<Node> /*FunctionLikeDeclaration*/) -> bool {
        match func.ref_(self).kind() {
            SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction => true,
            SyntaxKind::MethodDeclaration => {
                func.ref_(self).parent().ref_(self).kind() == SyntaxKind::ObjectLiteralExpression
            }
            _ => false,
        }
    }

    pub(super) fn check_all_code_paths_in_non_void_function_return_or_throw(
        &self,
        func: Id<Node>, /*FunctionLikeDeclaration | MethodSignature*/
        return_type: Option<Id<Type>>,
    ) -> io::Result<()> {
        if !self.produce_diagnostics {
            return Ok(());
        }

        let function_flags = get_function_flags(Some(func), self);
        let type_ = return_type
            .try_map(|return_type| self.unwrap_return_type(return_type, function_flags))?;

        if matches!(
            type_,
            Some(type_) if self.maybe_type_of_kind(type_, TypeFlags::Any | TypeFlags::Void)
        ) {
            return Ok(());
        }

        if func.ref_(self).kind() == SyntaxKind::MethodSignature || {
            let func_ref = func.ref_(self);
            let func_as_function_like_declaration = func_ref.as_function_like_declaration();
            node_is_missing(func_as_function_like_declaration.maybe_body().refed(self).as_deref())
                || func_as_function_like_declaration
                    .maybe_body()
                    .unwrap()
                    .ref_(self).kind()
                    != SyntaxKind::Block
                || !self.function_has_implicit_return(func)?
        } {
            return Ok(());
        }

        let has_explicit_return = func.ref_(self).flags().intersects(NodeFlags::HasExplicitReturn);
        let error_node =
            get_effective_return_type_node(func, self).unwrap_or(func);

        if matches!(
            type_,
            Some(type_) if type_.ref_(self).flags().intersects(TypeFlags::Never)
        ) {
            self.error(
                Some(error_node),
                &Diagnostics::A_function_returning_never_cannot_have_a_reachable_end_point,
                None,
            );
        } else if type_.is_some() && !has_explicit_return {
            self.error(
                Some(error_node),
                &Diagnostics::A_function_whose_declared_type_is_neither_void_nor_any_must_return_a_value,
                None,
            );
        } else if matches!(
            type_,
            Some(type_) if self.strict_null_checks && !self.is_type_assignable_to(self.undefined_type(), type_)?
        ) {
            self.error(
                Some(error_node),
                &Diagnostics::Function_lacks_ending_return_statement_and_return_type_does_not_include_undefined,
                None,
            );
        } else if self.compiler_options.ref_(self).no_implicit_returns == Some(true) {
            if type_.is_none() {
                if !has_explicit_return {
                    return Ok(());
                }
                let inferred_return_type =
                    self.get_return_type_of_signature(self.get_signature_from_declaration_(func)?)?;
                if self.is_unwrapped_return_type_void_or_any(func, inferred_return_type)? {
                    return Ok(());
                }
            }
            self.error(
                Some(error_node),
                &Diagnostics::Not_all_code_paths_return_a_value,
                None,
            );
        }

        Ok(())
    }
}

pub(super) struct CheckAndAggregateYieldOperandTypesReturn {
    pub yield_types: Vec<Id<Type>>,
    pub next_types: Vec<Id<Type>>,
}
