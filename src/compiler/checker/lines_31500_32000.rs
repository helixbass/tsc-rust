#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use super::{signature_has_rest_parameter, CheckMode, IterationTypeKind, TypeFacts, WideningKind};
use crate::{
    create_symbol_table, get_effective_type_annotation_node, get_function_flags, is_import_call,
    is_omitted_expression, is_transient_symbol, last, some, CheckFlags, Diagnostics, FunctionFlags,
    HasTypeInterface, InferenceContext, NamedDeclarationInterface, Node, NodeInterface, Signature,
    Symbol, SymbolFlags, SymbolInterface, SyntaxKind, TransientSymbolInterface, Type, TypeChecker,
    TypeFlags, TypeInterface, UnionReduction, __String,
};

impl TypeChecker {
    pub(super) fn get_type_of_first_parameter_of_signature_with_fallback(
        &self,
        signature: &Signature,
        fallback_type: &Type,
    ) -> Rc<Type> {
        if !signature.parameters().is_empty() {
            self.get_type_at_position(signature, 0)
        } else {
            fallback_type.type_wrapper()
        }
    }

    pub(super) fn infer_from_annotated_parameters(
        &self,
        signature: &Signature,
        context: Rc<Signature>,
        inference_context: &InferenceContext,
    ) {
        let len = signature.parameters().len()
            - if signature_has_rest_parameter(signature) {
                1
            } else {
                0
            };
        for i in 0..len {
            let declaration = signature.parameters()[i].maybe_value_declaration().unwrap();
            if declaration
                .as_parameter_declaration()
                .maybe_type()
                .is_some()
            {
                let type_node = get_effective_type_annotation_node(&declaration);
                if let Some(type_node) = type_node.as_ref() {
                    self.infer_types(
                        &inference_context.inferences,
                        &self.get_type_from_type_node_(type_node),
                        &self.get_type_at_position(&context, i),
                        None,
                        None,
                    );
                }
            }
        }
        let rest_type = self.get_effective_rest_type(&context);
        if let Some(rest_type) = rest_type
            .as_ref()
            .filter(|rest_type| rest_type.flags().intersects(TypeFlags::TypeParameter))
        {
            let instantiated_context = self.instantiate_signature(
                context.clone(),
                &inference_context.non_fixing_mapper(),
                None,
            );
            self.assign_contextual_parameter_types(signature, &instantiated_context);
            let rest_pos = self.get_parameter_count(&context) - 1;
            self.infer_types(
                &inference_context.inferences,
                &self.get_rest_type_at_position(signature, rest_pos),
                rest_type,
                None,
                None,
            );
        }
    }

    pub(super) fn assign_contextual_parameter_types(
        &self,
        signature: &Signature,
        context: &Signature,
    ) {
        if let Some(context_type_parameters) = context.maybe_type_parameters().as_ref() {
            if signature.maybe_type_parameters().is_none() {
                *signature.maybe_type_parameters_mut() = Some(context_type_parameters.clone());
            } else {
                return;
            }
        }
        if let Some(context_this_parameter) = context.maybe_this_parameter().as_ref() {
            let parameter = signature.maybe_this_parameter().clone();
            if match parameter.as_ref() {
                None => true,
                Some(parameter) => matches!(
                    parameter.maybe_value_declaration().as_ref(),
                    Some(parameter_value_declaration) if parameter_value_declaration.as_has_type().maybe_type().is_none()
                ),
            } {
                if parameter.is_none() {
                    *signature.maybe_this_parameter_mut() = Some(
                        self.create_symbol_with_type(context_this_parameter, Option::<&Type>::None),
                    );
                }
                self.assign_parameter_type(
                    signature.maybe_this_parameter().as_ref().unwrap(),
                    Some(self.get_type_of_symbol(context_this_parameter)),
                );
            }
        }
        let len = signature.parameters().len()
            - if signature_has_rest_parameter(signature) {
                1
            } else {
                0
            };
        for i in 0..len {
            let parameter = &signature.parameters()[i];
            if get_effective_type_annotation_node(&parameter.maybe_value_declaration().unwrap())
                .is_none()
            {
                let contextual_parameter_type = self.try_get_type_at_position(context, i);
                self.assign_parameter_type(parameter, contextual_parameter_type);
            }
        }
        if signature_has_rest_parameter(signature) {
            let parameter = last(signature.parameters());
            if is_transient_symbol(parameter)
                || get_effective_type_annotation_node(&parameter.maybe_value_declaration().unwrap())
                    .is_none()
            {
                let contextual_parameter_type = self.get_rest_type_at_position(context, len);
                self.assign_parameter_type(parameter, Some(contextual_parameter_type));
            }
        }
    }

    pub(super) fn assign_non_contextual_parameter_types(&self, signature: &Signature) {
        if let Some(signature_this_parameter) = signature.maybe_this_parameter().as_ref() {
            self.assign_parameter_type(signature_this_parameter, Option::<&Type>::None);
        }
        for parameter in signature.parameters() {
            self.assign_parameter_type(parameter, Option::<&Type>::None);
        }
    }

    pub(super) fn assign_parameter_type<TType: Borrow<Type>>(
        &self,
        parameter: &Symbol,
        type_: Option<TType>,
    ) {
        let links = self.get_symbol_links(parameter);
        let type_ = type_.map(|type_| type_.borrow().type_wrapper());
        if (*links).borrow().type_.is_none() {
            let declaration = parameter.maybe_value_declaration().unwrap();
            links.borrow_mut().type_ = Some(type_.unwrap_or_else(|| {
                self.get_widened_type_for_variable_like_declaration(&declaration, Some(true))
            }));
            let declaration_name = declaration.as_named_declaration().name();
            if declaration_name.kind() != SyntaxKind::Identifier {
                if Rc::ptr_eq(
                    (*links).borrow().type_.as_ref().unwrap(),
                    &self.unknown_type(),
                ) {
                    links.borrow_mut().type_ =
                        Some(self.get_type_from_binding_pattern(&declaration_name, None, None));
                }
                self.assign_binding_element_types(&declaration_name);
            }
        }
    }

    pub(super) fn assign_binding_element_types(&self, pattern: &Node /*BindingPattern*/) {
        for element in pattern.as_has_elements().elements() {
            if !is_omitted_expression(element) {
                let element_as_binding_element = element.as_binding_element();
                if element_as_binding_element.name().kind() == SyntaxKind::Identifier {
                    self.get_symbol_links(&self.get_symbol_of_node(element).unwrap())
                        .borrow_mut()
                        .type_ = self.get_type_for_binding_element(element);
                } else {
                    self.assign_binding_element_types(&element_as_binding_element.name());
                }
            }
        }
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
        let global_promise_like_type = self.get_global_promise_like_type(true);
        if !Rc::ptr_eq(&global_promise_like_type, &self.empty_generic_type()) {
            let promised_type = self
                .get_awaited_type_no_alias(
                    &self.unwrap_awaited_type(promised_type),
                    Option::<&Node>::None,
                    None,
                    None,
                )
                .unwrap_or_else(|| self.unknown_type());
            return self
                .create_type_reference(&global_promise_like_type, Some(vec![promised_type]));
        }

        self.unknown_type()
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

    pub(super) fn create_new_target_expression_type(&self, target_type: &Type) -> Rc<Type> {
        let symbol: Rc<Symbol> = self
            .create_symbol(
                SymbolFlags::None,
                __String::new("NewTargetExpression".to_owned()),
                None,
            )
            .into();

        let target_property_symbol: Rc<Symbol> = self
            .create_symbol(
                SymbolFlags::Property,
                __String::new("target".to_owned()),
                Some(CheckFlags::Readonly),
            )
            .into();
        target_property_symbol.set_parent(Some(symbol.clone()));
        target_property_symbol
            .as_transient_symbol()
            .symbol_links()
            .borrow_mut()
            .type_ = Some(target_type.type_wrapper());

        let members = Rc::new(RefCell::new(create_symbol_table(Some(&[
            target_property_symbol,
        ]))));
        *symbol.maybe_members() = Some(members.clone());
        self.create_anonymous_type(Some(symbol), members, vec![], vec![], vec![])
            .into()
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
        let mut fallback_return_type = self.void_type();
        if func_body.kind() != SyntaxKind::Block {
            return_type = Some(self.check_expression_cached(
                &func_body,
                check_mode.map(|check_mode| check_mode & !CheckMode::SkipGenericFunctions),
            ));
            if is_async {
                return_type = Some(self.unwrap_awaited_type(
                    &self.check_awaited_type(
                        return_type.as_ref().unwrap(),
                        false,
                        func,
                        &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member,
                        None,
                    )
                ));
            }
        } else if is_generator {
            let return_types = self.check_and_aggregate_return_expression_types(func, check_mode);
            match return_types {
                None => {
                    fallback_return_type = self.never_type();
                }
                Some(return_types) => {
                    if !return_types.is_empty() {
                        return_type = Some(self.get_union_type(
                            return_types,
                            Some(UnionReduction::Subtype),
                            Option::<&Symbol>::None,
                            None,
                            Option::<&Type>::None,
                        ));
                    }
                }
            }
            let CheckAndAggregateYieldOperandTypesReturn {
                yield_types,
                next_types,
            } = self.check_and_aggregate_yield_operand_types(func, check_mode);
            yield_type = if some(Some(&yield_types), Option::<fn(&Rc<Type>) -> bool>::None) {
                Some(self.get_union_type(
                    yield_types,
                    Some(UnionReduction::Subtype),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                ))
            } else {
                None
            };
            next_type = if some(Some(&next_types), Option::<fn(&Rc<Type>) -> bool>::None) {
                Some(self.get_intersection_type(&next_types, Option::<&Symbol>::None, None))
            } else {
                None
            };
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
                let contextual_signature =
                    self.get_contextual_signature_for_function_like_declaration(func);
                let contextual_type = contextual_signature.and_then(|contextual_signature| {
                    if Rc::ptr_eq(
                        &contextual_signature,
                        &self.get_signature_from_declaration_(func),
                    ) {
                        if is_generator {
                            None
                        } else {
                            return_type.clone()
                        }
                    } else {
                        self.instantiate_contextual_type(
                            Some(self.get_return_type_of_signature(contextual_signature)),
                            func,
                            None,
                        )
                    }
                });
                if is_generator {
                    yield_type = self
                        .get_widened_literal_like_type_for_contextual_iteration_type_if_needed(
                            yield_type.as_deref(),
                            contextual_type.as_deref(),
                            IterationTypeKind::Yield,
                            is_async,
                        );
                    return_type = self
                        .get_widened_literal_like_type_for_contextual_iteration_type_if_needed(
                            return_type.as_deref(),
                            contextual_type.as_deref(),
                            IterationTypeKind::Return,
                            is_async,
                        );
                    next_type = self
                        .get_widened_literal_like_type_for_contextual_iteration_type_if_needed(
                            next_type.as_deref(),
                            contextual_type.as_deref(),
                            IterationTypeKind::Next,
                            is_async,
                        );
                } else {
                    return_type = self
                        .get_widened_literal_like_type_for_contextual_return_type_if_needed(
                            return_type.as_deref(),
                            contextual_type.as_deref(),
                            is_async,
                        );
                }
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
            self.create_generator_return_type(
                &yield_type.unwrap_or_else(|| self.never_type()),
                &return_type.unwrap_or(fallback_return_type),
                &next_type
                    .or_else(|| self.get_contextual_iteration_type(IterationTypeKind::Next, func))
                    .unwrap_or_else(|| self.unknown_type()),
                is_async,
            )
        } else {
            if is_async {
                self.create_promise_type(&return_type.unwrap_or(fallback_return_type))
            } else {
                return_type.unwrap_or(fallback_return_type)
            }
        }
    }

    pub(super) fn create_generator_return_type(
        &self,
        yield_type: &Type,
        return_type: &Type,
        next_type: &Type,
        is_async_generator: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_and_aggregate_yield_operand_types(
        &self,
        func: &Node, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> CheckAndAggregateYieldOperandTypesReturn {
        unimplemented!()
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

pub(super) struct CheckAndAggregateYieldOperandTypesReturn {
    pub yield_types: Vec<Rc<Type>>,
    pub next_types: Vec<Rc<Type>>,
}
