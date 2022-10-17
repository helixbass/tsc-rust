#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{
    get_iteration_types_key_from_iteration_type_kind, IterationTypeKind, IterationUse, TypeFacts,
};
use crate::{
    append, is_class_static_block_declaration, map, some, ObjectTypeInterface, Signature,
    SignatureKind, SymbolFlags, SymbolInterface, SyntaxKind, __String, escape_leading_underscores,
    get_containing_function_or_class_static_block, get_function_flags, Diagnostics, FunctionFlags,
    IterationTypeCacheKey, IterationTypes, IterationTypesResolver, Node, NodeInterface, Symbol,
    Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_async_from_sync_iteration_types<TErrorNode: Borrow<Node>>(
        &self,
        iteration_types: &IterationTypes,
        error_node: Option<TErrorNode>,
    ) -> Rc<IterationTypes> {
        if ptr::eq(iteration_types, &*self.no_iteration_types()) {
            return self.no_iteration_types();
        }
        if ptr::eq(iteration_types, &*self.any_iteration_types()) {
            return self.any_iteration_types();
        }
        let yield_type = &iteration_types.yield_type();
        let return_type = &iteration_types.return_type();
        let next_type = &iteration_types.next_type();
        if error_node.is_some() {
            self.get_global_awaited_symbol(true);
        }
        let error_node = error_node.map(|error_node| error_node.borrow().node_wrapper());
        self.create_iteration_types(
            Some(
                self.get_awaited_type_(yield_type, error_node.as_deref(), None, None)
                    .unwrap_or_else(|| self.any_type()),
            ),
            Some(
                self.get_awaited_type_(return_type, error_node.as_deref(), None, None)
                    .unwrap_or_else(|| self.any_type()),
            ),
            Some(next_type.clone()),
        )
    }

    pub(super) fn get_iteration_types_of_iterable_worker<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        use_: IterationUse,
        error_node: Option<TErrorNode>,
    ) -> Rc<IterationTypes> {
        if self.is_type_any(Some(type_)) {
            return self.any_iteration_types();
        }

        if use_.intersects(IterationUse::AllowsAsyncIterablesFlag) {
            let iteration_types = self
                .get_iteration_types_of_iterable_cached(type_, &self.async_iteration_types_resolver)
                .or_else(|| {
                    self.get_iteration_types_of_iterable_fast(
                        type_,
                        &self.async_iteration_types_resolver,
                    )
                });
            if let Some(iteration_types) = iteration_types.as_ref() {
                return if use_.intersects(IterationUse::ForOfFlag) {
                    self.get_async_from_sync_iteration_types(iteration_types, error_node)
                } else {
                    iteration_types.clone()
                };
            }
        }

        if use_.intersects(IterationUse::AllowsSyncIterablesFlag) {
            let iteration_types = self
                .get_iteration_types_of_iterable_cached(type_, &self.sync_iteration_types_resolver)
                .or_else(|| {
                    self.get_iteration_types_of_iterable_fast(
                        type_,
                        &self.sync_iteration_types_resolver,
                    )
                });
            if let Some(iteration_types) = iteration_types.as_ref() {
                if use_.intersects(IterationUse::AllowsAsyncIterablesFlag) {
                    if !Rc::ptr_eq(iteration_types, &self.no_iteration_types()) {
                        return self.set_cached_iteration_types(
                            type_,
                            IterationTypeCacheKey::IterationTypesOfAsyncIterable,
                            self.get_async_from_sync_iteration_types(iteration_types, error_node),
                        );
                    }
                } else {
                    return iteration_types.clone();
                }
            }
        }

        let error_node = error_node.map(|error_node| error_node.borrow().node_wrapper());
        if use_.intersects(IterationUse::AllowsAsyncIterablesFlag) {
            let iteration_types = self.get_iteration_types_of_iterable_slow(
                type_,
                &self.async_iteration_types_resolver,
                error_node.as_deref(),
            );
            if !Rc::ptr_eq(&iteration_types, &self.no_iteration_types()) {
                return iteration_types;
            }
        }

        if use_.intersects(IterationUse::AllowsSyncIterablesFlag) {
            let iteration_types = self.get_iteration_types_of_iterable_slow(
                type_,
                &self.sync_iteration_types_resolver,
                error_node.as_deref(),
            );
            if !Rc::ptr_eq(&iteration_types, &self.no_iteration_types()) {
                if use_.intersects(IterationUse::AllowsAsyncIterablesFlag) {
                    return self.set_cached_iteration_types(
                        type_,
                        IterationTypeCacheKey::IterationTypesOfAsyncIterable,
                        // iterationTypes ?
                        self.get_async_from_sync_iteration_types(
                            &iteration_types,
                            error_node.as_deref(),
                        ), // : noIterationTypes
                    );
                } else {
                    return iteration_types;
                }
            }
        }

        self.no_iteration_types()
    }

    pub(super) fn get_iteration_types_of_iterable_cached(
        &self,
        type_: &Type,
        resolver: &IterationTypesResolver,
    ) -> Option<Rc<IterationTypes>> {
        self.get_cached_iteration_types(type_, resolver.iterable_cache_key)
    }

    pub(super) fn get_iteration_types_of_global_iterable_type(
        &self,
        global_type: &Type,
        resolver: &IterationTypesResolver,
    ) -> Rc<IterationTypes> {
        let global_iteration_types = self
            .get_iteration_types_of_iterable_cached(global_type, resolver)
            .unwrap_or_else(|| {
                self.get_iteration_types_of_iterable_slow(
                    global_type,
                    resolver,
                    Option::<&Node>::None,
                )
            });
        if Rc::ptr_eq(&global_iteration_types, &self.no_iteration_types()) {
            self.default_iteration_types()
        } else {
            global_iteration_types
        }
    }

    pub(super) fn get_iteration_types_of_iterable_fast(
        &self,
        type_: &Type,
        resolver: &IterationTypesResolver,
    ) -> Option<Rc<IterationTypes>> {
        let mut global_type: Rc<Type>;
        if self.is_reference_to_type(type_, &*{
            let type_ = (resolver.get_global_iterable_type)(self, false);
            global_type = type_.clone();
            type_
        }) || self.is_reference_to_type(type_, &*{
            let type_ = (resolver.get_global_iterable_iterator_type)(self, false);
            global_type = type_.clone();
            type_
        }) {
            let yield_type = self.get_type_arguments(type_)[0].clone();
            let iteration_types =
                self.get_iteration_types_of_global_iterable_type(&global_type, resolver);
            let return_type = &iteration_types.return_type();
            let next_type = &iteration_types.next_type();
            return Some(
                self.set_cached_iteration_types(
                    type_,
                    resolver.iterable_cache_key,
                    self.create_iteration_types(
                        Some(
                            (resolver.resolve_iteration_type)(self, &yield_type, None)
                                .unwrap_or_else(|| yield_type),
                        ),
                        Some(
                            (resolver.resolve_iteration_type)(self, return_type, None)
                                .unwrap_or_else(|| return_type.clone()),
                        ),
                        Some(next_type.clone()),
                    ),
                ),
            );
        }

        if self.is_reference_to_type(type_, &*(resolver.get_global_generator_type)(self, false)) {
            let type_arguments = self.get_type_arguments(type_);
            let yield_type = &type_arguments[0];
            let return_type = &type_arguments[1];
            let next_type = &type_arguments[2];
            return Some(
                self.set_cached_iteration_types(
                    type_,
                    resolver.iterable_cache_key,
                    self.create_iteration_types(
                        Some(
                            (resolver.resolve_iteration_type)(self, yield_type, None)
                                .unwrap_or_else(|| yield_type.clone()),
                        ),
                        Some(
                            (resolver.resolve_iteration_type)(self, return_type, None)
                                .unwrap_or_else(|| return_type.clone()),
                        ),
                        Some(next_type.clone()),
                    ),
                ),
            );
        }
        None
    }

    pub(super) fn get_property_name_for_known_symbol_name(&self, symbol_name: &str) -> __String {
        let ctor_type = self.get_global_es_symbol_constructor_symbol(false);
        let unique_type = ctor_type.as_ref().and_then(|ctor_type| {
            self.get_type_of_property_of_type_(
                &self.get_type_of_symbol(ctor_type),
                &escape_leading_underscores(symbol_name),
            )
        });
        if let Some(unique_type) = unique_type
            .as_ref()
            .filter(|unique_type| self.is_type_usable_as_property_name(unique_type))
        {
            self.get_property_name_from_type(unique_type).into_owned()
        } else {
            format!("__@{}", symbol_name)
        }
    }

    pub(super) fn get_iteration_types_of_iterable_slow<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        resolver: &IterationTypesResolver,
        error_node: Option<TErrorNode>,
    ) -> Rc<IterationTypes> {
        let method = self.get_property_of_type_(
            type_,
            &self.get_property_name_for_known_symbol_name(resolver.iterator_symbol_name),
            None,
        );
        let method_type = method
            .as_ref()
            .filter(|method| !method.flags().intersects(SymbolFlags::Optional))
            .map(|method| self.get_type_of_symbol(method));
        if self.is_type_any(method_type.as_deref()) {
            return self.set_cached_iteration_types(
                type_,
                resolver.iterable_cache_key,
                self.any_iteration_types(),
            );
        }

        let signatures = method_type
            .as_ref()
            .map(|method_type| self.get_signatures_of_type(method_type, SignatureKind::Call));
        if !some(
            signatures.as_deref(),
            Option::<fn(&Rc<Signature>) -> bool>::None,
        ) {
            return self.set_cached_iteration_types(
                type_,
                resolver.iterable_cache_key,
                self.no_iteration_types(),
            );
        }
        let signatures = signatures.unwrap();

        let iterator_type = self.get_intersection_type(
            &map(&signatures, |signature: &Rc<Signature>, _| {
                self.get_return_type_of_signature(signature.clone())
            }),
            Option::<&Symbol>::None,
            None,
        );
        let iteration_types = self
            .get_iteration_types_of_iterator(&iterator_type, resolver, error_node)
            .unwrap_or_else(|| self.no_iteration_types());
        self.set_cached_iteration_types(type_, resolver.iterable_cache_key, iteration_types)
    }

    pub(super) fn report_type_not_iterable_error(
        &self,
        error_node: &Node,
        type_: &Type,
        allow_async_iterables: bool,
    ) {
        let message = if allow_async_iterables {
            &*Diagnostics::Type_0_must_have_a_Symbol_asyncIterator_method_that_returns_an_async_iterator
        } else {
            &*Diagnostics::Type_0_must_have_a_Symbol_iterator_method_that_returns_an_iterator
        };
        self.error_and_maybe_suggest_await(
            error_node,
            self.get_awaited_type_of_promise(type_, Option::<&Node>::None, None, None)
                .is_some(),
            message,
            Some(vec![self.type_to_string_(
                type_,
                Option::<&Node>::None,
                None,
                None,
            )]),
        );
    }

    pub(super) fn get_iteration_types_of_iterator<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        resolver: &IterationTypesResolver,
        error_node: Option<TErrorNode>,
    ) -> Option<Rc<IterationTypes>> {
        if self.is_type_any(Some(type_)) {
            return Some(self.any_iteration_types());
        }

        let iteration_types = self
            .get_iteration_types_of_iterator_cached(type_, resolver)
            .or_else(|| self.get_iteration_types_of_iterator_fast(type_, resolver))
            .unwrap_or_else(|| {
                self.get_iteration_types_of_iterator_slow(type_, resolver, error_node)
            });
        if Rc::ptr_eq(&iteration_types, &self.no_iteration_types()) {
            None
        } else {
            Some(iteration_types)
        }
    }

    pub(super) fn get_iteration_types_of_iterator_cached(
        &self,
        type_: &Type,
        resolver: &IterationTypesResolver,
    ) -> Option<Rc<IterationTypes>> {
        self.get_cached_iteration_types(type_, resolver.iterator_cache_key)
    }

    pub(super) fn get_iteration_types_of_iterator_fast(
        &self,
        type_: &Type,
        resolver: &IterationTypesResolver,
    ) -> Option<Rc<IterationTypes>> {
        let global_type = (resolver.get_global_iterable_iterator_type)(self, false);
        if self.is_reference_to_type(type_, &global_type) {
            let yield_type = self.get_type_arguments(type_)[0].clone();
            let global_iteration_types = self
                .get_iteration_types_of_iterator_cached(&global_type, resolver)
                .unwrap_or_else(|| {
                    self.get_iteration_types_of_iterator_slow(
                        &global_type,
                        resolver,
                        Option::<&Node>::None,
                    )
                });
            let iteration_types = if Rc::ptr_eq(&global_iteration_types, &self.no_iteration_types())
            {
                self.default_iteration_types()
            } else {
                global_iteration_types
            };
            let return_type = &iteration_types.return_type();
            let next_type = &iteration_types.next_type();
            return Some(self.set_cached_iteration_types(
                type_,
                resolver.iterator_cache_key,
                self.create_iteration_types(
                    Some(yield_type),
                    Some(return_type.clone()),
                    Some(next_type.clone()),
                ),
            ));
        }
        if self.is_reference_to_type(type_, &(resolver.get_global_iterator_type)(self, false))
            || self.is_reference_to_type(type_, &(resolver.get_global_generator_type)(self, false))
        {
            let type_arguments = self.get_type_arguments(type_);
            let yield_type = &type_arguments[0];
            let return_type = &type_arguments[1];
            let next_type = &type_arguments[2];
            return Some(self.set_cached_iteration_types(
                type_,
                resolver.iterator_cache_key,
                self.create_iteration_types(
                    Some(yield_type.clone()),
                    Some(return_type.clone()),
                    Some(next_type.clone()),
                ),
            ));
        }
        None
    }

    pub(super) fn is_iterator_result(
        &self,
        type_: &Type,
        kind: IterationTypeKind, /*IterationTypeKind.Yield | IterationTypeKind.Return*/
    ) -> bool {
        let done_type = self
            .get_type_of_property_of_type_(type_, "done")
            .unwrap_or_else(|| self.false_type());
        self.is_type_assignable_to(
            &*if kind == IterationTypeKind::Yield {
                self.false_type()
            } else {
                self.true_type()
            },
            &done_type,
        )
    }

    pub(super) fn is_yield_iterator_result(&self, type_: &Type) -> bool {
        self.is_iterator_result(type_, IterationTypeKind::Yield)
    }

    pub(super) fn is_return_iterator_result(&self, type_: &Type) -> bool {
        self.is_iterator_result(type_, IterationTypeKind::Return)
    }

    pub(super) fn get_iteration_types_of_iterator_result(
        &self,
        type_: &Type,
    ) -> Rc<IterationTypes> {
        if self.is_type_any(Some(type_)) {
            return self.any_iteration_types();
        }

        let cached_types = self.get_cached_iteration_types(
            type_,
            IterationTypeCacheKey::IterationTypesOfIteratorResult,
        );
        if let Some(cached_types) = cached_types {
            return cached_types;
        }

        if self.is_reference_to_type(type_, &self.get_global_iterator_yield_result_type(false)) {
            let yield_type = self.get_type_arguments(type_)[0].clone();
            return self.set_cached_iteration_types(
                type_,
                IterationTypeCacheKey::IterationTypesOfIteratorResult,
                self.create_iteration_types(Some(yield_type), None, None),
            );
        }
        if self.is_reference_to_type(type_, &self.get_global_iterator_return_result_type(false)) {
            let return_type = self.get_type_arguments(type_)[0].clone();
            return self.set_cached_iteration_types(
                type_,
                IterationTypeCacheKey::IterationTypesOfIteratorResult,
                self.create_iteration_types(None, Some(return_type), None),
            );
        }

        let yield_iterator_result =
            self.filter_type(type_, |type_| self.is_yield_iterator_result(type_));
        let yield_type = if !Rc::ptr_eq(&yield_iterator_result, &self.never_type()) {
            self.get_type_of_property_of_type_(&yield_iterator_result, "value")
        } else {
            None
        };

        let return_iterator_result =
            self.filter_type(type_, |type_| self.is_return_iterator_result(type_));
        let return_type = if !Rc::ptr_eq(&return_iterator_result, &self.never_type()) {
            self.get_type_of_property_of_type_(&return_iterator_result, "value")
        } else {
            None
        };

        if yield_type.is_none() && return_type.is_none() {
            return self.set_cached_iteration_types(
                type_,
                IterationTypeCacheKey::IterationTypesOfIteratorResult,
                self.no_iteration_types(),
            );
        }

        self.set_cached_iteration_types(
            type_,
            IterationTypeCacheKey::IterationTypesOfIteratorResult,
            self.create_iteration_types(
                yield_type,
                Some(return_type.unwrap_or_else(|| self.void_type())),
                None,
            ),
        )
    }

    pub(super) fn get_iteration_types_of_method<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        resolver: &IterationTypesResolver,
        method_name: &str, /*"next" | "return" | "throw"*/
        error_node: Option<TErrorNode>,
    ) -> Option<Rc<IterationTypes>> {
        let method = self.get_property_of_type_(type_, method_name, None);

        if method.is_none() && method_name != "next" {
            return None;
        }

        let method_type = method
            .as_ref()
            .filter(|method| {
                !(method_name == "next" && method.flags().intersects(SymbolFlags::Optional))
            })
            .map(|method| {
                if method_name == "next" {
                    self.get_type_of_symbol(method)
                } else {
                    self.get_type_with_facts(
                        &self.get_type_of_symbol(method),
                        TypeFacts::NEUndefinedOrNull,
                    )
                }
            });

        if self.is_type_any(method_type.as_deref()) {
            return Some(if method_name == "next" {
                self.any_iteration_types()
            } else {
                self.any_iteration_types_except_next()
            });
        }

        let method_signatures = if let Some(method_type) = method_type.as_ref() {
            self.get_signatures_of_type(method_type, SignatureKind::Call)
        } else {
            vec![]
        };
        if method_signatures.is_empty() {
            if error_node.is_some() {
                let diagnostic = if method_name == "next" {
                    resolver.must_have_a_next_method_diagnostic
                } else {
                    resolver.must_be_a_method_diagnostic
                };
                self.error(error_node, diagnostic, Some(vec![method_name.to_owned()]));
            }
            return if method_name == "next" {
                Some(self.any_iteration_types())
            } else {
                None
            };
        }

        if let Some(method_type_symbol) = method_type
            .as_ref()
            .and_then(|method_type| method_type.maybe_symbol())
            .as_ref()
        {
            if method_signatures.len() == 1 {
                let global_generator_type = (resolver.get_global_generator_type)(self, false);
                let global_iterator_type = (resolver.get_global_iterator_type)(self, false);
                let is_generator_method = matches!(
                    global_generator_type.maybe_symbol().and_then(|global_generator_type_symbol| {
                        global_generator_type_symbol.maybe_members().clone()
                    }).and_then(|global_generator_type_symbol_members| {
                        (*global_generator_type_symbol_members).borrow().get(method_name).cloned()
                    }).as_ref(),
                    Some(global_generator_type_symbol_member) if Rc::ptr_eq(
                        global_generator_type_symbol_member,
                        method_type_symbol
                    )
                );
                let is_iterator_method = !is_generator_method
                    && matches!(
                        global_iterator_type.maybe_symbol().and_then(|global_iterator_type_symbol| {
                            global_iterator_type_symbol.maybe_members().clone()
                        }).and_then(|global_iterator_type_symbol_members| {
                            (*global_iterator_type_symbol_members).borrow().get(method_name).cloned()
                        }).as_ref(),
                        Some(global_iterator_type_symbol_member) if Rc::ptr_eq(
                            global_iterator_type_symbol_member,
                            method_type_symbol
                        )
                    );
                if is_generator_method || is_iterator_method {
                    let global_type = if is_generator_method {
                        &global_generator_type
                    } else {
                        &global_iterator_type
                    };
                    let method_type = method_type.as_ref().unwrap();
                    let mapper = method_type.as_object_type().maybe_mapper();
                    return Some(
                        self.create_iteration_types(
                            Some(
                                self.get_mapped_type(
                                    &global_type
                                        .as_generic_type()
                                        .maybe_type_parameters()
                                        .unwrap()[0],
                                    &mapper.clone().unwrap(),
                                ),
                            ),
                            Some(
                                self.get_mapped_type(
                                    &global_type
                                        .as_generic_type()
                                        .maybe_type_parameters()
                                        .unwrap()[1],
                                    &mapper.clone().unwrap(),
                                ),
                            ),
                            if method_name == "next" {
                                Some(
                                    self.get_mapped_type(
                                        &global_type
                                            .as_generic_type()
                                            .maybe_type_parameters()
                                            .unwrap()[2],
                                        &mapper.unwrap(),
                                    ),
                                )
                            } else {
                                None
                            },
                        ),
                    );
                }
            }
        }

        let mut method_parameter_types: Option<Vec<Rc<Type>>> = None;
        let mut method_return_types: Option<Vec<Rc<Type>>> = None;
        for signature in &method_signatures {
            if method_name != "throw"
                && some(
                    Some(signature.parameters()),
                    Option::<fn(&Rc<Symbol>) -> bool>::None,
                )
            {
                if method_parameter_types.is_none() {
                    method_parameter_types = Some(vec![]);
                }
                append(
                    method_parameter_types.as_mut().unwrap(),
                    Some(self.get_type_at_position(signature, 0)),
                );
            }
            if method_return_types.is_none() {
                method_return_types = Some(vec![]);
            }
            append(
                method_return_types.as_mut().unwrap(),
                Some(self.get_return_type_of_signature(signature.clone())),
            );
        }

        let mut return_types: Option<Vec<Rc<Type>>> = None;
        let mut next_type: Option<Rc<Type>> = None;
        let error_node = error_node.map(|error_node| error_node.borrow().node_wrapper());
        if method_name != "throw" {
            let method_parameter_type = if let Some(method_parameter_types) = method_parameter_types
            {
                self.get_union_type(
                    method_parameter_types,
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                )
            } else {
                self.unknown_type()
            };
            if method_name == "next" {
                next_type = Some(method_parameter_type.clone());
            } else if method_name == "return" {
                let resolved_method_parameter_type = (resolver.resolve_iteration_type)(
                    self,
                    &method_parameter_type,
                    error_node.clone(),
                );
                if return_types.is_none() && resolved_method_parameter_type.is_some() {
                    return_types = Some(vec![]);
                }
                if resolved_method_parameter_type.is_some() {
                    append(
                        return_types.as_mut().unwrap(),
                        resolved_method_parameter_type,
                    );
                }
            }
        }

        let yield_type: Rc<Type>;
        let method_return_type = if let Some(method_return_types) = method_return_types.as_ref() {
            self.get_intersection_type(method_return_types, Option::<&Symbol>::None, None)
        } else {
            self.never_type()
        };
        let resolved_method_return_type =
            (resolver.resolve_iteration_type)(self, &method_return_type, error_node.clone())
                .unwrap_or_else(|| self.any_type());
        let iteration_types =
            self.get_iteration_types_of_iterator_result(&resolved_method_return_type);
        if Rc::ptr_eq(&iteration_types, &self.no_iteration_types()) {
            if error_node.is_some() {
                self.error(
                    error_node.as_deref(),
                    resolver.must_have_a_value_diagnostic,
                    Some(vec![method_name.to_owned()]),
                );
            }
            yield_type = self.any_type();
            if return_types.is_none() {
                return_types = Some(vec![]);
            }
            append(return_types.as_mut().unwrap(), Some(self.any_type()));
        } else {
            yield_type = iteration_types.yield_type();
            if return_types.is_none() {
                return_types = Some(vec![]);
            }
            append(
                return_types.as_mut().unwrap(),
                Some(iteration_types.return_type()),
            );
        }

        Some(self.create_iteration_types(
            Some(yield_type),
            Some(self.get_union_type(
                return_types.unwrap(),
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )),
            next_type,
        ))
    }

    pub(super) fn get_iteration_types_of_iterator_slow<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        resolver: &IterationTypesResolver,
        error_node: Option<TErrorNode>,
    ) -> Rc<IterationTypes> {
        let error_node = error_node.map(|error_node| error_node.borrow().node_wrapper());
        let iteration_types = self.combine_iteration_types(&[
            self.get_iteration_types_of_method(type_, resolver, "next", error_node.as_deref()),
            self.get_iteration_types_of_method(type_, resolver, "return", error_node.as_deref()),
            self.get_iteration_types_of_method(type_, resolver, "throw", error_node.as_deref()),
        ]);
        self.set_cached_iteration_types(type_, resolver.iterator_cache_key, iteration_types)
    }

    pub(super) fn get_iteration_type_of_generator_function_return_type(
        &self,
        kind: IterationTypeKind,
        return_type: &Type,
        is_async_generator: bool,
    ) -> Option<Rc<Type>> {
        if self.is_type_any(Some(return_type)) {
            return None;
        }

        let iteration_types = self
            .get_iteration_types_of_generator_function_return_type(return_type, is_async_generator);
        iteration_types.map(|iteration_types| {
            iteration_types.get_by_key(get_iteration_types_key_from_iteration_type_kind(kind))
        })
    }

    pub(super) fn get_iteration_types_of_generator_function_return_type(
        &self,
        type_: &Type,
        is_async_generator: bool,
    ) -> Option<Rc<IterationTypes>> {
        if self.is_type_any(Some(type_)) {
            return Some(self.any_iteration_types());
        }

        let use_ = if is_async_generator {
            IterationUse::AsyncGeneratorReturnType
        } else {
            IterationUse::GeneratorReturnType
        };
        let resolver = if is_async_generator {
            &self.async_iteration_types_resolver
        } else {
            &self.sync_iteration_types_resolver
        };
        self.get_iteration_types_of_iterable(type_, use_, Option::<&Node>::None)
            .or_else(|| {
                self.get_iteration_types_of_iterator(type_, resolver, Option::<&Node>::None)
            })
    }

    pub(super) fn check_break_or_continue_statement(
        &self,
        node: &Node, /*BreakOrContinueStatement*/
    ) {
        if !self.check_grammar_statement_in_ambient_context(node) {
            self.check_grammar_break_or_continue_statement(node);
        }
    }

    pub(super) fn unwrap_return_type(
        &self,
        return_type: &Type,
        function_flags: FunctionFlags,
    ) -> Rc<Type> {
        let is_generator = function_flags.intersects(FunctionFlags::Generator);
        let is_async = function_flags.intersects(FunctionFlags::Async);
        if is_generator {
            self.get_iteration_type_of_generator_function_return_type(
                IterationTypeKind::Return,
                return_type,
                is_async,
            )
            .unwrap_or_else(|| self.error_type())
        } else if is_async {
            self.get_awaited_type_no_alias(return_type, Option::<&Node>::None, None, None)
                .unwrap_or_else(|| self.error_type())
        } else {
            return_type.type_wrapper()
        }
    }

    pub(super) fn is_unwrapped_return_type_void_or_any(
        &self,
        func: &Node, /*SignatureDeclaration*/
        return_type: &Type,
    ) -> bool {
        let unwrapped_return_type =
            self.unwrap_return_type(return_type, get_function_flags(Some(func)));
        /* !!unwrappedReturnType &&*/
        self.maybe_type_of_kind(
            &unwrapped_return_type,
            TypeFlags::Void | TypeFlags::AnyOrUnknown,
        )
    }

    pub(super) fn check_return_statement(&self, node: &Node /*ReturnStatement*/) {
        if self.check_grammar_statement_in_ambient_context(node) {
            return;
        }

        let container = get_containing_function_or_class_static_block(node);
        if matches!(
            container.as_ref(),
            Some(container) if is_class_static_block_declaration(container)
        ) {
            self.grammar_error_on_first_token(
                node,
                &Diagnostics::A_return_statement_cannot_be_used_inside_a_class_static_block,
                None,
            );
            return;
        }

        if container.is_none() {
            self.grammar_error_on_first_token(
                node,
                &Diagnostics::A_return_statement_can_only_be_used_within_a_function_body,
                None,
            );
            return;
        }
        let container = container.unwrap();

        let signature = self.get_signature_from_declaration_(&container);
        let return_type = self.get_return_type_of_signature(signature);
        let function_flags = get_function_flags(Some(&*container));
        let node_as_return_statement = node.as_return_statement();
        if self.strict_null_checks
            || node_as_return_statement.expression.is_some()
            || return_type.flags().intersects(TypeFlags::Never)
        {
            let expr_type = match node_as_return_statement.expression.as_ref() {
                Some(expression) => self.check_expression_cached(&expression, None),
                None => self.undefined_type(),
            };
            if container.kind() == SyntaxKind::SetAccessor {
                if node_as_return_statement.expression.is_some() {
                    self.error(
                        Some(node),
                        &Diagnostics::Setters_cannot_return_a_value,
                        None,
                    );
                }
            } else if container.kind() == SyntaxKind::Constructor {
                if matches!(
                    node_as_return_statement.expression.as_ref(),
                    Some(node_expression) if !self.check_type_assignable_to_and_optionally_elaborate(
                        &expr_type,
                        &return_type,
                        Some(node),
                        Some(&**node_expression),
                        None, None,
                    )
                ) {
                    self.error(
                        Some(node),
                        &Diagnostics::Return_type_of_constructor_signature_must_be_assignable_to_the_instance_type_of_the_class,
                        None,
                    );
                }
            } else if self.get_return_type_from_annotation(&container).is_some() {
                let unwrapped_return_type = self
                    .unwrap_return_type(&return_type, function_flags)/*?? returnType*/;
                let unwrapped_expr_type = if function_flags.intersects(FunctionFlags::Async) {
                    self.check_awaited_type(
                        &expr_type,
                        false,
                        node,
                        &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member,
                        None,
                    )
                } else {
                    expr_type
                };
                // if unwrappedReturnType {
                self.check_type_assignable_to_and_optionally_elaborate(
                    &unwrapped_expr_type,
                    &unwrapped_return_type,
                    Some(node),
                    node_as_return_statement.expression.as_deref(),
                    None,
                    None,
                );
                // }
            }
        } else if container.kind() != SyntaxKind::Constructor
            && self.compiler_options.no_implicit_returns == Some(true)
            && !self.is_unwrapped_return_type_void_or_any(&container, &return_type)
        {
            self.error(
                Some(node),
                &Diagnostics::Not_all_code_paths_return_a_value,
                None,
            );
        }
    }
}
