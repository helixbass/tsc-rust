use std::io;

use id_arena::Id;

use super::{
    get_iteration_types_key_from_iteration_type_kind, IterationTypeKind, IterationUse, TypeFacts,
};
use crate::{
    append, is_class_static_block_declaration, some, ObjectTypeInterface, Signature, SignatureKind,
    SymbolFlags, SymbolInterface, SyntaxKind, __String, escape_leading_underscores,
    get_containing_function_or_class_static_block, get_function_flags, released, try_map,
    Diagnostics, FunctionFlags, InArena, IterationTypeCacheKey, IterationTypes,
    IterationTypesResolver, Node, NodeInterface, OptionTry, Symbol, Type, TypeChecker, TypeFlags,
    TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_async_from_sync_iteration_types(
        &self,
        iteration_types: Id<IterationTypes>,
        error_node: Option<Id<Node>>,
    ) -> io::Result<Id<IterationTypes>> {
        if iteration_types == self.no_iteration_types() {
            return Ok(self.no_iteration_types());
        }
        if iteration_types == self.any_iteration_types() {
            return Ok(self.any_iteration_types());
        }
        let yield_type = iteration_types.ref_(self).yield_type();
        let return_type = iteration_types.ref_(self).return_type();
        let next_type = iteration_types.ref_(self).next_type();
        if error_node.is_some() {
            self.get_global_awaited_symbol(true)?;
        }
        Ok(self.create_iteration_types(
            Some(
                self.get_awaited_type_(yield_type, error_node, None, None)?
                    .unwrap_or_else(|| self.any_type()),
            ),
            Some(
                self.get_awaited_type_(return_type, error_node, None, None)?
                    .unwrap_or_else(|| self.any_type()),
            ),
            Some(next_type.clone()),
        ))
    }

    pub(super) fn get_iteration_types_of_iterable_worker(
        &self,
        type_: Id<Type>,
        use_: IterationUse,
        error_node: Option<Id<Node>>,
    ) -> io::Result<Id<IterationTypes>> {
        if self.is_type_any(Some(type_)) {
            return Ok(self.any_iteration_types());
        }

        if use_.intersects(IterationUse::AllowsAsyncIterablesFlag) {
            let iteration_types = self
                .get_iteration_types_of_iterable_cached(type_, &self.async_iteration_types_resolver)
                .try_or_else(|| {
                    self.get_iteration_types_of_iterable_fast(
                        type_,
                        &self.async_iteration_types_resolver,
                    )
                })?;
            if let Some(iteration_types) = iteration_types {
                return Ok(if use_.intersects(IterationUse::ForOfFlag) {
                    self.get_async_from_sync_iteration_types(iteration_types, error_node)?
                } else {
                    iteration_types.clone()
                });
            }
        }

        if use_.intersects(IterationUse::AllowsSyncIterablesFlag) {
            let iteration_types = self
                .get_iteration_types_of_iterable_cached(type_, &self.sync_iteration_types_resolver)
                .try_or_else(|| {
                    self.get_iteration_types_of_iterable_fast(
                        type_,
                        &self.sync_iteration_types_resolver,
                    )
                })?;
            if let Some(iteration_types) = iteration_types {
                if use_.intersects(IterationUse::AllowsAsyncIterablesFlag) {
                    if iteration_types != self.no_iteration_types() {
                        return Ok(self.set_cached_iteration_types(
                            type_,
                            IterationTypeCacheKey::IterationTypesOfAsyncIterable,
                            self.get_async_from_sync_iteration_types(iteration_types, error_node)?,
                        ));
                    }
                } else {
                    return Ok(iteration_types.clone());
                }
            }
        }

        if use_.intersects(IterationUse::AllowsAsyncIterablesFlag) {
            let iteration_types = self.get_iteration_types_of_iterable_slow(
                type_,
                &self.async_iteration_types_resolver,
                error_node,
            )?;
            if iteration_types != self.no_iteration_types() {
                return Ok(iteration_types);
            }
        }

        if use_.intersects(IterationUse::AllowsSyncIterablesFlag) {
            let iteration_types = self.get_iteration_types_of_iterable_slow(
                type_,
                &self.sync_iteration_types_resolver,
                error_node,
            )?;
            if iteration_types != self.no_iteration_types() {
                if use_.intersects(IterationUse::AllowsAsyncIterablesFlag) {
                    return Ok(self.set_cached_iteration_types(
                        type_,
                        IterationTypeCacheKey::IterationTypesOfAsyncIterable,
                        // iterationTypes ?
                        self.get_async_from_sync_iteration_types(iteration_types, error_node)?, // : noIterationTypes
                    ));
                } else {
                    return Ok(iteration_types);
                }
            }
        }

        Ok(self.no_iteration_types())
    }

    pub(super) fn get_iteration_types_of_iterable_cached(
        &self,
        type_: Id<Type>,
        resolver: &IterationTypesResolver,
    ) -> Option<Id<IterationTypes>> {
        self.get_cached_iteration_types(type_, resolver.iterable_cache_key)
    }

    pub(super) fn get_iteration_types_of_global_iterable_type(
        &self,
        global_type: Id<Type>,
        resolver: &IterationTypesResolver,
    ) -> io::Result<Id<IterationTypes>> {
        let global_iteration_types = self
            .get_iteration_types_of_iterable_cached(global_type, resolver)
            .try_unwrap_or_else(|| {
                self.get_iteration_types_of_iterable_slow(
                    global_type,
                    resolver,
                    Option::<Id<Node>>::None,
                )
            })?;
        Ok(if global_iteration_types == self.no_iteration_types() {
            self.default_iteration_types()
        } else {
            global_iteration_types
        })
    }

    pub(super) fn get_iteration_types_of_iterable_fast(
        &self,
        type_: Id<Type>,
        resolver: &IterationTypesResolver,
    ) -> io::Result<Option<Id<IterationTypes>>> {
        let mut global_type: Id<Type>;
        if self.is_reference_to_type(type_, {
            let type_ = (resolver.get_global_iterable_type)(self, false)?;
            global_type = type_.clone();
            type_
        }) || self.is_reference_to_type(type_, {
            let type_ = (resolver.get_global_iterable_iterator_type)(self, false)?;
            global_type = type_.clone();
            type_
        }) {
            let yield_type = self.get_type_arguments(type_)?[0].clone();
            let iteration_types =
                self.get_iteration_types_of_global_iterable_type(global_type, resolver)?;
            let return_type = iteration_types.ref_(self).return_type();
            let next_type = iteration_types.ref_(self).next_type();
            return Ok(Some(
                self.set_cached_iteration_types(
                    type_,
                    resolver.iterable_cache_key,
                    self.create_iteration_types(
                        Some(
                            (resolver.resolve_iteration_type)(self, yield_type, None)?
                                .unwrap_or_else(|| yield_type),
                        ),
                        Some(
                            (resolver.resolve_iteration_type)(self, return_type, None)?
                                .unwrap_or_else(|| return_type.clone()),
                        ),
                        Some(next_type.clone()),
                    ),
                ),
            ));
        }

        if self.is_reference_to_type(type_, (resolver.get_global_generator_type)(self, false)?) {
            let type_arguments = self.get_type_arguments(type_)?;
            let yield_type = type_arguments[0];
            let return_type = type_arguments[1];
            let next_type = type_arguments[2];
            return Ok(Some(
                self.set_cached_iteration_types(
                    type_,
                    resolver.iterable_cache_key,
                    self.create_iteration_types(
                        Some(
                            (resolver.resolve_iteration_type)(self, yield_type, None)?
                                .unwrap_or_else(|| yield_type.clone()),
                        ),
                        Some(
                            (resolver.resolve_iteration_type)(self, return_type, None)?
                                .unwrap_or_else(|| return_type.clone()),
                        ),
                        Some(next_type.clone()),
                    ),
                ),
            ));
        }
        Ok(None)
    }

    pub(super) fn get_property_name_for_known_symbol_name(
        &self,
        symbol_name: &str,
    ) -> io::Result<__String> {
        let ctor_type = self.get_global_es_symbol_constructor_symbol(false)?;
        let unique_type = ctor_type.try_and_then(|ctor_type| {
            self.get_type_of_property_of_type_(
                self.get_type_of_symbol(ctor_type)?,
                &escape_leading_underscores(symbol_name),
            )
        })?;
        Ok(
            if let Some(unique_type) =
                unique_type.filter(|&unique_type| self.is_type_usable_as_property_name(unique_type))
            {
                self.get_property_name_from_type(unique_type)
            } else {
                format!("__@{}", symbol_name)
            },
        )
    }

    pub(super) fn get_iteration_types_of_iterable_slow(
        &self,
        type_: Id<Type>,
        resolver: &IterationTypesResolver,
        error_node: Option<Id<Node>>,
    ) -> io::Result<Id<IterationTypes>> {
        let method = self.get_property_of_type_(
            type_,
            &*self.get_property_name_for_known_symbol_name(resolver.iterator_symbol_name)?,
            None,
        )?;
        let method_type = method
            .filter(|&method| !method.ref_(self).flags().intersects(SymbolFlags::Optional))
            .try_map(|method| self.get_type_of_symbol(method))?;
        if self.is_type_any(method_type) {
            return Ok(self.set_cached_iteration_types(
                type_,
                resolver.iterable_cache_key,
                self.any_iteration_types(),
            ));
        }

        let signatures = method_type
            .try_map(|method_type| self.get_signatures_of_type(method_type, SignatureKind::Call))?;
        if !some(
            signatures.as_deref(),
            Option::<fn(&Id<Signature>) -> bool>::None,
        ) {
            return Ok(self.set_cached_iteration_types(
                type_,
                resolver.iterable_cache_key,
                self.no_iteration_types(),
            ));
        }
        let signatures = signatures.unwrap();

        let iterator_type = self.get_intersection_type(
            &try_map(&signatures, |signature: &Id<Signature>, _| {
                self.get_return_type_of_signature(signature.clone())
            })?,
            Option::<Id<Symbol>>::None,
            None,
        )?;
        let iteration_types = self
            .get_iteration_types_of_iterator(iterator_type, resolver, error_node)?
            .unwrap_or_else(|| self.no_iteration_types());
        Ok(self.set_cached_iteration_types(type_, resolver.iterable_cache_key, iteration_types))
    }

    pub(super) fn report_type_not_iterable_error(
        &self,
        error_node: Id<Node>,
        type_: Id<Type>,
        allow_async_iterables: bool,
    ) -> io::Result<()> {
        let message = if allow_async_iterables {
            &*Diagnostics::Type_0_must_have_a_Symbol_asyncIterator_method_that_returns_an_async_iterator
        } else {
            &*Diagnostics::Type_0_must_have_a_Symbol_iterator_method_that_returns_an_iterator
        };
        self.error_and_maybe_suggest_await(
            error_node,
            self.get_awaited_type_of_promise(type_, Option::<Id<Node>>::None, None, None)?
                .is_some(),
            message,
            Some(vec![self.type_to_string_(
                type_,
                Option::<Id<Node>>::None,
                None,
                None,
            )?]),
        );

        Ok(())
    }

    pub(super) fn get_iteration_types_of_iterator(
        &self,
        type_: Id<Type>,
        resolver: &IterationTypesResolver,
        error_node: Option<Id<Node>>,
    ) -> io::Result<Option<Id<IterationTypes>>> {
        if self.is_type_any(Some(type_)) {
            return Ok(Some(self.any_iteration_types()));
        }

        let iteration_types = self
            .get_iteration_types_of_iterator_cached(type_, resolver)
            .try_or_else(|| self.get_iteration_types_of_iterator_fast(type_, resolver))?
            .try_unwrap_or_else(|| {
                self.get_iteration_types_of_iterator_slow(type_, resolver, error_node)
            })?;
        Ok(if iteration_types == self.no_iteration_types() {
            None
        } else {
            Some(iteration_types)
        })
    }

    pub(super) fn get_iteration_types_of_iterator_cached(
        &self,
        type_: Id<Type>,
        resolver: &IterationTypesResolver,
    ) -> Option<Id<IterationTypes>> {
        self.get_cached_iteration_types(type_, resolver.iterator_cache_key)
    }

    pub(super) fn get_iteration_types_of_iterator_fast(
        &self,
        type_: Id<Type>,
        resolver: &IterationTypesResolver,
    ) -> io::Result<Option<Id<IterationTypes>>> {
        let global_type = (resolver.get_global_iterable_iterator_type)(self, false)?;
        if self.is_reference_to_type(type_, global_type) {
            let yield_type = self.get_type_arguments(type_)?[0].clone();
            let global_iteration_types = self
                .get_iteration_types_of_iterator_cached(global_type, resolver)
                .try_unwrap_or_else(|| {
                    self.get_iteration_types_of_iterator_slow(
                        global_type,
                        resolver,
                        Option::<Id<Node>>::None,
                    )
                })?;
            let iteration_types = if global_iteration_types == self.no_iteration_types() {
                self.default_iteration_types()
            } else {
                global_iteration_types
            };
            let return_type = iteration_types.ref_(self).return_type();
            let next_type = iteration_types.ref_(self).next_type();
            return Ok(Some(self.set_cached_iteration_types(
                type_,
                resolver.iterator_cache_key,
                self.create_iteration_types(
                    Some(yield_type),
                    Some(return_type.clone()),
                    Some(next_type.clone()),
                ),
            )));
        }
        if self.is_reference_to_type(type_, (resolver.get_global_iterator_type)(self, false)?)
            || self.is_reference_to_type(type_, (resolver.get_global_generator_type)(self, false)?)
        {
            let type_arguments = self.get_type_arguments(type_)?;
            let yield_type = &type_arguments[0];
            let return_type = &type_arguments[1];
            let next_type = &type_arguments[2];
            return Ok(Some(self.set_cached_iteration_types(
                type_,
                resolver.iterator_cache_key,
                self.create_iteration_types(
                    Some(yield_type.clone()),
                    Some(return_type.clone()),
                    Some(next_type.clone()),
                ),
            )));
        }
        Ok(None)
    }

    pub(super) fn is_iterator_result(
        &self,
        type_: Id<Type>,
        kind: IterationTypeKind, /*IterationTypeKind.Yield | IterationTypeKind.Return*/
    ) -> io::Result<bool> {
        let done_type = self
            .get_type_of_property_of_type_(type_, "done")?
            .unwrap_or_else(|| self.false_type());
        self.is_type_assignable_to(
            if kind == IterationTypeKind::Yield {
                self.false_type()
            } else {
                self.true_type()
            },
            done_type,
        )
    }

    pub(super) fn is_yield_iterator_result(&self, type_: Id<Type>) -> io::Result<bool> {
        self.is_iterator_result(type_, IterationTypeKind::Yield)
    }

    pub(super) fn is_return_iterator_result(&self, type_: Id<Type>) -> io::Result<bool> {
        self.is_iterator_result(type_, IterationTypeKind::Return)
    }

    pub(super) fn get_iteration_types_of_iterator_result(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Id<IterationTypes>> {
        if self.is_type_any(Some(type_)) {
            return Ok(self.any_iteration_types());
        }

        let cached_types = self.get_cached_iteration_types(
            type_,
            IterationTypeCacheKey::IterationTypesOfIteratorResult,
        );
        if let Some(cached_types) = cached_types {
            return Ok(cached_types);
        }

        if self.is_reference_to_type(type_, self.get_global_iterator_yield_result_type(false)?) {
            let yield_type = self.get_type_arguments(type_)?[0].clone();
            return Ok(self.set_cached_iteration_types(
                type_,
                IterationTypeCacheKey::IterationTypesOfIteratorResult,
                self.create_iteration_types(Some(yield_type), None, None),
            ));
        }
        if self.is_reference_to_type(type_, self.get_global_iterator_return_result_type(false)?) {
            let return_type = self.get_type_arguments(type_)?[0].clone();
            return Ok(self.set_cached_iteration_types(
                type_,
                IterationTypeCacheKey::IterationTypesOfIteratorResult,
                self.create_iteration_types(None, Some(return_type), None),
            ));
        }

        let yield_iterator_result =
            self.try_filter_type(type_, |type_| self.is_yield_iterator_result(type_))?;
        let yield_type = if yield_iterator_result != self.never_type() {
            self.get_type_of_property_of_type_(yield_iterator_result, "value")?
        } else {
            None
        };

        let return_iterator_result =
            self.try_filter_type(type_, |type_| self.is_return_iterator_result(type_))?;
        let return_type = if return_iterator_result != self.never_type() {
            self.get_type_of_property_of_type_(return_iterator_result, "value")?
        } else {
            None
        };

        if yield_type.is_none() && return_type.is_none() {
            return Ok(self.set_cached_iteration_types(
                type_,
                IterationTypeCacheKey::IterationTypesOfIteratorResult,
                self.no_iteration_types(),
            ));
        }

        Ok(self.set_cached_iteration_types(
            type_,
            IterationTypeCacheKey::IterationTypesOfIteratorResult,
            self.create_iteration_types(
                yield_type,
                Some(return_type.unwrap_or_else(|| self.void_type())),
                None,
            ),
        ))
    }

    pub(super) fn get_iteration_types_of_method(
        &self,
        type_: Id<Type>,
        resolver: &IterationTypesResolver,
        method_name: &str, /*"next" | "return" | "throw"*/
        error_node: Option<Id<Node>>,
    ) -> io::Result<Option<Id<IterationTypes>>> {
        let method = self.get_property_of_type_(type_, method_name, None)?;

        if method.is_none() && method_name != "next" {
            return Ok(None);
        }

        let method_type = method
            .filter(|&method| {
                !(method_name == "next"
                    && method.ref_(self).flags().intersects(SymbolFlags::Optional))
            })
            .try_map(|method| -> io::Result<_> {
                Ok(if method_name == "next" {
                    self.get_type_of_symbol(method)?
                } else {
                    self.get_type_with_facts(
                        self.get_type_of_symbol(method)?,
                        TypeFacts::NEUndefinedOrNull,
                    )?
                })
            })?;

        if self.is_type_any(method_type) {
            return Ok(Some(if method_name == "next" {
                self.any_iteration_types()
            } else {
                self.any_iteration_types_except_next()
            }));
        }

        let method_signatures = if let Some(method_type) = method_type {
            self.get_signatures_of_type(method_type, SignatureKind::Call)?
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
            return Ok(if method_name == "next" {
                Some(self.any_iteration_types())
            } else {
                None
            });
        }

        if let Some(method_type_symbol) =
            method_type.and_then(|method_type| method_type.ref_(self).maybe_symbol())
        {
            if method_signatures.len() == 1 {
                let global_generator_type = (resolver.get_global_generator_type)(self, false)?;
                let global_iterator_type = (resolver.get_global_iterator_type)(self, false)?;
                let is_generator_method = matches!(
                    global_generator_type.ref_(self).maybe_symbol().and_then(|global_generator_type_symbol| {
                        global_generator_type_symbol.ref_(self).maybe_members().clone()
                    }).and_then(|global_generator_type_symbol_members| {
                        global_generator_type_symbol_members.ref_(self).get(method_name).cloned()
                    }),
                    Some(global_generator_type_symbol_member) if global_generator_type_symbol_member == method_type_symbol
                );
                let is_iterator_method = !is_generator_method
                    && matches!(
                        global_iterator_type.ref_(self).maybe_symbol().and_then(|global_iterator_type_symbol| {
                            global_iterator_type_symbol.ref_(self).maybe_members().clone()
                        }).and_then(|global_iterator_type_symbol_members| {
                            global_iterator_type_symbol_members.ref_(self).get(method_name).cloned()
                        }),
                        Some(global_iterator_type_symbol_member) if global_iterator_type_symbol_member == method_type_symbol
                    );
                if is_generator_method || is_iterator_method {
                    let global_type = if is_generator_method {
                        global_generator_type
                    } else {
                        global_iterator_type
                    };
                    let method_type = method_type.unwrap();
                    let mapper = method_type.ref_(self).as_object_type().maybe_mapper();
                    return Ok(Some(
                        self.create_iteration_types(
                            Some(
                                self.get_mapped_type(
                                    global_type
                                        .ref_(self)
                                        .as_generic_type()
                                        .maybe_type_parameters()
                                        .unwrap()[0],
                                    mapper.clone().unwrap(),
                                )?,
                            ),
                            Some(
                                self.get_mapped_type(
                                    global_type
                                        .ref_(self)
                                        .as_generic_type()
                                        .maybe_type_parameters()
                                        .unwrap()[1],
                                    mapper.clone().unwrap(),
                                )?,
                            ),
                            if method_name == "next" {
                                Some(
                                    self.get_mapped_type(
                                        global_type
                                            .ref_(self)
                                            .as_generic_type()
                                            .maybe_type_parameters()
                                            .unwrap()[2],
                                        mapper.unwrap(),
                                    )?,
                                )
                            } else {
                                None
                            },
                        ),
                    ));
                }
            }
        }

        let mut method_parameter_types: Option<Vec<Id<Type>>> = None;
        let mut method_return_types: Option<Vec<Id<Type>>> = None;
        for &signature in &method_signatures {
            if method_name != "throw"
                && some(
                    Some(signature.ref_(self).parameters()),
                    Option::<fn(&Id<Symbol>) -> bool>::None,
                )
            {
                if method_parameter_types.is_none() {
                    method_parameter_types = Some(vec![]);
                }
                append(
                    method_parameter_types.as_mut().unwrap(),
                    Some(self.get_type_at_position(signature, 0)?),
                );
            }
            if method_return_types.is_none() {
                method_return_types = Some(vec![]);
            }
            append(
                method_return_types.as_mut().unwrap(),
                Some(self.get_return_type_of_signature(signature.clone())?),
            );
        }

        let mut return_types: Option<Vec<Id<Type>>> = None;
        let mut next_type: Option<Id<Type>> = None;
        if method_name != "throw" {
            let method_parameter_type = if let Some(method_parameter_types) = method_parameter_types
            {
                self.get_union_type(
                    &method_parameter_types,
                    None,
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?
            } else {
                self.unknown_type()
            };
            if method_name == "next" {
                next_type = Some(method_parameter_type.clone());
            } else if method_name == "return" {
                let resolved_method_parameter_type = (resolver.resolve_iteration_type)(
                    self,
                    method_parameter_type,
                    error_node.clone(),
                )?;
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

        let yield_type: Id<Type>;
        let method_return_type = if let Some(method_return_types) = method_return_types.as_ref() {
            self.get_intersection_type(method_return_types, Option::<Id<Symbol>>::None, None)?
        } else {
            self.never_type()
        };
        let resolved_method_return_type =
            (resolver.resolve_iteration_type)(self, method_return_type, error_node.clone())?
                .unwrap_or_else(|| self.any_type());
        let iteration_types =
            self.get_iteration_types_of_iterator_result(resolved_method_return_type)?;
        if iteration_types == self.no_iteration_types() {
            if error_node.is_some() {
                self.error(
                    error_node,
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
            yield_type = iteration_types.ref_(self).yield_type();
            if return_types.is_none() {
                return_types = Some(vec![]);
            }
            append(
                return_types.as_mut().unwrap(),
                Some(iteration_types.ref_(self).return_type()),
            );
        }

        Ok(Some(self.create_iteration_types(
            Some(yield_type),
            Some(self.get_union_type(
                &return_types.unwrap(),
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?),
            next_type,
        )))
    }

    pub(super) fn get_iteration_types_of_iterator_slow(
        &self,
        type_: Id<Type>,
        resolver: &IterationTypesResolver,
        error_node: Option<Id<Node>>,
    ) -> io::Result<Id<IterationTypes>> {
        let iteration_types = self.combine_iteration_types(&[
            self.get_iteration_types_of_method(type_, resolver, "next", error_node)?,
            self.get_iteration_types_of_method(type_, resolver, "return", error_node)?,
            self.get_iteration_types_of_method(type_, resolver, "throw", error_node)?,
        ])?;
        Ok(self.set_cached_iteration_types(type_, resolver.iterator_cache_key, iteration_types))
    }

    pub(super) fn get_iteration_type_of_generator_function_return_type(
        &self,
        kind: IterationTypeKind,
        return_type: Id<Type>,
        is_async_generator: bool,
    ) -> io::Result<Option<Id<Type>>> {
        if self.is_type_any(Some(return_type)) {
            return Ok(None);
        }

        let iteration_types = self.get_iteration_types_of_generator_function_return_type(
            return_type,
            is_async_generator,
        )?;
        Ok(iteration_types.map(|iteration_types| {
            iteration_types
                .ref_(self)
                .get_by_key(get_iteration_types_key_from_iteration_type_kind(kind))
        }))
    }

    pub(super) fn get_iteration_types_of_generator_function_return_type(
        &self,
        type_: Id<Type>,
        is_async_generator: bool,
    ) -> io::Result<Option<Id<IterationTypes>>> {
        if self.is_type_any(Some(type_)) {
            return Ok(Some(self.any_iteration_types()));
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
        self.get_iteration_types_of_iterable(type_, use_, Option::<Id<Node>>::None)?
            .try_or_else(|| {
                self.get_iteration_types_of_iterator(type_, resolver, Option::<Id<Node>>::None)
            })
    }

    pub(super) fn check_break_or_continue_statement(
        &self,
        node: Id<Node>, /*BreakOrContinueStatement*/
    ) {
        if !self.check_grammar_statement_in_ambient_context(node) {
            self.check_grammar_break_or_continue_statement(node);
        }
    }

    pub(super) fn unwrap_return_type(
        &self,
        return_type: Id<Type>,
        function_flags: FunctionFlags,
    ) -> io::Result<Id<Type>> {
        let is_generator = function_flags.intersects(FunctionFlags::Generator);
        let is_async = function_flags.intersects(FunctionFlags::Async);
        Ok(if is_generator {
            self.get_iteration_type_of_generator_function_return_type(
                IterationTypeKind::Return,
                return_type,
                is_async,
            )?
            .unwrap_or_else(|| self.error_type())
        } else if is_async {
            self.get_awaited_type_no_alias(return_type, Option::<Id<Node>>::None, None, None)?
                .unwrap_or_else(|| self.error_type())
        } else {
            return_type
        })
    }

    pub(super) fn is_unwrapped_return_type_void_or_any(
        &self,
        func: Id<Node>, /*SignatureDeclaration*/
        return_type: Id<Type>,
    ) -> io::Result<bool> {
        let unwrapped_return_type =
            self.unwrap_return_type(return_type, get_function_flags(Some(func), self))?;
        /* !!unwrappedReturnType &&*/
        Ok(self.maybe_type_of_kind(
            unwrapped_return_type,
            TypeFlags::Void | TypeFlags::AnyOrUnknown,
        ))
    }

    pub(super) fn check_return_statement(
        &self,
        node: Id<Node>, /*ReturnStatement*/
    ) -> io::Result<()> {
        if self.check_grammar_statement_in_ambient_context(node) {
            return Ok(());
        }

        let container = get_containing_function_or_class_static_block(node, self);
        if matches!(
            container,
            Some(container) if is_class_static_block_declaration(&container.ref_(self))
        ) {
            self.grammar_error_on_first_token(
                node,
                &Diagnostics::A_return_statement_cannot_be_used_inside_a_class_static_block,
                None,
            );
            return Ok(());
        }

        if container.is_none() {
            self.grammar_error_on_first_token(
                node,
                &Diagnostics::A_return_statement_can_only_be_used_within_a_function_body,
                None,
            );
            return Ok(());
        }
        let container = container.unwrap();

        let signature = self.get_signature_from_declaration_(container)?;
        let return_type = self.get_return_type_of_signature(signature)?;
        let function_flags = get_function_flags(Some(container), self);
        if self.strict_null_checks
            || node.ref_(self).as_return_statement().expression.is_some()
            || return_type.ref_(self).flags().intersects(TypeFlags::Never)
        {
            let expr_type = match released!(node.ref_(self).as_return_statement().expression) {
                Some(expression) => self.check_expression_cached(expression, None)?,
                None => self.undefined_type(),
            };
            if container.ref_(self).kind() == SyntaxKind::SetAccessor {
                if node.ref_(self).as_return_statement().expression.is_some() {
                    self.error(
                        Some(node),
                        &Diagnostics::Setters_cannot_return_a_value,
                        None,
                    );
                }
            } else if container.ref_(self).kind() == SyntaxKind::Constructor {
                if matches!(
                    released!(node.ref_(self).as_return_statement().expression),
                    Some(node_expression) if !self.check_type_assignable_to_and_optionally_elaborate(
                        expr_type,
                        return_type,
                        Some(node),
                        Some(node_expression),
                        None, None,
                    )?
                ) {
                    self.error(
                        Some(node),
                        &Diagnostics::Return_type_of_constructor_signature_must_be_assignable_to_the_instance_type_of_the_class,
                        None,
                    );
                }
            } else if self.get_return_type_from_annotation(container)?.is_some() {
                let unwrapped_return_type = self
                    .unwrap_return_type(return_type, function_flags)?/*?? returnType*/;
                let unwrapped_expr_type = if function_flags.intersects(FunctionFlags::Async) {
                    self.check_awaited_type(
                        expr_type,
                        false,
                        node,
                        &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member,
                        None,
                    )?
                } else {
                    expr_type
                };
                // if unwrappedReturnType {
                self.check_type_assignable_to_and_optionally_elaborate(
                    unwrapped_expr_type,
                    unwrapped_return_type,
                    Some(node),
                    released!(node.ref_(self).as_return_statement().expression),
                    None,
                    None,
                )?;
                // }
            }
        } else if container.ref_(self).kind() != SyntaxKind::Constructor
            && self.compiler_options.ref_(self).no_implicit_returns == Some(true)
            && !self.is_unwrapped_return_type_void_or_any(container, return_type)?
        {
            self.error(
                Some(node),
                &Diagnostics::Not_all_code_paths_return_a_value,
                None,
            );
        }

        Ok(())
    }
}
