#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{IterationTypeKind, IterationUse};
use crate::{
    filter_iter, for_each, get_containing_function_or_class_static_block, get_function_flags,
    DiagnosticMessage, Diagnostics, FunctionFlags, HasTypeParametersInterface,
    IterationTypeCacheKey, IterationTypes, IterationTypesResolver, Node, NodeInterface, Symbol,
    SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
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

    pub(super) fn get_iteration_types_of_iterable_slow<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        resolver: &IterationTypesResolver,
        error_node: Option<TErrorNode>,
    ) -> Rc<IterationTypes> {
        unimplemented!()
    }

    pub(super) fn report_type_not_iterable_error(
        &self,
        error_node: &Node,
        type_: &Type,
        allow_async_iterables: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn get_iteration_type_of_generator_function_return_type(
        &self,
        kind: IterationTypeKind,
        return_type: &Type,
        is_async_generator: bool,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_iteration_types_of_generator_function_return_type(
        &self,
        type_: &Type,
        is_async_generator: bool,
    ) -> Option<Rc<IterationTypes>> {
        unimplemented!()
    }

    pub(super) fn unwrap_return_type(
        &self,
        return_type: &Type,
        function_flags: FunctionFlags,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_unwrapped_return_type_void_or_any(
        &self,
        func: &Node, /*SignatureDeclaration*/
        return_type: &Type,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_return_statement(&self, node: &Node /*ReturnStatement*/) {
        let container = get_containing_function_or_class_static_block(node);

        if container.is_none() {
            unimplemented!()
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
            if false {
                unimplemented!()
            } else if self.get_return_type_from_annotation(&container).is_some() {
                let unwrapped_return_type = self
                    .unwrap_return_type(&return_type, function_flags)/*.unwrap_or(return_type)*/;
                let unwrapped_expr_type = if function_flags.intersects(FunctionFlags::Async) {
                    self.check_awaited_type(&expr_type, false, node, &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member, None)
                } else {
                    expr_type
                };
                // if unwrappedReturnType {
                self.check_type_assignable_to_and_optionally_elaborate(
                    &unwrapped_expr_type,
                    &unwrapped_return_type,
                    Some(node),
                    node_as_return_statement.expression.clone(),
                    None,
                    None,
                );
                // }
            }
        }
    }

    pub(super) fn check_index_constraints(
        &self,
        type_: &Type,
        symbol: &Symbol,
        is_static_index: Option<bool>,
    ) {
        unimplemented!()
    }

    pub(super) fn check_type_name_is_reserved(
        &self,
        name: &Node, /*Identifier*/
        message: &'static DiagnosticMessage,
    ) {
        unimplemented!()
    }

    pub(super) fn check_class_name_collision_with_object(&self, name: &Node /*Identifier*/) {
        unimplemented!()
    }

    pub(super) fn check_type_parameters(
        &self,
        type_parameter_declarations: Option<&[Rc<Node /*TypeParameterDeclaration*/>]>,
    ) {
        if let Some(type_parameter_declarations) = type_parameter_declarations {
            for node in type_parameter_declarations {
                self.check_type_parameter(&node);
            }
        }
    }

    pub(super) fn check_class_expression(&self, node: &Node /*ClassExpression*/) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_target_symbol(&self, s: &Symbol) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn is_property_without_initializer(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_property_initialized_in_static_blocks(
        &self,
        prop_name: &Node, /*Identifier | PrivateIdentifier*/
        prop_type: &Type,
        static_blocks: &[Rc<Node /*ClassStaticBlockDeclaration*/>],
        start_pos: isize,
        end_pos: isize,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_interface_declaration(&self, node: &Node /*InterfaceDeclaration*/) {
        let node_as_interface_declaration = node.as_interface_declaration();
        self.check_type_parameters(
            node_as_interface_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        for_each(&node_as_interface_declaration.members, |member, _| {
            self.check_source_element(Some(&**member));
            Option::<()>::None
        });
    }

    pub(super) fn check_type_alias_declaration(&self, node: &Node /*TypeAliasDeclaration*/) {
        let node_as_type_alias_declaration = node.as_type_alias_declaration();
        self.check_type_parameters(
            node_as_type_alias_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        if false {
            unimplemented!()
        } else {
            self.check_source_element(Some(&*node_as_type_alias_declaration.type_));
        }
    }

    pub(super) fn check_alias_symbol(
        &self,
        node: &Node, /*ImportEqualsDeclaration | VariableDeclaration | ImportClause | NamespaceImport | ImportSpecifier | ExportSpecifier | NamespaceExport*/
    ) {
        unimplemented!()
    }
}
