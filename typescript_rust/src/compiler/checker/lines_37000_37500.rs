use std::{borrow::Borrow, io, ptr};

use gc::Gc;
use id_arena::Id;

use super::{
    get_iteration_types_key_from_iteration_type_kind, CheckMode, IterationTypeKind, IterationUse,
};
use crate::{
    append, filter, get_containing_function_or_class_static_block, get_function_flags,
    is_binary_expression, is_binding_pattern, is_class_static_block_declaration, is_identifier,
    try_for_each, try_for_each_child_bool, DiagnosticMessage, Diagnostics, ExternalEmitHelpers,
    FunctionFlags, IterationTypeCacheKey, IterationTypes, NamedDeclarationInterface, Node,
    NodeArray, NodeInterface, OptionTry, ScriptTarget, Symbol, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface, UnionOrIntersectionTypeInterface, UnionReduction,
};

impl TypeChecker {
    pub(super) fn is_symbol_used_in_binary_expression_chain(
        &self,
        node: &Node,
        tested_symbol: &Symbol,
    ) -> io::Result<bool> {
        let mut node = node.node_wrapper();
        while is_binary_expression(&node)
            && node.as_binary_expression().operator_token.kind()
                == SyntaxKind::AmpersandAmpersandToken
        {
            let is_used = try_for_each_child_bool(
                &node.as_binary_expression().right,
                |child| self.is_symbol_used_in_binary_expression_chain_visit(tested_symbol, child),
                Option::<fn(&NodeArray) -> io::Result<bool>>::None,
            )?;
            if is_used {
                return Ok(true);
            }
            node = node.parent();
        }
        Ok(false)
    }

    pub(super) fn is_symbol_used_in_binary_expression_chain_visit(
        &self,
        tested_symbol: &Symbol,
        child: &Node,
    ) -> io::Result<bool> {
        if is_identifier(child) {
            let symbol = self.get_symbol_at_location_(child, None)?;
            if matches!(
                symbol.as_ref(),
                Some(symbol) if ptr::eq(&**symbol, tested_symbol)
            ) {
                return Ok(true);
            }
        }
        try_for_each_child_bool(
            child,
            |child| self.is_symbol_used_in_binary_expression_chain_visit(tested_symbol, child),
            Option::<fn(&NodeArray) -> io::Result<bool>>::None,
        )
    }

    pub(super) fn check_do_statement(&self, node: &Node /*DoStatement*/) -> io::Result<()> {
        self.check_grammar_statement_in_ambient_context(node);

        let node_as_do_statement = node.as_do_statement();
        self.check_source_element(Some(&*node_as_do_statement.statement))?;
        self.check_truthiness_expression(&node_as_do_statement.expression, None)?;

        Ok(())
    }

    pub(super) fn check_while_statement(
        &self,
        node: &Node, /*WhileStatement*/
    ) -> io::Result<()> {
        self.check_grammar_statement_in_ambient_context(node);

        let node_as_while_statement = node.as_while_statement();
        self.check_truthiness_expression(&node_as_while_statement.expression, None)?;
        self.check_source_element(Some(&*node_as_while_statement.statement))?;

        Ok(())
    }

    pub(super) fn check_truthiness_of_type(&self, type_: Id<Type>, node: &Node) -> Id<Type> {
        if self.type_(type_).flags().intersects(TypeFlags::Void) {
            self.error(
                Some(node),
                &Diagnostics::An_expression_of_type_void_cannot_be_tested_for_truthiness,
                None,
            );
        }
        type_
    }

    pub(super) fn check_truthiness_expression(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        Ok(self.check_truthiness_of_type(self.check_expression(node, check_mode, None)?, node))
    }

    pub(super) fn check_for_statement(&self, node: &Node /*ForStatement*/) -> io::Result<()> {
        let node_as_for_statement = node.as_for_statement();
        if !self.check_grammar_statement_in_ambient_context(node) {
            if let Some(node_initializer) =
                node_as_for_statement
                    .initializer
                    .as_ref()
                    .filter(|node_initializer| {
                        node_initializer.kind() == SyntaxKind::VariableDeclarationList
                    })
            {
                self.check_grammar_variable_declaration_list(node_initializer);
            }
        }

        if let Some(node_initializer) = node_as_for_statement.initializer.as_ref() {
            if node_initializer.kind() == SyntaxKind::VariableDeclarationList {
                try_for_each(
                    &node_initializer.as_variable_declaration_list().declarations,
                    |declaration: &Gc<Node>, _| -> io::Result<Option<()>> {
                        self.check_variable_declaration(declaration)?;
                        Ok(None)
                    },
                )?;
            } else {
                self.check_expression(node_initializer, None, None)?;
            }
        }

        if let Some(node_condition) = node_as_for_statement.condition.as_ref() {
            self.check_truthiness_expression(node_condition, None)?;
        }
        if let Some(node_incrementor) = node_as_for_statement.incrementor.as_ref() {
            self.check_expression(node_incrementor, None, None)?;
        }
        self.check_source_element(Some(&*node_as_for_statement.statement))?;
        if node.maybe_locals().is_some() {
            self.register_for_unused_identifiers_check(node);
        }

        Ok(())
    }

    pub(super) fn check_for_of_statement(
        &self,
        node: &Node, /*ForOfStatement*/
    ) -> io::Result<()> {
        self.check_grammar_for_in_or_for_of_statement(node);

        let container = get_containing_function_or_class_static_block(node);
        let node_as_for_of_statement = node.as_for_of_statement();
        if let Some(node_await_modifier) = node_as_for_of_statement.await_modifier.as_ref() {
            if matches!(
                container.as_ref(),
                Some(container) if is_class_static_block_declaration(container)
            ) {
                self.grammar_error_on_node(
                    node_await_modifier,
                    &Diagnostics::For_await_loops_cannot_be_used_inside_a_class_static_block,
                    None,
                );
            } else {
                let function_flags = get_function_flags(container.as_deref());
                if function_flags & (FunctionFlags::Invalid | FunctionFlags::Async)
                    == FunctionFlags::Async
                    && self.language_version < ScriptTarget::ESNext
                {
                    self.check_external_emit_helpers(
                        node,
                        ExternalEmitHelpers::ForAwaitOfIncludes,
                    )?;
                }
            }
        } else if self.compiler_options.downlevel_iteration == Some(true)
            && self.language_version < ScriptTarget::ES2015
        {
            self.check_external_emit_helpers(node, ExternalEmitHelpers::ForOfIncludes)?;
        }

        if node_as_for_of_statement.initializer.kind() == SyntaxKind::VariableDeclarationList {
            self.check_for_in_or_for_of_variable_declaration(node)?;
        } else {
            let var_expr = &node_as_for_of_statement.initializer;
            let iterated_type = self.check_right_hand_side_of_for_of(node)?;

            if matches!(
                var_expr.kind(),
                SyntaxKind::ArrayLiteralExpression | SyntaxKind::ObjectLiteralExpression
            ) {
                self.check_destructuring_assignment(
                    var_expr,
                    iterated_type, /*|| errorType*/
                    None,
                    None,
                )?;
            } else {
                let left_type = self.check_expression(var_expr, None, None)?;
                self.check_reference_expression(
                    var_expr,
                    &Diagnostics::The_left_hand_side_of_a_for_of_statement_must_be_a_variable_or_a_property_access,
                    &Diagnostics::The_left_hand_side_of_a_for_of_statement_may_not_be_an_optional_property_access,
                );

                // if (iteratedType) {
                self.check_type_assignable_to_and_optionally_elaborate(
                    iterated_type,
                    left_type,
                    Some(&**var_expr),
                    Some(&*node_as_for_of_statement.expression),
                    None,
                    None,
                )?;
                // }
            }
        }

        self.check_source_element(Some(&*node_as_for_of_statement.statement))?;
        if node.maybe_locals().is_some() {
            self.register_for_unused_identifiers_check(node);
        }

        Ok(())
    }

    pub(super) fn check_for_in_statement(
        &self,
        node: &Node, /*ForInStatement*/
    ) -> io::Result<()> {
        self.check_grammar_for_in_or_for_of_statement(node);

        let node_as_for_in_statement = node.as_for_in_statement();
        let right_type = self.get_non_nullable_type_if_needed(self.check_expression(
            &node_as_for_in_statement.expression,
            None,
            None,
        )?)?;
        if node_as_for_in_statement.initializer.kind() == SyntaxKind::VariableDeclarationList {
            let variable = node_as_for_in_statement
                .initializer
                .as_variable_declaration_list()
                .declarations
                .get(0);
            if let Some(variable) = variable.filter(|variable| {
                is_binding_pattern(variable.as_variable_declaration().maybe_name())
            }) {
                self.error(
                    variable.as_variable_declaration().maybe_name(),
                    &Diagnostics::The_left_hand_side_of_a_for_in_statement_cannot_be_a_destructuring_pattern,
                    None,
                );
            }
            self.check_for_in_or_for_of_variable_declaration(node)?;
        } else {
            let var_expr = &node_as_for_in_statement.initializer;
            let left_type = self.check_expression(var_expr, None, None)?;
            if matches!(
                var_expr.kind(),
                SyntaxKind::ArrayLiteralExpression | SyntaxKind::ObjectLiteralExpression
            ) {
                self.error(
                    Some(&**var_expr),
                    &Diagnostics::The_left_hand_side_of_a_for_in_statement_cannot_be_a_destructuring_pattern,
                    None,
                );
            } else if !self
                .is_type_assignable_to(self.get_index_type_or_string(right_type)?, left_type)?
            {
                self.error(
                    Some(&**var_expr),
                    &Diagnostics::The_left_hand_side_of_a_for_in_statement_must_be_of_type_string_or_any,
                    None,
                );
            } else {
                self.check_reference_expression(
                    var_expr,
                    &Diagnostics::The_left_hand_side_of_a_for_in_statement_must_be_a_variable_or_a_property_access,
                    &Diagnostics::The_left_hand_side_of_a_for_in_statement_may_not_be_an_optional_property_access,
                );
            }
        }

        if right_type == self.never_type()
            || !self.is_type_assignable_to_kind(
                right_type,
                TypeFlags::NonPrimitive | TypeFlags::InstantiableNonPrimitive,
                None,
            )?
        {
            self.error(
                Some(&*node_as_for_in_statement.expression),
                &Diagnostics::The_right_hand_side_of_a_for_in_statement_must_be_of_type_any_an_object_type_or_a_type_parameter_but_here_has_type_0,
                Some(vec![
                    self.type_to_string_(
                        right_type,
                        Option::<&Node>::None,
                        None,None,
                    )?
                ])
            );
        }

        self.check_source_element(Some(&*node_as_for_in_statement.statement))?;
        if node.maybe_locals().is_some() {
            self.register_for_unused_identifiers_check(node);
        }

        Ok(())
    }

    pub(super) fn check_for_in_or_for_of_variable_declaration(
        &self,
        iteration_statement: &Node, /*ForInOrOfStatement*/
    ) -> io::Result<()> {
        let variable_declaration_list = iteration_statement
            .as_has_initializer()
            .maybe_initializer()
            .unwrap();
        let variable_declaration_list = variable_declaration_list.as_variable_declaration_list();
        if !variable_declaration_list.declarations.is_empty() {
            let decl = &variable_declaration_list.declarations[0];
            self.check_variable_declaration(decl)?;
        }

        Ok(())
    }

    pub(super) fn check_right_hand_side_of_for_of(
        &self,
        statement: &Node, /*ForOfStatement*/
    ) -> io::Result<Id<Type>> {
        let statement_as_for_of_statement = statement.as_for_of_statement();
        let use_ = if statement_as_for_of_statement.await_modifier.is_some() {
            IterationUse::ForAwaitOf
        } else {
            IterationUse::ForOf
        };
        self.check_iterated_type_or_element_type(
            use_,
            self.check_non_null_expression(&statement_as_for_of_statement.expression)?,
            self.undefined_type(),
            Some(&*statement_as_for_of_statement.expression),
        )
    }

    pub(super) fn check_iterated_type_or_element_type(
        &self,
        use_: IterationUse,
        input_type: Id<Type>,
        sent_type: Id<Type>,
        error_node: Option<impl Borrow<Node>>,
    ) -> io::Result<Id<Type>> {
        if self.is_type_any(Some(input_type)) {
            return Ok(input_type);
        }
        Ok(self
            .get_iterated_type_or_element_type(use_, input_type, sent_type, error_node, true)?
            .unwrap_or_else(|| self.any_type()))
    }

    pub(super) fn get_iterated_type_or_element_type(
        &self,
        use_: IterationUse,
        input_type: Id<Type>,
        sent_type: Id<Type>,
        error_node: Option<impl Borrow<Node>>,
        check_assignability: bool,
    ) -> io::Result<Option<Id<Type>>> {
        let allow_async_iterables = use_.intersects(IterationUse::AllowsAsyncIterablesFlag);
        let error_node = error_node.map(|error_node| error_node.borrow().node_wrapper());
        if input_type == self.never_type() {
            self.report_type_not_iterable_error(
                error_node.as_ref().unwrap(),
                input_type,
                allow_async_iterables,
            )?;
            return Ok(None);
        }

        let uplevel_iteration = self.language_version >= ScriptTarget::ES2015;
        let downlevel_iteration = if uplevel_iteration {
            Some(true)
        } else {
            self.compiler_options.downlevel_iteration
        };
        let possible_out_of_bounds = self.compiler_options.no_unchecked_indexed_access
            == Some(true)
            && use_.intersects(IterationUse::PossiblyOutOfBounds);

        if uplevel_iteration || downlevel_iteration == Some(true) || allow_async_iterables {
            let iteration_types = self.get_iteration_types_of_iterable(
                input_type,
                use_,
                if uplevel_iteration {
                    error_node.as_deref()
                } else {
                    None
                },
            )?;
            if check_assignability {
                if let Some(iteration_types) = iteration_types.as_ref() {
                    let diagnostic = if use_.intersects(IterationUse::ForOfFlag) {
                        Some(&*Diagnostics::Cannot_iterate_value_because_the_next_method_of_its_iterator_expects_type_1_but_for_of_will_always_send_0)
                    } else if use_.intersects(IterationUse::SpreadFlag) {
                        Some(&*Diagnostics::Cannot_iterate_value_because_the_next_method_of_its_iterator_expects_type_1_but_array_spread_will_always_send_0)
                    } else if use_.intersects(IterationUse::DestructuringFlag) {
                        Some(&*Diagnostics::Cannot_iterate_value_because_the_next_method_of_its_iterator_expects_type_1_but_array_destructuring_will_always_send_0)
                    } else if use_.intersects(IterationUse::YieldStarFlag) {
                        Some(&*Diagnostics::Cannot_delegate_iteration_to_value_because_the_next_method_of_its_iterator_expects_type_1_but_the_containing_generator_will_always_send_0)
                    } else {
                        None
                    };
                    if let Some(diagnostic) = diagnostic {
                        self.check_type_assignable_to(
                            sent_type,
                            iteration_types.next_type(),
                            error_node.as_deref(),
                            Some(diagnostic),
                            None,
                            None,
                        )?;
                    }
                }
            }
            if iteration_types.is_some() || uplevel_iteration {
                return Ok(if possible_out_of_bounds {
                    self.include_undefined_in_index_signature(
                        iteration_types
                            .as_ref()
                            .map(|iteration_types| iteration_types.yield_type()),
                    )?
                } else {
                    iteration_types
                        .as_ref()
                        .map(|iteration_types| iteration_types.yield_type())
                });
            }
        }

        let mut array_type = input_type;
        let mut reported_error = false;
        let mut has_string_constituent = false;

        if use_.intersects(IterationUse::AllowsStringInputFlag) {
            if array_type.flags().intersects(TypeFlags::Union) {
                let array_types = self.type_(input_type).as_union_type().types();
                let filtered_types = filter(array_types, |&t: &Id<Type>| {
                    !self.type_(t).flags().intersects(TypeFlags::StringLike)
                });
                if filtered_types.len() != array_types.len() {
                    array_type = self.get_union_type(
                        &filtered_types,
                        Some(UnionReduction::Subtype),
                        Option::<&Symbol>::None,
                        None,
                        None,
                    )?;
                }
            } else if array_type.flags().intersects(TypeFlags::StringLike) {
                array_type = self.never_type();
            }

            has_string_constituent = array_type != input_type;
            if has_string_constituent {
                if self.language_version < ScriptTarget::ES5 {
                    if error_node.is_some() {
                        self.error(
                            error_node.as_deref(),
                            &Diagnostics::Using_a_string_in_a_for_of_statement_is_only_supported_in_ECMAScript_5_and_higher,
                            None,
                        );
                        reported_error = true;
                    }
                }

                if array_type.flags().intersects(TypeFlags::Never) {
                    return Ok(if possible_out_of_bounds {
                        self.include_undefined_in_index_signature(Some(self.string_type()))?
                    } else {
                        Some(self.string_type())
                    });
                }
            }
        }

        if !self.is_array_like_type(&array_type)? {
            if let Some(error_node) = error_node.as_ref() {
                if !reported_error {
                    let allows_strings = use_.intersects(IterationUse::AllowsStringInputFlag)
                        && !has_string_constituent;
                    let (default_diagnostic, maybe_missing_await) = self
                        .get_iteration_diagnostic_details(
                            use_,
                            input_type,
                            allows_strings,
                            downlevel_iteration,
                        )?;
                    self.error_and_maybe_suggest_await(
                        error_node,
                        maybe_missing_await
                            && self
                                .get_awaited_type_of_promise(
                                    &array_type,
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                )?
                                .is_some(),
                        default_diagnostic,
                        Some(vec![self.type_to_string_(
                            &array_type,
                            Option::<&Node>::None,
                            None,
                            None,
                        )?]),
                    );
                }
            }
            return Ok(if has_string_constituent {
                if possible_out_of_bounds {
                    self.include_undefined_in_index_signature(Some(self.string_type()))?
                } else {
                    Some(self.string_type())
                }
            } else {
                None
            });
        }

        let array_element_type = self.get_index_type_of_type_(array_type, self.number_type())?;
        if has_string_constituent {
            if let Some(array_element_type) = array_element_type.as_ref() {
                if self.type_(array_element_type).flags().intersects(TypeFlags::StringLike)
                    && self.compiler_options.no_unchecked_indexed_access != Some(true)
                {
                    return Ok(Some(self.string_type()));
                }

                return Ok(Some(self.get_union_type(
                    &if possible_out_of_bounds {
                        vec![
                            array_element_type.clone(),
                            self.string_type(),
                            self.undefined_type(),
                        ]
                    } else {
                        vec![array_element_type.clone(), self.string_type()]
                    },
                    Some(UnionReduction::Subtype),
                    Option::<&Symbol>::None,
                    None,
                    None,
                )?));
            }
        }

        Ok(if use_.intersects(IterationUse::PossiblyOutOfBounds) {
            self.include_undefined_in_index_signature(array_element_type)?
        } else {
            array_element_type
        })
    }

    pub(super) fn get_iteration_diagnostic_details(
        &self,
        use_: IterationUse,
        input_type: Id<Type>,
        allows_strings: bool,
        downlevel_iteration: Option<bool>,
    ) -> io::Result<(&'static DiagnosticMessage, bool)> {
        if downlevel_iteration == Some(true) {
            return Ok(if allows_strings {
                (&Diagnostics::Type_0_is_not_an_array_type_or_a_string_type_or_does_not_have_a_Symbol_iterator_method_that_returns_an_iterator, true)
            } else {
                (&Diagnostics::Type_0_is_not_an_array_type_or_does_not_have_a_Symbol_iterator_method_that_returns_an_iterator, true)
            });
        }

        let yield_type = self.get_iteration_type_of_iterable(
            use_,
            IterationTypeKind::Yield,
            input_type,
            Option::<&Node>::None,
        )?;

        if yield_type.is_some() {
            return Ok((
                &Diagnostics::Type_0_is_not_an_array_type_or_a_string_type_Use_compiler_option_downlevelIteration_to_allow_iterating_of_iterators,
                false
            ));
        }

        if self.is_es2015_or_later_iterable(
            self.type_(input_type
                ).maybe_symbol()
                .as_ref()
                .map(|input_type_symbol| input_type_symbol.escaped_name()),
        ) {
            return Ok((
                &Diagnostics::Type_0_can_only_be_iterated_through_when_using_the_downlevelIteration_flag_or_with_a_target_of_es2015_or_higher,
                true,
            ));
        }

        Ok(if allows_strings {
            (
                &Diagnostics::Type_0_is_not_an_array_type_or_a_string_type,
                true,
            )
        } else {
            (&Diagnostics::Type_0_is_not_an_array_type, true)
        })
    }

    pub(super) fn is_es2015_or_later_iterable(&self, n: Option<&str /*__String*/>) -> bool {
        if let Some(n) = n {
            match n {
                "Float32Array" | "Float64Array" | "Int16Array" | "Int32Array" | "Int8Array"
                | "NodeList" | "Uint16Array" | "Uint32Array" | "Uint8Array"
                | "Uint8ClampedArray" => {
                    return true;
                }
                _ => (),
            }
        }
        false
    }

    pub(super) fn get_iteration_type_of_iterable(
        &self,
        use_: IterationUse,
        type_kind: IterationTypeKind,
        input_type: Id<Type>,
        error_node: Option<impl Borrow<Node>>,
    ) -> io::Result<Option<Id<Type>>> {
        if self.is_type_any(Some(input_type)) {
            return Ok(None);
        }

        let iteration_types = self.get_iteration_types_of_iterable(input_type, use_, error_node)?;
        Ok(iteration_types.as_ref().map(|iteration_types| {
            iteration_types.get_by_key(get_iteration_types_key_from_iteration_type_kind(type_kind))
        }))
    }

    pub(super) fn create_iteration_types(
        &self,
        yield_type: Option<Id<Type>>,
        return_type: Option<Id<Type>>,
        next_type: Option<Id<Type>>,
    ) -> Gc<IterationTypes> {
        let yield_type = yield_type.unwrap_or_else(|| self.never_type());
        let return_type = return_type.unwrap_or_else(|| self.never_type());
        let next_type = next_type.unwrap_or_else(|| self.unknown_type());
        if self.type_(yield_type).flags().intersects(TypeFlags::Intrinsic)
            && self.type_(return_type).flags().intersects(
                TypeFlags::Any
                    | TypeFlags::Never
                    | TypeFlags::Unknown
                    | TypeFlags::Void
                    | TypeFlags::Undefined,
            )
            && self.type_(next_type).flags().intersects(
                TypeFlags::Any
                    | TypeFlags::Never
                    | TypeFlags::Unknown
                    | TypeFlags::Void
                    | TypeFlags::Undefined,
            )
        {
            let id = self.get_type_list_id(Some(&[
                yield_type.clone(),
                return_type.clone(),
                next_type.clone(),
            ]));
            let mut iteration_types_cache = self.iteration_types_cache();
            let iteration_types = iteration_types_cache.entry(id).or_insert_with(|| {
                Gc::new(IterationTypes::new(yield_type, return_type, next_type))
            });
            return iteration_types.clone();
        }
        Gc::new(IterationTypes::new(yield_type, return_type, next_type))
    }

    pub(super) fn combine_iteration_types(
        &self,
        array: &[Option<Gc<IterationTypes>>],
    ) -> io::Result<Gc<IterationTypes>> {
        let mut yield_types: Option<Vec<Id<Type>>> = None;
        let mut return_types: Option<Vec<Id<Type>>> = None;
        let mut next_types: Option<Vec<Id<Type>>> = None;
        for iteration_types in array {
            if iteration_types.is_none() {
                continue;
            }
            let iteration_types = iteration_types.clone().unwrap();
            if Gc::ptr_eq(&iteration_types, &self.no_iteration_types()) {
                continue;
            }
            if Gc::ptr_eq(&iteration_types, &self.any_iteration_types()) {
                return Ok(self.any_iteration_types());
            }
            if yield_types.is_none() {
                yield_types = Some(vec![]);
            }
            append(
                yield_types.as_mut().unwrap(),
                Some(iteration_types.yield_type()),
            );
            if return_types.is_none() {
                return_types = Some(vec![]);
            }
            append(
                return_types.as_mut().unwrap(),
                Some(iteration_types.return_type()),
            );
            if next_types.is_none() {
                next_types = Some(vec![]);
            }
            append(
                next_types.as_mut().unwrap(),
                Some(iteration_types.next_type()),
            );
        }
        if yield_types.is_some() || return_types.is_some() || next_types.is_some() {
            return Ok(self.create_iteration_types(
                yield_types.try_map(|yield_types| {
                    self.get_union_type(&yield_types, None, Option::<&Symbol>::None, None, None)
                })?,
                return_types.try_map(|return_types| {
                    self.get_union_type(&return_types, None, Option::<&Symbol>::None, None, None)
                })?,
                next_types.try_map(|next_types| {
                    self.get_intersection_type(&next_types, Option::<&Symbol>::None, None)
                })?,
            ));
        }
        Ok(self.no_iteration_types())
    }

    pub(super) fn get_cached_iteration_types(
        &self,
        type_: Id<Type>,
        cache_key: IterationTypeCacheKey,
    ) -> Option<Gc<IterationTypes>> {
        self.type_(type_).get_by_iteration_type_cache_key(cache_key)
    }

    pub(super) fn set_cached_iteration_types(
        &self,
        type_: Id<Type>,
        cache_key: IterationTypeCacheKey,
        cached_types: Gc<IterationTypes>,
    ) -> Gc<IterationTypes> {
        self.type_(type_).set_by_iteration_type_cache_key(cache_key, Some(cached_types.clone()));
        cached_types
    }

    pub(super) fn get_iteration_types_of_iterable(
        &self,
        type_: Id<Type>,
        use_: IterationUse,
        error_node: Option<impl Borrow<Node>>,
    ) -> io::Result<Option<Gc<IterationTypes>>> {
        if self.is_type_any(Some(type_)) {
            return Ok(Some(self.any_iteration_types()));
        }

        let error_node = error_node.map(|error_node| error_node.borrow().node_wrapper());
        if !self.type_(type_).flags().intersects(TypeFlags::Union) {
            let iteration_types =
                self.get_iteration_types_of_iterable_worker(type_, use_, error_node.as_deref())?;
            if Gc::ptr_eq(&iteration_types, &self.no_iteration_types()) {
                if let Some(error_node) = error_node.as_ref() {
                    self.report_type_not_iterable_error(
                        error_node,
                        type_,
                        use_.intersects(IterationUse::AllowsAsyncIterablesFlag),
                    )?;
                }
                return Ok(None);
            }
            return Ok(Some(iteration_types));
        }

        let cache_key = if use_.intersects(IterationUse::AllowsAsyncIterablesFlag) {
            IterationTypeCacheKey::IterationTypesOfAsyncIterable
        } else {
            IterationTypeCacheKey::IterationTypesOfIterable
        };
        let cached_types = self.get_cached_iteration_types(type_, cache_key);
        if let Some(cached_types) = cached_types.as_ref() {
            return Ok(if Gc::ptr_eq(cached_types, &self.no_iteration_types()) {
                None
            } else {
                Some(cached_types.clone())
            });
        }

        let mut all_iteration_types: Option<Vec<Gc<IterationTypes>>> = None;
        for constituent in self.type_(type_).as_union_type().types() {
            let iteration_types = self.get_iteration_types_of_iterable_worker(
                constituent,
                use_,
                error_node.as_deref(),
            )?;
            if Gc::ptr_eq(&iteration_types, &self.no_iteration_types()) {
                if let Some(error_node) = error_node.as_ref() {
                    self.report_type_not_iterable_error(
                        error_node,
                        type_,
                        use_.intersects(IterationUse::AllowsAsyncIterablesFlag),
                    )?;
                }
                self.set_cached_iteration_types(type_, cache_key, self.no_iteration_types());
                return Ok(None);
            } else {
                if all_iteration_types.is_none() {
                    all_iteration_types = Some(vec![]);
                }
                append(all_iteration_types.as_mut().unwrap(), Some(iteration_types));
            }
        }

        let iteration_types = if let Some(all_iteration_types) = all_iteration_types {
            self.combine_iteration_types(
                &all_iteration_types
                    .into_iter()
                    .map(Option::Some)
                    .collect::<Vec<_>>(),
            )?
        } else {
            self.no_iteration_types()
        };
        self.set_cached_iteration_types(type_, cache_key, iteration_types.clone());
        Ok(
            if Gc::ptr_eq(&iteration_types, &self.no_iteration_types()) {
                None
            } else {
                Some(iteration_types)
            },
        )
    }
}
