#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, IterationTypeKind, IterationUse};
use crate::{
    for_each, for_each_child_bool, get_containing_function_or_class_static_block,
    get_function_flags, is_binary_expression, is_binding_pattern,
    is_class_static_block_declaration, is_identifier, DiagnosticMessage, Diagnostics,
    ExternalEmitHelpers, FunctionFlags, HasTypeParametersInterface, IterationTypes,
    IterationTypesResolver, NamedDeclarationInterface, Node, NodeArray, NodeInterface,
    ScriptTarget, Symbol, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn is_symbol_used_in_binary_expression_chain(
        &self,
        node: &Node,
        tested_symbol: &Symbol,
    ) -> bool {
        let mut node = node.node_wrapper();
        while is_binary_expression(&node)
            && node.as_binary_expression().operator_token.kind()
                == SyntaxKind::AmpersandAmpersandToken
        {
            let is_used = for_each_child_bool(
                &node.as_binary_expression().right,
                |child| self.is_symbol_used_in_binary_expression_chain_visit(tested_symbol, child),
                Option::<fn(&NodeArray) -> bool>::None,
            );
            if is_used {
                return true;
            }
            node = node.parent();
        }
        false
    }

    pub(super) fn is_symbol_used_in_binary_expression_chain_visit(
        &self,
        tested_symbol: &Symbol,
        child: &Node,
    ) -> bool {
        if is_identifier(child) {
            let symbol = self.get_symbol_at_location_(child, None);
            if matches!(
                symbol.as_ref(),
                Some(symbol) if ptr::eq(&**symbol, tested_symbol)
            ) {
                return true;
            }
        }
        for_each_child_bool(
            child,
            |child| self.is_symbol_used_in_binary_expression_chain_visit(tested_symbol, child),
            Option::<fn(&NodeArray) -> bool>::None,
        )
    }

    pub(super) fn check_do_statement(&self, node: &Node /*DoStatement*/) {
        self.check_grammar_statement_in_ambient_context(node);

        let node_as_do_statement = node.as_do_statement();
        self.check_source_element(Some(&*node_as_do_statement.statement));
        self.check_truthiness_expression(&node_as_do_statement.expression, None);
    }

    pub(super) fn check_while_statement(&self, node: &Node /*WhileStatement*/) {
        self.check_grammar_statement_in_ambient_context(node);

        let node_as_while_statement = node.as_while_statement();
        self.check_truthiness_expression(&node_as_while_statement.expression, None);
        self.check_source_element(Some(&*node_as_while_statement.statement));
    }

    pub(super) fn check_truthiness_of_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Void) {
            self.error(
                Some(node),
                &Diagnostics::An_expression_of_type_void_cannot_be_tested_for_truthiness,
                None,
            );
        }
        type_.type_wrapper()
    }

    pub(super) fn check_truthiness_expression(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_truthiness_of_type(&self.check_expression(node, check_mode, None), node)
    }

    pub(super) fn check_for_statement(&self, node: &Node /*ForStatement*/) {
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
                for_each(
                    &node_initializer.as_variable_declaration_list().declarations,
                    |declaration: &Rc<Node>, _| -> Option<()> {
                        self.check_variable_declaration(declaration);
                        None
                    },
                );
            } else {
                self.check_expression(node_initializer, None, None);
            }
        }

        if let Some(node_condition) = node_as_for_statement.condition.as_ref() {
            self.check_truthiness_expression(node_condition, None);
        }
        if let Some(node_incrementor) = node_as_for_statement.incrementor.as_ref() {
            self.check_expression(node_incrementor, None, None);
        }
        self.check_source_element(Some(&*node_as_for_statement.statement));
        if node.maybe_locals().is_some() {
            self.register_for_unused_identifiers_check(node);
        }
    }

    pub(super) fn check_for_of_statement(&self, node: &Node /*ForOfStatement*/) {
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
                    self.check_external_emit_helpers(node, ExternalEmitHelpers::ForAwaitOfIncludes);
                }
            }
        } else if self.compiler_options.downlevel_iteration == Some(true)
            && self.language_version < ScriptTarget::ES2015
        {
            self.check_external_emit_helpers(node, ExternalEmitHelpers::ForOfIncludes);
        }

        if node_as_for_of_statement.initializer.kind() == SyntaxKind::VariableDeclarationList {
            self.check_for_in_or_for_of_variable_declaration(node);
        } else {
            let var_expr = &node_as_for_of_statement.initializer;
            let iterated_type = self.check_right_hand_side_of_for_of(node);

            if matches!(
                var_expr.kind(),
                SyntaxKind::ArrayLiteralExpression | SyntaxKind::ObjectLiteralExpression
            ) {
                self.check_destructuring_assignment(
                    var_expr,
                    &iterated_type, /*|| errorType*/
                    None,
                    None,
                );
            } else {
                let left_type = self.check_expression(var_expr, None, None);
                self.check_reference_expression(
                    var_expr,
                    &Diagnostics::The_left_hand_side_of_a_for_of_statement_must_be_a_variable_or_a_property_access,
                    &Diagnostics::The_left_hand_side_of_a_for_of_statement_may_not_be_an_optional_property_access,
                );

                // if (iteratedType) {
                self.check_type_assignable_to_and_optionally_elaborate(
                    &iterated_type,
                    &left_type,
                    Some(&**var_expr),
                    Some(&*node_as_for_of_statement.expression),
                    None,
                    None,
                );
                // }
            }
        }

        self.check_source_element(Some(&*node_as_for_of_statement.statement));
        if node.maybe_locals().is_some() {
            self.register_for_unused_identifiers_check(node);
        }
    }

    pub(super) fn check_for_in_statement(&self, node: &Node /*ForInStatement*/) {
        self.check_grammar_for_in_or_for_of_statement(node);

        let node_as_for_in_statement = node.as_for_in_statement();
        let right_type = self.get_non_nullable_type_if_needed(&self.check_expression(
            &node_as_for_in_statement.expression,
            None,
            None,
        ));
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
            self.check_for_in_or_for_of_variable_declaration(node);
        } else {
            let var_expr = &node_as_for_in_statement.initializer;
            let left_type = self.check_expression(var_expr, None, None);
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
                .is_type_assignable_to(&self.get_index_type_or_string(&right_type), &left_type)
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

        if Rc::ptr_eq(&right_type, &self.never_type())
            || !self.is_type_assignable_to_kind(
                &right_type,
                TypeFlags::NonPrimitive | TypeFlags::InstantiableNonPrimitive,
                None,
            )
        {
            self.error(
                Some(&*node_as_for_in_statement.expression),
                &Diagnostics::The_right_hand_side_of_a_for_in_statement_must_be_of_type_any_an_object_type_or_a_type_parameter_but_here_has_type_0,
                Some(vec![
                    self.type_to_string_(
                        &right_type,
                        Option::<&Node>::None,
                        None,None,
                    )
                ])
            );
        }

        self.check_source_element(Some(&*node_as_for_in_statement.statement));
        if node.maybe_locals().is_some() {
            self.register_for_unused_identifiers_check(node);
        }
    }

    pub(super) fn check_for_in_or_for_of_variable_declaration(
        &self,
        iteration_statement: &Node, /*ForInOrOfStatement*/
    ) {
        let variable_declaration_list = iteration_statement
            .as_has_initializer()
            .maybe_initializer()
            .unwrap();
        let variable_declaration_list = variable_declaration_list.as_variable_declaration_list();
        if !variable_declaration_list.declarations.is_empty() {
            let decl = &variable_declaration_list.declarations[0];
            self.check_variable_declaration(decl);
        }
    }

    pub(super) fn check_right_hand_side_of_for_of(
        &self,
        statement: &Node, /*ForOfStatement*/
    ) -> Rc<Type> {
        let statement_as_for_of_statement = statement.as_for_of_statement();
        let use_ = if statement_as_for_of_statement.await_modifier.is_some() {
            IterationUse::ForAwaitOf
        } else {
            IterationUse::ForOf
        };
        self.check_iterated_type_or_element_type(
            use_,
            &self.check_non_null_expression(&statement_as_for_of_statement.expression),
            &self.undefined_type(),
            Some(&*statement_as_for_of_statement.expression),
        )
    }

    pub(super) fn check_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
    ) -> Rc<Type> {
        if self.is_type_any(Some(input_type)) {
            return input_type.type_wrapper();
        }
        self.get_iterated_type_or_element_type(use_, input_type, sent_type, error_node, true)
            .unwrap_or_else(|| self.any_type())
    }

    pub(super) fn get_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
        check_assignability: bool,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_iteration_type_of_iterable<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        type_kind: IterationTypeKind,
        input_type: &Type,
        error_node: Option<TErrorNode>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn create_iteration_types(
        &self,
        yield_type: Option<Rc<Type>>,
        return_type: Option<Rc<Type>>,
        next_type: Option<Rc<Type>>,
    ) -> IterationTypes {
        let yield_type = yield_type.unwrap_or_else(|| self.never_type());
        let return_type = return_type.unwrap_or_else(|| self.never_type());
        let next_type = next_type.unwrap_or_else(|| self.unknown_type());
        IterationTypes::new(yield_type, return_type, next_type)
    }

    pub(super) fn get_iteration_types_of_iterable<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        use_: IterationUse,
        error_node: Option<TErrorNode>,
    ) -> Option<IterationTypes> {
        unimplemented!()
    }

    pub(super) fn get_iteration_types_of_global_iterable_type(
        &self,
        global_type: &Type,
        resolver: &IterationTypesResolver,
    ) -> IterationTypes {
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
    ) -> Option<IterationTypes> {
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
