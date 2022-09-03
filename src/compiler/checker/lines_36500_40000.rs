#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, IterationTypeKind, IterationUse};
use crate::{
    declaration_name_to_string, find_ancestor, for_each, get_ancestor, get_combined_node_flags,
    get_containing_function_or_class_static_block, get_effective_initializer,
    get_enclosing_block_scope_container, get_function_flags, get_module_instance_state,
    get_name_of_declaration, get_source_file_of_node, is_binding_element, is_class_expression,
    is_class_like, is_enum_declaration, is_external_or_common_js_module, is_function_expression,
    is_function_like, is_identifier, is_module_declaration, is_named_declaration,
    is_parameter_declaration, ClassLikeDeclarationInterface, Debug_, DiagnosticMessage,
    Diagnostics, FunctionFlags, HasInitializerInterface, HasTypeParametersInterface,
    IterationTypes, IterationTypesResolver, ModuleInstanceState, ModuleKind, Node, NodeCheckFlags,
    NodeFlags, NodeInterface, ScriptTarget, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_if_this_is_captured_in_enclosing_scope(&self, node: &Node) {
        find_ancestor(Some(node), |current: &Node| {
            if self
                .get_node_check_flags(current)
                .intersects(NodeCheckFlags::CaptureThis)
            {
                let is_declaration = node.kind() != SyntaxKind::Identifier;
                if is_declaration {
                    self.error(
                            get_name_of_declaration(Some(node)),
                            &Diagnostics::Duplicate_identifier_this_Compiler_uses_variable_declaration_this_to_capture_this_reference,
                            None,
                        );
                } else {
                    self.error(
                            Some(node),
                            &Diagnostics::Expression_resolves_to_variable_declaration_this_that_compiler_uses_to_capture_this_reference,
                            None,
                        );
                }
                return true;
            }
            false
        });
    }

    pub(super) fn check_if_new_target_is_captured_in_enclosing_scope(&self, node: &Node) {
        find_ancestor(Some(node), |current: &Node| {
            if self
                .get_node_check_flags(current)
                .intersects(NodeCheckFlags::CaptureNewTarget)
            {
                let is_declaration = node.kind() != SyntaxKind::Identifier;
                if is_declaration {
                    self.error(
                            get_name_of_declaration(Some(node)),
                            &Diagnostics::Duplicate_identifier_newTarget_Compiler_uses_variable_declaration_newTarget_to_capture_new_target_meta_property_reference,
                            None,
                        );
                } else {
                    self.error(
                            Some(node),
                            &Diagnostics::Expression_resolves_to_variable_declaration_newTarget_that_compiler_uses_to_capture_new_target_meta_property_reference,
                            None,
                        );
                }
                return true;
            }
            false
        });
    }

    pub(super) fn check_collision_with_require_exports_in_generated_code<TName: Borrow<Node>>(
        &self,
        node: &Node,
        name: Option<TName /*Identifier*/>,
    ) {
        if self.module_kind >= ModuleKind::ES2015
            && !(self.module_kind >= ModuleKind::Node12
                && get_source_file_of_node(Some(node))
                    .unwrap()
                    .as_source_file()
                    .maybe_implied_node_format()
                    == Some(ModuleKind::CommonJS))
        {
            return;
        }

        if name.is_none() {
            return;
        }
        let name = name.unwrap();
        let name: &Node = name.borrow();
        if !self.need_collision_check_for_identifier(node, Some(name), "require")
            && !self.need_collision_check_for_identifier(node, Some(name), "exports")
        {
            return;
        }

        if is_module_declaration(node)
            && get_module_instance_state(node, None) != ModuleInstanceState::Instantiated
        {
            return;
        }

        let parent = self.get_declaration_container(node);
        if parent.kind() == SyntaxKind::SourceFile && is_external_or_common_js_module(&parent) {
            self.error_skipped_on(
                "noEmit".to_owned(),
                Some(name),
                &Diagnostics::Duplicate_identifier_0_Compiler_reserves_name_1_in_top_level_scope_of_a_module,
                Some(vec![
                    declaration_name_to_string(Some(name)).into_owned(),
                    declaration_name_to_string(Some(name)).into_owned(),
                ])
            );
        }
    }

    pub(super) fn check_collision_with_global_promise_in_generated_code<TName: Borrow<Node>>(
        &self,
        node: &Node,
        name: Option<TName /*Identifier*/>,
    ) {
        if name.is_none() {
            return;
        }
        let name = name.unwrap();
        let name: &Node = name.borrow();
        if self.language_version >= ScriptTarget::ES2017
            || !self.need_collision_check_for_identifier(node, Some(name), "Promise")
        {
            return;
        }

        if is_module_declaration(node)
            && get_module_instance_state(node, None) != ModuleInstanceState::Instantiated
        {
            return;
        }

        let parent = self.get_declaration_container(node);
        if parent.kind() == SyntaxKind::SourceFile
            && is_external_or_common_js_module(&parent)
            && parent.flags().intersects(NodeFlags::HasAsyncFunctions)
        {
            self.error_skipped_on(
                "noEmit".to_owned(),
                Some(name),
                &Diagnostics::Duplicate_identifier_0_Compiler_reserves_name_1_in_top_level_scope_of_a_module_containing_async_functions,
                Some(vec![
                    declaration_name_to_string(Some(name)).into_owned(),
                    declaration_name_to_string(Some(name)).into_owned(),
                ])
            );
        }
    }

    pub(super) fn record_potential_collision_with_weak_map_set_in_generated_code(
        &self,
        node: &Node,
        name: &Node, /*Identifier*/
    ) {
        if self.language_version <= ScriptTarget::ES2021
            && (self.need_collision_check_for_identifier(node, Some(name), "WeakMap")
                || self.need_collision_check_for_identifier(node, Some(name), "WeakSet"))
        {
            self.potential_weak_map_set_collisions()
                .push(node.node_wrapper());
        }
    }

    pub(super) fn check_weak_map_set_collision(&self, node: &Node) {
        let enclosing_block_scope = get_enclosing_block_scope_container(node).unwrap();
        if self
            .get_node_check_flags(&enclosing_block_scope)
            .intersects(NodeCheckFlags::ContainsClassWithPrivateIdentifiers)
        {
            Debug_.assert(
                is_named_declaration(node) &&
                is_identifier(&node.as_named_declaration().name()) /*&& typeof node.name.escapedText === "string"*/,
                Some("The target of a WeakMap/WeakSet collision check should be an identifier")
            );
            self.error_skipped_on(
                "noEmit".to_owned(),
                Some(node),
                &Diagnostics::Compiler_reserves_name_0_when_emitting_private_identifier_downlevel,
                Some(vec![node
                    .as_named_declaration()
                    .name()
                    .as_identifier()
                    .escaped_text
                    .clone()
                    .into_string()]),
            );
        }
    }

    pub(super) fn record_potential_collision_with_reflect_in_generated_code<TName: Borrow<Node>>(
        &self,
        node: &Node,
        name: Option<TName /*Identifier*/>,
    ) {
        if name.is_none() {
            return;
        }
        let name = name.unwrap();
        let name: &Node = name.borrow();
        if self.language_version >= ScriptTarget::ES2015
            && self.language_version <= ScriptTarget::ES2021
            && self.need_collision_check_for_identifier(node, Some(name), "Reflect")
        {
            self.potential_reflect_collisions()
                .push(node.node_wrapper());
        }
    }

    pub(super) fn check_reflect_collision(&self, node: &Node) {
        let mut has_collision = false;
        if is_class_expression(node) {
            for member in node.as_class_expression().members() {
                if self
                    .get_node_check_flags(member)
                    .intersects(NodeCheckFlags::ContainsSuperPropertyInStaticInitializer)
                {
                    has_collision = true;
                    break;
                }
            }
        } else if is_function_expression(node) {
            if self
                .get_node_check_flags(node)
                .intersects(NodeCheckFlags::ContainsSuperPropertyInStaticInitializer)
            {
                has_collision = true;
            }
        } else {
            let container = get_enclosing_block_scope_container(node);
            if matches!(
                container.as_ref(),
                Some(container) if self.get_node_check_flags(container).intersects(NodeCheckFlags::ContainsSuperPropertyInStaticInitializer)
            ) {
                has_collision = true;
            }
        }
        if has_collision {
            Debug_.assert(
                is_named_declaration(node) && is_identifier(&node.as_named_declaration().name()),
                Some("The target of a Reflect collision check should be an identifier"),
            );
            self.error_skipped_on(
                "noEmit".to_owned(),
                Some(node),
                &Diagnostics::Duplicate_identifier_0_Compiler_reserves_name_1_when_emitting_super_references_in_static_initializers,
                Some(vec![
                    declaration_name_to_string(node.as_named_declaration().maybe_name()).into_owned(),
                    "Reflect".to_owned()
                ])
            );
        }
    }

    pub(super) fn check_collisions_for_declaration_name<TName: Borrow<Node>>(
        &self,
        node: &Node,
        name: Option<TName /*Identifier*/>,
    ) {
        if name.is_none() {
            return;
        }
        let name = name.unwrap();
        let name: &Node = name.borrow();
        self.check_collision_with_require_exports_in_generated_code(node, Some(name));
        self.check_collision_with_global_promise_in_generated_code(node, Some(name));
        self.record_potential_collision_with_weak_map_set_in_generated_code(node, name);
        self.record_potential_collision_with_reflect_in_generated_code(node, Some(name));
        if is_class_like(node) {
            self.check_type_name_is_reserved(name, &Diagnostics::Class_name_cannot_be_0);
            if !node.flags().intersects(NodeFlags::Ambient) {
                self.check_class_name_collision_with_object(name);
            }
        } else if is_enum_declaration(node) {
            self.check_type_name_is_reserved(name, &Diagnostics::Enum_name_cannot_be_0);
        }
    }

    pub(super) fn check_var_declared_names_not_shadowed(
        &self,
        node: &Node, /*VariableDeclaration | BindingElement*/
    ) {
        if get_combined_node_flags(node).intersects(NodeFlags::BlockScoped)
            || is_parameter_declaration(node)
        {
            return;
        }

        if node.kind() == SyntaxKind::VariableDeclaration
            && node.as_variable_declaration().maybe_initializer().is_none()
        {
            return;
        }

        let symbol = self.get_symbol_of_node(node).unwrap();
        if symbol
            .flags()
            .intersects(SymbolFlags::FunctionScopedVariable)
        {
            let node_name = node.as_named_declaration().name();
            if !is_identifier(&node_name) {
                Debug_.fail(None);
            }
            let local_declaration_symbol = self.resolve_name_(
                Some(node),
                &node_name.as_identifier().escaped_text,
                SymbolFlags::Variable,
                None,
                Option::<Rc<Node>>::None,
                false,
                None,
            );
            if let Some(local_declaration_symbol) =
                local_declaration_symbol
                    .as_ref()
                    .filter(|local_declaration_symbol| {
                        !Rc::ptr_eq(local_declaration_symbol, &symbol)
                            && local_declaration_symbol
                                .flags()
                                .intersects(SymbolFlags::BlockScopedVariable)
                    })
            {
                if self
                    .get_declaration_node_flags_from_symbol(local_declaration_symbol)
                    .intersects(NodeFlags::BlockScoped)
                {
                    let var_decl_list = get_ancestor(
                        local_declaration_symbol.maybe_value_declaration(),
                        SyntaxKind::VariableDeclarationList,
                    )
                    .unwrap();
                    let container =
                        if var_decl_list.parent().kind() == SyntaxKind::VariableStatement {
                            var_decl_list.parent().maybe_parent()
                        } else {
                            None
                        };

                    let names_share_scope = matches!(
                        container.as_ref(),
                        Some(container) if container.kind() == SyntaxKind::Block && is_function_like(container.maybe_parent()) ||
                            matches!(
                                container.kind(),
                                SyntaxKind::ModuleBlock |
                                SyntaxKind::ModuleDeclaration |
                                SyntaxKind::SourceFile
                            )
                    );

                    if !names_share_scope {
                        let name = self.symbol_to_string_(
                            local_declaration_symbol,
                            Option::<&Node>::None,
                            None,
                            None,
                            None,
                        );
                        self.error(
                            Some(node),
                            &Diagnostics::Cannot_initialize_outer_scoped_variable_0_in_the_same_scope_as_block_scoped_declaration_1,
                            Some(vec![
                                name.clone(),
                                name,
                            ])
                        );
                    }
                }
            }
        }
    }

    pub(super) fn convert_auto_to_any(&self, type_: &Type) -> Rc<Type> {
        if ptr::eq(type_, &*self.auto_type()) {
            self.any_type()
        } else if ptr::eq(type_, &*self.auto_array_type()) {
            self.any_array_type()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn check_variable_like_declaration(&self, node: &Node) {
        let node_as_variable_like_declaration = node.as_variable_like_declaration();
        if !is_binding_element(node) {
            self.check_source_element(node_as_variable_like_declaration.maybe_type());
        }

        let symbol = self.get_symbol_of_node(node).unwrap();

        let type_ = self.convert_auto_to_any(&self.get_type_of_symbol(&*symbol));
        let value_declaration = symbol.maybe_value_declaration();
        if value_declaration.is_some() && ptr::eq(node, &*value_declaration.unwrap()) {
            let initializer = get_effective_initializer(node);
            if let Some(initializer) = initializer {
                if true {
                    let initializer_type = self.check_expression_cached(&initializer, None);
                    self.check_type_assignable_to_and_optionally_elaborate(
                        &initializer_type,
                        &type_,
                        Some(node),
                        Some(&*initializer),
                        None,
                        None,
                    );
                }
            }
        } else {
            unimplemented!()
        }
    }

    pub(super) fn error_next_variable_or_property_declaration_must_have_same_type<
        TFirstDeclaration: Borrow<Node>,
    >(
        &self,
        first_declaration: Option<TFirstDeclaration /*Declaration*/>,
        first_type: &Type,
        next_declaration: &Node, /*Declaration*/
        next_type: &Type,
    ) {
        unimplemented!()
    }

    pub(super) fn check_variable_declaration(&self, node: &Node /*VariableDeclaration*/) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_variable_statement(&self, node: &Node /*VariableStatement*/) {
        for_each(
            &node
                .as_variable_statement()
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
            |declaration, _| Some(self.check_source_element(Some(&**declaration))),
        );
    }

    pub(super) fn check_expression_statement(&self, node: &Node /*ExpressionStatement*/) {
        let expression = &node.as_expression_statement().expression;
        self.check_expression(expression, None, None);
    }

    pub(super) fn check_if_statement(&self, node: &Node /*IfStatement*/) {
        let node_as_if_statement = node.as_if_statement();
        let type_ = self.check_truthiness_expression(&node_as_if_statement.expression, None);
        self.check_source_element(Some(&*node_as_if_statement.then_statement));

        if node_as_if_statement.then_statement.kind() == SyntaxKind::EmptyStatement {
            self.error(
                Some(&*node_as_if_statement.then_statement),
                &Diagnostics::The_body_of_an_if_statement_cannot_be_the_empty_statement,
                None,
            );
        }

        self.check_source_element(node_as_if_statement.else_statement.clone());
    }

    pub(super) fn check_testing_known_truthy_callable_or_awaitable_type<TBody: Borrow<Node>>(
        &self,
        cond_expr: &Node, /*Expression*/
        type_: &Type,
        body: Option<TBody /*Statement | Expression*/>,
    ) {
        unimplemented!()
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

    pub(super) fn check_right_hand_side_of_for_of(
        &self,
        statement: &Node, /*ForOfStatement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
        check_assignability: bool,
    ) -> Rc<Type> {
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
}
