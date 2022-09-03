#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, IterationTypeKind, IterationUse};
use crate::{
    declaration_name_to_string, find_ancestor, for_each, get_ancestor, get_combined_node_flags,
    get_containing_function, get_containing_function_or_class_static_block,
    get_effective_initializer, get_enclosing_block_scope_container, get_function_flags,
    get_module_instance_state, get_name_of_declaration, get_source_file_of_node,
    is_array_binding_pattern, is_binding_element, is_binding_pattern, is_class_expression,
    is_class_like, is_enum_declaration, is_external_or_common_js_module, is_function_expression,
    is_function_like, is_identifier, is_in_js_file, is_module_declaration, is_named_declaration,
    is_object_binding_pattern, is_object_literal_expression, is_parameter_declaration,
    is_prototype_access, is_require_variable_declaration, is_variable_like, node_is_missing, some,
    ClassLikeDeclarationInterface, Debug_, DiagnosticMessage, Diagnostics, ExternalEmitHelpers,
    FunctionFlags, HasInitializerInterface, HasTypeParametersInterface, IterationTypes,
    IterationTypesResolver, ModuleInstanceState, ModuleKind, Node, NodeCheckFlags, NodeFlags,
    NodeInterface, ScriptTarget, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type,
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

    pub(super) fn check_variable_like_declaration(
        &self,
        node: &Node, /*ParameterDeclaration | PropertyDeclaration | PropertySignature | VariableDeclaration | BindingElement*/
    ) {
        self.check_decorators(node);
        let node_as_variable_like_declaration = node.as_variable_like_declaration();
        if !is_binding_element(node) {
            self.check_source_element(node_as_variable_like_declaration.maybe_type());
        }

        let node_name = node_as_variable_like_declaration.maybe_name();
        if node_name.is_none() {
            return;
        }
        let node_name = node_name.unwrap();

        if node_name.kind() == SyntaxKind::ComputedPropertyName {
            self.check_computed_property_name(&node_name);
            if let Some(node_initializer) = node_as_variable_like_declaration
                .maybe_initializer()
                .as_ref()
            {
                self.check_expression_cached(node_initializer, None);
            }
        }

        if is_binding_element(node) {
            let node_as_binding_element = node.as_binding_element();
            if is_object_binding_pattern(&node.parent())
                && node_as_binding_element.dot_dot_dot_token.is_some()
                && self.language_version <= ScriptTarget::ES2018
            {
                self.check_external_emit_helpers(node, ExternalEmitHelpers::Rest);
            }
            if let Some(node_property_name) =
                node_as_binding_element
                    .property_name
                    .as_ref()
                    .filter(|node_property_name| {
                        node_property_name.kind() == SyntaxKind::ComputedPropertyName
                    })
            {
                self.check_computed_property_name(node_property_name);
            }

            let parent = node.parent().parent();
            let parent_type = self.get_type_for_binding_element_parent(&parent);
            let name = node_as_binding_element
                .property_name
                .clone()
                .unwrap_or_else(|| node_name.clone());
            if let Some(parent_type) = parent_type.as_ref() {
                if !is_binding_pattern(Some(&*name)) {
                    let expr_type = self.get_literal_type_from_property_name(&name);
                    if self.is_type_usable_as_property_name(&expr_type) {
                        let name_text = self.get_property_name_from_type(&expr_type);
                        let property = self.get_property_of_type_(parent_type, &name_text, None);
                        if let Some(property) = property.as_ref() {
                            self.mark_property_as_referenced(
                                property,
                                Option::<&Node>::None,
                                false,
                            );
                            self.check_property_accessibility(
                                node,
                                matches!(
                                    parent.as_has_initializer().maybe_initializer().as_ref(),
                                    Some(parent_initializer) if parent_initializer.kind() == SyntaxKind::SuperKeyword
                                ),
                                false,
                                parent_type,
                                property,
                                None,
                            );
                        }
                    }
                }
            }
        }

        if is_binding_pattern(Some(&*node_name)) {
            if node_name.kind() == SyntaxKind::ArrayBindingPattern
                && self.language_version < ScriptTarget::ES2015
                && self.compiler_options.downlevel_iteration == Some(true)
            {
                self.check_external_emit_helpers(node, ExternalEmitHelpers::Read);
            }

            for_each(
                node_name.as_has_elements().elements(),
                |element: &Rc<Node>, _| -> Option<()> {
                    self.check_source_element(Some(&**element));
                    None
                },
            );
        }
        if node_as_variable_like_declaration
            .maybe_initializer()
            .is_some()
            && is_parameter_declaration(node)
            && node_is_missing(
                get_containing_function(node)
                    .unwrap()
                    .as_function_like_declaration()
                    .maybe_body(),
            )
        {
            self.error(
                Some(node),
                &Diagnostics::A_parameter_initializer_is_only_allowed_in_a_function_or_constructor_implementation,
                None,
            );
            return;
        }
        if is_binding_pattern(Some(&*node_name)) {
            let need_check_initializer = node_as_variable_like_declaration
                .maybe_initializer()
                .is_some()
                && node.parent().parent().kind() != SyntaxKind::ForInStatement;
            let need_check_widened_type = node_name.as_has_elements().elements().is_empty();
            if need_check_initializer || need_check_widened_type {
                let widened_type = self.get_widened_type_for_variable_like_declaration(node, None);
                if need_check_initializer {
                    let initializer_type = self.check_expression_cached(
                        &node_as_variable_like_declaration
                            .maybe_initializer()
                            .unwrap(),
                        None,
                    );
                    if self.strict_null_checks && need_check_widened_type {
                        self.check_non_null_non_void_type(&initializer_type, node);
                    } else {
                        self.check_type_assignable_to_and_optionally_elaborate(
                            &initializer_type,
                            &self.get_widened_type_for_variable_like_declaration(node, None),
                            Some(node),
                            node_as_variable_like_declaration.maybe_initializer(),
                            None,
                            None,
                        );
                    }
                }
                if need_check_widened_type {
                    if is_array_binding_pattern(&node_name) {
                        self.check_iterated_type_or_element_type(
                            IterationUse::Destructuring,
                            &widened_type,
                            &self.undefined_type(),
                            Some(node),
                        );
                    } else if self.strict_null_checks {
                        self.check_non_null_non_void_type(&widened_type, node);
                    }
                }
            }
            return;
        }
        let symbol = self.get_symbol_of_node(node).unwrap();
        if symbol.flags().intersects(SymbolFlags::Alias) && is_require_variable_declaration(node) {
            self.check_alias_symbol(node);
            return;
        }

        let type_ = self.convert_auto_to_any(&self.get_type_of_symbol(&symbol));
        if matches!(
            symbol.maybe_value_declaration().as_ref(),
            Some(symbol_value_declaration) if ptr::eq(
                node,
                &**symbol_value_declaration
            )
        ) {
            let initializer = get_effective_initializer(node);
            if let Some(initializer) = initializer.as_ref() {
                let is_js_object_literal_initializer = is_in_js_file(Some(node))
                    && is_object_literal_expression(initializer)
                    && (initializer
                        .as_object_literal_expression()
                        .properties
                        .is_empty()
                        || is_prototype_access(&node_name))
                    && matches!(
                        symbol.maybe_exports().as_ref(),
                        Some(symbol_exports) if !(**symbol_exports).borrow().is_empty()
                    );
                if !is_js_object_literal_initializer
                    && node.parent().parent().kind() != SyntaxKind::ForInStatement
                {
                    self.check_type_assignable_to_and_optionally_elaborate(
                        &self.check_expression_cached(initializer, None),
                        &type_,
                        Some(node),
                        Some(&**initializer),
                        None,
                        None,
                    );
                }
            }
            if let Some(symbol_declarations) = symbol
                .maybe_declarations()
                .as_ref()
                .filter(|symbol_declarations| symbol_declarations.len() > 1)
            {
                if some(
                    Some(symbol_declarations),
                    Some(|d: &Rc<Node>| {
                        !ptr::eq(&**d, node)
                            && is_variable_like(d)
                            && !self.are_declaration_flags_identical(d, node)
                    }),
                ) {
                    self.error(
                        Some(&*node_name),
                        &Diagnostics::All_declarations_of_0_must_have_identical_modifiers,
                        Some(vec![
                            declaration_name_to_string(Some(&*node_name)).into_owned()
                        ]),
                    );
                }
            }
        } else {
            let declaration_type = self.convert_auto_to_any(
                &self.get_widened_type_for_variable_like_declaration(node, None),
            );

            if !self.is_error_type(&type_)
                && !self.is_error_type(&declaration_type)
                && !self.is_type_identical_to(&type_, &declaration_type)
                && !symbol.flags().intersects(SymbolFlags::Assignment)
            {
                self.error_next_variable_or_property_declaration_must_have_same_type(
                    symbol.maybe_value_declaration(),
                    &type_,
                    node,
                    &declaration_type,
                );
            }
            if let Some(node_initializer) = node_as_variable_like_declaration
                .maybe_initializer()
                .as_ref()
            {
                self.check_type_assignable_to_and_optionally_elaborate(
                    &self.check_expression_cached(node_initializer, None),
                    &declaration_type,
                    Some(node),
                    Some(&**node_initializer),
                    None,
                    None,
                );
            }
            if matches!(
                symbol.maybe_value_declaration().as_ref(),
                Some(symbol_value_declaration) if !self.are_declaration_flags_identical(
                    node,
                    symbol_value_declaration
                )
            ) {
                self.error(
                    Some(&*node_name),
                    &Diagnostics::All_declarations_of_0_must_have_identical_modifiers,
                    Some(vec![
                        declaration_name_to_string(Some(&*node_name)).into_owned()
                    ]),
                );
            }
        }
        if !matches!(
            node.kind(),
            SyntaxKind::PropertyDeclaration | SyntaxKind::PropertySignature
        ) {
            self.check_exports_on_merged_declarations(node);
            if matches!(
                node.kind(),
                SyntaxKind::VariableDeclaration | SyntaxKind::BindingElement
            ) {
                self.check_var_declared_names_not_shadowed(node);
            }
            self.check_collisions_for_declaration_name(node, Some(&*node_name));
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

    pub(super) fn are_declaration_flags_identical(
        &self,
        left: &Node,  /*Declaration*/
        right: &Node, /*Declaration*/
    ) -> bool {
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

    pub(super) fn check_alias_symbol(
        &self,
        node: &Node, /*ImportEqualsDeclaration | VariableDeclaration | ImportClause | NamespaceImport | ImportSpecifier | ExportSpecifier | NamespaceExport*/
    ) {
        unimplemented!()
    }
}
