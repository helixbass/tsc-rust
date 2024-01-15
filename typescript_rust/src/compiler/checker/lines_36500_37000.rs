use std::{borrow::Borrow, io, ptr};

use gc::Gc;
use id_arena::Id;

use super::IterationUse;
use crate::{
    add_related_info, are_option_gcs_equal, create_diagnostic_for_node, declaration_name_to_string,
    find_ancestor, get_ancestor, get_combined_node_flags, get_containing_function,
    get_effective_initializer, get_enclosing_block_scope_container, get_module_instance_state,
    get_name_of_declaration, get_selected_effective_modifier_flags, get_source_file_of_node,
    has_question_token, is_array_binding_pattern, is_binary_expression, is_binding_element,
    is_binding_pattern, is_call_expression, is_class_expression, is_class_like,
    is_enum_declaration, is_external_or_common_js_module, is_function_expression, is_function_like,
    is_identifier, is_in_js_file, is_module_declaration, is_named_declaration,
    is_object_binding_pattern, is_object_literal_expression, is_parameter_declaration,
    is_property_access_expression, is_prototype_access, is_require_variable_declaration,
    is_variable_like, node_is_missing, some, try_for_each, try_for_each_child_bool,
    ClassLikeDeclarationInterface, Debug_, Diagnostics, ExternalEmitHelpers, HasArena,
    HasInitializerInterface, InArena, ModifierFlags, ModuleInstanceState, ModuleKind, Node,
    NodeArray, NodeCheckFlags, NodeFlags, NodeInterface, OptionTry, ScriptTarget, SignatureKind,
    Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_if_this_is_captured_in_enclosing_scope(&self, node: Id<Node>) {
        find_ancestor(Some(node), |current: Id<Node>| {
            if self
                .get_node_check_flags(current)
                .intersects(NodeCheckFlags::CaptureThis)
            {
                let is_declaration = node.kind() != SyntaxKind::Identifier;
                if is_declaration {
                    self.error(
                            get_name_of_declaration(Some(node), self),
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

    pub(super) fn check_if_new_target_is_captured_in_enclosing_scope(&self, node: Id<Node>) {
        find_ancestor(Some(node), |current: Id<Node>| {
            if self
                .get_node_check_flags(current)
                .intersects(NodeCheckFlags::CaptureNewTarget)
            {
                let is_declaration = node.kind() != SyntaxKind::Identifier;
                if is_declaration {
                    self.error(
                            get_name_of_declaration(Some(node), self),
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

    pub(super) fn check_collision_with_require_exports_in_generated_code(
        &self,
        node: Id<Node>,
        name: Option<Id<Node> /*Identifier*/>,
    ) {
        if self.module_kind >= ModuleKind::ES2015
            && !(self.module_kind >= ModuleKind::Node12
                && get_source_file_of_node(node, self)
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
        let name: Id<Node> = name.borrow();
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
                    declaration_name_to_string(Some(name), self).into_owned(),
                    declaration_name_to_string(Some(name), self).into_owned(),
                ])
            );
        }
    }

    pub(super) fn check_collision_with_global_promise_in_generated_code(
        &self,
        node: Id<Node>,
        name: Option<Id<Node> /*Identifier*/>,
    ) {
        if name.is_none() {
            return;
        }
        let name = name.unwrap();
        let name: Id<Node> = name.borrow();
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
                    declaration_name_to_string(Some(name), self).into_owned(),
                    declaration_name_to_string(Some(name), self).into_owned(),
                ])
            );
        }
    }

    pub(super) fn record_potential_collision_with_weak_map_set_in_generated_code(
        &self,
        node: Id<Node>,
        name: Id<Node>, /*Identifier*/
    ) {
        if self.language_version <= ScriptTarget::ES2021
            && (self.need_collision_check_for_identifier(node, Some(name), "WeakMap")
                || self.need_collision_check_for_identifier(node, Some(name), "WeakSet"))
        {
            self.potential_weak_map_set_collisions()
                .push(node.node_wrapper());
        }
    }

    pub(super) fn check_weak_map_set_collision(&self, node: Id<Node>) {
        let enclosing_block_scope = get_enclosing_block_scope_container(node, self).unwrap();
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
                    .clone()]),
            );
        }
    }

    pub(super) fn record_potential_collision_with_reflect_in_generated_code(
        &self,
        node: Id<Node>,
        name: Option<Id<Node> /*Identifier*/>,
    ) {
        if name.is_none() {
            return;
        }
        let name = name.unwrap();
        let name: Id<Node> = name.borrow();
        if self.language_version >= ScriptTarget::ES2015
            && self.language_version <= ScriptTarget::ES2021
            && self.need_collision_check_for_identifier(node, Some(name), "Reflect")
        {
            self.potential_reflect_collisions()
                .push(node.node_wrapper());
        }
    }

    pub(super) fn check_reflect_collision(&self, node: Id<Node>) {
        let mut has_collision = false;
        if is_class_expression(node) {
            for member in &node.as_class_expression().members() {
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
            let container = get_enclosing_block_scope_container(node, self);
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
                    declaration_name_to_string(node.as_named_declaration().maybe_name(), self).into_owned(),
                    "Reflect".to_owned()
                ])
            );
        }
    }

    pub(super) fn check_collisions_for_declaration_name(
        &self,
        node: Id<Node>,
        name: Option<Id<Node> /*Identifier*/>,
    ) {
        if name.is_none() {
            return;
        }
        let name = name.unwrap();
        let name: Id<Node> = name.borrow();
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
        node: Id<Node>, /*VariableDeclaration | BindingElement*/
    ) -> io::Result<()> {
        if get_combined_node_flags(node).intersects(NodeFlags::BlockScoped)
            || is_parameter_declaration(node, self)
        {
            return Ok(());
        }

        if node.kind() == SyntaxKind::VariableDeclaration
            && node.as_variable_declaration().maybe_initializer().is_none()
        {
            return Ok(());
        }

        let symbol = self.get_symbol_of_node(node)?.unwrap();
        if symbol
            .ref_(self)
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
                Option::<Id<Node>>::None,
                false,
                None,
            )?;
            if let Some(local_declaration_symbol) =
                local_declaration_symbol.filter(|&local_declaration_symbol| {
                    local_declaration_symbol != symbol
                        && local_declaration_symbol
                            .ref_(self)
                            .flags()
                            .intersects(SymbolFlags::BlockScopedVariable)
                })
            {
                if self
                    .get_declaration_node_flags_from_symbol(local_declaration_symbol)
                    .intersects(NodeFlags::BlockScoped)
                {
                    let var_decl_list = get_ancestor(
                        local_declaration_symbol
                            .ref_(self)
                            .maybe_value_declaration(),
                        SyntaxKind::VariableDeclarationList,
                        self,
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
                            Option::<Id<Node>>::None,
                            None,
                            None,
                            None,
                        )?;
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

        Ok(())
    }

    pub(super) fn convert_auto_to_any(&self, type_: Id<Type>) -> Id<Type> {
        if type_ == self.auto_type() {
            self.any_type()
        } else if type_ == self.auto_array_type() {
            self.any_array_type()
        } else {
            type_
        }
    }

    pub(super) fn check_variable_like_declaration(
        &self,
        node: Id<Node>, /*ParameterDeclaration | PropertyDeclaration | PropertySignature | VariableDeclaration | BindingElement*/
    ) -> io::Result<()> {
        self.check_decorators(node)?;
        if !is_binding_element(node) {
            self.check_source_element(node.as_variable_like_declaration().maybe_type())?;
        }

        let node_name = node.as_named_declaration().maybe_name();
        if node_name.is_none() {
            return Ok(());
        }
        let node_name = node_name.unwrap();

        let node_as_has_initializer = node.as_has_initializer();
        if node_name.kind() == SyntaxKind::ComputedPropertyName {
            self.check_computed_property_name(&node_name)?;
            if let Some(node_initializer) = node_as_has_initializer.maybe_initializer().as_ref() {
                self.check_expression_cached(node_initializer, None)?;
            }
        }

        if is_binding_element(node) {
            let node_as_binding_element = node.as_binding_element();
            if is_object_binding_pattern(&node.parent())
                && node_as_binding_element.dot_dot_dot_token.is_some()
                && self.language_version <= ScriptTarget::ES2018
            {
                self.check_external_emit_helpers(node, ExternalEmitHelpers::Rest)?;
            }
            if let Some(node_property_name) =
                node_as_binding_element
                    .property_name
                    .as_ref()
                    .filter(|node_property_name| {
                        node_property_name.kind() == SyntaxKind::ComputedPropertyName
                    })
            {
                self.check_computed_property_name(node_property_name)?;
            }

            let parent = node.parent().parent();
            let parent_type = self.get_type_for_binding_element_parent(&parent)?;
            let name = node_as_binding_element
                .property_name
                .clone()
                .unwrap_or_else(|| node_name.clone());
            if let Some(parent_type) = parent_type {
                if !is_binding_pattern(Some(&*name)) {
                    let expr_type = self.get_literal_type_from_property_name(&name)?;
                    if self.is_type_usable_as_property_name(expr_type) {
                        let name_text = self.get_property_name_from_type(expr_type);
                        let property = self.get_property_of_type_(parent_type, &name_text, None)?;
                        if let Some(property) = property {
                            self.mark_property_as_referenced(
                                property,
                                Option::<Id<Node>>::None,
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
                            )?;
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
                self.check_external_emit_helpers(node, ExternalEmitHelpers::Read)?;
            }

            try_for_each(
                &node_name.as_has_elements().elements(),
                |element: &Id<Node>, _| -> io::Result<Option<()>> {
                    self.check_source_element(Some(&**element))?;
                    Ok(None)
                },
            )?;
        }
        if node_as_has_initializer.maybe_initializer().is_some()
            && is_parameter_declaration(node, self)
            && node_is_missing(
                get_containing_function(node)
                    .unwrap()
                    .maybe_as_function_like_declaration()
                    .and_then(|node| node.maybe_body()),
            )
        {
            self.error(
                Some(node),
                &Diagnostics::A_parameter_initializer_is_only_allowed_in_a_function_or_constructor_implementation,
                None,
            );
            return Ok(());
        }
        if is_binding_pattern(Some(&*node_name)) {
            let need_check_initializer = node_as_has_initializer.maybe_initializer().is_some()
                && node.parent().parent().kind() != SyntaxKind::ForInStatement;
            let need_check_widened_type = node_name.as_has_elements().elements().is_empty();
            if need_check_initializer || need_check_widened_type {
                let widened_type =
                    self.get_widened_type_for_variable_like_declaration(node, None)?;
                if need_check_initializer {
                    let initializer_type = self.check_expression_cached(
                        &node_as_has_initializer.maybe_initializer().unwrap(),
                        None,
                    )?;
                    if self.strict_null_checks && need_check_widened_type {
                        self.check_non_null_non_void_type(initializer_type, node)?;
                    } else {
                        self.check_type_assignable_to_and_optionally_elaborate(
                            initializer_type,
                            self.get_widened_type_for_variable_like_declaration(node, None)?,
                            Some(node),
                            node_as_has_initializer.maybe_initializer(),
                            None,
                            None,
                        )?;
                    }
                }
                if need_check_widened_type {
                    if is_array_binding_pattern(&node_name) {
                        self.check_iterated_type_or_element_type(
                            IterationUse::Destructuring,
                            widened_type,
                            self.undefined_type(),
                            Some(node),
                        )?;
                    } else if self.strict_null_checks {
                        self.check_non_null_non_void_type(widened_type, node)?;
                    }
                }
            }
            return Ok(());
        }
        let symbol = self.get_symbol_of_node(node)?.unwrap();
        if symbol.ref_(self).flags().intersects(SymbolFlags::Alias)
            && is_require_variable_declaration(node, self)
        {
            self.check_alias_symbol(node)?;
            return Ok(());
        }

        let type_ = self.convert_auto_to_any(self.get_type_of_symbol(symbol)?);
        if matches!(
            symbol.ref_(self).maybe_value_declaration().as_ref(),
            Some(symbol_value_declaration) if ptr::eq(
                node,
                &**symbol_value_declaration
            )
        ) {
            let initializer = get_effective_initializer(node, self);
            if let Some(initializer) = initializer.as_ref() {
                let is_js_object_literal_initializer = is_in_js_file(Some(node))
                    && is_object_literal_expression(initializer)
                    && (initializer
                        .as_object_literal_expression()
                        .properties
                        .is_empty()
                        || is_prototype_access(node_name, self))
                    && matches!(
                        symbol.ref_(self).maybe_exports().as_ref(),
                        Some(symbol_exports) if !(**symbol_exports).borrow().is_empty()
                    );
                if !is_js_object_literal_initializer
                    && node.parent().parent().kind() != SyntaxKind::ForInStatement
                {
                    self.check_type_assignable_to_and_optionally_elaborate(
                        self.check_expression_cached(initializer, None)?,
                        type_,
                        Some(node),
                        Some(&**initializer),
                        None,
                        None,
                    )?;
                }
            }
            if let Some(symbol_declarations) = symbol
                .ref_(self)
                .maybe_declarations()
                .as_ref()
                .filter(|symbol_declarations| symbol_declarations.len() > 1)
            {
                if some(
                    Some(symbol_declarations),
                    Some(|d: &Id<Node>| {
                        !ptr::eq(&**d, node)
                            && is_variable_like(d)
                            && !self.are_declaration_flags_identical(d, node)
                    }),
                ) {
                    self.error(
                        Some(&*node_name),
                        &Diagnostics::All_declarations_of_0_must_have_identical_modifiers,
                        Some(vec![
                            declaration_name_to_string(Some(node_name), self).into_owned()
                        ]),
                    );
                }
            }
        } else {
            let declaration_type = self.convert_auto_to_any(
                self.get_widened_type_for_variable_like_declaration(node, None)?,
            );

            if !self.is_error_type(type_)
                && !self.is_error_type(declaration_type)
                && !self.is_type_identical_to(type_, declaration_type)?
                && !symbol
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::Assignment)
            {
                self.error_next_variable_or_property_declaration_must_have_same_type(
                    symbol.ref_(self).maybe_value_declaration(),
                    type_,
                    node,
                    declaration_type,
                )?;
            }
            if let Some(node_initializer) = node_as_has_initializer.maybe_initializer().as_ref() {
                self.check_type_assignable_to_and_optionally_elaborate(
                    self.check_expression_cached(node_initializer, None)?,
                    declaration_type,
                    Some(node),
                    Some(&**node_initializer),
                    None,
                    None,
                )?;
            }
            if matches!(
                symbol.ref_(self).maybe_value_declaration().as_ref(),
                Some(symbol_value_declaration) if !self.are_declaration_flags_identical(
                    node,
                    symbol_value_declaration
                )
            ) {
                self.error(
                    Some(&*node_name),
                    &Diagnostics::All_declarations_of_0_must_have_identical_modifiers,
                    Some(vec![
                        declaration_name_to_string(Some(node_name), self).into_owned()
                    ]),
                );
            }
        }
        if !matches!(
            node.kind(),
            SyntaxKind::PropertyDeclaration | SyntaxKind::PropertySignature
        ) {
            self.check_exports_on_merged_declarations(node)?;
            if matches!(
                node.kind(),
                SyntaxKind::VariableDeclaration | SyntaxKind::BindingElement
            ) {
                self.check_var_declared_names_not_shadowed(node)?;
            }
            self.check_collisions_for_declaration_name(node, Some(&*node_name));
        }

        Ok(())
    }

    pub(super) fn error_next_variable_or_property_declaration_must_have_same_type(
        &self,
        first_declaration: Option<Id<Node> /*Declaration*/>,
        first_type: Id<Type>,
        next_declaration: Id<Node>, /*Declaration*/
        next_type: Id<Type>,
    ) -> io::Result<()> {
        let next_declaration_name = get_name_of_declaration(Some(next_declaration), self);
        let message = if matches!(
            next_declaration.kind(),
            SyntaxKind::PropertyDeclaration | SyntaxKind::PropertySignature
        ) {
            &*Diagnostics::Subsequent_property_declarations_must_have_the_same_type_Property_0_must_be_of_type_1_but_here_has_type_2
        } else {
            &*Diagnostics::Subsequent_variable_declarations_must_have_the_same_type_Variable_0_must_be_of_type_1_but_here_has_type_2
        };
        let decl_name = declaration_name_to_string(next_declaration_name, self).into_owned();
        let err = self.error(
            next_declaration_name.as_deref(),
            message,
            Some(vec![
                decl_name.clone(),
                self.type_to_string_(first_type, Option::<Id<Node>>::None, None, None)?,
                self.type_to_string_(next_type, Option::<Id<Node>>::None, None, None)?,
            ]),
        );
        if let Some(first_declaration) = first_declaration {
            let first_declaration: Id<Node> = first_declaration.borrow();
            add_related_info(
                &err,
                vec![Gc::new(
                    create_diagnostic_for_node(
                        first_declaration,
                        &Diagnostics::_0_was_also_declared_here,
                        Some(vec![decl_name]),
                    )
                    .into(),
                )],
            );
        }

        Ok(())
    }

    pub(super) fn are_declaration_flags_identical(
        &self,
        left: Id<Node>,  /*Declaration*/
        right: Id<Node>, /*Declaration*/
    ) -> bool {
        if left.kind() == SyntaxKind::Parameter && right.kind() == SyntaxKind::VariableDeclaration
            || left.kind() == SyntaxKind::VariableDeclaration
                && right.kind() == SyntaxKind::Parameter
        {
            return true;
        }

        if has_question_token(left) != has_question_token(right) {
            return false;
        }

        let interesting_flags = ModifierFlags::Private
            | ModifierFlags::Protected
            | ModifierFlags::Async
            | ModifierFlags::Abstract
            | ModifierFlags::Readonly
            | ModifierFlags::Static;

        get_selected_effective_modifier_flags(left, interesting_flags, self)
            == get_selected_effective_modifier_flags(right, interesting_flags, self)
    }

    pub(super) fn check_variable_declaration(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
    ) -> io::Result<()> {
        // tracing?.push(tracing.Phase.Check, "checkVariableDeclaration", { kind: node.kind, pos: node.pos, end: node.end });
        self.check_grammar_variable_declaration(node)?;
        self.check_variable_like_declaration(node)?;
        // tracing?.pop();

        Ok(())
    }

    pub(super) fn check_binding_element(
        &self,
        node: Id<Node>, /*BindingElement*/
    ) -> io::Result<()> {
        self.check_grammar_binding_element(node);
        self.check_variable_like_declaration(node)?;

        Ok(())
    }

    pub(super) fn check_variable_statement(
        &self,
        node: Id<Node>, /*VariableStatement*/
    ) -> io::Result<()> {
        let node_as_variable_statement = node.as_variable_statement();
        if !self.check_grammar_decorators_and_modifiers(node)
            && !self.check_grammar_variable_declaration_list(
                &node_as_variable_statement.declaration_list,
            )
        {
            self.check_grammar_for_disallowed_let_or_const_statement(node);
        }
        try_for_each(
            &node_as_variable_statement
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
            |declaration, _| -> io::Result<Option<()>> {
                self.check_source_element(Some(&**declaration))?;
                Ok(None)
            },
        )?;

        Ok(())
    }

    pub(super) fn check_expression_statement(
        &self,
        node: Id<Node>, /*ExpressionStatement*/
    ) -> io::Result<()> {
        self.check_grammar_statement_in_ambient_context(node);

        self.check_expression(&node.as_expression_statement().expression, None, None)?;

        Ok(())
    }

    pub(super) fn check_if_statement(&self, node: Id<Node> /*IfStatement*/) -> io::Result<()> {
        self.check_grammar_statement_in_ambient_context(node);
        let node_as_if_statement = node.as_if_statement();
        let type_ = self.check_truthiness_expression(&node_as_if_statement.expression, None)?;
        self.check_testing_known_truthy_callable_or_awaitable_type(
            &node_as_if_statement.expression,
            type_,
            Some(&*node_as_if_statement.then_statement),
        )?;
        self.check_source_element(Some(&*node_as_if_statement.then_statement))?;

        if node_as_if_statement.then_statement.kind() == SyntaxKind::EmptyStatement {
            self.error(
                Some(&*node_as_if_statement.then_statement),
                &Diagnostics::The_body_of_an_if_statement_cannot_be_the_empty_statement,
                None,
            );
        }

        self.check_source_element(node_as_if_statement.else_statement.as_deref())?;

        Ok(())
    }

    pub(super) fn check_testing_known_truthy_callable_or_awaitable_type(
        &self,
        cond_expr: Id<Node>, /*Expression*/
        type_: Id<Type>,
        body: Option<Id<Node> /*Statement | Expression*/>,
    ) -> io::Result<()> {
        if !self.strict_null_checks {
            return Ok(());
        }
        if self.get_falsy_flags(type_) != TypeFlags::None {
            return Ok(());
        }

        let location = if is_binary_expression(cond_expr) {
            &*cond_expr.as_binary_expression().right
        } else {
            cond_expr
        };
        if is_property_access_expression(location)
            && self.is_type_assertion(&location.as_property_access_expression().expression)
        {
            return Ok(());
        }

        let tested_node = if is_identifier(location) {
            Some(location.node_wrapper())
        } else if is_property_access_expression(location) {
            Some(location.as_property_access_expression().name.clone())
        } else if is_binary_expression(location)
            && is_identifier(&location.as_binary_expression().right)
        {
            Some(location.as_binary_expression().right.clone())
        } else {
            None
        };

        let call_signatures = self.get_signatures_of_type(type_, SignatureKind::Call)?;
        let is_promise = self
            .get_awaited_type_of_promise(type_, Option::<Id<Node>>::None, None, None)?
            .is_some();
        if call_signatures.is_empty() && !is_promise {
            return Ok(());
        }

        let tested_symbol = tested_node
            .as_ref()
            .try_and_then(|tested_node| self.get_symbol_at_location_(tested_node, None))?;
        if tested_symbol.is_none() && !is_promise {
            return Ok(());
        }

        let body = body.map(|body| body.borrow().node_wrapper());
        let is_used = matches!(
            tested_symbol,
            Some(tested_symbol) if is_binary_expression(&cond_expr.parent()) &&
                self.is_symbol_used_in_binary_expression_chain(&cond_expr.parent(), tested_symbol)?
        ) || matches!(
            (tested_symbol, body.as_ref()),
            (Some(tested_symbol), Some(body)) if self.is_symbol_used_in_condition_body(
                cond_expr,
                body,
                tested_node.as_ref().unwrap(),
                tested_symbol,
            )?
        );
        if !is_used {
            if is_promise {
                self.error_and_maybe_suggest_await(
                    location,
                    true,
                    &Diagnostics::This_condition_will_always_return_true_since_this_0_is_always_defined,
                    Some(vec![
                        self.get_type_name_for_error_display(type_)?
                    ])
                );
            } else {
                self.error(
                    Some(location),
                    &Diagnostics::This_condition_will_always_return_true_since_this_function_is_always_defined_Did_you_mean_to_call_it_instead,
                    None,
                );
            }
        }

        Ok(())
    }

    pub(super) fn is_symbol_used_in_condition_body(
        &self,
        expr: Id<Node>, /*Expression*/
        body: Id<Node>, /*Statement | Expression*/
        tested_node: Id<Node>,
        tested_symbol: Id<Symbol>,
    ) -> io::Result<bool> {
        try_for_each_child_bool(
            body,
            |child_node| {
                self.is_symbol_used_in_condition_body_check(
                    expr,
                    tested_node,
                    tested_symbol,
                    child_node,
                )
            },
            Option::<fn(&NodeArray) -> io::Result<bool>>::None,
        )
    }

    pub(super) fn is_symbol_used_in_condition_body_check(
        &self,
        expr: Id<Node>,
        tested_node: Id<Node>,
        tested_symbol: Id<Symbol>,
        child_node: Id<Node>,
    ) -> io::Result<bool> {
        if is_identifier(child_node) {
            let child_symbol = self.get_symbol_at_location_(child_node, None)?;
            if matches!(
                child_symbol,
                Some(child_symbol) if child_symbol == tested_symbol
            ) {
                if is_identifier(expr) {
                    return Ok(true);
                }
                let mut tested_expression = tested_node.maybe_parent();
                let mut child_expression = child_node.maybe_parent();
                while let (Some(tested_expression_present), Some(child_expression_present)) =
                    (tested_expression.as_ref(), child_expression.as_ref())
                {
                    if is_identifier(tested_expression_present)
                        && is_identifier(child_expression_present)
                        || tested_expression_present.kind() == SyntaxKind::ThisKeyword
                            && child_expression_present.kind() == SyntaxKind::ThisKeyword
                    {
                        return Ok(self
                            .get_symbol_at_location_(tested_expression_present, None)?
                            == self.get_symbol_at_location_(child_expression_present, None)?);
                    } else if is_property_access_expression(tested_expression_present)
                        && is_property_access_expression(child_expression_present)
                    {
                        let tested_expression_present_as_property_access_expression =
                            tested_expression_present.as_property_access_expression();
                        let child_expression_present_as_property_access_expression =
                            child_expression_present.as_property_access_expression();
                        if self.get_symbol_at_location_(
                            &tested_expression_present_as_property_access_expression.name,
                            None,
                        )? != self.get_symbol_at_location_(
                            &child_expression_present_as_property_access_expression.name,
                            None,
                        )? {
                            return Ok(false);
                        }
                        child_expression = Some(
                            child_expression_present_as_property_access_expression
                                .expression
                                .clone(),
                        );
                        tested_expression = Some(
                            tested_expression_present_as_property_access_expression
                                .expression
                                .clone(),
                        );
                    } else if is_call_expression(tested_expression_present)
                        && is_call_expression(child_expression_present)
                    {
                        child_expression = Some(
                            child_expression_present
                                .as_call_expression()
                                .expression
                                .clone(),
                        );
                        tested_expression = Some(
                            tested_expression_present
                                .as_call_expression()
                                .expression
                                .clone(),
                        );
                    } else {
                        return Ok(false);
                    }
                }
            }
        }
        try_for_each_child_bool(
            child_node,
            |child_node| {
                self.is_symbol_used_in_condition_body_check(
                    expr,
                    tested_node,
                    tested_symbol,
                    child_node,
                )
            },
            Option::<fn(&NodeArray) -> io::Result<bool>>::None,
        )
    }
}
