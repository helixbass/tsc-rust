use std::{collections::HashMap, convert::TryInto, io, ptr};

use gc::Gc;
use id_arena::Id;

use crate::{
    add_related_info, create_diagnostic_for_node, first, get_containing_function,
    get_function_flags, get_set_accessor_value_parameter, get_source_file_of_node,
    get_this_parameter, has_effective_readonly_modifier, has_syntactic_modifier, id_text,
    is_binding_pattern, is_comma_sequence, is_declaration_readonly, is_effective_external_module,
    is_element_access_expression, is_entity_name_expression, is_for_of_statement,
    is_function_like_or_class_static_block_declaration, is_identifier, is_in_js_file,
    is_in_top_level_context, is_iteration_statement, is_jsdoc_type_expression, is_jsdoc_type_tag,
    is_let, is_private_identifier, is_property_access_expression, is_static,
    is_string_or_numeric_literal_like, is_var_const, is_variable_declaration,
    is_variable_declaration_in_variable_statement, last, maybe_is_class_like, token_to_string,
    walk_up_parenthesized_types, Debug_, Diagnostic, DiagnosticMessage,
    DiagnosticRelatedInformation, Diagnostics, FunctionFlags, FunctionLikeDeclarationInterface,
    HasArena, HasInitializerInterface, HasQuestionTokenInterface, HasTypeInterface, InArena,
    ModifierFlags, ModuleKind, NamedDeclarationInterface, Node, NodeFlags, NodeInterface,
    ReadonlyTextRange, ScriptTarget, SyntaxKind, TypeChecker, TypeFlags, TypeInterface, __String,
    OptionInArena,
};

impl TypeChecker {
    pub(super) fn check_grammar_jsx_element(
        &self,
        node: Id<Node>, /*JsxOpeningLikeElement*/
    ) -> bool {
        let node_ref = node.ref_(self);
        let node_as_jsx_opening_like_element = node_ref.as_jsx_opening_like_element();
        self.check_grammar_jsx_name(node_as_jsx_opening_like_element.tag_name());
        self.check_grammar_type_arguments(
            node,
            node.ref_(self).as_has_type_arguments()
                .maybe_type_arguments()
                .as_deref(),
        );
        let mut seen: HashMap<__String, bool> = HashMap::new();

        for attr in &node_as_jsx_opening_like_element
            .attributes()
            .ref_(self).as_jsx_attributes()
            .properties
        {
            if attr.ref_(self).kind() == SyntaxKind::JsxSpreadAttribute {
                continue;
            }

            let attr_ref = attr.ref_(self);
            let attr_as_jsx_attribute = attr_ref.as_jsx_attribute();
            let name = attr_as_jsx_attribute.name;
            let initializer = attr_as_jsx_attribute.initializer;
            let name_ref = name.ref_(self);
            let name_as_identifier = name_ref.as_identifier();
            if seen.get(&name_as_identifier.escaped_text).copied() != Some(true) {
                seen.insert(name_as_identifier.escaped_text.clone(), true);
            } else {
                return self.grammar_error_on_node(
                    name,
                    &Diagnostics::JSX_elements_cannot_have_multiple_attributes_with_the_same_name,
                    None,
                );
            }

            if let Some(initializer) = initializer.filter(|initializer| {
                initializer.ref_(self).kind() == SyntaxKind::JsxExpression
                    && initializer.ref_(self).as_jsx_expression().expression.is_none()
            }) {
                return self.grammar_error_on_node(
                    initializer,
                    &Diagnostics::JSX_attributes_must_only_be_assigned_a_non_empty_expression,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn check_grammar_jsx_name(
        &self,
        node: Id<Node>, /*JsxTagNameExpression*/
    ) -> bool {
        if is_property_access_expression(&node.ref_(self)) {
            let mut prop_name: Id<Node /*JsxTagNameExpression*/> = node;
            while {
                let check = self
                    .check_grammar_jsx_nested_identifier(prop_name.ref_(self).as_named_declaration().name());
                if check {
                    return check;
                }
                prop_name = prop_name.ref_(self).as_has_expression().expression();
                is_property_access_expression(&prop_name.ref_(self))
            } {}
            let check = self.check_grammar_jsx_nested_identifier(prop_name);
            if check {
                return check;
            }
        }
        false
    }

    pub(super) fn check_grammar_jsx_nested_identifier(
        &self,
        name: Id<Node>, /*MemberName | ThisExpression*/
    ) -> bool {
        if is_identifier(&name.ref_(self)) && id_text(&name.ref_(self)).contains(":") {
            return self.grammar_error_on_node(
                name,
                &Diagnostics::JSX_property_access_expressions_cannot_include_JSX_namespace_names,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_jsx_expression(
        &self,
        node: Id<Node>, /*JsxExpression*/
    ) -> bool {
        if let Some(node_expression) = node
            .ref_(self).as_jsx_expression()
            .expression
            .filter(|&node_expression| is_comma_sequence(node_expression, self))
        {
            return self.grammar_error_on_node(
                node_expression,
                &Diagnostics::JSX_expressions_may_not_use_the_comma_operator_Did_you_mean_to_write_an_array,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_for_in_or_for_of_statement(
        &self,
        for_in_or_of_statement: Id<Node>, /*ForInOrOfStatement*/
    ) -> bool {
        if self.check_grammar_statement_in_ambient_context(for_in_or_of_statement) {
            return true;
        }

        if for_in_or_of_statement.ref_(self).kind() == SyntaxKind::ForOfStatement {
            if let Some(for_in_or_of_statement_await_modifier) = for_in_or_of_statement
                .ref_(self).as_for_of_statement()
                .await_modifier
            {
                if !for_in_or_of_statement
                    .ref_(self).flags()
                    .intersects(NodeFlags::AwaitContext)
                {
                    let source_file = get_source_file_of_node(for_in_or_of_statement, self);
                    if is_in_top_level_context(for_in_or_of_statement, self) {
                        if !self.has_parse_diagnostics(source_file) {
                            if !is_effective_external_module(&source_file.ref_(self), &self.compiler_options.ref_(self)) {
                                self.diagnostics().add(
                                    Gc::new(
                                        create_diagnostic_for_node(
                                            for_in_or_of_statement_await_modifier,
                                            &Diagnostics::for_await_loops_are_only_allowed_at_the_top_level_of_a_file_when_that_file_is_a_module_but_this_file_has_no_imports_or_exports_Consider_adding_an_empty_export_to_make_this_file_a_module,
                                            None,
                                            self,
                                        ).into()
                                    )
                                );
                            }
                            if !matches!(
                                self.module_kind,
                                ModuleKind::ES2022 | ModuleKind::ESNext | ModuleKind::System
                            ) && !(self.module_kind == ModuleKind::NodeNext
                                && get_source_file_of_node(for_in_or_of_statement, self)
                                    .ref_(self).as_source_file()
                                    .maybe_implied_node_format()
                                    == Some(ModuleKind::ESNext))
                                || self.language_version < ScriptTarget::ES2017
                            {
                                self.diagnostics().add(
                                    Gc::new(
                                        create_diagnostic_for_node(
                                            for_in_or_of_statement_await_modifier,
                                            &Diagnostics::Top_level_for_await_loops_are_only_allowed_when_the_module_option_is_set_to_es2022_esnext_system_or_nodenext_and_the_target_option_is_set_to_es2017_or_higher,
                                            None,
                                            self,
                                        ).into()
                                    )
                                );
                            }
                        }
                    } else {
                        if !self.has_parse_diagnostics(source_file) {
                            let diagnostic: Gc<Diagnostic> = Gc::new(
                                create_diagnostic_for_node(
                                    for_in_or_of_statement_await_modifier,
                                    &Diagnostics::for_await_loops_are_only_allowed_within_async_functions_and_at_the_top_levels_of_modules,
                                    None,
                                    self,
                                ).into()
                            );
                            let func = get_containing_function(for_in_or_of_statement, self);
                            if let Some(func) = func
                                .filter(|func| func.ref_(self).kind() != SyntaxKind::Constructor)
                            {
                                Debug_.assert(
                                    !get_function_flags(Some(func), self)
                                        .intersects(FunctionFlags::Async),
                                    Some("Enclosing function should never be an async function."),
                                );
                                let related_info: Gc<DiagnosticRelatedInformation> = Gc::new(
                                    create_diagnostic_for_node(
                                        func,
                                        &Diagnostics::Did_you_mean_to_mark_this_function_as_async,
                                        None,
                                        self,
                                    )
                                    .into(),
                                );
                                add_related_info(&diagnostic, vec![related_info]);
                            }
                            self.diagnostics().add(diagnostic);
                            return true;
                        }
                    }
                    return false;
                }
            }
        }

        if is_for_of_statement(&for_in_or_of_statement.ref_(self))
            && !for_in_or_of_statement
                .ref_(self).flags()
                .intersects(NodeFlags::AwaitContext)
        {
            let for_in_or_of_statement_ref = for_in_or_of_statement.ref_(self);
            let for_in_or_of_statement_as_for_of_statement = for_in_or_of_statement_ref.as_for_of_statement();
            if is_identifier(&for_in_or_of_statement_as_for_of_statement.initializer.ref_(self))
                && for_in_or_of_statement_as_for_of_statement
                    .initializer
                    .ref_(self).as_identifier()
                    .escaped_text
                    == "async"
            {
                self.grammar_error_on_node(
                    for_in_or_of_statement_as_for_of_statement.initializer,
                    &Diagnostics::The_left_hand_side_of_a_for_of_statement_may_not_be_async,
                    None,
                );
                return false;
            }
        }

        if for_in_or_of_statement
            .ref_(self).as_has_initializer()
            .maybe_initializer()
            .unwrap()
            .ref_(self).kind()
            == SyntaxKind::VariableDeclarationList
        {
            let variable_list = for_in_or_of_statement
                .ref_(self).as_has_initializer()
                .maybe_initializer()
                .unwrap();
            if !self.check_grammar_variable_declaration_list(variable_list) {
                let variable_list_ref = variable_list.ref_(self);
                let declarations = &variable_list_ref.as_variable_declaration_list().declarations;

                if declarations.is_empty() {
                    return false;
                }

                if declarations.len() > 1 {
                    let diagnostic = if for_in_or_of_statement.ref_(self).kind() == SyntaxKind::ForInStatement
                    {
                        &*Diagnostics::Only_a_single_variable_declaration_is_allowed_in_a_for_in_statement
                    } else {
                        &*Diagnostics::Only_a_single_variable_declaration_is_allowed_in_a_for_of_statement
                    };
                    return self.grammar_error_on_first_token(
                        variable_list.ref_(self).as_variable_declaration_list().declarations[1],
                        diagnostic,
                        None,
                    );
                }
                let first_declaration = declarations[0];

                if first_declaration
                    .ref_(self).as_has_initializer()
                    .maybe_initializer()
                    .is_some()
                {
                    let diagnostic = if for_in_or_of_statement.ref_(self).kind() == SyntaxKind::ForInStatement
                    {
                        &*Diagnostics::The_variable_declaration_of_a_for_in_statement_cannot_have_an_initializer
                    } else {
                        &*Diagnostics::The_variable_declaration_of_a_for_of_statement_cannot_have_an_initializer
                    };
                    return self.grammar_error_on_node(
                        first_declaration.ref_(self).as_named_declaration().name(),
                        diagnostic,
                        None,
                    );
                }
                if first_declaration.ref_(self).as_has_type().maybe_type().is_some() {
                    let diagnostic = if for_in_or_of_statement.ref_(self).kind() == SyntaxKind::ForInStatement
                    {
                        &*Diagnostics::The_left_hand_side_of_a_for_in_statement_cannot_use_a_type_annotation
                    } else {
                        &*Diagnostics::The_left_hand_side_of_a_for_of_statement_cannot_use_a_type_annotation
                    };
                    return self.grammar_error_on_node(first_declaration, diagnostic, None);
                }
            }
        }

        false
    }

    pub(super) fn check_grammar_accessor(
        &self,
        accessor: Id<Node>, /*AccessorDeclaration*/
    ) -> bool {
        let accessor_ref = accessor.ref_(self);
        let accessor_as_function_like_declaration = accessor_ref.as_function_like_declaration();
        if !accessor.ref_(self).flags().intersects(NodeFlags::Ambient)
            && !matches!(
                accessor.ref_(self).parent().ref_(self).kind(),
                SyntaxKind::TypeLiteral | SyntaxKind::InterfaceDeclaration
            )
        {
            if self.language_version < ScriptTarget::ES5 {
                return self.grammar_error_on_node(
                    accessor_as_function_like_declaration.name(),
                    &Diagnostics::Accessors_are_only_available_when_targeting_ECMAScript_5_and_higher,
                    None,
                );
            }
            if self.language_version < ScriptTarget::ES2015
                && is_private_identifier(&accessor_as_function_like_declaration.name().ref_(self))
            {
                return self.grammar_error_on_node(
                    accessor_as_function_like_declaration.name(),
                    &Diagnostics::Private_identifiers_are_only_available_when_targeting_ECMAScript_2015_and_higher,
                    None,
                );
            }
            if accessor_as_function_like_declaration.maybe_body().is_none()
                && !has_syntactic_modifier(accessor, ModifierFlags::Abstract, self)
            {
                return self.grammar_error_at_pos(
                    accessor,
                    accessor.ref_(self).end() - 1,
                    ";".len().try_into().unwrap(),
                    &Diagnostics::_0_expected,
                    Some(vec!["{".to_owned()]),
                );
            }
        }
        if let Some(accessor_body) = accessor_as_function_like_declaration.maybe_body() {
            if has_syntactic_modifier(accessor, ModifierFlags::Abstract, self) {
                return self.grammar_error_on_node(
                    accessor,
                    &Diagnostics::An_abstract_accessor_cannot_have_an_implementation,
                    None,
                );
            }
            if matches!(
                accessor.ref_(self).parent().ref_(self).kind(),
                SyntaxKind::TypeLiteral | SyntaxKind::InterfaceDeclaration
            ) {
                return self.grammar_error_on_node(
                    accessor_body,
                    &Diagnostics::An_implementation_cannot_be_declared_in_ambient_contexts,
                    None,
                );
            }
        }
        if accessor_as_function_like_declaration
            .maybe_type_parameters()
            .is_some()
        {
            return self.grammar_error_on_node(
                accessor_as_function_like_declaration.name(),
                &Diagnostics::An_accessor_cannot_have_type_parameters,
                None,
            );
        }
        if !self.does_accessor_have_correct_parameter_count(accessor) {
            return self.grammar_error_on_node(
                accessor_as_function_like_declaration.name(),
                if accessor.ref_(self).kind() == SyntaxKind::GetAccessor {
                    &*Diagnostics::A_get_accessor_cannot_have_parameters
                } else {
                    &*Diagnostics::A_set_accessor_must_have_exactly_one_parameter
                },
                None,
            );
        }
        if accessor.ref_(self).kind() == SyntaxKind::SetAccessor {
            if accessor_as_function_like_declaration.maybe_type().is_some() {
                return self.grammar_error_on_node(
                    accessor_as_function_like_declaration.name(),
                    &Diagnostics::A_set_accessor_cannot_have_a_return_type_annotation,
                    None,
                );
            }
            let parameter = Debug_.check_defined(
                get_set_accessor_value_parameter(accessor, self),
                Some("Return value does not match parameter count assertion."),
            );
            let parameter_ref = parameter.ref_(self);
            let parameter_as_parameter_declaration = parameter_ref.as_parameter_declaration();
            if parameter_as_parameter_declaration
                .dot_dot_dot_token
                .is_some()
            {
                return self.grammar_error_on_node(
                    parameter_as_parameter_declaration
                        .dot_dot_dot_token
                        .unwrap(),
                    &Diagnostics::A_set_accessor_cannot_have_rest_parameter,
                    None,
                );
            }
            if parameter_as_parameter_declaration.question_token.is_some() {
                return self.grammar_error_on_node(
                    parameter_as_parameter_declaration
                        .question_token
                        .unwrap(),
                    &Diagnostics::A_set_accessor_cannot_have_an_optional_parameter,
                    None,
                );
            }
            if parameter_as_parameter_declaration
                .maybe_initializer()
                .is_some()
            {
                return self.grammar_error_on_node(
                    accessor_as_function_like_declaration.name(),
                    &Diagnostics::A_set_accessor_parameter_cannot_have_an_initializer,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn does_accessor_have_correct_parameter_count(
        &self,
        accessor: Id<Node>, /*AccessorDeclaration*/
    ) -> bool {
        self.get_accessor_this_parameter(accessor).is_some()
            || accessor.ref_(self).as_function_like_declaration().parameters().len()
                == if accessor.ref_(self).kind() == SyntaxKind::GetAccessor {
                    0
                } else {
                    1
                }
    }

    pub(super) fn get_accessor_this_parameter(
        &self,
        accessor: Id<Node>, /*AccessorDeclaration*/
    ) -> Option<Id<Node /*ParameterDeclaration*/>> {
        if accessor.ref_(self).as_function_like_declaration().parameters().len()
            == if accessor.ref_(self).kind() == SyntaxKind::GetAccessor {
                1
            } else {
                2
            }
        {
            return get_this_parameter(accessor, self);
        }
        None
    }

    pub(super) fn check_grammar_type_operator_node(
        &self,
        node: Id<Node>, /*TypeOperatorNode*/
    ) -> bool {
        let node_ref = node.ref_(self);
        let node_as_type_operator_node = node_ref.as_type_operator_node();
        if node_as_type_operator_node.operator == SyntaxKind::UniqueKeyword {
            if node_as_type_operator_node.type_.ref_(self).kind() != SyntaxKind::SymbolKeyword {
                return self.grammar_error_on_node(
                    node_as_type_operator_node.type_,
                    &Diagnostics::_0_expected,
                    Some(vec![token_to_string(SyntaxKind::SymbolKeyword)
                        .unwrap()
                        .to_owned()]),
                );
            }

            let mut parent = walk_up_parenthesized_types(node.ref_(self).parent(), self).unwrap();
            if is_in_js_file(Some(&parent.ref_(self))) && is_jsdoc_type_expression(&parent.ref_(self)) {
                parent = parent.ref_(self).parent();
                if is_jsdoc_type_tag(&parent.ref_(self)) {
                    parent = parent.ref_(self).parent().ref_(self).parent();
                }
            }
            match parent.ref_(self).kind() {
                SyntaxKind::VariableDeclaration => {
                    let decl = parent;
                    if decl.ref_(self).as_variable_declaration().name().ref_(self).kind() != SyntaxKind::Identifier {
                        return self.grammar_error_on_node(
                            node,
                            &Diagnostics::unique_symbol_types_may_not_be_used_on_a_variable_declaration_with_a_binding_name,
                            None,
                        );
                    }
                    if !is_variable_declaration_in_variable_statement(decl, self) {
                        return self.grammar_error_on_node(
                            node,
                            &Diagnostics::unique_symbol_types_are_only_allowed_on_variables_in_a_variable_statement,
                            None,
                        );
                    }
                    if !decl.ref_(self).parent().ref_(self).flags().intersects(NodeFlags::Const) {
                        return self.grammar_error_on_node(
                            parent.ref_(self).as_variable_declaration().name(),
                            &Diagnostics::A_variable_whose_type_is_a_unique_symbol_type_must_be_const,
                            None,
                        );
                    }
                }

                SyntaxKind::PropertyDeclaration => {
                    if !is_static(parent, self) || !has_effective_readonly_modifier(parent, self) {
                        return self.grammar_error_on_node(
                            parent.ref_(self).as_property_declaration().name(),
                            &Diagnostics::A_property_of_a_class_whose_type_is_a_unique_symbol_type_must_be_both_static_and_readonly,
                            None,
                        );
                    }
                }

                SyntaxKind::PropertySignature => {
                    if !has_syntactic_modifier(parent, ModifierFlags::Readonly, self) {
                        return self.grammar_error_on_node(
                            parent.ref_(self).as_property_signature().name(),
                            &Diagnostics::A_property_of_an_interface_or_type_literal_whose_type_is_a_unique_symbol_type_must_be_readonly,
                            None,
                        );
                    }
                }

                _ => {
                    return self.grammar_error_on_node(
                        node,
                        &Diagnostics::unique_symbol_types_are_not_allowed_here,
                        None,
                    );
                }
            }
        } else if node_as_type_operator_node.operator == SyntaxKind::ReadonlyKeyword {
            if !matches!(
                node_as_type_operator_node.type_.ref_(self).kind(),
                SyntaxKind::ArrayType | SyntaxKind::TupleType
            ) {
                return self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::readonly_type_modifier_is_only_permitted_on_array_and_tuple_literal_types,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn check_grammar_for_invalid_dynamic_name(
        &self,
        node: Id<Node>, /*DeclarationName*/
        message: &'static DiagnosticMessage,
    ) -> io::Result<bool> {
        if self.is_non_bindable_dynamic_name(node)? {
            return Ok(self.grammar_error_on_node(node, message, None));
        }
        Ok(false)
    }

    #[allow(clippy::if_same_then_else)]
    pub(super) fn check_grammar_method(
        &self,
        node: Id<Node>, /*MethodDeclaration | MethodSignature*/
    ) -> io::Result<bool> {
        if self.check_grammar_function_like_declaration(node)? {
            return Ok(true);
        }

        if node.ref_(self).kind() == SyntaxKind::MethodDeclaration {
            let node_ref = node.ref_(self);
            let node_as_method_declaration = node_ref.as_method_declaration();
            if node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ObjectLiteralExpression {
                if matches!(
                    node_as_method_declaration.maybe_modifiers().as_ref(),
                    Some(node_modifiers) if !(
                        node_modifiers.len() == 1 && first(&*node_modifiers).ref_(self).kind() == SyntaxKind::AsyncKeyword
                    )
                ) {
                    return Ok(self.grammar_error_on_first_token(
                        node,
                        &Diagnostics::Modifiers_cannot_appear_here,
                        None,
                    ));
                } else if self.check_grammar_for_invalid_question_mark(
                    node_as_method_declaration.maybe_question_token(),
                    &Diagnostics::An_object_member_cannot_be_declared_optional,
                ) {
                    return Ok(true);
                } else if self.check_grammar_for_invalid_exclamation_token(
                    *node_as_method_declaration
                        .maybe_exclamation_token(),
                    &Diagnostics::A_definite_assignment_assertion_is_not_permitted_in_this_context,
                ) {
                    return Ok(true);
                } else if node_as_method_declaration.maybe_body().is_none() {
                    return Ok(self.grammar_error_at_pos(
                        node,
                        node.ref_(self).end() - 1,
                        ";".len().try_into().unwrap(),
                        &Diagnostics::_0_expected,
                        Some(vec!["{".to_owned()]),
                    ));
                }
            }
            if self.check_grammar_for_generator(node) {
                return Ok(true);
            }
        }

        let node_ref = node.ref_(self);
        let node_as_named_declaration = node_ref.as_named_declaration();
        if maybe_is_class_like(node.ref_(self).maybe_parent().refed(self).as_deref()) {
            if self.language_version < ScriptTarget::ES2015
                && is_private_identifier(&node_as_named_declaration.name().ref_(self))
            {
                return Ok(self.grammar_error_on_node(
                    node_as_named_declaration.name(),
                    &Diagnostics::Private_identifiers_are_only_available_when_targeting_ECMAScript_2015_and_higher,
                    None,
                ));
            }
            if node.ref_(self).flags().intersects(NodeFlags::Ambient) {
                return self.check_grammar_for_invalid_dynamic_name(
                    node_as_named_declaration.name(),
                    &Diagnostics::A_computed_property_name_in_an_ambient_context_must_refer_to_an_expression_whose_type_is_a_literal_type_or_a_unique_symbol_type,
                );
            } else if node.ref_(self).kind() == SyntaxKind::MethodDeclaration
                && node.ref_(self).as_method_declaration().maybe_body().is_none()
            {
                return self.check_grammar_for_invalid_dynamic_name(
                    node_as_named_declaration.name(),
                    &Diagnostics::A_computed_property_name_in_a_method_overload_must_refer_to_an_expression_whose_type_is_a_literal_type_or_a_unique_symbol_type
                );
            }
        } else if node.ref_(self).parent().ref_(self).kind() == SyntaxKind::InterfaceDeclaration {
            return self.check_grammar_for_invalid_dynamic_name(
                node_as_named_declaration.name(),
                &Diagnostics::A_computed_property_name_in_an_interface_must_refer_to_an_expression_whose_type_is_a_literal_type_or_a_unique_symbol_type
            );
        } else if node.ref_(self).parent().ref_(self).kind() == SyntaxKind::TypeLiteral {
            return self.check_grammar_for_invalid_dynamic_name(
                node_as_named_declaration.name(),
                &Diagnostics::A_computed_property_name_in_a_type_literal_must_refer_to_an_expression_whose_type_is_a_literal_type_or_a_unique_symbol_type
            );
        }
        Ok(false)
    }

    pub(super) fn check_grammar_break_or_continue_statement(
        &self,
        node: Id<Node>, /*BreakOrContinueStatement*/
    ) -> bool {
        let mut current: Option<Id<Node>> = Some(node);
        let node_ref = node.ref_(self);
        let node_as_has_label = node_ref.as_has_label();
        while let Some(current_present) = current {
            if is_function_like_or_class_static_block_declaration(Some(&current_present.ref_(self))) {
                return self.grammar_error_on_node(
                    node,
                    &Diagnostics::Jump_target_cannot_cross_function_boundary,
                    None,
                );
            }

            match current_present.ref_(self).kind() {
                SyntaxKind::LabeledStatement => {
                    let current_present_ref = current_present.ref_(self);
                    let current_as_labeled_statement = current_present_ref.as_labeled_statement();
                    if matches!(
                        node_as_has_label.maybe_label(),
                        Some(node_label) if current_as_labeled_statement.label.ref_(self).as_identifier().escaped_text == node_label.ref_(self).as_identifier().escaped_text
                    ) {
                        let is_misplaced_continue_label = node.ref_(self).kind()
                            == SyntaxKind::ContinueStatement
                            && !is_iteration_statement(
                                current_as_labeled_statement.statement,
                                true,
                                self,
                            );

                        if is_misplaced_continue_label {
                            return self.grammar_error_on_node(
                                node,
                                &Diagnostics::A_continue_statement_can_only_jump_to_a_label_of_an_enclosing_iteration_statement,
                                None,
                            );
                        }

                        return false;
                    }
                }
                SyntaxKind::SwitchStatement => {
                    if node.ref_(self).kind() == SyntaxKind::BreakStatement
                        && node_as_has_label.maybe_label().is_none()
                    {
                        return false;
                    }
                }
                _ => {
                    if is_iteration_statement(current_present, false, self)
                        && node_as_has_label.maybe_label().is_none()
                    {
                        return false;
                    }
                }
            }

            current = current_present.ref_(self).maybe_parent();
        }

        if node_as_has_label.maybe_label().is_some() {
            let message = if node.ref_(self).kind() == SyntaxKind::BreakStatement {
                &*Diagnostics::A_break_statement_can_only_jump_to_a_label_of_an_enclosing_statement
            } else {
                &*Diagnostics::A_continue_statement_can_only_jump_to_a_label_of_an_enclosing_iteration_statement
            };

            self.grammar_error_on_node(node, message, None)
        } else {
            let message = if node.ref_(self).kind() == SyntaxKind::BreakStatement {
                &*Diagnostics::A_break_statement_can_only_be_used_within_an_enclosing_iteration_or_switch_statement
            } else {
                &*Diagnostics::A_continue_statement_can_only_be_used_within_an_enclosing_iteration_statement
            };
            self.grammar_error_on_node(node, message, None)
        }
    }

    pub(super) fn check_grammar_binding_element(
        &self,
        node: Id<Node>, /*BindingElement*/
    ) -> bool {
        let node_ref = node.ref_(self);
        let node_as_binding_element = node_ref.as_binding_element();
        if node_as_binding_element.dot_dot_dot_token.is_some() {
            let node_parent = node.ref_(self).parent();
            let elements = node_parent.ref_(self).as_has_elements().elements();
            if node != *last(&*elements) {
                return self.grammar_error_on_node(
                    node,
                    &Diagnostics::A_rest_element_must_be_last_in_a_destructuring_pattern,
                    None,
                );
            }
            self.check_grammar_for_disallowed_trailing_comma(
                Some(&elements),
                Some(
                    &Diagnostics::A_rest_parameter_or_binding_pattern_may_not_have_a_trailing_comma,
                ),
            );

            if node_as_binding_element.property_name.is_some() {
                return self.grammar_error_on_node(
                    node_as_binding_element.name(),
                    &Diagnostics::A_rest_element_cannot_have_a_property_name,
                    None,
                );
            }
        }

        if node_as_binding_element.dot_dot_dot_token.is_some() {
            if let Some(node_initializer) = node_as_binding_element.maybe_initializer() {
                return self.grammar_error_at_pos(
                    node,
                    node_initializer.ref_(self).pos() - 1,
                    1,
                    &Diagnostics::A_rest_element_cannot_have_an_initializer,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn is_string_or_number_literal_expression(
        &self,
        expr: Id<Node>, /*Expression*/
    ) -> bool {
        is_string_or_numeric_literal_like(&expr.ref_(self))
            || expr.ref_(self).kind() == SyntaxKind::PrefixUnaryExpression && {
                let expr_ref = expr.ref_(self);
                let expr_as_prefix_unary_expression = expr_ref.as_prefix_unary_expression();
                expr_as_prefix_unary_expression.operator == SyntaxKind::MinusToken
                    && expr_as_prefix_unary_expression.operand.ref_(self).kind() == SyntaxKind::NumericLiteral
            }
    }

    pub(super) fn is_big_int_literal_expression(&self, expr: Id<Node> /*Expression*/) -> bool {
        expr.ref_(self).kind() == SyntaxKind::BigIntLiteral
            || expr.ref_(self).kind() == SyntaxKind::PrefixUnaryExpression && {
                let expr_ref = expr.ref_(self);
                let expr_as_prefix_unary_expression = expr_ref.as_prefix_unary_expression();
                expr_as_prefix_unary_expression.operator == SyntaxKind::MinusToken
                    && expr_as_prefix_unary_expression.operand.ref_(self).kind() == SyntaxKind::BigIntLiteral
            }
    }

    pub(super) fn is_simple_literal_enum_reference(
        &self,
        expr: Id<Node>, /*Expression*/
    ) -> io::Result<bool> {
        if (is_property_access_expression(&expr.ref_(self))
            || is_element_access_expression(&expr.ref_(self))
                && self.is_string_or_number_literal_expression(
                    expr.ref_(self).as_element_access_expression().argument_expression,
                ))
            && is_entity_name_expression(expr.ref_(self).as_has_expression().expression(), self)
        {
            return Ok(self
                .check_expression_cached(expr, None)?
                .ref_(self)
                .flags()
                .intersects(TypeFlags::EnumLiteral));
        }
        Ok(false)
    }

    pub(super) fn check_ambient_initializer(
        &self,
        node: Id<Node>, /*VariableDeclaration | PropertyDeclaration | PropertySignature*/
    ) -> io::Result<bool> {
        let initializer = node.ref_(self).as_has_initializer().maybe_initializer();
        if let Some(initializer) = initializer {
            let is_invalid_initializer = !(self
                .is_string_or_number_literal_expression(initializer)
                || self.is_simple_literal_enum_reference(initializer)?
                || matches!(
                    initializer.ref_(self).kind(),
                    SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword
                )
                || self.is_big_int_literal_expression(initializer));
            let is_const_or_readonly = is_declaration_readonly(node, self)
                || is_variable_declaration(&node.ref_(self)) && is_var_const(node, self);
            if is_const_or_readonly && node.ref_(self).as_has_type().maybe_type().is_none() {
                if is_invalid_initializer {
                    return Ok(self.grammar_error_on_node(
                        initializer,
                        &Diagnostics::A_const_initializer_in_an_ambient_context_must_be_a_string_or_numeric_literal_or_literal_enum_reference,
                        None,
                    ));
                }
            } else {
                return Ok(self.grammar_error_on_node(
                    initializer,
                    &Diagnostics::Initializers_are_not_allowed_in_ambient_contexts,
                    None,
                ));
            }
            if !is_const_or_readonly || is_invalid_initializer {
                return Ok(self.grammar_error_on_node(
                    initializer,
                    &Diagnostics::Initializers_are_not_allowed_in_ambient_contexts,
                    None,
                ));
            }
        }
        Ok(false)
    }

    pub(super) fn check_grammar_variable_declaration(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
    ) -> io::Result<bool> {
        let node_ref = node.ref_(self);
        let node_as_variable_declaration = node_ref.as_variable_declaration();
        if !matches!(
            node.ref_(self).parent().ref_(self).parent().ref_(self).kind(),
            SyntaxKind::ForInStatement | SyntaxKind::ForOfStatement
        ) {
            if node.ref_(self).flags().intersects(NodeFlags::Ambient) {
                self.check_ambient_initializer(node)?;
            } else if node_as_variable_declaration.maybe_initializer().is_none() {
                if is_binding_pattern(node_as_variable_declaration.maybe_name().refed(self).as_deref())
                    && !is_binding_pattern(node.ref_(self).maybe_parent().refed(self).as_deref())
                {
                    return Ok(self.grammar_error_on_node(
                        node,
                        &Diagnostics::A_destructuring_declaration_must_have_an_initializer,
                        None,
                    ));
                }
                if is_var_const(node, self) {
                    return Ok(self.grammar_error_on_node(
                        node,
                        &Diagnostics::const_declarations_must_be_initialized,
                        None,
                    ));
                }
            }
        }

        if node_as_variable_declaration.exclamation_token.is_some()
            && (node.ref_(self).parent().ref_(self).parent().ref_(self).kind() != SyntaxKind::VariableStatement
                || node_as_variable_declaration.maybe_type().is_none()
                || node_as_variable_declaration.maybe_initializer().is_some()
                || node.ref_(self).flags().intersects(NodeFlags::Ambient))
        {
            let message = if node_as_variable_declaration.maybe_initializer().is_some() {
                &*Diagnostics::Declarations_with_initializers_cannot_also_have_definite_assignment_assertions
            } else if node_as_variable_declaration.maybe_type().is_none() {
                &*Diagnostics::Declarations_with_definite_assignment_assertions_must_also_have_type_annotations
            } else {
                &*Diagnostics::A_definite_assignment_assertion_is_not_permitted_in_this_context
            };
            return Ok(self.grammar_error_on_node(
                node_as_variable_declaration
                    .exclamation_token
                    .unwrap(),
                message,
                None,
            ));
        }

        if (self.module_kind < ModuleKind::ES2015
            || get_source_file_of_node(node, self)
                .ref_(self).as_source_file()
                .maybe_implied_node_format()
                == Some(ModuleKind::CommonJS))
            && self.module_kind != ModuleKind::System
            && !node
                .ref_(self).parent()
                .ref_(self).parent()
                .ref_(self).flags()
                .intersects(NodeFlags::Ambient)
            && has_syntactic_modifier(node.ref_(self).parent().ref_(self).parent(), ModifierFlags::Export, self)
        {
            self.check_es_module_marker(node_as_variable_declaration.name());
        }

        let check_let_const_names = is_let(node, self) || is_var_const(node, self);

        Ok(check_let_const_names
            && self.check_grammar_name_in_let_or_const_declarations(
                node_as_variable_declaration.name(),
            ))
    }
}
