#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::{EmitResolverCreateResolver, UnusedKind};
use crate::{
    first, get_source_file_of_node, has_syntactic_modifier, is_binding_pattern, is_class_like,
    is_declaration_readonly, is_element_access_expression, is_entity_name_expression,
    is_function_like_or_class_static_block_declaration, is_iteration_statement, is_let,
    is_private_identifier, is_property_access_expression, is_string_or_numeric_literal_like,
    is_var_const, is_variable_declaration, last, DiagnosticMessage, Diagnostics,
    ExternalEmitHelpers, HasInitializerInterface, HasTypeInterface, ModifierFlags, ModuleKind,
    NamedDeclarationInterface, NodeArray, NodeFlags, ReadonlyTextRange, ScriptTarget, SyntaxKind,
    TypeFlags, TypeInterface, __String, bind_source_file, for_each,
    is_external_or_common_js_module, CancellationTokenDebuggable, Diagnostic,
    EmitResolverDebuggable, IndexInfo, Node, NodeInterface, StringOrNumber, Symbol, SymbolFlags,
    Type, TypeChecker,
};

impl TypeChecker {
    pub(super) fn is_duplicated_common_js_export(&self, declarations: Option<&[Rc<Node>]>) -> bool {
        unimplemented!()
    }

    pub(super) fn check_source_element<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef>) {
        if let Some(node) = node {
            let node = node.borrow();
            self.check_source_element_worker(node);
        }
    }

    pub(super) fn check_source_element_worker(&self, node: &Node) {
        match node {
            Node::PropertySignature(_) => self.check_property_signature(node),
            Node::TypeReferenceNode(_) => self.check_type_reference_node(node),
            Node::KeywordTypeNode(_) | Node::LiteralTypeNode(_) => (),
            Node::ArrayTypeNode(_) => self.check_array_type(node),
            Node::UnionTypeNode(_) => self.check_union_or_intersection_type(node),
            Node::FunctionDeclaration(_) => self.check_function_declaration(node),
            Node::Block(_) => self.check_block(node),
            Node::VariableStatement(_) => self.check_variable_statement(node),
            Node::ExpressionStatement(_) => self.check_expression_statement(node),
            Node::IfStatement(_) => self.check_if_statement(node),
            Node::ReturnStatement(_) => self.check_return_statement(node),
            Node::VariableDeclaration(_) => self.check_variable_declaration(node),
            Node::InterfaceDeclaration(_) => self.check_interface_declaration(node),
            Node::TypeAliasDeclaration(_) => self.check_type_alias_declaration(node),
            _ => unimplemented!("{:?}", node.kind()),
        };
    }

    pub(super) fn get_type_from_jsdoc_variadic_type(
        &self,
        node: &Node, /*JSDocVariadicType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_node_deferred(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn check_source_file(&self, source_file: &Node /*SourceFile*/) {
        self.check_source_file_worker(source_file)
    }

    pub(super) fn unused_is_error(&self, kind: UnusedKind, is_ambient: bool) -> bool {
        unimplemented!()
    }

    pub(super) fn get_potentially_unused_identifiers(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Node /*PotentiallyUnusedIdentifier*/>> {
        unimplemented!()
    }

    pub(super) fn check_source_file_worker(&self, node: &Node /*SourceFile*/) {
        if true {
            for_each(&node.as_source_file().statements, |statement, _index| {
                self.check_source_element(Some(&**statement));
                Option::<()>::None
            });
        }
    }

    pub fn get_diagnostics(
        &self,
        source_file: &Node, /*SourceFile*/
        ct: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    pub(super) fn get_diagnostics_worker(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self
            .diagnostics()
            .get_diagnostics(Some(&source_file.as_source_file().file_name()));

        semantic_diagnostics
    }

    pub(super) fn get_symbols_in_scope_(
        &self,
        location_in: &Node,
        meaning: SymbolFlags,
    ) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_type_reference_identifier(&self, node: &Node /*EntityName*/) -> bool {
        unimplemented!()
    }

    pub(super) fn for_each_enclosing_class<TReturn, TCallback: FnMut(&Node) -> Option<TReturn>>(
        &self,
        node: &Node,
        callback: TCallback,
    ) -> Option<TReturn> {
        unimplemented!()
    }

    pub(super) fn is_node_used_during_class_initialization(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_node_within_class(
        &self,
        node: &Node,
        class_declaration: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_in_right_side_of_import_or_export_assignment(
        &self,
        node: &Node, /*EntityName*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_symbol_at_location_(
        &self,
        node: &Node,
        ignore_errors: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_index_infos_at_location_(&self, node: &Node) -> Option<Vec<Rc<IndexInfo>>> {
        unimplemented!()
    }

    pub(super) fn get_shorthand_assignment_value_symbol_<TNode: Borrow<Node>>(
        &self,
        location: Option<TNode>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_export_specifier_local_target_symbol_(
        &self,
        node: &Node, /*Identifier | ExportSpecifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_node(&self, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_of_assignment_pattern_(
        &self,
        expr: &Node, /*AssignmentPattern*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_property_symbol_of_destructuring_assignment_(
        &self,
        location: &Node, /*Identifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_parent_type_of_class_element(
        &self,
        node: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_class_element_property_key_type(
        &self,
        element: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_augmented_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn type_has_call_or_construct_signatures(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_const_enum_or_const_enum_only_module(&self, s: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn is_implementation_of_overload_(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_enum_member_value(
        &self,
        node: &Node, /*EnumMember*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn can_have_constant_value(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_constant_value_(
        &self,
        node: &Node, /*EnumMember | AccessExpression*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn is_function_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_referenced_value_symbol(
        &self,
        reference: &Node, /*Identifier*/
        start_in_declaration_container: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_fragment_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn create_resolver(&self) -> Rc<dyn EmitResolverDebuggable> {
        Rc::new(EmitResolverCreateResolver::new())
    }

    pub(super) fn initialize_type_checker(&mut self) {
        for file in self.host.get_source_files() {
            bind_source_file(&*file, self.compiler_options.clone());
            println!("post-binding: {:#?}", file);
        }

        for file in self.host.get_source_files() {
            if !is_external_or_common_js_module(&file) {
                self.merge_symbol_table(
                    &mut *self.globals_mut(),
                    &RefCell::borrow(&file.locals()),
                    None,
                );
            }
        }

        self.global_object_type =
            self.get_global_type(&__String::new("Object".to_owned()), 0, true);
        self.global_boolean_type =
            self.get_global_type(&__String::new("Boolean".to_owned()), 0, true);
    }

    pub(super) fn check_external_emit_helpers(
        &self,
        location: &Node,
        helpers: ExternalEmitHelpers,
    ) {
        unimplemented!()
    }

    pub(super) fn check_grammar_decorators_and_modifiers(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_disallowed_trailing_comma(
        &self,
        list: Option<&NodeArray>,
        diag: Option<&'static DiagnosticMessage>,
    ) -> bool {
        let diag = diag.unwrap_or(&Diagnostics::Trailing_comma_not_allowed);
        unimplemented!()
    }

    pub(super) fn check_grammar_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration | MethodSignature*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_index_signature(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_type_arguments(
        &self,
        node: &Node,
        type_arguments: Option<&NodeArray /*<TypeNode>*/>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_tagged_template_chain(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_arguments(
        &self,
        args: Option<&NodeArray /*<Expression>*/>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_computed_property_name(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_generator(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_invalid_question_mark<TQuestionToken: Borrow<Node>>(
        &self,
        question_token: Option<TQuestionToken /*QuestionToken*/>,
        message: &DiagnosticMessage,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_invalid_exclamation_token<TExclamationToken: Borrow<Node>>(
        &self,
        exclamation_token: Option<TExclamationToken /*ExclamationToken*/>,
        message: &DiagnosticMessage,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        in_destructuring: bool,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_jsx_element(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_jsx_expression(&self, node: &Node /*JsxExpression*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_accessor_this_parameter(
        &self,
        accessor: &Node, /*AccessorDeclaration*/
    ) -> Option<Rc<Node /*ParameterDeclaration*/>> {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_invalid_dynamic_name(
        &self,
        node: &Node, /*DeclarationName*/
        message: &'static DiagnosticMessage,
    ) -> bool {
        if self.is_non_bindable_dynamic_name(node) {
            return self.grammar_error_on_node(node, message, None);
        }
        false
    }

    pub(super) fn check_grammar_method(
        &self,
        node: &Node, /*MethodDeclaration | MethodSignature*/
    ) -> bool {
        if self.check_grammar_function_like_declaration(node) {
            return true;
        }

        let node_as_function_like_declaration = node.as_function_like_declaration();
        if node.kind() == SyntaxKind::MethodDeclaration {
            if node.parent().kind() == SyntaxKind::ObjectLiteralExpression {
                if matches!(
                    node_as_function_like_declaration.maybe_modifiers().as_ref(),
                    Some(node_modifiers) if !(
                        node_modifiers.len() == 1 && first(&*node_modifiers).kind() == SyntaxKind::AsyncKeyword
                    )
                ) {
                    return self.grammar_error_on_first_token(
                        node,
                        &Diagnostics::Modifiers_cannot_appear_here,
                        None,
                    );
                } else if self.check_grammar_for_invalid_question_mark(
                    node_as_function_like_declaration.maybe_question_token(),
                    &Diagnostics::An_object_member_cannot_be_declared_optional,
                ) {
                    return true;
                } else if self.check_grammar_for_invalid_exclamation_token(
                    node_as_function_like_declaration
                        .maybe_exclamation_token()
                        .as_deref(),
                    &Diagnostics::A_definite_assignment_assertion_is_not_permitted_in_this_context,
                ) {
                    return true;
                } else if node_as_function_like_declaration.maybe_body().is_none() {
                    return self.grammar_error_at_pos(
                        node,
                        node.end() - 1,
                        ";".len().try_into().unwrap(),
                        &Diagnostics::_0_expected,
                        Some(vec!["{".to_owned()]),
                    );
                }
            }
            if self.check_grammar_for_generator(node) {
                return true;
            }
        }

        if is_class_like(&node.parent()) {
            if self.language_version < ScriptTarget::ES2015
                && is_private_identifier(&node_as_function_like_declaration.name())
            {
                return self.grammar_error_on_node(
                    &node_as_function_like_declaration.name(),
                    &Diagnostics::Private_identifiers_are_only_available_when_targeting_ECMAScript_2015_and_higher,
                    None,
                );
            }
            if node.flags().intersects(NodeFlags::Ambient) {
                return self.check_grammar_for_invalid_dynamic_name(
                    &node_as_function_like_declaration.name(),
                    &Diagnostics::A_computed_property_name_in_an_ambient_context_must_refer_to_an_expression_whose_type_is_a_literal_type_or_a_unique_symbol_type,
                );
            } else if node.kind() == SyntaxKind::MethodDeclaration
                && node_as_function_like_declaration.maybe_body().is_none()
            {
                return self.check_grammar_for_invalid_dynamic_name(
                    &node_as_function_like_declaration.name(),
                    &Diagnostics::A_computed_property_name_in_a_method_overload_must_refer_to_an_expression_whose_type_is_a_literal_type_or_a_unique_symbol_type
                );
            }
        } else if node.parent().kind() == SyntaxKind::InterfaceDeclaration {
            return self.check_grammar_for_invalid_dynamic_name(
                &node_as_function_like_declaration.name(),
                &Diagnostics::A_computed_property_name_in_an_interface_must_refer_to_an_expression_whose_type_is_a_literal_type_or_a_unique_symbol_type
            );
        } else if node.parent().kind() == SyntaxKind::TypeLiteral {
            return self.check_grammar_for_invalid_dynamic_name(
                &node_as_function_like_declaration.name(),
                &Diagnostics::A_computed_property_name_in_a_type_literal_must_refer_to_an_expression_whose_type_is_a_literal_type_or_a_unique_symbol_type
            );
        }
        false
    }

    pub(super) fn check_grammar_break_or_continue_statement(
        &self,
        node: &Node, /*BreakOrContinueStatement*/
    ) -> bool {
        let mut current: Option<Rc<Node>> = Some(node.node_wrapper());
        let node_as_has_label = node.as_has_label();
        while let Some(current_present) = current.as_ref() {
            if is_function_like_or_class_static_block_declaration(Some(&**current_present)) {
                return self.grammar_error_on_node(
                    node,
                    &Diagnostics::Jump_target_cannot_cross_function_boundary,
                    None,
                );
            }

            match current_present.kind() {
                SyntaxKind::LabeledStatement => {
                    let current_as_labeled_statement = current_present.as_labeled_statement();
                    if matches!(
                        node_as_has_label.maybe_label().as_ref(),
                        Some(node_label) if current_as_labeled_statement.label.as_identifier().escaped_text == node_label.as_identifier().escaped_text
                    ) {
                        let is_misplaced_continue_label = node.kind()
                            == SyntaxKind::ContinueStatement
                            && !is_iteration_statement(
                                &current_as_labeled_statement.statement,
                                true,
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
                    if node.kind() == SyntaxKind::BreakStatement
                        && node_as_has_label.maybe_label().is_none()
                    {
                        return false;
                    }
                }
                _ => {
                    if is_iteration_statement(current_present, false)
                        && node_as_has_label.maybe_label().is_none()
                    {
                        return false;
                    }
                }
            }

            current = current_present.maybe_parent();
        }

        if node_as_has_label.maybe_label().is_some() {
            let message = if node.kind() == SyntaxKind::BreakStatement {
                &*Diagnostics::A_break_statement_can_only_jump_to_a_label_of_an_enclosing_statement
            } else {
                &*Diagnostics::A_continue_statement_can_only_jump_to_a_label_of_an_enclosing_iteration_statement
            };

            self.grammar_error_on_node(node, message, None)
        } else {
            let message = if node.kind() == SyntaxKind::BreakStatement {
                &*Diagnostics::A_break_statement_can_only_be_used_within_an_enclosing_iteration_or_switch_statement
            } else {
                &*Diagnostics::A_continue_statement_can_only_be_used_within_an_enclosing_iteration_statement
            };
            self.grammar_error_on_node(node, message, None)
        }
    }

    pub(super) fn check_grammar_binding_element(
        &self,
        node: &Node, /*BindingElement*/
    ) -> bool {
        let node_as_binding_element = node.as_binding_element();
        if node_as_binding_element.dot_dot_dot_token.is_some() {
            let node_parent = node.parent();
            let elements = node_parent.as_has_elements().elements();
            if !ptr::eq(node, &**last(&*elements)) {
                return self.grammar_error_on_node(
                    node,
                    &Diagnostics::A_rest_element_must_be_last_in_a_destructuring_pattern,
                    None,
                );
            }
            self.check_grammar_for_disallowed_trailing_comma(
                Some(elements),
                Some(
                    &Diagnostics::A_rest_parameter_or_binding_pattern_may_not_have_a_trailing_comma,
                ),
            );

            if node_as_binding_element.property_name.is_some() {
                return self.grammar_error_on_node(
                    &node_as_binding_element.name(),
                    &Diagnostics::A_rest_element_cannot_have_a_property_name,
                    None,
                );
            }
        }

        if node_as_binding_element.dot_dot_dot_token.is_some() {
            if let Some(node_initializer) = node_as_binding_element.maybe_initializer().as_ref() {
                return self.grammar_error_at_pos(
                    node,
                    node_initializer.pos() - 1,
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
        expr: &Node, /*Expression*/
    ) -> bool {
        is_string_or_numeric_literal_like(expr)
            || expr.kind() == SyntaxKind::PrefixUnaryExpression && {
                let expr_as_prefix_unary_expression = expr.as_prefix_unary_expression();
                expr_as_prefix_unary_expression.operator == SyntaxKind::MinusToken
                    && expr_as_prefix_unary_expression.operand.kind() == SyntaxKind::NumericLiteral
            }
    }

    pub(super) fn is_big_int_literal_expression(&self, expr: &Node /*Expression*/) -> bool {
        expr.kind() == SyntaxKind::BigIntLiteral
            || expr.kind() == SyntaxKind::PrefixUnaryExpression && {
                let expr_as_prefix_unary_expression = expr.as_prefix_unary_expression();
                expr_as_prefix_unary_expression.operator == SyntaxKind::MinusToken
                    && expr_as_prefix_unary_expression.operand.kind() == SyntaxKind::BigIntLiteral
            }
    }

    pub(super) fn is_simple_literal_enum_reference(&self, expr: &Node /*Expression*/) -> bool {
        if (is_property_access_expression(expr)
            || is_element_access_expression(expr)
                && self.is_string_or_number_literal_expression(
                    &expr.as_element_access_expression().argument_expression,
                ))
            && is_entity_name_expression(&expr.as_has_expression().expression())
        {
            return self
                .check_expression_cached(expr, None)
                .flags()
                .intersects(TypeFlags::EnumLiteral);
        }
        false
    }

    pub(super) fn check_ambient_initializer(
        &self,
        node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature*/
    ) -> bool {
        let initializer = node.as_has_initializer().maybe_initializer();
        if let Some(initializer) = initializer.as_ref() {
            let is_invalid_initializer = !(self
                .is_string_or_number_literal_expression(initializer)
                || self.is_simple_literal_enum_reference(initializer)
                || matches!(
                    initializer.kind(),
                    SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword
                )
                || self.is_big_int_literal_expression(initializer));
            let is_const_or_readonly = is_declaration_readonly(node)
                || is_variable_declaration(node) && is_var_const(node);
            if is_const_or_readonly && node.as_has_type().maybe_type().is_none() {
                if is_invalid_initializer {
                    return self.grammar_error_on_node(
                        initializer,
                        &Diagnostics::A_const_initializer_in_an_ambient_context_must_be_a_string_or_numeric_literal_or_literal_enum_reference,
                        None,
                    );
                }
            } else {
                return self.grammar_error_on_node(
                    initializer,
                    &Diagnostics::Initializers_are_not_allowed_in_ambient_contexts,
                    None,
                );
            }
            if !is_const_or_readonly || is_invalid_initializer {
                return self.grammar_error_on_node(
                    initializer,
                    &Diagnostics::Initializers_are_not_allowed_in_ambient_contexts,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn check_grammar_variable_declaration(
        &self,
        node: &Node, /*VariableDeclaration*/
    ) -> bool {
        let node_as_variable_declaration = node.as_variable_declaration();
        if !matches!(
            node.parent().parent().kind(),
            SyntaxKind::ForInStatement | SyntaxKind::ForOfStatement
        ) {
            if node.flags().intersects(NodeFlags::Ambient) {
                self.check_ambient_initializer(node);
            } else if node_as_variable_declaration.maybe_initializer().is_none() {
                if is_binding_pattern(node_as_variable_declaration.maybe_name())
                    && !is_binding_pattern(node.maybe_parent())
                {
                    return self.grammar_error_on_node(
                        node,
                        &Diagnostics::A_destructuring_declaration_must_have_an_initializer,
                        None,
                    );
                }
                if is_var_const(node) {
                    return self.grammar_error_on_node(
                        node,
                        &Diagnostics::const_declarations_must_be_initialized,
                        None,
                    );
                }
            }
        }

        if node_as_variable_declaration.exclamation_token.is_some()
            && (node.parent().parent().kind() != SyntaxKind::VariableStatement
                || node_as_variable_declaration.maybe_type().is_none()
                || node_as_variable_declaration.maybe_initializer().is_some()
                || node.flags().intersects(NodeFlags::Ambient))
        {
            let message = if node_as_variable_declaration.maybe_initializer().is_some() {
                &*Diagnostics::Declarations_with_initializers_cannot_also_have_definite_assignment_assertions
            } else if node_as_variable_declaration.maybe_type().is_none() {
                &*Diagnostics::Declarations_with_definite_assignment_assertions_must_also_have_type_annotations
            } else {
                &*Diagnostics::A_definite_assignment_assertion_is_not_permitted_in_this_context
            };
            return self.grammar_error_on_node(
                node_as_variable_declaration
                    .exclamation_token
                    .as_ref()
                    .unwrap(),
                message,
                None,
            );
        }

        if (self.module_kind < ModuleKind::ES2015
            || get_source_file_of_node(Some(node))
                .unwrap()
                .as_source_file()
                .maybe_implied_node_format()
                == Some(ModuleKind::CommonJS))
            && self.module_kind != ModuleKind::System
            && !node
                .parent()
                .parent()
                .flags()
                .intersects(NodeFlags::Ambient)
            && has_syntactic_modifier(&node.parent().parent(), ModifierFlags::Export)
        {
            self.check_es_module_marker(&node_as_variable_declaration.name());
        }

        let check_let_const_names = is_let(node) || is_var_const(node);

        check_let_const_names
            && self.check_grammar_name_in_let_or_const_declarations(
                &node_as_variable_declaration.name(),
            )
    }
}
