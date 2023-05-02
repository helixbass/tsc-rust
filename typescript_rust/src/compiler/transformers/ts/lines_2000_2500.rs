use std::ptr;

use gc::Gc;

use crate::{
    flatten_destructuring_assignment, get_initialized_variables,
    get_leading_comment_ranges_of_node, has_syntactic_modifier, is_assertion_expression,
    is_binding_name, is_binding_pattern, is_expression, is_modifier, map,
    move_range_past_decorators, move_range_past_modifiers, node_is_missing,
    parameter_is_this_keyword, set_comment_range, set_emit_flags, set_source_map_range,
    set_text_range, skip_outer_expressions, visit_each_child, visit_function_body, visit_node,
    visit_nodes, visit_parameter_list, EmitFlags, FlattenLevel, FunctionLikeDeclarationInterface,
    HasInitializerInterface, ModifierFlags, NamedDeclarationInterface, Node, NodeArray, NodeExt,
    NodeInterface, NonEmpty, OuterExpressionKinds, ReadonlyTextRange,
    SignatureDeclarationInterface, VisitResult,
};

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn visit_method_declaration(
        &self,
        node: &Node, /*MethodDeclaration*/
    ) -> VisitResult {
        let node_as_method_declaration = node.as_method_declaration();
        if !self.should_emit_function_like_declaration(node) {
            return None;
        }
        let updated = self.factory.update_method_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.modifier_visitor(node)),
                Some(is_modifier),
                None,
                None,
            ),
            node_as_method_declaration.maybe_asterisk_token(),
            self.visit_property_name_of_class_element(node),
            None,
            Option::<Gc<NodeArray>>::None,
            visit_parameter_list(
                Some(&node_as_method_declaration.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            visit_function_body(
                node_as_method_declaration.maybe_body().as_deref(),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            ),
        );
        if !ptr::eq(&*updated, node) {
            set_comment_range(&updated, node);
            set_source_map_range(
                updated.clone(),
                Some((&move_range_past_decorators(node)).into()),
            );
        }
        Some(updated.into())
    }

    pub(super) fn should_emit_accessor_declaration(
        &self,
        node: &Node, /*AccessorDeclaration*/
    ) -> bool {
        !(node_is_missing(node.as_function_like_declaration().maybe_body())
            && has_syntactic_modifier(node, ModifierFlags::Abstract))
    }

    pub(super) fn visit_get_accessor(
        &self,
        node: &Node, /*GetAccessorDeclaration*/
    ) -> VisitResult {
        let node_as_get_accessor_declaration = node.as_get_accessor_declaration();
        if !self.should_emit_accessor_declaration(node) {
            return None;
        }
        let updated = self.factory.update_get_accessor_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.modifier_visitor(node)),
                Some(is_modifier),
                None,
                None,
            ),
            self.visit_property_name_of_class_element(node),
            visit_parameter_list(
                Some(&node_as_get_accessor_declaration.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            Some(
                visit_function_body(
                    node_as_get_accessor_declaration.maybe_body().as_deref(),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
                )
                .unwrap_or_else(|| self.factory.create_block(vec![], None).wrap()),
            ),
        );
        if !ptr::eq(&*updated, node) {
            set_comment_range(&updated, node);
            set_source_map_range(
                updated.clone(),
                Some((&move_range_past_decorators(node)).into()),
            );
        }
        Some(updated.into())
    }

    pub(super) fn visit_set_accessor(
        &self,
        node: &Node, /*SetAccessorDeclaration*/
    ) -> VisitResult {
        let node_as_set_accessor_declaration = node.as_set_accessor_declaration();
        if !self.should_emit_accessor_declaration(node) {
            return None;
        }
        let updated = self.factory.update_set_accessor_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.modifier_visitor(node)),
                Some(is_modifier),
                None,
                None,
            ),
            self.visit_property_name_of_class_element(node),
            visit_parameter_list(
                Some(&node_as_set_accessor_declaration.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            Some(
                visit_function_body(
                    node_as_set_accessor_declaration.maybe_body().as_deref(),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
                )
                .unwrap_or_else(|| self.factory.create_block(vec![], None).wrap()),
            ),
        );
        if !ptr::eq(&*updated, node) {
            set_comment_range(&updated, node);
            set_source_map_range(
                updated.clone(),
                Some((&move_range_past_decorators(node)).into()),
            );
        }
        Some(updated.into())
    }

    pub(super) fn visit_function_declaration(
        &self,
        node: &Node, /*FunctionDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_function_declaration = node.as_function_declaration();
        if !self.should_emit_function_like_declaration(node) {
            return Some(
                self.factory
                    .create_not_emitted_statement(node.node_wrapper())
                    .into(),
            );
        }
        let updated = self.factory.update_function_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.modifier_visitor(node)),
                Some(is_modifier),
                None,
                None,
            ),
            node_as_function_declaration.maybe_asterisk_token(),
            node_as_function_declaration.maybe_name(),
            Option::<Gc<NodeArray>>::None,
            visit_parameter_list(
                Some(&node_as_function_declaration.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            Some(
                visit_function_body(
                    node_as_function_declaration.maybe_body().as_deref(),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
                )
                .unwrap_or_else(|| self.factory.create_block(vec![], None).wrap()),
            ),
        );
        if self.is_export_of_namespace(node) {
            let mut statements: Vec<Gc<Node /*Statement*/>> = vec![updated];
            self.add_export_member_assignment(&mut statements, node);
            return Some(statements.into());
        }
        Some(updated.into())
    }

    pub(super) fn visit_function_expression(
        &self,
        node: &Node, /*FunctionExpression*/
    ) -> Gc<Node /*Expression*/> {
        let node_as_function_expression = node.as_function_expression();
        if !self.should_emit_function_like_declaration(node) {
            return self.factory.create_omitted_expression().wrap();
        }
        let updated = self.factory.update_function_expression(
            node,
            visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.modifier_visitor(node)),
                Some(is_modifier),
                None,
                None,
            ),
            node_as_function_expression.maybe_asterisk_token(),
            node_as_function_expression.maybe_name(),
            Option::<Gc<NodeArray>>::None,
            visit_parameter_list(
                Some(&node_as_function_expression.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            visit_function_body(
                node_as_function_expression.maybe_body().as_deref(),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
            .unwrap_or_else(|| self.factory.create_block(vec![], None).wrap()),
        );
        updated
    }

    pub(super) fn visit_arrow_function(&self, node: &Node /*ArrowFunction*/) -> VisitResult {
        let node_as_arrow_function = node.as_arrow_function();
        let updated = self.factory.update_arrow_function(
            node,
            visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.modifier_visitor(node)),
                Some(is_modifier),
                None,
                None,
            ),
            Option::<Gc<NodeArray>>::None,
            visit_parameter_list(
                Some(&node_as_arrow_function.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            node_as_arrow_function.equals_greater_than_token.clone(),
            visit_function_body(
                node_as_arrow_function.maybe_body().as_deref(),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
            .unwrap(),
        );
        Some(updated.into())
    }

    pub(super) fn visit_parameter(&self, node: &Node /*ParameterDeclaration*/) -> VisitResult {
        let node_as_parameter_declaration = node.as_parameter_declaration();
        if parameter_is_this_keyword(node) {
            return None;
        }

        let updated = self.factory.update_parameter_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            node_as_parameter_declaration.dot_dot_dot_token.clone(),
            visit_node(
                node_as_parameter_declaration.maybe_name(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_binding_name),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            ),
            None,
            None,
            visit_node(
                node_as_parameter_declaration.maybe_initializer(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            ),
        );
        if !ptr::eq(&*updated, node) {
            set_comment_range(&updated, node);
            set_text_range(
                &*updated,
                Some(&move_range_past_modifiers(node).into_readonly_text_range()),
            );
            set_source_map_range(
                updated.clone(),
                Some((&move_range_past_modifiers(node)).into()),
            );
            set_emit_flags(
                updated.as_parameter_declaration().name(),
                EmitFlags::NoTrailingSourceMap,
            );
        }
        Some(updated.into())
    }

    pub(super) fn visit_variable_statement(
        &self,
        node: &Node, /*VariableStatement*/
    ) -> Option<Gc<Node /*Statement*/>> {
        let node_as_variable_statement = node.as_variable_statement();
        if self.is_export_of_namespace(node) {
            let variables = get_initialized_variables(&node_as_variable_statement.declaration_list);
            if variables.is_empty() {
                return None;
            }

            Some(
                self.factory
                    .create_expression_statement(
                        self.factory
                            .inline_expressions(&map(&variables, |variable: &Gc<Node>, _| {
                                self.transform_initialized_variable(variable)
                            })),
                    )
                    .wrap()
                    .set_text_range(Some(node)),
            )
        } else {
            visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
                Option::<fn(&Node) -> VisitResult>::None,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
        }
    }

    pub(super) fn transform_initialized_variable(
        &self,
        node: &Node, /*InitializedVariableDeclaration*/
    ) -> Gc<Node /*Expression*/> {
        let node_as_variable_declaration = node.as_variable_declaration();
        let ref name = node_as_variable_declaration.name();
        if is_binding_pattern(Some(&**name)) {
            flatten_destructuring_assignment(
                node,
                Some(|node: &Node| self.visitor(node)),
                &**self.context,
                FlattenLevel::All,
                Some(false),
                Some(
                    |export_name: &Node,
                     export_value: &Node,
                     location: Option<&dyn ReadonlyTextRange>| {
                        self.create_namespace_export_expression(export_name, export_value, location)
                    },
                ),
            )
        } else {
            self.factory
                .create_assignment(
                    self.get_namespace_member_name_with_source_maps_and_without_comments(name),
                    visit_node(
                        node_as_variable_declaration.maybe_initializer(),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )
                    .unwrap(),
                )
                .wrap()
                .set_text_range(Some(node))
        }
    }

    pub(super) fn visit_variable_declaration(
        &self,
        node: &Node, /*VariableDeclaration*/
    ) -> VisitResult {
        let node_as_variable_declaration = node.as_variable_declaration();
        Some(
            self.factory
                .update_variable_declaration(
                    node,
                    visit_node(
                        node_as_variable_declaration.maybe_name(),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_binding_name),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    ),
                    None,
                    None,
                    visit_node(
                        node_as_variable_declaration.maybe_initializer(),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    ),
                )
                .into(),
        )
    }

    pub(super) fn visit_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
    ) -> Gc<Node /*Expression*/> {
        let node_as_parenthesized_expression = node.as_parenthesized_expression();
        let ref inner_expression = skip_outer_expressions(
            &node_as_parenthesized_expression.expression,
            Some(!OuterExpressionKinds::Assertions),
        );
        if is_assertion_expression(inner_expression) {
            let expression = visit_node(
                Some(&*node_as_parenthesized_expression.expression),
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )
            .unwrap();

            if get_leading_comment_ranges_of_node(&expression, &self.current_source_file())
                .is_non_empty()
            {
                return self
                    .factory
                    .update_parenthesized_expression(node, expression);
            }
            return self
                .factory
                .create_partially_emitted_expression(expression, Some(node.node_wrapper()))
                .wrap();
        }

        visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
            Option::<
                fn(
                    Option<&NodeArray>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<usize>,
                    Option<usize>,
                ) -> Option<Gc<NodeArray>>,
            >::None,
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .unwrap()
    }

    pub(super) fn visit_assertion_expression(
        &self,
        _node: &Node, /*AssertionExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_non_null_expression(
        &self,
        _node: &Node, /*NonNullExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_call_expression(
        &self,
        _node: &Node, /*CallExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_new_expression(&self, _node: &Node /*NewExpression*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_tagged_template_expression(
        &self,
        _node: &Node, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_jsx_self_closing_element(
        &self,
        _node: &Node, /*JsxSelfClosingElement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_jsx_jsx_opening_element(
        &self,
        _node: &Node, /*JsxOpeningElement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_enum_declaration(
        &self,
        _node: &Node, /*EnumDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }
}
