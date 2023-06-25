use std::io;

use gc::Gc;

use super::TransformSystemModule;
use crate::{
    first_or_undefined, get_external_module_name_literal, get_node_id, is_array_literal_expression,
    is_assignment_expression, is_declaration_name_of_enum_or_namespace, is_expression,
    is_generated_identifier, is_identifier, is_local_name, is_object_literal_expression,
    is_property_assignment, is_shorthand_property_assignment, is_spread_element, is_string_literal,
    try_flatten_destructuring_assignment, try_maybe_visit_node, try_some, try_visit_each_child,
    try_visit_node, FlattenLevel, GetOrInsertDefault, HasInitializerInterface,
    LiteralLikeNodeInterface, MapOrDefault, Matches, NamedDeclarationInterface, Node, NodeArray,
    NodeExt, NodeInterface, ReadonlyTextRange, SyntaxKind, VisitResult, _d,
    is_prefix_unary_expression,
};

impl TransformSystemModule {
    pub(super) fn visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/ {
        self.visitor_worker(node, false)
    }

    pub(super) fn discarded_value_visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/
    {
        self.visitor_worker(node, true)
    }

    pub(super) fn visit_expression_statement(
        &self,
        node: &Node, /*ExpressionStatement*/
    ) -> io::Result<VisitResult> {
        let node_as_expression_statement = node.as_expression_statement();
        Ok(Some(
            self.factory
                .update_expression_statement(
                    node,
                    try_visit_node(
                        &node_as_expression_statement.expression,
                        Some(|node: &Node| self.discarded_value_visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> {
        let node_as_parenthesized_expression = node.as_parenthesized_expression();
        Ok(Some(
            self.factory
                .update_parenthesized_expression(
                    node,
                    try_visit_node(
                        &node_as_parenthesized_expression.expression,
                        Some(|node: &Node| {
                            if value_is_discarded {
                                self.discarded_value_visitor(node)
                            } else {
                                self.visitor(node)
                            }
                        }),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_partially_emitted_expression(
        &self,
        node: &Node, /*PartiallyEmittedExpression*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> {
        let node_as_partially_emitted_expression = node.as_partially_emitted_expression();
        Ok(Some(
            self.factory
                .update_partially_emitted_expression(
                    node,
                    try_visit_node(
                        &node_as_partially_emitted_expression.expression,
                        Some(|node: &Node| {
                            if value_is_discarded {
                                self.discarded_value_visitor(node)
                            } else {
                                self.visitor(node)
                            }
                        }),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_import_call_expression(
        &self,
        node: &Node, /*ImportCall*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let node_as_call_expression = node.as_call_expression();
        let external_module_name = get_external_module_name_literal(
            &self.factory,
            node,
            &self.current_source_file(),
            &**self.host,
            &**self.resolver,
            &self.compiler_options,
        );
        let first_argument = try_maybe_visit_node(
            first_or_undefined(&node_as_call_expression.arguments).cloned(),
            Some(|node: &Node| self.visitor(node)),
            Option::<fn(&Node) -> bool>::None,
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?;
        let argument = external_module_name
            .filter(|external_module_name| {
                !first_argument.as_ref().matches(|first_argument| {
                    is_string_literal(first_argument)
                        && &*first_argument.as_string_literal().text()
                            == &*external_module_name.as_string_literal().text()
                })
            })
            .or(first_argument);
        Ok(self.factory.create_call_expression(
            self.factory.create_property_access_expression(
                self.context_object(),
                self.factory.create_identifier("import"),
            ),
            Option::<Gc<NodeArray>>::None,
            Some(argument.map_or_default(|argument| vec![argument])),
        ))
    }

    pub(super) fn visit_destructuring_assignment(
        &self,
        node: &Node, /*DestructuringAssignment*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> /*<Expression>*/ {
        let node_as_binary_expression = node.as_binary_expression();
        if self.has_exported_reference_in_destructuring_target(&node_as_binary_expression.left)? {
            return Ok(Some(
                try_flatten_destructuring_assignment(
                    node,
                    Some(|node: &Node| self.visitor(node)),
                    &**self.context,
                    FlattenLevel::All,
                    Some(!value_is_discarded),
                    Option::<
                        fn(&Node, &Node, Option<&dyn ReadonlyTextRange>) -> io::Result<Gc<Node>>,
                    >::None,
                )?
                .into(),
            ));
        }

        Ok(Some(
            try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?.into(),
        ))
    }

    pub(super) fn has_exported_reference_in_destructuring_target(
        &self,
        node: &Node, /*Expression | ObjectLiteralElementLike*/
    ) -> io::Result<bool> {
        Ok(if is_assignment_expression(node, Some(true)) {
            self.has_exported_reference_in_destructuring_target(&node.as_binary_expression().left)?
        } else if is_spread_element(node) {
            self.has_exported_reference_in_destructuring_target(
                &node.as_spread_element().expression,
            )?
        } else if is_object_literal_expression(node) {
            try_some(
                Some(&node.as_object_literal_expression().properties),
                Some(|property: &Gc<Node>| {
                    self.has_exported_reference_in_destructuring_target(property)
                }),
            )?
        } else if is_array_literal_expression(node) {
            try_some(
                Some(&node.as_array_literal_expression().elements),
                Some(|element: &Gc<Node>| {
                    self.has_exported_reference_in_destructuring_target(element)
                }),
            )?
        } else if is_shorthand_property_assignment(node) {
            self.has_exported_reference_in_destructuring_target(
                &node.as_shorthand_property_assignment().name(),
            )?
        } else if is_property_assignment(node) {
            self.has_exported_reference_in_destructuring_target(
                &node.as_property_assignment().maybe_initializer().unwrap(),
            )?
        } else if is_identifier(node) {
            let container = self.resolver.get_referenced_export_container(node, None)?;
            container.matches(|container| container.kind() == SyntaxKind::SourceFile)
        } else {
            false
        })
    }

    pub(super) fn visit_prefix_or_postfix_unary_expression(
        &self,
        node: &Node, /*PrefixUnaryExpression | PostfixUnaryExpression*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> {
        let node_as_unary_expression = node.as_unary_expression();
        let node_operand = &node_as_unary_expression.operand();
        if matches!(
            node_as_unary_expression.operator(),
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ) && is_identifier(node_operand)
            && !is_generated_identifier(node_operand)
            && !is_local_name(node_operand)
            && !is_declaration_name_of_enum_or_namespace(node_operand)
        {
            let exported_names = self.get_exports(node_operand);
            if let Some(exported_names) = exported_names {
                let mut temp: Option<Gc<Node /*Identifier*/>> = _d();
                let mut expression/*: Expression*/ = try_visit_node(
                    &node_operand,
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )?;
                if is_prefix_unary_expression(node) {
                    expression = self
                        .factory
                        .update_prefix_unary_expression(node, expression);
                } else {
                    expression = self
                        .factory
                        .update_postfix_unary_expression(node, expression);
                    if !value_is_discarded {
                        temp = Some(self.factory.create_temp_variable(
                            Some(|node: &Node| {
                                self.context.hoist_variable_declaration(node);
                            }),
                            None,
                        ));
                        expression = self
                            .factory
                            .create_assignment(temp.clone().unwrap(), expression)
                            .set_text_range(Some(node));
                    }
                    expression = self
                        .factory
                        .create_comma(expression, self.factory.clone_node(node_operand))
                        .set_text_range(Some(node));
                }

                for export_name in &exported_names {
                    expression = self.create_export_expression(
                        export_name,
                        &self.prevent_substitution(expression),
                    );
                }

                if let Some(temp) = temp {
                    expression = self
                        .factory
                        .create_comma(expression, temp)
                        .set_text_range(Some(node));
                }

                return Ok(Some(expression.into()));
            }
        }
        Ok(Some(
            try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?.into(),
        ))
    }

    pub(super) fn modifier_visitor(&self, node: &Node /*FunctionDeclaration*/) -> VisitResult /*<Node>*/
    {
        match node.kind() {
            SyntaxKind::ExportKeyword | SyntaxKind::DefaultKeyword => return None,
            _ => (),
        }
        Some(node.node_wrapper().into())
    }

    pub(super) fn get_exports(&self, _name: &Node /*Identifier*/) -> Option<Vec<Gc<Node>>> {
        unimplemented!()
    }

    pub(super) fn prevent_substitution(&self, node: Gc<Node>) -> Gc<Node> {
        self.maybe_no_substitution_mut()
            .get_or_insert_default_()
            .insert(get_node_id(&node), true);
        node
    }
}
