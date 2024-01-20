use std::io;

use gc::Gc;
use id_arena::Id;

use super::TransformSystemModule;
use crate::{
    first_or_undefined, get_external_module_name_literal, get_node_id, is_array_literal_expression,
    is_assignment_expression, is_declaration_name_of_enum_or_namespace, is_expression,
    is_generated_identifier, is_identifier, is_local_name, is_object_literal_expression,
    is_property_assignment, is_shorthand_property_assignment, is_spread_element, is_string_literal,
    try_flatten_destructuring_assignment, try_maybe_visit_node, try_some, try_visit_each_child,
    try_visit_node, FlattenLevel, GetOrInsertDefault, HasInitializerInterface,
    LiteralLikeNodeInterface, MapOrDefault, Matches, NamedDeclarationInterface, Node, NodeArray,
    NodeExt, NodeInterface, ReadonlyTextRange, SyntaxKind, VisitResult, _d, get_original_node_id,
    is_prefix_unary_expression, NonEmpty, OptionTry,
};

impl TransformSystemModule {
    pub(super) fn visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/ {
        self.visitor_worker(node, false)
    }

    pub(super) fn discarded_value_visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/
    {
        self.visitor_worker(node, true)
    }

    pub(super) fn visit_expression_statement(
        &self,
        node: Id<Node>, /*ExpressionStatement*/
    ) -> io::Result<VisitResult> {
        let node_as_expression_statement = node.as_expression_statement();
        Ok(Some(
            self.factory
                .update_expression_statement(
                    node,
                    try_visit_node(
                        &node_as_expression_statement.expression,
                        Some(|node: Id<Node>| self.discarded_value_visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_parenthesized_expression(
        &self,
        node: Id<Node>, /*ParenthesizedExpression*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> {
        let node_as_parenthesized_expression = node.as_parenthesized_expression();
        Ok(Some(
            self.factory
                .update_parenthesized_expression(
                    node,
                    try_visit_node(
                        &node_as_parenthesized_expression.expression,
                        Some(|node: Id<Node>| {
                            if value_is_discarded {
                                self.discarded_value_visitor(node)
                            } else {
                                self.visitor(node)
                            }
                        }),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_partially_emitted_expression(
        &self,
        node: Id<Node>, /*PartiallyEmittedExpression*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> {
        let node_as_partially_emitted_expression = node.as_partially_emitted_expression();
        Ok(Some(
            self.factory
                .update_partially_emitted_expression(
                    node,
                    try_visit_node(
                        &node_as_partially_emitted_expression.expression,
                        Some(|node: Id<Node>| {
                            if value_is_discarded {
                                self.discarded_value_visitor(node)
                            } else {
                                self.visitor(node)
                            }
                        }),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_import_call_expression(
        &self,
        node: Id<Node>, /*ImportCall*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_as_call_expression = node.as_call_expression();
        let external_module_name = get_external_module_name_literal(
            &self.factory,
            node,
            &self.current_source_file(),
            &**self.host,
            &**self.resolver,
            &self.compiler_options,
        )?;
        let first_argument = try_maybe_visit_node(
            first_or_undefined(&node_as_call_expression.arguments).cloned(),
            Some(|node: Id<Node>| self.visitor(node)),
            Option::<fn(Id<Node>) -> bool>::None,
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
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
        node: Id<Node>, /*DestructuringAssignment*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> /*<Expression>*/ {
        let node_as_binary_expression = node.as_binary_expression();
        if self.has_exported_reference_in_destructuring_target(&node_as_binary_expression.left)? {
            return Ok(Some(
                try_flatten_destructuring_assignment(
                    node,
                    Some(|node: Id<Node>| self.visitor(node)),
                    self.context.clone(),
                    FlattenLevel::All,
                    Some(!value_is_discarded),
                    Option::<
                        fn(
                            Id<Node>,
                            Id<Node>,
                            Option<&dyn ReadonlyTextRange>,
                        ) -> io::Result<Id<Node>>,
                    >::None,
                    self,
                )?
                .into(),
            ));
        }

        Ok(Some(
            try_visit_each_child(&node.ref_(self), |node: Id<Node>| self.visitor(node), &**self.context)?
                .into(),
        ))
    }

    pub(super) fn has_exported_reference_in_destructuring_target(
        &self,
        node: Id<Node>, /*Expression | ObjectLiteralElementLike*/
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
                Some(|property: &Id<Node>| {
                    self.has_exported_reference_in_destructuring_target(property)
                }),
            )?
        } else if is_array_literal_expression(node) {
            try_some(
                Some(&node.as_array_literal_expression().elements),
                Some(|element: &Id<Node>| {
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
        node: Id<Node>, /*PrefixUnaryExpression | PostfixUnaryExpression*/
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
            let exported_names = self.get_exports(node_operand)?;
            if let Some(exported_names) = exported_names {
                let mut temp: Option<Id<Node /*Identifier*/>> = _d();
                let mut expression/*: Expression*/ = try_visit_node(
                    &node_operand,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
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
                            Some(|node: Id<Node>| {
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
            try_visit_each_child(&node.ref_(self), |node: Id<Node>| self.visitor(node), &**self.context)?
                .into(),
        ))
    }

    pub(super) fn modifier_visitor(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> VisitResult /*<Node>*/ {
        match node.kind() {
            SyntaxKind::ExportKeyword | SyntaxKind::DefaultKeyword => return None,
            _ => (),
        }
        Some(node.node_wrapper().into())
    }

    pub(super) fn get_exports(
        &self,
        name: Id<Node>, /*Identifier*/
    ) -> io::Result<Option<Vec<Id<Node>>>> {
        let mut exported_names: Option<Vec<Id<Node /*Identifier*/>>> = _d();
        if !is_generated_identifier(name) {
            let value_declaration = self
                .resolver
                .get_referenced_import_declaration(name)?
                .try_or_else(|| self.resolver.get_referenced_value_declaration(name))?;

            if let Some(value_declaration) = value_declaration {
                let export_container = self
                    .resolver
                    .get_referenced_export_container(name, Some(false))?;
                if export_container
                    .matches(|export_container| export_container.kind() == SyntaxKind::SourceFile)
                {
                    exported_names.get_or_insert_default_().push(
                        self.factory
                            .get_declaration_name(Some(&*value_declaration), None, None),
                    );
                }

                if let Some(value) = self
                    .maybe_module_info()
                    .as_ref()
                    .and_then(|module_info| {
                        module_info
                            .exported_bindings
                            .get(&get_original_node_id(&value_declaration))
                    })
                    .non_empty()
                {
                    exported_names
                        .get_or_insert_default_()
                        .extend(value.into_iter().cloned());
                }
            }
        }

        Ok(exported_names)
    }

    pub(super) fn prevent_substitution(&self, node: Id<Node>) -> Id<Node> {
        self.maybe_no_substitution_mut()
            .get_or_insert_default_()
            .insert(get_node_id(&node), true);
        node
    }
}
