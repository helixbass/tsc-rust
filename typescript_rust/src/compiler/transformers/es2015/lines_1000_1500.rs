use std::{borrow::Borrow, io};

use gc::Gc;

use super::TransformES2015;
use crate::{
    flatten_destructuring_binding, get_emit_flags, insert_statement_after_custom_prologue,
    is_binding_pattern, is_expression, last_or_undefined, some, try_flatten_destructuring_binding,
    try_visit_node, EmitFlags, FlattenLevel, GeneratedIdentifierFlags, HasInitializerInterface,
    Matches, NamedDeclarationInterface, Node, NodeArray, NodeExt, NodeInterface, SyntaxKind,
};

impl TransformES2015 {
    pub(super) fn is_sufficiently_covered_by_return_statements(
        &self,
        statement: &Node, /*Statement*/
    ) -> bool {
        if statement.kind() == SyntaxKind::ReturnStatement {
            return true;
        } else if statement.kind() == SyntaxKind::IfStatement {
            let if_statement = statement.as_if_statement();
            if let Some(if_statement_else_statement) = if_statement.else_statement.as_ref() {
                return self
                    .is_sufficiently_covered_by_return_statements(&if_statement.then_statement)
                    && self.is_sufficiently_covered_by_return_statements(
                        &if_statement_else_statement,
                    );
            }
        } else if statement.kind() == SyntaxKind::Block {
            let last_statement = last_or_undefined(&statement.as_block().statements);
            if last_statement.matches(|last_statement| {
                self.is_sufficiently_covered_by_return_statements(last_statement)
            }) {
                return true;
            }
        }

        false
    }

    pub(super) fn create_actual_this(&self) -> Gc<Node> {
        self.factory
            .create_this()
            .wrap()
            .set_emit_flags(EmitFlags::NoSubstitution)
    }

    pub(super) fn create_default_super_call_or_this(&self) -> Gc<Node> {
        self.factory
            .create_logical_or(
                self.factory
                    .create_logical_and(
                        self.factory
                            .create_strict_inequality(
                                self.factory.create_unique_name(
                                    "_super",
                                    Some(
                                        GeneratedIdentifierFlags::Optimistic
                                            | GeneratedIdentifierFlags::FileLevel,
                                    ),
                                ),
                                self.factory.create_null().wrap(),
                            )
                            .wrap(),
                        self.factory.create_function_apply_call(
                            self.factory.create_unique_name(
                                "_super",
                                Some(
                                    GeneratedIdentifierFlags::Optimistic
                                        | GeneratedIdentifierFlags::FileLevel,
                                ),
                            ),
                            self.create_actual_this(),
                            self.factory
                                .create_identifier("arguments", Option::<Gc<NodeArray>>::None, None)
                                .wrap(),
                        ),
                    )
                    .wrap(),
                self.create_actual_this(),
            )
            .wrap()
    }

    pub(super) fn visit_parameter(
        &self,
        node: &Node, /*ParameterDeclaration*/
    ) -> Option<Gc<Node /*ParameterDeclaration*/>> {
        let node_as_parameter_declaration = node.as_parameter_declaration();
        if node_as_parameter_declaration.dot_dot_dot_token.is_some() {
            None
        } else if is_binding_pattern(node_as_parameter_declaration.maybe_name()) {
            Some(
                self.factory
                    .create_parameter_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        None,
                        Some(self.factory.get_generated_name_for_node(Some(node), None)),
                        None,
                        None,
                        None,
                    )
                    .wrap()
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper())),
            )
        } else if node_as_parameter_declaration.maybe_initializer().is_some() {
            Some(
                self.factory
                    .create_parameter_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        None,
                        node_as_parameter_declaration.maybe_name(),
                        None,
                        None,
                        None,
                    )
                    .wrap()
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper())),
            )
        } else {
            Some(node.node_wrapper())
        }
    }

    pub(super) fn has_default_value_or_binding_pattern(
        &self,
        node: &Node, /*ParameterDeclaration*/
    ) -> bool {
        let node_as_parameter_declaration = node.as_parameter_declaration();
        node_as_parameter_declaration.maybe_initializer().is_some()
            || is_binding_pattern(node_as_parameter_declaration.maybe_name())
    }

    pub(super) fn add_default_value_assignments_if_needed(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> io::Result<bool> {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        if !some(
            Some(&node_as_function_like_declaration.parameters()),
            Some(|parameter: &Gc<Node>| self.has_default_value_or_binding_pattern(parameter)),
        ) {
            return Ok(false);
        }

        let mut added = false;
        for parameter in &node_as_function_like_declaration.parameters() {
            let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
            let ref name = parameter_as_parameter_declaration.maybe_name().unwrap();
            let initializer = parameter_as_parameter_declaration.maybe_initializer();
            let dot_dot_dot_token = parameter_as_parameter_declaration
                .dot_dot_dot_token
                .as_ref();

            if dot_dot_dot_token.is_some() {
                continue;
            }

            if is_binding_pattern(Some(&**name)) {
                added = self.insert_default_value_assignment_for_binding_pattern(
                    statements,
                    parameter,
                    name,
                    initializer,
                )?;
            } else if let Some(initializer) = initializer.as_ref() {
                self.insert_default_value_assignment_for_initializer(
                    statements,
                    parameter,
                    name,
                    initializer,
                )?;
                added = true;
            }
        }
        Ok(added)
    }

    pub(super) fn insert_default_value_assignment_for_binding_pattern(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        parameter: &Node,                       /*ParameterDeclaration*/
        name: &Node,                            /*BindingPattern*/
        initializer: Option<impl Borrow<Node>>, /*Expression*/
    ) -> io::Result<bool> {
        let name_as_has_elements = name.as_has_elements();
        if !name_as_has_elements.elements().is_empty() {
            insert_statement_after_custom_prologue(
                statements,
                Some(
                    self.factory
                        .create_variable_statement(
                            Option::<Gc<NodeArray>>::None,
                            self.factory
                                .create_variable_declaration_list(
                                    try_flatten_destructuring_binding(
                                        parameter,
                                        |node: &Node| self.visitor(node),
                                        &**self.context,
                                        FlattenLevel::All,
                                        Some(
                                            self.factory
                                                .get_generated_name_for_node(Some(parameter), None),
                                        ),
                                        None,
                                        None,
                                    )?,
                                    None,
                                )
                                .wrap(),
                        )
                        .wrap()
                        .set_emit_flags(EmitFlags::CustomPrologue),
                ),
            );
            return Ok(true);
        } else if let Some(initializer) = initializer {
            insert_statement_after_custom_prologue(
                statements,
                Some(
                    self.factory
                        .create_expression_statement(
                            self.factory
                                .create_assignment(
                                    self.factory
                                        .get_generated_name_for_node(Some(parameter), None),
                                    try_visit_node(
                                        Some(initializer),
                                        Some(|node: &Node| self.visitor(node)),
                                        Some(is_expression),
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    )?
                                    .unwrap(),
                                )
                                .wrap(),
                        )
                        .wrap()
                        .set_emit_flags(EmitFlags::CustomPrologue),
                ),
            );
            return Ok(true);
        }
        Ok(false)
    }

    pub(super) fn insert_default_value_assignment_for_initializer(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        parameter: &Node,   /*ParameterDeclaration*/
        name: &Node,        /*Identifier*/
        initializer: &Node, /*Expression*/
    ) -> io::Result<()> {
        let initializer = try_visit_node(
            Some(initializer),
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?
        .unwrap();
        let statement = self
            .factory
            .create_if_statement(
                self.factory
                    .create_type_check(self.factory.clone_node(name), "undefined"),
                self.factory
                    .create_block(
                        vec![self
                            .factory
                            .create_expression_statement(
                                self.factory
                                    .create_assignment(
                                        self.factory
                                            .clone_node(name)
                                            .set_text_range(Some(name))
                                            .and_set_parent(name.maybe_parent())
                                            .set_emit_flags(EmitFlags::NoSourceMap),
                                        initializer.clone().set_emit_flags(
                                            EmitFlags::NoSourceMap
                                                | get_emit_flags(&initializer)
                                                | EmitFlags::NoComments,
                                        ),
                                    )
                                    .wrap()
                                    .set_text_range(Some(parameter))
                                    .set_emit_flags(EmitFlags::NoComments),
                            )
                            .wrap()],
                        None,
                    )
                    .wrap()
                    .set_text_range(Some(parameter))
                    .set_emit_flags(
                        EmitFlags::SingleLine
                            | EmitFlags::NoTrailingSourceMap
                            | EmitFlags::NoTokenSourceMaps
                            | EmitFlags::NoComments,
                    ),
                None,
            )
            .wrap()
            .start_on_new_line()
            .set_text_range(Some(parameter))
            .set_emit_flags(
                EmitFlags::NoTokenSourceMaps
                    | EmitFlags::NoTrailingSourceMap
                    | EmitFlags::CustomPrologue
                    | EmitFlags::NoComments,
            );
        insert_statement_after_custom_prologue(statements, Some(statement));

        Ok(())
    }

    pub(super) fn add_rest_parameter_if_needed(
        &self,
        _statements: &mut Vec<Gc<Node /*Statement*/>>,
        _node: &Node, /*FunctionLikeDeclaration*/
        _in_constructor_with_synthesized_super: bool,
    ) -> bool {
        unimplemented!()
    }
}
