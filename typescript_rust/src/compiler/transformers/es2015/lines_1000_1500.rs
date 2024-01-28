use std::{borrow::Borrow, io};

use gc::Gc;
use id_arena::Id;

use super::TransformES2015;
use crate::{
    get_emit_flags, insert_statement_after_custom_prologue,
    insert_statements_after_custom_prologue, is_binding_pattern, is_expression, last_or_undefined,
    some, try_flatten_destructuring_binding, try_visit_node, EmitFlags, FlattenLevel,
    GeneratedIdentifierFlags, HasInitializerInterface, Matches, NamedDeclarationInterface, Node,
    NodeArray, NodeExt, NodeInterface, Number, SyntaxKind,
    HasArena, InArena, OptionInArena,
};

impl TransformES2015 {
    pub(super) fn is_sufficiently_covered_by_return_statements(
        &self,
        statement: Id<Node>, /*Statement*/
    ) -> bool {
        if statement.ref_(self).kind() == SyntaxKind::ReturnStatement {
            return true;
        } else if statement.ref_(self).kind() == SyntaxKind::IfStatement {
            let statement_ref = statement.ref_(self);
            let if_statement = statement_ref.as_if_statement();
            if let Some(if_statement_else_statement) = if_statement.else_statement {
                return self
                    .is_sufficiently_covered_by_return_statements(if_statement.then_statement)
                    && self.is_sufficiently_covered_by_return_statements(
                        if_statement_else_statement,
                    );
            }
        } else if statement.ref_(self).kind() == SyntaxKind::Block {
            let last_statement = last_or_undefined(&statement.ref_(self).as_block().statements.ref_(self)).copied();
            if last_statement.matches(|last_statement| {
                self.is_sufficiently_covered_by_return_statements(last_statement)
            }) {
                return true;
            }
        }

        false
    }

    pub(super) fn create_actual_this(&self) -> Id<Node> {
        self.factory
            .ref_(self).create_this()
            .set_emit_flags(EmitFlags::NoSubstitution, self)
    }

    pub(super) fn create_default_super_call_or_this(&self) -> Id<Node> {
        self.factory.ref_(self).create_logical_or(
            self.factory.ref_(self).create_logical_and(
                self.factory.ref_(self).create_strict_inequality(
                    self.factory.ref_(self).create_unique_name(
                        "_super",
                        Some(
                            GeneratedIdentifierFlags::Optimistic
                                | GeneratedIdentifierFlags::FileLevel,
                        ),
                    ),
                    self.factory.ref_(self).create_null(),
                ),
                self.factory.ref_(self).create_function_apply_call(
                    self.factory.ref_(self).create_unique_name(
                        "_super",
                        Some(
                            GeneratedIdentifierFlags::Optimistic
                                | GeneratedIdentifierFlags::FileLevel,
                        ),
                    ),
                    self.create_actual_this(),
                    self.factory.ref_(self).create_identifier("arguments"),
                ),
            ),
            self.create_actual_this(),
        )
    }

    pub(super) fn visit_parameter(
        &self,
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> Option<Id<Node /*ParameterDeclaration*/>> {
        let node_ref = node.ref_(self);
        let node_as_parameter_declaration = node_ref.as_parameter_declaration();
        if node_as_parameter_declaration.dot_dot_dot_token.is_some() {
            None
        } else if is_binding_pattern(node_as_parameter_declaration.maybe_name().refed(self).as_deref()) {
            Some(
                self.factory
                    .ref_(self).create_parameter_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        None,
                        Some(self.factory.ref_(self).get_generated_name_for_node(Some(node), None)),
                        None,
                        None,
                        None,
                    )
                    .set_text_range(Some(&*node.ref_(self)), self)
                    .set_original_node(Some(node), self),
            )
        } else if node_as_parameter_declaration.maybe_initializer().is_some() {
            Some(
                self.factory
                    .ref_(self).create_parameter_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        None,
                        node_as_parameter_declaration.maybe_name(),
                        None,
                        None,
                        None,
                    )
                    .set_text_range(Some(&*node.ref_(self)), self)
                    .set_original_node(Some(node), self),
            )
        } else {
            Some(node)
        }
    }

    pub(super) fn has_default_value_or_binding_pattern(
        &self,
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> bool {
        let node_ref = node.ref_(self);
        let node_as_parameter_declaration = node_ref.as_parameter_declaration();
        node_as_parameter_declaration.maybe_initializer().is_some()
            || is_binding_pattern(node_as_parameter_declaration.maybe_name().refed(self).as_deref())
    }

    pub(super) fn add_default_value_assignments_if_needed(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        node: Id<Node>, /*FunctionLikeDeclaration*/
    ) -> io::Result<bool> {
        let node_ref = node.ref_(self);
        let node_as_function_like_declaration = node_ref.as_function_like_declaration();
        if !some(
            Some(&*node_as_function_like_declaration.parameters().ref_(self)),
            Some(|&parameter: &Id<Node>| self.has_default_value_or_binding_pattern(parameter)),
        ) {
            return Ok(false);
        }

        let mut added = false;
        for &parameter in &*node_as_function_like_declaration.parameters().ref_(self) {
            let parameter_ref = parameter.ref_(self);
            let parameter_as_parameter_declaration = parameter_ref.as_parameter_declaration();
            let name = parameter_as_parameter_declaration.maybe_name().unwrap();
            let initializer = parameter_as_parameter_declaration.maybe_initializer();
            let dot_dot_dot_token = parameter_as_parameter_declaration
                .dot_dot_dot_token
                .as_ref();

            if dot_dot_dot_token.is_some() {
                continue;
            }

            if is_binding_pattern(Some(&*name.ref_(self))) {
                added = self.insert_default_value_assignment_for_binding_pattern(
                    statements,
                    parameter,
                    name,
                    initializer,
                )?;
            } else if let Some(initializer) = initializer {
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
        statements: &mut Vec<Id<Node /*Statement*/>>,
        parameter: Id<Node>,           /*ParameterDeclaration*/
        name: Id<Node>,                /*BindingPattern*/
        initializer: Option<Id<Node>>, /*Expression*/
    ) -> io::Result<bool> {
        let name_ref = name.ref_(self);
        let name_as_has_elements = name_ref.as_has_elements();
        if !name_as_has_elements.elements().ref_(self).is_empty() {
            insert_statement_after_custom_prologue(
                statements,
                Some(
                    self.factory
                        .ref_(self).create_variable_statement(
                            Option::<Id<NodeArray>>::None,
                            self.factory.ref_(self).create_variable_declaration_list(
                                try_flatten_destructuring_binding(
                                    parameter,
                                    |node: Id<Node>| self.visitor(node),
                                    self.context.clone(),
                                    FlattenLevel::All,
                                    Some(
                                        self.factory
                                            .ref_(self).get_generated_name_for_node(Some(parameter), None),
                                    ),
                                    None,
                                    None,
                                    self,
                                )?,
                                None,
                            ),
                        )
                        .set_emit_flags(EmitFlags::CustomPrologue, self),
                ),
                self,
            );
            return Ok(true);
        } else if let Some(initializer) = initializer {
            insert_statement_after_custom_prologue(
                statements,
                Some(
                    self.factory
                        .ref_(self).create_expression_statement(
                            self.factory.ref_(self).create_assignment(
                                self.factory
                                    .ref_(self).get_generated_name_for_node(Some(parameter), None),
                                try_visit_node(
                                    initializer,
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Some(|node| is_expression(node, self)),
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                )?,
                            ),
                        )
                        .set_emit_flags(EmitFlags::CustomPrologue, self),
                ),
                self,
            );
            return Ok(true);
        }
        Ok(false)
    }

    pub(super) fn insert_default_value_assignment_for_initializer(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        parameter: Id<Node>,   /*ParameterDeclaration*/
        name: Id<Node>,        /*Identifier*/
        initializer: Id<Node>, /*Expression*/
    ) -> io::Result<()> {
        let initializer = try_visit_node(
            initializer,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        let statement = self
            .factory
            .ref_(self).create_if_statement(
                self.factory
                    .ref_(self).create_type_check(self.factory.ref_(self).clone_node(name), "undefined"),
                self.factory
                    .ref_(self).create_block(
                        vec![self.factory.ref_(self).create_expression_statement(
                            self.factory
                                .ref_(self).create_assignment(
                                    self.factory
                                        .ref_(self).clone_node(name)
                                        .set_text_range(Some(&*name.ref_(self)), self)
                                        .and_set_parent(name.ref_(self).maybe_parent(), self)
                                        .set_emit_flags(EmitFlags::NoSourceMap, self),
                                    initializer.set_emit_flags(
                                        EmitFlags::NoSourceMap
                                            | get_emit_flags(&initializer.ref_(self))
                                            | EmitFlags::NoComments,
                                        self,
                                    ),
                                )
                                .set_text_range(Some(&*parameter.ref_(self)), self)
                                .set_emit_flags(EmitFlags::NoComments, self),
                        )],
                        None,
                    )
                    .set_text_range(Some(&*parameter.ref_(self)), self)
                    .set_emit_flags(
                        EmitFlags::SingleLine
                            | EmitFlags::NoTrailingSourceMap
                            | EmitFlags::NoTokenSourceMaps
                            | EmitFlags::NoComments,
                        self,
                    ),
                None,
            )
            .start_on_new_line(self)
            .set_text_range(Some(&*parameter.ref_(self)), self)
            .set_emit_flags(
                EmitFlags::NoTokenSourceMaps
                    | EmitFlags::NoTrailingSourceMap
                    | EmitFlags::CustomPrologue
                    | EmitFlags::NoComments,
                self,
            );
        insert_statement_after_custom_prologue(statements, Some(statement), self);

        Ok(())
    }

    pub(super) fn should_add_rest_parameter(
        &self,
        node: Option<Id<Node /*ParameterDeclaration*/>>,
        in_constructor_with_synthesized_super: bool,
    ) -> bool {
        node.matches(|node| {
            node.ref_(self).as_parameter_declaration().dot_dot_dot_token.is_some()
        }) && !in_constructor_with_synthesized_super
    }

    pub(super) fn add_rest_parameter_if_needed(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        node: Id<Node>, /*FunctionLikeDeclaration*/
        in_constructor_with_synthesized_super: bool,
    ) -> io::Result<bool> {
        let node_ref = node.ref_(self);
        let node_as_function_like_declaration = node_ref.as_function_like_declaration();
        let mut prologue_statements: Vec<Id<Node /*Statement*/>> = Default::default();
        let parameter = last_or_undefined(&node_as_function_like_declaration.parameters().ref_(self)).cloned();
        if !self
            .should_add_rest_parameter(parameter, in_constructor_with_synthesized_super)
        {
            return Ok(false);
        }
        let parameter = parameter.unwrap();
        let parameter_ref = parameter.ref_(self);
        let parameter_as_parameter_declaration = parameter_ref.as_parameter_declaration();

        let parameter_name = parameter_as_parameter_declaration.name();
        let declaration_name = if parameter_name.ref_(self).kind() == SyntaxKind::Identifier {
            self.factory
                .ref_(self).clone_node(parameter_name)
                .set_text_range(Some(&*parameter_name.ref_(self)), self)
                .and_set_parent(parameter_name.ref_(self).maybe_parent(), self)
        } else {
            self.factory
                .ref_(self).create_temp_variable(Option::<fn(Id<Node>)>::None, None)
        }
        .set_emit_flags(EmitFlags::NoSourceMap, self);

        let expression_name = if parameter_name.ref_(self).kind() == SyntaxKind::Identifier {
            self.factory.ref_(self).clone_node(parameter_name)
        } else {
            declaration_name
        };
        let rest_index = node_as_function_like_declaration.parameters().ref_(self).len() - 1;
        let temp = self.factory.ref_(self).create_loop_variable(None);

        prologue_statements.push(
            self.factory
                .ref_(self).create_variable_statement(
                    Option::<Id<NodeArray>>::None,
                    self.factory.ref_(self).create_variable_declaration_list(
                        vec![self.factory.ref_(self).create_variable_declaration(
                            Some(declaration_name),
                            None,
                            None,
                            Some(
                                self.factory
                                    .ref_(self).create_array_literal_expression(Some(vec![]), None),
                            ),
                        )],
                        None,
                    ),
                )
                .set_text_range(Some(&*parameter.ref_(self)), self)
                .set_emit_flags(EmitFlags::CustomPrologue, self),
        );

        let for_statement =
            self.factory
                .ref_(self).create_for_statement(
                    Some(
                        self.factory
                            .ref_(self).create_variable_declaration_list(
                                vec![self.factory.ref_(self).create_variable_declaration(
                                    Some(temp.clone()),
                                    None,
                                    None,
                                    Some(self.factory.ref_(self).create_numeric_literal(
                                        Number::new(rest_index as f64),
                                        None,
                                    )),
                                )],
                                None,
                            )
                            .set_text_range(Some(&*parameter.ref_(self)), self),
                    ),
                    Some(
                        self.factory
                            .ref_(self).create_less_than(
                                temp.clone(),
                                self.factory.ref_(self).create_property_access_expression(
                                    self.factory.ref_(self).create_identifier("arguments"),
                                    "length",
                                ),
                            )
                            .set_text_range(Some(&*parameter.ref_(self)), self),
                    ),
                    Some(
                        self.factory
                            .ref_(self).create_postfix_increment(temp.clone())
                            .set_text_range(Some(&*parameter.ref_(self)), self),
                    ),
                    self.factory.ref_(self).create_block(
                        vec![self
                            .factory
                            .ref_(self).create_expression_statement(self.factory.ref_(self).create_assignment(
                                self.factory.ref_(self).create_element_access_expression(
                                    expression_name.clone(),
                                    if rest_index == 0 {
                                        temp.clone()
                                    } else {
                                        self.factory.ref_(self).create_subtract(
                                            temp.clone(),
                                            self.factory.ref_(self).create_numeric_literal(
                                                Number::new(rest_index as f64),
                                                None,
                                            ),
                                        )
                                    },
                                ),
                                self.factory.ref_(self).create_element_access_expression(
                                    self.factory.ref_(self).create_identifier("arguments"),
                                    temp.clone(),
                                ),
                            ))
                            .set_text_range(Some(&*parameter.ref_(self)), self)
                            .start_on_new_line(self)],
                        None,
                    ),
                )
                .set_emit_flags(EmitFlags::CustomPrologue, self)
                .start_on_new_line(self);

        prologue_statements.push(for_statement);

        if parameter_name.ref_(self).kind() != SyntaxKind::Identifier {
            prologue_statements.push(
                self.factory
                    .ref_(self).create_variable_statement(
                        Option::<Id<NodeArray>>::None,
                        self.factory.ref_(self).create_variable_declaration_list(
                            try_flatten_destructuring_binding(
                                parameter,
                                |node: Id<Node>| self.visitor(node),
                                self.context.clone(),
                                FlattenLevel::All,
                                Some(expression_name.clone()),
                                None,
                                None,
                                self,
                            )?,
                            None,
                        ),
                    )
                    .set_text_range(Some(&*parameter.ref_(self)), self)
                    .set_emit_flags(EmitFlags::CustomPrologue, self),
            );
        }

        insert_statements_after_custom_prologue(statements, Some(&prologue_statements), self);
        Ok(true)
    }
}
