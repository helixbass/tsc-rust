use std::{io, ptr};

use gc::Gc;
use itertools::Itertools;

use super::TransformTypeScript;
use crate::{
    add_prologue_directives_and_initial_super_call, add_range, find_ancestor,
    get_parse_node_factory, get_parse_tree_node, has_static_modifier, has_syntactic_modifier,
    id_text, is_computed_property_name, is_conditional_type_node, is_expression, is_identifier,
    is_left_hand_side_expression, is_modifier, is_parameter_property_declaration,
    is_private_identifier, is_property_name, is_simple_inlineable_expression, is_statement,
    move_range_past_decorators, move_range_pos, node_is_missing, set_comment_range,
    set_source_map_range, skip_partially_emitted_expressions,
    try_add_prologue_directives_and_initial_super_call, try_maybe_visit_each_child,
    try_visit_each_child, try_visit_function_body, try_visit_node, try_visit_nodes,
    try_visit_parameter_list, visit_each_child, visit_function_body, visit_node, visit_nodes,
    visit_parameter_list, AsDoubleDeref, EmitFlags, FunctionLikeDeclarationInterface,
    HasInitializerInterface, Matches, ModifierFlags, Node, NodeArray, NodeArrayExt, NodeExt,
    NodeFlags, NodeInterface, NonEmpty, PeekableExt, ScriptTarget, SignatureDeclarationInterface,
    SyntaxKind, TypeReferenceSerializationKind, VisitResult,
};

impl TransformTypeScript {
    pub(super) fn serialize_type_list(
        &self,
        types: &[Gc<Node /*TypeNode*/>],
    ) -> io::Result<Gc<Node /*SerializedTypeNode*/>> {
        let mut serialized_union: Option<Gc<Node /*SerializedTypeNode*/>> = Default::default();
        for type_node in types {
            let mut type_node = type_node.clone();
            while type_node.kind() == SyntaxKind::ParenthesizedType {
                type_node = type_node.as_parenthesized_type_node().type_.clone();
            }
            if type_node.kind() == SyntaxKind::NeverKeyword {
                continue;
            }
            if !self.strict_null_checks
                && (type_node.kind() == SyntaxKind::LiteralType
                    && type_node.as_literal_type_node().literal.kind() == SyntaxKind::NullKeyword
                    || type_node.kind() == SyntaxKind::UndefinedKeyword)
            {
                continue;
            }
            let serialized_individual = self.serialize_type_node(Some(&*type_node))?;

            if is_identifier(&serialized_individual)
                && serialized_individual.as_identifier().escaped_text == "Object"
            {
                return Ok(serialized_individual);
            } else if let Some(serialized_union) = serialized_union.as_ref() {
                if !is_identifier(serialized_union)
                    || !is_identifier(&serialized_individual)
                    || serialized_union.as_identifier().escaped_text
                        != serialized_individual.as_identifier().escaped_text
                {
                    return Ok(self
                        .factory
                        .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                        .wrap());
                }
            } else {
                serialized_union = Some(serialized_individual);
            }
        }

        Ok(serialized_union.unwrap_or_else(|| self.factory.create_void_zero()))
    }

    pub(super) fn serialize_type_reference_node(
        &self,
        node: &Node, /*TypeReferenceNode*/
    ) -> io::Result<Gc<Node /*SerializedTypeNode*/>> {
        let node_as_type_reference_node = node.as_type_reference_node();
        let kind = self.resolver.get_type_reference_serialization_kind(
            &node_as_type_reference_node.type_name,
            self.maybe_current_name_scope()
                .or_else(|| self.maybe_current_lexical_scope())
                .as_deref(),
        )?;
        Ok(match kind {
            TypeReferenceSerializationKind::Unknown => {
                if find_ancestor(Some(node), |n: &Node| {
                    n.maybe_parent().matches(|ref n_parent| {
                        is_conditional_type_node(n_parent) && {
                            let n_parent_as_conditional_type_node =
                                n_parent.as_conditional_type_node();
                            ptr::eq(&*n_parent_as_conditional_type_node.true_type, n)
                                || ptr::eq(&*n_parent_as_conditional_type_node.false_type, n)
                        }
                    })
                })
                .is_some()
                {
                    return Ok(self
                        .factory
                        .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                        .wrap());
                }

                let serialized = self.serialize_entity_name_as_expression_fallback(
                    &node_as_type_reference_node.type_name,
                );
                let temp = self.factory.create_temp_variable(
                    Some(|node: &Node| self.context.hoist_variable_declaration(node)),
                    None,
                );
                self.factory
                    .create_conditional_expression(
                        self.factory.create_type_check(
                            self.factory
                                .create_assignment(temp.clone(), serialized)
                                .wrap(),
                            "function",
                        ),
                        None,
                        temp,
                        None,
                        self.factory
                            .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                            .wrap(),
                    )
                    .wrap()
            }
            TypeReferenceSerializationKind::TypeWithConstructSignatureAndValue => {
                self.serialize_entity_name_as_expression(&node_as_type_reference_node.type_name)
            }
            TypeReferenceSerializationKind::VoidNullableOrNeverType => {
                self.factory.create_void_zero()
            }
            TypeReferenceSerializationKind::BigIntLikeType => {
                self.get_global_big_int_name_with_fallback()
            }
            TypeReferenceSerializationKind::BooleanType => self
                .factory
                .create_identifier("Boolean", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::NumberLikeType => self
                .factory
                .create_identifier("Number", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::StringLikeType => self
                .factory
                .create_identifier("String", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::ArrayLikeType => self
                .factory
                .create_identifier("Array", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::ESSymbolType => {
                if self.language_version < ScriptTarget::ES2015 {
                    self.get_global_symbol_name_with_fallback()
                } else {
                    self.factory
                        .create_identifier("Symbol", Option::<Gc<NodeArray>>::None, None)
                        .wrap()
                }
            }
            TypeReferenceSerializationKind::TypeWithCallSignature => self
                .factory
                .create_identifier("Function", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::Promise => self
                .factory
                .create_identifier("Promise", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::ObjectType => self
                .factory
                .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            // default:
            //     return Debug.assertNever(kind);
        })
    }

    pub(super) fn create_checked_value(
        &self,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        self.factory
            .create_logical_and(
                self.factory
                    .create_strict_inequality(
                        self.factory.create_type_of_expression(left).wrap(),
                        self.factory
                            .create_string_literal("undefined".to_owned(), None, None)
                            .wrap(),
                    )
                    .wrap(),
                right,
            )
            .wrap()
    }

    pub(super) fn serialize_entity_name_as_expression_fallback(
        &self,
        node: &Node, /*EntityName*/
    ) -> Gc<Node /*BinaryExpression*/> {
        if node.kind() == SyntaxKind::Identifier {
            let copied = self.serialize_entity_name_as_expression(node);
            return self.create_checked_value(copied.clone(), copied);
        }
        let node_as_qualified_name = node.as_qualified_name();
        if node_as_qualified_name.left.kind() == SyntaxKind::Identifier {
            return self.create_checked_value(
                self.serialize_entity_name_as_expression(&node_as_qualified_name.left),
                self.serialize_entity_name_as_expression(node),
            );
        }
        let left = self.serialize_entity_name_as_expression_fallback(&node_as_qualified_name.left);
        let temp = self.factory.create_temp_variable(
            Some(|node: &Node| self.context.hoist_variable_declaration(node)),
            None,
        );
        self.factory
            .create_logical_and(
                self.factory
                    .create_logical_and(
                        left.as_binary_expression().left.clone(),
                        self.factory
                            .create_strict_inequality(
                                self.factory
                                    .create_assignment(
                                        temp.clone(),
                                        left.as_binary_expression().right.clone(),
                                    )
                                    .wrap(),
                                self.factory.create_void_zero(),
                            )
                            .wrap(),
                    )
                    .wrap(),
                self.factory
                    .create_property_access_expression(temp, node_as_qualified_name.right.clone())
                    .wrap(),
            )
            .wrap()
    }

    pub(super) fn serialize_entity_name_as_expression(
        &self,
        node: &Node, /*EntityName*/
    ) -> Gc<Node /*SerializedEntityNameAsExpression*/> {
        match node.kind() {
            SyntaxKind::Identifier => get_parse_node_factory()
                .clone_node(node)
                .set_text_range(Some(node))
                .and_set_parent(node.maybe_parent())
                .and_set_original(None)
                .and_set_parent(get_parse_tree_node(
                    self.maybe_current_lexical_scope(),
                    Option::<fn(&Node) -> bool>::None,
                )),
            SyntaxKind::QualifiedName => self.serialize_qualified_name_as_expression(node),
            _ => unreachable!(),
        }
    }

    pub(super) fn serialize_qualified_name_as_expression(
        &self,
        node: &Node, /*QualifiedName*/
    ) -> Gc<Node /*SerializedEntityNameAsExpression*/> {
        let node_as_qualified_name = node.as_qualified_name();
        self.factory
            .create_property_access_expression(
                self.serialize_entity_name_as_expression(&node_as_qualified_name.left),
                node_as_qualified_name.right.clone(),
            )
            .wrap()
    }

    pub(super) fn get_global_symbol_name_with_fallback(
        &self,
    ) -> Gc<Node /*ConditionalExpression*/> {
        self.factory
            .create_conditional_expression(
                self.factory.create_type_check(
                    self.factory
                        .create_identifier("Symbol", Option::<Gc<NodeArray>>::None, None)
                        .wrap(),
                    "function",
                ),
                None,
                self.factory
                    .create_identifier("Symbol", Option::<Gc<NodeArray>>::None, None)
                    .wrap(),
                None,
                self.factory
                    .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                    .wrap(),
            )
            .wrap()
    }

    pub(super) fn get_global_big_int_name_with_fallback(&self) -> Gc<Node /*SerializedTypeNode*/> {
        if self.language_version < ScriptTarget::ESNext {
            self.factory
                .create_conditional_expression(
                    self.factory.create_type_check(
                        self.factory
                            .create_identifier("BigInt", Option::<Gc<NodeArray>>::None, None)
                            .wrap(),
                        "function",
                    ),
                    None,
                    self.factory
                        .create_identifier("BigInt", Option::<Gc<NodeArray>>::None, None)
                        .wrap(),
                    None,
                    self.factory
                        .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                        .wrap(),
                )
                .wrap()
        } else {
            self.factory
                .create_identifier("BigInt", Option::<Gc<NodeArray>>::None, None)
                .wrap()
        }
    }

    pub(super) fn get_expression_for_property_name(
        &self,
        member: &Node, /*ClassElement | EnumMember*/
        generate_name_for_computed_property_name: bool,
    ) -> Gc<Node /*Expression*/> {
        let name = member.as_named_declaration().name();
        if is_private_identifier(&name) {
            self.factory
                .create_identifier("", Option::<Gc<NodeArray>>::None, None)
                .wrap()
        } else if is_computed_property_name(&name) {
            let name_as_computed_property_name = name.as_computed_property_name();
            if generate_name_for_computed_property_name
                && !is_simple_inlineable_expression(&name_as_computed_property_name.expression)
            {
                self.factory.get_generated_name_for_node(Some(name), None)
            } else {
                name_as_computed_property_name.expression.clone()
            }
        } else if is_identifier(&name) {
            self.factory
                .create_string_literal(id_text(&name).to_owned(), None, None)
                .wrap()
        } else {
            self.factory.clone_node(&name)
        }
    }

    pub(super) fn visit_property_name_of_class_element(
        &self,
        member: &Node, /*ClassElement*/
    ) -> io::Result<Gc<Node /*PropertyName*/>> {
        let ref name = member.as_named_declaration().name();
        if is_computed_property_name(name)
            && (!has_static_modifier(member)
                && self.maybe_current_class_has_parameter_properties() == Some(true)
                || member.maybe_decorators().is_non_empty())
        {
            let name_as_computed_property_name = name.as_computed_property_name();
            let expression = try_visit_node(
                Some(&*name_as_computed_property_name.expression),
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?
            .unwrap();
            let ref inner_expression = skip_partially_emitted_expressions(&expression);
            if !is_simple_inlineable_expression(inner_expression) {
                let generated_name = self
                    .factory
                    .get_generated_name_for_node(Some(&**name), None);
                self.context.hoist_variable_declaration(&generated_name);
                return Ok(self.factory.update_computed_property_name(
                    name,
                    self.factory
                        .create_assignment(generated_name, expression)
                        .wrap(),
                ));
            }
        }
        Ok(try_visit_node(
            Some(&**name),
            Some(|node: &Node| self.visitor(node)),
            Some(is_property_name),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?
        .unwrap())
    }

    pub(super) fn visit_heritage_clause(
        &self,
        node: &Node, /*HeritageClause*/
    ) -> io::Result<Option<Gc<Node /*HeritageClause*/>>> {
        let node_as_heritage_clause = node.as_heritage_clause();
        if node_as_heritage_clause.token == SyntaxKind::ImplementsKeyword {
            return Ok(None);
        }
        try_maybe_visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
        )
    }

    pub(super) fn visit_expression_with_type_arguments(
        &self,
        node: &Node, /*ExpressionWithTypeArguments*/
    ) -> io::Result<Gc<Node /*ExpressionWithTypeArguments*/>> {
        Ok(self.factory.update_expression_with_type_arguments(
            node,
            try_visit_node(
                Some(&*node.as_expression_with_type_arguments().expression),
                Some(|node: &Node| self.visitor(node)),
                Some(is_left_hand_side_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?
            .unwrap(),
            Option::<Gc<NodeArray>>::None,
        ))
    }

    pub(super) fn should_emit_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> bool {
        !node_is_missing(node.as_function_like_declaration().maybe_body())
    }

    pub(super) fn visit_property_declaration(
        &self,
        node: &Node, /*PropertyDeclaration*/
    ) -> io::Result<VisitResult> /*<Node>*/ {
        if node.flags().intersects(NodeFlags::Ambient)
            || has_syntactic_modifier(node, ModifierFlags::Abstract)
        {
            return Ok(None);
        }
        let updated = self.factory.update_property_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            try_visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_modifier),
                None,
                None,
            )?,
            self.visit_property_name_of_class_element(node)?,
            None,
            None,
            try_visit_node(
                node.as_property_declaration().maybe_initializer(),
                Some(|node: &Node| self.visitor(node)),
                Option::<fn(&Node) -> bool>::None,
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
        );
        if !ptr::eq(&*updated, node) {
            set_comment_range(&updated, node);
            set_source_map_range(
                updated.clone(),
                Some((&move_range_past_decorators(node)).into()),
            );
        }
        Ok(Some(updated.into()))
    }

    pub(super) fn visit_constructor(
        &self,
        node: &Node, /*ConstructorDeclaration*/
    ) -> io::Result<VisitResult> /*<Node>*/ {
        let node_as_constructor_declaration = node.as_constructor_declaration();
        if !self.should_emit_function_like_declaration(node) {
            return Ok(None);
        }

        Ok(Some(
            self.factory
                .update_constructor_declaration(
                    node,
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    try_visit_parameter_list(
                        Some(&node_as_constructor_declaration.parameters()),
                        |node: &Node| self.visitor(node),
                        &**self.context,
                    )?
                    .unwrap(),
                    Some(self.transform_constructor_body(
                        &node_as_constructor_declaration.maybe_body().unwrap(),
                        node,
                    )?),
                )
                .into(),
        ))
    }

    pub(super) fn transform_constructor_body(
        &self,
        body: &Node,        /*Block*/
        constructor: &Node, /*ConstructorDeclaration*/
    ) -> io::Result<Gc<Node>> {
        let body_as_block = body.as_block();
        let parameters_with_property_assignments =
            /*constructor &&*/
            constructor
                .as_constructor_declaration()
                .parameters()
                .owned_iter()
                .filter(|p| {
                    is_parameter_property_declaration(p, constructor)
                });
        if parameters_with_property_assignments
            .clone()
            .peekable()
            .is_empty_()
        {
            return Ok(try_visit_function_body(
                Some(body),
                |node: &Node| self.visitor(node),
                &**self.context,
            )?
            .unwrap());
        }

        let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();
        let index_of_first_statement: usize/* = 0*/;

        self.context.resume_lexical_environment();

        index_of_first_statement = try_add_prologue_directives_and_initial_super_call(
            &self.factory,
            constructor,
            &mut statements,
            |node: &Node| self.visitor(node),
        )?;

        add_range(
            &mut statements,
            Some(
                &parameters_with_property_assignments
                    .map(|ref parameter| {
                        self.transform_parameter_with_property_assignment(parameter)
                            .unwrap()
                    })
                    .collect_vec(),
            ),
            None,
            None,
        );

        add_range(
            &mut statements,
            try_visit_nodes(
                Some(&body_as_block.statements),
                Some(|node: &Node| self.visitor(node)),
                Some(is_statement),
                Some(index_of_first_statement),
                None,
            )?
            .as_double_deref(),
            None,
            None,
        );

        statements = self
            .factory
            .merge_lexical_environment(
                statements,
                self.context.end_lexical_environment().as_deref(),
            )
            .as_vec_owned();
        Ok(self
            .factory
            .create_block(
                self.factory
                    .create_node_array(Some(statements), None)
                    .set_text_range(Some(&*body_as_block.statements)),
                Some(true),
            )
            .wrap()
            .set_text_range(Some(body))
            .set_original_node(Some(body.node_wrapper())))
    }

    pub(super) fn transform_parameter_with_property_assignment(
        &self,
        node: &Node, /*ParameterPropertyDeclaration*/
    ) -> Option<Gc<Node>> {
        let ref name = node.as_named_declaration().name();
        if !is_identifier(name) {
            return None;
        }

        let property_name = self
            .factory
            .clone_node(name)
            .set_text_range(Some(&**name))
            .and_set_parent(name.maybe_parent())
            .set_emit_flags(EmitFlags::NoComments | EmitFlags::NoSourceMap);

        let local_name = self
            .factory
            .clone_node(name)
            .set_text_range(Some(&**name))
            .and_set_parent(name.maybe_parent())
            .set_emit_flags(EmitFlags::NoComments);

        Some(
            self.factory
                .create_expression_statement(
                    self.factory
                        .create_assignment(
                            self.factory
                                .create_property_access_expression(
                                    self.factory.create_this().wrap(),
                                    property_name,
                                )
                                .wrap()
                                .set_text_range(
                                    node.as_named_declaration().maybe_name().as_deref(),
                                ),
                            local_name,
                        )
                        .wrap(),
                )
                .wrap()
                .set_original_node(Some(node.node_wrapper()))
                .set_text_range(Some(&move_range_pos(node, -1).into_readonly_text_range()))
                .remove_all_comments()
                .start_on_new_line(),
        )
    }
}
