use std::io;

use id_arena::Id;

use super::{
    try_visit_function_body_full, try_visit_iteration_body, try_visit_lexical_environment,
    try_visit_parameter_list_full,
};
use crate::{
    is_array_binding_element, is_assert_clause, is_assert_entry, is_assertion_key,
    is_asserts_keyword, is_asterisk_token, is_await_keyword, is_binary_operator_token,
    is_binding_element, is_binding_name, is_block, is_case_block, is_case_or_default_clause,
    is_catch_clause, is_class_element, is_colon_token, is_decorator, is_dot_dot_dot_token,
    is_entity_name, is_enum_member, is_equals_greater_than_token, is_exclamation_token,
    is_export_specifier, is_expression, is_expression_with_type_arguments, is_for_initializer,
    is_heritage_clause, is_identifier, is_identifier_or_this_type_node, is_import_clause,
    is_import_specifier, is_jsx_attribute_like, is_jsx_attributes, is_jsx_child,
    is_jsx_closing_element, is_jsx_closing_fragment, is_jsx_opening_element,
    is_jsx_opening_fragment, is_jsx_tag_name_expression, is_member_name, is_modifier,
    is_module_body, is_module_name, is_module_reference, is_named_export_bindings,
    is_named_import_bindings, is_object_literal_element_like, is_parameter_declaration,
    is_property_name, is_question_dot_token, is_question_or_exclamation_token,
    is_question_or_plus_or_minus_token, is_question_token,
    is_readonly_keyword_or_plus_or_minus_token, is_statement, is_string_literal,
    is_string_literal_or_jsx_expression, is_template_head, is_template_literal,
    is_template_literal_type_span, is_template_middle_or_template_tail, is_template_span, is_token,
    is_type_element, is_type_node, is_type_node_or_type_parameter_declaration,
    is_type_parameter_declaration, is_variable_declaration, is_variable_declaration_list, released,
    return_ok_default_if_none, try_maybe_visit_node, try_maybe_visit_nodes,
    ClassLikeDeclarationInterface, FunctionLikeDeclarationInterface, HasArena,
    HasInitializerInterface, HasMembersInterface, HasQuestionTokenInterface,
    HasStatementsInterface, HasTypeArgumentsInterface, HasTypeInterface,
    HasTypeParametersInterface, InArena, InterfaceOrClassLikeDeclarationInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeFlags, NodeInterface,
    SignatureDeclarationInterface, SyntaxKind, TransformationContext, VisitResult,
};

pub fn try_visit_each_child(
    node: Id<Node>,
    visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> io::Result<Id<Node>> {
    Ok(try_maybe_visit_each_child(Some(node), visitor, context, arena)?.unwrap())
}

pub fn try_maybe_visit_each_child(
    node: Option<Id<Node>>,
    visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> io::Result<Option<Id<Node>>> {
    return try_maybe_visit_each_child_full(
        node,
        visitor,
        context,
        Option::<
            fn(
                Option<Id<NodeArray>>,
                Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                Option<&dyn Fn(Id<Node>) -> bool>,
                Option<usize>,
                Option<usize>,
            ) -> io::Result<Option<Id<NodeArray>>>,
        >::None,
        Option::<fn(Id<Node>) -> io::Result<VisitResult>>::None,
        Option::<
            fn(
                Option<Id<Node>>,
                Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                Option<&dyn Fn(Id<Node>) -> bool>,
                Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>,
            ) -> io::Result<Option<Id<Node>>>,
        >::None,
        arena,
    );
}

pub fn try_maybe_visit_each_child_full(
    node: Option<Id<Node>>,
    mut visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    mut nodes_visitor: Option<
        impl FnMut(
            Option<Id<NodeArray>>,
            Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
            Option<&dyn Fn(Id<Node>) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> io::Result<Option<Id<NodeArray>>>,
    >,
    token_visitor: Option<impl Fn(Id<Node>) -> io::Result<VisitResult>>,
    mut node_visitor: Option<
        impl FnMut(
            Option<Id<Node>>,
            Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
            Option<&dyn Fn(Id<Node>) -> bool>,
            Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>,
        ) -> io::Result<Option<Id<Node>>>,
    >,
    arena: &impl HasArena,
) -> io::Result<Option<Id<Node>>> {
    let mut nodes_visitor =
        move |nodes: Option<Id<NodeArray>>,
              visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
              test: Option<&dyn Fn(Id<Node>) -> bool>,
              start: Option<usize>,
              count: Option<usize>|
              -> io::Result<Option<Id<NodeArray>>> {
            if let Some(nodes_visitor) = nodes_visitor.as_mut() {
                nodes_visitor(nodes, visitor, test, start, count)
            } else {
                try_maybe_visit_nodes(nodes, visitor, test, start, count, arena)
            }
        };
    let mut node_visitor =
        move |node: Option<Id<Node>>,
              visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
              lift: Option<&dyn Fn(Id<Node>) -> bool>,
              test: Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>|
              -> io::Result<Option<Id<Node>>> {
            if let Some(node_visitor) = node_visitor.as_mut() {
                node_visitor(node, visitor, lift, test)
            } else {
                try_maybe_visit_node(node, visitor, lift, test)
            }
        };
    let node = return_ok_default_if_none!(node);

    let kind = node.ref_(arena).kind();

    if kind > SyntaxKind::FirstToken && kind <= SyntaxKind::LastToken
        || kind == SyntaxKind::ThisType
    {
        return Ok(Some(node));
    }

    let factory = context.factory();

    Ok(match kind {
        SyntaxKind::Identifier => Some(factory.ref_(arena).update_identifier(
            node,
            released!(nodes_visitor(
                node.ref_(arena).as_identifier().maybe_type_arguments(),
                Some(&mut |node: Id<Node>| visitor(node)),
                Some(&|node: Id<Node>| {
                    is_type_node_or_type_parameter_declaration(&node.ref_(arena))
                }),
                None,
                None,
            ))?,
        )),
        SyntaxKind::QualifiedName => Some(
            factory.ref_(arena).update_qualified_name(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_qualified_name().left),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_entity_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_qualified_name().right),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ComputedPropertyName => Some(
            factory.ref_(arena).update_computed_property_name(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_computed_property_name().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TypeParameter => Some(
            factory.ref_(arena).update_type_parameter_declaration(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_type_parameter_declaration().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_type_parameter_declaration().constraint,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_type_parameter_declaration().default,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::Parameter => Some(factory.ref_(arena).update_parameter_declaration(
            node,
            released!(nodes_visitor(
                node.ref_(arena).maybe_decorators(),
                Some(&mut |node: Id<Node>| visitor(node)),
                Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                None,
                None,
            ))?,
            released!(nodes_visitor(
                node.ref_(arena).maybe_modifiers(),
                Some(&mut |node: Id<Node>| visitor(node)),
                Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                None,
                None,
            ))?,
            released!(node_visitor(
                    node.ref_(arena)
                        .as_parameter_declaration()
                        .dot_dot_dot_token,
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_dot_dot_dot_token(&node.ref_(arena))),
                    None,
                ))?,
            released!(node_visitor(
                node.ref_(arena).as_parameter_declaration().maybe_name(),
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_binding_name(&node.ref_(arena))),
                None,
            ))?,
            released!(node_visitor(
                    node.ref_(arena)
                        .as_parameter_declaration()
                        .maybe_question_token(),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_question_token(&node.ref_(arena))),
                    None,
                ))?,
            released!(node_visitor(
                node.ref_(arena).as_parameter_declaration().maybe_type(),
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                None,
            ))?,
            released!(node_visitor(
                    node.ref_(arena)
                        .as_parameter_declaration()
                        .maybe_initializer(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?,
        )),
        SyntaxKind::Decorator => Some(
            factory.ref_(arena).update_decorator(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_decorator().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::PropertySignature => Some(
            factory.ref_(arena).update_property_signature(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_property_signature().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena)
                        .as_property_signature()
                        .maybe_question_token(),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_token(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_property_signature().maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::PropertyDeclaration => Some(
            factory.ref_(arena).update_property_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_property_declaration().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena)
                        .as_property_declaration()
                        .maybe_question_token()
                        .or(node.ref_(arena).as_property_declaration().exclamation_token),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_question_or_exclamation_token(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_property_declaration().maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena)
                        .as_property_declaration()
                        .maybe_initializer(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::MethodSignature => Some(
            factory.ref_(arena).update_method_signature(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_method_signature().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena)
                        .as_method_signature()
                        .maybe_question_token(),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_question_token(&node.ref_(arena))),
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_method_signature()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_method_signature().parameters()),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_parameter_declaration(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_method_signature().maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::MethodDeclaration => Some(
            factory.ref_(arena).update_method_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena)
                        .as_method_declaration()
                        .maybe_asterisk_token(),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_asterisk_token(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_method_declaration().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena)
                        .as_method_declaration()
                        .maybe_question_token(),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_question_token(&node.ref_(arena))),
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_method_declaration()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                try_visit_parameter_list_full(
                    Some(node.ref_(arena).as_method_declaration().parameters()),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |nodes: Option<Id<NodeArray>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         test: Option<&dyn Fn(Id<Node>) -> bool>,
                         start: Option<usize>,
                         count: Option<usize>| {
                            released!(nodes_visitor(nodes, visitor, test, start, count))
                        },
                    ),
                    arena,
                )?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_method_declaration().maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
                try_visit_function_body_full(
                    node.ref_(arena).as_method_declaration().maybe_body(),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |node: Option<Id<Node>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         lift: Option<&dyn Fn(Id<Node>) -> bool>,
                         test: Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>| {
                            node_visitor(node, visitor, lift, test)
                        },
                    ),
                    arena,
                )?,
            ),
        ),
        SyntaxKind::Constructor => Some(
            factory.ref_(arena).update_constructor_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                try_visit_parameter_list_full(
                    Some(node.ref_(arena).as_constructor_declaration().parameters()),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |nodes: Option<Id<NodeArray>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         test: Option<&dyn Fn(Id<Node>) -> bool>,
                         start: Option<usize>,
                         count: Option<usize>| {
                            nodes_visitor(nodes, visitor, test, start, count)
                        },
                    ),
                    arena,
                )?
                .unwrap(),
                try_visit_function_body_full(
                    node.ref_(arena).as_constructor_declaration().maybe_body(),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |node: Option<Id<Node>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         lift: Option<&dyn Fn(Id<Node>) -> bool>,
                         test: Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>| {
                            node_visitor(node, visitor, lift, test)
                        },
                    ),
                    arena,
                )?,
            ),
        ),
        SyntaxKind::GetAccessor => Some(
            factory.ref_(arena).update_get_accessor_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_get_accessor_declaration().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                try_visit_parameter_list_full(
                    Some(node.ref_(arena).as_get_accessor_declaration().parameters()),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |nodes: Option<Id<NodeArray>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         test: Option<&dyn Fn(Id<Node>) -> bool>,
                         start: Option<usize>,
                         count: Option<usize>| {
                            nodes_visitor(nodes, visitor, test, start, count)
                        },
                    ),
                    arena,
                )?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_get_accessor_declaration().maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
                try_visit_function_body_full(
                    node.ref_(arena).as_get_accessor_declaration().maybe_body(),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |node: Option<Id<Node>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         lift: Option<&dyn Fn(Id<Node>) -> bool>,
                         test: Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>| {
                            node_visitor(node, visitor, lift, test)
                        },
                    ),
                    arena,
                )?,
            ),
        ),
        SyntaxKind::SetAccessor => Some(
            factory.ref_(arena).update_set_accessor_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_set_accessor_declaration().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                try_visit_parameter_list_full(
                    Some(node.ref_(arena).as_set_accessor_declaration().parameters()),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |nodes: Option<Id<NodeArray>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         test: Option<&dyn Fn(Id<Node>) -> bool>,
                         start: Option<usize>,
                         count: Option<usize>| {
                            nodes_visitor(nodes, visitor, test, start, count)
                        },
                    ),
                    arena,
                )?
                .unwrap(),
                try_visit_function_body_full(
                    node.ref_(arena).as_set_accessor_declaration().maybe_body(),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |node: Option<Id<Node>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         lift: Option<&dyn Fn(Id<Node>) -> bool>,
                         test: Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>| {
                            node_visitor(node, visitor, lift, test)
                        },
                    ),
                    arena,
                )?,
            ),
        ),
        SyntaxKind::ClassStaticBlockDeclaration => {
            context.start_lexical_environment();
            context.suspend_lexical_environment();
            Some(
                factory.ref_(arena).update_class_static_block_declaration(
                    node,
                    released!(nodes_visitor(
                        node.ref_(arena).maybe_decorators(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    ))?,
                    released!(nodes_visitor(
                        node.ref_(arena).maybe_modifiers(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    ))?,
                    try_visit_function_body_full(
                        Some(node.ref_(arena).as_class_static_block_declaration().body),
                        |node: Id<Node>| visitor(node),
                        context,
                        Some(|
                            node: Option<Id<Node>>,
                            visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                            lift: Option<&dyn Fn(Id<Node>) -> bool>,
                            test: Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>
                        | {
                            node_visitor(node, visitor, lift, test)
                        }),
                        arena,
                    )?.unwrap(),
                )
            )
        }
        SyntaxKind::CallSignature => Some(
            factory.ref_(arena).update_call_signature(
                node,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_call_signature_declaration()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    Some(
                        node.ref_(arena)
                            .as_call_signature_declaration()
                            .parameters(),
                    ),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_parameter_declaration(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena)
                        .as_call_signature_declaration()
                        .maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::ConstructSignature => Some(
            factory.ref_(arena).update_construct_signature(
                node,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_construct_signature_declaration()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    Some(
                        node.ref_(arena)
                            .as_construct_signature_declaration()
                            .parameters(),
                    ),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_parameter_declaration(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena)
                        .as_construct_signature_declaration()
                        .maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::IndexSignature => Some(
            factory.ref_(arena).update_index_signature(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    Some(
                        node.ref_(arena)
                            .as_index_signature_declaration()
                            .parameters(),
                    ),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_parameter_declaration(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(
                        node.ref_(arena)
                            .as_index_signature_declaration()
                            .maybe_type()
                            .unwrap(),
                    ),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TypePredicate => Some(
            factory.ref_(arena).update_type_predicate_node(
                node,
                released!(node_visitor(
                    node.ref_(arena).as_type_predicate_node().asserts_modifier,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_asserts_keyword(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_type_predicate_node().parameter_name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier_or_this_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_type_predicate_node().maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::TypeReference => Some(
            factory.ref_(arena).update_type_reference_node(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_type_reference_node().type_name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_entity_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_type_reference_node()
                        .maybe_type_arguments(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                    None,
                ))?,
            ),
        ),
        SyntaxKind::FunctionType => Some(
            factory.ref_(arena).update_function_type_node(
                node,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_function_type_node()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_function_type_node().parameters()),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_parameter_declaration(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_function_type_node().maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::ConstructorType => Some(
            factory.ref_(arena).update_constructor_type_node(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_constructor_type_node()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_constructor_type_node().parameters()),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_parameter_declaration(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_constructor_type_node().maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::TypeQuery => Some(
            factory.ref_(arena).update_type_query_node(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_type_query_node().expr_name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_entity_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TypeLiteral => Some(
            factory.ref_(arena).update_type_literal_node(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_type_literal_node().members),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_element(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ArrayType => Some(
            factory.ref_(arena).update_array_type_node(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_array_type_node().element_type),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TupleType => Some(
            factory.ref_(arena).update_tuple_type_node(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_tuple_type_node().elements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::OptionalType => Some(
            factory.ref_(arena).update_optional_type_node(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_optional_type_node().type_),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::RestType => Some(
            factory.ref_(arena).update_rest_type_node(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_rest_type_node().type_),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::UnionType => Some(
            factory.ref_(arena).update_union_type_node(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_union_type_node().types),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::IntersectionType => Some(
            factory.ref_(arena).update_intersection_type_node(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_intersection_type_node().types),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ConditionalType => Some(
            factory.ref_(arena).update_conditional_type_node(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_conditional_type_node().check_type),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_conditional_type_node().extends_type),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_conditional_type_node().true_type),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_conditional_type_node().false_type),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::InferType => Some(
            factory.ref_(arena).update_infer_type_node(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_infer_type_node().type_parameter),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ImportType => Some(
            factory.ref_(arena).update_import_type_node(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_import_type_node().argument),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_import_type_node().qualifier,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_entity_name(&node.ref_(arena))),
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_import_type_node()
                        .maybe_type_arguments(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                Some(node.ref_(arena).as_import_type_node().is_type_of()),
            ),
        ),
        SyntaxKind::NamedTupleMember => Some(
            factory.ref_(arena).update_named_tuple_member(
                node,
                released!(node_visitor(
                    node.ref_(arena).as_named_tuple_member().dot_dot_dot_token,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_dot_dot_dot_token(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_named_tuple_member().name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_named_tuple_member().question_token,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_question_token(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_named_tuple_member().type_),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ParenthesizedType => Some(
            factory.ref_(arena).update_parenthesized_type(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_parenthesized_type_node().type_),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TypeOperator => Some(
            factory.ref_(arena).update_type_operator_node(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_type_operator_node().type_),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::IndexedAccessType => Some(
            factory.ref_(arena).update_indexed_access_type_node(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_indexed_access_type_node().object_type),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_indexed_access_type_node().index_type),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::MappedType => Some(
            factory.ref_(arena).update_mapped_type_node(
                node,
                released!(node_visitor(
                    node.ref_(arena).as_mapped_type_node().readonly_token,
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| {
                        is_readonly_keyword_or_plus_or_minus_token(&node.ref_(arena))
                    }),
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_mapped_type_node().type_parameter),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_mapped_type_node().name_type,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_mapped_type_node().question_token,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_question_or_plus_or_minus_token(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_mapped_type_node().type_,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).as_mapped_type_node().members,
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_element(&node.ref_(arena))),
                    None,
                    None,
                ))?,
            ),
        ),
        SyntaxKind::LiteralType => Some(
            factory.ref_(arena).update_literal_type_node(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_literal_type_node().literal),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TemplateLiteralType => Some(
            factory.ref_(arena).update_template_literal_type(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_template_literal_type_node().head),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_template_head(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    Some(
                        node.ref_(arena)
                            .as_template_literal_type_node()
                            .template_spans,
                    ),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_template_literal_type_span(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TemplateLiteralTypeSpan => Some(
            factory.ref_(arena).update_template_literal_type_span(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_template_literal_type_span().type_),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_template_literal_type_span().literal),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_template_middle_or_template_tail(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ObjectBindingPattern => Some(
            factory.ref_(arena).update_object_binding_pattern(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_object_binding_pattern().elements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_binding_element(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ArrayBindingPattern => Some(
            factory.ref_(arena).update_array_binding_pattern(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_array_binding_pattern().elements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_array_binding_element(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::BindingElement => Some(
            factory.ref_(arena).update_binding_element(
                node,
                released!(node_visitor(
                    node.ref_(arena).as_binding_element().dot_dot_dot_token,
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_dot_dot_dot_token(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_binding_element().property_name,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_binding_element().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_binding_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_binding_element().maybe_initializer(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::ArrayLiteralExpression => Some(
            factory.ref_(arena).update_array_literal_expression(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_array_literal_expression().elements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ObjectLiteralExpression => Some(
            factory.ref_(arena).update_object_literal_expression(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_object_literal_expression().properties),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_object_literal_element_like(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::PropertyAccessExpression => {
            if node
                .ref_(arena)
                .flags()
                .intersects(NodeFlags::OptionalChain)
            {
                return Ok(Some(
                    factory.ref_(arena).update_property_access_chain(
                        node,
                        released!(node_visitor(
                            Some(node.ref_(arena).as_property_access_expression().expression),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                        ))?
                        .unwrap(),
                        released!(node_visitor(
                            node.ref_(arena)
                                .as_property_access_expression()
                                .question_dot_token,
                            Some(&mut |node: Id<Node>| {
                                Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                    token_visitor(node)?
                                } else {
                                    Some(node.into())
                                })
                            }),
                            Some(&|node: Id<Node>| is_question_dot_token(&node.ref_(arena))),
                            None,
                        ))?,
                        released!(node_visitor(
                            Some(node.ref_(arena).as_property_access_expression().name()),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_member_name(&node.ref_(arena))),
                            None,
                        ))?
                        .unwrap(),
                    ),
                ));
            }
            Some(
                factory.ref_(arena).update_property_access_expression(
                    node,
                    released!(node_visitor(
                        Some(node.ref_(arena).as_property_access_expression().expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    ))?
                    .unwrap(),
                    released!(node_visitor(
                        Some(node.ref_(arena).as_property_access_expression().name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_member_name(&node.ref_(arena))),
                        None,
                    ))?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ElementAccessExpression => {
            if node
                .ref_(arena)
                .flags()
                .intersects(NodeFlags::OptionalChain)
            {
                return Ok(Some(
                    factory.ref_(arena).update_element_access_chain(
                        node,
                        released!(node_visitor(
                            Some(node.ref_(arena).as_element_access_expression().expression),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                        ))?
                        .unwrap(),
                        released!(node_visitor(
                            node.ref_(arena)
                                .as_element_access_expression()
                                .question_dot_token,
                            Some(&mut |node: Id<Node>| {
                                Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                    token_visitor(node)?
                                } else {
                                    Some(node.into())
                                })
                            }),
                            Some(&|node: Id<Node>| is_question_dot_token(&node.ref_(arena))),
                            None,
                        ))?,
                        released!(node_visitor(
                            Some(
                                node.ref_(arena)
                                    .as_element_access_expression()
                                    .argument_expression,
                            ),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                        ))?
                        .unwrap(),
                    ),
                ));
            }
            Some(
                factory.ref_(arena).update_element_access_expression(
                    node,
                    released!(node_visitor(
                        Some(node.ref_(arena).as_element_access_expression().expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    ))?
                    .unwrap(),
                    released!(node_visitor(
                        Some(
                            node.ref_(arena)
                                .as_element_access_expression()
                                .argument_expression,
                        ),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    ))?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::CallExpression => {
            if node
                .ref_(arena)
                .flags()
                .intersects(NodeFlags::OptionalChain)
            {
                return Ok(Some(
                    factory.ref_(arena).update_call_chain(
                        node,
                        released!(node_visitor(
                            Some(node.ref_(arena).as_call_expression().expression),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                        ))?
                        .unwrap(),
                        released!(node_visitor(
                            node.ref_(arena).as_call_expression().question_dot_token,
                            Some(&mut |node: Id<Node>| {
                                Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                    token_visitor(node)?
                                } else {
                                    Some(node.into())
                                })
                            }),
                            Some(&|node: Id<Node>| is_question_dot_token(&node.ref_(arena))),
                            None,
                        ))?,
                        released!(nodes_visitor(
                            node.ref_(arena).as_call_expression().maybe_type_arguments(),
                            Some(&mut |node: Id<Node>| visitor(node)),
                            Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                            None,
                            None,
                        ))?,
                        released!(nodes_visitor(
                            Some(node.ref_(arena).as_call_expression().arguments),
                            Some(&mut |node: Id<Node>| visitor(node)),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                            None,
                        ))?
                        .unwrap(),
                    ),
                ));
            }
            Some(
                factory.ref_(arena).update_call_expression(
                    node,
                    released!(node_visitor(
                        Some(node.ref_(arena).as_call_expression().expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    ))?
                    .unwrap(),
                    released!(nodes_visitor(
                        node.ref_(arena).as_call_expression().maybe_type_arguments(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    ))?,
                    released!(nodes_visitor(
                        Some(node.ref_(arena).as_call_expression().arguments),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                        None,
                    ))?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::NewExpression => Some(
            factory.ref_(arena).update_new_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_new_expression().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    node.ref_(arena).as_new_expression().maybe_type_arguments(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).as_new_expression().arguments,
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                    None,
                ))?,
            ),
        ),
        SyntaxKind::TaggedTemplateExpression => Some(
            factory.ref_(arena).update_tagged_template_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_tagged_template_expression().tag),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_tagged_template_expression()
                        .maybe_type_arguments(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_tagged_template_expression().template),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_template_literal(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TypeAssertionExpression => Some(
            factory.ref_(arena).update_type_assertion(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_type_assertion().type_),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_type_assertion().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ParenthesizedExpression => Some(
            factory.ref_(arena).update_parenthesized_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_parenthesized_expression().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::FunctionExpression => Some(
            factory.ref_(arena).update_function_expression(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena)
                        .as_function_expression()
                        .maybe_asterisk_token(),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_asterisk_token(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_function_expression().maybe_name(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_function_expression()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                try_visit_parameter_list_full(
                    Some(node.ref_(arena).as_function_expression().parameters()),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |nodes: Option<Id<NodeArray>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         test: Option<&dyn Fn(Id<Node>) -> bool>,
                         start: Option<usize>,
                         count: Option<usize>| {
                            nodes_visitor(nodes, visitor, test, start, count)
                        },
                    ),
                    arena,
                )?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_function_expression().maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
                try_visit_function_body_full(
                    Some(
                        node.ref_(arena)
                            .as_function_expression()
                            .maybe_body()
                            .unwrap(),
                    ),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |node: Option<Id<Node>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         lift: Option<&dyn Fn(Id<Node>) -> bool>,
                         test: Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>| {
                            node_visitor(node, visitor, lift, test)
                        },
                    ),
                    arena,
                )?
                .unwrap(),
            ),
        ),
        SyntaxKind::ArrowFunction => Some(
            factory.ref_(arena).update_arrow_function(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).as_arrow_function().maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                try_visit_parameter_list_full(
                    Some(node.ref_(arena).as_arrow_function().parameters()),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |nodes: Option<Id<NodeArray>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         test: Option<&dyn Fn(Id<Node>) -> bool>,
                         start: Option<usize>,
                         count: Option<usize>| {
                            nodes_visitor(nodes, visitor, test, start, count)
                        },
                    ),
                    arena,
                )?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_arrow_function().maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    Some(
                        node.ref_(arena)
                            .as_arrow_function()
                            .equals_greater_than_token,
                    ),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_equals_greater_than_token(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                try_visit_function_body_full(
                    Some(node.ref_(arena).as_arrow_function().maybe_body().unwrap()),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |node: Option<Id<Node>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         lift: Option<&dyn Fn(Id<Node>) -> bool>,
                         test: Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>| {
                            node_visitor(node, visitor, lift, test)
                        },
                    ),
                    arena,
                )?
                .unwrap(),
            ),
        ),
        SyntaxKind::DeleteExpression => Some(
            factory.ref_(arena).update_delete_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_delete_expression().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TypeOfExpression => Some(
            factory.ref_(arena).update_type_of_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_type_of_expression().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::VoidExpression => Some(
            factory.ref_(arena).update_void_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_void_expression().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::AwaitExpression => Some(
            factory.ref_(arena).update_await_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_await_expression().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::PrefixUnaryExpression => Some(
            factory.ref_(arena).update_prefix_unary_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_prefix_unary_expression().operand),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::PostfixUnaryExpression => Some(
            factory.ref_(arena).update_postfix_unary_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_postfix_unary_expression().operand),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::BinaryExpression => Some(
            factory.ref_(arena).update_binary_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_binary_expression().left),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_binary_expression().operator_token),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_binary_operator_token(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_binary_expression().right),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ConditionalExpression => Some(
            factory.ref_(arena).update_conditional_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_conditional_expression().condition),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_conditional_expression().question_token),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_question_token(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_conditional_expression().when_true),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_conditional_expression().colon_token),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_colon_token(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_conditional_expression().when_false),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TemplateExpression => Some(
            factory.ref_(arena).update_template_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_template_expression().head),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_template_head(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_template_expression().template_spans),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_template_span(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::YieldExpression => Some(factory.ref_(arena).update_yield_expression(
            node,
            released!(node_visitor(
                node.ref_(arena).as_yield_expression().asterisk_token,
                Some(&mut |node: Id<Node>| {
                    Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                        token_visitor(node)?
                    } else {
                        Some(node.into())
                    })
                }),
                Some(&|node: Id<Node>| is_asterisk_token(&node.ref_(arena))),
                None,
            ))?,
            released!(node_visitor(
                node.ref_(arena).as_yield_expression().expression,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_expression(node, arena)),
                None,
            ))?,
        )),
        SyntaxKind::SpreadElement => Some(
            factory.ref_(arena).update_spread_element(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_spread_element().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ClassExpression => Some(
            factory.ref_(arena).update_class_expression(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_class_expression().maybe_name(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_class_expression()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_class_expression()
                        .maybe_heritage_clauses(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_heritage_clause(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_class_expression().members()),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_class_element(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ExpressionWithTypeArguments => Some(
            factory.ref_(arena).update_expression_with_type_arguments(
                node,
                released!(node_visitor(
                    Some(
                        node.ref_(arena)
                            .as_expression_with_type_arguments()
                            .expression,
                    ),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_expression_with_type_arguments()
                        .maybe_type_arguments(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                    None,
                ))?,
            ),
        ),
        SyntaxKind::AsExpression => Some(
            factory.ref_(arena).update_as_expression(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_as_expression().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_as_expression().type_),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::NonNullExpression => {
            if node
                .ref_(arena)
                .flags()
                .intersects(NodeFlags::OptionalChain)
            {
                return Ok(Some(
                    factory.ref_(arena).update_non_null_chain(
                        node,
                        released!(node_visitor(
                            Some(node.ref_(arena).as_non_null_expression().expression),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                        ))?
                        .unwrap(),
                    ),
                ));
            }
            Some(
                factory.ref_(arena).update_non_null_expression(
                    node,
                    released!(node_visitor(
                        Some(node.ref_(arena).as_non_null_expression().expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    ))?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::MetaProperty => Some(
            factory.ref_(arena).update_meta_property(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_meta_property().name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TemplateSpan => Some(
            factory.ref_(arena).update_template_span(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_template_span().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_template_span().literal),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_template_middle_or_template_tail(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::Block => Some(
            factory.ref_(arena).update_block(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_block().statements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_statement(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::VariableStatement => Some(
            factory.ref_(arena).update_variable_statement(
                node,
                released!(nodes_visitor(
                    released!(node.ref_(arena).maybe_modifiers()),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(released!(
                        node.ref_(arena).as_variable_statement().declaration_list
                    )),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_variable_declaration_list(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ExpressionStatement => Some(
            factory.ref_(arena).update_expression_statement(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_expression_statement().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::IfStatement => Some(
            factory.ref_(arena).update_if_statement(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_if_statement().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_if_statement().then_statement),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_statement(node, arena)),
                    Some(&|nodes: &[Id<Node>]| factory.ref_(arena).lift_to_block(nodes)),
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_if_statement().else_statement,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_statement(node, arena)),
                    Some(&|nodes: &[Id<Node>]| factory.ref_(arena).lift_to_block(nodes)),
                ))?,
            ),
        ),
        SyntaxKind::DoStatement => Some(
            factory.ref_(arena).update_do_statement(
                node,
                try_visit_iteration_body(
                    node.ref_(arena).as_do_statement().statement,
                    |node: Id<Node>| visitor(node),
                    context,
                    arena,
                )?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_do_statement().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::WhileStatement => Some(
            factory.ref_(arena).update_while_statement(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_while_statement().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                try_visit_iteration_body(
                    node.ref_(arena).as_while_statement().statement,
                    |node: Id<Node>| visitor(node),
                    context,
                    arena,
                )?,
            ),
        ),
        SyntaxKind::ForStatement => Some(factory.ref_(arena).update_for_statement(
            node,
            released!(node_visitor(
                node.ref_(arena).as_for_statement().initializer,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_for_initializer(node, arena)),
                None,
            ))?,
            released!(node_visitor(
                node.ref_(arena).as_for_statement().condition,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_expression(node, arena)),
                None,
            ))?,
            released!(node_visitor(
                node.ref_(arena).as_for_statement().incrementor,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_expression(node, arena)),
                None,
            ))?,
            try_visit_iteration_body(
                node.ref_(arena).as_for_statement().statement,
                |node: Id<Node>| visitor(node),
                context,
                arena,
            )?,
        )),
        SyntaxKind::ForInStatement => Some(
            factory.ref_(arena).update_for_in_statement(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_for_in_statement().initializer),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_for_initializer(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_for_in_statement().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                try_visit_iteration_body(
                    node.ref_(arena).as_for_in_statement().statement,
                    |node: Id<Node>| visitor(node),
                    context,
                    arena,
                )?,
            ),
        ),
        SyntaxKind::ForOfStatement => Some(
            factory.ref_(arena).update_for_of_statement(
                node,
                released!(node_visitor(
                    node.ref_(arena).as_for_of_statement().await_modifier,
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_await_keyword(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_for_of_statement().initializer),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_for_initializer(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_for_of_statement().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                try_visit_iteration_body(
                    node.ref_(arena).as_for_of_statement().statement,
                    |node: Id<Node>| visitor(node),
                    context,
                    arena,
                )?,
            ),
        ),
        SyntaxKind::ContinueStatement => Some(factory.ref_(arena).update_continue_statement(
            node,
            released!(node_visitor(
                node.ref_(arena).as_continue_statement().label,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                None,
            ))?,
        )),
        SyntaxKind::BreakStatement => Some(factory.ref_(arena).update_break_statement(
            node,
            released!(node_visitor(
                node.ref_(arena).as_break_statement().label,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                None,
            ))?,
        )),
        SyntaxKind::ReturnStatement => Some(factory.ref_(arena).update_return_statement(
            node,
            released!(node_visitor(
                node.ref_(arena).as_return_statement().expression,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_expression(node, arena)),
                None,
            ))?,
        )),
        SyntaxKind::WithStatement => Some(
            factory.ref_(arena).update_with_statement(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_with_statement().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_with_statement().statement),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_statement(node, arena)),
                    Some(&|nodes: &[Id<Node>]| factory.ref_(arena).lift_to_block(nodes)),
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::SwitchStatement => Some(
            factory.ref_(arena).update_switch_statement(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_switch_statement().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_switch_statement().case_block),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_case_block(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::LabeledStatement => Some(
            factory.ref_(arena).update_labeled_statement(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_labeled_statement().label),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_labeled_statement().statement),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_statement(node, arena)),
                    Some(&|nodes: &[Id<Node>]| factory.ref_(arena).lift_to_block(nodes)),
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ThrowStatement => Some(
            factory.ref_(arena).update_throw_statement(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_throw_statement().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TryStatement => Some(
            factory.ref_(arena).update_try_statement(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_try_statement().try_block),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_block(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_try_statement().catch_clause,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_catch_clause(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_try_statement().finally_block,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_block(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::VariableDeclaration => Some(factory.ref_(arena).update_variable_declaration(
            node,
            released!(node_visitor(
                node.ref_(arena).as_variable_declaration().maybe_name(),
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_binding_name(&node.ref_(arena))),
                None,
            ))?,
            released!(node_visitor(
                node.ref_(arena).as_variable_declaration().exclamation_token,
                Some(&mut |node: Id<Node>| {
                    Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                        token_visitor(node)?
                    } else {
                        Some(node.into())
                    })
                }),
                Some(&|node: Id<Node>| is_exclamation_token(&node.ref_(arena))),
                None,
            ))?,
            released!(node_visitor(
                node.ref_(arena).as_variable_declaration().maybe_type(),
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                None,
            ))?,
            released!(node_visitor(
                    node.ref_(arena)
                        .as_variable_declaration()
                        .maybe_initializer(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?,
        )),
        SyntaxKind::VariableDeclarationList => Some(
            factory.ref_(arena).update_variable_declaration_list(
                node,
                released!(nodes_visitor(
                    Some(released!(
                        node.ref_(arena).as_variable_declaration_list().declarations
                    )),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_variable_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::FunctionDeclaration => Some(
            factory.ref_(arena).update_function_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena)
                        .as_function_declaration()
                        .maybe_asterisk_token(),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_asterisk_token(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_function_declaration().maybe_name(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_function_declaration()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                try_visit_parameter_list_full(
                    Some(node.ref_(arena).as_function_declaration().parameters()),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |nodes: Option<Id<NodeArray>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         test: Option<&dyn Fn(Id<Node>) -> bool>,
                         start: Option<usize>,
                         count: Option<usize>| {
                            nodes_visitor(nodes, visitor, test, start, count)
                        },
                    ),
                    arena,
                )?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_function_declaration().maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?,
                try_visit_function_body_full(
                    node.ref_(arena).as_function_declaration().maybe_body(),
                    |node: Id<Node>| visitor(node),
                    context,
                    Some(
                        |node: Option<Id<Node>>,
                         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                         lift: Option<&dyn Fn(Id<Node>) -> bool>,
                         test: Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>| {
                            node_visitor(node, visitor, lift, test)
                        },
                    ),
                    arena,
                )?,
            ),
        ),
        SyntaxKind::ClassDeclaration => Some(
            factory.ref_(arena).update_class_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_class_declaration().maybe_name(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_class_declaration()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_class_declaration()
                        .maybe_heritage_clauses(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_heritage_clause(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_class_declaration().members()),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_class_element(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::InterfaceDeclaration => Some(
            factory.ref_(arena).update_interface_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_interface_declaration().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_interface_declaration()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_interface_declaration()
                        .maybe_heritage_clauses(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_heritage_clause(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_interface_declaration().members()),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_element(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::TypeAliasDeclaration => Some(
            factory.ref_(arena).update_type_alias_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_type_alias_declaration().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_type_alias_declaration()
                        .maybe_type_parameters(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(
                        node.ref_(arena)
                            .as_type_alias_declaration()
                            .maybe_type()
                            .unwrap(),
                    ),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::EnumDeclaration => Some(
            factory.ref_(arena).update_enum_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_enum_declaration().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_enum_declaration().members),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_enum_member(&node.ref_(arena))),
                    None,
                    None,
                ))?,
            ),
        ),
        SyntaxKind::ModuleDeclaration => Some(
            factory.ref_(arena).update_module_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_module_declaration().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_module_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_module_declaration().body,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_module_body(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::ModuleBlock => Some(
            factory.ref_(arena).update_module_block(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_module_block().statements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_statement(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::CaseBlock => Some(
            factory.ref_(arena).update_case_block(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_case_block().clauses),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_case_or_default_clause(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::NamespaceExportDeclaration => Some(
            factory.ref_(arena).update_namespace_export_declaration(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_namespace_export_declaration().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ImportEqualsDeclaration => Some(
            factory.ref_(arena).update_import_equals_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                node.ref_(arena).as_import_equals_declaration().is_type_only,
                released!(node_visitor(
                    Some(node.ref_(arena).as_import_equals_declaration().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(
                        node.ref_(arena)
                            .as_import_equals_declaration()
                            .module_reference,
                    ),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_module_reference(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ImportDeclaration => Some(
            factory.ref_(arena).update_import_declaration(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    node.ref_(arena).as_import_declaration().import_clause,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_import_clause(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_import_declaration().module_specifier),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_import_declaration().assert_clause,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_assert_clause(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::AssertClause => Some(
            factory.ref_(arena).update_assert_clause(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_assert_clause().elements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_assert_entry(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
                node.ref_(arena).as_assert_clause().multi_line,
            ),
        ),
        SyntaxKind::AssertEntry => Some(
            factory.ref_(arena).update_assert_entry(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_assert_entry().name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_assertion_key(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_assert_entry().value),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_string_literal(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ImportClause => Some(factory.ref_(arena).update_import_clause(
            node,
            node.ref_(arena).as_import_clause().is_type_only,
            released!(node_visitor(
                node.ref_(arena).as_import_clause().name,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                None,
            ))?,
            released!(node_visitor(
                node.ref_(arena).as_import_clause().named_bindings,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_named_import_bindings(&node.ref_(arena))),
                None,
            ))?,
        )),
        SyntaxKind::NamespaceImport => Some(
            factory.ref_(arena).update_namespace_import(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_namespace_import().name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::NamespaceExport => Some(
            factory.ref_(arena).update_namespace_export(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_namespace_export().name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::NamedImports => Some(
            factory.ref_(arena).update_named_imports(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_named_imports().elements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_import_specifier(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ImportSpecifier => Some(
            factory.ref_(arena).update_import_specifier(
                node,
                node.ref_(arena).as_import_specifier().is_type_only,
                released!(node_visitor(
                    node.ref_(arena).as_import_specifier().property_name,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_import_specifier().name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ExportAssignment => Some(
            factory.ref_(arena).update_export_assignment(
                node,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_decorators(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(nodes_visitor(
                    node.ref_(arena).maybe_modifiers(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_export_assignment().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ExportDeclaration => Some(factory.ref_(arena).update_export_declaration(
            node,
            released!(nodes_visitor(
                node.ref_(arena).maybe_decorators(),
                Some(&mut |node: Id<Node>| visitor(node)),
                Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                None,
                None,
            ))?,
            released!(nodes_visitor(
                node.ref_(arena).maybe_modifiers(),
                Some(&mut |node: Id<Node>| visitor(node)),
                Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                None,
                None,
            ))?,
            node.ref_(arena).as_export_declaration().is_type_only,
            released!(node_visitor(
                node.ref_(arena).as_export_declaration().export_clause,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_named_export_bindings(&node.ref_(arena))),
                None,
            ))?,
            released!(node_visitor(
                node.ref_(arena).as_export_declaration().module_specifier,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_expression(node, arena)),
                None,
            ))?,
            released!(node_visitor(
                node.ref_(arena).as_export_declaration().assert_clause,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_assert_clause(&node.ref_(arena))),
                None,
            ))?,
        )),
        SyntaxKind::NamedExports => Some(
            factory.ref_(arena).update_named_exports(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_named_exports().elements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_export_specifier(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ExportSpecifier => Some(
            factory.ref_(arena).update_export_specifier(
                node,
                node.ref_(arena).as_export_specifier().is_type_only,
                released!(node_visitor(
                    node.ref_(arena).as_export_specifier().property_name,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_export_specifier().name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ExternalModuleReference => Some(
            factory.ref_(arena).update_external_module_reference(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_external_module_reference().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::JsxElement => Some(
            factory.ref_(arena).update_jsx_element(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_jsx_element().opening_element),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_jsx_opening_element(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_jsx_element().children),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_jsx_child(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_jsx_element().closing_element),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_jsx_closing_element(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::JsxSelfClosingElement => Some(
            factory.ref_(arena).update_jsx_self_closing_element(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_jsx_self_closing_element().tag_name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_jsx_tag_name_expression(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_jsx_self_closing_element()
                        .maybe_type_arguments(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_jsx_self_closing_element().attributes),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_jsx_attributes(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::JsxOpeningElement => Some(
            factory.ref_(arena).update_jsx_opening_element(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_jsx_opening_element().tag_name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_jsx_tag_name_expression(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    node.ref_(arena)
                        .as_jsx_opening_element()
                        .maybe_type_arguments(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_jsx_opening_element().attributes),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_jsx_attributes(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::JsxClosingElement => Some(
            factory.ref_(arena).update_jsx_closing_element(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_jsx_closing_element().tag_name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_jsx_tag_name_expression(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::JsxFragment => Some(
            factory.ref_(arena).update_jsx_fragment(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_jsx_fragment().opening_fragment),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_jsx_opening_fragment(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_jsx_fragment().children),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_jsx_child(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_jsx_fragment().closing_fragment),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_jsx_closing_fragment(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::JsxAttribute => Some(
            factory.ref_(arena).update_jsx_attribute(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_jsx_attribute().name),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_jsx_attribute().maybe_initializer(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_string_literal_or_jsx_expression(&node.ref_(arena))),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::JsxAttributes => Some(
            factory.ref_(arena).update_jsx_attributes(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_jsx_attributes().properties),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_jsx_attribute_like(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::JsxSpreadAttribute => Some(
            factory.ref_(arena).update_jsx_spread_attribute(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_jsx_spread_attribute().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::JsxExpression => Some(factory.ref_(arena).update_jsx_expression(
            node,
            released!(node_visitor(
                node.ref_(arena).as_jsx_expression().expression,
                Some(&mut visitor),
                Some(&|node: Id<Node>| is_expression(node, arena)),
                None,
            ))?,
        )),
        SyntaxKind::CaseClause => Some(
            factory.ref_(arena).update_case_clause(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_case_clause().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_case_clause().statements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_statement(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::DefaultClause => Some(
            factory.ref_(arena).update_default_clause(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_default_clause().statements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_statement(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::HeritageClause => Some(
            factory.ref_(arena).update_heritage_clause(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_heritage_clause().types),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_expression_with_type_arguments(&node.ref_(arena))),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::CatchClause => Some(
            factory.ref_(arena).update_catch_clause(
                node,
                released!(node_visitor(
                    node.ref_(arena).as_catch_clause().variable_declaration,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_variable_declaration(&node.ref_(arena))),
                    None,
                ))?,
                released!(node_visitor(
                    Some(node.ref_(arena).as_catch_clause().block),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_block(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::PropertyAssignment => Some(
            factory.ref_(arena).update_property_assignment(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_property_assignment().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    Some(node.ref_(arena).as_property_assignment().initializer),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::ShorthandPropertyAssignment => Some(
            factory.ref_(arena).update_shorthand_property_assignment(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_shorthand_property_assignment().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena)
                        .as_shorthand_property_assignment()
                        .object_assignment_initializer,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::SpreadAssignment => Some(
            factory.ref_(arena).update_spread_assignment(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_spread_assignment().expression),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::EnumMember => Some(
            factory.ref_(arena).update_enum_member(
                node,
                released!(node_visitor(
                    Some(node.ref_(arena).as_enum_member().name()),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                    None,
                ))?
                .unwrap(),
                released!(node_visitor(
                    node.ref_(arena).as_enum_member().maybe_initializer(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?,
            ),
        ),
        SyntaxKind::SourceFile => Some(factory.ref_(arena).update_source_file(
            node,
            try_visit_lexical_environment(
                node.ref_(arena).as_source_file().statements(),
                |node: Id<Node>| visitor(node),
                context,
                arena,
            )?,
            None,
            None,
            None,
            None,
            None,
        )),
        SyntaxKind::PartiallyEmittedExpression => Some(
            factory.ref_(arena).update_partially_emitted_expression(
                node,
                released!(node_visitor(
                    Some(
                        node.ref_(arena)
                            .as_partially_emitted_expression()
                            .expression,
                    ),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                ))?
                .unwrap(),
            ),
        ),
        SyntaxKind::CommaListExpression => Some(
            factory.ref_(arena).update_comma_list_expression(
                node,
                released!(nodes_visitor(
                    Some(node.ref_(arena).as_comma_list_expression().elements),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                    None,
                ))?
                .unwrap(),
            ),
        ),
        _ => Some(node),
    })
}
