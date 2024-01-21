use std::{borrow::Borrow, io};

use gc::Gc;
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
    is_type_parameter_declaration, is_variable_declaration, is_variable_declaration_list,
    return_ok_default_if_none, try_maybe_visit_node, try_maybe_visit_nodes,
    ClassLikeDeclarationInterface, FunctionLikeDeclarationInterface, HasInitializerInterface,
    HasMembersInterface, HasQuestionTokenInterface, HasStatementsInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface,
    InterfaceOrClassLikeDeclarationInterface, NamedDeclarationInterface, Node, NodeArray,
    NodeFlags, NodeInterface, SignatureDeclarationInterface, SyntaxKind, TransformationContext,
    VisitResult,
    HasArena, InArena,
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
                Option<&NodeArray>,
                Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                Option<&dyn Fn(Id<Node>) -> bool>,
                Option<usize>,
                Option<usize>,
            ) -> io::Result<Option<Gc<NodeArray>>>,
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
            Option<&NodeArray>,
            Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
            Option<&dyn Fn(Id<Node>) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> io::Result<Option<Gc<NodeArray>>>,
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
        move |nodes: Option<&NodeArray>,
              visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
              test: Option<&dyn Fn(Id<Node>) -> bool>,
              start: Option<usize>,
              count: Option<usize>|
              -> io::Result<Option<Gc<NodeArray>>> {
            if let Some(nodes_visitor) = nodes_visitor.as_mut() {
                nodes_visitor(nodes, visitor, test, start, count)
            } else {
                try_maybe_visit_nodes(nodes, visitor, test, start, count)
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
        SyntaxKind::Identifier => {
            let node_ref = node.ref_(arena);
            let node_as_identifier = node_ref.as_identifier();
            Some(factory.update_identifier(
                node,
                nodes_visitor(
                    node_as_identifier.maybe_type_arguments().as_deref(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_type_node_or_type_parameter_declaration(&node.ref_(arena))),
                    None,
                    None,
                )?,
            ))
        }
        SyntaxKind::QualifiedName => {
            let node_ref = node.ref_(arena);
            let node_as_qualified_name = node_ref.as_qualified_name();
            Some(
                factory.update_qualified_name(
                    node,
                    node_visitor(
                        Some(node_as_qualified_name.left),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_entity_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_qualified_name.right),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ComputedPropertyName => {
            let node_ref = node.ref_(arena);
            let node_as_computed_property_name = node_ref.as_computed_property_name();
            Some(
                factory.update_computed_property_name(
                    node,
                    node_visitor(
                        Some(node_as_computed_property_name.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeParameter => {
            let node_ref = node.ref_(arena);
            let node_as_type_parameter_declaration = node_ref.as_type_parameter_declaration();
            Some(
                factory.update_type_parameter_declaration(
                    node,
                    node_visitor(
                        Some(node_as_type_parameter_declaration.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_type_parameter_declaration.constraint,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_type_parameter_declaration.default,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::Parameter => {
            let node_ref = node.ref_(arena);
            let node_as_parameter_declaration = node_ref.as_parameter_declaration();
            Some(
                factory.update_parameter_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_parameter_declaration.dot_dot_dot_token,
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_dot_dot_dot_token(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_parameter_declaration.maybe_name(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_binding_name(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_parameter_declaration
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
                    )?,
                    node_visitor(
                        node_as_parameter_declaration.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_parameter_declaration.maybe_initializer(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::Decorator => {
            let node_as_decorator = node.as_decorator();
            Some(
                factory.update_decorator(
                    node,
                    node_visitor(
                        Some(node_as_decorator.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::PropertySignature => {
            let node_ref = node.ref_(arena);
            let node_as_property_signature = node_ref.as_property_signature();
            Some(
                factory.update_property_signature(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_property_signature.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_property_signature.maybe_question_token(),
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&is_token),
                        None,
                    )?,
                    node_visitor(
                        node_as_property_signature.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::PropertyDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_property_declaration = node_ref.as_property_declaration();
            Some(
                factory.update_property_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_property_declaration.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_property_declaration
                            .maybe_question_token()
                            .or(node_as_property_declaration.exclamation_token),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_question_or_exclamation_token(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_property_declaration.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_property_declaration.maybe_initializer(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::MethodSignature => {
            let node_ref = node.ref_(arena);
            let node_as_method_signature = node_ref.as_method_signature();
            Some(
                factory.update_method_signature(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_method_signature.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_method_signature.maybe_question_token(),
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_question_token(&node.ref_(arena))),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_method_signature.maybe_type_parameters().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_method_signature.parameters()),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_method_signature.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::MethodDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_method_declaration = node_ref.as_method_declaration();
            Some(
                factory.update_method_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_method_declaration.maybe_asterisk_token(),
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_asterisk_token(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_method_declaration.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_method_declaration.maybe_question_token(),
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_question_token(&node.ref_(arena))),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_method_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    try_visit_parameter_list_full(
                        Some(&node_as_method_declaration.parameters()),
                        |node: Id<Node>| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(Id<Node>) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_method_declaration.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                    try_visit_function_body_full(
                        node_as_method_declaration.maybe_body(),
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
                    )?,
                ),
            )
        }
        SyntaxKind::Constructor => {
            let node_ref = node.ref_(arena);
            let node_as_constructor_declaration = node_ref.as_constructor_declaration();
            Some(
                factory.update_constructor_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    try_visit_parameter_list_full(
                        Some(&node_as_constructor_declaration.parameters()),
                        |node: Id<Node>| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(Id<Node>) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    try_visit_function_body_full(
                        node_as_constructor_declaration.maybe_body(),
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
                    )?,
                )
            )
        }
        SyntaxKind::GetAccessor => {
            let node_ref = node.ref_(arena);
            let node_as_get_accessor_declaration = node_ref.as_get_accessor_declaration();
            Some(
                factory.update_get_accessor_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_get_accessor_declaration.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    try_visit_parameter_list_full(
                        Some(&node_as_get_accessor_declaration.parameters()),
                        |node: Id<Node>| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(Id<Node>) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_get_accessor_declaration.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                    try_visit_function_body_full(
                        node_as_get_accessor_declaration.maybe_body(),
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
                    )?,
                )
            )
        }
        SyntaxKind::SetAccessor => {
            let node_ref = node.ref_(arena);
            let node_as_set_accessor_declaration = node_ref.as_set_accessor_declaration();
            Some(
                factory.update_set_accessor_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_set_accessor_declaration.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    try_visit_parameter_list_full(
                        Some(&node_as_set_accessor_declaration.parameters()),
                        |node: Id<Node>| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(Id<Node>) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    try_visit_function_body_full(
                        node_as_set_accessor_declaration.maybe_body(),
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
                    )?,
                )
            )
        }
        SyntaxKind::ClassStaticBlockDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_class_static_block_declaration = node_ref.as_class_static_block_declaration();
            context.start_lexical_environment();
            context.suspend_lexical_environment();
            Some(
                factory.update_class_static_block_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    try_visit_function_body_full(
                        Some(node_as_class_static_block_declaration.body),
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
                    )?.unwrap(),
                )
            )
        }
        SyntaxKind::CallSignature => {
            let node_ref = node.ref_(arena);
            let node_as_call_signature_declaration = node_ref.as_call_signature_declaration();
            Some(
                factory.update_call_signature(
                    node,
                    nodes_visitor(
                        node_as_call_signature_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_call_signature_declaration.parameters()),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_call_signature_declaration.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::ConstructSignature => {
            let node_ref = node.ref_(arena);
            let node_as_construct_signature_declaration = node_ref.as_construct_signature_declaration();
            Some(
                factory.update_construct_signature(
                    node,
                    nodes_visitor(
                        node_as_construct_signature_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_construct_signature_declaration.parameters()),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_construct_signature_declaration
                            .maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::IndexSignature => {
            let node_ref = node.ref_(arena);
            let node_as_index_signature_declaration = node_ref.as_index_signature_declaration();
            Some(
                factory.update_index_signature(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_index_signature_declaration.parameters()),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_index_signature_declaration.maybe_type().unwrap()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypePredicate => {
            let node_as_type_predicate_node = node.ref_(arena).as_type_predicate_node();
            Some(
                factory.update_type_predicate_node(
                    node,
                    node_visitor(
                        node_as_type_predicate_node.asserts_modifier,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_asserts_keyword(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_type_predicate_node.parameter_name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier_or_this_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_type_predicate_node.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::TypeReference => {
            let node_ref = node.ref_(arena);
            let node_as_type_reference_node = node_ref.as_type_reference_node();
            Some(
                factory.update_type_reference_node(
                    node,
                    node_visitor(
                        Some(node_as_type_reference_node.type_name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_entity_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_type_reference_node
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::FunctionType => {
            let node_ref = node.ref_(arena);
            let node_as_function_type_node = node_ref.as_function_type_node();
            Some(
                factory.update_function_type_node(
                    node,
                    nodes_visitor(
                        node_as_function_type_node
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_function_type_node.parameters()),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_function_type_node.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::ConstructorType => {
            let node_ref = node.ref_(arena);
            let node_as_constructor_type_node = node_ref.as_constructor_type_node();
            Some(
                factory.update_constructor_type_node(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_constructor_type_node
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_constructor_type_node.parameters()),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_constructor_type_node.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::TypeQuery => {
            let node_ref = node.ref_(arena);
            let node_as_type_query_node = node_ref.as_type_query_node();
            Some(
                factory.update_type_query_node(
                    node,
                    node_visitor(
                        Some(node_as_type_query_node.expr_name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_entity_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeLiteral => {
            let node_ref = node.ref_(arena);
            let node_as_type_literal_node = node_ref.as_type_literal_node();
            Some(
                factory.update_type_literal_node(
                    node,
                    nodes_visitor(
                        Some(&node_as_type_literal_node.members),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_element(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ArrayType => {
            let node_ref = node.ref_(arena);
            let node_as_array_type_node = node_ref.as_array_type_node();
            Some(
                factory.update_array_type_node(
                    node,
                    node_visitor(
                        Some(&node_as_array_type_node.element_type),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TupleType => {
            let node_ref = node.ref_(arena);
            let node_as_tuple_type_node = node_ref.as_tuple_type_node();
            Some(
                factory.update_tuple_type_node(
                    node,
                    nodes_visitor(
                        Some(&node_as_tuple_type_node.elements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::OptionalType => {
            let node_ref = node.ref_(arena);
            let node_as_optional_type_node = node_ref.as_optional_type_node();
            Some(
                factory.update_optional_type_node(
                    node,
                    node_visitor(
                        Some(node_as_optional_type_node.type_),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::RestType => {
            let node_ref = node.ref_(arena);
            let node_as_rest_type_node = node_ref.as_rest_type_node();
            Some(
                factory.update_rest_type_node(
                    node,
                    node_visitor(
                        Some(node_as_rest_type_node.type_),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::UnionType => {
            let node_ref = node.ref_(arena);
            let node_as_union_type_node = node_ref.as_union_type_node();
            Some(
                factory.update_union_type_node(
                    node,
                    nodes_visitor(
                        Some(&node_as_union_type_node.types),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::IntersectionType => {
            let node_ref = node.ref_(arena);
            let node_as_intersection_type_node = node_ref.as_intersection_type_node();
            Some(
                factory.update_intersection_type_node(
                    node,
                    nodes_visitor(
                        Some(&node_as_intersection_type_node.types),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ConditionalType => {
            let node_ref = node.ref_(arena);
            let node_as_conditional_type_node = node_ref.as_conditional_type_node();
            Some(
                factory.update_conditional_type_node(
                    node,
                    node_visitor(
                        Some(node_as_conditional_type_node.check_type),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_conditional_type_node.extends_type),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_conditional_type_node.true_type),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_conditional_type_node.false_type),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::InferType => {
            let node_ref = node.ref_(arena);
            let node_as_infer_type_node = node_ref.as_infer_type_node();
            Some(
                factory.update_infer_type_node(
                    node,
                    node_visitor(
                        Some(node_as_infer_type_node.type_parameter),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ImportType => {
            let node_ref = node.ref_(arena);
            let node_as_import_type_node = node_ref.as_import_type_node();
            Some(
                factory.update_import_type_node(
                    node,
                    node_visitor(
                        Some(node_as_import_type_node.argument),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_import_type_node.qualifier,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_entity_name(&node.ref_(arena))),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_import_type_node.maybe_type_arguments().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    Some(node_as_import_type_node.is_type_of()),
                ),
            )
        }
        SyntaxKind::NamedTupleMember => {
            let node_ref = node.ref_(arena);
            let node_as_named_tuple_member = node_ref.as_named_tuple_member();
            Some(
                factory.update_named_tuple_member(
                    node,
                    node_visitor(
                        node_as_named_tuple_member.dot_dot_dot_token,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_dot_dot_dot_token(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_named_tuple_member.name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_named_tuple_member.question_token,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_question_token(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_named_tuple_member.type_),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ParenthesizedType => {
            let node_ref = node.ref_(arena);
            let node_as_parenthesized_type_node = node_ref.as_parenthesized_type_node();
            Some(
                factory.update_parenthesized_type(
                    node,
                    node_visitor(
                        Some(node_as_parenthesized_type_node.type_),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeOperator => {
            let node_ref = node.ref_(arena);
            let node_as_type_operator_node = node_ref.as_type_operator_node();
            Some(
                factory.update_type_operator_node(
                    node,
                    node_visitor(
                        Some(node_as_type_operator_node.type_),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::IndexedAccessType => {
            let node_ref = node.ref_(arena);
            let node_as_indexed_access_type_node = node_ref.as_indexed_access_type_node();
            Some(
                factory.update_indexed_access_type_node(
                    node,
                    node_visitor(
                        Some(node_as_indexed_access_type_node.object_type),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_indexed_access_type_node.index_type),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::MappedType => {
            let node_ref = node.ref_(arena);
            let node_as_mapped_type_node = node_ref.as_mapped_type_node();
            Some(
                factory.update_mapped_type_node(
                    node,
                    node_visitor(
                        node_as_mapped_type_node.readonly_token,
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_readonly_keyword_or_plus_or_minus_token(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_mapped_type_node.type_parameter),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_mapped_type_node.name_type,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_mapped_type_node.question_token,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_question_or_plus_or_minus_token(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_mapped_type_node.type_,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_mapped_type_node.members.as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_element(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::LiteralType => {
            let node_ref = node.ref_(arena);
            let node_as_literal_type_node = node_ref.as_literal_type_node();
            Some(
                factory.update_literal_type_node(
                    node,
                    node_visitor(
                        Some(node_as_literal_type_node.literal),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TemplateLiteralType => {
            let node_ref = node.ref_(arena);
            let node_as_template_literal_type_node = node_ref.as_template_literal_type_node();
            Some(
                factory.update_template_literal_type(
                    node,
                    node_visitor(
                        Some(node_as_template_literal_type_node.head),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_template_head(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_template_literal_type_node.template_spans),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_template_literal_type_span(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TemplateLiteralTypeSpan => {
            let node_ref = node.ref_(arena);
            let node_as_template_literal_type_span = node_ref.as_template_literal_type_span();
            Some(
                factory.update_template_literal_type_span(
                    node,
                    node_visitor(
                        Some(node_as_template_literal_type_span.type_),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_template_literal_type_span.literal),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_template_middle_or_template_tail(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ObjectBindingPattern => {
            let node_ref = node.ref_(arena);
            let node_as_object_binding_pattern = node_ref.as_object_binding_pattern();
            Some(
                factory.update_object_binding_pattern(
                    node,
                    nodes_visitor(
                        Some(&node_as_object_binding_pattern.elements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_binding_element(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ArrayBindingPattern => {
            let node_ref = node.ref_(arena);
            let node_as_array_binding_pattern = node_ref.as_array_binding_pattern();
            Some(
                factory.update_array_binding_pattern(
                    node,
                    nodes_visitor(
                        Some(&node_as_array_binding_pattern.elements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_array_binding_element(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::BindingElement => {
            let node_ref = node.ref_(arena);
            let node_as_binding_element = node_ref.as_binding_element();
            Some(
                factory.update_binding_element(
                    node,
                    node_visitor(
                        node_as_binding_element.dot_dot_dot_token,
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_dot_dot_dot_token(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_binding_element.property_name,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_binding_element.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_binding_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_binding_element.maybe_initializer(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::ArrayLiteralExpression => {
            let node_ref = node.ref_(arena);
            let node_as_array_literal_expression = node_ref.as_array_literal_expression();
            Some(
                factory.update_array_literal_expression(
                    node,
                    nodes_visitor(
                        Some(&node_as_array_literal_expression.elements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ObjectLiteralExpression => {
            let .as_object_literal_expression_ref = .as_object_literal_expression.ref_(arena);
            let node_as_object_literal_expression = node_ref.as_object_literal_expression();
            Some(
                factory.update_object_literal_expression(
                    node,
                    nodes_visitor(
                        Some(&node_as_object_literal_expression.properties),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_object_literal_element_like(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::PropertyAccessExpression => {
            let node_ref = node.ref_(arena);
            let node_as_property_access_expression = node_ref.as_property_access_expression();
            if node.ref_(arena).flags().intersects(NodeFlags::OptionalChain) {
                return Ok(Some(
                    factory.update_property_access_chain(
                        node,
                        node_visitor(
                            Some(node_as_property_access_expression.expression),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                        )?
                        .unwrap(),
                        node_visitor(
                            node_as_property_access_expression
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
                        )?,
                        node_visitor(
                            Some(node_as_property_access_expression.name()),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_member_name(&node.ref_(arena))),
                            None,
                        )?
                        .unwrap(),
                    ),
                ));
            }
            Some(
                factory.update_property_access_expression(
                    node,
                    node_visitor(
                        Some(node_as_property_access_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_property_access_expression.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_member_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ElementAccessExpression => {
            let node_ref = node.ref_(arena);
            let node_as_element_access_expression = node_ref.as_element_access_expression();
            if node.ref_(arena).flags().intersects(NodeFlags::OptionalChain) {
                return Ok(Some(
                    factory.update_element_access_chain(
                        node,
                        node_visitor(
                            Some(node_as_element_access_expression.expression),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                        )?
                        .unwrap(),
                        node_visitor(
                            node_as_element_access_expression
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
                        )?,
                        node_visitor(
                            Some(node_as_element_access_expression.argument_expression),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                        )?
                        .unwrap(),
                    ),
                ));
            }
            Some(
                factory.update_element_access_expression(
                    node,
                    node_visitor(
                        Some(node_as_element_access_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_element_access_expression.argument_expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::CallExpression => {
            let node_ref = node.ref_(arena);
            let node_as_call_expression = node_ref.as_call_expression();
            if node.ref_(arena).flags().intersects(NodeFlags::OptionalChain) {
                return Ok(Some(
                    factory.update_call_chain(
                        node,
                        node_visitor(
                            Some(node_as_call_expression.expression),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                        )?
                        .unwrap(),
                        node_visitor(
                            node_as_call_expression.question_dot_token,
                            Some(&mut |node: Id<Node>| {
                                Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                    token_visitor(node)?
                                } else {
                                    Some(node.into())
                                })
                            }),
                            Some(&|node: Id<Node>| is_question_dot_token(&node.ref_(arena))),
                            None,
                        )?,
                        nodes_visitor(
                            node_as_call_expression.maybe_type_arguments().as_deref(),
                            Some(&mut |node: Id<Node>| visitor(node)),
                            Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                            None,
                            None,
                        )?,
                        nodes_visitor(
                            Some(&node_as_call_expression.arguments),
                            Some(&mut |node: Id<Node>| visitor(node)),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                            None,
                        )?
                        .unwrap(),
                    ),
                ));
            }
            Some(
                factory.update_call_expression(
                    node,
                    node_visitor(
                        Some(node_as_call_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_call_expression.maybe_type_arguments().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_call_expression.arguments),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::NewExpression => {
            let node_ref = node.ref_(arena);
            let node_as_new_expression = node_ref.as_new_expression();
            Some(
                factory.update_new_expression(
                    node,
                    node_visitor(
                        Some(node_as_new_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_new_expression.maybe_type_arguments().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_new_expression.arguments.as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::TaggedTemplateExpression => {
            let node_ref = node.ref_(arena);
            let node_as_tagged_template_expression = node_ref.as_tagged_template_expression();
            Some(
                factory.update_tagged_template_expression(
                    node,
                    node_visitor(
                        Some(node_as_tagged_template_expression.tag),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_tagged_template_expression
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_tagged_template_expression.template),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_template_literal(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeAssertionExpression => {
            let node_ref = node.ref_(arena);
            let node_as_type_assertion = node_ref.as_type_assertion();
            Some(
                factory.update_type_assertion(
                    node,
                    node_visitor(
                        Some(node_as_type_assertion.type_),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_type_assertion.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ParenthesizedExpression => {
            let node_ref = node.ref_(arena);
            let node_as_parenthesized_expression = node_ref.as_parenthesized_expression();
            Some(
                factory.update_parenthesized_expression(
                    node,
                    node_visitor(
                        Some(node_as_parenthesized_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::FunctionExpression => {
            let node_ref = node.ref_(arena);
            let node_as_function_expression = node_ref.as_function_expression();
            Some(
                factory.update_function_expression(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_function_expression.maybe_asterisk_token(),
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_asterisk_token(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_function_expression.maybe_name(),
                        Some(&mut visitor),
                        Some(&ref_(arena).is_identifier),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_function_expression.maybe_type_parameters().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    try_visit_parameter_list_full(
                        Some(&node_as_function_expression.parameters()),
                        |node: Id<Node>| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(Id<Node>) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_function_expression.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                    try_visit_function_body_full(
                        Some(node_as_function_expression.maybe_body().unwrap()),
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
                    )?.unwrap(),
                )
            )
        }
        SyntaxKind::ArrowFunction => {
            let node_ref = node.ref_(arena);
            let node_as_arrow_function = node_ref.as_arrow_function();
            Some(
                factory.update_arrow_function(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_arrow_function.maybe_type_parameters().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&ref_(arena).is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    try_visit_parameter_list_full(
                        Some(&node_as_arrow_function.parameters()),
                        |node: Id<Node>| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(Id<Node>) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_arrow_function.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_arrow_function.equals_greater_than_token),
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_equals_greater_than_token(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    try_visit_function_body_full(
                        Some(node_as_arrow_function.maybe_body().unwrap()),
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
                    )?.unwrap(),
                )
            )
        }
        SyntaxKind::DeleteExpression => {
            let node_ref = node.ref_(arena);
            let node_as_delete_expression = node_ref.as_delete_expression();
            Some(
                factory.update_delete_expression(
                    node,
                    node_visitor(
                        Some(node_as_delete_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeOfExpression => {
            let node_ref = node.ref_(arena);
            let node_as_type_of_expression = node_ref.as_type_of_expression();
            Some(
                factory.update_type_of_expression(
                    node,
                    node_visitor(
                        Some(node_as_type_of_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::VoidExpression => {
            let node_ref = node.ref_(arena);
            let node_as_void_expression = node_ref.as_void_expression();
            Some(
                factory.update_void_expression(
                    node,
                    node_visitor(
                        Some(node_as_void_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::AwaitExpression => {
            let node_ref = node.ref_(arena);
            let node_as_await_expression = node_ref.as_await_expression();
            Some(
                factory.update_await_expression(
                    node,
                    node_visitor(
                        Some(node_as_await_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::PrefixUnaryExpression => {
            let node_ref = node.ref_(arena);
            let node_as_prefix_unary_expression = node_ref.as_prefix_unary_expression();
            Some(
                factory.update_prefix_unary_expression(
                    node,
                    node_visitor(
                        Some(node_as_prefix_unary_expression.operand),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::PostfixUnaryExpression => {
            let node_ref = node.ref_(arena);
            let node_as_postfix_unary_expression = node_ref.as_postfix_unary_expression();
            Some(
                factory.update_postfix_unary_expression(
                    node,
                    node_visitor(
                        Some(node_as_postfix_unary_expression.operand),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::BinaryExpression => {
            let node_ref = node.ref_(arena);
            let node_as_binary_expression = node_ref.as_binary_expression();
            Some(
                factory.update_binary_expression(
                    node,
                    node_visitor(
                        Some(node_as_binary_expression.left),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_binary_expression.operator_token),
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_binary_operator_token(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_binary_expression.right),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ConditionalExpression => {
            let node_ref = node.ref_(arena);
            let node_as_conditional_expression = node_ref.as_conditional_expression();
            Some(
                factory.update_conditional_expression(
                    node,
                    node_visitor(
                        Some(node_as_conditional_expression.condition),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_conditional_expression.question_token),
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_question_token(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_conditional_expression.when_true),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_conditional_expression.colon_token),
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_colon_token(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_conditional_expression.when_false),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TemplateExpression => {
            let node_ref = node.ref_(arena);
            let node_as_template_expression = node_ref.as_template_expression();
            Some(
                factory.update_template_expression(
                    node,
                    node_visitor(
                        Some(node_as_template_expression.head),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_template_head(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_template_expression.template_spans),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_template_span(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::YieldExpression => {
            let node_ref = node.ref_(arena);
            let node_as_yield_expression = node_ref.as_yield_expression();
            Some(factory.update_yield_expression(
                node,
                node_visitor(
                    node_as_yield_expression.asterisk_token.as_deref(),
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_asterisk_token(&node.ref_(arena))),
                    None,
                )?,
                node_visitor(
                    node_as_yield_expression.expression,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(&node.ref_(arena))),
                    None,
                )?,
            ))
        }
        SyntaxKind::SpreadElement => {
            let node_ref = node.ref_(arena);
            let node_as_spread_element = node_ref.as_spread_element();
            Some(
                factory.update_spread_element(
                    node,
                    node_visitor(
                        Some(node_as_spread_element.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ClassExpression => {
            let node_ref = node.ref_(arena);
            let node_as_class_expression = node_ref.as_class_expression();
            Some(
                factory.update_class_expression(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_class_expression.maybe_name(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_class_expression.maybe_type_parameters().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_class_expression.maybe_heritage_clauses().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_heritage_clause(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_class_expression.members()),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_class_element(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExpressionWithTypeArguments => {
            let node_ref = node.ref_(arena);
            let node_as_expression_with_type_arguments = node_ref.as_expression_with_type_arguments();
            Some(
                factory.update_expression_with_type_arguments(
                    node,
                    node_visitor(
                        Some(node_as_expression_with_type_arguments.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_expression_with_type_arguments
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::AsExpression => {
            let node_ref = node.ref_(arena);
            let node_as_as_expression = node_ref.as_as_expression();
            Some(
                factory.update_as_expression(
                    node,
                    node_visitor(
                        Some(node_as_as_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_as_expression.type_),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::NonNullExpression => {
            let node_ref = node.ref_(arena);
            let node_as_non_null_expression = node_ref.as_non_null_expression();
            if node.ref_(arena).flags().intersects(NodeFlags::OptionalChain) {
                return Ok(Some(
                    factory.update_non_null_chain(
                        node,
                        node_visitor(
                            Some(node_as_non_null_expression.expression),
                            Some(&mut visitor),
                            Some(&|node: Id<Node>| is_expression(node, arena)),
                            None,
                        )?
                        .unwrap(),
                    ),
                ));
            }
            Some(
                factory.update_non_null_expression(
                    node,
                    node_visitor(
                        Some(node_as_non_null_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::MetaProperty => {
            let node_ref = node.ref_(arena);
            let node_as_meta_property = node_ref.as_meta_property();
            Some(
                factory.update_meta_property(
                    node,
                    node_visitor(
                        Some(node_as_meta_property.name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TemplateSpan => {
            let node_ref = node.ref_(arena);
            let node_as_template_span = node_ref.as_template_span();
            Some(
                factory.update_template_span(
                    node,
                    node_visitor(
                        Some(node_as_template_span.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_template_span.literal),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_template_middle_or_template_tail(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::Block => {
            let node_ref = node.ref_(arena);
            let node_as_block = node_ref.as_block();
            Some(
                factory.update_block(
                    node,
                    nodes_visitor(
                        Some(&node_as_block.statements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_statement(node, arena)),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::VariableStatement => {
            let node_ref = node.ref_(arena);
            let node_as_variable_statement = node_ref.as_variable_statement();
            Some(
                factory.update_variable_statement(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_variable_statement.declaration_list),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_variable_declaration_list(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExpressionStatement => {
            let node_ref = node.ref_(arena);
            let node_as_expression_statement = node_ref.as_expression_statement();
            Some(
                factory.update_expression_statement(
                    node,
                    node_visitor(
                        Some(node_as_expression_statement.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::IfStatement => {
            let node_ref = node.ref_(arena);
            let node_as_if_statement = node_ref.as_if_statement();
            Some(
                factory.update_if_statement(
                    node,
                    node_visitor(
                        Some(node_as_if_statement.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_if_statement.then_statement),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_statement(node, arena)),
                        Some(&|nodes: &[Id<Node>]| factory.lift_to_block(nodes)),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_if_statement.else_statement,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_statement(node, arena)),
                        Some(&|nodes: &[Id<Node>]| factory.lift_to_block(nodes)),
                    )?,
                ),
            )
        }
        SyntaxKind::DoStatement => {
            let node_ref = node.ref_(arena);
            let node_as_do_statement = node_ref.as_do_statement();
            Some(
                factory.update_do_statement(
                    node,
                    try_visit_iteration_body(
                        node_as_do_statement.statement,
                        |node: Id<Node>| visitor(node),
                        context,
                    )?,
                    node_visitor(
                        Some(node_as_do_statement.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::WhileStatement => {
            let node_ref = node.ref_(arena);
            let node_as_while_statement = node_ref.as_while_statement();
            Some(
                factory.update_while_statement(
                    node,
                    node_visitor(
                        Some(node_as_while_statement.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    try_visit_iteration_body(
                        node_as_while_statement.statement,
                        |node: Id<Node>| visitor(node),
                        context,
                    )?,
                ),
            )
        }
        SyntaxKind::ForStatement => {
            let node_ref = node.ref_(arena);
            let node_as_for_statement = node_ref.as_for_statement();
            Some(factory.update_for_statement(
                node,
                node_visitor(
                    node_as_for_statement.initializer,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_for_initializer(node, arena)),
                    None,
                )?,
                node_visitor(
                    node_as_for_statement.condition,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                )?,
                node_visitor(
                    node_as_for_statement.incrementor,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                )?,
                try_visit_iteration_body(
                    node_as_for_statement.statement,
                    |node: Id<Node>| visitor(node),
                    context,
                )?,
            ))
        }
        SyntaxKind::ForInStatement => {
            let node_ref = node.ref_(arena);
            let node_as_for_in_statement = node_ref.as_for_in_statement();
            Some(
                factory.update_for_in_statement(
                    node,
                    node_visitor(
                        Some(node_as_for_in_statement.initializer),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_for_initializer(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_for_in_statement.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    try_visit_iteration_body(
                        node_as_for_in_statement.statement,
                        |node: Id<Node>| visitor(node),
                        context,
                    )?,
                ),
            )
        }
        SyntaxKind::ForOfStatement => {
            let node_ref = node.ref_(arena);
            let node_as_for_of_statement = node_ref.as_for_of_statement();
            Some(
                factory.update_for_of_statement(
                    node,
                    node_visitor(
                        node_as_for_of_statement.await_modifier,
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_await_keyword(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_for_of_statement.initializer),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_for_initializer(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_for_of_statement.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    try_visit_iteration_body(
                        node_as_for_of_statement.statement,
                        |node: Id<Node>| visitor(node),
                        context,
                    )?,
                ),
            )
        }
        SyntaxKind::ContinueStatement => {
            let node_ref = node.ref_(arena);
            let node_as_continue_statement = node_ref.as_continue_statement();
            Some(factory.update_continue_statement(
                node,
                node_visitor(
                    node_as_continue_statement.label,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                )?,
            ))
        }
        SyntaxKind::BreakStatement => {
            let node_ref = node.ref_(arena);
            let node_as_break_statement = node_ref.as_break_statement();
            Some(factory.update_break_statement(
                node,
                node_visitor(
                    node_as_break_statement.label,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                )?,
            ))
        }
        SyntaxKind::ReturnStatement => {
            let node_ref = node.ref_(arena);
            let node_as_return_statement = node_ref.as_return_statement();
            Some(factory.update_return_statement(
                node,
                node_visitor(
                    node_as_return_statement.expression,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                )?,
            ))
        }
        SyntaxKind::WithStatement => {
            let node_ref = node.ref_(arena);
            let node_as_with_statement = node_ref.as_with_statement();
            Some(
                factory.update_with_statement(
                    node,
                    node_visitor(
                        Some(node_as_with_statement.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_with_statement.statement),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_statement(node, arena)),
                        Some(&|nodes: &[Id<Node>]| factory.lift_to_block(nodes)),
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::SwitchStatement => {
            let node_ref = node.ref_(arena);
            let node_as_switch_statement = node_ref.as_switch_statement();
            Some(
                factory.update_switch_statement(
                    node,
                    node_visitor(
                        Some(node_as_switch_statement.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_switch_statement.case_block),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_case_block(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::LabeledStatement => {
            let node_ref = node.ref_(arena);
            let node_as_labeled_statement = node_ref.as_labeled_statement();
            Some(
                factory.update_labeled_statement(
                    node,
                    node_visitor(
                        Some(node_as_labeled_statement.label),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_labeled_statement.statement),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_statement(node, arena)),
                        Some(&|nodes: &[Id<Node>]| factory.lift_to_block(nodes)),
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ThrowStatement => {
            let node_ref = node.ref_(arena);
            let node_as_throw_statement = node_ref.as_throw_statement();
            Some(
                factory.update_throw_statement(
                    node,
                    node_visitor(
                        Some(node_as_throw_statement.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TryStatement => {
            let node_ref = node.ref_(arena);
            let node_as_try_statement = node_ref.as_try_statement();
            Some(
                factory.update_try_statement(
                    node,
                    node_visitor(
                        Some(node_as_try_statement.try_block),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_block(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_try_statement.catch_clause,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_catch_clause(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_try_statement.finally_block,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_block(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::VariableDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_variable_declaration = node_ref.as_variable_declaration();
            Some(factory.update_variable_declaration(
                node,
                node_visitor(
                    node_as_variable_declaration.maybe_name(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_binding_name(&node.ref_(arena))),
                    None,
                )?,
                node_visitor(
                    node_as_variable_declaration.exclamation_token,
                    Some(&mut |node: Id<Node>| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            Some(node.into())
                        })
                    }),
                    Some(&|node: Id<Node>| is_exclamation_token(&node.ref_(arena))),
                    None,
                )?,
                node_visitor(
                    node_as_variable_declaration.maybe_type(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                    None,
                )?,
                node_visitor(
                    node_as_variable_declaration.maybe_initializer(),
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                )?,
            ))
        }
        SyntaxKind::VariableDeclarationList => {
            let node_ref = node.ref_(arena);
            let node_as_variable_declaration_list = node_ref.as_variable_declaration_list();
            Some(
                factory.update_variable_declaration_list(
                    node,
                    nodes_visitor(
                        Some(&node_as_variable_declaration_list.declarations),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_variable_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::FunctionDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_function_declaration = node_ref.as_function_declaration();
            Some(
                factory.update_function_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_function_declaration.maybe_asterisk_token(),
                        Some(&mut |node: Id<Node>| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                Some(node.into())
                            })
                        }),
                        Some(&|node: Id<Node>| is_asterisk_token(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        node_as_function_declaration.maybe_name(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_function_declaration.maybe_type_parameters().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    try_visit_parameter_list_full(
                        Some(&node_as_function_declaration.parameters()),
                        |node: Id<Node>| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(Id<Node>) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_function_declaration.maybe_type(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?,
                    try_visit_function_body_full(
                        node_as_function_declaration.maybe_body(),
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
                    )?,
                )
            )
        }
        SyntaxKind::ClassDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_class_declaration = node_ref.as_class_declaration();
            Some(
                factory.update_class_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_class_declaration.maybe_name(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_class_declaration.maybe_type_parameters().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_class_declaration
                            .maybe_heritage_clauses()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_heritage_clause(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_class_declaration.members()),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_class_element(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::InterfaceDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_interface_declaration = node_ref.as_interface_declaration();
            Some(
                factory.update_interface_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_interface_declaration.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_interface_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_interface_declaration
                            .maybe_heritage_clauses()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_heritage_clause(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_interface_declaration.members()),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_element(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeAliasDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_type_alias_declaration = node_ref.as_type_alias_declaration();
            Some(
                factory.update_type_alias_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_type_alias_declaration.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_type_alias_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_parameter_declaration(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_type_alias_declaration.maybe_type().unwrap()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::EnumDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_enum_declaration = node_ref.as_enum_declaration();
            Some(
                factory.update_enum_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_enum_declaration.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_enum_declaration.members),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_enum_member(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::ModuleDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_module_declaration = node_ref.as_module_declaration();
            Some(
                factory.update_module_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_module_declaration.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_module_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_module_declaration.body,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_module_body(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::ModuleBlock => {
            let node_ref = node.ref_(arena);
            let node_as_module_block = node_ref.as_module_block();
            Some(
                factory.update_module_block(
                    node,
                    nodes_visitor(
                        Some(&node_as_module_block.statements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_statement(node, arena)),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::CaseBlock => {
            let node_ref = node.ref_(arena);
            let node_as_case_block = node_ref.as_case_block();
            Some(
                factory.update_case_block(
                    node,
                    nodes_visitor(
                        Some(&node_as_case_block.clauses),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_case_or_default_clause(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::NamespaceExportDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_namespace_export_declaration = node_ref.as_namespace_export_declaration();
            Some(
                factory.update_namespace_export_declaration(
                    node,
                    node_visitor(
                        Some(node_as_namespace_export_declaration.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ImportEqualsDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_import_equals_declaration = node_ref.as_import_equals_declaration();
            Some(
                factory.update_import_equals_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_as_import_equals_declaration.is_type_only,
                    node_visitor(
                        Some(node_as_import_equals_declaration.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_import_equals_declaration.module_reference),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_module_reference(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ImportDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_import_declaration = node_ref.as_import_declaration();
            Some(
                factory.update_import_declaration(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_import_declaration.import_clause,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_import_clause(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_import_declaration.module_specifier),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_import_declaration.assert_clause,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_assert_clause(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::AssertClause => {
            let node_ref = node.ref_(arena);
            let node_as_assert_clause = node_ref.as_assert_clause();
            Some(
                factory.update_assert_clause(
                    node,
                    nodes_visitor(
                        Some(&node_as_assert_clause.elements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_assert_entry(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_as_assert_clause.multi_line,
                ),
            )
        }
        SyntaxKind::AssertEntry => {
            let node_ref = node.ref_(arena);
            let node_as_assert_entry = node_ref.as_assert_entry();
            Some(
                factory.update_assert_entry(
                    node,
                    node_visitor(
                        Some(node_as_assert_entry.name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_assertion_key(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_assert_entry.value),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_string_literal(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ImportClause => {
            let node_ref = node.ref_(arena);
            let node_as_import_clause = node_ref.as_import_clause();
            Some(factory.update_import_clause(
                node,
                node_as_import_clause.is_type_only,
                node_visitor(
                    node_as_import_clause.name,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                    None,
                )?,
                node_visitor(
                    node_as_import_clause.named_bindings,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_named_import_bindings(&node.ref_(arena))),
                    None,
                )?,
            ))
        }
        SyntaxKind::NamespaceImport => {
            let node_ref = node.ref_(arena);
            let node_as_namespace_import = node_ref.as_namespace_import();
            Some(
                factory.update_namespace_import(
                    node,
                    node_visitor(
                        Some(node_as_namespace_import.name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::NamespaceExport => {
            let node_ref = node.ref_(arena);
            let node_as_namespace_export = node_ref.as_namespace_export();
            Some(
                factory.update_namespace_export(
                    node,
                    node_visitor(
                        Some(node_as_namespace_export.name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::NamedImports => {
            let node_ref = node.ref_(arena);
            let node_as_named_imports = node_ref.as_named_imports();
            Some(
                factory.update_named_imports(
                    node,
                    nodes_visitor(
                        Some(&node_as_named_imports.elements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_import_specifier(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ImportSpecifier => {
            let node_ref = node.ref_(arena);
            let node_as_import_specifier = node_ref.as_import_specifier();
            Some(
                factory.update_import_specifier(
                    node,
                    node_as_import_specifier.is_type_only,
                    node_visitor(
                        node_as_import_specifier.property_name,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_import_specifier.name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExportAssignment => {
            let node_ref = node.ref_(arena);
            let node_as_export_assignment = node_ref.as_export_assignment();
            Some(
                factory.update_export_assignment(
                    node,
                    nodes_visitor(
                        node.ref_(arena).maybe_decorators().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.ref_(arena).maybe_modifiers().as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_export_assignment.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExportDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_export_declaration = node_ref.as_export_declaration();
            Some(factory.update_export_declaration(
                node,
                nodes_visitor(
                    node.ref_(arena).maybe_decorators().as_deref(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_decorator(&node.ref_(arena))),
                    None,
                    None,
                )?,
                nodes_visitor(
                    node.ref_(arena).maybe_modifiers().as_deref(),
                    Some(&mut |node: Id<Node>| visitor(node)),
                    Some(&|node: Id<Node>| is_modifier(&node.ref_(arena))),
                    None,
                    None,
                )?,
                node_as_export_declaration.is_type_only,
                node_visitor(
                    node_as_export_declaration.export_clause,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_named_export_bindings(&node.ref_(arena))),
                    None,
                )?,
                node_visitor(
                    node_as_export_declaration.module_specifier,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                )?,
                node_visitor(
                    node_as_export_declaration.assert_clause,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_assert_clause(&node.ref_(arena))),
                    None,
                )?,
            ))
        }
        SyntaxKind::NamedExports => {
            let node_ref = node.ref_(arena);
            let node_as_named_exports = node_ref.as_named_exports();
            Some(
                factory.update_named_exports(
                    node,
                    nodes_visitor(
                        Some(&node_as_named_exports.elements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_export_specifier(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExportSpecifier => {
            let node_ref = node.ref_(arena);
            let node_as_export_specifier = node_ref.as_export_specifier();
            Some(
                factory.update_export_specifier(
                    node,
                    node_as_export_specifier.is_type_only,
                    node_visitor(
                        node_as_export_specifier.property_name,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_export_specifier.name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExternalModuleReference => {
            let node_ref = node.ref_(arena);
            let node_as_external_module_reference = node_ref.as_external_module_reference();
            Some(
                factory.update_external_module_reference(
                    node,
                    node_visitor(
                        Some(node_as_external_module_reference.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxElement => {
            let node_ref = node.ref_(arena);
            let node_as_jsx_element = node_ref.as_jsx_element();
            Some(
                factory.update_jsx_element(
                    node,
                    node_visitor(
                        Some(node_as_jsx_element.opening_element),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_jsx_opening_element(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_jsx_element.children),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_jsx_child(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_jsx_element.closing_element),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_jsx_closing_element(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxSelfClosingElement => {
            let node_ref = node.ref_(arena);
            let node_as_jsx_self_closing_element = node_ref.as_jsx_self_closing_element();
            Some(
                factory.update_jsx_self_closing_element(
                    node,
                    node_visitor(
                        Some(node_as_jsx_self_closing_element.tag_name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_jsx_tag_name_expression(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_jsx_self_closing_element
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_jsx_self_closing_element.attributes),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_jsx_attributes(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxOpeningElement => {
            let node_ref = node.ref_(arena);
            let node_as_jsx_opening_element = node_ref.as_jsx_opening_element();
            Some(
                factory.update_jsx_opening_element(
                    node,
                    node_visitor(
                        Some(node_as_jsx_opening_element.tag_name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_jsx_tag_name_expression(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_jsx_opening_element
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_type_node(&node.ref_(arena))),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_jsx_opening_element.attributes),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_jsx_attributes(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxClosingElement => {
            let node_ref = node.ref_(arena);
            let node_as_jsx_closing_element = node_ref.as_jsx_closing_element();
            Some(
                factory.update_jsx_closing_element(
                    node,
                    node_visitor(
                        Some(node_as_jsx_closing_element.tag_name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_jsx_tag_name_expression(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxFragment => {
            let node_ref = node.ref_(arena);
            let node_as_jsx_fragment = node_ref.as_jsx_fragment();
            Some(
                factory.update_jsx_fragment(
                    node,
                    node_visitor(
                        Some(node_as_jsx_fragment.opening_fragment),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_jsx_opening_fragment(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_jsx_fragment.children),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_jsx_child(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_jsx_fragment.closing_fragment),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_jsx_closing_fragment(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxAttribute => {
            let node_ref = node.ref_(arena);
            let node_as_jsx_attribute = node_ref.as_jsx_attribute();
            Some(
                factory.update_jsx_attribute(
                    node,
                    node_visitor(
                        Some(node_as_jsx_attribute.name),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_jsx_attribute.maybe_initializer(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_string_literal_or_jsx_expression(&node.ref_(arena))),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::JsxAttributes => {
            let node_ref = node.ref_(arena);
            let node_as_jsx_attributes = node_ref.as_jsx_attributes();
            Some(
                factory.update_jsx_attributes(
                    node,
                    nodes_visitor(
                        Some(&node_as_jsx_attributes.properties),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_jsx_attribute_like(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxSpreadAttribute => {
            let node_ref = node.ref_(arena);
            let node_as_jsx_spread_attribute = node_ref.as_jsx_spread_attribute();
            Some(
                factory.update_jsx_spread_attribute(
                    node,
                    node_visitor(
                        Some(node_as_jsx_spread_attribute.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxExpression => {
            let node_ref = node.ref_(arena);
            let node_as_jsx_expression = node_ref.as_jsx_expression();
            Some(factory.update_jsx_expression(
                node,
                node_visitor(
                    node_as_jsx_expression.expression,
                    Some(&mut visitor),
                    Some(&|node: Id<Node>| is_expression(node, arena)),
                    None,
                )?,
            ))
        }
        SyntaxKind::CaseClause => {
            let node_ref = node.ref_(arena);
            let node_as_case_clause = node_ref.as_case_clause();
            Some(
                factory.update_case_clause(
                    node,
                    node_visitor(
                        Some(node_as_case_clause.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_case_clause.statements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_statement(node, arena)),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::DefaultClause => {
            let node_ref = node.ref_(arena);
            let node_as_default_clause = node_ref.as_default_clause();
            Some(
                factory.update_default_clause(
                    node,
                    nodes_visitor(
                        Some(&node_as_default_clause.statements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_statement(node, arena)),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::HeritageClause => {
            let node_ref = node.ref_(arena);
            let node_as_heritage_clause = node_ref.as_heritage_clause();
            Some(
                factory.update_heritage_clause(
                    node,
                    nodes_visitor(
                        Some(&node_as_heritage_clause.types),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_expression_with_type_arguments(&node.ref_(arena))),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::CatchClause => {
            let node_ref = node.ref_(arena);
            let node_as_catch_clause = node_ref.as_catch_clause();
            Some(
                factory.update_catch_clause(
                    node,
                    node_visitor(
                        node_as_catch_clause.variable_declaration,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_variable_declaration(&node.ref_(arena))),
                        None,
                    )?,
                    node_visitor(
                        Some(node_as_catch_clause.block),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_block(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::PropertyAssignment => {
            let node_ref = node.ref_(arena);
            let node_as_property_assignment = node_ref.as_property_assignment();
            Some(
                factory.update_property_assignment(
                    node,
                    node_visitor(
                        Some(node_as_property_assignment.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(node_as_property_assignment.initializer),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ShorthandPropertyAssignment => {
            let node_ref = node.ref_(arena);
            let node_as_shorthand_property_assignment = node_ref.as_shorthand_property_assignment();
            Some(
                factory.update_shorthand_property_assignment(
                    node,
                    node_visitor(
                        Some(node_as_shorthand_property_assignment.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_identifier(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_shorthand_property_assignment
                            .object_assignment_initializer,
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::SpreadAssignment => {
            let node_ref = node.ref_(arena);
            let node_as_spread_assignment = node_ref.as_spread_assignment();
            Some(
                factory.update_spread_assignment(
                    node,
                    node_visitor(
                        Some(node_as_spread_assignment.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::EnumMember => {
            let node_ref = node.ref_(arena);
            let node_as_enum_member = node_ref.as_enum_member();
            Some(
                factory.update_enum_member(
                    node,
                    node_visitor(
                        Some(node_as_enum_member.name()),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_property_name(&node.ref_(arena))),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_enum_member.maybe_initializer(),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::SourceFile => {
            let node_ref = node.ref_(arena);
            let node_as_source_file = node_ref.as_source_file();
            Some(factory.update_source_file(
                node,
                try_visit_lexical_environment(
                    &node_as_source_file.statements(),
                    |node: Id<Node>| visitor(node),
                    context,
                )?,
                None,
                None,
                None,
                None,
                None,
            ))
        }
        SyntaxKind::PartiallyEmittedExpression => {
            let node_ref = node.ref_(arena);
            let node_as_partially_emitted_expression = node_ref.as_partially_emitted_expression();
            Some(
                factory.update_partially_emitted_expression(
                    node,
                    node_visitor(
                        Some(node_as_partially_emitted_expression.expression),
                        Some(&mut visitor),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::CommaListExpression => {
            let node_ref = node.ref_(arena);
            let node_as_comma_list_expression = node_ref.as_comma_list_expression();
            Some(
                factory.update_comma_list_expression(
                    node,
                    nodes_visitor(
                        Some(&node_as_comma_list_expression.elements),
                        Some(&mut |node: Id<Node>| visitor(node)),
                        Some(&|node: Id<Node>| is_expression(node, arena)),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        _ => Some(node),
    })
}
