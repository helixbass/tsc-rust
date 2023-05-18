use std::{borrow::Borrow, io};

use gc::Gc;

use super::{
    try_visit_function_body_full, try_visit_iteration_body, try_visit_lexical_environment,
    try_visit_node, try_visit_nodes, try_visit_parameter_list_full,
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
    return_ok_default_if_none, ClassLikeDeclarationInterface, FunctionLikeDeclarationInterface,
    HasInitializerInterface, HasMembersInterface, HasQuestionTokenInterface,
    HasStatementsInterface, HasTypeArgumentsInterface, HasTypeInterface,
    HasTypeParametersInterface, InterfaceOrClassLikeDeclarationInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeFlags, NodeInterface,
    SignatureDeclarationInterface, SyntaxKind, TransformationContext, VisitResult,
};

pub fn try_visit_each_child(
    node: &Node,
    visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
) -> io::Result<Gc<Node>> {
    Ok(try_maybe_visit_each_child(Some(node), visitor, context)?.unwrap())
}

pub fn try_maybe_visit_each_child(
    node: Option<impl Borrow<Node>>,
    visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
) -> io::Result<Option<Gc<Node>>> {
    return try_maybe_visit_each_child_full(
        node,
        visitor,
        context,
        Option::<
            fn(
                Option<&NodeArray>,
                Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                Option<&dyn Fn(&Node) -> bool>,
                Option<usize>,
                Option<usize>,
            ) -> io::Result<Option<Gc<NodeArray>>>,
        >::None,
        Option::<fn(&Node) -> io::Result<VisitResult>>::None,
        Option::<
            fn(
                Option<&Node>,
                Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                Option<&dyn Fn(&Node) -> bool>,
                Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
            ) -> io::Result<Option<Gc<Node>>>,
        >::None,
    );
}

pub fn try_maybe_visit_each_child_full(
    node: Option<impl Borrow<Node>>,
    mut visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    mut nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> io::Result<Option<Gc<NodeArray>>>,
    >,
    token_visitor: Option<impl Fn(&Node) -> io::Result<VisitResult>>,
    mut node_visitor: Option<
        impl FnMut(
            Option<&Node>,
            Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
        ) -> io::Result<Option<Gc<Node>>>,
    >,
) -> io::Result<Option<Gc<Node>>> {
    let mut nodes_visitor =
        move |nodes: Option<&NodeArray>,
              visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
              test: Option<&dyn Fn(&Node) -> bool>,
              start: Option<usize>,
              count: Option<usize>|
              -> io::Result<Option<Gc<NodeArray>>> {
            if let Some(nodes_visitor) = nodes_visitor.as_mut() {
                nodes_visitor(nodes, visitor, test, start, count)
            } else {
                try_visit_nodes(nodes, visitor, test, start, count)
            }
        };
    let mut node_visitor =
        move |node: Option<&Node>,
              visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
              lift: Option<&dyn Fn(&Node) -> bool>,
              test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>|
              -> io::Result<Option<Gc<Node>>> {
            if let Some(node_visitor) = node_visitor.as_mut() {
                node_visitor(node, visitor, lift, test)
            } else {
                try_visit_node(node, visitor, lift, test)
            }
        };
    let node = return_ok_default_if_none!(node);
    let node: &Node = node.borrow();

    let kind = node.kind();

    if kind > SyntaxKind::FirstToken && kind <= SyntaxKind::LastToken
        || kind == SyntaxKind::ThisType
    {
        return Ok(Some(node.node_wrapper()));
    }

    let factory = context.factory();

    Ok(match kind {
        SyntaxKind::Identifier => {
            let node_as_identifier = node.as_identifier();
            Some(factory.update_identifier(
                node,
                nodes_visitor(
                    node_as_identifier.maybe_type_arguments().as_deref(),
                    Some(&mut |node: &Node| visitor(node)),
                    Some(&is_type_node_or_type_parameter_declaration),
                    None,
                    None,
                )?,
            ))
        }
        SyntaxKind::QualifiedName => {
            let node_as_qualified_name = node.as_qualified_name();
            Some(
                factory.update_qualified_name(
                    node,
                    node_visitor(
                        Some(&node_as_qualified_name.left),
                        Some(&mut visitor),
                        Some(&is_entity_name),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_qualified_name.right),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ComputedPropertyName => {
            let node_as_computed_property_name = node.as_computed_property_name();
            Some(
                factory.update_computed_property_name(
                    node,
                    node_visitor(
                        Some(&node_as_computed_property_name.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeParameter => {
            let node_as_type_parameter_declaration = node.as_type_parameter_declaration();
            Some(
                factory.update_type_parameter_declaration(
                    node,
                    node_visitor(
                        Some(&node_as_type_parameter_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_type_parameter_declaration.constraint.as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                    node_visitor(
                        node_as_type_parameter_declaration.default.as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::Parameter => {
            let node_as_parameter_declaration = node.as_parameter_declaration();
            Some(
                factory.update_parameter_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_parameter_declaration.dot_dot_dot_token.as_deref(),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_dot_dot_dot_token),
                        None,
                    )?,
                    node_visitor(
                        node_as_parameter_declaration.maybe_name().as_deref(),
                        Some(&mut visitor),
                        Some(&is_binding_name),
                        None,
                    )?,
                    node_visitor(
                        node_as_parameter_declaration
                            .maybe_question_token()
                            .as_deref(),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_question_token),
                        None,
                    )?,
                    node_visitor(
                        node_as_parameter_declaration.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                    node_visitor(
                        node_as_parameter_declaration.maybe_initializer().as_deref(),
                        Some(&mut visitor),
                        Some(&is_expression),
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
                        Some(&node_as_decorator.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::PropertySignature => {
            let node_as_property_signature = node.as_property_signature();
            Some(
                factory.update_property_signature(
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_property_signature.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_property_signature.maybe_question_token().as_deref(),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_token),
                        None,
                    )?,
                    node_visitor(
                        node_as_property_signature.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::PropertyDeclaration => {
            let node_as_property_declaration = node.as_property_declaration();
            Some(
                factory.update_property_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_property_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_property_declaration
                            .maybe_question_token()
                            .or_else(|| node_as_property_declaration.exclamation_token.clone())
                            .as_deref(),
                        Some(&mut visitor),
                        Some(&is_question_or_exclamation_token),
                        None,
                    )?,
                    node_visitor(
                        node_as_property_declaration.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                    node_visitor(
                        node_as_property_declaration.maybe_initializer().as_deref(),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::MethodSignature => {
            let node_as_method_signature = node.as_method_signature();
            Some(
                factory.update_method_signature(
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_method_signature.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_method_signature.maybe_question_token().as_deref(),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_question_token),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_method_signature.maybe_type_parameters().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_method_signature.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_method_signature.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::MethodDeclaration => {
            let node_as_method_declaration = node.as_method_declaration();
            Some(
                factory.update_method_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_method_declaration.maybe_asterisk_token().as_deref(),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_asterisk_token),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_method_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_method_declaration.maybe_question_token().as_deref(),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_question_token),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_method_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    try_visit_parameter_list_full(
                        Some(&node_as_method_declaration.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(&Node) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_method_declaration.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                    try_visit_function_body_full(
                        node_as_method_declaration.maybe_body().as_deref(),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            node: Option<&Node>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            lift: Option<&dyn Fn(&Node) -> bool>,
                            test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>
                        | {
                            node_visitor(node, visitor, lift, test)
                        }),
                    )?,
                ),
            )
        }
        SyntaxKind::Constructor => {
            let node_as_constructor_declaration = node.as_constructor_declaration();
            Some(
                factory.update_constructor_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    try_visit_parameter_list_full(
                        Some(&node_as_constructor_declaration.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(&Node) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    try_visit_function_body_full(
                        node_as_constructor_declaration.maybe_body().as_deref(),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            node: Option<&Node>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            lift: Option<&dyn Fn(&Node) -> bool>,
                            test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>
                        | {
                            node_visitor(node, visitor, lift, test)
                        }),
                    )?,
                )
            )
        }
        SyntaxKind::GetAccessor => {
            let node_as_get_accessor_declaration = node.as_get_accessor_declaration();
            Some(
                factory.update_get_accessor_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_get_accessor_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )?
                    .unwrap(),
                    try_visit_parameter_list_full(
                        Some(&node_as_get_accessor_declaration.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(&Node) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_get_accessor_declaration.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                    try_visit_function_body_full(
                        node_as_get_accessor_declaration.maybe_body().as_deref(),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            node: Option<&Node>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            lift: Option<&dyn Fn(&Node) -> bool>,
                            test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>
                        | {
                            node_visitor(node, visitor, lift, test)
                        }),
                    )?,
                )
            )
        }
        SyntaxKind::SetAccessor => {
            let node_as_set_accessor_declaration = node.as_set_accessor_declaration();
            Some(
                factory.update_set_accessor_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_set_accessor_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )?
                    .unwrap(),
                    try_visit_parameter_list_full(
                        Some(&node_as_set_accessor_declaration.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(&Node) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    try_visit_function_body_full(
                        node_as_set_accessor_declaration.maybe_body().as_deref(),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            node: Option<&Node>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            lift: Option<&dyn Fn(&Node) -> bool>,
                            test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>
                        | {
                            node_visitor(node, visitor, lift, test)
                        }),
                    )?,
                )
            )
        }
        SyntaxKind::ClassStaticBlockDeclaration => {
            let node_as_class_static_block_declaration = node.as_class_static_block_declaration();
            context.start_lexical_environment();
            context.suspend_lexical_environment();
            Some(
                factory.update_class_static_block_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    try_visit_function_body_full(
                        Some(&node_as_class_static_block_declaration.body),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            node: Option<&Node>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            lift: Option<&dyn Fn(&Node) -> bool>,
                            test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>
                        | {
                            node_visitor(node, visitor, lift, test)
                        }),
                    )?.unwrap(),
                )
            )
        }
        SyntaxKind::CallSignature => {
            let node_as_call_signature_declaration = node.as_call_signature_declaration();
            Some(
                factory.update_call_signature(
                    node,
                    nodes_visitor(
                        node_as_call_signature_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_call_signature_declaration.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_call_signature_declaration.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::ConstructSignature => {
            let node_as_construct_signature_declaration = node.as_construct_signature_declaration();
            Some(
                factory.update_construct_signature(
                    node,
                    nodes_visitor(
                        node_as_construct_signature_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_construct_signature_declaration.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_construct_signature_declaration
                            .maybe_type()
                            .as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::IndexSignature => {
            let node_as_index_signature_declaration = node.as_index_signature_declaration();
            Some(
                factory.update_index_signature(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_index_signature_declaration.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_index_signature_declaration.maybe_type().unwrap()),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypePredicate => {
            let node_as_type_predicate_node = node.as_type_predicate_node();
            Some(
                factory.update_type_predicate_node(
                    node,
                    node_visitor(
                        node_as_type_predicate_node.asserts_modifier.as_deref(),
                        Some(&mut visitor),
                        Some(&is_asserts_keyword),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_type_predicate_node.parameter_name),
                        Some(&mut visitor),
                        Some(&is_identifier_or_this_type_node),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_type_predicate_node.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::TypeReference => {
            let node_as_type_reference_node = node.as_type_reference_node();
            Some(
                factory.update_type_reference_node(
                    node,
                    node_visitor(
                        Some(&node_as_type_reference_node.type_name),
                        Some(&mut visitor),
                        Some(&is_entity_name),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_type_reference_node
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::FunctionType => {
            let node_as_function_type_node = node.as_function_type_node();
            Some(
                factory.update_function_type_node(
                    node,
                    nodes_visitor(
                        node_as_function_type_node
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_function_type_node.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_function_type_node.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::ConstructorType => {
            let node_as_constructor_type_node = node.as_constructor_type_node();
            Some(
                factory.update_constructor_type_node(
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_constructor_type_node
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_constructor_type_node.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_constructor_type_node.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::TypeQuery => {
            let node_as_type_query_node = node.as_type_query_node();
            Some(
                factory.update_type_query_node(
                    node,
                    node_visitor(
                        Some(&node_as_type_query_node.expr_name),
                        Some(&mut visitor),
                        Some(&is_entity_name),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeLiteral => {
            let node_as_type_literal_node = node.as_type_literal_node();
            Some(
                factory.update_type_literal_node(
                    node,
                    nodes_visitor(
                        Some(&node_as_type_literal_node.members),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_element),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ArrayType => {
            let node_as_array_type_node = node.as_array_type_node();
            Some(
                factory.update_array_type_node(
                    node,
                    node_visitor(
                        Some(&node_as_array_type_node.element_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TupleType => {
            let node_as_tuple_type_node = node.as_tuple_type_node();
            Some(
                factory.update_tuple_type_node(
                    node,
                    nodes_visitor(
                        Some(&node_as_tuple_type_node.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::OptionalType => {
            let node_as_optional_type_node = node.as_optional_type_node();
            Some(
                factory.update_optional_type_node(
                    node,
                    node_visitor(
                        Some(&node_as_optional_type_node.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::RestType => {
            let node_as_rest_type_node = node.as_rest_type_node();
            Some(
                factory.update_rest_type_node(
                    node,
                    node_visitor(
                        Some(&node_as_rest_type_node.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::UnionType => {
            let node_as_union_type_node = node.as_union_type_node();
            Some(
                factory.update_union_type_node(
                    node,
                    nodes_visitor(
                        Some(&node_as_union_type_node.types),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::IntersectionType => {
            let node_as_intersection_type_node = node.as_intersection_type_node();
            Some(
                factory.update_intersection_type_node(
                    node,
                    nodes_visitor(
                        Some(&node_as_intersection_type_node.types),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ConditionalType => {
            let node_as_conditional_type_node = node.as_conditional_type_node();
            Some(
                factory.update_conditional_type_node(
                    node,
                    node_visitor(
                        Some(&node_as_conditional_type_node.check_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_type_node.extends_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_type_node.true_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_type_node.false_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::InferType => {
            let node_as_infer_type_node = node.as_infer_type_node();
            Some(
                factory.update_infer_type_node(
                    node,
                    node_visitor(
                        Some(&node_as_infer_type_node.type_parameter),
                        Some(&mut visitor),
                        Some(&is_type_parameter_declaration),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ImportType => {
            let node_as_import_type_node = node.as_import_type_node();
            Some(
                factory.update_import_type_node(
                    node,
                    node_visitor(
                        Some(&node_as_import_type_node.argument),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_import_type_node.qualifier.as_deref(),
                        Some(&mut visitor),
                        Some(&is_entity_name),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_import_type_node.maybe_type_arguments().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )?,
                    Some(node_as_import_type_node.is_type_of()),
                ),
            )
        }
        SyntaxKind::NamedTupleMember => {
            let node_as_named_tuple_member = node.as_named_tuple_member();
            Some(
                factory.update_named_tuple_member(
                    node,
                    node_visitor(
                        node_as_named_tuple_member.dot_dot_dot_token.as_deref(),
                        Some(&mut visitor),
                        Some(&is_dot_dot_dot_token),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_named_tuple_member.name),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_named_tuple_member.question_token.as_deref(),
                        Some(&mut visitor),
                        Some(&is_question_token),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_named_tuple_member.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ParenthesizedType => {
            let node_as_parenthesized_type_node = node.as_parenthesized_type_node();
            Some(
                factory.update_parenthesized_type(
                    node,
                    node_visitor(
                        Some(&node_as_parenthesized_type_node.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeOperator => {
            let node_as_type_operator_node = node.as_type_operator_node();
            Some(
                factory.update_type_operator_node(
                    node,
                    node_visitor(
                        Some(&node_as_type_operator_node.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::IndexedAccessType => {
            let node_as_indexed_access_type_node = node.as_indexed_access_type_node();
            Some(
                factory.update_indexed_access_type_node(
                    node,
                    node_visitor(
                        Some(&node_as_indexed_access_type_node.object_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_indexed_access_type_node.index_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::MappedType => {
            let node_as_mapped_type_node = node.as_mapped_type_node();
            Some(
                factory.update_mapped_type_node(
                    node,
                    node_visitor(
                        node_as_mapped_type_node.readonly_token.as_deref(),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_readonly_keyword_or_plus_or_minus_token),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_mapped_type_node.type_parameter),
                        Some(&mut visitor),
                        Some(&is_type_parameter_declaration),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_mapped_type_node.name_type.as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                    node_visitor(
                        node_as_mapped_type_node.question_token.as_deref(),
                        Some(&mut visitor),
                        Some(&is_question_or_plus_or_minus_token),
                        None,
                    )?,
                    node_visitor(
                        node_as_mapped_type_node.type_.as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_mapped_type_node.members.as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_element),
                        None,
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::LiteralType => {
            let node_as_literal_type_node = node.as_literal_type_node();
            Some(
                factory.update_literal_type_node(
                    node,
                    node_visitor(
                        Some(&node_as_literal_type_node.literal),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TemplateLiteralType => {
            let node_as_template_literal_type_node = node.as_template_literal_type_node();
            Some(
                factory.update_template_literal_type(
                    node,
                    node_visitor(
                        Some(&node_as_template_literal_type_node.head),
                        Some(&mut visitor),
                        Some(&is_template_head),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_template_literal_type_node.template_spans),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_template_literal_type_span),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TemplateLiteralTypeSpan => {
            let node_as_template_literal_type_span = node.as_template_literal_type_span();
            Some(
                factory.update_template_literal_type_span(
                    node,
                    node_visitor(
                        Some(&node_as_template_literal_type_span.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_template_literal_type_span.literal),
                        Some(&mut visitor),
                        Some(&is_template_middle_or_template_tail),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ObjectBindingPattern => {
            let node_as_object_binding_pattern = node.as_object_binding_pattern();
            Some(
                factory.update_object_binding_pattern(
                    node,
                    nodes_visitor(
                        Some(&node_as_object_binding_pattern.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_binding_element),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ArrayBindingPattern => {
            let node_as_array_binding_pattern = node.as_array_binding_pattern();
            Some(
                factory.update_array_binding_pattern(
                    node,
                    nodes_visitor(
                        Some(&node_as_array_binding_pattern.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_array_binding_element),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::BindingElement => {
            let node_as_binding_element = node.as_binding_element();
            Some(
                factory.update_binding_element(
                    node,
                    node_visitor(
                        node_as_binding_element.dot_dot_dot_token.as_deref(),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_dot_dot_dot_token),
                        None,
                    )?,
                    node_visitor(
                        node_as_binding_element.property_name.as_deref(),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_binding_element.name()),
                        Some(&mut visitor),
                        Some(&is_binding_name),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_binding_element.maybe_initializer().as_deref(),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::ArrayLiteralExpression => {
            let node_as_array_literal_expression = node.as_array_literal_expression();
            Some(
                factory.update_array_literal_expression(
                    node,
                    nodes_visitor(
                        Some(&node_as_array_literal_expression.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_expression),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ObjectLiteralExpression => {
            let node_as_object_literal_expression = node.as_object_literal_expression();
            Some(
                factory.update_object_literal_expression(
                    node,
                    nodes_visitor(
                        Some(&node_as_object_literal_expression.properties),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_object_literal_element_like),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::PropertyAccessExpression => {
            let node_as_property_access_expression = node.as_property_access_expression();
            if node.flags().intersects(NodeFlags::OptionalChain) {
                return Ok(Some(
                    factory.update_property_access_chain(
                        node,
                        node_visitor(
                            Some(&node_as_property_access_expression.expression),
                            Some(&mut visitor),
                            Some(&is_expression),
                            None,
                        )?
                        .unwrap(),
                        node_visitor(
                            node_as_property_access_expression
                                .question_dot_token
                                .as_deref(),
                            Some(&mut |node: &Node| {
                                Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                    token_visitor(node)?
                                } else {
                                    None
                                })
                            }),
                            Some(&is_question_dot_token),
                            None,
                        )?,
                        node_visitor(
                            Some(&node_as_property_access_expression.name()),
                            Some(&mut visitor),
                            Some(&is_member_name),
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
                        Some(&node_as_property_access_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_property_access_expression.name()),
                        Some(&mut visitor),
                        Some(&is_member_name),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ElementAccessExpression => {
            let node_as_element_access_expression = node.as_element_access_expression();
            if node.flags().intersects(NodeFlags::OptionalChain) {
                return Ok(Some(
                    factory.update_element_access_chain(
                        node,
                        node_visitor(
                            Some(&node_as_element_access_expression.expression),
                            Some(&mut visitor),
                            Some(&is_expression),
                            None,
                        )?
                        .unwrap(),
                        node_visitor(
                            node_as_element_access_expression
                                .question_dot_token
                                .as_deref(),
                            Some(&mut |node: &Node| {
                                Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                    token_visitor(node)?
                                } else {
                                    None
                                })
                            }),
                            Some(&is_question_dot_token),
                            None,
                        )?,
                        node_visitor(
                            Some(&node_as_element_access_expression.argument_expression),
                            Some(&mut visitor),
                            Some(&is_expression),
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
                        Some(&node_as_element_access_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_element_access_expression.argument_expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::CallExpression => {
            let node_as_call_expression = node.as_call_expression();
            if node.flags().intersects(NodeFlags::OptionalChain) {
                return Ok(Some(
                    factory.update_call_chain(
                        node,
                        node_visitor(
                            Some(&node_as_call_expression.expression),
                            Some(&mut visitor),
                            Some(&is_expression),
                            None,
                        )?
                        .unwrap(),
                        node_visitor(
                            node_as_call_expression.question_dot_token.as_deref(),
                            Some(&mut |node: &Node| {
                                Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                    token_visitor(node)?
                                } else {
                                    None
                                })
                            }),
                            Some(&is_question_dot_token),
                            None,
                        )?,
                        nodes_visitor(
                            node_as_call_expression.maybe_type_arguments().as_deref(),
                            Some(&mut |node: &Node| visitor(node)),
                            Some(&is_type_node),
                            None,
                            None,
                        )?,
                        nodes_visitor(
                            Some(&node_as_call_expression.arguments),
                            Some(&mut |node: &Node| visitor(node)),
                            Some(&is_expression),
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
                        Some(&node_as_call_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_call_expression.maybe_type_arguments().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_call_expression.arguments),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_expression),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::NewExpression => {
            let node_as_new_expression = node.as_new_expression();
            Some(
                factory.update_new_expression(
                    node,
                    node_visitor(
                        Some(&node_as_new_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_new_expression.maybe_type_arguments().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_new_expression.arguments.as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_expression),
                        None,
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::TaggedTemplateExpression => {
            let node_as_tagged_template_expression = node.as_tagged_template_expression();
            Some(
                factory.update_tagged_template_expression(
                    node,
                    node_visitor(
                        Some(&node_as_tagged_template_expression.tag),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_tagged_template_expression
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_tagged_template_expression.template),
                        Some(&mut visitor),
                        Some(&is_template_literal),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeAssertionExpression => {
            let node_as_type_assertion = node.as_type_assertion();
            Some(
                factory.update_type_assertion(
                    node,
                    node_visitor(
                        Some(&node_as_type_assertion.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_type_assertion.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ParenthesizedExpression => {
            let node_as_parenthesized_expression = node.as_parenthesized_expression();
            Some(
                factory.update_parenthesized_expression(
                    node,
                    node_visitor(
                        Some(&node_as_parenthesized_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::FunctionExpression => {
            let node_as_function_expression = node.as_function_expression();
            Some(
                factory.update_function_expression(
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_function_expression.maybe_asterisk_token().as_deref(),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_asterisk_token),
                        None,
                    )?,
                    node_visitor(
                        node_as_function_expression.maybe_name().as_deref(),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_function_expression.maybe_type_parameters().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    try_visit_parameter_list_full(
                        Some(&node_as_function_expression.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(&Node) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_function_expression.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                    try_visit_function_body_full(
                        Some(&node_as_function_expression.maybe_body().unwrap()),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            node: Option<&Node>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            lift: Option<&dyn Fn(&Node) -> bool>,
                            test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>
                        | {
                            node_visitor(node, visitor, lift, test)
                        }),
                    )?.unwrap(),
                )
            )
        }
        SyntaxKind::ArrowFunction => {
            let node_as_arrow_function = node.as_arrow_function();
            Some(
                factory.update_arrow_function(
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_arrow_function.maybe_type_parameters().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    try_visit_parameter_list_full(
                        Some(&node_as_arrow_function.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(&Node) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_arrow_function.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_arrow_function.equals_greater_than_token),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_equals_greater_than_token),
                        None,
                    )?
                    .unwrap(),
                    try_visit_function_body_full(
                        Some(&node_as_arrow_function.maybe_body().unwrap()),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            node: Option<&Node>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            lift: Option<&dyn Fn(&Node) -> bool>,
                            test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>
                        | {
                            node_visitor(node, visitor, lift, test)
                        }),
                    )?.unwrap(),
                )
            )
        }
        SyntaxKind::DeleteExpression => {
            let node_as_delete_expression = node.as_delete_expression();
            Some(
                factory.update_delete_expression(
                    node,
                    node_visitor(
                        Some(&node_as_delete_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeOfExpression => {
            let node_as_type_of_expression = node.as_type_of_expression();
            Some(
                factory.update_type_of_expression(
                    node,
                    node_visitor(
                        Some(&node_as_type_of_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::VoidExpression => {
            let node_as_void_expression = node.as_void_expression();
            Some(
                factory.update_void_expression(
                    node,
                    node_visitor(
                        Some(&node_as_void_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::AwaitExpression => {
            let node_as_await_expression = node.as_await_expression();
            Some(
                factory.update_await_expression(
                    node,
                    node_visitor(
                        Some(&node_as_await_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::PrefixUnaryExpression => {
            let node_as_prefix_unary_expression = node.as_prefix_unary_expression();
            Some(
                factory.update_prefix_unary_expression(
                    node,
                    node_visitor(
                        Some(&node_as_prefix_unary_expression.operand),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::PostfixUnaryExpression => {
            let node_as_postfix_unary_expression = node.as_postfix_unary_expression();
            Some(
                factory.update_postfix_unary_expression(
                    node,
                    node_visitor(
                        Some(&node_as_postfix_unary_expression.operand),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::BinaryExpression => {
            let node_as_binary_expression = node.as_binary_expression();
            Some(
                factory.update_binary_expression(
                    node,
                    node_visitor(
                        Some(&node_as_binary_expression.left),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_binary_expression.operator_token),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_binary_operator_token),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_binary_expression.right),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ConditionalExpression => {
            let node_as_conditional_expression = node.as_conditional_expression();
            Some(
                factory.update_conditional_expression(
                    node,
                    node_visitor(
                        Some(&node_as_conditional_expression.condition),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_expression.question_token),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_question_token),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_expression.when_true),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_expression.colon_token),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_colon_token),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_expression.when_false),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TemplateExpression => {
            let node_as_template_expression = node.as_template_expression();
            Some(
                factory.update_template_expression(
                    node,
                    node_visitor(
                        Some(&node_as_template_expression.head),
                        Some(&mut visitor),
                        Some(&is_template_head),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_template_expression.template_spans),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_template_span),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::YieldExpression => {
            let node_as_yield_expression = node.as_yield_expression();
            Some(factory.update_yield_expression(
                node,
                node_visitor(
                    node_as_yield_expression.asterisk_token.as_deref(),
                    Some(&mut |node: &Node| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            None
                        })
                    }),
                    Some(&is_asterisk_token),
                    None,
                )?,
                node_visitor(
                    node_as_yield_expression.expression.as_deref(),
                    Some(&mut visitor),
                    Some(&is_expression),
                    None,
                )?,
            ))
        }
        SyntaxKind::SpreadElement => {
            let node_as_spread_element = node.as_spread_element();
            Some(
                factory.update_spread_element(
                    node,
                    node_visitor(
                        Some(&node_as_spread_element.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ClassExpression => {
            let node_as_class_expression = node.as_class_expression();
            Some(
                factory.update_class_expression(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_class_expression.maybe_name().as_deref(),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_class_expression.maybe_type_parameters().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_class_expression.maybe_heritage_clauses().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_heritage_clause),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_class_expression.members()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_class_element),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExpressionWithTypeArguments => {
            let node_as_expression_with_type_arguments = node.as_expression_with_type_arguments();
            Some(
                factory.update_expression_with_type_arguments(
                    node,
                    node_visitor(
                        Some(&node_as_expression_with_type_arguments.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_expression_with_type_arguments
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::AsExpression => {
            let node_as_as_expression = node.as_as_expression();
            Some(
                factory.update_as_expression(
                    node,
                    node_visitor(
                        Some(&node_as_as_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_as_expression.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::NonNullExpression => {
            let node_as_non_null_expression = node.as_non_null_expression();
            if node.flags().intersects(NodeFlags::OptionalChain) {
                return Ok(Some(
                    factory.update_non_null_chain(
                        node,
                        node_visitor(
                            Some(&node_as_non_null_expression.expression),
                            Some(&mut visitor),
                            Some(&is_expression),
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
                        Some(&node_as_non_null_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::MetaProperty => {
            let node_as_meta_property = node.as_meta_property();
            Some(
                factory.update_meta_property(
                    node,
                    node_visitor(
                        Some(&node_as_meta_property.name),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TemplateSpan => {
            let node_as_template_span = node.as_template_span();
            Some(
                factory.update_template_span(
                    node,
                    node_visitor(
                        Some(&node_as_template_span.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_template_span.literal),
                        Some(&mut visitor),
                        Some(&is_template_middle_or_template_tail),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::Block => {
            let node_as_block = node.as_block();
            Some(
                factory.update_block(
                    node,
                    nodes_visitor(
                        Some(&node_as_block.statements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_statement),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::VariableStatement => {
            let node_as_variable_statement = node.as_variable_statement();
            Some(
                factory.update_variable_statement(
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_variable_statement.declaration_list),
                        Some(&mut visitor),
                        Some(&is_variable_declaration_list),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExpressionStatement => {
            let node_as_expression_statement = node.as_expression_statement();
            Some(
                factory.update_expression_statement(
                    node,
                    node_visitor(
                        Some(&node_as_expression_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::IfStatement => {
            let node_as_if_statement = node.as_if_statement();
            Some(
                factory.update_if_statement(
                    node,
                    node_visitor(
                        Some(&node_as_if_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_if_statement.then_statement),
                        Some(&mut visitor),
                        Some(&is_statement),
                        Some(&|nodes: &[Gc<Node>]| factory.lift_to_block(nodes)),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_if_statement.else_statement.as_deref(),
                        Some(&mut visitor),
                        Some(&is_statement),
                        Some(&|nodes: &[Gc<Node>]| factory.lift_to_block(nodes)),
                    )?,
                ),
            )
        }
        SyntaxKind::DoStatement => {
            let node_as_do_statement = node.as_do_statement();
            Some(
                factory.update_do_statement(
                    node,
                    try_visit_iteration_body(
                        &node_as_do_statement.statement,
                        |node: &Node| visitor(node),
                        context,
                    )?,
                    node_visitor(
                        Some(&node_as_do_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::WhileStatement => {
            let node_as_while_statement = node.as_while_statement();
            Some(
                factory.update_while_statement(
                    node,
                    node_visitor(
                        Some(&node_as_while_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    try_visit_iteration_body(
                        &node_as_while_statement.statement,
                        |node: &Node| visitor(node),
                        context,
                    )?,
                ),
            )
        }
        SyntaxKind::ForStatement => {
            let node_as_for_statement = node.as_for_statement();
            Some(factory.update_for_statement(
                node,
                node_visitor(
                    node_as_for_statement.initializer.as_deref(),
                    Some(&mut visitor),
                    Some(&is_for_initializer),
                    None,
                )?,
                node_visitor(
                    node_as_for_statement.condition.as_deref(),
                    Some(&mut visitor),
                    Some(&is_expression),
                    None,
                )?,
                node_visitor(
                    node_as_for_statement.incrementor.as_deref(),
                    Some(&mut visitor),
                    Some(&is_expression),
                    None,
                )?,
                try_visit_iteration_body(
                    &node_as_for_statement.statement,
                    |node: &Node| visitor(node),
                    context,
                )?,
            ))
        }
        SyntaxKind::ForInStatement => {
            let node_as_for_in_statement = node.as_for_in_statement();
            Some(
                factory.update_for_in_statement(
                    node,
                    node_visitor(
                        Some(&node_as_for_in_statement.initializer),
                        Some(&mut visitor),
                        Some(&is_for_initializer),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_for_in_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    try_visit_iteration_body(
                        &node_as_for_in_statement.statement,
                        |node: &Node| visitor(node),
                        context,
                    )?,
                ),
            )
        }
        SyntaxKind::ForOfStatement => {
            let node_as_for_of_statement = node.as_for_of_statement();
            Some(
                factory.update_for_of_statement(
                    node,
                    node_visitor(
                        node_as_for_of_statement.await_modifier.as_deref(),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_await_keyword),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_for_of_statement.initializer),
                        Some(&mut visitor),
                        Some(&is_for_initializer),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_for_of_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    try_visit_iteration_body(
                        &node_as_for_of_statement.statement,
                        |node: &Node| visitor(node),
                        context,
                    )?,
                ),
            )
        }
        SyntaxKind::ContinueStatement => {
            let node_as_continue_statement = node.as_continue_statement();
            Some(factory.update_continue_statement(
                node,
                node_visitor(
                    node_as_continue_statement.label.as_deref(),
                    Some(&mut visitor),
                    Some(&is_identifier),
                    None,
                )?,
            ))
        }
        SyntaxKind::BreakStatement => {
            let node_as_break_statement = node.as_break_statement();
            Some(factory.update_break_statement(
                node,
                node_visitor(
                    node_as_break_statement.label.as_deref(),
                    Some(&mut visitor),
                    Some(&is_identifier),
                    None,
                )?,
            ))
        }
        SyntaxKind::ReturnStatement => {
            let node_as_return_statement = node.as_return_statement();
            Some(factory.update_return_statement(
                node,
                node_visitor(
                    node_as_return_statement.expression.as_deref(),
                    Some(&mut visitor),
                    Some(&is_expression),
                    None,
                )?,
            ))
        }
        SyntaxKind::WithStatement => {
            let node_as_with_statement = node.as_with_statement();
            Some(
                factory.update_with_statement(
                    node,
                    node_visitor(
                        Some(&node_as_with_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_with_statement.statement),
                        Some(&mut visitor),
                        Some(&is_statement),
                        Some(&|nodes: &[Gc<Node>]| factory.lift_to_block(nodes)),
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::SwitchStatement => {
            let node_as_switch_statement = node.as_switch_statement();
            Some(
                factory.update_switch_statement(
                    node,
                    node_visitor(
                        Some(&node_as_switch_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_switch_statement.case_block),
                        Some(&mut visitor),
                        Some(&is_case_block),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::LabeledStatement => {
            let node_as_labeled_statement = node.as_labeled_statement();
            Some(
                factory.update_labeled_statement(
                    node,
                    node_visitor(
                        Some(&node_as_labeled_statement.label),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_labeled_statement.statement),
                        Some(&mut visitor),
                        Some(&is_statement),
                        Some(&|nodes: &[Gc<Node>]| factory.lift_to_block(nodes)),
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ThrowStatement => {
            let node_as_throw_statement = node.as_throw_statement();
            Some(
                factory.update_throw_statement(
                    node,
                    node_visitor(
                        Some(&node_as_throw_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TryStatement => {
            let node_as_try_statement = node.as_try_statement();
            Some(
                factory.update_try_statement(
                    node,
                    node_visitor(
                        Some(&node_as_try_statement.try_block),
                        Some(&mut visitor),
                        Some(&is_block),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_try_statement.catch_clause.as_deref(),
                        Some(&mut visitor),
                        Some(&is_catch_clause),
                        None,
                    )?,
                    node_visitor(
                        node_as_try_statement.finally_block.as_deref(),
                        Some(&mut visitor),
                        Some(&is_block),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::VariableDeclaration => {
            let node_as_variable_declaration = node.as_variable_declaration();
            Some(factory.update_variable_declaration(
                node,
                node_visitor(
                    node_as_variable_declaration.maybe_name().as_deref(),
                    Some(&mut visitor),
                    Some(&is_binding_name),
                    None,
                )?,
                node_visitor(
                    node_as_variable_declaration.exclamation_token.as_deref(),
                    Some(&mut |node: &Node| {
                        Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                            token_visitor(node)?
                        } else {
                            None
                        })
                    }),
                    Some(&is_exclamation_token),
                    None,
                )?,
                node_visitor(
                    node_as_variable_declaration.maybe_type().as_deref(),
                    Some(&mut visitor),
                    Some(&is_type_node),
                    None,
                )?,
                node_visitor(
                    node_as_variable_declaration.maybe_initializer().as_deref(),
                    Some(&mut visitor),
                    Some(&is_expression),
                    None,
                )?,
            ))
        }
        SyntaxKind::VariableDeclarationList => {
            let node_as_variable_declaration_list = node.as_variable_declaration_list();
            Some(
                factory.update_variable_declaration_list(
                    node,
                    nodes_visitor(
                        Some(&node_as_variable_declaration_list.declarations),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_variable_declaration),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::FunctionDeclaration => {
            let node_as_function_declaration = node.as_function_declaration();
            Some(
                factory.update_function_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_function_declaration.maybe_asterisk_token().as_deref(),
                        Some(&mut |node: &Node| {
                            Ok(if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)?
                            } else {
                                None
                            })
                        }),
                        Some(&is_asterisk_token),
                        None,
                    )?,
                    node_visitor(
                        node_as_function_declaration.maybe_name().as_deref(),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_function_declaration.maybe_type_parameters().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    try_visit_parameter_list_full(
                        Some(&node_as_function_declaration.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            nodes: Option<&NodeArray>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(&Node) -> bool>,
                            start: Option<usize>,
                            count: Option<usize>
                        | {
                            nodes_visitor(nodes, visitor, test, start, count)
                        }),
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_function_declaration.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?,
                    try_visit_function_body_full(
                        node_as_function_declaration.maybe_body().as_deref(),
                        |node: &Node| visitor(node),
                        context,
                        Some(|
                            node: Option<&Node>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            lift: Option<&dyn Fn(&Node) -> bool>,
                            test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>
                        | {
                            node_visitor(node, visitor, lift, test)
                        }),
                    )?,
                )
            )
        }
        SyntaxKind::ClassDeclaration => {
            let node_as_class_declaration = node.as_class_declaration();
            Some(
                factory.update_class_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_class_declaration.maybe_name().as_deref(),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?,
                    nodes_visitor(
                        node_as_class_declaration.maybe_type_parameters().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_class_declaration
                            .maybe_heritage_clauses()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_heritage_clause),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_class_declaration.members()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_class_element),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::InterfaceDeclaration => {
            let node_as_interface_declaration = node.as_interface_declaration();
            Some(
                factory.update_interface_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_interface_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_interface_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node_as_interface_declaration
                            .maybe_heritage_clauses()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_heritage_clause),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        Some(&node_as_interface_declaration.members()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_element),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::TypeAliasDeclaration => {
            let node_as_type_alias_declaration = node.as_type_alias_declaration();
            Some(
                factory.update_type_alias_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_type_alias_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_type_alias_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_type_alias_declaration.maybe_type().unwrap()),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::EnumDeclaration => {
            let node_as_enum_declaration = node.as_enum_declaration();
            Some(
                factory.update_enum_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_enum_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_enum_declaration.members),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_enum_member),
                        None,
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::ModuleDeclaration => {
            let node_as_module_declaration = node.as_module_declaration();
            Some(
                factory.update_module_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_module_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_module_name),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_module_declaration.body.as_deref(),
                        Some(&mut visitor),
                        Some(&is_module_body),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::ModuleBlock => {
            let node_as_module_block = node.as_module_block();
            Some(
                factory.update_module_block(
                    node,
                    nodes_visitor(
                        Some(&node_as_module_block.statements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_statement),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::CaseBlock => {
            let node_as_case_block = node.as_case_block();
            Some(
                factory.update_case_block(
                    node,
                    nodes_visitor(
                        Some(&node_as_case_block.clauses),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_case_or_default_clause),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::NamespaceExportDeclaration => {
            let node_as_namespace_export_declaration = node.as_namespace_export_declaration();
            Some(
                factory.update_namespace_export_declaration(
                    node,
                    node_visitor(
                        Some(&node_as_namespace_export_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ImportEqualsDeclaration => {
            let node_as_import_equals_declaration = node.as_import_equals_declaration();
            Some(
                factory.update_import_equals_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_as_import_equals_declaration.is_type_only,
                    node_visitor(
                        Some(&node_as_import_equals_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_import_equals_declaration.module_reference),
                        Some(&mut visitor),
                        Some(&is_module_reference),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ImportDeclaration => {
            let node_as_import_declaration = node.as_import_declaration();
            Some(
                factory.update_import_declaration(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        node_as_import_declaration.import_clause.as_deref(),
                        Some(&mut visitor),
                        Some(&is_import_clause),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_import_declaration.module_specifier),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_import_declaration.assert_clause.as_deref(),
                        Some(&mut visitor),
                        Some(&is_assert_clause),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::AssertClause => {
            let node_as_assert_clause = node.as_assert_clause();
            Some(
                factory.update_assert_clause(
                    node,
                    nodes_visitor(
                        Some(&node_as_assert_clause.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_assert_entry),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_as_assert_clause.multi_line,
                ),
            )
        }
        SyntaxKind::AssertEntry => {
            let node_as_assert_entry = node.as_assert_entry();
            Some(
                factory.update_assert_entry(
                    node,
                    node_visitor(
                        Some(&node_as_assert_entry.name),
                        Some(&mut visitor),
                        Some(&is_assertion_key),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_assert_entry.value),
                        Some(&mut visitor),
                        Some(&is_string_literal),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ImportClause => {
            let node_as_import_clause = node.as_import_clause();
            Some(factory.update_import_clause(
                node,
                node_as_import_clause.is_type_only,
                node_visitor(
                    node_as_import_clause.name.as_deref(),
                    Some(&mut visitor),
                    Some(&is_identifier),
                    None,
                )?,
                node_visitor(
                    node_as_import_clause.named_bindings.as_deref(),
                    Some(&mut visitor),
                    Some(&is_named_import_bindings),
                    None,
                )?,
            ))
        }
        SyntaxKind::NamespaceImport => {
            let node_as_namespace_import = node.as_namespace_import();
            Some(
                factory.update_namespace_import(
                    node,
                    node_visitor(
                        Some(&node_as_namespace_import.name),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::NamespaceExport => {
            let node_as_namespace_export = node.as_namespace_export();
            Some(
                factory.update_namespace_export(
                    node,
                    node_visitor(
                        Some(&node_as_namespace_export.name),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::NamedImports => {
            let node_as_named_imports = node.as_named_imports();
            Some(
                factory.update_named_imports(
                    node,
                    nodes_visitor(
                        Some(&node_as_named_imports.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_import_specifier),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ImportSpecifier => {
            let node_as_import_specifier = node.as_import_specifier();
            Some(
                factory.update_import_specifier(
                    node,
                    node_as_import_specifier.is_type_only,
                    node_visitor(
                        node_as_import_specifier.property_name.as_deref(),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_import_specifier.name),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExportAssignment => {
            let node_as_export_assignment = node.as_export_assignment();
            Some(
                factory.update_export_assignment(
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    )?,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_export_assignment.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExportDeclaration => {
            let node_as_export_declaration = node.as_export_declaration();
            Some(factory.update_export_declaration(
                node,
                nodes_visitor(
                    node.maybe_decorators().as_deref(),
                    Some(&mut |node: &Node| visitor(node)),
                    Some(&is_decorator),
                    None,
                    None,
                )?,
                nodes_visitor(
                    node.maybe_modifiers().as_deref(),
                    Some(&mut |node: &Node| visitor(node)),
                    Some(&is_modifier),
                    None,
                    None,
                )?,
                node_as_export_declaration.is_type_only,
                node_visitor(
                    node_as_export_declaration.export_clause.as_deref(),
                    Some(&mut visitor),
                    Some(&is_named_export_bindings),
                    None,
                )?,
                node_visitor(
                    node_as_export_declaration.module_specifier.as_deref(),
                    Some(&mut visitor),
                    Some(&is_expression),
                    None,
                )?,
                node_visitor(
                    node_as_export_declaration.assert_clause.as_deref(),
                    Some(&mut visitor),
                    Some(&is_assert_clause),
                    None,
                )?,
            ))
        }
        SyntaxKind::NamedExports => {
            let node_as_named_exports = node.as_named_exports();
            Some(
                factory.update_named_exports(
                    node,
                    nodes_visitor(
                        Some(&node_as_named_exports.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_export_specifier),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExportSpecifier => {
            let node_as_export_specifier = node.as_export_specifier();
            Some(
                factory.update_export_specifier(
                    node,
                    node_as_export_specifier.is_type_only,
                    node_visitor(
                        node_as_export_specifier.property_name.as_deref(),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_export_specifier.name),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ExternalModuleReference => {
            let node_as_external_module_reference = node.as_external_module_reference();
            Some(
                factory.update_external_module_reference(
                    node,
                    node_visitor(
                        Some(&node_as_external_module_reference.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxElement => {
            let node_as_jsx_element = node.as_jsx_element();
            Some(
                factory.update_jsx_element(
                    node,
                    node_visitor(
                        Some(&node_as_jsx_element.opening_element),
                        Some(&mut visitor),
                        Some(&is_jsx_opening_element),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_jsx_element.children),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_jsx_child),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_jsx_element.closing_element),
                        Some(&mut visitor),
                        Some(&is_jsx_closing_element),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxSelfClosingElement => {
            let node_as_jsx_self_closing_element = node.as_jsx_self_closing_element();
            Some(
                factory.update_jsx_self_closing_element(
                    node,
                    node_visitor(
                        Some(&node_as_jsx_self_closing_element.tag_name),
                        Some(&mut visitor),
                        Some(&is_jsx_tag_name_expression),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_jsx_self_closing_element
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_jsx_self_closing_element.attributes),
                        Some(&mut visitor),
                        Some(&is_jsx_attributes),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxOpeningElement => {
            let node_as_jsx_opening_element = node.as_jsx_opening_element();
            Some(
                factory.update_jsx_opening_element(
                    node,
                    node_visitor(
                        Some(&node_as_jsx_opening_element.tag_name),
                        Some(&mut visitor),
                        Some(&is_jsx_tag_name_expression),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        node_as_jsx_opening_element
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_jsx_opening_element.attributes),
                        Some(&mut visitor),
                        Some(&is_jsx_attributes),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxClosingElement => {
            let node_as_jsx_closing_element = node.as_jsx_closing_element();
            Some(
                factory.update_jsx_closing_element(
                    node,
                    node_visitor(
                        Some(&node_as_jsx_closing_element.tag_name),
                        Some(&mut visitor),
                        Some(&is_jsx_tag_name_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxFragment => {
            let node_as_jsx_fragment = node.as_jsx_fragment();
            Some(
                factory.update_jsx_fragment(
                    node,
                    node_visitor(
                        Some(&node_as_jsx_fragment.opening_fragment),
                        Some(&mut visitor),
                        Some(&is_jsx_opening_fragment),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_jsx_fragment.children),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_jsx_child),
                        None,
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_jsx_fragment.closing_fragment),
                        Some(&mut visitor),
                        Some(&is_jsx_closing_fragment),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxAttribute => {
            let node_as_jsx_attribute = node.as_jsx_attribute();
            Some(
                factory.update_jsx_attribute(
                    node,
                    node_visitor(
                        Some(&node_as_jsx_attribute.name),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_jsx_attribute.maybe_initializer().as_deref(),
                        Some(&mut visitor),
                        Some(&is_string_literal_or_jsx_expression),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::JsxAttributes => {
            let node_as_jsx_attributes = node.as_jsx_attributes();
            Some(
                factory.update_jsx_attributes(
                    node,
                    nodes_visitor(
                        Some(&node_as_jsx_attributes.properties),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_jsx_attribute_like),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxSpreadAttribute => {
            let node_as_jsx_spread_attribute = node.as_jsx_spread_attribute();
            Some(
                factory.update_jsx_spread_attribute(
                    node,
                    node_visitor(
                        Some(&node_as_jsx_spread_attribute.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::JsxExpression => {
            let node_as_jsx_expression = node.as_jsx_expression();
            Some(factory.update_jsx_expression(
                node,
                node_visitor(
                    node_as_jsx_expression.expression.as_deref(),
                    Some(&mut visitor),
                    Some(&is_expression),
                    None,
                )?,
            ))
        }
        SyntaxKind::CaseClause => {
            let node_as_case_clause = node.as_case_clause();
            Some(
                factory.update_case_clause(
                    node,
                    node_visitor(
                        Some(&node_as_case_clause.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_case_clause.statements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_statement),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::DefaultClause => {
            let node_as_default_clause = node.as_default_clause();
            Some(
                factory.update_default_clause(
                    node,
                    nodes_visitor(
                        Some(&node_as_default_clause.statements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_statement),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::HeritageClause => {
            let node_as_heritage_clause = node.as_heritage_clause();
            Some(
                factory.update_heritage_clause(
                    node,
                    nodes_visitor(
                        Some(&node_as_heritage_clause.types),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_expression_with_type_arguments),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::CatchClause => {
            let node_as_catch_clause = node.as_catch_clause();
            Some(
                factory.update_catch_clause(
                    node,
                    node_visitor(
                        node_as_catch_clause.variable_declaration.as_deref(),
                        Some(&mut visitor),
                        Some(&is_variable_declaration),
                        None,
                    )?,
                    node_visitor(
                        Some(&node_as_catch_clause.block),
                        Some(&mut visitor),
                        Some(&is_block),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::PropertyAssignment => {
            let node_as_property_assignment = node.as_property_assignment();
            Some(
                factory.update_property_assignment(
                    node,
                    node_visitor(
                        Some(&node_as_property_assignment.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_property_assignment.initializer),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::ShorthandPropertyAssignment => {
            let node_as_shorthand_property_assignment = node.as_shorthand_property_assignment();
            Some(
                factory.update_shorthand_property_assignment(
                    node,
                    node_visitor(
                        Some(&node_as_shorthand_property_assignment.name()),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_shorthand_property_assignment
                            .object_assignment_initializer
                            .as_deref(),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::SpreadAssignment => {
            let node_as_spread_assignment = node.as_spread_assignment();
            Some(
                factory.update_spread_assignment(
                    node,
                    node_visitor(
                        Some(&node_as_spread_assignment.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::EnumMember => {
            let node_as_enum_member = node.as_enum_member();
            Some(
                factory.update_enum_member(
                    node,
                    node_visitor(
                        Some(&node_as_enum_member.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )?
                    .unwrap(),
                    node_visitor(
                        node_as_enum_member.maybe_initializer().as_deref(),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?,
                ),
            )
        }
        SyntaxKind::SourceFile => {
            let node_as_source_file = node.as_source_file();
            Some(factory.update_source_file(
                node,
                try_visit_lexical_environment(
                    &node_as_source_file.statements(),
                    |node: &Node| visitor(node),
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
            let node_as_partially_emitted_expression = node.as_partially_emitted_expression();
            Some(
                factory.update_partially_emitted_expression(
                    node,
                    node_visitor(
                        Some(&node_as_partially_emitted_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        SyntaxKind::CommaListExpression => {
            let node_as_comma_list_expression = node.as_comma_list_expression();
            Some(
                factory.update_comma_list_expression(
                    node,
                    nodes_visitor(
                        Some(&node_as_comma_list_expression.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_expression),
                        None,
                        None,
                    )?
                    .unwrap(),
                ),
            )
        }
        _ => Some(node.node_wrapper()),
    })
}
