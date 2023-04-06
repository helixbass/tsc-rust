use gc::Gc;
use std::borrow::Borrow;

use crate::{
    is_array_binding_element, is_asserts_keyword, is_asterisk_token, is_binary_operator_token,
    is_binding_element, is_binding_name, is_block, is_class_element, is_colon_token, is_decorator,
    is_dot_dot_dot_token, is_entity_name, is_equals_greater_than_token, is_expression,
    is_heritage_clause, is_identifier, is_identifier_or_this_type_node, is_member_name,
    is_modifier, is_object_literal_element_like, is_parameter_declaration, is_property_name,
    is_question_dot_token, is_question_or_exclamation_token, is_question_or_plus_or_minus_token,
    is_question_token, is_readonly_keyword_or_plus_or_minus_token, is_statement, is_template_head,
    is_template_literal, is_template_literal_type_span, is_template_middle_or_template_tail,
    is_template_span, is_token, is_type_element, is_type_node,
    is_type_node_or_type_parameter_declaration, is_type_parameter_declaration,
    is_variable_declaration_list, set_text_range_pos_end, single_or_undefined, with_factory,
    with_synthetic_factory, ClassLikeDeclarationInterface, Debug_,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasQuestionTokenInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface,
    InterfaceOrClassLikeDeclarationInterface, NamedDeclarationInterface, Node, NodeArray,
    NodeFlags, NodeInterface, NonEmpty, ReadonlyTextRange, SignatureDeclarationInterface,
    SingleNodeOrVecNode, SyntaxKind, TransformationContext, VisitResult, VisitResultInterface,
};

pub fn visit_node(
    node: Option<impl Borrow<Node>>,
    visitor: Option<impl FnMut(&Node) -> VisitResult>,
    test: Option<impl Fn(&Node) -> bool>,
    lift: Option<impl Fn(&[Gc<Node>]) -> Gc<Node>>,
) -> Option<Gc<Node>> {
    let node = node?;
    let node = node.borrow();
    let mut visitor = visitor?;

    let visited = visitor(node);
    if visited.ptr_eq_node(node) {
        return Some(node.node_wrapper());
    }
    let visited = visited?;
    let visited_node = match &visited {
        SingleNodeOrVecNode::VecNode(visited) => {
            if let Some(lift) = lift {
                Some(lift(visited))
            } else {
                extract_single_node(visited)
            }
        }
        SingleNodeOrVecNode::SingleNode(visited) => Some(visited.clone()),
    };

    Debug_.assert_node(visited_node.as_deref(), test, None);
    visited_node
}

pub fn visit_nodes(
    nodes: Option<&NodeArray>,
    visitor: Option<impl FnMut(&Node) -> VisitResult>,
    test: Option<impl Fn(&Node) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
) -> Option<Gc<NodeArray>> {
    let nodes = nodes?;
    if visitor.is_none() {
        return Some(nodes.rc_wrapper());
    }
    let mut visitor = visitor.unwrap();

    let mut updated: Option<Vec<Gc<Node>>> = Default::default();

    let length = nodes.len();
    let start = start.unwrap_or(0); /*start < 0*/

    let count = if count.is_none() || count.unwrap() > length - start {
        length - start
    } else {
        count.unwrap()
    };

    let mut has_trailing_comma: Option<bool> = Default::default();
    let mut pos: isize = -1;
    let mut end: isize = -1;
    if start > 0 || count < length {
        updated = Some(vec![]);
        has_trailing_comma = Some(nodes.has_trailing_comma && start + count == length);
    }

    for i in 0..count {
        let node = nodes.get(i + start);
        let visited = node.and_then(|node| visitor(node));
        if updated.is_some()
            || match visited.as_ref() {
                None => true,
                Some(visited) => !matches!(
                    node,
                    Some(node) if visited.ptr_eq_node(node)
                ),
            }
        {
            if updated.is_none() {
                updated = Some(nodes[0..i].to_owned());
                has_trailing_comma = Some(nodes.has_trailing_comma);
                pos = nodes.pos();
                end = nodes.end();
            }
            if let Some(visited) = visited {
                match &visited {
                    SingleNodeOrVecNode::VecNode(visited) => {
                        for visited_node in visited {
                            Debug_.assert_node(
                                Some(&**visited_node),
                                test.as_ref().map(|test| |node: &Node| test(node)),
                                None,
                            );
                            updated.as_mut().unwrap().push(visited_node.clone());
                        }
                    }
                    SingleNodeOrVecNode::SingleNode(visited) => {
                        Debug_.assert_node(
                            Some(&**visited),
                            test.as_ref().map(|test| |node: &Node| test(node)),
                            None,
                        );
                        updated.as_mut().unwrap().push(visited.clone());
                    }
                }
            }
        }
    }

    if let Some(updated) = updated {
        let updated_array =
            with_factory(|factory_| factory_.create_node_array(Some(updated), has_trailing_comma));
        set_text_range_pos_end(&*updated_array, pos, end);
        return Some(updated_array);
    }

    Some(nodes.rc_wrapper())
}

pub fn visit_parameter_list(
    nodes: Option<&NodeArray>,
    visitor: impl FnMut(&Node) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    mut nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> VisitResult>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> Option<Gc<NodeArray>>,
    >,
) -> Option<Gc<NodeArray>> {
    unimplemented!()
}

pub fn visit_function_body(
    node: Option<&Node /*ConciseBody*/>,
    visitor: impl FnMut(&Node) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    mut node_visitor: Option<
        impl FnMut(
            Option<&Node>,
            Option<&mut dyn FnMut(&Node) -> VisitResult>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
        ) -> Option<Gc<Node>>,
    >,
) -> Option<Gc<Node /*ConciseBody*/>> {
    unimplemented!()
}

pub fn visit_iteration_body(
    body: &Node, /*Statement*/
    visitor: impl FnMut(&Node) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
) -> Gc<Node /*Statement*/> {
    context.start_block_scope();
    let updated = visit_node(
        Some(body),
        Some(visitor),
        Some(is_statement),
        Some(|nodes: &[Gc<Node>]| {
            with_synthetic_factory(|synthetic_factory_| {
                context.factory().lift_to_block(synthetic_factory_, nodes)
            })
        }),
    )
    .unwrap();
    let declarations = context.end_block_scope();
    if let Some(mut declarations) = declarations.non_empty()
    /*some(declarations)*/
    {
        if is_block(&updated) {
            declarations.extend(updated.as_block().statements.iter().cloned());
            return with_synthetic_factory(|synthetic_factory_| {
                context
                    .factory()
                    .update_block(synthetic_factory_, &updated, declarations)
            });
        }
        declarations.push(updated);
        return with_synthetic_factory(|synthetic_factory_| {
            context
                .factory()
                .create_block(synthetic_factory_, declarations, None)
                .into()
        });
    }
    updated
}

pub fn visit_each_child(
    node: Option<impl Borrow<Node>>,
    mut visitor: impl FnMut(&Node) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    mut nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> VisitResult>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> Option<Gc<NodeArray>>,
    >,
    token_visitor: Option<impl Fn(&Node) -> VisitResult>,
    mut node_visitor: Option<
        impl FnMut(
            Option<&Node>,
            Option<&mut dyn FnMut(&Node) -> VisitResult>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
        ) -> Option<Gc<Node>>,
    >,
) -> Option<Gc<Node>> {
    let mut nodes_visitor = move |nodes: Option<&NodeArray>,
                                  visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                                  test: Option<&dyn Fn(&Node) -> bool>,
                                  start: Option<usize>,
                                  count: Option<usize>|
          -> Option<Gc<NodeArray>> {
        if let Some(nodes_visitor) = nodes_visitor.as_mut() {
            nodes_visitor(nodes, visitor, test, start, count)
        } else {
            visit_nodes(nodes, visitor, test, start, count)
        }
    };
    let mut node_visitor = move |node: Option<&Node>,
                                 visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                                 lift: Option<&dyn Fn(&Node) -> bool>,
                                 test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>|
          -> Option<Gc<Node>> {
        if let Some(node_visitor) = node_visitor.as_mut() {
            node_visitor(node, visitor, lift, test)
        } else {
            visit_node(node, visitor, lift, test)
        }
    };
    let node = node?;
    let node: &Node = node.borrow();

    let kind = node.kind();

    if kind > SyntaxKind::FirstToken && kind <= SyntaxKind::LastToken
        || kind == SyntaxKind::ThisType
    {
        return Some(node.node_wrapper());
    }

    let factory = context.factory();

    match kind {
        SyntaxKind::Identifier => {
            let node_as_identifier = node.as_identifier();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_identifier(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node_as_identifier.maybe_type_arguments().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node_or_type_parameter_declaration),
                        None,
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::QualifiedName => {
            let node_as_qualified_name = node.as_qualified_name();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_qualified_name(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_qualified_name.left),
                        Some(&mut visitor),
                        Some(&is_entity_name),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_qualified_name.right),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ComputedPropertyName => {
            let node_as_computed_property_name = node.as_computed_property_name();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_computed_property_name(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_computed_property_name.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::TypeParameter => {
            let node_as_type_parameter_declaration = node.as_type_parameter_declaration();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_type_parameter_declaration(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_type_parameter_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_type_parameter_declaration.constraint.as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                    node_visitor(
                        node_as_type_parameter_declaration.default.as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::Parameter => {
            let node_as_parameter_declaration = node.as_parameter_declaration();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_parameter_declaration(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    node_visitor(
                        node_as_parameter_declaration.dot_dot_dot_token.as_deref(),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_dot_dot_dot_token),
                        None,
                    ),
                    node_visitor(
                        node_as_parameter_declaration.maybe_name().as_deref(),
                        Some(&mut visitor),
                        Some(&is_binding_name),
                        None,
                    ),
                    node_visitor(
                        node_as_parameter_declaration
                            .maybe_question_token()
                            .as_deref(),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_question_token),
                        None,
                    ),
                    node_visitor(
                        node_as_parameter_declaration.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                    node_visitor(
                        node_as_parameter_declaration.maybe_initializer().as_deref(),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::Decorator => {
            let node_as_decorator = node.as_decorator();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_decorator(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_decorator.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::PropertySignature => {
            let node_as_property_signature = node.as_property_signature();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_property_signature(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_property_signature.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_property_signature.maybe_question_token().as_deref(),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_token),
                        None,
                    ),
                    node_visitor(
                        node_as_property_signature.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::PropertyDeclaration => {
            let node_as_property_declaration = node.as_property_declaration();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_property_declaration(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_property_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_property_declaration
                            .maybe_question_token()
                            .or_else(|| node_as_property_declaration.exclamation_token.clone())
                            .as_deref(),
                        Some(&mut visitor),
                        Some(&is_question_or_exclamation_token),
                        None,
                    ),
                    node_visitor(
                        node_as_property_declaration.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                    node_visitor(
                        node_as_property_declaration.maybe_initializer().as_deref(),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::MethodSignature => {
            let node_as_method_signature = node.as_method_signature();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_method_signature(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_method_signature.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_method_signature.maybe_question_token().as_deref(),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_question_token),
                        None,
                    ),
                    nodes_visitor(
                        node_as_method_signature.maybe_type_parameters().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        Some(&node_as_method_signature.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_method_signature.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::MethodDeclaration => {
            let node_as_method_declaration = node.as_method_declaration();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_method_declaration(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    node_visitor(
                        node_as_method_declaration.maybe_asterisk_token().as_deref(),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_asterisk_token),
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_method_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_method_declaration.maybe_question_token().as_deref(),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_question_token),
                        None,
                    ),
                    nodes_visitor(
                        node_as_method_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    ),
                    visit_parameter_list(
                        Some(&node_as_method_declaration.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |nodes: Option<&NodeArray>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             test: Option<&dyn Fn(&Node) -> bool>,
                             start: Option<usize>,
                             count: Option<usize>|
                             -> Option<Gc<NodeArray>> {
                                nodes_visitor(nodes, visitor, test, start, count)
                            },
                        ),
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_method_declaration.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                    visit_function_body(
                        node_as_method_declaration.maybe_body().as_deref(),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |node: Option<&Node>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             lift: Option<&dyn Fn(&Node) -> bool>,
                             test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>|
                             -> Option<Gc<Node>> {
                                node_visitor(node, visitor, lift, test)
                            },
                        ),
                    ),
                )
            }))
        }
        SyntaxKind::Constructor => {
            let node_as_constructor_declaration = node.as_constructor_declaration();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_constructor_declaration(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    visit_parameter_list(
                        Some(&node_as_constructor_declaration.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |nodes: Option<&NodeArray>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             test: Option<&dyn Fn(&Node) -> bool>,
                             start: Option<usize>,
                             count: Option<usize>| {
                                nodes_visitor(nodes, visitor, test, start, count)
                            },
                        ),
                    )
                    .unwrap(),
                    visit_function_body(
                        node_as_constructor_declaration.maybe_body().as_deref(),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |node: Option<&Node>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             lift: Option<&dyn Fn(&Node) -> bool>,
                             test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>| {
                                node_visitor(node, visitor, lift, test)
                            },
                        ),
                    ),
                )
            }))
        }
        SyntaxKind::GetAccessor => {
            let node_as_get_accessor_declaration = node.as_get_accessor_declaration();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_get_accessor_declaration(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_get_accessor_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )
                    .unwrap(),
                    visit_parameter_list(
                        Some(&node_as_get_accessor_declaration.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |nodes: Option<&NodeArray>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             test: Option<&dyn Fn(&Node) -> bool>,
                             start: Option<usize>,
                             count: Option<usize>| {
                                nodes_visitor(nodes, visitor, test, start, count)
                            },
                        ),
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_get_accessor_declaration.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                    visit_function_body(
                        node_as_get_accessor_declaration.maybe_body().as_deref(),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |node: Option<&Node>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             lift: Option<&dyn Fn(&Node) -> bool>,
                             test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>| {
                                node_visitor(node, visitor, lift, test)
                            },
                        ),
                    ),
                )
            }))
        }
        SyntaxKind::SetAccessor => {
            let node_as_set_accessor_declaration = node.as_set_accessor_declaration();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_set_accessor_declaration(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_set_accessor_declaration.name()),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    )
                    .unwrap(),
                    visit_parameter_list(
                        Some(&node_as_set_accessor_declaration.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |nodes: Option<&NodeArray>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             test: Option<&dyn Fn(&Node) -> bool>,
                             start: Option<usize>,
                             count: Option<usize>| {
                                nodes_visitor(nodes, visitor, test, start, count)
                            },
                        ),
                    )
                    .unwrap(),
                    visit_function_body(
                        node_as_set_accessor_declaration.maybe_body().as_deref(),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |node: Option<&Node>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             lift: Option<&dyn Fn(&Node) -> bool>,
                             test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>| {
                                node_visitor(node, visitor, lift, test)
                            },
                        ),
                    ),
                )
            }))
        }
        SyntaxKind::ClassStaticBlockDeclaration => {
            let node_as_class_static_block_declaration = node.as_class_static_block_declaration();
            context.start_lexical_environment();
            context.suspend_lexical_environment();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_class_static_block_declaration(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    visit_function_body(
                        Some(&node_as_class_static_block_declaration.body),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |node: Option<&Node>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             lift: Option<&dyn Fn(&Node) -> bool>,
                             test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>| {
                                node_visitor(node, visitor, lift, test)
                            },
                        ),
                    ).unwrap(),
                )
            }))
        }
        SyntaxKind::CallSignature => {
            let node_as_call_signature_declaration = node.as_call_signature_declaration();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_call_signature(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node_as_call_signature_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        Some(&node_as_call_signature_declaration.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_call_signature_declaration.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::ConstructSignature => {
            let node_as_construct_signature_declaration = node.as_construct_signature_declaration();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_construct_signature(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node_as_construct_signature_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        Some(&node_as_construct_signature_declaration.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_construct_signature_declaration
                            .maybe_type()
                            .as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::IndexSignature => {
            let node_as_index_signature_declaration = node.as_index_signature_declaration();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_index_signature(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        Some(&node_as_index_signature_declaration.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_index_signature_declaration.maybe_type().unwrap()),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::TypePredicate => {
            let node_as_type_predicate_node = node.as_type_predicate_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_type_predicate_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        node_as_type_predicate_node.asserts_modifier.as_deref(),
                        Some(&mut visitor),
                        Some(&is_asserts_keyword),
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_type_predicate_node.parameter_name),
                        Some(&mut visitor),
                        Some(&is_identifier_or_this_type_node),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_type_predicate_node.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::TypeReference => {
            let node_as_type_reference_node = node.as_type_reference_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_type_reference_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_type_reference_node.type_name),
                        Some(&mut visitor),
                        Some(&is_entity_name),
                        None,
                    )
                    .unwrap(),
                    nodes_visitor(
                        node_as_type_reference_node
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::FunctionType => {
            let node_as_function_type_node = node.as_function_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_function_type_node(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node_as_function_type_node
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        Some(&node_as_function_type_node.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_function_type_node.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::ConstructorType => {
            let node_as_constructor_type_node = node.as_constructor_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_constructor_type_node(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node_as_constructor_type_node
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        Some(&node_as_constructor_type_node.parameters()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_parameter_declaration),
                        None,
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_constructor_type_node.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::TypeQuery => {
            let node_as_type_query_node = node.as_type_query_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_type_query_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_type_query_node.expr_name),
                        Some(&mut visitor),
                        Some(&is_entity_name),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::TypeLiteral => {
            let node_as_type_literal_node = node.as_type_literal_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_type_literal_node(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        Some(&node_as_type_literal_node.members),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_element),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ArrayType => {
            let node_as_array_type_node = node.as_array_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_array_type_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_array_type_node.element_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::TupleType => {
            let node_as_tuple_type_node = node.as_tuple_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_tuple_type_node(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        Some(&node_as_tuple_type_node.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::OptionalType => {
            let node_as_optional_type_node = node.as_optional_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_optional_type_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_optional_type_node.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::RestType => {
            let node_as_rest_type_node = node.as_rest_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_rest_type_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_rest_type_node.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::UnionType => {
            let node_as_union_type_node = node.as_union_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_union_type_node(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        Some(&node_as_union_type_node.types),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::IntersectionType => {
            let node_as_intersection_type_node = node.as_intersection_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_intersection_type_node(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        Some(&node_as_intersection_type_node.types),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ConditionalType => {
            let node_as_conditional_type_node = node.as_conditional_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_conditional_type_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_conditional_type_node.check_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_type_node.extends_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_type_node.true_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_type_node.false_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::InferType => {
            let node_as_infer_type_node = node.as_infer_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_infer_type_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_infer_type_node.type_parameter),
                        Some(&mut visitor),
                        Some(&is_type_parameter_declaration),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ImportType => {
            let node_as_import_type_node = node.as_import_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_import_type_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_import_type_node.argument),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_import_type_node.qualifier.as_deref(),
                        Some(&mut visitor),
                        Some(&is_entity_name),
                        None,
                    ),
                    nodes_visitor(
                        node_as_import_type_node.maybe_type_arguments().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    ),
                    Some(node_as_import_type_node.is_type_of()),
                )
            }))
        }
        SyntaxKind::NamedTupleMember => {
            let node_as_named_tuple_member = node.as_named_tuple_member();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_named_tuple_member(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        node_as_named_tuple_member.dot_dot_dot_token.as_deref(),
                        Some(&mut visitor),
                        Some(&is_dot_dot_dot_token),
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_named_tuple_member.name),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_named_tuple_member.question_token.as_deref(),
                        Some(&mut visitor),
                        Some(&is_question_token),
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_named_tuple_member.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ParenthesizedType => {
            let node_as_parenthesized_type_node = node.as_parenthesized_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_parenthesized_type(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_parenthesized_type_node.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::TypeOperator => {
            let node_as_type_operator_node = node.as_type_operator_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_type_operator_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_type_operator_node.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::IndexedAccessType => {
            let node_as_indexed_access_type_node = node.as_indexed_access_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_indexed_access_type_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_indexed_access_type_node.object_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_indexed_access_type_node.index_type),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::MappedType => {
            let node_as_mapped_type_node = node.as_mapped_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_mapped_type_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        node_as_mapped_type_node.readonly_token.as_deref(),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_readonly_keyword_or_plus_or_minus_token),
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_mapped_type_node.type_parameter),
                        Some(&mut visitor),
                        Some(&is_type_parameter_declaration),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_mapped_type_node.name_type.as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                    node_visitor(
                        node_as_mapped_type_node.question_token.as_deref(),
                        Some(&mut visitor),
                        Some(&is_question_or_plus_or_minus_token),
                        None,
                    ),
                    node_visitor(
                        node_as_mapped_type_node.type_.as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                    nodes_visitor(
                        node_as_mapped_type_node.members.as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_element),
                        None,
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::LiteralType => {
            let node_as_literal_type_node = node.as_literal_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_literal_type_node(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_literal_type_node.literal),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::TemplateLiteralType => {
            let node_as_template_literal_type_node = node.as_template_literal_type_node();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_template_literal_type(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_template_literal_type_node.head),
                        Some(&mut visitor),
                        Some(&is_template_head),
                        None,
                    )
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_template_literal_type_node.template_spans),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_template_literal_type_span),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::TemplateLiteralTypeSpan => {
            let node_as_template_literal_type_span = node.as_template_literal_type_span();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_template_literal_type_span(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_template_literal_type_span.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_template_literal_type_span.literal),
                        Some(&mut visitor),
                        Some(&is_template_middle_or_template_tail),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ObjectBindingPattern => {
            let node_as_object_binding_pattern = node.as_object_binding_pattern();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_object_binding_pattern(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        Some(&node_as_object_binding_pattern.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_binding_element),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ArrayBindingPattern => {
            let node_as_array_binding_pattern = node.as_array_binding_pattern();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_array_binding_pattern(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        Some(&node_as_array_binding_pattern.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_array_binding_element),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::BindingElement => {
            let node_as_binding_element = node.as_binding_element();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_binding_element(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        node_as_binding_element.dot_dot_dot_token.as_deref(),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_dot_dot_dot_token),
                        None,
                    ),
                    node_visitor(
                        node_as_binding_element.property_name.as_deref(),
                        Some(&mut visitor),
                        Some(&is_property_name),
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_binding_element.name()),
                        Some(&mut visitor),
                        Some(&is_binding_name),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_binding_element.maybe_initializer().as_deref(),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::ArrayLiteralExpression => {
            let node_as_array_literal_expression = node.as_array_literal_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_array_literal_expression(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        Some(&node_as_array_literal_expression.elements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_expression),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ObjectLiteralExpression => {
            let node_as_object_literal_expression = node.as_object_literal_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_object_literal_expression(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        Some(&node_as_object_literal_expression.properties),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_object_literal_element_like),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::PropertyAccessExpression => {
            let node_as_property_access_expression = node.as_property_access_expression();
            if node.flags().intersects(NodeFlags::OptionalChain) {
                return Some(with_synthetic_factory(|synthetic_factory_| {
                    factory.update_property_access_chain(
                        synthetic_factory_,
                        node,
                        node_visitor(
                            Some(&node_as_property_access_expression.expression),
                            Some(&mut visitor),
                            Some(&is_expression),
                            None,
                        )
                        .unwrap(),
                        node_visitor(
                            node_as_property_access_expression
                                .question_dot_token
                                .as_deref(),
                            Some(&mut |node: &Node| {
                                if let Some(token_visitor) = token_visitor.as_ref() {
                                    token_visitor(node)
                                } else {
                                    None
                                }
                            }),
                            Some(&is_question_dot_token),
                            None,
                        ),
                        node_visitor(
                            Some(&node_as_property_access_expression.name()),
                            Some(&mut visitor),
                            Some(&is_member_name),
                            None,
                        )
                        .unwrap(),
                    )
                }));
            }
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_property_access_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_property_access_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_property_access_expression.name()),
                        Some(&mut visitor),
                        Some(&is_member_name),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ElementAccessExpression => {
            let node_as_element_access_expression = node.as_element_access_expression();
            if node.flags().intersects(NodeFlags::OptionalChain) {
                return Some(with_synthetic_factory(|synthetic_factory_| {
                    factory.update_element_access_chain(
                        synthetic_factory_,
                        node,
                        node_visitor(
                            Some(&node_as_element_access_expression.expression),
                            Some(&mut visitor),
                            Some(&is_expression),
                            None,
                        )
                        .unwrap(),
                        node_visitor(
                            node_as_element_access_expression
                                .question_dot_token
                                .as_deref(),
                            Some(&mut |node: &Node| {
                                if let Some(token_visitor) = token_visitor.as_ref() {
                                    token_visitor(node)
                                } else {
                                    None
                                }
                            }),
                            Some(&is_question_dot_token),
                            None,
                        ),
                        node_visitor(
                            Some(&node_as_element_access_expression.argument_expression),
                            Some(&mut visitor),
                            Some(&is_expression),
                            None,
                        )
                        .unwrap(),
                    )
                }));
            }
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_element_access_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_element_access_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_element_access_expression.argument_expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::CallExpression => {
            let node_as_call_expression = node.as_call_expression();
            if node.flags().intersects(NodeFlags::OptionalChain) {
                return Some(with_synthetic_factory(|synthetic_factory_| {
                    factory.update_call_chain(
                        synthetic_factory_,
                        node,
                        node_visitor(
                            Some(&node_as_call_expression.expression),
                            Some(&mut visitor),
                            Some(&is_expression),
                            None,
                        )
                        .unwrap(),
                        node_visitor(
                            node_as_call_expression.question_dot_token.as_deref(),
                            Some(&mut |node: &Node| {
                                if let Some(token_visitor) = token_visitor.as_ref() {
                                    token_visitor(node)
                                } else {
                                    None
                                }
                            }),
                            Some(&is_question_dot_token),
                            None,
                        ),
                        nodes_visitor(
                            node_as_call_expression.maybe_type_arguments().as_deref(),
                            Some(&mut |node: &Node| visitor(node)),
                            Some(&is_type_node),
                            None,
                            None,
                        ),
                        nodes_visitor(
                            Some(&node_as_call_expression.arguments),
                            Some(&mut |node: &Node| visitor(node)),
                            Some(&is_expression),
                            None,
                            None,
                        )
                        .unwrap(),
                    )
                }));
            }
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_call_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_call_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    nodes_visitor(
                        node_as_call_expression.maybe_type_arguments().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        Some(&node_as_call_expression.arguments),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_expression),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::NewExpression => {
            let node_as_new_expression = node.as_new_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_new_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_new_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    nodes_visitor(
                        node_as_new_expression.maybe_type_arguments().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node_as_new_expression.arguments.as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_expression),
                        None,
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::TaggedTemplateExpression => {
            let node_as_tagged_template_expression = node.as_tagged_template_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_tagged_template_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_tagged_template_expression.tag),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    nodes_visitor(
                        node_as_tagged_template_expression
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_tagged_template_expression.template),
                        Some(&mut visitor),
                        Some(&is_template_literal),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::TypeAssertionExpression => {
            let node_as_type_assertion = node.as_type_assertion();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_type_assertion(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_type_assertion.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_type_assertion.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ParenthesizedExpression => {
            let node_as_parenthesized_expression = node.as_parenthesized_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_parenthesized_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_parenthesized_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::FunctionExpression => {
            let node_as_function_expression = node.as_function_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_function_expression(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    node_visitor(
                        node_as_function_expression.maybe_asterisk_token().as_deref(),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_asterisk_token),
                        None,
                    ),
                    node_visitor(
                        node_as_function_expression.maybe_name().as_deref(),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    ),
                    nodes_visitor(
                        node_as_function_expression.maybe_type_parameters().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    ),
                    visit_parameter_list(
                        Some(&node_as_function_expression.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |nodes: Option<&NodeArray>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             test: Option<&dyn Fn(&Node) -> bool>,
                             start: Option<usize>,
                             count: Option<usize>| {
                                nodes_visitor(nodes, visitor, test, start, count)
                            },
                        ),
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_function_expression.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                    visit_function_body(
                        Some(&node_as_function_expression.maybe_body().unwrap()),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |node: Option<&Node>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             lift: Option<&dyn Fn(&Node) -> bool>,
                             test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>| {
                                node_visitor(node, visitor, lift, test)
                            },
                        ),
                    ).unwrap(),
                )
            }))
        }
        SyntaxKind::ArrowFunction => {
            let node_as_arrow_function = node.as_arrow_function();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_arrow_function(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node_as_arrow_function.maybe_type_parameters().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    ),
                    visit_parameter_list(
                        Some(&node_as_arrow_function.parameters()),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |nodes: Option<&NodeArray>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             test: Option<&dyn Fn(&Node) -> bool>,
                             start: Option<usize>,
                             count: Option<usize>| {
                                nodes_visitor(nodes, visitor, test, start, count)
                            },
                        ),
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_arrow_function.maybe_type().as_deref(),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_arrow_function.equals_greater_than_token),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_equals_greater_than_token),
                        None,
                    )
                    .unwrap(),
                    visit_function_body(
                        Some(&node_as_arrow_function.maybe_body().unwrap()),
                        |node: &Node| visitor(node),
                        context,
                        Some(
                            |node: Option<&Node>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             lift: Option<&dyn Fn(&Node) -> bool>,
                             test: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>| {
                                node_visitor(node, visitor, lift, test)
                            },
                        ),
                    ).unwrap(),
                )
            }))
        }
        SyntaxKind::DeleteExpression => {
            let node_as_delete_expression = node.as_delete_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_delete_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_delete_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::TypeOfExpression => {
            let node_as_type_of_expression = node.as_type_of_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_type_of_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_type_of_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::VoidExpression => {
            let node_as_void_expression = node.as_void_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_void_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_void_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::AwaitExpression => {
            let node_as_await_expression = node.as_await_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_await_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_await_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::PrefixUnaryExpression => {
            let node_as_prefix_unary_expression = node.as_prefix_unary_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_prefix_unary_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_prefix_unary_expression.operand),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::PostfixUnaryExpression => {
            let node_as_postfix_unary_expression = node.as_postfix_unary_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_postfix_unary_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_postfix_unary_expression.operand),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::BinaryExpression => {
            let node_as_binary_expression = node.as_binary_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_binary_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_binary_expression.left),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_binary_expression.operator_token),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_binary_operator_token),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_binary_expression.right),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ConditionalExpression => {
            let node_as_conditional_expression = node.as_conditional_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_conditional_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_conditional_expression.condition),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_expression.question_token),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_question_token),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_expression.when_true),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_expression.colon_token),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_colon_token),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_conditional_expression.when_false),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::TemplateExpression => {
            let node_as_template_expression = node.as_template_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_template_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_template_expression.head),
                        Some(&mut visitor),
                        Some(&is_template_head),
                        None,
                    )
                    .unwrap(),
                    nodes_visitor(
                        Some(&node_as_template_expression.template_spans),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_template_span),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::YieldExpression => {
            let node_as_yield_expression = node.as_yield_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_yield_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        node_as_yield_expression.asterisk_token.as_deref(),
                        Some(&mut |node: &Node| {
                            if let Some(token_visitor) = token_visitor.as_ref() {
                                token_visitor(node)
                            } else {
                                None
                            }
                        }),
                        Some(&is_asterisk_token),
                        None,
                    ),
                    node_visitor(
                        node_as_yield_expression.expression.as_deref(),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::SpreadElement => {
            let node_as_spread_element = node.as_spread_element();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_spread_element(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_spread_element.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ClassExpression => {
            let node_as_class_expression = node.as_class_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_class_expression(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_decorators().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_decorator),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    node_visitor(
                        node_as_class_expression.maybe_name().as_deref(),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    ),
                    nodes_visitor(
                        node_as_class_expression.maybe_type_parameters().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_parameter_declaration),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        node_as_class_expression.maybe_heritage_clauses().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_heritage_clause),
                        None,
                        None,
                    ),
                    nodes_visitor(
                        Some(&node_as_class_expression.members()),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_class_element),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ExpressionWithTypeArguments => {
            let node_as_expression_with_type_arguments = node.as_expression_with_type_arguments();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_expression_with_type_arguments(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_expression_with_type_arguments.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    nodes_visitor(
                        node_as_expression_with_type_arguments
                            .maybe_type_arguments()
                            .as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_type_node),
                        None,
                        None,
                    ),
                )
            }))
        }
        SyntaxKind::AsExpression => {
            let node_as_as_expression = node.as_as_expression();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_as_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_as_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_as_expression.type_),
                        Some(&mut visitor),
                        Some(&is_type_node),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::NonNullExpression => {
            let node_as_non_null_expression = node.as_non_null_expression();
            if node.flags().intersects(NodeFlags::OptionalChain) {
                return Some(with_synthetic_factory(|synthetic_factory_| {
                    factory.update_non_null_chain(
                        synthetic_factory_,
                        node,
                        node_visitor(
                            Some(&node_as_non_null_expression.expression),
                            Some(&mut visitor),
                            Some(&is_expression),
                            None,
                        )
                        .unwrap(),
                    )
                }));
            }
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_non_null_expression(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_non_null_expression.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::MetaProperty => {
            let node_as_meta_property = node.as_meta_property();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_meta_property(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_meta_property.name),
                        Some(&mut visitor),
                        Some(&is_identifier),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::TemplateSpan => {
            let node_as_template_span = node.as_template_span();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_template_span(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_template_span.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_template_span.literal),
                        Some(&mut visitor),
                        Some(&is_template_middle_or_template_tail),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::Block => {
            let node_as_block = node.as_block();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_block(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        Some(&node_as_block.statements),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_statement),
                        None,
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::VariableStatement => {
            let node_as_variable_statement = node.as_variable_statement();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_variable_statement(
                    synthetic_factory_,
                    node,
                    nodes_visitor(
                        node.maybe_modifiers().as_deref(),
                        Some(&mut |node: &Node| visitor(node)),
                        Some(&is_modifier),
                        None,
                        None,
                    ),
                    node_visitor(
                        Some(&node_as_variable_statement.declaration_list),
                        Some(&mut visitor),
                        Some(&is_variable_declaration_list),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::ExpressionStatement => {
            let node_as_expression_statement = node.as_expression_statement();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_expression_statement(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_expression_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::IfStatement => {
            let node_as_if_statement = node.as_if_statement();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_if_statement(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_if_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    node_visitor(
                        Some(&node_as_if_statement.then_statement),
                        Some(&mut visitor),
                        Some(&is_statement),
                        Some(&|nodes: &[Gc<Node>]| {
                            factory.lift_to_block(synthetic_factory_, nodes)
                        }),
                    )
                    .unwrap(),
                    node_visitor(
                        node_as_if_statement.else_statement.as_deref(),
                        Some(&mut visitor),
                        Some(&is_statement),
                        Some(&|nodes: &[Gc<Node>]| {
                            factory.lift_to_block(synthetic_factory_, nodes)
                        }),
                    ),
                )
            }))
        }
        SyntaxKind::DoStatement => {
            let node_as_do_statement = node.as_do_statement();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_do_statement(
                    synthetic_factory_,
                    node,
                    visit_iteration_body(
                        &node_as_do_statement.statement,
                        |node: &Node| visitor(node),
                        context,
                    ),
                    node_visitor(
                        Some(&node_as_do_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                )
            }))
        }
        SyntaxKind::WhileStatement => {
            let node_as_while_statement = node.as_while_statement();
            Some(with_synthetic_factory(|synthetic_factory_| {
                factory.update_while_statement(
                    synthetic_factory_,
                    node,
                    node_visitor(
                        Some(&node_as_while_statement.expression),
                        Some(&mut visitor),
                        Some(&is_expression),
                        None,
                    )
                    .unwrap(),
                    visit_iteration_body(
                        &node_as_while_statement.statement,
                        |node: &Node| visitor(node),
                        context,
                    ),
                )
            }))
        }
        _ => Some(node.node_wrapper()),
    }
}

fn extract_single_node(nodes: &[Gc<Node>]) -> Option<Gc<Node>> {
    Debug_.assert(nodes.len() <= 1, Some("Too many nodes written to output."));
    single_or_undefined(Some(nodes)).cloned()
}
