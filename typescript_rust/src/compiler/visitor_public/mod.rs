use gc::Gc;
use std::{borrow::Borrow, io};

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
    return_ok_default_if_none, set_text_range_pos_end, single_or_undefined, with_factory,
    ClassLikeDeclarationInterface, Debug_, FunctionLikeDeclarationInterface,
    HasInitializerInterface, HasMembersInterface, HasQuestionTokenInterface,
    HasStatementsInterface, HasTypeArgumentsInterface, HasTypeInterface,
    HasTypeParametersInterface, InterfaceOrClassLikeDeclarationInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeFlags, NodeInterface, NonEmpty, OptionTry,
    ReadonlyTextRange, SignatureDeclarationInterface, SingleNodeOrVecNode, SyntaxKind,
    TransformationContext, VisitResult, VisitResultInterface,
};

mod try_visit_each_child;
mod visit_each_child;
pub use try_visit_each_child::{try_visit_each_child, try_visit_each_child_full};
pub use visit_each_child::visit_each_child;

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

pub fn try_visit_node<TError>(
    node: Option<impl Borrow<Node>>,
    visitor: Option<impl FnMut(&Node) -> Result<VisitResult, TError>>,
    test: Option<impl Fn(&Node) -> bool>,
    lift: Option<impl Fn(&[Gc<Node>]) -> Gc<Node>>,
) -> Result<Option<Gc<Node>>, TError> {
    if node.is_none() {
        return Ok(None);
    }
    let node = node.unwrap();
    let node = node.borrow();
    if visitor.is_none() {
        return Ok(None);
    }
    let mut visitor = visitor.unwrap();

    let visited = visitor(node)?;
    if visited.ptr_eq_node(node) {
        return Ok(Some(node.node_wrapper()));
    }
    if visited.is_none() {
        return Ok(None);
    }
    let visited = visited.unwrap();
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
    Ok(visited_node)
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

pub fn try_visit_nodes<TError>(
    nodes: Option<&NodeArray>,
    visitor: Option<impl FnMut(&Node) -> Result<VisitResult, TError>>,
    test: Option<impl Fn(&Node) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
) -> Result<Option<Gc<NodeArray>>, TError> {
    if nodes.is_none() {
        return Ok(None);
    }
    let nodes = nodes.unwrap();
    if visitor.is_none() {
        return Ok(Some(nodes.rc_wrapper()));
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
        let visited = node.try_and_then(|node| visitor(node))?;
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
        return Ok(Some(updated_array));
    }

    Ok(Some(nodes.rc_wrapper()))
}

pub fn visit_lexical_environment(
    _statements: &NodeArray, /*<Statement>*/
    _visitor: impl FnMut(&Node) -> VisitResult,
    _context: &(impl TransformationContext + ?Sized),
    _start: Option<isize>,
    _ensure_use_strict: Option<bool>,
    _nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> VisitResult>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> Option<Gc<NodeArray>>,
    >,
) -> Gc<NodeArray> {
    unimplemented!()
}

pub fn try_visit_lexical_environment(
    statements: &NodeArray, /*<Statement>*/
    visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
) -> io::Result<Gc<NodeArray>> {
    try_visit_lexical_environment_full(
        statements,
        visitor,
        context,
        None,
        None,
        Option::<
            fn(
                Option<&NodeArray>,
                Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                Option<&dyn Fn(&Node) -> bool>,
                Option<usize>,
                Option<usize>,
            ) -> io::Result<Option<Gc<NodeArray>>>,
        >::None,
    )
}

pub fn try_visit_lexical_environment_full(
    _statements: &NodeArray, /*<Statement>*/
    _visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    _context: &(impl TransformationContext + ?Sized),
    _start: Option<isize>,
    _ensure_use_strict: Option<bool>,
    _nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> io::Result<Option<Gc<NodeArray>>>,
    >,
) -> io::Result<Gc<NodeArray>> {
    unimplemented!()
}

pub fn visit_parameter_list(
    _nodes: Option<&NodeArray>,
    _visitor: impl FnMut(&Node) -> VisitResult,
    _context: &(impl TransformationContext + ?Sized),
    _nodes_visitor: Option<
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

pub fn try_visit_parameter_list(
    nodes: Option<&NodeArray>,
    visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
) -> io::Result<Option<Gc<NodeArray>>> {
    try_visit_parameter_list_full(
        nodes,
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
    )
}

pub fn try_visit_parameter_list_full(
    _nodes: Option<&NodeArray>,
    _visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    _context: &(impl TransformationContext + ?Sized),
    _nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> io::Result<Option<Gc<NodeArray>>>,
    >,
) -> io::Result<Option<Gc<NodeArray>>> {
    unimplemented!()
}

pub fn visit_function_body(
    _node: Option<&Node /*ConciseBody*/>,
    _visitor: impl FnMut(&Node) -> VisitResult,
    _context: &(impl TransformationContext + ?Sized),
    _node_visitor: Option<
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

pub fn try_visit_function_body<TError>(
    node: Option<&Node /*ConciseBody*/>,
    visitor: impl FnMut(&Node) -> Result<VisitResult, TError>,
    context: &(impl TransformationContext + ?Sized),
) -> Result<Option<Gc<Node /*ConciseBody*/>>, TError> {
    try_visit_function_body_full(
        node,
        visitor,
        context,
        Option::<
            fn(
                Option<&Node>,
                Option<&mut dyn FnMut(&Node) -> Result<VisitResult, TError>>,
                Option<&dyn Fn(&Node) -> bool>,
                Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
            ) -> Result<Option<Gc<Node>>, TError>,
        >::None,
    )
}

pub fn try_visit_function_body_full<TError>(
    _node: Option<&Node /*ConciseBody*/>,
    _visitor: impl FnMut(&Node) -> Result<VisitResult, TError>,
    _context: &(impl TransformationContext + ?Sized),
    _node_visitor: Option<
        impl FnMut(
            Option<&Node>,
            Option<&mut dyn FnMut(&Node) -> Result<VisitResult, TError>>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
        ) -> Result<Option<Gc<Node>>, TError>,
    >,
) -> Result<Option<Gc<Node /*ConciseBody*/>>, TError> {
    unimplemented!()
}

pub fn visit_iteration_body(
    body: &Node, /*Statement*/
    mut visitor: impl FnMut(&Node) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
) -> Gc<Node /*Statement*/> {
    try_visit_iteration_body(body, |a| Ok(visitor(a)), context).unwrap()
}

pub fn try_visit_iteration_body(
    body: &Node, /*Statement*/
    visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
) -> io::Result<Gc<Node /*Statement*/>> {
    context.start_block_scope();
    let updated = try_visit_node(
        Some(body),
        Some(visitor),
        Some(is_statement),
        Some(|nodes: &[Gc<Node>]| context.factory().lift_to_block(nodes)),
    )?
    .unwrap();
    let declarations = context.end_block_scope();
    if let Some(mut declarations) = declarations.non_empty()
    /*some(declarations)*/
    {
        if is_block(&updated) {
            declarations.extend(updated.as_block().statements.iter().cloned());
            return Ok(context.factory().update_block(&updated, declarations));
        }
        declarations.push(updated);
        return Ok(context.factory().create_block(declarations, None).wrap());
    }
    Ok(updated)
}

fn extract_single_node(nodes: &[Gc<Node>]) -> Option<Gc<Node>> {
    Debug_.assert(nodes.len() <= 1, Some("Too many nodes written to output."));
    single_or_undefined(Some(nodes)).cloned()
}
