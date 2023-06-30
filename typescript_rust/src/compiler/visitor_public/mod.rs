use std::{borrow::Borrow, io};

use gc::Gc;

use crate::{
    is_block, is_statement, set_text_range_pos_end, single_or_undefined, with_factory, Debug_,
    HasInitializerInterface, HasTypeInterface, NamedDeclarationInterface, Node, NodeArray,
    NodeArrayExt, NodeExt, NodeInterface, NonEmpty, OptionTry, ReadonlyTextRange,
    SingleNodeOrVecNode, TransformationContext, VisitResult, VisitResultInterface,
};

mod try_visit_each_child;
mod visit_each_child;
pub use try_visit_each_child::*;
pub use visit_each_child::*;

use crate::{
    _d, get_emit_flags, get_emit_script_target, get_factory, is_binding_pattern, is_concise_body,
    is_parameter_declaration, return_ok_default_if_none, EmitFlags, LexicalEnvironmentFlags,
    ScriptTarget,
};

pub fn visit_node(
    node: &Node,
    visitor: Option<impl FnMut(&Node) -> VisitResult>,
    test: Option<impl Fn(&Node) -> bool>,
    lift: Option<impl Fn(&[Gc<Node>]) -> Gc<Node>>,
) -> Gc<Node> {
    maybe_visit_node(Some(node), visitor, test, lift).unwrap()
}

pub fn maybe_visit_node(
    node: Option<impl Borrow<Node>>,
    visitor: Option<impl FnMut(&Node) -> VisitResult>,
    test: Option<impl Fn(&Node) -> bool>,
    lift: Option<impl Fn(&[Gc<Node>]) -> Gc<Node>>,
) -> Option<Gc<Node>> {
    try_maybe_visit_node(
        node,
        visitor.map(|mut visitor| move |node: &Node| Ok(visitor(node))),
        test,
        lift,
    )
    .unwrap()
}

pub fn try_visit_node(
    node: &Node,
    visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult>>,
    test: Option<impl Fn(&Node) -> bool>,
    lift: Option<impl Fn(&[Gc<Node>]) -> Gc<Node>>,
) -> io::Result<Gc<Node>> {
    Ok(try_maybe_visit_node(Some(node), visitor, test, lift)?.unwrap())
}

pub fn try_maybe_visit_node(
    node: Option<impl Borrow<Node>>,
    visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult>>,
    test: Option<impl Fn(&Node) -> bool>,
    lift: Option<impl Fn(&[Gc<Node>]) -> Gc<Node>>,
) -> io::Result<Option<Gc<Node>>> {
    let node = return_ok_default_if_none!(node);
    let node = node.borrow();
    if visitor.is_none() {
        return Ok(Some(node.node_wrapper()));
    }
    let mut visitor = visitor.unwrap();

    let visited = visitor(node)?;
    if visited.ptr_eq_node(node) {
        return Ok(Some(node.node_wrapper()));
    }
    let visited = return_ok_default_if_none!(visited);
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
    nodes: &NodeArray,
    visitor: Option<impl FnMut(&Node) -> VisitResult>,
    test: Option<impl Fn(&Node) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
) -> Gc<NodeArray> {
    maybe_visit_nodes(Some(nodes), visitor, test, start, count).unwrap()
}

pub fn maybe_visit_nodes(
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

pub fn try_visit_nodes(
    nodes: &NodeArray,
    visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult>>,
    test: Option<impl Fn(&Node) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
) -> io::Result<Gc<NodeArray>> {
    Ok(try_maybe_visit_nodes(Some(nodes), visitor, test, start, count)?.unwrap())
}

pub fn try_maybe_visit_nodes(
    nodes: Option<&NodeArray>,
    visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult>>,
    test: Option<impl Fn(&Node) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
) -> io::Result<Option<Gc<NodeArray>>> {
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
    statements: &NodeArray, /*<Statement>*/
    mut visitor: impl FnMut(&Node) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    start: Option<usize>,
    ensure_use_strict: Option<bool>,
    mut nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> VisitResult>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> Option<Gc<NodeArray>>,
    >,
) -> Gc<NodeArray> {
    // try_visit_lexical_environment_full(
    //     statements,
    //     |node: &Node| Ok(visitor(node)),
    //     context,
    //     start,
    //     ensure_use_strict,
    //     nodes_visitor.map(|mut nodes_visitor| {
    //         move |nodes: Option<&NodeArray>,
    //               visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
    //               test: Option<&dyn Fn(&Node) -> bool>,
    //               start: Option<usize>,
    //               count: Option<usize>| {
    //             let mut visitor = visitor.map(|visitor| {
    //                 Box::new(move |node: &Node| visitor(node).unwrap())
    //                     as Box<dyn FnMut(&Node) -> VisitResult>
    //             });
    //             let ret = {
    //                 let visitor = visitor.as_deref_mut();
    //                 nodes_visitor(nodes, visitor, test, start, count).clone()
    //             };
    //             Ok(ret)
    //         }
    //     }),
    // )
    // .unwrap()
    context.start_lexical_environment();
    let mut nodes_visitor_or_default = |nodes: Option<&NodeArray>,
                                        visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                                        test: Option<&dyn Fn(&Node) -> bool>,
                                        start: Option<usize>,
                                        count: Option<usize>|
     -> Option<Gc<NodeArray>> {
        if let Some(nodes_visitor) = nodes_visitor.as_mut() {
            nodes_visitor(nodes, visitor, test, start, count)
        } else {
            maybe_visit_nodes(
                nodes,
                visitor.map(|visitor| |node: &Node| visitor(node)),
                test.map(|test| |node: &Node| test(node)),
                start,
                count,
            )
        }
    };
    let mut statements = nodes_visitor_or_default(
        Some(statements),
        Some(&mut |node: &Node| visitor(node)),
        Some(&|node: &Node| is_statement(node)),
        start,
        None,
    )
    .unwrap();
    if ensure_use_strict == Some(true) {
        statements = context.factory().ensure_use_strict(&statements);
    }
    get_factory()
        .merge_lexical_environment(statements, context.end_lexical_environment().as_deref())
        .as_node_array()
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
    statements: &NodeArray, /*<Statement>*/
    mut visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    start: Option<usize>,
    ensure_use_strict: Option<bool>,
    mut nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> io::Result<Option<Gc<NodeArray>>>,
    >,
) -> io::Result<Gc<NodeArray>> {
    context.start_lexical_environment();
    let mut nodes_visitor_or_default =
        |nodes: Option<&NodeArray>,
         visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
         test: Option<&dyn Fn(&Node) -> bool>,
         start: Option<usize>,
         count: Option<usize>|
         -> io::Result<Option<Gc<NodeArray>>> {
            if let Some(nodes_visitor) = nodes_visitor.as_mut() {
                nodes_visitor(nodes, visitor, test, start, count)
            } else {
                try_maybe_visit_nodes(
                    nodes,
                    visitor.map(|visitor| |node: &Node| visitor(node)),
                    test.map(|test| |node: &Node| test(node)),
                    start,
                    count,
                )
            }
        };
    let mut statements = nodes_visitor_or_default(
        Some(statements),
        Some(&mut |node: &Node| visitor(node)),
        Some(&|node: &Node| is_statement(node)),
        start,
        None,
    )?
    .unwrap();
    if ensure_use_strict == Some(true) {
        statements = context.factory().ensure_use_strict(&statements);
    }
    Ok(get_factory()
        .merge_lexical_environment(statements, context.end_lexical_environment().as_deref())
        .as_node_array())
}

pub fn visit_parameter_list(
    nodes: Option<&NodeArray>,
    visitor: impl FnMut(&Node) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
) -> Option<Gc<NodeArray>> {
    visit_parameter_list_full(
        nodes,
        visitor,
        context,
        Option::<
            fn(
                Option<&NodeArray>,
                Option<&mut dyn FnMut(&Node) -> VisitResult>,
                Option<&dyn Fn(&Node) -> bool>,
                Option<usize>,
                Option<usize>,
            ) -> Option<Gc<NodeArray>>,
        >::None,
    )
}

pub fn visit_parameter_list_full(
    nodes: Option<&NodeArray>,
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
) -> Option<Gc<NodeArray>> {
    let mut nodes_visitor = |nodes: Option<&NodeArray>,
                             visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                             test: Option<&dyn Fn(&Node) -> bool>,
                             start: Option<usize>,
                             count: Option<usize>|
     -> Option<Gc<NodeArray>> {
        if let Some(nodes_visitor) = nodes_visitor.as_mut() {
            nodes_visitor(nodes, visitor, test, start, count)
        } else {
            maybe_visit_nodes(
                nodes,
                visitor.map(|visitor| |node: &Node| visitor(node)),
                test.map(|test| |node: &Node| test(node)),
                start,
                count,
            )
        }
    };
    let mut updated: Option<Gc<NodeArray /*<ParameterDeclaration>*/>> = _d();
    context.start_lexical_environment();
    if let Some(nodes) = nodes {
        context.set_lexical_environment_flags(LexicalEnvironmentFlags::InParameters, true);
        updated = nodes_visitor(
            Some(nodes),
            Some(&mut visitor),
            Some(&is_parameter_declaration),
            None,
            None,
        );

        if context
            .get_lexical_environment_flags()
            .intersects(LexicalEnvironmentFlags::VariablesHoistedInParameters)
            && get_emit_script_target(&context.get_compiler_options()) >= ScriptTarget::ES2015
        {
            updated = Some(add_default_value_assignments_if_needed(
                updated.as_ref().unwrap(),
                context,
            ));
        }
        context.set_lexical_environment_flags(LexicalEnvironmentFlags::InParameters, false);
    }
    context.suspend_lexical_environment();
    updated
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
    nodes: Option<&NodeArray>,
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
) -> io::Result<Option<Gc<NodeArray>>> {
    let mut nodes_visitor = |nodes: Option<&NodeArray>,
                             visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                             test: Option<&dyn Fn(&Node) -> bool>,
                             start: Option<usize>,
                             count: Option<usize>|
     -> io::Result<Option<Gc<NodeArray>>> {
        if let Some(nodes_visitor) = nodes_visitor.as_mut() {
            nodes_visitor(nodes, visitor, test, start, count)
        } else {
            try_maybe_visit_nodes(
                nodes,
                visitor.map(|visitor| |node: &Node| visitor(node)),
                test.map(|test| |node: &Node| test(node)),
                start,
                count,
            )
        }
    };
    let mut updated: Option<Gc<NodeArray /*<ParameterDeclaration>*/>> = _d();
    context.start_lexical_environment();
    if let Some(nodes) = nodes {
        context.set_lexical_environment_flags(LexicalEnvironmentFlags::InParameters, true);
        updated = nodes_visitor(
            Some(nodes),
            Some(&mut visitor),
            Some(&is_parameter_declaration),
            None,
            None,
        )?;

        if context
            .get_lexical_environment_flags()
            .intersects(LexicalEnvironmentFlags::VariablesHoistedInParameters)
            && get_emit_script_target(&context.get_compiler_options()) >= ScriptTarget::ES2015
        {
            updated = Some(add_default_value_assignments_if_needed(
                updated.as_ref().unwrap(),
                context,
            ));
        }
        context.set_lexical_environment_flags(LexicalEnvironmentFlags::InParameters, false);
    }
    context.suspend_lexical_environment();
    Ok(updated)
}

fn add_default_value_assignments_if_needed(
    parameters: &NodeArray, /*<ParameterDeclaration>*/
    context: &(impl TransformationContext + ?Sized),
) -> Gc<NodeArray /*<ParameterDeclaration>*/> {
    let mut result: Option<Vec<Gc<Node /*ParameterDeclaration*/>>> = _d();
    for (i, parameter) in parameters.iter().enumerate() {
        let updated = add_default_value_assignment_if_needed(parameter, context);
        if result.is_some() || !Gc::ptr_eq(&updated, parameter) {
            result
                .get_or_insert_with(|| parameters[..i].to_owned())
                .push(updated);
        }
    }
    result.map_or_else(
        || parameters.rc_wrapper(),
        |result| {
            context
                .factory()
                .create_node_array(Some(result), Some(parameters.has_trailing_comma))
                .set_text_range(Some(parameters))
        },
    )
}

fn add_default_value_assignment_if_needed(
    parameter: &Node, /*ParameterDeclaration*/
    context: &(impl TransformationContext + ?Sized),
) -> Gc<Node> {
    let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
    if parameter_as_parameter_declaration
        .dot_dot_dot_token
        .is_some()
    {
        parameter.node_wrapper()
    } else if is_binding_pattern(parameter_as_parameter_declaration.maybe_name()) {
        add_default_value_assignment_for_binding_pattern(parameter, context)
    } else if let Some(ref parameter_initializer) =
        parameter_as_parameter_declaration.maybe_initializer()
    {
        add_default_value_assignment_for_initializer(
            parameter,
            &parameter_as_parameter_declaration.name(),
            parameter_initializer,
            context,
        )
    } else {
        parameter.node_wrapper()
    }
}

fn add_default_value_assignment_for_binding_pattern(
    parameter: &Node, /*ParameterDeclaration*/
    context: &(impl TransformationContext + ?Sized),
) -> Gc<Node> {
    let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
    let factory = context.factory();
    context.add_initialization_statement(&factory.create_variable_statement(
        Option::<Gc<NodeArray>>::None,
        factory.create_variable_declaration_list(
            vec![
                    factory.create_variable_declaration(
                        parameter_as_parameter_declaration.maybe_name(),
                        None,
                        parameter_as_parameter_declaration.maybe_type(),
                        Some(
                            parameter_as_parameter_declaration.maybe_initializer().map_or_else(
                                || factory.get_generated_name_for_node(
                                    Some(parameter),
                                    None,
                                ),
                                |parameter_initializer| factory.create_conditional_expression(
                                    factory.create_strict_equality(
                                        factory.get_generated_name_for_node(
                                            Some(parameter),
                                            None,
                                        ),
                                        factory.create_void_zero()
                                    ),
                                    None,
                                    parameter_initializer,
                                    None,
                                    factory.get_generated_name_for_node(
                                        Some(parameter),
                                        None,
                                    ),
                                )
                            )
                        )
                    ),
                ],
            None,
        ),
    ));
    factory.update_parameter_declaration(
        parameter,
        parameter.maybe_decorators(),
        parameter.maybe_modifiers(),
        parameter_as_parameter_declaration.dot_dot_dot_token.clone(),
        Some(factory.get_generated_name_for_node(Some(parameter), None)),
        parameter_as_parameter_declaration.question_token.clone(),
        parameter_as_parameter_declaration.maybe_type(),
        None,
    )
}

fn add_default_value_assignment_for_initializer(
    parameter: &Node,   /*ParameterDeclaration*/
    name: &Node,        /*Identifier*/
    initializer: &Node, /*Expression*/
    context: &(impl TransformationContext + ?Sized),
) -> Gc<Node> {
    let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
    let factory = context.factory();
    context.add_initialization_statement(
        &factory.create_if_statement(
            factory.create_type_check(factory.clone_node(name), "undefined"),
            factory
                .create_block(
                    vec![factory.create_expression_statement(
                        factory
                            .create_assignment(
                                factory
                                    .clone_node(name)
                                    .set_emit_flags(EmitFlags::NoSourceMap),
                                initializer.node_wrapper().set_emit_flags(
                                    EmitFlags::NoSourceMap
                                        | get_emit_flags(initializer)
                                        | EmitFlags::NoComments,
                                ),
                            )
                            .set_text_range(Some(parameter))
                            .set_emit_flags(EmitFlags::NoComments),
                    )],
                    None,
                )
                .set_text_range(Some(parameter))
                .set_emit_flags(
                    EmitFlags::SingleLine
                        | EmitFlags::NoTrailingSourceMap
                        | EmitFlags::NoTokenSourceMaps
                        | EmitFlags::NoComments,
                ),
            None,
        ),
    );
    factory.update_parameter_declaration(
        parameter,
        parameter.maybe_decorators(),
        parameter.maybe_modifiers(),
        parameter_as_parameter_declaration.dot_dot_dot_token.clone(),
        parameter_as_parameter_declaration.maybe_name(),
        parameter_as_parameter_declaration.question_token.clone(),
        parameter_as_parameter_declaration.maybe_type(),
        None,
    )
}

pub fn visit_function_body(
    node: Option<&Node /*ConciseBody*/>,
    visitor: impl FnMut(&Node) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
) -> Option<Gc<Node /*ConciseBody*/>> {
    visit_function_body_full(
        node,
        visitor,
        context,
        Option::<
            fn(
                Option<&Node>,
                Option<&mut dyn FnMut(&Node) -> VisitResult>,
                Option<&dyn Fn(&Node) -> bool>,
                Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
            ) -> Option<Gc<Node>>,
        >::None,
    )
}

pub fn visit_function_body_full(
    node: Option<&Node /*ConciseBody*/>,
    mut visitor: impl FnMut(&Node) -> VisitResult,
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
    context.resume_lexical_environment();
    let mut node_visitor = |node: Option<&Node>,
                            visitor: Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            test: Option<&dyn Fn(&Node) -> bool>,
                            lift: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>|
     -> Option<Gc<Node>> {
        if let Some(node_visitor) = node_visitor.as_mut() {
            node_visitor(node, visitor, test, lift)
        } else {
            maybe_visit_node(
                node,
                visitor.map(|visitor| |node: &Node| visitor(node)),
                test.map(|test| |node: &Node| test(node)),
                lift.map(|lift| |node: &[Gc<Node>]| lift(node)),
            )
        }
    };
    let updated = node_visitor(node, Some(&mut visitor), Some(&is_concise_body), None);
    let declarations = context.end_lexical_environment();
    if let Some(declarations) = declarations.non_empty() {
        if updated.is_none() {
            return Some(context.factory().create_block(declarations, None));
        }
        let updated = updated.unwrap();
        let block = context
            .factory()
            .converters()
            .convert_to_function_block(&updated, None);
        let statements = get_factory()
            .merge_lexical_environment(block.as_block().statements.clone(), Some(&declarations))
            .as_node_array();
        return Some(context.factory().update_block(&block, statements));
    }
    updated
}

pub fn try_visit_function_body(
    node: Option<&Node /*ConciseBody*/>,
    visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
) -> io::Result<Option<Gc<Node /*ConciseBody*/>>> {
    try_visit_function_body_full(
        node,
        visitor,
        context,
        Option::<
            fn(
                Option<&Node>,
                Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                Option<&dyn Fn(&Node) -> bool>,
                Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
            ) -> io::Result<Option<Gc<Node>>>,
        >::None,
    )
}

pub fn try_visit_function_body_full(
    node: Option<&Node /*ConciseBody*/>,
    mut visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    mut node_visitor: Option<
        impl FnMut(
            Option<&Node>,
            Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
        ) -> io::Result<Option<Gc<Node>>>,
    >,
) -> io::Result<Option<Gc<Node /*ConciseBody*/>>> {
    context.resume_lexical_environment();
    let mut node_visitor = |node: Option<&Node>,
                            visitor: Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                            test: Option<&dyn Fn(&Node) -> bool>,
                            lift: Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>|
     -> io::Result<Option<Gc<Node>>> {
        if let Some(node_visitor) = node_visitor.as_mut() {
            node_visitor(node, visitor, test, lift)
        } else {
            try_maybe_visit_node(
                node,
                visitor.map(|visitor| |node: &Node| visitor(node)),
                test.map(|test| |node: &Node| test(node)),
                lift.map(|lift| |node: &[Gc<Node>]| lift(node)),
            )
        }
    };
    let updated = node_visitor(node, Some(&mut visitor), Some(&is_concise_body), None)?;
    let declarations = context.end_lexical_environment();
    if let Some(declarations) = declarations.non_empty() {
        if updated.is_none() {
            return Ok(Some(context.factory().create_block(declarations, None)));
        }
        let updated = updated.unwrap();
        let block = context
            .factory()
            .converters()
            .convert_to_function_block(&updated, None);
        let statements = get_factory()
            .merge_lexical_environment(block.as_block().statements.clone(), Some(&declarations))
            .as_node_array();
        return Ok(Some(context.factory().update_block(&block, statements)));
    }
    Ok(updated)
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
        body,
        Some(visitor),
        Some(is_statement),
        Some(|nodes: &[Gc<Node>]| context.factory().lift_to_block(nodes)),
    )?;
    let declarations = context.end_block_scope();
    if let Some(mut declarations) = declarations.non_empty()
    /*some(declarations)*/
    {
        if is_block(&updated) {
            declarations.extend(updated.as_block().statements.iter().cloned());
            return Ok(context.factory().update_block(&updated, declarations));
        }
        declarations.push(updated);
        return Ok(context.factory().create_block(declarations, None));
    }
    Ok(updated)
}

fn extract_single_node(nodes: &[Gc<Node>]) -> Option<Gc<Node>> {
    Debug_.assert(nodes.len() <= 1, Some("Too many nodes written to output."));
    single_or_undefined(Some(nodes)).cloned()
}
