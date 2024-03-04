use std::io;

use id_arena::Id;

use crate::{
    is_block, is_statement, set_text_range_pos_end, single_or_undefined, Debug_,
    HasInitializerInterface, HasTypeInterface, NamedDeclarationInterface, Node, NodeArray,
    NodeArrayExt, NodeExt, NodeInterface, NonEmpty, OptionTry, ReadonlyTextRange,
    SingleNodeOrVecNode, TransformationContext, VisitResult, VisitResultInterface, _d,
    get_emit_flags, get_emit_script_target, get_factory, is_binding_pattern, is_concise_body,
    is_parameter_declaration, return_ok_default_if_none, EmitFlags, HasArena, InArena,
    LexicalEnvironmentFlags, OptionInArena, ScriptTarget,
};

mod try_visit_each_child;
mod visit_each_child;
pub use try_visit_each_child::*;
pub use visit_each_child::*;

use crate::released;

#[inline(always)]
pub fn visit_node(
    node: Id<Node>,
    visitor: Option<impl FnMut(Id<Node>) -> VisitResult>,
    test: Option<impl Fn(Id<Node>) -> bool>,
    lift: Option<impl Fn(&[Id<Node>]) -> Id<Node>>,
) -> Id<Node> {
    maybe_visit_node(Some(node), visitor, test, lift).unwrap()
}

#[inline(always)]
pub fn maybe_visit_node(
    node: Option<Id<Node>>,
    visitor: Option<impl FnMut(Id<Node>) -> VisitResult>,
    test: Option<impl Fn(Id<Node>) -> bool>,
    lift: Option<impl Fn(&[Id<Node>]) -> Id<Node>>,
) -> Option<Id<Node>> {
    try_maybe_visit_node(
        node,
        visitor.map(|mut visitor| move |node: Id<Node>| Ok(visitor(node))),
        test,
        lift,
    )
    .unwrap()
}

#[inline(always)]
pub fn try_visit_node(
    node: Id<Node>,
    visitor: Option<impl FnMut(Id<Node>) -> io::Result<VisitResult>>,
    test: Option<impl Fn(Id<Node>) -> bool>,
    lift: Option<impl Fn(&[Id<Node>]) -> Id<Node>>,
) -> io::Result<Id<Node>> {
    Ok(try_maybe_visit_node(Some(node), visitor, test, lift)?.unwrap())
}

pub fn try_maybe_visit_node(
    node: Option<Id<Node>>,
    visitor: Option<impl FnMut(Id<Node>) -> io::Result<VisitResult>>,
    test: Option<impl Fn(Id<Node>) -> bool>,
    lift: Option<impl Fn(&[Id<Node>]) -> Id<Node>>,
) -> io::Result<Option<Id<Node>>> {
    let node = return_ok_default_if_none!(node);
    let Some(mut visitor) = visitor else {
        return Ok(Some(node));
    };

    let visited = visitor(node)?;
    if visited.ptr_eq_node(node) {
        return Ok(Some(node));
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

    Debug_.assert_node(visited_node, test, None);
    Ok(visited_node)
}

#[inline(always)]
pub fn visit_nodes(
    nodes: Id<NodeArray>,
    visitor: Option<impl FnMut(Id<Node>) -> VisitResult>,
    test: Option<impl Fn(Id<Node>) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
    arena: &impl HasArena,
) -> Id<NodeArray> {
    maybe_visit_nodes(Some(nodes), visitor, test, start, count, arena).unwrap()
}

pub fn maybe_visit_nodes(
    nodes: Option<Id<NodeArray>>,
    visitor: Option<impl FnMut(Id<Node>) -> VisitResult>,
    test: Option<impl Fn(Id<Node>) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
    arena: &impl HasArena,
) -> Option<Id<NodeArray>> {
    let nodes = nodes?;
    if visitor.is_none() {
        return Some(nodes);
    }
    let mut visitor = visitor.unwrap();

    let mut updated: Option<Vec<Id<Node>>> = Default::default();

    let length = nodes.ref_(arena).len();
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
        has_trailing_comma = Some(nodes.ref_(arena).has_trailing_comma && start + count == length);
    }

    for i in 0..count {
        let node = nodes.ref_(arena).get(i + start).copied();
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
                updated = Some(nodes.ref_(arena)[0..i].to_owned());
                has_trailing_comma = Some(nodes.ref_(arena).has_trailing_comma);
                pos = nodes.ref_(arena).pos();
                end = nodes.ref_(arena).end();
            }
            if let Some(visited) = visited {
                match &visited {
                    SingleNodeOrVecNode::VecNode(visited) => {
                        for &visited_node in visited {
                            Debug_.assert_node(
                                Some(visited_node),
                                test.as_ref().map(|test| |node: Id<Node>| test(node)),
                                None,
                            );
                            updated.as_mut().unwrap().push(visited_node.clone());
                        }
                    }
                    SingleNodeOrVecNode::SingleNode(visited) => {
                        let visited = *visited;
                        Debug_.assert_node(
                            Some(visited),
                            test.as_ref().map(|test| |node: Id<Node>| test(node)),
                            None,
                        );
                        updated.as_mut().unwrap().push(visited.clone());
                    }
                }
            }
        }
    }

    if let Some(updated) = updated {
        let updated_array = get_factory(arena).create_node_array(Some(updated), has_trailing_comma);
        set_text_range_pos_end(&*updated_array.ref_(arena), pos, end);
        return Some(updated_array);
    }

    Some(nodes)
}

#[inline(always)]
pub fn try_visit_nodes(
    nodes: Id<NodeArray>,
    visitor: Option<impl FnMut(Id<Node>) -> io::Result<VisitResult>>,
    test: Option<impl Fn(Id<Node>) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
    arena: &impl HasArena,
) -> io::Result<Id<NodeArray>> {
    Ok(try_maybe_visit_nodes(Some(nodes), visitor, test, start, count, arena)?.unwrap())
}

pub fn try_maybe_visit_nodes(
    nodes: Option<Id<NodeArray>>,
    visitor: Option<impl FnMut(Id<Node>) -> io::Result<VisitResult>>,
    test: Option<impl Fn(Id<Node>) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
    arena: &impl HasArena,
) -> io::Result<Option<Id<NodeArray>>> {
    let Some(nodes) = nodes else {
        return Ok(None);
    };
    let Some(mut visitor) = visitor else {
        return Ok(Some(nodes));
    };

    let mut updated: Option<Vec<Id<Node>>> = Default::default();

    let length = nodes.ref_(arena).len();
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
        has_trailing_comma = Some(nodes.ref_(arena).has_trailing_comma && start + count == length);
    }

    for i in 0..count {
        let node = nodes.ref_(arena).get(i + start).copied();
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
                updated = Some(nodes.ref_(arena)[0..i].to_owned());
                has_trailing_comma = Some(nodes.ref_(arena).has_trailing_comma);
                pos = nodes.ref_(arena).pos();
                end = nodes.ref_(arena).end();
            }
            if let Some(visited) = visited {
                match &visited {
                    SingleNodeOrVecNode::VecNode(visited) => {
                        for &visited_node in visited {
                            Debug_.assert_node(
                                Some(visited_node),
                                test.as_ref().map(|test| |node: Id<Node>| test(node)),
                                None,
                            );
                            updated.as_mut().unwrap().push(visited_node.clone());
                        }
                    }
                    SingleNodeOrVecNode::SingleNode(visited) => {
                        let visited = *visited;
                        Debug_.assert_node(
                            Some(visited),
                            test.as_ref().map(|test| |node: Id<Node>| test(node)),
                            None,
                        );
                        updated.as_mut().unwrap().push(visited.clone());
                    }
                }
            }
        }
    }

    if let Some(updated) = updated {
        let updated_array = get_factory(arena).create_node_array(Some(updated), has_trailing_comma);
        set_text_range_pos_end(&*updated_array.ref_(arena), pos, end);
        return Ok(Some(updated_array));
    }

    Ok(Some(nodes))
}

pub fn visit_lexical_environment(
    statements: Id<NodeArray>, /*<Statement>*/
    mut visitor: impl FnMut(Id<Node>) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    start: Option<usize>,
    ensure_use_strict: Option<bool>,
    mut nodes_visitor: Option<
        impl FnMut(
            Option<Id<NodeArray>>,
            Option<&mut dyn FnMut(Id<Node>) -> VisitResult>,
            Option<&dyn Fn(Id<Node>) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> Option<Id<NodeArray>>,
    >,
    arena: &impl HasArena,
) -> Id<NodeArray> {
    // try_visit_lexical_environment_full(
    //     statements,
    //     |node: Id<Node>| Ok(visitor(node)),
    //     context,
    //     start,
    //     ensure_use_strict,
    //     nodes_visitor.map(|mut nodes_visitor| {
    //         move |nodes: Option<&NodeArray>,
    //               visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
    //               test: Option<&dyn Fn(Id<Node>) -> bool>,
    //               start: Option<usize>,
    //               count: Option<usize>| {
    //             let mut visitor = visitor.map(|visitor| {
    //                 Box::new(move |node: Id<Node>| visitor(node).unwrap())
    //                     as Box<dyn FnMut(Id<Node>) -> VisitResult>
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
    let mut nodes_visitor_or_default =
        |nodes: Option<Id<NodeArray>>,
         visitor: Option<&mut dyn FnMut(Id<Node>) -> VisitResult>,
         test: Option<&dyn Fn(Id<Node>) -> bool>,
         start: Option<usize>,
         count: Option<usize>|
         -> Option<Id<NodeArray>> {
            if let Some(nodes_visitor) = nodes_visitor.as_mut() {
                nodes_visitor(nodes, visitor, test, start, count)
            } else {
                maybe_visit_nodes(
                    nodes,
                    visitor.map(|visitor| |node: Id<Node>| visitor(node)),
                    test.map(|test| |node: Id<Node>| test(node)),
                    start,
                    count,
                    arena,
                )
            }
        };
    let mut statements = nodes_visitor_or_default(
        Some(statements),
        Some(&mut |node: Id<Node>| visitor(node)),
        Some(&|node: Id<Node>| is_statement(node, arena)),
        start,
        None,
    )
    .unwrap();
    if ensure_use_strict == Some(true) {
        statements = context.factory().ref_(arena).ensure_use_strict(statements);
    }
    get_factory(arena)
        .merge_lexical_environment(statements, context.end_lexical_environment().as_deref())
        .as_node_array()
}

pub fn try_visit_lexical_environment(
    statements: Id<NodeArray>, /*<Statement>*/
    visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> io::Result<Id<NodeArray>> {
    try_visit_lexical_environment_full(
        statements,
        visitor,
        context,
        None,
        None,
        Option::<
            fn(
                Option<Id<NodeArray>>,
                Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                Option<&dyn Fn(Id<Node>) -> bool>,
                Option<usize>,
                Option<usize>,
            ) -> io::Result<Option<Id<NodeArray>>>,
        >::None,
        arena,
    )
}

pub fn try_visit_lexical_environment_full(
    statements: Id<NodeArray>, /*<Statement>*/
    mut visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    start: Option<usize>,
    ensure_use_strict: Option<bool>,
    mut nodes_visitor: Option<
        impl FnMut(
            Option<Id<NodeArray>>,
            Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
            Option<&dyn Fn(Id<Node>) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> io::Result<Option<Id<NodeArray>>>,
    >,
    arena: &impl HasArena,
) -> io::Result<Id<NodeArray>> {
    context.start_lexical_environment();
    let mut nodes_visitor_or_default =
        |nodes: Option<Id<NodeArray>>,
         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
         test: Option<&dyn Fn(Id<Node>) -> bool>,
         start: Option<usize>,
         count: Option<usize>|
         -> io::Result<Option<Id<NodeArray>>> {
            if let Some(nodes_visitor) = nodes_visitor.as_mut() {
                nodes_visitor(nodes, visitor, test, start, count)
            } else {
                try_maybe_visit_nodes(
                    nodes,
                    visitor.map(|visitor| |node: Id<Node>| visitor(node)),
                    test.map(|test| |node: Id<Node>| test(node)),
                    start,
                    count,
                    arena,
                )
            }
        };
    let mut statements = nodes_visitor_or_default(
        Some(statements),
        Some(&mut |node: Id<Node>| visitor(node)),
        Some(&|node: Id<Node>| is_statement(node, arena)),
        start,
        None,
    )?
    .unwrap();
    if ensure_use_strict == Some(true) {
        statements = context.factory().ref_(arena).ensure_use_strict(statements);
    }
    Ok(get_factory(arena)
        .merge_lexical_environment(statements, context.end_lexical_environment().as_deref())
        .as_node_array())
}

pub fn visit_parameter_list(
    nodes: Option<Id<NodeArray>>,
    visitor: impl FnMut(Id<Node>) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> Option<Id<NodeArray>> {
    visit_parameter_list_full(
        nodes,
        visitor,
        context,
        Option::<
            fn(
                Option<Id<NodeArray>>,
                Option<&mut dyn FnMut(Id<Node>) -> VisitResult>,
                Option<&dyn Fn(Id<Node>) -> bool>,
                Option<usize>,
                Option<usize>,
            ) -> Option<Id<NodeArray>>,
        >::None,
        arena,
    )
}

pub fn visit_parameter_list_full(
    nodes: Option<Id<NodeArray>>,
    mut visitor: impl FnMut(Id<Node>) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    mut nodes_visitor: Option<
        impl FnMut(
            Option<Id<NodeArray>>,
            Option<&mut dyn FnMut(Id<Node>) -> VisitResult>,
            Option<&dyn Fn(Id<Node>) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> Option<Id<NodeArray>>,
    >,
    arena: &impl HasArena,
) -> Option<Id<NodeArray>> {
    let mut nodes_visitor = |nodes: Option<Id<NodeArray>>,
                             visitor: Option<&mut dyn FnMut(Id<Node>) -> VisitResult>,
                             test: Option<&dyn Fn(Id<Node>) -> bool>,
                             start: Option<usize>,
                             count: Option<usize>|
     -> Option<Id<NodeArray>> {
        if let Some(nodes_visitor) = nodes_visitor.as_mut() {
            nodes_visitor(nodes, visitor, test, start, count)
        } else {
            maybe_visit_nodes(
                nodes,
                visitor.map(|visitor| |node: Id<Node>| visitor(node)),
                test.map(|test| |node: Id<Node>| test(node)),
                start,
                count,
                arena,
            )
        }
    };
    let mut updated: Option<Id<NodeArray /*<ParameterDeclaration>*/>> = _d();
    context.start_lexical_environment();
    if let Some(nodes) = nodes {
        context.set_lexical_environment_flags(LexicalEnvironmentFlags::InParameters, true);
        updated = nodes_visitor(
            Some(nodes),
            Some(&mut visitor),
            Some(&|node: Id<Node>| is_parameter_declaration(node, arena)),
            None,
            None,
        );

        if context
            .get_lexical_environment_flags()
            .intersects(LexicalEnvironmentFlags::VariablesHoistedInParameters)
            && get_emit_script_target(&context.get_compiler_options().ref_(arena))
                >= ScriptTarget::ES2015
        {
            updated = Some(add_default_value_assignments_if_needed(
                updated.unwrap(),
                context,
                arena,
            ));
        }
        context.set_lexical_environment_flags(LexicalEnvironmentFlags::InParameters, false);
    }
    context.suspend_lexical_environment();
    updated
}

pub fn try_visit_parameter_list(
    nodes: Option<Id<NodeArray>>,
    visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> io::Result<Option<Id<NodeArray>>> {
    try_visit_parameter_list_full(
        nodes,
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
        arena,
    )
}

pub fn try_visit_parameter_list_full(
    nodes: Option<Id<NodeArray>>,
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
    arena: &impl HasArena,
) -> io::Result<Option<Id<NodeArray>>> {
    let mut nodes_visitor =
        |nodes: Option<Id<NodeArray>>,
         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
         test: Option<&dyn Fn(Id<Node>) -> bool>,
         start: Option<usize>,
         count: Option<usize>|
         -> io::Result<Option<Id<NodeArray>>> {
            if let Some(nodes_visitor) = nodes_visitor.as_mut() {
                nodes_visitor(nodes, visitor, test, start, count)
            } else {
                try_maybe_visit_nodes(
                    nodes,
                    visitor.map(|visitor| |node: Id<Node>| visitor(node)),
                    test.map(|test| |node: Id<Node>| test(node)),
                    start,
                    count,
                    arena,
                )
            }
        };
    let mut updated: Option<Id<NodeArray /*<ParameterDeclaration>*/>> = _d();
    context.start_lexical_environment();
    if let Some(nodes) = nodes {
        context.set_lexical_environment_flags(LexicalEnvironmentFlags::InParameters, true);
        updated = nodes_visitor(
            Some(nodes),
            Some(&mut visitor),
            Some(&|node: Id<Node>| is_parameter_declaration(node, arena)),
            None,
            None,
        )?;

        if context
            .get_lexical_environment_flags()
            .intersects(LexicalEnvironmentFlags::VariablesHoistedInParameters)
            && get_emit_script_target(&context.get_compiler_options().ref_(arena))
                >= ScriptTarget::ES2015
        {
            updated = Some(add_default_value_assignments_if_needed(
                updated.unwrap(),
                context,
                arena,
            ));
        }
        context.set_lexical_environment_flags(LexicalEnvironmentFlags::InParameters, false);
    }
    context.suspend_lexical_environment();
    Ok(updated)
}

fn add_default_value_assignments_if_needed(
    parameters: Id<NodeArray>, /*<ParameterDeclaration>*/
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> Id<NodeArray /*<ParameterDeclaration>*/> {
    let mut result: Option<Vec<Id<Node /*ParameterDeclaration*/>>> = _d();
    for (i, parameter) in released!(parameters.ref_(arena).clone()).iter().enumerate() {
        let parameter = *parameter;
        let updated = add_default_value_assignment_if_needed(parameter, context, arena);
        if result.is_some() || updated != parameter {
            result
                .get_or_insert_with(|| parameters.ref_(arena)[..i].to_owned())
                .push(updated);
        }
    }
    result.map_or(parameters, |result| {
        context
            .factory()
            .ref_(arena)
            .create_node_array(
                Some(result),
                Some(parameters.ref_(arena).has_trailing_comma),
            )
            .set_text_range(Some(&*parameters.ref_(arena)), arena)
    })
}

fn add_default_value_assignment_if_needed(
    parameter: Id<Node>, /*ParameterDeclaration*/
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> Id<Node> {
    if parameter
        .ref_(arena)
        .as_parameter_declaration()
        .dot_dot_dot_token
        .is_some()
    {
        parameter
    } else if is_binding_pattern(
        parameter
            .ref_(arena)
            .as_parameter_declaration()
            .maybe_name()
            .refed(arena)
            .as_deref(),
    ) {
        add_default_value_assignment_for_binding_pattern(parameter, context, arena)
    } else if let Some(parameter_initializer) = released!(parameter
        .ref_(arena)
        .as_parameter_declaration()
        .maybe_initializer())
    {
        add_default_value_assignment_for_initializer(
            parameter,
            released!(parameter.ref_(arena).as_parameter_declaration().name()),
            parameter_initializer,
            context,
            arena,
        )
    } else {
        parameter
    }
}

fn add_default_value_assignment_for_binding_pattern(
    parameter: Id<Node>, /*ParameterDeclaration*/
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> Id<Node> {
    let factory = context.factory();
    context.add_initialization_statement(factory.ref_(arena).create_variable_statement(
        Option::<Id<NodeArray>>::None,
        factory.ref_(arena).create_variable_declaration_list(
            vec![factory.ref_(arena).create_variable_declaration(
                    released!(parameter.ref_(arena).as_parameter_declaration().maybe_name()),
                    None,
                    released!(parameter.ref_(arena).as_parameter_declaration().maybe_type()),
                    Some(
                        released!(parameter.ref_(arena).as_parameter_declaration()
                            .maybe_initializer())
                            .map_or_else(
                                || {
                                    factory
                                        .ref_(arena)
                                        .get_generated_name_for_node(Some(parameter), None)
                                },
                                |parameter_initializer| {
                                    factory.ref_(arena).create_conditional_expression(
                                        factory.ref_(arena).create_strict_equality(
                                            factory
                                                .ref_(arena)
                                                .get_generated_name_for_node(Some(parameter), None),
                                            factory.ref_(arena).create_void_zero(),
                                        ),
                                        None,
                                        parameter_initializer,
                                        None,
                                        factory
                                            .ref_(arena)
                                            .get_generated_name_for_node(Some(parameter), None),
                                    )
                                },
                            ),
                    ),
                )],
            None,
        ),
    ));
    factory.ref_(arena).update_parameter_declaration(
        parameter,
        released!(parameter.ref_(arena).maybe_decorators()),
        released!(parameter.ref_(arena).maybe_modifiers()),
        released!(
            parameter
                .ref_(arena)
                .as_parameter_declaration()
                .dot_dot_dot_token
        ),
        Some(
            factory
                .ref_(arena)
                .get_generated_name_for_node(Some(parameter), None),
        ),
        released!(
            parameter
                .ref_(arena)
                .as_parameter_declaration()
                .question_token
        ),
        released!(parameter
            .ref_(arena)
            .as_parameter_declaration()
            .maybe_type()),
        None,
    )
}

fn add_default_value_assignment_for_initializer(
    parameter: Id<Node>,   /*ParameterDeclaration*/
    name: Id<Node>,        /*Identifier*/
    initializer: Id<Node>, /*Expression*/
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> Id<Node> {
    let factory = context.factory();
    context.add_initialization_statement(
        factory.ref_(arena).create_if_statement(
            factory
                .ref_(arena)
                .create_type_check(factory.ref_(arena).clone_node(name), "undefined"),
            released!(factory
                .ref_(arena)
                .create_block(
                    vec![factory
                        .ref_(arena)
                        .create_expression_statement(released!(factory
                            .ref_(arena)
                            .create_assignment(
                                factory
                                    .ref_(arena)
                                    .clone_node(name)
                                    .set_emit_flags(EmitFlags::NoSourceMap, arena),
                                initializer.set_emit_flags(
                                    EmitFlags::NoSourceMap
                                        | get_emit_flags(initializer, arena)
                                        | EmitFlags::NoComments,
                                    arena,
                                ),
                            )
                            .set_text_range(Some(&*parameter.ref_(arena)), arena)
                            .set_emit_flags(EmitFlags::NoComments, arena)))],
                    None,
                )
                .set_text_range(Some(&*parameter.ref_(arena)), arena)
                .set_emit_flags(
                    EmitFlags::SingleLine
                        | EmitFlags::NoTrailingSourceMap
                        | EmitFlags::NoTokenSourceMaps
                        | EmitFlags::NoComments,
                    arena,
                )),
            None,
        ),
    );
    factory.ref_(arena).update_parameter_declaration(
        parameter,
        released!(parameter.ref_(arena).maybe_decorators()),
        released!(parameter.ref_(arena).maybe_modifiers()),
        released!(
            parameter
                .ref_(arena)
                .as_parameter_declaration()
                .dot_dot_dot_token
        ),
        released!(parameter
            .ref_(arena)
            .as_parameter_declaration()
            .maybe_name()),
        released!(
            parameter
                .ref_(arena)
                .as_parameter_declaration()
                .question_token
        ),
        released!(parameter
            .ref_(arena)
            .as_parameter_declaration()
            .maybe_type()),
        None,
    )
}

pub fn visit_function_body(
    node: Option<Id<Node> /*ConciseBody*/>,
    visitor: impl FnMut(Id<Node>) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> Option<Id<Node /*ConciseBody*/>> {
    visit_function_body_full(
        node,
        visitor,
        context,
        Option::<
            fn(
                Option<Id<Node>>,
                Option<&mut dyn FnMut(Id<Node>) -> VisitResult>,
                Option<&dyn Fn(Id<Node>) -> bool>,
                Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>,
            ) -> Option<Id<Node>>,
        >::None,
        arena,
    )
}

pub fn visit_function_body_full(
    node: Option<Id<Node> /*ConciseBody*/>,
    mut visitor: impl FnMut(Id<Node>) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    mut node_visitor: Option<
        impl FnMut(
            Option<Id<Node>>,
            Option<&mut dyn FnMut(Id<Node>) -> VisitResult>,
            Option<&dyn Fn(Id<Node>) -> bool>,
            Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>,
        ) -> Option<Id<Node>>,
    >,
    arena: &impl HasArena,
) -> Option<Id<Node /*ConciseBody*/>> {
    context.resume_lexical_environment();
    let mut node_visitor = |node: Option<Id<Node>>,
                            visitor: Option<&mut dyn FnMut(Id<Node>) -> VisitResult>,
                            test: Option<&dyn Fn(Id<Node>) -> bool>,
                            lift: Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>|
     -> Option<Id<Node>> {
        if let Some(node_visitor) = node_visitor.as_mut() {
            node_visitor(node, visitor, test, lift)
        } else {
            maybe_visit_node(
                node,
                visitor.map(|visitor| |node: Id<Node>| visitor(node)),
                test.map(|test| |node: Id<Node>| test(node)),
                lift.map(|lift| |node: &[Id<Node>]| lift(node)),
            )
        }
    };
    let updated = node_visitor(
        node,
        Some(&mut visitor),
        Some(&|node: Id<Node>| is_concise_body(node, arena)),
        None,
    );
    let declarations = context.end_lexical_environment();
    if let Some(declarations) = declarations.non_empty() {
        if updated.is_none() {
            return Some(
                context
                    .factory()
                    .ref_(arena)
                    .create_block(declarations, None),
            );
        }
        let updated = updated.unwrap();
        let block = context
            .factory()
            .ref_(arena)
            .converters()
            .convert_to_function_block(updated, None);
        let statements = get_factory(arena)
            .merge_lexical_environment(
                block.ref_(arena).as_block().statements.clone(),
                Some(&declarations),
            )
            .as_node_array();
        return Some(
            context
                .factory()
                .ref_(arena)
                .update_block(block, statements),
        );
    }
    updated
}

pub fn try_visit_function_body(
    node: Option<Id<Node> /*ConciseBody*/>,
    visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> io::Result<Option<Id<Node /*ConciseBody*/>>> {
    try_visit_function_body_full(
        node,
        visitor,
        context,
        Option::<
            fn(
                Option<Id<Node>>,
                Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                Option<&dyn Fn(Id<Node>) -> bool>,
                Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>,
            ) -> io::Result<Option<Id<Node>>>,
        >::None,
        arena,
    )
}

pub fn try_visit_function_body_full(
    node: Option<Id<Node> /*ConciseBody*/>,
    mut visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    mut node_visitor: Option<
        impl FnMut(
            Option<Id<Node>>,
            Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
            Option<&dyn Fn(Id<Node>) -> bool>,
            Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>,
        ) -> io::Result<Option<Id<Node>>>,
    >,
    arena: &impl HasArena,
) -> io::Result<Option<Id<Node /*ConciseBody*/>>> {
    context.resume_lexical_environment();
    let mut node_visitor =
        |node: Option<Id<Node>>,
         visitor: Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
         test: Option<&dyn Fn(Id<Node>) -> bool>,
         lift: Option<&dyn Fn(&[Id<Node>]) -> Id<Node>>|
         -> io::Result<Option<Id<Node>>> {
            if let Some(node_visitor) = node_visitor.as_mut() {
                node_visitor(node, visitor, test, lift)
            } else {
                try_maybe_visit_node(
                    node,
                    visitor.map(|visitor| |node: Id<Node>| visitor(node)),
                    test.map(|test| |node: Id<Node>| test(node)),
                    lift.map(|lift| |node: &[Id<Node>]| lift(node)),
                )
            }
        };
    let updated = node_visitor(
        node,
        Some(&mut visitor),
        Some(&|node: Id<Node>| is_concise_body(node, arena)),
        None,
    )?;
    let declarations = context.end_lexical_environment();
    if let Some(declarations) = declarations.non_empty() {
        if updated.is_none() {
            return Ok(Some(
                context
                    .factory()
                    .ref_(arena)
                    .create_block(declarations, None),
            ));
        }
        let updated = updated.unwrap();
        let block = context
            .factory()
            .ref_(arena)
            .converters()
            .convert_to_function_block(updated, None);
        let statements = get_factory(arena)
            .merge_lexical_environment(
                block.ref_(arena).as_block().statements.clone(),
                Some(&declarations),
            )
            .as_node_array();
        return Ok(Some(
            context
                .factory()
                .ref_(arena)
                .update_block(block, statements),
        ));
    }
    Ok(updated)
}

pub fn visit_iteration_body(
    body: Id<Node>, /*Statement*/
    mut visitor: impl FnMut(Id<Node>) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> Id<Node /*Statement*/> {
    try_visit_iteration_body(body, |a| Ok(visitor(a)), context, arena).unwrap()
}

pub fn try_visit_iteration_body(
    body: Id<Node>, /*Statement*/
    visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    arena: &impl HasArena,
) -> io::Result<Id<Node /*Statement*/>> {
    context.start_block_scope();
    let updated = try_visit_node(
        body,
        Some(visitor),
        Some(|node: Id<Node>| is_statement(node, arena)),
        Some(|nodes: &[Id<Node>]| context.factory().ref_(arena).lift_to_block(nodes)),
    )?;
    let declarations = context.end_block_scope();
    if let Some(mut declarations) = declarations.non_empty()
    /*some(declarations)*/
    {
        if is_block(&updated.ref_(arena)) {
            declarations.extend(
                updated
                    .ref_(arena)
                    .as_block()
                    .statements
                    .ref_(arena)
                    .iter()
                    .cloned(),
            );
            return Ok(context
                .factory()
                .ref_(arena)
                .update_block(updated, declarations));
        }
        declarations.push(updated);
        return Ok(context
            .factory()
            .ref_(arena)
            .create_block(declarations, None));
    }
    Ok(updated)
}

fn extract_single_node(nodes: &[Id<Node>]) -> Option<Id<Node>> {
    Debug_.assert(nodes.len() <= 1, Some("Too many nodes written to output."));
    single_or_undefined(Some(nodes)).cloned()
}
