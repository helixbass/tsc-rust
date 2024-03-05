use std::io;

use id_arena::Id;

use crate::{
    is_expression, try_visit_node, HasArena, InArena, Node, TransformationContext, VisitResult, _d,
    get_factory, get_source_text_of_node_from_source_file, has_invalid_escape, is_external_module,
    is_no_substitution_template_literal, regex, released, try_visit_each_child, Matches, NodeArray,
    NodeExt, NodeInterface, SyntaxKind, TokenFlags,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ProcessLevel {
    LiftRestriction,
    All,
}

pub fn process_tagged_template_expression(
    context: &(impl TransformationContext + ?Sized),
    node: Id<Node>, /*TaggedTemplateExpression*/
    mut visitor: impl FnMut(Id<Node>) -> VisitResult,
    current_source_file: Id<Node>, /*SourceFile*/
    record_tagged_template_string: impl FnMut(Id<Node> /*Identifier*/),
    level: ProcessLevel,
    arena: &impl HasArena,
) -> Id<Node> {
    try_process_tagged_template_expression(
        context,
        node,
        |node: Id<Node>| Ok(visitor(node)),
        current_source_file,
        record_tagged_template_string,
        level,
        arena,
    )
    .unwrap()
}

pub fn try_process_tagged_template_expression(
    context: &(impl TransformationContext + ?Sized),
    node: Id<Node>, /*TaggedTemplateExpression*/
    mut visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    current_source_file: Id<Node>, /*SourceFile*/
    mut record_tagged_template_string: impl FnMut(Id<Node> /*Identifier*/),
    level: ProcessLevel,
    arena: &impl HasArena,
) -> io::Result<Id<Node>> {
    let tag = try_visit_node(
        node.ref_(arena).as_tagged_template_expression().tag,
        Some(|node: Id<Node>| visitor(node)),
        Some(|node| is_expression(node, arena)),
        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
    )?;

    let mut template_arguments: Vec<Id<Node /*Expression*/>> = _d();
    let mut cooked_strings: Vec<Id<Node /*Expression*/>> = _d();
    let mut raw_strings: Vec<Id<Node /*Expression*/>> = _d();
    let template = node.ref_(arena).as_tagged_template_expression().template;

    if level == ProcessLevel::LiftRestriction && !has_invalid_escape(template, arena) {
        return try_visit_each_child(node, |node: Id<Node>| visitor(node), context, arena);
    }

    if is_no_substitution_template_literal(&template.ref_(arena)) {
        cooked_strings.push(create_template_cooked(template, arena));
        raw_strings.push(get_raw_literal(template, current_source_file, arena));
    } else {
        cooked_strings.push(create_template_cooked(
            template.ref_(arena).as_template_expression().head,
            arena,
        ));
        raw_strings.push(get_raw_literal(
            template.ref_(arena).as_template_expression().head,
            current_source_file,
            arena,
        ));
        for &template_span in &*template
            .ref_(arena)
            .as_template_expression()
            .template_spans
            .ref_(arena)
        {
            cooked_strings.push(create_template_cooked(
                template_span.ref_(arena).as_template_span().literal,
                arena,
            ));
            raw_strings.push(get_raw_literal(
                template_span.ref_(arena).as_template_span().literal,
                current_source_file,
                arena,
            ));
            template_arguments.push(try_visit_node(
                template_span.ref_(arena).as_template_span().expression,
                Some(|node: Id<Node>| visitor(node)),
                Some(|node| is_expression(node, arena)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?);
        }
    }

    let helper_call = context
        .get_emit_helper_factory()
        .ref_(arena)
        .create_template_object_helper(
            get_factory(arena).create_array_literal_expression(Some(cooked_strings), None),
            get_factory(arena).create_array_literal_expression(Some(raw_strings), None),
        );

    template_arguments.insert(
        0,
        if is_external_module(&current_source_file.ref_(arena)) {
            let temp_var = get_factory(arena).create_unique_name("templateObject", None);
            record_tagged_template_string(temp_var);
            get_factory(arena).create_logical_or(
                temp_var,
                get_factory(arena).create_assignment(temp_var, helper_call),
            )
        } else {
            helper_call
        },
    );

    Ok(get_factory(arena).create_call_expression(
        tag,
        Option::<Id<NodeArray>>::None,
        Some(template_arguments),
    ))
}

fn create_template_cooked(
    template: Id<Node>, /*TemplateHead | TemplateMiddle | TemplateTail | NoSubstitutionTemplateLiteral*/
    arena: &impl HasArena,
) -> Id<Node> {
    if template
        .ref_(arena)
        .as_template_literal_like_node()
        .maybe_template_flags()
        .matches(|template_flags| template_flags != TokenFlags::None)
    {
        get_factory(arena).create_void_zero()
    } else {
        get_factory(arena).create_string_literal(
            released!(template.ref_(arena).as_literal_like_node().text().clone()),
            None,
            None,
        )
    }
}

fn get_raw_literal(
    node: Id<Node>,                /*TemplateLiteralLikeNode*/
    current_source_file: Id<Node>, /*SourceFile*/
    arena: &impl HasArena,
) -> Id<Node> {
    let text = node
        .ref_(arena)
        .as_template_literal_like_node()
        .maybe_raw_text()
        .map(ToOwned::to_owned)
        .unwrap_or_else(|| {
            // Debug.assertIsDefined(currentSourceFile,
            //                       "Template literal node is missing 'rawText' and does not have a source file. Possibly bad transform.");
            let text =
                get_source_text_of_node_from_source_file(current_source_file, node, None, arena);

            let is_last = matches!(
                node.ref_(arena).kind(),
                SyntaxKind::NoSubstitutionTemplateLiteral | SyntaxKind::TemplateTail
            );
            text[1..(text.len() - if is_last { 1 } else { 2 })].to_owned()
        });

    let text = regex!(r#"\r\n?"#).replace_all(&text, "\n");
    get_factory(arena)
        .create_string_literal(text.into_owned(), None, None)
        .set_text_range(Some(&*node.ref_(arena)), arena)
}
