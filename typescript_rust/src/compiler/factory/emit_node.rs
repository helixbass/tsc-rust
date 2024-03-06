use std::rc::Rc;

use id_arena::Id;

use crate::{
    get_parse_tree_node, is_parse_tree_node, maybe_get_source_file_of_node, push_if_unique_eq,
    return_if_none, BaseTextRange, Debug_, EmitFlags, EmitHelper, EmitNode, GetOrInsertDefault,
    HasArena, InArena, Node, NodeInterface, NonEmpty, ReadonlyTextRange, SnippetElement,
    SourceMapRange, StringOrNumber, SyntaxKind, SynthesizedComment,
};

pub(crate) fn get_or_create_emit_node(node: Id<Node>, arena: &impl HasArena) -> Id<EmitNode> {
    match node.ref_(arena).maybe_emit_node() {
        None => {
            if is_parse_tree_node(&node.ref_(arena)) {
                if node.ref_(arena).kind() == SyntaxKind::SourceFile {
                    let ret = arena.alloc_emit_node(EmitNode {
                        annotated_nodes: Some(vec![node]),
                        // ..Default::default()
                        flags: Default::default(),
                        leading_comments: Default::default(),
                        trailing_comments: Default::default(),
                        comment_range: Default::default(),
                        source_map_range: Default::default(),
                        token_source_map_ranges: Default::default(),
                        constant_value: Default::default(),
                        external_helpers_module_name: Default::default(),
                        external_helpers: Default::default(),
                        helpers: Default::default(),
                        starts_on_new_line: Default::default(),
                        snippet_element: Default::default(),
                    });
                    node.ref_(arena).set_emit_node(Some(ret.clone()));
                    return ret;
                }

                let source_file = maybe_get_source_file_of_node(
                    get_parse_tree_node(
                        maybe_get_source_file_of_node(Some(node), arena),
                        Option::<fn(Id<Node>) -> bool>::None,
                        arena,
                    ),
                    arena,
                )
                .unwrap_or_else(|| Debug_.fail(Some("Could not determine parsed source file.")));
                get_or_create_emit_node(source_file, arena)
                    .ref_mut(arena)
                    .annotated_nodes
                    .as_mut()
                    .unwrap()
                    .push(node);
            }

            node.ref_(arena)
                .set_emit_node(Some(arena.alloc_emit_node(Default::default())));
        }
        Some(node_emit_node) => {
            Debug_.assert(
                !node_emit_node
                    .ref_(arena)
                    .flags
                    .map_or(false, |flags| flags.intersects(EmitFlags::Immutable)),
                Some("Invalid attempt to mutate an immutable node."),
            );
        }
    }
    node.ref_(arena).maybe_emit_node().unwrap()
}

pub fn dispose_emit_nodes(source_file: Option<Id<Node> /*SourceFile*/>, arena: &impl HasArena) {
    let annotated_nodes = maybe_get_source_file_of_node(
        get_parse_tree_node(source_file, Option::<fn(Id<Node>) -> _>::None, arena),
        arena,
    )
    .and_then(|source_file| source_file.ref_(arena).maybe_emit_node())
    .and_then(|emit_node| emit_node.ref_(arena).annotated_nodes.clone());
    if let Some(annotated_nodes) = annotated_nodes.as_ref() {
        for node in annotated_nodes {
            node.ref_(arena).set_emit_node(None);
        }
    }
}

pub fn remove_all_comments(node: Id<Node>, arena: &impl HasArena) -> Id<Node> {
    let emit_node = get_or_create_emit_node(node, arena);
    let mut emit_node = emit_node.ref_mut(arena);
    emit_node.flags = Some(emit_node.flags.unwrap_or_default() | EmitFlags::NoComments);
    emit_node.leading_comments = None;
    emit_node.trailing_comments = None;
    node
}

pub fn set_emit_flags(node: Id<Node>, emit_flags: EmitFlags, arena: &impl HasArena) -> Id<Node> {
    get_or_create_emit_node(node, arena).ref_mut(arena).flags = Some(emit_flags);
    node
}

pub fn add_emit_flags(node: Id<Node>, emit_flags: EmitFlags, arena: &impl HasArena) -> Id<Node> {
    let emit_node = get_or_create_emit_node(node, arena);
    let mut emit_node = emit_node.ref_mut(arena);
    emit_node.flags = Some(emit_node.flags.unwrap_or(EmitFlags::None) | emit_flags);
    node
}

pub fn get_source_map_range(node: &Node, arena: &impl HasArena) -> Id<SourceMapRange> {
    node.maybe_emit_node()
        .and_then(|node_emit_node| node_emit_node.ref_(arena).source_map_range.clone())
        .unwrap_or_else(|| arena.alloc_source_map_range(node.into()))
}

pub fn set_source_map_range(
    node: Id<Node>,
    range: Option<Id<SourceMapRange>>,
    arena: &impl HasArena,
) -> Id<Node> {
    get_or_create_emit_node(node, arena)
        .ref_mut(arena)
        .source_map_range = range;
    node
}

pub fn set_token_source_map_range(
    node: Id<Node>,
    token: SyntaxKind,
    range: Option<Id<SourceMapRange>>,
    arena: &impl HasArena,
) -> Id<Node> {
    let emit_node = get_or_create_emit_node(node, arena);
    emit_node
        .ref_mut(arena)
        .token_source_map_ranges
        .get_or_insert_default_()
        .insert(token, range);
    node
}

pub(crate) fn get_starts_on_new_line(node: Id<Node>, arena: &impl HasArena) -> Option<bool> {
    node.ref_(arena)
        .maybe_emit_node()
        .and_then(|emit_node| emit_node.ref_(arena).starts_on_new_line)
}

pub(crate) fn set_starts_on_new_line(node: Id<Node>, new_line: bool, arena: &impl HasArena)
/*-> Id<Node>*/
{
    get_or_create_emit_node(node, arena)
        .ref_mut(arena)
        .starts_on_new_line = Some(new_line);
    // node
}

pub fn get_comment_range(node: Id<Node>, arena: &impl HasArena) -> BaseTextRange {
    // TODO: these semantics wouldn't work if the return value is treated mutably?
    node.ref_(arena)
        .maybe_emit_node()
        .and_then(|node_emit_node| node_emit_node.ref_(arena).comment_range.clone())
        .unwrap_or_else(|| BaseTextRange::new(node.ref_(arena).pos(), node.ref_(arena).end()))
}

pub fn set_comment_range(
    node: Id<Node>,
    range: &impl ReadonlyTextRange, /*TextRange*/
    arena: &impl HasArena,
) -> Id<Node> {
    get_or_create_emit_node(node, arena)
        .ref_mut(arena)
        .comment_range = Some(range.into());
    node
}

pub fn get_synthetic_leading_comments(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Vec<Rc<SynthesizedComment>>> {
    node.ref_(arena)
        .maybe_emit_node()
        .and_then(|node_emit_node| node_emit_node.ref_(arena).leading_comments.clone())
}

pub fn set_synthetic_leading_comments(
    node: Id<Node>,
    comments: Option<Vec<Rc<SynthesizedComment>>>,
    arena: &impl HasArena,
) -> Id<Node> {
    get_or_create_emit_node(node, arena)
        .ref_mut(arena)
        .leading_comments = comments;
    node
}

pub fn add_synthetic_leading_comment(
    node: Id<Node>,
    kind: SyntaxKind, /*SyntaxKind.SingleLineCommentTrivia | SyntaxKind.MultiLineCommentTrivia*/
    text: &str,
    has_trailing_new_line: Option<bool>,
    arena: &impl HasArena,
) /*-> Id<Node>*/
{
    let mut synthetic_leading_comments = get_synthetic_leading_comments(node, arena);
    if synthetic_leading_comments.is_none() {
        synthetic_leading_comments = Some(Default::default());
    }
    synthetic_leading_comments
        .as_mut()
        .unwrap()
        .push(Rc::new(SynthesizedComment {
            kind,
            has_trailing_new_line,
            text: text.to_owned(),
            has_leading_new_line: None,
        }));
    set_synthetic_leading_comments(node, synthetic_leading_comments, arena);
}

pub fn get_synthetic_trailing_comments(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Vec<Rc<SynthesizedComment>>> {
    node.ref_(arena)
        .maybe_emit_node()
        .and_then(|node_emit_node| node_emit_node.ref_(arena).trailing_comments.clone())
}

pub fn set_synthetic_trailing_comments(
    node: Id<Node>,
    comments: Option<Vec<Rc<SynthesizedComment>>>,
    arena: &impl HasArena,
)
/*-> Id<Node>*/
{
    get_or_create_emit_node(node, arena)
        .ref_mut(arena)
        .trailing_comments = comments;
}

pub fn add_synthetic_trailing_comment(
    node: Id<Node>,
    kind: SyntaxKind, /*SyntaxKind.SingleLineCommentTrivia | SyntaxKind.MultiLineCommentTrivia*/
    text: &str,
    has_trailing_new_line: Option<bool>,
    arena: &impl HasArena,
) /*-> Id<Node>*/
{
    let mut synthetic_trailing_comments = get_synthetic_trailing_comments(node, arena);
    if synthetic_trailing_comments.is_none() {
        synthetic_trailing_comments = Some(Default::default());
    }
    synthetic_trailing_comments
        .as_mut()
        .unwrap()
        .push(Rc::new(SynthesizedComment {
            kind,
            has_trailing_new_line,
            text: text.to_owned(),
            has_leading_new_line: None,
        }));
    set_synthetic_trailing_comments(node, synthetic_trailing_comments, arena);
}

pub fn move_synthetic_comments(
    node: Id<Node>,
    original: Id<Node>,
    arena: &impl HasArena,
) -> Id<Node> {
    set_synthetic_leading_comments(node, get_synthetic_leading_comments(original, arena), arena);
    set_synthetic_trailing_comments(
        node,
        get_synthetic_trailing_comments(original, arena),
        arena,
    );
    let emit = get_or_create_emit_node(original, arena);
    let mut emit = emit.ref_mut(arena);
    emit.leading_comments = None;
    emit.trailing_comments = None;
    node
}

pub fn get_constant_value(
    node: Id<Node /*AccessExpression*/>,
    arena: &impl HasArena,
) -> Option<StringOrNumber> {
    node.ref_(arena)
        .maybe_emit_node()
        .and_then(|node_emit_node| node_emit_node.ref_(arena).constant_value.clone())
}

pub fn set_constant_value(
    node: Id<Node>, /*AccessExpression*/
    value: StringOrNumber,
    arena: &impl HasArena,
) -> Id<Node /*AccessExpression*/> {
    let emit_node = get_or_create_emit_node(node, arena);
    emit_node.ref_mut(arena).constant_value = Some(value);
    node
}

pub fn add_emit_helper(node: Id<Node>, helper: Id<EmitHelper>, arena: &impl HasArena) -> Id<Node> {
    let emit_node = get_or_create_emit_node(node, arena);
    emit_node
        .ref_mut(arena)
        .helpers
        .get_or_insert_default_()
        .push(helper);
    node
}

pub fn add_emit_helpers(
    node: Id<Node>,
    helpers: Option<&[Id<EmitHelper>]>,
    arena: &impl HasArena,
) -> Id<Node> {
    if let Some(helpers) = helpers.non_empty() {
        let emit_node = get_or_create_emit_node(node, arena);
        let mut emit_node = emit_node.ref_mut(arena);
        for helper in helpers {
            push_if_unique_eq(emit_node.helpers.get_or_insert_default_(), helper);
        }
    }
    node
}

pub fn get_emit_helpers(node: Id<Node>, arena: &impl HasArena) -> Option<Vec<Id<EmitHelper>>> {
    node.ref_(arena)
        .maybe_emit_node()
        .and_then(|node_emit_node| node_emit_node.ref_(arena).helpers.clone())
}

pub fn move_emit_helpers(
    source: Id<Node>,
    target: Id<Node>,
    mut predicate: impl FnMut(&EmitHelper) -> bool,
    arena: &impl HasArena,
) {
    let source_emit_node = return_if_none!(source.ref_(arena).maybe_emit_node());
    if source_emit_node.ref_(arena).helpers.is_none() {
        return;
    }
    if source_emit_node
        .ref_(arena)
        .helpers
        .as_ref()
        .unwrap()
        .is_empty()
    {
        return;
    }

    let target_emit_node = get_or_create_emit_node(target, arena);
    let mut target_emit_node = target_emit_node.ref_mut(arena);
    let mut helpers_removed = 0;
    for i in 0..source_emit_node.ref_(arena).helpers.as_ref().unwrap().len() {
        let helper = source_emit_node.ref_(arena).helpers.as_ref().unwrap()[i].clone();
        if predicate(&helper.ref_(arena)) {
            helpers_removed += 1;
            target_emit_node
                .helpers
                .get_or_insert_default_()
                .push(helper);
        } else if helpers_removed > 0 {
            source_emit_node.ref_(arena).helpers.as_mut().unwrap()[i - helpers_removed] = helper;
        }
    }

    if helpers_removed > 0 {
        source_emit_node
            .ref_(arena)
            .helpers
            .as_mut()
            .unwrap()
            .truncate(source_emit_helpers.len() - helpers_removed);
    }
}

pub(crate) fn get_snippet_element(node: Id<Node>, arena: &impl HasArena) -> Option<SnippetElement> {
    node.ref_(arena)
        .maybe_emit_node()
        .and_then(|node_emit_node| node_emit_node.ref_(arena).snippet_element)
}
