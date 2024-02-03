use std::{borrow::Borrow, cmp, convert::TryInto, ptr};

use id_arena::Id;

use super::{
    get_indent_size, get_indent_string, get_line_of_local_position_from_line_map,
    get_set_accessor_value_parameter,
};
use crate::{
    compute_line_and_character_of_position, flat_map, get_factory,
    get_jsdoc_deprecated_tag_no_cache, get_jsdoc_override_tag_no_cache,
    get_jsdoc_private_tag_no_cache, get_jsdoc_protected_tag_no_cache,
    get_jsdoc_public_tag_no_cache, get_jsdoc_readonly_tag_no_cache, get_jsdoc_return_type,
    get_jsdoc_tags, get_jsdoc_type, get_leading_comment_ranges, is_binary_expression,
    is_class_element, is_class_static_block_declaration, is_expression_with_type_arguments,
    is_function_declaration, is_heritage_clause, is_in_js_file, is_jsdoc_property_like_tag,
    is_jsdoc_signature, is_jsdoc_template_tag, is_jsdoc_type_alias, is_left_hand_side_expression,
    is_parameter, is_pinned_comment, is_property_access_entity_name_expression,
    is_white_space_single_line, last, maybe_filter, maybe_is_class_like, maybe_text_char_at_index,
    skip_trivia, text_char_at_index, text_substring, trim_string, AsDoubleDeref, CharacterCodes,
    CommentRange, DetachedCommentInfo, EmitTextWriter, HasArena, InArena, ModifierFlags,
    ModifiersArray, Node, NodeFlags, NodeInterface, OptionInArena, ReadonlyTextRange,
    SourceTextAsChars, SyntaxKind, TextRange,
};

pub fn get_effective_type_annotation_node(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Id<Node /*TypeNode*/>> {
    if !is_in_js_file(Some(&node.ref_(arena))) && is_function_declaration(&node.ref_(arena)) {
        return None;
    }
    let type_ = node
        .ref_(arena)
        .maybe_as_has_type()
        .and_then(|has_type| has_type.maybe_type());
    if type_.is_some() || !is_in_js_file(Some(&node.ref_(arena))) {
        return type_;
    }
    if is_jsdoc_property_like_tag(&node.ref_(arena)) {
        node.ref_(arena)
            .as_jsdoc_property_like_tag()
            .type_expression
            .map(|type_expression| type_expression.ref_(arena).as_jsdoc_type_expression().type_)
    } else {
        get_jsdoc_type(node, arena)
    }
}

pub fn get_type_annotation_node(node: &Node) -> Option<Id<Node /*TypeNode*/>> {
    node.maybe_as_has_type()
        .and_then(|has_type| has_type.maybe_type())
}

pub fn get_effective_return_type_node(
    node: Id<Node>, /*SignatureDeclaration | JSDocSignature*/
    arena: &impl HasArena,
) -> Option<Id<Node /*TypeNode*/>> {
    if is_jsdoc_signature(&node.ref_(arena)) {
        node.ref_(arena)
            .as_jsdoc_signature()
            .type_
            .and_then(|type_| {
                type_
                    .ref_(arena)
                    .as_base_jsdoc_type_like_tag()
                    .type_expression
            })
            .map(|type_expression| type_expression.ref_(arena).as_jsdoc_type_expression().type_)
    } else {
        node.ref_(arena)
            .as_signature_declaration()
            .maybe_type()
            .or_else(|| {
                if is_in_js_file(Some(&node.ref_(arena))) {
                    get_jsdoc_return_type(node, arena)
                } else {
                    None
                }
            })
    }
}

pub fn get_jsdoc_type_parameter_declarations(
    node: Id<Node>, /*DeclarationWithTypeParameters*/
    arena: &impl HasArena,
) -> Vec<Id<Node /*TypeParameterDeclaration*/>> {
    flat_map(
        Some(&*get_jsdoc_tags(node, arena).ref_(arena)),
        |&tag, _| {
            if is_non_type_alias_template(tag, arena) {
                tag.ref_(arena)
                    .as_jsdoc_template_tag()
                    .type_parameters
                    .ref_(arena)
                    .to_vec()
            } else {
                vec![]
            }
        },
    )
}

pub fn is_non_type_alias_template(tag: Id<Node> /*JSDocTag*/, arena: &impl HasArena) -> bool {
    is_jsdoc_template_tag(&tag.ref_(arena))
        && !(tag.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::JSDocComment
            && tag
                .ref_(arena)
                .parent()
                .ref_(arena)
                .as_jsdoc()
                .tags
                .map_or(false, |tags| {
                    tags.ref_(arena)
                        .iter()
                        .any(|tag| is_jsdoc_type_alias(&tag.ref_(arena)))
                }))
}

pub fn get_effective_set_accessor_type_annotation_node(
    node: Id<Node>,
    /*SetAccessorDeclaration*/ arena: &impl HasArena,
) -> Option<Id<Node /*TypeNode*/>> {
    let parameter = get_set_accessor_value_parameter(node, arena);
    parameter.and_then(|parameter| get_effective_type_annotation_node(parameter, arena))
}

pub fn emit_new_line_before_leading_comments<TNode: ReadonlyTextRange>(
    line_map: &[usize],
    writer: &dyn EmitTextWriter,
    node: &TNode,
    leading_comments: Option<&[CommentRange]>,
) {
    emit_new_line_before_leading_comments_of_position(
        line_map,
        writer,
        node.pos(),
        leading_comments,
    )
}

pub fn emit_new_line_before_leading_comments_of_position(
    line_map: &[usize],
    writer: &dyn EmitTextWriter,
    pos: isize,
    leading_comments: Option<&[CommentRange]>,
) {
    if let Some(leading_comments) = leading_comments {
        if !leading_comments.is_empty()
            && pos != leading_comments[0].pos()
            && get_line_of_local_position_from_line_map(line_map, pos.try_into().unwrap())
                != get_line_of_local_position_from_line_map(
                    line_map,
                    leading_comments[0].pos().try_into().unwrap(),
                )
        {
            writer.write_line(None);
        }
    }
}

pub fn emit_new_line_before_leading_comment_of_position(
    line_map: &[usize],
    writer: &dyn EmitTextWriter,
    pos: isize,
    comment_pos: isize,
) {
    if pos != comment_pos
        && get_line_of_local_position_from_line_map(line_map, pos.try_into().unwrap())
            != get_line_of_local_position_from_line_map(line_map, comment_pos.try_into().unwrap())
    {
        writer.write_line(None);
    }
}

pub fn emit_comments<
    TWriteComment: FnMut(&SourceTextAsChars, &[usize], &dyn EmitTextWriter, isize, isize, &str),
    TComment: Borrow<CommentRange>,
>(
    text: &SourceTextAsChars,
    line_map: &[usize],
    writer: &dyn EmitTextWriter,
    comments: Option<&[TComment]>,
    leading_separator: bool,
    trailing_separator: bool,
    new_line: &str,
    mut write_comment: TWriteComment,
) {
    comments
        .filter(|comments| !comments.is_empty())
        .map(|comments| {
            if leading_separator {
                writer.write_space(" ");
            }

            let mut emit_intervening_separator = false;
            for comment in comments {
                let comment = comment.borrow();
                if emit_intervening_separator {
                    writer.write_space(" ");
                    emit_intervening_separator = false;
                }

                write_comment(
                    text,
                    line_map,
                    writer,
                    comment.pos(),
                    comment.end(),
                    new_line,
                );
                if matches!(comment.has_trailing_new_line, Some(true)) {
                    writer.write_line(None);
                } else {
                    emit_intervening_separator = true;
                }
            }

            if emit_intervening_separator && trailing_separator {
                writer.write_space(" ");
            }
        });
}

pub fn emit_detached_comments(
    text: &SourceTextAsChars,
    line_map: &[usize],
    writer: &dyn EmitTextWriter,
    write_comment: impl FnMut(&SourceTextAsChars, &[usize], &dyn EmitTextWriter, isize, isize, &str),
    node: &impl ReadonlyTextRange,
    new_line: &str,
    remove_comments: bool,
) -> Option<DetachedCommentInfo> {
    let mut leading_comments: Option<Vec<CommentRange>> = None;
    let mut current_detached_comment_info: Option<DetachedCommentInfo> = None;
    if remove_comments {
        if node.pos() == 0 {
            let is_pinned_comment_local =
                |comment: &CommentRange| is_pinned_comment(text, comment.pos().try_into().unwrap());
            leading_comments = maybe_filter(
                get_leading_comment_ranges(text, node.pos().try_into().unwrap()).as_deref(),
                is_pinned_comment_local,
            );
        }
    } else {
        leading_comments = get_leading_comment_ranges(text, node.pos().try_into().unwrap());
    }

    if let Some(leading_comments) = leading_comments {
        let mut detached_comments: Vec<&CommentRange> = vec![];
        let mut last_comment: Option<&CommentRange> = None;

        for comment in &leading_comments {
            if let Some(last_comment) = last_comment {
                let last_comment_line = get_line_of_local_position_from_line_map(
                    line_map,
                    last_comment.end().try_into().unwrap(),
                );
                let comment_line = get_line_of_local_position_from_line_map(
                    line_map,
                    comment.pos().try_into().unwrap(),
                );

                if comment_line >= last_comment_line + 2 {
                    break;
                }
            }

            detached_comments.push(comment);
            last_comment = Some(comment);
        }

        if !detached_comments.is_empty() {
            let last_comment_line = get_line_of_local_position_from_line_map(
                line_map,
                last(&detached_comments).end().try_into().unwrap(),
            );
            let node_line = get_line_of_local_position_from_line_map(
                line_map,
                skip_trivia(text, node.pos(), None, None, None)
                    .try_into()
                    .unwrap(),
            );
            if node_line >= last_comment_line + 2 {
                emit_new_line_before_leading_comments(
                    line_map,
                    writer,
                    node,
                    Some(&leading_comments),
                );
                emit_comments(
                    text,
                    line_map,
                    writer,
                    Some(&*detached_comments),
                    false,
                    true,
                    new_line,
                    write_comment,
                );
                current_detached_comment_info = Some(DetachedCommentInfo {
                    node_pos: node.pos(),
                    detached_comment_end_pos: last(&detached_comments).end(),
                });
            }
        }
    }

    current_detached_comment_info
}

pub fn write_comment_range(
    text: &SourceTextAsChars,
    line_map: &[usize],
    writer: &dyn EmitTextWriter,
    comment_pos: usize,
    comment_end: usize,
    new_line: &str,
) {
    if matches!(
        maybe_text_char_at_index(text, comment_pos + 1),
        Some(CharacterCodes::asterisk)
    ) {
        let first_comment_line_and_character =
            compute_line_and_character_of_position(line_map, comment_pos);
        let line_count = line_map.len();
        let mut first_comment_line_indent: Option<usize> = None;
        let mut pos = comment_pos;
        let mut current_line = first_comment_line_and_character.line;
        while pos < comment_end {
            let next_line_start = if current_line + 1 == line_count {
                text.len() + 1
            } else {
                line_map[current_line + 1]
            };

            if pos != comment_pos {
                if first_comment_line_indent.is_none() {
                    first_comment_line_indent = Some(calculate_indent(
                        text,
                        line_map[first_comment_line_and_character.line],
                        comment_pos,
                    ));
                }

                let current_writer_indent_spacing = writer.get_indent() * get_indent_size();

                let spaces_to_emit = isize::try_from(current_writer_indent_spacing).unwrap()
                    - isize::try_from(first_comment_line_indent.unwrap()).unwrap()
                    + isize::try_from(calculate_indent(text, pos, next_line_start)).unwrap();
                if spaces_to_emit > 0 {
                    let spaces_to_emit: usize = spaces_to_emit.try_into().unwrap();
                    let mut number_of_single_spaces_to_emit = spaces_to_emit % get_indent_size();
                    let indent_size_space_string = get_indent_string(
                        (spaces_to_emit - number_of_single_spaces_to_emit) / get_indent_size(),
                    );

                    writer.raw_write(&indent_size_space_string);

                    while number_of_single_spaces_to_emit > 0 {
                        writer.raw_write(" ");
                        number_of_single_spaces_to_emit -= 1;
                    }
                } else {
                    writer.raw_write("");
                }
            }

            write_trimmed_current_line(text, comment_end, writer, new_line, pos, next_line_start);

            pos = next_line_start;

            current_line += 1;
        }
    } else {
        writer.write_comment(&text_substring(text, comment_pos, comment_end));
    }
}

fn write_trimmed_current_line(
    text: &SourceTextAsChars,
    comment_end: usize,
    writer: &dyn EmitTextWriter,
    new_line: &str,
    pos: usize,
    next_line_start: usize,
) {
    let end = cmp::min(comment_end, next_line_start - 1);
    let current_line_text = text_substring(text, pos, end);
    let current_line_text = trim_string(&current_line_text);
    if !current_line_text.is_empty() {
        writer.write_comment(&current_line_text);
        if end != comment_end {
            writer.write_line(None);
        }
    } else {
        writer.raw_write(new_line);
    }
}

fn calculate_indent(text: &SourceTextAsChars, mut pos: usize, end: usize) -> usize {
    let mut current_line_indent = 0;
    while pos < end && is_white_space_single_line(text_char_at_index(text, pos)) {
        if text_char_at_index(text, pos) == CharacterCodes::tab {
            current_line_indent += get_indent_size() - (current_line_indent % get_indent_size());
        } else {
            current_line_indent += 1;
        }
        pos += 1;
    }

    current_line_indent
}

pub fn has_effective_modifiers(node: Id<Node>, arena: &impl HasArena) -> bool {
    get_effective_modifier_flags(node, arena) != ModifierFlags::None
}

pub fn has_syntactic_modifiers(node: Id<Node>, arena: &impl HasArena) -> bool {
    get_syntactic_modifier_flags(node, arena) != ModifierFlags::None
}

pub fn has_effective_modifier(node: Id<Node>, flags: ModifierFlags, arena: &impl HasArena) -> bool {
    get_selected_effective_modifier_flags(node, flags, arena) != ModifierFlags::None
}

pub fn has_syntactic_modifier(node: Id<Node>, flags: ModifierFlags, arena: &impl HasArena) -> bool {
    get_selected_syntactic_modifier_flags(node, flags, arena) != ModifierFlags::None
}

pub fn is_static(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_class_element(&node.ref_(arena)) && has_static_modifier(node, arena)
        || is_class_static_block_declaration(&node.ref_(arena))
}

pub fn has_static_modifier(node: Id<Node>, arena: &impl HasArena) -> bool {
    has_syntactic_modifier(node, ModifierFlags::Static, arena)
}

pub fn has_override_modifier(node: Id<Node>, arena: &impl HasArena) -> bool {
    has_effective_modifier(node, ModifierFlags::Override, arena)
}

pub fn has_abstract_modifier(node: Id<Node>, arena: &impl HasArena) -> bool {
    has_syntactic_modifier(node, ModifierFlags::Abstract, arena)
}

pub fn has_ambient_modifier(node: Id<Node>, arena: &impl HasArena) -> bool {
    has_syntactic_modifier(node, ModifierFlags::Ambient, arena)
}

pub fn has_effective_readonly_modifier(node: Id<Node>, arena: &impl HasArena) -> bool {
    has_effective_modifier(node, ModifierFlags::Readonly, arena)
}

pub fn get_selected_effective_modifier_flags(
    node: Id<Node>,
    flags: ModifierFlags,
    arena: &impl HasArena,
) -> ModifierFlags {
    get_effective_modifier_flags(node, arena) & flags
}

fn get_selected_syntactic_modifier_flags(
    node: Id<Node>,
    flags: ModifierFlags,
    arena: &impl HasArena,
) -> ModifierFlags {
    get_syntactic_modifier_flags(node, arena) & flags
}

fn get_modifier_flags_worker(
    node: Id<Node>,
    include_jsdoc: bool,
    always_include_jsdoc: Option<bool>,
    arena: &impl HasArena,
) -> ModifierFlags {
    if node.ref_(arena).kind() >= SyntaxKind::FirstToken
        && node.ref_(arena).kind() <= SyntaxKind::LastToken
    {
        return ModifierFlags::None;
    }

    if !node
        .ref_(arena)
        .modifier_flags_cache()
        .intersects(ModifierFlags::HasComputedFlags)
    {
        node.ref_(arena).set_modifier_flags_cache(
            get_syntactic_modifier_flags_no_cache(node, arena) | ModifierFlags::HasComputedFlags,
        );
    }

    if include_jsdoc
        && !node
            .ref_(arena)
            .modifier_flags_cache()
            .intersects(ModifierFlags::HasComputedJSDocModifiers)
        && (always_include_jsdoc == Some(true) || is_in_js_file(Some(&node.ref_(arena))))
        && node.ref_(arena).maybe_parent().is_some()
    {
        node.ref_(arena).set_modifier_flags_cache(
            node.ref_(arena).modifier_flags_cache()
                | get_jsdoc_modifier_flags_no_cache(node, arena)
                | ModifierFlags::HasComputedJSDocModifiers,
        );
    }

    node.ref_(arena).modifier_flags_cache()
        & !(ModifierFlags::HasComputedFlags | ModifierFlags::HasComputedJSDocModifiers)
}

pub fn get_effective_modifier_flags(node: Id<Node>, arena: &impl HasArena) -> ModifierFlags {
    get_modifier_flags_worker(node, true, None, arena)
}

pub fn get_effective_modifier_flags_always_include_jsdoc(
    node: Id<Node>,
    arena: &impl HasArena,
) -> ModifierFlags {
    get_modifier_flags_worker(node, true, Some(true), arena)
}

pub fn get_syntactic_modifier_flags(node: Id<Node>, arena: &impl HasArena) -> ModifierFlags {
    get_modifier_flags_worker(node, false, None, arena)
}

fn get_jsdoc_modifier_flags_no_cache(node: Id<Node>, arena: &impl HasArena) -> ModifierFlags {
    let mut flags = ModifierFlags::None;
    if node.ref_(arena).maybe_parent().is_some() && !is_parameter(&node.ref_(arena)) {
        if is_in_js_file(Some(&node.ref_(arena))) {
            if get_jsdoc_public_tag_no_cache(node, arena).is_some() {
                flags |= ModifierFlags::Public;
            }
            if get_jsdoc_private_tag_no_cache(node, arena).is_some() {
                flags |= ModifierFlags::Private;
            }
            if get_jsdoc_protected_tag_no_cache(node, arena).is_some() {
                flags |= ModifierFlags::Protected;
            }
            if get_jsdoc_readonly_tag_no_cache(node, arena).is_some() {
                flags |= ModifierFlags::Readonly;
            }
            if get_jsdoc_override_tag_no_cache(node, arena).is_some() {
                flags |= ModifierFlags::Override;
            }
        }
        if get_jsdoc_deprecated_tag_no_cache(node, arena).is_some() {
            flags |= ModifierFlags::Deprecated;
        }
    }

    flags
}

pub fn get_effective_modifier_flags_no_cache(
    node: Id<Node>,
    arena: &impl HasArena,
) -> ModifierFlags {
    get_syntactic_modifier_flags_no_cache(node, arena)
        | get_jsdoc_modifier_flags_no_cache(node, arena)
}

fn get_syntactic_modifier_flags_no_cache(node: Id<Node>, arena: &impl HasArena) -> ModifierFlags {
    let mut flags = modifiers_to_flags(
        node.ref_(arena)
            .maybe_modifiers()
            .refed(arena)
            .as_double_deref(),
        arena,
    );
    if node
        .ref_(arena)
        .flags()
        .intersects(NodeFlags::NestedNamespace)
        || node.ref_(arena).kind() == SyntaxKind::Identifier
            && matches!(
                node.ref_(arena)
                    .as_identifier()
                    .maybe_is_in_jsdoc_namespace(),
                Some(true)
            )
    {
        flags |= ModifierFlags::Export;
    }
    flags
}

pub fn modifiers_to_flags(
    modifiers: Option<&[Id<Node /*Modifier*/>]>,
    arena: &impl HasArena,
) -> ModifierFlags {
    let mut flags = ModifierFlags::None;
    if let Some(modifiers) = modifiers {
        for modifier in modifiers {
            flags |= modifier_to_flag(modifier.ref_(arena).kind());
        }
    }
    flags
}

pub fn modifier_to_flag(token: SyntaxKind) -> ModifierFlags {
    match token {
        SyntaxKind::StaticKeyword => ModifierFlags::Static,
        SyntaxKind::PublicKeyword => ModifierFlags::Public,
        SyntaxKind::ProtectedKeyword => ModifierFlags::Protected,
        SyntaxKind::PrivateKeyword => ModifierFlags::Private,
        SyntaxKind::AbstractKeyword => ModifierFlags::Abstract,
        SyntaxKind::ExportKeyword => ModifierFlags::Export,
        SyntaxKind::DeclareKeyword => ModifierFlags::Ambient,
        SyntaxKind::ConstKeyword => ModifierFlags::Const,
        SyntaxKind::DefaultKeyword => ModifierFlags::Default,
        SyntaxKind::AsyncKeyword => ModifierFlags::Async,
        SyntaxKind::ReadonlyKeyword => ModifierFlags::Readonly,
        SyntaxKind::OverrideKeyword => ModifierFlags::Override,
        _ => ModifierFlags::None,
    }
}

pub fn create_modifiers(
    modifier_flags: ModifierFlags,
    arena: &impl HasArena,
) -> Option<ModifiersArray> {
    if modifier_flags != ModifierFlags::None {
        Some(get_factory(arena).create_node_array(
            Some(get_factory(arena).create_modifiers_from_modifier_flags(modifier_flags)),
            None,
        ))
    } else {
        None
    }
}

pub fn is_logical_operator(token: SyntaxKind) -> bool {
    matches!(
        token,
        SyntaxKind::BarBarToken
            | SyntaxKind::AmpersandAmpersandToken
            | SyntaxKind::ExclamationToken
    )
}

pub fn is_logical_or_coalescing_assignment_operator(token: SyntaxKind) -> bool {
    matches!(
        token,
        SyntaxKind::BarBarEqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken
    )
}

pub fn is_logical_or_coalescing_assignment_expression(
    expr: Id<Node>, /*BinaryExpression*/
    arena: &impl HasArena,
) -> bool {
    is_logical_or_coalescing_assignment_operator(
        expr.ref_(arena)
            .as_binary_expression()
            .operator_token
            .ref_(arena)
            .kind(),
    )
}

pub fn is_assignment_operator(token: SyntaxKind) -> bool {
    token >= SyntaxKind::FirstAssignment && token <= SyntaxKind::LastAssignment
}

pub fn try_get_class_extending_expression_with_type_arguments(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Id<Node /*ClassLikeDeclaration*/>> {
    let cls = try_get_class_implementing_or_extending_expression_with_type_arguments(node, arena);
    cls.filter(|cls| !cls.is_implements).map(|cls| cls.class)
}

pub struct ClassImplementingOrExtendingExpressionWithTypeArguments {
    pub class: Id<Node /*ClassLikeDeclaration*/>,
    pub is_implements: bool,
}

pub fn try_get_class_implementing_or_extending_expression_with_type_arguments(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<ClassImplementingOrExtendingExpressionWithTypeArguments> {
    if is_expression_with_type_arguments(&node.ref_(arena))
        && is_heritage_clause(&node.ref_(arena).parent().ref_(arena))
        && maybe_is_class_like(
            node.ref_(arena)
                .parent()
                .ref_(arena)
                .maybe_parent()
                .map(|parent| parent.ref_(arena))
                .as_deref(),
        )
    {
        Some(ClassImplementingOrExtendingExpressionWithTypeArguments {
            class: node.ref_(arena).parent().ref_(arena).parent(),
            is_implements: node
                .ref_(arena)
                .parent()
                .ref_(arena)
                .as_heritage_clause()
                .token
                == SyntaxKind::ImplementsKeyword,
        })
    } else {
        None
    }
}

pub fn is_assignment_expression(
    node: Id<Node>,
    exclude_compound_assignment: Option<bool>,
    arena: &impl HasArena,
) -> bool {
    let exclude_compound_assignment = exclude_compound_assignment.unwrap_or(false);
    if !is_binary_expression(&node.ref_(arena)) {
        return false;
    }
    let node_ref = node.ref_(arena);
    let node_as_binary_expression = node_ref.as_binary_expression();
    (if exclude_compound_assignment {
        node_as_binary_expression.operator_token.ref_(arena).kind() == SyntaxKind::EqualsToken
    } else {
        is_assignment_operator(node_as_binary_expression.operator_token.ref_(arena).kind())
    }) && is_left_hand_side_expression(node_as_binary_expression.left, arena)
}

pub fn is_left_hand_side_of_assignment(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_assignment_expression(node.ref_(arena).parent(), None, arena)
        && node
            .ref_(arena)
            .parent()
            .ref_(arena)
            .as_binary_expression()
            .left
            == node
}

pub fn is_destructuring_assignment(node: Id<Node>, arena: &impl HasArena) -> bool {
    if is_assignment_expression(node, Some(true), arena) {
        let kind = node
            .ref_(arena)
            .as_binary_expression()
            .left
            .ref_(arena)
            .kind();
        return matches!(
            kind,
            SyntaxKind::ObjectLiteralExpression | SyntaxKind::ArrayLiteralExpression
        );
    }

    false
}

pub fn is_expression_with_type_arguments_in_class_extends_clause(
    node: Id<Node>,
    arena: &impl HasArena,
) -> bool {
    try_get_class_extending_expression_with_type_arguments(node, arena).is_some()
}

pub fn is_entity_name_expression(node: Id<Node>, arena: &impl HasArena) -> bool {
    node.ref_(arena).kind() == SyntaxKind::Identifier
        || is_property_access_entity_name_expression(node, arena)
}
