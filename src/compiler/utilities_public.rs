use std::borrow::Borrow;
use std::cmp;
use std::ptr;
use std::rc::Rc;

use crate::{
    find, flat_map, get_emit_script_target, get_jsdoc_comments_and_tags, is_identifier, is_jsdoc,
    is_jsdoc_parameter_tag, is_jsdoc_template_tag, is_jsdoc_type_tag, is_rooted_disk_path,
    path_is_relative, skip_outer_expressions, CharacterCodes, CompilerOptions, Debug_,
    NamedDeclarationInterface, Node, NodeFlags, NodeInterface, OuterExpressionKinds, ScriptTarget,
    SyntaxKind, TextChangeRange, TextRange, TextSpan, __String, compare_diagnostics, is_block,
    is_module_block, is_source_file, sort_and_deduplicate, Diagnostic, SortedArray,
};

pub fn is_external_module_name_relative(module_name: &str) -> bool {
    path_is_relative(module_name) || is_rooted_disk_path(module_name)
}

pub fn sort_and_deduplicate_diagnostics(
    diagnostics: &[Rc<Diagnostic>],
) -> SortedArray<Rc<Diagnostic>> {
    sort_and_deduplicate(
        diagnostics,
        &|a, b| compare_diagnostics(&**a, &**b),
        Option::<&fn(&Rc<Diagnostic>, &Rc<Diagnostic>) -> bool>::None,
    )
}

pub fn get_default_lib_file_name(options: &CompilerOptions) -> &'static str {
    match get_emit_script_target(options) {
        ScriptTarget::ESNext => "lib.esnext.full.d.ts",
        ScriptTarget::ES2021 => "lib.es2021.full.d.ts",
        ScriptTarget::ES2020 => "lib.es2020.full.d.ts",
        ScriptTarget::ES2019 => "lib.es2019.full.d.ts",
        ScriptTarget::ES2018 => "lib.es2018.full.d.ts",
        ScriptTarget::ES2017 => "lib.es2017.full.d.ts",
        ScriptTarget::ES2016 => "lib.es2016.full.d.ts",
        ScriptTarget::ES2015 => "lib.es6.d.ts",
        _ => "lib.d.ts",
    }
}

pub fn text_span_end(span: &TextSpan) -> isize {
    span.start + span.length
}

pub fn text_span_is_empty(span: &TextSpan) -> bool {
    span.length == 0
}

pub fn text_span_contains_position(span: &TextSpan, position: isize) -> bool {
    position >= span.start && position < text_span_end(span)
}

pub(crate) fn text_range_contains_position_inclusive<TSpan: TextRange>(
    span: &TSpan,
    position: isize,
) -> bool {
    position >= span.pos() && position <= span.end()
}

pub fn text_span_contains_text_span(span: &TextSpan, other: &TextSpan) -> bool {
    other.start >= span.start && text_span_end(other) <= text_span_end(span)
}

pub fn text_span_overlaps_with(span: &TextSpan, other: &TextSpan) -> bool {
    text_span_overlap(span, other).is_some()
}

pub fn text_span_overlap(span1: &TextSpan, span2: &TextSpan) -> Option<TextSpan> {
    let overlap = text_span_intersection(span1, span2);
    match overlap {
        Some(overlap) if overlap.length == 0 => None,
        _ => overlap,
    }
}

pub fn text_span_intersects_with_text_span(span: &TextSpan, other: &TextSpan) -> bool {
    decoded_text_span_intersects_with(span.start, span.length, other.start, other.length)
}

pub fn text_span_intersects_with(span: &TextSpan, start: isize, length: isize) -> bool {
    decoded_text_span_intersects_with(span.start, span.length, start, length)
}

pub fn decoded_text_span_intersects_with(
    start1: isize,
    length1: isize,
    start2: isize,
    length2: isize,
) -> bool {
    let end1 = start1 + length1;
    let end2 = start2 + length2;
    start2 <= end1 && end2 >= start1
}

pub fn text_span_intersects_with_position(span: &TextSpan, position: isize) -> bool {
    position <= text_span_end(span) && position >= span.start
}

pub fn text_span_intersection(span1: &TextSpan, span2: &TextSpan) -> Option<TextSpan> {
    let start = cmp::max(span1.start, span2.start);
    let end = cmp::min(text_span_end(span1), text_span_end(span2));
    if start <= end {
        Some(create_text_span_from_bounds(start, end))
    } else {
        None
    }
}

fn create_text_span(start: isize, length: isize) -> TextSpan {
    if start < 0 {
        panic!("start < 0");
    }
    if length < 0 {
        panic!("length < 0");
    }

    TextSpan { start, length }
}

pub fn create_text_span_from_bounds(start: isize, end: isize) -> TextSpan {
    create_text_span(start, end - start)
}

pub fn text_change_range_new_span(range: &TextChangeRange) -> TextSpan {
    create_text_span(range.span.start, range.new_length)
}

pub fn text_change_range_is_unchanged(range: &TextChangeRange) -> bool {
    text_span_is_empty(&range.span) && range.new_length == 0
}

pub fn create_text_change_range(span: TextSpan, new_length: isize) -> TextChangeRange {
    if new_length < 0 {
        panic!("newLength < 0");
    }

    TextChangeRange { span, new_length }
}

lazy_static! {
    pub static ref unchanged_text_change_range: TextChangeRange =
        create_text_change_range(create_text_span(0, 0), 0);
}

pub fn collapse_text_change_ranges_across_multiple_versions(
    changes: &[TextChangeRange],
) -> TextChangeRange {
    if changes.is_empty() {
        let unchanged_text_change_range_ref: &TextChangeRange = &unchanged_text_change_range;
        return *unchanged_text_change_range_ref;
    }

    if changes.len() == 1 {
        return changes[0];
    }

    let change0 = changes[0];

    let mut old_start_n = change0.span.start;
    let mut old_end_n = text_span_end(&change0.span);
    let mut new_end_n = old_start_n + change0.new_length;

    for next_change in changes.iter().skip(1) {
        let old_start_1 = old_start_n;
        let old_end_1 = old_end_n;
        let new_end_1 = new_end_n;

        let old_start_2 = next_change.span.start;
        let old_end_2 = text_span_end(&next_change.span);
        let new_end_2 = old_start_2 + next_change.new_length;

        old_start_n = cmp::min(old_start_1, old_start_2);
        old_end_n = cmp::max(old_end_1, old_end_1 + (old_end_2 - new_end_1));
        new_end_n = cmp::max(new_end_2, old_end_2 + (new_end_1 - old_end_2));
    }

    create_text_change_range(
        create_text_span_from_bounds(old_start_n, old_end_n),
        new_end_n - old_start_n,
    )
}

pub fn get_type_parameter_owner(d: &Node /*Declaration*/) -> Option<Rc<Node>> {
    if
    /*d && */
    d.kind() == SyntaxKind::TypeParameter {
        let mut current = Some(d.node_wrapper());
        while current.is_some() {
            let current_present = current.clone().unwrap();
            if is_function_like(current)
                || is_class_like(&current_present)
                || current_present.kind() == SyntaxKind::InterfaceDeclaration
            {
                return current;
            }
            current = current_present.maybe_parent();
        }
    }
    None
}

// export type ParameterPropertyDeclaration = ParameterDeclaration & { parent: ConstructorDeclaration, name: Identifier };
pub fn is_parameter_property_declaration(node: &Node, parent: &Node) -> bool {
    has_syntactic_modifier(node, ModifierFlags::ParameterPropertyModifier)
        && parent.kind() == SyntaxKind::Constructor
}

pub fn is_empty_binding_pattern(node: &Node /*BindingName*/) -> bool {
    if is_binding_pattern(node) {
        return every(
            &node.as_binding_pattern().elements,
            is_empty_binding_element,
        );
    }
    false
}

fn get_combined_flags<TNode: NodeInterface, TCallback: FnMut(&Node) -> NodeFlags>(
    node: &TNode,
    mut get_flags: TCallback,
) -> NodeFlags {
    let mut node = Some(node.node_wrapper());
    let mut flags = get_flags(node.as_ref().unwrap());
    if node.as_ref().unwrap().kind() == SyntaxKind::VariableDeclaration {
        node = node.as_ref().unwrap().maybe_parent();
    }
    if let Some(node_present) = node.as_ref() {
        if node_present.kind() == SyntaxKind::VariableDeclarationList {
            flags |= get_flags(node_present);
            node = node_present.maybe_parent();
        }
    }
    if let Some(node) = node {
        if node.kind() == SyntaxKind::VariableStatement {
            flags |= get_flags(&*node);
        }
    }
    flags
}

pub fn get_combined_node_flags<TNode: NodeInterface>(node: &TNode) -> NodeFlags {
    get_combined_flags(node, |n| n.flags())
}

pub fn escape_leading_underscores(identifier: &str) -> __String {
    __String::new(
        if identifier.chars().count() >= 2
            && identifier.chars().nth(0).unwrap() == CharacterCodes::underscore
            && identifier.chars().nth(1).unwrap() == CharacterCodes::underscore
        {
            format!("_{}", identifier)
        } else {
            identifier.to_string()
        },
    )
}

pub fn unescape_leading_underscores(identifier: &__String) -> String {
    let mut chars = identifier.chars();
    if
    /*chars.count() >= 3 &&*/
    matches!(chars.next(), Some(CharacterCodes::underscore))
        && matches!(chars.next(), Some(CharacterCodes::underscore))
        && matches!(chars.next(), Some(CharacterCodes::underscore))
    {
        identifier.chars().skip(1).collect()
    } else {
        identifier.chars().collect()
    }
}

pub fn id_text<TNode: NodeInterface>(
    identifier_or_private_name: &TNode, /*Identifier | PrivateIdentifier*/
) -> String {
    unescape_leading_underscores(
        &identifier_or_private_name
            .node_wrapper()
            .as_identifier()
            .escaped_text,
    )
}

pub(crate) fn is_named_declaration(node: &Node) -> bool {
    node.maybe_as_named_declaration()
        .map_or(false, |node| node.maybe_name().is_some())
}

fn get_non_assigned_name_of_declaration<TNode: NodeInterface>(
    declaration: &TNode,
) -> Option<Rc<Node>> {
    Some(declaration.node_wrapper().as_named_declaration().name())
}

pub fn get_name_of_declaration<TNode: NodeInterface>(declaration: &TNode) -> Option<Rc<Node>> {
    get_non_assigned_name_of_declaration(declaration)
}

fn get_jsdoc_parameter_tags_worker(
    param: &Node, /*ParameterDeclaration*/
    no_cache: Option<bool>,
) -> Vec<Rc<Node /*JSDocParameterTag*/>> {
    let param_as_parameter_declaration = param.as_parameter_declaration();
    /*if param.name {*/
    if is_identifier(&*param_as_parameter_declaration.name()) {
        let name = param_as_parameter_declaration
            .name()
            .as_identifier()
            .escaped_text
            .clone();
        return get_jsdoc_tags_worker(&param.parent(), no_cache)
            .into_iter()
            .filter(|tag| {
                if !is_jsdoc_parameter_tag(&**tag) {
                    return false;
                }
                let tag_as_jsdoc_parameter_tag = tag.as_jsdoc_property_like_tag();
                if !is_identifier(&*tag_as_jsdoc_parameter_tag.name) {
                    return false;
                }
                tag_as_jsdoc_parameter_tag.name.as_identifier().escaped_text == name
            })
            .collect();
    } else {
        let i = param
            .parent()
            .as_signature_declaration()
            .parameters()
            .iter()
            .position(|parameter| ptr::eq(&**parameter, param));
        Debug_.assert(
            i.is_some(),
            Some("Parameters should always be in their parent's parameter lists"),
        );
        let i = i.unwrap();
        let param_tags: Vec<Rc<Node>> = get_jsdoc_tags_worker(&param.parent(), no_cache)
            .into_iter()
            .filter(|tag| is_jsdoc_parameter_tag(&**tag))
            .collect();
        if i < param_tags.len() {
            return vec![param_tags[i].clone()];
        }
    }
    /*}*/
    vec![]
}

pub fn get_jsdoc_parameter_tags(
    param: &Node, /*ParameterDeclaration*/
) -> Vec<Rc<Node /*JSDocParameterTag*/>> {
    get_jsdoc_parameter_tags_worker(param, Some(false))
}

pub(crate) fn get_jsdoc_parameter_tags_no_cache(
    param: &Node, /*ParameterDeclaration*/
) -> Vec<Rc<Node /*JSDocParameterTag*/>> {
    get_jsdoc_parameter_tags_worker(param, Some(true))
}

fn get_jsdoc_type_parameter_tags_worker(
    param: &Node, /*TypeParameterDeclaration*/
    no_cache: Option<bool>,
) -> Vec<Rc<Node /*JSDocTemplateTag*/>> {
    let name = param
        .as_type_parameter_declaration()
        .name()
        .as_identifier()
        .escaped_text
        .clone();
    get_jsdoc_tags_worker(&param.parent(), no_cache)
        .into_iter()
        .filter(|tag| {
            if !is_jsdoc_template_tag(&**tag) {
                return false;
            }
            let tag_as_jsdoc_template_tag = tag.as_jsdoc_template_tag();
            tag_as_jsdoc_template_tag.type_parameters.iter().any(|tp| {
                tp.as_type_parameter_declaration()
                    .name()
                    .as_identifier()
                    .escaped_text
                    == name
            })
        })
        .collect()
}

pub fn get_jsdoc_type_parameter_tags(
    param: &Node, /*TypeParameterDeclaration*/
) -> Vec<Rc<Node /*JSDocTemplateTag*/>> {
    get_jsdoc_type_parameter_tags_worker(param, Some(false))
}

pub(crate) fn get_jsdoc_type_parameter_tags_no_cache(
    param: &Node, /*TypeParameterDeclaration*/
) -> Vec<Rc<Node /*JSDocTemplateTag*/>> {
    get_jsdoc_type_parameter_tags_worker(param, Some(true))
}

pub fn get_jsdoc_type_tag(node: &Node) -> Option<Rc<Node /*JSDocTypeTag*/>> {
    let tag = get_first_jsdoc_tag(node, is_jsdoc_type_tag, None);
    // if matches!(tag, Some(tag) if matches!(tag.as_jsdoc_type_tag().type_expression, Some(type_expression) /*if type_expression.type_.is_some()*/))
    if tag.is_some() {
        return tag;
    }
    None
}

fn get_jsdoc_tags_worker(node: &Node, no_cache: Option<bool>) -> Vec<Rc<Node /*JSDocTag*/>> {
    let mut tags: Option<Vec<Rc<Node>>> = node.maybe_js_doc_cache().clone();
    if tags.is_none() || no_cache.unwrap_or(false) {
        let comments = get_jsdoc_comments_and_tags(node, no_cache);
        Debug_.assert(
            comments.len() < 2 || !Rc::ptr_eq(&comments[0], &comments[1]),
            None,
        );
        tags = Some(flat_map(Some(comments), |j, _| {
            if is_jsdoc(&*j) {
                j.as_jsdoc()
                    .tags
                    .as_ref()
                    .map_or(vec![], |tags| tags.iter().map(Clone::clone).collect())
            } else {
                vec![j]
            }
        }));
        if !no_cache.unwrap_or(false) {
            node.set_js_doc_cache(tags.clone().unwrap());
        }
    }
    tags.unwrap()
}

fn get_first_jsdoc_tag<TPredicate: FnMut(&Node /*JSDocTag*/) -> bool>(
    node: &Node,
    mut predicate: TPredicate,
    no_cache: Option<bool>,
) -> Option<Rc<Node>> {
    find(&get_jsdoc_tags_worker(node, no_cache), |element, _| {
        predicate(element)
    })
    .map(Clone::clone)
}

pub fn get_effective_type_parameter_declarations(
    node: &Node,
) -> Vec<Rc<Node /*TypeParameterDeclaration*/>> {
    if let Some(type_parameters) = node.as_has_type_parameters().maybe_type_parameters() {
        return type_parameters.into();
    }
    vec![]
}

pub fn is_member_name<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Identifier || node.kind() == SyntaxKind::PrivateIdentifier
}

pub fn skip_partially_emitted_expressions(node: &Node) -> Rc<Node> {
    skip_outer_expressions(
        node,
        Some(OuterExpressionKinds::PartiallyEmittedExpressions),
    )
}

pub fn is_literal_kind(kind: SyntaxKind) -> bool {
    SyntaxKind::FirstLiteralToken <= kind && kind <= SyntaxKind::LastLiteralToken
}

pub fn is_template_literal_kind(kind: SyntaxKind) -> bool {
    SyntaxKind::FirstTemplateToken <= kind && kind <= SyntaxKind::LastTemplateToken
}

pub fn is_modifier_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::AbstractKeyword
            | SyntaxKind::AsyncKeyword
            | SyntaxKind::ConstKeyword
            | SyntaxKind::DeclareKeyword
            | SyntaxKind::DefaultKeyword
            | SyntaxKind::ExportKeyword
            | SyntaxKind::PublicKeyword
            | SyntaxKind::PrivateKeyword
            | SyntaxKind::ProtectedKeyword
            | SyntaxKind::ReadonlyKeyword
            | SyntaxKind::StaticKeyword
            | SyntaxKind::OverrideKeyword
    )
}

pub fn is_property_name<TNode: NodeInterface>(node: &TNode) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Identifier
            | SyntaxKind::PrivateIdentifier
            | SyntaxKind::StringLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::ComputedPropertyName
    )
}

pub fn is_function_like<TNodeRef: Borrow<Node>>(node: Option<TNodeRef>) -> bool {
    node.map_or(false, |node| is_function_like_kind(node.borrow().kind()))
}

fn is_function_like_declaration_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::FunctionDeclaration
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
    )
}

fn is_function_like_kind(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::MethodSignature
        | SyntaxKind::CallSignature
        | SyntaxKind::JSDocSignature
        | SyntaxKind::ConstructSignature
        | SyntaxKind::IndexSignature
        | SyntaxKind::FunctionType
        | SyntaxKind::JSDocFunctionType
        | SyntaxKind::ConstructorType => true,
        _ => is_function_like_declaration_kind(kind),
    }
}

pub fn is_function_or_module_block<TNode: NodeInterface>(node: &TNode) -> bool {
    is_source_file(node)
        || is_module_block(node)
        || is_block(node) && is_function_like(node.maybe_parent())
}

pub fn is_binding_pattern<TNode: NodeInterface>(node: &TNode) -> bool {
    if true {
        let kind = node.kind();
        return kind == SyntaxKind::ArrayBindingPattern || kind == SyntaxKind::ObjectBindingPattern;
    }

    false
}

pub(crate) fn is_left_hand_side_expression(node: &Node) -> bool {
    is_left_hand_side_expression_kind(skip_partially_emitted_expressions(node).kind())
}

fn is_left_hand_side_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::ArrayLiteralExpression
        | SyntaxKind::ObjectLiteralExpression
        | SyntaxKind::Identifier
        | SyntaxKind::NumericLiteral
        | SyntaxKind::FalseKeyword
        | SyntaxKind::TrueKeyword => true,
        _ => false,
    }
}

fn is_unary_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::PrefixUnaryExpression => true,
        _ => is_left_hand_side_expression_kind(kind),
    }
}

pub fn is_expression(node: &Node) -> bool {
    is_expression_kind(skip_partially_emitted_expressions(node).kind())
}

fn is_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        _ => is_unary_expression_kind(kind),
    }
}

pub fn has_jsdoc_nodes<TNode: NodeInterface>(node: &TNode) -> bool {
    node.maybe_js_doc().map_or(false, |jsdoc| !jsdoc.is_empty())
}

pub fn has_initializer<TNode: NodeInterface>(node: &TNode) -> bool {
    node.node_wrapper()
        .maybe_as_has_initializer()
        .and_then(|node| node.maybe_initializer())
        .is_some()
}

pub fn has_only_expression_initializer<TNode: NodeInterface>(node: &TNode) -> bool {
    match node.kind() {
        SyntaxKind::VariableDeclaration
        | SyntaxKind::Parameter
        | SyntaxKind::BindingElement
        | SyntaxKind::PropertySignature
        | SyntaxKind::PropertyDeclaration
        | SyntaxKind::PropertyAssignment
        | SyntaxKind::EnumMember => true,
        _ => false,
    }
}

pub fn is_string_literal_like<TNode: NodeInterface>(node: &TNode) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::StringLiteral | SyntaxKind::NoSubstitutionTemplateLiteral
    )
}
