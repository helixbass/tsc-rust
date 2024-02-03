use std::{convert::TryInto};

use id_arena::Id;

use crate::{
    find_ancestor, first_or_undefined, for_each_child_recursively,
    get_effective_type_annotation_node, get_root_declaration, has_jsdoc_nodes,
    has_syntactic_modifier, ignored_paths, is_binary_expression, is_comma_list_expression,
    is_expression_node, is_expression_statement, is_for_statement, is_identifier, is_jsdoc_node,
    is_parameter, is_parenthesized_expression, is_part_of_type_query,
    is_shorthand_property_assignment, is_type_reference_node, is_void_expression, last,
    parameter_is_this_keyword, some, string_contains, CharacterCodes, CompilerOptions, Debug_,
    FindAncestorCallbackReturn, ForEachChildRecursivelyCallbackReturn, HasArena, InArena,
    ModifierFlags, NamedDeclarationInterface, Node, NodeArray, NodeFlags, NodeInterface,
    PseudoBigInt, ReadonlyTextRange, Symbol, SymbolInterface, SyntaxKind,
};

pub fn skip_type_checking(
    source_file: &Node, /*SourceFile*/
    options: &CompilerOptions,
    is_source_of_project_reference_redirect: impl Fn(&str) -> bool,
) -> bool {
    let source_file_as_source_file = source_file.as_source_file();
    (options.skip_lib_check == Some(true) && source_file_as_source_file.is_declaration_file()
        || options.skip_default_lib_check == Some(true)
            && source_file_as_source_file.has_no_default_lib())
        || is_source_of_project_reference_redirect(&source_file_as_source_file.file_name())
}

pub fn parse_pseudo_big_int(string_value: &str) -> String {
    let string_value = string_value.chars().collect::<Vec<_>>();
    let log_2_base: usize;
    match string_value.get(1).copied() {
        Some(CharacterCodes::b) | Some(CharacterCodes::B) => {
            log_2_base = 1;
        }
        Some(CharacterCodes::o) | Some(CharacterCodes::O) => {
            log_2_base = 3;
        }
        Some(CharacterCodes::x) | Some(CharacterCodes::X) => {
            log_2_base = 4;
        }
        _ => {
            let n_index = string_value.len() - 1;
            let mut non_zero_start = 0;
            while string_value.get(non_zero_start).copied() == Some(CharacterCodes::_0) {
                non_zero_start += 1;
            }
            return if non_zero_start < n_index {
                string_value[non_zero_start..n_index].into_iter().collect()
            } else {
                "0".to_owned()
            };
        }
    }

    let start_index = 2;
    let end_index = string_value.len() - 1;
    let bits_needed = (end_index - start_index) * log_2_base;
    let mut segments: Vec<u16> =
        vec![0; (bits_needed >> 4) + if bits_needed & 15 != 0 { 1 } else { 0 }];
    let mut i = end_index - 1;
    let mut bit_offset = 0;
    while i >= start_index {
        let segment = bit_offset >> 4;
        let digit_char = string_value[i];
        let digit: u32 = if digit_char <= CharacterCodes::_9 {
            Into::<u32>::into(digit_char) - Into::<u32>::into(CharacterCodes::_0)
        } else {
            10 + Into::<u32>::into(digit_char)
                - Into::<u32>::into(if digit_char <= CharacterCodes::F {
                    CharacterCodes::A
                } else {
                    CharacterCodes::a
                })
        };
        let shifted_digit = digit << (bit_offset & 15);
        segments[segment] |= TryInto::<u16>::try_into(shifted_digit).unwrap();
        let residual = shifted_digit >> 16;
        if residual != 0 {
            segments[segment + 1] |= TryInto::<u16>::try_into(residual).unwrap();
        }
        i -= 1;
        bit_offset += log_2_base;
    }

    let mut base_10_value = "".to_owned();
    let mut first_nonzero_segment = segments.len() - 1;
    let mut segments_remaining = true;
    while segments_remaining {
        let mut mod_10 = 0;
        segments_remaining = false;
        let mut segment = first_nonzero_segment;
        loop
        /*while (segment >= 0)*/
        {
            let new_segment = mod_10 << 16 | Into::<u32>::into(segments[segment]);
            let segment_value: u32 = (new_segment / 10) | 0;
            segments[segment] = TryInto::<u16>::try_into(segment_value).unwrap();
            mod_10 = new_segment - segment_value * 10;
            if segment_value != 0 && !segments_remaining {
                first_nonzero_segment = segment;
                segments_remaining = true;
            }
            if segment == 0 {
                break;
            } else {
                segment -= 1;
            }
        }
        base_10_value.insert_str(0, &format!("{}", mod_10));
    }
    base_10_value
}

pub fn pseudo_big_int_to_string(pseudo_big_int: &PseudoBigInt) -> String {
    let negative = pseudo_big_int.negative;
    let base_10_value = &pseudo_big_int.base_10_value;
    format!(
        "{}{}",
        if negative && base_10_value != "0" {
            "-"
        } else {
            ""
        },
        base_10_value
    )
}

pub fn is_valid_type_only_alias_use_site(use_site: Id<Node>, arena: &impl HasArena) -> bool {
    use_site.ref_(arena).flags().intersects(NodeFlags::Ambient)
        || is_part_of_type_query(use_site, arena)
        || is_identifier_in_non_emitting_heritage_clause(use_site, arena)
        || is_part_of_possibly_valid_type_or_abstract_computed_property_name(use_site, arena)
        || !(is_expression_node(use_site, arena)
            || is_shorthand_property_name_use_site(use_site, arena))
}

fn is_shorthand_property_name_use_site(use_site: Id<Node>, arena: &impl HasArena) -> bool {
    is_identifier(&use_site.ref_(arena))
        && is_shorthand_property_assignment(&use_site.ref_(arena).parent().ref_(arena))
        && use_site
            .ref_(arena)
            .parent()
            .ref_(arena)
            .as_shorthand_property_assignment()
            .name()
            == use_site
}

fn is_part_of_possibly_valid_type_or_abstract_computed_property_name(
    mut node: Id<Node>,
    arena: &impl HasArena,
) -> bool {
    while matches!(
        node.ref_(arena).kind(),
        SyntaxKind::Identifier | SyntaxKind::PropertyAccessExpression
    ) {
        node = node.ref_(arena).parent();
    }
    if node.ref_(arena).kind() != SyntaxKind::ComputedPropertyName {
        return false;
    }
    if has_syntactic_modifier(node.ref_(arena).parent(), ModifierFlags::Abstract, arena) {
        return true;
    }
    let container_kind = node
        .ref_(arena)
        .parent()
        .ref_(arena)
        .parent()
        .ref_(arena)
        .kind();
    matches!(
        container_kind,
        SyntaxKind::InterfaceDeclaration | SyntaxKind::TypeLiteral
    )
}

fn is_identifier_in_non_emitting_heritage_clause(node: Id<Node>, arena: &impl HasArena) -> bool {
    if node.ref_(arena).kind() != SyntaxKind::Identifier {
        return false;
    }
    let heritage_clause = find_ancestor(
        node.ref_(arena).maybe_parent(),
        |parent| match parent.ref_(arena).kind() {
            SyntaxKind::HeritageClause => true.into(),
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ExpressionWithTypeArguments => {
                false.into()
            }
            _ => FindAncestorCallbackReturn::Quit,
        },
        arena,
    );
    matches!(
        heritage_clause,
        Some(heritage_clause) if heritage_clause.ref_(arena).as_heritage_clause().token == SyntaxKind::ImplementsKeyword ||
            heritage_clause.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::InterfaceDeclaration
    )
}

pub fn is_identifier_type_reference(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_type_reference_node(&node.ref_(arena))
        && is_identifier(
            &node
                .ref_(arena)
                .as_type_reference_node()
                .type_name
                .ref_(arena),
        )
}

pub fn array_is_homogeneous<TItem, TComparer: FnMut(&TItem, &TItem) -> bool>(
    array: &[TItem],
    mut comparer: TComparer, /*= equateValues*/
) -> bool {
    if array.len() < 2 {
        return true;
    }
    let first = &array[0];
    for i in 1..array.len() {
        let target = &array[i];
        if !comparer(first, target) {
            return false;
        }
    }
    true
}

pub fn set_text_range_pos<TRange: ReadonlyTextRange + ?Sized>(
    range: &TRange,
    pos: isize,
) -> &TRange {
    range.set_pos(pos);
    range
}

pub(crate) fn set_text_range_end<TRange: ReadonlyTextRange + ?Sized>(
    range: &TRange,
    end: isize,
) -> &TRange {
    range.set_end(end);
    range
}

pub fn set_text_range_pos_end(range: &(impl ReadonlyTextRange + ?Sized), pos: isize, end: isize) {
    set_text_range_end(set_text_range_pos(range, pos), end);
}

pub fn set_text_range_pos_width(range: &impl ReadonlyTextRange, pos: isize, width: isize) {
    set_text_range_pos_end(range, pos, pos + width);
}

pub fn set_node_flags(node: Option<&Node>, new_flags: NodeFlags) -> Option<&Node> {
    node.map(|node| {
        node.set_flags(new_flags);
        node
    })
}

pub fn set_parent(child: &Node, parent: Option<Id<Node>>) -> &Node {
    if let Some(parent) = parent {
        child.set_parent(Some(parent));
    }
    child
}

pub fn maybe_set_parent(child: Option<&Node>, parent: Option<Id<Node>>) -> Option<&Node> {
    if let Some(child) = child {
        if let Some(parent) = parent {
            child.set_parent(Some(parent.clone()));
        }
    }
    child
}

pub fn set_parent_recursive(root_node: Option<Id<Node>>, incremental: bool, arena: &impl HasArena) {
    let Some(root_node) = root_node else {
        return /*rootNode*/;
    };
    let is_jsdoc_node_root_node = is_jsdoc_node(&root_node.ref_(arena));
    for_each_child_recursively(
        root_node,
        |child, parent| -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
            if is_jsdoc_node_root_node {
                bind_parent_to_child_ignoring_jsdoc(incremental, &child.ref_(arena), parent)
            } else {
                bind_parent_to_child(incremental, child, parent, arena)
            }
        },
        Option::<fn(Id<NodeArray>, Id<Node>) -> Option<ForEachChildRecursivelyCallbackReturn<()>>>::None,
        arena,
    );
}

fn bind_parent_to_child_ignoring_jsdoc(
    incremental: bool,
    child: &Node,
    parent: Id<Node>,
) -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
    if incremental
        && matches!(
            child.maybe_parent(),
            Some(child_parent) if child_parent == parent
        )
    {
        return Some(ForEachChildRecursivelyCallbackReturn::Skip);
    }
    set_parent(child, Some(parent));
    None
}

fn bind_jsdoc(
    incremental: bool,
    child: Id<Node>,
    arena: &impl HasArena,
) -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
    if has_jsdoc_nodes(&child.ref_(arena)) {
        for &doc in &child.ref_(arena).maybe_js_doc().unwrap() {
            bind_parent_to_child_ignoring_jsdoc(incremental, &doc.ref_(arena), child);
            for_each_child_recursively(
                doc,
                |child, parent| -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
                    bind_parent_to_child_ignoring_jsdoc(incremental, &child.ref_(arena), parent)
                },
                Option::<
                    fn(
                        Id<NodeArray>,
                        Id<Node>,
                    ) -> Option<ForEachChildRecursivelyCallbackReturn<()>>,
                >::None,
                arena,
            );
        }
    }
    None
}

fn bind_parent_to_child(
    incremental: bool,
    child: Id<Node>,
    parent: Id<Node>,
    arena: &impl HasArena,
) -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
    bind_parent_to_child_ignoring_jsdoc(incremental, &child.ref_(arena), parent)
        .or_else(|| bind_jsdoc(incremental, child, arena))
}

pub fn is_packed_array_literal(_node: Id<Node> /*Expression*/) -> bool {
    unimplemented!()
}

pub fn expression_result_is_unused(
    mut node: Id<Node>, /*Expression*/
    arena: &impl HasArena,
) -> bool {
    Debug_.assert_is_defined(&node.ref_(arena).maybe_parent(), None);
    loop {
        let parent = node.ref_(arena).parent();
        if is_parenthesized_expression(&parent.ref_(arena)) {
            node = parent;
            continue;
        }
        if is_expression_statement(&parent.ref_(arena))
            || is_void_expression(&parent.ref_(arena))
            || is_for_statement(&parent.ref_(arena)) && {
                let parent_ref = parent.ref_(arena);
                let parent_as_for_statement = parent_ref.as_for_statement();
                matches!(
                    parent_as_for_statement.initializer,
                    Some(parent_initializer) if parent_initializer == node
                ) || matches!(
                    parent_as_for_statement.incrementor,
                    Some(parent_incrementor) if parent_incrementor == node
                )
            }
        {
            return true;
        }
        if is_comma_list_expression(&parent.ref_(arena)) {
            if node
                != *last(
                    &parent
                        .ref_(arena)
                        .as_comma_list_expression()
                        .elements
                        .ref_(arena),
                )
            {
                return true;
            }
            node = parent;
            continue;
        }
        if is_binary_expression(&parent.ref_(arena))
            && parent
                .ref_(arena)
                .as_binary_expression()
                .operator_token
                .ref_(arena)
                .kind()
                == SyntaxKind::CommaToken
        {
            if node == parent.ref_(arena).as_binary_expression().left {
                return true;
            }
            node = parent;
            continue;
        }
        return false;
    }
}

pub fn contains_ignored_path(path: &str) -> bool {
    some(
        Some(&**ignored_paths),
        Some(|p: &&str| string_contains(path, *p)),
    )
}

pub fn get_containing_node_array(_node: Id<Node>) -> Option<Id<NodeArray>> {
    unimplemented!()
}

pub fn has_context_sensitive_parameters(
    node: Id<Node>, /*FunctionLikeDeclaration*/
    arena: &impl HasArena,
) -> bool {
    let node_ref = node.ref_(arena);
    let node_as_function_like_declaration = node_ref.as_function_like_declaration();
    if node_as_function_like_declaration
        .maybe_type_parameters()
        .is_none()
    {
        if some(
            Some(&*node_as_function_like_declaration.parameters().ref_(arena)),
            Some(|&p: &Id<Node>| get_effective_type_annotation_node(p, arena).is_none()),
        ) {
            return true;
        }
        if node_ref.kind() != SyntaxKind::ArrowFunction {
            let node_parameters = node_as_function_like_declaration.parameters();
            let parameter = first_or_undefined(&node_parameters.ref_(arena)).copied();
            if !matches!(
                parameter,
                Some(parameter) if parameter_is_this_keyword(parameter, arena)
            ) {
                return true;
            }
        }
    }
    false
}

pub fn is_infinity_or_nan_string(name: &str) -> bool {
    matches!(name, "Infinity" | "-Infinity" | "NaN")
}

pub fn is_catch_clause_variable_declaration(node: Id<Node>, arena: &impl HasArena) -> bool {
    node.ref_(arena).kind() == SyntaxKind::VariableDeclaration
        && node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::CatchClause
}

pub fn is_parameter_or_catch_clause_variable(symbol: Id<Symbol>, arena: &impl HasArena) -> bool {
    let declaration = symbol
        .ref_(arena)
        .maybe_value_declaration()
        .map(|symbol_value_declaration| get_root_declaration(symbol_value_declaration, arena));
    matches!(
        declaration,
        Some(declaration) if is_parameter(&declaration.ref_(arena)) ||
            is_catch_clause_variable_declaration(declaration, arena)
    )
}

pub fn is_function_expression_or_arrow_function(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
    )
}
