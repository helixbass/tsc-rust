use std::{
    borrow::{Borrow, Cow},
    cell::RefCell,
    convert::TryInto,
    io, ptr,
};

use base64::{engine::general_purpose, Engine as _};
use gc::Gc;
use id_arena::Id;
use regex::{Captures, Regex};

use super::supported_ts_extensions_for_extract_extension;
use crate::{
    TransformFlags, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface,
    __String, entity_name_to_string, file_extension_is, filter, find, get_combined_modifier_flags,
    get_element_or_property_access_name, get_lines_between_positions, get_parse_tree_node,
    get_property_name_for_property_name_node, get_sys, has_syntactic_modifier,
    is_assignment_operator, is_bindable_static_access_expression, is_class_like,
    is_element_access_expression, is_entity_name_expression, is_identifier, is_jsdoc_member_name,
    is_namespace_export_declaration, is_property_access_expression, is_property_name,
    is_qualified_name, parse_config_file_text_to_json, position_is_synthesized, skip_trivia,
    token_to_string, unescape_leading_underscores, walk_up_parenthesized_expressions, AllArenas,
    BaseDiagnostic, BaseDiagnosticRelatedInformation, BaseNode, BaseSymbol, BaseTextRange,
    BaseType, BundleFileSection, CheckFlags, CompilerOptions, Debug_, Diagnostic,
    DiagnosticInterface, DiagnosticMessage, DiagnosticRelatedInformation,
    DiagnosticRelatedInformationInterface, DiagnosticWithDetachedLocation, DiagnosticWithLocation,
    Extension, HasArena, HasInitializerInterface, InArena, MapLike, ModifierFlags,
    NamedDeclarationInterface, NewLineKind, Node, NodeFlags, NodeInterface, ObjectFlags,
    ReadonlyTextRange, Signature, SignatureFlags, SignatureKind, SourceFileLike, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, BoolExt,
};

pub fn get_first_identifier(mut node: Id<Node>, arena: &impl HasArena) -> Id<Node /*Identifier*/> {
    match node.ref_(arena).kind() {
        SyntaxKind::Identifier => node,
        SyntaxKind::QualifiedName => {
            while {
                node = node.ref_(arena).as_qualified_name().left;
                node.ref_(arena).kind() != SyntaxKind::Identifier
            } {}
            node
        }
        SyntaxKind::PropertyAccessExpression => {
            while {
                node = node.ref_(arena).as_property_access_expression().expression;
                node.ref_(arena).kind() != SyntaxKind::Identifier
            } {}
            node
        }
        _ => panic!("Unexpected syntax kind"),
    }
}

pub fn is_dotted_name(node: Id<Node> /*Expression*/, arena: &impl HasArena) -> bool {
    matches!(
        node.ref_(arena).kind(),
        SyntaxKind::Identifier
            | SyntaxKind::ThisKeyword
            | SyntaxKind::SuperKeyword
            | SyntaxKind::MetaProperty
    ) || node.ref_(arena).kind() == SyntaxKind::PropertyAccessExpression
        && is_dotted_name(node.ref_(arena).as_property_access_expression().expression, arena)
        || node.ref_(arena).kind() == SyntaxKind::ParenthesizedExpression
            && is_dotted_name(node.ref_(arena).as_parenthesized_expression().expression, arena)
}

pub fn is_property_access_entity_name_expression(node: Id<Node>, arena: &impl HasArena) -> bool {
    if !is_property_access_expression(&node.ref_(arena)) {
        return false;
    }
    let node_ref = node.ref_(arena);
    let node_as_property_access_expression = node_ref.as_property_access_expression();
    is_identifier(&node_as_property_access_expression.name.ref_(arena))
        && is_entity_name_expression(node_as_property_access_expression.expression, arena)
}

pub fn try_get_property_access_or_identifier_to_string(
    expr: Id<Node>, /*Expression*/
    arena: &impl HasArena,
) -> Option<String> {
    if is_property_access_expression(&expr.ref_(arena)) {
        let expr_ref = expr.ref_(arena);
        let expr_as_property_access_expression = expr_ref.as_property_access_expression();
        let base_str = try_get_property_access_or_identifier_to_string(
            expr_as_property_access_expression.expression,
            arena,
        );
        if let Some(base_str) = base_str {
            return Some(
                format!(
                    "{}.{}",
                    base_str,
                    entity_name_to_string(expr_as_property_access_expression.name, arena)
                )
            );
        }
    } else if is_element_access_expression(&expr.ref_(arena)) {
        let expr_ref = expr.ref_(arena);
        let expr_as_element_access_expression = expr_ref.as_element_access_expression();
        let base_str = try_get_property_access_or_identifier_to_string(
            expr_as_element_access_expression.expression,
            arena,
        );
        if let Some(base_str) = base_str {
            if is_property_name(&expr_as_element_access_expression.argument_expression.ref_(arena)) {
                return Some(
                    format!(
                        "{}.{}",
                        base_str,
                        &*get_property_name_for_property_name_node(
                            expr_as_element_access_expression.argument_expression,
                            arena,
                        )
                        .unwrap()
                    )
                );
            }
        }
    } else if is_identifier(&expr.ref_(arena)) {
        return Some(unescape_leading_underscores(&expr.ref_(arena).as_identifier().escaped_text).to_owned());
    }
    None
}

pub fn is_prototype_access(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_bindable_static_access_expression(node, None, arena)
        && match get_element_or_property_access_name(node, arena) {
            Some(name) => name == "prototype",
            None => false,
        }
}

pub fn is_right_side_of_qualified_name_or_property_access(node: Id<Node>, arena: &impl HasArena) -> bool {
    node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::QualifiedName
        && node.ref_(arena).parent().ref_(arena).as_qualified_name().right == node
        || node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::PropertyAccessExpression
            && node.ref_(arena).parent().ref_(arena).as_property_access_expression().name == node
}

pub fn is_right_side_of_qualified_name_or_property_access_or_jsdoc_member_name(
    node: Id<Node>,
    arena: &impl HasArena,
) -> bool {
    is_qualified_name(&node.ref_(arena).parent().ref_(arena)) && node.ref_(arena).parent().ref_(arena).as_qualified_name().right == node
        || is_property_access_expression(&node.ref_(arena).parent().ref_(arena))
            && node.ref_(arena).parent().ref_(arena).as_property_access_expression().name == node
        || is_jsdoc_member_name(&node.ref_(arena).parent().ref_(arena))
            && node.ref_(arena).parent().ref_(arena).as_jsdoc_member_name().right == node
}

pub fn is_empty_object_literal(expression: Id<Node>, arena: &impl HasArena) -> bool {
    expression.ref_(arena).kind() == SyntaxKind::ObjectLiteralExpression
        && expression
            .ref_(arena).as_object_literal_expression()
            .properties
            .ref_(arena).is_empty()
}

pub fn is_empty_array_literal(expression: Id<Node>, arena: &impl HasArena) -> bool {
    expression.ref_(arena).kind() == SyntaxKind::ArrayLiteralExpression
        && expression.ref_(arena).as_array_literal_expression().elements.ref_(arena).is_empty()
}

pub fn get_local_symbol_for_export_default(
    symbol: Id<Symbol>,
    arena: &impl HasArena,
) -> Option<Id<Symbol>> {
    if !is_export_default_symbol(symbol, arena) || symbol.ref_(arena).maybe_declarations().is_none() {
        return None;
    }
    for decl in symbol.ref_(arena).maybe_declarations().as_ref().unwrap() {
        if let Some(decl_local_symbol) = decl.ref_(arena).maybe_local_symbol() {
            return Some(decl_local_symbol);
        }
    }
    None
}

fn is_export_default_symbol(symbol: Id<Symbol>, arena: &impl HasArena) -> bool {
    /*symbol &&*/
    match symbol
        .ref_(arena).maybe_declarations()
        .as_ref()
        .filter(|declarations| !declarations.is_empty())
    {
        None => false,
        Some(symbol_declarations) => {
            has_syntactic_modifier(symbol_declarations[0], ModifierFlags::Default, arena)
        }
    }
}

pub fn try_extract_ts_extension(file_name: &str) -> Option<Extension> {
    find(
        &supported_ts_extensions_for_extract_extension,
        |extension, _| file_extension_is(file_name, extension.to_str()),
    )
    .copied()
}

pub fn convert_to_base64(input: &str) -> String {
    general_purpose::STANDARD_NO_PAD.encode(input)
}

pub fn base64_encode(
    host_base64_encode: Option<impl FnMut(&str) -> Option<String>>,
    input: &str,
) -> String {
    host_base64_encode
        .and_then(|mut host_base64_encode| host_base64_encode(input))
        .unwrap_or_else(|| convert_to_base64(input))
}

pub fn read_json(
    path: &str,
    mut host_read_file: impl FnMut(&str) -> io::Result<Option<String>>,
    arena: &impl HasArena,
) -> serde_json::Value {
    let json_text = host_read_file(path);
    if json_text.is_err() {
        return serde_json::Value::Object(serde_json::Map::new());
    }
    let json_text = json_text.unwrap();
    if json_text.is_none() {
        return serde_json::Value::Object(serde_json::Map::new());
    }
    let json_text = json_text.unwrap();
    let result = match parse_config_file_text_to_json(path, json_text, arena) {
        Err(_) => {
            return serde_json::Value::Object(serde_json::Map::new());
        }
        Ok(value) => value,
    };
    if result.error.is_some() {
        return serde_json::Value::Object(serde_json::Map::new());
    }
    result.config.unwrap()
}

pub fn directory_probably_exists(
    directory_name: &str,
    mut host_directory_exists: impl FnMut(&str) -> Option<bool>,
    mut host_is_directory_exists_supported: impl FnMut() -> bool,
) -> bool {
    !host_is_directory_exists_supported() || host_directory_exists(directory_name).unwrap()
}

const carriage_return_line_feed: &str = "\r\n";
const line_feed: &str = "\n";
pub fn get_new_line_character(
    new_line: Option<NewLineKind>,
    get_new_line: Option<impl Fn() -> String>,
    arena: &impl HasArena,
) -> String {
    match new_line {
        Some(NewLineKind::CarriageReturnLineFeed) => {
            return carriage_return_line_feed.to_owned();
        }
        Some(NewLineKind::LineFeed) => {
            return line_feed.to_owned();
        }
        _ => (),
    }
    if let Some(get_new_line) = get_new_line {
        get_new_line()
    } else {
        get_sys(arena).ref_(arena).new_line().to_owned()
    }
}

pub fn create_range(pos: isize, end: Option<isize>) -> BaseTextRange {
    let end = end.unwrap_or(pos);
    Debug_.assert(end >= pos || end == -1, None);
    BaseTextRange::new(pos, end)
}

pub fn move_range_end(range: &impl ReadonlyTextRange, end: isize) -> BaseTextRange {
    create_range(range.pos(), Some(end))
}

pub fn move_range_pos(range: &impl ReadonlyTextRange, pos: isize) -> BaseTextRange {
    create_range(pos, Some(range.end()))
}

pub fn move_range_past_decorators(node: Id<Node>, arena: &impl HasArena) -> BaseTextRange {
    if let Some(node_decorators) = node
        .ref_(arena).maybe_decorators()
        .filter(|node_decorators| !node_decorators.ref_(arena).is_empty())
    {
        move_range_pos(&*node.ref_(arena), node_decorators.ref_(arena).end())
    } else {
        (&*node.ref_(arena)).into()
    }
}

pub fn move_range_past_modifiers(node: Id<Node>, arena: &impl HasArena) -> BaseTextRange {
    if let Some(node_modifiers) = node
        .ref_(arena).maybe_modifiers()
        .filter(|node_modifiers| !node_modifiers.ref_(arena).is_empty())
    {
        move_range_pos(&*node.ref_(arena), node_modifiers.ref_(arena).end())
    } else {
        move_range_past_decorators(node, arena)
    }
}

pub fn create_token_range(pos: isize, token: SyntaxKind) -> BaseTextRange {
    create_range(
        pos,
        Some(pos + isize::try_from(token_to_string(token).unwrap().len()).unwrap()),
    )
}

pub fn range_is_on_single_line(
    range: &impl ReadonlyTextRange,
    source_file: &Node, /*SourceFile*/
) -> bool {
    range_start_is_on_same_line_as_range_end(range, range, source_file)
}

pub fn range_start_positions_are_on_same_line(
    range1: &impl ReadonlyTextRange,
    range2: &impl ReadonlyTextRange,
    source_file: &Node, /*SourceFile*/
) -> bool {
    positions_are_on_same_line(
        get_start_position_of_range(range1, source_file, false),
        get_start_position_of_range(range2, source_file, false),
        source_file,
    )
}

pub fn range_end_positions_are_on_same_line(
    range1: &impl ReadonlyTextRange,
    range2: &impl ReadonlyTextRange,
    source_file: &Node, /*SourceFile*/
) -> bool {
    positions_are_on_same_line(range1.end(), range2.end(), source_file)
}

pub fn range_start_is_on_same_line_as_range_end(
    range1: &impl ReadonlyTextRange,
    range2: &impl ReadonlyTextRange,
    source_file: &Node, /*SourceFile*/
) -> bool {
    positions_are_on_same_line(
        get_start_position_of_range(range1, source_file, false),
        range2.end(),
        source_file,
    )
}

pub fn range_end_is_on_same_line_as_range_start(
    range1: &impl ReadonlyTextRange,
    range2: &impl ReadonlyTextRange,
    source_file: &Node, /*SourceFile*/
) -> bool {
    positions_are_on_same_line(
        range1.end(),
        get_start_position_of_range(range2, source_file, false),
        source_file,
    )
}

pub fn get_lines_between_range_end_and_range_start(
    _range1: &impl ReadonlyTextRange,
    _range2: &impl ReadonlyTextRange,
    _source_file: Id<Node>, /*SourceFile*/
    _include_second_range_comments: bool,
) -> usize {
    unimplemented!()
}

pub fn positions_are_on_same_line(
    pos1: isize,
    pos2: isize,
    source_file: &Node, /*SourceFile*/
) -> bool {
    get_lines_between_positions(source_file.as_source_file(), pos1, pos2) == 0
}

pub fn get_start_position_of_range(
    range: &impl ReadonlyTextRange,
    source_file: &Node, /*SourceFile*/
    include_comments: bool,
) -> isize {
    if position_is_synthesized(range.pos()) {
        -1
    } else {
        skip_trivia(
            &source_file.as_source_file().text_as_chars(),
            range.pos(),
            Some(false),
            Some(include_comments),
            None,
        )
    }
}

pub fn get_lines_between_position_and_preceding_non_whitespace_character(
    _pos: isize,
    _stop_pos: isize,
    _source_file: Id<Node>, /*SourceFile*/
    _include_comments: Option<bool>,
) -> usize {
    unimplemented!()
}

pub fn get_lines_between_position_and_next_non_whitespace_character(
    _pos: isize,
    _stop_pos: isize,
    _source_file: Id<Node>, /*SourceFile*/
    _include_comments: Option<bool>,
) -> usize {
    unimplemented!()
}

pub fn is_declaration_name_of_enum_or_namespace(node: Id<Node> /*Identifier*/, arena: &impl HasArena) -> bool {
    let parse_node = get_parse_tree_node(Some(node), Option::<fn(Id<Node>) -> bool>::None, arena);
    if let Some(parse_node) = parse_node {
        match parse_node.ref_(arena).parent().ref_(arena).kind() {
            SyntaxKind::EnumDeclaration | SyntaxKind::ModuleDeclaration => {
                return parse_node == parse_node.ref_(arena).parent().ref_(arena).as_named_declaration().name();
            }
            _ => (),
        }
    }
    false
}

pub fn get_initialized_variables(node: Id<Node> /*VariableDeclarationList*/, arena: &impl HasArena) -> Vec<Id<Node>> {
    filter(
        &node.ref_(arena).as_variable_declaration_list().declarations.ref_(arena),
        |declaration: &Id<Node>| is_initialized_variable(&declaration.ref_(arena)),
    )
}

fn is_initialized_variable(node: &Node /*VariableDeclaration*/) -> bool {
    node.as_variable_declaration().maybe_initializer().is_some()
}

pub fn is_watch_set(options: &CompilerOptions) -> bool {
    matches!(options.watch, Some(true))
}

pub fn get_check_flags(symbol: &Symbol) -> CheckFlags {
    match symbol {
        Symbol::TransientSymbol(transient_symbol) => transient_symbol.check_flags(),
        _ => CheckFlags::None,
    }
}

pub fn get_declaration_modifier_flags_from_symbol(
    s: Id<Symbol>,
    is_write: Option<bool>,
    arena: &impl HasArena,
) -> ModifierFlags {
    let is_write = is_write.unwrap_or(false);
    if let Some(s_value_declaration) = s.ref_(arena).maybe_value_declaration() {
        let declaration = is_write.then_and(|| {
            s.ref_(arena).maybe_declarations().as_ref().and_then(|s_declarations| {
                find(s_declarations, |d: &Id<Node>, _| {
                    d.ref_(arena).kind() == SyntaxKind::SetAccessor
                })
                .copied()
            })
        }).unwrap_or(s_value_declaration);
        let flags = get_combined_modifier_flags(declaration, arena);
        return if matches!(
            s.ref_(arena).maybe_parent(),
            Some(s_parent) if s_parent.ref_(arena).flags().intersects(SymbolFlags::Class)
        ) {
            flags
        } else {
            flags & !ModifierFlags::AccessibilityModifier
        };
    }
    if get_check_flags(&s.ref_(arena)).intersects(CheckFlags::Synthetic) {
        let check_flags = s.ref_(arena).as_transient_symbol().check_flags();
        let access_modifier = if check_flags.intersects(CheckFlags::ContainsPrivate) {
            ModifierFlags::Private
        } else if check_flags.intersects(CheckFlags::ContainsPublic) {
            ModifierFlags::Public
        } else {
            ModifierFlags::Protected
        };
        let static_modifier = if check_flags.intersects(CheckFlags::ContainsStatic) {
            ModifierFlags::Static
        } else {
            ModifierFlags::None
        };
        return access_modifier | static_modifier;
    }
    if s.ref_(arena).flags().intersects(SymbolFlags::Prototype) {
        return ModifierFlags::Public | ModifierFlags::Static;
    }
    ModifierFlags::None
}

pub fn get_combined_local_and_export_symbol_flags(_symbol: Id<Symbol>) -> SymbolFlags {
    unimplemented!()
}

pub fn is_write_only_access(node: Id<Node>, arena: &impl HasArena) -> bool {
    access_kind(node, arena) == AccessKind::Write
}

pub fn is_write_access(node: Id<Node>, arena: &impl HasArena) -> bool {
    access_kind(node, arena) != AccessKind::Read
}

#[derive(PartialEq, Eq)]
enum AccessKind {
    Read,
    Write,
    ReadWrite,
}

fn access_kind(node: Id<Node>, arena: &impl HasArena) -> AccessKind {
    let Some(parent) = node.ref_(arena).maybe_parent() else {
        return AccessKind::Read;
    };

    match parent.ref_(arena).kind() {
        SyntaxKind::ParenthesizedExpression => access_kind(parent, arena),
        SyntaxKind::PostfixUnaryExpression | SyntaxKind::PrefixUnaryExpression => {
            let parent_ref = parent.ref_(arena);
            let parent_as_unary_expression = parent_ref.as_unary_expression();
            let operator = parent_as_unary_expression.operator();
            if matches!(
                operator,
                SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
            ) {
                write_or_read_write(parent, arena)
            } else {
                AccessKind::Read
            }
        }
        SyntaxKind::BinaryExpression => {
            let parent_ref = parent.ref_(arena);
            let parent_as_binary_expression = parent_ref.as_binary_expression();
            let left = parent_as_binary_expression.left;
            let operator_token = parent_as_binary_expression.operator_token;
            if left == node && is_assignment_operator(operator_token.ref_(arena).kind()) {
                if operator_token.ref_(arena).kind() == SyntaxKind::EqualsToken {
                    AccessKind::Write
                } else {
                    write_or_read_write(parent, arena)
                }
            } else {
                AccessKind::Read
            }
        }
        SyntaxKind::PropertyAccessExpression => {
            if parent.ref_(arena).as_property_access_expression().name != node {
                AccessKind::Read
            } else {
                access_kind(parent, arena)
            }
        }
        SyntaxKind::PropertyAssignment => {
            let parent_access = access_kind(parent.ref_(arena).parent(), arena);
            if node == parent.ref_(arena).as_property_assignment().name() {
                reverse_access_kind(parent_access)
            } else {
                parent_access
            }
        }
        SyntaxKind::ShorthandPropertyAssignment => {
            if matches!(
                parent.ref_(arena).as_shorthand_property_assignment().object_assignment_initializer,
                Some(parent_object_assignment_initializer) if node == parent_object_assignment_initializer
            ) {
                AccessKind::Read
            } else {
                access_kind(parent.ref_(arena).parent(), arena)
            }
        }
        SyntaxKind::ArrayLiteralExpression => access_kind(parent, arena),
        _ => AccessKind::Read,
    }
}

fn write_or_read_write(parent: Id<Node>, arena: &impl HasArena) -> AccessKind {
    if let Some(grandparent) = parent.ref_(arena).maybe_parent() {
        if walk_up_parenthesized_expressions(grandparent, arena)
            .unwrap()
            .ref_(arena).kind()
            == SyntaxKind::ExpressionStatement
        {
            return AccessKind::Write;
        }
    }
    AccessKind::ReadWrite
}

fn reverse_access_kind(a: AccessKind) -> AccessKind {
    match a {
        AccessKind::Read => AccessKind::Write,
        AccessKind::Write => AccessKind::Read,
        AccessKind::ReadWrite => AccessKind::ReadWrite,
    }
}

pub fn get_class_like_declaration_of_symbol(
    symbol: Id<Symbol>,
    arena: &impl HasArena,
) -> Option<Id<Node /*ClassLikeDeclaration*/>> {
    symbol
        .ref_(arena).maybe_declarations()
        .as_ref()
        .and_then(|symbol_declarations| {
            symbol_declarations
                .into_iter()
                .find(|declaration| is_class_like(&declaration.ref_(arena)))
                .copied()
        })
}

pub fn get_object_flags(type_: &Type) -> ObjectFlags {
    if type_.flags().intersects(TypeFlags::ObjectFlagsType) {
        type_.as_object_flags_type().object_flags()
    } else {
        ObjectFlags::None
    }
}

pub fn type_has_call_or_construct_signatures(
    type_: Id<Type>,
    checker: &TypeChecker,
) -> io::Result<bool> {
    Ok(!checker
        .get_signatures_of_type(type_, SignatureKind::Call)?
        .is_empty()
        || !checker
            .get_signatures_of_type(type_, SignatureKind::Construct)?
            .is_empty())
}

pub fn is_umd_export_symbol(symbol: Option<Id<Symbol>>, arena: &impl HasArena) -> bool {
    matches!(
        symbol,
        Some(symbol) if matches!(
            symbol.ref_(arena).maybe_declarations().as_ref(),
            Some(symbol_declarations) if matches!(
                symbol_declarations.get(0),
                Some(symbol_declarations_0) if is_namespace_export_declaration(&symbol_declarations_0.ref_(arena))
            )
        )
    )
}

pub fn is_type_node_kind(kind: SyntaxKind) -> bool {
    kind >= SyntaxKind::FirstTypeNode && kind <= SyntaxKind::LastTypeNode
        || matches!(
            kind,
            SyntaxKind::AnyKeyword
                | SyntaxKind::UnknownKeyword
                | SyntaxKind::NumberKeyword
                | SyntaxKind::BigIntKeyword
                | SyntaxKind::ObjectKeyword
                | SyntaxKind::BooleanKeyword
                | SyntaxKind::StringKeyword
                | SyntaxKind::SymbolKeyword
                | SyntaxKind::VoidKeyword
                | SyntaxKind::UndefinedKeyword
                | SyntaxKind::NeverKeyword
                | SyntaxKind::ExpressionWithTypeArguments
                | SyntaxKind::JSDocAllType
                | SyntaxKind::JSDocUnknownType
                | SyntaxKind::JSDocNullableType
                | SyntaxKind::JSDocNonNullableType
                | SyntaxKind::JSDocOptionalType
                | SyntaxKind::JSDocFunctionType
                | SyntaxKind::JSDocVariadicType
        )
}

pub fn is_access_expression(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression
    )
}

pub fn is_bundle_file_text_like(_section: &BundleFileSection) -> bool {
    unimplemented!()
}

pub fn get_leftmost_access_expression(
    mut expr: Id<Node> /*Expression*/,
    arena: &impl HasArena,
) -> Id<Node /*Expression*/> {
    while is_access_expression(&expr.ref_(arena)) {
        expr = expr.ref_(arena).as_has_expression().expression();
    }
    expr
}

pub fn get_leftmost_expression(
    mut node: Id<Node>, /*Expression*/
    stop_at_call_expressions: bool,
    arena: &impl HasArena,
) -> Id<Node /*Expression*/> {
    loop {
        match node.ref_(arena).kind() {
            SyntaxKind::PostfixUnaryExpression => {
                node = node.ref_(arena).as_postfix_unary_expression().operand;
                continue;
            }

            SyntaxKind::BinaryExpression => {
                node = node.ref_(arena).as_binary_expression().left;
                continue;
            }

            SyntaxKind::ConditionalExpression => {
                node = node.ref_(arena).as_conditional_expression().condition;
                continue;
            }

            SyntaxKind::TaggedTemplateExpression => {
                node = node.ref_(arena).as_tagged_template_expression().tag;
                continue;
            }

            SyntaxKind::CallExpression => {
                if stop_at_call_expressions {
                    return node;
                }
                node = node.ref_(arena).as_has_expression().expression();
                continue;
            }
            SyntaxKind::AsExpression
            | SyntaxKind::ElementAccessExpression
            | SyntaxKind::PropertyAccessExpression
            | SyntaxKind::NonNullExpression
            | SyntaxKind::PartiallyEmittedExpression => {
                node = node.ref_(arena).as_has_expression().expression();
                continue;
            }
            _ => (),
        }

        return node;
    }
}

#[allow(non_snake_case)]
fn Symbol(flags: SymbolFlags, name: __String) -> BaseSymbol {
    BaseSymbol::new(flags, name)
}

#[allow(non_snake_case)]
fn _Type(flags: TypeFlags) -> BaseType {
    BaseType::new(flags)
}

#[allow(non_snake_case)]
fn _Signature(flags: SignatureFlags) -> Signature {
    Signature::new(flags)
}

#[allow(non_snake_case)]
fn Node(kind: SyntaxKind, pos: isize, end: isize, arena: &AllArenas) -> BaseNode {
    BaseNode::new(kind, NodeFlags::None, TransformFlags::None, pos, end, arena)
}

#[allow(non_snake_case)]
fn Token(kind: SyntaxKind, pos: isize, end: isize, arena: &AllArenas) -> BaseNode {
    BaseNode::new(kind, NodeFlags::None, TransformFlags::None, pos, end, arena)
}

#[allow(non_snake_case)]
fn Identifier(kind: SyntaxKind, pos: isize, end: isize, arena: &AllArenas) -> BaseNode {
    BaseNode::new(kind, NodeFlags::None, TransformFlags::None, pos, end, arena)
}

pub struct ObjectAllocator {}

impl ObjectAllocator {
    pub fn get_node_constructor(&self) -> fn(SyntaxKind, isize, isize, &AllArenas) -> BaseNode {
        Node
    }

    pub fn get_token_constructor(&self) -> fn(SyntaxKind, isize, isize, &AllArenas) -> BaseNode {
        Token
    }

    pub fn get_identifier_constructor(&self) -> fn(SyntaxKind, isize, isize, &AllArenas) -> BaseNode {
        Identifier
    }

    pub fn get_private_identifier_constructor(&self) -> fn(SyntaxKind, isize, isize, &AllArenas) -> BaseNode {
        Node
    }

    pub fn get_source_file_constructor(&self) -> fn(SyntaxKind, isize, isize, &AllArenas) -> BaseNode {
        Node
    }

    pub fn get_symbol_constructor(&self) -> fn(SymbolFlags, __String) -> BaseSymbol {
        Symbol
    }

    pub fn get_type_constructor(&self) -> fn(TypeFlags) -> BaseType {
        _Type
    }

    pub fn get_signature_constructor(&self) -> fn(SignatureFlags) -> Signature {
        _Signature
    }
}

lazy_static! {
    pub static ref object_allocator: ObjectAllocator = ObjectAllocator {};
}

pub fn format_string_from_args(text: &str, args: Vec<String>) -> String {
    let re = Regex::new(r"\{(\d+)\}").unwrap();
    re.replace_all(text, |captures: &Captures| {
        let index = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
        Debug_.check_defined(args.get(index), None)
    })
    .to_string()
}

thread_local! {
    pub static localized_diagnostic_messages: RefCell<Option<MapLike<String>>> = RefCell::new(None);
}

pub(crate) fn set_localized_diagnostic_messages(messages: Option<MapLike<String>>) {
    localized_diagnostic_messages.with(|localized_diagnostic_messages_| {
        *localized_diagnostic_messages_.borrow_mut() = messages;
    })
}

pub fn get_locale_specific_message(message: &DiagnosticMessage) -> String {
    message.message.to_string()
}

pub fn create_detached_diagnostic(
    file_name: &str,
    start: isize,
    length: isize,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithDetachedLocation {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        if !args.is_empty() {
            text = format_string_from_args(&text, args);
        }
    }

    DiagnosticWithDetachedLocation::new(
        BaseDiagnostic::new(
            BaseDiagnosticRelatedInformation::new(
                message.category,
                message.code,
                None,
                Some(start),
                Some(length),
                text,
            ),
            None,
        ),
        file_name.to_string(),
    )
}

fn is_diagnostic_with_detached_location(
    diagnostic: &DiagnosticRelatedInformation, /*DiagnosticRelatedInformation | DiagnosticWithDetachedLocation*/
) -> bool {
    matches!(
        diagnostic,
        DiagnosticRelatedInformation::Diagnostic(Diagnostic::DiagnosticWithDetachedLocation(_))
    )
}

pub fn attach_file_to_diagnostic(
    diagnostic: &DiagnosticWithDetachedLocation,
    file: &Node, /*SourceFile*/
    arena: &impl HasArena,
) -> DiagnosticWithLocation {
    let file_as_source_file = file.as_source_file();
    let file_name = file_as_source_file.file_name();
    let length: isize = file_as_source_file.text().len().try_into().unwrap();
    Debug_.assert_equal(&diagnostic.file_name, &*file_name, None, None);
    Debug_.assert_less_than_or_equal(diagnostic.start(), length);
    Debug_.assert_less_than_or_equal(diagnostic.start() + diagnostic.length(), length);
    let diagnostic_with_location = DiagnosticWithLocation::new(BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            diagnostic.category(),
            diagnostic.code(),
            Some(file.arena_id()),
            Some(diagnostic.start()),
            Some(diagnostic.length()),
            diagnostic.message_text().clone(),
        ),
        None,
    ));
    if let Some(related_information) = diagnostic.maybe_related_information().as_ref() {
        *diagnostic_with_location.maybe_related_information_mut() = Some(
            related_information
                .iter()
                .map(|&related| {
                    if is_diagnostic_with_detached_location(&related.ref_(arena))
                        && &related.ref_(arena).as_diagnostic_with_detached_location().file_name == &*file_name
                    {
                        Debug_.assert_less_than_or_equal(related.ref_(arena).start(), length);
                        Debug_
                            .assert_less_than_or_equal(related.ref_(arena).start() + related.ref_(arena).length(), length);
                        arena.alloc_diagnostic_related_information(
                            attach_file_to_diagnostic(
                                related.ref_(arena).as_diagnostic_with_detached_location(),
                                file,
                                arena,
                            )
                            .into(),
                        )
                    } else {
                        related
                    }
                })
                .collect(),
        );
    }
    diagnostic_with_location
}

pub fn attach_file_to_diagnostics(
    diagnostics: &[Id<Diagnostic /*DiagnosticWithDetachedLocation*/>],
    file: &Node, /*SourceFile*/
    arena: &impl HasArena,
) -> Vec<Id<Diagnostic /*DiagnosticWithLocation*/>> {
    diagnostics
        .iter()
        .map(|diagnostic| {
            arena.alloc_diagnostic(
                attach_file_to_diagnostic(diagnostic.ref_(arena).as_diagnostic_with_detached_location(), file, arena)
                    .into(),
            )
        })
        .collect()
}

pub fn create_file_diagnostic(
    file: Id<Node>, /*SourceFile*/
    start: isize,
    length: isize,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithLocation {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        if !args.is_empty() {
            text = format_string_from_args(&text, args);
        }
    }

    DiagnosticWithLocation::new(BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            message.category,
            message.code,
            Some(file),
            Some(start),
            Some(length),
            text,
        ),
        None,
    ))
}

pub fn format_message(
    _dummy: Option<()>,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> String {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        if !args.is_empty() {
            text = format_string_from_args(&text, args);
        }
    }

    text
}

pub fn create_compiler_diagnostic(
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> BaseDiagnostic {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        if !args.is_empty() {
            text = format_string_from_args(&text, args);
        }
    }

    BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            message.category,
            message.code,
            None,
            None,
            None,
            text,
        ),
        None,
    )
}
