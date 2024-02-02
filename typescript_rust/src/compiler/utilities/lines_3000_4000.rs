use std::{
    borrow::{Borrow, Cow},
    cell::Cell,
    cmp::Ordering,
    collections::HashMap,
    convert::TryFrom,
    iter::FromIterator,
    ops::Deref,
    ptr,
};

use bitflags::bitflags;
use id_arena::Id;
use regex::{Captures, Regex};

use super::is_quote_or_backtick;
use crate::{
    InArena,    binary_search, compare_diagnostics, compare_diagnostics_skip_related_information,
    compare_strings_case_sensitive, concatenate, filter, flat_map_to_mutable,
    get_assignment_declaration_kind, get_jsdoc_augments_tag, get_jsdoc_implements_tags,
    get_parse_tree_node, get_symbol_id, has_syntactic_modifier, id_text, is_binary_expression,
    is_class_expression, is_class_like, is_declaration, is_element_access_expression,
    is_entity_name_expression, is_export_assignment, is_in_js_file, is_interface_declaration,
    is_no_substitution_template_literal, is_numeric_literal, is_prefix_unary_expression,
    is_property_access_expression, is_source_file, is_string_literal_like,
    maybe_text_char_at_index, position_is_synthesized, single_element_array, skip_parentheses,
    some, starts_with, string_to_token, token_to_string, AssignmentDeclarationKind, CharacterCodes,
    Debug_, InterfaceOrClassLikeDeclarationInterface, ModifierFlags, NamedDeclarationInterface,
    Node, NodeArray, NodeInterface, ReadonlyTextRange, SortedArray, Symbol, SymbolInterface,
    SyntaxKind, TokenFlags, __String, escape_leading_underscores, get_name_of_declaration,
    insert_sorted, is_member_name, Diagnostic, DiagnosticCollection,
    DiagnosticRelatedInformationInterface, HasArena, AllArenas,
};

pub fn is_literal_computed_property_declaration_name(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_string_or_numeric_literal_like(&node.ref_(arena))
        && node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::ComputedPropertyName
        && is_declaration(node.ref_(arena).parent().ref_(arena).parent(), arena)
}

pub fn is_identifier_name(node: Id<Node> /*Identifier*/, arena: &impl HasArena) -> bool {
    let parent = node.ref_(arena).parent();
    match parent.ref_(arena).kind() {
        SyntaxKind::PropertyDeclaration
        | SyntaxKind::PropertySignature
        | SyntaxKind::MethodDeclaration
        | SyntaxKind::MethodSignature
        | SyntaxKind::GetAccessor
        | SyntaxKind::SetAccessor
        | SyntaxKind::EnumMember
        | SyntaxKind::PropertyAssignment
        | SyntaxKind::PropertyAccessExpression => parent.ref_(arena).as_named_declaration().maybe_name() == Some(node),
        SyntaxKind::QualifiedName => parent.ref_(arena).as_qualified_name().right == node,
        SyntaxKind::BindingElement => parent.ref_(arena).as_binding_element().property_name == Some(node),
        SyntaxKind::ImportSpecifier => parent.ref_(arena).as_import_specifier().property_name == Some(node),
        SyntaxKind::ExportSpecifier | SyntaxKind::JsxAttribute => true,
        _ => false,
    }
}

pub fn is_alias_symbol_declaration(node: Id<Node>, arena: &impl HasArena) -> bool {
    matches!(
        node.ref_(arena).kind(),
        SyntaxKind::ImportEqualsDeclaration | SyntaxKind::NamespaceExportDeclaration
    ) || node.ref_(arena).kind() == SyntaxKind::ImportClause && node.ref_(arena).as_import_clause().name.is_some()
        || matches!(
            node.ref_(arena).kind(),
            SyntaxKind::NamespaceImport
                | SyntaxKind::NamespaceExport
                | SyntaxKind::ImportSpecifier
                | SyntaxKind::ExportSpecifier
        )
        || node.ref_(arena).kind() == SyntaxKind::ExportAssignment && export_assignment_is_alias(node, arena)
        || is_binary_expression(&node.ref_(arena))
            && get_assignment_declaration_kind(node, arena) == AssignmentDeclarationKind::ModuleExports
            && export_assignment_is_alias(node, arena)
        || is_property_access_expression(&node.ref_(arena)) && is_binary_expression(&node.ref_(arena).parent().ref_(arena)) && {
            let node_parent = node.ref_(arena).parent();
            let node_parent_ref = node_parent.ref_(arena);
            let node_parent_as_binary_expression = node_parent_ref.as_binary_expression();
            node_parent_as_binary_expression.left == node
                && node_parent_as_binary_expression.operator_token.ref_(arena).kind() == SyntaxKind::EqualsToken
                && is_aliasable_expression(node_parent_as_binary_expression.right, arena)
        }
        || node.ref_(arena).kind() == SyntaxKind::ShorthandPropertyAssignment
        || node.ref_(arena).kind() == SyntaxKind::PropertyAssignment
            && is_aliasable_expression(node.ref_(arena).as_property_assignment().initializer, arena)
}

pub fn get_alias_declaration_from_name(
    mut node: Id<Node>, /*EntityName*/
    arena: &impl HasArena,
) -> Option<Id<Node /*Declaration*/>> {
    match node.ref_(arena).parent().ref_(arena).kind() {
        SyntaxKind::ImportClause
        | SyntaxKind::ImportSpecifier
        | SyntaxKind::NamespaceImport
        | SyntaxKind::ExportSpecifier
        | SyntaxKind::ExportAssignment
        | SyntaxKind::ImportEqualsDeclaration => Some(node.ref_(arena).parent()),
        SyntaxKind::QualifiedName => {
            while {
                node = node.ref_(arena).parent();
                node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::QualifiedName
            } {}
            get_alias_declaration_from_name(node, arena)
        }
        _ => None,
    }
}

pub fn is_aliasable_expression(e: Id<Node> /*Expression*/, arena: &impl HasArena) -> bool {
    is_entity_name_expression(e, arena) || is_class_expression(&e.ref_(arena))
}

pub fn export_assignment_is_alias(node: Id<Node> /*ExportAssignment | BinaryExpression*/, arena: &impl HasArena) -> bool {
    let e = get_export_assignment_expression(&node.ref_(arena));
    is_aliasable_expression(e, arena)
}

pub fn get_export_assignment_expression(
    node: &Node, /*ExportAssignment | BinaryExpression*/
) -> Id<Node /*Expression*/> {
    if is_export_assignment(node) {
        node.as_export_assignment().expression
    } else {
        node.as_binary_expression().right
    }
}

pub fn get_property_assignment_alias_like_expression(
    node: Id<Node>, /*PropertyAssignment | ShorthandPropertyAssignment | PropertyAccessExpression*/
    arena: &impl HasArena,
) -> Id<Node /*Expression*/> {
    if node.ref_(arena).kind() == SyntaxKind::ShorthandPropertyAssignment {
        node.ref_(arena).as_shorthand_property_assignment().name()
    } else if node.ref_(arena).kind() == SyntaxKind::PropertyAssignment {
        node.ref_(arena).as_property_assignment().initializer
    } else {
        node.ref_(arena).parent().ref_(arena).as_binary_expression().right
    }
}

pub fn get_effective_base_type_node(
    node: Id<Node>, /*ClassLikeDeclaration | InterfaceDeclaration*/
    arena: &impl HasArena,
) -> Option<Id<Node>> {
    let base_type = get_class_extends_heritage_element(node, arena);
    if base_type.is_some() && is_in_js_file(Some(&node.ref_(arena))) {
        let tag = get_jsdoc_augments_tag(node, arena);
        if let Some(tag) = tag {
            return Some(tag.ref_(arena).as_jsdoc_augments_tag().class);
        }
    }
    base_type
}

pub fn get_class_extends_heritage_element(
    node: Id<Node>, /*ClassLikeDeclaration | InterfaceDeclaration (or maybe also eg FunctionDeclaration)*/
    arena: &impl HasArena,
) -> Option<Id<Node>> {
    let heritage_clause = get_heritage_clause(
        node.ref_(arena).maybe_as_interface_or_class_like_declaration()
            .and_then(|node| node.maybe_heritage_clauses()),
        SyntaxKind::ExtendsKeyword,
        arena,
    )?;
    let heritage_clause_ref = heritage_clause.ref_(arena);
    let heritage_clause_as_heritage_clause = heritage_clause_ref.as_heritage_clause();
    if !heritage_clause_as_heritage_clause.types.ref_(arena).is_empty() {
        Some(heritage_clause_as_heritage_clause.types.ref_(arena)[0])
    } else {
        None
    }
}

pub fn get_effective_implements_type_nodes(
    node: Id<Node>, /*ClassLikeDeclaration*/
    arena: &impl HasArena,
) -> Option<Vec<Id<Node>>> {
    if is_in_js_file(Some(&node.ref_(arena))) {
        Some(
            get_jsdoc_implements_tags(node, arena)
                .iter()
                .map(|n| n.ref_(arena).as_jsdoc_implements_tag().class)
                .collect(),
        )
    } else {
        let heritage_clause = get_heritage_clause(
            node.ref_(arena).as_interface_or_class_like_declaration()
                .maybe_heritage_clauses(),
            SyntaxKind::ImplementsKeyword,
            arena,
        )?;
        Some(heritage_clause.ref_(arena).as_heritage_clause().types.ref_(arena).to_vec())
    }
}

pub fn get_all_super_type_nodes(node: Id<Node>, arena: &impl HasArena) -> Vec<Id<Node>> {
    if is_interface_declaration(&node.ref_(arena)) {
        get_interface_base_type_nodes(node, arena).map_or_else(|| vec![], |node_array| node_array.ref_(arena).to_vec())
    } else if is_class_like(&node.ref_(arena)) {
        concatenate(
            single_element_array(get_effective_base_type_node(node, arena)).unwrap_or_else(|| vec![]),
            get_effective_implements_type_nodes(node, arena).unwrap_or_else(|| vec![]),
        )
    } else {
        vec![]
    }
}

pub fn get_interface_base_type_nodes(
    node: Id<Node>, /*InterfaceDeclaration*/
    arena: &impl HasArena,
) -> Option<Id<NodeArray>> {
    let heritage_clause = get_heritage_clause(
        node.ref_(arena).as_interface_declaration()
            .maybe_heritage_clauses(),
        SyntaxKind::ExtendsKeyword,
        arena,
    )?;
    Some(heritage_clause.ref_(arena).as_heritage_clause().types.clone())
}

pub fn get_heritage_clause(clauses: Option<Id<NodeArray>>, kind: SyntaxKind, arena: &impl HasArena) -> Option<Id<Node>> {
    let clauses = clauses?;
    for &clause in &*clauses.ref_(arena) {
        if clause.ref_(arena).as_heritage_clause().token == kind {
            return Some(clause);
        }
    }
    None
}

pub fn get_ancestor(node: Option<Id<Node>>, kind: SyntaxKind, arena: &impl HasArena) -> Option<Id<Node>> {
    let mut node = Some(node?);
    while let Some(node_present) = node {
        if node_present.ref_(arena).kind() == kind {
            return Some(node_present);
        }
        node = node_present.ref_(arena).maybe_parent();
    }
    None
}

pub fn is_keyword(token: SyntaxKind) -> bool {
    SyntaxKind::FirstKeyword <= token && token <= SyntaxKind::LastKeyword
}

pub fn is_contextual_keyword(token: SyntaxKind) -> bool {
    SyntaxKind::FirstContextualKeyword <= token && token <= SyntaxKind::LastContextualKeyword
}

pub fn is_non_contextual_keyword(token: SyntaxKind) -> bool {
    is_keyword(token) && !is_contextual_keyword(token)
}

pub fn is_future_reserved_word(token: SyntaxKind) -> bool {
    SyntaxKind::FirstFutureReservedWord <= token && token <= SyntaxKind::LastFutureReservedWord
}

pub fn is_string_a_non_contextual_keyword(name: &str) -> bool {
    let token = string_to_token(name);
    token.is_some() && is_non_contextual_keyword(token.unwrap())
}

pub fn is_string_a_keyword(name: &str) -> bool {
    let token = string_to_token(name);
    token.is_some() && is_keyword(token.unwrap())
}

pub fn is_identifier_a_non_contextual_keyword(node: &Node /*Identifier*/) -> bool {
    let original_keyword_kind = node.as_identifier().original_keyword_kind;
    original_keyword_kind.is_some() && !is_contextual_keyword(original_keyword_kind.unwrap())
}

pub fn is_trivia(token: SyntaxKind) -> bool {
    SyntaxKind::FirstTriviaToken <= token && token <= SyntaxKind::LastTriviaToken
}

bitflags! {
    pub struct FunctionFlags: u32 {
        const Normal = 0;
        const Generator = 1 << 0;
        const Async = 1 << 1;
        const Invalid = 1 << 2;
        const AsyncGenerator = Self::Async.bits | Self::Generator.bits;
    }
}

pub fn get_function_flags(
    node: Option<Id<Node> /*SignatureDeclaration*/>,
    arena: &impl HasArena,
) -> FunctionFlags {
    let Some(node) = node else {
        return FunctionFlags::Invalid;
    };

    let mut flags = FunctionFlags::Normal;
    match node.ref_(arena).kind() {
        SyntaxKind::FunctionDeclaration
        | SyntaxKind::FunctionExpression
        | SyntaxKind::MethodDeclaration => {
            if node
                .ref_(arena).as_function_like_declaration()
                .maybe_asterisk_token()
                .is_some()
            {
                flags |= FunctionFlags::Generator;
            }
            if has_syntactic_modifier(node, ModifierFlags::Async, arena) {
                flags |= FunctionFlags::Async;
            }
        }

        SyntaxKind::ArrowFunction => {
            if has_syntactic_modifier(node, ModifierFlags::Async, arena) {
                flags |= FunctionFlags::Async;
            }
        }
        _ => (),
    }

    if node
        .ref_(arena).maybe_as_function_like_declaration()
        .and_then(|node| node.maybe_body())
        .is_none()
    {
        flags |= FunctionFlags::Invalid;
    }

    flags
}

pub fn is_async_function(node: Id<Node>, arena: &impl HasArena) -> bool {
    match node.ref_(arena).kind() {
        SyntaxKind::FunctionDeclaration
        | SyntaxKind::FunctionExpression
        | SyntaxKind::ArrowFunction
        | SyntaxKind::MethodDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_function_like_declaration = node_ref.as_function_like_declaration();
            node_as_function_like_declaration.maybe_body().is_some()
                && node_as_function_like_declaration
                    .maybe_asterisk_token()
                    .is_none()
                && has_syntactic_modifier(node, ModifierFlags::Async, arena)
        }
        _ => false,
    }
}

pub fn is_string_or_numeric_literal_like(node: &Node) -> bool {
    is_string_literal_like(node) || is_numeric_literal(node)
}

pub fn is_signed_numeric_literal(node: Id<Node>, arena: &impl HasArena) -> bool {
    if !is_prefix_unary_expression(&node.ref_(arena)) {
        return false;
    }
    let node_ref = node.ref_(arena);
    let node_as_prefix_unary_expression = node_ref.as_prefix_unary_expression();
    matches!(
        node_as_prefix_unary_expression.operator,
        SyntaxKind::PlusToken | SyntaxKind::MinusToken
    ) && is_numeric_literal(&node_as_prefix_unary_expression.operand.ref_(arena))
}

pub fn has_dynamic_name(declaration: Id<Node> /*Declaration*/, arena: &impl HasArena) -> bool {
    let name = get_name_of_declaration(Some(declaration), arena);
    if let Some(name) = name {
        is_dynamic_name(name, arena)
    } else {
        false
    }
}

pub fn is_dynamic_name(name: Id<Node> /*DeclarationName*/, arena: &impl HasArena) -> bool {
    if !matches!(
        name.ref_(arena).kind(),
        SyntaxKind::ComputedPropertyName | SyntaxKind::ElementAccessExpression
    ) {
        return false;
    }
    let expr = if is_element_access_expression(&name.ref_(arena)) {
        skip_parentheses(
            name.ref_(arena).as_element_access_expression().argument_expression,
            None,
            arena,
        )
    } else {
        name.ref_(arena).as_computed_property_name().expression
    };
    !is_string_or_numeric_literal_like(&expr.ref_(arena)) && !is_signed_numeric_literal(expr, arena)
}

pub fn get_property_name_for_property_name_node(
    name: Id<Node>, /*PropertyName*/
    arena: &impl HasArena,
) -> Option<String /*__String*/> {
    match name.ref_(arena).kind() {
        SyntaxKind::Identifier => Some(name.ref_(arena).as_identifier().escaped_text.clone()),
        SyntaxKind::PrivateIdentifier => Some(name.ref_(arena).as_private_identifier().escaped_text.clone()),
        SyntaxKind::StringLiteral | SyntaxKind::NumericLiteral => Some(
            escape_leading_underscores(&name.ref_(arena).as_literal_like_node().text())
                .into_owned(),
        ),
        SyntaxKind::ComputedPropertyName => {
            let name_expression = name.ref_(arena).as_computed_property_name().expression;
            if is_string_or_numeric_literal_like(&name_expression.ref_(arena)) {
                Some(
                    escape_leading_underscores(&name_expression.ref_(arena).as_literal_like_node().text())
                        .into_owned(),
                )
            } else if is_signed_numeric_literal(name_expression, arena) {
                let name_expression_ref = name_expression.ref_(arena);
                let name_expression_as_prefix_unary_expression =
                    name_expression_ref.as_prefix_unary_expression();
                if name_expression_as_prefix_unary_expression.operator == SyntaxKind::MinusToken {
                    Some(
                        format!(
                            "{}{}",
                            token_to_string(name_expression_as_prefix_unary_expression.operator)
                                .unwrap(),
                            name_expression_as_prefix_unary_expression
                                .operand
                                .ref_(arena)
                                .as_literal_like_node()
                                .text()
                        )
                    )
                } else {
                    Some(
                        name_expression_as_prefix_unary_expression
                            .operand
                            .ref_(arena).as_literal_like_node()
                            .text()
                            .clone()
                    )
                }
            } else {
                None
            }
        }
        _ => Debug_.assert_never(name, None),
    }
}

pub fn is_property_name_literal(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Identifier
            | SyntaxKind::StringLiteral
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::NumericLiteral
    )
}

pub fn get_text_of_identifier_or_literal(node: &Node) -> Cow<'_, str> {
    if is_member_name(node) {
        id_text(node).into()
    } else {
        node.as_literal_like_node().text().to_owned().into()
    }
}

pub fn get_escaped_text_of_identifier_or_literal(node: &Node) -> Cow<'_, str> /*__String*/
{
    if is_member_name(node) {
        node.as_member_name().escaped_text().into()
    } else {
        escape_leading_underscores(&node.as_literal_like_node().text())
            .into_owned()
            .into()
    }
}

pub fn get_property_name_for_unique_es_symbol(symbol: &Symbol) -> __String {
    format!("__@{}@{}", get_symbol_id(symbol), symbol.escaped_name())
}

pub fn get_symbol_name_for_private_identifier(
    containing_class_symbol: &Symbol,
    description: &str, /*__String*/
) -> __String {
    format!(
        "__#{}@{}",
        get_symbol_id(containing_class_symbol),
        description
    )
}

pub fn is_known_symbol(symbol: &Symbol) -> bool {
    starts_with(symbol.escaped_name(), "__@")
}

pub fn is_private_identifier_symbol(symbol: &Symbol) -> bool {
    starts_with(symbol.escaped_name(), "__#")
}

pub fn is_es_symbol_identifier(node: &Node) -> bool {
    node.kind() == SyntaxKind::Identifier && node.as_identifier().escaped_text == "Symbol"
}

pub fn is_push_or_unshift_identifier(node: &Node /*Identifier*/) -> bool {
    matches!(
        node.as_identifier().escaped_text.deref(),
        "push" | "unshift"
    )
}

pub fn is_parameter_declaration(node: Id<Node> /*VariableLikeDeclaration*/, arena: &impl HasArena) -> bool {
    let root = get_root_declaration(node, arena);
    root.ref_(arena).kind() == SyntaxKind::Parameter
}

pub fn get_root_declaration(mut node: Id<Node>, arena: &impl HasArena) -> Id<Node> {
    while node.ref_(arena).kind() == SyntaxKind::BindingElement {
        node = node.ref_(arena).parent().ref_(arena).parent();
    }
    node
}

pub fn node_starts_new_lexical_environment(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Constructor
            | SyntaxKind::FunctionExpression
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::ArrowFunction
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::SourceFile
    )
}

pub fn node_is_synthesized<TRange: ReadonlyTextRange>(range: &TRange) -> bool {
    position_is_synthesized(range.pos()) || position_is_synthesized(range.end())
}

pub fn get_original_source_file(source_file: Id<Node> /*SourceFile*/, arena: &impl HasArena) -> Id<Node /*SourceFile*/> {
    get_parse_tree_node(
        Some(source_file),
        Some(|node: Id<Node>| is_source_file(&node.ref_(arena))),
        arena,
    )
    .unwrap_or(source_file)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

pub fn get_expression_associativity(expression: Id<Node> /*Expression*/, arena: &impl HasArena) -> Associativity {
    let operator = get_operator(expression, arena);
    let has_arguments = expression.ref_(arena).kind() == SyntaxKind::NewExpression
        && expression.ref_(arena).as_new_expression().arguments.is_some();
    get_operator_associativity(expression.ref_(arena).kind(), operator, Some(has_arguments))
}

pub fn get_operator_associativity(
    kind: SyntaxKind,
    operator: SyntaxKind,
    has_arguments: Option<bool>,
) -> Associativity {
    let has_arguments = has_arguments.unwrap_or(false);
    match kind {
        SyntaxKind::NewExpression => {
            return if has_arguments {
                Associativity::Left
            } else {
                Associativity::Right
            };
        }

        SyntaxKind::PrefixUnaryExpression
        | SyntaxKind::TypeOfExpression
        | SyntaxKind::VoidExpression
        | SyntaxKind::DeleteExpression
        | SyntaxKind::AwaitExpression
        | SyntaxKind::ConditionalExpression
        | SyntaxKind::YieldExpression => {
            return Associativity::Right;
        }

        SyntaxKind::BinaryExpression => match operator {
            SyntaxKind::AsteriskAsteriskToken
            | SyntaxKind::EqualsToken
            | SyntaxKind::PlusEqualsToken
            | SyntaxKind::MinusEqualsToken
            | SyntaxKind::AsteriskAsteriskEqualsToken
            | SyntaxKind::AsteriskEqualsToken
            | SyntaxKind::SlashEqualsToken
            | SyntaxKind::PercentEqualsToken
            | SyntaxKind::LessThanLessThanEqualsToken
            | SyntaxKind::GreaterThanGreaterThanEqualsToken
            | SyntaxKind::GreaterThanGreaterThanGreaterThanEqualsToken
            | SyntaxKind::AmpersandEqualsToken
            | SyntaxKind::CaretEqualsToken
            | SyntaxKind::BarEqualsToken
            | SyntaxKind::BarBarEqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken => {
                return Associativity::Right;
            }
            _ => (),
        },
        _ => (),
    }
    Associativity::Left
}

pub fn get_expression_precedence(expression: Id<Node> /*Expression*/, arena: &impl HasArena) -> OperatorPrecedence {
    let operator = get_operator(expression, arena);
    let has_arguments = expression.ref_(arena).kind() == SyntaxKind::NewExpression
        && expression.ref_(arena).as_new_expression().arguments.is_some();
    get_operator_precedence(expression.ref_(arena).kind(), operator, Some(has_arguments))
}

pub fn get_operator(expression: Id<Node>, arena: &impl HasArena) -> SyntaxKind {
    match &*expression.ref_(arena) {
        Node::BinaryExpression(expression) => expression.operator_token.ref_(arena).kind(),
        Node::PrefixUnaryExpression(expression) => expression.operator,
        Node::PostfixUnaryExpression(expression) => expression.operator,
        _ => expression.ref_(arena).kind(),
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum OperatorPrecedence {
    Comma,
    Spread,
    Yield,
    Assignment,
    Conditional,
    LogicalOR,
    LogicalAND,
    BitwiseOR,
    BitwiseXOR,
    BitwiseAND,
    Equality,
    Relational,
    Shift,
    Additive,
    Multiplicative,
    Exponentiation,
    Unary,
    Update,
    LeftHandSide,
    Member,
    Primary,
    Invalid = -1,
}

impl OperatorPrecedence {
    pub const Coalesce: OperatorPrecedence = OperatorPrecedence::Conditional;
    pub const Highest: OperatorPrecedence = OperatorPrecedence::Primary;
    pub const Lowest: OperatorPrecedence = OperatorPrecedence::Comma;
}

impl Ord for OperatorPrecedence {
    fn cmp(&self, other: &Self) -> Ordering {
        (*self as isize).cmp(&(*other as isize))
    }
}

impl PartialOrd for OperatorPrecedence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub fn get_operator_precedence(
    node_kind: SyntaxKind,
    operator_kind: SyntaxKind,
    has_arguments: Option<bool>,
) -> OperatorPrecedence {
    let has_arguments = has_arguments.unwrap_or(false);
    match node_kind {
        SyntaxKind::CommaListExpression => OperatorPrecedence::Comma,

        SyntaxKind::SpreadElement => OperatorPrecedence::Spread,

        SyntaxKind::YieldExpression => OperatorPrecedence::Yield,

        SyntaxKind::ConditionalExpression => OperatorPrecedence::Conditional,

        SyntaxKind::BinaryExpression => match operator_kind {
            SyntaxKind::CommaToken => OperatorPrecedence::Comma,

            SyntaxKind::EqualsToken
            | SyntaxKind::PlusEqualsToken
            | SyntaxKind::MinusEqualsToken
            | SyntaxKind::AsteriskAsteriskEqualsToken
            | SyntaxKind::AsteriskEqualsToken
            | SyntaxKind::SlashEqualsToken
            | SyntaxKind::PercentEqualsToken
            | SyntaxKind::LessThanLessThanEqualsToken
            | SyntaxKind::GreaterThanGreaterThanEqualsToken
            | SyntaxKind::GreaterThanGreaterThanGreaterThanEqualsToken
            | SyntaxKind::AmpersandEqualsToken
            | SyntaxKind::CaretEqualsToken
            | SyntaxKind::BarEqualsToken
            | SyntaxKind::BarBarEqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken => OperatorPrecedence::Assignment,

            _ => get_binary_operator_precedence(operator_kind),
        },
        SyntaxKind::TypeAssertionExpression
        | SyntaxKind::NonNullExpression
        | SyntaxKind::PrefixUnaryExpression
        | SyntaxKind::TypeOfExpression
        | SyntaxKind::VoidExpression
        | SyntaxKind::DeleteExpression
        | SyntaxKind::AwaitExpression => OperatorPrecedence::Unary,

        SyntaxKind::PostfixUnaryExpression => OperatorPrecedence::Update,

        SyntaxKind::CallExpression => OperatorPrecedence::LeftHandSide,

        SyntaxKind::NewExpression => {
            if has_arguments {
                OperatorPrecedence::Member
            } else {
                OperatorPrecedence::LeftHandSide
            }
        }

        SyntaxKind::TaggedTemplateExpression
        | SyntaxKind::PropertyAccessExpression
        | SyntaxKind::ElementAccessExpression
        | SyntaxKind::MetaProperty => OperatorPrecedence::Member,

        SyntaxKind::AsExpression => OperatorPrecedence::Relational,

        SyntaxKind::ThisKeyword
        | SyntaxKind::SuperKeyword
        | SyntaxKind::Identifier
        | SyntaxKind::PrivateIdentifier
        | SyntaxKind::NullKeyword
        | SyntaxKind::TrueKeyword
        | SyntaxKind::FalseKeyword
        | SyntaxKind::NumericLiteral
        | SyntaxKind::BigIntLiteral
        | SyntaxKind::StringLiteral
        | SyntaxKind::ArrayLiteralExpression
        | SyntaxKind::ObjectLiteralExpression
        | SyntaxKind::FunctionExpression
        | SyntaxKind::ArrowFunction
        | SyntaxKind::ClassExpression
        | SyntaxKind::RegularExpressionLiteral
        | SyntaxKind::NoSubstitutionTemplateLiteral
        | SyntaxKind::TemplateExpression
        | SyntaxKind::ParenthesizedExpression
        | SyntaxKind::OmittedExpression
        | SyntaxKind::JsxElement
        | SyntaxKind::JsxSelfClosingElement
        | SyntaxKind::JsxFragment => OperatorPrecedence::Primary,

        _ => OperatorPrecedence::Invalid,
    }
}

pub fn get_binary_operator_precedence(kind: SyntaxKind) -> OperatorPrecedence {
    match kind {
        SyntaxKind::QuestionQuestionToken => OperatorPrecedence::Coalesce,
        SyntaxKind::BarBarToken => OperatorPrecedence::LogicalOR,
        SyntaxKind::AmpersandAmpersandToken => OperatorPrecedence::LogicalAND,
        SyntaxKind::BarToken => OperatorPrecedence::BitwiseOR,
        SyntaxKind::CaretToken => OperatorPrecedence::BitwiseXOR,
        SyntaxKind::AmpersandToken => OperatorPrecedence::BitwiseAND,
        SyntaxKind::EqualsEqualsToken
        | SyntaxKind::ExclamationEqualsToken
        | SyntaxKind::EqualsEqualsEqualsToken
        | SyntaxKind::ExclamationEqualsEqualsToken => OperatorPrecedence::Equality,
        SyntaxKind::LessThanToken
        | SyntaxKind::GreaterThanToken
        | SyntaxKind::LessThanEqualsToken
        | SyntaxKind::GreaterThanEqualsToken
        | SyntaxKind::InstanceOfKeyword
        | SyntaxKind::InKeyword
        | SyntaxKind::AsKeyword => OperatorPrecedence::Relational,
        SyntaxKind::LessThanLessThanToken
        | SyntaxKind::GreaterThanGreaterThanToken
        | SyntaxKind::GreaterThanGreaterThanGreaterThanToken => OperatorPrecedence::Shift,
        SyntaxKind::PlusToken | SyntaxKind::MinusToken => OperatorPrecedence::Additive,
        SyntaxKind::AsteriskToken | SyntaxKind::SlashToken | SyntaxKind::PercentToken => {
            OperatorPrecedence::Multiplicative
        }
        SyntaxKind::AsteriskAsteriskToken => OperatorPrecedence::Exponentiation,
        _ => OperatorPrecedence::Invalid,
    }
}

pub fn get_semantic_jsx_children(children: &[Id<Node /*JsxChild*/>], arena: &impl HasArena) -> Vec<Id<Node>> {
    filter(children, |i: &Id<Node>| match i.ref_(arena).kind() {
        SyntaxKind::JsxExpression => i.ref_(arena).as_jsx_expression().expression.is_some(),
        SyntaxKind::JsxText => !i.ref_(arena).as_jsx_text().contains_only_trivia_white_spaces,
        _ => true,
    })
}

pub fn create_diagnostic_collection(arena: *const AllArenas) -> DiagnosticCollection {
    DiagnosticCollection::new(arena)
}

impl DiagnosticCollection {
    pub fn new(arena: *const AllArenas) -> Self {
        DiagnosticCollection {
            non_file_diagnostics: SortedArray::<Id<Diagnostic>>::new(vec![]),
            files_with_diagnostics: SortedArray::<String>::new(vec![]),
            file_diagnostics: HashMap::<String, SortedArray<Id<Diagnostic>>>::new(),
            has_read_non_file_diagnostics: Cell::new(false),
            arena,
        }
    }

    fn has_read_non_file_diagnostics(&self) -> bool {
        self.has_read_non_file_diagnostics.get()
    }

    fn set_has_read_non_file_diagnostics(&self, has_read_non_file_diagnostics: bool) {
        self.has_read_non_file_diagnostics
            .set(has_read_non_file_diagnostics);
    }

    pub fn lookup(&self, diagnostic: Id<Diagnostic>) -> Option<Id<Diagnostic>> {
        let diagnostics: Option<&SortedArray<Id<Diagnostic>>>;
        if let Some(diagnostic_file) = diagnostic.ref_(self).maybe_file() {
            diagnostics = self
                .file_diagnostics
                .get(&*diagnostic_file.ref_(self).as_source_file().file_name());
        } else {
            diagnostics = Some(&self.non_file_diagnostics);
        }
        let diagnostics = diagnostics?;
        let result = binary_search(
            diagnostics,
            &diagnostic,
            |diagnostic, _| diagnostic,
            |a, b| compare_diagnostics_skip_related_information(&*a.ref_(self), &*b.ref_(self), self),
            None,
        );
        if result >= 0 {
            return Some(diagnostics[usize::try_from(result).unwrap()].clone());
        }
        None
    }

    pub fn add(&mut self, diagnostic: Id<Diagnostic>) {
        if let Some(diagnostic_file) = {
            let diagnostic_file = diagnostic.ref_(self).maybe_file();
            diagnostic_file
        } {
            let file_name = diagnostic_file.ref_(self).as_source_file().file_name().clone();
            if self
                .file_diagnostics
                .get(&*diagnostic_file.ref_(self).as_source_file().file_name())
                .is_none()
            {
                let diagnostics = SortedArray::new(vec![]);
                self.file_diagnostics.insert(
                    file_name.clone(),
                    diagnostics,
                );
                insert_sorted(
                    &mut self.files_with_diagnostics,
                    file_name.clone(),
                    |a: &String, b: &String| compare_strings_case_sensitive(a, b),
                );
            }
            let diagnostics = self
                .file_diagnostics
                .get_mut(&*file_name)
                .unwrap();
            let arena = unsafe { &*self.arena };
            insert_sorted(
                diagnostics,
                diagnostic,
                |a: &Id<Diagnostic>, b: &Id<Diagnostic>| compare_diagnostics(&*a.ref_(arena), &*b.ref_(arena), arena),
            );
        } else {
            if self.has_read_non_file_diagnostics() {
                self.set_has_read_non_file_diagnostics(false);
                self.non_file_diagnostics = SortedArray::new(vec![]);
            }

            let diagnostics = &mut self.non_file_diagnostics;
            let arena = unsafe { &*self.arena };
            insert_sorted(
                diagnostics,
                diagnostic,
                |a: &Id<Diagnostic>, b: &Id<Diagnostic>| compare_diagnostics(&*a.ref_(arena), &*b.ref_(arena), arena),
            );
        }
    }

    pub fn get_global_diagnostics(&self) -> Vec<Id<Diagnostic>> {
        self.set_has_read_non_file_diagnostics(true);
        self.non_file_diagnostics.to_vec()
    }

    pub fn get_diagnostics(&self, file_name: Option<&str>) -> Vec<Id<Diagnostic>> {
        if let Some(file_name) = file_name {
            return self
                .file_diagnostics
                .get(file_name)
                .map(|sorted_array| sorted_array.into())
                .unwrap_or(vec![]);
        }

        let mut file_diags: Vec<Id<Diagnostic>> =
            flat_map_to_mutable(Some(&*self.files_with_diagnostics), |f, _| {
                self.file_diagnostics
                    .get(f)
                    .map_or_else(|| vec![], |sorted_array| sorted_array.to_vec())
            });
        if self.non_file_diagnostics.is_empty() {
            return file_diags;
        }
        file_diags.splice(0..0, self.non_file_diagnostics.to_vec());
        file_diags
    }
}

impl HasArena for DiagnosticCollection {
    fn arena(&self) -> &AllArenas {
        unsafe { &*self.arena }
    }
}

lazy_static! {
    static ref template_substitution_reg_exp: Regex = Regex::new(r"\$\{").unwrap();
}
pub(super) fn escape_template_substitution(str: &str) -> String {
    template_substitution_reg_exp
        .replace_all(str, "\\${")
        .to_string()
}

pub(crate) fn has_invalid_escape(template: Id<Node> /*TemplateLiteral*/, arena: &impl HasArena) -> bool {
    /*template &&*/
    if is_no_substitution_template_literal(&template.ref_(arena)) {
        matches!(
            template.ref_(arena).as_template_literal_like_node().maybe_template_flags(),
            Some(template_flags) if template_flags != TokenFlags::None
        )
    } else {
        let template_ref = template.ref_(arena);
        let template_as_template_expression = template_ref.as_template_expression();
        matches!(
            template_as_template_expression.head.ref_(arena).as_template_literal_like_node().maybe_template_flags(),
            Some(template_flags) if template_flags != TokenFlags::None
        ) || some(
            Some(&*template_as_template_expression.template_spans.ref_(arena)),
            Some(
                |span: &Id<Node>| matches!(
                    span.ref_(arena).as_template_span().literal.ref_(arena).as_template_literal_like_node().maybe_template_flags(),
                    Some(template_flags) if template_flags != TokenFlags::None
                ),
            ),
        )
    }
}

lazy_static! {
    static ref double_quote_escaped_chars_reg_exp: Regex = Regex::new(r#"[\\"\u0000-\u001f\t\v\f\u0008\r\n\u2028\u2029\u0085]"#/*/g*/).unwrap();
}
lazy_static! {
    static ref single_quote_escaped_chars_reg_exp: Regex = Regex::new(r#"[\\'\u0000-\u001f\t\v\f\u0008\r\n\u2028\u2029\u0085]"#/*/g*/).unwrap();
}
lazy_static! {
    static ref backtick_quote_escaped_chars_reg_exp: Regex = Regex::new(r#"\r\n|[\\`\u0000-\u001f\t\v\f\u0008\r\u2028\u2029\u0085]"#/*/g*/).unwrap();
}
lazy_static! {
    static ref escaped_chars_map: HashMap<&'static str, &'static str> =
        HashMap::from_iter(IntoIterator::into_iter([
            ("\t", "\\t"),
            ("\u{000b}", "\\v"),
            ("\u{000c}", "\\f"),
            ("\u{0008}", "\\b"),
            ("\r", "\\r"),
            ("\n", "\\n"),
            ("\\", "\\\\"),
            ("\"", "\\\""),
            ("'", "\\'"),
            ("`", "\\`"),
            ("\u{2028}", "\\u2028"),
            ("\u{2029}", "\\u2029"),
            ("\u{0085}", "\\u0085"),
            ("\r\n", "\\r\\n"),
        ]),);
}

pub fn encode_utf16_escape_sequence(char_code: u32) -> String {
    let hex_char_code = format!("{:X}", char_code);
    let padded_hex_code = format!("{:0>4}", hex_char_code);
    format!("\\u{}", padded_hex_code)
}

fn get_replacement(c: &str, offset: usize, input: &str) -> Cow<'static, str> {
    let c_as_chars = c.chars().collect::<Vec<_>>();
    if matches!(c_as_chars.get(0), Some(&CharacterCodes::null_character)) {
        let input_after_offset_as_chars = input[offset..].chars().collect::<Vec<_>>();
        let look_ahead = maybe_text_char_at_index(
            &input_after_offset_as_chars,
            /*offset + */ c_as_chars.len(),
        );
        if matches!(look_ahead, Some(look_ahead) if look_ahead >= CharacterCodes::_0 && look_ahead <= CharacterCodes::_9)
        {
            return "\\x00".into();
        }
        return "\\0".into();
    }
    if let Some(escaped) = escaped_chars_map.get(&c) {
        return (*escaped).into();
    }
    encode_utf16_escape_sequence(c_as_chars[0] as u32).into()
}

pub fn escape_string<'string>(
    s: &'string str,
    quote_char: Option<
        char, /*CharacterCodes.doubleQuote | CharacterCodes.singleQuote | CharacterCodes.backtick*/
    >,
) -> Cow<'string, str> {
    let escaped_chars_reg_exp = match quote_char {
        Some(CharacterCodes::backtick) => &*backtick_quote_escaped_chars_reg_exp,
        Some(CharacterCodes::single_quote) => &*single_quote_escaped_chars_reg_exp,
        _ => &*double_quote_escaped_chars_reg_exp,
    };
    escaped_chars_reg_exp.replace_all(s, |captures: &Captures| {
        let match_ = captures.get(0).unwrap();
        get_replacement(match_.as_str(), match_.start(), s)
    })
}

lazy_static! {
    static ref non_ascii_chars: Regex = Regex::new(r#"[^\u0000-\u007F]"#/*/g*/).unwrap();
}
pub fn escape_non_ascii_string<'string>(
    s: &'string str,
    quote_char: Option<
        char, /*CharacterCodes.doubleQuote | CharacterCodes.singleQuote | CharacterCodes.backtick*/
    >,
) -> Cow<'string, str> {
    let s = escape_string(s, quote_char);
    if non_ascii_chars.is_match(&s) {
        non_ascii_chars
            .replace_all(&s, |captures: &Captures| {
                let c = captures.get(0).unwrap().as_str();
                encode_utf16_escape_sequence(c.chars().next().unwrap() as u32)
            })
            .into_owned()
            .into()
    } else {
        s
    }
}

lazy_static! {
    static ref jsx_double_quote_escaped_chars_reg_exp: Regex = Regex::new(r#"["\u0000-\u001f\u2028\u2029\u0085]"#/*/g*/).unwrap();
}
lazy_static! {
    static ref jsx_single_quote_escaped_chars_reg_exp: Regex = Regex::new(r#"['\u0000-\u001f\u2028\u2029\u0085]"#/*/g*/).unwrap();
}
lazy_static! {
    static ref jsx_escaped_chars_map: HashMap<&'static str, &'static str> =
        HashMap::from_iter(IntoIterator::into_iter([("\"", "&quot;"), ("'", "&apos;")]));
}

fn encode_jsx_character_entity(char_code: u32) -> String {
    let hex_char_code = format!("{:X}", char_code);
    format!("&#x{};", hex_char_code)
}

fn get_jsx_attribute_string_replacement(c: &str) -> Cow<'static, str> {
    let c_as_chars = c.chars().collect::<Vec<_>>();
    if c_as_chars[0] == CharacterCodes::null_character {
        return "&#0;".into();
    }
    if let Some(escaped) = jsx_escaped_chars_map.get(&c) {
        return (*escaped).into();
    }
    encode_jsx_character_entity(c_as_chars[0] as u32).into()
}

pub fn escape_jsx_attribute_string<'string>(
    s: &'string str,
    quote_char: Option<char /*CharacterCodes.doubleQuote | CharacterCodes.singleQuote*/>,
) -> Cow<'string, str> {
    let escaped_chars_reg_exp = match quote_char {
        Some(quote_char) if quote_char == CharacterCodes::single_quote => {
            &*jsx_single_quote_escaped_chars_reg_exp
        }
        _ => &*jsx_double_quote_escaped_chars_reg_exp,
    };
    escaped_chars_reg_exp.replace_all(s, |captures: &Captures| {
        let match_ = captures.get(0).unwrap();
        get_jsx_attribute_string_replacement(match_.as_str())
    })
}

pub fn strip_quotes<'name>(name: &'name str) -> Cow<'name, str> {
    let name_as_chars = name.chars().collect::<Vec<_>>();
    let length = name_as_chars.len();
    if length >= 2
        && name_as_chars[0] == name_as_chars[length - 1]
        && is_quote_or_backtick(name_as_chars[0])
    {
        return name_as_chars[1..length - 1]
            .iter()
            .collect::<String>()
            .into();
    }
    name.into()
}
