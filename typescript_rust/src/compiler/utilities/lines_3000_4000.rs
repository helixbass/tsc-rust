use bitflags::bitflags;
use gc::Gc;
use regex::{Captures, Regex};
use std::borrow::{Borrow, Cow};
use std::cell::Cell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::FromIterator;
use std::ops::Deref;
use std::ptr;

use super::is_quote_or_backtick;
use crate::{
    binary_search, compare_diagnostics, compare_diagnostics_skip_related_information,
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
    DiagnosticRelatedInformationInterface,
};

pub fn is_literal_computed_property_declaration_name(node: &Node) -> bool {
    is_string_or_numeric_literal_like(node)
        && node.parent().kind() == SyntaxKind::ComputedPropertyName
        && is_declaration(&node.parent().parent())
}

pub fn is_identifier_name(node: &Node /*Identifier*/) -> bool {
    let parent = node.parent();
    match parent.kind() {
        SyntaxKind::PropertyDeclaration
        | SyntaxKind::PropertySignature
        | SyntaxKind::MethodDeclaration
        | SyntaxKind::MethodSignature
        | SyntaxKind::GetAccessor
        | SyntaxKind::SetAccessor
        | SyntaxKind::EnumMember
        | SyntaxKind::PropertyAssignment
        | SyntaxKind::PropertyAccessExpression => {
            matches!(parent.as_named_declaration().maybe_name(), Some(name) if ptr::eq(&*name, node))
        }
        SyntaxKind::QualifiedName => ptr::eq(&*parent.as_qualified_name().right, node),
        SyntaxKind::BindingElement => {
            matches!(parent.as_binding_element().property_name.as_ref(), Some(property_name) if ptr::eq(&**property_name, node))
        }
        SyntaxKind::ImportSpecifier => {
            matches!(parent.as_import_specifier().property_name.as_ref(), Some(property_name) if ptr::eq(&**property_name, node))
        }
        SyntaxKind::ExportSpecifier | SyntaxKind::JsxAttribute => true,
        _ => false,
    }
}

pub fn is_alias_symbol_declaration(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ImportEqualsDeclaration | SyntaxKind::NamespaceExportDeclaration
    ) || node.kind() == SyntaxKind::ImportClause && node.as_import_clause().name.is_some()
        || matches!(
            node.kind(),
            SyntaxKind::NamespaceImport
                | SyntaxKind::NamespaceExport
                | SyntaxKind::ImportSpecifier
                | SyntaxKind::ExportSpecifier
        )
        || node.kind() == SyntaxKind::ExportAssignment && export_assignment_is_alias(node)
        || is_binary_expression(node)
            && get_assignment_declaration_kind(node) == AssignmentDeclarationKind::ModuleExports
            && export_assignment_is_alias(node)
        || is_property_access_expression(node) && is_binary_expression(&node.parent()) && {
            let node_parent = node.parent();
            let node_parent_as_binary_expression = node_parent.as_binary_expression();
            ptr::eq(&*node_parent_as_binary_expression.left, node)
                && node_parent_as_binary_expression.operator_token.kind() == SyntaxKind::EqualsToken
                && is_aliasable_expression(&node_parent_as_binary_expression.right)
        }
        || node.kind() == SyntaxKind::ShorthandPropertyAssignment
        || node.kind() == SyntaxKind::PropertyAssignment
            && is_aliasable_expression(&node.as_property_assignment().initializer)
}

pub fn get_alias_declaration_from_name(
    node: &Node, /*EntityName*/
) -> Option<Gc<Node /*Declaration*/>> {
    match node.parent().kind() {
        SyntaxKind::ImportClause
        | SyntaxKind::ImportSpecifier
        | SyntaxKind::NamespaceImport
        | SyntaxKind::ExportSpecifier
        | SyntaxKind::ExportAssignment
        | SyntaxKind::ImportEqualsDeclaration => Some(node.parent()),
        SyntaxKind::QualifiedName => {
            let mut node = node.node_wrapper();
            while {
                node = node.parent();
                node.parent().kind() == SyntaxKind::QualifiedName
            } {}
            get_alias_declaration_from_name(&node)
        }
        _ => None,
    }
}

pub fn is_aliasable_expression(e: &Node /*Expression*/) -> bool {
    is_entity_name_expression(e) || is_class_expression(e)
}

pub fn export_assignment_is_alias(node: &Node, /*ExportAssignment | BinaryExpression*/) -> bool {
    let e = get_export_assignment_expression(node);
    is_aliasable_expression(&e)
}

pub fn get_export_assignment_expression(
    node: &Node, /*ExportAssignment | BinaryExpression*/
) -> Gc<Node /*Expression*/> {
    if is_export_assignment(node) {
        node.as_export_assignment().expression.clone()
    } else {
        node.as_binary_expression().right.clone()
    }
}

pub fn get_property_assignment_alias_like_expression(
    node: &Node, /*PropertyAssignment | ShorthandPropertyAssignment | PropertyAccessExpression*/
) -> Gc<Node /*Expression*/> {
    if node.kind() == SyntaxKind::ShorthandPropertyAssignment {
        node.as_shorthand_property_assignment().name()
    } else if node.kind() == SyntaxKind::PropertyAssignment {
        node.as_property_assignment().initializer.clone()
    } else {
        node.parent().as_binary_expression().right.clone()
    }
}

pub fn get_effective_base_type_node(
    node: &Node, /*ClassLikeDeclaration | InterfaceDeclaration*/
) -> Option<Gc<Node>> {
    let base_type = get_class_extends_heritage_element(node);
    if base_type.is_some() && is_in_js_file(Some(node)) {
        let tag = get_jsdoc_augments_tag(node);
        if let Some(tag) = tag {
            return Some(tag.as_jsdoc_augments_tag().class.clone());
        }
    }
    base_type
}

pub fn get_class_extends_heritage_element(
    node: &Node, /*ClassLikeDeclaration | InterfaceDeclaration (or maybe also eg FunctionDeclaration)*/
) -> Option<Gc<Node>> {
    let heritage_clause = get_heritage_clause(
        node.maybe_as_interface_or_class_like_declaration()
            .and_then(|node| node.maybe_heritage_clauses())
            .as_deref(),
        SyntaxKind::ExtendsKeyword,
    )?;
    let heritage_clause_as_heritage_clause = heritage_clause.as_heritage_clause();
    if !heritage_clause_as_heritage_clause.types.is_empty() {
        Some(heritage_clause_as_heritage_clause.types[0].clone())
    } else {
        None
    }
}

pub fn get_effective_implements_type_nodes(
    node: &Node, /*ClassLikeDeclaration*/
) -> Option<Vec<Gc<Node>>> {
    if is_in_js_file(Some(node)) {
        Some(
            get_jsdoc_implements_tags(node)
                .iter()
                .map(|n| n.as_jsdoc_implements_tag().class.clone())
                .collect(),
        )
    } else {
        let heritage_clause = get_heritage_clause(
            node.as_interface_or_class_like_declaration()
                .maybe_heritage_clauses()
                .as_deref(),
            SyntaxKind::ImplementsKeyword,
        )?;
        Some(heritage_clause.as_heritage_clause().types.to_vec())
    }
}

pub fn get_all_super_type_nodes(node: &Node) -> Vec<Gc<Node>> {
    if is_interface_declaration(node) {
        get_interface_base_type_nodes(node).map_or_else(|| vec![], |node_array| node_array.to_vec())
    } else if is_class_like(node) {
        concatenate(
            single_element_array(get_effective_base_type_node(node)).unwrap_or_else(|| vec![]),
            get_effective_implements_type_nodes(node).unwrap_or_else(|| vec![]),
        )
    } else {
        vec![]
    }
}

pub fn get_interface_base_type_nodes(
    node: &Node, /*InterfaceDeclaration*/
) -> Option<Gc<NodeArray>> {
    let heritage_clause = get_heritage_clause(
        node.as_interface_declaration()
            .maybe_heritage_clauses()
            .as_deref(),
        SyntaxKind::ExtendsKeyword,
    )?;
    Some(heritage_clause.as_heritage_clause().types.clone())
}

pub fn get_heritage_clause(clauses: Option<&NodeArray>, kind: SyntaxKind) -> Option<Gc<Node>> {
    let clauses = clauses?;
    for clause in clauses {
        if clause.as_heritage_clause().token == kind {
            return Some(clause.clone());
        }
    }
    None
}

pub fn get_ancestor<TNode: Borrow<Node>>(
    node: Option<TNode>,
    kind: SyntaxKind,
) -> Option<Gc<Node>> {
    let node = node?;
    let node = node.borrow();
    let mut node = Some(node.node_wrapper());
    while let Some(node_present) = node {
        if node_present.kind() == kind {
            return Some(node_present);
        }
        node = node_present.maybe_parent();
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

pub fn get_function_flags<TNode: Borrow<Node>>(
    node: Option<TNode /*SignatureDeclaration*/>,
) -> FunctionFlags {
    if node.is_none() {
        return FunctionFlags::Invalid;
    }
    let node = node.unwrap();
    let node = node.borrow();

    let mut flags = FunctionFlags::Normal;
    match node.kind() {
        SyntaxKind::FunctionDeclaration
        | SyntaxKind::FunctionExpression
        | SyntaxKind::MethodDeclaration => {
            if node
                .as_function_like_declaration()
                .maybe_asterisk_token()
                .is_some()
            {
                flags |= FunctionFlags::Generator;
            }
            if has_syntactic_modifier(node, ModifierFlags::Async) {
                flags |= FunctionFlags::Async;
            }
        }

        SyntaxKind::ArrowFunction => {
            if has_syntactic_modifier(node, ModifierFlags::Async) {
                flags |= FunctionFlags::Async;
            }
        }
        _ => (),
    }

    if node
        .maybe_as_function_like_declaration()
        .and_then(|node| node.maybe_body())
        .is_none()
    {
        flags |= FunctionFlags::Invalid;
    }

    flags
}

pub fn is_async_function(node: &Node) -> bool {
    match node.kind() {
        SyntaxKind::FunctionDeclaration
        | SyntaxKind::FunctionExpression
        | SyntaxKind::ArrowFunction
        | SyntaxKind::MethodDeclaration => {
            let node_as_function_like_declaration = node.as_function_like_declaration();
            node_as_function_like_declaration.maybe_body().is_some()
                && node_as_function_like_declaration
                    .maybe_asterisk_token()
                    .is_none()
                && has_syntactic_modifier(node, ModifierFlags::Async)
        }
        _ => false,
    }
}

pub fn is_string_or_numeric_literal_like(node: &Node) -> bool {
    is_string_literal_like(node) || is_numeric_literal(node)
}

pub fn is_signed_numeric_literal(node: &Node) -> bool {
    if !is_prefix_unary_expression(node) {
        return false;
    }
    let node_as_prefix_unary_expression = node.as_prefix_unary_expression();
    matches!(
        node_as_prefix_unary_expression.operator,
        SyntaxKind::PlusToken | SyntaxKind::MinusToken
    ) && is_numeric_literal(&node_as_prefix_unary_expression.operand)
}

pub fn has_dynamic_name(declaration: &Node /*Declaration*/) -> bool {
    let name = get_name_of_declaration(Some(declaration));
    if let Some(name) = name {
        is_dynamic_name(&name)
    } else {
        false
    }
}

pub fn is_dynamic_name(name: &Node /*DeclarationName*/) -> bool {
    if !matches!(
        name.kind(),
        SyntaxKind::ComputedPropertyName | SyntaxKind::ElementAccessExpression
    ) {
        return false;
    }
    let expr = if is_element_access_expression(name) {
        skip_parentheses(
            &name.as_element_access_expression().argument_expression,
            None,
        )
    } else {
        name.as_computed_property_name().expression.clone()
    };
    !is_string_or_numeric_literal_like(&expr) && !is_signed_numeric_literal(&expr)
}

pub fn get_property_name_for_property_name_node<'name>(
    name: &'name Node, /*PropertyName*/
) -> Option<Cow<'name, str> /*__String*/> {
    match name.kind() {
        SyntaxKind::Identifier => Some((&*name.as_identifier().escaped_text).into()),
        SyntaxKind::PrivateIdentifier => Some((&*name.as_private_identifier().escaped_text).into()),
        SyntaxKind::StringLiteral | SyntaxKind::NumericLiteral => Some(
            escape_leading_underscores(&name.as_literal_like_node().text())
                .into_owned()
                .into(),
        ),
        SyntaxKind::ComputedPropertyName => {
            let name_expression = &name.as_computed_property_name().expression;
            if is_string_or_numeric_literal_like(name_expression) {
                Some(
                    escape_leading_underscores(&name_expression.as_literal_like_node().text())
                        .into_owned()
                        .into(),
                )
            } else if is_signed_numeric_literal(name_expression) {
                let name_expression_as_prefix_unary_expression =
                    name_expression.as_prefix_unary_expression();
                if name_expression_as_prefix_unary_expression.operator == SyntaxKind::MinusToken {
                    Some(
                        format!(
                            "{}{}",
                            token_to_string(name_expression_as_prefix_unary_expression.operator)
                                .unwrap(),
                            name_expression_as_prefix_unary_expression
                                .operand
                                .as_literal_like_node()
                                .text()
                        )
                        .into(),
                    )
                } else {
                    Some(
                        name_expression_as_prefix_unary_expression
                            .operand
                            .as_literal_like_node()
                            .text()
                            .clone()
                            .into(),
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

pub fn get_text_of_identifier_or_literal<'node>(node: &'node Node) -> Cow<'node, str> {
    if is_member_name(node) {
        id_text(node).into()
    } else {
        node.as_literal_like_node().text().to_owned().into()
    }
}

pub fn get_escaped_text_of_identifier_or_literal<'node>(node: &'node Node) -> Cow<'node, str> /*__String*/
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

pub fn is_parameter_declaration(node: &Node /*VariableLikeDeclaration*/) -> bool {
    let root = get_root_declaration(node);
    root.kind() == SyntaxKind::Parameter
}

pub fn get_root_declaration(node: &Node) -> Gc<Node> {
    let mut node = node.node_wrapper();
    while node.kind() == SyntaxKind::BindingElement {
        node = node.parent().parent();
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

pub fn get_original_source_file(source_file: &Node /*SourceFile*/) -> Gc<Node /*SourceFile*/> {
    get_parse_tree_node(Some(source_file), Some(|node: &Node| is_source_file(node)))
        .unwrap_or_else(|| source_file.node_wrapper())
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

pub fn get_expression_associativity(expression: &Node /*Expression*/) -> Associativity {
    let operator = get_operator(expression);
    let has_arguments = expression.kind() == SyntaxKind::NewExpression
        && expression.as_new_expression().arguments.is_some();
    get_operator_associativity(expression.kind(), operator, Some(has_arguments))
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

pub fn get_expression_precedence(expression: &Node /*Expression*/) -> OperatorPrecedence {
    let operator = get_operator(expression);
    let has_arguments = expression.kind() == SyntaxKind::NewExpression
        && expression.as_new_expression().arguments.is_some();
    get_operator_precedence(expression.kind(), operator, Some(has_arguments))
}

pub fn get_operator(expression: &Node) -> SyntaxKind {
    match expression {
        Node::BinaryExpression(expression) => expression.operator_token.kind(),
        Node::PrefixUnaryExpression(expression) => expression.operator,
        Node::PostfixUnaryExpression(expression) => expression.operator,
        _ => expression.kind(),
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

pub fn get_semantic_jsx_children(children: &[Gc<Node /*JsxChild*/>]) -> Vec<Gc<Node>> {
    filter(children, |i: &Gc<Node>| match i.kind() {
        SyntaxKind::JsxExpression => i.as_jsx_expression().expression.is_some(),
        SyntaxKind::JsxText => !i.as_jsx_text().contains_only_trivia_white_spaces,
        _ => true,
    })
}

pub fn create_diagnostic_collection() -> DiagnosticCollection {
    DiagnosticCollection::new()
}

impl DiagnosticCollection {
    pub fn new() -> Self {
        DiagnosticCollection {
            non_file_diagnostics: SortedArray::<Gc<Diagnostic>>::new(vec![]),
            files_with_diagnostics: SortedArray::<String>::new(vec![]),
            file_diagnostics: HashMap::<String, SortedArray<Gc<Diagnostic>>>::new(),
            has_read_non_file_diagnostics: Cell::new(false),
        }
    }

    fn has_read_non_file_diagnostics(&self) -> bool {
        self.has_read_non_file_diagnostics.get()
    }

    fn set_has_read_non_file_diagnostics(&self, has_read_non_file_diagnostics: bool) {
        self.has_read_non_file_diagnostics
            .set(has_read_non_file_diagnostics);
    }

    pub fn lookup(&self, diagnostic: Gc<Diagnostic>) -> Option<Gc<Diagnostic>> {
        let diagnostics: Option<&SortedArray<Gc<Diagnostic>>>;
        if let Some(diagnostic_file) = diagnostic.maybe_file() {
            diagnostics = self
                .file_diagnostics
                .get(&*diagnostic_file.as_source_file().file_name());
        } else {
            diagnostics = Some(&self.non_file_diagnostics);
        }
        let diagnostics = diagnostics?;
        let result = binary_search(
            diagnostics,
            &diagnostic,
            |diagnostic, _| diagnostic,
            |a, b| compare_diagnostics_skip_related_information(&**a, &**b),
            None,
        );
        if result >= 0 {
            return Some(diagnostics[usize::try_from(result).unwrap()].clone());
        }
        None
    }

    pub fn add(&mut self, diagnostic: Gc<Diagnostic>) {
        if let Some(diagnostic_file) = diagnostic.maybe_file() {
            let diagnostic_file_as_source_file = diagnostic_file.as_source_file();
            if self
                .file_diagnostics
                .get(&*diagnostic_file_as_source_file.file_name())
                .is_none()
            {
                let diagnostics = SortedArray::new(vec![]);
                self.file_diagnostics.insert(
                    diagnostic_file_as_source_file.file_name().clone(),
                    diagnostics,
                );
                insert_sorted(
                    &mut self.files_with_diagnostics,
                    diagnostic_file_as_source_file.file_name().clone(),
                    |a: &String, b: &String| compare_strings_case_sensitive(a, b),
                );
            }
            let diagnostics = self
                .file_diagnostics
                .get_mut(&*diagnostic_file_as_source_file.file_name())
                .unwrap();
            insert_sorted(
                diagnostics,
                diagnostic,
                |a: &Gc<Diagnostic>, b: &Gc<Diagnostic>| compare_diagnostics(&**a, &**b),
            );
        } else {
            if self.has_read_non_file_diagnostics() {
                self.set_has_read_non_file_diagnostics(false);
                self.non_file_diagnostics = SortedArray::new(vec![]);
            }

            let diagnostics = &mut self.non_file_diagnostics;
            insert_sorted(
                diagnostics,
                diagnostic,
                |a: &Gc<Diagnostic>, b: &Gc<Diagnostic>| compare_diagnostics(&**a, &**b),
            );
        }
    }

    pub fn get_global_diagnostics(&self) -> Vec<Gc<Diagnostic>> {
        self.set_has_read_non_file_diagnostics(true);
        self.non_file_diagnostics.to_vec()
    }

    pub fn get_diagnostics(&self, file_name: Option<&str>) -> Vec<Gc<Diagnostic>> {
        if let Some(file_name) = file_name {
            return self
                .file_diagnostics
                .get(file_name)
                .map(|sorted_array| sorted_array.into())
                .unwrap_or(vec![]);
        }

        let mut file_diags: Vec<Gc<Diagnostic>> =
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

lazy_static! {
    static ref template_substitution_reg_exp: Regex = Regex::new(r"\$\{").unwrap();
}
pub(super) fn escape_template_substitution(str: &str) -> String {
    template_substitution_reg_exp
        .replace_all(str, "\\${")
        .to_string()
}

pub(crate) fn has_invalid_escape(template: &Node /*TemplateLiteral*/) -> bool {
    /*template &&*/
    if is_no_substitution_template_literal(template) {
        matches!(template.as_template_literal_like_node().maybe_template_flags(), Some(template_flags) if template_flags != TokenFlags::None)
    } else {
        let template_as_template_expression = template.as_template_expression();
        matches!(template_as_template_expression.head.as_template_literal_like_node().maybe_template_flags(), Some(template_flags) if template_flags != TokenFlags::None)
            || some(
                Some(&template_as_template_expression.template_spans),
                Some(
                    |span: &Gc<Node>| matches!(span.as_template_span().literal.as_template_literal_like_node().maybe_template_flags(), Some(template_flags) if template_flags != TokenFlags::None),
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
