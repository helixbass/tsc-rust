#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use regex::Regex;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::ops::Deref;
use std::ptr;
use std::rc::Rc;

use crate::{
    compare_diagnostics, concatenate, filter, get_assignment_declaration_kind,
    get_jsdoc_augments_tag, get_jsdoc_implements_tags, get_parse_tree_node, get_symbol_id,
    has_syntactic_modifier, id_text, is_binary_expression, is_class_expression, is_class_like,
    is_declaration, is_element_access_expression, is_entity_name_expression, is_export_assignment,
    is_in_js_file, is_interface_declaration, is_no_substituion_template_literal,
    is_numeric_literal, is_prefix_unary_expression, is_property_access_expression, is_source_file,
    is_string_literal_like, position_is_synthesized, single_element_array, skip_parentheses, some,
    starts_with, string_to_token, token_to_string, AssignmentDeclarationKind, Debug_,
    InterfaceOrClassLikeDeclarationInterface, ModifierFlags, NamedDeclarationInterface, Node,
    NodeArray, NodeInterface, ReadonlyTextRange, SortedArray, Symbol, SymbolInterface, SyntaxKind,
    TokenFlags, __String, escape_leading_underscores, get_name_of_declaration, insert_sorted,
    is_member_name, Diagnostic, DiagnosticCollection, DiagnosticRelatedInformationInterface,
};

pub fn is_literal_computed_property_name_declaration_name(node: &Node) -> bool {
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
) -> Option<Rc<Node /*Declaration*/>> {
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
) -> Rc<Node /*Expression*/> {
    if is_export_assignment(node) {
        node.as_export_assignment().expression.clone()
    } else {
        node.as_binary_expression().right.clone()
    }
}

pub fn get_property_assignment_alias_like_expression(
    node: &Node, /*PropertyAssignment | ShorthandPropertyAssignment | PropertyAccessExpression*/
) -> Rc<Node /*Expression*/> {
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
) -> Option<Rc<Node>> {
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
    node: &Node, /*ClassLikeDeclaration | InterfaceDeclaration*/
) -> Option<Rc<Node>> {
    let heritage_clause = get_heritage_clause(
        node.as_interface_or_class_like_declaration()
            .maybe_heritage_clauses(),
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
) -> Option<Vec<Rc<Node>>> {
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
                .maybe_heritage_clauses(),
            SyntaxKind::ImplementsKeyword,
        )?;
        Some(heritage_clause.as_heritage_clause().types.to_vec())
    }
}

pub fn get_all_super_type_nodes(node: &Node) -> Vec<Rc<Node>> {
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

pub fn get_interface_base_type_nodes(node: &Node, /*InterfaceDeclaration*/) -> Option<NodeArray> {
    let heritage_clause = get_heritage_clause(
        node.as_interface_declaration().maybe_heritage_clauses(),
        SyntaxKind::ExtendsKeyword,
    )?;
    Some(heritage_clause.as_heritage_clause().types.clone())
}

pub fn get_heritage_clause(clauses: Option<&NodeArray>, kind: SyntaxKind) -> Option<Rc<Node>> {
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
) -> Option<Rc<Node>> {
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

pub fn get_property_name_for_property_name_node(name: &Node, /*PropertyName*/) -> Option<__String> {
    match name.kind() {
        SyntaxKind::Identifier => Some(name.as_identifier().escaped_text.clone()),
        SyntaxKind::PrivateIdentifier => Some(name.as_private_identifier().escaped_text.clone()),
        SyntaxKind::StringLiteral | SyntaxKind::NumericLiteral => Some(escape_leading_underscores(
            &name.as_literal_like_node().text(),
        )),
        SyntaxKind::ComputedPropertyName => {
            let name_expression = &name.as_computed_property_name().expression;
            if is_string_or_numeric_literal_like(name_expression) {
                Some(escape_leading_underscores(
                    &name_expression.as_literal_like_node().text(),
                ))
            } else if is_signed_numeric_literal(name_expression) {
                let name_expression_as_prefix_unary_expression =
                    name_expression.as_prefix_unary_expression();
                if name_expression_as_prefix_unary_expression.operator == SyntaxKind::MinusToken {
                    Some(__String::new(format!(
                        "{}{}",
                        token_to_string(name_expression_as_prefix_unary_expression.operator)
                            .unwrap(),
                        name_expression_as_prefix_unary_expression
                            .operand
                            .as_literal_like_node()
                            .text()
                    )))
                } else {
                    Some(__String::new(
                        name_expression_as_prefix_unary_expression
                            .operand
                            .as_literal_like_node()
                            .text()
                            .clone(),
                    ))
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

pub fn get_text_of_identifier_or_literal(node: &Node) -> String {
    if is_member_name(node) {
        id_text(node)
    } else {
        node.as_literal_like_node().text().to_owned()
    }
}

pub fn get_escaped_text_of_identifier_or_literal(node: &Node) -> __String {
    if is_member_name(node) {
        node.as_member_name().escaped_text()
    } else {
        escape_leading_underscores(&node.as_literal_like_node().text())
    }
}

pub fn get_property_name_for_unique_es_symbol(symbol: &Symbol) -> __String {
    __String::new(format!(
        "__@{}@{}",
        get_symbol_id(symbol),
        symbol.escaped_name().deref()
    ))
}

pub fn get_symbol_name_for_private_identifier(
    containing_class_symbol: &Symbol,
    description: &__String,
) -> __String {
    __String::new(format!(
        "__#{}@{}",
        get_symbol_id(containing_class_symbol),
        description.deref()
    ))
}

pub fn is_known_symbol(symbol: &Symbol) -> bool {
    starts_with(symbol.escaped_name(), "__@")
}

pub fn is_private_identifier_symbol(symbol: &Symbol) -> bool {
    starts_with(symbol.escaped_name(), "__#")
}

pub fn is_es_symbol_identifier(node: &Node) -> bool {
    node.kind() == SyntaxKind::Identifier && node.as_identifier().escaped_text.eq_str("Symbol")
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

pub fn get_root_declaration(node: &Node) -> Rc<Node> {
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

pub fn get_original_source_file(source_file: &Node /*SourceFile*/) -> Rc<Node /*SourceFile*/> {
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

pub fn get_semantic_jsx_children(children: &[Rc<Node /*JsxChild*/>]) -> Vec<Rc<Node>> {
    filter(Some(children), |i| match i.kind() {
        SyntaxKind::JsxExpression => i.as_jsx_expression().expression.is_some(),
        SyntaxKind::JsxText => !i.as_jsx_text().contains_only_trivia_white_spaces,
        _ => true,
    })
    .unwrap()
}

pub fn create_diagnostic_collection() -> DiagnosticCollection {
    DiagnosticCollection::new()
}

impl DiagnosticCollection {
    pub fn new() -> Self {
        DiagnosticCollection {
            file_diagnostics: HashMap::<String, SortedArray<Rc<Diagnostic>>>::new(),
        }
    }

    pub fn add(&mut self, diagnostic: Rc<Diagnostic>) {
        if let Some(diagnostics) = self
            .file_diagnostics
            .get_mut(&*diagnostic.file().unwrap().as_source_file().file_name())
        {
            insert_sorted(
                diagnostics,
                diagnostic,
                |rc_diagnostic_a: &Rc<Diagnostic>, rc_diagnostic_b: &Rc<Diagnostic>| {
                    compare_diagnostics(&**rc_diagnostic_a, &**rc_diagnostic_b)
                },
            );
            return;
        }
        let diagnostics: SortedArray<Rc<Diagnostic>> = SortedArray::new(vec![]);
        self.file_diagnostics.insert(
            diagnostic
                .file()
                .unwrap()
                .as_source_file()
                .file_name()
                .to_string(),
            diagnostics,
        );
        let diagnostics = self
            .file_diagnostics
            .get_mut(&*diagnostic.file().unwrap().as_source_file().file_name())
            .unwrap();
        insert_sorted(
            diagnostics,
            diagnostic,
            |rc_diagnostic_a: &Rc<Diagnostic>, rc_diagnostic_b: &Rc<Diagnostic>| {
                compare_diagnostics(&**rc_diagnostic_a, &**rc_diagnostic_b)
            },
        );
    }

    pub fn get_diagnostics(&self, file_name: &str) -> Vec<Rc<Diagnostic>> {
        self.file_diagnostics
            .get(file_name)
            .map(|sorted_array| sorted_array.into())
            .unwrap_or(vec![])
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
    if is_no_substituion_template_literal(template) {
        matches!(template.as_template_literal_like_node().maybe_template_flags(), Some(template_flags) if template_flags != TokenFlags::None)
    } else {
        let template_as_template_expression = template.as_template_expression();
        matches!(template_as_template_expression.head.as_template_literal_like_node().maybe_template_flags(), Some(template_flags) if template_flags != TokenFlags::None)
            || some(
                Some(&template_as_template_expression.template_spans),
                Some(
                    |span: &Rc<Node>| matches!(span.as_template_span().literal.as_template_literal_like_node().maybe_template_flags(), Some(template_flags) if template_flags != TokenFlags::None),
                ),
            )
    }
}

pub fn escape_string(
    s: &str,
    quote_char: Option<
        char, /*CharacterCodes.doubleQuote | CharacterCodes.singleQuote | CharacterCodes.backtick*/
    >,
) -> String {
    unimplemented!()
}

pub fn escape_non_ascii_string(
    s: &str,
    quote_char: Option<
        char, /*CharacterCodes.doubleQuote | CharacterCodes.singleQuote | CharacterCodes.backtick*/
    >,
) -> String {
    s.to_string()
}

pub fn escape_jsx_attribute_string(
    s: &str,
    quote_char: Option<char /*CharacterCodes.doubleQuote | CharacterCodes.singleQuote*/>,
) -> String {
    unimplemented!()
}
