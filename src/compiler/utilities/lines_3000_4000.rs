#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use regex::Regex;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    compare_diagnostics, has_syntactic_modifier, id_text, is_no_substituion_template_literal,
    is_numeric_literal, is_string_literal_like, position_is_synthesized, some, ModifierFlags, Node,
    NodeInterface, ReadonlyTextRange, SortedArray, SyntaxKind, TokenFlags, __String,
    escape_leading_underscores, get_name_of_declaration, insert_sorted, is_member_name, Diagnostic,
    DiagnosticCollection, DiagnosticRelatedInformationInterface,
};

pub fn is_keyword(token: SyntaxKind) -> bool {
    SyntaxKind::FirstKeyword <= token && token <= SyntaxKind::LastKeyword
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

pub fn get_function_flags<TNodeRef: Borrow<Node>>(
    node: Option<TNodeRef /*SignatureDeclaration*/>,
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

pub fn is_string_or_numeric_literal_like(node: &Node) -> bool {
    is_string_literal_like(node) || is_numeric_literal(node)
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
    false
}

pub fn is_property_name_literal(node: &Node) -> bool {
    match node.kind() {
        SyntaxKind::Identifier
        | SyntaxKind::StringLiteral
        | SyntaxKind::NoSubstitutionTemplateLiteral
        | SyntaxKind::NumericLiteral => true,
        _ => false,
    }
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
        escape_leading_underscores(&*node.as_literal_like_node().text())
    }
}

pub fn get_root_declaration(node: &Node) -> Rc<Node> {
    let mut node = node.node_wrapper();
    while node.kind() == SyntaxKind::BindingElement {
        node = node.parent().parent();
    }
    node
}

pub fn node_is_synthesized<TRange: ReadonlyTextRange>(range: &TRange) -> bool {
    position_is_synthesized(range.pos()) || position_is_synthesized(range.end())
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

pub fn get_expression_precedence(expression: &Node) -> OperatorPrecedence {
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

pub fn escape_non_ascii_string(
    s: &str,
    quote_char: Option<
        char, /*CharacterCodes.doubleQuote | CharacterCodes.singleQuote | CharacterCodes.backtick*/
    >,
) -> String {
    s.to_string()
}
