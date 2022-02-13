#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::Cell;
use std::rc::Rc;

use super::{
    BaseFunctionLikeDeclaration, BaseNode, HasTypeArgumentsInterface, NamedDeclarationInterface,
    Node, NodeArray, SyntaxKind,
};
use local_macros::ast_type;

#[derive(Debug)]
#[ast_type]
pub struct BinaryExpression {
    pub _node: BaseNode,
    pub left: Rc<Node>,
    pub operator_token: Rc<Node>,
    pub right: Rc<Node>,
    cached_literal_kind: Cell<Option<SyntaxKind>>,
}

impl BinaryExpression {
    pub fn new(
        base_node: BaseNode,
        left: Rc<Node>,
        operator_token: Rc<Node>,
        right: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            left,
            operator_token,
            right,
            cached_literal_kind: Cell::new(None),
        }
    }

    pub fn maybe_cached_literal_kind(&self) -> Option<SyntaxKind> {
        self.cached_literal_kind.get()
    }

    pub fn set_cached_literal_kind(&self, cached_literal_kind: SyntaxKind) {
        self.cached_literal_kind.set(Some(cached_literal_kind));
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ConditionalExpression {
    _node: BaseNode,
    pub condition: Rc<Node /*Expression*/>,
    pub question_token: Rc<Node /*QuestionToken*/>,
    pub when_true: Rc<Node /*Expression*/>,
    pub colon_token: Rc<Node /*ColonToken*/>,
    pub when_false: Rc<Node /*Expression*/>,
}

impl ConditionalExpression {
    pub fn new(
        base_node: BaseNode,
        condition: Rc<Node /*Expression*/>,
        question_token: Rc<Node /*QuestionToken*/>,
        when_true: Rc<Node /*Expression*/>,
        colon_token: Rc<Node /*ColonToken*/>,
        when_false: Rc<Node /*Expression*/>,
    ) -> Self {
        Self {
            _node: base_node,
            condition,
            question_token,
            when_true,
            colon_token,
            when_false,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface"
)]
pub struct FunctionExpression {
    _function_like_declaration: BaseFunctionLikeDeclaration,
}

impl FunctionExpression {
    pub fn new(function_like_declaration: BaseFunctionLikeDeclaration) -> Self {
        Self {
            _function_like_declaration: function_like_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface"
)]
pub struct ArrowFunction {
    _function_like_declaration: BaseFunctionLikeDeclaration,
    pub equals_greater_than_token: Rc<Node /*EqualsGreaterThanToken*/>,
}

impl ArrowFunction {
    pub fn new(
        function_like_declaration: BaseFunctionLikeDeclaration,
        equals_greater_than_token: Rc<Node>,
    ) -> Self {
        Self {
            _function_like_declaration: function_like_declaration,
            equals_greater_than_token,
        }
    }
}

#[derive(Debug)]
#[ast_type(impl_from = false)]
pub struct BaseLiteralLikeNode {
    _node: BaseNode,
    text: String,
    is_unterminated: Cell<Option<bool>>,
    has_extended_unicode_escape: Cell<Option<bool>>,
}

impl BaseLiteralLikeNode {
    pub fn new(base_node: BaseNode, text: String) -> Self {
        Self {
            _node: base_node,
            text,
            is_unterminated: Cell::new(None),
            has_extended_unicode_escape: Cell::new(None),
        }
    }
}

impl LiteralLikeNodeInterface for BaseLiteralLikeNode {
    fn text(&self) -> &str {
        &self.text
    }

    fn is_unterminated(&self) -> Option<bool> {
        self.is_unterminated.get()
    }

    fn set_is_unterminated(&self, is_unterminated: Option<bool>) {
        self.is_unterminated.set(is_unterminated);
    }

    fn has_extended_unicode_escape(&self) -> Option<bool> {
        self.has_extended_unicode_escape.get()
    }

    fn set_has_extended_unicode_escape(&self, has_extended_unicode_escape: Option<bool>) {
        self.has_extended_unicode_escape
            .set(has_extended_unicode_escape);
    }
}

pub trait LiteralLikeNodeInterface {
    fn text(&self) -> &str;
    fn is_unterminated(&self) -> Option<bool>;
    fn set_is_unterminated(&self, is_unterminated: Option<bool>);
    fn has_extended_unicode_escape(&self) -> Option<bool>;
    fn set_has_extended_unicode_escape(&self, has_extended_unicode_escape: Option<bool>);
}

#[derive(Debug)]
#[ast_type(interfaces = "LiteralLikeNodeInterface")]
pub struct TemplateLiteralLikeNode {
    _literal_like_node: BaseLiteralLikeNode,
    pub raw_text: Option<String>,
    pub template_flags: Option<TokenFlags>,
}

impl TemplateLiteralLikeNode {
    pub fn new(
        literal_like_node: BaseLiteralLikeNode,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> Self {
        Self {
            _literal_like_node: literal_like_node,
            raw_text,
            template_flags,
        }
    }
}

impl TemplateLiteralLikeNodeInterface for TemplateLiteralLikeNode {
    fn maybe_raw_text(&self) -> Option<&str> {
        self.raw_text.as_deref()
    }

    fn maybe_template_flags(&self) -> Option<TokenFlags> {
        self.template_flags
    }
}

pub trait TemplateLiteralLikeNodeInterface {
    fn maybe_raw_text(&self) -> Option<&str>;
    fn maybe_template_flags(&self) -> Option<TokenFlags>;
}

#[derive(Debug)]
#[ast_type(interfaces = "LiteralLikeNodeInterface")]
pub struct RegularExpressionLiteral {
    _literal_like_node: BaseLiteralLikeNode,
}

impl RegularExpressionLiteral {
    pub fn new(base_literal_like_node: BaseLiteralLikeNode) -> Self {
        Self {
            _literal_like_node: base_literal_like_node,
        }
    }
}

bitflags! {
    pub struct TokenFlags: u32 {
        const None = 0;
        const PrecedingLineBreak = 1 << 0;
        const PrecedingJSDocComment = 1 << 1;
        const Unterminated = 1 << 2;
        const ExtendedUnicodeEscape = 1 << 3;
        const Scientific = 1 << 4;
        const Octal = 1 << 5;
        const HexSpecifier = 1 << 6;
        const BinarySpecifier = 1 << 7;
        const OctalSpecifier = 1 << 8;
        const ContainsSeparator = 1 << 9;
        const UnicodeEscape = 1 << 10;
        const ContainsInvalidEscape = 1 << 11;

        const BinaryOrOctalSpecifier = Self::BinarySpecifier.bits | Self::OctalSpecifier.bits;
        const NumericLiteralFlags = Self::Scientific.bits | Self::Octal.bits | Self::HexSpecifier.bits | Self::BinaryOrOctalSpecifier.bits | Self::ContainsSeparator.bits;
        const TemplateLiteralLikeFlags = Self::ContainsInvalidEscape.bits;
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "LiteralLikeNodeInterface")]
pub struct NumericLiteral {
    _literal_like_node: BaseLiteralLikeNode,
    pub(crate) numeric_literal_flags: TokenFlags,
}

impl NumericLiteral {
    pub fn new(
        base_literal_like_node: BaseLiteralLikeNode,
        numeric_literal_flags: TokenFlags,
    ) -> Self {
        Self {
            _literal_like_node: base_literal_like_node,
            numeric_literal_flags,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "LiteralLikeNodeInterface")]
pub struct BigIntLiteral {
    _literal_like_node: BaseLiteralLikeNode,
}

impl BigIntLiteral {
    pub fn new(base_literal_like_node: BaseLiteralLikeNode) -> Self {
        Self {
            _literal_like_node: base_literal_like_node,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TemplateExpression {
    _node: BaseNode,
    pub head: Rc<Node /*TemplateHead*/>,
    pub template_spans: NodeArray, /*<TemplateSpan>*/
}

impl TemplateExpression {
    pub fn new(base_node: BaseNode, head: Rc<Node>, template_spans: NodeArray) -> Self {
        Self {
            _node: base_node,
            head,
            template_spans,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TemplateSpan {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
    pub literal: Rc<Node /*TemplateMiddle | TemplateTail*/>,
}

impl TemplateSpan {
    pub fn new(base_node: BaseNode, expression: Rc<Node>, literal: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            literal,
        }
    }
}

pub trait HasExpressionInterface {
    fn expression(&self) -> Rc<Node>;
}

#[derive(Debug)]
#[ast_type]
pub struct ParenthesizedExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
}

impl ParenthesizedExpression {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ParenthesizedExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ArrayLiteralExpression {
    _node: BaseNode,
    pub elements: NodeArray, /*<Expression>*/
    pub(crate) multi_line: Option<bool>,
}

impl ArrayLiteralExpression {
    pub fn new(base_node: BaseNode, elements: NodeArray, multi_line: Option<bool>) -> Self {
        Self {
            _node: base_node,
            elements,
            multi_line,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct SpreadElement {
    _node: BaseNode,
    pub expression: Rc<Node /*<Expression>*/>,
}

impl SpreadElement {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for SpreadElement {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ObjectLiteralExpression {
    _node: BaseNode,
    pub properties: NodeArray, /*<ObjectLiteralElementLike>*/
    pub multi_line: Option<bool>,
}

impl ObjectLiteralExpression {
    pub fn new(base_node: BaseNode, properties: NodeArray, multi_line: Option<bool>) -> Self {
        Self {
            _node: base_node,
            properties,
            multi_line,
        }
    }
}

pub trait HasQuestionDotTokenInterface {
    fn maybe_question_dot_token(&self) -> Option<Rc<Node>>;
}

#[derive(Debug)]
#[ast_type]
pub struct PropertyAccessExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*LeftHandSideExpression*/>,
    pub question_dot_token: Option<Rc<Node /*QuestionDotToken*/>>,
    pub name: Rc<Node /*MemberName*/>,
}

impl PropertyAccessExpression {
    pub fn new(
        base_node: BaseNode,
        expression: Rc<Node>,
        question_dot_token: Option<Rc<Node>>,
        name: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            question_dot_token,
            name,
        }
    }
}

impl HasExpressionInterface for PropertyAccessExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

impl NamedDeclarationInterface for PropertyAccessExpression {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Rc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = name;
    }
}

impl HasQuestionDotTokenInterface for PropertyAccessExpression {
    fn maybe_question_dot_token(&self) -> Option<Rc<Node>> {
        self.question_dot_token.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ElementAccessExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*LeftHandSideExpression*/>,
    pub question_dot_token: Option<Rc<Node /*QuestionDotToken*/>>,
    pub argument_expression: Rc<Node /*Expression*/>,
}

impl ElementAccessExpression {
    pub fn new(
        base_node: BaseNode,
        expression: Rc<Node>,
        question_dot_token: Option<Rc<Node>>,
        argument_expression: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            question_dot_token,
            argument_expression,
        }
    }
}

impl HasExpressionInterface for ElementAccessExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

impl HasQuestionDotTokenInterface for ElementAccessExpression {
    fn maybe_question_dot_token(&self) -> Option<Rc<Node>> {
        self.question_dot_token.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct CallExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*LeftHandSideExpression*/>,
    pub question_dot_token: Option<Rc<Node /*QuestionDotToken*/>>,
    pub type_arguments: Option<NodeArray /*<TypeNode>*/>,
    pub arguments: NodeArray, /*<Expression>*/
}

impl CallExpression {
    pub fn new(
        base_node: BaseNode,
        expression: Rc<Node>,
        question_dot_token: Option<Rc<Node>>,
        type_arguments: Option<NodeArray>,
        arguments: NodeArray,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            question_dot_token,
            type_arguments,
            arguments,
        }
    }
}

impl HasExpressionInterface for CallExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

impl HasQuestionDotTokenInterface for CallExpression {
    fn maybe_question_dot_token(&self) -> Option<Rc<Node>> {
        self.question_dot_token.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ExpressionWithTypeArguments {
    _node: BaseNode,
    pub type_arguments: Option<NodeArray /*<TypeNode>*/>,
    pub expression: Rc<Node /*LeftHandSideExpression*/>,
}

impl ExpressionWithTypeArguments {
    pub fn new(
        base_node: BaseNode,
        expression: Rc<Node>,
        type_arguments: Option<NodeArray>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            type_arguments,
        }
    }
}

impl HasExpressionInterface for ExpressionWithTypeArguments {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct NewExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*LeftHandSideExpression*/>,
    pub type_arguments: Option<NodeArray /*<TypeNode>*/>,
    pub arguments: Option<NodeArray /*<Expression>*/>,
}

impl NewExpression {
    pub fn new(
        base_node: BaseNode,
        expression: Rc<Node>,
        type_arguments: Option<NodeArray>,
        arguments: Option<NodeArray>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            type_arguments,
            arguments,
        }
    }
}

impl HasExpressionInterface for NewExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TaggedTemplateExpression {
    _node: BaseNode,
    pub tag: Rc<Node /*LeftHandSideExpression*/>,
    pub type_arguments: Option<NodeArray /*<TypeNode>*/>,
    pub template: Rc<Node /*TemplateLiteral*/>,
    pub(crate) question_dot_token: Option<Rc<Node /*QuestionDotToken*/>>,
}

impl TaggedTemplateExpression {
    pub fn new(
        base_node: BaseNode,
        tag: Rc<Node>,
        type_arguments: Option<NodeArray>,
        template: Rc<Node>,
        question_dot_token: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            tag,
            type_arguments,
            template,
            question_dot_token,
        }
    }
}

impl HasTypeArgumentsInterface for TaggedTemplateExpression {
    fn maybe_type_arguments(&self) -> Option<&NodeArray> {
        self.type_arguments.as_ref()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct AsExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
    pub type_: Rc<Node /*TypeNode*/>,
}

impl AsExpression {
    pub fn new(base_node: BaseNode, expression: Rc<Node>, type_: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            type_,
        }
    }
}

impl HasExpressionInterface for AsExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TypeAssertion {
    _node: BaseNode,
    pub type_: Rc<Node /*TypeNode*/>,
    pub expression: Rc<Node /*UnaryExpression*/>,
}

impl TypeAssertion {
    pub fn new(base_node: BaseNode, expression: Rc<Node>, type_: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
            expression,
        }
    }
}

impl HasExpressionInterface for TypeAssertion {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct NonNullExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
}

impl NonNullExpression {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for NonNullExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}
