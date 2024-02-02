use std::cell::{Cell, Ref, RefCell};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;
use local_macros::ast_type;

use super::{
    BaseFunctionLikeDeclaration, BaseNode, HasConditionInterface, HasElementsInterface,
    HasQuestionTokenInterface, HasTypeArgumentsInterface, HasTypeInterface,
    NamedDeclarationInterface, Node, NodeArray, SyntaxKind,
};

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct BinaryExpression {
    pub _node: BaseNode,
    pub left: Id<Node>,
    pub operator_token: Id<Node>,
    pub right: Id<Node>,
    #[unsafe_ignore_trace]
    cached_literal_kind: Cell<Option<SyntaxKind>>,
}

impl BinaryExpression {
    pub fn new(
        base_node: BaseNode,
        left: Id<Node>,
        operator_token: Id<Node>,
        right: Id<Node>,
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

pub trait HasLeftAndRightInterface {
    fn left(&self) -> Id<Node>;
    fn right(&self) -> Id<Node>;
}

impl HasLeftAndRightInterface for BinaryExpression {
    fn left(&self) -> Id<Node> {
        self.left.clone()
    }

    fn right(&self) -> Id<Node> {
        self.right.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ConditionalExpression {
    _node: BaseNode,
    pub condition: Id<Node /*Expression*/>,
    pub question_token: Id<Node /*QuestionToken*/>,
    pub when_true: Id<Node /*Expression*/>,
    pub colon_token: Id<Node /*ColonToken*/>,
    pub when_false: Id<Node /*Expression*/>,
}

impl ConditionalExpression {
    pub fn new(
        base_node: BaseNode,
        condition: Id<Node /*Expression*/>,
        question_token: Id<Node /*QuestionToken*/>,
        when_true: Id<Node /*Expression*/>,
        colon_token: Id<Node /*ColonToken*/>,
        when_false: Id<Node /*Expression*/>,
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

impl HasConditionInterface for ConditionalExpression {
    fn maybe_condition(&self) -> Option<Id<Node>> {
        Some(self.condition.clone())
    }
}

impl HasQuestionTokenInterface for ConditionalExpression {
    fn maybe_question_token(&self) -> Option<Id<Node>> {
        Some(self.question_token.clone())
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface, HasQuestionTokenInterface"
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

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface, HasQuestionTokenInterface"
)]
pub struct ArrowFunction {
    _function_like_declaration: BaseFunctionLikeDeclaration,
    pub equals_greater_than_token: Id<Node /*EqualsGreaterThanToken*/>,
}

impl ArrowFunction {
    pub fn new(
        function_like_declaration: BaseFunctionLikeDeclaration,
        equals_greater_than_token: Id<Node>,
    ) -> Self {
        Self {
            _function_like_declaration: function_like_declaration,
            equals_greater_than_token,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(impl_from = false)]
pub struct BaseLiteralLikeNode {
    _node: BaseNode,
    #[unsafe_ignore_trace]
    text: RefCell<String>,
    #[unsafe_ignore_trace]
    is_unterminated: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    has_extended_unicode_escape: Cell<Option<bool>>,
}

impl BaseLiteralLikeNode {
    pub fn new(base_node: BaseNode, text: String) -> Self {
        Self {
            _node: base_node,
            text: RefCell::new(text),
            is_unterminated: Cell::new(None),
            has_extended_unicode_escape: Cell::new(None),
        }
    }
}

impl LiteralLikeNodeInterface for BaseLiteralLikeNode {
    fn text(&self) -> Ref<String> {
        self.text.borrow()
    }

    fn set_text(&self, text: String) {
        *self.text.borrow_mut() = text;
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
    fn text(&self) -> Ref<String>;
    fn set_text(&self, text: String);
    fn is_unterminated(&self) -> Option<bool>;
    fn set_is_unterminated(&self, is_unterminated: Option<bool>);
    fn has_extended_unicode_escape(&self) -> Option<bool>;
    fn set_has_extended_unicode_escape(&self, has_extended_unicode_escape: Option<bool>);
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "LiteralLikeNodeInterface")]
pub struct TemplateLiteralLikeNode {
    _literal_like_node: BaseLiteralLikeNode,
    pub raw_text: Option<String>,
    #[unsafe_ignore_trace]
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

pub trait TemplateLiteralLikeNodeInterface: LiteralLikeNodeInterface {
    fn maybe_raw_text(&self) -> Option<&str>;
    fn maybe_template_flags(&self) -> Option<TokenFlags>;
}

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "LiteralLikeNodeInterface")]
pub struct NumericLiteral {
    _literal_like_node: BaseLiteralLikeNode,
    #[unsafe_ignore_trace]
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

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TemplateExpression {
    _node: BaseNode,
    pub head: Id<Node /*TemplateHead*/>,
    pub template_spans: Id<NodeArray>, /*<TemplateSpan>*/
}

impl TemplateExpression {
    pub fn new(base_node: BaseNode, head: Id<Node>, template_spans: Id<NodeArray>) -> Self {
        Self {
            _node: base_node,
            head,
            template_spans,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TemplateSpan {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
    pub literal: Id<Node /*TemplateMiddle | TemplateTail*/>,
}

impl TemplateSpan {
    pub fn new(base_node: BaseNode, expression: Id<Node>, literal: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            literal,
        }
    }
}

impl HasExpressionInterface for TemplateSpan {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

pub trait HasExpressionInterface {
    fn expression(&self) -> Id<Node>;
    fn maybe_expression(&self) -> Option<Id<Node>> {
        Some(self.expression())
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ParenthesizedExpression {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
}

impl ParenthesizedExpression {
    pub fn new(base_node: BaseNode, expression: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ParenthesizedExpression {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ArrayLiteralExpression {
    _node: BaseNode,
    pub elements: Id<NodeArray>, /*<Expression>*/
    pub(crate) multi_line: Option<bool>,
}

impl ArrayLiteralExpression {
    pub fn new(base_node: BaseNode, elements: Id<NodeArray>, multi_line: Option<bool>) -> Self {
        Self {
            _node: base_node,
            elements,
            multi_line,
        }
    }
}

impl HasElementsInterface for ArrayLiteralExpression {
    fn elements(&self) -> Id<NodeArray> {
        self.elements.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct SpreadElement {
    _node: BaseNode,
    pub expression: Id<Node /*<Expression>*/>,
}

impl SpreadElement {
    pub fn new(base_node: BaseNode, expression: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for SpreadElement {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ObjectLiteralExpression {
    _node: BaseNode,
    pub properties: Id<NodeArray>, /*<ObjectLiteralElementLike>*/
    pub multi_line: Option<bool>,
}

impl ObjectLiteralExpression {
    pub fn new(base_node: BaseNode, properties: Id<NodeArray>, multi_line: Option<bool>) -> Self {
        Self {
            _node: base_node,
            properties,
            multi_line,
        }
    }
}

pub trait HasPropertiesInterface {
    fn properties(&self) -> Id<NodeArray>;
}

impl HasPropertiesInterface for ObjectLiteralExpression {
    fn properties(&self) -> Id<NodeArray> {
        self.properties.clone()
    }
}

pub trait HasQuestionDotTokenInterface {
    fn maybe_question_dot_token(&self) -> Option<Id<Node>>;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct PropertyAccessExpression {
    _node: BaseNode,
    pub expression: Id<Node /*LeftHandSideExpression*/>,
    pub question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
    pub name: Id<Node /*MemberName*/>,
}

impl PropertyAccessExpression {
    pub fn new(
        base_node: BaseNode,
        expression: Id<Node>,
        question_dot_token: Option<Id<Node>>,
        name: Id<Node>,
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
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

impl NamedDeclarationInterface for PropertyAccessExpression {
    fn maybe_name(&self) -> Option<Id<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Id<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Id<Node>) {
        self.name = name;
    }
}

impl HasQuestionDotTokenInterface for PropertyAccessExpression {
    fn maybe_question_dot_token(&self) -> Option<Id<Node>> {
        self.question_dot_token.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ElementAccessExpression {
    _node: BaseNode,
    pub expression: Id<Node /*LeftHandSideExpression*/>,
    pub question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
    pub argument_expression: Id<Node /*Expression*/>,
}

impl ElementAccessExpression {
    pub fn new(
        base_node: BaseNode,
        expression: Id<Node>,
        question_dot_token: Option<Id<Node>>,
        argument_expression: Id<Node>,
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
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

impl HasQuestionDotTokenInterface for ElementAccessExpression {
    fn maybe_question_dot_token(&self) -> Option<Id<Node>> {
        self.question_dot_token.clone()
    }
}

pub trait HasArgumentsInterface {
    fn maybe_arguments(&self) -> Option<Id<NodeArray>>;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct CallExpression {
    _node: BaseNode,
    pub expression: Id<Node /*LeftHandSideExpression*/>,
    pub question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
    type_arguments: Cell<Option<Id<NodeArray> /*<TypeNode>*/>>,
    pub arguments: Id<NodeArray>, /*<Expression>*/
}

impl CallExpression {
    pub fn new(
        base_node: BaseNode,
        expression: Id<Node>,
        question_dot_token: Option<Id<Node>>,
        type_arguments: Option<Id<NodeArray>>,
        arguments: Id<NodeArray>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            question_dot_token,
            type_arguments: Cell::new(type_arguments),
            arguments,
        }
    }
}

impl HasExpressionInterface for CallExpression {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

impl HasQuestionDotTokenInterface for CallExpression {
    fn maybe_question_dot_token(&self) -> Option<Id<Node>> {
        self.question_dot_token.clone()
    }
}

impl HasArgumentsInterface for CallExpression {
    fn maybe_arguments(&self) -> Option<Id<NodeArray>> {
        Some(self.arguments.clone())
    }
}

impl HasTypeArgumentsInterface for CallExpression {
    fn maybe_type_arguments(&self) -> Option<Id<NodeArray>> {
        self.type_arguments.get()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ExpressionWithTypeArguments {
    _node: BaseNode,
    type_arguments: Cell<Option<Id<NodeArray> /*<TypeNode>*/>>,
    pub expression: Id<Node /*LeftHandSideExpression*/>,
}

impl ExpressionWithTypeArguments {
    pub fn new(
        base_node: BaseNode,
        expression: Id<Node>,
        type_arguments: Option<Id<NodeArray>>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            type_arguments: Cell::new(type_arguments),
        }
    }
}

impl HasExpressionInterface for ExpressionWithTypeArguments {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

impl HasTypeArgumentsInterface for ExpressionWithTypeArguments {
    fn maybe_type_arguments(&self) -> Option<Id<NodeArray>> {
        self.type_arguments.get()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct NewExpression {
    _node: BaseNode,
    pub expression: Id<Node /*LeftHandSideExpression*/>,
    type_arguments: Cell<Option<Id<NodeArray> /*<TypeNode>*/>>,
    pub arguments: Option<Id<NodeArray> /*<Expression>*/>,
}

impl NewExpression {
    pub fn new(
        base_node: BaseNode,
        expression: Id<Node>,
        type_arguments: Option<Id<NodeArray>>,
        arguments: Option<Id<NodeArray>>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            type_arguments: Cell::new(type_arguments),
            arguments,
        }
    }
}

impl HasExpressionInterface for NewExpression {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

impl HasArgumentsInterface for NewExpression {
    fn maybe_arguments(&self) -> Option<Id<NodeArray>> {
        self.arguments.clone()
    }
}

impl HasTypeArgumentsInterface for NewExpression {
    fn maybe_type_arguments(&self) -> Option<Id<NodeArray>> {
        self.type_arguments.get()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TaggedTemplateExpression {
    _node: BaseNode,
    pub tag: Id<Node /*LeftHandSideExpression*/>,
    type_arguments: Cell<Option<Id<NodeArray> /*<TypeNode>*/>>,
    pub template: Id<Node /*TemplateLiteral*/>,
    pub(crate) question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
}

impl TaggedTemplateExpression {
    pub fn new(
        base_node: BaseNode,
        tag: Id<Node>,
        type_arguments: Option<Id<NodeArray>>,
        template: Id<Node>,
        question_dot_token: Option<Id<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            tag,
            type_arguments: Cell::new(type_arguments),
            template,
            question_dot_token,
        }
    }
}

impl HasTypeArgumentsInterface for TaggedTemplateExpression {
    fn maybe_type_arguments(&self) -> Option<Id<NodeArray>> {
        self.type_arguments.get()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct AsExpression {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
    pub type_: Id<Node /*TypeNode*/>,
}

impl AsExpression {
    pub fn new(base_node: BaseNode, expression: Id<Node>, type_: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            type_,
        }
    }
}

impl HasExpressionInterface for AsExpression {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

impl HasTypeInterface for AsExpression {
    fn maybe_type(&self) -> Option<Id<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Id<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TypeAssertion {
    _node: BaseNode,
    pub type_: Id<Node /*TypeNode*/>,
    pub expression: Id<Node /*UnaryExpression*/>,
}

impl TypeAssertion {
    pub fn new(base_node: BaseNode, expression: Id<Node>, type_: Id<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
            expression,
        }
    }
}

impl HasExpressionInterface for TypeAssertion {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

impl HasTypeInterface for TypeAssertion {
    fn maybe_type(&self) -> Option<Id<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Id<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct NonNullExpression {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
}

impl NonNullExpression {
    pub fn new(base_node: BaseNode, expression: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for NonNullExpression {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}
