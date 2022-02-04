#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::rc::Rc;

use super::{
    BaseBindingLikeDeclaration, BaseFunctionLikeDeclaration, BaseNamedDeclaration, BaseNode,
    BaseTextRange, BaseVariableLikeDeclaration, BindingLikeDeclarationInterface, Diagnostic,
    FunctionDeclaration, HasInitializerInterface, HasTypeArgumentsInterface, HasTypeInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, Path, StringLiteral, Symbol,
    SyntaxKind, TextRange, TypeCheckerHost, VariableLikeDeclarationInterface,
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

#[derive(Debug)]
#[ast_type]
pub struct MetaProperty {
    _node: BaseNode,
    pub keyword_token: SyntaxKind, /*SyntaxKind.NewKeyword | SyntaxKind.ImportKeyword*/
    pub name: Rc<Node /*Identifier*/>,
}

impl MetaProperty {
    pub fn new(base_node: BaseNode, keyword_token: SyntaxKind, name: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            keyword_token,
            name,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxAttribute {
    _node: BaseNode,
    pub name: Rc<Node /*Identifier*/>,
    pub initializer: Option<Rc<Node /*StringLiteral | JsxExpression*/>>,
}

impl JsxAttribute {
    pub fn new(base_node: BaseNode, name: Rc<Node>, initializer: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
            initializer,
        }
    }
}

impl NamedDeclarationInterface for JsxAttribute {
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

impl HasInitializerInterface for JsxAttribute {
    fn maybe_initializer(&self) -> Option<Rc<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Rc<Node>) {
        self.initializer = Some(initializer);
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxText {
    _node: BaseNode,
    pub text: String,
    pub is_unterminated: Cell<Option<bool>>,
    pub has_extended_unicode_escape: Cell<Option<bool>>,
    pub contains_only_trivia_white_spaces: bool,
}

impl JsxText {
    pub fn new(base_node: BaseNode, text: String, contains_only_trivia_white_spaces: bool) -> Self {
        Self {
            _node: base_node,
            text,
            is_unterminated: Cell::new(None),
            has_extended_unicode_escape: Cell::new(None),
            contains_only_trivia_white_spaces,
        }
    }
}

impl LiteralLikeNodeInterface for JsxText {
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

#[derive(Debug)]
#[ast_type]
pub struct EmptyStatement {
    _node: BaseNode,
}

impl EmptyStatement {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct DebuggerStatement {
    _node: BaseNode,
}

impl DebuggerStatement {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct Block {
    _node: BaseNode,
    pub statements: NodeArray, /*<Statement>*/
    pub(crate) multi_line: Option<bool>,
}

impl Block {
    pub fn new(base_node: BaseNode, statements: NodeArray, multi_line: Option<bool>) -> Self {
        Self {
            _node: base_node,
            statements,
            multi_line,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct VariableStatement {
    _node: BaseNode,
    pub declaration_list: Rc</*VariableDeclarationList*/ Node>,
}

impl VariableStatement {
    pub fn new(base_node: BaseNode, declaration_list: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            declaration_list,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ExpressionStatement {
    _node: BaseNode,
    pub expression: Rc</*Expression*/ Node>,
}

impl ExpressionStatement {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct IfStatement {
    _node: BaseNode,
    pub expression: Rc</*Expression*/ Node>,
    pub then_statement: Rc</*Statement*/ Node>,
    pub else_statement: Option<Rc</*Statement*/ Node>>,
}

impl IfStatement {
    pub fn new(
        base_node: BaseNode,
        expression: Rc<Node>,
        then_statement: Rc<Node>,
        else_statement: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            then_statement,
            else_statement,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct DoStatement {
    _node: BaseNode,
    pub statement: Rc</*Statement*/ Node>,
    pub expression: Rc</*Expression*/ Node>,
}

impl DoStatement {
    pub fn new(base_node: BaseNode, statement: Rc<Node>, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            statement,
            expression,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct WhileStatement {
    _node: BaseNode,
    pub statement: Rc</*Statement*/ Node>,
    pub expression: Rc</*Expression*/ Node>,
}

impl WhileStatement {
    pub fn new(base_node: BaseNode, expression: Rc<Node>, statement: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            statement,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ForStatement {
    _node: BaseNode,
    pub statement: Rc<Node /*Statement*/>,
    pub initializer: Option<Rc<Node /*ForInitializer*/>>,
    pub condition: Option<Rc<Node /*Expression*/>>,
    pub incrementor: Option<Rc<Node /*Expression*/>>,
}

impl ForStatement {
    pub fn new(
        base_node: BaseNode,
        initializer: Option<Rc<Node>>,
        condition: Option<Rc<Node>>,
        incrementor: Option<Rc<Node>>,
        statement: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            statement,
            initializer,
            condition,
            incrementor,
        }
    }
}

impl HasInitializerInterface for ForStatement {
    fn maybe_initializer(&self) -> Option<Rc<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Rc<Node>) {
        self.initializer = Some(initializer);
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ForInStatement {
    _node: BaseNode,
    pub statement: Rc<Node /*Statement*/>,
    pub initializer: Rc<Node /*ForInitializer*/>,
    pub expression: Rc<Node /*Expression*/>,
}

impl ForInStatement {
    pub fn new(
        base_node: BaseNode,
        initializer: Rc<Node>,
        expression: Rc<Node>,
        statement: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            statement,
            initializer,
            expression,
        }
    }
}

impl HasInitializerInterface for ForInStatement {
    fn maybe_initializer(&self) -> Option<Rc<Node>> {
        Some(self.initializer.clone())
    }

    fn set_initializer(&mut self, initializer: Rc<Node>) {
        self.initializer = initializer;
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ForOfStatement {
    _node: BaseNode,
    pub await_modifier: Option<Rc<Node /*AwaitKeywordToken*/>>,
    pub statement: Rc<Node /*Statement*/>,
    pub initializer: Rc<Node /*ForInitializer*/>,
    pub expression: Rc<Node /*Expression*/>,
}

impl ForOfStatement {
    pub fn new(
        base_node: BaseNode,
        await_modifier: Option<Rc<Node>>,
        initializer: Rc<Node>,
        expression: Rc<Node>,
        statement: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            await_modifier,
            statement,
            initializer,
            expression,
        }
    }
}

impl HasInitializerInterface for ForOfStatement {
    fn maybe_initializer(&self) -> Option<Rc<Node>> {
        Some(self.initializer.clone())
    }

    fn set_initializer(&mut self, initializer: Rc<Node>) {
        self.initializer = initializer;
    }
}

#[derive(Debug)]
#[ast_type]
pub struct BreakStatement {
    _node: BaseNode,
    pub label: Option<Rc<Node /*Identifier*/>>,
}

impl BreakStatement {
    pub fn new(base_node: BaseNode, label: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            label,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ContinueStatement {
    _node: BaseNode,
    pub label: Option<Rc<Node /*Identifier*/>>,
}

impl ContinueStatement {
    pub fn new(base_node: BaseNode, label: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            label,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ReturnStatement {
    _node: BaseNode,
    pub expression: Option<Rc</*Expression*/ Node>>,
}

impl ReturnStatement {
    pub fn new(base_node: BaseNode, expression: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct WithStatement {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
    pub statement: Rc<Node /*Statement*/>,
}

impl WithStatement {
    pub fn new(base_node: BaseNode, expression: Rc<Node>, statement: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            statement,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct SwitchStatement {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
    pub case_block: Rc<Node /*CaseBlock*/>,
    pub possibly_exhaustive: Option<bool>,
}

impl SwitchStatement {
    pub fn new(base_node: BaseNode, expression: Rc<Node>, case_block: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            case_block,
            possibly_exhaustive: None,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct CaseBlock {
    _node: BaseNode,
    pub clauses: NodeArray, /*<CaseOrDefaultClause>*/
}

impl CaseBlock {
    pub fn new(base_node: BaseNode, clauses: NodeArray) -> Self {
        Self {
            _node: base_node,
            clauses,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct LabeledStatement {
    _node: BaseNode,
    pub label: Rc<Node /*Identifier*/>,
    pub statement: Rc<Node /*Statement*/>,
}

impl LabeledStatement {
    pub fn new(base_node: BaseNode, label: Rc<Node>, statement: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            label,
            statement,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ThrowStatement {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
}

impl ThrowStatement {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ThrowStatement {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TryStatement {
    _node: BaseNode,
    pub try_block: Rc<Node /*Block*/>,
    pub catch_clause: Option<Rc<Node /*CatchClause*/>>,
    pub finally_block: Option<Rc<Node /*Block*/>>,
}

impl TryStatement {
    pub fn new(
        base_node: BaseNode,
        try_block: Rc<Node>,
        catch_clause: Option<Rc<Node>>,
        finally_block: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            try_block,
            catch_clause,
            finally_block,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub enum TypeElement {
    PropertySignature(PropertySignature),
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasInitializerInterface, BindingLikeDeclarationInterface"
)]
pub struct BindingElement {
    _binding_like_declaration: BaseBindingLikeDeclaration, /*name: BindingName*/
    pub property_name: Option<Rc<Node /*PropertyName*/>>,
    pub dot_dot_dot_token: Option<Rc<Node /*DotDotDotToken*/>>,
}

impl BindingElement {
    pub fn new(
        base_binding_like_declaration: BaseBindingLikeDeclaration,
        property_name: Option<Rc<Node>>,
        dot_dot_dot_token: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _binding_like_declaration: base_binding_like_declaration,
            property_name,
            dot_dot_dot_token,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeElement", interfaces = "NamedDeclarationInterface")]
pub struct PropertySignature {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub question_token: Option<Rc<Node /*QuestionToken*/>>,
    pub type_: Option<Rc<Node /*TypeNode*/>>,
    pub initializer: Option<Rc<Node /*Expression*/>>,
}

impl PropertySignature {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        question_token: Option<Rc<Node>>,
        type_: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            question_token,
            type_,
            initializer: None,
        }
    }
}

impl HasTypeInterface for PropertySignature {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = Some(type_);
    }
}

impl HasInitializerInterface for PropertySignature {
    fn maybe_initializer(&self) -> Option<Rc<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Rc<Node>) {
        self.initializer = Some(initializer);
    }
}

impl BindingLikeDeclarationInterface for PropertySignature {}

impl VariableLikeDeclarationInterface for PropertySignature {}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasInitializerInterface, BindingLikeDeclarationInterface, HasTypeInterface, VariableLikeDeclarationInterface"
)]
pub struct PropertyDeclaration {
    _variable_like_declaration: BaseVariableLikeDeclaration,
    pub question_token: Option<Rc<Node /*QuestionToken*/>>,
    pub exclamation_token: Option<Rc<Node /*ExclamationToken*/>>,
}

impl PropertyDeclaration {
    pub fn new(
        base_variable_like_declaration: BaseVariableLikeDeclaration,
        question_token: Option<Rc<Node>>,
        exclamation_token: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _variable_like_declaration: base_variable_like_declaration,
            question_token,
            exclamation_token,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct PropertyAssignment {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub initializer: Rc<Node /*Expression*/>,
}

impl PropertyAssignment {
    pub fn new(base_named_declaration: BaseNamedDeclaration, initializer: Rc<Node>) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            initializer,
        }
    }
}

impl HasInitializerInterface for PropertyAssignment {
    fn maybe_initializer(&self) -> Option<Rc<Node>> {
        Some(self.initializer.clone())
    }

    fn set_initializer(&mut self, initializer: Rc<Node>) {
        self.initializer = initializer;
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct ShorthandPropertyAssignment {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub question_token: Option<Rc<Node /*QuestionToken*/>>,
    pub exclamation_token: Option<Rc<Node /*ExclamationToken*/>>,
    pub equals_token: Option<Rc<Node /*EqualsToken*/>>,
    pub object_assignment_initializer: Option<Rc<Node /*Expression*/>>,
}

impl ShorthandPropertyAssignment {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        object_assignment_initializer: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            question_token: None,
            exclamation_token: None,
            equals_token: None,
            object_assignment_initializer,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct SpreadAssignment {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
}

// TODO: should implement NamedDeclarationInterface for SpreadAssignment since it extends
// NamedDeclaration (even though it appears to never have a populated name field?
impl SpreadAssignment {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for SpreadAssignment {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

pub trait HasElementsInterface: NodeInterface {
    fn elements(&self) -> &NodeArray;
}

#[derive(Debug)]
#[ast_type]
pub struct ObjectBindingPattern {
    _node: BaseNode,
    pub elements: NodeArray, /*<BindingElement>*/
}

impl ObjectBindingPattern {
    pub fn new(base_node: BaseNode, elements: NodeArray) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

impl HasElementsInterface for ObjectBindingPattern {
    fn elements(&self) -> &NodeArray {
        &self.elements
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ArrayBindingPattern {
    _node: BaseNode,
    pub elements: NodeArray, /*<ArrayBindingElement>*/
}

impl ArrayBindingPattern {
    pub fn new(base_node: BaseNode, elements: NodeArray) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

impl HasElementsInterface for ArrayBindingPattern {
    fn elements(&self) -> &NodeArray {
        &self.elements
    }
}

pub trait HasTypeParametersInterface {
    fn maybe_type_parameters(&self) -> Option<&NodeArray>;
}

pub trait GenericNamedDeclarationInterface:
    NamedDeclarationInterface + HasTypeParametersInterface
{
}

#[derive(Debug)]
#[ast_type(impl_from = false, interfaces = "NamedDeclarationInterface")]
pub struct BaseGenericNamedDeclaration {
    _named_declaration: BaseNamedDeclaration,
    pub type_parameters: Option<NodeArray /*<TypeParameterDeclaration>*/>,
}

impl BaseGenericNamedDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        type_parameters: Option<NodeArray>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            type_parameters,
        }
    }
}

impl HasTypeParametersInterface for BaseGenericNamedDeclaration {
    fn maybe_type_parameters(&self) -> Option<&NodeArray> {
        self.type_parameters.as_ref()
    }
}

impl GenericNamedDeclarationInterface for BaseGenericNamedDeclaration {}

pub trait InterfaceOrClassLikeDeclarationInterface {
    fn maybe_heritage_clauses(&self) -> Option<&NodeArray>;
}

#[derive(Debug)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct BaseInterfaceOrClassLikeDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration,
    heritage_clauses: Option<NodeArray /*<HeritageClause>*/>,
}

impl BaseInterfaceOrClassLikeDeclaration {
    pub fn new(
        base_generic_named_declaration: BaseGenericNamedDeclaration,
        heritage_clauses: Option<NodeArray>,
    ) -> Self {
        Self {
            _generic_named_declaration: base_generic_named_declaration,
            heritage_clauses,
        }
    }
}

impl InterfaceOrClassLikeDeclarationInterface for BaseInterfaceOrClassLikeDeclaration {
    fn maybe_heritage_clauses(&self) -> Option<&NodeArray> {
        self.heritage_clauses.as_ref()
    }
}

pub trait ClassLikeDeclarationInterface {
    fn members(&self) -> &NodeArray;
}

#[derive(Debug)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, InterfaceOrClassLikeDeclarationInterface"
)]
pub struct ClassLikeDeclarationBase {
    _interface_or_class_like_declaration: BaseInterfaceOrClassLikeDeclaration,
    members: NodeArray, /*<ClassElement>*/
}

impl ClassLikeDeclarationBase {
    pub fn new(
        base_interface_or_class_like_declaration: BaseInterfaceOrClassLikeDeclaration,
        members: NodeArray,
    ) -> Self {
        Self {
            _interface_or_class_like_declaration: base_interface_or_class_like_declaration,
            members,
        }
    }
}

impl ClassLikeDeclarationInterface for ClassLikeDeclarationBase {
    fn members(&self) -> &NodeArray {
        &self.members
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, InterfaceOrClassLikeDeclarationInterface, ClassLikeDeclarationInterface"
)]
pub struct ClassDeclaration {
    _class_like_declaration: ClassLikeDeclarationBase,
}

impl ClassDeclaration {
    pub fn new(base_class_like_declaration: ClassLikeDeclarationBase) -> Self {
        Self {
            _class_like_declaration: base_class_like_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, InterfaceOrClassLikeDeclarationInterface, ClassLikeDeclarationInterface"
)]
pub struct ClassExpression {
    _class_like_declaration: ClassLikeDeclarationBase,
}

impl ClassExpression {
    pub fn new(base_class_like_declaration: ClassLikeDeclarationBase) -> Self {
        Self {
            _class_like_declaration: base_class_like_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, InterfaceOrClassLikeDeclarationInterface"
)]
pub struct InterfaceDeclaration {
    _interface_or_class_like_declaration: BaseInterfaceOrClassLikeDeclaration, /*name: Identifier*/
    pub members: NodeArray,                                                    /*<TypeElement>*/
}

impl InterfaceDeclaration {
    pub fn new(
        base_interface_or_class_like_declaration: BaseInterfaceOrClassLikeDeclaration,
        members: NodeArray,
    ) -> Self {
        Self {
            _interface_or_class_like_declaration: base_interface_or_class_like_declaration,
            members,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct TypeAliasDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration, /*name: Identifier*/
    pub type_: Rc<Node /*TypeNode*/>,
}

impl TypeAliasDeclaration {
    pub fn new(
        base_generic_named_declaration: BaseGenericNamedDeclaration,
        type_: Rc<Node>,
    ) -> Self {
        Self {
            _generic_named_declaration: base_generic_named_declaration,
            type_,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct EnumMember {
    _node: BaseNode,
    pub name: Rc<Node /*PropertyName*/>,
    pub initializer: Option<Rc<Node /*Expression*/>>,
}

impl EnumMember {
    pub fn new(base_node: BaseNode, name: Rc<Node>, initializer: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
            initializer,
        }
    }
}

impl NamedDeclarationInterface for EnumMember {
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

impl HasInitializerInterface for EnumMember {
    fn maybe_initializer(&self) -> Option<Rc<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Rc<Node>) {
        self.initializer = Some(initializer);
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct EnumDeclaration {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub members: NodeArray,                   /*<EnumMember>*/
}

impl EnumDeclaration {
    pub fn new(base_named_declaration: BaseNamedDeclaration, members: NodeArray) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            members,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ModuleDeclaration {
    _node: BaseNode,
    pub name: Rc<Node /*ModuleName*/>,
    pub body: Option<Rc<Node /*ModuleBody | JSDocNamespaceDeclaration*/>>,
}

impl ModuleDeclaration {
    pub fn new(base_node: BaseNode, name: Rc<Node>, body: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
            body,
        }
    }
}

impl NamedDeclarationInterface for ModuleDeclaration {
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

#[derive(Debug)]
#[ast_type]
pub struct ModuleBlock {
    _node: BaseNode,
    pub statements: NodeArray, /*<Statement>*/
}

impl ModuleBlock {
    pub fn new(base_node: BaseNode, statements: NodeArray) -> Self {
        Self {
            _node: base_node,
            statements,
        }
    }
}

pub trait HasIsTypeOnlyInterface {
    fn is_type_only(&self) -> bool;
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct ImportEqualsDeclaration {
    _named_declaration: BaseNamedDeclaration,
    pub is_type_only: bool,
    pub module_reference: Rc<Node /*ModuleReference*/>,
}

impl ImportEqualsDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        is_type_only: bool,
        module_reference: Rc<Node>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            is_type_only,
            module_reference,
        }
    }
}

impl HasIsTypeOnlyInterface for ImportEqualsDeclaration {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ImportClause {
    _node: BaseNode,
    pub is_type_only: bool,
    pub name: Option<Rc<Node /*Identifier*/>>,
    pub named_bindings: Option<Rc<Node /*NamedImportBindings*/>>,
}

impl ImportClause {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        name: Option<Rc<Node>>,
        named_bindings: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            is_type_only,
            name,
            named_bindings,
        }
    }
}

impl NamedDeclarationInterface for ImportClause {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Rc<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = Some(name);
    }
}

impl HasIsTypeOnlyInterface for ImportClause {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ExportDeclaration {
    _node: BaseNode,
    pub is_type_only: bool,
    pub export_clause: Option<Rc<Node /*NamedExportBindings*/>>,
    pub module_specifier: Option<Rc<Node /*Expression*/>>,
    pub assert_clause: Option<Rc<Node /*AssertClause*/>>,
}

impl ExportDeclaration {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        export_clause: Option<Rc<Node>>,
        module_specifier: Option<Rc<Node>>,
        assert_clause: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            is_type_only,
            export_clause,
            module_specifier,
            assert_clause,
        }
    }
}

impl HasIsTypeOnlyInterface for ExportDeclaration {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ImportSpecifier {
    _node: BaseNode,
    pub property_name: Option<Rc<Node /*Identifier*/>>,
    pub name: Rc<Node /*Identifier*/>,
    pub is_type_only: bool,
}

impl ImportSpecifier {
    pub fn new(
        base_node: BaseNode,
        property_name: Option<Rc<Node>>,
        name: Rc<Node>,
        is_type_only: bool,
    ) -> Self {
        Self {
            _node: base_node,
            property_name,
            name,
            is_type_only,
        }
    }
}

impl NamedDeclarationInterface for ImportSpecifier {
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

impl HasIsTypeOnlyInterface for ImportSpecifier {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ExportSpecifier {
    _node: BaseNode,
    pub is_type_only: bool,
    pub property_name: Option<Rc<Node /*Identifier*/>>,
    pub name: Rc<Node /*Identifier*/>,
}

impl ExportSpecifier {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        property_name: Option<Rc<Node>>,
        name: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            is_type_only,
            property_name,
            name,
        }
    }
}

impl NamedDeclarationInterface for ExportSpecifier {
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

impl HasIsTypeOnlyInterface for ExportSpecifier {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ExportAssignment {
    _node: BaseNode,
    pub is_export_equals: Option<bool>,
    pub expression: Rc<Node /*Expression*/>,
}

// TODO: should implement HasExpressionInterface for ExportAssignment?
impl ExportAssignment {
    pub fn new(base_node: BaseNode, is_export_equals: Option<bool>, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            is_export_equals,
            expression,
        }
    }
}

pub type CommentKind = SyntaxKind; /*SyntaxKind.SingleLineCommentTrivia | SyntaxKind.MultiLineCommentTrivia*/

pub struct CommentRange {
    pos: Cell<isize>,
    end: Cell<isize>,
    has_trailing_new_line: Option<bool>,
    kind: CommentKind,
}

impl CommentRange {
    pub fn new(
        kind: CommentKind,
        pos: isize,
        end: isize,
        has_trailing_new_line: Option<bool>,
    ) -> Self {
        Self {
            kind,
            pos: Cell::new(pos),
            end: Cell::new(end),
            has_trailing_new_line,
        }
    }
}

impl TextRange for CommentRange {
    fn pos(&self) -> isize {
        self.pos.get()
    }

    fn set_pos(&self, pos: isize) {
        self.pos.set(pos);
    }

    fn end(&self) -> isize {
        self.end.get()
    }

    fn set_end(&self, end: isize) {
        self.end.set(end);
    }
}

// TODO: should eg implement a CommentRangeInterface for CommentRange + SynthesizedComment?
#[derive(Debug)]
pub struct SynthesizedComment {
    has_trailing_new_line: Option<bool>,
    kind: CommentKind,
    text: String,
    has_leading_new_line: Option<bool>,
}

impl TextRange for SynthesizedComment {
    fn pos(&self) -> isize {
        -1
    }

    fn set_pos(&self, pos: isize) {
        panic!("Shouldn't call set_pos() on a SynthesizedComment")
    }

    fn end(&self) -> isize {
        -1
    }

    fn set_end(&self, end: isize) {
        panic!("Shouldn't call set_end() on a SynthesizedComment")
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocTypeExpression {
    _node: BaseNode,
    pub type_: Rc<Node /*TypeNode*/>,
}

impl JSDocTypeExpression {
    pub fn new(base_node: BaseNode, type_: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for JSDocTypeExpression {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = type_;
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocMemberName {
    _node: BaseNode,
    pub left: Rc<Node /*EntityName | JSDocMemberName*/>,
    pub right: Rc<Node /*Identifier*/>,
}

impl JSDocMemberName {
    pub fn new(base_node: BaseNode, left: Rc<Node>, right: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            left,
            right,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct BaseJSDocUnaryType {
    _node: BaseNode,
    pub type_: Option<Rc<Node /*TypeNode*/>>,
}

impl BaseJSDocUnaryType {
    pub fn new(base_node: BaseNode, type_: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for BaseJSDocUnaryType {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = Some(type_);
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDoc {
    _node: BaseNode,
    pub tags: Option<NodeArray /*<JSDocTag>*/>,
    pub comment: Option<StringOrNodeArray /*<JSDocComment>*/>,
}

impl JSDoc {
    pub fn new(
        base_node: BaseNode,
        tags: Option<NodeArray>,
        comment: Option<StringOrNodeArray>,
    ) -> Self {
        Self {
            _node: base_node,
            tags,
            comment,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub enum JSDocTag {
    BaseJSDocTag(BaseJSDocTag),
    JSDocAugmentsTag(JSDocAugmentsTag),
    JSDocImplementsTag(JSDocImplementsTag),
    BaseJSDocTypeLikeTag(BaseJSDocTypeLikeTag),
    JSDocTemplateTag(JSDocTemplateTag),
    JSDocSeeTag(JSDocSeeTag),
    JSDocTypedefTag(JSDocTypedefTag),
    JSDocCallbackTag(JSDocCallbackTag),
    JSDocPropertyLikeTag(JSDocPropertyLikeTag),
}

pub trait JSDocTagInterface {
    fn tag_name(&self) -> Rc<Node /*Identifier*/>;
    fn maybe_comment(&self) -> Option<&StringOrNodeArray /*<JSDocComment>*/>;
}

pub trait JSDocLinkLikeInterface {
    fn maybe_name(&self) -> Option<Rc<Node>>;
    fn text(&self) -> &str;
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocLink {
    _node: BaseNode,
    pub name: Option<Rc<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLink {
    pub fn new(base_node: BaseNode, name: Option<Rc<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLink {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn text(&self) -> &str {
        &self.text
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocLinkCode {
    _node: BaseNode,
    pub name: Option<Rc<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLinkCode {
    pub fn new(base_node: BaseNode, name: Option<Rc<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLinkCode {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn text(&self) -> &str {
        &self.text
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocLinkPlain {
    _node: BaseNode,
    pub name: Option<Rc<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLinkPlain {
    pub fn new(base_node: BaseNode, name: Option<Rc<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLinkPlain {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn text(&self) -> &str {
        &self.text
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocText {
    _node: BaseNode,
    pub text: String,
}

impl JSDocText {
    pub fn new(base_node: BaseNode, text: String) -> Self {
        Self {
            _node: base_node,
            text,
        }
    }
}

#[derive(Debug)]
pub enum StringOrNodeArray {
    String(String),
    NodeArray(NodeArray),
}

impl From<String> for StringOrNodeArray {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<NodeArray> for StringOrNodeArray {
    fn from(value: NodeArray) -> Self {
        Self::NodeArray(value)
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "JSDocTag")]
pub struct BaseJSDocTag {
    _node: BaseNode,
    tag_name: Rc<Node /*Identifier*/>,
    comment: Option<StringOrNodeArray /*<JSDocComment>*/>,
}

impl BaseJSDocTag {
    pub fn new(
        base_node: BaseNode,
        tag_name: Rc<Node>,
        comment: Option<StringOrNodeArray>,
    ) -> Self {
        Self {
            _node: base_node,
            tag_name,
            comment,
        }
    }
}

impl JSDocTagInterface for BaseJSDocTag {
    fn tag_name(&self) -> Rc<Node> {
        self.tag_name.clone()
    }

    fn maybe_comment(&self) -> Option<&StringOrNodeArray> {
        self.comment.as_ref()
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "JSDocTag", interfaces = "JSDocTagInterface")]
pub struct JSDocAugmentsTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub class: Rc<
        Node, /*ExpressionWithTypeArguments & { readonly expression: Identifier | PropertyAccessEntityNameExpression*/
    >,
}

impl JSDocAugmentsTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, class: Rc<Node>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            class,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "JSDocTag", interfaces = "JSDocTagInterface")]
pub struct JSDocImplementsTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub class: Rc<
        Node, /*ExpressionWithTypeArguments & { readonly expression: Identifier | PropertyAccessEntityNameExpression*/
    >,
}

impl JSDocImplementsTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, class: Rc<Node>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            class,
        }
    }
}

pub trait JSDocTypeLikeTagInterface: JSDocTagInterface {
    fn maybe_type_expression(&self) -> Option<Rc<Node>>;
    fn type_expression(&self) -> Rc<Node>;
}

#[derive(Debug)]
#[ast_type(ancestors = "JSDocTag", interfaces = "JSDocTagInterface")]
pub struct BaseJSDocTypeLikeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
}

impl BaseJSDocTypeLikeTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, type_expression: Option<Rc<Node>>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            type_expression,
        }
    }
}

impl JSDocTypeLikeTagInterface for BaseJSDocTypeLikeTag {
    fn maybe_type_expression(&self) -> Option<Rc<Node>> {
        self.type_expression.clone()
    }

    fn type_expression(&self) -> Rc<Node> {
        self.type_expression.clone().unwrap()
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "JSDocTag", interfaces = "JSDocTagInterface")]
pub struct JSDocTemplateTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub constraint: Option<Rc<Node /*JSDocTypeExpression*/>>,
    pub type_parameters: NodeArray, /*<TypeParameterDeclaration>*/
}

impl JSDocTemplateTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        constraint: Option<Rc<Node>>,
        type_parameters: NodeArray,
    ) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            constraint,
            type_parameters,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "JSDocTag", interfaces = "JSDocTagInterface")]
pub struct JSDocSeeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub name: Option<Rc<Node /*JSDocNameReference*/>>,
}

impl JSDocSeeTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, name: Option<Rc<Node>>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            name,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "JSDocTag", interfaces = "JSDocTagInterface")]
pub struct JSDocTypedefTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub full_name: Option<Rc<Node /*JSDocNamespaceDeclaration | Identifier*/>>,
    pub name: Option<Rc<Node /*Identifier*/>>,
    pub type_expression: Option<Rc<Node /*JSDocTypeExpression | JSDocTypeLiteral*/>>,
}

impl JSDocTypedefTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        full_name: Option<Rc<Node>>,
        name: Option<Rc<Node>>,
        type_expression: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            full_name,
            name,
            type_expression,
        }
    }
}

impl NamedDeclarationInterface for JSDocTypedefTag {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Rc<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = Some(name);
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "JSDocTag", interfaces = "JSDocTagInterface")]
pub struct JSDocCallbackTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub full_name: Option<Rc<Node /*JSDocNamespaceDeclaration | Identifier*/>>,
    pub name: Option<Rc<Node /*Identifier*/>>,
    pub type_expression: Rc<Node /*JSDocSignature*/>,
}

impl JSDocCallbackTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        full_name: Option<Rc<Node>>,
        name: Option<Rc<Node>>,
        type_expression: Rc<Node>,
    ) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            full_name,
            name,
            type_expression,
        }
    }
}

impl NamedDeclarationInterface for JSDocCallbackTag {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Rc<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = Some(name);
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "JSDocTag", interfaces = "JSDocTagInterface")]
pub struct JSDocPropertyLikeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub name: Rc<Node /*EntityName*/>,
    pub type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
    pub is_name_first: bool,
    pub is_bracketed: bool,
}

impl JSDocPropertyLikeTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        name: Rc<Node>,
        type_expression: Option<Rc<Node>>,
        is_name_first: bool,
        is_bracketed: bool,
    ) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            name,
            type_expression,
            is_name_first,
            is_bracketed,
        }
    }
}

impl NamedDeclarationInterface for JSDocPropertyLikeTag {
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

pub type FlowNode = ();

pub type SourceTextAsChars = Vec<char>;

pub fn str_to_source_text_as_chars(str_: &str) -> SourceTextAsChars {
    str_.chars().collect()
}

pub fn text_len(text: &SourceTextAsChars) -> usize {
    text.len()
}

pub fn maybe_text_char_at_index(text: &SourceTextAsChars, index: usize) -> Option<char> {
    text.get(index).map(|ch| *ch)
}

pub fn text_char_at_index(text: &SourceTextAsChars, index: usize) -> char {
    maybe_text_char_at_index(text, index).unwrap()
}

pub fn text_substring(text: &SourceTextAsChars, start: usize, end: usize) -> String {
    text[start..end].into_iter().collect()
}

pub fn text_str_num_chars(text: &str, start: usize, end: usize) -> usize {
    text[start..end].chars().count()
}

pub trait SourceFileLike {
    fn text(&self) -> &str;
    fn text_as_chars(&self) -> &SourceTextAsChars;
    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>>;
    fn line_map(&self) -> Ref<Vec<usize>>;
    fn maybe_get_position_of_line_and_character(
        &self,
        line: usize,
        character: usize,
        allow_edits: Option<bool>,
    ) -> Option<usize>;
}

#[derive(Debug)]
#[ast_type]
pub struct SourceFile {
    _node: BaseNode,
    _symbols_without_a_symbol_table_strong_references: RefCell<Vec<Rc<Symbol>>>,
    pub statements: NodeArray,

    file_name: RefCell<String>,
    path: RefCell<Option<Path>>,
    pub text: String,
    pub text_as_chars: SourceTextAsChars,

    parse_diagnostics: RefCell<Option<Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>>>,

    line_map: RefCell<Option<Vec<usize>>>,
}

impl SourceFile {
    pub fn new(
        base_node: BaseNode,
        statements: NodeArray,
        file_name: String,
        text: String,
    ) -> Self {
        let text_as_chars = text.chars().collect();
        Self {
            _node: base_node,
            _symbols_without_a_symbol_table_strong_references: RefCell::new(vec![]),
            statements,
            file_name: RefCell::new(file_name),
            path: RefCell::new(None),
            text,
            text_as_chars,
            parse_diagnostics: RefCell::new(None),
            line_map: RefCell::new(None),
        }
    }

    pub fn file_name(&self) -> Ref<String> {
        self.file_name.borrow()
    }

    pub fn set_file_name(&self, file_name: String) {
        *self.file_name.borrow_mut() = file_name;
    }

    pub fn maybe_path(&self) -> Ref<Option<Path>> {
        self.path.borrow()
    }

    pub fn set_path(&self, path: Path) {
        *self.path.borrow_mut() = Some(path);
    }

    pub fn parse_diagnostics(&self) -> Ref<Vec<Rc<Diagnostic>>> {
        Ref::map(self.parse_diagnostics.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_parse_diagnostics(&self, parse_diagnostics: Vec<Rc<Diagnostic>>) {
        *self.parse_diagnostics.borrow_mut() = Some(parse_diagnostics);
    }

    pub fn keep_strong_reference_to_symbol(&self, symbol: Rc<Symbol>) {
        self._symbols_without_a_symbol_table_strong_references
            .borrow_mut()
            .push(symbol);
    }
}

impl SourceFileLike for SourceFile {
    fn text(&self) -> &str {
        &self.text
    }

    fn text_as_chars(&self) -> &SourceTextAsChars {
        &self.text_as_chars
    }

    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>> {
        self.line_map.borrow_mut()
    }

    fn line_map(&self) -> Ref<Vec<usize>> {
        Ref::map(self.line_map.borrow(), |line_map| {
            line_map.as_ref().unwrap()
        })
    }

    fn maybe_get_position_of_line_and_character(
        &self,
        line: usize,
        character: usize,
        allow_edits: Option<bool>,
    ) -> Option<usize> {
        None
    }
}

#[derive(Clone, Debug)]
pub struct CommentDirective {
    pub range: BaseTextRange,
    pub type_: CommentDirectiveType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CommentDirectiveType {
    ExpectError,
    Ignore,
}

pub trait Program: TypeCheckerHost {
    fn get_syntactic_diagnostics(&mut self) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>;
    fn get_semantic_diagnostics(&mut self) -> Vec<Rc<Diagnostic>>;
}
