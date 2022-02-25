#![allow(non_upper_case_globals)]

use std::cell::{Cell, Ref, RefCell, RefMut};
use std::rc::Rc;

use super::{
    BaseBindingLikeDeclaration, BaseNamedDeclaration, BaseNode, BaseVariableLikeDeclaration,
    BindingLikeDeclarationInterface, FlowNode, HasExpressionInterface, HasInitializerInterface,
    HasStatementsInterface, HasTypeInterface, LiteralLikeNodeInterface, NamedDeclarationInterface,
    Node, NodeArray, NodeInterface, SyntaxKind, VariableLikeDeclarationInterface,
};
use local_macros::ast_type;

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
pub struct JsxElement {
    _node: BaseNode,
    pub opening_element: Rc<Node /*JsxOpeningElement*/>,
    pub children: NodeArray, /*<JsxChild>*/
    pub closing_element: Rc<Node /*JsxClosingElement*/>,
}

impl JsxElement {
    pub fn new(
        base_node: BaseNode,
        opening_element: Rc<Node>,
        children: NodeArray,
        closing_element: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            opening_element,
            children,
            closing_element,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxAttributes {
    _node: BaseNode,
    pub properties: NodeArray, /*<JsxAttributeLike>*/
}

impl JsxAttributes {
    pub fn new(base_node: BaseNode, properties: NodeArray) -> Self {
        Self {
            _node: base_node,
            properties,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxOpeningElement {
    _node: BaseNode,
    pub tag_name: Rc<Node /*JsxTagNameExpression*/>,
    pub type_arguments: Option<NodeArray /*<TypeNode>*/>,
    pub attributes: Rc<Node /*JsxAttributes*/>,
}

impl JsxOpeningElement {
    pub fn new(
        base_node: BaseNode,
        tag_name: Rc<Node>,
        type_arguments: Option<NodeArray>,
        attributes: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            tag_name,
            type_arguments,
            attributes,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxSelfClosingElement {
    _node: BaseNode,
    pub tag_name: Rc<Node /*JsxTagNameExpression*/>,
    pub type_arguments: Option<NodeArray /*<TypeNode>*/>,
    pub attributes: Rc<Node /*JsxAttributes*/>,
}

impl JsxSelfClosingElement {
    pub fn new(
        base_node: BaseNode,
        tag_name: Rc<Node>,
        type_arguments: Option<NodeArray>,
        attributes: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            tag_name,
            type_arguments,
            attributes,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxFragment {
    _node: BaseNode,
    pub opening_fragment: Rc<Node /*JsxOpeningFragment*/>,
    pub children: NodeArray, /*<JsxChild>*/
    pub closing_fragment: Rc<Node /*JsxClosingFragment*/>,
}

impl JsxFragment {
    pub fn new(
        base_node: BaseNode,
        opening_fragment: Rc<Node>,
        children: NodeArray,
        closing_fragment: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            opening_fragment,
            children,
            closing_fragment,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxOpeningFragment {
    _node: BaseNode,
}

impl JsxOpeningFragment {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxClosingFragment {
    _node: BaseNode,
}

impl JsxClosingFragment {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
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
pub struct JsxSpreadAttribute {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
}

impl JsxSpreadAttribute {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for JsxSpreadAttribute {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxClosingElement {
    _node: BaseNode,
    pub tag_name: Rc<Node /*JsxTagNameExpression*/>,
}

impl JsxClosingElement {
    pub fn new(base_node: BaseNode, tag_name: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            tag_name,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxExpression {
    _node: BaseNode,
    pub dot_dot_dot_token: Option<Rc<Node /*Token<SyntaxKind.DotDotDotToken>*/>>,
    pub expression: Option<Rc<Node /*Expression*/>>,
}

impl JsxExpression {
    pub fn new(
        base_node: BaseNode,
        dot_dot_dot_token: Option<Rc<Node>>,
        expression: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            dot_dot_dot_token,
            expression,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxText {
    _node: BaseNode,
    text: RefCell<String>,
    pub is_unterminated: Cell<Option<bool>>,
    pub has_extended_unicode_escape: Cell<Option<bool>>,
    pub contains_only_trivia_white_spaces: bool,
}

impl JsxText {
    pub fn new(base_node: BaseNode, text: String, contains_only_trivia_white_spaces: bool) -> Self {
        Self {
            _node: base_node,
            text: RefCell::new(text),
            is_unterminated: Cell::new(None),
            has_extended_unicode_escape: Cell::new(None),
            contains_only_trivia_white_spaces,
        }
    }
}

impl LiteralLikeNodeInterface for JsxText {
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

#[derive(Debug)]
#[ast_type]
pub struct CommaListExpression {
    _node: BaseNode,
    pub elements: NodeArray, /*<Expression>*/
}

impl CommaListExpression {
    pub fn new(base_node: BaseNode, elements: NodeArray) -> Self {
        Self {
            _node: base_node,
            elements,
        }
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
pub struct MissingDeclaration {
    _node: BaseNode,
}

impl MissingDeclaration {
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

impl HasStatementsInterface for Block {
    fn statements(&self) -> &[Rc<Node>] {
        &self.statements
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

pub trait HasConditionInterface {
    fn maybe_condition(&self) -> Option<Rc<Node>>;
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

impl HasConditionInterface for ForStatement {
    fn maybe_condition(&self) -> Option<Rc<Node>> {
        self.condition.clone()
    }
}

pub trait HasStatementInterface {
    fn statement(&self) -> Rc<Node>;
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

impl HasStatementInterface for ForInStatement {
    fn statement(&self) -> Rc<Node> {
        self.statement.clone()
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

impl HasStatementInterface for ForOfStatement {
    fn statement(&self) -> Rc<Node> {
        self.statement.clone()
    }
}

pub trait HasLabelInterface {
    fn maybe_label(&self) -> Option<Rc<Node>>;
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

impl HasLabelInterface for BreakStatement {
    fn maybe_label(&self) -> Option<Rc<Node>> {
        self.label.clone()
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

impl HasLabelInterface for ContinueStatement {
    fn maybe_label(&self) -> Option<Rc<Node>> {
        self.label.clone()
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
    possibly_exhaustive: Cell<Option<bool>>,
}

impl SwitchStatement {
    pub fn new(base_node: BaseNode, expression: Rc<Node>, case_block: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            case_block,
            possibly_exhaustive: Cell::new(None),
        }
    }

    pub fn set_possibly_exhaustive(&self, possibly_exhaustive: Option<bool>) {
        self.possibly_exhaustive.set(possibly_exhaustive);
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
pub struct CaseClause {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
    pub statements: NodeArray, /*<Statement>*/
    fallthrough_flow_node: RefCell<Option<Rc<FlowNode>>>,
}

impl CaseClause {
    pub fn new(base_node: BaseNode, expression: Rc<Node>, statements: NodeArray) -> Self {
        Self {
            _node: base_node,
            expression,
            statements,
            fallthrough_flow_node: RefCell::new(None),
        }
    }

    pub fn set_fallthrough_flow_node(&self, fallthrough_flow_node: Option<Rc<FlowNode>>) {
        *self.fallthrough_flow_node.borrow_mut() = fallthrough_flow_node;
    }
}

#[derive(Debug)]
#[ast_type]
pub struct DefaultClause {
    _node: BaseNode,
    pub statements: NodeArray, /*<Statement>*/
    pub(crate) fallthrough_flow_node: Option<FlowNode>,
}

impl DefaultClause {
    pub fn new(base_node: BaseNode, statements: NodeArray) -> Self {
        Self {
            _node: base_node,
            statements,
            fallthrough_flow_node: None,
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
#[ast_type]
pub struct CatchClause {
    _node: BaseNode,
    pub variable_declaration: Option<Rc<Node /*VariableDeclaration*/>>,
    pub block: Rc<Node /*Block*/>,
}

impl CatchClause {
    pub fn new(
        base_node: BaseNode,
        variable_declaration: Option<Rc<Node>>,
        block: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            variable_declaration,
            block,
        }
    }
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

pub trait HasQuestionTokenInterface {
    fn maybe_question_token(&self) -> Option<Rc<Node>>;
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
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

    fn set_type(&mut self, type_: Option<Rc<Node>>) {
        self.type_ = type_;
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

impl HasQuestionTokenInterface for PropertySignature {
    fn maybe_question_token(&self) -> Option<Rc<Node>> {
        self.question_token.clone()
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

impl HasQuestionTokenInterface for PropertyDeclaration {
    fn maybe_question_token(&self) -> Option<Rc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct PropertyAssignment {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub initializer: Rc<Node /*Expression*/>,
    pub question_token: Option<Rc<Node /*QuestionToken*/>>,
    pub exclamation_token: Option<Rc<Node /*ExclamationToken*/>>,
}

impl PropertyAssignment {
    pub fn new(base_named_declaration: BaseNamedDeclaration, initializer: Rc<Node>) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            initializer,
            question_token: None,
            exclamation_token: None,
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
    fn maybe_type_parameters(&self) -> RefMut<Option<NodeArray>>;
}

pub trait GenericNamedDeclarationInterface:
    NamedDeclarationInterface + HasTypeParametersInterface
{
}

#[derive(Debug)]
#[ast_type(impl_from = false, interfaces = "NamedDeclarationInterface")]
pub struct BaseGenericNamedDeclaration {
    _named_declaration: BaseNamedDeclaration,
    type_parameters: RefCell<Option<NodeArray /*<TypeParameterDeclaration>*/>>,
}

impl BaseGenericNamedDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        type_parameters: Option<NodeArray>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            type_parameters: RefCell::new(type_parameters),
        }
    }
}

impl HasTypeParametersInterface for BaseGenericNamedDeclaration {
    fn maybe_type_parameters(&self) -> RefMut<Option<NodeArray>> {
        self.type_parameters.borrow_mut()
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
#[ast_type]
pub struct HeritageClause {
    _node: BaseNode,
    pub token: SyntaxKind, /*SyntaxKind.ExtendsKeyword | SyntaxKind.ImplementsKeyword*/
    pub types: NodeArray,  /*<ExpressionWithTypeArguments>*/
}

impl HeritageClause {
    pub fn new(base_node: BaseNode, token: SyntaxKind, types: NodeArray) -> Self {
        Self {
            _node: base_node,
            token,
            types,
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
pub struct JSDocNamespaceDeclaration {
    _node: BaseNode,
    pub name: Rc<Node /*Identifier*/>,
    pub body: Option<Rc<Node /*JSDocNamespaceBody*/>>,
}

impl JSDocNamespaceDeclaration {
    pub fn new(base_node: BaseNode, name: Rc<Node>, body: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
            body,
        }
    }
}

impl NamedDeclarationInterface for JSDocNamespaceDeclaration {
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

impl HasStatementsInterface for ModuleBlock {
    fn statements(&self) -> &[Rc<Node>] {
        &self.statements
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
