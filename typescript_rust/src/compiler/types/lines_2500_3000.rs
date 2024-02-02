use std::cell::{Cell, Ref, RefCell};

use id_arena::Id;
use local_macros::ast_type;

use super::{
    BaseBindingLikeDeclaration, BaseNamedDeclaration, BaseNode, BaseVariableLikeDeclaration,
    BindingLikeDeclarationInterface, FlowNode, HasExpressionInterface, HasInitializerInterface,
    HasMembersInterface, HasPropertiesInterface, HasPropertyNameInterface, HasStatementsInterface,
    HasTypeArgumentsInterface, HasTypeInterface, LiteralLikeNodeInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, SyntaxKind,
    VariableLikeDeclarationInterface,
};
use crate::HasDotDotDotTokenInterface;

#[derive(Debug)]
#[ast_type]
pub struct MetaProperty {
    _node: BaseNode,
    pub keyword_token: SyntaxKind, /*SyntaxKind.NewKeyword | SyntaxKind.ImportKeyword*/
    pub name: Id<Node /*Identifier*/>,
}

impl MetaProperty {
    pub fn new(base_node: BaseNode, keyword_token: SyntaxKind, name: Id<Node>) -> Self {
        Self {
            _node: base_node,
            keyword_token,
            name,
        }
    }
}

pub trait HasChildrenInterface {
    fn children(&self) -> Id<NodeArray>;
}

#[derive(Debug)]
#[ast_type]
pub struct JsxElement {
    _node: BaseNode,
    pub opening_element: Id<Node /*JsxOpeningElement*/>,
    pub children: Id<NodeArray>, /*<JsxChild>*/
    pub closing_element: Id<Node /*JsxClosingElement*/>,
}

impl JsxElement {
    pub fn new(
        base_node: BaseNode,
        opening_element: Id<Node>,
        children: Id<NodeArray>,
        closing_element: Id<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            opening_element,
            children,
            closing_element,
        }
    }
}

impl HasChildrenInterface for JsxElement {
    fn children(&self) -> Id<NodeArray> {
        self.children.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxAttributes {
    _node: BaseNode,
    pub properties: Id<NodeArray>, /*<JsxAttributeLike>*/
}

impl JsxAttributes {
    pub fn new(base_node: BaseNode, properties: Id<NodeArray>) -> Self {
        Self {
            _node: base_node,
            properties,
        }
    }
}

impl HasPropertiesInterface for JsxAttributes {
    fn properties(&self) -> Id<NodeArray> {
        self.properties.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxOpeningElement {
    _node: BaseNode,
    pub tag_name: Id<Node /*JsxTagNameExpression*/>,
    type_arguments: Cell<Option<Id<NodeArray> /*<TypeNode>*/>>,
    pub attributes: Id<Node /*JsxAttributes*/>,
}

impl JsxOpeningElement {
    pub fn new(
        base_node: BaseNode,
        tag_name: Id<Node>,
        type_arguments: Option<Id<NodeArray>>,
        attributes: Id<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            tag_name,
            type_arguments: Cell::new(type_arguments),
            attributes,
        }
    }
}

pub trait HasTagNameInterface {
    fn tag_name(&self) -> Id<Node>;
}

pub trait JsxOpeningLikeElementInterface: HasTagNameInterface + HasTypeArgumentsInterface {
    fn attributes(&self) -> Id<Node>;
}

impl HasTagNameInterface for JsxOpeningElement {
    fn tag_name(&self) -> Id<Node> {
        self.tag_name.clone()
    }
}

impl JsxOpeningLikeElementInterface for JsxOpeningElement {
    fn attributes(&self) -> Id<Node> {
        self.attributes.clone()
    }
}

impl HasTypeArgumentsInterface for JsxOpeningElement {
    fn maybe_type_arguments(&self) -> Option<Id<NodeArray>> {
        self.type_arguments.get()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxSelfClosingElement {
    _node: BaseNode,
    pub tag_name: Id<Node /*JsxTagNameExpression*/>,
    type_arguments: Cell<Option<Id<NodeArray> /*<TypeNode>*/>>,
    pub attributes: Id<Node /*JsxAttributes*/>,
}

impl JsxSelfClosingElement {
    pub fn new(
        base_node: BaseNode,
        tag_name: Id<Node>,
        type_arguments: Option<Id<NodeArray>>,
        attributes: Id<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            tag_name,
            type_arguments: Cell::new(type_arguments),
            attributes,
        }
    }
}

impl HasTagNameInterface for JsxSelfClosingElement {
    fn tag_name(&self) -> Id<Node> {
        self.tag_name.clone()
    }
}

impl JsxOpeningLikeElementInterface for JsxSelfClosingElement {
    fn attributes(&self) -> Id<Node> {
        self.attributes.clone()
    }
}

impl HasTypeArgumentsInterface for JsxSelfClosingElement {
    fn maybe_type_arguments(&self) -> Option<Id<NodeArray>> {
        self.type_arguments.get()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxFragment {
    _node: BaseNode,
    pub opening_fragment: Id<Node /*JsxOpeningFragment*/>,
    pub children: Id<NodeArray>, /*<JsxChild>*/
    pub closing_fragment: Id<Node /*JsxClosingFragment*/>,
}

impl JsxFragment {
    pub fn new(
        base_node: BaseNode,
        opening_fragment: Id<Node>,
        children: Id<NodeArray>,
        closing_fragment: Id<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            opening_fragment,
            children,
            closing_fragment,
        }
    }
}

impl HasChildrenInterface for JsxFragment {
    fn children(&self) -> Id<NodeArray> {
        self.children.clone()
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
    pub name: Id<Node /*Identifier*/>,
    pub initializer: Option<Id<Node /*StringLiteral | JsxExpression*/>>,
}

impl JsxAttribute {
    pub fn new(base_node: BaseNode, name: Id<Node>, initializer: Option<Id<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
            initializer,
        }
    }
}

impl NamedDeclarationInterface for JsxAttribute {
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

impl HasInitializerInterface for JsxAttribute {
    fn maybe_initializer(&self) -> Option<Id<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Id<Node>) {
        self.initializer = Some(initializer);
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxSpreadAttribute {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
}

impl JsxSpreadAttribute {
    pub fn new(base_node: BaseNode, expression: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for JsxSpreadAttribute {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxClosingElement {
    _node: BaseNode,
    pub tag_name: Id<Node /*JsxTagNameExpression*/>,
}

impl JsxClosingElement {
    pub fn new(base_node: BaseNode, tag_name: Id<Node>) -> Self {
        Self {
            _node: base_node,
            tag_name,
        }
    }
}

impl HasTagNameInterface for JsxClosingElement {
    fn tag_name(&self) -> Id<Node> {
        self.tag_name.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxExpression {
    _node: BaseNode,
    pub dot_dot_dot_token: Option<Id<Node /*Token<SyntaxKind.DotDotDotToken>*/>>,
    pub expression: Option<Id<Node /*Expression*/>>,
}

impl JsxExpression {
    pub fn new(
        base_node: BaseNode,
        dot_dot_dot_token: Option<Id<Node>>,
        expression: Option<Id<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            dot_dot_dot_token,
            expression,
        }
    }
}

impl HasExpressionInterface for JsxExpression {
    fn expression(&self) -> Id<Node> {
        self.expression.clone().unwrap()
    }

    fn maybe_expression(&self) -> Option<Id<Node>> {
        self.expression.clone()
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
    pub elements: Id<NodeArray>, /*<Expression>*/
}

impl CommaListExpression {
    pub fn new(base_node: BaseNode, elements: Id<NodeArray>) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

impl HasElementsInterface for CommaListExpression {
    fn elements(&self) -> Id<NodeArray> {
        self.elements.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct SyntheticReferenceExpression {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
    pub this_arg: Id<Node /*Expression*/>,
}

impl SyntheticReferenceExpression {
    pub fn new(
        base_node: BaseNode,
        expression: Id<Node /*Expression*/>,
        this_arg: Id<Node /*Expression*/>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            this_arg,
        }
    }
}

impl HasExpressionInterface for SyntheticReferenceExpression {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
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
    pub statements: Id<NodeArray>, /*<Statement>*/
    pub(crate) multi_line: Option<bool>,
}

impl Block {
    pub fn new(base_node: BaseNode, statements: Id<NodeArray>, multi_line: Option<bool>) -> Self {
        Self {
            _node: base_node,
            statements,
            multi_line,
        }
    }
}

impl HasStatementsInterface for Block {
    fn statements(&self) -> Id<NodeArray> {
        self.statements.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct VariableStatement {
    _node: BaseNode,
    pub declaration_list: Id<Node /*VariableDeclarationList*/>,
}

impl VariableStatement {
    pub fn new(base_node: BaseNode, declaration_list: Id<Node>) -> Self {
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
    pub expression: Id<Node /*Expression*/>,
}

impl ExpressionStatement {
    pub fn new(base_node: BaseNode, expression: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ExpressionStatement {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct IfStatement {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
    pub then_statement: Id<Node /*Statement*/>,
    pub else_statement: Option<Id<Node /*Statement*/>>,
}

impl IfStatement {
    pub fn new(
        base_node: BaseNode,
        expression: Id<Node>,
        then_statement: Id<Node>,
        else_statement: Option<Id<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            then_statement,
            else_statement,
        }
    }
}

impl HasExpressionInterface for IfStatement {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct DoStatement {
    _node: BaseNode,
    pub statement: Id<Node /*Statement*/>,
    pub expression: Id<Node /*Expression*/>,
}

impl DoStatement {
    pub fn new(base_node: BaseNode, statement: Id<Node>, expression: Id<Node>) -> Self {
        Self {
            _node: base_node,
            statement,
            expression,
        }
    }
}

impl HasStatementInterface for DoStatement {
    fn statement(&self) -> Id<Node> {
        self.statement.clone()
    }
}

impl HasExpressionInterface for DoStatement {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct WhileStatement {
    _node: BaseNode,
    pub statement: Id<Node /*Statement*/>,
    pub expression: Id<Node /*Expression*/>,
}

impl WhileStatement {
    pub fn new(base_node: BaseNode, expression: Id<Node>, statement: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            statement,
        }
    }
}

impl HasExpressionInterface for WhileStatement {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

impl HasStatementInterface for WhileStatement {
    fn statement(&self) -> Id<Node> {
        self.statement.clone()
    }
}

pub trait HasConditionInterface {
    fn maybe_condition(&self) -> Option<Id<Node>>;
}

#[derive(Debug)]
#[ast_type]
pub struct ForStatement {
    _node: BaseNode,
    pub statement: Id<Node /*Statement*/>,
    pub initializer: Option<Id<Node /*ForInitializer*/>>,
    pub condition: Option<Id<Node /*Expression*/>>,
    pub incrementor: Option<Id<Node /*Expression*/>>,
}

impl ForStatement {
    pub fn new(
        base_node: BaseNode,
        initializer: Option<Id<Node>>,
        condition: Option<Id<Node>>,
        incrementor: Option<Id<Node>>,
        statement: Id<Node>,
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
    fn maybe_initializer(&self) -> Option<Id<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Id<Node>) {
        self.initializer = Some(initializer);
    }
}

impl HasConditionInterface for ForStatement {
    fn maybe_condition(&self) -> Option<Id<Node>> {
        self.condition.clone()
    }
}

impl HasStatementInterface for ForStatement {
    fn statement(&self) -> Id<Node> {
        self.statement.clone()
    }
}

pub trait HasStatementInterface {
    fn statement(&self) -> Id<Node>;
}

#[derive(Debug)]
#[ast_type]
pub struct ForInStatement {
    _node: BaseNode,
    pub statement: Id<Node /*Statement*/>,
    pub initializer: Id<Node /*ForInitializer*/>,
    pub expression: Id<Node /*Expression*/>,
}

impl ForInStatement {
    pub fn new(
        base_node: BaseNode,
        initializer: Id<Node>,
        expression: Id<Node>,
        statement: Id<Node>,
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
    fn maybe_initializer(&self) -> Option<Id<Node>> {
        Some(self.initializer.clone())
    }

    fn set_initializer(&mut self, initializer: Id<Node>) {
        self.initializer = initializer;
    }
}

impl HasStatementInterface for ForInStatement {
    fn statement(&self) -> Id<Node> {
        self.statement.clone()
    }
}

impl HasExpressionInterface for ForInStatement {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ForOfStatement {
    _node: BaseNode,
    pub await_modifier: Option<Id<Node /*AwaitKeywordToken*/>>,
    pub statement: Id<Node /*Statement*/>,
    pub initializer: Id<Node /*ForInitializer*/>,
    pub expression: Id<Node /*Expression*/>,
}

impl ForOfStatement {
    pub fn new(
        base_node: BaseNode,
        await_modifier: Option<Id<Node>>,
        initializer: Id<Node>,
        expression: Id<Node>,
        statement: Id<Node>,
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
    fn maybe_initializer(&self) -> Option<Id<Node>> {
        Some(self.initializer.clone())
    }

    fn set_initializer(&mut self, initializer: Id<Node>) {
        self.initializer = initializer;
    }
}

impl HasStatementInterface for ForOfStatement {
    fn statement(&self) -> Id<Node> {
        self.statement.clone()
    }
}

impl HasExpressionInterface for ForOfStatement {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

pub trait HasLabelInterface {
    fn maybe_label(&self) -> Option<Id<Node>>;
}

#[derive(Debug)]
#[ast_type]
pub struct BreakStatement {
    _node: BaseNode,
    pub label: Option<Id<Node /*Identifier*/>>,
}

impl BreakStatement {
    pub fn new(base_node: BaseNode, label: Option<Id<Node>>) -> Self {
        Self {
            _node: base_node,
            label,
        }
    }
}

impl HasLabelInterface for BreakStatement {
    fn maybe_label(&self) -> Option<Id<Node>> {
        self.label.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ContinueStatement {
    _node: BaseNode,
    pub label: Option<Id<Node /*Identifier*/>>,
}

impl ContinueStatement {
    pub fn new(base_node: BaseNode, label: Option<Id<Node>>) -> Self {
        Self {
            _node: base_node,
            label,
        }
    }
}

impl HasLabelInterface for ContinueStatement {
    fn maybe_label(&self) -> Option<Id<Node>> {
        self.label.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ReturnStatement {
    _node: BaseNode,
    pub expression: Option<Id<Node /*Expression*/>>,
}

impl ReturnStatement {
    pub fn new(base_node: BaseNode, expression: Option<Id<Node>>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ReturnStatement {
    fn expression(&self) -> Id<Node> {
        self.expression.clone().unwrap()
    }

    fn maybe_expression(&self) -> Option<Id<Node>> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct WithStatement {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
    pub statement: Id<Node /*Statement*/>,
}

impl WithStatement {
    pub fn new(base_node: BaseNode, expression: Id<Node>, statement: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            statement,
        }
    }
}

impl HasExpressionInterface for WithStatement {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

impl HasStatementInterface for WithStatement {
    fn statement(&self) -> Id<Node> {
        self.statement.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct SwitchStatement {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
    pub case_block: Id<Node /*CaseBlock*/>,
    possibly_exhaustive: Cell<Option<bool>>,
}

impl SwitchStatement {
    pub fn new(base_node: BaseNode, expression: Id<Node>, case_block: Id<Node>) -> Self {
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

impl HasExpressionInterface for SwitchStatement {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct CaseBlock {
    _node: BaseNode,
    pub clauses: Id<NodeArray>, /*<CaseOrDefaultClause>*/
}

impl CaseBlock {
    pub fn new(base_node: BaseNode, clauses: Id<NodeArray>) -> Self {
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
    pub expression: Id<Node /*Expression*/>,
    pub statements: Id<NodeArray>, /*<Statement>*/
    fallthrough_flow_node: Cell<Option<Id<FlowNode>>>,
}

impl CaseClause {
    pub fn new(base_node: BaseNode, expression: Id<Node>, statements: Id<NodeArray>) -> Self {
        Self {
            _node: base_node,
            expression,
            statements,
            fallthrough_flow_node: Default::default(),
        }
    }
}

impl HasStatementsInterface for CaseClause {
    fn statements(&self) -> Id<NodeArray> {
        self.statements.clone()
    }
}

pub trait CaseOrDefaultClauseInterface: HasStatementsInterface {
    fn maybe_fallthrough_flow_node(&self) -> Option<Id<FlowNode>>;
    fn set_fallthrough_flow_node(&self, fallthrough_flow_node: Option<Id<FlowNode>>);
}

impl CaseOrDefaultClauseInterface for CaseClause {
    fn maybe_fallthrough_flow_node(&self) -> Option<Id<FlowNode>> {
        self.fallthrough_flow_node.get()
    }

    fn set_fallthrough_flow_node(&self, fallthrough_flow_node: Option<Id<FlowNode>>) {
        self.fallthrough_flow_node.set(fallthrough_flow_node);
    }
}

impl HasExpressionInterface for CaseClause {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct DefaultClause {
    _node: BaseNode,
    pub statements: Id<NodeArray>, /*<Statement>*/
    fallthrough_flow_node: Cell<Option<Id<FlowNode>>>,
}

impl DefaultClause {
    pub fn new(base_node: BaseNode, statements: Id<NodeArray>) -> Self {
        Self {
            _node: base_node,
            statements,
            fallthrough_flow_node: Default::default(),
        }
    }
}

impl HasStatementsInterface for DefaultClause {
    fn statements(&self) -> Id<NodeArray> {
        self.statements.clone()
    }
}

impl CaseOrDefaultClauseInterface for DefaultClause {
    fn maybe_fallthrough_flow_node(&self) -> Option<Id<FlowNode>> {
        self.fallthrough_flow_node.get()
    }

    fn set_fallthrough_flow_node(&self, fallthrough_flow_node: Option<Id<FlowNode>>) {
        self.fallthrough_flow_node.set(fallthrough_flow_node);
    }
}

#[derive(Debug)]
#[ast_type]
pub struct LabeledStatement {
    _node: BaseNode,
    pub label: Id<Node /*Identifier*/>,
    pub statement: Id<Node /*Statement*/>,
}

impl LabeledStatement {
    pub fn new(base_node: BaseNode, label: Id<Node>, statement: Id<Node>) -> Self {
        Self {
            _node: base_node,
            label,
            statement,
        }
    }
}

impl HasStatementInterface for LabeledStatement {
    fn statement(&self) -> Id<Node> {
        self.statement.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ThrowStatement {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
}

impl ThrowStatement {
    pub fn new(base_node: BaseNode, expression: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ThrowStatement {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TryStatement {
    _node: BaseNode,
    pub try_block: Id<Node /*Block*/>,
    pub catch_clause: Option<Id<Node /*CatchClause*/>>,
    pub finally_block: Option<Id<Node /*Block*/>>,
}

impl TryStatement {
    pub fn new(
        base_node: BaseNode,
        try_block: Id<Node>,
        catch_clause: Option<Id<Node>>,
        finally_block: Option<Id<Node>>,
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
    pub variable_declaration: Option<Id<Node /*VariableDeclaration*/>>,
    pub block: Id<Node /*Block*/>,
}

impl CatchClause {
    pub fn new(
        base_node: BaseNode,
        variable_declaration: Option<Id<Node>>,
        block: Id<Node>,
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
    pub property_name: Option<Id<Node /*PropertyName*/>>,
    pub dot_dot_dot_token: Option<Id<Node /*DotDotDotToken*/>>,
}

impl BindingElement {
    pub fn new(
        base_binding_like_declaration: BaseBindingLikeDeclaration,
        property_name: Option<Id<Node>>,
        dot_dot_dot_token: Option<Id<Node>>,
    ) -> Self {
        Self {
            _binding_like_declaration: base_binding_like_declaration,
            property_name,
            dot_dot_dot_token,
        }
    }
}

impl HasPropertyNameInterface for BindingElement {
    fn maybe_property_name(&self) -> Option<Id<Node>> {
        self.property_name.clone()
    }
}

impl HasDotDotDotTokenInterface for BindingElement {
    fn maybe_dot_dot_dot_token(&self) -> Option<Id<Node>> {
        self.dot_dot_dot_token.clone()
    }
}

pub trait HasQuestionTokenInterface {
    fn maybe_question_token(&self) -> Option<Id<Node>>;
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct PropertySignature {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub question_token: Option<Id<Node /*QuestionToken*/>>,
    pub type_: Option<Id<Node /*TypeNode*/>>,
    pub initializer: Option<Id<Node /*Expression*/>>,
}

impl PropertySignature {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        question_token: Option<Id<Node>>,
        type_: Option<Id<Node>>,
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
    fn maybe_type(&self) -> Option<Id<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Option<Id<Node>>) {
        self.type_ = type_;
    }
}

impl HasInitializerInterface for PropertySignature {
    fn maybe_initializer(&self) -> Option<Id<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Id<Node>) {
        self.initializer = Some(initializer);
    }
}

impl HasQuestionTokenInterface for PropertySignature {
    fn maybe_question_token(&self) -> Option<Id<Node>> {
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
    pub question_token: Option<Id<Node /*QuestionToken*/>>,
    pub exclamation_token: Option<Id<Node /*ExclamationToken*/>>,
}

impl PropertyDeclaration {
    pub fn new(
        base_variable_like_declaration: BaseVariableLikeDeclaration,
        question_token: Option<Id<Node>>,
        exclamation_token: Option<Id<Node>>,
    ) -> Self {
        Self {
            _variable_like_declaration: base_variable_like_declaration,
            question_token,
            exclamation_token,
        }
    }
}

impl HasQuestionTokenInterface for PropertyDeclaration {
    fn maybe_question_token(&self) -> Option<Id<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct PropertyAssignment {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub initializer: Id<Node /*Expression*/>,
    pub question_token: Option<Id<Node /*QuestionToken*/>>,
    pub exclamation_token: Option<Id<Node /*ExclamationToken*/>>,
}

impl PropertyAssignment {
    pub fn new(base_named_declaration: BaseNamedDeclaration, initializer: Id<Node>) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            initializer,
            question_token: None,
            exclamation_token: None,
        }
    }
}

impl HasInitializerInterface for PropertyAssignment {
    fn maybe_initializer(&self) -> Option<Id<Node>> {
        Some(self.initializer.clone())
    }

    fn set_initializer(&mut self, initializer: Id<Node>) {
        self.initializer = initializer;
    }
}

impl HasQuestionTokenInterface for PropertyAssignment {
    fn maybe_question_token(&self) -> Option<Id<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct ShorthandPropertyAssignment {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub question_token: Option<Id<Node /*QuestionToken*/>>,
    pub exclamation_token: Option<Id<Node /*ExclamationToken*/>>,
    pub equals_token: Option<Id<Node /*EqualsToken*/>>,
    pub object_assignment_initializer: Option<Id<Node /*Expression*/>>,
}

impl ShorthandPropertyAssignment {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        object_assignment_initializer: Option<Id<Node>>,
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

impl HasQuestionTokenInterface for ShorthandPropertyAssignment {
    fn maybe_question_token(&self) -> Option<Id<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct SpreadAssignment {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
}

impl SpreadAssignment {
    pub fn new(base_node: BaseNode, expression: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for SpreadAssignment {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

impl NamedDeclarationInterface for SpreadAssignment {
    fn maybe_name(&self) -> Option<Id<Node>> {
        None
    }

    fn name(&self) -> Id<Node> {
        unreachable!()
    }

    fn set_name(&mut self, _name: Id<Node>) {
        unreachable!()
    }
}

pub trait HasElementsInterface: NodeInterface {
    fn elements(&self) -> Id<NodeArray>;
}

#[derive(Debug)]
#[ast_type]
pub struct ObjectBindingPattern {
    _node: BaseNode,
    pub elements: Id<NodeArray>, /*<BindingElement>*/
}

impl ObjectBindingPattern {
    pub fn new(base_node: BaseNode, elements: Id<NodeArray>) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

impl HasElementsInterface for ObjectBindingPattern {
    fn elements(&self) -> Id<NodeArray> {
        self.elements.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ArrayBindingPattern {
    _node: BaseNode,
    pub elements: Id<NodeArray>, /*<ArrayBindingElement>*/
}

impl ArrayBindingPattern {
    pub fn new(base_node: BaseNode, elements: Id<NodeArray>) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

impl HasElementsInterface for ArrayBindingPattern {
    fn elements(&self) -> Id<NodeArray> {
        self.elements.clone()
    }
}

pub trait HasTypeParametersInterface {
    fn maybe_type_parameters(&self) -> Option<Id<NodeArray>>;
    fn set_type_parameters(&self, type_parameters: Option<Id<NodeArray>>);
}

pub trait GenericNamedDeclarationInterface:
    NamedDeclarationInterface + HasTypeParametersInterface
{
}

#[derive(Debug)]
#[ast_type(impl_from = false, interfaces = "NamedDeclarationInterface")]
pub struct BaseGenericNamedDeclaration {
    _named_declaration: BaseNamedDeclaration,
    type_parameters: Cell<Option<Id<NodeArray> /*<TypeParameterDeclaration>*/>>,
}

impl BaseGenericNamedDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        type_parameters: Option<Id<NodeArray>>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            type_parameters: Cell::new(type_parameters),
        }
    }
}

impl HasTypeParametersInterface for BaseGenericNamedDeclaration {
    fn maybe_type_parameters(&self) -> Option<Id<NodeArray>> {
        self.type_parameters.get()
    }

    fn set_type_parameters(&self, type_parameters: Option<Id<NodeArray>>) {
        self.type_parameters.set(type_parameters);
    }
}

impl GenericNamedDeclarationInterface for BaseGenericNamedDeclaration {}

pub trait InterfaceOrClassLikeDeclarationInterface {
    fn maybe_heritage_clauses(&self) -> Option<Id<NodeArray>>;
}

#[derive(Debug)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct BaseInterfaceOrClassLikeDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration,
    heritage_clauses: Option<Id<NodeArray> /*<HeritageClause>*/>,
}

impl BaseInterfaceOrClassLikeDeclaration {
    pub fn new(
        base_generic_named_declaration: BaseGenericNamedDeclaration,
        heritage_clauses: Option<Id<NodeArray>>,
    ) -> Self {
        Self {
            _generic_named_declaration: base_generic_named_declaration,
            heritage_clauses,
        }
    }
}

impl InterfaceOrClassLikeDeclarationInterface for BaseInterfaceOrClassLikeDeclaration {
    fn maybe_heritage_clauses(&self) -> Option<Id<NodeArray>> {
        self.heritage_clauses.clone()
    }
}

pub trait ClassLikeDeclarationInterface:
    NamedDeclarationInterface
    + HasTypeParametersInterface
    + GenericNamedDeclarationInterface
    + InterfaceOrClassLikeDeclarationInterface
{
    fn members(&self) -> Id<NodeArray>;
}

#[derive(Debug)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, InterfaceOrClassLikeDeclarationInterface"
)]
pub struct ClassLikeDeclarationBase {
    _interface_or_class_like_declaration: BaseInterfaceOrClassLikeDeclaration,
    members: Id<NodeArray>, /*<ClassElement>*/
}

impl ClassLikeDeclarationBase {
    pub fn new(
        base_interface_or_class_like_declaration: BaseInterfaceOrClassLikeDeclaration,
        members: Id<NodeArray>,
    ) -> Self {
        Self {
            _interface_or_class_like_declaration: base_interface_or_class_like_declaration,
            members,
        }
    }
}

impl ClassLikeDeclarationInterface for ClassLikeDeclarationBase {
    fn members(&self) -> Id<NodeArray> {
        self.members.clone()
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
    pub members: Id<NodeArray>,                                                /*<TypeElement>*/
}

impl InterfaceDeclaration {
    pub fn new(
        base_interface_or_class_like_declaration: BaseInterfaceOrClassLikeDeclaration,
        members: Id<NodeArray>,
    ) -> Self {
        Self {
            _interface_or_class_like_declaration: base_interface_or_class_like_declaration,
            members,
        }
    }
}

impl HasMembersInterface for InterfaceDeclaration {
    fn members(&self) -> Id<NodeArray> {
        self.members.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct HeritageClause {
    _node: BaseNode,
    pub token: SyntaxKind, /*SyntaxKind.ExtendsKeyword | SyntaxKind.ImplementsKeyword*/
    pub types: Id<NodeArray>, /*<ExpressionWithTypeArguments>*/
}

impl HeritageClause {
    pub fn new(base_node: BaseNode, token: SyntaxKind, types: Id<NodeArray>) -> Self {
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
    pub type_: Id<Node /*TypeNode*/>,
}

impl TypeAliasDeclaration {
    pub fn new(
        base_generic_named_declaration: BaseGenericNamedDeclaration,
        type_: Id<Node>,
    ) -> Self {
        Self {
            _generic_named_declaration: base_generic_named_declaration,
            type_,
        }
    }
}

impl HasTypeInterface for TypeAliasDeclaration {
    fn maybe_type(&self) -> Option<Id<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Id<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug)]
#[ast_type]
pub struct EnumMember {
    _node: BaseNode,
    pub name: Id<Node /*PropertyName*/>,
    pub initializer: Option<Id<Node /*Expression*/>>,
}

impl EnumMember {
    pub fn new(base_node: BaseNode, name: Id<Node>, initializer: Option<Id<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
            initializer,
        }
    }
}

impl NamedDeclarationInterface for EnumMember {
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

impl HasInitializerInterface for EnumMember {
    fn maybe_initializer(&self) -> Option<Id<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Id<Node>) {
        self.initializer = Some(initializer);
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct EnumDeclaration {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub members: Id<NodeArray>,               /*<EnumMember>*/
}

impl EnumDeclaration {
    pub fn new(base_named_declaration: BaseNamedDeclaration, members: Id<NodeArray>) -> Self {
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
    pub name: Id<Node /*ModuleName*/>,
    pub body: Option<Id<Node /*ModuleBody | JSDocNamespaceDeclaration*/>>,
}

impl ModuleDeclaration {
    pub fn new(base_node: BaseNode, name: Id<Node>, body: Option<Id<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
            body,
        }
    }
}

impl NamedDeclarationInterface for ModuleDeclaration {
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

#[derive(Debug)]
#[ast_type]
pub struct ModuleBlock {
    _node: BaseNode,
    pub statements: Id<NodeArray>, /*<Statement>*/
}

impl ModuleBlock {
    pub fn new(base_node: BaseNode, statements: Id<NodeArray>) -> Self {
        Self {
            _node: base_node,
            statements,
        }
    }
}

impl HasStatementsInterface for ModuleBlock {
    fn statements(&self) -> Id<NodeArray> {
        self.statements.clone()
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
    pub module_reference: Id<Node /*ModuleReference*/>,
}

impl ImportEqualsDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        is_type_only: bool,
        module_reference: Id<Node>,
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
