#![allow(non_upper_case_globals)]

use gc::Gc;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::rc::Rc;

use super::{
    BaseBindingLikeDeclaration, BaseNamedDeclaration, BaseNode, BaseVariableLikeDeclaration,
    BindingLikeDeclarationInterface, FlowNode, HasExpressionInterface, HasInitializerInterface,
    HasMembersInterface, HasPropertiesInterface, HasPropertyNameInterface, HasStatementsInterface,
    HasTypeArgumentsInterface, HasTypeInterface, LiteralLikeNodeInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, SyntaxKind,
    VariableLikeDeclarationInterface,
};
use local_macros::ast_type;

#[derive(Debug)]
#[ast_type]
pub struct MetaProperty {
    _node: BaseNode,
    pub keyword_token: SyntaxKind, /*SyntaxKind.NewKeyword | SyntaxKind.ImportKeyword*/
    pub name: Gc<Node /*Identifier*/>,
}

impl MetaProperty {
    pub fn new(base_node: BaseNode, keyword_token: SyntaxKind, name: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            keyword_token,
            name,
        }
    }
}

pub trait HasChildrenInterface {
    fn children(&self) -> &NodeArray;
}

#[derive(Debug)]
#[ast_type]
pub struct JsxElement {
    _node: BaseNode,
    pub opening_element: Gc<Node /*JsxOpeningElement*/>,
    pub children: NodeArray, /*<JsxChild>*/
    pub closing_element: Gc<Node /*JsxClosingElement*/>,
}

impl JsxElement {
    pub fn new(
        base_node: BaseNode,
        opening_element: Gc<Node>,
        children: NodeArray,
        closing_element: Gc<Node>,
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
    fn children(&self) -> &NodeArray {
        &self.children
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

impl HasPropertiesInterface for JsxAttributes {
    fn properties(&self) -> &NodeArray {
        &self.properties
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxOpeningElement {
    _node: BaseNode,
    pub tag_name: Gc<Node /*JsxTagNameExpression*/>,
    type_arguments: RefCell<Option<NodeArray /*<TypeNode>*/>>,
    pub attributes: Gc<Node /*JsxAttributes*/>,
}

impl JsxOpeningElement {
    pub fn new(
        base_node: BaseNode,
        tag_name: Gc<Node>,
        type_arguments: Option<NodeArray>,
        attributes: Gc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            tag_name,
            type_arguments: RefCell::new(type_arguments),
            attributes,
        }
    }
}

pub trait HasTagNameInterface {
    fn tag_name(&self) -> Gc<Node>;
}

pub trait JsxOpeningLikeElementInterface: HasTagNameInterface + HasTypeArgumentsInterface {
    fn attributes(&self) -> Gc<Node>;
}

impl HasTagNameInterface for JsxOpeningElement {
    fn tag_name(&self) -> Gc<Node> {
        self.tag_name.clone()
    }
}

impl JsxOpeningLikeElementInterface for JsxOpeningElement {
    fn attributes(&self) -> Gc<Node> {
        self.attributes.clone()
    }
}

impl HasTypeArgumentsInterface for JsxOpeningElement {
    fn maybe_type_arguments(&self) -> Ref<Option<NodeArray>> {
        self.type_arguments.borrow()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxSelfClosingElement {
    _node: BaseNode,
    pub tag_name: Gc<Node /*JsxTagNameExpression*/>,
    type_arguments: RefCell<Option<NodeArray /*<TypeNode>*/>>,
    pub attributes: Gc<Node /*JsxAttributes*/>,
}

impl JsxSelfClosingElement {
    pub fn new(
        base_node: BaseNode,
        tag_name: Gc<Node>,
        type_arguments: Option<NodeArray>,
        attributes: Gc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            tag_name,
            type_arguments: RefCell::new(type_arguments),
            attributes,
        }
    }
}

impl HasTagNameInterface for JsxSelfClosingElement {
    fn tag_name(&self) -> Gc<Node> {
        self.tag_name.clone()
    }
}

impl JsxOpeningLikeElementInterface for JsxSelfClosingElement {
    fn attributes(&self) -> Gc<Node> {
        self.attributes.clone()
    }
}

impl HasTypeArgumentsInterface for JsxSelfClosingElement {
    fn maybe_type_arguments(&self) -> Ref<Option<NodeArray>> {
        self.type_arguments.borrow()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxFragment {
    _node: BaseNode,
    pub opening_fragment: Gc<Node /*JsxOpeningFragment*/>,
    pub children: NodeArray, /*<JsxChild>*/
    pub closing_fragment: Gc<Node /*JsxClosingFragment*/>,
}

impl JsxFragment {
    pub fn new(
        base_node: BaseNode,
        opening_fragment: Gc<Node>,
        children: NodeArray,
        closing_fragment: Gc<Node>,
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
    fn children(&self) -> &NodeArray {
        &self.children
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
    pub name: Gc<Node /*Identifier*/>,
    pub initializer: Option<Gc<Node /*StringLiteral | JsxExpression*/>>,
}

impl JsxAttribute {
    pub fn new(base_node: BaseNode, name: Gc<Node>, initializer: Option<Gc<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
            initializer,
        }
    }
}

impl NamedDeclarationInterface for JsxAttribute {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = name;
    }
}

impl HasInitializerInterface for JsxAttribute {
    fn maybe_initializer(&self) -> Option<Gc<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Gc<Node>) {
        self.initializer = Some(initializer);
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxSpreadAttribute {
    _node: BaseNode,
    pub expression: Gc<Node /*Expression*/>,
}

impl JsxSpreadAttribute {
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for JsxSpreadAttribute {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxClosingElement {
    _node: BaseNode,
    pub tag_name: Gc<Node /*JsxTagNameExpression*/>,
}

impl JsxClosingElement {
    pub fn new(base_node: BaseNode, tag_name: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            tag_name,
        }
    }
}

impl HasTagNameInterface for JsxClosingElement {
    fn tag_name(&self) -> Gc<Node> {
        self.tag_name.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JsxExpression {
    _node: BaseNode,
    pub dot_dot_dot_token: Option<Gc<Node /*Token<SyntaxKind.DotDotDotToken>*/>>,
    pub expression: Option<Gc<Node /*Expression*/>>,
}

impl JsxExpression {
    pub fn new(
        base_node: BaseNode,
        dot_dot_dot_token: Option<Gc<Node>>,
        expression: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            dot_dot_dot_token,
            expression,
        }
    }
}

impl HasExpressionInterface for JsxExpression {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone().unwrap()
    }

    fn maybe_expression(&self) -> Option<Gc<Node>> {
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

impl HasElementsInterface for CommaListExpression {
    fn elements(&self) -> &NodeArray {
        &self.elements
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
    fn statements(&self) -> &NodeArray {
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
    pub fn new(base_node: BaseNode, declaration_list: Gc<Node>) -> Self {
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
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ExpressionStatement {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
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
        expression: Gc<Node>,
        then_statement: Gc<Node>,
        else_statement: Option<Gc<Node>>,
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
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
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
    pub fn new(base_node: BaseNode, statement: Gc<Node>, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            statement,
            expression,
        }
    }
}

impl HasExpressionInterface for DoStatement {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
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
    pub fn new(base_node: BaseNode, expression: Gc<Node>, statement: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            statement,
        }
    }
}

impl HasExpressionInterface for WhileStatement {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

pub trait HasConditionInterface {
    fn maybe_condition(&self) -> Option<Gc<Node>>;
}

#[derive(Debug)]
#[ast_type]
pub struct ForStatement {
    _node: BaseNode,
    pub statement: Gc<Node /*Statement*/>,
    pub initializer: Option<Gc<Node /*ForInitializer*/>>,
    pub condition: Option<Gc<Node /*Expression*/>>,
    pub incrementor: Option<Gc<Node /*Expression*/>>,
}

impl ForStatement {
    pub fn new(
        base_node: BaseNode,
        initializer: Option<Gc<Node>>,
        condition: Option<Gc<Node>>,
        incrementor: Option<Gc<Node>>,
        statement: Gc<Node>,
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
    fn maybe_initializer(&self) -> Option<Gc<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Gc<Node>) {
        self.initializer = Some(initializer);
    }
}

impl HasConditionInterface for ForStatement {
    fn maybe_condition(&self) -> Option<Gc<Node>> {
        self.condition.clone()
    }
}

pub trait HasStatementInterface {
    fn statement(&self) -> Gc<Node>;
}

#[derive(Debug)]
#[ast_type]
pub struct ForInStatement {
    _node: BaseNode,
    pub statement: Gc<Node /*Statement*/>,
    pub initializer: Gc<Node /*ForInitializer*/>,
    pub expression: Gc<Node /*Expression*/>,
}

impl ForInStatement {
    pub fn new(
        base_node: BaseNode,
        initializer: Gc<Node>,
        expression: Gc<Node>,
        statement: Gc<Node>,
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
    fn maybe_initializer(&self) -> Option<Gc<Node>> {
        Some(self.initializer.clone())
    }

    fn set_initializer(&mut self, initializer: Gc<Node>) {
        self.initializer = initializer;
    }
}

impl HasStatementInterface for ForInStatement {
    fn statement(&self) -> Gc<Node> {
        self.statement.clone()
    }
}

impl HasExpressionInterface for ForInStatement {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ForOfStatement {
    _node: BaseNode,
    pub await_modifier: Option<Gc<Node /*AwaitKeywordToken*/>>,
    pub statement: Gc<Node /*Statement*/>,
    pub initializer: Gc<Node /*ForInitializer*/>,
    pub expression: Gc<Node /*Expression*/>,
}

impl ForOfStatement {
    pub fn new(
        base_node: BaseNode,
        await_modifier: Option<Gc<Node>>,
        initializer: Gc<Node>,
        expression: Gc<Node>,
        statement: Gc<Node>,
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
    fn maybe_initializer(&self) -> Option<Gc<Node>> {
        Some(self.initializer.clone())
    }

    fn set_initializer(&mut self, initializer: Gc<Node>) {
        self.initializer = initializer;
    }
}

impl HasStatementInterface for ForOfStatement {
    fn statement(&self) -> Gc<Node> {
        self.statement.clone()
    }
}

impl HasExpressionInterface for ForOfStatement {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

pub trait HasLabelInterface {
    fn maybe_label(&self) -> Option<Gc<Node>>;
}

#[derive(Debug)]
#[ast_type]
pub struct BreakStatement {
    _node: BaseNode,
    pub label: Option<Gc<Node /*Identifier*/>>,
}

impl BreakStatement {
    pub fn new(base_node: BaseNode, label: Option<Gc<Node>>) -> Self {
        Self {
            _node: base_node,
            label,
        }
    }
}

impl HasLabelInterface for BreakStatement {
    fn maybe_label(&self) -> Option<Gc<Node>> {
        self.label.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ContinueStatement {
    _node: BaseNode,
    pub label: Option<Gc<Node /*Identifier*/>>,
}

impl ContinueStatement {
    pub fn new(base_node: BaseNode, label: Option<Gc<Node>>) -> Self {
        Self {
            _node: base_node,
            label,
        }
    }
}

impl HasLabelInterface for ContinueStatement {
    fn maybe_label(&self) -> Option<Gc<Node>> {
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
    pub fn new(base_node: BaseNode, expression: Option<Gc<Node>>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ReturnStatement {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone().unwrap()
    }

    fn maybe_expression(&self) -> Option<Gc<Node>> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct WithStatement {
    _node: BaseNode,
    pub expression: Gc<Node /*Expression*/>,
    pub statement: Gc<Node /*Statement*/>,
}

impl WithStatement {
    pub fn new(base_node: BaseNode, expression: Gc<Node>, statement: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            statement,
        }
    }
}

impl HasExpressionInterface for WithStatement {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct SwitchStatement {
    _node: BaseNode,
    pub expression: Gc<Node /*Expression*/>,
    pub case_block: Gc<Node /*CaseBlock*/>,
    possibly_exhaustive: Cell<Option<bool>>,
}

impl SwitchStatement {
    pub fn new(base_node: BaseNode, expression: Gc<Node>, case_block: Gc<Node>) -> Self {
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
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
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
    pub expression: Gc<Node /*Expression*/>,
    pub statements: NodeArray, /*<Statement>*/
    fallthrough_flow_node: RefCell<Option<Rc<FlowNode>>>,
}

impl CaseClause {
    pub fn new(base_node: BaseNode, expression: Gc<Node>, statements: NodeArray) -> Self {
        Self {
            _node: base_node,
            expression,
            statements,
            fallthrough_flow_node: RefCell::new(None),
        }
    }
}

impl HasStatementsInterface for CaseClause {
    fn statements(&self) -> &NodeArray {
        &self.statements
    }
}

pub trait CaseOrDefaultClauseInterface: HasStatementsInterface {
    fn maybe_fallthrough_flow_node(&self) -> Option<Rc<FlowNode>>;
    fn set_fallthrough_flow_node(&self, fallthrough_flow_node: Option<Rc<FlowNode>>);
}

impl CaseOrDefaultClauseInterface for CaseClause {
    fn maybe_fallthrough_flow_node(&self) -> Option<Rc<FlowNode>> {
        self.fallthrough_flow_node.borrow().clone()
    }

    fn set_fallthrough_flow_node(&self, fallthrough_flow_node: Option<Rc<FlowNode>>) {
        *self.fallthrough_flow_node.borrow_mut() = fallthrough_flow_node;
    }
}

impl HasExpressionInterface for CaseClause {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct DefaultClause {
    _node: BaseNode,
    pub statements: NodeArray, /*<Statement>*/
    fallthrough_flow_node: RefCell<Option<Rc<FlowNode>>>,
}

impl DefaultClause {
    pub fn new(base_node: BaseNode, statements: NodeArray) -> Self {
        Self {
            _node: base_node,
            statements,
            fallthrough_flow_node: RefCell::new(None),
        }
    }
}

impl HasStatementsInterface for DefaultClause {
    fn statements(&self) -> &NodeArray {
        &self.statements
    }
}

impl CaseOrDefaultClauseInterface for DefaultClause {
    fn maybe_fallthrough_flow_node(&self) -> Option<Rc<FlowNode>> {
        self.fallthrough_flow_node.borrow().clone()
    }

    fn set_fallthrough_flow_node(&self, fallthrough_flow_node: Option<Rc<FlowNode>>) {
        *self.fallthrough_flow_node.borrow_mut() = fallthrough_flow_node;
    }
}

#[derive(Debug)]
#[ast_type]
pub struct LabeledStatement {
    _node: BaseNode,
    pub label: Gc<Node /*Identifier*/>,
    pub statement: Gc<Node /*Statement*/>,
}

impl LabeledStatement {
    pub fn new(base_node: BaseNode, label: Gc<Node>, statement: Gc<Node>) -> Self {
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
    pub expression: Gc<Node /*Expression*/>,
}

impl ThrowStatement {
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ThrowStatement {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TryStatement {
    _node: BaseNode,
    pub try_block: Gc<Node /*Block*/>,
    pub catch_clause: Option<Gc<Node /*CatchClause*/>>,
    pub finally_block: Option<Gc<Node /*Block*/>>,
}

impl TryStatement {
    pub fn new(
        base_node: BaseNode,
        try_block: Gc<Node>,
        catch_clause: Option<Gc<Node>>,
        finally_block: Option<Gc<Node>>,
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
    pub variable_declaration: Option<Gc<Node /*VariableDeclaration*/>>,
    pub block: Gc<Node /*Block*/>,
}

impl CatchClause {
    pub fn new(
        base_node: BaseNode,
        variable_declaration: Option<Gc<Node>>,
        block: Gc<Node>,
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
    pub property_name: Option<Gc<Node /*PropertyName*/>>,
    pub dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
}

impl BindingElement {
    pub fn new(
        base_binding_like_declaration: BaseBindingLikeDeclaration,
        property_name: Option<Gc<Node>>,
        dot_dot_dot_token: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _binding_like_declaration: base_binding_like_declaration,
            property_name,
            dot_dot_dot_token,
        }
    }
}

impl HasPropertyNameInterface for BindingElement {
    fn maybe_property_name(&self) -> Option<Gc<Node>> {
        self.property_name.clone()
    }
}

pub trait HasQuestionTokenInterface {
    fn maybe_question_token(&self) -> Option<Gc<Node>>;
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct PropertySignature {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub question_token: Option<Gc<Node /*QuestionToken*/>>,
    pub type_: Option<Gc<Node /*TypeNode*/>>,
    pub initializer: Option<Gc<Node /*Expression*/>>,
}

impl PropertySignature {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        question_token: Option<Gc<Node>>,
        type_: Option<Gc<Node>>,
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
    fn maybe_type(&self) -> Option<Gc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_;
    }
}

impl HasInitializerInterface for PropertySignature {
    fn maybe_initializer(&self) -> Option<Gc<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Gc<Node>) {
        self.initializer = Some(initializer);
    }
}

impl HasQuestionTokenInterface for PropertySignature {
    fn maybe_question_token(&self) -> Option<Gc<Node>> {
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
    pub question_token: Option<Gc<Node /*QuestionToken*/>>,
    pub exclamation_token: Option<Gc<Node /*ExclamationToken*/>>,
}

impl PropertyDeclaration {
    pub fn new(
        base_variable_like_declaration: BaseVariableLikeDeclaration,
        question_token: Option<Gc<Node>>,
        exclamation_token: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _variable_like_declaration: base_variable_like_declaration,
            question_token,
            exclamation_token,
        }
    }
}

impl HasQuestionTokenInterface for PropertyDeclaration {
    fn maybe_question_token(&self) -> Option<Gc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct PropertyAssignment {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub initializer: Gc<Node /*Expression*/>,
    pub question_token: Option<Gc<Node /*QuestionToken*/>>,
    pub exclamation_token: Option<Gc<Node /*ExclamationToken*/>>,
}

impl PropertyAssignment {
    pub fn new(base_named_declaration: BaseNamedDeclaration, initializer: Gc<Node>) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            initializer,
            question_token: None,
            exclamation_token: None,
        }
    }
}

impl HasInitializerInterface for PropertyAssignment {
    fn maybe_initializer(&self) -> Option<Gc<Node>> {
        Some(self.initializer.clone())
    }

    fn set_initializer(&mut self, initializer: Gc<Node>) {
        self.initializer = initializer;
    }
}

impl HasQuestionTokenInterface for PropertyAssignment {
    fn maybe_question_token(&self) -> Option<Gc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct ShorthandPropertyAssignment {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub question_token: Option<Gc<Node /*QuestionToken*/>>,
    pub exclamation_token: Option<Gc<Node /*ExclamationToken*/>>,
    pub equals_token: Option<Gc<Node /*EqualsToken*/>>,
    pub object_assignment_initializer: Option<Gc<Node /*Expression*/>>,
}

impl ShorthandPropertyAssignment {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        object_assignment_initializer: Option<Gc<Node>>,
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
    fn maybe_question_token(&self) -> Option<Gc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct SpreadAssignment {
    _node: BaseNode,
    pub expression: Gc<Node /*Expression*/>,
}

impl SpreadAssignment {
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for SpreadAssignment {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

impl NamedDeclarationInterface for SpreadAssignment {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        None
    }

    fn name(&self) -> Gc<Node> {
        unreachable!()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        unreachable!()
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
    fn maybe_type_parameters(&self) -> Ref<Option<NodeArray>>;
    fn maybe_type_parameters_mut(&self) -> RefMut<Option<NodeArray>>;
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
    fn maybe_type_parameters(&self) -> Ref<Option<NodeArray>> {
        self.type_parameters.borrow()
    }

    fn maybe_type_parameters_mut(&self) -> RefMut<Option<NodeArray>> {
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

pub trait ClassLikeDeclarationInterface:
    NamedDeclarationInterface
    + HasTypeParametersInterface
    + GenericNamedDeclarationInterface
    + InterfaceOrClassLikeDeclarationInterface
{
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

impl HasMembersInterface for InterfaceDeclaration {
    fn members(&self) -> &NodeArray {
        &self.members
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
    pub type_: Gc<Node /*TypeNode*/>,
}

impl TypeAliasDeclaration {
    pub fn new(
        base_generic_named_declaration: BaseGenericNamedDeclaration,
        type_: Gc<Node>,
    ) -> Self {
        Self {
            _generic_named_declaration: base_generic_named_declaration,
            type_,
        }
    }
}

impl HasTypeInterface for TypeAliasDeclaration {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug)]
#[ast_type]
pub struct EnumMember {
    _node: BaseNode,
    pub name: Gc<Node /*PropertyName*/>,
    pub initializer: Option<Gc<Node /*Expression*/>>,
}

impl EnumMember {
    pub fn new(base_node: BaseNode, name: Gc<Node>, initializer: Option<Gc<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
            initializer,
        }
    }
}

impl NamedDeclarationInterface for EnumMember {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = name;
    }
}

impl HasInitializerInterface for EnumMember {
    fn maybe_initializer(&self) -> Option<Gc<Node>> {
        self.initializer.clone()
    }

    fn set_initializer(&mut self, initializer: Gc<Node>) {
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
    pub name: Gc<Node /*ModuleName*/>,
    pub body: Option<Gc<Node /*ModuleBody | JSDocNamespaceDeclaration*/>>,
}

impl ModuleDeclaration {
    pub fn new(base_node: BaseNode, name: Gc<Node>, body: Option<Gc<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
            body,
        }
    }
}

impl NamedDeclarationInterface for ModuleDeclaration {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = name;
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocNamespaceDeclaration {
    _node: BaseNode,
    pub name: Gc<Node /*Identifier*/>,
    pub body: Option<Gc<Node /*JSDocNamespaceBody*/>>,
}

impl JSDocNamespaceDeclaration {
    pub fn new(base_node: BaseNode, name: Gc<Node>, body: Option<Gc<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
            body,
        }
    }
}

impl NamedDeclarationInterface for JSDocNamespaceDeclaration {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Gc<Node>) {
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
    fn statements(&self) -> &NodeArray {
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
    pub module_reference: Gc<Node /*ModuleReference*/>,
}

impl ImportEqualsDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        is_type_only: bool,
        module_reference: Gc<Node>,
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
