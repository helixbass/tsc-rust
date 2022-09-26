#![allow(non_upper_case_globals)]

use std::cell::{Cell, RefCell};
use std::rc::Rc;

use super::{
    BaseFunctionLikeDeclaration, BaseGenericNamedDeclaration, BaseLiteralLikeNode, BaseNode,
    BaseSignatureDeclaration, BaseVariableLikeDeclaration, FlowNode, HasExpressionInterface,
    HasQuestionTokenInterface, HasTypeInterface, Node, NodeArray, SyntaxKind, Type,
};
use local_macros::ast_type;

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface, HasQuestionTokenInterface"
)]
pub struct ConstructorDeclaration {
    _function_like_declaration: BaseFunctionLikeDeclaration,
}

impl ConstructorDeclaration {
    pub fn new(function_like_declaration: BaseFunctionLikeDeclaration) -> Self {
        Self {
            _function_like_declaration: function_like_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct SemicolonClassElement {
    _node: BaseNode,
}

impl SemicolonClassElement {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface, HasQuestionTokenInterface"
)]
pub struct GetAccessorDeclaration {
    _function_like_declaration: BaseFunctionLikeDeclaration,
}

impl GetAccessorDeclaration {
    pub fn new(function_like_declaration: BaseFunctionLikeDeclaration) -> Self {
        Self {
            _function_like_declaration: function_like_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface, HasQuestionTokenInterface"
)]
pub struct SetAccessorDeclaration {
    _function_like_declaration: BaseFunctionLikeDeclaration,
}

impl SetAccessorDeclaration {
    pub fn new(function_like_declaration: BaseFunctionLikeDeclaration) -> Self {
        Self {
            _function_like_declaration: function_like_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface"
)]
pub struct IndexSignatureDeclaration {
    _signature_declaration: BaseSignatureDeclaration,
}

impl IndexSignatureDeclaration {
    pub fn new(signature_declaration: BaseSignatureDeclaration) -> Self {
        Self {
            _signature_declaration: signature_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct ClassStaticBlockDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration,
    pub body: Rc<Node /*Block*/>,
    end_flow_node: RefCell<Option<Rc<FlowNode>>>,
    return_flow_node: RefCell<Option<Rc<FlowNode>>>,
}

impl ClassStaticBlockDeclaration {
    pub fn new(generic_named_declaration: BaseGenericNamedDeclaration, body: Rc<Node>) -> Self {
        Self {
            _generic_named_declaration: generic_named_declaration,
            body,
            end_flow_node: RefCell::new(None),
            return_flow_node: RefCell::new(None),
        }
    }

    pub fn maybe_end_flow_node(&self) -> Option<Rc<FlowNode>> {
        self.end_flow_node.borrow().clone()
    }

    pub fn set_end_flow_node(&self, end_flow_node: Option<Rc<FlowNode>>) {
        *self.end_flow_node.borrow_mut() = end_flow_node;
    }

    pub fn maybe_return_flow_node(&self) -> Option<Rc<FlowNode>> {
        self.return_flow_node.borrow().clone()
    }

    pub fn set_return_flow_node(&self, return_flow_node: Option<Rc<FlowNode>>) {
        *self.return_flow_node.borrow_mut() = return_flow_node;
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasInitializerInterface, BindingLikeDeclarationInterface, HasTypeInterface, VariableLikeDeclarationInterface"
)]
pub struct VariableDeclaration {
    _variable_like_declaration: BaseVariableLikeDeclaration,
    pub exclamation_token: Option<Rc<Node /*ExclamationToken*/>>,
}

impl VariableDeclaration {
    pub fn new(
        base_variable_like_declaration: BaseVariableLikeDeclaration,
        exclamation_token: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _variable_like_declaration: base_variable_like_declaration,
            exclamation_token,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct VariableDeclarationList {
    _node: BaseNode,
    pub declarations: NodeArray, /*<VariableDeclaration>*/
}

impl VariableDeclarationList {
    pub fn new(base_node: BaseNode, declarations: NodeArray) -> Self {
        Self {
            _node: base_node,
            declarations,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasInitializerInterface, BindingLikeDeclarationInterface, HasTypeInterface, VariableLikeDeclarationInterface"
)]
pub struct ParameterDeclaration {
    _variable_like_declaration: BaseVariableLikeDeclaration,
    pub dot_dot_dot_token: Option<Rc<Node /*DotDotDotToken*/>>,
    pub question_token: Option<Rc<Node /*QuestionToken*/>>,
}

impl ParameterDeclaration {
    pub fn new(
        base_variable_like_declaration: BaseVariableLikeDeclaration,
        dot_dot_dot_token: Option<Rc<Node>>,
        question_token: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _variable_like_declaration: base_variable_like_declaration,
            dot_dot_dot_token,
            question_token,
        }
    }
}

pub trait HasDotDotDotTokenInterface {
    fn maybe_dot_dot_dot_token(&self) -> Option<Rc<Node>>;
}

impl HasDotDotDotTokenInterface for ParameterDeclaration {
    fn maybe_dot_dot_dot_token(&self) -> Option<Rc<Node>> {
        self.dot_dot_dot_token.clone()
    }
}

impl HasQuestionTokenInterface for ParameterDeclaration {
    fn maybe_question_token(&self) -> Option<Rc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct KeywordTypeNode {
    _node: BaseNode,
}

impl KeywordTypeNode {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

impl From<BaseNode> for KeywordTypeNode {
    fn from(base_node: BaseNode) -> Self {
        KeywordTypeNode::new(base_node)
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ImportTypeNode {
    _node: BaseNode,
    pub type_arguments: Option<NodeArray /*<TypeNode>*/>,
    is_type_of: Cell<bool>,
    pub argument: Rc<Node /*<TypeNode>*/>,
    pub qualifier: Option<Rc<Node /*<EntityName>*/>>,
}

impl ImportTypeNode {
    pub fn new(
        base_node: BaseNode,
        argument: Rc<Node>,
        qualifier: Option<Rc<Node>>,
        type_arguments: Option<NodeArray>,
        is_type_of: bool,
    ) -> Self {
        Self {
            _node: base_node,
            type_arguments,
            is_type_of: Cell::new(is_type_of),
            argument,
            qualifier,
        }
    }

    pub fn is_type_of(&self) -> bool {
        self.is_type_of.get()
    }

    pub fn set_is_type_of(&self, is_type_of: bool) {
        self.is_type_of.set(is_type_of)
    }
}

impl HasTypeArgumentsInterface for ImportTypeNode {
    fn maybe_type_arguments(&self) -> Option<&NodeArray> {
        self.type_arguments.as_ref()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ThisTypeNode {
    _node: BaseNode,
}

impl ThisTypeNode {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, SignatureDeclarationInterface"
)]
pub struct FunctionTypeNode {
    _signature_declaration: BaseSignatureDeclaration,
}

impl FunctionTypeNode {
    pub fn new(base_signature_declaration: BaseSignatureDeclaration) -> Self {
        Self {
            _signature_declaration: base_signature_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, SignatureDeclarationInterface"
)]
pub struct ConstructorTypeNode {
    _signature_declaration: BaseSignatureDeclaration,
}

impl ConstructorTypeNode {
    pub fn new(base_signature_declaration: BaseSignatureDeclaration) -> Self {
        Self {
            _signature_declaration: base_signature_declaration,
        }
    }
}

pub trait HasTypeArgumentsInterface {
    fn maybe_type_arguments(&self) -> Option<&NodeArray>;
}

#[derive(Debug)]
#[ast_type]
pub struct TypeReferenceNode {
    _node: BaseNode,
    pub type_name: Rc<Node /*EntityName*/>,
    pub type_arguments: Option<NodeArray /*<TypeNode>*/>,
}

impl TypeReferenceNode {
    pub fn new(
        base_node: BaseNode,
        type_name: Rc<Node>,
        type_arguments: Option<NodeArray>,
    ) -> Self {
        Self {
            _node: base_node,
            type_name,
            type_arguments,
        }
    }
}

impl HasTypeArgumentsInterface for TypeReferenceNode {
    fn maybe_type_arguments(&self) -> Option<&NodeArray> {
        self.type_arguments.as_ref()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TypePredicateNode {
    _node: BaseNode,
    pub asserts_modifier: Option<Rc<Node /*AssertsToken*/>>,
    pub parameter_name: Rc<Node /*Identifier | ThisTypeNode*/>,
    pub type_: Option<Rc<Node /*TypeNode*/>>,
}

impl TypePredicateNode {
    pub fn new(
        base_node: BaseNode,
        asserts_modifier: Option<Rc<Node>>,
        parameter_name: Rc<Node>,
        type_: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            asserts_modifier,
            parameter_name,
            type_,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TypeQueryNode {
    _node: BaseNode,
    pub expr_name: Rc<Node /*EntityName*/>,
}

impl TypeQueryNode {
    pub fn new(base_node: BaseNode, expr_name: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expr_name,
        }
    }
}

pub trait HasMembersInterface {
    fn members(&self) -> &NodeArray;
}

#[derive(Debug)]
#[ast_type]
pub struct TypeLiteralNode {
    _node: BaseNode,
    pub members: NodeArray, /*<TypeElement>*/
}

impl TypeLiteralNode {
    pub fn new(base_node: BaseNode, members: NodeArray) -> Self {
        Self {
            _node: base_node,
            members,
        }
    }
}

impl HasMembersInterface for TypeLiteralNode {
    fn members(&self) -> &NodeArray {
        &self.members
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ArrayTypeNode {
    _node: BaseNode,
    pub element_type: Rc<Node /*TypeNode*/>,
}

impl ArrayTypeNode {
    pub fn new(base_node: BaseNode, element_type: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            element_type,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TupleTypeNode {
    _node: BaseNode,
    pub elements: NodeArray, /*<TypeNode | NamedTupleMember>*/
}

impl TupleTypeNode {
    pub fn new(base_node: BaseNode, elements: NodeArray) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct NamedTupleMember {
    _node: BaseNode,
    pub dot_dot_dot_token: Option<Rc<Node /*DotDotDotToken*/>>,
    pub name: Rc<Node /*Identifier*/>,
    pub question_token: Option<Rc<Node /*QuestionToken*/>>,
    pub type_: Rc<Node /*TypeNode*/>,
}

impl NamedTupleMember {
    pub fn new(
        base_node: BaseNode,
        dot_dot_dot_token: Option<Rc<Node>>,
        name: Rc<Node>,
        question_token: Option<Rc<Node>>,
        type_: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            dot_dot_dot_token,
            name,
            question_token,
            type_,
        }
    }
}

impl HasTypeInterface for NamedTupleMember {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Rc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

impl HasDotDotDotTokenInterface for NamedTupleMember {
    fn maybe_dot_dot_dot_token(&self) -> Option<Rc<Node>> {
        self.dot_dot_dot_token.clone()
    }
}

impl HasQuestionTokenInterface for NamedTupleMember {
    fn maybe_question_token(&self) -> Option<Rc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct OptionalTypeNode {
    _node: BaseNode,
    pub type_: Rc<Node /*TypeNode*/>,
}

impl OptionalTypeNode {
    pub fn new(base_node: BaseNode, type_: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct RestTypeNode {
    _node: BaseNode,
    pub type_: Rc<Node /*TypeNode*/>,
}

impl RestTypeNode {
    pub fn new(base_node: BaseNode, type_: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for RestTypeNode {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Rc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

pub trait UnionOrIntersectionTypeNodeInterface {
    fn types(&self) -> &NodeArray;
}

#[derive(Debug)]
#[ast_type]
pub struct UnionTypeNode {
    _node: BaseNode,
    pub types: NodeArray, /*<TypeNode>*/
}

impl UnionTypeNode {
    pub fn new(base_node: BaseNode, types: NodeArray) -> Self {
        Self {
            _node: base_node,
            types,
        }
    }
}

impl UnionOrIntersectionTypeNodeInterface for UnionTypeNode {
    fn types(&self) -> &NodeArray {
        &self.types
    }
}

#[derive(Debug)]
#[ast_type]
pub struct IntersectionTypeNode {
    _node: BaseNode,
    pub types: NodeArray, /*<TypeNode>*/
}

impl IntersectionTypeNode {
    pub fn new(base_node: BaseNode, types: NodeArray) -> Self {
        Self {
            _node: base_node,
            types,
        }
    }
}

impl UnionOrIntersectionTypeNodeInterface for IntersectionTypeNode {
    fn types(&self) -> &NodeArray {
        &self.types
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ConditionalTypeNode {
    _node: BaseNode,
    pub check_type: Rc<Node /*TypeNode*/>,
    pub extends_type: Rc<Node /*TypeNode*/>,
    pub true_type: Rc<Node /*TypeNode*/>,
    pub false_type: Rc<Node /*TypeNode*/>,
}

impl ConditionalTypeNode {
    pub fn new(
        base_node: BaseNode,
        check_type: Rc<Node>,
        extends_type: Rc<Node>,
        true_type: Rc<Node>,
        false_type: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            check_type,
            extends_type,
            true_type,
            false_type,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct InferTypeNode {
    _node: BaseNode,
    pub type_parameter: Rc<Node /*TypeParameterDeclaration*/>,
}

impl InferTypeNode {
    pub fn new(base_node: BaseNode, type_parameter: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            type_parameter,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ParenthesizedTypeNode {
    _node: BaseNode,
    pub type_: Rc<Node /*TypeNode*/>,
}

impl ParenthesizedTypeNode {
    pub fn new(base_node: BaseNode, type_: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for ParenthesizedTypeNode {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Rc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TypeOperatorNode {
    _node: BaseNode,
    pub operator: SyntaxKind, /*SyntaxKind.KeyOfKeyword | SyntaxKind.UniqueKeyword | SyntaxKind.ReadonlyKeyword*/
    pub type_: Rc<Node /*TypeNode*/>,
}

impl TypeOperatorNode {
    pub fn new(base_node: BaseNode, operator: SyntaxKind, type_: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            operator,
            type_,
        }
    }
}

impl HasTypeInterface for TypeOperatorNode {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Rc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug)]
#[ast_type]
pub struct IndexedAccessTypeNode {
    _node: BaseNode,
    pub object_type: Rc<Node /*TypeNode*/>,
    pub index_type: Rc<Node /*TypeNode*/>,
}

impl IndexedAccessTypeNode {
    pub fn new(base_node: BaseNode, object_type: Rc<Node>, index_type: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            object_type,
            index_type,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct MappedTypeNode {
    _node: BaseNode,
    pub readonly_token: Option<Rc<Node /*ReadonlyToken | PlusToken | MinusToken*/>>,
    pub type_parameter: Rc<Node /*TypeParameterDeclaration*/>,
    pub name_type: Option<Rc<Node /*TypeNode*/>>,
    pub question_token: Option<Rc<Node /*QuestionToken | PlusToken | MinusToken*/>>,
    pub type_: Option<Rc<Node /*TypeNode*/>>,
    pub members: Option<NodeArray /*<TypeElement>*/>,
}

impl MappedTypeNode {
    pub fn new(
        base_node: BaseNode,
        readonly_token: Option<Rc<Node>>,
        type_parameter: Rc<Node>,
        name_type: Option<Rc<Node>>,
        question_token: Option<Rc<Node>>,
        type_: Option<Rc<Node>>,
        members: Option<NodeArray>,
    ) -> Self {
        Self {
            _node: base_node,
            readonly_token,
            type_parameter,
            name_type,
            question_token,
            type_,
            members,
        }
    }
}

impl HasTypeInterface for MappedTypeNode {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Option<Rc<Node>>) {
        self.type_ = type_;
    }
}

impl HasQuestionTokenInterface for MappedTypeNode {
    fn maybe_question_token(&self) -> Option<Rc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct LiteralTypeNode {
    _node: BaseNode,
    pub literal: Rc<Node>, // TODO: should be weak?
}

impl LiteralTypeNode {
    pub fn new(base_node: BaseNode, literal: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            literal,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "LiteralLikeNodeInterface")]
pub struct StringLiteral {
    _literal_like_node: BaseLiteralLikeNode,
    pub(crate) text_source_node:
        Option<Rc<Node /*Identifier | StringLiteralLike | NumericLiteral*/>>,
    pub(crate) single_quote: Option<bool>,
}

impl StringLiteral {
    pub fn new(base_literal_like_node: BaseLiteralLikeNode, single_quote: Option<bool>) -> Self {
        Self {
            _literal_like_node: base_literal_like_node,
            text_source_node: None,
            single_quote,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TemplateLiteralTypeNode {
    pub _node: BaseNode,
    pub head: Rc<Node /*TemplateHead*/>,
    pub template_spans: NodeArray, /*<TemplateLiteralTypeSpan>*/
}

impl TemplateLiteralTypeNode {
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
pub struct TemplateLiteralTypeSpan {
    pub _node: BaseNode,
    pub type_: Rc<Node /*TypeNode*/>,
    pub literal: Rc<Node /*TemplateMiddle | TemplateTail*/>,
}

impl TemplateLiteralTypeSpan {
    pub fn new(base_node: BaseNode, type_: Rc<Node>, literal: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
            literal,
        }
    }
}

impl HasTypeInterface for TemplateLiteralTypeSpan {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Rc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug)]
#[ast_type]
pub struct OmittedExpression {
    pub _node: BaseNode,
}

impl OmittedExpression {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct PartiallyEmittedExpression {
    pub _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
}

impl PartiallyEmittedExpression {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for PartiallyEmittedExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

pub trait UnaryExpressionInterface {
    fn operator(&self) -> SyntaxKind;
    fn operand(&self) -> Rc<Node>;
}

#[derive(Debug)]
#[ast_type]
pub struct PrefixUnaryExpression {
    pub _node: BaseNode,
    pub operator: SyntaxKind, /*PrefixUnaryOperator*/
    pub operand: Rc<Node /*UnaryExpression*/>,
}

impl PrefixUnaryExpression {
    pub fn new(base_node: BaseNode, operator: SyntaxKind, operand: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            operator,
            operand,
        }
    }
}

impl UnaryExpressionInterface for PrefixUnaryExpression {
    fn operator(&self) -> SyntaxKind {
        self.operator
    }
    fn operand(&self) -> Rc<Node> {
        self.operand.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct PostfixUnaryExpression {
    pub _node: BaseNode,
    pub operand: Rc<Node /*LeftHandSideExpression*/>,
    pub operator: SyntaxKind, /*PostfixUnaryOperator*/
}

impl PostfixUnaryExpression {
    pub fn new(base_node: BaseNode, operand: Rc<Node>, operator: SyntaxKind) -> Self {
        Self {
            _node: base_node,
            operand,
            operator,
        }
    }
}

impl UnaryExpressionInterface for PostfixUnaryExpression {
    fn operator(&self) -> SyntaxKind {
        self.operator
    }
    fn operand(&self) -> Rc<Node> {
        self.operand.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct DeleteExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*UnaryExpression*/>,
}

impl DeleteExpression {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for DeleteExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TypeOfExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*UnaryExpression*/>,
}

impl TypeOfExpression {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for TypeOfExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct VoidExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*UnaryExpression*/>,
}

impl VoidExpression {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for VoidExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct AwaitExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*UnaryExpression*/>,
}

impl AwaitExpression {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for AwaitExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct YieldExpression {
    _node: BaseNode,
    pub asterisk_token: Option<Rc<Node /*AsteriskToken*/>>,
    pub expression: Option<Rc<Node /*Expression*/>>,
}

impl YieldExpression {
    pub fn new(
        base_node: BaseNode,
        expression: Option<Rc<Node>>,
        asterisk_token: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            asterisk_token,
        }
    }
}

impl HasExpressionInterface for YieldExpression {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone().unwrap()
    }

    fn maybe_expression(&self) -> Option<Rc<Node>> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct SyntheticExpression {
    _node: BaseNode,
    pub is_spread: bool,
    pub type_: Rc<Type>,
    pub tuple_name_source: Option<Rc<Node /*ParameterDeclaration | NamedTupleMember*/>>,
}
