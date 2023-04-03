#![allow(non_upper_case_globals)]

use gc::{Finalize, Gc, GcCell, GcCellRef, Trace};
use std::cell::{Cell, Ref, RefCell};
use std::rc::Rc;

use super::{
    BaseFunctionLikeDeclaration, BaseGenericNamedDeclaration, BaseLiteralLikeNode, BaseNode,
    BaseSignatureDeclaration, BaseVariableLikeDeclaration, FlowNode, HasElementsInterface,
    HasExpressionInterface, HasQuestionTokenInterface, HasTypeInterface, NamedDeclarationInterface,
    Node, NodeArray, SyntaxKind, Type,
};
use local_macros::ast_type;

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct SemicolonClassElement {
    _node: BaseNode,
}

impl SemicolonClassElement {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

impl NamedDeclarationInterface for SemicolonClassElement {
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

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct ClassStaticBlockDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration,
    pub body: Gc<Node /*Block*/>,
    end_flow_node: GcCell<Option<Gc<FlowNode>>>,
    return_flow_node: GcCell<Option<Gc<FlowNode>>>,
}

impl ClassStaticBlockDeclaration {
    pub fn new(generic_named_declaration: BaseGenericNamedDeclaration, body: Gc<Node>) -> Self {
        Self {
            _generic_named_declaration: generic_named_declaration,
            body,
            end_flow_node: Default::default(),
            return_flow_node: Default::default(),
        }
    }

    pub fn maybe_end_flow_node(&self) -> Option<Gc<FlowNode>> {
        self.end_flow_node.borrow().clone()
    }

    pub fn set_end_flow_node(&self, end_flow_node: Option<Gc<FlowNode>>) {
        *self.end_flow_node.borrow_mut() = end_flow_node;
    }

    pub fn maybe_return_flow_node(&self) -> Option<Gc<FlowNode>> {
        self.return_flow_node.borrow().clone()
    }

    pub fn set_return_flow_node(&self, return_flow_node: Option<Gc<FlowNode>>) {
        *self.return_flow_node.borrow_mut() = return_flow_node;
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasInitializerInterface, BindingLikeDeclarationInterface, HasTypeInterface, VariableLikeDeclarationInterface"
)]
pub struct VariableDeclaration {
    _variable_like_declaration: BaseVariableLikeDeclaration,
    pub exclamation_token: Option<Gc<Node /*ExclamationToken*/>>,
}

impl VariableDeclaration {
    pub fn new(
        base_variable_like_declaration: BaseVariableLikeDeclaration,
        exclamation_token: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _variable_like_declaration: base_variable_like_declaration,
            exclamation_token,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct VariableDeclarationList {
    _node: BaseNode,
    pub declarations: Gc<NodeArray /*<VariableDeclaration>*/>,
}

impl VariableDeclarationList {
    pub fn new(base_node: BaseNode, declarations: Gc<NodeArray>) -> Self {
        Self {
            _node: base_node,
            declarations,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasInitializerInterface, BindingLikeDeclarationInterface, HasTypeInterface, VariableLikeDeclarationInterface"
)]
pub struct ParameterDeclaration {
    _variable_like_declaration: BaseVariableLikeDeclaration,
    pub dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
    pub question_token: Option<Gc<Node /*QuestionToken*/>>,
}

impl ParameterDeclaration {
    pub fn new(
        base_variable_like_declaration: BaseVariableLikeDeclaration,
        dot_dot_dot_token: Option<Gc<Node>>,
        question_token: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _variable_like_declaration: base_variable_like_declaration,
            dot_dot_dot_token,
            question_token,
        }
    }
}

pub trait HasDotDotDotTokenInterface {
    fn maybe_dot_dot_dot_token(&self) -> Option<Gc<Node>>;
}

impl HasDotDotDotTokenInterface for ParameterDeclaration {
    fn maybe_dot_dot_dot_token(&self) -> Option<Gc<Node>> {
        self.dot_dot_dot_token.clone()
    }
}

impl HasQuestionTokenInterface for ParameterDeclaration {
    fn maybe_question_token(&self) -> Option<Gc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ImportTypeNode {
    _node: BaseNode,
    type_arguments: GcCell<Option<Gc<NodeArray /*<TypeNode>*/>>>,
    #[unsafe_ignore_trace]
    is_type_of: Cell<bool>,
    pub argument: Gc<Node /*<TypeNode>*/>,
    pub qualifier: Option<Gc<Node /*<EntityName>*/>>,
}

impl ImportTypeNode {
    pub fn new(
        base_node: BaseNode,
        argument: Gc<Node>,
        qualifier: Option<Gc<Node>>,
        type_arguments: Option<Gc<NodeArray>>,
        is_type_of: bool,
    ) -> Self {
        Self {
            _node: base_node,
            type_arguments: GcCell::new(type_arguments),
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
    fn maybe_type_arguments(&self) -> Option<Gc<NodeArray>> {
        self.type_arguments.borrow().clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ThisTypeNode {
    _node: BaseNode,
}

impl ThisTypeNode {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
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
    // TODO: changed this from Option<&NodeArray> to Ref<Option<NodeArray>> (and changed everything
    // except Identifier to have an unnecessary RefCell wrapper) because Identifier needs to mutate
    // its type_arguments, don't know if there's "a better way"?
    fn maybe_type_arguments(&self) -> Option<Gc<NodeArray>>;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TypeReferenceNode {
    _node: BaseNode,
    pub type_name: Gc<Node /*EntityName*/>,
    type_arguments: GcCell<Option<Gc<NodeArray /*<TypeNode>*/>>>,
}

impl TypeReferenceNode {
    pub fn new(
        base_node: BaseNode,
        type_name: Gc<Node>,
        type_arguments: Option<Gc<NodeArray>>,
    ) -> Self {
        Self {
            _node: base_node,
            type_name,
            type_arguments: GcCell::new(type_arguments),
        }
    }
}

impl HasTypeArgumentsInterface for TypeReferenceNode {
    fn maybe_type_arguments(&self) -> Option<Gc<NodeArray>> {
        self.type_arguments.borrow().clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TypePredicateNode {
    _node: BaseNode,
    pub asserts_modifier: Option<Gc<Node /*AssertsToken*/>>,
    pub parameter_name: Gc<Node /*Identifier | ThisTypeNode*/>,
    pub type_: Option<Gc<Node /*TypeNode*/>>,
}

impl TypePredicateNode {
    pub fn new(
        base_node: BaseNode,
        asserts_modifier: Option<Gc<Node>>,
        parameter_name: Gc<Node>,
        type_: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            asserts_modifier,
            parameter_name,
            type_,
        }
    }
}

impl HasTypeInterface for TypePredicateNode {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_;
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TypeQueryNode {
    _node: BaseNode,
    pub expr_name: Gc<Node /*EntityName*/>,
}

impl TypeQueryNode {
    pub fn new(base_node: BaseNode, expr_name: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expr_name,
        }
    }
}

pub trait HasMembersInterface {
    fn members(&self) -> Gc<NodeArray>;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TypeLiteralNode {
    _node: BaseNode,
    pub members: Gc<NodeArray>, /*<TypeElement>*/
}

impl TypeLiteralNode {
    pub fn new(base_node: BaseNode, members: Gc<NodeArray>) -> Self {
        Self {
            _node: base_node,
            members,
        }
    }
}

impl HasMembersInterface for TypeLiteralNode {
    fn members(&self) -> Gc<NodeArray> {
        self.members.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ArrayTypeNode {
    _node: BaseNode,
    pub element_type: Gc<Node /*TypeNode*/>,
}

impl ArrayTypeNode {
    pub fn new(base_node: BaseNode, element_type: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            element_type,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TupleTypeNode {
    _node: BaseNode,
    pub elements: Gc<NodeArray /*<TypeNode | NamedTupleMember>*/>,
}

impl TupleTypeNode {
    pub fn new(base_node: BaseNode, elements: Gc<NodeArray>) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

impl HasElementsInterface for TupleTypeNode {
    fn elements(&self) -> Gc<NodeArray> {
        self.elements.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct NamedTupleMember {
    _node: BaseNode,
    pub dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
    pub name: Gc<Node /*Identifier*/>,
    pub question_token: Option<Gc<Node /*QuestionToken*/>>,
    pub type_: Gc<Node /*TypeNode*/>,
}

impl NamedTupleMember {
    pub fn new(
        base_node: BaseNode,
        dot_dot_dot_token: Option<Gc<Node>>,
        name: Gc<Node>,
        question_token: Option<Gc<Node>>,
        type_: Gc<Node>,
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
    fn maybe_type(&self) -> Option<Gc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

impl HasDotDotDotTokenInterface for NamedTupleMember {
    fn maybe_dot_dot_dot_token(&self) -> Option<Gc<Node>> {
        self.dot_dot_dot_token.clone()
    }
}

impl HasQuestionTokenInterface for NamedTupleMember {
    fn maybe_question_token(&self) -> Option<Gc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct OptionalTypeNode {
    _node: BaseNode,
    pub type_: Gc<Node /*TypeNode*/>,
}

impl OptionalTypeNode {
    pub fn new(base_node: BaseNode, type_: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for OptionalTypeNode {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct RestTypeNode {
    _node: BaseNode,
    pub type_: Gc<Node /*TypeNode*/>,
}

impl RestTypeNode {
    pub fn new(base_node: BaseNode, type_: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for RestTypeNode {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

pub trait UnionOrIntersectionTypeNodeInterface {
    fn types(&self) -> Gc<NodeArray>;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct UnionTypeNode {
    _node: BaseNode,
    pub types: Gc<NodeArray /*<TypeNode>*/>,
}

impl UnionTypeNode {
    pub fn new(base_node: BaseNode, types: Gc<NodeArray>) -> Self {
        Self {
            _node: base_node,
            types,
        }
    }
}

impl UnionOrIntersectionTypeNodeInterface for UnionTypeNode {
    fn types(&self) -> Gc<NodeArray> {
        self.types.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct IntersectionTypeNode {
    _node: BaseNode,
    pub types: Gc<NodeArray /*<TypeNode>*/>,
}

impl IntersectionTypeNode {
    pub fn new(base_node: BaseNode, types: Gc<NodeArray>) -> Self {
        Self {
            _node: base_node,
            types,
        }
    }
}

impl UnionOrIntersectionTypeNodeInterface for IntersectionTypeNode {
    fn types(&self) -> Gc<NodeArray> {
        self.types.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ConditionalTypeNode {
    _node: BaseNode,
    pub check_type: Gc<Node /*TypeNode*/>,
    pub extends_type: Gc<Node /*TypeNode*/>,
    pub true_type: Gc<Node /*TypeNode*/>,
    pub false_type: Gc<Node /*TypeNode*/>,
}

impl ConditionalTypeNode {
    pub fn new(
        base_node: BaseNode,
        check_type: Gc<Node>,
        extends_type: Gc<Node>,
        true_type: Gc<Node>,
        false_type: Gc<Node>,
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

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct InferTypeNode {
    _node: BaseNode,
    pub type_parameter: Gc<Node /*TypeParameterDeclaration*/>,
}

impl InferTypeNode {
    pub fn new(base_node: BaseNode, type_parameter: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            type_parameter,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ParenthesizedTypeNode {
    _node: BaseNode,
    pub type_: Gc<Node /*TypeNode*/>,
}

impl ParenthesizedTypeNode {
    pub fn new(base_node: BaseNode, type_: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for ParenthesizedTypeNode {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TypeOperatorNode {
    _node: BaseNode,
    #[unsafe_ignore_trace]
    pub operator: SyntaxKind, /*SyntaxKind.KeyOfKeyword | SyntaxKind.UniqueKeyword | SyntaxKind.ReadonlyKeyword*/
    pub type_: Gc<Node /*TypeNode*/>,
}

impl TypeOperatorNode {
    pub fn new(base_node: BaseNode, operator: SyntaxKind, type_: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            operator,
            type_,
        }
    }
}

impl HasTypeInterface for TypeOperatorNode {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct IndexedAccessTypeNode {
    _node: BaseNode,
    pub object_type: Gc<Node /*TypeNode*/>,
    pub index_type: Gc<Node /*TypeNode*/>,
}

impl IndexedAccessTypeNode {
    pub fn new(base_node: BaseNode, object_type: Gc<Node>, index_type: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            object_type,
            index_type,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct MappedTypeNode {
    _node: BaseNode,
    pub readonly_token: Option<Gc<Node /*ReadonlyToken | PlusToken | MinusToken*/>>,
    pub type_parameter: Gc<Node /*TypeParameterDeclaration*/>,
    pub name_type: Option<Gc<Node /*TypeNode*/>>,
    pub question_token: Option<Gc<Node /*QuestionToken | PlusToken | MinusToken*/>>,
    pub type_: Option<Gc<Node /*TypeNode*/>>,
    pub members: Option<Gc<NodeArray /*<TypeElement>*/>>,
}

impl MappedTypeNode {
    pub fn new(
        base_node: BaseNode,
        readonly_token: Option<Gc<Node>>,
        type_parameter: Gc<Node>,
        name_type: Option<Gc<Node>>,
        question_token: Option<Gc<Node>>,
        type_: Option<Gc<Node>>,
        members: Option<Gc<NodeArray>>,
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
    fn maybe_type(&self) -> Option<Gc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_;
    }
}

impl HasQuestionTokenInterface for MappedTypeNode {
    fn maybe_question_token(&self) -> Option<Gc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct LiteralTypeNode {
    _node: BaseNode,
    pub literal: Gc<Node>,
}

impl LiteralTypeNode {
    pub fn new(base_node: BaseNode, literal: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            literal,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "LiteralLikeNodeInterface")]
pub struct StringLiteral {
    _literal_like_node: BaseLiteralLikeNode,
    pub(crate) text_source_node:
        Option<Gc<Node /*Identifier | StringLiteralLike | NumericLiteral*/>>,
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

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TemplateLiteralTypeNode {
    pub _node: BaseNode,
    pub head: Gc<Node /*TemplateHead*/>,
    pub template_spans: Gc<NodeArray /*<TemplateLiteralTypeSpan>*/>,
}

impl TemplateLiteralTypeNode {
    pub fn new(base_node: BaseNode, head: Gc<Node>, template_spans: Gc<NodeArray>) -> Self {
        Self {
            _node: base_node,
            head,
            template_spans,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TemplateLiteralTypeSpan {
    pub _node: BaseNode,
    pub type_: Gc<Node /*TypeNode*/>,
    pub literal: Gc<Node /*TemplateMiddle | TemplateTail*/>,
}

impl TemplateLiteralTypeSpan {
    pub fn new(base_node: BaseNode, type_: Gc<Node>, literal: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
            literal,
        }
    }
}

impl HasTypeInterface for TemplateLiteralTypeSpan {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct OmittedExpression {
    pub _node: BaseNode,
}

impl OmittedExpression {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct PartiallyEmittedExpression {
    pub _node: BaseNode,
    pub expression: Gc<Node /*Expression*/>,
}

impl PartiallyEmittedExpression {
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for PartiallyEmittedExpression {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

pub trait UnaryExpressionInterface {
    fn operator(&self) -> SyntaxKind;
    fn operand(&self) -> Gc<Node>;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct PrefixUnaryExpression {
    pub _node: BaseNode,
    #[unsafe_ignore_trace]
    pub operator: SyntaxKind, /*PrefixUnaryOperator*/
    pub operand: Gc<Node /*UnaryExpression*/>,
}

impl PrefixUnaryExpression {
    pub fn new(base_node: BaseNode, operator: SyntaxKind, operand: Gc<Node>) -> Self {
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
    fn operand(&self) -> Gc<Node> {
        self.operand.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct PostfixUnaryExpression {
    pub _node: BaseNode,
    pub operand: Gc<Node /*LeftHandSideExpression*/>,
    #[unsafe_ignore_trace]
    pub operator: SyntaxKind, /*PostfixUnaryOperator*/
}

impl PostfixUnaryExpression {
    pub fn new(base_node: BaseNode, operand: Gc<Node>, operator: SyntaxKind) -> Self {
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
    fn operand(&self) -> Gc<Node> {
        self.operand.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct DeleteExpression {
    _node: BaseNode,
    pub expression: Gc<Node /*UnaryExpression*/>,
}

impl DeleteExpression {
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for DeleteExpression {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct TypeOfExpression {
    _node: BaseNode,
    pub expression: Gc<Node /*UnaryExpression*/>,
}

impl TypeOfExpression {
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for TypeOfExpression {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct VoidExpression {
    _node: BaseNode,
    pub expression: Gc<Node /*UnaryExpression*/>,
}

impl VoidExpression {
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for VoidExpression {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct AwaitExpression {
    _node: BaseNode,
    pub expression: Gc<Node /*UnaryExpression*/>,
}

impl AwaitExpression {
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for AwaitExpression {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct YieldExpression {
    _node: BaseNode,
    pub asterisk_token: Option<Gc<Node /*AsteriskToken*/>>,
    pub expression: Option<Gc<Node /*Expression*/>>,
}

impl YieldExpression {
    pub fn new(
        base_node: BaseNode,
        expression: Option<Gc<Node>>,
        asterisk_token: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            asterisk_token,
        }
    }
}

impl HasExpressionInterface for YieldExpression {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone().unwrap()
    }

    fn maybe_expression(&self) -> Option<Gc<Node>> {
        self.expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct SyntheticExpression {
    _node: BaseNode,
    pub is_spread: bool,
    pub type_: Gc<Type>,
    pub tuple_name_source: Option<Gc<Node /*ParameterDeclaration | NamedTupleMember*/>>,
}

impl SyntheticExpression {
    pub fn new(
        base_node: BaseNode,
        is_spread: bool,
        type_: Gc<Type>,
        tuple_name_source: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            is_spread,
            type_,
            tuple_name_source,
        }
    }
}
