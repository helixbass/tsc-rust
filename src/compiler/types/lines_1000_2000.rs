#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, RefCell};
use std::ops::Deref;
use std::rc::Rc;

use super::{
    ArrayLiteralExpression, AsExpression, BaseGenericNamedDeclaration, BaseJSDocUnaryType,
    BaseLiteralLikeNode, BaseNode, BinaryExpression, CallExpression, ConditionalExpression,
    ElementAccessExpression, FlowNode, FunctionExpression, HasExpressionInterface,
    HasInitializerInterface, HasTypeInterface, JSDocTypeExpression, NewExpression, Node,
    NodeInterface, NonNullExpression, ObjectLiteralExpression, ParenthesizedExpression,
    PropertyAccessExpression, ReadonlyTextRange, SpreadElement, SyntaxKind,
    TaggedTemplateExpression, TemplateExpression, TransformFlags, TypeAssertion, VoidExpression,
    __String,
};
use local_macros::ast_type;

#[derive(Clone, Debug)]
pub struct NodeArray {
    _nodes: Vec<Rc<Node>>,
    pos: Cell<isize>,
    end: Cell<isize>,
    pub has_trailing_comma: bool,
    pub(crate) transform_flags: Option<TransformFlags>,
    pub is_missing_list: bool,
}

impl NodeArray {
    pub fn new(
        nodes: Vec<Rc<Node>>,
        pos: isize,
        end: isize,
        has_trailing_comma: bool,
        transform_flags: Option<TransformFlags>,
    ) -> Self {
        NodeArray {
            _nodes: nodes,
            pos: Cell::new(pos),
            end: Cell::new(end),
            has_trailing_comma,
            transform_flags,
            is_missing_list: false,
        }
    }

    pub fn iter(&self) -> NodeArrayIter {
        NodeArrayIter(Box::new(self._nodes.iter()))
    }

    pub fn len(&self) -> usize {
        self._nodes.len()
    }

    pub fn to_vec(&self) -> Vec<Rc<Node>> {
        self._nodes.clone()
    }
}

impl ReadonlyTextRange for NodeArray {
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

// impl Default for NodeArray {
//     fn default() -> Self {
//         Self::new(vec![])
//     }
// }

impl From<&NodeArray> for Vec<Rc<Node>> {
    fn from(node_array: &NodeArray) -> Self {
        node_array._nodes.clone()
    }
}

impl<'node_array> From<&'node_array NodeArray> for &'node_array [Rc<Node>] {
    fn from(node_array: &'node_array NodeArray) -> Self {
        &node_array._nodes
    }
}

pub struct NodeArrayIter<'node_array>(
    Box<dyn Iterator<Item = &'node_array Rc<Node>> + 'node_array>,
);

impl<'node_array> Iterator for NodeArrayIter<'node_array> {
    type Item = &'node_array Rc<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'node_array> IntoIterator for &'node_array NodeArray {
    type Item = &'node_array Rc<Node>;
    type IntoIter = NodeArrayIter<'node_array>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl Deref for NodeArray {
    type Target = [Rc<Node>];

    fn deref(&self) -> &Self::Target {
        &self._nodes
    }
}

#[derive(Clone, Debug)]
pub enum NodeArrayOrVec {
    NodeArray(NodeArray),
    Vec(Vec<Rc<Node>>),
}

impl From<NodeArray> for NodeArrayOrVec {
    fn from(node_array: NodeArray) -> Self {
        NodeArrayOrVec::NodeArray(node_array)
    }
}

impl From<Vec<Rc<Node>>> for NodeArrayOrVec {
    fn from(vec: Vec<Rc<Node>>) -> Self {
        NodeArrayOrVec::Vec(vec)
    }
}

impl Deref for NodeArrayOrVec {
    type Target = [Rc<Node>];

    fn deref(&self) -> &Self::Target {
        match self {
            NodeArrayOrVec::NodeArray(node_array) => node_array.deref(),
            NodeArrayOrVec::Vec(vec) => vec.deref(),
        }
    }
}

bitflags! {
    pub struct GeneratedIdentifierFlags: u32 {
        const None = 0;
        const Auto = 1;
        const Loop = 2;
        const Unique = 3;
        const Node = 4;
        const KindMask = 7;

        const ReservedInNestedScopes = 1 << 3;
        const Optimistic = 1 << 4;
        const FileLevel = 1 << 5;
        const AllowNameSubstitution = 1 << 6;
    }
}

#[derive(Debug)]
#[ast_type]
pub struct Identifier {
    _node: BaseNode,
    pub escaped_text: __String,
    pub original_keyword_kind: Option<SyntaxKind>,
    pub(crate) auto_generate_flags: Option<GeneratedIdentifierFlags>,
    pub(crate) auto_generate_id: Option<usize>,
    pub(crate) type_arguments: Option<NodeArray /*<TypeNode | TypeParameterDeclaration>*/>,
}

impl Identifier {
    pub fn new(base_node: BaseNode, escaped_text: __String) -> Self {
        Self {
            _node: base_node,
            escaped_text,
            original_keyword_kind: None,
            auto_generate_flags: None,
            auto_generate_id: None,
            type_arguments: None,
        }
    }

    pub fn maybe_auto_generate_flags(&self) -> Option<GeneratedIdentifierFlags> {
        self.auto_generate_flags.clone()
    }
}

impl MemberNameInterface for Identifier {
    fn escaped_text(&self) -> __String {
        self.escaped_text.clone()
    }
}

pub type ModifiersArray = NodeArray; /*<Modifier>*/

#[derive(Debug)]
#[ast_type]
pub struct QualifiedName {
    _node: BaseNode,
    pub left: Rc<Node /*EntityName*/>,
    pub right: Rc<Node /*Identifier*/>,
    pub(crate) jsdoc_dot_pos: Option<usize>,
}

impl QualifiedName {
    pub fn new(base_node: BaseNode, left: Rc<Node>, right: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            left,
            right,
            jsdoc_dot_pos: None,
        }
    }
}

pub trait MemberNameInterface: NodeInterface {
    fn escaped_text(&self) -> __String;
}

pub trait NamedDeclarationInterface: NodeInterface {
    fn maybe_name(&self) -> Option<Rc<Node>>;
    fn name(&self) -> Rc<Node>;
    fn set_name(&mut self, name: Rc<Node>);
}

#[derive(Debug)]
#[ast_type(impl_from = false)]
pub struct BaseNamedDeclaration {
    _node: BaseNode,
    name: Option<Rc<Node>>,
}

impl BaseNamedDeclaration {
    pub fn new(base_node: BaseNode, name: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
        }
    }
}

impl NamedDeclarationInterface for BaseNamedDeclaration {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Rc<Node> {
        self.name.as_ref().unwrap().clone()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = Some(name);
    }
}

pub trait BindingLikeDeclarationInterface:
    NamedDeclarationInterface + HasInitializerInterface
{
}

#[derive(Debug)]
#[ast_type(impl_from = false, interfaces = "NamedDeclarationInterface")]
pub struct BaseBindingLikeDeclaration {
    _named_declaration: BaseNamedDeclaration,
    initializer: Option<Rc<Node>>,
}

impl BaseBindingLikeDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        initializer: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            initializer,
        }
    }
}

impl HasInitializerInterface for BaseBindingLikeDeclaration {
    fn maybe_initializer(&self) -> Option<Rc<Node>> {
        self.initializer.as_ref().map(Clone::clone)
    }

    fn set_initializer(&mut self, initializer: Rc<Node>) {
        self.initializer = Some(initializer);
    }
}

impl BindingLikeDeclarationInterface for BaseBindingLikeDeclaration {}

pub trait VariableLikeDeclarationInterface:
    BindingLikeDeclarationInterface + HasTypeInterface
{
}

#[derive(Debug)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasInitializerInterface, BindingLikeDeclarationInterface"
)]
pub struct BaseVariableLikeDeclaration {
    _binding_like_declaration: BaseBindingLikeDeclaration,
    type_: Option<Rc<Node>>,
}

impl BaseVariableLikeDeclaration {
    pub fn new(
        base_binding_like_declaration: BaseBindingLikeDeclaration,
        type_: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _binding_like_declaration: base_binding_like_declaration,
            type_,
        }
    }
}

impl HasTypeInterface for BaseVariableLikeDeclaration {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        self.type_.as_ref().map(Clone::clone)
    }

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = Some(type_);
    }
}

impl VariableLikeDeclarationInterface for BaseVariableLikeDeclaration {}

#[derive(Debug)]
#[ast_type]
pub struct ComputedPropertyName {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
}

impl ComputedPropertyName {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ComputedPropertyName {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct PrivateIdentifier {
    _node: BaseNode,
    pub escaped_text: __String,
}

impl PrivateIdentifier {
    pub fn new(base_node: BaseNode, escaped_text: __String) -> Self {
        Self {
            _node: base_node,
            escaped_text,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct Decorator {
    _node: BaseNode,
    pub expression: Rc<Node /*LeftHandSideExpression*/>,
}

impl Decorator {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for Decorator {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct TypeParameterDeclaration {
    _named_declaration: BaseNamedDeclaration,
    pub constraint: Option<Rc<Node /*TypeNode*/>>,
    pub default: Option<Rc<Node /*TypeNode*/>>,
    pub expression: Option<Rc<Node /*Expression*/>>,
}

impl TypeParameterDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        constraint: Option<Rc<Node>>,
        default: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            constraint,
            default,
            expression: None,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface"
)]
pub enum SignatureDeclarationBase {
    FunctionLikeDeclarationBase(FunctionLikeDeclarationBase),
}

pub trait SignatureDeclarationInterface: NamedDeclarationInterface + HasTypeInterface {
    fn parameters(&self) -> &NodeArray /*<ParameterDeclaration>*/;
}

#[derive(Debug)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct BaseSignatureDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration,
    parameters: NodeArray, /*<ParameterDeclaration>*/
    type_: Option<Rc<Node /*TypeNode*/>>,
}

impl BaseSignatureDeclaration {
    pub fn new(
        generic_named_declaration: BaseGenericNamedDeclaration,
        parameters: NodeArray,
        type_: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _generic_named_declaration: generic_named_declaration,
            parameters,
            type_,
        }
    }
}

impl HasTypeInterface for BaseSignatureDeclaration {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = Some(type_);
    }
}

impl SignatureDeclarationInterface for BaseSignatureDeclaration {
    fn parameters(&self) -> &NodeArray {
        &self.parameters
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface"
)]
pub struct CallSignatureDeclaration {
    _signature_declaration: BaseSignatureDeclaration,
}

impl CallSignatureDeclaration {
    pub fn new(signature_declaration: BaseSignatureDeclaration) -> Self {
        Self {
            _signature_declaration: signature_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface"
)]
pub struct ConstructSignatureDeclaration {
    _signature_declaration: BaseSignatureDeclaration,
}

impl ConstructSignatureDeclaration {
    pub fn new(signature_declaration: BaseSignatureDeclaration) -> Self {
        Self {
            _signature_declaration: signature_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    ancestors = "SignatureDeclarationBase",
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface"
)]
pub enum FunctionLikeDeclarationBase {
    BaseFunctionLikeDeclaration(BaseFunctionLikeDeclaration),
}

pub trait FunctionLikeDeclarationInterface {
    fn maybe_body(&self) -> Option<Rc<Node>>;
    fn maybe_asterisk_token(&self) -> Option<Rc<Node>>;
    fn maybe_question_token(&self) -> Option<Rc<Node>>;
    fn maybe_exclamation_token(&self) -> Option<Rc<Node>>;
}

#[derive(Debug)]
#[ast_type(
    ancestors = "FunctionLikeDeclarationBase, SignatureDeclarationBase",
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface"
)]
pub struct BaseFunctionLikeDeclaration {
    _signature_declaration: BaseSignatureDeclaration,
    pub asterisk_token: Option<Rc<Node /*AsteriskToken*/>>,
    pub question_token: Option<Rc<Node /*QuestionToken*/>>,
    pub exclamation_token: Option<Rc<Node /*ExclamationToken*/>>,
    body: Option<Rc<Node /*Block | Expression*/>>,
}

impl BaseFunctionLikeDeclaration {
    pub fn new(signature_declaration: BaseSignatureDeclaration, body: Option<Rc<Node>>) -> Self {
        Self {
            _signature_declaration: signature_declaration,
            body,
            asterisk_token: None,
            question_token: None,
            exclamation_token: None,
        }
    }
}

impl FunctionLikeDeclarationInterface for BaseFunctionLikeDeclaration {
    fn maybe_body(&self) -> Option<Rc<Node>> {
        self.body.clone()
    }

    fn maybe_asterisk_token(&self) -> Option<Rc<Node>> {
        self.asterisk_token.clone()
    }

    fn maybe_question_token(&self) -> Option<Rc<Node>> {
        self.question_token.clone()
    }

    fn maybe_exclamation_token(&self) -> Option<Rc<Node>> {
        self.exclamation_token.clone()
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface"
)]
pub struct FunctionDeclaration {
    _function_like_declaration: BaseFunctionLikeDeclaration,
}

impl FunctionDeclaration {
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
pub struct MethodSignature {
    _signature_declaration: BaseSignatureDeclaration,
    pub question_token: Option<Rc<Node /*QuestionToken*/>>,
}

impl MethodSignature {
    pub fn new(
        signature_declaration: BaseSignatureDeclaration,
        question_token: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _signature_declaration: signature_declaration,
            question_token,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface"
)]
pub struct MethodDeclaration {
    _function_like_declaration: BaseFunctionLikeDeclaration,
}

impl MethodDeclaration {
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
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface"
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
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface"
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
    end_flow_node: RefCell<Option<FlowNode>>,
    return_flow_node: RefCell<Option<FlowNode>>,
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
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasInitializerInterface, BindingLikeDeclarationInterface, HasTypeInterface, VariableLikeDeclarationInterface"
)]
pub struct VariableDeclaration {
    _variable_like_declaration: BaseVariableLikeDeclaration,
}

impl VariableDeclaration {
    pub fn new(base_variable_like_declaration: BaseVariableLikeDeclaration) -> Self {
        Self {
            _variable_like_declaration: base_variable_like_declaration,
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
    pub is_type_of: bool,
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
            is_type_of,
            argument,
            qualifier,
        }
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

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = type_;
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

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = type_;
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

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = type_;
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

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = type_;
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

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = Some(type_);
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

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = type_;
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

#[derive(Debug)]
#[ast_type]
pub struct DeleteExpression {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
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
    pub expression: Rc<Node /*Expression*/>,
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
