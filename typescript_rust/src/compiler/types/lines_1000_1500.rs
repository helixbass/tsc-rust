use std::{
    cell::{Cell, RefCell},
    ops::Deref,
    slice,
};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRefMut, Trace};
use id_arena::{Arena, Id};
use local_macros::{ast_type, enum_unwrapped};

use super::{
    BaseGenericNamedDeclaration, BaseNode, FlowNode, HasExpressionInterface,
    HasInitializerInterface, HasLeftAndRightInterface, HasQuestionTokenInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, Node, NodeInterface,
    ReadonlyTextRange, SyntaxKind, TransformFlags, __String, *,
};
use crate::set_text_range_node_array;

#[derive(Clone)]
pub struct NodeArray {
    id: Id<Self>,
    _nodes: Vec<Id<Node>>,
    pos: isize,
    end: isize,
    pub has_trailing_comma: bool,
    transform_flags: Option<TransformFlags>,
    is_missing_list: bool,
}

impl std::fmt::Debug for NodeArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NodeArray")
            // .field("_rc_wrapper", &self._rc_wrapper)
            .field("_nodes", &self._nodes)
            .field("pos", &self.pos)
            .field("end", &self.end)
            .field("has_trailing_comma", &self.has_trailing_comma)
            .field("transform_flags", &self.transform_flags)
            .field("is_missing_list", &self.is_missing_list)
            .finish()
    }
}

impl NodeArray {
    pub fn new(
        arena: &RefCell<Arena<Self>>,
        nodes: Vec<Id<Node>>,
        pos: isize,
        end: isize,
        has_trailing_comma: bool,
        transform_flags: Option<TransformFlags>,
    ) -> Id<Self> {
        arena.borrow_mut().alloc_with_id(|id| Self {
            _nodes: nodes,
            id,
            pos,
            end,
            has_trailing_comma,
            transform_flags,
            is_missing_list: Default::default(),
        })
    }

    pub fn iter(&self) -> NodeArrayIter {
        NodeArrayIter(self._nodes.iter())
    }

    pub fn owned_iter(&self) -> NodeArrayOwnedIter {
        self.rc_wrapper().into()
    }

    pub fn to_vec(&self) -> Vec<Gc<Node>> {
        self._nodes.clone()
    }

    pub fn maybe_transform_flags(&self) -> Option<TransformFlags> {
        self.transform_flags
    }

    pub fn set_transform_flags(&mut self, transform_flags: Option<TransformFlags>) {
        self.transform_flags = transform_flags;
    }

    pub fn is_missing_list(&self) -> bool {
        self.is_missing_list
    }

    pub fn set_is_missing_list(&mut self, is_missing_list: bool) {
        self.is_missing_list = is_missing_list;
    }
}

impl ReadonlyTextRange for NodeArray {
    fn pos(&self) -> isize {
        self.pos
    }

    fn set_pos(&mut self, pos: isize) {
        self.pos = pos;
    }

    fn end(&self) -> isize {
        self.end
    }

    fn set_end(&mut self, end: isize) {
        self.end = end;
    }
}

// impl Default for NodeArray {
//     fn default() -> Self {
//         Self::new(vec![])
//     }
// }

impl From<&NodeArray> for Vec<Gc<Node>> {
    fn from(node_array: &NodeArray) -> Self {
        node_array._nodes.clone()
    }
}

impl<'node_array> From<&'node_array NodeArray> for &'node_array [Gc<Node>] {
    fn from(node_array: &'node_array NodeArray) -> Self {
        &node_array._nodes
    }
}

#[derive(Clone)]
pub struct NodeArrayIter<'node_array>(slice::Iter<'node_array, Gc<Node>>);

impl<'node_array> Iterator for NodeArrayIter<'node_array> {
    type Item = &'node_array Gc<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'node_array> IntoIterator for &'node_array NodeArray {
    type Item = &'node_array Gc<Node>;
    type IntoIter = NodeArrayIter<'node_array>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Clone, Trace, Finalize)]
pub struct NodeArrayOwnedIter {
    node_array: Gc<NodeArray>,
    index: usize,
}

impl From<Gc<NodeArray>> for NodeArrayOwnedIter {
    fn from(value: Gc<NodeArray>) -> Self {
        Self {
            node_array: value,
            index: 0,
        }
    }
}

impl Iterator for NodeArrayOwnedIter {
    type Item = Gc<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.node_array.len() {
            None
        } else {
            let ret = self.node_array[self.index].clone();
            self.index += 1;
            Some(ret)
        }
    }
}

impl Deref for NodeArray {
    type Target = [Gc<Node>];

    fn deref(&self) -> &Self::Target {
        &self._nodes
    }
}

pub trait NodeArrayExt {
    fn set_text_range(self, location: Option<&impl ReadonlyTextRange>) -> Self;
}

impl NodeArrayExt for Gc<NodeArray> {
    fn set_text_range(self, location: Option<&impl ReadonlyTextRange>) -> Self {
        set_text_range_node_array(self, location)
    }
}

#[derive(Clone, Debug)]
pub enum NodeArrayOrVec {
    NodeArray(Id<NodeArray>),
    Vec(Vec<Id<Node>>),
}

impl NodeArrayOrVec {
    pub fn as_vec_owned(self) -> Vec<Id<Node>> {
        enum_unwrapped!(self, [NodeArrayOrVec, Vec])
    }

    pub fn as_node_array(self) -> Id<NodeArray> {
        enum_unwrapped!(self, [NodeArrayOrVec, NodeArray])
    }
}

use gc::GcCellRef;

impl From<Id<NodeArray>> for NodeArrayOrVec {
    fn from(node_array: Id<NodeArray>) -> Self {
        NodeArrayOrVec::NodeArray(node_array)
    }
}

impl From<Vec<Id<Node>>> for NodeArrayOrVec {
    fn from(vec: Vec<Id<Node>>) -> Self {
        NodeArrayOrVec::Vec(vec)
    }
}

impl Deref for NodeArrayOrVec {
    type Target = [Gc<Node>];

    fn deref(&self) -> &Self::Target {
        match self {
            NodeArrayOrVec::NodeArray(node_array) => node_array.deref(),
            NodeArrayOrVec::Vec(vec) => vec.deref(),
        }
    }
}

impl From<&NodeArrayOrVec> for Vec<Gc<Node>> {
    fn from(value: &NodeArrayOrVec) -> Vec<Gc<Node>> {
        match value {
            NodeArrayOrVec::NodeArray(value) => value.to_vec(),
            NodeArrayOrVec::Vec(value) => value.clone(),
        }
    }
}

impl IntoIterator for NodeArrayOrVec {
    type Item = Gc<Node>;
    type IntoIter = <Vec<Gc<Node>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::NodeArray(value) => value.to_vec().into_iter(),
            Self::Vec(value) => value.into_iter(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum RcNodeOrNodeArrayOrVec {
    RcNode(Gc<Node>),
    NodeArray(Gc<NodeArray>),
    Vec(Vec<Gc<Node>>),
}

impl From<Gc<Node>> for RcNodeOrNodeArrayOrVec {
    fn from(value: Gc<Node>) -> Self {
        Self::RcNode(value)
    }
}

impl From<Gc<NodeArray>> for RcNodeOrNodeArrayOrVec {
    fn from(value: Gc<NodeArray>) -> Self {
        Self::NodeArray(value)
    }
}

impl From<Vec<Gc<Node>>> for RcNodeOrNodeArrayOrVec {
    fn from(value: Vec<Gc<Node>>) -> Self {
        Self::Vec(value)
    }
}

bitflags! {
    #[derive(Default)]
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

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct Identifier {
    _node: BaseNode,
    #[unsafe_ignore_trace]
    pub escaped_text: __String,
    #[unsafe_ignore_trace]
    pub original_keyword_kind: Option<SyntaxKind>,
    #[unsafe_ignore_trace]
    pub(crate) auto_generate_flags: Cell<Option<GeneratedIdentifierFlags>>,
    pub(crate) auto_generate_id: Option<usize>,
    generated_import_reference: GcCell<Option<Gc<Node /*ImportSpecifier*/>>>,
    #[unsafe_ignore_trace]
    is_in_jsdoc_namespace: Cell<Option<bool>>,
    type_arguments: GcCell<Option<Gc<NodeArray> /*<TypeNode | TypeParameterDeclaration>*/>>,
    #[unsafe_ignore_trace]
    jsdoc_dot_pos: Cell<Option<isize>>,
}

impl Identifier {
    pub fn new(base_node: BaseNode, escaped_text: __String) -> Self {
        Self {
            _node: base_node,
            escaped_text,
            original_keyword_kind: Default::default(),
            auto_generate_flags: Default::default(),
            auto_generate_id: Default::default(),
            generated_import_reference: Default::default(),
            is_in_jsdoc_namespace: Default::default(),
            type_arguments: Default::default(),
            jsdoc_dot_pos: Default::default(),
        }
    }

    pub fn maybe_auto_generate_flags(&self) -> Option<GeneratedIdentifierFlags> {
        self.auto_generate_flags.get()
    }

    pub fn auto_generate_flags(&self) -> GeneratedIdentifierFlags {
        self.auto_generate_flags.get().unwrap()
    }

    pub fn set_auto_generate_flags(&self, auto_generate_flags: Option<GeneratedIdentifierFlags>) {
        self.auto_generate_flags.set(auto_generate_flags);
    }

    pub fn maybe_generated_import_reference(&self) -> GcCellRef<Option<Gc<Node>>> {
        self.generated_import_reference.borrow()
    }

    pub fn set_generated_import_reference(&self, generated_import_reference: Option<Gc<Node>>) {
        *self.generated_import_reference.borrow_mut() = generated_import_reference;
    }

    pub fn maybe_is_in_jsdoc_namespace(&self) -> Option<bool> {
        self.is_in_jsdoc_namespace.get()
    }

    pub fn set_is_in_jsdoc_namespace(&self, is_in_jsdoc_namespace: Option<bool>) {
        self.is_in_jsdoc_namespace.set(is_in_jsdoc_namespace);
    }

    pub fn maybe_type_arguments_mut(&self) -> GcCellRefMut<Option<Gc<NodeArray>>> {
        self.type_arguments.borrow_mut()
    }
}

impl MemberNameInterface for Identifier {
    fn escaped_text(&self) -> &str /*__String*/ {
        &self.escaped_text
    }
}

pub trait HasJSDocDotPosInterface {
    fn maybe_jsdoc_dot_pos(&self) -> Option<isize>;
    fn set_jsdoc_dot_pos(&self, jsdoc_dot_pos: Option<isize>);
}

impl HasJSDocDotPosInterface for Identifier {
    fn maybe_jsdoc_dot_pos(&self) -> Option<isize> {
        self.jsdoc_dot_pos.get()
    }

    fn set_jsdoc_dot_pos(&self, jsdoc_dot_pos: Option<isize>) {
        self.jsdoc_dot_pos.set(jsdoc_dot_pos);
    }
}

impl HasTypeArgumentsInterface for Identifier {
    fn maybe_type_arguments(&self) -> Option<Gc<NodeArray>> {
        self.type_arguments.borrow().clone()
    }
}

pub type ModifiersArray = Id<NodeArray>; /*<Modifier>*/

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct QualifiedName {
    _node: BaseNode,
    pub left: Gc<Node /*EntityName*/>,
    pub right: Gc<Node /*Identifier*/>,
    #[unsafe_ignore_trace]
    jsdoc_dot_pos: Cell<Option<isize>>,
}

impl QualifiedName {
    pub fn new(base_node: BaseNode, left: Gc<Node>, right: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            left,
            right,
            jsdoc_dot_pos: Cell::new(None),
        }
    }
}

impl HasJSDocDotPosInterface for QualifiedName {
    fn maybe_jsdoc_dot_pos(&self) -> Option<isize> {
        self.jsdoc_dot_pos.get()
    }

    fn set_jsdoc_dot_pos(&self, jsdoc_dot_pos: Option<isize>) {
        self.jsdoc_dot_pos.set(jsdoc_dot_pos);
    }
}

impl HasLeftAndRightInterface for QualifiedName {
    fn left(&self) -> Gc<Node> {
        self.left.clone()
    }

    fn right(&self) -> Gc<Node> {
        self.right.clone()
    }
}

pub trait MemberNameInterface: NodeInterface {
    fn escaped_text(&self) -> &str /*__String*/;
}

pub trait NamedDeclarationInterface: NodeInterface {
    fn maybe_name(&self) -> Option<Gc<Node>>;
    fn name(&self) -> Gc<Node>;
    fn set_name(&mut self, name: Gc<Node>);
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(impl_from = false)]
pub struct BaseNamedDeclaration {
    _node: BaseNode,
    name: Option<Gc<Node>>,
}

impl BaseNamedDeclaration {
    pub fn new(base_node: BaseNode, name: Option<Gc<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
        }
    }
}

impl NamedDeclarationInterface for BaseNamedDeclaration {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Gc<Node> {
        self.name.as_ref().unwrap().clone()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = Some(name);
    }
}

pub trait BindingLikeDeclarationInterface:
    NamedDeclarationInterface + HasInitializerInterface
{
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(impl_from = false, interfaces = "NamedDeclarationInterface")]
pub struct BaseBindingLikeDeclaration {
    _named_declaration: BaseNamedDeclaration,
    initializer: Option<Gc<Node>>,
}

impl BaseBindingLikeDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        initializer: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            initializer,
        }
    }
}

impl HasInitializerInterface for BaseBindingLikeDeclaration {
    fn maybe_initializer(&self) -> Option<Gc<Node>> {
        self.initializer.as_ref().map(Clone::clone)
    }

    fn set_initializer(&mut self, initializer: Gc<Node>) {
        self.initializer = Some(initializer);
    }
}

impl BindingLikeDeclarationInterface for BaseBindingLikeDeclaration {}

pub trait VariableLikeDeclarationInterface:
    BindingLikeDeclarationInterface + HasTypeInterface
{
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasInitializerInterface, BindingLikeDeclarationInterface"
)]
pub struct BaseVariableLikeDeclaration {
    _binding_like_declaration: BaseBindingLikeDeclaration,
    type_: Option<Gc<Node>>,
}

impl BaseVariableLikeDeclaration {
    pub fn new(
        base_binding_like_declaration: BaseBindingLikeDeclaration,
        type_: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _binding_like_declaration: base_binding_like_declaration,
            type_,
        }
    }
}

impl HasTypeInterface for BaseVariableLikeDeclaration {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        self.type_.as_ref().map(Clone::clone)
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_;
    }
}

impl VariableLikeDeclarationInterface for BaseVariableLikeDeclaration {}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ComputedPropertyName {
    _node: BaseNode,
    pub expression: Gc<Node /*Expression*/>,
}

impl ComputedPropertyName {
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ComputedPropertyName {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct PrivateIdentifier {
    _node: BaseNode,
    #[unsafe_ignore_trace]
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

impl MemberNameInterface for PrivateIdentifier {
    fn escaped_text(&self) -> &str /*__String*/ {
        &self.escaped_text
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct Decorator {
    _node: BaseNode,
    pub expression: Gc<Node /*LeftHandSideExpression*/>,
}

impl Decorator {
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for Decorator {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct TypeParameterDeclaration {
    _named_declaration: BaseNamedDeclaration,
    pub constraint: Option<Gc<Node /*TypeNode*/>>,
    pub default: Option<Gc<Node /*TypeNode*/>>,
    pub expression: Option<Gc<Node /*Expression*/>>,
}

impl TypeParameterDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        constraint: Option<Gc<Node>>,
        default: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            constraint,
            default,
            expression: None,
        }
    }
}

impl HasExpressionInterface for TypeParameterDeclaration {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone().unwrap()
    }

    fn maybe_expression(&self) -> Option<Gc<Node>> {
        self.expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, HasTypeParametersInterface"
)]
pub enum SignatureDeclarationBase {
    FunctionLikeDeclarationBase(FunctionLikeDeclarationBase),
}

pub trait SignatureDeclarationInterface:
    NamedDeclarationInterface + HasTypeInterface + HasTypeParametersInterface
{
    fn parameters(&self) -> Gc<NodeArray> /*<ParameterDeclaration>*/;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct BaseSignatureDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration,
    parameters: Gc<NodeArray>, /*<ParameterDeclaration>*/
    type_: Option<Gc<Node /*TypeNode*/>>,
    // TODO
    // /* @internal */ typeArguments?: NodeArray<TypeNode>;
}

impl BaseSignatureDeclaration {
    pub fn new(
        generic_named_declaration: BaseGenericNamedDeclaration,
        parameters: Gc<NodeArray>,
        type_: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _generic_named_declaration: generic_named_declaration,
            parameters,
            type_,
        }
    }
}

impl HasTypeInterface for BaseSignatureDeclaration {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_;
    }
}

impl SignatureDeclarationInterface for BaseSignatureDeclaration {
    fn parameters(&self) -> Gc<NodeArray> {
        self.parameters.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    ancestors = "SignatureDeclarationBase",
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface, HasQuestionTokenInterface"
)]
pub enum FunctionLikeDeclarationBase {
    BaseFunctionLikeDeclaration(BaseFunctionLikeDeclaration),
}

pub trait FunctionLikeDeclarationInterface:
    SignatureDeclarationInterface + HasQuestionTokenInterface
{
    fn maybe_body(&self) -> Option<Gc<Node>>;
    fn maybe_asterisk_token(&self) -> Option<Gc<Node>>;
    fn maybe_exclamation_token(&self) -> GcCellRefMut<Option<Gc<Node>>>;
    fn maybe_end_flow_node(&self) -> Option<Gc<FlowNode>>;
    fn set_end_flow_node(&self, end_flow_node: Option<Gc<FlowNode>>);
    fn maybe_return_flow_node(&self) -> Option<Gc<FlowNode>>;
    fn set_return_flow_node(&self, return_flow_node: Option<Gc<FlowNode>>);
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    ancestors = "FunctionLikeDeclarationBase, SignatureDeclarationBase",
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface"
)]
pub struct BaseFunctionLikeDeclaration {
    _signature_declaration: BaseSignatureDeclaration,
    pub asterisk_token: Option<Gc<Node /*AsteriskToken*/>>,
    pub question_token: Option<Gc<Node /*QuestionToken*/>>,
    pub exclamation_token: GcCell<Option<Gc<Node /*ExclamationToken*/>>>,
    body: Option<Gc<Node /*Block | Expression*/>>,
    end_flow_node: GcCell<Option<Gc<FlowNode>>>,
    return_flow_node: GcCell<Option<Gc<FlowNode>>>,
}

impl BaseFunctionLikeDeclaration {
    pub fn new(signature_declaration: BaseSignatureDeclaration, body: Option<Gc<Node>>) -> Self {
        Self {
            _signature_declaration: signature_declaration,
            body,
            asterisk_token: None,
            question_token: None,
            exclamation_token: GcCell::new(None),
            end_flow_node: GcCell::new(None),
            return_flow_node: GcCell::new(None),
        }
    }
}

impl FunctionLikeDeclarationInterface for BaseFunctionLikeDeclaration {
    fn maybe_body(&self) -> Option<Gc<Node>> {
        self.body.clone()
    }

    fn maybe_asterisk_token(&self) -> Option<Gc<Node>> {
        self.asterisk_token.clone()
    }

    fn maybe_exclamation_token(&self) -> GcCellRefMut<Option<Gc<Node>>> {
        self.exclamation_token.borrow_mut()
    }

    fn maybe_end_flow_node(&self) -> Option<Gc<FlowNode>> {
        self.end_flow_node.borrow().clone()
    }

    fn set_end_flow_node(&self, end_flow_node: Option<Gc<FlowNode>>) {
        *self.end_flow_node.borrow_mut() = end_flow_node;
    }

    fn maybe_return_flow_node(&self) -> Option<Gc<FlowNode>> {
        self.return_flow_node.borrow().clone()
    }

    fn set_return_flow_node(&self, return_flow_node: Option<Gc<FlowNode>>) {
        *self.return_flow_node.borrow_mut() = return_flow_node;
    }
}

impl HasQuestionTokenInterface for BaseFunctionLikeDeclaration {
    fn maybe_question_token(&self) -> Option<Gc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface, HasQuestionTokenInterface"
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

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface"
)]
pub struct MethodSignature {
    _signature_declaration: BaseSignatureDeclaration,
    pub question_token: Option<Gc<Node /*QuestionToken*/>>,
}

impl MethodSignature {
    pub fn new(
        signature_declaration: BaseSignatureDeclaration,
        question_token: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _signature_declaration: signature_declaration,
            question_token,
        }
    }
}

impl HasQuestionTokenInterface for MethodSignature {
    fn maybe_question_token(&self) -> Option<Gc<Node>> {
        self.question_token.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface, HasQuestionTokenInterface"
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
