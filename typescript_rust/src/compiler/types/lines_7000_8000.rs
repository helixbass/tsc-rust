use std::{
    any::Any,
    cell::{Cell, RefCell},
    io,
};

use bitflags::bitflags;
use derive_builder::Builder;
use local_macros::enum_unwrapped;

use super::{CompilerOptions, Diagnostic, EmitHint, Node, NodeArray, NodeArrayOrVec, SyntaxKind};
use crate::{
    BaseNodeFactory, EmitHelper, EmitHelperFactory, EmitHost, EmitResolver, HasArena,
    NodeFactoryFlags, TransformNodesTransformationResult,
};

#[derive(Default, Builder, Clone)]
#[builder(default, setter(into))]
pub struct PropertyDescriptorAttributes {
    #[builder(setter(strip_option))]
    pub enumerable: Option<BoolOrRcNode /*Expression*/>,
    #[builder(setter(strip_option))]
    pub configurable: Option<BoolOrRcNode /*Expression*/>,
    #[builder(setter(strip_option))]
    pub writable: Option<BoolOrRcNode /*Expression*/>,
    #[builder(setter(strip_option))]
    pub value: Option<Id<Node /*Expression*/>>,
    pub get: Option<Id<Node /*Expression*/>>,
    pub set: Option<Id<Node /*Expression*/>>,
}

#[derive(Clone)]
pub enum BoolOrRcNode {
    Bool(bool),
    RcNode(Id<Node>),
}

impl From<bool> for BoolOrRcNode {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<Id<Node>> for BoolOrRcNode {
    fn from(value: Id<Node>) -> Self {
        Self::RcNode(value)
    }
}

bitflags! {
    pub struct OuterExpressionKinds: u32 {
        const None = 0;
        const Parentheses = 1 << 0;
        const TypeAssertions = 1 << 1;
        const NonNullAssertions = 1 << 2;
        const PartiallyEmittedExpressions = 1 << 3;

        const Assertions = Self::TypeAssertions.bits | Self::NonNullAssertions.bits;
        const All = Self::Parentheses.bits | Self::Assertions.bits | Self::PartiallyEmittedExpressions.bits;

        const ExcludeJSDocTypeAssertion = 1 << 4;
    }
}

pub struct CallBinding {
    pub target: Id<Node /*LeftHandSideExpression*/>,
    pub this_arg: Id<Node /*Expression*/>,
}

pub trait ParenthesizerRules {
    // fn get_parenthesize_left_side_of_binary_for_operator(&self, binary_operator: SyntaxKind) ->
    // fn get_parenthesize_right_side_of_binary_for_operator(&self, binary_operator: SyntaxKind) ->
    fn parenthesize_left_side_of_binary(
        &self,
        binary_operator: SyntaxKind,
        left_side: Id<Node>, /*Expression*/
    ) -> Id<Node /*Expression*/>;
    fn parenthesize_right_side_of_binary(
        &self,
        binary_operator: SyntaxKind,
        left_side: Option<Id<Node /*Expression*/>>,
        right_side: Id<Node>, /*Expression*/
    ) -> Id<Node /*Expression*/>;
    fn parenthesize_expression_of_computed_property_name(
        &self,
        expression: Id<Node>, /*Expression*/
    ) -> Id<Node /*Expression*/>;
    fn parenthesize_condition_of_conditional_expression(
        &self,
        condition: Id<Node>, /*Expression*/
    ) -> Id<Node /*Expression*/>;
    fn parenthesize_branch_of_conditional_expression(
        &self,
        branch: Id<Node>, /*Expression*/
    ) -> Id<Node /*Expression*/>;
    fn parenthesize_expression_of_export_default(
        &self,
        expression: Id<Node>, /*Expression*/
    ) -> Id<Node /*Expression*/>;
    fn parenthesize_expression_of_new(
        &self,
        expression: Id<Node>, /*Expression*/
    ) -> Id<Node /*LeftHandSideExpression*/>;
    fn parenthesize_left_side_of_access(
        &self,
        expression: Id<Node>, /*Expression*/
    ) -> Id<Node /*LeftHandSideExpression*/>;
    fn parenthesize_operand_of_postfix_unary(
        &self,
        operand: Id<Node>, /*Expression*/
    ) -> Id<Node /*LeftHandSideExpression*/>;
    fn parenthesize_operand_of_prefix_unary(
        &self,
        operand: Id<Node>, /*Expression*/
    ) -> Id<Node /*UnaryExpression*/>;
    fn parenthesize_expressions_of_comma_delimited_list(
        &self,
        elements: NodeArrayOrVec, /*<Expression>*/
    ) -> Id<NodeArray> /*<Expression>*/;
    fn parenthesize_expression_for_disallowed_comma(
        &self,
        expression: Id<Node>, /*Expression*/
    ) -> Id<Node /*Expression*/>;
    fn parenthesize_expression_of_expression_statement(
        &self,
        expression: Id<Node>, /*Expression*/
    ) -> Id<Node /*Expression*/>;
    fn parenthesize_concise_body_of_arrow_function(
        &self,
        expression: Id<Node>, /*Expression | ConciseBody*/
    ) -> Id<Node /*Expression | ConciseBody*/>;
    fn parenthesize_member_of_conditional_type(
        &self,
        member: Id<Node>, /*TypeNode*/
    ) -> Id<Node /*TypeNode*/>;
    fn parenthesize_member_of_element_type(
        &self,
        member: Id<Node>, /*TypeNode*/
    ) -> Id<Node /*TypeNode*/>;
    fn parenthesize_element_type_of_array_type(
        &self,
        member: Id<Node>, /*TypeNode*/
    ) -> Id<Node /*TypeNode*/>;
    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        members: NodeArrayOrVec, /*<TypeNode>*/
    ) -> Id<NodeArray> /*<TypeNode>*/;
    fn parenthesize_type_arguments(
        &self,
        type_parameters: Option<NodeArrayOrVec /*<TypeNode>*/>,
    ) -> Option<Id<NodeArray> /*<TypeNode>*/>;
}

pub trait NodeConverters {
    fn convert_to_function_block(
        &self,
        node: Id<Node>, /*ConciseBody*/
        multi_line: Option<bool>,
    ) -> Id<Node /*Block*/>;
    fn convert_to_function_expression(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> Id<Node /*FunctionExpression*/>;
    fn convert_to_array_assignment_element(
        &self,
        element: Id<Node>, /*ArrayBindingOrAssignmentElement*/
    ) -> Id<Node /*Expression*/>;
    fn convert_to_object_assignment_element(
        &self,
        element: Id<Node>, /*ObjectBindingOrAssignmentElement*/
    ) -> Id<Node /*ObjectLiteralElementLike*/>;
    fn convert_to_assignment_pattern(
        &self,
        node: Id<Node>, /*BindingOrAssignmentPattern*/
    ) -> Id<Node /*AssignmentPattern*/>;
    fn convert_to_object_assignment_pattern(
        &self,
        node: Id<Node>, /*ObjectBindingOrAssignmentPattern*/
    ) -> Id<Node /*ObjectLiteralExpression*/>;
    fn convert_to_array_assignment_pattern(
        &self,
        node: Id<Node>, /*ArrayBindingOrAssignmentPattern*/
    ) -> Id<Node /*ArrayLiteralExpression*/>;
    fn convert_to_assignment_element_target(
        &self,
        node: Id<Node>, /*BindingOrAssignmentElementTarget*/
    ) -> Id<Node /*Expression*/>;
}

pub struct NodeFactory {
    pub(crate) arena: *const AllArenas,
    pub base_factory: Id<Box<dyn BaseNodeFactory>>,
    pub flags: NodeFactoryFlags,
    pub parenthesizer_rules: Cell<Option<Id<Box<dyn ParenthesizerRules>>>>,
    pub converters: RefCell<Option<Box<dyn NodeConverters>>>,
}

bitflags! {
    pub struct LexicalEnvironmentFlags: u32 {
        const None = 0;
        const InParameters = 1 << 0;
        const VariablesHoistedInParameters = 1 << 1;
    }
}

pub trait CoreTransformationContext {
    fn factory(&self) -> Id<NodeFactory>;

    fn get_compiler_options(&self) -> Id<CompilerOptions>;

    fn start_lexical_environment(&self);

    fn set_lexical_environment_flags(&self, flags: LexicalEnvironmentFlags, value: bool);
    fn get_lexical_environment_flags(&self) -> LexicalEnvironmentFlags;

    fn suspend_lexical_environment(&self);

    fn resume_lexical_environment(&self);

    fn end_lexical_environment(&self) -> Option<Vec<Id<Node /*Statement*/>>>;

    fn hoist_function_declaration(&self, node: Id<Node> /*FunctionDeclaration*/);

    fn hoist_variable_declaration(&self, node: Id<Node> /*Identifier*/);

    fn start_block_scope(&self);

    fn end_block_scope(&self) -> Option<Vec<Id<Node /*Statement*/>>>;

    fn add_block_scoped_variable(&self, node: Id<Node> /*Identifier*/);

    fn add_initialization_statement(&self, node: Id<Node> /*Statement*/);
}

pub trait TransformationContext: CoreTransformationContext + HasArena {
    fn get_emit_resolver(&self) -> Id<Box<dyn EmitResolver>>;
    fn get_emit_host(&self) -> Id<Box<dyn EmitHost>>;
    fn get_emit_helper_factory(&self) -> Id<EmitHelperFactory>;

    fn request_emit_helper(&self, helper: Id<EmitHelper>);

    fn read_emit_helpers(&self) -> Option<Vec<Id<EmitHelper>>>;

    fn enable_substitution(&self, kind: SyntaxKind);

    fn is_substitution_enabled(&self, node: Id<Node>) -> bool;

    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>>;
    fn override_on_substitute_node(
        &self,
        overrider: &mut dyn FnMut(
            Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
        )
            -> Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    );
    fn pop_overridden_on_substitute_node(
        &self,
    ) -> Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>;

    fn enable_emit_notification(&self, kind: SyntaxKind);

    fn is_emit_notification_enabled(&self, node: Id<Node>) -> bool;

    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()>;
    fn override_on_emit_node(
        &self,
        overrider: &mut dyn FnMut(
            Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
        ) -> Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    );
    fn pop_overridden_on_emit_node(&self) -> Id<Box<dyn TransformationContextOnEmitNodeOverrider>>;

    fn add_diagnostic(&self, diag: Id<Diagnostic /*DiagnosticWithLocation*/>);
}

pub trait TransformationContextOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()>;
}

pub trait TransformationContextOnSubstituteNodeOverrider {
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>>;
}

pub trait TransformationResult {
    fn transformed(&self) -> Vec<Id<Node>>;

    fn diagnostics(&self) -> Option<Vec<Id<Diagnostic /*DiagnosticWithLocation*/>>>;

    fn substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>>;

    fn emit_node_with_notification(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()>;

    fn is_emit_notification_enabled(&self, node: Id<Node>) -> Option<bool>;

    fn dispose(&self);
}

pub type TransformerFactory = Id<Box<dyn TransformerFactoryInterface>>;

pub trait TransformerFactoryInterface {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer;
}

pub type Transformer = Id<Box<dyn TransformerInterface>>;

pub trait TransformerInterface {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>>;
    fn as_dyn_any(&self) -> &dyn Any;
}

pub type VisitResult = Option<SingleNodeOrVecNode>;

pub trait VisitResultInterface {
    fn ptr_eq_node(&self, node: Id<Node>) -> bool;
    fn into_single_node(self) -> Id<Node>;
}

impl VisitResultInterface for VisitResult {
    fn ptr_eq_node(&self, node: Id<Node>) -> bool {
        matches!(
            self,
            Some(single_node_or_vec_node) if single_node_or_vec_node.ptr_eq_node(node)
        )
    }

    fn into_single_node(self) -> Id<Node> {
        self.unwrap().as_single_node()
    }
}

mod _SingleNodeOrVecNodeDeriveTraceScope {
    use super::*;

    #[derive(Clone)]
    pub enum SingleNodeOrVecNode {
        SingleNode(Id<Node>),
        VecNode(Vec<Id<Node>>),
    }
}
pub use _SingleNodeOrVecNodeDeriveTraceScope::SingleNodeOrVecNode;
use id_arena::Id;

use crate::AllArenas;

impl SingleNodeOrVecNode {
    // TODO: suppress `gc`-generated Drop implementation to avoid this complaining?
    // fn as_single_node(self) -> Id<Node> {
    //     enum_unwrapped!(self, [SingleNodeOrVecNode, SingleNode])
    // }

    // fn as_vec_node(self) -> Vec<Id<Node>> {
    //     enum_unwrapped!(self, [SingleNodeOrVecNode, VecNode])
    // }
    pub fn as_single_node(&self) -> Id<Node> {
        enum_unwrapped!(self, [SingleNodeOrVecNode, SingleNode]).clone()
    }

    pub fn as_vec_node(&self) -> &Vec<Id<Node>> {
        enum_unwrapped!(self, [SingleNodeOrVecNode, VecNode])
    }

    pub fn iter(&self) -> SingleNodeOrVecNodeIter {
        SingleNodeOrVecNodeIter::new(self)
    }

    pub fn ptr_eq_node(&self, node: Id<Node>) -> bool {
        matches!(
            self,
            SingleNodeOrVecNode::SingleNode(single_node) if *single_node == node
        )
    }
}

impl From<Id<Node>> for SingleNodeOrVecNode {
    fn from(value: Id<Node>) -> Self {
        Self::SingleNode(value)
    }
}

impl From<Vec<Id<Node>>> for SingleNodeOrVecNode {
    fn from(value: Vec<Id<Node>>) -> Self {
        Self::VecNode(value)
    }
}

impl From<SingleNodeOrVecNode> for Vec<Id<Node>> {
    fn from(value: SingleNodeOrVecNode) -> Self {
        match value {
            SingleNodeOrVecNode::SingleNode(value) => vec![value],
            SingleNodeOrVecNode::VecNode(value) => value,
        }
    }
}

pub struct SingleNodeOrVecNodeIter<'single_node_or_vec_node> {
    single_node_or_vec_node: &'single_node_or_vec_node SingleNodeOrVecNode,
    current_index: usize,
}

impl<'single_node_or_vec_node> SingleNodeOrVecNodeIter<'single_node_or_vec_node> {
    pub fn new(single_node_or_vec_node: &'single_node_or_vec_node SingleNodeOrVecNode) -> Self {
        Self {
            single_node_or_vec_node,
            current_index: 0,
        }
    }
}

impl<'single_node_or_vec_node> Iterator for SingleNodeOrVecNodeIter<'single_node_or_vec_node> {
    type Item = &'single_node_or_vec_node Id<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.single_node_or_vec_node {
            SingleNodeOrVecNode::SingleNode(single_node_or_vec_node) => {
                if self.current_index > 0 {
                    return None;
                }
                self.current_index += 1;
                Some(single_node_or_vec_node)
            }
            SingleNodeOrVecNode::VecNode(single_node_or_vec_node) => {
                if self.current_index >= single_node_or_vec_node.len() {
                    return None;
                }
                let ret = &single_node_or_vec_node[self.current_index];
                self.current_index += 1;
                Some(ret)
            }
        }
    }
}

impl<'single_node_or_vec_node> IntoIterator for &'single_node_or_vec_node SingleNodeOrVecNode {
    type Item = &'single_node_or_vec_node Id<Node>;
    type IntoIter = SingleNodeOrVecNodeIter<'single_node_or_vec_node>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
