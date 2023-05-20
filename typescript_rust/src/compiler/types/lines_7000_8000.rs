use std::{io, ptr, rc::Rc};

use bitflags::bitflags;
use derive_builder::Builder;
use gc::{Finalize, Gc, GcCell, Trace};
use local_macros::enum_unwrapped;

use super::{CompilerOptions, Diagnostic, EmitHint, Node, NodeArray, NodeArrayOrVec, SyntaxKind};
use crate::{
    BaseNodeFactory, BaseNodeFactorySynthetic, EmitHelper, EmitHelperFactory, EmitHost,
    EmitResolver, NodeFactoryFlags,
};

#[derive(Default, Builder, Clone)]
#[builder(default, setter(strip_option, into))]
pub struct PropertyDescriptorAttributes {
    pub enumerable: Option<BoolOrRcNode /*Expression*/>,
    pub configurable: Option<BoolOrRcNode /*Expression*/>,
    pub writable: Option<BoolOrRcNode /*Expression*/>,
    pub value: Option<Gc<Node /*Expression*/>>,
    pub get: Option<Gc<Node /*Expression*/>>,
    pub set: Option<Gc<Node /*Expression*/>>,
}

#[derive(Clone)]
pub enum BoolOrRcNode {
    Bool(bool),
    RcNode(Gc<Node>),
}

impl From<bool> for BoolOrRcNode {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<Gc<Node>> for BoolOrRcNode {
    fn from(value: Gc<Node>) -> Self {
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

pub trait ParenthesizerRules<TBaseNodeFactory: BaseNodeFactory>: Trace + Finalize {
    // fn get_parenthesize_left_side_of_binary_for_operator(&self, binary_operator: SyntaxKind) ->
    // fn get_parenthesize_right_side_of_binary_for_operator(&self, binary_operator: SyntaxKind) ->
    fn parenthesize_left_side_of_binary(
        &self,
        binary_operator: SyntaxKind,
        left_side: &Node, /*Expression*/
    ) -> Gc<Node /*Expression*/>;
    fn parenthesize_right_side_of_binary(
        &self,
        binary_operator: SyntaxKind,
        left_side: Option<Gc<Node /*Expression*/>>,
        right_side: &Node, /*Expression*/
    ) -> Gc<Node /*Expression*/>;
    fn parenthesize_expression_of_computed_property_name(
        &self,
        expression: &Node, /*Expression*/
    ) -> Gc<Node /*Expression*/>;
    fn parenthesize_condition_of_conditional_expression(
        &self,
        condition: &Node, /*Expression*/
    ) -> Gc<Node /*Expression*/>;
    fn parenthesize_branch_of_conditional_expression(
        &self,
        branch: &Node, /*Expression*/
    ) -> Gc<Node /*Expression*/>;
    fn parenthesize_expression_of_export_default(
        &self,
        expression: &Node, /*Expression*/
    ) -> Gc<Node /*Expression*/>;
    fn parenthesize_expression_of_new(
        &self,
        expression: &Node, /*Expression*/
    ) -> Gc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_left_side_of_access(
        &self,
        expression: &Node, /*Expression*/
    ) -> Gc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_operand_of_postfix_unary(
        &self,
        operand: &Node, /*Expression*/
    ) -> Gc<Node /*LeftHandSideExpression*/>;
    fn parenthesize_operand_of_prefix_unary(
        &self,
        operand: &Node, /*Expression*/
    ) -> Gc<Node /*UnaryExpression*/>;
    fn parenthesize_expressions_of_comma_delimited_list(
        &self,
        elements: NodeArrayOrVec, /*<Expression>*/
    ) -> Gc<NodeArray> /*<Expression>*/;
    fn parenthesize_expression_for_disallowed_comma(
        &self,
        expression: &Node, /*Expression*/
    ) -> Gc<Node /*Expression*/>;
    fn parenthesize_expression_of_expression_statement(
        &self,
        expression: &Node, /*Expression*/
    ) -> Gc<Node /*Expression*/>;
    fn parenthesize_concise_body_of_arrow_function(
        &self,
        expression: &Node, /*Expression | ConciseBody*/
    ) -> Gc<Node /*Expression | ConciseBody*/>;
    fn parenthesize_member_of_conditional_type(
        &self,
        member: &Node, /*TypeNode*/
    ) -> Gc<Node /*TypeNode*/>;
    fn parenthesize_member_of_element_type(
        &self,
        member: &Node, /*TypeNode*/
    ) -> Gc<Node /*TypeNode*/>;
    fn parenthesize_element_type_of_array_type(
        &self,
        member: &Node, /*TypeNode*/
    ) -> Gc<Node /*TypeNode*/>;
    fn parenthesize_constituent_types_of_union_or_intersection_type(
        &self,
        members: NodeArrayOrVec, /*<TypeNode>*/
    ) -> Gc<NodeArray> /*<TypeNode>*/;
    fn parenthesize_type_arguments(
        &self,
        type_parameters: Option<NodeArrayOrVec /*<TypeNode>*/>,
    ) -> Option<Gc<NodeArray> /*<TypeNode>*/>;
}

pub trait NodeConverters<TBaseNodeFactory: BaseNodeFactory>: Trace + Finalize {
    fn convert_to_function_block(
        &self,
        node: &Node, /*ConciseBody*/
        multi_line: Option<bool>,
    ) -> Gc<Node /*Block*/>;
    fn convert_to_function_expression(
        &self,
        node: &Node, /*FunctionDeclaration*/
    ) -> Gc<Node /*FunctionExpression*/>;
    fn convert_to_array_assignment_element(
        &self,
        element: &Node, /*ArrayBindingOrAssignmentElement*/
    ) -> Gc<Node /*Expression*/>;
    fn convert_to_object_assignment_element(
        &self,
        element: &Node, /*ObjectBindingOrAssignmentElement*/
    ) -> Gc<Node /*ObjectLiteralElementLike*/>;
    fn convert_to_assignment_pattern(
        &self,
        node: &Node, /*BindingOrAssignmentPattern*/
    ) -> Gc<Node /*AssignmentPattern*/>;
    fn convert_to_object_assignment_pattern(
        &self,
        node: &Node, /*ObjectBindingOrAssignmentPattern*/
    ) -> Gc<Node /*ObjectLiteralExpression*/>;
    fn convert_to_array_assignment_pattern(
        &self,
        node: &Node, /*ArrayBindingOrAssignmentPattern*/
    ) -> Gc<Node /*ArrayLiteralExpression*/>;
    fn convert_to_assignment_element_target(
        &self,
        node: &Node, /*BindingOrAssignmentElementTarget*/
    ) -> Gc<Node /*Expression*/>;
}

#[derive(Trace, Finalize)]
pub struct NodeFactory<TBaseNodeFactory: Trace + Finalize + 'static> {
    pub base_factory: Gc<TBaseNodeFactory>,
    #[unsafe_ignore_trace]
    pub flags: NodeFactoryFlags,
    pub parenthesizer_rules: GcCell<Option<Gc<Box<dyn ParenthesizerRules<TBaseNodeFactory>>>>>,
    pub converters: GcCell<Option<Box<dyn NodeConverters<TBaseNodeFactory>>>>,
}

bitflags! {
    pub struct LexicalEnvironmentFlags: u32 {
        const None = 0;
        const InParameters = 1 << 0;
        const VariablesHoistedInParameters = 1 << 1;
    }
}

pub trait CoreTransformationContext<TBaseNodeFactory: BaseNodeFactory + Trace + Finalize>:
    Trace + Finalize
{
    fn factory(&self) -> Gc<NodeFactory<TBaseNodeFactory>>;
    fn base_factory(&self) -> Gc<TBaseNodeFactory>;

    fn get_compiler_options(&self) -> Gc<CompilerOptions>;

    fn start_lexical_environment(&self);

    fn set_lexical_environment_flags(&self, flags: LexicalEnvironmentFlags, value: bool);
    fn get_lexical_environment_flags(&self) -> LexicalEnvironmentFlags;

    fn suspend_lexical_environment(&self);

    fn resume_lexical_environment(&self);

    fn end_lexical_environment(&self) -> Option<Vec<Gc<Node /*Statement*/>>>;

    fn hoist_function_declaration(&self, node: &Node /*FunctionDeclaration*/);

    fn hoist_variable_declaration(&self, node: &Node /*Identifier*/);

    fn start_block_scope(&self);

    fn end_block_scope(&self) -> Option<Vec<Gc<Node /*Statement*/>>>;

    fn add_block_scoped_variable(&self, node: &Node /*Identifier*/);

    fn add_initialization_statement(&self, node: &Node /*Statement*/);
}

pub trait TransformationContext: CoreTransformationContext<BaseNodeFactorySynthetic> {
    fn get_emit_resolver(&self) -> Gc<Box<dyn EmitResolver>>;
    fn get_emit_host(&self) -> Gc<Box<dyn EmitHost>>;
    fn get_emit_helper_factory(&self) -> Rc<EmitHelperFactory>;

    fn request_emit_helper(&self, helper: Gc<EmitHelper>);

    fn read_emit_helpers(&self) -> Option<Vec<Gc<EmitHelper>>>;

    fn enable_substitution(&self, kind: SyntaxKind);

    fn is_substitution_enabled(&self, node: &Node) -> bool;

    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> io::Result<Gc<Node>>;
    fn override_on_substitute_node(
        &self,
        overrider: &mut dyn FnMut(
            Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
        )
            -> Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    );

    fn enable_emit_notification(&self, kind: SyntaxKind);

    fn is_emit_notification_enabled(&self, node: &Node) -> bool;

    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()>;
    fn override_on_emit_node(
        &self,
        overrider: &mut dyn FnMut(
            Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
        ) -> Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    );

    fn add_diagnostic(&self, diag: Gc<Diagnostic /*DiagnosticWithLocation*/>);
}

pub trait TransformationContextOnEmitNodeOverrider: Trace + Finalize {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()>;
}

pub trait TransformationContextOnSubstituteNodeOverrider: Trace + Finalize {
    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> io::Result<Gc<Node>>;
}

pub trait TransformationResult {
    fn transformed(&self) -> Vec<Gc<Node>>;

    fn diagnostics(&self) -> Option<Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>>>;

    fn substitute_node(&self, hint: EmitHint, node: &Node) -> io::Result<Gc<Node>>;

    fn emit_node_with_notification(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()>;

    fn is_emit_notification_enabled(&self, node: &Node) -> Option<bool>;

    fn dispose(&self);
}

pub type TransformerFactory = Gc<Box<dyn TransformerFactoryInterface>>;

pub trait TransformerFactoryInterface: Trace + Finalize {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer;
}

pub type Transformer = Gc<Box<dyn TransformerInterface>>;

pub trait TransformerInterface: Trace + Finalize {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>>;
}

pub type VisitResult = Option<SingleNodeOrVecNode>;

pub trait VisitResultInterface {
    fn ptr_eq_node(&self, node: &Node) -> bool;
    fn into_single_node(self) -> Gc<Node>;
}

impl VisitResultInterface for VisitResult {
    fn ptr_eq_node(&self, node: &Node) -> bool {
        matches!(
            self,
            Some(single_node_or_vec_node) if single_node_or_vec_node.ptr_eq_node(node)
        )
    }

    fn into_single_node(self) -> Gc<Node> {
        self.unwrap().as_single_node()
    }
}

mod _SingleNodeOrVecNodeDeriveTraceScope {
    use local_macros::Trace;

    use super::*;

    #[derive(Clone, Trace, Finalize)]
    pub enum SingleNodeOrVecNode {
        SingleNode(Gc<Node>),
        VecNode(Vec<Gc<Node>>),
    }
}
pub use _SingleNodeOrVecNodeDeriveTraceScope::SingleNodeOrVecNode;

impl SingleNodeOrVecNode {
    // TODO: suppress `gc`-generated Drop implementation to avoid this complaining?
    // fn as_single_node(self) -> Gc<Node> {
    //     enum_unwrapped!(self, [SingleNodeOrVecNode, SingleNode])
    // }

    // fn as_vec_node(self) -> Vec<Gc<Node>> {
    //     enum_unwrapped!(self, [SingleNodeOrVecNode, VecNode])
    // }
    pub fn as_single_node(&self) -> Gc<Node> {
        enum_unwrapped!(self, [SingleNodeOrVecNode, SingleNode]).clone()
    }

    pub fn as_vec_node(&self) -> &Vec<Gc<Node>> {
        enum_unwrapped!(self, [SingleNodeOrVecNode, VecNode])
    }

    pub fn iter(&self) -> SingleNodeOrVecNodeIter {
        SingleNodeOrVecNodeIter::new(self)
    }

    pub fn ptr_eq_node(&self, node: &Node) -> bool {
        matches!(
            self,
            SingleNodeOrVecNode::SingleNode(single_node) if ptr::eq(
                &**single_node,
                node,
            )
        )
    }
}

impl From<Gc<Node>> for SingleNodeOrVecNode {
    fn from(value: Gc<Node>) -> Self {
        Self::SingleNode(value)
    }
}

impl From<Vec<Gc<Node>>> for SingleNodeOrVecNode {
    fn from(value: Vec<Gc<Node>>) -> Self {
        Self::VecNode(value)
    }
}

impl From<SingleNodeOrVecNode> for Vec<Gc<Node>> {
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
    type Item = &'single_node_or_vec_node Gc<Node>;

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
    type Item = &'single_node_or_vec_node Gc<Node>;
    type IntoIter = SingleNodeOrVecNodeIter<'single_node_or_vec_node>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
