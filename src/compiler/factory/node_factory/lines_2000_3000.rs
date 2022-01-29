#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{propagate_child_flags, propagate_children_flags};
use crate::{
    add_range, append_if_unique, create_base_node_factory, create_parenthesizer_rules,
    escape_leading_underscores, is_call_chain, is_import_keyword, is_named_declaration,
    is_omitted_expression, is_outer_expression, is_property_name, is_super_property,
    last_or_undefined, null_parenthesizer_rules, pseudo_big_int_to_string, set_text_range,
    ArrayLiteralExpression, ArrayTypeNode, BaseBindingLikeDeclaration, BaseFunctionLikeDeclaration,
    BaseGenericNamedDeclaration, BaseInterfaceOrClassLikeDeclaration, BaseLiteralLikeNode,
    BaseNamedDeclaration, BaseNode, BaseNodeFactory, BaseNodeFactoryConcrete,
    BaseSignatureDeclaration, BaseVariableLikeDeclaration, BigIntLiteral, BinaryExpression, Block,
    CallExpression, Debug_, EmitFlags, EmitNode, EmptyStatement, Expression, ExpressionStatement,
    FunctionDeclaration, Identifier, IfStatement, InterfaceDeclaration, IntersectionTypeNode,
    LiteralLikeNode, LiteralLikeNodeInterface, LiteralTypeNode, Node, NodeArray, NodeArrayOrVec,
    NodeFactory, NodeFlags, NodeInterface, NumericLiteral, ObjectLiteralExpression,
    OuterExpressionKinds, ParameterDeclaration, ParenthesizedExpression, ParenthesizedTypeNode,
    ParenthesizerRules, PrefixUnaryExpression, PropertyAssignment, PropertySignature, PseudoBigInt,
    ReadonlyTextRange, ReturnStatement, ShorthandPropertyAssignment, SourceFile, SourceMapRange,
    Statement, StringLiteral, SyntaxKind, TemplateExpression, TemplateLiteralLikeNode,
    TemplateSpan, TokenFlags, TransformFlags, TypeAliasDeclaration, TypeLiteralNode, TypeNode,
    TypeParameterDeclaration, TypePredicateNode, TypeReferenceNode, UnionTypeNode,
    VariableDeclaration, VariableDeclarationList, VariableStatement,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_parenthesized_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Rc<Node>,
    ) -> ParenthesizedTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::ParenthesizedType);
        let mut node = ParenthesizedTypeNode::new(node, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_literal_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        literal: Rc<Node>,
    ) -> LiteralTypeNode {
        let node = self.create_token(base_factory, SyntaxKind::LiteralType);
        let node = LiteralTypeNode::new(node, literal);
        node
    }

    pub(crate) fn create_base_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
    ) -> BaseNode {
        let node = self.create_base_node(base_factory, kind);
        node
    }

    pub fn create_array_literal_expression<TElements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: Option<TElements>, /*Expression*/
    ) -> ArrayLiteralExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ArrayLiteralExpression);
        let elements = elements.map(|elements| match elements.into() {
            NodeArrayOrVec::NodeArray(node_array) => node_array.to_vec(),
            NodeArrayOrVec::Vec(elements) => elements,
        });
        let last_element = elements
            .as_ref()
            .and_then(|elements| last_or_undefined(&elements));
        let has_trailing_comma = last_element.and_then(|last_element| {
            if is_omitted_expression(last_element) {
                Some(true)
            } else {
                None
            }
        });
        let elements_array = self.create_node_array(elements, has_trailing_comma);
        let node = ArrayLiteralExpression::new(node, elements_array);
        node
    }

    pub fn create_object_literal_expression<TProperties: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        properties: Option<TProperties>, /*ObjectLiteralElementLike*/
    ) -> ObjectLiteralExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ObjectLiteralExpression);
        let node = ObjectLiteralExpression::new(node, self.create_node_array(properties, None));
        node
    }

    pub fn create_call_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
        type_arguments: Option<Vec<Rc<Node /*TypeNode*/>>>,
        arguments_array: Vec<Rc<Node /*expression*/>>,
    ) -> CallExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::CallExpression);
        let mut node = CallExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
            None,
            self.as_node_array(type_arguments),
            self.parenthesizer_rules()
                .parenthesize_expressions_of_comma_delimited_list(
                    base_factory,
                    self.create_node_array(Some(arguments_array), None),
                ),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_children_flags(node.type_arguments.as_ref())
                | propagate_children_flags(Some(&node.arguments)),
        );
        if node.type_arguments.is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        if is_import_keyword(&node.expression) {
            node.add_transform_flags(TransformFlags::ContainsDynamicImport);
        } else if is_super_property(&node.expression) {
            node.add_transform_flags(TransformFlags::ContainsLexicalThis);
        }
        node
    }

    pub fn update_call_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,       /*CallExpression*/
        expression: &Node, /*Expression*/
        type_arguments: Option<&[Rc<Node /*TypeNode*/>]>,
        arguments_array: &[Rc<Node /*expression*/>],
    ) -> Rc<Node /*CallExpression*/> {
        let node_as_call_expression = node.as_call_expression();
        if is_call_chain(node) {
            return self.update_call_chain(
                base_factory,
                node,
                expression,
                node_as_call_expression.question_dot_token.clone(),
                type_arguments,
                arguments_array,
            );
        }
        if !ptr::eq(&*node_as_call_expression.expression, expression)
            || !match (
                node_as_call_expression.type_arguments.as_ref(),
                type_arguments,
            ) {
                (Some(node_type_arguments), Some(type_arguments)) => {
                    node_type_arguments.len() == type_arguments.len()
                        && node_type_arguments.iter().enumerate().all(
                            |(index, node_type_argument)| {
                                Rc::ptr_eq(node_type_argument, &type_arguments[index])
                            },
                        )
                }
                (None, None) => true,
                _ => false,
            }
            || !(node_as_call_expression.arguments.len() == arguments_array.len()
                && node_as_call_expression.arguments.iter().enumerate().all(
                    |(index, node_argument)| Rc::ptr_eq(node_argument, &arguments_array[index]),
                ))
        {
            self.update(
                self.create_call_expression(
                    base_factory,
                    expression.node_wrapper(),
                    type_arguments.map(|type_arguments| type_arguments.to_vec()),
                    arguments_array.to_vec(),
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_call_chain(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
        question_dot_token: Option<Rc<Node /*QuestionDotToken*/>>,
        type_arguments: Option<Vec<Rc<Node /*TypeNode*/>>>,
        arguments_array: Vec<Rc<Node /*expression*/>>,
    ) -> CallExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::CallExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let mut node = CallExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
            question_dot_token,
            self.as_node_array(type_arguments),
            self.parenthesizer_rules()
                .parenthesize_expressions_of_comma_delimited_list(
                    base_factory,
                    self.create_node_array(Some(arguments_array), None),
                ),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(node.question_dot_token.clone())
                | propagate_children_flags(node.type_arguments.as_ref())
                | propagate_children_flags(Some(&node.arguments))
                | TransformFlags::ContainsES2020,
        );
        if node.type_arguments.is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        if is_super_property(&node.expression) {
            node.add_transform_flags(TransformFlags::ContainsLexicalThis);
        }
        node
    }

    pub fn update_call_chain(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,       /*CallExpression*/
        expression: &Node, /*Expression*/
        question_dot_token: Option<Rc<Node /*QuestionDotToken*/>>,
        type_arguments: Option<&[Rc<Node /*TypeNode*/>]>,
        arguments_array: &[Rc<Node /*expression*/>],
    ) -> Rc<Node /*CallExpression*/> {
        Debug_.assert(
            node.flags().intersects(NodeFlags::OptionalChain),
            Some("Cannot update a CallExpression using updateCallChain. Use updateCall instead"),
        );
        let node_as_call_expression = node.as_call_expression();
        if !ptr::eq(&*node_as_call_expression.expression, expression)
            || !match (
                node_as_call_expression.question_dot_token.as_ref(),
                question_dot_token.as_ref(),
            ) {
                (Some(node_question_dot_token), Some(question_dot_token)) => {
                    Rc::ptr_eq(node_question_dot_token, question_dot_token)
                }
                (None, None) => true,
                _ => false,
            }
            || !match (
                node_as_call_expression.type_arguments.as_ref(),
                type_arguments,
            ) {
                (Some(node_type_arguments), Some(type_arguments)) => {
                    node_type_arguments.len() == type_arguments.len()
                        && node_type_arguments.iter().enumerate().all(
                            |(index, node_type_argument)| {
                                Rc::ptr_eq(node_type_argument, &type_arguments[index])
                            },
                        )
                }
                (None, None) => true,
                _ => false,
            }
            || !(node_as_call_expression.arguments.len() == arguments_array.len()
                && node_as_call_expression.arguments.iter().enumerate().all(
                    |(index, node_argument)| Rc::ptr_eq(node_argument, &arguments_array[index]),
                ))
        {
            self.update(
                self.create_call_chain(
                    base_factory,
                    expression.node_wrapper(),
                    question_dot_token,
                    type_arguments.map(|type_arguments| type_arguments.to_vec()),
                    arguments_array.to_vec(),
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_parenthesized_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
    ) -> ParenthesizedExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ParenthesizedExpression);
        let node = ParenthesizedExpression::new(node, expression);
        node
    }

    pub fn create_prefix_unary_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        operator: SyntaxKind,
        operand: Rc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::PrefixUnaryExpression);
        let node = PrefixUnaryExpression::new(node, operator, operand);
        node
    }

    pub fn create_binary_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        operator: Rc<Node>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::BinaryExpression);
        let node = BinaryExpression::new(node, left, operator, right);
        node
    }

    pub fn create_template_expression<TTemplateSpans: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        head: Rc<Node /*TemplateHead*/>,
        template_spans: TTemplateSpans,
    ) -> TemplateExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::TemplateExpression);
        let node = TemplateExpression::new(
            node,
            head,
            self.create_node_array(Some(template_spans), None),
        );
        node
    }

    pub fn create_template_literal_like_node(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        text: String,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        let template_flags = template_flags.unwrap_or(TokenFlags::None);
        let node = self.create_base_token(base_factory, kind);
        let node = BaseLiteralLikeNode::new(node, text);
        let node = TemplateLiteralLikeNode::new(
            node,
            raw_text,
            Some(template_flags & TokenFlags::TemplateLiteralLikeFlags),
        );
        node
    }
}
