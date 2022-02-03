#![allow(non_upper_case_globals)]

use std::ptr;
use std::rc::Rc;

use super::{propagate_child_flags, propagate_children_flags, propagate_identifier_name_flags};
use crate::{
    is_call_chain, is_generated_identifier, is_identifier, is_import_keyword, is_local_name,
    is_omitted_expression, is_super_property, last_or_undefined, ArrayBindingPattern,
    ArrayLiteralExpression, BaseLiteralLikeNode, BaseNode, BaseNodeFactory, BinaryExpression,
    BindingElement, CallExpression, Debug_, FunctionExpression, ImportTypeNode,
    IndexedAccessTypeNode, InferTypeNode, LiteralTypeNode, MappedTypeNode, Node, NodeArray,
    NodeArrayOrVec, NodeFactory, NodeFlags, NodeInterface, ObjectBindingPattern,
    ObjectLiteralExpression, ParenthesizedExpression, ParenthesizedTypeNode,
    PostfixUnaryExpression, PrefixUnaryExpression, SpreadElement, StringOrRcNode, SyntaxKind,
    SyntaxKindOrRcNode, TemplateExpression, TemplateLiteralLikeNode, TemplateLiteralTypeNode,
    ThisTypeNode, TokenFlags, TransformFlags, TypeOperatorNode,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_infer_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        type_parameter: Rc<Node /*TypeParameterDeclaration*/>,
    ) -> InferTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::InferType);
        let mut node = InferTypeNode::new(node, type_parameter);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_template_literal_type<TTemplateSpans: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        head: Rc<Node /*TemplateHead*/>,
        template_spans: TTemplateSpans, /*<TemplateLiteralTypeSpan>*/
    ) -> TemplateLiteralTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TemplateLiteralType);
        let mut node = TemplateLiteralTypeNode::new(
            node,
            head,
            self.create_node_array(Some(template_spans), None),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_import_type_node<TTypeArguments: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        argument: Rc<Node /*TypeNode*/>,
        qualifier: Option<Rc<Node /*EntityName*/>>,
        type_arguments: Option<TTypeArguments /*<TypeNode>*/>,
        is_type_of: Option<bool>,
    ) -> ImportTypeNode {
        let is_type_of = is_type_of.unwrap_or(false);
        let node = self.create_base_node(base_factory, SyntaxKind::ImportType);
        let mut node = ImportTypeNode::new(
            node,
            argument,
            qualifier,
            type_arguments.and_then(|type_arguments| {
                self.parenthesizer_rules()
                    .parenthesize_type_arguments(base_factory, Some(type_arguments.into()))
            }),
            is_type_of,
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_parenthesized_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Rc<Node /*TypeNode*/>,
    ) -> ParenthesizedTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::ParenthesizedType);
        let mut node = ParenthesizedTypeNode::new(node, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_this_type_node(&self, base_factory: &TBaseNodeFactory) -> ThisTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::ThisType);
        let mut node = ThisTypeNode::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_type_operator_node(
        &self,
        base_factory: &TBaseNodeFactory,
        operator: SyntaxKind,
        type_: Rc<Node /*TypeNode*/>,
    ) -> TypeOperatorNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypeOperator);
        let mut node = TypeOperatorNode::new(
            node,
            operator,
            self.parenthesizer_rules()
                .parenthesize_member_of_element_type(base_factory, &type_),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_indexed_access_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        operator: SyntaxKind,
        object_type: Rc<Node /*TypeNode*/>,
        index_type: Rc<Node /*TypeNode*/>,
    ) -> IndexedAccessTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::IndexedAccessType);
        let mut node = IndexedAccessTypeNode::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_member_of_element_type(base_factory, &object_type),
            index_type,
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_mapped_type_node<TMembers: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        readonly_token: Option<Rc<Node /*ReadonlyKeyword | PlusToken | MinusToken*/>>,
        type_parameter: Rc<Node /*TypeParameterDeclaration*/>,
        name_type: Option<Rc<Node /*TypeNode*/>>,
        question_token: Option<Rc<Node /*QuestionToken | PlusToken | MinusToken*/>>,
        type_: Option<Rc<Node /*TypeNode*/>>,
        members: Option<TMembers /*<TypeElement>*/>,
    ) -> MappedTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::MappedType);
        let mut node = MappedTypeNode::new(
            node,
            readonly_token,
            type_parameter,
            name_type,
            question_token,
            type_,
            members.map(|members| self.create_node_array(Some(members), None)),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_literal_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        literal: Rc<Node /*LiteralTypeNode["literal"]*/>,
    ) -> LiteralTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::LiteralType);
        let mut node = LiteralTypeNode::new(node, literal);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_object_binding_pattern<TElements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: TElements, /*<BindingElement>*/
    ) -> ObjectBindingPattern {
        let node = self.create_base_node(base_factory, SyntaxKind::ObjectBindingPattern);
        let mut node =
            ObjectBindingPattern::new(node, self.create_node_array(Some(elements), None));
        node.add_transform_flags(
            propagate_children_flags(Some(&node.elements))
                | TransformFlags::ContainsES2015
                | TransformFlags::ContainsBindingPattern,
        );
        if node
            .transform_flags()
            .intersects(TransformFlags::ContainsRestOrSpread)
        {
            node.add_transform_flags(
                TransformFlags::ContainsES2018 | TransformFlags::ContainsObjectRestOrSpread,
            );
        }
        node
    }

    pub fn create_array_binding_pattern<TElements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: TElements, /*<BindingElement>*/
    ) -> ArrayBindingPattern {
        let node = self.create_base_node(base_factory, SyntaxKind::ArrayBindingPattern);
        let mut node = ArrayBindingPattern::new(node, self.create_node_array(Some(elements), None));
        node.add_transform_flags(
            propagate_children_flags(Some(&node.elements))
                | TransformFlags::ContainsES2015
                | TransformFlags::ContainsBindingPattern,
        );
        node
    }

    pub fn create_binding_element<
        TPropertyName: Into<StringOrRcNode>,
        TName: Into<StringOrRcNode>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        dot_dot_dot_token: Option<Rc<Node /*DotDotDotToken*/>>,
        property_name: Option<TPropertyName /*PropertyName*/>,
        name: TName, /*BindingName*/
        initializer: Option<Rc<Node /*Expression*/>>,
    ) -> BindingElement {
        let node = self.create_base_binding_like_declaration(
            base_factory,
            SyntaxKind::BindingElement,
            Option::<NodeArray>::None,
            Option::<NodeArray>::None,
            Some(name),
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(base_factory, &initializer)
            }),
        );
        let dot_dot_dot_token_is_some = dot_dot_dot_token.is_some();
        let mut node = BindingElement::new(
            node,
            self.as_name(base_factory, property_name),
            dot_dot_dot_token,
        );
        node.add_transform_flags(
            propagate_child_flags(node.dot_dot_dot_token.clone()) | TransformFlags::ContainsES2015,
        );
        if let Some(node_property_name) = node.property_name.as_ref() {
            node.add_transform_flags(if is_identifier(node_property_name) {
                propagate_identifier_name_flags(&node_property_name)
            } else {
                propagate_child_flags(Some(&**node_property_name))
            });
        }
        if dot_dot_dot_token_is_some {
            node.add_transform_flags(TransformFlags::ContainsRestOrSpread);
        }
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
        multi_line: Option<bool>,
    ) -> ArrayLiteralExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ArrayLiteralExpression);
        let elements = elements.map(|elements| elements.into());
        let elements_vec = elements.clone().map(|elements| match elements {
            NodeArrayOrVec::NodeArray(node_array) => node_array.to_vec(),
            NodeArrayOrVec::Vec(elements) => elements,
        });
        let last_element = elements_vec
            .as_ref()
            .and_then(|elements_vec| last_or_undefined(elements_vec));
        let elements_array = self.create_node_array(
            elements,
            last_element.and_then(|last_element| {
                if is_omitted_expression(last_element) {
                    Some(true)
                } else {
                    None
                }
            }),
        );
        let mut node = ArrayLiteralExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expressions_of_comma_delimited_list(base_factory, elements_array),
            multi_line,
        );
        node.add_transform_flags(propagate_children_flags(Some(&node.elements)));
        node
    }

    pub fn create_object_literal_expression<TProperties: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        properties: Option<TProperties>, /*ObjectLiteralElementLike*/
        multi_line: Option<bool>,
    ) -> ObjectLiteralExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ObjectLiteralExpression);
        let mut node = ObjectLiteralExpression::new(
            node,
            self.create_node_array(properties, None),
            multi_line,
        );
        node.add_transform_flags(propagate_children_flags(Some(&node.properties)));
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

    pub fn create_function_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<NodeArray>,
        asterisk_token: Option<Rc<Node>>,
        name: Option<Rc<Node>>,
        type_parameters: Option<NodeArray>,
        parameters: NodeArray,
        type_: Option<Rc<Node>>,
        body: Option<Rc<Node>>,
    ) -> FunctionExpression {
        let mut node = self.create_base_function_like_declaration(
            base_factory,
            SyntaxKind::FunctionExpression,
            Option::<NodeArray>::None,
            modifiers,
            name,
            type_parameters,
            Some(parameters),
            type_,
            body,
        );
        node.asterisk_token = asterisk_token;
        FunctionExpression::new(node)
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

    pub fn create_postfix_unary_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Rc<Node /*Expression*/>,
        operator: SyntaxKind,
    ) -> PostfixUnaryExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::PostfixUnaryExpression);
        let mut node = PostfixUnaryExpression::new(node, operand, operator);
        node.add_transform_flags(propagate_child_flags(Some(&*node.operand)));
        if is_identifier(&node.operand)
            && !is_generated_identifier(&node.operand)
            && !is_local_name(&node.operand)
        {
            node.add_transform_flags(TransformFlags::ContainsUpdateExpressionForIdentifier);
        }
        node
    }

    pub fn create_binary_expression<TOperator: Into<SyntaxKindOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        operator: TOperator,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::BinaryExpression);
        let operator_token = self.as_token(base_factory, operator.into());
        let operator_kind = operator_token.kind();
        let node = BinaryExpression::new(node, left, operator_token, right);
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

    pub fn create_spread_element(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
    ) -> SpreadElement {
        let node = self.create_base_expression(base_factory, SyntaxKind::SpreadElement);
        let mut node = SpreadElement::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(base_factory, &expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | TransformFlags::ContainsES2015
                | TransformFlags::ContainsRestOrSpread,
        );
        node
    }
}
