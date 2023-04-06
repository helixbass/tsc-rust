#![allow(non_upper_case_globals)]

use gc::Gc;
use std::ptr;

use super::{
    get_cooked_text, propagate_child_flags, propagate_children_flags,
    propagate_identifier_name_flags, CookedText,
};
use crate::{
    are_option_gcs_equal, get_elements_of_binding_or_assignment_pattern,
    get_target_of_binding_or_assignment_element, has_invalid_escape, has_node_array_changed,
    has_option_node_array_changed, is_array_literal_expression, is_assignment_pattern,
    is_call_chain, is_generated_identifier, is_identifier, is_import_keyword, is_local_name,
    is_logical_or_coalescing_assignment_operator, is_object_literal_expression,
    is_omitted_expression, is_super_keyword, is_super_property, last_or_undefined,
    modifiers_to_flags, ArrayBindingPattern, ArrayLiteralExpression, ArrowFunction, AsDoubleDeref,
    AwaitExpression, BaseLiteralLikeNode, BaseNode, BaseNodeFactory, BinaryExpression,
    BindingElement, CallExpression, ClassExpression, ClassLikeDeclarationInterface,
    ConditionalExpression, Debug_, DeleteExpression, ElementAccessExpression, FunctionExpression,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasTypeArgumentsInterface,
    HasTypeInterface, HasTypeParametersInterface, ImportTypeNode, IndexedAccessTypeNode,
    InferTypeNode, InterfaceOrClassLikeDeclarationInterface, LiteralTypeNode, MappedTypeNode,
    ModifierFlags, NamedDeclarationInterface, NewExpression, Node, NodeArray, NodeArrayOrVec,
    NodeFactory, NodeFlags, NodeInterface, ObjectBindingPattern, ObjectLiteralExpression,
    ParenthesizedExpression, ParenthesizedTypeNode, PostfixUnaryExpression, PrefixUnaryExpression,
    PropertyAccessExpression, SignatureDeclarationInterface, SpreadElement, StrOrRcNode,
    StringOrNumberOrBoolOrRcNode, SyntaxKind, SyntaxKindOrRcNode, TaggedTemplateExpression,
    TemplateExpression, TemplateLiteralLikeNode, TemplateLiteralTypeNode, ThisTypeNode, TokenFlags,
    TransformFlags, TypeAssertion, TypeOfExpression, TypeOperatorNode, VoidExpression,
    YieldExpression,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_infer_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        type_parameter: Gc<Node /*TypeParameterDeclaration*/>,
    ) -> InferTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::InferType);
        let mut node = InferTypeNode::new(node, type_parameter);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_infer_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*InferTypeNode*/
        type_parameter: Gc<Node /*TypeParameterDeclaration*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_template_literal_type(
        &self,
        base_factory: &TBaseNodeFactory,
        head: Gc<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateLiteralTypeSpan>*/
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

    pub fn update_template_literal_type(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*TemplateLiteralTypeSpan*/
        head: Gc<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateLiteralTypeSpan>*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_import_type_node<TTypeArguments: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        argument: Gc<Node /*TypeNode*/>,
        qualifier: Option<Gc<Node /*EntityName*/>>,
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

    pub fn update_import_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ImportTypeNode*/
        argument: Gc<Node /*TypeNode*/>,
        qualifier: Option<Gc<Node /*EntityName*/>>,
        type_arguments: Option<impl Into<NodeArrayOrVec /*<TypeNode>*/>>,
        is_type_of: Option<bool>,
    ) -> Gc<Node> {
        let node_as_import_type_node = node.as_import_type_node();
        let is_type_of = is_type_of.unwrap_or_else(|| node_as_import_type_node.is_type_of());
        let type_arguments = type_arguments.map(Into::into);
        if !Gc::ptr_eq(&node_as_import_type_node.argument, &argument)
            || !are_option_gcs_equal(
                node_as_import_type_node.qualifier.as_ref(),
                qualifier.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_import_type_node.maybe_type_arguments().as_deref(),
                type_arguments.as_ref(),
            )
            || node_as_import_type_node.is_type_of() != is_type_of
        {
            self.update(
                self.create_import_type_node(
                    base_factory,
                    argument,
                    qualifier,
                    type_arguments,
                    Some(is_type_of),
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_parenthesized_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Gc<Node /*TypeNode*/>,
    ) -> ParenthesizedTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::ParenthesizedType);
        let mut node = ParenthesizedTypeNode::new(node, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_parenthesized_type(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ParenthesizedTypeNode*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        unimplemented!()
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
        type_: Gc<Node /*TypeNode*/>,
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

    pub fn update_type_operator_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*TypeOperatorNode*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_indexed_access_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        object_type: Gc<Node /*TypeNode*/>,
        index_type: Gc<Node /*TypeNode*/>,
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

    pub fn update_indexed_access_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*IndexedAccessTypeNode*/
        object_type: Gc<Node /*TypeNode*/>,
        index_type: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_mapped_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        readonly_token: Option<Gc<Node /*ReadonlyKeyword | PlusToken | MinusToken*/>>,
        type_parameter: Gc<Node /*TypeParameterDeclaration*/>,
        name_type: Option<Gc<Node /*TypeNode*/>>,
        question_token: Option<Gc<Node /*QuestionToken | PlusToken | MinusToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        members: Option<impl Into<NodeArrayOrVec> /*<TypeElement>*/>,
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

    pub fn update_mapped_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*MappedTypeNode*/
        readonly_token: Option<Gc<Node /*ReadonlyKeyword | PlusToken | MinusToken*/>>,
        type_parameter: Gc<Node /*TypeParameterDeclaration*/>,
        name_type: Option<Gc<Node /*TypeNode*/>>,
        question_token: Option<Gc<Node /*QuestionToken | PlusToken | MinusToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        members: Option<Gc<NodeArray /*<TypeElement>*/>>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_literal_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        literal: Gc<Node /*LiteralTypeNode["literal"]*/>,
    ) -> LiteralTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::LiteralType);
        let mut node = LiteralTypeNode::new(node, literal);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_literal_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*LiteralTypeNode*/
        literal: Gc<Node /*LiteralTypeNode["literal"]*/>,
    ) -> Gc<Node> {
        let node_as_literal_type_node = node.as_literal_type_node();
        if !Gc::ptr_eq(&node_as_literal_type_node.literal, &literal) {
            self.update(
                self.create_literal_type_node(base_factory, literal).into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_object_binding_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
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

    pub fn update_object_binding_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ObjectBindingPattern*/
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
    ) -> Gc<Node> {
        let node_as_object_binding_pattern = node.as_object_binding_pattern();
        let elements = elements.into();
        if has_node_array_changed(&node_as_object_binding_pattern.elements, &elements) {
            self.update(
                self.create_object_binding_pattern(base_factory, elements)
                    .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_array_binding_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
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

    pub fn update_array_binding_pattern(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ArrayBindingPattern*/
        elements: impl Into<NodeArrayOrVec /*<ArrayBindingElement>*/>,
    ) -> Gc<Node> {
        let node_as_array_binding_pattern = node.as_array_binding_pattern();
        let elements = elements.into();
        if has_node_array_changed(&node_as_array_binding_pattern.elements, &elements) {
            self.update(
                self.create_array_binding_pattern(base_factory, elements)
                    .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_binding_element<'property_name, 'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
        property_name: Option<impl Into<StrOrRcNode<'property_name> /*PropertyName*/>>,
        name: impl Into<StrOrRcNode<'name>>, /*BindingName*/
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> BindingElement {
        let node = self.create_base_binding_like_declaration(
            base_factory,
            SyntaxKind::BindingElement,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
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

    pub fn update_binding_element(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*BindingElement*/
        dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
        property_name: Option<Gc<Node /*PropertyName*/>>,
        name: Gc<Node /*BindingName*/>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        let node_as_binding_element = node.as_binding_element();
        if !are_option_gcs_equal(
            node_as_binding_element.property_name.as_ref(),
            property_name.as_ref(),
        ) || !are_option_gcs_equal(
            node_as_binding_element.dot_dot_dot_token.as_ref(),
            dot_dot_dot_token.as_ref(),
        ) || !Gc::ptr_eq(&node_as_binding_element.name(), &name)
            || !are_option_gcs_equal(
                node_as_binding_element.maybe_initializer().as_ref(),
                initializer.as_ref(),
            )
        {
            self.update(
                self.create_binding_element(
                    base_factory,
                    dot_dot_dot_token,
                    property_name,
                    name,
                    initializer,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub(crate) fn create_base_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
    ) -> BaseNode {
        let node = self.create_base_node(base_factory, kind);
        node
    }

    pub fn create_array_literal_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: Option<impl Into<NodeArrayOrVec>>, /*Expression*/
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
                .parenthesize_expressions_of_comma_delimited_list(
                    base_factory,
                    elements_array.into(),
                ),
            multi_line,
        );
        node.add_transform_flags(propagate_children_flags(Some(&node.elements)));
        node
    }

    pub fn update_array_literal_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,                         /*ArrayLiteralExpression*/
        elements: impl Into<NodeArrayOrVec>, /*Expression*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_object_literal_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        properties: Option<impl Into<NodeArrayOrVec> /*ObjectLiteralElementLike*/>,
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

    pub fn update_object_literal_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,                           /*ObjectLiteralExpression*/
        properties: impl Into<NodeArrayOrVec>, /*ObjectLiteralElementLike*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_property_access_expression<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        name: impl Into<StrOrRcNode<'name>>,
    ) -> PropertyAccessExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::PropertyAccessExpression);
        let node = PropertyAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
            None,
            self.as_name(base_factory, Some(name)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | if is_identifier(&node.name) {
                    propagate_identifier_name_flags(&node.name)
                } else {
                    propagate_child_flags(Some(&*node.name))
                },
        );
        if is_super_keyword(&expression) {
            node.add_transform_flags(
                TransformFlags::ContainsES2017 | TransformFlags::ContainsES2018,
            );
        }
        node
    }

    pub fn create_property_access_chain<'name, TName: Into<StrOrRcNode<'name>>>(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        question_dot_token: Option<Gc<Node /*QuestionDotToken*/>>,
        name: TName,
    ) -> PropertyAccessExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::PropertyAccessExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let mut node = PropertyAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
            question_dot_token,
            self.as_name(base_factory, Some(name)).unwrap(),
        );
        node.add_transform_flags(
            TransformFlags::ContainsES2020
                | propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(node.question_dot_token.clone())
                | if is_identifier(&node.name) {
                    propagate_identifier_name_flags(&node.name)
                } else {
                    propagate_child_flags(Some(&*node.name))
                },
        );
        node
    }

    pub fn create_element_access_expression<TIndex: Into<StringOrNumberOrBoolOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        index: TIndex,
    ) -> ElementAccessExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ElementAccessExpression);
        let mut node = ElementAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
            None,
            self.as_expression(base_factory, Some(index)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.argument_expression)),
        );
        if is_super_keyword(&expression) {
            node.add_transform_flags(
                TransformFlags::ContainsES2017 | TransformFlags::ContainsES2018,
            );
        }
        node
    }

    pub fn create_element_access_chain<TIndex: Into<StringOrNumberOrBoolOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        question_dot_token: Option<Gc<Node /*QuestionDotToken*/>>,
        index: TIndex,
    ) -> ElementAccessExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ElementAccessExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let mut node = ElementAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
            question_dot_token,
            self.as_expression(base_factory, Some(index)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(node.question_dot_token.clone())
                | propagate_child_flags(Some(&*node.argument_expression))
                | TransformFlags::ContainsES2020,
        );
        node
    }

    pub fn create_call_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec /*<TypeNode>*/>>,
        arguments_array: Option<impl Into<NodeArrayOrVec /*<Expression>*/>>,
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
                    self.create_node_array(arguments_array, None).into(),
                ),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_children_flags(node.maybe_type_arguments().as_deref())
                | propagate_children_flags(Some(&node.arguments)),
        );
        if node.maybe_type_arguments().is_some() {
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
        type_arguments: Option<&[Gc<Node /*TypeNode*/>]>,
        arguments_array: &[Gc<Node /*expression*/>],
    ) -> Gc<Node /*CallExpression*/> {
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
                node_as_call_expression.maybe_type_arguments().as_ref(),
                type_arguments,
            ) {
                (Some(node_type_arguments), Some(type_arguments)) => {
                    node_type_arguments.len() == type_arguments.len()
                        && node_type_arguments.iter().enumerate().all(
                            |(index, node_type_argument)| {
                                Gc::ptr_eq(node_type_argument, &type_arguments[index])
                            },
                        )
                }
                (None, None) => true,
                _ => false,
            }
            || !(node_as_call_expression.arguments.len() == arguments_array.len()
                && node_as_call_expression.arguments.iter().enumerate().all(
                    |(index, node_argument)| Gc::ptr_eq(node_argument, &arguments_array[index]),
                ))
        {
            self.update(
                self.create_call_expression(
                    base_factory,
                    expression.node_wrapper(),
                    type_arguments.map(|type_arguments| type_arguments.to_vec()),
                    Some(arguments_array.to_vec()),
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_call_chain<
        TTypeArguments: Into<NodeArrayOrVec>,
        TArgumentsArray: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        question_dot_token: Option<Gc<Node /*QuestionDotToken*/>>,
        type_arguments: Option<TTypeArguments /*<TypeNode>*/>,
        arguments_array: Option<TArgumentsArray /*<Expression>*/>,
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
                    self.create_node_array(arguments_array, None).into(),
                ),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(node.question_dot_token.clone())
                | propagate_children_flags(node.maybe_type_arguments().as_deref())
                | propagate_children_flags(Some(&node.arguments))
                | TransformFlags::ContainsES2020,
        );
        if node.maybe_type_arguments().is_some() {
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
        question_dot_token: Option<Gc<Node /*QuestionDotToken*/>>,
        type_arguments: Option<&[Gc<Node /*TypeNode*/>]>,
        arguments_array: &[Gc<Node /*expression*/>],
    ) -> Gc<Node /*CallExpression*/> {
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
                    Gc::ptr_eq(node_question_dot_token, question_dot_token)
                }
                (None, None) => true,
                _ => false,
            }
            || !match (
                node_as_call_expression.maybe_type_arguments().as_ref(),
                type_arguments,
            ) {
                (Some(node_type_arguments), Some(type_arguments)) => {
                    node_type_arguments.len() == type_arguments.len()
                        && node_type_arguments.iter().enumerate().all(
                            |(index, node_type_argument)| {
                                Gc::ptr_eq(node_type_argument, &type_arguments[index])
                            },
                        )
                }
                (None, None) => true,
                _ => false,
            }
            || !(node_as_call_expression.arguments.len() == arguments_array.len()
                && node_as_call_expression.arguments.iter().enumerate().all(
                    |(index, node_argument)| Gc::ptr_eq(node_argument, &arguments_array[index]),
                ))
        {
            self.update(
                self.create_call_chain(
                    base_factory,
                    expression.node_wrapper(),
                    question_dot_token,
                    type_arguments.map(|type_arguments| type_arguments.to_vec()),
                    Some(arguments_array.to_vec()),
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_new_expression<
        TTypeArguments: Into<NodeArrayOrVec>,
        TArgumentsArray: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        type_arguments: Option<TTypeArguments /*<TypeNode>*/>,
        arguments_array: Option<TArgumentsArray /*<Expression>*/>,
    ) -> NewExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::NewExpression);
        let mut node = NewExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_of_new(base_factory, &expression),
            self.as_node_array(type_arguments),
            arguments_array.map(|arguments_array| {
                self.parenthesizer_rules()
                    .parenthesize_expressions_of_comma_delimited_list(
                        base_factory,
                        arguments_array.into(),
                    )
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_children_flags(node.maybe_type_arguments().as_deref())
                | propagate_children_flags(node.arguments.as_deref())
                | TransformFlags::ContainsES2020,
        );
        if node.maybe_type_arguments().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn create_tagged_template_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        tag: Gc<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec> /*<TypeNode>*/>,
        template: Gc<Node /*TemplateLiteral*/>,
    ) -> TaggedTemplateExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::TaggedTemplateExpression);
        let mut node = TaggedTemplateExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &tag),
            self.as_node_array(type_arguments),
            template,
            None,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.tag))
                | propagate_children_flags(node.maybe_type_arguments().as_deref())
                | propagate_child_flags(Some(&*node.template))
                | TransformFlags::ContainsES2015,
        );
        if node.maybe_type_arguments().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        if has_invalid_escape(&node.template) {
            node.add_transform_flags(TransformFlags::ContainsES2018);
        }
        node
    }

    pub fn create_type_assertion(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Gc<Node /*TypeNode*/>,
        expression: Gc<Node /*Expression*/>,
    ) -> TypeAssertion {
        let node = self.create_base_expression(base_factory, SyntaxKind::TypeAssertionExpression);
        let mut node = TypeAssertion::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(base_factory, &expression),
            type_,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.type_))
                | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn create_parenthesized_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
    ) -> ParenthesizedExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ParenthesizedExpression);
        let mut node = ParenthesizedExpression::new(node, expression);
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn create_function_expression<
        'name,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StrOrRcNode<'name>>,
        TTypeParameters: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<TModifiers>,
        asterisk_token: Option<Gc<Node>>,
        name: Option<TName>,
        type_parameters: Option<TTypeParameters>,
        parameters: TParameters,
        type_: Option<Gc<Node>>,
        body: Gc<Node>,
    ) -> FunctionExpression {
        let mut node = self.create_base_function_like_declaration(
            base_factory,
            SyntaxKind::FunctionExpression,
            Option::<Gc<NodeArray>>::None,
            modifiers,
            name,
            type_parameters,
            Some(parameters),
            type_,
            Some(body),
        );
        node.asterisk_token = asterisk_token;
        let mut node = FunctionExpression::new(node);
        node.add_transform_flags(propagate_child_flags(node.maybe_asterisk_token()));
        if node.maybe_type_parameters().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        if modifiers_to_flags(node.maybe_modifiers().as_double_deref())
            .intersects(ModifierFlags::Async)
        {
            if node.maybe_asterisk_token().is_some() {
                node.add_transform_flags(TransformFlags::ContainsES2018);
            } else {
                node.add_transform_flags(TransformFlags::ContainsES2017);
            }
        } else if node.maybe_asterisk_token().is_some() {
            node.add_transform_flags(TransformFlags::ContainsGenerator);
        }
        node
    }

    pub fn update_function_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*FunctionExpression*/
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Gc<Node /*AsteriskToken*/>>,
        name: Option<Gc<Node /*Identifier*/>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        body: Gc<Node /*Block*/>,
    ) -> Gc<Node> {
        let node_as_function_expression = node.as_function_expression();
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let parameters = parameters.into();
        if !are_option_gcs_equal(
            node_as_function_expression.maybe_name().as_ref(),
            name.as_ref(),
        ) || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !are_option_gcs_equal(
                node_as_function_expression.maybe_asterisk_token().as_ref(),
                asterisk_token.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_function_expression
                    .maybe_type_parameters()
                    .as_deref(),
                type_parameters.as_ref(),
            )
            || has_node_array_changed(&node_as_function_expression.parameters(), &parameters)
            || !are_option_gcs_equal(
                node_as_function_expression.maybe_type().as_ref(),
                type_.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_function_expression.maybe_body().unwrap(), &body)
        {
            self.update_base_function_like_declaration(
                self.create_function_expression(
                    base_factory,
                    modifiers,
                    asterisk_token,
                    name,
                    type_parameters,
                    parameters,
                    type_,
                    body,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_arrow_function(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node>>,
        equals_greater_than_token: Option<Gc<Node /*EqualsGreaterThanToken*/>>,
        body: Gc<Node /*ConciseBody*/>,
    ) -> ArrowFunction {
        let mut node = self.create_base_function_like_declaration(
            base_factory,
            SyntaxKind::ArrowFunction,
            Option::<Gc<NodeArray>>::None,
            modifiers,
            Option::<Gc<Node>>::None,
            type_parameters,
            Some(parameters),
            type_,
            Some(
                self.parenthesizer_rules()
                    .parenthesize_concise_body_of_arrow_function(base_factory, &body),
            ),
        );
        let mut node = ArrowFunction::new(
            node,
            equals_greater_than_token.unwrap_or_else(|| {
                self.create_token(base_factory, SyntaxKind::EqualsGreaterThanToken)
                    .into()
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.equals_greater_than_token))
                | TransformFlags::ContainsES2015,
        );
        if modifiers_to_flags(node.maybe_modifiers().as_double_deref())
            .intersects(ModifierFlags::Async)
        {
            node.add_transform_flags(
                TransformFlags::ContainsES2017 | TransformFlags::ContainsLexicalThis,
            );
        }
        node
    }

    pub fn update_arrow_function(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ArrowFunction*/
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        equals_greater_than_token: Gc<Node /*EqualsGreaterThanToken*/>,
        body: Gc<Node /*ConciseBody*/>,
    ) -> Gc<Node /*ArrowFunction*/> {
        let node_as_arrow_function = node.as_arrow_function();
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let parameters = parameters.into();
        if has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || has_option_node_array_changed(
                node_as_arrow_function.maybe_type_parameters().as_deref(),
                type_parameters.as_ref(),
            )
            || has_node_array_changed(&node_as_arrow_function.parameters(), &parameters)
            || !are_option_gcs_equal(node_as_arrow_function.maybe_type().as_ref(), type_.as_ref())
            || !Gc::ptr_eq(
                &node_as_arrow_function.equals_greater_than_token,
                &equals_greater_than_token,
            )
            || !Gc::ptr_eq(&node_as_arrow_function.maybe_body().unwrap(), &body)
        {
            self.update_base_function_like_declaration(
                self.create_arrow_function(
                    base_factory,
                    modifiers,
                    type_parameters,
                    parameters,
                    type_,
                    Some(equals_greater_than_token),
                    body,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_delete_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
    ) -> DeleteExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::DeleteExpression);
        let mut node = DeleteExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(base_factory, &expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn create_type_of_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
    ) -> TypeOfExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::TypeOfExpression);
        let mut node = TypeOfExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(base_factory, &expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn create_void_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
    ) -> VoidExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::VoidExpression);
        let mut node = VoidExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(base_factory, &expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn create_await_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
    ) -> AwaitExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::AwaitExpression);
        let mut node = AwaitExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(base_factory, &expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | TransformFlags::ContainsES2017
                | TransformFlags::ContainsES2018
                | TransformFlags::ContainsAwait,
        );
        node
    }

    pub fn create_prefix_unary_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        operator: SyntaxKind, /*PrefixUnaryOperator*/
        operand: Gc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::PrefixUnaryExpression);
        let mut node = PrefixUnaryExpression::new(
            node,
            operator,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(base_factory, &operand),
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.operand)));
        if matches!(
            operator,
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ) && is_identifier(&node.operand)
            && !is_generated_identifier(&node.operand)
            && !is_local_name(&node.operand)
        {
            node.add_transform_flags(TransformFlags::ContainsUpdateExpressionForIdentifier);
        }
        node
    }

    pub fn create_postfix_unary_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Gc<Node /*Expression*/>,
        operator: SyntaxKind,
    ) -> PostfixUnaryExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::PostfixUnaryExpression);
        let mut node = PostfixUnaryExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_postfix_unary(base_factory, &operand),
            operator,
        );
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
        left: Gc<Node /*Expression*/>,
        operator: TOperator,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::BinaryExpression);
        let operator_token = self.as_token(base_factory, operator.into());
        let operator_kind = operator_token.kind();
        let mut node = BinaryExpression::new(
            node,
            self.parenthesizer_rules().parenthesize_left_side_of_binary(
                base_factory,
                operator_kind,
                &left,
            ),
            operator_token,
            self.parenthesizer_rules()
                .parenthesize_right_side_of_binary(base_factory, operator_kind, Some(left), &right),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.left))
                | propagate_child_flags(Some(&*node.operator_token))
                | propagate_child_flags(Some(&*node.right)),
        );
        if operator_kind == SyntaxKind::QuestionQuestionToken {
            node.add_transform_flags(TransformFlags::ContainsES2020);
        } else if operator_kind == SyntaxKind::EqualsToken {
            if is_object_literal_expression(&node.left) {
                node.add_transform_flags(
                    TransformFlags::ContainsES2015
                        | TransformFlags::ContainsES2018
                        | TransformFlags::ContainsDestructuringAssignment
                        | self.propagate_assignment_pattern_flags(&node.left),
                );
            } else if is_array_literal_expression(&node.left) {
                node.add_transform_flags(
                    TransformFlags::ContainsES2015
                        | TransformFlags::ContainsDestructuringAssignment
                        | self.propagate_assignment_pattern_flags(&node.left),
                );
            }
        } else if matches!(
            operator_kind,
            SyntaxKind::AsteriskAsteriskToken | SyntaxKind::AsteriskAsteriskEqualsToken
        ) {
            node.add_transform_flags(TransformFlags::ContainsES2016);
        } else if is_logical_or_coalescing_assignment_operator(operator_kind) {
            node.add_transform_flags(TransformFlags::ContainsES2021);
        }
        node
    }

    fn propagate_assignment_pattern_flags(
        &self,
        node: &Node, /*AssignmentPattern*/
    ) -> TransformFlags {
        if node
            .transform_flags()
            .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            return TransformFlags::ContainsObjectRestOrSpread;
        }
        if node
            .transform_flags()
            .intersects(TransformFlags::ContainsES2018)
        {
            for element in get_elements_of_binding_or_assignment_pattern(node) {
                let target = get_target_of_binding_or_assignment_element(&element);
                if let Some(target) = target.filter(|target| is_assignment_pattern(target)) {
                    if target
                        .transform_flags()
                        .intersects(TransformFlags::ContainsObjectRestOrSpread)
                    {
                        return TransformFlags::ContainsObjectRestOrSpread;
                    }
                    if target
                        .transform_flags()
                        .intersects(TransformFlags::ContainsES2018)
                    {
                        let flags = self.propagate_assignment_pattern_flags(&target);
                        if flags != TransformFlags::None {
                            return flags;
                        }
                    }
                }
            }
        }
        TransformFlags::None
    }

    pub fn create_conditional_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        condition: Gc<Node /*Expression*/>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        when_true: Gc<Node /*Expression*/>,
        colon_token: Option<Gc<Node /*ColonToken*/>>,
        when_false: Gc<Node /*Expression*/>,
    ) -> ConditionalExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ConditionalExpression);
        let mut node = ConditionalExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_condition_of_conditional_expression(base_factory, &condition),
            question_token.unwrap_or_else(|| {
                self.create_token(base_factory, SyntaxKind::QuestionToken)
                    .into()
            }),
            self.parenthesizer_rules()
                .parenthesize_branch_of_conditional_expression(base_factory, &when_true),
            colon_token.unwrap_or_else(|| {
                self.create_token(base_factory, SyntaxKind::ColonToken)
                    .into()
            }),
            self.parenthesizer_rules()
                .parenthesize_branch_of_conditional_expression(base_factory, &when_false),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.condition))
                | propagate_child_flags(Some(&*node.question_token))
                | propagate_child_flags(Some(&*node.when_true))
                | propagate_child_flags(Some(&*node.colon_token))
                | propagate_child_flags(Some(&*node.when_false)),
        );
        node
    }

    pub fn create_template_expression<TTemplateSpans: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        head: Gc<Node /*TemplateHead*/>,
        template_spans: TTemplateSpans, /*<TemplateSpan>*/
    ) -> TemplateExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::TemplateExpression);
        let mut node = TemplateExpression::new(
            node,
            head,
            self.create_node_array(Some(template_spans), None),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.head))
                | propagate_children_flags(Some(&node.template_spans))
                | TransformFlags::ContainsES2015,
        );
        node
    }

    fn create_template_literal_like_node_checked(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        mut text: Option<String>,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        let template_flags = template_flags.unwrap_or(TokenFlags::None);
        Debug_.assert(
            template_flags & !TokenFlags::TemplateLiteralLikeFlags == TokenFlags::None,
            Some("Unsupported template flags."),
        );
        let mut cooked: Option<CookedText> = None;
        if let Some(raw_text) = raw_text.as_ref() {
            if match text.as_ref() {
                Some(text) => raw_text != text,
                None => true,
            } {
                cooked = Some(get_cooked_text(kind, raw_text));
                if matches!(cooked, Some(CookedText::InvalidValue)) {
                    Debug_.fail(Some("Invalid raw text"));
                }
            }
        }
        match text.as_ref() {
            None => {
                if cooked.is_none() {
                    Debug_.fail(Some(
                        "Arguments 'text' and 'rawText' may not both be undefined.",
                    ));
                }
                let cooked = cooked.unwrap();
                text = Some(match cooked {
                    CookedText::InvalidValue => panic!("Expected String"),
                    CookedText::String(cooked) => cooked,
                });
            }
            Some(text) => {
                if let Some(cooked) = cooked.as_ref() {
                    Debug_.assert(matches!(cooked, CookedText::String(cooked) if text == cooked), Some("Expected argument 'text' to be the normalized (i.e. 'cooked') version of argument 'rawText'."));
                }
            }
        }
        self.create_template_literal_like_node(
            base_factory,
            kind,
            text.unwrap(),
            raw_text,
            Some(template_flags),
        )
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
        let mut node = TemplateLiteralLikeNode::new(
            node,
            raw_text,
            Some(template_flags & TokenFlags::TemplateLiteralLikeFlags),
        );
        node.add_transform_flags(TransformFlags::ContainsES2015);
        if matches!(node.template_flags, Some(template_flags) if template_flags != TokenFlags::None)
        {
            node.add_transform_flags(TransformFlags::ContainsES2018);
        }
        node
    }

    pub fn create_template_head(
        &self,
        base_factory: &TBaseNodeFactory,
        text: Option<String>,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        self.create_template_literal_like_node_checked(
            base_factory,
            SyntaxKind::TemplateHead,
            text,
            raw_text,
            template_flags,
        )
    }

    pub fn create_template_middle(
        &self,
        base_factory: &TBaseNodeFactory,
        text: Option<String>,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        self.create_template_literal_like_node_checked(
            base_factory,
            SyntaxKind::TemplateMiddle,
            text,
            raw_text,
            template_flags,
        )
    }

    pub fn create_template_tail(
        &self,
        base_factory: &TBaseNodeFactory,
        text: Option<String>,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        self.create_template_literal_like_node_checked(
            base_factory,
            SyntaxKind::TemplateTail,
            text,
            raw_text,
            template_flags,
        )
    }

    pub fn create_no_substitution_template_literal(
        &self,
        base_factory: &TBaseNodeFactory,
        text: Option<String>,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        self.create_template_literal_like_node_checked(
            base_factory,
            SyntaxKind::NoSubstitutionTemplateLiteral,
            text,
            raw_text,
            template_flags,
        )
    }

    pub fn create_yield_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        asterisk_token: Option<Gc<Node /*AsteriskToken*/>>,
        expression: Option<Gc<Node /*Expression*/>>,
    ) -> YieldExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::YieldExpression);
        let mut node = YieldExpression::new(
            node,
            expression.map(|expression| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(base_factory, &expression)
            }),
            asterisk_token,
        );
        node.add_transform_flags(
            propagate_child_flags(node.expression.clone())
                | propagate_child_flags(node.asterisk_token.clone())
                | TransformFlags::ContainsES2015
                | TransformFlags::ContainsES2018
                | TransformFlags::ContainsYield,
        );
        node
    }

    pub fn create_spread_element(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
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

    pub fn create_class_expression<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<
            impl Into<StrOrRcNode<'name>>,
            /*string | Identifier*/
        >,
        type_parameters: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeParameterDeclaration>*/
        >,
        heritage_clauses: Option<
            impl Into<NodeArrayOrVec>,
            /*<HeritageClause>*/
        >,
        members: impl Into<NodeArrayOrVec>,
        /*<ClassElement>*/
    ) -> ClassExpression {
        let node = self.create_base_class_like_declaration(
            base_factory,
            SyntaxKind::ClassExpression,
            decorators,
            modifiers,
            name,
            type_parameters,
            heritage_clauses,
            members,
        );
        let mut node = ClassExpression::new(node);
        node.add_transform_flags(TransformFlags::ContainsES2015);
        node
    }

    pub fn update_class_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ClassExpression*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<Gc<Node>>,
        type_parameters: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeParameterDeclaration>*/
        >,
        heritage_clauses: Option<
            impl Into<NodeArrayOrVec>,
            /*<HeritageClause>*/
        >,
        members: impl Into<NodeArrayOrVec>,
        /*<ClassElement>*/
    ) -> Gc<Node> {
        let node_as_class_expression = node.as_class_expression();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let heritage_clauses = heritage_clauses.map(Into::into);
        let members = members.into();
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !are_option_gcs_equal(
                node_as_class_expression.maybe_name().as_ref(),
                name.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_class_expression.maybe_type_parameters().as_deref(),
                type_parameters.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_class_expression.maybe_heritage_clauses().as_deref(),
                heritage_clauses.as_ref(),
            )
            || has_node_array_changed(&node_as_class_expression.members(), &members)
        {
            self.update(
                self.create_class_expression(
                    base_factory,
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    heritage_clauses,
                    members,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }
}
