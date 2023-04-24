use gc::{Finalize, Gc, Trace};

use super::{
    get_cooked_text, propagate_child_flags, propagate_children_flags,
    propagate_identifier_name_flags, CookedText,
};
use crate::{
    are_option_gcs_equal, get_elements_of_binding_or_assignment_pattern,
    get_target_of_binding_or_assignment_element, has_invalid_escape, has_node_array_changed,
    has_option_node_array_changed, is_array_literal_expression, is_assignment_pattern,
    is_call_chain, is_element_access_chain, is_generated_identifier, is_identifier,
    is_import_keyword, is_local_name, is_logical_or_coalescing_assignment_operator,
    is_object_literal_expression, is_omitted_expression, is_property_access_chain,
    is_super_keyword, is_super_property, last_or_undefined, modifiers_to_flags,
    ArrayBindingPattern, ArrayLiteralExpression, ArrowFunction, AsDoubleDeref, AwaitExpression,
    BaseLiteralLikeNode, BaseNode, BaseNodeFactory, BinaryExpression, BindingElement,
    CallExpression, ClassExpression, ClassLikeDeclarationInterface, ConditionalExpression, Debug_,
    DeleteExpression, ElementAccessExpression, FunctionExpression,
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

impl<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize> NodeFactory<TBaseNodeFactory> {
    pub fn create_infer_type_node(
        &self,
        type_parameter: Gc<Node /*TypeParameterDeclaration*/>,
    ) -> InferTypeNode {
        let node = self.create_base_node(SyntaxKind::InferType);
        let node = InferTypeNode::new(node, type_parameter);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_infer_type_node(
        &self,
        node: &Node, /*InferTypeNode*/
        type_parameter: Gc<Node /*TypeParameterDeclaration*/>,
    ) -> Gc<Node> {
        let node_as_infer_type_node = node.as_infer_type_node();
        if !Gc::ptr_eq(&node_as_infer_type_node.type_parameter, &type_parameter) {
            self.update(self.create_infer_type_node(type_parameter).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_template_literal_type(
        &self,
        head: Gc<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateLiteralTypeSpan>*/
    ) -> TemplateLiteralTypeNode {
        let node = self.create_base_node(SyntaxKind::TemplateLiteralType);
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
        node: &Node, /*TemplateLiteralTypeSpan*/
        head: Gc<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateLiteralTypeSpan>*/
    ) -> Gc<Node> {
        let node_as_template_literal_type_node = node.as_template_literal_type_node();
        let template_spans = template_spans.into();
        if !Gc::ptr_eq(&node_as_template_literal_type_node.head, &head)
            || has_node_array_changed(
                &node_as_template_literal_type_node.template_spans,
                &template_spans,
            )
        {
            self.update(
                self.create_template_literal_type(head, template_spans)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_import_type_node<TTypeArguments: Into<NodeArrayOrVec>>(
        &self,
        argument: Gc<Node /*TypeNode*/>,
        qualifier: Option<Gc<Node /*EntityName*/>>,
        type_arguments: Option<TTypeArguments /*<TypeNode>*/>,
        is_type_of: Option<bool>,
    ) -> ImportTypeNode {
        let is_type_of = is_type_of.unwrap_or(false);
        let node = self.create_base_node(SyntaxKind::ImportType);
        let mut node = ImportTypeNode::new(
            node,
            argument,
            qualifier,
            type_arguments.and_then(|type_arguments| {
                self.parenthesizer_rules()
                    .parenthesize_type_arguments(Some(type_arguments.into()))
            }),
            is_type_of,
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_import_type_node(
        &self,
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
                self.create_import_type_node(argument, qualifier, type_arguments, Some(is_type_of))
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_parenthesized_type(&self, type_: Gc<Node /*TypeNode*/>) -> ParenthesizedTypeNode {
        let node = self.create_base_node(SyntaxKind::ParenthesizedType);
        let mut node = ParenthesizedTypeNode::new(node, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_parenthesized_type(
        &self,
        node: &Node, /*ParenthesizedTypeNode*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_parenthesized_type_node = node.as_parenthesized_type_node();
        if !Gc::ptr_eq(&node_as_parenthesized_type_node.type_, &type_) {
            self.update(self.create_parenthesized_type(type_).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_this_type_node(&self) -> ThisTypeNode {
        let node = self.create_base_node(SyntaxKind::ThisType);
        let mut node = ThisTypeNode::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_type_operator_node(
        &self,
        operator: SyntaxKind,
        type_: Gc<Node /*TypeNode*/>,
    ) -> TypeOperatorNode {
        let node = self.create_base_node(SyntaxKind::TypeOperator);
        let mut node = TypeOperatorNode::new(
            node,
            operator,
            self.parenthesizer_rules()
                .parenthesize_member_of_element_type(&type_),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_operator_node(
        &self,
        node: &Node, /*TypeOperatorNode*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_type_operator_node = node.as_type_operator_node();
        if !Gc::ptr_eq(&node_as_type_operator_node.type_, &type_) {
            self.update(
                self.create_type_operator_node(node_as_type_operator_node.operator, type_)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_indexed_access_type_node(
        &self,
        object_type: Gc<Node /*TypeNode*/>,
        index_type: Gc<Node /*TypeNode*/>,
    ) -> IndexedAccessTypeNode {
        let node = self.create_base_node(SyntaxKind::IndexedAccessType);
        let mut node = IndexedAccessTypeNode::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_member_of_element_type(&object_type),
            index_type,
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_indexed_access_type_node(
        &self,
        node: &Node, /*IndexedAccessTypeNode*/
        object_type: Gc<Node /*TypeNode*/>,
        index_type: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_indexed_access_type_node = node.as_indexed_access_type_node();
        if !Gc::ptr_eq(&node_as_indexed_access_type_node.object_type, &object_type)
            || !Gc::ptr_eq(&node_as_indexed_access_type_node.index_type, &index_type)
        {
            self.update(
                self.create_indexed_access_type_node(object_type, index_type)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_mapped_type_node(
        &self,
        readonly_token: Option<Gc<Node /*ReadonlyKeyword | PlusToken | MinusToken*/>>,
        type_parameter: Gc<Node /*TypeParameterDeclaration*/>,
        name_type: Option<Gc<Node /*TypeNode*/>>,
        question_token: Option<Gc<Node /*QuestionToken | PlusToken | MinusToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        members: Option<impl Into<NodeArrayOrVec> /*<TypeElement>*/>,
    ) -> MappedTypeNode {
        let node = self.create_base_node(SyntaxKind::MappedType);
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
        node: &Node, /*MappedTypeNode*/
        readonly_token: Option<Gc<Node /*ReadonlyKeyword | PlusToken | MinusToken*/>>,
        type_parameter: Gc<Node /*TypeParameterDeclaration*/>,
        name_type: Option<Gc<Node /*TypeNode*/>>,
        question_token: Option<Gc<Node /*QuestionToken | PlusToken | MinusToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        members: Option<Gc<NodeArray /*<TypeElement>*/>>,
    ) -> Gc<Node> {
        let node_as_mapped_type_node = node.as_mapped_type_node();
        if !are_option_gcs_equal(
            node_as_mapped_type_node.readonly_token.as_ref(),
            readonly_token.as_ref(),
        ) || !Gc::ptr_eq(&node_as_mapped_type_node.type_parameter, &type_parameter)
            || !are_option_gcs_equal(
                node_as_mapped_type_node.name_type.as_ref(),
                name_type.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_mapped_type_node.question_token.as_ref(),
                question_token.as_ref(),
            )
            || !are_option_gcs_equal(node_as_mapped_type_node.type_.as_ref(), type_.as_ref())
            || !are_option_gcs_equal(node_as_mapped_type_node.members.as_ref(), members.as_ref())
        {
            self.update(
                self.create_mapped_type_node(
                    readonly_token,
                    type_parameter,
                    name_type,
                    question_token,
                    type_,
                    members,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_literal_type_node(
        &self,
        literal: Gc<Node /*LiteralTypeNode["literal"]*/>,
    ) -> LiteralTypeNode {
        let node = self.create_base_node(SyntaxKind::LiteralType);
        let mut node = LiteralTypeNode::new(node, literal);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_literal_type_node(
        &self,
        node: &Node, /*LiteralTypeNode*/
        literal: Gc<Node /*LiteralTypeNode["literal"]*/>,
    ) -> Gc<Node> {
        let node_as_literal_type_node = node.as_literal_type_node();
        if !Gc::ptr_eq(&node_as_literal_type_node.literal, &literal) {
            self.update(self.create_literal_type_node(literal).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_object_binding_pattern(
        &self,
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
    ) -> ObjectBindingPattern {
        let node = self.create_base_node(SyntaxKind::ObjectBindingPattern);
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
        node: &Node, /*ObjectBindingPattern*/
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
    ) -> Gc<Node> {
        let node_as_object_binding_pattern = node.as_object_binding_pattern();
        let elements = elements.into();
        if has_node_array_changed(&node_as_object_binding_pattern.elements, &elements) {
            self.update(self.create_object_binding_pattern(elements).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_array_binding_pattern(
        &self,
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
    ) -> ArrayBindingPattern {
        let node = self.create_base_node(SyntaxKind::ArrayBindingPattern);
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
        node: &Node, /*ArrayBindingPattern*/
        elements: impl Into<NodeArrayOrVec /*<ArrayBindingElement>*/>,
    ) -> Gc<Node> {
        let node_as_array_binding_pattern = node.as_array_binding_pattern();
        let elements = elements.into();
        if has_node_array_changed(&node_as_array_binding_pattern.elements, &elements) {
            self.update(self.create_array_binding_pattern(elements).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_binding_element<'property_name, 'name>(
        &self,
        dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
        property_name: Option<impl Into<StrOrRcNode<'property_name> /*PropertyName*/>>,
        name: impl Into<StrOrRcNode<'name>>, /*BindingName*/
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> BindingElement {
        let node = self.create_base_binding_like_declaration(
            SyntaxKind::BindingElement,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(name),
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(&initializer)
            }),
        );
        let dot_dot_dot_token_is_some = dot_dot_dot_token.is_some();
        let mut node = BindingElement::new(node, self.as_name(property_name), dot_dot_dot_token);
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
                self.create_binding_element(dot_dot_dot_token, property_name, name, initializer)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub(crate) fn create_base_expression(&self, kind: SyntaxKind) -> BaseNode {
        let node = self.create_base_node(kind);
        node
    }

    pub fn create_array_literal_expression(
        &self,
        elements: Option<impl Into<NodeArrayOrVec>>, /*Expression*/
        multi_line: Option<bool>,
    ) -> ArrayLiteralExpression {
        let node = self.create_base_expression(SyntaxKind::ArrayLiteralExpression);
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
                .parenthesize_expressions_of_comma_delimited_list(elements_array.into()),
            multi_line,
        );
        node.add_transform_flags(propagate_children_flags(Some(&node.elements)));
        node
    }

    pub fn update_array_literal_expression(
        &self,
        node: &Node,                         /*ArrayLiteralExpression*/
        elements: impl Into<NodeArrayOrVec>, /*Expression*/
    ) -> Gc<Node> {
        let node_as_array_literal_expression = node.as_array_literal_expression();
        let elements = elements.into();
        if has_node_array_changed(&node_as_array_literal_expression.elements, &elements) {
            self.update(
                self.create_array_literal_expression(
                    Some(elements),
                    node_as_array_literal_expression.multi_line,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_object_literal_expression(
        &self,
        properties: Option<impl Into<NodeArrayOrVec> /*ObjectLiteralElementLike*/>,
        multi_line: Option<bool>,
    ) -> ObjectLiteralExpression {
        let node = self.create_base_expression(SyntaxKind::ObjectLiteralExpression);
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
        node: &Node,                           /*ObjectLiteralExpression*/
        properties: impl Into<NodeArrayOrVec>, /*ObjectLiteralElementLike*/
    ) -> Gc<Node> {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        let properties = properties.into();
        if has_node_array_changed(&node_as_object_literal_expression.properties, &properties) {
            self.update(
                self.create_object_literal_expression(
                    Some(properties),
                    node_as_object_literal_expression.multi_line,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_property_access_expression<'name>(
        &self,
        expression: Gc<Node /*Expression*/>,
        name: impl Into<StrOrRcNode<'name>>,
    ) -> PropertyAccessExpression {
        let node = self.create_base_expression(SyntaxKind::PropertyAccessExpression);
        let node = PropertyAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
            None,
            self.as_name(Some(name)).unwrap(),
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

    pub fn update_property_access_expression(
        &self,
        node: &Node, /*PropertyAccessExpression*/
        expression: Gc<Node /*Expression*/>,
        name: Gc<Node /*Identifier | PrivateIdentifier*/>,
    ) -> Gc<Node> {
        let node_as_property_access_expression = node.as_property_access_expression();
        if is_property_access_chain(node) {
            return self.update_property_access_chain(
                node,
                expression,
                node_as_property_access_expression
                    .question_dot_token
                    .clone(),
                name,
            );
        }
        if !Gc::ptr_eq(&node_as_property_access_expression.expression, &expression)
            || !Gc::ptr_eq(&node_as_property_access_expression.name, &name)
        {
            self.update(
                self.create_property_access_expression(expression, name)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_property_access_chain<'name>(
        &self,
        expression: Gc<Node /*Expression*/>,
        question_dot_token: Option<Gc<Node /*QuestionDotToken*/>>,
        name: impl Into<StrOrRcNode<'name>>,
    ) -> PropertyAccessExpression {
        let node = self.create_base_expression(SyntaxKind::PropertyAccessExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let mut node = PropertyAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
            question_dot_token,
            self.as_name(Some(name)).unwrap(),
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

    pub fn update_property_access_chain(
        &self,
        node: &Node, /*PropertyAccessChain*/
        expression: Gc<Node /*Expression*/>,
        question_dot_token: Option<Gc<Node /*QuestionDotToken*/>>,
        name: Gc<Node /*Identifier | PrivateIdentifier*/>,
    ) -> Gc<Node> {
        let node_as_property_access_expression = node.as_property_access_expression();
        Debug_.assert(
            node.flags().intersects(NodeFlags::OptionalChain),
            Some("Cannot update a PropertyAccessExpression using updatePropertyAccessChain. Use updatePropertyAccess instead.")
        );
        if !Gc::ptr_eq(&node_as_property_access_expression.expression, &expression)
            || !are_option_gcs_equal(
                node_as_property_access_expression
                    .question_dot_token
                    .as_ref(),
                question_dot_token.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_property_access_expression.expression, &expression)
        {
            self.update(
                self.create_property_access_chain(expression, question_dot_token, name)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_element_access_expression(
        &self,
        expression: Gc<Node /*Expression*/>,
        index: impl Into<StringOrNumberOrBoolOrRcNode>,
    ) -> ElementAccessExpression {
        let node = self.create_base_expression(SyntaxKind::ElementAccessExpression);
        let mut node = ElementAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
            None,
            self.as_expression(Some(index)).unwrap(),
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

    pub fn update_element_access_expression(
        &self,
        node: &Node, /*ElementAccessExpression*/
        expression: Gc<Node /*Expression*/>,
        argument_expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_element_access_expression = node.as_element_access_expression();
        if is_element_access_chain(node) {
            return self.update_element_access_chain(
                node,
                expression,
                node_as_element_access_expression.question_dot_token.clone(),
                argument_expression,
            );
        }
        if !Gc::ptr_eq(&node_as_element_access_expression.expression, &expression)
            || !Gc::ptr_eq(
                &node_as_element_access_expression.argument_expression,
                &argument_expression,
            )
        {
            self.update(
                self.create_element_access_expression(expression, argument_expression)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_element_access_chain(
        &self,
        expression: Gc<Node /*Expression*/>,
        question_dot_token: Option<Gc<Node /*QuestionDotToken*/>>,
        index: impl Into<StringOrNumberOrBoolOrRcNode>,
    ) -> ElementAccessExpression {
        let node = self.create_base_expression(SyntaxKind::ElementAccessExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let mut node = ElementAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
            question_dot_token,
            self.as_expression(Some(index)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(node.question_dot_token.clone())
                | propagate_child_flags(Some(&*node.argument_expression))
                | TransformFlags::ContainsES2020,
        );
        node
    }

    pub fn update_element_access_chain(
        &self,
        node: &Node, /*ElementAccessChain*/
        expression: Gc<Node /*Expression*/>,
        question_dot_token: Option<Gc<Node /*QuestionDotToken*/>>,
        argument_expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_element_access_expression = node.as_element_access_expression();
        Debug_.assert(
            node.flags().intersects(NodeFlags::OptionalChain),
            Some("Cannot update a ElementAccessExpression using updateElementAccessChain. Use updateElementAccess instead.")
        );
        if !Gc::ptr_eq(&node_as_element_access_expression.expression, &expression)
            || !are_option_gcs_equal(
                node_as_element_access_expression
                    .question_dot_token
                    .as_ref(),
                question_dot_token.as_ref(),
            )
            || !Gc::ptr_eq(
                &node_as_element_access_expression.argument_expression,
                &argument_expression,
            )
        {
            self.update(
                self.create_element_access_chain(
                    expression,
                    question_dot_token,
                    argument_expression,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_call_expression(
        &self,
        expression: Gc<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec /*<TypeNode>*/>>,
        arguments_array: Option<impl Into<NodeArrayOrVec /*<Expression>*/>>,
    ) -> CallExpression {
        let node = self.create_base_expression(SyntaxKind::CallExpression);
        let mut node = CallExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
            None,
            self.as_node_array(type_arguments),
            self.parenthesizer_rules()
                .parenthesize_expressions_of_comma_delimited_list(
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
        node: &Node, /*CallExpression*/
        expression: Gc<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec /*<TypeNode>*/>>,
        arguments_array: impl Into<NodeArrayOrVec /*<Expression>*/>,
    ) -> Gc<Node /*CallExpression*/> {
        let node_as_call_expression = node.as_call_expression();
        if is_call_chain(node) {
            return self.update_call_chain(
                node,
                expression,
                node_as_call_expression.question_dot_token.clone(),
                type_arguments,
                arguments_array,
            );
        }
        let type_arguments = type_arguments.map(Into::into);
        let arguments_array = arguments_array.into();
        if !Gc::ptr_eq(&node_as_call_expression.expression, &expression)
            || has_option_node_array_changed(
                node_as_call_expression.maybe_type_arguments().as_deref(),
                type_arguments.as_ref(),
            )
            || has_node_array_changed(&node_as_call_expression.arguments, &arguments_array)
        {
            self.update(
                self.create_call_expression(expression, type_arguments, Some(arguments_array))
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_call_chain(
        &self,
        expression: Gc<Node /*Expression*/>,
        question_dot_token: Option<Gc<Node /*QuestionDotToken*/>>,
        type_arguments: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeNode>*/
        >,
        arguments_array: Option<
            impl Into<NodeArrayOrVec>,
            /*<Expression>*/
        >,
    ) -> CallExpression {
        let node = self.create_base_expression(SyntaxKind::CallExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let mut node = CallExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
            question_dot_token,
            self.as_node_array(type_arguments),
            self.parenthesizer_rules()
                .parenthesize_expressions_of_comma_delimited_list(
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
        node: &Node, /*CallChain*/
        expression: Gc<Node /*Expression*/>,
        question_dot_token: Option<Gc<Node /*QuestionDotToken*/>>,
        type_arguments: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeNode>*/
        >,
        arguments_array: impl Into<NodeArrayOrVec>,
        /*<Expression>*/
    ) -> Gc<Node /*CallExpression*/> {
        let node_as_call_expression = node.as_call_expression();
        let type_arguments = type_arguments.map(Into::into);
        let arguments_array = arguments_array.into();
        Debug_.assert(
            node.flags().intersects(NodeFlags::OptionalChain),
            Some("Cannot update a CallExpression using updateCallChain. Use updateCall instead."),
        );
        if !Gc::ptr_eq(&node_as_call_expression.expression, &expression)
            || !are_option_gcs_equal(
                node_as_call_expression.question_dot_token.as_ref(),
                question_dot_token.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_call_expression.maybe_type_arguments().as_deref(),
                type_arguments.as_ref(),
            )
            || has_node_array_changed(&node_as_call_expression.arguments, &arguments_array)
        {
            self.update(
                self.create_call_chain(
                    expression,
                    question_dot_token,
                    type_arguments,
                    Some(arguments_array),
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_new_expression(
        &self,
        expression: Gc<Node /*Expression*/>,
        type_arguments: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeNode>*/
        >,
        arguments_array: Option<
            impl Into<NodeArrayOrVec>,
            /*<Expression>*/
        >,
    ) -> NewExpression {
        let node = self.create_base_expression(SyntaxKind::NewExpression);
        let mut node = NewExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_of_new(&expression),
            self.as_node_array(type_arguments),
            arguments_array.map(|arguments_array| {
                self.parenthesizer_rules()
                    .parenthesize_expressions_of_comma_delimited_list(arguments_array.into())
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

    pub fn update_new_expression(
        &self,
        node: &Node, /*NewExpression*/
        expression: Gc<Node /*Expression*/>,
        type_arguments: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeNode>*/
        >,
        arguments_array: Option<
            impl Into<NodeArrayOrVec>,
            /*<Expression>*/
        >,
    ) -> Gc<Node> {
        let node_as_new_expression = node.as_new_expression();
        let type_arguments = type_arguments.map(Into::into);
        let arguments_array = arguments_array.map(Into::into);
        if !Gc::ptr_eq(&node_as_new_expression.expression, &expression)
            || has_option_node_array_changed(
                node_as_new_expression.maybe_type_arguments().as_deref(),
                type_arguments.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_new_expression.arguments.as_deref(),
                arguments_array.as_ref(),
            )
        {
            self.update(
                self.create_new_expression(expression, type_arguments, arguments_array)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_tagged_template_expression(
        &self,
        tag: Gc<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec> /*<TypeNode>*/>,
        template: Gc<Node /*TemplateLiteral*/>,
    ) -> TaggedTemplateExpression {
        let node = self.create_base_expression(SyntaxKind::TaggedTemplateExpression);
        let mut node = TaggedTemplateExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&tag),
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

    pub fn update_tagged_template_expression(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
        tag: Gc<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec> /*<TypeNode>*/>,
        template: Gc<Node /*TemplateLiteral*/>,
    ) -> Gc<Node> {
        let node_as_tagged_template_expression = node.as_tagged_template_expression();
        let type_arguments = type_arguments.map(Into::into);
        if !Gc::ptr_eq(&node_as_tagged_template_expression.tag, &tag)
            || has_option_node_array_changed(
                node_as_tagged_template_expression
                    .maybe_type_arguments()
                    .as_deref(),
                type_arguments.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_tagged_template_expression.template, &template)
        {
            self.update(
                self.create_tagged_template_expression(tag, type_arguments, template)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_type_assertion(
        &self,
        type_: Gc<Node /*TypeNode*/>,
        expression: Gc<Node /*Expression*/>,
    ) -> TypeAssertion {
        let node = self.create_base_expression(SyntaxKind::TypeAssertionExpression);
        let mut node = TypeAssertion::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(&expression),
            type_,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.type_))
                | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_type_assertion(
        &self,
        node: &Node, /*TypeAssertion*/
        type_: Gc<Node /*TypeNode*/>,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_type_assertion = node.as_type_assertion();
        if !Gc::ptr_eq(&node_as_type_assertion.type_, &type_)
            || !Gc::ptr_eq(&node_as_type_assertion.expression, &expression)
        {
            self.update(self.create_type_assertion(type_, expression).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_parenthesized_expression(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> ParenthesizedExpression {
        let node = self.create_base_expression(SyntaxKind::ParenthesizedExpression);
        let mut node = ParenthesizedExpression::new(node, expression);
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn update_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_parenthesized_expression = node.as_parenthesized_expression();
        if !Gc::ptr_eq(&node_as_parenthesized_expression.expression, &expression) {
            self.update(
                self.create_parenthesized_expression(expression).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_function_expression<'name>(
        &self,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Gc<Node>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node>>,
        body: Gc<Node>,
    ) -> FunctionExpression {
        let mut node = self.create_base_function_like_declaration(
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
                    modifiers,
                    asterisk_token,
                    name,
                    type_parameters,
                    parameters,
                    type_,
                    body,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_arrow_function(
        &self,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node>>,
        equals_greater_than_token: Option<Gc<Node /*EqualsGreaterThanToken*/>>,
        body: Gc<Node /*ConciseBody*/>,
    ) -> ArrowFunction {
        let mut node = self.create_base_function_like_declaration(
            SyntaxKind::ArrowFunction,
            Option::<Gc<NodeArray>>::None,
            modifiers,
            Option::<Gc<Node>>::None,
            type_parameters,
            Some(parameters),
            type_,
            Some(
                self.parenthesizer_rules()
                    .parenthesize_concise_body_of_arrow_function(&body),
            ),
        );
        let mut node = ArrowFunction::new(
            node,
            equals_greater_than_token
                .unwrap_or_else(|| self.create_token(SyntaxKind::EqualsGreaterThanToken).wrap()),
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
                    modifiers,
                    type_parameters,
                    parameters,
                    type_,
                    Some(equals_greater_than_token),
                    body,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_delete_expression(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> DeleteExpression {
        let node = self.create_base_expression(SyntaxKind::DeleteExpression);
        let mut node = DeleteExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(&expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn update_delete_expression(
        &self,
        node: &Node, /*DeleteExpression*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_delete_expression = node.as_delete_expression();
        if !Gc::ptr_eq(&node_as_delete_expression.expression, &expression) {
            self.update(self.create_delete_expression(expression).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_type_of_expression(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> TypeOfExpression {
        let node = self.create_base_expression(SyntaxKind::TypeOfExpression);
        let mut node = TypeOfExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(&expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn update_type_of_expression(
        &self,
        node: &Node, /*TypeOfExpression*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_type_of_expression = node.as_type_of_expression();
        if !Gc::ptr_eq(&node_as_type_of_expression.expression, &expression) {
            self.update(self.create_type_of_expression(expression).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_void_expression(&self, expression: Gc<Node /*Expression*/>) -> VoidExpression {
        let node = self.create_base_expression(SyntaxKind::VoidExpression);
        let mut node = VoidExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(&expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node
    }

    pub fn update_void_expression(
        &self,
        node: &Node, /*VoidExpression*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_void_expression = node.as_void_expression();
        if !Gc::ptr_eq(&node_as_void_expression.expression, &expression) {
            self.update(self.create_void_expression(expression).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_await_expression(&self, expression: Gc<Node /*Expression*/>) -> AwaitExpression {
        let node = self.create_base_expression(SyntaxKind::AwaitExpression);
        let mut node = AwaitExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(&expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | TransformFlags::ContainsES2017
                | TransformFlags::ContainsES2018
                | TransformFlags::ContainsAwait,
        );
        node
    }

    pub fn update_await_expression(
        &self,
        node: &Node, /*AwaitExpression*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_await_expression = node.as_await_expression();
        if !Gc::ptr_eq(&node_as_await_expression.expression, &expression) {
            self.update(self.create_await_expression(expression).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_prefix_unary_expression(
        &self,
        operator: SyntaxKind, /*PrefixUnaryOperator*/
        operand: Gc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        let node = self.create_base_expression(SyntaxKind::PrefixUnaryExpression);
        let mut node = PrefixUnaryExpression::new(
            node,
            operator,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(&operand),
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

    pub fn update_prefix_unary_expression(
        &self,
        node: &Node, /*PrefixUnaryExpression*/
        operand: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_prefix_unary_expression = node.as_prefix_unary_expression();
        if !Gc::ptr_eq(&node_as_prefix_unary_expression.operand, &operand) {
            self.update(
                self.create_prefix_unary_expression(
                    node_as_prefix_unary_expression.operator,
                    operand,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_postfix_unary_expression(
        &self,
        operand: Gc<Node /*Expression*/>,
        operator: SyntaxKind,
    ) -> PostfixUnaryExpression {
        let node = self.create_base_expression(SyntaxKind::PostfixUnaryExpression);
        let mut node = PostfixUnaryExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_postfix_unary(&operand),
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

    pub fn update_postfix_unary_expression(
        &self,
        node: &Node, /*PostfixUnaryExpression*/
        operand: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_postfix_unary_expression = node.as_postfix_unary_expression();
        if !Gc::ptr_eq(&node_as_postfix_unary_expression.operand, &operand) {
            self.update(
                self.create_postfix_unary_expression(
                    operand,
                    node_as_postfix_unary_expression.operator,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_binary_expression(
        &self,
        left: Gc<Node /*Expression*/>,
        operator: impl Into<SyntaxKindOrRcNode>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        let node = self.create_base_expression(SyntaxKind::BinaryExpression);
        let operator_token = self.as_token(operator.into());
        let operator_kind = operator_token.kind();
        let mut node = BinaryExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_binary(operator_kind, &left),
            operator_token,
            self.parenthesizer_rules()
                .parenthesize_right_side_of_binary(operator_kind, Some(left), &right),
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

    pub fn update_binary_expression(
        &self,
        node: &Node, /*BinaryExpression*/
        left: Gc<Node /*Expression*/>,
        operator: Gc<Node /*BinaryOperatorToken*/>,
        right: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_binary_expression = node.as_binary_expression();
        if !Gc::ptr_eq(&node_as_binary_expression.left, &left)
            || !Gc::ptr_eq(&node_as_binary_expression.operator_token, &operator)
            || !Gc::ptr_eq(&node_as_binary_expression.right, &right)
        {
            self.update(
                self.create_binary_expression(left, operator, right).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
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
        condition: Gc<Node /*Expression*/>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        when_true: Gc<Node /*Expression*/>,
        colon_token: Option<Gc<Node /*ColonToken*/>>,
        when_false: Gc<Node /*Expression*/>,
    ) -> ConditionalExpression {
        let node = self.create_base_expression(SyntaxKind::ConditionalExpression);
        let mut node = ConditionalExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_condition_of_conditional_expression(&condition),
            question_token.unwrap_or_else(|| self.create_token(SyntaxKind::QuestionToken).wrap()),
            self.parenthesizer_rules()
                .parenthesize_branch_of_conditional_expression(&when_true),
            colon_token.unwrap_or_else(|| self.create_token(SyntaxKind::ColonToken).wrap()),
            self.parenthesizer_rules()
                .parenthesize_branch_of_conditional_expression(&when_false),
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

    pub fn update_conditional_expression(
        &self,
        node: &Node, /*ConditionalExpression*/
        condition: Gc<Node /*Expression*/>,
        question_token: Gc<Node /*QuestionToken*/>,
        when_true: Gc<Node /*Expression*/>,
        colon_token: Gc<Node /*ColonToken*/>,
        when_false: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_conditional_expression = node.as_conditional_expression();
        if !Gc::ptr_eq(&node_as_conditional_expression.condition, &condition)
            || !Gc::ptr_eq(
                &node_as_conditional_expression.question_token,
                &question_token,
            )
            || !Gc::ptr_eq(&node_as_conditional_expression.when_true, &when_true)
            || !Gc::ptr_eq(&node_as_conditional_expression.colon_token, &colon_token)
            || !Gc::ptr_eq(&node_as_conditional_expression.when_false, &when_false)
        {
            self.update(
                self.create_conditional_expression(
                    condition,
                    Some(question_token),
                    when_true,
                    Some(colon_token),
                    when_false,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_template_expression(
        &self,
        head: Gc<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateSpan>*/
    ) -> TemplateExpression {
        let node = self.create_base_expression(SyntaxKind::TemplateExpression);
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

    pub fn update_template_expression(
        &self,
        node: &Node, /*TemplateExpression*/
        head: Gc<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateSpan>*/
    ) -> Gc<Node> {
        let node_as_template_expression = node.as_template_expression();
        let template_spans = template_spans.into();
        if !Gc::ptr_eq(&node_as_template_expression.head, &head)
            || has_node_array_changed(&node_as_template_expression.template_spans, &template_spans)
        {
            self.update(
                self.create_template_expression(head, template_spans).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    fn create_template_literal_like_node_checked(
        &self,
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
        self.create_template_literal_like_node(kind, text.unwrap(), raw_text, Some(template_flags))
    }

    pub fn create_template_literal_like_node(
        &self,
        kind: SyntaxKind,
        text: String,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        let template_flags = template_flags.unwrap_or(TokenFlags::None);
        let node = self.create_base_token(kind);
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
        text: Option<String>,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        self.create_template_literal_like_node_checked(
            SyntaxKind::TemplateHead,
            text,
            raw_text,
            template_flags,
        )
    }

    pub fn create_template_middle(
        &self,
        text: Option<String>,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        self.create_template_literal_like_node_checked(
            SyntaxKind::TemplateMiddle,
            text,
            raw_text,
            template_flags,
        )
    }

    pub fn create_template_tail(
        &self,
        text: Option<String>,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        self.create_template_literal_like_node_checked(
            SyntaxKind::TemplateTail,
            text,
            raw_text,
            template_flags,
        )
    }

    pub fn create_no_substitution_template_literal(
        &self,
        text: Option<String>,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        self.create_template_literal_like_node_checked(
            SyntaxKind::NoSubstitutionTemplateLiteral,
            text,
            raw_text,
            template_flags,
        )
    }

    pub fn create_yield_expression(
        &self,
        asterisk_token: Option<Gc<Node /*AsteriskToken*/>>,
        expression: Option<Gc<Node /*Expression*/>>,
    ) -> YieldExpression {
        let node = self.create_base_expression(SyntaxKind::YieldExpression);
        let mut node = YieldExpression::new(
            node,
            expression.map(|expression| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(&expression)
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

    pub fn update_yield_expression(
        &self,
        node: &Node, /*Gc<Node>*/
        asterisk_token: Option<Gc<Node /*AsteriskToken*/>>,
        expression: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        let node_as_yield_expression = node.as_yield_expression();
        if !are_option_gcs_equal(
            node_as_yield_expression.expression.as_ref(),
            expression.as_ref(),
        ) || !are_option_gcs_equal(
            node_as_yield_expression.asterisk_token.as_ref(),
            asterisk_token.as_ref(),
        ) {
            self.update(
                self.create_yield_expression(asterisk_token, expression)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_spread_element(&self, expression: Gc<Node /*Expression*/>) -> SpreadElement {
        let node = self.create_base_expression(SyntaxKind::SpreadElement);
        let mut node = SpreadElement::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(&expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | TransformFlags::ContainsES2015
                | TransformFlags::ContainsRestOrSpread,
        );
        node
    }

    pub fn update_spread_element(
        &self,
        node: &Node, /*SpreadElement*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_spread_element = node.as_spread_element();
        if !Gc::ptr_eq(&node_as_spread_element.expression, &expression) {
            self.update(self.create_spread_element(expression).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_class_expression<'name>(
        &self,
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
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    heritage_clauses,
                    members,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }
}
