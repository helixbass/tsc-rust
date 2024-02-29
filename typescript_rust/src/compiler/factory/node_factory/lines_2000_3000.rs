use id_arena::Id;
use local_macros::generate_node_factory_method_wrapper;

use super::{
    get_cooked_text, propagate_child_flags, propagate_children_flags,
    propagate_identifier_name_flags, CookedText,
};
use crate::{
    get_elements_of_binding_or_assignment_pattern, get_target_of_binding_or_assignment_element,
    has_invalid_escape, has_node_array_changed, has_option_node_array_changed,
    is_array_literal_expression, is_assignment_pattern, is_call_chain, is_element_access_chain,
    is_generated_identifier, is_identifier, is_import_keyword, is_local_name,
    is_logical_or_coalescing_assignment_operator, is_object_literal_expression,
    is_omitted_expression, is_property_access_chain, is_super_keyword, is_super_property,
    last_or_undefined, modifiers_to_flags, ArrayBindingPattern, ArrayLiteralExpression,
    ArrowFunction, AsDoubleDeref, AwaitExpression, BaseLiteralLikeNode, BaseNode, BinaryExpression,
    BindingElement, CallExpression, ClassExpression, ClassLikeDeclarationInterface,
    ConditionalExpression, Debug_, DeleteExpression, ElementAccessExpression, FunctionExpression,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasTypeArgumentsInterface,
    HasTypeInterface, HasTypeParametersInterface, ImportTypeNode, InArena, IndexedAccessTypeNode,
    InferTypeNode, InterfaceOrClassLikeDeclarationInterface, LiteralTypeNode, MappedTypeNode,
    ModifierFlags, NamedDeclarationInterface, NewExpression, Node, NodeArray, NodeArrayOrVec,
    NodeFactory, NodeFlags, NodeInterface, ObjectBindingPattern, ObjectLiteralExpression,
    OptionInArena, ParenthesizedExpression, ParenthesizedTypeNode, PostfixUnaryExpression,
    PrefixUnaryExpression, PropertyAccessExpression, SignatureDeclarationInterface, SpreadElement,
    StrOrRcNode, StringOrNumberOrBoolOrRcNode, SyntaxKind, SyntaxKindOrRcNode,
    TaggedTemplateExpression, TemplateExpression, TemplateLiteralLikeNode, TemplateLiteralTypeNode,
    ThisTypeNode, TokenFlags, TransformFlags, TypeAssertion, TypeOfExpression, TypeOperatorNode,
    VoidExpression, YieldExpression,
};

impl NodeFactory {
    #[generate_node_factory_method_wrapper]
    pub fn create_infer_type_node_raw(
        &self,
        type_parameter: Id<Node /*TypeParameterDeclaration*/>,
    ) -> InferTypeNode {
        let node = self.create_base_node(SyntaxKind::InferType);
        let node = InferTypeNode::new(node, type_parameter);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_infer_type_node(
        &self,
        node: Id<Node>, /*InferTypeNode*/
        type_parameter: Id<Node /*TypeParameterDeclaration*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_infer_type_node = node_ref.as_infer_type_node();
        if node_as_infer_type_node.type_parameter != type_parameter {
            self.update(self.create_infer_type_node(type_parameter), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_template_literal_type_raw(
        &self,
        head: Id<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateLiteralTypeSpan>*/
    ) -> TemplateLiteralTypeNode {
        let node = self.create_base_node(SyntaxKind::TemplateLiteralType);
        let node = TemplateLiteralTypeNode::new(
            node,
            head,
            self.create_node_array(Some(template_spans), None),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_template_literal_type(
        &self,
        node: Id<Node>, /*TemplateLiteralTypeSpan*/
        head: Id<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateLiteralTypeSpan>*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_template_literal_type_node = node_ref.as_template_literal_type_node();
        let template_spans = template_spans.into();
        if node_as_template_literal_type_node.head != head
            || has_node_array_changed(
                node_as_template_literal_type_node.template_spans,
                &template_spans,
            )
        {
            self.update(
                self.create_template_literal_type(head, template_spans),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_import_type_node_raw<TTypeArguments: Into<NodeArrayOrVec>>(
        &self,
        argument: Id<Node /*TypeNode*/>,
        qualifier: Option<Id<Node /*EntityName*/>>,
        type_arguments: Option<TTypeArguments /*<TypeNode>*/>,
        is_type_of: Option<bool>,
    ) -> ImportTypeNode {
        let is_type_of = is_type_of.unwrap_or(false);
        let node = self.create_base_node(SyntaxKind::ImportType);
        let node = ImportTypeNode::new(
            node,
            argument,
            qualifier,
            type_arguments.and_then(|type_arguments| {
                self.parenthesizer_rules()
                    .ref_(self)
                    .parenthesize_type_arguments(Some(type_arguments.into()))
            }),
            is_type_of,
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_import_type_node(
        &self,
        node: Id<Node>, /*ImportTypeNode*/
        argument: Id<Node /*TypeNode*/>,
        qualifier: Option<Id<Node /*EntityName*/>>,
        type_arguments: Option<impl Into<NodeArrayOrVec /*<TypeNode>*/>>,
        is_type_of: Option<bool>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_import_type_node = node_ref.as_import_type_node();
        let is_type_of = is_type_of.unwrap_or_else(|| node_as_import_type_node.is_type_of());
        let type_arguments = type_arguments.map(Into::into);
        if node_as_import_type_node.argument != argument
            || node_as_import_type_node.qualifier != qualifier
            || has_option_node_array_changed(
                node_as_import_type_node.maybe_type_arguments(),
                type_arguments.as_ref(),
            )
            || node_as_import_type_node.is_type_of() != is_type_of
        {
            self.update(
                self.create_import_type_node(argument, qualifier, type_arguments, Some(is_type_of)),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_parenthesized_type_raw(
        &self,
        type_: Id<Node /*TypeNode*/>,
    ) -> ParenthesizedTypeNode {
        let node = self.create_base_node(SyntaxKind::ParenthesizedType);
        let node = ParenthesizedTypeNode::new(node, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_parenthesized_type(
        &self,
        node: Id<Node>, /*ParenthesizedTypeNode*/
        type_: Id<Node /*TypeNode*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_parenthesized_type_node = node_ref.as_parenthesized_type_node();
        if node_as_parenthesized_type_node.type_ != type_ {
            self.update(self.create_parenthesized_type(type_), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_this_type_node_raw(&self) -> ThisTypeNode {
        let node = self.create_base_node(SyntaxKind::ThisType);
        let node = ThisTypeNode::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_operator_node_raw(
        &self,
        operator: SyntaxKind,
        type_: Id<Node /*TypeNode*/>,
    ) -> TypeOperatorNode {
        let node = self.create_base_node(SyntaxKind::TypeOperator);
        let node = TypeOperatorNode::new(
            node,
            operator,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_member_of_element_type(type_),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_operator_node(
        &self,
        node: Id<Node>, /*TypeOperatorNode*/
        type_: Id<Node /*TypeNode*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_type_operator_node = node_ref.as_type_operator_node();
        if node_as_type_operator_node.type_ != type_ {
            self.update(
                self.create_type_operator_node(node_as_type_operator_node.operator, type_),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_indexed_access_type_node_raw(
        &self,
        object_type: Id<Node /*TypeNode*/>,
        index_type: Id<Node /*TypeNode*/>,
    ) -> IndexedAccessTypeNode {
        let node = self.create_base_node(SyntaxKind::IndexedAccessType);
        let node = IndexedAccessTypeNode::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_member_of_element_type(object_type),
            index_type,
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_indexed_access_type_node(
        &self,
        node: Id<Node>, /*IndexedAccessTypeNode*/
        object_type: Id<Node /*TypeNode*/>,
        index_type: Id<Node /*TypeNode*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_indexed_access_type_node = node_ref.as_indexed_access_type_node();
        if node_as_indexed_access_type_node.object_type != object_type
            || node_as_indexed_access_type_node.index_type != index_type
        {
            self.update(
                self.create_indexed_access_type_node(object_type, index_type),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_mapped_type_node_raw(
        &self,
        readonly_token: Option<Id<Node /*ReadonlyKeyword | PlusToken | MinusToken*/>>,
        type_parameter: Id<Node /*TypeParameterDeclaration*/>,
        name_type: Option<Id<Node /*TypeNode*/>>,
        question_token: Option<Id<Node /*QuestionToken | PlusToken | MinusToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        members: Option<impl Into<NodeArrayOrVec> /*<TypeElement>*/>,
    ) -> MappedTypeNode {
        let node = self.create_base_node(SyntaxKind::MappedType);
        let node = MappedTypeNode::new(
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
        node: Id<Node>, /*MappedTypeNode*/
        readonly_token: Option<Id<Node /*ReadonlyKeyword | PlusToken | MinusToken*/>>,
        type_parameter: Id<Node /*TypeParameterDeclaration*/>,
        name_type: Option<Id<Node /*TypeNode*/>>,
        question_token: Option<Id<Node /*QuestionToken | PlusToken | MinusToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        members: Option<Id<NodeArray /*<TypeElement>*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_mapped_type_node = node_ref.as_mapped_type_node();
        if node_as_mapped_type_node.readonly_token != readonly_token
            || node_as_mapped_type_node.name_type != name_type
            || node_as_mapped_type_node.question_token != question_token
            || node_as_mapped_type_node.type_ != type_
            || node_as_mapped_type_node.members != members
        {
            self.update(
                self.create_mapped_type_node(
                    readonly_token,
                    type_parameter,
                    name_type,
                    question_token,
                    type_,
                    members,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_literal_type_node_raw(
        &self,
        literal: Id<Node /*LiteralTypeNode["literal"]*/>,
    ) -> LiteralTypeNode {
        let node = self.create_base_node(SyntaxKind::LiteralType);
        let node = LiteralTypeNode::new(node, literal);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_literal_type_node(
        &self,
        node: Id<Node>, /*LiteralTypeNode*/
        literal: Id<Node /*LiteralTypeNode["literal"]*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_literal_type_node = node_ref.as_literal_type_node();
        if node_as_literal_type_node.literal != literal {
            self.update(self.create_literal_type_node(literal), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_object_binding_pattern_raw(
        &self,
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
    ) -> ObjectBindingPattern {
        let node = self.create_base_node(SyntaxKind::ObjectBindingPattern);
        let node = ObjectBindingPattern::new(node, self.create_node_array(Some(elements), None));
        node.add_transform_flags(
            propagate_children_flags(Some(&node.elements.ref_(self)))
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
        node: Id<Node>, /*ObjectBindingPattern*/
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_object_binding_pattern = node_ref.as_object_binding_pattern();
        let elements = elements.into();
        if has_node_array_changed(node_as_object_binding_pattern.elements, &elements) {
            self.update(self.create_object_binding_pattern(elements), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_array_binding_pattern_raw(
        &self,
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
    ) -> ArrayBindingPattern {
        let node = self.create_base_node(SyntaxKind::ArrayBindingPattern);
        let node = ArrayBindingPattern::new(node, self.create_node_array(Some(elements), None));
        node.add_transform_flags(
            propagate_children_flags(Some(&node.elements.ref_(self)))
                | TransformFlags::ContainsES2015
                | TransformFlags::ContainsBindingPattern,
        );
        node
    }

    pub fn update_array_binding_pattern(
        &self,
        node: Id<Node>, /*ArrayBindingPattern*/
        elements: impl Into<NodeArrayOrVec /*<ArrayBindingElement>*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_array_binding_pattern = node_ref.as_array_binding_pattern();
        let elements = elements.into();
        if has_node_array_changed(node_as_array_binding_pattern.elements, &elements) {
            self.update(self.create_array_binding_pattern(elements), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_binding_element_raw<'property_name, 'name>(
        &self,
        dot_dot_dot_token: Option<Id<Node /*DotDotDotToken*/>>,
        property_name: Option<impl Into<StrOrRcNode<'property_name> /*PropertyName*/>>,
        name: impl Into<StrOrRcNode<'name>>, /*BindingName*/
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> BindingElement {
        let node = self.create_base_binding_like_declaration(
            SyntaxKind::BindingElement,
            Option::<Id<NodeArray>>::None,
            Option::<Id<NodeArray>>::None,
            Some(name),
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .ref_(self)
                    .parenthesize_expression_for_disallowed_comma(initializer)
            }),
        );
        let dot_dot_dot_token_is_some = dot_dot_dot_token.is_some();
        let node = BindingElement::new(node, self.as_name(property_name), dot_dot_dot_token);
        node.add_transform_flags(
            propagate_child_flags(node.dot_dot_dot_token.clone(), self)
                | TransformFlags::ContainsES2015,
        );
        if let Some(node_property_name) = node.property_name {
            node.add_transform_flags(if is_identifier(&node_property_name.ref_(self)) {
                propagate_identifier_name_flags(node_property_name, self)
            } else {
                propagate_child_flags(Some(node_property_name), self)
            });
        }
        if dot_dot_dot_token_is_some {
            node.add_transform_flags(TransformFlags::ContainsRestOrSpread);
        }
        node
    }

    pub fn update_binding_element(
        &self,
        node: Id<Node>, /*BindingElement*/
        dot_dot_dot_token: Option<Id<Node /*DotDotDotToken*/>>,
        property_name: Option<Id<Node /*PropertyName*/>>,
        name: Id<Node /*BindingName*/>,
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_binding_element = node_ref.as_binding_element();
        if node_as_binding_element.property_name != property_name
            || node_as_binding_element.dot_dot_dot_token != dot_dot_dot_token
            || node_as_binding_element.name() != name
            || node_as_binding_element.maybe_initializer() != initializer
        {
            self.update(
                self.create_binding_element(dot_dot_dot_token, property_name, name, initializer),
                node,
            )
        } else {
            node
        }
    }

    pub(crate) fn create_base_expression(&self, kind: SyntaxKind) -> BaseNode {
        let node = self.create_base_node(kind);
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_array_literal_expression_raw(
        &self,
        elements: Option<impl Into<NodeArrayOrVec>>, /*Expression*/
        multi_line: Option<bool>,
    ) -> ArrayLiteralExpression {
        let node = self.create_base_expression(SyntaxKind::ArrayLiteralExpression);
        let elements = elements.map(|elements| elements.into());
        let elements_vec = elements.clone().map(|elements| match elements {
            NodeArrayOrVec::NodeArray(node_array) => node_array.ref_(self).to_vec(),
            NodeArrayOrVec::Vec(elements) => elements,
        });
        let last_element = elements_vec
            .as_ref()
            .and_then(|elements_vec| last_or_undefined(elements_vec));
        let elements_array = self.create_node_array(
            elements,
            last_element.and_then(|last_element| {
                if is_omitted_expression(&last_element.ref_(self)) {
                    Some(true)
                } else {
                    None
                }
            }),
        );
        let node = ArrayLiteralExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_expressions_of_comma_delimited_list(elements_array.into()),
            multi_line,
        );
        node.add_transform_flags(propagate_children_flags(Some(&node.elements.ref_(self))));
        node
    }

    pub fn update_array_literal_expression(
        &self,
        node: Id<Node>,                      /*ArrayLiteralExpression*/
        elements: impl Into<NodeArrayOrVec>, /*Expression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_array_literal_expression = node_ref.as_array_literal_expression();
        let elements = elements.into();
        if has_node_array_changed(node_as_array_literal_expression.elements, &elements) {
            self.update(
                self.create_array_literal_expression(
                    Some(elements),
                    node_as_array_literal_expression.multi_line,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_object_literal_expression_raw(
        &self,
        properties: Option<impl Into<NodeArrayOrVec> /*ObjectLiteralElementLike*/>,
        multi_line: Option<bool>,
    ) -> ObjectLiteralExpression {
        let node = self.create_base_expression(SyntaxKind::ObjectLiteralExpression);
        let node = ObjectLiteralExpression::new(
            node,
            self.create_node_array(properties, None),
            multi_line,
        );
        node.add_transform_flags(propagate_children_flags(Some(&node.properties.ref_(self))));
        node
    }

    pub fn update_object_literal_expression(
        &self,
        node: Id<Node>,                        /*ObjectLiteralExpression*/
        properties: impl Into<NodeArrayOrVec>, /*ObjectLiteralElementLike*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_object_literal_expression = node_ref.as_object_literal_expression();
        let properties = properties.into();
        if has_node_array_changed(node_as_object_literal_expression.properties, &properties) {
            self.update(
                self.create_object_literal_expression(
                    Some(properties),
                    node_as_object_literal_expression.multi_line,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_property_access_expression_raw<'name>(
        &self,
        expression: Id<Node /*Expression*/>,
        name: impl Into<StrOrRcNode<'name>>,
    ) -> PropertyAccessExpression {
        let node = self.create_base_expression(SyntaxKind::PropertyAccessExpression);
        let node = PropertyAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(expression),
            None,
            self.as_name(Some(name)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | if is_identifier(&node.name.ref_(self)) {
                    propagate_identifier_name_flags(node.name, self)
                } else {
                    propagate_child_flags(Some(node.name), self)
                },
        );
        if is_super_keyword(&expression.ref_(self)) {
            node.add_transform_flags(
                TransformFlags::ContainsES2017 | TransformFlags::ContainsES2018,
            );
        }
        node
    }

    pub fn update_property_access_expression(
        &self,
        node: Id<Node>, /*PropertyAccessExpression*/
        expression: Id<Node /*Expression*/>,
        name: Id<Node /*Identifier | PrivateIdentifier*/>,
    ) -> Id<Node> {
        if is_property_access_chain(&node.ref_(self)) {
            return self.update_property_access_chain(
                node,
                expression,
                node.ref_(self)
                    .as_property_access_expression()
                    .question_dot_token
                    .clone(),
                name,
            );
        }
        if node.ref_(self).as_property_access_expression().expression != expression
            || node.ref_(self).as_property_access_expression().name != name
        {
            self.update(
                self.create_property_access_expression(expression, name),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_property_access_chain_raw<'name>(
        &self,
        expression: Id<Node /*Expression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
        name: impl Into<StrOrRcNode<'name>>,
    ) -> PropertyAccessExpression {
        let node = self.create_base_expression(SyntaxKind::PropertyAccessExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let node = PropertyAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(expression),
            question_dot_token,
            self.as_name(Some(name)).unwrap(),
        );
        node.add_transform_flags(
            TransformFlags::ContainsES2020
                | propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(node.question_dot_token.clone(), self)
                | if is_identifier(&node.name.ref_(self)) {
                    propagate_identifier_name_flags(node.name, self)
                } else {
                    propagate_child_flags(Some(node.name), self)
                },
        );
        node
    }

    pub fn update_property_access_chain(
        &self,
        node: Id<Node>, /*PropertyAccessChain*/
        expression: Id<Node /*Expression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
        name: Id<Node /*Identifier | PrivateIdentifier*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_property_access_expression = node_ref.as_property_access_expression();
        Debug_.assert(
            node.ref_(self).flags().intersects(NodeFlags::OptionalChain),
            Some("Cannot update a PropertyAccessExpression using updatePropertyAccessChain. Use updatePropertyAccess instead.")
        );
        if node_as_property_access_expression.expression != expression
            || node_as_property_access_expression.question_dot_token != question_dot_token
            || node_as_property_access_expression.expression != expression
        {
            self.update(
                self.create_property_access_chain(expression, question_dot_token, name),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_element_access_expression_raw(
        &self,
        expression: Id<Node /*Expression*/>,
        index: impl Into<StringOrNumberOrBoolOrRcNode>,
    ) -> ElementAccessExpression {
        let node = self.create_base_expression(SyntaxKind::ElementAccessExpression);
        let node = ElementAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(expression),
            None,
            self.as_expression(index),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(Some(node.argument_expression), self),
        );
        if is_super_keyword(&expression.ref_(self)) {
            node.add_transform_flags(
                TransformFlags::ContainsES2017 | TransformFlags::ContainsES2018,
            );
        }
        node
    }

    pub fn update_element_access_expression(
        &self,
        node: Id<Node>, /*ElementAccessExpression*/
        expression: Id<Node /*Expression*/>,
        argument_expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_element_access_expression = node_ref.as_element_access_expression();
        if is_element_access_chain(&node.ref_(self)) {
            return self.update_element_access_chain(
                node,
                expression,
                node_as_element_access_expression.question_dot_token.clone(),
                argument_expression,
            );
        }
        if node_as_element_access_expression.expression != expression
            || node_as_element_access_expression.argument_expression != argument_expression
        {
            self.update(
                self.create_element_access_expression(expression, argument_expression),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_element_access_chain_raw(
        &self,
        expression: Id<Node /*Expression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
        index: impl Into<StringOrNumberOrBoolOrRcNode>,
    ) -> ElementAccessExpression {
        let node = self.create_base_expression(SyntaxKind::ElementAccessExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let node = ElementAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(expression),
            question_dot_token,
            self.as_expression(index),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(node.question_dot_token.clone(), self)
                | propagate_child_flags(Some(node.argument_expression), self)
                | TransformFlags::ContainsES2020,
        );
        node
    }

    pub fn update_element_access_chain(
        &self,
        node: Id<Node>, /*ElementAccessChain*/
        expression: Id<Node /*Expression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
        argument_expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_element_access_expression = node_ref.as_element_access_expression();
        Debug_.assert(
            node.ref_(self).flags().intersects(NodeFlags::OptionalChain),
            Some("Cannot update a ElementAccessExpression using updateElementAccessChain. Use updateElementAccess instead.")
        );
        if node_as_element_access_expression.expression != expression
            || node_as_element_access_expression.question_dot_token != question_dot_token
            || node_as_element_access_expression.argument_expression != argument_expression
        {
            self.update(
                self.create_element_access_chain(
                    expression,
                    question_dot_token,
                    argument_expression,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_call_expression_raw(
        &self,
        expression: Id<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec /*<TypeNode>*/>>,
        arguments_array: Option<impl Into<NodeArrayOrVec /*<Expression>*/>>,
    ) -> CallExpression {
        let node = self.create_base_expression(SyntaxKind::CallExpression);
        let node = CallExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(expression),
            None,
            self.as_node_array(type_arguments),
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_expressions_of_comma_delimited_list(
                    self.create_node_array(arguments_array, None).into(),
                ),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_children_flags(node.maybe_type_arguments().refed(self).as_deref())
                | propagate_children_flags(Some(&node.arguments.ref_(self))),
        );
        if node.maybe_type_arguments().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        if is_import_keyword(&node.expression.ref_(self)) {
            node.add_transform_flags(TransformFlags::ContainsDynamicImport);
        } else if is_super_property(node.expression, self) {
            node.add_transform_flags(TransformFlags::ContainsLexicalThis);
        }
        node
    }

    pub fn update_call_expression(
        &self,
        node: Id<Node>, /*CallExpression*/
        expression: Id<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec /*<TypeNode>*/>>,
        arguments_array: impl Into<NodeArrayOrVec /*<Expression>*/>,
    ) -> Id<Node /*CallExpression*/> {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        if is_call_chain(&node.ref_(self)) {
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
        if node_as_call_expression.expression != expression
            || has_option_node_array_changed(
                node_as_call_expression.maybe_type_arguments(),
                type_arguments.as_ref(),
            )
            || has_node_array_changed(node_as_call_expression.arguments, &arguments_array)
        {
            self.update(
                self.create_call_expression(expression, type_arguments, Some(arguments_array)),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_call_chain_raw(
        &self,
        expression: Id<Node /*Expression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
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
        let node = CallExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(expression),
            question_dot_token,
            self.as_node_array(type_arguments),
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_expressions_of_comma_delimited_list(
                    self.create_node_array(arguments_array, None).into(),
                ),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(node.question_dot_token.clone(), self)
                | propagate_children_flags(node.maybe_type_arguments().refed(self).as_deref())
                | propagate_children_flags(Some(&node.arguments.ref_(self)))
                | TransformFlags::ContainsES2020,
        );
        if node.maybe_type_arguments().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        if is_super_property(node.expression, self) {
            node.add_transform_flags(TransformFlags::ContainsLexicalThis);
        }
        node
    }

    pub fn update_call_chain(
        &self,
        node: Id<Node>, /*CallChain*/
        expression: Id<Node /*Expression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
        type_arguments: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeNode>*/
        >,
        arguments_array: impl Into<NodeArrayOrVec>,
        /*<Expression>*/
    ) -> Id<Node /*CallExpression*/> {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        let type_arguments = type_arguments.map(Into::into);
        let arguments_array = arguments_array.into();
        Debug_.assert(
            node.ref_(self).flags().intersects(NodeFlags::OptionalChain),
            Some("Cannot update a CallExpression using updateCallChain. Use updateCall instead."),
        );
        if node_as_call_expression.expression != expression
            || node_as_call_expression.question_dot_token != question_dot_token
            || has_option_node_array_changed(
                node_as_call_expression.maybe_type_arguments(),
                type_arguments.as_ref(),
            )
            || has_node_array_changed(node_as_call_expression.arguments, &arguments_array)
        {
            self.update(
                self.create_call_chain(
                    expression,
                    question_dot_token,
                    type_arguments,
                    Some(arguments_array),
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_new_expression_raw(
        &self,
        expression: Id<Node /*Expression*/>,
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
        let node = NewExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_expression_of_new(expression),
            self.as_node_array(type_arguments),
            arguments_array.map(|arguments_array| {
                self.parenthesizer_rules()
                    .ref_(self)
                    .parenthesize_expressions_of_comma_delimited_list(arguments_array.into())
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_children_flags(node.maybe_type_arguments().refed(self).as_deref())
                | propagate_children_flags(node.arguments.refed(self).as_deref())
                | TransformFlags::ContainsES2020,
        );
        if node.maybe_type_arguments().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_new_expression(
        &self,
        node: Id<Node>, /*NewExpression*/
        expression: Id<Node /*Expression*/>,
        type_arguments: Option<
            impl Into<NodeArrayOrVec>,
            /*<TypeNode>*/
        >,
        arguments_array: Option<
            impl Into<NodeArrayOrVec>,
            /*<Expression>*/
        >,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_new_expression = node_ref.as_new_expression();
        let type_arguments = type_arguments.map(Into::into);
        let arguments_array = arguments_array.map(Into::into);
        if node_as_new_expression.expression != expression
            || has_option_node_array_changed(
                node_as_new_expression.maybe_type_arguments(),
                type_arguments.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_new_expression.arguments,
                arguments_array.as_ref(),
            )
        {
            self.update(
                self.create_new_expression(expression, type_arguments, arguments_array),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_tagged_template_expression_raw(
        &self,
        tag: Id<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec> /*<TypeNode>*/>,
        template: Id<Node /*TemplateLiteral*/>,
    ) -> TaggedTemplateExpression {
        let node = self.create_base_expression(SyntaxKind::TaggedTemplateExpression);
        let node = TaggedTemplateExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(tag),
            self.as_node_array(type_arguments),
            template,
            None,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.tag), self)
                | propagate_children_flags(node.maybe_type_arguments().refed(self).as_deref())
                | propagate_child_flags(Some(node.template), self)
                | TransformFlags::ContainsES2015,
        );
        if node.maybe_type_arguments().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        if has_invalid_escape(node.template, self) {
            node.add_transform_flags(TransformFlags::ContainsES2018);
        }
        node
    }

    pub fn update_tagged_template_expression(
        &self,
        node: Id<Node>, /*TaggedTemplateExpression*/
        tag: Id<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec> /*<TypeNode>*/>,
        template: Id<Node /*TemplateLiteral*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_tagged_template_expression = node_ref.as_tagged_template_expression();
        let type_arguments = type_arguments.map(Into::into);
        if node_as_tagged_template_expression.tag != tag
            || has_option_node_array_changed(
                node_as_tagged_template_expression.maybe_type_arguments(),
                type_arguments.as_ref(),
            )
            || node_as_tagged_template_expression.template != template
        {
            self.update(
                self.create_tagged_template_expression(tag, type_arguments, template),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_assertion_raw(
        &self,
        type_: Id<Node /*TypeNode*/>,
        expression: Id<Node /*Expression*/>,
    ) -> TypeAssertion {
        let node = self.create_base_expression(SyntaxKind::TypeAssertionExpression);
        let node = TypeAssertion::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_operand_of_prefix_unary(expression),
            type_,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(Some(node.type_), self)
                | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_type_assertion(
        &self,
        node: Id<Node>, /*TypeAssertion*/
        type_: Id<Node /*TypeNode*/>,
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_type_assertion = node_ref.as_type_assertion();
        if node_as_type_assertion.type_ != type_ || node_as_type_assertion.expression != expression
        {
            self.update(self.create_type_assertion(type_, expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_parenthesized_expression_raw(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> ParenthesizedExpression {
        let node = self.create_base_expression(SyntaxKind::ParenthesizedExpression);
        let node = ParenthesizedExpression::new(node, expression);
        node.add_transform_flags(propagate_child_flags(Some(node.expression), self));
        node
    }

    pub fn update_parenthesized_expression(
        &self,
        node: Id<Node>, /*ParenthesizedExpression*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_parenthesized_expression = node_ref.as_parenthesized_expression();
        if node_as_parenthesized_expression.expression != expression {
            self.update(self.create_parenthesized_expression(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_function_expression_raw<'name>(
        &self,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Id<Node>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: Option<impl Into<NodeArrayOrVec>>,
        type_: Option<Id<Node>>,
        body: Id<Node>,
    ) -> FunctionExpression {
        let mut node = self.create_base_function_like_declaration(
            SyntaxKind::FunctionExpression,
            Option::<Id<NodeArray>>::None,
            modifiers,
            name,
            type_parameters,
            parameters,
            type_,
            Some(body),
        );
        node.asterisk_token = asterisk_token;
        let node = FunctionExpression::new(node);
        node.add_transform_flags(propagate_child_flags(node.maybe_asterisk_token(), self));
        if node.maybe_type_parameters().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        if modifiers_to_flags(node.maybe_modifiers().refed(self).as_double_deref(), self)
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
        node: Id<Node>, /*FunctionExpression*/
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Id<Node /*AsteriskToken*/>>,
        name: Option<Id<Node /*Identifier*/>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Id<Node /*TypeNode*/>>,
        body: Id<Node /*Block*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_function_expression = node_ref.as_function_expression();
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let parameters = parameters.into();
        if node_as_function_expression.maybe_name() != name
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || node_as_function_expression.maybe_asterisk_token() != asterisk_token
            || has_option_node_array_changed(
                node_as_function_expression.maybe_type_parameters(),
                type_parameters.as_ref(),
            )
            || has_node_array_changed(node_as_function_expression.parameters(), &parameters)
            || node_as_function_expression.maybe_type() != type_
            || node_as_function_expression.maybe_body().unwrap() != body
        {
            self.update_base_function_like_declaration(
                self.create_function_expression(
                    modifiers,
                    asterisk_token,
                    name,
                    type_parameters,
                    Some(parameters),
                    type_,
                    body,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_arrow_function_raw(
        &self,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Id<Node>>,
        equals_greater_than_token: Option<Id<Node /*EqualsGreaterThanToken*/>>,
        body: Id<Node /*ConciseBody*/>,
    ) -> ArrowFunction {
        let node = self.create_base_function_like_declaration(
            SyntaxKind::ArrowFunction,
            Option::<Id<NodeArray>>::None,
            modifiers,
            Option::<Id<Node>>::None,
            type_parameters,
            Some(parameters),
            type_,
            Some(
                self.parenthesizer_rules()
                    .ref_(self)
                    .parenthesize_concise_body_of_arrow_function(body),
            ),
        );
        let node = ArrowFunction::new(
            node,
            equals_greater_than_token
                .unwrap_or_else(|| self.create_token(SyntaxKind::EqualsGreaterThanToken)),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.equals_greater_than_token), self)
                | TransformFlags::ContainsES2015,
        );
        if modifiers_to_flags(node.maybe_modifiers().refed(self).as_double_deref(), self)
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
        node: Id<Node>, /*ArrowFunction*/
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Id<Node /*TypeNode*/>>,
        equals_greater_than_token: Id<Node /*EqualsGreaterThanToken*/>,
        body: Id<Node /*ConciseBody*/>,
    ) -> Id<Node /*ArrowFunction*/> {
        let node_ref = node.ref_(self);
        let node_as_arrow_function = node_ref.as_arrow_function();
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let parameters = parameters.into();
        if has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || has_option_node_array_changed(
                node_as_arrow_function.maybe_type_parameters(),
                type_parameters.as_ref(),
            )
            || has_node_array_changed(node_as_arrow_function.parameters(), &parameters)
            || node_as_arrow_function.maybe_type() != type_
            || node_as_arrow_function.equals_greater_than_token != equals_greater_than_token
            || node_as_arrow_function.maybe_body().unwrap() != body
        {
            self.update_base_function_like_declaration(
                self.create_arrow_function(
                    modifiers,
                    type_parameters,
                    parameters,
                    type_,
                    Some(equals_greater_than_token),
                    body,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_delete_expression_raw(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> DeleteExpression {
        let node = self.create_base_expression(SyntaxKind::DeleteExpression);
        let node = DeleteExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_operand_of_prefix_unary(expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(node.expression), self));
        node
    }

    pub fn update_delete_expression(
        &self,
        node: Id<Node>, /*DeleteExpression*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_delete_expression = node_ref.as_delete_expression();
        if node_as_delete_expression.expression != expression {
            self.update(self.create_delete_expression(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_of_expression_raw(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> TypeOfExpression {
        let node = self.create_base_expression(SyntaxKind::TypeOfExpression);
        let node = TypeOfExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_operand_of_prefix_unary(expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(node.expression), self));
        node
    }

    pub fn update_type_of_expression(
        &self,
        node: Id<Node>, /*TypeOfExpression*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_type_of_expression = node_ref.as_type_of_expression();
        if node_as_type_of_expression.expression != expression {
            self.update(self.create_type_of_expression(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_void_expression_raw(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> VoidExpression {
        let node = self.create_base_expression(SyntaxKind::VoidExpression);
        let node = VoidExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_operand_of_prefix_unary(expression),
        );
        node.add_transform_flags(propagate_child_flags(Some(node.expression), self));
        node
    }

    pub fn update_void_expression(
        &self,
        node: Id<Node>, /*VoidExpression*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_void_expression = node_ref.as_void_expression();
        if node_as_void_expression.expression != expression {
            self.update(self.create_void_expression(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_await_expression_raw(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> AwaitExpression {
        let node = self.create_base_expression(SyntaxKind::AwaitExpression);
        let node = AwaitExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_operand_of_prefix_unary(expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | TransformFlags::ContainsES2017
                | TransformFlags::ContainsES2018
                | TransformFlags::ContainsAwait,
        );
        node
    }

    pub fn update_await_expression(
        &self,
        node: Id<Node>, /*AwaitExpression*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_await_expression = node_ref.as_await_expression();
        if node_as_await_expression.expression != expression {
            self.update(self.create_await_expression(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_prefix_unary_expression_raw(
        &self,
        operator: SyntaxKind, /*PrefixUnaryOperator*/
        operand: Id<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        let node = self.create_base_expression(SyntaxKind::PrefixUnaryExpression);
        let node = PrefixUnaryExpression::new(
            node,
            operator,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_operand_of_prefix_unary(operand),
        );
        node.add_transform_flags(propagate_child_flags(Some(node.operand), self));
        if matches!(
            operator,
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ) && is_identifier(&node.operand.ref_(self))
            && !is_generated_identifier(&node.operand.ref_(self))
            && !is_local_name(node.operand, self)
        {
            node.add_transform_flags(TransformFlags::ContainsUpdateExpressionForIdentifier);
        }
        node
    }

    pub fn update_prefix_unary_expression(
        &self,
        node: Id<Node>, /*PrefixUnaryExpression*/
        operand: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_prefix_unary_expression = node_ref.as_prefix_unary_expression();
        if node_as_prefix_unary_expression.operand != operand {
            self.update(
                self.create_prefix_unary_expression(
                    node_as_prefix_unary_expression.operator,
                    operand,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_postfix_unary_expression_raw(
        &self,
        operand: Id<Node /*Expression*/>,
        operator: SyntaxKind,
    ) -> PostfixUnaryExpression {
        let node = self.create_base_expression(SyntaxKind::PostfixUnaryExpression);
        let node = PostfixUnaryExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_operand_of_postfix_unary(operand),
            operator,
        );
        node.add_transform_flags(propagate_child_flags(Some(node.operand), self));
        if is_identifier(&node.operand.ref_(self))
            && !is_generated_identifier(&node.operand.ref_(self))
            && !is_local_name(node.operand, self)
        {
            node.add_transform_flags(TransformFlags::ContainsUpdateExpressionForIdentifier);
        }
        node
    }

    pub fn update_postfix_unary_expression(
        &self,
        node: Id<Node>, /*PostfixUnaryExpression*/
        operand: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_postfix_unary_expression = node_ref.as_postfix_unary_expression();
        if node_as_postfix_unary_expression.operand != operand {
            self.update(
                self.create_postfix_unary_expression(
                    operand,
                    node_as_postfix_unary_expression.operator,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_binary_expression_raw(
        &self,
        left: Id<Node /*Expression*/>,
        operator: impl Into<SyntaxKindOrRcNode>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        let node = self.create_base_expression(SyntaxKind::BinaryExpression);
        let operator_token = self.as_token(operator.into());
        let operator_kind = operator_token.ref_(self).kind();
        let node = BinaryExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_binary(operator_kind, left),
            operator_token,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_right_side_of_binary(operator_kind, Some(left), right),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.left), self)
                | propagate_child_flags(Some(node.operator_token), self)
                | propagate_child_flags(Some(node.right), self),
        );
        if operator_kind == SyntaxKind::QuestionQuestionToken {
            node.add_transform_flags(TransformFlags::ContainsES2020);
        } else if operator_kind == SyntaxKind::EqualsToken {
            if is_object_literal_expression(&node.left.ref_(self)) {
                node.add_transform_flags(
                    TransformFlags::ContainsES2015
                        | TransformFlags::ContainsES2018
                        | TransformFlags::ContainsDestructuringAssignment
                        | self.propagate_assignment_pattern_flags(node.left),
                );
            } else if is_array_literal_expression(&node.left.ref_(self)) {
                node.add_transform_flags(
                    TransformFlags::ContainsES2015
                        | TransformFlags::ContainsDestructuringAssignment
                        | self.propagate_assignment_pattern_flags(node.left),
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
        node: Id<Node>, /*BinaryExpression*/
        left: Id<Node /*Expression*/>,
        operator: Id<Node /*BinaryOperatorToken*/>,
        right: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        if node_as_binary_expression.left != left
            || node_as_binary_expression.operator_token != operator
            || node_as_binary_expression.right != right
        {
            self.update(self.create_binary_expression(left, operator, right), node)
        } else {
            node
        }
    }

    fn propagate_assignment_pattern_flags(
        &self,
        node: Id<Node>, /*AssignmentPattern*/
    ) -> TransformFlags {
        if node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            return TransformFlags::ContainsObjectRestOrSpread;
        }
        if node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsES2018)
        {
            for element in get_elements_of_binding_or_assignment_pattern(node, self) {
                let target = get_target_of_binding_or_assignment_element(element, self);
                if let Some(target) =
                    target.filter(|target| is_assignment_pattern(&target.ref_(self)))
                {
                    if target
                        .ref_(self)
                        .transform_flags()
                        .intersects(TransformFlags::ContainsObjectRestOrSpread)
                    {
                        return TransformFlags::ContainsObjectRestOrSpread;
                    }
                    if target
                        .ref_(self)
                        .transform_flags()
                        .intersects(TransformFlags::ContainsES2018)
                    {
                        let flags = self.propagate_assignment_pattern_flags(target);
                        if flags != TransformFlags::None {
                            return flags;
                        }
                    }
                }
            }
        }
        TransformFlags::None
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_conditional_expression_raw(
        &self,
        condition: Id<Node /*Expression*/>,
        question_token: Option<Id<Node /*QuestionToken*/>>,
        when_true: Id<Node /*Expression*/>,
        colon_token: Option<Id<Node /*ColonToken*/>>,
        when_false: Id<Node /*Expression*/>,
    ) -> ConditionalExpression {
        let node = self.create_base_expression(SyntaxKind::ConditionalExpression);
        let node = ConditionalExpression::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_condition_of_conditional_expression(condition),
            question_token.unwrap_or_else(|| self.create_token(SyntaxKind::QuestionToken)),
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_branch_of_conditional_expression(when_true),
            colon_token.unwrap_or_else(|| self.create_token(SyntaxKind::ColonToken)),
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_branch_of_conditional_expression(when_false),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.condition), self)
                | propagate_child_flags(Some(node.question_token), self)
                | propagate_child_flags(Some(node.when_true), self)
                | propagate_child_flags(Some(node.colon_token), self)
                | propagate_child_flags(Some(node.when_false), self),
        );
        node
    }

    pub fn update_conditional_expression(
        &self,
        node: Id<Node>, /*ConditionalExpression*/
        condition: Id<Node /*Expression*/>,
        question_token: Id<Node /*QuestionToken*/>,
        when_true: Id<Node /*Expression*/>,
        colon_token: Id<Node /*ColonToken*/>,
        when_false: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_conditional_expression = node_ref.as_conditional_expression();
        if node_as_conditional_expression.condition != condition
            || node_as_conditional_expression.question_token != question_token
            || node_as_conditional_expression.when_true != when_true
            || node_as_conditional_expression.colon_token != colon_token
            || node_as_conditional_expression.when_false != when_false
        {
            self.update(
                self.create_conditional_expression(
                    condition,
                    Some(question_token),
                    when_true,
                    Some(colon_token),
                    when_false,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_template_expression_raw(
        &self,
        head: Id<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateSpan>*/
    ) -> TemplateExpression {
        let node = self.create_base_expression(SyntaxKind::TemplateExpression);
        let node = TemplateExpression::new(
            node,
            head,
            self.create_node_array(Some(template_spans), None),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.head), self)
                | propagate_children_flags(Some(&node.template_spans.ref_(self)))
                | TransformFlags::ContainsES2015,
        );
        node
    }

    pub fn update_template_expression(
        &self,
        node: Id<Node>, /*TemplateExpression*/
        head: Id<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateSpan>*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_template_expression = node_ref.as_template_expression();
        let template_spans = template_spans.into();
        if node_as_template_expression.head != head
            || has_node_array_changed(node_as_template_expression.template_spans, &template_spans)
        {
            self.update(self.create_template_expression(head, template_spans), node)
        } else {
            node
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
        self.create_template_literal_like_node_raw(
            kind,
            text.unwrap(),
            raw_text,
            Some(template_flags),
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_template_literal_like_node_raw(
        &self,
        kind: SyntaxKind,
        text: String,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        let template_flags = template_flags.unwrap_or(TokenFlags::None);
        let node = self.create_base_token(kind);
        let node = BaseLiteralLikeNode::new(node, text);
        let node = TemplateLiteralLikeNode::new(
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

    #[generate_node_factory_method_wrapper]
    pub fn create_template_head_raw(
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

    #[generate_node_factory_method_wrapper]
    pub fn create_template_middle_raw(
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

    #[generate_node_factory_method_wrapper]
    pub fn create_template_tail_raw(
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

    #[generate_node_factory_method_wrapper]
    pub fn create_no_substitution_template_literal_raw(
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

    #[generate_node_factory_method_wrapper]
    pub fn create_yield_expression_raw(
        &self,
        asterisk_token: Option<Id<Node /*AsteriskToken*/>>,
        expression: Option<Id<Node /*Expression*/>>,
    ) -> YieldExpression {
        let node = self.create_base_expression(SyntaxKind::YieldExpression);
        let node = YieldExpression::new(
            node,
            expression.map(|expression| {
                self.parenthesizer_rules()
                    .ref_(self)
                    .parenthesize_expression_for_disallowed_comma(expression)
            }),
            asterisk_token,
        );
        node.add_transform_flags(
            propagate_child_flags(node.expression.clone(), self)
                | propagate_child_flags(node.asterisk_token.clone(), self)
                | TransformFlags::ContainsES2015
                | TransformFlags::ContainsES2018
                | TransformFlags::ContainsYield,
        );
        node
    }

    pub fn update_yield_expression(
        &self,
        node: Id<Node>, /*Id<Node>*/
        asterisk_token: Option<Id<Node /*AsteriskToken*/>>,
        expression: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_yield_expression = node_ref.as_yield_expression();
        if node_as_yield_expression.expression != expression
            || node_as_yield_expression.asterisk_token != asterisk_token
        {
            self.update(
                self.create_yield_expression(asterisk_token, expression),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_spread_element_raw(&self, expression: Id<Node /*Expression*/>) -> SpreadElement {
        let node = self.create_base_expression(SyntaxKind::SpreadElement);
        let node = SpreadElement::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_expression_for_disallowed_comma(expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | TransformFlags::ContainsES2015
                | TransformFlags::ContainsRestOrSpread,
        );
        node
    }

    pub fn update_spread_element(
        &self,
        node: Id<Node>, /*SpreadElement*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_spread_element = node_ref.as_spread_element();
        if node_as_spread_element.expression != expression {
            self.update(self.create_spread_element(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_class_expression_raw<'name>(
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
        let node = ClassExpression::new(node);
        node.add_transform_flags(TransformFlags::ContainsES2015);
        node
    }

    pub fn update_class_expression(
        &self,
        node: Id<Node>, /*ClassExpression*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<Id<Node>>,
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
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_class_expression = node_ref.as_class_expression();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let heritage_clauses = heritage_clauses.map(Into::into);
        let members = members.into();
        if has_option_node_array_changed(node.ref_(self).maybe_decorators(), decorators.as_ref())
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers(), modifiers.as_ref())
            || node_as_class_expression.maybe_name() != name
            || has_option_node_array_changed(
                node_as_class_expression.maybe_type_parameters(),
                type_parameters.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_class_expression.maybe_heritage_clauses(),
                heritage_clauses.as_ref(),
            )
            || has_node_array_changed(node_as_class_expression.members(), &members)
        {
            self.update(
                self.create_class_expression(
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    heritage_clauses,
                    members,
                ),
                node,
            )
        } else {
            node
        }
    }
}
