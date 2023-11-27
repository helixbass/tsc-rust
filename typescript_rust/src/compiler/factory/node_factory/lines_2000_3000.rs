use gc::{Finalize, Gc, Trace};
use id_arena::Id;
use local_macros::generate_node_factory_method_wrapper;

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
    is_super_keyword, is_super_property, last_or_undefined, modifiers_to_flags, AllArenas,
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
    #[generate_node_factory_method_wrapper]
    pub fn create_infer_type_node_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        type_parameter: Gc<Node /*TypeParameterDeclaration*/>,
    ) -> InferTypeNode {
        let node = self.create_base_node(id, SyntaxKind::InferType);
        let mut node = InferTypeNode::new(node, type_parameter);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_infer_type_node(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*InferTypeNode*/
        type_parameter: Id<Node /*TypeParameterDeclaration*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_infer_type_node = node.as_infer_type_node();
            node_as_infer_type_node.type_parameter != type_parameter
        } {
            self.update(
                arena,
                self.create_infer_type_node(arena, type_parameter),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_template_literal_type_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        head: Id<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateLiteralTypeSpan>*/
    ) -> TemplateLiteralTypeNode {
        let node = self.create_base_node(id, SyntaxKind::TemplateLiteralType);
        let mut node = TemplateLiteralTypeNode::new(
            node,
            head,
            self.create_node_array(arena, Some(template_spans), None),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_template_literal_type(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*TemplateLiteralTypeSpan*/
        head: Id<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateLiteralTypeSpan>*/
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_template_literal_type_node = node.as_template_literal_type_node();
            let template_spans = template_spans.into();
            node_as_template_literal_type_node.head != head
                || has_node_array_changed(
                    &node_as_template_literal_type_node.template_spans,
                    &template_spans,
                )
        } {
            self.update(
                arena,
                self.create_template_literal_type(arena, head, template_spans),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_import_type_node_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        argument: Id<Node /*TypeNode*/>,
        qualifier: Option<Id<Node /*EntityName*/>>,
        type_arguments: Option<impl Into<NodeArrayOrVec> /*<TypeNode>*/>,
        is_type_of: Option<bool>,
    ) -> ImportTypeNode {
        let is_type_of = is_type_of.unwrap_or(false);
        let node = self.create_base_node(id, SyntaxKind::ImportType);
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
        arena: &AllArenas,
        node: Id<Node>, /*ImportTypeNode*/
        argument: Id<Node /*TypeNode*/>,
        qualifier: Option<Id<Node /*EntityName*/>>,
        type_arguments: Option<impl Into<NodeArrayOrVec /*<TypeNode>*/>>,
        is_type_of: Option<bool>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_import_type_node = node.as_import_type_node();
            let is_type_of = is_type_of.unwrap_or_else(|| node_as_import_type_node.is_type_of());
            let type_arguments = type_arguments.map(Into::into);
            node_as_import_type_node.argument != argument
                || node_as_import_type_node.qualifier != qualifier
                || has_option_node_array_changed(
                    node_as_import_type_node.maybe_type_arguments().as_deref(),
                    type_arguments.as_ref(),
                )
                || node_as_import_type_node.is_type_of() != is_type_of
        } {
            self.update(
                arena,
                self.create_import_type_node(
                    arena,
                    argument,
                    qualifier,
                    type_arguments,
                    Some(is_type_of),
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_parenthesized_type_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        type_: Id<Node /*TypeNode*/>,
    ) -> ParenthesizedTypeNode {
        let node = self.create_base_node(id, SyntaxKind::ParenthesizedType);
        let mut node = ParenthesizedTypeNode::new(node, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_parenthesized_type(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*ParenthesizedTypeNode*/
        type_: Id<Node /*TypeNode*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_parenthesized_type_node = node.as_parenthesized_type_node();
            node_as_parenthesized_type_node.type_ != type_
        } {
            self.update(arena, self.create_parenthesized_type(arena, type_), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_this_type_node_raw(&self, arena: &AllArenas, id: Id<Node>) -> ThisTypeNode {
        let node = self.create_base_node(id, SyntaxKind::ThisType);
        let mut node = ThisTypeNode::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_operator_node_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        operator: SyntaxKind,
        type_: Id<Node /*TypeNode*/>,
    ) -> TypeOperatorNode {
        let node = self.create_base_node(id, SyntaxKind::TypeOperator);
        let node = TypeOperatorNode::new(
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
        arena: &AllArenas,
        node: Id<Node>, /*TypeOperatorNode*/
        type_: Id<Node /*TypeNode*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_type_operator_node = node.as_type_operator_node();
            node_as_type_operator_node.type_ != type_
        } {
            self.update(
                arena,
                self.create_type_operator_node(arena, node_as_type_operator_node.operator, type_),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_indexed_access_type_node_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        object_type: Id<Node /*TypeNode*/>,
        index_type: Id<Node /*TypeNode*/>,
    ) -> IndexedAccessTypeNode {
        let node = self.create_base_node(id, SyntaxKind::IndexedAccessType);
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
        arena: &AllArenas,
        node: Id<Node>, /*IndexedAccessTypeNode*/
        object_type: Id<Node /*TypeNode*/>,
        index_type: Id<Node /*TypeNode*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_indexed_access_type_node = node.as_indexed_access_type_node();
            node_as_indexed_access_type_node.object_type != object_type
                || node_as_indexed_access_type_node.index_type != index_type
        } {
            self.update(
                arena,
                self.create_indexed_access_type_node(arena, object_type, index_type),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_mapped_type_node_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        readonly_token: Option<Id<Node /*ReadonlyKeyword | PlusToken | MinusToken*/>>,
        type_parameter: Id<Node /*TypeParameterDeclaration*/>,
        name_type: Option<Id<Node /*TypeNode*/>>,
        question_token: Option<Id<Node /*QuestionToken | PlusToken | MinusToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        members: Option<impl Into<NodeArrayOrVec> /*<TypeElement>*/>,
    ) -> MappedTypeNode {
        let node = self.create_base_node(id, SyntaxKind::MappedType);
        let mut node = MappedTypeNode::new(
            node,
            readonly_token,
            type_parameter,
            name_type,
            question_token,
            type_,
            members.map(|members| self.create_node_array(arena, Some(members), None)),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_mapped_type_node(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*MappedTypeNode*/
        readonly_token: Option<Id<Node /*ReadonlyKeyword | PlusToken | MinusToken*/>>,
        type_parameter: Id<Node /*TypeParameterDeclaration*/>,
        name_type: Option<Id<Node /*TypeNode*/>>,
        question_token: Option<Id<Node /*QuestionToken | PlusToken | MinusToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        members: Option<Gc<NodeArray /*<TypeElement>*/>>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_mapped_type_node = node.as_mapped_type_node();
            node_as_mapped_type_node.readonly_token != readonly_token
                || node_as_mapped_type_node.type_parameter != type_parameter
                || node_as_mapped_type_node.name_type != name_type
                || node_as_mapped_type_node.question_token != question_token
                || node_as_mapped_type_node.type_ != type_
                || node_as_mapped_type_node.members != members
        } {
            self.update(
                arena,
                self.create_mapped_type_node(
                    arena,
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
        arena: &AllArenas,
        id: Id<Node>,
        literal: Id<Node /*LiteralTypeNode["literal"]*/>,
    ) -> LiteralTypeNode {
        let node = self.create_base_node(id, SyntaxKind::LiteralType);
        let mut node = LiteralTypeNode::new(node, literal);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_literal_type_node(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*LiteralTypeNode*/
        literal: Id<Node /*LiteralTypeNode["literal"]*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_literal_type_node = node.as_literal_type_node();
            node_as_literal_type_node.literal != literal
        } {
            self.update(arena, self.create_literal_type_node(arena, literal), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_object_binding_pattern_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
    ) -> ObjectBindingPattern {
        let node = self.create_base_node(id, SyntaxKind::ObjectBindingPattern);
        let mut node =
            ObjectBindingPattern::new(node, self.create_node_array(arena, Some(elements), None));
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
        arena: &AllArenas,
        node: Id<Node>, /*ObjectBindingPattern*/
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_object_binding_pattern = node.as_object_binding_pattern();
            let elements = elements.into();
            has_node_array_changed(&node_as_object_binding_pattern.elements, &elements)
        } {
            self.update(
                arena,
                self.create_object_binding_pattern(arena, elements),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_array_binding_pattern_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        elements: impl Into<NodeArrayOrVec /*<BindingElement>*/>,
    ) -> ArrayBindingPattern {
        let node = self.create_base_node(id, SyntaxKind::ArrayBindingPattern);
        let mut node =
            ArrayBindingPattern::new(node, self.create_node_array(arena, Some(elements), None));
        node.add_transform_flags(
            propagate_children_flags(Some(&node.elements))
                | TransformFlags::ContainsES2015
                | TransformFlags::ContainsBindingPattern,
        );
        node
    }

    pub fn update_array_binding_pattern(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*ArrayBindingPattern*/
        elements: impl Into<NodeArrayOrVec /*<ArrayBindingElement>*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_array_binding_pattern = node.as_array_binding_pattern();
            let elements = elements.into();
            has_node_array_changed(&node_as_array_binding_pattern.elements, &elements)
        } {
            self.update(
                arena,
                self.create_array_binding_pattern(arena, elements),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_binding_element_raw<'property_name, 'name>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        dot_dot_dot_token: Option<Id<Node /*DotDotDotToken*/>>,
        property_name: Option<impl Into<StrOrRcNode<'property_name> /*PropertyName*/>>,
        name: impl Into<StrOrRcNode<'name>>, /*BindingName*/
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> BindingElement {
        let node = self.create_base_binding_like_declaration(
            arena,
            id,
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
            propagate_child_flags(arena, node.dot_dot_dot_token.clone())
                | TransformFlags::ContainsES2015,
        );
        if let Some(node_property_name) = node.property_name.as_ref() {
            node.add_transform_flags(if is_identifier(node_property_name) {
                propagate_identifier_name_flags(arena, &node_property_name)
            } else {
                propagate_child_flags(arena, Some(&**node_property_name))
            });
        }
        if dot_dot_dot_token_is_some {
            node.add_transform_flags(TransformFlags::ContainsRestOrSpread);
        }
        node
    }

    pub fn update_binding_element(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*BindingElement*/
        dot_dot_dot_token: Option<Id<Node /*DotDotDotToken*/>>,
        property_name: Option<Id<Node /*PropertyName*/>>,
        name: Id<Node /*BindingName*/>,
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_binding_element = node.as_binding_element();
            node_as_binding_element.property_name != property_name
                || node_as_binding_element.dot_dot_dot_token != dot_dot_dot_token
                || node_as_binding_element.name() != name
                || node_as_binding_element.maybe_initializer() != initializer
        } {
            self.update(
                arena,
                self.create_binding_element(
                    arena,
                    dot_dot_dot_token,
                    property_name,
                    name,
                    initializer,
                ),
                node,
            )
        } else {
            node
        }
    }

    pub(crate) fn create_base_expression(&self, id: Id<Node>, kind: SyntaxKind) -> BaseNode {
        let node = self.create_base_node(id, kind);
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_array_literal_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        elements: Option<impl Into<NodeArrayOrVec>>, /*Expression*/
        multi_line: Option<bool>,
    ) -> ArrayLiteralExpression {
        let node = self.create_base_expression(id, SyntaxKind::ArrayLiteralExpression);
        let elements = elements.map(Into::into);
        let elements_vec = elements.clone().map(|elements| match elements {
            NodeArrayOrVec::NodeArray(node_array) => node_array.to_vec(),
            NodeArrayOrVec::Vec(elements) => elements,
        });
        let last_element = elements_vec
            .as_ref()
            .and_then(|elements_vec| last_or_undefined(elements_vec));
        let elements_array = self.create_node_array(
            arena,
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
        arena: &AllArenas,
        node: Id<Node>,                      /*ArrayLiteralExpression*/
        elements: impl Into<NodeArrayOrVec>, /*Expression*/
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_array_literal_expression = node.as_array_literal_expression();
            let elements = elements.into();
            has_node_array_changed(&node_as_array_literal_expression.elements, &elements)
        } {
            self.update(
                arena,
                self.create_array_literal_expression(
                    arena,
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
        arena: &AllArenas,
        id: Id<Node>,
        properties: Option<impl Into<NodeArrayOrVec> /*ObjectLiteralElementLike*/>,
        multi_line: Option<bool>,
    ) -> ObjectLiteralExpression {
        let node = self.create_base_expression(id, SyntaxKind::ObjectLiteralExpression);
        let mut node = ObjectLiteralExpression::new(
            node,
            self.create_node_array(arena, properties, None),
            multi_line,
        );
        node.add_transform_flags(propagate_children_flags(Some(&node.properties)));
        node
    }

    pub fn update_object_literal_expression(
        &self,
        arena: &AllArenas,
        node: Id<Node>,                        /*ObjectLiteralExpression*/
        properties: impl Into<NodeArrayOrVec>, /*ObjectLiteralElementLike*/
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_object_literal_expression = node.as_object_literal_expression();
            let properties = properties.into();
            has_node_array_changed(&node_as_object_literal_expression.properties, &properties)
        } {
            self.update(
                arena,
                self.create_object_literal_expression(
                    arena,
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
        arena: &AllArenas,
        id: Id<Node>,
        expression: Id<Node /*Expression*/>,
        name: impl Into<StrOrRcNode<'name>>,
    ) -> PropertyAccessExpression {
        let node = self.create_base_expression(id, SyntaxKind::PropertyAccessExpression);
        let mut node = PropertyAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
            None,
            self.as_name(Some(name)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(arena, Some(&*node.expression))
                | if is_identifier(&node.name) {
                    propagate_identifier_name_flags(arena, &node.name)
                } else {
                    propagate_child_flags(arena, Some(&*node.name))
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
        arena: &AllArenas,
        node: Id<Node>, /*PropertyAccessExpression*/
        expression: Id<Node /*Expression*/>,
        name: Id<Node /*Identifier | PrivateIdentifier*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_property_access_expression = node.as_property_access_expression();
            is_property_access_chain(node)
        } {
            return self.update_property_access_chain(
                arena,
                node,
                expression,
                arena
                    .node(node)
                    .as_property_access_expression()
                    .question_dot_token,
                name,
            );
        }
        if {
            let node = arena.node(node);
            let node_as_property_access_expression = node.as_property_access_expression();
            node_as_property_access_expression.expression != expression
                || node_as_property_access_expression.name != name
        } {
            self.update(
                arena,
                self.create_property_access_expression(arena, expression, name),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_property_access_chain_raw<'name>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        expression: Id<Node /*Expression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
        name: impl Into<StrOrRcNode<'name>>,
    ) -> PropertyAccessExpression {
        let mut node = self.create_base_expression(id, SyntaxKind::PropertyAccessExpression);
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
                | propagate_child_flags(arena, Some(&*node.expression))
                | propagate_child_flags(arena, node.question_dot_token.clone())
                | if is_identifier(&node.name) {
                    propagate_identifier_name_flags(arena, &node.name)
                } else {
                    propagate_child_flags(arena, Some(&*node.name))
                },
        );
        node
    }

    pub fn update_property_access_chain(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*PropertyAccessChain*/
        expression: Id<Node /*Expression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
        name: Id<Node /*Identifier | PrivateIdentifier*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_property_access_expression = node.as_property_access_expression();
            Debug_.assert(
                node.flags().intersects(NodeFlags::OptionalChain),
                Some("Cannot update a PropertyAccessExpression using updatePropertyAccessChain. Use updatePropertyAccess instead.")
            );
            node_as_property_access_expression.expression != expression
                || node_as_property_access_expression.question_dot_token != question_dot_token
                || node_as_property_access_expression.expression != expression
        } {
            self.update(
                arena,
                self.create_property_access_chain(arena, expression, question_dot_token, name),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_element_access_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        expression: Id<Node /*Expression*/>,
        index: impl Into<StringOrNumberOrBoolOrRcNode>,
    ) -> ElementAccessExpression {
        let node = self.create_base_expression(id, SyntaxKind::ElementAccessExpression);
        let mut node = ElementAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
            None,
            self.as_expression(index),
        );
        node.add_transform_flags(
            propagate_child_flags(arena, Some(&*node.expression))
                | propagate_child_flags(arena, Some(&*node.argument_expression)),
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
        arena: &AllArenas,
        node: Id<Node>, /*ElementAccessExpression*/
        expression: Id<Node /*Expression*/>,
        argument_expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_element_access_expression = node.as_element_access_expression();
            is_element_access_chain(node)
        } {
            return self.update_element_access_chain(
                arena,
                node,
                expression,
                arena
                    .node(node)
                    .as_element_access_expression()
                    .question_dot_token,
                argument_expression,
            );
        }
        if {
            let node = arena.node(node);
            let node_as_element_access_expression = node.as_element_access_expression();
            node_as_element_access_expression.expression != expression
                || node_as_element_access_expression.argument_expression != argument_expression
        } {
            self.update(
                arena,
                self.create_element_access_expression(arena, expression, argument_expression),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_element_access_chain_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        expression: Id<Node /*Expression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
        index: impl Into<StringOrNumberOrBoolOrRcNode>,
    ) -> ElementAccessExpression {
        let mut node = self.create_base_expression(id, SyntaxKind::ElementAccessExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let mut node = ElementAccessExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
            question_dot_token,
            self.as_expression(index),
        );
        node.add_transform_flags(
            propagate_child_flags(arena, Some(&*node.expression))
                | propagate_child_flags(arena, node.question_dot_token.clone())
                | propagate_child_flags(arena, Some(&*node.argument_expression))
                | TransformFlags::ContainsES2020,
        );
        node
    }

    pub fn update_element_access_chain(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*ElementAccessChain*/
        expression: Id<Node /*Expression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
        argument_expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_element_access_expression = node.as_element_access_expression();
            Debug_.assert(
                node.flags().intersects(NodeFlags::OptionalChain),
                Some("Cannot update a ElementAccessExpression using updateElementAccessChain. Use updateElementAccess instead.")
            );
            node_as_element_access_expression.expression != expression
                || node_as_element_access_expression.question_dot_token != question_dot_token
                || node_as_element_access_expression.argument_expression != argument_expression
        } {
            self.update(
                arena,
                self.create_element_access_chain(
                    arena,
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
        arena: &AllArenas,
        id: Id<Node>,
        expression: Id<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec /*<TypeNode>*/>>,
        arguments_array: Option<impl Into<NodeArrayOrVec /*<Expression>*/>>,
    ) -> CallExpression {
        let node = self.create_base_expression(id, SyntaxKind::CallExpression);
        let mut node = CallExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
            None,
            self.as_node_array(type_arguments),
            self.parenthesizer_rules()
                .parenthesize_expressions_of_comma_delimited_list(
                    self.create_node_array(arena, arguments_array, None).into(),
                ),
        );
        node.add_transform_flags(
            propagate_child_flags(arena, Some(&*node.expression))
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
        arena: &AllArenas,
        node: Id<Node>, /*CallExpression*/
        expression: Id<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec /*<TypeNode>*/>>,
        arguments_array: impl Into<NodeArrayOrVec /*<Expression>*/>,
    ) -> Id<Node /*CallExpression*/> {
        if {
            let node = arena.node(node);
            is_call_chain(node)
        } {
            return self.update_call_chain(
                arena,
                node,
                expression,
                arena.node(node).as_call_expression().question_dot_token,
                type_arguments,
                arguments_array,
            );
        }
        if {
            let node = arena.node(node);
            let node_as_call_expression = node.as_call_expression();
            let type_arguments = type_arguments.map(Into::into);
            let arguments_array = arguments_array.into();
            node_as_call_expression.expression != expression
                || has_option_node_array_changed(
                    node_as_call_expression.maybe_type_arguments().as_deref(),
                    type_arguments.as_ref(),
                )
                || has_node_array_changed(&node_as_call_expression.arguments, &arguments_array)
        } {
            self.update(
                arena,
                self.create_call_expression(
                    arena,
                    expression,
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
    pub fn create_call_chain_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
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
        let mut node = self.create_base_expression(id, SyntaxKind::CallExpression);
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
            propagate_child_flags(arena, Some(&*node.expression))
                | propagate_child_flags(arena, node.question_dot_token.clone())
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
        arena: &AllArenas,
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
        if {
            let node = arena.node(node);
            let node_as_call_expression = node.as_call_expression();
            let type_arguments = type_arguments.map(Into::into);
            let arguments_array = arguments_array.into();
            Debug_.assert(
                node.flags().intersects(NodeFlags::OptionalChain),
                Some(
                    "Cannot update a CallExpression using updateCallChain. Use updateCall instead.",
                ),
            );
            node_as_call_expression.expression != expression
                || node_as_call_expression.question_dot_token != question_dot_token
                || has_option_node_array_changed(
                    node_as_call_expression.maybe_type_arguments().as_deref(),
                    type_arguments.as_ref(),
                )
                || has_node_array_changed(&node_as_call_expression.arguments, &arguments_array)
        } {
            self.update(
                arena,
                self.create_call_chain(
                    arena,
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
        arena: &AllArenas,
        id: Id<Node>,
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
        let node = self.create_base_expression(id, SyntaxKind::NewExpression);
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
            propagate_child_flags(arena, Some(&*node.expression))
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
        arena: &AllArenas,
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
        if {
            let node = arena.node(node);
            let node_as_new_expression = node.as_new_expression();
            let type_arguments = type_arguments.map(Into::into);
            let arguments_array = arguments_array.map(Into::into);
            node_as_new_expression.expression != expression
                || has_option_node_array_changed(
                    node_as_new_expression.maybe_type_arguments().as_deref(),
                    type_arguments.as_ref(),
                )
                || has_option_node_array_changed(
                    node_as_new_expression.arguments.as_deref(),
                    arguments_array.as_ref(),
                )
        } {
            self.update(
                arena,
                self.create_new_expression(arena, expression, type_arguments, arguments_array),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_tagged_template_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        tag: Id<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec> /*<TypeNode>*/>,
        template: Id<Node /*TemplateLiteral*/>,
    ) -> TaggedTemplateExpression {
        let node = self.create_base_expression(id, SyntaxKind::TaggedTemplateExpression);
        let mut node = TaggedTemplateExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&tag),
            self.as_node_array(type_arguments),
            template,
            None,
        );
        node.add_transform_flags(
            propagate_child_flags(arena, Some(&*node.tag))
                | propagate_children_flags(node.maybe_type_arguments().as_deref())
                | propagate_child_flags(arena, Some(&*node.template))
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
        arena: &AllArenas,
        node: Id<Node>, /*TaggedTemplateExpression*/
        tag: Id<Node /*Expression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec> /*<TypeNode>*/>,
        template: Id<Node /*TemplateLiteral*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_tagged_template_expression = node.as_tagged_template_expression();
            let type_arguments = type_arguments.map(Into::into);
            node_as_tagged_template_expression.tag != tag
                || has_option_node_array_changed(
                    node_as_tagged_template_expression
                        .maybe_type_arguments()
                        .as_deref(),
                    type_arguments.as_ref(),
                )
                || node_as_tagged_template_expression.template != template
        } {
            self.update(
                arena,
                self.create_tagged_template_expression(arena, tag, type_arguments, template),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_assertion_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        type_: Id<Node /*TypeNode*/>,
        expression: Id<Node /*Expression*/>,
    ) -> TypeAssertion {
        let node = self.create_base_expression(id, SyntaxKind::TypeAssertionExpression);
        let mut node = TypeAssertion::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_operand_of_prefix_unary(&expression),
            type_,
        );
        node.add_transform_flags(
            propagate_child_flags(arena, Some(&*node.expression))
                | propagate_child_flags(arena, Some(&*node.type_))
                | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_type_assertion(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*TypeAssertion*/
        type_: Id<Node /*TypeNode*/>,
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_type_assertion = node.as_type_assertion();
            node_as_type_assertion.type_ != type_ || node_as_type_assertion.expression != expression
        } {
            self.update(
                arena,
                self.create_type_assertion(arena, type_, expression),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_parenthesized_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        expression: Id<Node /*Expression*/>,
    ) -> ParenthesizedExpression {
        let node = self.create_base_expression(id, SyntaxKind::ParenthesizedExpression);
        let mut node = ParenthesizedExpression::new(node, expression);
        node.add_transform_flags(propagate_child_flags(arena, Some(&*node.expression)));
        node
    }

    pub fn update_parenthesized_expression(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*ParenthesizedExpression*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_parenthesized_expression = node.as_parenthesized_expression();
            node_as_parenthesized_expression.expression != expression
        } {
            self.update(
                arena,
                self.create_parenthesized_expression(arena, expression),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_function_expression_raw<'name>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Id<Node>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: Option<impl Into<NodeArrayOrVec>>,
        type_: Option<Id<Node>>,
        body: Id<Node>,
    ) -> FunctionExpression {
        let mut node = self.create_base_function_like_declaration(
            arena,
            id,
            SyntaxKind::FunctionExpression,
            Option::<Gc<NodeArray>>::None,
            modifiers,
            name,
            type_parameters,
            parameters,
            type_,
            Some(body),
        );
        node.asterisk_token = asterisk_token;
        let mut node = FunctionExpression::new(node);
        node.add_transform_flags(propagate_child_flags(arena, node.maybe_asterisk_token()));
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
                    Some(parameters),
                    type_,
                    body,
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_arrow_function_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node>>,
        equals_greater_than_token: Option<Gc<Node /*EqualsGreaterThanToken*/>>,
        body: Gc<Node /*ConciseBody*/>,
    ) -> ArrowFunction {
        let node = self.create_base_function_like_declaration(
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
        let node = ArrowFunction::new(
            node,
            equals_greater_than_token
                .unwrap_or_else(|| self.create_token(SyntaxKind::EqualsGreaterThanToken)),
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
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_delete_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        expression: Gc<Node /*Expression*/>,
    ) -> DeleteExpression {
        let node = self.create_base_expression(SyntaxKind::DeleteExpression);
        let node = DeleteExpression::new(
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
            self.update(self.create_delete_expression(expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_of_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        expression: Gc<Node /*Expression*/>,
    ) -> TypeOfExpression {
        let node = self.create_base_expression(SyntaxKind::TypeOfExpression);
        let node = TypeOfExpression::new(
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
            self.update(self.create_type_of_expression(expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_void_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        expression: Gc<Node /*Expression*/>,
    ) -> VoidExpression {
        let node = self.create_base_expression(SyntaxKind::VoidExpression);
        let node = VoidExpression::new(
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
            self.update(self.create_void_expression(expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_await_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        expression: Gc<Node /*Expression*/>,
    ) -> AwaitExpression {
        let node = self.create_base_expression(SyntaxKind::AwaitExpression);
        let node = AwaitExpression::new(
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
            self.update(self.create_await_expression(expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_prefix_unary_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        operator: SyntaxKind, /*PrefixUnaryOperator*/
        operand: Gc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        let node = self.create_base_expression(id, SyntaxKind::PrefixUnaryExpression);
        let node = PrefixUnaryExpression::new(
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
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_postfix_unary_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        operand: Gc<Node /*Expression*/>,
        operator: SyntaxKind,
    ) -> PostfixUnaryExpression {
        let node = self.create_base_expression(SyntaxKind::PostfixUnaryExpression);
        let node = PostfixUnaryExpression::new(
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
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_binary_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        operator: impl Into<SyntaxKindOrRcNode>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        let node = self.create_base_expression(SyntaxKind::BinaryExpression);
        let operator_token = self.as_token(operator.into());
        let operator_kind = operator_token.kind();
        let node = BinaryExpression::new(
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
            self.update(self.create_binary_expression(left, operator, right), node)
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

    #[generate_node_factory_method_wrapper]
    pub fn create_conditional_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        condition: Gc<Node /*Expression*/>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        when_true: Gc<Node /*Expression*/>,
        colon_token: Option<Gc<Node /*ColonToken*/>>,
        when_false: Gc<Node /*Expression*/>,
    ) -> ConditionalExpression {
        let node = self.create_base_expression(SyntaxKind::ConditionalExpression);
        let node = ConditionalExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_condition_of_conditional_expression(&condition),
            question_token.unwrap_or_else(|| self.create_token(SyntaxKind::QuestionToken)),
            self.parenthesizer_rules()
                .parenthesize_branch_of_conditional_expression(&when_true),
            colon_token.unwrap_or_else(|| self.create_token(SyntaxKind::ColonToken)),
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
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_template_expression_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        head: Gc<Node /*TemplateHead*/>,
        template_spans: impl Into<NodeArrayOrVec>, /*<TemplateSpan>*/
    ) -> TemplateExpression {
        let node = self.create_base_expression(SyntaxKind::TemplateExpression);
        let node = TemplateExpression::new(
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
            self.update(self.create_template_expression(head, template_spans), node)
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
        arena: &AllArenas,
        id: Id<Node>,
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
        arena: &AllArenas,
        id: Id<Node>,
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
        arena: &AllArenas,
        id: Id<Node>,
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
        arena: &AllArenas,
        id: Id<Node>,
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
        arena: &AllArenas,
        id: Id<Node>,
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
        arena: &AllArenas,
        id: Id<Node>,
        asterisk_token: Option<Gc<Node /*AsteriskToken*/>>,
        expression: Option<Gc<Node /*Expression*/>>,
    ) -> YieldExpression {
        let node = self.create_base_expression(SyntaxKind::YieldExpression);
        let node = YieldExpression::new(
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
                self.create_yield_expression(asterisk_token, expression),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_spread_element_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        expression: Gc<Node /*Expression*/>,
    ) -> SpreadElement {
        let node = self.create_base_expression(SyntaxKind::SpreadElement);
        let node = SpreadElement::new(
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
            self.update(self.create_spread_element(expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_class_expression_raw<'name>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
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
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }
}
