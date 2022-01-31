#![allow(non_upper_case_globals)]

use std::rc::Rc;

use crate::{
    ArrayTypeNode, BaseNode, BaseNodeFactory, IntersectionTypeNode, Node, NodeArray,
    NodeArrayOrVec, NodeFactory, ParameterDeclaration, PropertySignature, SyntaxKind,
    TypeLiteralNode, TypeNode, TypeParameterDeclaration, TypePredicateNode, TypeReferenceNode,
    UnionTypeNode,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_true(&self, base_factory: &TBaseNodeFactory) -> BaseNode {
        self.create_token(base_factory, SyntaxKind::TrueKeyword)
    }

    pub fn create_false(&self, base_factory: &TBaseNodeFactory) -> BaseNode {
        self.create_token(base_factory, SyntaxKind::FalseKeyword)
    }

    pub fn create_type_parameter_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node>,
        constraint: Option<Rc<Node /*TypeNode*/>>,
        default_type: Option<Rc<Node /*TypeNode*/>>,
    ) -> TypeParameterDeclaration {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::TypeParameter,
            None,
            None,
            Some(name),
        );
        let node = TypeParameterDeclaration::new(node, constraint, default_type);
        node
    }

    pub fn create_parameter_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        dot_dot_dot_token: Option<Rc<Node /*DotDotDotToken*/>>,
        name: Rc<Node>,
        question_token: Option<Rc<Node /*QuestionToken*/>>,
        type_: Option<Rc<Node /*TypeNode*/>>,
        initializer: Option<Rc<Node /*Expression*/>>,
    ) -> ParameterDeclaration {
        let node = self.create_base_variable_like_declaration(
            base_factory,
            SyntaxKind::Parameter,
            decorators,
            modifiers,
            Some(name),
            type_,
            initializer,
        );
        let node = ParameterDeclaration::new(node, dot_dot_dot_token, question_token);
        node
    }

    pub fn create_property_signature(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<NodeArray>,
        name: Rc<Node>,
        question_token: Option<Rc<Node>>,
        type_: Option<Rc<Node>>,
    ) -> PropertySignature {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertySignature,
            None,
            modifiers,
            Some(name),
        );
        let node = PropertySignature::new(node, question_token, type_);
        node
    }

    pub fn create_keyword_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        token: SyntaxKind,
    ) -> BaseNode {
        self.create_token(base_factory, token)
    }

    pub fn create_type_predicate_node(
        &self,
        base_factory: &TBaseNodeFactory,
        asserts_modifier: Option<Rc<Node>>,
        parameter_name: Rc<Node>,
        type_: Option<Rc<Node>>,
    ) -> TypePredicateNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypePredicate);
        let node = TypePredicateNode::new(node, asserts_modifier, parameter_name, type_);
        node
    }

    pub fn create_type_reference_node<TTypeArguments: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        type_name: Rc<Node>,
        type_arguments: Option<TTypeArguments>,
    ) -> TypeReferenceNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypeReference);
        let node = TypeReferenceNode::new(
            node,
            self.as_name(type_name),
            type_arguments.map(|type_arguments| self.create_node_array(Some(type_arguments), None)),
        );
        node
    }

    pub fn create_type_literal_node(
        &self,
        base_factory: &TBaseNodeFactory,
        members: Option<Vec<Rc<Node>>>,
    ) -> TypeLiteralNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypeLiteral);
        let node = TypeLiteralNode::new(node, self.create_node_array(members, None));
        node
    }

    pub fn create_array_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        element_type: Rc<Node>,
    ) -> ArrayTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::ArrayType);
        let node = ArrayTypeNode::new(node, element_type);
        node
    }

    pub fn create_union_or_intersection_type_node<TElements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind, /*SyntaxKind.UnionType | SyntaxKind.IntersectionType*/
        types: TElements, /*<TypeNode>*/
    ) -> TypeNode {
        let node = self.create_base_node(base_factory, kind);
        let types = self
            .parenthesizer_rules()
            .parenthesize_constituent_types_of_union_or_intersection_type(
                base_factory,
                types.into(),
            );
        match kind {
            SyntaxKind::UnionType => UnionTypeNode::new(node, types).into(),
            SyntaxKind::IntersectionType => IntersectionTypeNode::new(node, types).into(),
            _ => panic!("Expected UnionType or IntersectionType"),
        }
    }

    pub fn create_union_type_node<TElements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        types: TElements, /*<TypeNode>*/
    ) -> TypeNode {
        self.create_union_or_intersection_type_node(base_factory, SyntaxKind::UnionType, types)
    }

    pub fn create_intersection_type_node<TElements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        types: TElements, /*<TypeNode>*/
    ) -> TypeNode {
        self.create_union_or_intersection_type_node(
            base_factory,
            SyntaxKind::IntersectionType,
            types,
        )
    }
}
