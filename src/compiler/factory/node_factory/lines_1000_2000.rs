#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{propagate_child_flags, propagate_identifier_name_flags};
use crate::{
    ArrayTypeNode, BaseNode, BaseNodeFactory, IntersectionTypeNode, ModifierFlags, Node, NodeArray,
    NodeArrayOrVec, NodeFactory, NodeInterface, ParameterDeclaration, PropertySignature,
    QualifiedName, StringOrRcNode, SyntaxKind, TypeLiteralNode, TypeNode, TypeParameterDeclaration,
    TypePredicateNode, TypeReferenceNode, UnionTypeNode,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_super(&self, base_factory: &TBaseNodeFactory) -> BaseNode {
        self.create_token(base_factory, SyntaxKind::SuperKeyword)
    }

    pub fn create_this(&self, base_factory: &TBaseNodeFactory) -> BaseNode {
        self.create_token(base_factory, SyntaxKind::ThisKeyword)
    }

    pub fn create_null(&self, base_factory: &TBaseNodeFactory) -> BaseNode {
        self.create_token(base_factory, SyntaxKind::NullKeyword)
    }

    pub fn create_true(&self, base_factory: &TBaseNodeFactory) -> BaseNode {
        self.create_token(base_factory, SyntaxKind::TrueKeyword)
    }

    pub fn create_false(&self, base_factory: &TBaseNodeFactory) -> BaseNode {
        self.create_token(base_factory, SyntaxKind::FalseKeyword)
    }

    pub fn create_modifier(&self, base_factory: &TBaseNodeFactory, kind: SyntaxKind) -> BaseNode {
        self.create_token(base_factory, kind)
    }

    pub fn create_modifiers_from_modifier_flags(
        &self,
        base_factory: &TBaseNodeFactory,
        flags: ModifierFlags,
    ) -> Vec<Rc<Node /*Modifier*/>> {
        let mut result = vec![];
        if flags.intersects(ModifierFlags::Export) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::ExportKeyword)
                    .into(),
            );
        }
        if flags.intersects(ModifierFlags::Ambient) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::DeclareKeyword)
                    .into(),
            );
        }
        if flags.intersects(ModifierFlags::Default) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::DefaultKeyword)
                    .into(),
            );
        }
        if flags.intersects(ModifierFlags::Const) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::ConstKeyword)
                    .into(),
            );
        }
        if flags.intersects(ModifierFlags::Public) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::PublicKeyword)
                    .into(),
            );
        }
        if flags.intersects(ModifierFlags::Private) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::PrivateKeyword)
                    .into(),
            );
        }
        if flags.intersects(ModifierFlags::Protected) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::ProtectedKeyword)
                    .into(),
            );
        }
        if flags.intersects(ModifierFlags::Abstract) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::AbstractKeyword)
                    .into(),
            );
        }
        if flags.intersects(ModifierFlags::Static) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::StaticKeyword)
                    .into(),
            );
        }
        if flags.intersects(ModifierFlags::Override) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::OverrideKeyword)
                    .into(),
            );
        }
        if flags.intersects(ModifierFlags::Readonly) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::ReadonlyKeyword)
                    .into(),
            );
        }
        if flags.intersects(ModifierFlags::Async) {
            result.push(
                self.create_modifier(base_factory, SyntaxKind::AsyncKeyword)
                    .into(),
            );
        }
        result
    }

    pub fn create_qualified_name<TRight: Into<StringOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*EntityName*/>,
        right: TRight,
    ) -> QualifiedName {
        let node = self.create_base_node(base_factory, SyntaxKind::QualifiedName);
        let mut node =
            QualifiedName::new(node, left, self.as_name(base_factory, Some(right)).unwrap());
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.left)) | propagate_identifier_name_flags(&node.right),
        );
        node
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
            self.as_name(base_factory, Some(type_name)).unwrap(),
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
