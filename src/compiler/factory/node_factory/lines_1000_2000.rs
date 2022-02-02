#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{propagate_child_flags, propagate_identifier_name_flags};
use crate::{
    has_static_modifier, is_computed_property_name, is_exclamation_token, is_question_token,
    is_this_identifier, modifiers_to_flags, ArrayTypeNode, BaseNode, BaseNodeFactory,
    ClassStaticBlockDeclaration, ComputedPropertyName, ConstructorDeclaration, Decorator,
    FunctionLikeDeclarationInterface, GetAccessorDeclaration, HasInitializerInterface,
    IntersectionTypeNode, MethodDeclaration, MethodSignature, ModifierFlags,
    NamedDeclarationInterface, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeInterface,
    ParameterDeclaration, PropertyDeclaration, PropertySignature, QualifiedName, StringOrRcNode,
    SyntaxKind, TransformFlags, TypeLiteralNode, TypeNode, TypeParameterDeclaration,
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

    pub fn create_computed_property_name(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
    ) -> ComputedPropertyName {
        let node = self.create_base_node(base_factory, SyntaxKind::ComputedPropertyName);
        let mut node = ComputedPropertyName::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_of_computed_property_name(base_factory, &expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | TransformFlags::ContainsES2015
                | TransformFlags::ContainsComputedPropertyName,
        );
        node
    }

    pub fn create_type_parameter_declaration<TName: Into<StringOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: TName,
        constraint: Option<Rc<Node /*TypeNode*/>>,
        default_type: Option<Rc<Node /*TypeNode*/>>,
    ) -> TypeParameterDeclaration {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::TypeParameter,
            Option::<NodeArray>::None,
            Option::<NodeArray>::None,
            Some(name),
        );
        let mut node = TypeParameterDeclaration::new(node, constraint, default_type);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_parameter_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        dot_dot_dot_token: Option<Rc<Node /*DotDotDotToken*/>>,
        name: TName,
        question_token: Option<Rc<Node /*QuestionToken*/>>,
        type_: Option<Rc<Node /*TypeNode*/>>,
        initializer: Option<Rc<Node /*Expression*/>>,
    ) -> ParameterDeclaration {
        let initializer_is_some = initializer.is_some();
        let node = self.create_base_variable_like_declaration(
            base_factory,
            SyntaxKind::Parameter,
            decorators,
            modifiers,
            Some(name),
            type_,
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(base_factory, &initializer)
            }),
        );
        let question_token_is_some = question_token.is_some();
        let dot_dot_dot_token_is_some = dot_dot_dot_token.is_some();
        let mut node = ParameterDeclaration::new(node, dot_dot_dot_token, question_token);
        if is_this_identifier(Some(node.name())) {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        } else {
            node.add_transform_flags(
                propagate_child_flags(node.dot_dot_dot_token.clone())
                    | propagate_child_flags(node.question_token.clone()),
            );
            if question_token_is_some {
                node.add_transform_flags(TransformFlags::ContainsTypeScript);
            }
            if modifiers_to_flags(node.maybe_modifiers())
                .intersects(ModifierFlags::ParameterPropertyModifier)
            {
                node.add_transform_flags(TransformFlags::ContainsTypeScriptClassSyntax);
            }
            if initializer_is_some || dot_dot_dot_token_is_some {
                node.add_transform_flags(TransformFlags::ContainsES2015);
            }
        }
        node
    }

    pub fn create_decorator(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
    ) -> Decorator {
        let node = self.create_base_node(base_factory, SyntaxKind::Decorator);
        let mut node = Decorator::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | TransformFlags::ContainsTypeScript
                | TransformFlags::ContainsTypeScriptClassSyntax,
        );
        node
    }

    pub fn create_property_signature<
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<TModifiers>,
        name: TName,
        question_token: Option<Rc<Node>>,
        type_: Option<Rc<Node>>,
    ) -> PropertySignature {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertySignature,
            Option::<NodeArray>::None,
            modifiers,
            Some(name),
        );
        let mut node = PropertySignature::new(node, question_token, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_property_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: TName,
        question_or_exclamation_token: Option<Rc<Node /*QuestionToken | ExclamationToken*/>>,
        type_: Option<Rc<Node /*TypeNode*/>>,
        initializer: Option<Rc<Node /*Expression*/>>,
    ) -> PropertyDeclaration {
        let node = self.create_base_variable_like_declaration(
            base_factory,
            SyntaxKind::PropertyDeclaration,
            decorators,
            modifiers,
            Some(name),
            type_,
            initializer,
        );
        let question_or_exclamation_token_is_some = question_or_exclamation_token.is_some();
        let mut node = PropertyDeclaration::new(
            node,
            question_or_exclamation_token
                .clone()
                .filter(|question_or_exclamation_token| {
                    is_question_token(question_or_exclamation_token)
                }),
            question_or_exclamation_token.filter(|question_or_exclamation_token| {
                is_exclamation_token(question_or_exclamation_token)
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(node.question_token.clone())
                | propagate_child_flags(node.exclamation_token.clone())
                | TransformFlags::ContainsClassFields,
        );
        if is_computed_property_name(&node.name())
            || has_static_modifier(&node) && node.maybe_initializer().is_some()
        {
            node.add_transform_flags(TransformFlags::ContainsTypeScriptClassSyntax);
        }
        if question_or_exclamation_token_is_some
            || modifiers_to_flags(node.maybe_modifiers()).intersects(ModifierFlags::Ambient)
        {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub(crate) fn create_method_signature<
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
        TTypeParameters: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<TModifiers>,
        name: Option<TName>,
        question_token: Option<Rc<Node /*QuestionToken*/>>,
        type_parameters: Option<TTypeParameters>,
        parameters: Option<TParameters>,
        type_: Option<Rc<Node>>,
    ) -> MethodSignature {
        let node = self.create_base_signature_declaration(
            base_factory,
            SyntaxKind::MethodSignature,
            Option::<NodeArray>::None,
            modifiers,
            name,
            type_parameters,
            parameters,
            type_,
        );
        let mut node = MethodSignature::new(node, question_token);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_method_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
        TTypeParameters: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        asterisk_token: Option<Rc<Node /*AsteriskToken*/>>,
        name: TName,
        question_token: Option<Rc<Node /*QuestionToken*/>>,
        type_parameters: Option<TTypeParameters>,
        parameters: TParameters,
        type_: Option<Rc<Node /*TypeNode*/>>,
        body: Option<Rc<Node /*Block*/>>,
    ) -> MethodDeclaration {
        let mut node = self.create_base_function_like_declaration(
            base_factory,
            SyntaxKind::MethodDeclaration,
            decorators,
            modifiers,
            Some(name),
            type_parameters,
            Some(parameters),
            type_,
            body,
        );
        let asterisk_token_is_some = asterisk_token.is_some();
        let question_token_is_some = question_token.is_some();
        node.asterisk_token = asterisk_token;
        node.question_token = question_token;
        let mut node = MethodDeclaration::new(node);
        node.add_transform_flags(
            propagate_child_flags(node.maybe_asterisk_token())
                | propagate_child_flags(node.maybe_question_token())
                | TransformFlags::ContainsES2015,
        );
        if question_token_is_some {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        if modifiers_to_flags(node.maybe_modifiers()).intersects(ModifierFlags::Async) {
            if asterisk_token_is_some {
                node.add_transform_flags(TransformFlags::ContainsES2018);
            } else {
                node.add_transform_flags(TransformFlags::ContainsES2017);
            }
        } else if asterisk_token_is_some {
            node.add_transform_flags(TransformFlags::ContainsGenerator);
        }
        node
    }

    pub fn create_class_static_block_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        body: Rc<Node /*Block*/>,
    ) -> ClassStaticBlockDeclaration {
        let node = self.create_base_generic_named_declaration(
            base_factory,
            SyntaxKind::ClassStaticBlockDeclaration,
            decorators,
            modifiers,
            Option::<Rc<Node>>::None,
            Option::<NodeArray>::None,
        );
        let mut node = ClassStaticBlockDeclaration::new(node, body.clone());
        node.add_transform_flags(
            propagate_child_flags(Some(&*body)) | TransformFlags::ContainsClassFields,
        );
        node
    }

    pub fn create_constructor_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        parameters: TParameters,
        body: Option<Rc<Node /*Block*/>>,
    ) -> ConstructorDeclaration {
        let node = self.create_base_function_like_declaration(
            base_factory,
            SyntaxKind::Constructor,
            decorators,
            modifiers,
            Option::<Rc<Node>>::None,
            Option::<NodeArray>::None,
            Some(parameters),
            None,
            body,
        );
        let mut node = ConstructorDeclaration::new(node);
        node.add_transform_flags(TransformFlags::ContainsES2015);
        node
    }

    pub fn create_get_accessor_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StringOrRcNode>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: TName,
        parameters: TParameters,
        type_: Option<Rc<Node /*TypeNode*/>>,
        body: Option<Rc<Node /*Block*/>>,
    ) -> GetAccessorDeclaration {
        let node = self.create_base_function_like_declaration(
            base_factory,
            SyntaxKind::GetAccessor,
            decorators,
            modifiers,
            Some(name),
            Option::<NodeArray>::None,
            Some(parameters),
            type_,
            body,
        );
        GetAccessorDeclaration::new(node)
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
