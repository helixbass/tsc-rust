#![allow(non_upper_case_globals)]

use gc::Gc;

use super::{propagate_child_flags, propagate_identifier_name_flags};
use crate::{
    are_option_gcs_equal, has_node_array_changed, has_option_node_array_changed,
    has_option_str_or_node_changed, has_static_modifier, is_computed_property_name,
    is_exclamation_token, is_question_token, is_this_identifier, modifiers_to_flags, ArrayTypeNode,
    AsDoubleDeref, BaseNode, BaseNodeFactory, CallSignatureDeclaration,
    ClassStaticBlockDeclaration, ComputedPropertyName, ConditionalTypeNode,
    ConstructSignatureDeclaration, ConstructorDeclaration, ConstructorTypeNode, Decorator,
    FunctionLikeDeclarationInterface, FunctionTypeNode, GetAccessorDeclaration,
    HasInitializerInterface, HasQuestionTokenInterface, HasTypeArgumentsInterface,
    HasTypeInterface, HasTypeParametersInterface, IndexSignatureDeclaration, IntersectionTypeNode,
    MethodDeclaration, MethodSignature, ModifierFlags, NamedDeclarationInterface, NamedTupleMember,
    Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeInterface, OptionalTypeNode,
    ParameterDeclaration, PropertyDeclaration, PropertySignature, QualifiedName, RestTypeNode,
    SetAccessorDeclaration, SignatureDeclarationInterface, StrOrRcNode, SyntaxKind,
    TemplateLiteralTypeSpan, TransformFlags, TupleTypeNode, TypeLiteralNode,
    TypeParameterDeclaration, TypePredicateNode, TypeQueryNode, TypeReferenceNode, UnionTypeNode,
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
    ) -> Vec<Gc<Node /*Modifier*/>> {
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

    pub fn create_qualified_name<'right>(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*EntityName*/>,
        right: impl Into<StrOrRcNode<'right>>,
    ) -> QualifiedName {
        let node = self.create_base_node(base_factory, SyntaxKind::QualifiedName);
        let mut node =
            QualifiedName::new(node, left, self.as_name(base_factory, Some(right)).unwrap());
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.left)) | propagate_identifier_name_flags(&node.right),
        );
        node
    }

    pub fn update_qualified_name(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*QualifiedName*/
        left: Gc<Node /*EntityName*/>,
        right: Gc<Node /*Identifier*/>,
    ) -> Gc<Node> {
        let node_as_qualified_name = node.as_qualified_name();
        if !Gc::ptr_eq(&node_as_qualified_name.left, &left)
            || !Gc::ptr_eq(&node_as_qualified_name.right, &right)
        {
            self.update(
                self.create_qualified_name(base_factory, left, right).into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_computed_property_name(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
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

    pub fn update_computed_property_name(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ComputedPropertyName*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_type_parameter_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: impl Into<StrOrRcNode<'name>>,
        constraint: Option<Gc<Node /*TypeNode*/>>,
        default_type: Option<Gc<Node /*TypeNode*/>>,
    ) -> TypeParameterDeclaration {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::TypeParameter,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(name),
        );
        let mut node = TypeParameterDeclaration::new(node, constraint, default_type);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_parameter_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*TypeParameterDeclaration*/
        name: Gc<Node>,
        constraint: Option<Gc<Node /*TypeNode*/>>,
        default_type: Option<Gc<Node /*TypeNode*/>>,
    ) -> Gc<Node> {
        let node_as_type_parameter_declaration = node.as_type_parameter_declaration();
        if !Gc::ptr_eq(&node_as_type_parameter_declaration.name(), &name)
            || !are_option_gcs_equal(
                node_as_type_parameter_declaration.constraint.as_ref(),
                constraint.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_type_parameter_declaration.default.as_ref(),
                default_type.as_ref(),
            )
        {
            self.update(
                self.create_type_parameter_declaration(
                    base_factory,
                    name,
                    constraint,
                    default_type,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_parameter_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> ParameterDeclaration {
        let initializer_is_some = initializer.is_some();
        let node = self.create_base_variable_like_declaration(
            base_factory,
            SyntaxKind::Parameter,
            decorators,
            modifiers,
            name,
            type_,
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(base_factory, &initializer)
            }),
        );
        let question_token_is_some = question_token.is_some();
        let dot_dot_dot_token_is_some = dot_dot_dot_token.is_some();
        let node = ParameterDeclaration::new(node, dot_dot_dot_token, question_token);
        if is_this_identifier(node.maybe_name()) {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        } else {
            node.add_transform_flags(
                propagate_child_flags(node.dot_dot_dot_token.clone())
                    | propagate_child_flags(node.question_token.clone()),
            );
            if question_token_is_some {
                node.add_transform_flags(TransformFlags::ContainsTypeScript);
            }
            if modifiers_to_flags(node.maybe_modifiers().as_double_deref())
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

    pub fn update_parameter_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ParameterDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        let node_as_parameter_declaration = node.as_parameter_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let name = name.map(Into::into);
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || has_option_str_or_node_changed(
                node_as_parameter_declaration.maybe_name().as_ref(),
                name.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_parameter_declaration
                    .maybe_question_token()
                    .as_ref(),
                question_token.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_parameter_declaration.maybe_type().as_ref(),
                type_.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_parameter_declaration.maybe_initializer().as_ref(),
                initializer.as_ref(),
            )
        {
            self.update(
                self.create_parameter_declaration(
                    base_factory,
                    decorators,
                    modifiers,
                    dot_dot_dot_token,
                    name,
                    question_token,
                    type_,
                    initializer,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_decorator(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
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

    pub fn update_decorator(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*Decorator*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_property_signature<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        question_token: Option<Gc<Node>>,
        type_: Option<Gc<Node>>,
    ) -> PropertySignature {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertySignature,
            Option::<Gc<NodeArray>>::None,
            modifiers,
            Some(name),
        );
        let mut node = PropertySignature::new(node, question_token, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_property_signature(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*PropertySignature*/
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Gc<Node>,
        question_token: Option<Gc<Node>>,
        type_: Option<Gc<Node>>,
    ) -> Gc<Node> {
        let node_as_property_signature = node.as_property_signature();
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !Gc::ptr_eq(&node_as_property_signature.name(), &name)
            || !are_option_gcs_equal(
                node_as_property_signature.question_token.as_ref(),
                question_token.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_property_signature.maybe_type().as_ref(),
                type_.as_ref(),
            )
        {
            self.update(
                self.create_property_signature(
                    base_factory,
                    modifiers,
                    name,
                    question_token,
                    type_,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_property_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        question_or_exclamation_token: Option<Gc<Node /*QuestionToken | ExclamationToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> /*PropertyDeclaration*/ {
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
        let node: Gc<Node> = node.into();
        let node_as_property_declaration = node.as_property_declaration();
        if is_computed_property_name(&node_as_property_declaration.name())
            || has_static_modifier(&node)
                && node_as_property_declaration.maybe_initializer().is_some()
        {
            node.add_transform_flags(TransformFlags::ContainsTypeScriptClassSyntax);
        }
        if question_or_exclamation_token_is_some
            || modifiers_to_flags(node.maybe_modifiers().as_double_deref())
                .intersects(ModifierFlags::Ambient)
        {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_property_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*PropertyDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        question_or_exclamation_token: Option<Gc<Node /*QuestionToken | ExclamationToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> /*PropertyDeclaration*/ {
        let node_as_property_declaration = node.as_property_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let name = name.into();
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !matches!(
                &name,
                StrOrRcNode::RcNode(name) if Gc::ptr_eq(
                    &node_as_property_declaration.name(),
                    name,
                )
            )
            || !are_option_gcs_equal(
                node_as_property_declaration.maybe_question_token().as_ref(),
                question_or_exclamation_token
                    .as_ref()
                    .filter(|question_or_exclamation_token| {
                        is_question_token(question_or_exclamation_token)
                    }),
            )
            || !are_option_gcs_equal(
                node_as_property_declaration.exclamation_token.as_ref(),
                question_or_exclamation_token
                    .as_ref()
                    .filter(|question_or_exclamation_token| {
                        is_exclamation_token(question_or_exclamation_token)
                    }),
            )
            || !are_option_gcs_equal(
                node_as_property_declaration.maybe_type().as_ref(),
                type_.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_property_declaration.maybe_initializer().as_ref(),
                initializer.as_ref(),
            )
        {
            self.update(
                self.create_property_declaration(
                    base_factory,
                    decorators,
                    modifiers,
                    name,
                    question_or_exclamation_token,
                    type_,
                    initializer,
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub(crate) fn create_method_signature<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: Option<impl Into<NodeArrayOrVec>>,
        type_: Option<Gc<Node>>,
    ) -> MethodSignature {
        let node = self.create_base_signature_declaration(
            base_factory,
            SyntaxKind::MethodSignature,
            Option::<Gc<NodeArray>>::None,
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

    pub(crate) fn update_method_signature(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*MethodSignature*/
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Gc<Node /*PropertyName*/>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        type_parameters: Option<Gc<NodeArray /*<TypeParameterDeclaration>*/>>,
        parameters: Gc<NodeArray /*<ParameterDeclaration>*/>,
        type_: Option<Gc<Node>>,
    ) -> Gc<Node> {
        let node_as_method_signature = node.as_method_signature();
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !Gc::ptr_eq(&node_as_method_signature.name(), &name)
            || !are_option_gcs_equal(
                node_as_method_signature.maybe_question_token().as_ref(),
                question_token.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_method_signature.maybe_type_parameters().as_ref(),
                type_parameters.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_method_signature.parameters(), &parameters)
            || !are_option_gcs_equal(
                node_as_method_signature.maybe_type().as_ref(),
                type_.as_ref(),
            )
        {
            self.update_base_signature_declaration(
                self.create_method_signature(
                    base_factory,
                    modifiers,
                    Some(name),
                    question_token,
                    type_parameters,
                    Some(parameters),
                    type_,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_method_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Gc<Node /*AsteriskToken*/>>,
        name: impl Into<StrOrRcNode<'name>>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        body: Option<Gc<Node /*Block*/>>,
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
        if modifiers_to_flags(node.maybe_modifiers().as_double_deref())
            .intersects(ModifierFlags::Async)
        {
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

    pub fn update_method_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*MethodDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Gc<Node /*AsteriskToken*/>>,
        name: Gc<Node /*PropertyName*/>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        body: Option<Gc<Node /*Block*/>>,
    ) -> Gc<Node> {
        let node_as_method_declaration = node.as_method_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let type_parameters = type_parameters.map(Into::into);
        let parameters = parameters.into();
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !are_option_gcs_equal(
                node_as_method_declaration.maybe_asterisk_token().as_ref(),
                asterisk_token.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_method_declaration.name(), &name)
            || !are_option_gcs_equal(
                node_as_method_declaration.maybe_question_token().as_ref(),
                question_token.as_ref(),
            )
            || has_option_node_array_changed(
                node_as_method_declaration
                    .maybe_type_parameters()
                    .as_deref(),
                type_parameters.as_ref(),
            )
            || has_node_array_changed(&node_as_method_declaration.parameters(), &parameters)
            || !are_option_gcs_equal(
                node_as_method_declaration.maybe_type().as_ref(),
                type_.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_method_declaration.maybe_body().as_ref(),
                body.as_ref(),
            )
        {
            self.update_base_function_like_declaration(
                self.create_method_declaration(
                    base_factory,
                    decorators,
                    modifiers,
                    asterisk_token,
                    name,
                    question_token,
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

    pub fn create_class_static_block_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        body: Gc<Node /*Block*/>,
    ) -> ClassStaticBlockDeclaration {
        let node = self.create_base_generic_named_declaration(
            base_factory,
            SyntaxKind::ClassStaticBlockDeclaration,
            decorators,
            modifiers,
            Option::<Gc<Node>>::None,
            Option::<Gc<NodeArray>>::None,
        );
        let mut node = ClassStaticBlockDeclaration::new(node, body.clone());
        node.add_transform_flags(
            propagate_child_flags(Some(&*body)) | TransformFlags::ContainsClassFields,
        );
        node
    }

    pub fn update_class_static_block_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ClassStaticBlockDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        body: Gc<Node /*Block*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_constructor_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        body: Option<Gc<Node /*Block*/>>,
    ) -> ConstructorDeclaration {
        let node = self.create_base_function_like_declaration(
            base_factory,
            SyntaxKind::Constructor,
            decorators,
            modifiers,
            Option::<Gc<Node>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(parameters),
            None,
            body,
        );
        let mut node = ConstructorDeclaration::new(node);
        node.add_transform_flags(TransformFlags::ContainsES2015);
        node
    }

    pub fn update_constructor_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ConstructorDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        body: Option<Gc<Node /*Block*/>>,
    ) -> Gc<Node> {
        let node_as_constructor_declaration = node.as_constructor_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let parameters = parameters.into();
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || has_node_array_changed(&node_as_constructor_declaration.parameters(), &parameters)
            || !are_option_gcs_equal(
                node_as_constructor_declaration.maybe_body().as_ref(),
                body.as_ref(),
            )
        {
            self.update_base_function_like_declaration(
                self.create_constructor_declaration(
                    base_factory,
                    decorators,
                    modifiers,
                    parameters,
                    body,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_get_accessor_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        body: Option<Gc<Node /*Block*/>>,
    ) -> GetAccessorDeclaration {
        let node = self.create_base_function_like_declaration(
            base_factory,
            SyntaxKind::GetAccessor,
            decorators,
            modifiers,
            Some(name),
            Option::<Gc<NodeArray>>::None,
            Some(parameters),
            type_,
            body,
        );
        GetAccessorDeclaration::new(node)
    }

    pub fn update_get_accessor_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*GetAccessorDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Gc<Node /*PropertyName*/>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        body: Option<Gc<Node /*Block*/>>,
    ) -> Gc<Node> {
        let node_as_get_accessor_declaration = node.as_get_accessor_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let parameters = parameters.into();
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !Gc::ptr_eq(&node_as_get_accessor_declaration.name(), &name)
            || has_node_array_changed(&node_as_get_accessor_declaration.parameters(), &parameters)
            || !are_option_gcs_equal(
                node_as_get_accessor_declaration.maybe_type().as_ref(),
                type_.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_get_accessor_declaration.maybe_body().as_ref(),
                body.as_ref(),
            )
        {
            self.update_base_function_like_declaration(
                self.create_get_accessor_declaration(
                    base_factory,
                    decorators,
                    modifiers,
                    name,
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

    pub fn create_set_accessor_declaration<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        parameters: impl Into<NodeArrayOrVec>,
        body: Option<Gc<Node /*Block*/>>,
    ) -> SetAccessorDeclaration {
        let node = self.create_base_function_like_declaration(
            base_factory,
            SyntaxKind::SetAccessor,
            decorators,
            modifiers,
            Some(name),
            Option::<Gc<NodeArray>>::None,
            Some(parameters),
            None,
            body,
        );
        SetAccessorDeclaration::new(node)
    }

    pub fn update_set_accessor_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*SetAccessorDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Gc<Node /*PropertyName*/>,
        parameters: impl Into<NodeArrayOrVec>,
        body: Option<Gc<Node /*Block*/>>,
    ) -> Gc<Node> {
        let node_as_set_accessor_declaration = node.as_set_accessor_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let parameters = parameters.into();
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !Gc::ptr_eq(&node_as_set_accessor_declaration.name(), &name)
            || has_node_array_changed(&node_as_set_accessor_declaration.parameters(), &parameters)
            || !are_option_gcs_equal(
                node_as_set_accessor_declaration.maybe_body().as_ref(),
                body.as_ref(),
            )
        {
            self.update_base_function_like_declaration(
                self.create_set_accessor_declaration(
                    base_factory,
                    decorators,
                    modifiers,
                    name,
                    parameters,
                    body,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_call_signature(
        &self,
        base_factory: &TBaseNodeFactory,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> CallSignatureDeclaration {
        let node = self.create_base_signature_declaration(
            base_factory,
            SyntaxKind::CallSignature,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<Node>>::None,
            type_parameters,
            Some(parameters),
            type_,
        );
        let mut node = CallSignatureDeclaration::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_call_signature(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*CallSignatureDeclaration*/
        type_parameters: Option<Gc<NodeArray /*<TypeParameterDeclaration>*/>>,
        parameters: Gc<NodeArray /*<ParameterDeclaration>*/>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> Gc<Node> {
        let node_as_call_signature_declaration = node.as_call_signature_declaration();
        if !are_option_gcs_equal(
            node_as_call_signature_declaration
                .maybe_type_parameters()
                .as_ref(),
            type_parameters.as_ref(),
        ) || !Gc::ptr_eq(
            &node_as_call_signature_declaration.parameters(),
            &parameters,
        ) || !are_option_gcs_equal(
            node_as_call_signature_declaration.maybe_type().as_ref(),
            type_.as_ref(),
        ) {
            self.update_base_signature_declaration(
                self.create_call_signature(base_factory, type_parameters, parameters, type_)
                    .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_construct_signature(
        &self,
        base_factory: &TBaseNodeFactory,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> ConstructSignatureDeclaration {
        let node = self.create_base_signature_declaration(
            base_factory,
            SyntaxKind::ConstructSignature,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<Node>>::None,
            type_parameters,
            Some(parameters),
            type_,
        );
        let mut node = ConstructSignatureDeclaration::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_construct_signature(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ConstructSignatureDeclaration*/
        type_parameters: Option<Gc<NodeArray /*<TypeParameterDeclaration>*/>>,
        parameters: Gc<NodeArray /*<ParameterDeclaration>*/>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> Gc<Node> {
        let node_as_construct_signature_declaration = node.as_construct_signature_declaration();
        if !are_option_gcs_equal(
            node_as_construct_signature_declaration
                .maybe_type_parameters()
                .as_ref(),
            type_parameters.as_ref(),
        ) || !Gc::ptr_eq(
            &node_as_construct_signature_declaration.parameters(),
            &parameters,
        ) || !are_option_gcs_equal(
            node_as_construct_signature_declaration
                .maybe_type()
                .as_ref(),
            type_.as_ref(),
        ) {
            self.update_base_signature_declaration(
                self.create_construct_signature(base_factory, type_parameters, parameters, type_)
                    .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_index_signature(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> IndexSignatureDeclaration {
        let node = self.create_base_signature_declaration(
            base_factory,
            SyntaxKind::IndexSignature,
            decorators,
            modifiers,
            Option::<Gc<Node>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(parameters),
            type_,
        );
        let mut node = IndexSignatureDeclaration::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_index_signature(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*IndexSignatureDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_index_signature_declaration = node.as_index_signature_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        let parameters = parameters.into();
        if !has_node_array_changed(
            &node_as_index_signature_declaration.parameters(),
            &parameters,
        ) || !Gc::ptr_eq(
            &node_as_index_signature_declaration.maybe_type().unwrap(),
            &type_,
        ) || !has_option_node_array_changed(
            node.maybe_decorators().as_deref(),
            decorators.as_ref(),
        ) || !has_option_node_array_changed(
            node.maybe_modifiers().as_deref(),
            modifiers.as_ref(),
        ) {
            self.update_base_signature_declaration(
                self.create_index_signature(
                    base_factory,
                    decorators,
                    modifiers,
                    parameters,
                    Some(type_),
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_template_literal_type_span(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Gc<Node /*TypeNode*/>,
        literal: Gc<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> TemplateLiteralTypeSpan {
        let node = self.create_base_node(base_factory, SyntaxKind::TemplateLiteralTypeSpan);
        let mut node = TemplateLiteralTypeSpan::new(node, type_, literal);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_template_literal_type_span(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*TemplateLiteralTypeSpan*/
        type_: Gc<Node /*TypeNode*/>,
        literal: Gc<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_keyword_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
    ) -> BaseNode {
        self.create_token(base_factory, kind)
    }

    pub fn create_type_predicate_node<'parameter_name>(
        &self,
        base_factory: &TBaseNodeFactory,
        asserts_modifier: Option<Gc<Node /*AssertsKeyword*/>>,
        parameter_name: impl Into<StrOrRcNode<'parameter_name>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> TypePredicateNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypePredicate);
        let mut node = TypePredicateNode::new(
            node,
            asserts_modifier,
            self.as_name(base_factory, Some(parameter_name)).unwrap(),
            type_,
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_predicate_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*TypePredicateNode*/
        asserts_modifier: Option<Gc<Node /*AssertsKeyword*/>>,
        parameter_name: Gc<Node>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_type_reference_node<'type_name>(
        &self,
        base_factory: &TBaseNodeFactory,
        type_name: impl Into<StrOrRcNode<'type_name>>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
    ) -> TypeReferenceNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypeReference);
        let mut node = TypeReferenceNode::new(
            node,
            self.as_name(base_factory, Some(type_name)).unwrap(),
            type_arguments.and_then(|type_arguments| {
                self.parenthesizer_rules().parenthesize_type_arguments(
                    base_factory,
                    Some(self.create_node_array(Some(type_arguments), None).into()),
                )
            }),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_reference_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*TypeReferenceNode*/
        type_name: Gc<Node /*EntityName*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec> /*<TypeNode>*/>,
    ) -> Gc<Node> {
        let type_arguments = type_arguments.map(Into::into);
        let node_as_type_reference_node = node.as_type_reference_node();
        let node_type_arguments = node_as_type_reference_node.maybe_type_arguments();
        if !Gc::ptr_eq(&node_as_type_reference_node.type_name, &type_name)
            || has_option_node_array_changed(
                node_type_arguments.as_deref(),
                type_arguments.as_ref(),
            )
        {
            self.update(
                self.create_type_reference_node(base_factory, type_name, type_arguments)
                    .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_function_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> FunctionTypeNode {
        let node = self.create_base_signature_declaration(
            base_factory,
            SyntaxKind::FunctionType,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<Node>>::None,
            type_parameters,
            Some(parameters),
            type_,
        );
        let mut node = FunctionTypeNode::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_function_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*FunctionTypeNode*/
        type_parameters: Option<Gc<NodeArray /*<TypeParameterDeclaration>*/>>,
        parameters: Gc<NodeArray /*<ParameterDeclaration>*/>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> Gc<Node> {
        let node_as_function_type_node = node.as_function_type_node();
        if !are_option_gcs_equal(
            node_as_function_type_node.maybe_type_parameters().as_ref(),
            type_parameters.as_ref(),
        ) || !Gc::ptr_eq(&node_as_function_type_node.parameters(), &parameters)
            || !are_option_gcs_equal(
                node_as_function_type_node.maybe_type().as_ref(),
                type_.as_ref(),
            )
        {
            self.update_base_signature_declaration(
                self.create_function_type_node(base_factory, type_parameters, parameters, type_)
                    .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_constructor_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> ConstructorTypeNode {
        let node = self.create_base_signature_declaration(
            base_factory,
            SyntaxKind::ConstructorType,
            Option::<Gc<NodeArray>>::None,
            modifiers,
            Option::<Gc<Node>>::None,
            type_parameters,
            Some(parameters),
            type_,
        );
        let mut node = ConstructorTypeNode::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_constructor_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ConstructorTypeNode*/
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        type_parameters: Option<Gc<NodeArray /*<TypeParameterDeclaration>*/>>,
        parameters: Gc<NodeArray /*<ParameterDeclaration>*/>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> Gc<Node> {
        let node_as_constructor_type_node = node.as_constructor_type_node();
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !are_option_gcs_equal(
                node_as_constructor_type_node
                    .maybe_type_parameters()
                    .as_ref(),
                type_parameters.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_constructor_type_node.parameters(), &parameters)
            || !are_option_gcs_equal(
                node_as_constructor_type_node.maybe_type().as_ref(),
                type_.as_ref(),
            )
        {
            self.update_base_signature_declaration(
                self.create_constructor_type_node(
                    base_factory,
                    modifiers,
                    type_parameters,
                    parameters,
                    type_,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_type_query_node(
        &self,
        base_factory: &TBaseNodeFactory,
        expr_name: Gc<Node /*EntityName*/>,
    ) -> TypeQueryNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypeQuery);
        let mut node = TypeQueryNode::new(node, expr_name);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_query_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*TypeQueryNode*/
        expr_name: Gc<Node /*EntityName*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_type_literal_node(
        &self,
        base_factory: &TBaseNodeFactory,
        members: Option<impl Into<NodeArrayOrVec>>,
    ) -> TypeLiteralNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypeLiteral);
        let mut node = TypeLiteralNode::new(node, self.create_node_array(members, None));
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_literal_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*TypeLiteralNode*/
        members: Gc<NodeArray>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_array_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        element_type: Gc<Node /*TypeNode*/>,
    ) -> ArrayTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::ArrayType);
        let mut node = ArrayTypeNode::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_element_type_of_array_type(base_factory, &element_type),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_array_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ArrayTypeNode*/
        element_type: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_tuple_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: Option<impl Into<NodeArrayOrVec>>,
    ) -> TupleTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TupleType);
        let mut node = TupleTypeNode::new(node, self.create_node_array(elements, None));
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_tuple_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*TupleTypeNode*/
        elements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_named_tuple_member(
        &self,
        base_factory: &TBaseNodeFactory,
        dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
        name: Gc<Node /*Identifier*/>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        type_: Gc<Node /*TypeNode*/>,
    ) -> NamedTupleMember {
        let node = self.create_base_node(base_factory, SyntaxKind::NamedTupleMember);
        let mut node = NamedTupleMember::new(node, dot_dot_dot_token, name, question_token, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_named_tuple_member(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*NamedTupleMember*/
        dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
        name: Gc<Node /*Identifier*/>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_optional_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Gc<Node /*TypeNode*/>,
    ) -> OptionalTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::OptionalType);
        let mut node = OptionalTypeNode::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_element_type_of_array_type(base_factory, &type_),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_optional_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*OptionalTypeNode*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_rest_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Gc<Node /*TypeNode*/>,
    ) -> RestTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::RestType);
        let mut node = RestTypeNode::new(node, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_rest_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*RestTypeNode*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_union_or_intersection_type_node<TTypes: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind, /*SyntaxKind.UnionType | SyntaxKind.IntersectionType*/
        types: TTypes,    /*<TypeNode>*/
    ) -> Node {
        let node = self.create_base_node(base_factory, kind);
        let types = self
            .parenthesizer_rules()
            .parenthesize_constituent_types_of_union_or_intersection_type(
                base_factory,
                types.into(),
            );
        let mut node: Node = match kind {
            SyntaxKind::UnionType => UnionTypeNode::new(node, types).into(),
            SyntaxKind::IntersectionType => IntersectionTypeNode::new(node, types).into(),
            _ => panic!("Expected UnionType or IntersectionType"),
        };
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn create_union_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        types: impl Into<NodeArrayOrVec>, /*<TypeNode>*/
    ) -> Node {
        self.create_union_or_intersection_type_node(base_factory, SyntaxKind::UnionType, types)
    }

    pub fn update_union_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*UnionTypeNode*/
        types: Gc<NodeArray /*<TypeNode>*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_intersection_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        types: impl Into<NodeArrayOrVec>, /*<TypeNode>*/
    ) -> Node {
        self.create_union_or_intersection_type_node(
            base_factory,
            SyntaxKind::IntersectionType,
            types,
        )
    }

    pub fn update_intersection_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*IntersectionTypeNode*/
        types: Gc<NodeArray /*<TypeNode>*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_conditional_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        check_type: Gc<Node /*TypeNode*/>,
        extends_type: Gc<Node /*TypeNode*/>,
        true_type: Gc<Node /*TypeNode*/>,
        false_type: Gc<Node /*TypeNode*/>,
    ) -> ConditionalTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::ConditionalType);
        let mut node = ConditionalTypeNode::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_member_of_conditional_type(base_factory, &check_type),
            self.parenthesizer_rules()
                .parenthesize_member_of_conditional_type(base_factory, &extends_type),
            true_type,
            false_type,
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_conditional_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ConditionalTypeNode*/
        check_type: Gc<Node /*TypeNode*/>,
        extends_type: Gc<Node /*TypeNode*/>,
        true_type: Gc<Node /*TypeNode*/>,
        false_type: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_conditional_type_node = node.as_conditional_type_node();
        if !Gc::ptr_eq(&node_as_conditional_type_node.check_type, &check_type)
            || !Gc::ptr_eq(&node_as_conditional_type_node.extends_type, &extends_type)
            || !Gc::ptr_eq(&node_as_conditional_type_node.true_type, &true_type)
            || !Gc::ptr_eq(&node_as_conditional_type_node.false_type, &false_type)
        {
            self.update(
                self.create_conditional_type_node(
                    base_factory,
                    check_type,
                    extends_type,
                    true_type,
                    false_type,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }
}
