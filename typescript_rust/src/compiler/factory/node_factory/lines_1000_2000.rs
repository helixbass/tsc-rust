use gc::{Finalize, Gc, Trace};
use id_arena::Id;
use local_macros::generate_node_factory_method_wrapper;

use super::{propagate_child_flags, propagate_identifier_name_flags};
use crate::{
    are_option_gcs_equal, has_node_array_changed, has_option_node_array_changed,
    has_option_str_or_node_changed, has_static_modifier, is_computed_property_name,
    is_exclamation_token, is_question_token, is_this_identifier, modifiers_to_flags, AllArenas,
    ArrayTypeNode, AsDoubleDeref, BaseNode, BaseNodeFactory, CallSignatureDeclaration,
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

impl<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize> NodeFactory<TBaseNodeFactory> {
    #[generate_node_factory_method_wrapper]
    pub fn create_super_raw(&self, arena: &AllArenas, id: Id<Node>) -> BaseNode {
        self.create_token_raw(arena, id, SyntaxKind::SuperKeyword)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_this_raw(&self, arena: &AllArenas, id: Id<Node>) -> BaseNode {
        self.create_token_raw(arena, id, SyntaxKind::ThisKeyword)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_null_raw(&self, arena: &AllArenas, id: Id<Node>) -> BaseNode {
        self.create_token_raw(arena, id, SyntaxKind::NullKeyword)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_true_raw(&self, arena: &AllArenas, id: Id<Node>) -> BaseNode {
        self.create_token_raw(arena, id, SyntaxKind::TrueKeyword)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_false_raw(&self, arena: &AllArenas, id: Id<Node>) -> BaseNode {
        self.create_token_raw(arena, id, SyntaxKind::FalseKeyword)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_modifier_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        kind: SyntaxKind,
    ) -> BaseNode {
        self.create_token_raw(arena, id, kind)
    }

    pub fn create_modifiers_from_modifier_flags(
        &self,
        arena: &AllArenas,
        flags: ModifierFlags,
    ) -> Vec<Id<Node /*Modifier*/>> {
        let mut result = vec![];
        if flags.intersects(ModifierFlags::Export) {
            result.push(self.create_modifier(arena, SyntaxKind::ExportKeyword));
        }
        if flags.intersects(ModifierFlags::Ambient) {
            result.push(self.create_modifier(arena, SyntaxKind::DeclareKeyword));
        }
        if flags.intersects(ModifierFlags::Default) {
            result.push(self.create_modifier(arena, SyntaxKind::DefaultKeyword));
        }
        if flags.intersects(ModifierFlags::Const) {
            result.push(self.create_modifier(arena, SyntaxKind::ConstKeyword));
        }
        if flags.intersects(ModifierFlags::Public) {
            result.push(self.create_modifier(arena, SyntaxKind::PublicKeyword));
        }
        if flags.intersects(ModifierFlags::Private) {
            result.push(self.create_modifier(arena, SyntaxKind::PrivateKeyword));
        }
        if flags.intersects(ModifierFlags::Protected) {
            result.push(self.create_modifier(arena, SyntaxKind::ProtectedKeyword));
        }
        if flags.intersects(ModifierFlags::Abstract) {
            result.push(self.create_modifier(arena, SyntaxKind::AbstractKeyword));
        }
        if flags.intersects(ModifierFlags::Static) {
            result.push(self.create_modifier(arena, SyntaxKind::StaticKeyword));
        }
        if flags.intersects(ModifierFlags::Override) {
            result.push(self.create_modifier(arena, SyntaxKind::OverrideKeyword));
        }
        if flags.intersects(ModifierFlags::Readonly) {
            result.push(self.create_modifier(arena, SyntaxKind::ReadonlyKeyword));
        }
        if flags.intersects(ModifierFlags::Async) {
            result.push(self.create_modifier(arena, SyntaxKind::AsyncKeyword));
        }
        result
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_qualified_name_raw<'right>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        left: Id<Node /*EntityName*/>,
        right: impl Into<StrOrRcNode<'right>>,
    ) -> QualifiedName {
        let node = self.create_base_node(id, SyntaxKind::QualifiedName);
        let node = QualifiedName::new(node, left, self.as_name(Some(right)).unwrap());
        node.add_transform_flags(
            propagate_child_flags(arena, Some(&*node.left))
                | propagate_identifier_name_flags(arena, &node.right),
        );
        node
    }

    pub fn update_qualified_name(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*QualifiedName*/
        left: Id<Node /*EntityName*/>,
        right: Id<Node /*Identifier*/>,
    ) -> Id<Node> {
        if {
            let node = &arena.nodes[node];
            let node_as_qualified_name = node.as_qualified_name();
            node_as_qualified_name.left != left || node_as_qualified_name.right != right
        } {
            self.update(arena, self.create_qualified_name(left, right), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_computed_property_name_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        expression: Gc<Node /*Expression*/>,
    ) -> ComputedPropertyName {
        let node = self.create_base_node(id, SyntaxKind::ComputedPropertyName);
        let mut node = ComputedPropertyName::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_of_computed_property_name(&expression),
        );
        node.add_transform_flags(
            propagate_child_flags(arena, Some(&*node.expression))
                | TransformFlags::ContainsES2015
                | TransformFlags::ContainsComputedPropertyName,
        );
        node
    }

    pub fn update_computed_property_name(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*ComputedPropertyName*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        if { arena.node(node).as_computed_property_name().expression != expression } {
            self.update(
                arena,
                self.create_computed_property_name(arena, expression),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_parameter_declaration_raw<'name>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        name: impl Into<StrOrRcNode<'name>>,
        constraint: Option<Id<Node /*TypeNode*/>>,
        default_type: Option<Id<Node /*TypeNode*/>>,
    ) -> TypeParameterDeclaration {
        let node = self.create_base_named_declaration(
            arena,
            id,
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
        arena: &AllArenas,
        node: Id<Node>, /*TypeParameterDeclaration*/
        name: Id<Node>,
        constraint: Option<Id<Node /*TypeNode*/>>,
        default_type: Option<Id<Node /*TypeNode*/>>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_type_parameter_declaration = node.as_type_parameter_declaration();
            node_as_type_parameter_declaration.name() != name
                || node_as_type_parameter_declaration.constraint != constraint
                || node_as_type_parameter_declaration.default != default_type
        } {
            self.update(
                arena,
                self.create_type_parameter_declaration(arena, name, constraint, default_type),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_parameter_declaration_raw<'name>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        dot_dot_dot_token: Option<Id<Node /*DotDotDotToken*/>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        question_token: Option<Id<Node /*QuestionToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> ParameterDeclaration {
        let initializer_is_some = initializer.is_some();
        let node = self.create_base_variable_like_declaration(
            arena,
            id,
            SyntaxKind::Parameter,
            decorators,
            modifiers,
            name,
            type_,
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(&initializer)
            }),
        );
        let question_token_is_some = question_token.is_some();
        let dot_dot_dot_token_is_some = dot_dot_dot_token.is_some();
        let mut node = ParameterDeclaration::new(node, dot_dot_dot_token, question_token);
        if is_this_identifier(node.maybe_name()) {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        } else {
            node.add_transform_flags(
                propagate_child_flags(arena, node.dot_dot_dot_token.clone())
                    | propagate_child_flags(arena, node.question_token.clone()),
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
        arena: &AllArenas,
        node: Id<Node>, /*ParameterDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        dot_dot_dot_token: Option<Id<Node /*DotDotDotToken*/>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        question_token: Option<Id<Node /*QuestionToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_parameter_declaration = node.as_parameter_declaration();
            let decorators = decorators.map(Into::into);
            let modifiers = modifiers.map(Into::into);
            let name = name.map(Into::into);
            has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
                || has_option_node_array_changed(
                    node.maybe_modifiers().as_deref(),
                    modifiers.as_ref(),
                )
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
        } {
            self.update(
                arena,
                self.create_parameter_declaration(
                    arena,
                    decorators,
                    modifiers,
                    dot_dot_dot_token,
                    name,
                    question_token,
                    type_,
                    initializer,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_decorator_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        expression: Id<Node /*Expression*/>,
    ) -> Decorator {
        let node = self.create_base_node(id, SyntaxKind::Decorator);
        let mut node = Decorator::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(&expression),
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
        arena: &AllArenas,
        node: Id<Node>, /*Decorator*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_decorator = node.as_decorator();
            node_as_decorator.expression != expression
        } {
            self.update(arena, self.create_decorator(arena, expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_property_signature_raw<'name>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        question_token: Option<Id<Node>>,
        type_: Option<Id<Node>>,
    ) -> PropertySignature {
        let node = self.create_base_named_declaration(
            arena,
            id,
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
        arena: &AllArenas,
        node: Id<Node>, /*PropertySignature*/
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Id<Node>,
        question_token: Option<Id<Node>>,
        type_: Option<Id<Node>>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_property_signature = node.as_property_signature();
            let modifiers = modifiers.map(Into::into);
            has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
                || node_as_property_signature.name() != name
                || node_as_property_signature.question_token != question_token
                || node_as_property_signature.maybe_type() != type_
        } {
            self.update(
                arena,
                self.create_property_signature(arena, modifiers, name, question_token, type_),
                node,
            )
        } else {
            node
        }
    }

    pub fn create_property_declaration<'name>(
        &self,
        arena: &AllArenas,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        question_or_exclamation_token: Option<Id<Node /*QuestionToken | ExclamationToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> /*PropertyDeclaration*/ {
        let node = arena.nodes.borrow_mut().alloc_with_id(|id| {
            let node = self.create_base_variable_like_declaration(
                arena,
                id,
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
                propagate_child_flags(arena, node.question_token.clone())
                    | propagate_child_flags(arena, node.exclamation_token.clone())
                    | TransformFlags::ContainsClassFields,
            );
            node
        });
        if {
            let node = arena.node(node);
            let node_as_property_declaration = node.as_property_declaration();
            is_computed_property_name(&node_as_property_declaration.name())
                || has_static_modifier(&node)
                    && node_as_property_declaration.maybe_initializer().is_some()
        } {
            arena
                .node_mut(node)
                .add_transform_flags(TransformFlags::ContainsTypeScriptClassSyntax);
        }
        if question_or_exclamation_token_is_some
            || modifiers_to_flags(node.maybe_modifiers().as_double_deref())
                .intersects(ModifierFlags::Ambient)
        {
            arena
                .node_mut(node)
                .add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_property_declaration<'name>(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*PropertyDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        question_or_exclamation_token: Option<Gc<Node /*QuestionToken | ExclamationToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> /*PropertyDeclaration*/ {
        if {
            let node = arena.node(node);
            let node_as_property_declaration = node.as_property_declaration();
            let decorators = decorators.map(Into::into);
            let modifiers = modifiers.map(Into::into);
            let name = name.into();
            has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
                || has_option_node_array_changed(
                    node.maybe_modifiers().as_deref(),
                    modifiers.as_ref(),
                )
                || !matches!(
                    &name,
                    StrOrRcNode::RcNode(name) if Gc::ptr_eq(
                        &node_as_property_declaration.name(),
                        name,
                    )
                )
                || !are_option_gcs_equal(
                    node_as_property_declaration.maybe_question_token().as_ref(),
                    question_or_exclamation_token.as_ref().filter(
                        |question_or_exclamation_token| {
                            is_question_token(question_or_exclamation_token)
                        },
                    ),
                )
                || !are_option_gcs_equal(
                    node_as_property_declaration.exclamation_token.as_ref(),
                    question_or_exclamation_token.as_ref().filter(
                        |question_or_exclamation_token| {
                            is_exclamation_token(question_or_exclamation_token)
                        },
                    ),
                )
                || !are_option_gcs_equal(
                    node_as_property_declaration.maybe_type().as_ref(),
                    type_.as_ref(),
                )
                || !are_option_gcs_equal(
                    node_as_property_declaration.maybe_initializer().as_ref(),
                    initializer.as_ref(),
                )
        } {
            self.update(
                arena,
                self.create_property_declaration(
                    arena,
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
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub(crate) fn create_method_signature_raw<'name>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        question_token: Option<Id<Node /*QuestionToken*/>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: Option<impl Into<NodeArrayOrVec>>,
        type_: Option<Id<Node>>,
    ) -> MethodSignature {
        let node = self.create_base_signature_declaration(
            arena,
            id,
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
        arena: &AllArenas,
        node: Id<Node>, /*MethodSignature*/
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Id<Node /*PropertyName*/>,
        question_token: Option<Id<Node /*QuestionToken*/>>,
        type_parameters: Option<Gc<NodeArray /*<TypeParameterDeclaration>*/>>,
        parameters: Gc<NodeArray /*<ParameterDeclaration>*/>,
        type_: Option<Id<Node>>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_method_signature = node.as_method_signature();
            let modifiers = modifiers.map(Into::into);
            has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
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
        } {
            self.update_base_signature_declaration(
                arena,
                self.create_method_signature(
                    arena,
                    modifiers,
                    Some(name),
                    question_token,
                    type_parameters,
                    Some(parameters),
                    type_,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_method_declaration_raw<'name>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Id<Node /*AsteriskToken*/>>,
        name: impl Into<StrOrRcNode<'name>>,
        question_token: Option<Id<Node /*QuestionToken*/>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Id<Node /*TypeNode*/>>,
        body: Option<Id<Node /*Block*/>>,
    ) -> MethodDeclaration {
        let mut node = self.create_base_function_like_declaration(
            arena,
            id,
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
            propagate_child_flags(arena, node.maybe_asterisk_token())
                | propagate_child_flags(arena, node.maybe_question_token())
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
        arena: &AllArenas,
        node: Id<Node>, /*MethodDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        asterisk_token: Option<Id<Node /*AsteriskToken*/>>,
        name: Id<Node /*PropertyName*/>,
        question_token: Option<Id<Node /*QuestionToken*/>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Id<Node /*TypeNode*/>>,
        body: Option<Id<Node /*Block*/>>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_method_declaration = node.as_method_declaration();
            let decorators = decorators.map(Into::into);
            let modifiers = modifiers.map(Into::into);
            let type_parameters = type_parameters.map(Into::into);
            let parameters = parameters.into();
            has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
                || has_option_node_array_changed(
                    node.maybe_modifiers().as_deref(),
                    modifiers.as_ref(),
                )
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
        } {
            self.update_base_function_like_declaration(
                arena,
                self.create_method_declaration(
                    arena,
                    decorators,
                    modifiers,
                    asterisk_token,
                    name,
                    question_token,
                    type_parameters,
                    parameters,
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
    pub fn create_class_static_block_declaration_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        body: Id<Node /*Block*/>,
    ) -> ClassStaticBlockDeclaration {
        let node = self.create_base_generic_named_declaration(
            id,
            SyntaxKind::ClassStaticBlockDeclaration,
            decorators,
            modifiers,
            Option::<Gc<Node>>::None,
            Option::<Gc<NodeArray>>::None,
        );
        let mut node = ClassStaticBlockDeclaration::new(node, body.clone());
        node.add_transform_flags(
            propagate_child_flags(arena, Some(&*body)) | TransformFlags::ContainsClassFields,
        );
        node
    }

    pub fn update_class_static_block_declaration(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*ClassStaticBlockDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        body: Id<Node /*Block*/>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_class_static_block_declaration = node.as_class_static_block_declaration();
            let decorators = decorators.map(Into::into);
            let modifiers = modifiers.map(Into::into);
            has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
                || has_option_node_array_changed(
                    node.maybe_modifiers().as_deref(),
                    modifiers.as_ref(),
                )
                || !Gc::ptr_eq(&node_as_class_static_block_declaration.body, &body)
        } {
            self.update(
                arena,
                self.create_class_static_block_declaration(arena, decorators, modifiers, body),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_constructor_declaration_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        parameters: Option<impl Into<NodeArrayOrVec>>,
        body: Option<Id<Node /*Block*/>>,
    ) -> ConstructorDeclaration {
        let node = self.create_base_function_like_declaration(
            arena,
            id,
            SyntaxKind::Constructor,
            decorators,
            modifiers,
            Option::<Gc<Node>>::None,
            Option::<Gc<NodeArray>>::None,
            parameters,
            None,
            body,
        );
        let mut node = ConstructorDeclaration::new(node);
        node.add_transform_flags(TransformFlags::ContainsES2015);
        node
    }

    pub fn update_constructor_declaration(
        &self,
        arena: &AllArenas,
        node: Id<Node>, /*ConstructorDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        body: Option<Id<Node /*Block*/>>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_constructor_declaration = node.as_constructor_declaration();
            let decorators = decorators.map(Into::into);
            let modifiers = modifiers.map(Into::into);
            let parameters = parameters.into();
            has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
                || has_option_node_array_changed(
                    node.maybe_modifiers().as_deref(),
                    modifiers.as_ref(),
                )
                || has_node_array_changed(
                    &node_as_constructor_declaration.parameters(),
                    &parameters,
                )
                || !are_option_gcs_equal(
                    node_as_constructor_declaration.maybe_body().as_ref(),
                    body.as_ref(),
                )
        } {
            self.update_base_function_like_declaration(
                arena,
                self.create_constructor_declaration(
                    arena,
                    decorators,
                    modifiers,
                    Some(parameters),
                    body,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_get_accessor_declaration_raw<'name>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Id<Node /*TypeNode*/>>,
        body: Option<Id<Node /*Block*/>>,
    ) -> GetAccessorDeclaration {
        let node = self.create_base_function_like_declaration(
            arena,
            id,
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
        arena: &AllArenas,
        node: Id<Node>, /*GetAccessorDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Id<Node /*PropertyName*/>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Id<Node /*TypeNode*/>>,
        body: Option<Id<Node /*Block*/>>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_get_accessor_declaration = node.as_get_accessor_declaration();
            let decorators = decorators.map(Into::into);
            let modifiers = modifiers.map(Into::into);
            let parameters = parameters.into();
            has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
                || has_option_node_array_changed(
                    node.maybe_modifiers().as_deref(),
                    modifiers.as_ref(),
                )
                || !Gc::ptr_eq(&node_as_get_accessor_declaration.name(), &name)
                || has_node_array_changed(
                    &node_as_get_accessor_declaration.parameters(),
                    &parameters,
                )
                || !are_option_gcs_equal(
                    node_as_get_accessor_declaration.maybe_type().as_ref(),
                    type_.as_ref(),
                )
                || !are_option_gcs_equal(
                    node_as_get_accessor_declaration.maybe_body().as_ref(),
                    body.as_ref(),
                )
        } {
            self.update_base_function_like_declaration(
                arena,
                self.create_get_accessor_declaration(
                    arena, decorators, modifiers, name, parameters, type_, body,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_set_accessor_declaration_raw<'name>(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: impl Into<StrOrRcNode<'name>>,
        parameters: impl Into<NodeArrayOrVec>,
        body: Option<Id<Node /*Block*/>>,
    ) -> SetAccessorDeclaration {
        let node = self.create_base_function_like_declaration(
            arena,
            id,
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
        arena: &AllArenas,
        node: Id<Node>, /*SetAccessorDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Id<Node /*PropertyName*/>,
        parameters: impl Into<NodeArrayOrVec>,
        body: Option<Id<Node /*Block*/>>,
    ) -> Id<Node> {
        if {
            let node = arena.node(node);
            let node_as_set_accessor_declaration = node.as_set_accessor_declaration();
            let decorators = decorators.map(Into::into);
            let modifiers = modifiers.map(Into::into);
            let parameters = parameters.into();
            has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
                || has_option_node_array_changed(
                    node.maybe_modifiers().as_deref(),
                    modifiers.as_ref(),
                )
                || !Gc::ptr_eq(&node_as_set_accessor_declaration.name(), &name)
                || has_node_array_changed(
                    &node_as_set_accessor_declaration.parameters(),
                    &parameters,
                )
                || !are_option_gcs_equal(
                    node_as_set_accessor_declaration.maybe_body().as_ref(),
                    body.as_ref(),
                )
        } {
            self.update_base_function_like_declaration(
                arena,
                self.create_set_accessor_declaration(arena, decorators, modifiers, name, parameters, body),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_call_signature_raw(
        &self,
        arena: &AllArenas,
        id: Id<Node>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Id<Node /*TypeNode*/>>,
    ) -> CallSignatureDeclaration {
        let node = self.create_base_signature_declaration(
            arena,
            id,
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
                self.create_call_signature(type_parameters, parameters, type_),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_construct_signature_raw(
        &self,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> ConstructSignatureDeclaration {
        let node = self.create_base_signature_declaration(
            SyntaxKind::ConstructSignature,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<Node>>::None,
            type_parameters,
            Some(parameters),
            type_,
        );
        let node = ConstructSignatureDeclaration::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_construct_signature(
        &self,
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
                self.create_construct_signature(type_parameters, parameters, type_),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_index_signature_raw(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> IndexSignatureDeclaration {
        let node = self.create_base_signature_declaration(
            SyntaxKind::IndexSignature,
            decorators,
            modifiers,
            Option::<Gc<Node>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(parameters),
            type_,
        );
        let node = IndexSignatureDeclaration::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_index_signature(
        &self,
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
        ) || !matches!(
            node_as_index_signature_declaration.maybe_type().as_ref(),
            Some(node_type) if Gc::ptr_eq(
                &type_,
                node_type
            )
        ) || !has_option_node_array_changed(
            node.maybe_decorators().as_deref(),
            decorators.as_ref(),
        ) || !has_option_node_array_changed(
            node.maybe_modifiers().as_deref(),
            modifiers.as_ref(),
        ) {
            self.update_base_signature_declaration(
                self.create_index_signature(decorators, modifiers, parameters, Some(type_)),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_template_literal_type_span_raw(
        &self,
        type_: Gc<Node /*TypeNode*/>,
        literal: Gc<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> TemplateLiteralTypeSpan {
        let node = self.create_base_node(SyntaxKind::TemplateLiteralTypeSpan);
        let node = TemplateLiteralTypeSpan::new(node, type_, literal);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_template_literal_type_span(
        &self,
        node: &Node, /*TemplateLiteralTypeSpan*/
        type_: Gc<Node /*TypeNode*/>,
        literal: Gc<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> Gc<Node> {
        let node_as_template_literal_type_span = node.as_template_literal_type_span();
        if !Gc::ptr_eq(&node_as_template_literal_type_span.type_, &type_)
            || !Gc::ptr_eq(&node_as_template_literal_type_span.literal, &literal)
        {
            self.update(self.create_template_literal_type_span(type_, literal), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_keyword_type_node_raw(&self, kind: SyntaxKind) -> BaseNode {
        self.create_token_raw(kind)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_predicate_node_raw<'parameter_name>(
        &self,
        asserts_modifier: Option<Gc<Node /*AssertsKeyword*/>>,
        parameter_name: impl Into<StrOrRcNode<'parameter_name>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> TypePredicateNode {
        let node = self.create_base_node(SyntaxKind::TypePredicate);
        let node = TypePredicateNode::new(
            node,
            asserts_modifier,
            self.as_name(Some(parameter_name)).unwrap(),
            type_,
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_predicate_node(
        &self,
        node: &Node, /*TypePredicateNode*/
        asserts_modifier: Option<Gc<Node /*AssertsKeyword*/>>,
        parameter_name: Gc<Node>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> Gc<Node> {
        let node_as_type_predicate_node = node.as_type_predicate_node();
        if !are_option_gcs_equal(
            node_as_type_predicate_node.asserts_modifier.as_ref(),
            asserts_modifier.as_ref(),
        ) || !Gc::ptr_eq(&node_as_type_predicate_node.parameter_name, &parameter_name)
            || !are_option_gcs_equal(node_as_type_predicate_node.type_.as_ref(), type_.as_ref())
        {
            self.update(
                self.create_type_predicate_node(asserts_modifier, parameter_name, type_),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_reference_node_raw<'type_name>(
        &self,
        type_name: impl Into<StrOrRcNode<'type_name>>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
    ) -> TypeReferenceNode {
        let node = self.create_base_node(SyntaxKind::TypeReference);
        let node = TypeReferenceNode::new(
            node,
            self.as_name(Some(type_name)).unwrap(),
            type_arguments.and_then(|type_arguments| {
                self.parenthesizer_rules().parenthesize_type_arguments(Some(
                    self.create_node_array(Some(type_arguments), None).into(),
                ))
            }),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_reference_node(
        &self,
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
                self.create_type_reference_node(type_name, type_arguments),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_function_type_node_raw(
        &self,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> FunctionTypeNode {
        let node = self.create_base_signature_declaration(
            SyntaxKind::FunctionType,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<Node>>::None,
            type_parameters,
            Some(parameters),
            type_,
        );
        let node = FunctionTypeNode::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_function_type_node(
        &self,
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
                self.create_function_type_node(type_parameters, parameters, type_),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_constructor_type_node_raw(
        &self,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: impl Into<NodeArrayOrVec>,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> ConstructorTypeNode {
        let node = self.create_base_signature_declaration(
            SyntaxKind::ConstructorType,
            Option::<Gc<NodeArray>>::None,
            modifiers,
            Option::<Gc<Node>>::None,
            type_parameters,
            Some(parameters),
            type_,
        );
        let node = ConstructorTypeNode::new(node);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_constructor_type_node(
        &self,
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
                self.create_constructor_type_node(modifiers, type_parameters, parameters, type_),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_query_node_raw(&self, expr_name: Gc<Node /*EntityName*/>) -> TypeQueryNode {
        let node = self.create_base_node(SyntaxKind::TypeQuery);
        let node = TypeQueryNode::new(node, expr_name);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_query_node(
        &self,
        node: &Node, /*TypeQueryNode*/
        expr_name: Gc<Node /*EntityName*/>,
    ) -> Gc<Node> {
        let node_as_type_query_node = node.as_type_query_node();
        if !Gc::ptr_eq(&node_as_type_query_node.expr_name, &expr_name) {
            self.update(self.create_type_query_node(expr_name), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_type_literal_node_raw(
        &self,
        members: Option<impl Into<NodeArrayOrVec>>,
    ) -> TypeLiteralNode {
        let node = self.create_base_node(SyntaxKind::TypeLiteral);
        let node = TypeLiteralNode::new(node, self.create_node_array(members, None));
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_type_literal_node(
        &self,
        node: &Node, /*TypeLiteralNode*/
        members: Gc<NodeArray>,
    ) -> Gc<Node> {
        let node_as_type_literal_node = node.as_type_literal_node();
        if !Gc::ptr_eq(&node_as_type_literal_node.members, &members) {
            self.update(self.create_type_literal_node(Some(members)), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_array_type_node_raw(&self, element_type: Gc<Node /*TypeNode*/>) -> ArrayTypeNode {
        let node = self.create_base_node(SyntaxKind::ArrayType);
        let node = ArrayTypeNode::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_element_type_of_array_type(&element_type),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_array_type_node(
        &self,
        node: &Node, /*ArrayTypeNode*/
        element_type: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_array_type_node = node.as_array_type_node();
        if !Gc::ptr_eq(&node_as_array_type_node.element_type, &element_type) {
            self.update(self.create_array_type_node(element_type), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_tuple_type_node_raw(
        &self,
        elements: Option<impl Into<NodeArrayOrVec>>,
    ) -> TupleTypeNode {
        let node = self.create_base_node(SyntaxKind::TupleType);
        let node = TupleTypeNode::new(node, self.create_node_array(elements, None));
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_tuple_type_node(
        &self,
        node: &Node, /*TupleTypeNode*/
        elements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_tuple_type_node = node.as_tuple_type_node();
        let elements = elements.into();
        if has_node_array_changed(&node_as_tuple_type_node.elements, &elements) {
            self.update(self.create_tuple_type_node(Some(elements)), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_named_tuple_member_raw(
        &self,
        dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
        name: Gc<Node /*Identifier*/>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        type_: Gc<Node /*TypeNode*/>,
    ) -> NamedTupleMember {
        let node = self.create_base_node(SyntaxKind::NamedTupleMember);
        let node = NamedTupleMember::new(node, dot_dot_dot_token, name, question_token, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_named_tuple_member(
        &self,
        node: &Node, /*NamedTupleMember*/
        dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
        name: Gc<Node /*Identifier*/>,
        question_token: Option<Gc<Node /*QuestionToken*/>>,
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_named_tuple_member = node.as_named_tuple_member();
        if !are_option_gcs_equal(
            node_as_named_tuple_member.dot_dot_dot_token.as_ref(),
            dot_dot_dot_token.as_ref(),
        ) || !Gc::ptr_eq(&node_as_named_tuple_member.name, &name)
            || !are_option_gcs_equal(
                node_as_named_tuple_member.question_token.as_ref(),
                question_token.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_named_tuple_member.type_, &type_)
        {
            self.update(
                self.create_named_tuple_member(dot_dot_dot_token, name, question_token, type_),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_optional_type_node_raw(&self, type_: Gc<Node /*TypeNode*/>) -> OptionalTypeNode {
        let node = self.create_base_node(SyntaxKind::OptionalType);
        let node = OptionalTypeNode::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_element_type_of_array_type(&type_),
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_optional_type_node(
        &self,
        node: &Node, /*OptionalTypeNode*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_optional_type_node = node.as_optional_type_node();
        if !Gc::ptr_eq(&node_as_optional_type_node.type_, &type_) {
            self.update(self.create_optional_type_node(type_), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_rest_type_node_raw(&self, type_: Gc<Node /*TypeNode*/>) -> RestTypeNode {
        let node = self.create_base_node(SyntaxKind::RestType);
        let node = RestTypeNode::new(node, type_);
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_rest_type_node(
        &self,
        node: &Node, /*RestTypeNode*/
        type_: Gc<Node /*TypeNode*/>,
    ) -> Gc<Node> {
        let node_as_rest_type_node = node.as_rest_type_node();
        if !Gc::ptr_eq(&node_as_rest_type_node.type_, &type_) {
            self.update(self.create_rest_type_node(type_), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_union_or_intersection_type_node_raw(
        &self,
        kind: SyntaxKind, /*SyntaxKind.UnionType | SyntaxKind.IntersectionType*/
        types: impl Into<NodeArrayOrVec>, /*<TypeNode>*/
    ) -> Node {
        let node = self.create_base_node(kind);
        let types = self
            .parenthesizer_rules()
            .parenthesize_constituent_types_of_union_or_intersection_type(types.into());
        let node: Node = match kind {
            SyntaxKind::UnionType => UnionTypeNode::new(node, types).into(),
            SyntaxKind::IntersectionType => IntersectionTypeNode::new(node, types).into(),
            _ => panic!("Expected UnionType or IntersectionType"),
        };
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_union_or_intersection_type_node(
        &self,
        node: &Node, /*UnionOrIntersectionTypeNode*/
        types: Gc<NodeArray /*<TypeNode>*/>,
    ) -> Gc<Node> {
        let node_as_union_or_intersection_type = node.as_union_or_intersection_type_node();
        if !Gc::ptr_eq(&node_as_union_or_intersection_type.types(), &types) {
            self.update(
                self.create_union_or_intersection_type_node(node.kind(), types),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_union_type_node_raw(
        &self,
        types: impl Into<NodeArrayOrVec>, /*<TypeNode>*/
    ) -> Node {
        self.create_union_or_intersection_type_node_raw(SyntaxKind::UnionType, types)
    }

    pub fn update_union_type_node(
        &self,
        node: &Node, /*UnionTypeNode*/
        types: Gc<NodeArray /*<TypeNode>*/>,
    ) -> Gc<Node> {
        self.update_union_or_intersection_type_node(node, types)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_intersection_type_node_raw(
        &self,
        types: impl Into<NodeArrayOrVec>, /*<TypeNode>*/
    ) -> Node {
        self.create_union_or_intersection_type_node_raw(SyntaxKind::IntersectionType, types)
    }

    pub fn update_intersection_type_node(
        &self,
        node: &Node, /*IntersectionTypeNode*/
        types: Gc<NodeArray /*<TypeNode>*/>,
    ) -> Gc<Node> {
        self.update_union_or_intersection_type_node(node, types)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_conditional_type_node_raw(
        &self,
        check_type: Gc<Node /*TypeNode*/>,
        extends_type: Gc<Node /*TypeNode*/>,
        true_type: Gc<Node /*TypeNode*/>,
        false_type: Gc<Node /*TypeNode*/>,
    ) -> ConditionalTypeNode {
        let node = self.create_base_node(SyntaxKind::ConditionalType);
        let node = ConditionalTypeNode::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_member_of_conditional_type(&check_type),
            self.parenthesizer_rules()
                .parenthesize_member_of_conditional_type(&extends_type),
            true_type,
            false_type,
        );
        node.add_transform_flags(TransformFlags::ContainsTypeScript);
        node
    }

    pub fn update_conditional_type_node(
        &self,
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
                self.create_conditional_type_node(check_type, extends_type, true_type, false_type),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }
}
