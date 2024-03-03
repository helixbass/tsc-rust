use std::{
    any::Any,
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashMap,
    io,
};

use bitflags::bitflags;
use id_arena::Id;
use local_macros::enum_unwrapped;

use crate::{
    chain_bundle, get_emit_script_target, get_use_define_for_class_fields, is_expression,
    is_private_identifier, is_statement, visit_each_child, visit_node, Debug_, EmitHint,
    EmitResolver, Node, NodeArray, NodeExt, NodeFactory, NodeId, NodeInterface, NonEmpty,
    ScriptTarget, SingleNodeOrVecNode, SyntaxKind, TransformFlags, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    UnderscoreEscapedMap, VecExt, VisitResult, _d, downcast_transformer_ref, get_emit_flags,
    get_original_node, impl_has_arena, is_arrow_function, is_get_accessor, is_identifier,
    is_modifier, is_set_accessor, is_simple_inlineable_expression, is_static_modifier,
    is_super_property, maybe_filter, maybe_visit_nodes, move_range_pos, ref_mut_unwrapped,
    ref_unwrapped, released, set_comment_range, visit_function_body, visit_parameter_list,
    AllArenas, AsDoubleDeref, CoreTransformationContext, EmitFlags, HasArena,
    HasInitializerInterface, InArena, NamedDeclarationInterface, NodeCheckFlags, OptionInArena,
    ReadonlyTextRangeConcrete, TransformNodesTransformationResult,
};

bitflags! {
    #[derive(Default)]
    pub(super) struct ClassPropertySubstitutionFlags: u32 {
        const None = 0;
        const ClassAliases = 1 << 0;
        const ClassStaticThisOrSuperReference = 1 << 1;
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PrivateIdentifierKind {
    Field,    /*= "f"*/
    Method,   /*= "m"*/
    Accessor, /*= "a"*/
}

impl AsRef<str> for PrivateIdentifierKind {
    fn as_ref(&self) -> &str {
        match self {
            Self::Field => "f",
            Self::Method => "m",
            Self::Accessor => "a",
        }
    }
}

pub(super) trait PrivateIdentifierInfoInterface {
    fn brand_check_identifier(&self) -> Id<Node /*Identifier*/>;
    fn is_static(&self) -> bool;
    fn is_valid(&self) -> bool;
    fn kind(&self) -> PrivateIdentifierKind;
    fn maybe_variable_name(&self) -> Option<Id<Node>>;
}

pub struct PrivateIdentifierAccessorInfo {
    brand_check_identifier: Id<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Accessor*/
    pub getter_name: Option<Id<Node /*Identifier*/>>,
    pub setter_name: Option<Id<Node /*Identifier*/>>,
}

impl PrivateIdentifierAccessorInfo {
    pub fn new(
        brand_check_identifier: Id<Node /*Identifier*/>,
        is_static: bool,
        is_valid: bool,
        getter_name: Option<Id<Node /*Identifier*/>>,
        setter_name: Option<Id<Node /*Identifier*/>>,
    ) -> Self {
        Self {
            is_static,
            kind: PrivateIdentifierKind::Accessor,
            brand_check_identifier,
            is_valid,
            getter_name,
            setter_name,
        }
    }
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierAccessorInfo {
    fn brand_check_identifier(&self) -> Id<Node /*Identifier*/> {
        self.brand_check_identifier.clone()
    }

    fn is_static(&self) -> bool {
        self.is_static
    }

    fn is_valid(&self) -> bool {
        self.is_valid
    }

    fn kind(&self) -> PrivateIdentifierKind {
        self.kind
    }

    fn maybe_variable_name(&self) -> Option<Id<Node>> {
        None
    }
}

pub struct PrivateIdentifierMethodInfo {
    brand_check_identifier: Id<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Method*/
    pub method_name: Id<Node /*Identifier*/>,
}

impl PrivateIdentifierMethodInfo {
    pub fn new(
        brand_check_identifier: Id<Node /*Identifier*/>,
        is_static: bool,
        is_valid: bool,
        method_name: Id<Node /*Identifier*/>,
    ) -> Self {
        Self {
            is_static,
            kind: PrivateIdentifierKind::Method,
            brand_check_identifier,
            is_valid,
            method_name,
        }
    }
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierMethodInfo {
    fn brand_check_identifier(&self) -> Id<Node /*Identifier*/> {
        self.brand_check_identifier.clone()
    }

    fn is_static(&self) -> bool {
        self.is_static
    }

    fn is_valid(&self) -> bool {
        self.is_valid
    }

    fn kind(&self) -> PrivateIdentifierKind {
        self.kind
    }

    fn maybe_variable_name(&self) -> Option<Id<Node>> {
        None
    }
}

pub struct PrivateIdentifierInstanceFieldInfo {
    brand_check_identifier: Id<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Field*/
}

impl PrivateIdentifierInstanceFieldInfo {
    pub fn new(brand_check_identifier: Id<Node /*Identifier*/>, is_valid: bool) -> Self {
        Self {
            is_static: false,
            kind: PrivateIdentifierKind::Field,
            brand_check_identifier,
            is_valid,
        }
    }
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierInstanceFieldInfo {
    fn brand_check_identifier(&self) -> Id<Node /*Identifier*/> {
        self.brand_check_identifier.clone()
    }

    fn is_static(&self) -> bool {
        self.is_static
    }

    fn is_valid(&self) -> bool {
        self.is_valid
    }

    fn kind(&self) -> PrivateIdentifierKind {
        self.kind
    }

    fn maybe_variable_name(&self) -> Option<Id<Node>> {
        None
    }
}

pub struct PrivateIdentifierStaticFieldInfo {
    brand_check_identifier: Id<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Field*/
    pub variable_name: Id<Node /*Identifier*/>,
}

impl PrivateIdentifierStaticFieldInfo {
    pub fn new(
        brand_check_identifier: Id<Node /*Identifier*/>,
        is_valid: bool,
        variable_name: Id<Node /*Identifier*/>,
    ) -> Self {
        Self {
            is_static: true,
            kind: PrivateIdentifierKind::Field,
            brand_check_identifier,
            is_valid,
            variable_name,
        }
    }
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierStaticFieldInfo {
    fn brand_check_identifier(&self) -> Id<Node /*Identifier*/> {
        self.brand_check_identifier.clone()
    }

    fn is_static(&self) -> bool {
        self.is_static
    }

    fn is_valid(&self) -> bool {
        self.is_valid
    }

    fn kind(&self) -> PrivateIdentifierKind {
        self.kind
    }

    fn maybe_variable_name(&self) -> Option<Id<Node>> {
        Some(self.variable_name.clone())
    }
}

pub enum PrivateIdentifierInfo {
    PrivateIdentifierMethodInfo(PrivateIdentifierMethodInfo),
    PrivateIdentifierInstanceFieldInfo(PrivateIdentifierInstanceFieldInfo),
    PrivateIdentifierStaticFieldInfo(PrivateIdentifierStaticFieldInfo),
    PrivateIdentifierAccessorInfo(PrivateIdentifierAccessorInfo),
}

impl PrivateIdentifierInfo {
    pub(super) fn as_private_identifier_method_info(&self) -> &PrivateIdentifierMethodInfo {
        enum_unwrapped!(self, [PrivateIdentifierInfo, PrivateIdentifierMethodInfo])
    }

    pub(super) fn as_private_identifier_accessor_info(&self) -> &PrivateIdentifierAccessorInfo {
        enum_unwrapped!(self, [PrivateIdentifierInfo, PrivateIdentifierAccessorInfo])
    }

    pub(super) fn as_private_identifier_accessor_info_mut(
        &mut self,
    ) -> &mut PrivateIdentifierAccessorInfo {
        enum_unwrapped!(self, [PrivateIdentifierInfo, PrivateIdentifierAccessorInfo])
    }
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierInfo {
    fn brand_check_identifier(&self) -> Id<Node /*Identifier*/> {
        match self {
            Self::PrivateIdentifierMethodInfo(value) => value.brand_check_identifier(),
            Self::PrivateIdentifierInstanceFieldInfo(value) => value.brand_check_identifier(),
            Self::PrivateIdentifierStaticFieldInfo(value) => value.brand_check_identifier(),
            Self::PrivateIdentifierAccessorInfo(value) => value.brand_check_identifier(),
        }
    }

    fn is_static(&self) -> bool {
        match self {
            Self::PrivateIdentifierMethodInfo(value) => value.is_static(),
            Self::PrivateIdentifierInstanceFieldInfo(value) => value.is_static(),
            Self::PrivateIdentifierStaticFieldInfo(value) => value.is_static(),
            Self::PrivateIdentifierAccessorInfo(value) => value.is_static(),
        }
    }

    fn is_valid(&self) -> bool {
        match self {
            Self::PrivateIdentifierMethodInfo(value) => value.is_valid(),
            Self::PrivateIdentifierInstanceFieldInfo(value) => value.is_valid(),
            Self::PrivateIdentifierStaticFieldInfo(value) => value.is_valid(),
            Self::PrivateIdentifierAccessorInfo(value) => value.is_valid(),
        }
    }

    fn kind(&self) -> PrivateIdentifierKind {
        match self {
            Self::PrivateIdentifierMethodInfo(value) => value.kind(),
            Self::PrivateIdentifierInstanceFieldInfo(value) => value.kind(),
            Self::PrivateIdentifierStaticFieldInfo(value) => value.kind(),
            Self::PrivateIdentifierAccessorInfo(value) => value.kind(),
        }
    }

    fn maybe_variable_name(&self) -> Option<Id<Node>> {
        match self {
            Self::PrivateIdentifierMethodInfo(value) => value.maybe_variable_name(),
            Self::PrivateIdentifierInstanceFieldInfo(value) => value.maybe_variable_name(),
            Self::PrivateIdentifierStaticFieldInfo(value) => value.maybe_variable_name(),
            Self::PrivateIdentifierAccessorInfo(value) => value.maybe_variable_name(),
        }
    }
}

impl From<PrivateIdentifierMethodInfo> for PrivateIdentifierInfo {
    fn from(value: PrivateIdentifierMethodInfo) -> Self {
        Self::PrivateIdentifierMethodInfo(value)
    }
}

impl From<PrivateIdentifierInstanceFieldInfo> for PrivateIdentifierInfo {
    fn from(value: PrivateIdentifierInstanceFieldInfo) -> Self {
        Self::PrivateIdentifierInstanceFieldInfo(value)
    }
}

impl From<PrivateIdentifierStaticFieldInfo> for PrivateIdentifierInfo {
    fn from(value: PrivateIdentifierStaticFieldInfo) -> Self {
        Self::PrivateIdentifierStaticFieldInfo(value)
    }
}

impl From<PrivateIdentifierAccessorInfo> for PrivateIdentifierInfo {
    fn from(value: PrivateIdentifierAccessorInfo) -> Self {
        Self::PrivateIdentifierAccessorInfo(value)
    }
}

#[derive(Default)]
pub struct PrivateIdentifierEnvironment {
    pub class_name: String,
    pub weak_set_name: Option<Id<Node /*Identifier*/>>,
    pub identifiers: UnderscoreEscapedMap<Id<PrivateIdentifierInfo>>,
}

#[derive(Default)]
pub struct ClassLexicalEnvironment {
    pub(super) facts: ClassFacts,
    pub class_constructor: Option<Id<Node /*Identifier*/>>,
    pub super_class_reference: Option<Id<Node /*Identifier*/>>,
    pub private_identifier_environment: Option<Id<PrivateIdentifierEnvironment>>,
}

bitflags! {
    #[derive(Default)]
    pub(super) struct ClassFacts: u32 {
        const None = 0;
        const ClassWasDecorated = 1 << 0;
        const NeedsClassConstructorReference = 1 << 1;
        const NeedsClassSuperReference = 1 << 2;
        const NeedsSubstitutionForThisInClassStaticField = 1 << 3;
    }
}

pub(super) struct TransformClassFields {
    pub(super) arena: *const AllArenas,
    pub(super) context: Id<TransformNodesTransformationResult>,
    pub(super) factory: Id<NodeFactory>,
    pub(super) resolver: Id<Box<dyn EmitResolver>>,
    pub(super) language_version: ScriptTarget,
    pub(super) use_define_for_class_fields: bool,
    pub(super) should_transform_private_elements_or_class_static_blocks: bool,
    pub(super) should_transform_super_in_static_initializers: bool,
    pub(super) should_transform_this_in_static_initializers: bool,
    pub(super) enabled_substitutions: Cell<Option<ClassPropertySubstitutionFlags>>,
    pub(super) class_aliases: RefCell<Option<HashMap<NodeId, Id<Node /*Identifier*/>>>>,
    pub(super) pending_expressions: RefCell<Option<Vec<Id<Node /*Expression*/>>>>,
    pub(super) pending_statements: RefCell<Option<Vec<Id<Node /*Statement*/>>>>,
    pub(super) class_lexical_environment_stack: RefCell<Vec<Option<Id<ClassLexicalEnvironment>>>>,
    pub(super) class_lexical_environment_map: RefCell<HashMap<NodeId, Id<ClassLexicalEnvironment>>>,
    pub(super) current_class_lexical_environment: Cell<Option<Id<ClassLexicalEnvironment>>>,
    pub(super) current_computed_property_name_class_lexical_environment:
        Cell<Option<Id<ClassLexicalEnvironment>>>,
    pub(super) current_static_property_declaration_or_static_block:
        Cell<Option<Id<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>>>,
}

impl TransformClassFields {
    fn new(
        context: Id<TransformNodesTransformationResult>,
        arena: *const AllArenas,
    ) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        let compiler_options = context_ref.get_compiler_options();
        let language_version = get_emit_script_target(&compiler_options.ref_(arena_ref));
        let use_define_for_class_fields =
            get_use_define_for_class_fields(&compiler_options.ref_(arena_ref));
        let ret = arena_ref.alloc_transformer(Box::new(Self {
            arena,
            factory: context_ref.factory(),
            resolver: context_ref.get_emit_resolver(),
            context: context.clone(),
            use_define_for_class_fields,
            should_transform_private_elements_or_class_static_blocks: language_version
                < ScriptTarget::ESNext,
            should_transform_super_in_static_initializers: (language_version
                <= ScriptTarget::ES2021
                || !use_define_for_class_fields)
                && language_version >= ScriptTarget::ES2015,
            should_transform_this_in_static_initializers: language_version <= ScriptTarget::ES2021
                || !use_define_for_class_fields,
            language_version,
            enabled_substitutions: Default::default(),
            class_aliases: Default::default(),
            pending_expressions: Default::default(),
            pending_statements: Default::default(),
            class_lexical_environment_stack: Default::default(),
            class_lexical_environment_map: Default::default(),
            current_class_lexical_environment: Default::default(),
            current_computed_property_name_class_lexical_environment: Default::default(),
            current_static_property_declaration_or_static_block: Default::default(),
        }));
        context_ref.override_on_emit_node(&mut |previous_on_emit_node| {
            arena_ref.alloc_transformation_context_on_emit_node_overrider(Box::new(
                TransformClassFieldsOnEmitNodeOverrider::new(ret, previous_on_emit_node, arena_ref),
            ))
        });
        context_ref.override_on_substitute_node(&mut |previous_on_substitute_node| {
            arena_ref.alloc_transformation_context_on_substitute_node_overrider(Box::new(
                TransformClassFieldsOnSubstituteNodeOverrider::new(
                    ret,
                    previous_on_substitute_node,
                    arena_ref,
                ),
            ))
        });
        ret
    }

    pub(super) fn maybe_enabled_substitutions(&self) -> Option<ClassPropertySubstitutionFlags> {
        self.enabled_substitutions.get()
    }

    pub(super) fn set_enabled_substitutions(
        &self,
        enabled_substitutions: Option<ClassPropertySubstitutionFlags>,
    ) {
        self.enabled_substitutions.set(enabled_substitutions);
    }

    pub(super) fn class_aliases(&self) -> Ref<HashMap<NodeId, Id<Node /*Identifier*/>>> {
        ref_unwrapped(&self.class_aliases)
    }

    pub(super) fn class_aliases_mut(&self) -> RefMut<HashMap<NodeId, Id<Node /*Identifier*/>>> {
        ref_mut_unwrapped(&self.class_aliases)
    }

    pub(super) fn set_class_aliases(
        &self,
        class_aliases: Option<HashMap<NodeId, Id<Node /*Identifier*/>>>,
    ) {
        *self.class_aliases.borrow_mut() = class_aliases;
    }

    pub(super) fn maybe_pending_expressions(&self) -> Ref<Option<Vec<Id<Node /*Expression*/>>>> {
        self.pending_expressions.borrow()
    }

    pub(super) fn pending_expressions(&self) -> Ref<Vec<Id<Node /*Expression*/>>> {
        ref_unwrapped(&self.pending_expressions)
    }

    pub(super) fn maybe_pending_expressions_mut(
        &self,
    ) -> RefMut<Option<Vec<Id<Node /*Expression*/>>>> {
        self.pending_expressions.borrow_mut()
    }

    pub(super) fn pending_expressions_mut(&self) -> RefMut<Vec<Id<Node /*Expression*/>>> {
        ref_mut_unwrapped(&self.pending_expressions)
    }

    pub(super) fn set_pending_expressions(
        &self,
        pending_expressions: Option<Vec<Id<Node /*Expression*/>>>,
    ) {
        *self.pending_expressions.borrow_mut() = pending_expressions;
    }

    pub(super) fn maybe_pending_statements(&self) -> Ref<Option<Vec<Id<Node /*Statement*/>>>> {
        self.pending_statements.borrow()
    }

    pub(super) fn pending_statements(&self) -> Ref<Vec<Id<Node /*Statement*/>>> {
        ref_unwrapped(&self.pending_statements)
    }

    pub(super) fn pending_statements_mut(&self) -> RefMut<Vec<Id<Node /*Statement*/>>> {
        ref_mut_unwrapped(&self.pending_statements)
    }

    pub(super) fn set_pending_statements(
        &self,
        pending_statements: Option<Vec<Id<Node /*Statement*/>>>,
    ) {
        *self.pending_statements.borrow_mut() = pending_statements;
    }

    pub(super) fn class_lexical_environment_stack(
        &self,
    ) -> Ref<Vec<Option<Id<ClassLexicalEnvironment>>>> {
        self.class_lexical_environment_stack.borrow()
    }

    pub(super) fn class_lexical_environment_stack_mut(
        &self,
    ) -> RefMut<Vec<Option<Id<ClassLexicalEnvironment>>>> {
        self.class_lexical_environment_stack.borrow_mut()
    }

    pub(super) fn class_lexical_environment_map(
        &self,
    ) -> Ref<HashMap<NodeId, Id<ClassLexicalEnvironment>>> {
        self.class_lexical_environment_map.borrow()
    }

    pub(super) fn class_lexical_environment_map_mut(
        &self,
    ) -> RefMut<HashMap<NodeId, Id<ClassLexicalEnvironment>>> {
        self.class_lexical_environment_map.borrow_mut()
    }

    pub(super) fn maybe_current_class_lexical_environment(
        &self,
    ) -> Option<Id<ClassLexicalEnvironment>> {
        self.current_class_lexical_environment.get()
    }

    pub(super) fn set_current_class_lexical_environment(
        &self,
        current_class_lexical_environment: Option<Id<ClassLexicalEnvironment>>,
    ) {
        self.current_class_lexical_environment
            .set(current_class_lexical_environment);
    }

    pub(super) fn maybe_current_computed_property_name_class_lexical_environment(
        &self,
    ) -> Option<Id<ClassLexicalEnvironment>> {
        self.current_computed_property_name_class_lexical_environment
            .get()
    }

    pub(super) fn set_current_computed_property_name_class_lexical_environment(
        &self,
        current_computed_property_name_class_lexical_environment: Option<
            Id<ClassLexicalEnvironment>,
        >,
    ) {
        self.current_computed_property_name_class_lexical_environment
            .set(current_computed_property_name_class_lexical_environment);
    }

    pub(super) fn maybe_current_static_property_declaration_or_static_block(
        &self,
    ) -> Option<Id<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>> {
        self.current_static_property_declaration_or_static_block
            .get()
    }

    pub(super) fn set_current_static_property_declaration_or_static_block(
        &self,
        current_static_property_declaration_or_static_block: Option<
            Id<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>,
        >,
    ) {
        self.current_static_property_declaration_or_static_block
            .set(current_static_property_declaration_or_static_block);
    }

    pub(super) fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        let options = self.context.ref_(self).get_compiler_options();
        if node.ref_(self).as_source_file().is_declaration_file()
            || self.use_define_for_class_fields
                && get_emit_script_target(&options.ref_(self)) == ScriptTarget::ESNext
        {
            return node;
        }
        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
        .add_emit_helpers(self.context.ref_(self).read_emit_helpers().as_deref(), self)
    }

    pub(super) fn visitor_worker(&self, node: Id<Node>, value_is_discarded: bool) -> VisitResult /*<Node>*/
    {
        if node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsClassFields)
        {
            match released!(node.ref_(self).kind()) {
                SyntaxKind::ClassExpression | SyntaxKind::ClassDeclaration => {
                    return self.visit_class_like(node);
                }
                SyntaxKind::PropertyDeclaration => {
                    return self.visit_property_declaration(node);
                }
                SyntaxKind::VariableStatement => {
                    return self.visit_variable_statement(node);
                }
                SyntaxKind::PrivateIdentifier => {
                    return self.visit_private_identifier(node);
                }
                SyntaxKind::ClassStaticBlockDeclaration => {
                    return self.visit_class_static_block_declaration(node);
                }
                _ => (),
            }
        }
        if node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsClassFields)
            || node
                .ref_(self)
                .transform_flags()
                .intersects(TransformFlags::ContainsLexicalSuper)
                && self.should_transform_super_in_static_initializers
                && self
                    .maybe_current_static_property_declaration_or_static_block()
                    .is_some()
                && self.maybe_current_class_lexical_environment().is_some()
        {
            match released!(node.ref_(self).kind()) {
                SyntaxKind::PrefixUnaryExpression | SyntaxKind::PostfixUnaryExpression => {
                    return self.visit_pre_or_postfix_unary_expression(node, value_is_discarded);
                }
                SyntaxKind::BinaryExpression => {
                    return self.visit_binary_expression(node, value_is_discarded);
                }
                SyntaxKind::CallExpression => {
                    return self.visit_call_expression(node);
                }
                SyntaxKind::TaggedTemplateExpression => {
                    return self.visit_tagged_template_expression(node);
                }
                SyntaxKind::PropertyAccessExpression => {
                    return self.visit_property_access_expression(node);
                }
                SyntaxKind::ElementAccessExpression => {
                    return self.visit_element_access_expression(node);
                }
                SyntaxKind::ExpressionStatement => {
                    return self.visit_expression_statement(node);
                }
                SyntaxKind::ForStatement => {
                    return self.visit_for_statement(node);
                }
                SyntaxKind::FunctionDeclaration
                | SyntaxKind::FunctionExpression
                | SyntaxKind::Constructor
                | SyntaxKind::MethodDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor => {
                    let saved_current_static_property_declaration_or_static_block =
                        self.maybe_current_static_property_declaration_or_static_block();
                    self.set_current_static_property_declaration_or_static_block(None);
                    let result = visit_each_child(
                        node,
                        |node: Id<Node>| self.visitor(node),
                        &*self.context.ref_(self),
                        self,
                    );
                    self.set_current_static_property_declaration_or_static_block(
                        saved_current_static_property_declaration_or_static_block,
                    );
                    return Some(result.into());
                }
                _ => (),
            }
        }
        Some(
            visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .into(),
        )
    }

    pub(super) fn discarded_value_visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, true)
    }

    pub(super) fn visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, false)
    }

    pub(super) fn heritage_clause_visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        match released!(node.ref_(self).kind()) {
            SyntaxKind::HeritageClause => {
                return Some(
                    visit_each_child(
                        node,
                        |node: Id<Node>| self.heritage_clause_visitor(node),
                        &*self.context.ref_(self),
                        self,
                    )
                    .into(),
                );
            }
            SyntaxKind::ExpressionWithTypeArguments => {
                return self.visit_expression_with_type_arguments(node);
            }
            _ => (),
        }
        self.visitor(node)
    }

    pub(super) fn visitor_destructuring_target(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        match node.ref_(self).kind() {
            SyntaxKind::ObjectLiteralExpression | SyntaxKind::ArrayLiteralExpression => {
                self.visit_assignment_pattern(node)
            }
            _ => self.visitor(node),
        }
    }

    pub(super) fn visit_private_identifier(
        &self,
        node: Id<Node>, /*PrivateIdentifier*/
    ) -> VisitResult {
        if !self.should_transform_private_elements_or_class_static_blocks {
            return Some(node.into());
        }
        if is_statement(node.ref_(self).parent(), self) {
            return Some(node.into());
        }
        Some(
            self.factory
                .ref_(self)
                .create_identifier("")
                .set_original_node(Some(node), self)
                .into(),
        )
    }

    pub(super) fn visit_private_identifier_in_in_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        if !self.should_transform_private_elements_or_class_static_blocks {
            return Some(node.into());
        }
        let priv_id = node_as_binary_expression.left;
        Debug_.assert_node(
            Some(priv_id),
            Some(|node: Id<Node>| is_private_identifier(&node.ref_(self))),
            None,
        );
        Debug_.assert(
            node_as_binary_expression.operator_token.ref_(self).kind() == SyntaxKind::InKeyword,
            None,
        );
        let info = self.access_private_identifier(priv_id);
        if let Some(info) = info {
            let receiver = visit_node(
                node_as_binary_expression.right,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            );

            return Some(
                self.context
                    .ref_(self)
                    .get_emit_helper_factory()
                    .ref_(self)
                    .create_class_private_field_in_helper(
                        info.ref_(self).brand_check_identifier(),
                        receiver,
                    )
                    .set_original_node(Some(node), self)
                    .into(),
            );
        }

        Some(
            visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .into(),
        )
    }

    pub(super) fn class_element_visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        match released!(node.ref_(self).kind()) {
            SyntaxKind::Constructor => None,
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor | SyntaxKind::MethodDeclaration => {
                self.visit_method_or_accessor_declaration(node)
            }
            SyntaxKind::PropertyDeclaration => self.visit_property_declaration(node),
            SyntaxKind::ComputedPropertyName => self.visit_computed_property_name(node),
            SyntaxKind::SemicolonClassElement => Some(node.into()),
            _ => self.visitor(node),
        }
    }

    pub(super) fn visit_variable_statement(
        &self,
        node: Id<Node>, /*VariableStatement*/
    ) -> VisitResult {
        let saved_pending_statements = self.maybe_pending_statements().clone();
        self.set_pending_statements(Some(_d()));

        let visited_node = visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        );
        let statement: SingleNodeOrVecNode =
            if self.maybe_pending_statements().as_ref().is_non_empty() {
                vec![visited_node]
                    .and_extend(self.pending_statements().iter().cloned())
                    .into()
            } else {
                visited_node.into()
            };

        self.set_pending_statements(saved_pending_statements);
        Some(statement)
    }

    pub(super) fn visit_computed_property_name(
        &self,
        name: Id<Node>, /*ComputedPropertyName*/
    ) -> VisitResult {
        let name_ref = name.ref_(self);
        let name_as_computed_property_name = name_ref.as_computed_property_name();
        let mut node = visit_each_child(
            name,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        );
        if self.maybe_pending_expressions().as_ref().is_non_empty() {
            let mut expressions = self.pending_expressions().clone();
            expressions.push(name_as_computed_property_name.expression.clone());
            self.set_pending_expressions(Some(_d()));
            node = self.factory.ref_(self).update_computed_property_name(
                node,
                self.factory.ref_(self).inline_expressions(&expressions),
            );
        }
        Some(node.into())
    }

    pub(super) fn visit_method_or_accessor_declaration(
        &self,
        node: Id<Node>, /*MethodDeclaration | AccessorDeclaration*/
    ) -> VisitResult {
        Debug_.assert(
            !node
                .ref_(self)
                .maybe_decorators()
                .refed(self)
                .as_double_deref()
                .is_non_empty(),
            None,
        );

        let node_name = node.ref_(self).as_function_like_declaration().name();
        if !self.should_transform_private_elements_or_class_static_blocks
            || !is_private_identifier(&node_name.ref_(self))
        {
            return Some(
                visit_each_child(
                    node,
                    |node: Id<Node>| self.class_element_visitor(node),
                    &*self.context.ref_(self),
                    self,
                )
                .into(),
            );
        }

        let info = self.access_private_identifier(node_name);
        Debug_.assert(
            info.is_some(),
            Some("Undeclared private name for property declaration."),
        );
        let info = info.unwrap();
        if !info.ref_(self).is_valid() {
            return Some(node.into());
        }

        let function_name = self.get_hoisted_function_name(node);
        if let Some(function_name) = function_name {
            self.get_pending_expressions().push(
                self.factory.ref_(self).create_assignment(
                    function_name.clone(),
                    self.factory.ref_(self).create_function_expression(
                        maybe_filter(
                            released!(node.ref_(self).maybe_modifiers())
                                .refed(self)
                                .as_double_deref(),
                            |m: &Id<Node>| !is_static_modifier(&m.ref_(self)),
                        ),
                        node.ref_(self)
                            .as_function_like_declaration()
                            .maybe_asterisk_token(),
                        Some(function_name),
                        Option::<Id<NodeArray>>::None,
                        visit_parameter_list(
                            Some(node.ref_(self).as_function_like_declaration().parameters()),
                            |node: Id<Node>| self.class_element_visitor(node),
                            &*self.context.ref_(self),
                            self,
                        ),
                        None,
                        visit_function_body(
                            Some(
                                node.ref_(self)
                                    .as_function_like_declaration()
                                    .maybe_body()
                                    .unwrap(),
                            ),
                            |node: Id<Node>| self.class_element_visitor(node),
                            &*self.context.ref_(self),
                            self,
                        )
                        .unwrap(),
                    ),
                ),
            );
        }

        None
    }

    pub(super) fn get_hoisted_function_name(
        &self,
        node: Id<Node>, /*MethodDeclaration | AccessorDeclaration*/
    ) -> Option<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_function_like_declaration = node_ref.as_function_like_declaration();
        let node_name = node_as_function_like_declaration.name();
        Debug_.assert(is_private_identifier(&node_name.ref_(self)), None);
        let info = self.access_private_identifier(node_name);
        Debug_.assert(
            info.is_some(),
            Some("Undeclared private name for property declaration."),
        );
        let info = info.unwrap();
        let info = info.ref_(self);

        if info.kind() == PrivateIdentifierKind::Method {
            return Some(info.as_private_identifier_method_info().method_name.clone());
        }

        if info.kind() == PrivateIdentifierKind::Accessor {
            if is_get_accessor(&node.ref_(self)) {
                return info
                    .as_private_identifier_accessor_info()
                    .getter_name
                    .clone();
            }
            if is_set_accessor(&node.ref_(self)) {
                return info
                    .as_private_identifier_accessor_info()
                    .setter_name
                    .clone();
            }
        }
        None
    }

    pub(super) fn visit_property_declaration(
        &self,
        node: Id<Node>, /*PropertyDeclaration*/
    ) -> VisitResult {
        Debug_.assert(
            !node
                .ref_(self)
                .maybe_decorators()
                .refed(self)
                .as_double_deref()
                .is_non_empty(),
            None,
        );

        if is_private_identifier(&node.ref_(self).as_property_declaration().name().ref_(self)) {
            if !self.should_transform_private_elements_or_class_static_blocks {
                return Some(
                    self.factory
                        .ref_(self)
                        .update_property_declaration(
                            node,
                            Option::<Id<NodeArray>>::None,
                            maybe_visit_nodes(
                                released!(node.ref_(self).maybe_modifiers()),
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                                None,
                                None,
                                self,
                            ),
                            released!(node.ref_(self).as_property_declaration().name()),
                            None,
                            None,
                            None,
                        )
                        .into(),
                );
            }

            let info =
                self.access_private_identifier(node.ref_(self).as_property_declaration().name());
            Debug_.assert(
                info.is_some(),
                Some("Undeclared private name for property declaration."),
            );
            let info = info.unwrap();
            if !info.ref_(self).is_valid() {
                return Some(node.into());
            }
        }
        let expr = self.get_property_name_expression_if_needed(
            released!(node.ref_(self).as_property_declaration().name()),
            released!(
                node.ref_(self)
                    .as_property_declaration()
                    .maybe_initializer()
                    .is_some()
                    || self.use_define_for_class_fields
            ),
        );
        if let Some(expr) = expr.filter(|expr| !is_simple_inlineable_expression(&expr.ref_(self))) {
            self.get_pending_expressions().push(expr);
        }
        None
    }

    pub(super) fn create_private_identifier_access(
        &self,
        info: &PrivateIdentifierInfo,
        receiver: Id<Node>, /*Expression*/
    ) -> Id<Node /*Expression*/> {
        self.create_private_identifier_access_helper(
            info,
            visit_node(
                receiver,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
        )
    }

    pub(super) fn create_private_identifier_access_helper(
        &self,
        info: &PrivateIdentifierInfo,
        receiver: Id<Node>, /*Expression*/
    ) -> Id<Node /*Expression*/> {
        set_comment_range(
            receiver,
            &ReadonlyTextRangeConcrete::from(move_range_pos(&*receiver.ref_(self), -1)),
            self,
        );

        match info.kind() {
            PrivateIdentifierKind::Accessor => self
                .context
                .ref_(self)
                .get_emit_helper_factory()
                .ref_(self)
                .create_class_private_field_get_helper(
                    receiver,
                    info.brand_check_identifier(),
                    info.kind(),
                    info.as_private_identifier_accessor_info()
                        .getter_name
                        .clone(),
                ),
            PrivateIdentifierKind::Method => self
                .context
                .ref_(self)
                .get_emit_helper_factory()
                .ref_(self)
                .create_class_private_field_get_helper(
                    receiver,
                    info.brand_check_identifier(),
                    info.kind(),
                    Some(info.as_private_identifier_method_info().method_name.clone()),
                ),
            PrivateIdentifierKind::Field => self
                .context
                .ref_(self)
                .get_emit_helper_factory()
                .ref_(self)
                .create_class_private_field_get_helper(
                    receiver,
                    info.brand_check_identifier(),
                    info.kind(),
                    info.maybe_variable_name(),
                ),
            // default:
            //     Debug.assertNever(info, "Unknown private element type");
        }
    }

    pub(super) fn visit_property_access_expression(
        &self,
        node: Id<Node>, /*PropertyAccessExpression*/
    ) -> VisitResult {
        if self.should_transform_private_elements_or_class_static_blocks
            && is_private_identifier(
                &node
                    .ref_(self)
                    .as_property_access_expression()
                    .name()
                    .ref_(self),
            )
        {
            let private_identifier_info = self
                .access_private_identifier(node.ref_(self).as_property_access_expression().name());
            if let Some(private_identifier_info) = private_identifier_info {
                return Some(
                    self.create_private_identifier_access(
                        &private_identifier_info.ref_(self),
                        released!(node.ref_(self).as_property_access_expression().expression),
                    )
                    .set_original_node(Some(node), self)
                    .set_text_range(Some(&*node.ref_(self)), self)
                    .into(),
                );
            }
        }
        if self.should_transform_super_in_static_initializers
            && is_super_property(node, self)
            && is_identifier(
                &node
                    .ref_(self)
                    .as_property_access_expression()
                    .name()
                    .ref_(self),
            )
            && self
                .maybe_current_static_property_declaration_or_static_block()
                .is_some()
        {
            if let Some(current_class_lexical_environment) =
                self.maybe_current_class_lexical_environment()
            {
                let current_class_lexical_environment =
                    current_class_lexical_environment.ref_(self);
                let class_constructor =
                    current_class_lexical_environment.class_constructor.as_ref();
                let super_class_reference = current_class_lexical_environment
                    .super_class_reference
                    .as_ref();
                let facts = current_class_lexical_environment.facts;
                if facts.intersects(ClassFacts::ClassWasDecorated) {
                    return Some(self.visit_invalid_super_property(node).into());
                }
                if let Some(class_constructor) = class_constructor {
                    if let Some(super_class_reference) = super_class_reference {
                        return Some(
                            self.factory
                                .ref_(self)
                                .create_reflect_get_call(
                                    super_class_reference.clone(),
                                    self.factory.ref_(self).create_string_literal_from_node(
                                        released!(node
                                            .ref_(self)
                                            .as_property_access_expression()
                                            .name()),
                                    ),
                                    Some(class_constructor.clone()),
                                )
                                .set_original_node(
                                    Some(
                                        node.ref_(self).as_property_access_expression().expression,
                                    ),
                                    self,
                                )
                                .set_text_range(
                                    Some(
                                        &*node
                                            .ref_(self)
                                            .as_property_access_expression()
                                            .expression
                                            .ref_(self),
                                    ),
                                    self,
                                )
                                .into(),
                        );
                    }
                }
            }
        }
        Some(
            visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .into(),
        )
    }
}

impl TransformerInterface for TransformClassFields {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl_has_arena!(TransformClassFields);

pub(super) struct TransformClassFieldsOnEmitNodeOverrider {
    arena: *const AllArenas,
    transform_class_fields: Transformer,
    previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformClassFieldsOnEmitNodeOverrider {
    fn new(
        transform_class_fields: Transformer,
        previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_class_fields,
            previous_on_emit_node,
        }
    }

    fn transform_class_fields(&self) -> debug_cell::Ref<'_, TransformClassFields> {
        downcast_transformer_ref(self.transform_class_fields, self)
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformClassFieldsOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        let original = get_original_node(node, self);
        if let Some(original_id) = released!(original.ref_(self).maybe_id()) {
            let class_lexical_environment = self
                .transform_class_fields()
                .class_lexical_environment_map()
                .get(&original_id)
                .cloned();
            if let Some(class_lexical_environment) = class_lexical_environment {
                let saved_class_lexical_environment = self
                    .transform_class_fields()
                    .maybe_current_class_lexical_environment();
                let saved_current_computed_property_name_class_lexical_environment = self
                    .transform_class_fields()
                    .maybe_current_computed_property_name_class_lexical_environment();
                self.transform_class_fields()
                    .set_current_class_lexical_environment(Some(class_lexical_environment.clone()));
                self.transform_class_fields()
                    .set_current_computed_property_name_class_lexical_environment(Some(
                        class_lexical_environment.clone(),
                    ));
                self.previous_on_emit_node
                    .ref_(self)
                    .on_emit_node(hint, node, emit_callback)?;
                self.transform_class_fields()
                    .set_current_class_lexical_environment(saved_class_lexical_environment);
                self.transform_class_fields()
                    .set_current_computed_property_name_class_lexical_environment(
                        saved_current_computed_property_name_class_lexical_environment,
                    );
                return Ok(());
            }
        }

        match released!(node.ref_(self).kind()) {
            SyntaxKind::FunctionExpression
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::Constructor => 'arm: {
                if node.ref_(self).kind() == SyntaxKind::FunctionExpression
                    && (is_arrow_function(&original.ref_(self))
                        || get_emit_flags(node, self).intersects(EmitFlags::AsyncFunctionBody))
                {
                    break 'arm;
                }

                let saved_class_lexical_environment = self
                    .transform_class_fields()
                    .maybe_current_class_lexical_environment();
                let saved_current_computed_property_name_class_lexical_environment = self
                    .transform_class_fields()
                    .maybe_current_computed_property_name_class_lexical_environment();
                self.transform_class_fields()
                    .set_current_class_lexical_environment(None);
                self.transform_class_fields()
                    .set_current_computed_property_name_class_lexical_environment(None);
                self.previous_on_emit_node
                    .ref_(self)
                    .on_emit_node(hint, node, emit_callback)?;
                self.transform_class_fields()
                    .set_current_class_lexical_environment(saved_class_lexical_environment);
                self.transform_class_fields()
                    .set_current_computed_property_name_class_lexical_environment(
                        saved_current_computed_property_name_class_lexical_environment,
                    );
                return Ok(());
            }

            SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::PropertyDeclaration => {
                let saved_class_lexical_environment = self
                    .transform_class_fields()
                    .maybe_current_class_lexical_environment();
                let saved_current_computed_property_name_class_lexical_environment = self
                    .transform_class_fields()
                    .maybe_current_computed_property_name_class_lexical_environment();
                self.transform_class_fields()
                    .set_current_computed_property_name_class_lexical_environment(
                        self.transform_class_fields()
                            .maybe_current_class_lexical_environment(),
                    );
                self.transform_class_fields()
                    .set_current_class_lexical_environment(None);
                self.previous_on_emit_node
                    .ref_(self)
                    .on_emit_node(hint, node, emit_callback)?;
                self.transform_class_fields()
                    .set_current_class_lexical_environment(saved_class_lexical_environment);
                self.transform_class_fields()
                    .set_current_computed_property_name_class_lexical_environment(
                        saved_current_computed_property_name_class_lexical_environment,
                    );
                return Ok(());
            }
            SyntaxKind::ComputedPropertyName => {
                let saved_class_lexical_environment = self
                    .transform_class_fields()
                    .maybe_current_class_lexical_environment();
                let saved_current_computed_property_name_class_lexical_environment = self
                    .transform_class_fields()
                    .maybe_current_computed_property_name_class_lexical_environment();
                self.transform_class_fields()
                    .set_current_class_lexical_environment(
                        self.transform_class_fields()
                            .maybe_current_computed_property_name_class_lexical_environment(),
                    );
                self.transform_class_fields()
                    .set_current_computed_property_name_class_lexical_environment(None);
                self.previous_on_emit_node
                    .ref_(self)
                    .on_emit_node(hint, node, emit_callback)?;
                self.transform_class_fields()
                    .set_current_class_lexical_environment(saved_class_lexical_environment);
                self.transform_class_fields()
                    .set_current_computed_property_name_class_lexical_environment(
                        saved_current_computed_property_name_class_lexical_environment,
                    );
                return Ok(());
            }
            _ => (),
        }
        self.previous_on_emit_node
            .ref_(self)
            .on_emit_node(hint, node, emit_callback)?;

        Ok(())
    }
}

impl_has_arena!(TransformClassFieldsOnEmitNodeOverrider);

pub(super) struct TransformClassFieldsOnSubstituteNodeOverrider {
    arena: *const AllArenas,
    transform_class_fields: Transformer,
    previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformClassFieldsOnSubstituteNodeOverrider {
    pub(super) fn new(
        transform_class_fields: Transformer,
        previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_class_fields,
            previous_on_substitute_node,
        }
    }

    fn transform_class_fields(&self) -> debug_cell::Ref<'_, TransformClassFields> {
        downcast_transformer_ref(self.transform_class_fields, self)
    }

    pub(super) fn substitute_expression(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> io::Result<Id<Node>> {
        Ok(match released!(node.ref_(self).kind()) {
            SyntaxKind::Identifier => self.substitute_expression_identifier(node)?,
            SyntaxKind::ThisKeyword => self.substitute_this_expression(node),
            _ => node,
        })
    }

    pub(super) fn substitute_this_expression(
        &self,
        node: Id<Node>, /*ThisExpression*/
    ) -> Id<Node> {
        if self
            .transform_class_fields()
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ClassPropertySubstitutionFlags::ClassStaticThisOrSuperReference)
        {
            if let Some(current_class_lexical_environment) = self
                .transform_class_fields()
                .maybe_current_class_lexical_environment()
            {
                let current_class_lexical_environment =
                    current_class_lexical_environment.ref_(self);
                let facts = current_class_lexical_environment.facts;
                let class_constructor = current_class_lexical_environment.class_constructor;
                if facts.intersects(ClassFacts::ClassWasDecorated) {
                    return self
                        .transform_class_fields()
                        .factory
                        .ref_(self)
                        .create_parenthesized_expression(
                            self.transform_class_fields()
                                .factory
                                .ref_(self)
                                .create_void_zero(),
                        );
                }
                if let Some(class_constructor) = class_constructor {
                    return self
                        .transform_class_fields()
                        .factory
                        .ref_(self)
                        .clone_node(class_constructor)
                        .set_original_node(Some(node), self)
                        .set_text_range(Some(&*node.ref_(self)), self);
                }
            }
        }
        node
    }

    pub(super) fn substitute_expression_identifier(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        Ok(self.try_substitute_class_alias(node)?.unwrap_or(node))
    }

    pub(super) fn try_substitute_class_alias(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Option<Id<Node /*Expression*/>>> {
        if self
            .transform_class_fields()
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ClassPropertySubstitutionFlags::ClassAliases)
        {
            if self
                .transform_class_fields()
                .resolver
                .ref_(self)
                .get_node_check_flags(node)
                .intersects(NodeCheckFlags::ConstructorReferenceInClass)
            {
                let declaration = self
                    .transform_class_fields()
                    .resolver
                    .ref_(self)
                    .get_referenced_value_declaration(node)?;
                if let Some(declaration) = declaration {
                    let class_alias = self
                        .transform_class_fields()
                        .class_aliases()
                        .get(&declaration.ref_(self).id())
                        .cloned();
                    if let Some(class_alias) = class_alias {
                        return Ok(Some(
                            self.transform_class_fields()
                                .factory
                                .ref_(self)
                                .clone_node(class_alias)
                                .set_source_map_range(
                                    Some(self.alloc_source_map_range((&*node.ref_(self)).into())),
                                    self,
                                )
                                .set_comment_range(&*node.ref_(self), self),
                        ));
                    }
                }
            }
        }

        Ok(None)
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformClassFieldsOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        let node = self
            .previous_on_substitute_node
            .ref_(self)
            .on_substitute_node(hint, node)?;
        if hint == EmitHint::Expression {
            return self.substitute_expression(node);
        }
        Ok(node)
    }
}

impl_has_arena!(TransformClassFieldsOnSubstituteNodeOverrider);

struct TransformClassFieldsFactory {
    arena: *const AllArenas,
}

impl TransformClassFieldsFactory {
    fn new(arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
        }
    }
}

impl TransformerFactoryInterface for TransformClassFieldsFactory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle(self).ref_(self).call(
            context.clone(),
            TransformClassFields::new(context, self.arena),
        )
    }
}

impl_has_arena!(TransformClassFieldsFactory);

pub fn transform_class_fields(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformClassFieldsFactory::new(arena)))
}
