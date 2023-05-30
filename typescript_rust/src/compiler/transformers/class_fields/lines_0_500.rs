use std::{cell::Cell, collections::HashMap, io, mem};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use local_macros::enum_unwrapped;

use crate::{
    chain_bundle, get_emit_script_target, get_use_define_for_class_fields, is_expression,
    is_private_identifier, is_statement, visit_each_child, visit_node, BaseNodeFactorySynthetic,
    CompilerOptions, Debug_, EmitHint, EmitResolver, Node, NodeArray, NodeExt, NodeFactory, NodeId,
    NodeInterface, NonEmpty, ScriptTarget, SingleNodeOrVecNode, SyntaxKind, TransformFlags,
    TransformationContext, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, UnderscoreEscapedMap, VecExt, VisitResult,
    _d, gc_cell_ref_mut_unwrapped, gc_cell_ref_unwrapped, is_get_accessor, is_identifier,
    is_modifier, is_set_accessor, is_simple_inlineable_expression, is_static_modifier,
    is_super_property, maybe_filter, move_range_pos, set_comment_range, visit_function_body,
    visit_nodes, visit_parameter_list, AsDoubleDeref, HasInitializerInterface,
    NamedDeclarationInterface, ReadonlyTextRangeConcrete,
};

bitflags! {
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

pub(super) trait PrivateIdentifierInfoInterface {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/>;
    fn is_static(&self) -> bool;
    fn is_valid(&self) -> bool;
    fn kind(&self) -> PrivateIdentifierKind;
    fn maybe_variable_name(&self) -> Option<Gc<Node>>;
}

#[derive(Trace, Finalize)]
pub(super) struct PrivateIdentifierAccessorInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    #[unsafe_ignore_trace]
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Accessor*/
    pub getter_name: Option<Gc<Node /*Identifier*/>>,
    pub setter_name: Option<Gc<Node /*Identifier*/>>,
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierAccessorInfo {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/> {
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

    fn maybe_variable_name(&self) -> Option<Gc<Node>> {
        None
    }
}

#[derive(Trace, Finalize)]
pub(super) struct PrivateIdentifierMethodInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    #[unsafe_ignore_trace]
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Method*/
    pub method_name: Gc<Node /*Identifier*/>,
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierMethodInfo {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/> {
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

    fn maybe_variable_name(&self) -> Option<Gc<Node>> {
        None
    }
}

#[derive(Trace, Finalize)]
pub(super) struct PrivateIdentifierInstanceFieldInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    #[unsafe_ignore_trace]
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Field*/
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierInstanceFieldInfo {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/> {
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

    fn maybe_variable_name(&self) -> Option<Gc<Node>> {
        None
    }
}

#[derive(Trace, Finalize)]
pub(super) struct PrivateIdentifierStaticFieldInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    #[unsafe_ignore_trace]
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Field*/
    pub variable_name: Gc<Node /*Identifier*/>,
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierStaticFieldInfo {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/> {
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

    fn maybe_variable_name(&self) -> Option<Gc<Node>> {
        Some(self.variable_name.clone())
    }
}

#[derive(Trace, Finalize)]
pub(super) enum PrivateIdentifierInfo {
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
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierInfo {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/> {
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

    fn maybe_variable_name(&self) -> Option<Gc<Node>> {
        match self {
            Self::PrivateIdentifierMethodInfo(value) => value.maybe_variable_name(),
            Self::PrivateIdentifierInstanceFieldInfo(value) => value.maybe_variable_name(),
            Self::PrivateIdentifierStaticFieldInfo(value) => value.maybe_variable_name(),
            Self::PrivateIdentifierAccessorInfo(value) => value.maybe_variable_name(),
        }
    }
}

#[derive(Trace, Finalize)]
pub(super) struct PrivateIdentifierEnvironment {
    pub class_name: String,
    pub weak_set_name: Option<Gc<Node /*Identifier*/>>,
    pub identifiers: UnderscoreEscapedMap<PrivateIdentifierInfo>,
}

#[derive(Trace, Finalize)]
pub(super) struct ClassLexicalEnvironment {
    #[unsafe_ignore_trace]
    pub facts: ClassFacts,
    pub class_constructor: Option<Gc<Node /*Identifier*/>>,
    pub super_class_reference: Option<Gc<Node /*Identifier*/>>,
    pub private_identifier_environment: Option<PrivateIdentifierEnvironment>,
}

bitflags! {
    pub(super) struct ClassFacts: u32 {
        const None = 0;
        const ClassWasDecorated = 1 << 0;
        const NeedsClassConstructorReference = 1 << 1;
        const NeedsClassSuperReference = 1 << 2;
        const NeedsSubstitutionForThisInClassStaticField = 1 << 3;
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformClassFields {
    pub(super) _transformer_wrapper: GcCell<Option<Transformer>>,
    pub(super) context: Gc<Box<dyn TransformationContext>>,
    pub(super) factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    pub(super) resolver: Gc<Box<dyn EmitResolver>>,
    pub(super) compiler_options: Gc<CompilerOptions>,
    #[unsafe_ignore_trace]
    pub(super) language_version: ScriptTarget,
    pub(super) use_define_for_class_fields: bool,
    pub(super) should_transform_private_elements_or_class_static_blocks: bool,
    pub(super) should_transform_super_in_static_initializers: bool,
    pub(super) should_transform_this_in_static_initializers: bool,
    #[unsafe_ignore_trace]
    pub(super) enabled_substitutions: Cell<Option<ClassPropertySubstitutionFlags>>,
    pub(super) class_aliases: GcCell<Option<Vec<Gc<Node /*Identifier*/>>>>,
    pub(super) pending_expressions: GcCell<Option<Vec<Gc<Node /*Expression*/>>>>,
    pub(super) pending_statements: GcCell<Option<Vec<Gc<Node /*Statement*/>>>>,
    pub(super) class_lexical_environment_stack: GcCell<Vec<Option<Gc<ClassLexicalEnvironment>>>>,
    pub(super) class_lexical_environment_map: GcCell<HashMap<NodeId, Gc<ClassLexicalEnvironment>>>,
    pub(super) current_class_lexical_environment: GcCell<Option<Gc<ClassLexicalEnvironment>>>,
    pub(super) current_computed_property_name_class_lexical_environment:
        GcCell<Option<Gc<ClassLexicalEnvironment>>>,
    pub(super) current_static_property_declaration_or_static_block:
        GcCell<Option<Gc<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>>>,
}

impl TransformClassFields {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let compiler_options = context.get_compiler_options();
        let language_version = get_emit_script_target(&compiler_options);
        let use_define_for_class_fields = get_use_define_for_class_fields(&compiler_options);
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            factory: context.factory(),
            resolver: context.get_emit_resolver(),
            context: context.clone(),
            use_define_for_class_fields,
            compiler_options,
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
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformClassFieldsOnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(
                TransformClassFieldsOnSubstituteNodeOverrider::new(
                    downcasted.clone(),
                    previous_on_substitute_node,
                ),
            ))
        });
        downcasted
    }

    pub(super) fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
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

    pub(super) fn maybe_class_aliases(&self) -> GcCellRef<Option<Vec<Gc<Node /*Identifier*/>>>> {
        self.class_aliases.borrow()
    }

    pub(super) fn maybe_class_aliases_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Node /*Identifier*/>>>> {
        self.class_aliases.borrow_mut()
    }

    pub(super) fn set_class_aliases(&self, class_aliases: Option<Vec<Gc<Node /*Identifier*/>>>) {
        *self.class_aliases.borrow_mut() = class_aliases;
    }

    pub(super) fn maybe_pending_expressions(
        &self,
    ) -> GcCellRef<Option<Vec<Gc<Node /*Expression*/>>>> {
        self.pending_expressions.borrow()
    }

    pub(super) fn pending_expressions(&self) -> GcCellRef<Vec<Gc<Node /*Expression*/>>> {
        gc_cell_ref_unwrapped(&self.pending_expressions)
    }

    pub(super) fn maybe_pending_expressions_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Node /*Expression*/>>>> {
        self.pending_expressions.borrow_mut()
    }

    pub(super) fn pending_expressions_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Node /*Expression*/>>>, Vec<Gc<Node /*Expression*/>>> {
        gc_cell_ref_mut_unwrapped(&self.pending_expressions)
    }

    pub(super) fn set_pending_expressions(
        &self,
        pending_expressions: Option<Vec<Gc<Node /*Expression*/>>>,
    ) {
        *self.pending_expressions.borrow_mut() = pending_expressions;
    }

    pub(super) fn maybe_pending_statements(
        &self,
    ) -> GcCellRef<Option<Vec<Gc<Node /*Statement*/>>>> {
        self.pending_statements.borrow()
    }

    pub(super) fn pending_statements(&self) -> GcCellRef<Vec<Gc<Node /*Statement*/>>> {
        gc_cell_ref_unwrapped(&self.pending_statements)
    }

    pub(super) fn maybe_pending_statements_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Node /*Statement*/>>>> {
        self.pending_statements.borrow_mut()
    }

    pub(super) fn pending_statements_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Node /*Statement*/>>>, Vec<Gc<Node /*Statement*/>>> {
        gc_cell_ref_mut_unwrapped(&self.pending_statements)
    }

    pub(super) fn set_pending_statements(
        &self,
        pending_statements: Option<Vec<Gc<Node /*Statement*/>>>,
    ) {
        *self.pending_statements.borrow_mut() = pending_statements;
    }

    pub(super) fn class_lexical_environment_stack(
        &self,
    ) -> GcCellRef<Vec<Option<Gc<ClassLexicalEnvironment>>>> {
        self.class_lexical_environment_stack.borrow()
    }

    pub(super) fn class_lexical_environment_stack_mut(
        &self,
    ) -> GcCellRefMut<Vec<Option<Gc<ClassLexicalEnvironment>>>> {
        self.class_lexical_environment_stack.borrow_mut()
    }

    pub(super) fn set_class_lexical_environment_stack(
        &self,
        class_lexical_environment_stack: Vec<Option<Gc<ClassLexicalEnvironment>>>,
    ) {
        *self.class_lexical_environment_stack.borrow_mut() = class_lexical_environment_stack;
    }

    pub(super) fn class_lexical_environment_map(
        &self,
    ) -> GcCellRef<HashMap<NodeId, Gc<ClassLexicalEnvironment>>> {
        self.class_lexical_environment_map.borrow()
    }

    pub(super) fn class_lexical_environment_map_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, Gc<ClassLexicalEnvironment>>> {
        self.class_lexical_environment_map.borrow_mut()
    }

    pub(super) fn set_class_lexical_environment_map(
        &self,
        class_lexical_environment_map: HashMap<NodeId, Gc<ClassLexicalEnvironment>>,
    ) {
        *self.class_lexical_environment_map.borrow_mut() = class_lexical_environment_map;
    }

    pub(super) fn maybe_current_class_lexical_environment(
        &self,
    ) -> Option<Gc<ClassLexicalEnvironment>> {
        self.current_class_lexical_environment.borrow().clone()
    }

    pub(super) fn maybe_current_class_lexical_environment_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<ClassLexicalEnvironment>>> {
        self.current_class_lexical_environment.borrow_mut()
    }

    pub(super) fn set_current_class_lexical_environment(
        &self,
        current_class_lexical_environment: Option<Gc<ClassLexicalEnvironment>>,
    ) {
        *self.current_class_lexical_environment.borrow_mut() = current_class_lexical_environment;
    }

    pub(super) fn maybe_current_computed_property_name_class_lexical_environment(
        &self,
    ) -> Option<Gc<ClassLexicalEnvironment>> {
        self.current_computed_property_name_class_lexical_environment
            .borrow()
            .clone()
    }

    pub(super) fn maybe_current_computed_property_name_class_lexical_environment_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<ClassLexicalEnvironment>>> {
        self.current_computed_property_name_class_lexical_environment
            .borrow_mut()
    }

    pub(super) fn set_current_computed_property_name_class_lexical_environment(
        &self,
        current_computed_property_name_class_lexical_environment: Option<
            Gc<ClassLexicalEnvironment>,
        >,
    ) {
        *self
            .current_computed_property_name_class_lexical_environment
            .borrow_mut() = current_computed_property_name_class_lexical_environment;
    }

    pub(super) fn maybe_current_static_property_declaration_or_static_block(
        &self,
    ) -> Option<Gc<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>> {
        self.current_static_property_declaration_or_static_block
            .borrow()
            .clone()
    }

    pub(super) fn maybe_current_static_property_declaration_or_static_block_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>>> {
        self.current_static_property_declaration_or_static_block
            .borrow_mut()
    }

    pub(super) fn set_current_static_property_declaration_or_static_block(
        &self,
        current_static_property_declaration_or_static_block: Option<
            Gc<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>,
        >,
    ) {
        *self
            .current_static_property_declaration_or_static_block
            .borrow_mut() = current_static_property_declaration_or_static_block;
    }

    pub(super) fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        let ref options = self.context.get_compiler_options();
        if node_as_source_file.is_declaration_file()
            || self.use_define_for_class_fields
                && get_emit_script_target(options) == ScriptTarget::ESNext
        {
            return node.node_wrapper();
        }
        visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
            .add_emit_helpers(self.context.read_emit_helpers().as_deref())
    }

    pub(super) fn visitor_worker(&self, node: &Node, value_is_discarded: bool) -> VisitResult /*<Node>*/
    {
        if node
            .transform_flags()
            .intersects(TransformFlags::ContainsClassFields)
        {
            match node.kind() {
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
            .transform_flags()
            .intersects(TransformFlags::ContainsClassFields)
            || node
                .transform_flags()
                .intersects(TransformFlags::ContainsLexicalSuper)
                && self.should_transform_super_in_static_initializers
                && self
                    .maybe_current_static_property_declaration_or_static_block()
                    .is_some()
                && self.maybe_current_class_lexical_environment().is_some()
        {
            match node.kind() {
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
                    let result =
                        visit_each_child(node, |node: &Node| self.visitor(node), &**self.context);
                    self.set_current_static_property_declaration_or_static_block(
                        saved_current_static_property_declaration_or_static_block,
                    );
                    return Some(result.into());
                }
                _ => (),
            }
        }
        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }

    pub(super) fn discarded_value_visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, true)
    }

    pub(super) fn visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, false)
    }

    pub(super) fn heritage_clause_visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        match node.kind() {
            SyntaxKind::HeritageClause => {
                return Some(
                    visit_each_child(
                        node,
                        |node: &Node| self.heritage_clause_visitor(node),
                        &**self.context,
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

    pub(super) fn visitor_destructuring_target(&self, node: &Node) -> VisitResult /*<Node>*/ {
        match node.kind() {
            SyntaxKind::ObjectLiteralExpression | SyntaxKind::ArrayLiteralExpression => {
                self.visit_assignment_pattern(node)
            }
            _ => self.visitor(node),
        }
    }

    pub(super) fn visit_private_identifier(
        &self,
        node: &Node, /*PrivateIdentifier*/
    ) -> VisitResult {
        if !self.should_transform_private_elements_or_class_static_blocks {
            return Some(node.node_wrapper().into());
        }
        if is_statement(&node.parent()) {
            return Some(node.node_wrapper().into());
        }
        Some(
            self.factory
                .create_identifier("", Option::<Gc<NodeArray>>::None, None)
                .wrap()
                .set_original_node(Some(node.node_wrapper()))
                .into(),
        )
    }

    pub(super) fn visit_private_identifier_in_in_expression(
        &self,
        node: &Node, /*BinaryExpression*/
    ) -> VisitResult {
        let node_as_binary_expression = node.as_binary_expression();
        if !self.should_transform_private_elements_or_class_static_blocks {
            return Some(node.node_wrapper().into());
        }
        let priv_id = &node_as_binary_expression.left;
        Debug_.assert_node(Some(&**priv_id), Some(is_private_identifier), None);
        Debug_.assert(
            node_as_binary_expression.operator_token.kind() == SyntaxKind::InKeyword,
            None,
        );
        let info = self.access_private_identifier(priv_id);
        if let Some(info) = info {
            let receiver = visit_node(
                &node_as_binary_expression.right,
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            );

            return Some(
                self.context
                    .get_emit_helper_factory()
                    .create_class_private_field_in_helper(info.brand_check_identifier(), receiver)
                    .set_original_node(Some(node.node_wrapper()))
                    .into(),
            );
        }

        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }

    pub(super) fn class_element_visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        match node.kind() {
            SyntaxKind::Constructor => None,
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor | SyntaxKind::MethodDeclaration => {
                self.visit_method_or_accessor_declaration(node)
            }
            SyntaxKind::PropertyDeclaration => self.visit_property_declaration(node),
            SyntaxKind::ComputedPropertyName => self.visit_computed_property_name(node),
            SyntaxKind::SemicolonClassElement => Some(node.node_wrapper().into()),
            _ => self.visitor(node),
        }
    }

    pub(super) fn visit_variable_statement(
        &self,
        node: &Node, /*VariableStatement*/
    ) -> VisitResult {
        let saved_pending_statements = self.maybe_pending_statements().clone();
        self.set_pending_statements(Some(_d()));

        let visited_node =
            visit_each_child(node, |node: &Node| self.visitor(node), &**self.context);
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
        name: &Node, /*ComputedPropertyName*/
    ) -> VisitResult {
        let name_as_computed_property_name = name.as_computed_property_name();
        let mut node = visit_each_child(name, |node: &Node| self.visitor(node), &**self.context);
        if self.maybe_pending_expressions().as_ref().is_non_empty() {
            let mut expressions = self.pending_expressions().clone();
            expressions.push(name_as_computed_property_name.expression.clone());
            self.set_pending_expressions(Some(_d()));
            node = self.factory.update_computed_property_name(
                &node,
                self.factory.inline_expressions(&expressions),
            );
        }
        Some(node.into())
    }

    pub(super) fn visit_method_or_accessor_declaration(
        &self,
        node: &Node, /*MethodDeclaration | AccessorDeclaration*/
    ) -> VisitResult {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        Debug_.assert(
            !node.maybe_decorators().as_double_deref().is_non_empty(),
            None,
        );

        let ref node_name = node_as_function_like_declaration.name();
        if !self.should_transform_private_elements_or_class_static_blocks
            || !is_private_identifier(node_name)
        {
            return Some(
                visit_each_child(
                    node,
                    |node: &Node| self.class_element_visitor(node),
                    &**self.context,
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
        if !info.is_valid() {
            return Some(node.node_wrapper().into());
        }

        let function_name = self.get_hoisted_function_name(node);
        if let Some(function_name) = function_name {
            self.get_pending_expressions().push(
                self.factory
                    .create_assignment(
                        function_name.clone(),
                        self.factory
                            .create_function_expression(
                                maybe_filter(
                                    node.maybe_modifiers().as_double_deref(),
                                    |m: &Gc<Node>| !is_static_modifier(m),
                                ),
                                node_as_function_like_declaration.maybe_asterisk_token(),
                                Some(function_name),
                                Option::<Gc<NodeArray>>::None,
                                visit_parameter_list(
                                    Some(&node_as_function_like_declaration.parameters()),
                                    |node: &Node| self.class_element_visitor(node),
                                    &**self.context,
                                    Option::<
                                        fn(
                                            Option<&NodeArray>,
                                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                                            Option<&dyn Fn(&Node) -> bool>,
                                            Option<usize>,
                                            Option<usize>,
                                        )
                                            -> Option<Gc<NodeArray>>,
                                    >::None,
                                ),
                                None,
                                visit_function_body(
                                    Some(&node_as_function_like_declaration.maybe_body().unwrap()),
                                    |node: &Node| self.class_element_visitor(node),
                                    &**self.context,
                                    Option::<
                                        fn(
                                            Option<&Node>,
                                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                                            Option<&dyn Fn(&Node) -> bool>,
                                            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                                        )
                                            -> Option<Gc<Node>>,
                                    >::None,
                                )
                                .unwrap(),
                            )
                            .wrap(),
                    )
                    .wrap(),
            );
        }

        None
    }

    pub(super) fn get_hoisted_function_name(
        &self,
        node: &Node, /*MethodDeclaration | AccessorDeclaration*/
    ) -> Option<Gc<Node>> {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        let ref node_name = node_as_function_like_declaration.name();
        Debug_.assert(is_private_identifier(node_name), None);
        let info = self.access_private_identifier(node_name);
        Debug_.assert(
            info.is_some(),
            Some("Undeclared private name for property declaration."),
        );
        let info = info.unwrap();

        if info.kind() == PrivateIdentifierKind::Method {
            return Some(info.as_private_identifier_method_info().method_name.clone());
        }

        if info.kind() == PrivateIdentifierKind::Accessor {
            if is_get_accessor(node) {
                return info
                    .as_private_identifier_accessor_info()
                    .getter_name
                    .clone();
            }
            if is_set_accessor(node) {
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
        node: &Node, /*PropertyDeclaration*/
    ) -> VisitResult {
        let node_as_property_declaration = node.as_property_declaration();
        Debug_.assert(
            !node.maybe_decorators().as_double_deref().is_non_empty(),
            None,
        );

        if is_private_identifier(&node_as_property_declaration.name()) {
            if !self.should_transform_private_elements_or_class_static_blocks {
                return Some(
                    self.factory
                        .update_property_declaration(
                            node,
                            Option::<Gc<NodeArray>>::None,
                            visit_nodes(
                                node.maybe_modifiers().as_deref(),
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_modifier),
                                None,
                                None,
                            ),
                            node_as_property_declaration.name(),
                            None,
                            None,
                            None,
                        )
                        .into(),
                );
            }

            let info = self.access_private_identifier(&node_as_property_declaration.name());
            Debug_.assert(
                info.is_some(),
                Some("Undeclared private name for property declaration."),
            );
            let info = info.unwrap();
            if !info.is_valid() {
                return Some(node.node_wrapper().into());
            }
        }
        let expr = self.get_property_name_expression_if_needed(
            &node_as_property_declaration.name(),
            node_as_property_declaration.maybe_initializer().is_some()
                || self.use_define_for_class_fields,
        );
        if let Some(expr) = expr.filter(|expr| !is_simple_inlineable_expression(expr)) {
            self.get_pending_expressions().push(expr);
        }
        None
    }

    pub(super) fn create_private_identifier_access(
        &self,
        info: &PrivateIdentifierInfo,
        receiver: &Node, /*Expression*/
    ) -> Gc<Node /*Expression*/> {
        self.create_private_identifier_access_helper(
            info,
            &visit_node(
                receiver,
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            ),
        )
    }

    pub(super) fn create_private_identifier_access_helper(
        &self,
        info: &PrivateIdentifierInfo,
        receiver: &Node, /*Expression*/
    ) -> Gc<Node /*Expression*/> {
        set_comment_range(
            receiver,
            &ReadonlyTextRangeConcrete::from(move_range_pos(receiver, -1)),
        );

        match info.kind() {
            PrivateIdentifierKind::Accessor => self
                .context
                .get_emit_helper_factory()
                .create_class_private_field_get_helper(
                    receiver.node_wrapper(),
                    info.brand_check_identifier(),
                    info.kind(),
                    info.as_private_identifier_accessor_info()
                        .getter_name
                        .clone(),
                ),
            PrivateIdentifierKind::Method => self
                .context
                .get_emit_helper_factory()
                .create_class_private_field_get_helper(
                    receiver.node_wrapper(),
                    info.brand_check_identifier(),
                    info.kind(),
                    Some(info.as_private_identifier_method_info().method_name.clone()),
                ),
            PrivateIdentifierKind::Field => self
                .context
                .get_emit_helper_factory()
                .create_class_private_field_get_helper(
                    receiver.node_wrapper(),
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
        node: &Node, /*PropertyAccessExpression*/
    ) -> VisitResult {
        let node_as_property_access_expression = node.as_property_access_expression();
        if self.should_transform_private_elements_or_class_static_blocks
            && is_private_identifier(&node_as_property_access_expression.name())
        {
            let private_identifier_info =
                self.access_private_identifier(&node_as_property_access_expression.name());
            if let Some(ref private_identifier_info) = private_identifier_info {
                return Some(
                    self.create_private_identifier_access(
                        private_identifier_info,
                        &node_as_property_access_expression.expression,
                    )
                    .set_original_node(Some(node.node_wrapper()))
                    .set_text_range(Some(node))
                    .into(),
                );
            }
        }
        if self.should_transform_super_in_static_initializers
            && is_super_property(node)
            && is_identifier(&node_as_property_access_expression.name())
        {
            if let Some(current_static_property_declaration_or_static_block) =
                self.maybe_current_static_property_declaration_or_static_block()
            {
                if let Some(current_class_lexical_environment) =
                    self.maybe_current_class_lexical_environment()
                {
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
                                    .create_reflect_get_call(
                                        super_class_reference.clone(),
                                        self.factory
                                            .create_string_literal_from_node(
                                                &node_as_property_access_expression.name(),
                                            )
                                            .wrap(),
                                        Some(class_constructor.clone()),
                                    )
                                    .set_original_node(Some(
                                        node_as_property_access_expression.expression.clone(),
                                    ))
                                    .set_text_range(Some(
                                        &*node_as_property_access_expression.expression,
                                    ))
                                    .into(),
                            );
                        }
                    }
                }
            }
        }
        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }
}

impl TransformerInterface for TransformClassFields {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(self.transform_source_file(node))
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformClassFieldsOnEmitNodeOverrider {
    transform_class_fields: Gc<Box<TransformClassFields>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformClassFieldsOnEmitNodeOverrider {
    fn new(
        transform_class_fields: Gc<Box<TransformClassFields>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_class_fields,
            previous_on_emit_node,
        }
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformClassFieldsOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        _hint: EmitHint,
        _node: &Node,
        _emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformClassFieldsOnSubstituteNodeOverrider {
    transform_class_fields: Gc<Box<TransformClassFields>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformClassFieldsOnSubstituteNodeOverrider {
    pub(super) fn new(
        transform_class_fields: Gc<Box<TransformClassFields>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_class_fields,
            previous_on_substitute_node,
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformClassFieldsOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, _hint: EmitHint, _node: &Node) -> io::Result<Gc<Node>> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformClassFieldsFactory {}

impl TransformClassFieldsFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformClassFieldsFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformClassFields::new(context).as_transformer(),
        )
    }
}

pub fn transform_class_fields() -> TransformerFactory {
    Gc::new(Box::new(TransformClassFieldsFactory::new()))
}
