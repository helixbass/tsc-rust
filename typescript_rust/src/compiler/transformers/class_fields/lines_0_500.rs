use std::{cell::Cell, collections::HashMap, io, mem};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use crate::{
    chain_bundle, get_emit_script_target, get_use_define_for_class_fields, is_expression,
    is_private_identifier, is_statement, visit_each_child, visit_node, BaseNodeFactorySynthetic,
    CompilerOptions, Debug_, EmitHint, EmitResolver, Node, NodeArray, NodeExt, NodeFactory, NodeId,
    NodeInterface, ScriptTarget, SyntaxKind, TransformFlags, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    UnderscoreEscapedMap, VisitResult,
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
}

#[derive(Trace, Finalize)]
pub(super) enum PrivateIdentifierInfo {
    PrivateIdentifierMethodInfo(PrivateIdentifierMethodInfo),
    PrivateIdentifierInstanceFieldInfo(PrivateIdentifierInstanceFieldInfo),
    PrivateIdentifierStaticFieldInfo(PrivateIdentifierStaticFieldInfo),
    PrivateIdentifierAccessorInfo(PrivateIdentifierAccessorInfo),
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

    pub(super) fn maybe_pending_expressions_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Node /*Expression*/>>>> {
        self.pending_expressions.borrow_mut()
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

    pub(super) fn maybe_pending_statements_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Node /*Statement*/>>>> {
        self.pending_statements.borrow_mut()
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

    pub(super) fn visit_variable_statement(
        &self,
        _node: &Node, /*VariableStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_property_declaration(
        &self,
        _node: &Node, /*PropertyDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_property_access_expression(
        &self,
        _node: &Node, /*PropertyAccessExpression*/
    ) -> VisitResult {
        unimplemented!()
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
