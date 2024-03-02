use std::{
    any::Any,
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashMap,
    io,
};

use bitflags::bitflags;
use derive_builder::Builder;
use id_arena::Id;
use indexmap::IndexMap;

use crate::{
    chain_bundle, downcast_transformer_ref, get_emit_flags, get_enclosing_block_scope_container,
    get_name_of_declaration, get_original_node, get_parse_tree_node, has_static_modifier,
    impl_has_arena, is_block, is_case_block, is_case_clause, is_catch_clause, is_class_element,
    is_class_like, is_default_clause, is_function_like, is_identifier, is_if_statement,
    is_internal_name, is_iteration_statement, is_labeled_statement, is_property_declaration,
    is_return_statement, is_switch_statement, is_try_statement, is_with_statement, ref_unwrapped,
    released, try_maybe_visit_each_child, AllArenas, CompilerOptions, CoreTransformationContext,
    EmitFlags, EmitHelperFactory, EmitHint, EmitResolver, GeneratedIdentifierFlags,
    GetOrInsertDefault, HasArena, InArena, Matches, Node, NodeExt, NodeFactory, NodeInterface,
    OptionTry, ReadonlyTextRange, SourceFileLike, SourceTextAsChars, SyntaxKind, TransformFlags,
    TransformNodesTransformationResult, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    VisitResult,
};

bitflags! {
    #[derive(Default)]
    pub(super) struct ES2015SubstitutionFlags: u32 {
        const None = 0;
        const CapturedThis = 1 << 0;
        const BlockScopedBindings = 1 << 1;
    }
}

#[derive(Clone)]
pub(super) struct LoopOutParameter {
    pub flags: LoopOutParameterFlags,
    pub original_name: Id<Node /*Identifier*/>,
    pub out_param_name: Id<Node /*Identifier*/>,
}

bitflags! {
    pub(super) struct LoopOutParameterFlags: u32 {
        const None = 0;
        const Body = 1 << 0;
        const Initializer = 1 << 1;
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum CopyDirection {
    ToOriginal,
    ToOutParameter,
}

bitflags! {
    #[derive(Default)]
    pub(super) struct Jump: u32 {
        const None = 0;
        const Break = 1 << 1;
        const Continue = 1 << 2;
        const Return = 1 << 3;
    }
}

#[derive(Clone, Builder)]
#[builder(setter(strip_option))]
pub struct ConvertedLoopState {
    #[builder(default)]
    pub labels: Option<HashMap<String, bool>>,
    #[builder(default)]
    pub labeled_non_local_breaks: Option<IndexMap<String, String>>,
    #[builder(default)]
    pub labeled_non_local_continues: Option<IndexMap<String, String>>,
    #[builder(default)]
    pub(super) non_local_jumps: Option<Jump>,
    #[builder(default)]
    pub(super) allowed_non_labeled_jumps: Option<Jump>,
    #[builder(default)]
    pub arguments_name: Option<Id<Node /*Identifier*/>>,
    #[builder(default)]
    pub this_name: Option<Id<Node /*Identifier*/>>,
    #[builder(default)]
    pub contains_lexical_this: Option<bool>,
    #[builder(default)]
    pub hoisted_local_variables: Option<Vec<Id<Node /*Identifier*/>>>,
    #[builder(default)]
    pub condition_variable: Option<Id<Node /*Identifier*/>>,
    pub loop_parameters: Vec<Id<Node /*ParameterDeclaration*/>>,
    pub(super) loop_out_parameters: Vec<LoopOutParameter>,
}

bitflags! {
    #[derive(Default)]
    pub(super) struct HierarchyFacts: u32 {
        const None = 0;
        const Function = 1 << 0;
        const ArrowFunction = 1 << 1;
        const AsyncFunctionBody = 1 << 2;
        const NonStaticClassElement = 1 << 3;
        const CapturesThis = 1 << 4;
        const ExportedVariableStatement = 1 << 5;
        const TopLevel = 1 << 6;
        const Block = 1 << 7;
        const IterationStatement = 1 << 8;
        const IterationStatementBlock = 1 << 9;
        const IterationContainer = 1 << 10;
        const ForStatement = 1 << 11;
        const ForInOrForOfStatement = 1 << 12;
        const ConstructorWithCapturedSuper = 1 << 13;
        const StaticInitializer = 1 << 14;

        const AncestorFactsMask = (Self::StaticInitializer.bits << 1) - 1;

        const BlockScopeIncludes = Self::None.bits;
        const BlockScopeExcludes = Self::TopLevel.bits | Self::Block.bits | Self::IterationStatement.bits | Self::IterationStatementBlock.bits | Self::ForStatement.bits | Self::ForInOrForOfStatement.bits;

        const SourceFileIncludes = Self::TopLevel.bits;
        const SourceFileExcludes = Self::BlockScopeExcludes.bits & !Self::TopLevel.bits | Self::IterationContainer.bits;

        const FunctionIncludes = Self::Function.bits | Self::TopLevel.bits;
        const FunctionExcludes = Self::BlockScopeExcludes.bits & !Self::TopLevel.bits | Self::ArrowFunction.bits | Self::AsyncFunctionBody.bits | Self::CapturesThis.bits | Self::NonStaticClassElement.bits | Self::ConstructorWithCapturedSuper.bits | Self::IterationContainer.bits | Self::StaticInitializer.bits;

        const AsyncFunctionBodyIncludes = Self::FunctionIncludes.bits | Self::AsyncFunctionBody.bits;
        const AsyncFunctionBodyExcludes = Self::FunctionExcludes.bits & !Self::NonStaticClassElement.bits;

        const ArrowFunctionIncludes = Self::ArrowFunction.bits | Self::TopLevel.bits;
        const ArrowFunctionExcludes = Self::BlockScopeExcludes.bits & !Self::TopLevel.bits | Self::ConstructorWithCapturedSuper.bits;

        const ConstructorIncludes = Self::FunctionIncludes.bits | Self::NonStaticClassElement.bits;
        const ConstructorExcludes = Self::FunctionExcludes.bits & !Self::NonStaticClassElement.bits;

        const DoOrWhileStatementIncludes = Self::IterationStatement.bits | Self::IterationContainer.bits;
        const DoOrWhileStatementExcludes = Self::None.bits;

        const ForStatementIncludes = Self::IterationStatement.bits | Self::ForStatement.bits | Self::IterationContainer.bits;
        const ForStatementExcludes = Self::BlockScopeExcludes.bits & !Self::ForStatement.bits;

        const ForInOrForOfStatementIncludes = Self::IterationStatement.bits | Self::ForInOrForOfStatement.bits | Self::IterationContainer.bits;
        const ForInOrForOfStatementExcludes = Self::BlockScopeExcludes.bits & !Self::ForInOrForOfStatement.bits;

        const BlockIncludes = Self::Block.bits;
        const BlockExcludes = Self::BlockScopeExcludes.bits & !Self::Block.bits;

        const IterationStatementBlockIncludes = Self::IterationStatementBlock.bits;
        const IterationStatementBlockExcludes = Self::BlockScopeExcludes.bits;

        const StaticInitializerIncludes = Self::FunctionIncludes.bits | Self::StaticInitializer.bits;
        const StaticInitializerExcludes = Self::FunctionExcludes.bits;

        const NewTarget = 1 << 15;
        const CapturedLexicalThis = 1 << 16;

        const SubtreeFactsMask = !Self::AncestorFactsMask.bits;

        const ArrowFunctionSubtreeExcludes = Self::None.bits;
        const FunctionSubtreeExcludes = Self::NewTarget.bits | Self::CapturedLexicalThis.bits;
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum SpreadSegmentKind {
    None,
    UnpackedSpread,
    PackedSpread,
}

#[derive(Clone)]
pub(super) struct SpreadSegment {
    pub kind: SpreadSegmentKind,
    pub expression: Id<Node /*Expression*/>,
}

pub(super) fn create_spread_segment(
    kind: SpreadSegmentKind,
    expression: Id<Node /*Expression*/>,
) -> SpreadSegment {
    SpreadSegment { kind, expression }
}

pub(super) struct TransformES2015 {
    pub(super) arena: *const AllArenas,
    pub(super) _arena_id: Cell<Option<Transformer>>,
    pub(super) context: Id<TransformNodesTransformationResult>,
    pub(super) factory: Id<NodeFactory>,
    pub(super) compiler_options: Id<CompilerOptions>,
    pub(super) resolver: Id<Box<dyn EmitResolver>>,
    pub(super) current_source_file: Cell<Option<Id<Node /*SourceFile*/>>>,
    pub(super) current_text: RefCell<Option<SourceTextAsChars>>,
    pub(super) hierarchy_facts: Cell<Option<HierarchyFacts>>,
    pub(super) tagged_template_string_declarations:
        RefCell<Option<Vec<Id<Node /*VariableDeclaration*/>>>>,
    pub(super) converted_loop_state: Cell<Option<Id<ConvertedLoopState>>>,
    pub(super) enabled_substitutions: Cell<Option<ES2015SubstitutionFlags>>,
}

impl TransformES2015 {
    pub(super) fn new(
        context: Id<TransformNodesTransformationResult>,
        arena: *const AllArenas,
    ) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        let ret = arena_ref.alloc_transformer(Box::new(Self {
            arena,
            _arena_id: Default::default(),
            factory: context_ref.factory(),
            compiler_options: context_ref.get_compiler_options(),
            resolver: context_ref.get_emit_resolver(),
            context: context.clone(),
            current_source_file: Default::default(),
            current_text: Default::default(),
            hierarchy_facts: Default::default(),
            tagged_template_string_declarations: Default::default(),
            converted_loop_state: Default::default(),
            enabled_substitutions: Default::default(),
        }));
        downcast_transformer_ref::<Self>(ret, arena_ref).set_arena_id(ret);
        context_ref.override_on_emit_node(&mut |previous_on_emit_node| {
            arena_ref.alloc_transformation_context_on_emit_node_overrider(Box::new(
                TransformES2015OnEmitNodeOverrider::new(ret, previous_on_emit_node, arena_ref),
            ))
        });
        context_ref.override_on_substitute_node(&mut |previous_on_substitute_node| {
            arena_ref.alloc_transformation_context_on_substitute_node_overrider(Box::new(
                TransformES2015OnSubstituteNodeOverrider::new(
                    ret,
                    previous_on_substitute_node,
                    arena_ref,
                ),
            ))
        });
        ret
    }

    pub(super) fn arena_id(&self) -> Transformer {
        self._arena_id.get().unwrap()
    }

    pub(super) fn set_arena_id(&self, id: Transformer) {
        self._arena_id.set(Some(id));
    }

    pub(super) fn maybe_current_source_file(&self) -> Option<Id<Node /*SourceFile*/>> {
        self.current_source_file.get()
    }

    pub(super) fn current_source_file(&self) -> Id<Node /*SourceFile*/> {
        self.current_source_file.get().unwrap()
    }

    pub(super) fn set_current_source_file(
        &self,
        current_source_file: Option<Id<Node /*SourceFile*/>>,
    ) {
        self.current_source_file.set(current_source_file);
    }

    pub(super) fn current_text(&self) -> Ref<SourceTextAsChars> {
        ref_unwrapped(&self.current_text)
    }

    pub(super) fn set_current_text(&self, current_text: Option<SourceTextAsChars>) {
        *self.current_text.borrow_mut() = current_text;
    }

    pub(super) fn maybe_hierarchy_facts(&self) -> Option<HierarchyFacts> {
        self.hierarchy_facts.get()
    }

    pub(super) fn set_hierarchy_facts(&self, hierarchy_facts: Option<HierarchyFacts>) {
        self.hierarchy_facts.set(hierarchy_facts);
    }

    pub(super) fn maybe_converted_loop_state(&self) -> Option<Id<ConvertedLoopState>> {
        self.converted_loop_state.get()
    }

    pub(super) fn converted_loop_state(&self) -> Id<ConvertedLoopState> {
        self.converted_loop_state.get().unwrap()
    }

    pub(super) fn set_converted_loop_state(
        &self,
        converted_loop_state: Option<Id<ConvertedLoopState>>,
    ) {
        self.converted_loop_state.set(converted_loop_state);
    }

    pub(super) fn maybe_enabled_substitutions(&self) -> Option<ES2015SubstitutionFlags> {
        self.enabled_substitutions.get()
    }

    pub(super) fn set_enabled_substitutions(
        &self,
        enabled_substitutions: Option<ES2015SubstitutionFlags>,
    ) {
        self.enabled_substitutions.set(enabled_substitutions);
    }

    pub(super) fn maybe_tagged_template_string_declarations(
        &self,
    ) -> Ref<Option<Vec<Id<Node /*VariableDeclaration*/>>>> {
        self.tagged_template_string_declarations.borrow()
    }

    pub(super) fn maybe_tagged_template_string_declarations_mut(
        &self,
    ) -> RefMut<Option<Vec<Id<Node /*VariableDeclaration*/>>>> {
        self.tagged_template_string_declarations.borrow_mut()
    }

    pub(super) fn set_tagged_template_string_declarations(
        &self,
        tagged_template_string_declarations: Option<Vec<Id<Node /*VariableDeclaration*/>>>,
    ) {
        *self.tagged_template_string_declarations.borrow_mut() =
            tagged_template_string_declarations;
    }

    pub(super) fn emit_helpers(&self) -> debug_cell::Ref<'_, EmitHelperFactory> {
        self.context.ref_(self).get_emit_helper_factory().ref_(self)
    }

    pub(super) fn record_tagged_template_string(&self, temp: Id<Node> /*Identifier*/) {
        self.maybe_tagged_template_string_declarations_mut()
            .get_or_insert_default_()
            .push(self.factory.ref_(self).create_variable_declaration(
                Some(temp),
                None,
                None,
                None,
            ));
    }

    pub(super) fn transform_source_file(
        &self,
        node: Id<Node>, /*SourceFile*/
    ) -> io::Result<Id<Node>> {
        if node.ref_(self).as_source_file().is_declaration_file() {
            return Ok(node);
        }

        self.set_current_source_file(Some(node));
        self.set_current_text(Some(
            node.ref_(self).as_source_file().text_as_chars().clone(),
        ));

        let visited = self
            .visit_source_file(node)?
            .add_emit_helpers(self.context.ref_(self).read_emit_helpers().as_deref(), self);

        self.set_current_source_file(None);
        self.set_current_text(None);
        self.set_tagged_template_string_declarations(None);
        self.set_hierarchy_facts(Some(HierarchyFacts::None));
        Ok(visited)
    }

    pub(super) fn enter_subtree(
        &self,
        exclude_facts: HierarchyFacts,
        include_facts: HierarchyFacts,
    ) -> Option<HierarchyFacts> {
        let ancestor_facts = self.maybe_hierarchy_facts();
        self.set_hierarchy_facts(Some(
            (self.maybe_hierarchy_facts().unwrap_or_default() & !exclude_facts | include_facts)
                & HierarchyFacts::AncestorFactsMask,
        ));
        ancestor_facts
    }

    pub(super) fn exit_subtree(
        &self,
        ancestor_facts: Option<HierarchyFacts>,
        exclude_facts: HierarchyFacts,
        include_facts: HierarchyFacts,
    ) {
        self.set_hierarchy_facts(Some(
            (self.maybe_hierarchy_facts().unwrap_or_default() & !exclude_facts | include_facts)
                & HierarchyFacts::SubtreeFactsMask
                | ancestor_facts.unwrap_or_default(),
        ));
    }

    pub(super) fn is_return_void_statement_in_constructor_with_captured_super(
        &self,
        node: Id<Node>,
    ) -> bool {
        self.maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::ConstructorWithCapturedSuper)
            && node.ref_(self).kind() == SyntaxKind::ReturnStatement
            && node.ref_(self).as_return_statement().expression.is_none()
    }

    pub(super) fn is_or_may_contain_return_completion(&self, node: Id<Node>) -> bool {
        node.ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsHoistedDeclarationOrCompletion)
            && (is_return_statement(&node.ref_(self))
                || is_if_statement(&node.ref_(self))
                || is_with_statement(&node.ref_(self))
                || is_switch_statement(&node.ref_(self))
                || is_case_block(&node.ref_(self))
                || is_case_clause(&node.ref_(self))
                || is_default_clause(&node.ref_(self))
                || is_try_statement(&node.ref_(self))
                || is_catch_clause(&node.ref_(self))
                || is_labeled_statement(&node.ref_(self))
                || is_iteration_statement(node, false, self)
                || is_block(&node.ref_(self)))
    }

    pub(super) fn should_visit_node(&self, node: Id<Node>) -> bool {
        node.ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsES2015)
            || self.maybe_converted_loop_state().is_some()
            || self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::ConstructorWithCapturedSuper)
                && self.is_or_may_contain_return_completion(node)
            || is_iteration_statement(node, false, self)
                && self.should_convert_iteration_statement(node)
            || get_emit_flags(node, self).intersects(EmitFlags::TypeScriptClassWrapper)
    }

    pub(super) fn visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/ {
        Ok(if self.should_visit_node(node) {
            self.visitor_worker(node, false)?
        } else {
            Some(node.into())
        })
    }

    pub(super) fn visitor_with_unused_expression_result(
        &self,
        node: Id<Node>,
    ) -> io::Result<VisitResult> /*<Node>*/ {
        Ok(if self.should_visit_node(node) {
            self.visitor_worker(node, true)?
        } else {
            Some(node.into())
        })
    }

    pub(super) fn class_wrapper_statement_visitor(
        &self,
        node: Id<Node>,
    ) -> io::Result<VisitResult> /*<Node>*/ {
        if self.should_visit_node(node) {
            let original = get_original_node(node, self);
            if is_property_declaration(&original.ref_(self)) && has_static_modifier(original, self)
            {
                let ancestor_facts = self.enter_subtree(
                    HierarchyFacts::StaticInitializerExcludes,
                    HierarchyFacts::StaticInitializerIncludes,
                );
                let result = self.visitor_worker(node, false)?;
                self.exit_subtree(
                    ancestor_facts,
                    HierarchyFacts::FunctionSubtreeExcludes,
                    HierarchyFacts::None,
                );
                return Ok(result);
            }
            return self.visitor_worker(node, false);
        }
        Ok(Some(node.into()))
    }

    pub(super) fn call_expression_visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/
    {
        if node.ref_(self).kind() == SyntaxKind::SuperKeyword {
            return Ok(Some(self.visit_super_keyword(true).into()));
        }
        self.visitor(node)
    }

    pub(super) fn visitor_worker(
        &self,
        node: Id<Node>,
        expression_result_is_unused: bool,
    ) -> io::Result<VisitResult> /*<Node>*/ {
        Ok(match released!(node.ref_(self).kind()) {
            SyntaxKind::StaticKeyword => None,
            SyntaxKind::ClassDeclaration => self.visit_class_declaration(node)?,
            SyntaxKind::ClassExpression => Some(self.visit_class_expression(node)?.into()),
            SyntaxKind::Parameter => self.visit_parameter(node).map(Into::into),
            SyntaxKind::FunctionDeclaration => Some(self.visit_function_declaration(node)?.into()),
            SyntaxKind::ArrowFunction => self.visit_arrow_function(node)?,
            SyntaxKind::FunctionExpression => Some(self.visit_function_expression(node)?.into()),
            SyntaxKind::VariableDeclaration => self.visit_variable_declaration(node)?,
            SyntaxKind::Identifier => Some(self.visit_identifier(node)?.into()),
            SyntaxKind::VariableDeclarationList => {
                Some(self.visit_variable_declaration_list(node)?.into())
            }
            SyntaxKind::SwitchStatement => Some(self.visit_switch_statement(node)?.into()),
            SyntaxKind::CaseBlock => Some(self.visit_case_block(node)?.into()),
            SyntaxKind::Block => Some(self.visit_block(node, false)?.into()),
            SyntaxKind::BreakStatement | SyntaxKind::ContinueStatement => {
                Some(self.visit_break_or_continue_statement(node)?.into())
            }
            SyntaxKind::LabeledStatement => self.visit_labeled_statement(node)?,
            SyntaxKind::DoStatement | SyntaxKind::WhileStatement => {
                self.visit_do_or_while_statement(node, Option::<Id<Node>>::None)
            }
            SyntaxKind::ForStatement => self.visit_for_statement(node, Option::<Id<Node>>::None),
            SyntaxKind::ForInStatement => {
                self.visit_for_in_statement(node, Option::<Id<Node>>::None)
            }
            SyntaxKind::ForOfStatement => {
                self.visit_for_of_statement(node, Option::<Id<Node>>::None)?
            }
            SyntaxKind::ExpressionStatement => Some(self.visit_expression_statement(node)?.into()),
            SyntaxKind::ObjectLiteralExpression => {
                Some(self.visit_object_literal_expression(node)?.into())
            }
            SyntaxKind::CatchClause => Some(self.visit_catch_clause(node)?.into()),
            SyntaxKind::ShorthandPropertyAssignment => {
                Some(self.visit_shorthand_property_assignment(node)?.into())
            }
            SyntaxKind::ComputedPropertyName => self.visit_computed_property_name(node)?,
            SyntaxKind::ArrayLiteralExpression => {
                Some(self.visit_array_literal_expression(node)?.into())
            }
            SyntaxKind::CallExpression => self.visit_call_expression(node)?,
            SyntaxKind::NewExpression => Some(self.visit_new_expression(node)?.into()),
            SyntaxKind::ParenthesizedExpression => Some(
                self.visit_parenthesized_expression(node, expression_result_is_unused)?
                    .into(),
            ),
            SyntaxKind::BinaryExpression => Some(
                self.visit_binary_expression(node, expression_result_is_unused)?
                    .into(),
            ),
            SyntaxKind::CommaListExpression => Some(
                self.visit_comma_list_expression(node, expression_result_is_unused)?
                    .into(),
            ),
            SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TemplateHead
            | SyntaxKind::TemplateMiddle
            | SyntaxKind::TemplateTail => Some(self.visit_template_literal(node).into()),
            SyntaxKind::StringLiteral => self.visit_string_literal(node),
            SyntaxKind::NumericLiteral => self.visit_numeric_literal(node),
            SyntaxKind::TaggedTemplateExpression => self.visit_tagged_template_expression(node)?,
            SyntaxKind::TemplateExpression => Some(self.visit_template_expression(node)?.into()),
            SyntaxKind::YieldExpression => Some(self.visit_yield_expression(node)?.into()),
            SyntaxKind::SpreadElement => self.visit_spread_element(node)?,
            SyntaxKind::SuperKeyword => Some(self.visit_super_keyword(false).into()),
            SyntaxKind::ThisKeyword => Some(self.visit_this_keyword(node).into()),
            SyntaxKind::MetaProperty => self.visit_meta_property(node),
            SyntaxKind::MethodDeclaration => Some(self.visit_method_declaration(node)?.into()),
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                Some(self.visit_accessor_declaration(node)?.into())
            }
            SyntaxKind::VariableStatement => self.visit_variable_statement(node)?.map(Into::into),
            SyntaxKind::ReturnStatement => Some(self.visit_return_statement(node)?.into()),
            SyntaxKind::VoidExpression => Some(self.visit_void_expression(node)?.into()),
            _ => try_maybe_visit_each_child(
                Some(node),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .map(Into::into),
        })
    }
}

impl TransformerInterface for TransformES2015 {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        self.transform_source_file(node)
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl_has_arena!(TransformES2015);

struct TransformES2015OnEmitNodeOverrider {
    arena: *const AllArenas,
    transform_es2015: Transformer,
    previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformES2015OnEmitNodeOverrider {
    fn new(
        transform_es2015: Transformer,
        previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_es2015,
            previous_on_emit_node,
        }
    }

    fn transform_es2015(&self) -> debug_cell::Ref<'_, TransformES2015> {
        downcast_transformer_ref(self.transform_es2015, self)
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformES2015OnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        if self
            .transform_es2015()
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ES2015SubstitutionFlags::CapturedThis)
            && is_function_like(Some(&node.ref_(self)))
        {
            let ancestor_facts = self.transform_es2015().enter_subtree(
                HierarchyFacts::FunctionExcludes,
                if get_emit_flags(node, self).intersects(EmitFlags::CapturesThis) {
                    HierarchyFacts::FunctionIncludes | HierarchyFacts::CapturesThis
                } else {
                    HierarchyFacts::FunctionIncludes
                },
            );
            self.previous_on_emit_node
                .ref_(self)
                .on_emit_node(hint, node, emit_callback)?;
            self.transform_es2015().exit_subtree(
                ancestor_facts,
                HierarchyFacts::None,
                HierarchyFacts::None,
            );
            return Ok(());
        }
        self.previous_on_emit_node
            .ref_(self)
            .on_emit_node(hint, node, emit_callback)?;
        return Ok(());
    }
}

impl_has_arena!(TransformES2015OnEmitNodeOverrider);

struct TransformES2015OnSubstituteNodeOverrider {
    arena: *const AllArenas,
    transform_es2015: Transformer,
    previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformES2015OnSubstituteNodeOverrider {
    pub(super) fn new(
        transform_es2015: Transformer,
        previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_es2015,
            previous_on_substitute_node,
        }
    }

    fn transform_es2015(&self) -> debug_cell::Ref<'_, TransformES2015> {
        downcast_transformer_ref(self.transform_es2015, self)
    }

    pub(super) fn substitute_identifier(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Id<Node>> {
        if self
            .transform_es2015()
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ES2015SubstitutionFlags::BlockScopedBindings)
            && !is_internal_name(node, self)
        {
            let original = get_parse_tree_node(
                Some(node),
                Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
                self,
            );
            if let Some(original) = original
                .try_filter(|&original| self.is_name_of_declaration_with_colliding_name(original))?
            {
                return Ok(self
                    .transform_es2015()
                    .factory
                    .ref_(self)
                    .get_generated_name_for_node(Some(original), None)
                    .set_text_range(Some(&*node.ref_(self)), self));
            }
        }

        Ok(node)
    }

    pub(super) fn is_name_of_declaration_with_colliding_name(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<bool> {
        Ok(match node.ref_(self).parent().ref_(self).kind() {
            SyntaxKind::BindingElement
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::VariableDeclaration => {
                node.ref_(self)
                    .parent()
                    .ref_(self)
                    .as_named_declaration()
                    .name()
                    == node
                    && self
                        .transform_es2015()
                        .resolver
                        .ref_(self)
                        .is_declaration_with_colliding_name(node.ref_(self).parent())?
            }
            _ => false,
        })
    }

    pub(super) fn substitute_expression(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Id<Node>> {
        match released!(node.ref_(self).kind()) {
            SyntaxKind::Identifier => {
                return self.substitute_expression_identifier(node);
            }
            SyntaxKind::ThisKeyword => {
                return Ok(self.substitute_this_keyword(node));
            }
            _ => (),
        }

        Ok(node)
    }

    pub(super) fn substitute_expression_identifier(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Id<Node /*Identifier*/>> {
        if self
            .transform_es2015()
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ES2015SubstitutionFlags::BlockScopedBindings)
            && !is_internal_name(node, self)
        {
            let declaration = self
                .transform_es2015()
                .resolver
                .ref_(self)
                .get_referenced_declaration_with_colliding_name(node)?;
            if let Some(declaration) = declaration.filter(|&declaration| {
                !(is_class_like(&declaration.ref_(self))
                    && self.is_part_of_class_body(declaration, node))
            }) {
                return Ok(self
                    .transform_es2015()
                    .factory
                    .ref_(self)
                    .get_generated_name_for_node(
                        get_name_of_declaration(Some(declaration), self),
                        None,
                    )
                    .set_text_range(Some(&*node.ref_(self)), self));
            }
        }

        Ok(node)
    }

    pub(super) fn is_part_of_class_body(
        &self,
        declaration: Id<Node>, /*ClassLikeDeclaration*/
        node: Id<Node>,        /*Identifier*/
    ) -> bool {
        let mut current_node =
            get_parse_tree_node(Some(node), Option::<fn(Id<Node>) -> bool>::None, self);
        if current_node.is_none_or_matches(|current_node| {
            current_node == declaration
                || current_node.ref_(self).end() <= declaration.ref_(self).pos()
                || current_node.ref_(self).pos() >= declaration.ref_(self).end()
        }) {
            return false;
        }
        let block_scope = get_enclosing_block_scope_container(declaration, self).unwrap();
        while let Some(current_node_present) = current_node {
            if current_node_present == block_scope || current_node_present == declaration {
                return false;
            }
            if is_class_element(&current_node_present.ref_(self))
                && current_node_present.ref_(self).parent() == declaration
            {
                return true;
            }
            current_node = current_node_present.ref_(self).maybe_parent();
        }
        false
    }

    pub(super) fn substitute_this_keyword(
        &self,
        node: Id<Node>, /*PrimaryExpression*/
    ) -> Id<Node /*PrimaryExpression*/> {
        if self
            .transform_es2015()
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ES2015SubstitutionFlags::CapturedThis)
            && self
                .transform_es2015()
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::CapturesThis)
        {
            return self
                .transform_es2015()
                .factory
                .ref_(self)
                .create_unique_name(
                    "_this",
                    Some(
                        GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel,
                    ),
                )
                .set_text_range(Some(&*node.ref_(self)), self);
        }
        node
    }
}

impl TransformationContextOnSubstituteNodeOverrider for TransformES2015OnSubstituteNodeOverrider {
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        let node = self
            .previous_on_substitute_node
            .ref_(self)
            .on_substitute_node(hint, node)?;

        if hint == EmitHint::Expression {
            return self.substitute_expression(node);
        }

        if is_identifier(&node.ref_(self)) {
            return self.substitute_identifier(node);
        }

        Ok(node)
    }
}

impl_has_arena!(TransformES2015OnSubstituteNodeOverrider);

struct TransformES2015Factory {
    arena: *const AllArenas,
}

impl TransformES2015Factory {
    fn new(arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
        }
    }
}

impl TransformerFactoryInterface for TransformES2015Factory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle(self)
            .ref_(self)
            .call(context, TransformES2015::new(context, self.arena))
    }
}

impl_has_arena!(TransformES2015Factory);

pub fn transform_es2015(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformES2015Factory::new(arena)))
}
