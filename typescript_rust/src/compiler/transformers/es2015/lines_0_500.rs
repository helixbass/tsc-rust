use std::{cell::Cell, collections::HashMap, io, mem, rc::Rc};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use crate::{
    chain_bundle, gc_cell_ref_mut_unwrapped, gc_cell_ref_unwrapped, get_emit_flags,
    get_original_node, has_static_modifier, is_block, is_case_block, is_case_clause,
    is_catch_clause, is_default_clause, is_if_statement, is_iteration_statement,
    is_labeled_statement, is_property_declaration, is_return_statement, is_switch_statement,
    is_try_statement, is_with_statement, maybe_visit_each_child, try_maybe_visit_each_child,
    visit_each_child, BaseNodeFactorySynthetic, CompilerOptions, EmitFlags, EmitHelperFactory,
    EmitHint, EmitResolver, Node, NodeArray, NodeExt, NodeFactory, NodeInterface, SourceFileLike,
    SourceTextAsChars, SyntaxKind, TransformFlags, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    VisitResult,
};

bitflags! {
    pub(super) struct ES2015SubstitutionFlags: u32 {
        const None = 0;
        const CapturedThis = 1 << 0;
        const BlockScopedBindings = 1 << 1;
    }
}

#[derive(Clone, Trace, Finalize)]
pub(super) struct LoopOutParameter {
    #[unsafe_ignore_trace]
    pub flags: LoopOutParameterFlags,
    pub original_name: Gc<Node /*Identifier*/>,
    pub out_param_name: Gc<Node /*Identifier*/>,
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

#[derive(Clone, Trace, Finalize)]
pub(super) struct ConvertedLoopState {
    pub labels: Option<HashMap<String, bool>>,
    pub labeled_non_local_breaks: Option<HashMap<String, String>>,
    pub labeled_non_local_continues: Option<HashMap<String, String>>,
    #[unsafe_ignore_trace]
    pub non_local_jumps: Option<Jump>,
    #[unsafe_ignore_trace]
    pub allowed_non_labeled_jumps: Option<Jump>,
    pub arguments_name: Option<Gc<Node /*Identifier*/>>,
    pub this_name: Option<Gc<Node /*Identifier*/>>,
    pub contains_lexical_this: Option<bool>,
    pub hoisted_local_variables: Option<Vec<Gc<Node /*Identifier*/>>>,
    pub condition_variable: Option<Gc<Node /*Identifier*/>>,
    pub loop_parameters: Vec<Gc<Node /*ParameterDeclaration*/>>,
    pub loop_out_parameters: Vec<LoopOutParameter>,
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

pub(super) struct SpreadSegment {
    pub kind: SpreadSegmentKind,
    pub expression: Gc<Node /*Expression*/>,
}

pub(super) fn create_spread_segment(
    kind: SpreadSegmentKind,
    expression: Gc<Node /*Expression*/>,
) -> SpreadSegment {
    SpreadSegment { kind, expression }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformES2015 {
    pub(super) _transformer_wrapper: GcCell<Option<Transformer>>,
    pub(super) context: Gc<Box<dyn TransformationContext>>,
    pub(super) factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    pub(super) compiler_options: Gc<CompilerOptions>,
    pub(super) resolver: Gc<Box<dyn EmitResolver>>,
    pub(super) current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    pub(super) current_text: GcCell<Option<SourceTextAsChars>>,
    #[unsafe_ignore_trace]
    pub(super) hierarchy_facts: Cell<Option<HierarchyFacts>>,
    pub(super) tagged_template_string_declarations:
        GcCell<Option<Vec<Gc<Node /*VariableDeclaration*/>>>>,
    pub(super) converted_loop_state: GcCell<Option<ConvertedLoopState>>,
    #[unsafe_ignore_trace]
    pub(super) enabled_substitutions: Cell<Option<ES2015SubstitutionFlags>>,
}

impl TransformES2015 {
    pub(super) fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            factory: context.factory(),
            compiler_options: context.get_compiler_options(),
            resolver: context.get_emit_resolver(),
            context: context.clone(),
            current_source_file: Default::default(),
            current_text: Default::default(),
            hierarchy_facts: Default::default(),
            tagged_template_string_declarations: Default::default(),
            converted_loop_state: Default::default(),
            enabled_substitutions: Default::default(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformES2015OnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformES2015OnSubstituteNodeOverrider::new(
                downcasted.clone(),
                previous_on_substitute_node,
            )))
        });
        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    pub(super) fn maybe_current_source_file(&self) -> GcCellRef<Option<Gc<Node /*SourceFile*/>>> {
        self.current_source_file.borrow()
    }

    pub(super) fn maybe_current_source_file_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node /*SourceFile*/>>> {
        self.current_source_file.borrow_mut()
    }

    pub(super) fn current_source_file(&self) -> GcCellRef<Gc<Node /*SourceFile*/>> {
        gc_cell_ref_unwrapped(&self.current_source_file)
    }

    pub(super) fn current_source_file_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node /*SourceFile*/>>, Gc<Node /*SourceFile*/>> {
        gc_cell_ref_mut_unwrapped(&self.current_source_file)
    }

    pub(super) fn set_current_source_file(
        &self,
        current_source_file: Option<Gc<Node /*SourceFile*/>>,
    ) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    pub(super) fn maybe_current_text(&self) -> GcCellRef<Option<SourceTextAsChars>> {
        self.current_text.borrow()
    }

    pub(super) fn current_text(&self) -> GcCellRef<SourceTextAsChars> {
        gc_cell_ref_unwrapped(&self.current_text)
    }

    pub(super) fn maybe_current_text_mut(&self) -> GcCellRefMut<Option<SourceTextAsChars>> {
        self.current_text.borrow_mut()
    }

    pub(super) fn current_text_mut(
        &self,
    ) -> GcCellRefMut<Option<SourceTextAsChars>, SourceTextAsChars> {
        gc_cell_ref_mut_unwrapped(&self.current_text)
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

    pub(super) fn maybe_converted_loop_state(&self) -> GcCellRef<Option<ConvertedLoopState>> {
        self.converted_loop_state.borrow()
    }

    pub(super) fn maybe_converted_loop_state_mut(
        &self,
    ) -> GcCellRefMut<Option<ConvertedLoopState>> {
        self.converted_loop_state.borrow_mut()
    }

    pub(super) fn set_converted_loop_state(
        &self,
        converted_loop_state: Option<ConvertedLoopState>,
    ) {
        *self.converted_loop_state.borrow_mut() = converted_loop_state;
    }

    pub(super) fn converted_loop_state(&self) -> GcCellRef<ConvertedLoopState> {
        gc_cell_ref_unwrapped(&self.converted_loop_state)
    }

    pub(super) fn converted_loop_state_mut(
        &self,
    ) -> GcCellRefMut<Option<ConvertedLoopState>, ConvertedLoopState> {
        gc_cell_ref_mut_unwrapped(&self.converted_loop_state)
    }

    pub(super) fn maybe_tagged_template_string_declarations(
        &self,
    ) -> GcCellRef<Option<Vec<Gc<Node /*VariableDeclaration*/>>>> {
        self.tagged_template_string_declarations.borrow()
    }

    pub(super) fn maybe_tagged_template_string_declarations_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Node /*VariableDeclaration*/>>>> {
        self.tagged_template_string_declarations.borrow_mut()
    }

    pub(super) fn set_tagged_template_string_declarations(
        &self,
        tagged_template_string_declarations: Option<Vec<Gc<Node /*VariableDeclaration*/>>>,
    ) {
        *self.tagged_template_string_declarations.borrow_mut() =
            tagged_template_string_declarations;
    }

    pub(super) fn emit_helpers(&self) -> Rc<EmitHelperFactory> {
        self.context.get_emit_helper_factory()
    }

    pub(super) fn record_tagged_template_string(&self, temp: &Node /*Identifier*/) {
        self.maybe_tagged_template_string_declarations_mut()
            .get_or_insert_with(|| Default::default())
            .push(
                self.factory
                    .create_variable_declaration(Some(temp.node_wrapper()), None, None, None)
                    .wrap(),
            );
    }

    pub(super) fn transform_source_file(
        &self,
        node: &Node, /*SourceFile*/
    ) -> io::Result<Gc<Node>> {
        let node_as_source_file = node.as_source_file();
        if node_as_source_file.is_declaration_file() {
            return Ok(node.node_wrapper());
        }

        self.set_current_source_file(Some(node.node_wrapper()));
        self.set_current_text(Some(node_as_source_file.text_as_chars().clone()));

        let visited = self
            .visit_source_file(node)?
            .add_emit_helpers(self.context.read_emit_helpers().as_deref());

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
        node: &Node,
    ) -> bool {
        self.maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::ConstructorWithCapturedSuper)
            && node.kind() == SyntaxKind::ReturnStatement
            && node.as_return_statement().expression.is_none()
    }

    pub(super) fn is_or_may_contain_return_completion(&self, node: &Node) -> bool {
        node.transform_flags()
            .intersects(TransformFlags::ContainsHoistedDeclarationOrCompletion)
            && (is_return_statement(node)
                || is_if_statement(node)
                || is_with_statement(node)
                || is_switch_statement(node)
                || is_case_block(node)
                || is_case_clause(node)
                || is_default_clause(node)
                || is_try_statement(node)
                || is_catch_clause(node)
                || is_labeled_statement(node)
                || is_iteration_statement(node, false)
                || is_block(node))
    }

    pub(super) fn should_visit_node(&self, node: &Node) -> bool {
        node.transform_flags()
            .intersects(TransformFlags::ContainsES2015)
            || self.maybe_converted_loop_state().is_some()
            || self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::ConstructorWithCapturedSuper)
                && self.is_or_may_contain_return_completion(node)
            || is_iteration_statement(node, false) && self.should_convert_iteration_statement(node)
            || get_emit_flags(node).intersects(EmitFlags::TypeScriptClassWrapper)
    }

    pub(super) fn visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/ {
        Ok(if self.should_visit_node(node) {
            self.visitor_worker(node, false)?
        } else {
            Some(node.node_wrapper().into())
        })
    }

    pub(super) fn visitor_with_unused_expression_result(
        &self,
        node: &Node,
    ) -> io::Result<VisitResult> /*<Node>*/ {
        Ok(if self.should_visit_node(node) {
            self.visitor_worker(node, true)?
        } else {
            Some(node.node_wrapper().into())
        })
    }

    pub(super) fn class_wrapper_statement_visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/
    {
        if self.should_visit_node(node) {
            let ref original =
                get_original_node(Some(node), Option::<fn(Option<Gc<Node>>) -> bool>::None)
                    .unwrap();
            if is_property_declaration(original) && has_static_modifier(original) {
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
        Ok(Some(node.node_wrapper().into()))
    }

    pub(super) fn call_expression_visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/
    {
        if node.kind() == SyntaxKind::SuperKeyword {
            return Ok(Some(self.visit_super_keyword(true).into()));
        }
        self.visitor(node)
    }

    pub(super) fn visitor_worker(
        &self,
        node: &Node,
        expression_result_is_unused: bool,
    ) -> io::Result<VisitResult> /*<Node>*/ {
        Ok(match node.kind() {
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
                self.visit_do_or_while_statement(node, Option::<&Node>::None)
            }
            SyntaxKind::ForStatement => self.visit_for_statement(node, Option::<&Node>::None),
            SyntaxKind::ForInStatement => self.visit_for_in_statement(node, Option::<&Node>::None),
            SyntaxKind::ForOfStatement => self.visit_for_of_statement(node, Option::<&Node>::None),
            SyntaxKind::ExpressionStatement => Some(self.visit_expression_statement(node)?.into()),
            SyntaxKind::ObjectLiteralExpression => {
                Some(self.visit_object_literal_expression(node).into())
            }
            SyntaxKind::CatchClause => Some(self.visit_catch_clause(node).into()),
            SyntaxKind::ShorthandPropertyAssignment => {
                Some(self.visit_shorthand_property_assignment(node).into())
            }
            SyntaxKind::ComputedPropertyName => self.visit_computed_property_name(node),
            SyntaxKind::ArrayLiteralExpression => {
                Some(self.visit_array_literal_expression(node).into())
            }
            SyntaxKind::CallExpression => self.visit_call_expression(node),
            SyntaxKind::NewExpression => Some(self.visit_new_expression(node).into()),
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
            SyntaxKind::TaggedTemplateExpression => self.visit_tagged_template_expression(node),
            SyntaxKind::TemplateExpression => Some(self.visit_template_expression(node).into()),
            SyntaxKind::YieldExpression => Some(self.visit_yield_expression(node).into()),
            SyntaxKind::SpreadElement => self.visit_spread_element(node),
            SyntaxKind::SuperKeyword => Some(self.visit_super_keyword(false).into()),
            SyntaxKind::ThisKeyword => Some(self.visit_this_keyword(node).into()),
            SyntaxKind::MetaProperty => self.visit_meta_property(node),
            SyntaxKind::MethodDeclaration => Some(self.visit_method_declaration(node).into()),
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                Some(self.visit_accessor_declaration(node).into())
            }
            SyntaxKind::VariableStatement => self.visit_variable_statement(node)?.map(Into::into),
            SyntaxKind::ReturnStatement => Some(self.visit_return_statement(node)?.into()),
            SyntaxKind::VoidExpression => Some(self.visit_void_expression(node)?.into()),
            _ => try_maybe_visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
            )?
            .map(Into::into),
        })
    }
}

impl TransformerInterface for TransformES2015 {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        self.transform_source_file(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformES2015OnEmitNodeOverrider {
    transform_es2015: Gc<Box<TransformES2015>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformES2015OnEmitNodeOverrider {
    fn new(
        transform_es2015: Gc<Box<TransformES2015>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es2015,
            previous_on_emit_node,
        }
    }

    fn is_super_container(&self, node: &Node) -> bool {
        let kind = node.kind();
        matches!(
            kind,
            SyntaxKind::ClassDeclaration
                | SyntaxKind::Constructor
                | SyntaxKind::MethodDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor
        )
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformES2015OnEmitNodeOverrider {
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
struct TransformES2015OnSubstituteNodeOverrider {
    transform_es2015: Gc<Box<TransformES2015>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformES2015OnSubstituteNodeOverrider {
    fn new(
        transform_es2015: Gc<Box<TransformES2015>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es2015,
            previous_on_substitute_node,
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider for TransformES2015OnSubstituteNodeOverrider {
    fn on_substitute_node(&self, _hint: EmitHint, _node: &Node) -> io::Result<Gc<Node>> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2015Factory {}

impl TransformES2015Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2015Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformES2015::new(context).as_transformer(),
        )
    }
}

pub fn transform_es2015() -> TransformerFactory {
    Gc::new(Box::new(TransformES2015Factory::new()))
}
