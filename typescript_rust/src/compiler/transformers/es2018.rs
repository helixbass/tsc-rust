use std::{
    borrow::Borrow,
    cell::Cell,
    collections::{HashMap, HashSet},
    io, mem,
};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use super::create_super_access_variable_statement;
use crate::{
    is_assignment_pattern, is_concise_body, is_expression, is_for_initializer, is_statement,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface, __String,
    add_emit_helper, add_emit_helpers, add_range, advanced_async_super_helper, async_super_helper,
    chain_bundle, create_for_of_binding_statement, flatten_destructuring_assignment,
    flatten_destructuring_binding, gc_cell_ref_unwrapped, get_emit_script_target, get_node_id,
    has_syntactic_modifier, insert_statements_after_standard_prologue, is_binding_pattern,
    is_block, is_destructuring_assignment, is_effective_strict_mode_source_file, is_identifier,
    is_modifier, is_object_literal_element_like, is_property_access_expression, is_property_name,
    is_super_property, is_token, is_variable_declaration_list, maybe_visit_each_child,
    maybe_visit_node, maybe_visit_nodes, process_tagged_template_expression, set_emit_flags,
    set_original_node, set_text_range_node_array, set_text_range_rc_node, skip_parentheses, some,
    unwrap_innermost_statement_of_label, visit_each_child, visit_iteration_body,
    visit_lexical_environment, visit_node, visit_parameter_list, BaseNodeFactorySynthetic,
    CompilerOptions, Debug_, EmitFlags, EmitHelperFactory, EmitHint, EmitResolver, FlattenLevel,
    FunctionFlags, FunctionLikeDeclarationInterface, GeneratedIdentifierFlags, GetOrInsertDefault,
    HasInitializerInterface, HasStatementsInterface, Matches, ModifierFlags,
    NamedDeclarationInterface, Node, NodeArray, NodeArrayExt, NodeArrayOrVec, NodeCheckFlags,
    NodeExt, NodeFactory, NodeFlags, NodeId, NodeInterface, ProcessLevel, ReadonlyTextRange,
    ReadonlyTextRangeConcrete, ScriptTarget, SignatureDeclarationInterface, SyntaxKind,
    TransformFlags, TransformationContext, VecExt, VecExtClone, VisitResult, With,
};

bitflags! {
    #[derive(Default)]
    struct ESNextSubstitutionFlags: u32 {
        const None = 0;
        const AsyncMethodsWithSuper = 1 << 0;
    }
}

bitflags! {
    #[derive(Default)]
    struct HierarchyFacts: u32 {
        const None = 0;

        const HasLexicalThis = 1 << 0;
        const IterationContainer = 1 << 1;

        const AncestorFactsMask = (Self::IterationContainer.bits << 1) - 1;

        const SourceFileIncludes = Self::HasLexicalThis.bits;
        const SourceFileExcludes = Self::IterationContainer.bits;
        const StrictModeSourceFileIncludes = Self::None.bits;

        const ClassOrFunctionIncludes = Self::HasLexicalThis.bits;
        const ClassOrFunctionExcludes = Self::IterationContainer.bits;

        const ArrowFunctionIncludes = Self::None.bits;
        const ArrowFunctionExcludes = Self::ClassOrFunctionExcludes.bits;

        const IterationStatementIncludes = Self::IterationContainer.bits;
        const IterationStatementExcludes = Self::None.bits;
    }
}

#[derive(Trace, Finalize)]
struct TransformES2018 {
    _transformer_wrapper: GcCell<Option<Transformer>>,
    context: Gc<Box<dyn TransformationContext>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    base_factory: Gc<BaseNodeFactorySynthetic>,
    resolver: Gc<Box<dyn EmitResolver>>,
    compiler_options: Gc<CompilerOptions>,
    #[unsafe_ignore_trace]
    language_version: ScriptTarget,
    #[unsafe_ignore_trace]
    exported_variable_statement: Cell<bool>,
    #[unsafe_ignore_trace]
    enabled_substitutions: Cell<ESNextSubstitutionFlags>,
    #[unsafe_ignore_trace]
    enclosing_function_flags: Cell<Option<FunctionFlags>>,
    #[unsafe_ignore_trace]
    enclosing_super_container_flags: Cell<NodeCheckFlags>,
    #[unsafe_ignore_trace]
    hierarchy_facts: Cell<HierarchyFacts>,
    current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    tagged_template_string_declarations: GcCell<Option<Vec<Gc<Node /*VariableDeclaration*/>>>>,
    captured_super_properties: GcCell<Option<HashSet<__String>>>,
    #[unsafe_ignore_trace]
    has_super_element_access: Cell<bool>,
    substituted_super_accessors: GcCell<HashMap<NodeId, bool>>,
}

impl TransformES2018 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let compiler_options = context.get_compiler_options();
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            factory: context.factory(),
            base_factory: context.base_factory(),
            resolver: context.get_emit_resolver(),
            language_version: get_emit_script_target(&compiler_options),
            compiler_options,
            context: context.clone(),
            exported_variable_statement: Default::default(),
            enabled_substitutions: Default::default(),
            enclosing_function_flags: Default::default(),
            enclosing_super_container_flags: Default::default(),
            hierarchy_facts: Default::default(),
            current_source_file: Default::default(),
            tagged_template_string_declarations: Default::default(),
            captured_super_properties: Default::default(),
            has_super_element_access: Default::default(),
            substituted_super_accessors: Default::default(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformES2018OnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformES2018OnSubstituteNodeOverrider::new(
                downcasted.clone(),
                previous_on_substitute_node,
            )))
        });
        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    fn exported_variable_statement(&self) -> bool {
        self.exported_variable_statement.get()
    }

    fn set_exported_variable_statement(&self, exported_variable_statement: bool) {
        self.exported_variable_statement
            .set(exported_variable_statement);
    }

    fn enabled_substitutions(&self) -> ESNextSubstitutionFlags {
        self.enabled_substitutions.get()
    }

    fn set_enabled_substitutions(&self, enabled_substitutions: ESNextSubstitutionFlags) {
        self.enabled_substitutions.set(enabled_substitutions);
    }

    fn maybe_enclosing_function_flags(&self) -> Option<FunctionFlags> {
        self.enclosing_function_flags.get()
    }

    fn set_enclosing_function_flags(&self, enclosing_function_flags: Option<FunctionFlags>) {
        self.enclosing_function_flags.set(enclosing_function_flags);
    }

    fn enclosing_super_container_flags(&self) -> NodeCheckFlags {
        self.enclosing_super_container_flags.get()
    }

    fn set_enclosing_super_container_flags(&self, enclosing_super_container_flags: NodeCheckFlags) {
        self.enclosing_super_container_flags
            .set(enclosing_super_container_flags);
    }

    fn hierarchy_facts(&self) -> HierarchyFacts {
        self.hierarchy_facts.get()
    }

    fn set_hierarchy_facts(&self, hierarchy_facts: HierarchyFacts) {
        self.hierarchy_facts.set(hierarchy_facts);
    }

    fn current_source_file(&self) -> Gc<Node> {
        self.current_source_file.borrow().clone().unwrap()
    }

    fn set_current_source_file(&self, current_source_file: Option<Gc<Node>>) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    fn maybe_tagged_template_string_declarations(&self) -> GcCellRef<Option<Vec<Gc<Node>>>> {
        self.tagged_template_string_declarations.borrow()
    }

    fn maybe_tagged_template_string_declarations_mut(&self) -> GcCellRefMut<Option<Vec<Gc<Node>>>> {
        self.tagged_template_string_declarations.borrow_mut()
    }

    fn set_tagged_template_string_declarations(
        &self,
        tagged_template_string_declarations: Option<Vec<Gc<Node>>>,
    ) {
        *self.tagged_template_string_declarations.borrow_mut() =
            tagged_template_string_declarations;
    }

    fn maybe_captured_super_properties(&self) -> GcCellRef<Option<HashSet<String>>> {
        self.captured_super_properties.borrow()
    }

    fn captured_super_properties(&self) -> GcCellRef<HashSet<String>> {
        gc_cell_ref_unwrapped(&self.captured_super_properties)
    }

    fn maybe_captured_super_properties_mut(&self) -> GcCellRefMut<Option<HashSet<String>>> {
        self.captured_super_properties.borrow_mut()
    }

    fn set_captured_super_properties(&self, captured_super_properties: Option<HashSet<String>>) {
        *self.captured_super_properties.borrow_mut() = captured_super_properties;
    }

    fn has_super_element_access(&self) -> bool {
        self.has_super_element_access.get()
    }

    fn set_has_super_element_access(&self, has_super_element_access: bool) {
        self.has_super_element_access.set(has_super_element_access);
    }

    fn substituted_super_accessors(&self) -> GcCellRef<HashMap<NodeId, bool>> {
        self.substituted_super_accessors.borrow()
    }

    fn substituted_super_accessors_mut(&self) -> GcCellRefMut<HashMap<NodeId, bool>> {
        self.substituted_super_accessors.borrow_mut()
    }

    fn emit_helpers(&self) -> Gc<EmitHelperFactory> {
        self.context.get_emit_helper_factory()
    }

    fn affects_subtree(
        &self,
        exclude_facts: HierarchyFacts,
        include_facts: HierarchyFacts,
    ) -> bool {
        self.hierarchy_facts() != (self.hierarchy_facts() & !exclude_facts | include_facts)
    }

    fn enter_subtree(
        &self,
        exclude_facts: HierarchyFacts,
        include_facts: HierarchyFacts,
    ) -> HierarchyFacts {
        let ancestor_facts = self.hierarchy_facts();
        self.set_hierarchy_facts(
            (self.hierarchy_facts() & !exclude_facts | include_facts)
                & HierarchyFacts::AncestorFactsMask,
        );
        ancestor_facts
    }

    fn exit_subtree(&self, ancestor_facts: HierarchyFacts) {
        self.set_hierarchy_facts(ancestor_facts);
    }

    fn record_tagged_template_string(&self, temp: &Node /*Identifier*/) {
        self.maybe_tagged_template_string_declarations_mut()
            .get_or_insert_default_()
            .push(self.factory.create_variable_declaration(
                Some(temp.node_wrapper()),
                None,
                None,
                None,
            ));
    }

    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        if node_as_source_file.is_declaration_file() {
            return node.node_wrapper();
        }

        self.set_current_source_file(Some(node.node_wrapper()));
        let visited = self.visit_source_file(node);
        add_emit_helpers(&visited, self.context.read_emit_helpers().as_deref());

        self.set_current_source_file(None);
        self.set_tagged_template_string_declarations(None);
        visited
    }

    fn visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, false)
    }

    fn visitor_with_unused_expression_result(&self, node: &Node) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, true)
    }

    fn visitor_no_async_modifier(&self, node: &Node) -> VisitResult /*<Node>*/ {
        if node.kind() == SyntaxKind::AsyncKeyword {
            return None;
        }
        Some(node.node_wrapper().into())
    }

    fn do_with_hierarchy_facts<TValue, TReturn>(
        &self,
        mut cb: impl FnMut(&TValue) -> TReturn,
        value: &TValue,
        exclude_facts: HierarchyFacts,
        include_facts: HierarchyFacts,
    ) -> TReturn {
        if self.affects_subtree(exclude_facts, include_facts) {
            let ancestor_facts = self.enter_subtree(exclude_facts, include_facts);
            let result = cb(value);
            self.exit_subtree(ancestor_facts);
            return result;
        }
        cb(value)
    }

    fn visit_default(&self, node: &Node) -> VisitResult /*<Node>*/ {
        maybe_visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
        )
        .map(Into::into)
    }

    fn visitor_worker(&self, node: &Node, expression_result_is_unused: bool) -> VisitResult /*<Node>*/
    {
        if !node
            .transform_flags()
            .intersects(TransformFlags::ContainsES2018)
        {
            return Some(node.node_wrapper().into());
        }
        match node.kind() {
            SyntaxKind::AwaitExpression => Some(self.visit_await_expression(node).into()),
            SyntaxKind::YieldExpression => self.visit_yield_expression(node),
            SyntaxKind::ReturnStatement => self.visit_return_statement(node),
            SyntaxKind::LabeledStatement => self.visit_labeled_statement(node),
            SyntaxKind::ObjectLiteralExpression => {
                Some(self.visit_object_literal_expression(node).into())
            }
            SyntaxKind::BinaryExpression => Some(
                self.visit_binary_expression(node, expression_result_is_unused)
                    .into(),
            ),
            SyntaxKind::CommaListExpression => Some(
                self.visit_comma_list_expression(node, expression_result_is_unused)
                    .into(),
            ),
            SyntaxKind::CatchClause => self.visit_catch_clause(node),
            SyntaxKind::VariableStatement => self.visit_variable_statement(node),
            SyntaxKind::VariableDeclaration => self.visit_variable_declaration(node),
            SyntaxKind::DoStatement | SyntaxKind::WhileStatement | SyntaxKind::ForInStatement => {
                self.do_with_hierarchy_facts(
                    |node: &Node| self.visit_default(node),
                    node,
                    HierarchyFacts::IterationStatementExcludes,
                    HierarchyFacts::IterationStatementIncludes,
                )
            }
            SyntaxKind::ForOfStatement => self.visit_for_of_statement(node, Option::<&Node>::None),
            SyntaxKind::ForStatement => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_for_statement(node),
                node,
                HierarchyFacts::IterationStatementExcludes,
                HierarchyFacts::IterationStatementIncludes,
            ),
            SyntaxKind::VoidExpression => self.visit_void_expression(node),
            SyntaxKind::Constructor => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_constructor_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::MethodDeclaration => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_method_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::GetAccessor => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_get_accessor_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::SetAccessor => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_set_accessor_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::FunctionDeclaration => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_function_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::FunctionExpression => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_function_expression(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::ArrowFunction => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_arrow_function(node),
                node,
                HierarchyFacts::ArrowFunctionExcludes,
                HierarchyFacts::ArrowFunctionIncludes,
            ),
            SyntaxKind::Parameter => Some(self.visit_parameter(node).into()),
            SyntaxKind::ExpressionStatement => Some(self.visit_expression_statement(node).into()),
            SyntaxKind::ParenthesizedExpression => Some(
                self.visit_parenthesized_expression(node, expression_result_is_unused)
                    .into(),
            ),
            SyntaxKind::TaggedTemplateExpression => self.visit_tagged_template_expression(node),
            SyntaxKind::PropertyAccessExpression => {
                if let Some(captured_super_properties) =
                    self.maybe_captured_super_properties_mut().as_mut()
                {
                    if is_property_access_expression(node)
                        && node.as_property_access_expression().expression.kind()
                            == SyntaxKind::SuperKeyword
                    {
                        captured_super_properties.insert(
                            node.as_property_access_expression()
                                .name
                                .as_member_name()
                                .escaped_text()
                                .to_owned(),
                        );
                    }
                }
                maybe_visit_each_child(
                    Some(node),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                )
                .map(Into::into)
            }
            SyntaxKind::ElementAccessExpression => {
                if self.maybe_captured_super_properties().is_some() {
                    if node.as_element_access_expression().expression.kind()
                        == SyntaxKind::SuperKeyword
                    {
                        self.set_has_super_element_access(true);
                    }
                }
                maybe_visit_each_child(
                    Some(node),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                )
                .map(Into::into)
            }
            SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => self
                .do_with_hierarchy_facts(
                    |node: &Node| self.visit_default(node),
                    node,
                    HierarchyFacts::ClassOrFunctionExcludes,
                    HierarchyFacts::ClassOrFunctionIncludes,
                ),
            _ => maybe_visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
            )
            .map(Into::into),
        }
    }

    fn visit_await_expression(
        &self,
        node: &Node, /*AwaitExpression*/
    ) -> Gc<Node /*Expression*/> {
        if self
            .maybe_enclosing_function_flags()
            .matches(|enclosing_function_flags| {
                enclosing_function_flags.intersects(FunctionFlags::Async)
            })
            && self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
        {
            return set_original_node(
                self.factory
                    .create_yield_expression(
                        None,
                        Some(self.emit_helpers().create_await_helper(visit_node(
                            &node.as_await_expression().expression,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_expression),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        ))),
                    )
                    .set_text_range(Some(node)),
                Some(node.node_wrapper()),
            );
        }
        visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    fn visit_yield_expression(&self, node: &Node /*YieldExpression*/) -> VisitResult {
        let node_as_yield_expression = node.as_yield_expression();
        if self
            .maybe_enclosing_function_flags()
            .matches(|enclosing_function_flags| {
                enclosing_function_flags.intersects(FunctionFlags::Async)
            })
            && self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
        {
            if node_as_yield_expression.asterisk_token.is_some() {
                let expression = visit_node(
                    Debug_.check_defined(node_as_yield_expression.expression.as_deref(), None),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                );

                return Some(
                    set_original_node(
                        self.factory
                            .create_yield_expression(
                                None,
                                Some(self.emit_helpers().create_await_helper(
                                    self.factory.update_yield_expression(
                                        node,
                                        node_as_yield_expression.asterisk_token.clone(),
                                        Some(set_text_range_rc_node(
                                            self.emit_helpers().create_async_delegator_helper(
                                                set_text_range_rc_node(
                                                    self.emit_helpers().create_async_values_helper(
                                                        expression.clone(),
                                                    ),
                                                    Some(&*expression),
                                                ),
                                            ),
                                            Some(&*expression),
                                        )),
                                    ),
                                )),
                            )
                            .set_text_range(Some(node)),
                        Some(node.node_wrapper()),
                    )
                    .into(),
                );
            }
        }

        Some(
            set_original_node(
                self.factory
                    .create_yield_expression(
                        None,
                        Some(self.create_downlevel_await(
                            &node_as_yield_expression.expression.as_ref().map_or_else(
                                || self.factory.create_void_zero(),
                                |node_expression| {
                                    visit_node(
                                        node_expression,
                                        Some(|node: &Node| self.visitor(node)),
                                        Some(is_expression),
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    )
                                },
                            ),
                        )),
                    )
                    .set_text_range(Some(node)),
                Some(node.node_wrapper()),
            )
            .into(),
        )
    }

    fn visit_return_statement(&self, node: &Node /*ReturnStatement*/) -> VisitResult {
        let node_as_return_statement = node.as_return_statement();
        if self
            .maybe_enclosing_function_flags()
            .matches(|enclosing_function_flags| {
                enclosing_function_flags.intersects(FunctionFlags::Async)
            })
            && self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
        {
            return Some(
                self.factory
                    .update_return_statement(
                        node,
                        Some(self.create_downlevel_await(
                            &node_as_return_statement.expression.as_ref().map_or_else(
                                || self.factory.create_void_zero(),
                                |node_expression| {
                                    visit_node(
                                        node_expression,
                                        Some(|node: &Node| self.visitor(node)),
                                        Some(is_expression),
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    )
                                },
                            ),
                        )),
                    )
                    .into(),
            );
        }

        maybe_visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
        )
        .map(Into::into)
    }

    fn visit_labeled_statement(&self, node: &Node /*LabeledStatement*/) -> VisitResult {
        if self
            .maybe_enclosing_function_flags()
            .matches(|enclosing_function_flags| {
                enclosing_function_flags.intersects(FunctionFlags::Async)
            })
        {
            let ref statement =
                unwrap_innermost_statement_of_label(node, Option::<fn(&Node)>::None);
            if statement.kind() == SyntaxKind::ForOfStatement
                && statement.as_for_of_statement().await_modifier.is_some()
            {
                return self.visit_for_of_statement(statement, Some(node));
            }
            return Some(
                self.factory
                    .restore_enclosing_label(
                        &visit_node(
                            statement,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_statement),
                            Some(|nodes: &[Gc<Node>]| self.factory.lift_to_block(nodes)),
                        ),
                        Some(node),
                        Option::<fn(&Node)>::None,
                    )
                    .into(),
            );
        }

        maybe_visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
        )
        .map(Into::into)
    }

    fn chunk_object_literal_elements(
        &self,
        elements: &[Gc<Node>], /*ObjectLiteralElementLike*/
    ) -> Vec<Gc<Node /*Expression*/>> {
        let mut chunk_object: Option<Vec<Gc<Node /*ObjectLiteralElementLike*/>>> =
            Default::default();
        let mut objects: Vec<Gc<Node /*Expression*/>> = Default::default();
        for e in elements {
            if e.kind() == SyntaxKind::SpreadAssignment {
                if let Some(chunk_object) = chunk_object.take() {
                    objects.push(
                        self.factory
                            .create_object_literal_expression(Some(chunk_object), None),
                    );
                }
                let target = &e.as_spread_assignment().expression;
                objects.push(visit_node(
                    target,
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ));
            } else {
                chunk_object.get_or_insert_default_().push(
                    if e.kind() == SyntaxKind::PropertyAssignment {
                        let e_as_property_assignment = e.as_property_assignment();
                        self.factory.create_property_assignment(
                            e_as_property_assignment.name(),
                            visit_node(
                                &e_as_property_assignment.maybe_initializer().unwrap(),
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_expression),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            ),
                        )
                    } else {
                        visit_node(
                            e,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_object_literal_element_like),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        )
                    },
                );
            }
        }
        if let Some(chunk_object) = chunk_object {
            objects.push(
                self.factory
                    .create_object_literal_expression(Some(chunk_object), None),
            );
        }

        objects
    }

    fn visit_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) -> Gc<Node /*Expression*/> {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        if node
            .transform_flags()
            .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            let mut objects =
                self.chunk_object_literal_elements(&node_as_object_literal_expression.properties);
            if !objects.is_empty() && objects[0].kind() != SyntaxKind::ObjectLiteralExpression {
                objects.insert(
                    0,
                    self.factory
                        .create_object_literal_expression(Option::<Gc<NodeArray>>::None, None),
                );
            }
            let mut expression = objects.get(0).cloned();
            if objects.len() > 1 {
                for i in 1..objects.len() {
                    expression = Some(
                        self.emit_helpers()
                            .create_assign_helper(vec![expression.unwrap(), objects[i].clone()]),
                    );
                }
                return expression.unwrap();
            } else {
                return self.emit_helpers().create_assign_helper(objects);
            }
        }
        visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    fn visit_expression_statement(
        &self,
        node: &Node, /*ExpressionStatement*/
    ) -> Gc<Node /*ExpressionStatement*/> {
        visit_each_child(
            node,
            |node: &Node| self.visitor_with_unused_expression_result(node),
            &**self.context,
        )
    }

    fn visit_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
        expression_result_is_unused: bool,
    ) -> Gc<Node /*ParenthesizedExpression*/> {
        visit_each_child(
            node,
            |node: &Node| {
                if expression_result_is_unused {
                    self.visitor_with_unused_expression_result(node)
                } else {
                    self.visitor(node)
                }
            },
            &**self.context,
        )
    }

    fn visit_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node /*SourceFile*/> {
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::SourceFileExcludes,
            if is_effective_strict_mode_source_file(node, &self.compiler_options) {
                HierarchyFacts::StrictModeSourceFileIncludes
            } else {
                HierarchyFacts::SourceFileIncludes
            },
        );
        self.set_exported_variable_statement(false);
        let ref visited = visit_each_child(node, |node: &Node| self.visitor(node), &**self.context);
        let statement =
            visited
                .as_source_file()
                .statements()
                .with(|statements| -> NodeArrayOrVec {
                    if let Some(tagged_template_string_declarations) =
                        self.maybe_tagged_template_string_declarations().as_ref()
                    {
                        statements
                            .to_vec()
                            .and_push(self.factory.create_variable_statement(
                                Option::<Gc<NodeArray>>::None,
                                self.factory.create_variable_declaration_list(
                                    tagged_template_string_declarations.clone(),
                                    None,
                                ),
                            ))
                            .into()
                    } else {
                        statements.into()
                    }
                });
        let result = self.factory.update_source_file(
            visited,
            set_text_range_node_array(
                self.factory.create_node_array(Some(statement), None),
                Some(&*node.as_source_file().statements()),
            ),
            None,
            None,
            None,
            None,
            None,
        );
        self.exit_subtree(ancestor_facts);
        result
    }

    fn visit_tagged_template_expression(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        Some(
            process_tagged_template_expression(
                &**self.context,
                node,
                |node: &Node| self.visitor(node),
                &self.current_source_file(),
                |node: &Node| {
                    self.record_tagged_template_string(node);
                },
                ProcessLevel::LiftRestriction,
            )
            .into(),
        )
    }

    fn visit_binary_expression(
        &self,
        node: &Node, /*BinaryExpression*/
        expression_result_is_unused: bool,
    ) -> Gc<Node /*Expression*/> {
        let node_as_binary_expression = node.as_binary_expression();
        if is_destructuring_assignment(node)
            && node_as_binary_expression
                .left
                .transform_flags()
                .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            return flatten_destructuring_assignment(
                node,
                Some(|node: &Node| self.visitor(node)),
                &**self.context,
                FlattenLevel::ObjectRest,
                Some(!expression_result_is_unused),
                Option::<fn(&Node, &Node, Option<&dyn ReadonlyTextRange>) -> Gc<Node>>::None,
            );
        }
        if node_as_binary_expression.operator_token.kind() == SyntaxKind::CommaToken {
            return self.factory.update_binary_expression(
                node,
                visit_node(
                    &node_as_binary_expression.left,
                    Some(|node: &Node| self.visitor_with_unused_expression_result(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ),
                node_as_binary_expression.operator_token.clone(),
                visit_node(
                    &node_as_binary_expression.right,
                    Some(|node: &Node| {
                        if expression_result_is_unused {
                            self.visitor_with_unused_expression_result(node)
                        } else {
                            self.visitor(node)
                        }
                    }),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ),
            );
        }
        visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    fn visit_comma_list_expression(
        &self,
        node: &Node, /*CommaListExpression*/
        expression_result_is_unused: bool,
    ) -> Gc<Node /*Expression*/> {
        let node_as_comma_list_expression = node.as_comma_list_expression();
        if expression_result_is_unused {
            return visit_each_child(
                node,
                |node: &Node| self.visitor_with_unused_expression_result(node),
                &**self.context,
            );
        }
        let mut result: Option<Vec<Gc<Node /*Expression*/>>> = None;
        for (i, element) in node_as_comma_list_expression.elements.iter().enumerate() {
            let visited = visit_node(
                element,
                Some(|node: &Node| {
                    if i < node_as_comma_list_expression.elements.len() - 1 {
                        self.visitor_with_unused_expression_result(node)
                    } else {
                        self.visitor(node)
                    }
                }),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            );
            if result.is_some() || !Gc::ptr_eq(&visited, element) {
                result
                    .get_or_insert_with(|| node_as_comma_list_expression.elements[0..i].to_owned())
                    .push(visited);
            }
        }
        let elements = result.map_or_else(
            || node_as_comma_list_expression.elements.clone(),
            |result| {
                self.factory
                    .create_node_array(Some(result), None)
                    .set_text_range(Some(&*node_as_comma_list_expression.elements))
            },
        );
        self.factory.update_comma_list_expression(node, elements)
    }

    fn visit_catch_clause(&self, node: &Node /*CatchClause*/) -> VisitResult {
        let node_as_catch_clause = node.as_catch_clause();
        if let Some(node_variable_declaration) = node_as_catch_clause
            .variable_declaration
            .as_ref()
            .filter(|node_variable_declaration| {
                let node_variable_declaration_as_variable_declaration =
                    node_variable_declaration.as_variable_declaration();
                is_binding_pattern(node_variable_declaration_as_variable_declaration.maybe_name())
                    && node_variable_declaration_as_variable_declaration
                        .name()
                        .transform_flags()
                        .intersects(TransformFlags::ContainsObjectRestOrSpread)
            })
        {
            let node_variable_declaration_as_variable_declaration =
                node_variable_declaration.as_variable_declaration();
            let name = self.factory.get_generated_name_for_node(
                node_variable_declaration_as_variable_declaration.maybe_name(),
                None,
            );
            let updated_decl = self.factory.update_variable_declaration(
                node_variable_declaration,
                node_variable_declaration_as_variable_declaration.maybe_name(),
                None,
                None,
                Some(name.clone()),
            );
            let visited_bindings = flatten_destructuring_binding(
                &updated_decl,
                |node: &Node| self.visitor(node),
                &**self.context,
                FlattenLevel::ObjectRest,
                Option::<&Node>::None,
                None,
                None,
            );
            let mut block = visit_node(
                &node_as_catch_clause.block,
                Some(|node: &Node| self.visitor(node)),
                Some(is_block),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            );
            if !visited_bindings.is_empty() {
                block = self.factory.update_block(
                    &block,
                    vec![self.factory.create_variable_statement(
                        Option::<Gc<NodeArray>>::None,
                        visited_bindings,
                    )]
                    .and_extend(block.as_block().statements.iter().cloned()),
                );
            }
            return Some(
                self.factory
                    .update_catch_clause(
                        node,
                        Some(self.factory.update_variable_declaration(
                            node_variable_declaration,
                            Some(name),
                            None,
                            None,
                            None,
                        )),
                        block,
                    )
                    .into(),
            );
        }
        maybe_visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
        )
        .map(Into::into)
    }

    fn visit_variable_statement(&self, node: &Node /*VariableStatement*/) -> VisitResult /*<VariableStatement>*/
    {
        if has_syntactic_modifier(node, ModifierFlags::Export) {
            let saved_exported_variable_statement = self.exported_variable_statement();
            self.set_exported_variable_statement(true);
            let visited = visit_each_child(node, |node: &Node| self.visitor(node), &**self.context);
            self.set_exported_variable_statement(saved_exported_variable_statement);
            return Some(visited.into());
        }
        maybe_visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
        )
        .map(Into::into)
    }

    fn visit_variable_declaration(&self, node: &Node /*VariableDeclaration*/) -> VisitResult /*<VariableDeclaration>*/
    {
        if self.exported_variable_statement() {
            let saved_exported_variable_statement = self.exported_variable_statement();
            self.set_exported_variable_statement(false);
            let visited = self.visit_variable_declaration_worker(node, true);
            self.set_exported_variable_statement(saved_exported_variable_statement);
            return visited;
        }
        self.visit_variable_declaration_worker(node, false)
    }

    fn visit_variable_declaration_worker(
        &self,
        node: &Node, /*VariableDeclaration*/
        exported_variable_statement: bool,
    ) -> VisitResult /*<VariableDeclaration>*/ {
        let node_as_variable_declaration = node.as_variable_declaration();
        if is_binding_pattern(node_as_variable_declaration.maybe_name())
            && node_as_variable_declaration
                .name()
                .transform_flags()
                .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            return Some(
                flatten_destructuring_binding(
                    node,
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    FlattenLevel::ObjectRest,
                    Option::<&Node>::None,
                    Some(exported_variable_statement),
                    None,
                )
                .into(),
            );
        }
        maybe_visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
        )
        .map(Into::into)
    }

    fn visit_for_statement(&self, node: &Node /*ForStatement*/) -> VisitResult /*<Statement>*/ {
        let node_as_for_statement = node.as_for_statement();
        Some(
            self.factory
                .update_for_statement(
                    node,
                    maybe_visit_node(
                        node_as_for_statement.initializer.as_deref(),
                        Some(|node: &Node| self.visitor_with_unused_expression_result(node)),
                        Some(is_for_initializer),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    ),
                    maybe_visit_node(
                        node_as_for_statement.condition.as_deref(),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    ),
                    maybe_visit_node(
                        node_as_for_statement.incrementor.as_deref(),
                        Some(|node: &Node| self.visitor_with_unused_expression_result(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    ),
                    visit_iteration_body(
                        &node_as_for_statement.statement,
                        |node: &Node| self.visitor(node),
                        &**self.context,
                    ),
                )
                .into(),
        )
    }

    fn visit_void_expression(&self, node: &Node /*VoidExpression*/) -> VisitResult {
        maybe_visit_each_child(
            Some(node),
            |node: &Node| self.visitor_with_unused_expression_result(node),
            &**self.context,
        )
        .map(Into::into)
    }

    fn visit_for_of_statement(
        &self,
        node: &Node, /*ForOfStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult /*<Statement>*/ {
        let node_as_for_of_statement = node.as_for_of_statement();
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::IterationStatementExcludes,
            HierarchyFacts::IterationStatementIncludes,
        );
        let mut node = node.node_wrapper();
        if node_as_for_of_statement
            .initializer
            .transform_flags()
            .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            node = self.transform_for_of_statement_with_object_rest(&node);
        }
        let result = if node_as_for_of_statement.await_modifier.is_some() {
            self.transform_for_await_of_statement(
                &node,
                outermost_labeled_statement,
                ancestor_facts,
            )
        } else {
            Some(
                self.factory
                    .restore_enclosing_label(
                        &visit_each_child(&node, |node: &Node| self.visitor(node), &**self.context),
                        outermost_labeled_statement,
                        Option::<fn(&Node)>::None,
                    )
                    .into(),
            )
        };
        self.exit_subtree(ancestor_facts);
        result
    }

    fn transform_for_of_statement_with_object_rest(
        &self,
        node: &Node, /*ForOfStatement*/
    ) -> Gc<Node> {
        let node_as_for_of_statement = node.as_for_of_statement();
        let initializer_without_parens =
            skip_parentheses(&node_as_for_of_statement.initializer, None);
        if is_variable_declaration_list(&initializer_without_parens)
            || is_assignment_pattern(&initializer_without_parens)
        {
            let body_location: Option<Gc<Node /*TextRange*/>>;
            let statements_location: Option<ReadonlyTextRangeConcrete /*TextRange*/>;
            let temp = self
                .factory
                .create_temp_variable(Option::<fn(&Node)>::None, None);
            let mut statements: Vec<Gc<Node /*Statement*/>> =
                vec![create_for_of_binding_statement(
                    &self.factory,
                    &initializer_without_parens,
                    &temp,
                )];
            #[allow(clippy::suspicious_else_formatting)]
            if is_block(&node_as_for_of_statement.statement) {
                let node_statement_as_block = node_as_for_of_statement.statement.as_block();
                add_range(
                    &mut statements,
                    Some(&node_statement_as_block.statements),
                    None,
                    None,
                );
                body_location = Some(node_as_for_of_statement.statement.clone());
                statements_location = Some((&*node_statement_as_block.statements).into());
            } else
            /*if (node.statement)*/
            {
                statements.push(node_as_for_of_statement.statement.clone());
                body_location = Some(node_as_for_of_statement.statement.clone());
                statements_location = Some((&*node_as_for_of_statement.statement).into());
            }
            return self.factory.update_for_of_statement(
                node,
                node_as_for_of_statement.await_modifier.clone(),
                self.factory
                    .create_variable_declaration_list(
                        vec![self
                            .factory
                            .create_variable_declaration(Some(temp), None, None, None)
                            .set_text_range(Some(&*node_as_for_of_statement.initializer))],
                        Some(NodeFlags::Let),
                    )
                    .set_text_range(Some(&*node_as_for_of_statement.initializer)),
                node_as_for_of_statement.expression.clone(),
                self.factory
                    .create_block(
                        self.factory
                            .create_node_array(Some(statements), None)
                            .set_text_range(statements_location.as_ref()),
                        Some(true),
                    )
                    .set_text_range(body_location.as_deref()),
            );
        }
        node.node_wrapper()
    }

    fn convert_for_of_statement_head(
        &self,
        node: &Node,        /*ForOfStatement*/
        bound_value: &Node, /*Expression*/
    ) -> Gc<Node> {
        let node_as_for_of_statement = node.as_for_of_statement();
        let binding = create_for_of_binding_statement(
            &self.factory,
            &node_as_for_of_statement.initializer,
            bound_value,
        );

        let mut body_location: Option<Gc<Node /*TextRange*/>> = Default::default();
        let mut statements_location: Option<Gc<NodeArray /*TextRange*/>> = Default::default();
        let mut statements: Vec<Gc<Node /*Statement*/>> = vec![visit_node(
            &binding,
            Some(|node: &Node| self.visitor(node)),
            Some(is_statement),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )];
        let statement = visit_iteration_body(
            &node_as_for_of_statement.statement,
            |node: &Node| self.visitor(node),
            &**self.context,
        );
        if is_block(&statement) {
            let statement_as_block = statement.as_block();
            add_range(
                &mut statements,
                Some(&statement_as_block.statements),
                None,
                None,
            );
            body_location = Some(statement.clone());
            statements_location = Some(statement_as_block.statements.clone());
        } else {
            statements.push(statement);
        }

        self.factory
            .create_block(
                self.factory
                    .create_node_array(Some(statements), None)
                    .set_text_range(statements_location.as_deref()),
                Some(true),
            )
            .set_text_range(body_location.as_deref())
            .set_emit_flags(EmitFlags::NoSourceMap | EmitFlags::NoTokenSourceMaps)
    }

    fn create_downlevel_await(&self, expression: &Node /*Expression*/) -> Gc<Node> {
        if self
            .maybe_enclosing_function_flags()
            .matches(|enclosing_function_flags| {
                enclosing_function_flags.intersects(FunctionFlags::Generator)
            })
        {
            self.factory.create_yield_expression(
                None,
                Some(
                    self.emit_helpers()
                        .create_await_helper(expression.node_wrapper()),
                ),
            )
        } else {
            self.factory
                .create_await_expression(expression.node_wrapper())
        }
    }

    fn transform_for_await_of_statement(
        &self,
        node: &Node, /*ForOfStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
        ancestor_facts: HierarchyFacts,
    ) -> VisitResult {
        let node_as_for_of_statement = node.as_for_of_statement();
        let expression = visit_node(
            &node_as_for_of_statement.expression,
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        );
        let iterator = if is_identifier(&expression) {
            self.factory
                .get_generated_name_for_node(Some(&*expression), None)
        } else {
            self.factory
                .create_temp_variable(Option::<fn(&Node)>::None, None)
        };
        let result = if is_identifier(&expression) {
            self.factory
                .get_generated_name_for_node(Some(&*iterator), None)
        } else {
            self.factory
                .create_temp_variable(Option::<fn(&Node)>::None, None)
        };
        let error_record = self.factory.create_unique_name("e", None);
        let catch_variable = self
            .factory
            .get_generated_name_for_node(Some(&*error_record), None);
        let return_method = self
            .factory
            .create_temp_variable(Option::<fn(&Node)>::None, None);
        let call_values = self
            .emit_helpers()
            .create_async_values_helper(expression)
            .set_text_range(Some(&*node_as_for_of_statement.expression));
        let call_next = self.factory.create_call_expression(
            self.factory
                .create_property_access_expression(iterator.clone(), "next"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![]),
        );
        let get_done = self
            .factory
            .create_property_access_expression(result.clone(), "done");
        let get_value = self
            .factory
            .create_property_access_expression(result.clone(), "value");
        let call_return =
            self.factory
                .create_function_call_call(return_method.clone(), iterator.clone(), vec![]);

        self.context.hoist_variable_declaration(&error_record);
        self.context.hoist_variable_declaration(&return_method);

        let initializer = if ancestor_facts.intersects(HierarchyFacts::IterationContainer) {
            self.factory.inline_expressions(&[
                self.factory
                    .create_assignment(error_record.clone(), self.factory.create_void_zero()),
                call_values,
            ])
        } else {
            call_values
        };

        let for_statement = self
            .factory
            .create_for_statement(
                Some(
                    self.factory
                        .create_variable_declaration_list(
                            vec![
                                self.factory
                                    .create_variable_declaration(
                                        Some(iterator.clone()),
                                        None,
                                        None,
                                        Some(initializer),
                                    )
                                    .set_text_range(Some(&*node_as_for_of_statement.expression)),
                                self.factory.create_variable_declaration(
                                    Some(result.clone()),
                                    None,
                                    None,
                                    None,
                                ),
                            ],
                            None,
                        )
                        .set_text_range(Some(&*node_as_for_of_statement.expression))
                        .set_emit_flags(EmitFlags::NoHoisting),
                ),
                Some(
                    self.factory.create_comma(
                        self.factory.create_assignment(
                            result.clone(),
                            self.create_downlevel_await(&call_next),
                        ),
                        self.factory.create_logical_not(get_done.clone()),
                    ),
                ),
                None,
                self.convert_for_of_statement_head(node, &get_value),
            )
            .set_text_range(Some(node))
            .set_emit_flags(EmitFlags::NoTokenTrailingSourceMaps);

        Some(
            self.factory
                .create_try_statement(
                    self.factory.create_block(
                        vec![self.factory.restore_enclosing_label(
                            &for_statement,
                            outermost_labeled_statement,
                            Option::<fn(&Node)>::None,
                        )],
                        None,
                    ),
                    Some(
                        self.factory.create_catch_clause(
                            Some(self.factory.create_variable_declaration(
                                Some(catch_variable.clone()),
                                None,
                                None,
                                None,
                            )),
                            self.factory
                                .create_block(
                                    vec![self.factory.create_expression_statement(
                                        self.factory.create_assignment(
                                            error_record.clone(),
                                            self.factory.create_object_literal_expression(
                                                Some(vec![self
                                                    .factory
                                                    .create_property_assignment(
                                                        "error",
                                                        catch_variable,
                                                    )]),
                                                None,
                                            ),
                                        ),
                                    )],
                                    None,
                                )
                                .set_emit_flags(EmitFlags::SingleLine),
                        ),
                    ),
                    Some(self.factory.create_block(
                        vec![self.factory.create_try_statement(
                                self.factory.create_block(
                                    vec![self
                                        .factory
                                        .create_if_statement(
                                            self.factory.create_logical_and(
                                                self.factory.create_logical_and(
                                                    result,
                                                    self.factory.create_logical_not(get_done),
                                                ),
                                                self.factory.create_assignment(
                                                    return_method,
                                                    self.factory.create_property_access_expression(
                                                        iterator, "return",
                                                    ),
                                                ),
                                            ),
                                            self.factory.create_expression_statement(
                                                self.create_downlevel_await(&call_return),
                                            ),
                                            None,
                                        )
                                        .set_emit_flags(EmitFlags::SingleLine)],
                                    None,
                                ),
                                None,
                                Some(
                                    self.factory
                                        .create_block(
                                            vec![self
                                                .factory
                                                .create_if_statement(
                                                    error_record.clone(),
                                                    self.factory.create_throw_statement(
                                                        self.factory
                                                            .create_property_access_expression(
                                                                error_record,
                                                                "error",
                                                            ),
                                                    ),
                                                    None,
                                                )
                                                .set_emit_flags(EmitFlags::SingleLine)],
                                            None,
                                        )
                                        .set_emit_flags(EmitFlags::SingleLine),
                                ),
                            )],
                        None,
                    )),
                )
                .into(),
        )
    }

    fn visit_parameter(
        &self,
        node: &Node, /*ParameterDeclaration*/
    ) -> Gc<Node /*ParameterDeclaration*/> {
        let node_as_parameter_declaration = node.as_parameter_declaration();
        if node
            .transform_flags()
            .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            return self.factory.update_parameter_declaration(
                node,
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                node_as_parameter_declaration.dot_dot_dot_token.clone(),
                Some(self.factory.get_generated_name_for_node(Some(node), None)),
                None,
                None,
                maybe_visit_node(
                    node_as_parameter_declaration.maybe_initializer(),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ),
            );
        }
        visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    fn visit_constructor_declaration(
        &self,
        node: &Node, /*ConstructorDeclaration*/
    ) -> VisitResult {
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.update_constructor_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            node.maybe_modifiers(),
            visit_parameter_list(
                Some(&node.as_constructor_declaration().parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            Some(self.transform_function_body(node)),
        );
        self.set_enclosing_function_flags(saved_enclosing_function_flags);
        Some(updated.into())
    }

    fn visit_get_accessor_declaration(
        &self,
        node: &Node, /*GetAccessorDeclaration*/
    ) -> VisitResult {
        let node_as_get_accessor_declaration = node.as_get_accessor_declaration();
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.update_get_accessor_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            node.maybe_modifiers(),
            visit_node(
                &node_as_get_accessor_declaration.name(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_property_name),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            ),
            visit_parameter_list(
                Some(&node_as_get_accessor_declaration.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            Some(self.transform_function_body(node)),
        );
        self.set_enclosing_function_flags(saved_enclosing_function_flags);
        Some(updated.into())
    }

    fn visit_set_accessor_declaration(
        &self,
        node: &Node, /*SetAccessorDeclaration*/
    ) -> VisitResult {
        let node_as_set_accessor_declaration = node.as_set_accessor_declaration();
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.update_set_accessor_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            node.maybe_modifiers(),
            visit_node(
                &node_as_set_accessor_declaration.name(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_property_name),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            ),
            visit_parameter_list(
                Some(&node_as_set_accessor_declaration.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            Some(self.transform_function_body(node)),
        );
        self.set_enclosing_function_flags(saved_enclosing_function_flags);
        Some(updated.into())
    }

    fn visit_method_declaration(&self, node: &Node /*MethodDeclaration*/) -> VisitResult {
        let node_as_method_declaration = node.as_method_declaration();
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.update_method_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            if self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
            {
                maybe_visit_nodes(
                    node.maybe_modifiers().as_deref(),
                    Some(|node: &Node| self.visitor_no_async_modifier(node)),
                    Some(is_modifier),
                    None,
                    None,
                )
            } else {
                node.maybe_modifiers()
            },
            if self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Async)
                })
            {
                None
            } else {
                node_as_method_declaration.maybe_asterisk_token()
            },
            visit_node(
                &node_as_method_declaration.name(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_property_name),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            ),
            maybe_visit_node(
                Option::<&Node>::None,
                Some(|node: &Node| self.visitor(node)),
                Some(is_token),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            ),
            Option::<Gc<NodeArray>>::None,
            visit_parameter_list(
                Some(&node_as_method_declaration.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            Some(
                if self
                    .maybe_enclosing_function_flags()
                    .matches(|enclosing_function_flags| {
                        enclosing_function_flags.intersects(FunctionFlags::Async)
                    })
                    && self
                        .maybe_enclosing_function_flags()
                        .matches(|enclosing_function_flags| {
                            enclosing_function_flags.intersects(FunctionFlags::Generator)
                        })
                {
                    self.transform_async_generator_function_body(node)
                } else {
                    self.transform_function_body(node)
                },
            ),
        );
        self.set_enclosing_function_flags(saved_enclosing_function_flags);
        Some(updated.into())
    }

    fn visit_function_declaration(&self, node: &Node /*FunctionDeclaration*/) -> VisitResult {
        let node_as_function_declaration = node.as_function_declaration();
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.update_function_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            if self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
            {
                maybe_visit_nodes(
                    node.maybe_modifiers().as_deref(),
                    Some(|node: &Node| self.visitor_no_async_modifier(node)),
                    Some(is_modifier),
                    None,
                    None,
                )
            } else {
                node.maybe_modifiers()
            },
            if self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Async)
                })
            {
                None
            } else {
                node_as_function_declaration.maybe_asterisk_token()
            },
            node_as_function_declaration.maybe_name(),
            Option::<Gc<NodeArray>>::None,
            visit_parameter_list(
                Some(&node_as_function_declaration.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            Some(
                if self
                    .maybe_enclosing_function_flags()
                    .matches(|enclosing_function_flags| {
                        enclosing_function_flags.intersects(FunctionFlags::Async)
                    })
                    && self
                        .maybe_enclosing_function_flags()
                        .matches(|enclosing_function_flags| {
                            enclosing_function_flags.intersects(FunctionFlags::Generator)
                        })
                {
                    self.transform_async_generator_function_body(node)
                } else {
                    self.transform_function_body(node)
                },
            ),
        );
        self.set_enclosing_function_flags(saved_enclosing_function_flags);
        Some(updated.into())
    }

    fn visit_arrow_function(&self, node: &Node /*ArrowFunction*/) -> VisitResult {
        let node_as_arrow_function = node.as_arrow_function();
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.update_arrow_function(
            node,
            node.maybe_modifiers(),
            Option::<Gc<NodeArray>>::None,
            visit_parameter_list(
                Some(&node_as_arrow_function.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            node_as_arrow_function.equals_greater_than_token.clone(),
            self.transform_function_body(node),
        );
        self.set_enclosing_function_flags(saved_enclosing_function_flags);
        Some(updated.into())
    }

    fn visit_function_expression(&self, node: &Node /*FunctionExpression*/) -> VisitResult {
        let node_as_function_expression = node.as_function_expression();
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.update_function_expression(
            node,
            if self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
            {
                maybe_visit_nodes(
                    node.maybe_modifiers().as_deref(),
                    Some(|node: &Node| self.visitor_no_async_modifier(node)),
                    Some(is_modifier),
                    None,
                    None,
                )
            } else {
                node.maybe_modifiers()
            },
            if self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Async)
                })
            {
                None
            } else {
                node_as_function_expression.maybe_asterisk_token()
            },
            node_as_function_expression.maybe_name(),
            Option::<Gc<NodeArray>>::None,
            visit_parameter_list(
                Some(&node_as_function_expression.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            if self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Async)
                })
                && self
                    .maybe_enclosing_function_flags()
                    .matches(|enclosing_function_flags| {
                        enclosing_function_flags.intersects(FunctionFlags::Generator)
                    })
            {
                self.transform_async_generator_function_body(node)
            } else {
                self.transform_function_body(node)
            },
        );
        self.set_enclosing_function_flags(saved_enclosing_function_flags);
        Some(updated.into())
    }

    fn transform_async_generator_function_body(
        &self,
        node: &Node, /*MethodDeclaration | AccessorDeclaration | FunctionDeclaration | FunctionExpression*/
    ) -> Gc<Node /*FunctionBody*/> {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        self.context.resume_lexical_environment();
        let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();
        let statement_offset = self.factory.copy_prologue(
            &node_as_function_like_declaration
                .maybe_body()
                .unwrap()
                .as_block()
                .statements,
            &mut statements,
            Some(false),
            Some(|node: &Node| self.visitor(node)),
        );
        let mut statements = self
            .append_object_rest_assignments_if_needed(Some(statements), node)
            .unwrap();

        let saved_captured_super_properties = self.maybe_captured_super_properties().clone();
        let saved_has_super_element_access = self.has_super_element_access();
        self.set_captured_super_properties(Some(Default::default()));
        self.set_has_super_element_access(false);

        let return_statement = self.factory.create_return_statement(Some(
            self.emit_helpers().create_async_generator_helper(
                self.factory.create_function_expression(
                    Option::<Gc<NodeArray>>::None,
                    Some(self.factory.create_token(SyntaxKind::AsteriskToken)),
                    node_as_function_like_declaration
                        .maybe_name()
                        .map(|node_name| {
                            self.factory
                                .get_generated_name_for_node(Some(node_name), None)
                        }),
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![]),
                    None,
                    self.factory.update_block(
                        &node_as_function_like_declaration.maybe_body().unwrap(),
                        visit_lexical_environment(
                            &node_as_function_like_declaration
                                .maybe_body()
                                .unwrap()
                                .as_block()
                                .statements,
                            |node: &Node| self.visitor(node),
                            &**self.context,
                            Some(statement_offset),
                            None,
                            Option::<
                                fn(
                                    Option<&NodeArray>,
                                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                                    Option<&dyn Fn(&Node) -> bool>,
                                    Option<usize>,
                                    Option<usize>,
                                ) -> Option<Gc<NodeArray>>,
                            >::None,
                        ),
                    ),
                ),
                self.hierarchy_facts()
                    .intersects(HierarchyFacts::HasLexicalThis),
            ),
        ));

        let emit_super_helpers = self.language_version >= ScriptTarget::ES2015
            && self.resolver.get_node_check_flags(node).intersects(
                NodeCheckFlags::AsyncMethodWithSuperBinding | NodeCheckFlags::AsyncMethodWithSuper,
            );

        if emit_super_helpers {
            self.enable_substitution_for_async_methods_with_super();
            let variable_statement = create_super_access_variable_statement(
                &self.factory,
                &**self.resolver,
                node,
                &self.captured_super_properties(),
            );
            self.substituted_super_accessors_mut()
                .insert(get_node_id(&variable_statement), true);
            insert_statements_after_standard_prologue(&mut statements, Some(&[variable_statement]));
        }

        statements.push(return_statement);

        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.end_lexical_environment().as_deref(),
        );
        let block = self.factory.update_block(
            &node_as_function_like_declaration.maybe_body().unwrap(),
            statements,
        );

        if emit_super_helpers && self.has_super_element_access() {
            if self
                .resolver
                .get_node_check_flags(node)
                .intersects(NodeCheckFlags::AsyncMethodWithSuperBinding)
            {
                add_emit_helper(&block, advanced_async_super_helper());
            } else if self
                .resolver
                .get_node_check_flags(node)
                .intersects(NodeCheckFlags::AsyncMethodWithSuper)
            {
                add_emit_helper(&block, async_super_helper());
            }
        }

        self.set_captured_super_properties(saved_captured_super_properties);
        self.set_has_super_element_access(saved_has_super_element_access);

        block
    }

    fn transform_function_body(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> Gc<Node /*ConciseBody*/> {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        self.context.resume_lexical_environment();
        let mut statement_offset = 0;
        let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();
        let body = maybe_visit_node(
            node_as_function_like_declaration.maybe_body(),
            Some(|node: &Node| self.visitor(node)),
            Some(is_concise_body),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )
        .unwrap_or_else(|| self.factory.create_block(vec![], None));
        if is_block(&body) {
            statement_offset = self.factory.copy_prologue(
                &body.as_block().statements,
                &mut statements,
                Some(false),
                Some(|node: &Node| self.visitor(node)),
            );
        }
        statements.add_range(
            self.append_object_rest_assignments_if_needed(None, node)
                .as_deref(),
            None,
            None,
        );
        let leading_statements = self.context.end_lexical_environment();
        if statement_offset > 0
            || !statements.is_empty()
            || some(
                leading_statements.as_deref(),
                Option::<fn(&Gc<Node>) -> bool>::None,
            )
        {
            let block = self
                .factory
                .converters()
                .convert_to_function_block(&body, Some(true));
            insert_statements_after_standard_prologue(
                &mut statements,
                leading_statements.as_deref(),
            );
            let block_as_block = block.as_block();
            statements.add_range(
                Some(&block_as_block.statements[statement_offset..]),
                None,
                None,
            );
            return self.factory.update_block(
                &block,
                self.factory
                    .create_node_array(Some(statements), None)
                    .set_text_range(Some(&*block_as_block.statements)),
            );
        }
        body
    }

    fn append_object_rest_assignments_if_needed(
        &self,
        mut statements: Option<Vec<Gc<Node /*Statement*/>>>,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> Option<Vec<Gc<Node /*Statement*/>>> {
        for parameter in &node.as_function_like_declaration().parameters() {
            if parameter
                .transform_flags()
                .intersects(TransformFlags::ContainsObjectRestOrSpread)
            {
                let temp = self
                    .factory
                    .get_generated_name_for_node(Some(&**parameter), None);
                let declarations = flatten_destructuring_binding(
                    parameter,
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    FlattenLevel::ObjectRest,
                    Some(temp),
                    Some(false),
                    Some(true),
                );
                if !declarations.is_empty() {
                    let statement = self.factory.create_variable_statement(
                        Option::<Gc<NodeArray>>::None,
                        self.factory
                            .create_variable_declaration_list(declarations, None),
                    );
                    set_emit_flags(&*statement, EmitFlags::CustomPrologue);
                    statements.get_or_insert_default_().push(statement);
                }
            }
        }
        statements
    }

    fn enable_substitution_for_async_methods_with_super(&self) {
        if !self
            .enabled_substitutions()
            .intersects(ESNextSubstitutionFlags::AsyncMethodsWithSuper)
        {
            self.set_enabled_substitutions(
                self.enabled_substitutions() | ESNextSubstitutionFlags::AsyncMethodsWithSuper,
            );

            self.context.enable_substitution(SyntaxKind::CallExpression);
            self.context
                .enable_substitution(SyntaxKind::PropertyAccessExpression);
            self.context
                .enable_substitution(SyntaxKind::ElementAccessExpression);

            self.context
                .enable_emit_notification(SyntaxKind::ClassDeclaration);
            self.context
                .enable_emit_notification(SyntaxKind::MethodDeclaration);
            self.context
                .enable_emit_notification(SyntaxKind::GetAccessor);
            self.context
                .enable_emit_notification(SyntaxKind::SetAccessor);
            self.context
                .enable_emit_notification(SyntaxKind::Constructor);
            self.context
                .enable_emit_notification(SyntaxKind::VariableStatement);
        }
    }
}

impl TransformerInterface for TransformES2018 {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(self.transform_source_file(node))
    }
}

#[derive(Trace, Finalize)]
struct TransformES2018OnEmitNodeOverrider {
    transform_es2018: Gc<Box<TransformES2018>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformES2018OnEmitNodeOverrider {
    fn new(
        transform_es2018: Gc<Box<TransformES2018>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es2018,
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

impl TransformationContextOnEmitNodeOverrider for TransformES2018OnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        if self
            .transform_es2018
            .enabled_substitutions()
            .intersects(ESNextSubstitutionFlags::AsyncMethodsWithSuper)
            && self.is_super_container(node)
        {
            let super_container_flags = self.transform_es2018.resolver.get_node_check_flags(node)
                & (NodeCheckFlags::AsyncMethodWithSuper
                    | NodeCheckFlags::AsyncMethodWithSuperBinding);
            if super_container_flags != self.transform_es2018.enclosing_super_container_flags() {
                let saved_enclosing_super_container_flags =
                    self.transform_es2018.enclosing_super_container_flags();
                self.transform_es2018
                    .set_enclosing_super_container_flags(super_container_flags);
                self.previous_on_emit_node
                    .on_emit_node(hint, node, emit_callback)?;
                self.transform_es2018
                    .set_enclosing_super_container_flags(saved_enclosing_super_container_flags);
                return Ok(());
            }
        } else if self.transform_es2018.enabled_substitutions() != ESNextSubstitutionFlags::None
            && self
                .transform_es2018
                .substituted_super_accessors()
                .get(&get_node_id(node))
                .copied()
                == Some(true)
        {
            let saved_enclosing_super_container_flags =
                self.transform_es2018.enclosing_super_container_flags();
            self.transform_es2018
                .set_enclosing_super_container_flags(NodeCheckFlags::None);
            self.previous_on_emit_node
                .on_emit_node(hint, node, emit_callback)?;
            self.transform_es2018
                .set_enclosing_super_container_flags(saved_enclosing_super_container_flags);
            return Ok(());
        }

        self.previous_on_emit_node
            .on_emit_node(hint, node, emit_callback)?;

        Ok(())
    }
}

#[derive(Trace, Finalize)]
struct TransformES2018OnSubstituteNodeOverrider {
    transform_es2018: Gc<Box<TransformES2018>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformES2018OnSubstituteNodeOverrider {
    fn new(
        transform_es2018: Gc<Box<TransformES2018>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es2018,
            previous_on_substitute_node,
        }
    }

    fn substitute_expression(&self, node: &Node /*Expression*/) -> Gc<Node> {
        match node.kind() {
            SyntaxKind::PropertyAccessExpression => {
                return self.substitute_property_access_expression(node);
            }
            SyntaxKind::ElementAccessExpression => {
                return self.substitute_element_access_expression(node);
            }
            SyntaxKind::CallExpression => {
                return self.substitute_call_expression(node);
            }
            _ => (),
        }
        node.node_wrapper()
    }

    fn substitute_property_access_expression(
        &self,
        node: &Node, /*PropertyAccessExpression*/
    ) -> Gc<Node> {
        let node_as_property_access_expression = node.as_property_access_expression();
        if node_as_property_access_expression.expression.kind() == SyntaxKind::SuperKeyword {
            return self
                .transform_es2018
                .factory
                .create_property_access_expression(
                    self.transform_es2018.factory.create_unique_name(
                        "_super",
                        Some(
                            GeneratedIdentifierFlags::Optimistic
                                | GeneratedIdentifierFlags::FileLevel,
                        ),
                    ),
                    node_as_property_access_expression.name.clone(),
                )
                .set_text_range(Some(node));
        }
        node.node_wrapper()
    }

    fn substitute_element_access_expression(
        &self,
        node: &Node, /*ElementAccessExpression*/
    ) -> Gc<Node> {
        let node_as_element_access_expression = node.as_element_access_expression();
        if node_as_element_access_expression.expression.kind() == SyntaxKind::SuperKeyword {
            return self.create_super_element_access_in_async_method(
                &node_as_element_access_expression.argument_expression,
                node,
            );
        }
        node.node_wrapper()
    }

    fn substitute_call_expression(&self, node: &Node /*CallExpression*/) -> Gc<Node> {
        let node_as_call_expression = node.as_call_expression();
        let expression = &node_as_call_expression.expression;
        if is_super_property(expression) {
            let argument_expression = if is_property_access_expression(expression) {
                self.substitute_property_access_expression(expression)
            } else {
                self.substitute_element_access_expression(expression)
            };
            return self.transform_es2018.factory.create_call_expression(
                self.transform_es2018
                    .factory
                    .create_property_access_expression(argument_expression, "call"),
                Option::<Gc<NodeArray>>::None,
                Some(
                    vec![self.transform_es2018.factory.create_this()]
                        .and_extend(node_as_call_expression.arguments.iter().cloned()),
                ),
            );
        }
        node.node_wrapper()
    }

    fn create_super_element_access_in_async_method(
        &self,
        argument_expression: &Node, /*Expression*/
        location: &impl ReadonlyTextRange,
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        if self
            .transform_es2018
            .enclosing_super_container_flags()
            .intersects(NodeCheckFlags::AsyncMethodWithSuperBinding)
        {
            self.transform_es2018
                .factory
                .create_property_access_expression(
                    self.transform_es2018.factory.create_call_expression(
                        self.transform_es2018
                            .factory
                            .create_identifier("_superIndex"),
                        Option::<Gc<NodeArray>>::None,
                        Some(vec![argument_expression.node_wrapper()]),
                    ),
                    "value",
                )
                .set_text_range(Some(location))
        } else {
            self.transform_es2018
                .factory
                .create_call_expression(
                    self.transform_es2018
                        .factory
                        .create_identifier("_superIndex"),
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![argument_expression.node_wrapper()]),
                )
                .set_text_range(Some(location))
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider for TransformES2018OnSubstituteNodeOverrider {
    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> io::Result<Gc<Node>> {
        let node = self
            .previous_on_substitute_node
            .on_substitute_node(hint, node)?;
        if hint == EmitHint::Expression
            && self.transform_es2018.enclosing_super_container_flags() != NodeCheckFlags::None
        {
            return Ok(self.substitute_expression(&node));
        }
        Ok(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformES2018Factory {}

impl TransformES2018Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2018Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformES2018::new(context).as_transformer(),
        )
    }
}

pub fn transform_es2018() -> TransformerFactory {
    Gc::new(Box::new(TransformES2018Factory::new()))
}
