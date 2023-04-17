use std::{borrow::Borrow, cell::Cell, collections::HashSet, mem, rc::Rc};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use crate::{
    is_expression, is_statement, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, __String, add_emit_helpers, chain_bundle,
    flatten_destructuring_assignment, get_emit_script_target, is_destructuring_assignment,
    is_effective_strict_mode_source_file, is_object_literal_element_like,
    is_property_access_expression, process_tagged_template_expression, set_original_node,
    set_text_range_node_array, set_text_range_rc_node, unwrap_innermost_statement_of_label,
    visit_each_child, visit_node, with_synthetic_factory, BaseNodeFactorySynthetic,
    CompilerOptions, Debug_, EmitHelperFactory, EmitHint, EmitResolver, FlattenLevel,
    FunctionFlags, HasInitializerInterface, HasStatementsInterface, Matches,
    NamedDeclarationInterface, Node, NodeArray, NodeArrayOrVec, NodeCheckFlags, NodeFactory,
    NodeInterface, ProcessLevel, ScriptTarget, SyntaxKind, TransformFlags, TransformationContext,
    VecExt, VisitResult, With,
};

bitflags! {
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
    enabled_substitutions: Cell<Option<ESNextSubstitutionFlags>>,
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
    substituted_super_accessors: GcCell<Vec<bool>>,
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

    fn enclosing_function_flags(&self) -> Option<FunctionFlags> {
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

    fn maybe_captured_super_properties_mut(&self) -> GcCellRefMut<Option<HashSet<String>>> {
        self.captured_super_properties.borrow_mut()
    }

    fn has_super_element_access(&self) -> bool {
        self.has_super_element_access.get()
    }

    fn set_has_super_element_access(&self, has_super_element_access: bool) {
        self.has_super_element_access.set(has_super_element_access);
    }

    fn emit_helpers(&self) -> Rc<EmitHelperFactory> {
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
            .get_or_insert_with(|| Default::default())
            .push(with_synthetic_factory(|synthetic_factory_| {
                self.factory
                    .create_variable_declaration(
                        synthetic_factory_,
                        Some(temp.node_wrapper()),
                        None,
                        None,
                        None,
                    )
                    .into()
            }));
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
        visit_each_child(
            Some(node),
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
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
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
                visit_each_child(
                    Some(node),
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
                    Option::<fn(&Node) -> VisitResult>::None,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
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
                visit_each_child(
                    Some(node),
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
                    Option::<fn(&Node) -> VisitResult>::None,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
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
            _ => visit_each_child(
                Some(node),
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
                Option::<fn(&Node) -> VisitResult>::None,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
            .map(Into::into),
        }
    }

    fn visit_await_expression(
        &self,
        node: &Node, /*AwaitExpression*/
    ) -> Gc<Node /*Expression*/> {
        if self
            .enclosing_function_flags()
            .matches(|enclosing_function_flags| {
                enclosing_function_flags.intersects(FunctionFlags::Async)
            })
            && self
                .enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
        {
            return set_original_node(
                set_text_range_rc_node(
                    with_synthetic_factory(|synthetic_factory_| {
                        self.factory
                            .create_yield_expression(
                                synthetic_factory_,
                                None,
                                Some(
                                    self.emit_helpers().create_await_helper(
                                        visit_node(
                                            Some(&*node.as_await_expression().expression),
                                            Some(|node: &Node| self.visitor(node)),
                                            Some(is_expression),
                                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                        )
                                        .unwrap(),
                                    ),
                                ),
                            )
                            .into()
                    }),
                    Some(node),
                ),
                Some(node.node_wrapper()),
            );
        }
        visit_each_child(
            Some(node),
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
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .unwrap()
    }

    fn visit_yield_expression(&self, node: &Node /*YieldExpression*/) -> VisitResult {
        let node_as_yield_expression = node.as_yield_expression();
        if self
            .enclosing_function_flags()
            .matches(|enclosing_function_flags| {
                enclosing_function_flags.intersects(FunctionFlags::Async)
            })
            && self
                .enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
        {
            if node_as_yield_expression.asterisk_token.is_some() {
                let expression = visit_node(
                    Some(
                        Debug_.check_defined(node_as_yield_expression.expression.as_deref(), None),
                    ),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
                .unwrap();

                return Some(
                    set_original_node(
                        set_text_range_rc_node(
                            with_synthetic_factory(|synthetic_factory_| {
                                self.factory
                                    .create_yield_expression(
                                        synthetic_factory_,
                                        None,
                                        Some(
                                            self.emit_helpers().create_await_helper(
                                                self.factory.update_yield_expression(
                                                    synthetic_factory_,
                                                    node,
                                                    node_as_yield_expression.asterisk_token.clone(),
                                                    Some(set_text_range_rc_node(
                                                        self.emit_helpers()
                                                            .create_async_delegator_helper(
                                                                set_text_range_rc_node(
                                                                    self.emit_helpers()
                                                                        .create_async_values_helper(
                                                                            expression.clone(),
                                                                        ),
                                                                    Some(&*expression),
                                                                ),
                                                            ),
                                                        Some(&*expression),
                                                    )),
                                                ),
                                            ),
                                        ),
                                    )
                                    .into()
                            }),
                            Some(node),
                        ),
                        Some(node.node_wrapper()),
                    )
                    .into(),
                );
            }
        }

        Some(
            set_original_node(
                set_text_range_rc_node(
                    with_synthetic_factory(|synthetic_factory_| {
                        self.factory
                            .create_yield_expression(
                                synthetic_factory_,
                                None,
                                Some(self.create_downlevel_await(
                                    &node_as_yield_expression.expression.as_ref().map_or_else(
                                        || {
                                            with_synthetic_factory(|synthetic_factory_| {
                                                self.factory.create_void_zero(synthetic_factory_)
                                            })
                                        },
                                        |node_expression| {
                                            visit_node(
                                                Some(&**node_expression),
                                                Some(|node: &Node| self.visitor(node)),
                                                Some(is_expression),
                                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                            )
                                            .unwrap()
                                        },
                                    ),
                                )),
                            )
                            .into()
                    }),
                    Some(node),
                ),
                Some(node.node_wrapper()),
            )
            .into(),
        )
    }

    fn visit_return_statement(&self, node: &Node /*ReturnStatement*/) -> VisitResult {
        let node_as_return_statement = node.as_return_statement();
        if self
            .enclosing_function_flags()
            .matches(|enclosing_function_flags| {
                enclosing_function_flags.intersects(FunctionFlags::Async)
            })
            && self
                .enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
        {
            return Some(with_synthetic_factory(|synthetic_factory_| {
                self.factory
                    .update_return_statement(
                        synthetic_factory_,
                        node,
                        Some(self.create_downlevel_await(
                            &node_as_return_statement.expression.as_ref().map_or_else(
                                || {
                                    with_synthetic_factory(|synthetic_factory_| {
                                        self.factory.create_void_zero(synthetic_factory_)
                                    })
                                },
                                |node_expression| {
                                    visit_node(
                                        Some(&**node_expression),
                                        Some(|node: &Node| self.visitor(node)),
                                        Some(is_expression),
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    )
                                    .unwrap()
                                },
                            ),
                        )),
                    )
                    .into()
            }));
        }

        visit_each_child(
            Some(node),
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
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .map(Into::into)
    }

    fn visit_labeled_statement(&self, node: &Node /*LabeledStatement*/) -> VisitResult {
        let node_as_labeled_statement = node.as_labeled_statement();
        if self
            .enclosing_function_flags()
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
            return Some(with_synthetic_factory(|synthetic_factory_| {
                self.factory
                    .restore_enclosing_label(
                        synthetic_factory_,
                        &visit_node(
                            Some(&**statement),
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_statement),
                            Some(|nodes: &[Gc<Node>]| {
                                self.factory.lift_to_block(synthetic_factory_, nodes)
                            }),
                        )
                        .unwrap(),
                        Some(node),
                        Option::<fn(&Node)>::None,
                    )
                    .into()
            }));
        }

        visit_each_child(
            Some(node),
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
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
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
                    objects.push(with_synthetic_factory(|synthetic_factory_| {
                        self.factory
                            .create_object_literal_expression(
                                synthetic_factory_,
                                Some(chunk_object),
                                None,
                            )
                            .into()
                    }));
                }
                let target = &e.as_spread_assignment().expression;
                objects.push(
                    visit_node(
                        Some(&**target),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )
                    .unwrap(),
                );
            } else {
                chunk_object.get_or_insert_with(|| Default::default()).push(
                    if e.kind() == SyntaxKind::PropertyAssignment {
                        let e_as_property_assignment = e.as_property_assignment();
                        with_synthetic_factory(|synthetic_factory_| {
                            self.factory
                                .create_property_assignment(
                                    synthetic_factory_,
                                    e_as_property_assignment.name(),
                                    visit_node(
                                        e_as_property_assignment.maybe_initializer(),
                                        Some(|node: &Node| self.visitor(node)),
                                        Some(is_expression),
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    )
                                    .unwrap(),
                                )
                                .into()
                        })
                    } else {
                        visit_node(
                            Some(&**e),
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_object_literal_element_like),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        )
                        .unwrap()
                    },
                );
            }
        }
        if let Some(chunk_object) = chunk_object {
            objects.push(with_synthetic_factory(|synthetic_factory_| {
                self.factory
                    .create_object_literal_expression(synthetic_factory_, Some(chunk_object), None)
                    .into()
            }));
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
                    with_synthetic_factory(|synthetic_factory_| {
                        self.factory
                            .create_object_literal_expression(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                None,
                            )
                            .into()
                    }),
                );
            }
            let mut expression = objects.get(0).cloned();
            if objects.len() > 1 {
                for i in 1..objects.len() {
                    expression = Some(
                        self.emit_helpers()
                            .create_assign_helper(&[expression.unwrap(), objects[i].clone()]),
                    );
                }
                return expression.unwrap();
            } else {
                return self.emit_helpers().create_assign_helper(&objects);
            }
        }
        visit_each_child(
            Some(node),
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
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .unwrap()
    }

    fn visit_expression_statement(
        &self,
        node: &Node, /*ExpressionStatement*/
    ) -> Gc<Node /*ExpressionStatement*/> {
        visit_each_child(
            Some(node),
            |node: &Node| self.visitor_with_unused_expression_result(node),
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
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .unwrap()
    }

    fn visit_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
        expression_result_is_unused: bool,
    ) -> Gc<Node /*ParenthesizedExpression*/> {
        visit_each_child(
            Some(node),
            |node: &Node| {
                if expression_result_is_unused {
                    self.visitor_with_unused_expression_result(node)
                } else {
                    self.visitor(node)
                }
            },
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
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .unwrap()
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
        let ref visited = visit_each_child(
            Some(node),
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
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .unwrap();
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
                            .and_push(
                                self.factory
                                    .create_variable_statement(
                                        &self.base_factory,
                                        Option::<Gc<NodeArray>>::None,
                                        Gc::<Node>::from(
                                            self.factory.create_variable_declaration_list(
                                                &self.base_factory,
                                                tagged_template_string_declarations.clone(),
                                                None,
                                            ),
                                        ),
                                    )
                                    .into(),
                            )
                            .into()
                    } else {
                        statements.into()
                    }
                });
        let result = with_synthetic_factory(|synthetic_factory_| {
            self.factory.update_source_file(
                synthetic_factory_,
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
            )
        });
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
                Option::<fn(&Node, &Node, Option<&Node>) -> Gc<Node>>::None,
            );
        }
        if node_as_binary_expression.operator_token.kind() == SyntaxKind::CommaToken {
            return self.factory.update_binary_expression(
                &self.base_factory,
                node,
                visit_node(
                    Some(&*node_as_binary_expression.left),
                    Some(|node: &Node| self.visitor_with_unused_expression_result(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
                .unwrap(),
                node_as_binary_expression.operator_token.clone(),
                visit_node(
                    Some(&*node_as_binary_expression.right),
                    Some(|node: &Node| {
                        if expression_result_is_unused {
                            self.visitor_with_unused_expression_result(node)
                        } else {
                            self.visitor(node)
                        }
                    }),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
                .unwrap(),
            );
        }
        visit_each_child(
            Some(node),
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
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .unwrap()
    }

    fn visit_comma_list_expression(
        &self,
        node: &Node, /*CommaListExpression*/
        expression_result_is_unused: bool,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    fn visit_catch_clause(&self, node: &Node /*CatchClause*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_variable_statement(&self, node: &Node /*VariableStatement*/) -> VisitResult /*<VariableStatement>*/
    {
        unimplemented!()
    }

    fn visit_variable_declaration(&self, node: &Node /*VariableDeclaration*/) -> VisitResult /*<VariableDeclaration>*/
    {
        unimplemented!()
    }

    fn visit_for_statement(&self, node: &Node /*ForStatement*/) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    fn visit_void_expression(&self, node: &Node /*VoidExpression*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_for_of_statement(
        &self,
        node: &Node, /*ForOfStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    fn create_downlevel_await(&self, expression: &Node /*Expression*/) -> Gc<Node> {
        unimplemented!()
    }

    fn visit_parameter(
        &self,
        node: &Node, /*ParameterDeclaration*/
    ) -> Gc<Node /*ParameterDeclaration*/> {
        unimplemented!()
    }

    fn visit_constructor_declaration(
        &self,
        node: &Node, /*ConstructorDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    fn visit_get_accessor_declaration(
        &self,
        node: &Node, /*GetAccessorDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    fn visit_set_accessor_declaration(
        &self,
        node: &Node, /*SetAccessorDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    fn visit_method_declaration(&self, node: &Node /*MethodDeclaration*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_function_declaration(&self, node: &Node /*FunctionDeclaration*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_arrow_function(&self, node: &Node /*ArrowFunction*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_function_expression(&self, node: &Node /*FunctionExpression*/) -> VisitResult {
        unimplemented!()
    }
}

impl TransformerInterface for TransformES2018 {
    fn call(&self, node: &Node) -> Gc<Node> {
        self.transform_source_file(node)
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
}

impl TransformationContextOnEmitNodeOverrider for TransformES2018OnEmitNodeOverrider {
    fn on_emit_node(&self, hint: EmitHint, node: &Node, emit_callback: &dyn Fn(EmitHint, &Node)) {
        unimplemented!()
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
}

impl TransformationContextOnSubstituteNodeOverrider for TransformES2018OnSubstituteNodeOverrider {
    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> Gc<Node> {
        unimplemented!()
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
