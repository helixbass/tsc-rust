use std::{
    any::Any,
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    io,
};

use bitflags::bitflags;
use id_arena::Id;

use super::create_super_access_variable_statement;
use crate::{
    is_assignment_pattern, is_concise_body, is_expression, is_for_initializer, is_statement,
    set_text_range_id_node, skip_parentheses, some, unwrap_innermost_statement_of_label,
    visit_each_child, visit_iteration_body, visit_lexical_environment, visit_node,
    visit_parameter_list, CompilerOptions, Debug_, EmitFlags, EmitHelperFactory, EmitHint,
    EmitResolver, FlattenLevel, FunctionFlags, FunctionLikeDeclarationInterface,
    GeneratedIdentifierFlags, GetOrInsertDefault, HasInitializerInterface, HasStatementsInterface,
    Matches, ModifierFlags, NamedDeclarationInterface, Node, NodeArray, NodeArrayExt,
    NodeArrayOrVec, NodeCheckFlags, NodeExt, NodeFactory, NodeFlags, NodeId, NodeInterface,
    ProcessLevel, ReadonlyTextRange, ReadonlyTextRangeConcrete, ScriptTarget,
    SignatureDeclarationInterface, SyntaxKind, TransformFlags, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface, VecExt,
    VecExtClone, VisitResult, With, __String, add_emit_helper, add_emit_helpers, add_range,
    advanced_async_super_helper, async_super_helper, chain_bundle, create_for_of_binding_statement,
    downcast_transformer_ref, flatten_destructuring_assignment, flatten_destructuring_binding,
    get_emit_script_target, get_node_id, has_syntactic_modifier, impl_has_arena,
    insert_statements_after_standard_prologue, is_binding_pattern, is_block,
    is_destructuring_assignment, is_effective_strict_mode_source_file, is_identifier, is_modifier,
    is_object_literal_element_like, is_property_access_expression, is_property_name,
    is_super_property, is_token, is_variable_declaration_list, maybe_visit_each_child,
    maybe_visit_node, maybe_visit_nodes, process_tagged_template_expression, ref_unwrapped,
    released, set_emit_flags, set_original_node, set_text_range_node_array, AllArenas,
    CoreTransformationContext, HasArena, InArena, OptionInArena,
    TransformNodesTransformationResult,
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

struct TransformES2018 {
    arena: *const AllArenas,
    context: Id<TransformNodesTransformationResult>,
    factory: Id<NodeFactory>,
    resolver: Id<Box<dyn EmitResolver>>,
    compiler_options: Id<CompilerOptions>,
    language_version: ScriptTarget,
    exported_variable_statement: Cell<bool>,
    enabled_substitutions: Cell<ESNextSubstitutionFlags>,
    enclosing_function_flags: Cell<Option<FunctionFlags>>,
    enclosing_super_container_flags: Cell<NodeCheckFlags>,
    hierarchy_facts: Cell<HierarchyFacts>,
    current_source_file: Cell<Option<Id<Node /*SourceFile*/>>>,
    tagged_template_string_declarations: RefCell<Option<Vec<Id<Node /*VariableDeclaration*/>>>>,
    captured_super_properties: RefCell<Option<HashSet<__String>>>,
    has_super_element_access: Cell<bool>,
    substituted_super_accessors: RefCell<HashMap<NodeId, bool>>,
}

impl TransformES2018 {
    fn new(
        context: Id<TransformNodesTransformationResult>,
        arena: *const AllArenas,
    ) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        let compiler_options = context_ref.get_compiler_options();
        let ret = arena_ref.alloc_transformer(Box::new(Self {
            arena,
            factory: context_ref.factory(),
            resolver: context_ref.get_emit_resolver(),
            language_version: get_emit_script_target(&compiler_options.ref_(arena_ref)),
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
        context_ref.override_on_emit_node(&mut |previous_on_emit_node| {
            arena_ref.alloc_transformation_context_on_emit_node_overrider(Box::new(
                TransformES2018OnEmitNodeOverrider::new(ret, previous_on_emit_node, arena_ref),
            ))
        });
        context_ref.override_on_substitute_node(&mut |previous_on_substitute_node| {
            arena_ref.alloc_transformation_context_on_substitute_node_overrider(Box::new(
                TransformES2018OnSubstituteNodeOverrider::new(
                    ret,
                    previous_on_substitute_node,
                    arena_ref,
                ),
            ))
        });
        ret
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

    fn current_source_file(&self) -> Id<Node> {
        self.current_source_file.get().unwrap()
    }

    fn set_current_source_file(&self, current_source_file: Option<Id<Node>>) {
        self.current_source_file.set(current_source_file);
    }

    fn maybe_tagged_template_string_declarations(&self) -> Ref<Option<Vec<Id<Node>>>> {
        self.tagged_template_string_declarations.borrow()
    }

    fn maybe_tagged_template_string_declarations_mut(&self) -> RefMut<Option<Vec<Id<Node>>>> {
        self.tagged_template_string_declarations.borrow_mut()
    }

    fn set_tagged_template_string_declarations(
        &self,
        tagged_template_string_declarations: Option<Vec<Id<Node>>>,
    ) {
        *self.tagged_template_string_declarations.borrow_mut() =
            tagged_template_string_declarations;
    }

    fn maybe_captured_super_properties(&self) -> Ref<Option<HashSet<String>>> {
        self.captured_super_properties.borrow()
    }

    fn captured_super_properties(&self) -> Ref<HashSet<String>> {
        ref_unwrapped(&self.captured_super_properties)
    }

    fn maybe_captured_super_properties_mut(&self) -> RefMut<Option<HashSet<String>>> {
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

    fn substituted_super_accessors(&self) -> Ref<HashMap<NodeId, bool>> {
        self.substituted_super_accessors.borrow()
    }

    fn substituted_super_accessors_mut(&self) -> RefMut<HashMap<NodeId, bool>> {
        self.substituted_super_accessors.borrow_mut()
    }

    fn emit_helpers(&self) -> debug_cell::Ref<'_, EmitHelperFactory> {
        self.context.ref_(self).get_emit_helper_factory().ref_(self)
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

    fn record_tagged_template_string(&self, temp: Id<Node> /*Identifier*/) {
        self.maybe_tagged_template_string_declarations_mut()
            .get_or_insert_default_()
            .push(self.factory.ref_(self).create_variable_declaration(
                Some(temp),
                None,
                None,
                None,
            ));
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        if node.ref_(self).as_source_file().is_declaration_file() {
            return node;
        }

        self.set_current_source_file(Some(node));
        let visited = self.visit_source_file(node);
        add_emit_helpers(
            visited,
            self.context.ref_(self).read_emit_helpers().as_deref(),
            self,
        );

        self.set_current_source_file(None);
        self.set_tagged_template_string_declarations(None);
        visited
    }

    fn visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, false)
    }

    fn visitor_with_unused_expression_result(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, true)
    }

    fn visitor_no_async_modifier(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        if node.ref_(self).kind() == SyntaxKind::AsyncKeyword {
            return None;
        }
        Some(node.into())
    }

    fn do_with_hierarchy_facts<TReturn>(
        &self,
        mut cb: impl FnMut(Id<Node>) -> TReturn,
        value: Id<Node>,
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

    fn visit_default(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        maybe_visit_each_child(
            Some(node),
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
        .map(Into::into)
    }

    fn visitor_worker(&self, node: Id<Node>, expression_result_is_unused: bool) -> VisitResult /*<Node>*/
    {
        if !node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsES2018)
        {
            return Some(node.into());
        }
        match released!(node.ref_(self).kind()) {
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
                    |node: Id<Node>| self.visit_default(node),
                    node,
                    HierarchyFacts::IterationStatementExcludes,
                    HierarchyFacts::IterationStatementIncludes,
                )
            }
            SyntaxKind::ForOfStatement => {
                self.visit_for_of_statement(node, Option::<Id<Node>>::None)
            }
            SyntaxKind::ForStatement => self.do_with_hierarchy_facts(
                |node: Id<Node>| self.visit_for_statement(node),
                node,
                HierarchyFacts::IterationStatementExcludes,
                HierarchyFacts::IterationStatementIncludes,
            ),
            SyntaxKind::VoidExpression => self.visit_void_expression(node),
            SyntaxKind::Constructor => self.do_with_hierarchy_facts(
                |node: Id<Node>| self.visit_constructor_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::MethodDeclaration => self.do_with_hierarchy_facts(
                |node: Id<Node>| self.visit_method_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::GetAccessor => self.do_with_hierarchy_facts(
                |node: Id<Node>| self.visit_get_accessor_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::SetAccessor => self.do_with_hierarchy_facts(
                |node: Id<Node>| self.visit_set_accessor_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::FunctionDeclaration => self.do_with_hierarchy_facts(
                |node: Id<Node>| self.visit_function_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::FunctionExpression => self.do_with_hierarchy_facts(
                |node: Id<Node>| self.visit_function_expression(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::ArrowFunction => self.do_with_hierarchy_facts(
                |node: Id<Node>| self.visit_arrow_function(node),
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
                    if is_property_access_expression(&node.ref_(self))
                        && node
                            .ref_(self)
                            .as_property_access_expression()
                            .expression
                            .ref_(self)
                            .kind()
                            == SyntaxKind::SuperKeyword
                    {
                        captured_super_properties.insert(
                            node.ref_(self)
                                .as_property_access_expression()
                                .name
                                .ref_(self)
                                .as_member_name()
                                .escaped_text()
                                .to_owned(),
                        );
                    }
                }
                maybe_visit_each_child(
                    Some(node),
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )
                .map(Into::into)
            }
            SyntaxKind::ElementAccessExpression => {
                if self.maybe_captured_super_properties().is_some() {
                    if node
                        .ref_(self)
                        .as_element_access_expression()
                        .expression
                        .ref_(self)
                        .kind()
                        == SyntaxKind::SuperKeyword
                    {
                        self.set_has_super_element_access(true);
                    }
                }
                maybe_visit_each_child(
                    Some(node),
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )
                .map(Into::into)
            }
            SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => self
                .do_with_hierarchy_facts(
                    |node: Id<Node>| self.visit_default(node),
                    node,
                    HierarchyFacts::ClassOrFunctionExcludes,
                    HierarchyFacts::ClassOrFunctionIncludes,
                ),
            _ => maybe_visit_each_child(
                Some(node),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .map(Into::into),
        }
    }

    fn visit_await_expression(
        &self,
        node: Id<Node>, /*AwaitExpression*/
    ) -> Id<Node /*Expression*/> {
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
                    .ref_(self)
                    .create_yield_expression(
                        None,
                        Some(self.emit_helpers().create_await_helper(visit_node(
                            node.ref_(self).as_await_expression().expression,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_expression(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        ))),
                    )
                    .set_text_range(Some(&*node.ref_(self)), self),
                Some(node),
                self,
            );
        }
        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    fn visit_yield_expression(&self, node: Id<Node> /*YieldExpression*/) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_yield_expression = node_ref.as_yield_expression();
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
                    Debug_.check_defined(node_as_yield_expression.expression, None),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                );

                return Some(
                    set_original_node(
                        self.factory
                            .ref_(self)
                            .create_yield_expression(
                                None,
                                Some(
                                    self.emit_helpers().create_await_helper(
                                        self.factory.ref_(self).update_yield_expression(
                                            node,
                                            node_as_yield_expression.asterisk_token.clone(),
                                            Some(set_text_range_id_node(
                                                self.emit_helpers().create_async_delegator_helper(
                                                    set_text_range_id_node(
                                                        self.emit_helpers()
                                                            .create_async_values_helper(expression),
                                                        Some(&*expression.ref_(self)),
                                                        self,
                                                    ),
                                                ),
                                                Some(&*expression.ref_(self)),
                                                self,
                                            )),
                                        ),
                                    ),
                                ),
                            )
                            .set_text_range(Some(&*node.ref_(self)), self),
                        Some(node),
                        self,
                    )
                    .into(),
                );
            }
        }

        Some(
            set_original_node(
                self.factory
                    .ref_(self)
                    .create_yield_expression(
                        None,
                        Some(self.create_downlevel_await(
                            node_as_yield_expression.expression.map_or_else(
                                || self.factory.ref_(self).create_void_zero(),
                                |node_expression| {
                                    visit_node(
                                        node_expression,
                                        Some(|node: Id<Node>| self.visitor(node)),
                                        Some(|node| is_expression(node, self)),
                                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                    )
                                },
                            ),
                        )),
                    )
                    .set_text_range(Some(&*node.ref_(self)), self),
                Some(node),
                self,
            )
            .into(),
        )
    }

    fn visit_return_statement(&self, node: Id<Node> /*ReturnStatement*/) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_return_statement = node_ref.as_return_statement();
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
                    .ref_(self)
                    .update_return_statement(
                        node,
                        Some(self.create_downlevel_await(
                            node_as_return_statement.expression.map_or_else(
                                || self.factory.ref_(self).create_void_zero(),
                                |node_expression| {
                                    visit_node(
                                        node_expression,
                                        Some(|node: Id<Node>| self.visitor(node)),
                                        Some(|node| is_expression(node, self)),
                                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
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
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
        .map(Into::into)
    }

    fn visit_labeled_statement(&self, node: Id<Node> /*LabeledStatement*/) -> VisitResult {
        if self
            .maybe_enclosing_function_flags()
            .matches(|enclosing_function_flags| {
                enclosing_function_flags.intersects(FunctionFlags::Async)
            })
        {
            let statement =
                unwrap_innermost_statement_of_label(node, Option::<fn(Id<Node>)>::None, self);
            if statement.ref_(self).kind() == SyntaxKind::ForOfStatement
                && statement
                    .ref_(self)
                    .as_for_of_statement()
                    .await_modifier
                    .is_some()
            {
                return self.visit_for_of_statement(statement, Some(node));
            }
            return Some(
                self.factory
                    .ref_(self)
                    .restore_enclosing_label(
                        visit_node(
                            statement,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_statement(node, self)),
                            Some(|nodes: &[Id<Node>]| self.factory.ref_(self).lift_to_block(nodes)),
                        ),
                        Some(node),
                        Option::<fn(Id<Node>)>::None,
                    )
                    .into(),
            );
        }

        maybe_visit_each_child(
            Some(node),
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
        .map(Into::into)
    }

    fn chunk_object_literal_elements(
        &self,
        elements: &[Id<Node>], /*ObjectLiteralElementLike*/
    ) -> Vec<Id<Node /*Expression*/>> {
        let mut chunk_object: Option<Vec<Id<Node /*ObjectLiteralElementLike*/>>> =
            Default::default();
        let mut objects: Vec<Id<Node /*Expression*/>> = Default::default();
        for &e in elements {
            if e.ref_(self).kind() == SyntaxKind::SpreadAssignment {
                if let Some(chunk_object) = chunk_object.take() {
                    objects.push(
                        self.factory
                            .ref_(self)
                            .create_object_literal_expression(Some(chunk_object), None),
                    );
                }
                let target = e.ref_(self).as_spread_assignment().expression;
                objects.push(visit_node(
                    target,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ));
            } else {
                chunk_object.get_or_insert_default_().push(
                    if e.ref_(self).kind() == SyntaxKind::PropertyAssignment {
                        let e_ref = e.ref_(self);
                        let e_as_property_assignment = e_ref.as_property_assignment();
                        self.factory.ref_(self).create_property_assignment(
                            e_as_property_assignment.name(),
                            visit_node(
                                e_as_property_assignment.maybe_initializer().unwrap(),
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                        )
                    } else {
                        visit_node(
                            e,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node: Id<Node>| is_object_literal_element_like(&node.ref_(self))),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )
                    },
                );
            }
        }
        if let Some(chunk_object) = chunk_object {
            objects.push(
                self.factory
                    .ref_(self)
                    .create_object_literal_expression(Some(chunk_object), None),
            );
        }

        objects
    }

    fn visit_object_literal_expression(
        &self,
        node: Id<Node>, /*ObjectLiteralExpression*/
    ) -> Id<Node /*Expression*/> {
        let node_ref = node.ref_(self);
        let node_as_object_literal_expression = node_ref.as_object_literal_expression();
        if node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            let mut objects = self.chunk_object_literal_elements(
                &node_as_object_literal_expression.properties.ref_(self),
            );
            if !objects.is_empty()
                && objects[0].ref_(self).kind() != SyntaxKind::ObjectLiteralExpression
            {
                objects.insert(
                    0,
                    self.factory
                        .ref_(self)
                        .create_object_literal_expression(Option::<Id<NodeArray>>::None, None),
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
        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    fn visit_expression_statement(
        &self,
        node: Id<Node>, /*ExpressionStatement*/
    ) -> Id<Node /*ExpressionStatement*/> {
        visit_each_child(
            node,
            |node: Id<Node>| self.visitor_with_unused_expression_result(node),
            &*self.context.ref_(self),
            self,
        )
    }

    fn visit_parenthesized_expression(
        &self,
        node: Id<Node>, /*ParenthesizedExpression*/
        expression_result_is_unused: bool,
    ) -> Id<Node /*ParenthesizedExpression*/> {
        visit_each_child(
            node,
            |node: Id<Node>| {
                if expression_result_is_unused {
                    self.visitor_with_unused_expression_result(node)
                } else {
                    self.visitor(node)
                }
            },
            &*self.context.ref_(self),
            self,
        )
    }

    fn visit_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node /*SourceFile*/> {
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::SourceFileExcludes,
            if is_effective_strict_mode_source_file(node, &self.compiler_options.ref_(self), self) {
                HierarchyFacts::StrictModeSourceFileIncludes
            } else {
                HierarchyFacts::SourceFileIncludes
            },
        );
        self.set_exported_variable_statement(false);
        let visited = visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        );
        let statement =
            visited
                .ref_(self)
                .as_source_file()
                .statements()
                .with(|statements| -> NodeArrayOrVec {
                    if let Some(tagged_template_string_declarations) =
                        self.maybe_tagged_template_string_declarations().as_ref()
                    {
                        statements
                            .ref_(self)
                            .to_vec()
                            .and_push(self.factory.ref_(self).create_variable_statement(
                                Option::<Id<NodeArray>>::None,
                                self.factory.ref_(self).create_variable_declaration_list(
                                    tagged_template_string_declarations.clone(),
                                    None,
                                ),
                            ))
                            .into()
                    } else {
                        statements.into()
                    }
                });
        let result = self.factory.ref_(self).update_source_file(
            visited,
            set_text_range_node_array(
                self.factory
                    .ref_(self)
                    .create_node_array(Some(statement), None),
                Some(&*node.ref_(self).as_source_file().statements().ref_(self)),
                self,
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
        node: Id<Node>, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        Some(
            process_tagged_template_expression(
                &*self.context.ref_(self),
                node,
                |node: Id<Node>| self.visitor(node),
                self.current_source_file(),
                |node: Id<Node>| {
                    self.record_tagged_template_string(node);
                },
                ProcessLevel::LiftRestriction,
            )
            .into(),
        )
    }

    fn visit_binary_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
        expression_result_is_unused: bool,
    ) -> Id<Node /*Expression*/> {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        if is_destructuring_assignment(node, self)
            && node_as_binary_expression
                .left
                .ref_(self)
                .transform_flags()
                .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            return flatten_destructuring_assignment(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                self.context.clone(),
                FlattenLevel::ObjectRest,
                Some(!expression_result_is_unused),
                Option::<fn(Id<Node>, Id<Node>, Option<&dyn ReadonlyTextRange>) -> Id<Node>>::None,
                self,
            );
        }
        if node_as_binary_expression.operator_token.ref_(self).kind() == SyntaxKind::CommaToken {
            return self.factory.ref_(self).update_binary_expression(
                node,
                visit_node(
                    node_as_binary_expression.left,
                    Some(|node: Id<Node>| self.visitor_with_unused_expression_result(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                node_as_binary_expression.operator_token.clone(),
                visit_node(
                    node_as_binary_expression.right,
                    Some(|node: Id<Node>| {
                        if expression_result_is_unused {
                            self.visitor_with_unused_expression_result(node)
                        } else {
                            self.visitor(node)
                        }
                    }),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            );
        }
        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    fn visit_comma_list_expression(
        &self,
        node: Id<Node>, /*CommaListExpression*/
        expression_result_is_unused: bool,
    ) -> Id<Node /*Expression*/> {
        let node_ref = node.ref_(self);
        let node_as_comma_list_expression = node_ref.as_comma_list_expression();
        if expression_result_is_unused {
            return visit_each_child(
                node,
                |node: Id<Node>| self.visitor_with_unused_expression_result(node),
                &*self.context.ref_(self),
                self,
            );
        }
        let mut result: Option<Vec<Id<Node /*Expression*/>>> = None;
        for (i, element) in node_as_comma_list_expression
            .elements
            .ref_(self)
            .iter()
            .enumerate()
        {
            let element = *element;
            let visited = visit_node(
                element,
                Some(|node: Id<Node>| {
                    if i < node_as_comma_list_expression.elements.ref_(self).len() - 1 {
                        self.visitor_with_unused_expression_result(node)
                    } else {
                        self.visitor(node)
                    }
                }),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            );
            if result.is_some() || visited != element {
                result
                    .get_or_insert_with(|| {
                        node_as_comma_list_expression.elements.ref_(self)[0..i].to_owned()
                    })
                    .push(visited);
            }
        }
        let elements = result.map_or_else(
            || node_as_comma_list_expression.elements.clone(),
            |result| {
                self.factory
                    .ref_(self)
                    .create_node_array(Some(result), None)
                    .set_text_range(
                        Some(&*node_as_comma_list_expression.elements.ref_(self)),
                        self,
                    )
            },
        );
        self.factory
            .ref_(self)
            .update_comma_list_expression(node, elements)
    }

    fn visit_catch_clause(&self, node: Id<Node> /*CatchClause*/) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_catch_clause = node_ref.as_catch_clause();
        if let Some(node_variable_declaration) =
            node_as_catch_clause
                .variable_declaration
                .filter(|node_variable_declaration| {
                    let node_variable_declaration_ref = node_variable_declaration.ref_(self);
                    let node_variable_declaration_as_variable_declaration =
                        node_variable_declaration_ref.as_variable_declaration();
                    is_binding_pattern(
                        node_variable_declaration_as_variable_declaration
                            .maybe_name()
                            .refed(self)
                            .as_deref(),
                    ) && node_variable_declaration_as_variable_declaration
                        .name()
                        .ref_(self)
                        .transform_flags()
                        .intersects(TransformFlags::ContainsObjectRestOrSpread)
                })
        {
            let node_variable_declaration_ref = node_variable_declaration.ref_(self);
            let node_variable_declaration_as_variable_declaration =
                node_variable_declaration_ref.as_variable_declaration();
            let name = self.factory.ref_(self).get_generated_name_for_node(
                node_variable_declaration_as_variable_declaration.maybe_name(),
                None,
            );
            let updated_decl = self.factory.ref_(self).update_variable_declaration(
                node_variable_declaration,
                node_variable_declaration_as_variable_declaration.maybe_name(),
                None,
                None,
                Some(name.clone()),
            );
            let visited_bindings = flatten_destructuring_binding(
                updated_decl,
                |node: Id<Node>| self.visitor(node),
                self.context.clone(),
                FlattenLevel::ObjectRest,
                Option::<Id<Node>>::None,
                None,
                None,
                self,
            );
            let mut block = visit_node(
                node_as_catch_clause.block,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_block(&node.ref_(self))),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            );
            if !visited_bindings.is_empty() {
                block = self.factory.ref_(self).update_block(
                    block,
                    vec![self.factory.ref_(self).create_variable_statement(
                        Option::<Id<NodeArray>>::None,
                        visited_bindings,
                    )]
                    .and_extend(
                        block
                            .ref_(self)
                            .as_block()
                            .statements
                            .ref_(self)
                            .iter()
                            .cloned(),
                    ),
                );
            }
            return Some(
                self.factory
                    .ref_(self)
                    .update_catch_clause(
                        node,
                        Some(self.factory.ref_(self).update_variable_declaration(
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
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
        .map(Into::into)
    }

    fn visit_variable_statement(&self, node: Id<Node> /*VariableStatement*/) -> VisitResult /*<VariableStatement>*/
    {
        if has_syntactic_modifier(node, ModifierFlags::Export, self) {
            let saved_exported_variable_statement = self.exported_variable_statement();
            self.set_exported_variable_statement(true);
            let visited = visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            );
            self.set_exported_variable_statement(saved_exported_variable_statement);
            return Some(visited.into());
        }
        maybe_visit_each_child(
            Some(node),
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
        .map(Into::into)
    }

    fn visit_variable_declaration(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
    ) -> VisitResult /*<VariableDeclaration>*/ {
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
        node: Id<Node>, /*VariableDeclaration*/
        exported_variable_statement: bool,
    ) -> VisitResult /*<VariableDeclaration>*/ {
        if is_binding_pattern(
            node.ref_(self)
                .as_variable_declaration()
                .maybe_name()
                .refed(self)
                .as_deref(),
        ) && node
            .ref_(self)
            .as_variable_declaration()
            .name()
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            return Some(
                flatten_destructuring_binding(
                    node,
                    |node: Id<Node>| self.visitor(node),
                    self.context.clone(),
                    FlattenLevel::ObjectRest,
                    Option::<Id<Node>>::None,
                    Some(exported_variable_statement),
                    None,
                    self,
                )
                .into(),
            );
        }
        maybe_visit_each_child(
            Some(node),
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
        .map(Into::into)
    }

    fn visit_for_statement(&self, node: Id<Node> /*ForStatement*/) -> VisitResult /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_for_statement = node_ref.as_for_statement();
        Some(
            self.factory
                .ref_(self)
                .update_for_statement(
                    node,
                    maybe_visit_node(
                        node_as_for_statement.initializer,
                        Some(|node: Id<Node>| self.visitor_with_unused_expression_result(node)),
                        Some(|node| is_for_initializer(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                    maybe_visit_node(
                        node_as_for_statement.condition,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                    maybe_visit_node(
                        node_as_for_statement.incrementor,
                        Some(|node: Id<Node>| self.visitor_with_unused_expression_result(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                    visit_iteration_body(
                        node_as_for_statement.statement,
                        |node: Id<Node>| self.visitor(node),
                        &*self.context.ref_(self),
                        self,
                    ),
                )
                .into(),
        )
    }

    fn visit_void_expression(&self, node: Id<Node> /*VoidExpression*/) -> VisitResult {
        maybe_visit_each_child(
            Some(node),
            |node: Id<Node>| self.visitor_with_unused_expression_result(node),
            &*self.context.ref_(self),
            self,
        )
        .map(Into::into)
    }

    fn visit_for_of_statement(
        &self,
        mut node: Id<Node>, /*ForOfStatement*/
        outermost_labeled_statement: Option<Id<Node /*LabeledStatement*/>>,
    ) -> VisitResult /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_for_of_statement = node_ref.as_for_of_statement();
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::IterationStatementExcludes,
            HierarchyFacts::IterationStatementIncludes,
        );
        if node_as_for_of_statement
            .initializer
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            node = self.transform_for_of_statement_with_object_rest(node);
        }
        let result = if node_as_for_of_statement.await_modifier.is_some() {
            self.transform_for_await_of_statement(node, outermost_labeled_statement, ancestor_facts)
        } else {
            Some(
                self.factory
                    .ref_(self)
                    .restore_enclosing_label(
                        visit_each_child(
                            node,
                            |node: Id<Node>| self.visitor(node),
                            &*self.context.ref_(self),
                            self,
                        ),
                        outermost_labeled_statement,
                        Option::<fn(Id<Node>)>::None,
                    )
                    .into(),
            )
        };
        self.exit_subtree(ancestor_facts);
        result
    }

    fn transform_for_of_statement_with_object_rest(
        &self,
        node: Id<Node>, /*ForOfStatement*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_for_of_statement = node_ref.as_for_of_statement();
        let initializer_without_parens =
            skip_parentheses(node_as_for_of_statement.initializer, None, self);
        if is_variable_declaration_list(&initializer_without_parens.ref_(self))
            || is_assignment_pattern(&initializer_without_parens.ref_(self))
        {
            let body_location: Option<Id<Node /*TextRange*/>>;
            let statements_location: Option<ReadonlyTextRangeConcrete /*TextRange*/>;
            let temp = self
                .factory
                .ref_(self)
                .create_temp_variable(Option::<fn(Id<Node>)>::None, None);
            let mut statements: Vec<Id<Node /*Statement*/>> =
                vec![create_for_of_binding_statement(
                    &self.factory.ref_(self),
                    initializer_without_parens,
                    temp,
                )];
            #[allow(clippy::suspicious_else_formatting)]
            if is_block(&node_as_for_of_statement.statement.ref_(self)) {
                let node_statement_ref = node_as_for_of_statement.statement.ref_(self);
                let node_statement_as_block = node_statement_ref.as_block();
                add_range(
                    &mut statements,
                    Some(&node_statement_as_block.statements.ref_(self)),
                    None,
                    None,
                );
                body_location = Some(node_as_for_of_statement.statement.clone());
                statements_location =
                    Some((&*node_statement_as_block.statements.ref_(self)).into());
            } else
            /*if (node.statement)*/
            {
                statements.push(node_as_for_of_statement.statement.clone());
                body_location = Some(node_as_for_of_statement.statement.clone());
                statements_location =
                    Some((&*node_as_for_of_statement.statement.ref_(self)).into());
            }
            return self.factory.ref_(self).update_for_of_statement(
                node,
                node_as_for_of_statement.await_modifier.clone(),
                self.factory
                    .ref_(self)
                    .create_variable_declaration_list(
                        vec![self
                            .factory
                            .ref_(self)
                            .create_variable_declaration(Some(temp), None, None, None)
                            .set_text_range(
                                Some(&*node_as_for_of_statement.initializer.ref_(self)),
                                self,
                            )],
                        Some(NodeFlags::Let),
                    )
                    .set_text_range(
                        Some(&*node_as_for_of_statement.initializer.ref_(self)),
                        self,
                    ),
                node_as_for_of_statement.expression,
                self.factory
                    .ref_(self)
                    .create_block(
                        self.factory
                            .ref_(self)
                            .create_node_array(Some(statements), None)
                            .set_text_range(statements_location.as_ref(), self),
                        Some(true),
                    )
                    .set_text_range(body_location.refed(self).as_deref(), self),
            );
        }
        node
    }

    fn convert_for_of_statement_head(
        &self,
        node: Id<Node>,        /*ForOfStatement*/
        bound_value: Id<Node>, /*Expression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_for_of_statement = node_ref.as_for_of_statement();
        let binding = create_for_of_binding_statement(
            &self.factory.ref_(self),
            node_as_for_of_statement.initializer,
            bound_value,
        );

        let mut body_location: Option<Id<Node /*TextRange*/>> = Default::default();
        let mut statements_location: Option<Id<NodeArray /*TextRange*/>> = Default::default();
        let mut statements: Vec<Id<Node /*Statement*/>> = vec![visit_node(
            binding,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_statement(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )];
        let statement = visit_iteration_body(
            node_as_for_of_statement.statement,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        );
        if is_block(&statement.ref_(self)) {
            let statement_ref = statement.ref_(self);
            let statement_as_block = statement_ref.as_block();
            add_range(
                &mut statements,
                Some(&statement_as_block.statements.ref_(self)),
                None,
                None,
            );
            body_location = Some(statement.clone());
            statements_location = Some(statement_as_block.statements.clone());
        } else {
            statements.push(statement);
        }

        self.factory
            .ref_(self)
            .create_block(
                self.factory
                    .ref_(self)
                    .create_node_array(Some(statements), None)
                    .set_text_range(statements_location.refed(self).as_deref(), self),
                Some(true),
            )
            .set_text_range(body_location.refed(self).as_deref(), self)
            .set_emit_flags(EmitFlags::NoSourceMap | EmitFlags::NoTokenSourceMaps, self)
    }

    fn create_downlevel_await(&self, expression: Id<Node> /*Expression*/) -> Id<Node> {
        if self
            .maybe_enclosing_function_flags()
            .matches(|enclosing_function_flags| {
                enclosing_function_flags.intersects(FunctionFlags::Generator)
            })
        {
            self.factory.ref_(self).create_yield_expression(
                None,
                Some(self.emit_helpers().create_await_helper(expression)),
            )
        } else {
            self.factory.ref_(self).create_await_expression(expression)
        }
    }

    fn transform_for_await_of_statement(
        &self,
        node: Id<Node>, /*ForOfStatement*/
        outermost_labeled_statement: Option<Id<Node /*LabeledStatement*/>>,
        ancestor_facts: HierarchyFacts,
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_for_of_statement = node_ref.as_for_of_statement();
        let expression = visit_node(
            node_as_for_of_statement.expression,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        let iterator = if is_identifier(&expression.ref_(self)) {
            self.factory
                .ref_(self)
                .get_generated_name_for_node(Some(expression), None)
        } else {
            self.factory
                .ref_(self)
                .create_temp_variable(Option::<fn(Id<Node>)>::None, None)
        };
        let result = if is_identifier(&expression.ref_(self)) {
            self.factory
                .ref_(self)
                .get_generated_name_for_node(Some(iterator), None)
        } else {
            self.factory
                .ref_(self)
                .create_temp_variable(Option::<fn(Id<Node>)>::None, None)
        };
        let error_record = self.factory.ref_(self).create_unique_name("e", None);
        let catch_variable = self
            .factory
            .ref_(self)
            .get_generated_name_for_node(Some(error_record), None);
        let return_method = self
            .factory
            .ref_(self)
            .create_temp_variable(Option::<fn(Id<Node>)>::None, None);
        let call_values = self
            .emit_helpers()
            .create_async_values_helper(expression)
            .set_text_range(Some(&*node_as_for_of_statement.expression.ref_(self)), self);
        let call_next = self.factory.ref_(self).create_call_expression(
            self.factory
                .ref_(self)
                .create_property_access_expression(iterator.clone(), "next"),
            Option::<Id<NodeArray>>::None,
            Some(vec![]),
        );
        let get_done = self
            .factory
            .ref_(self)
            .create_property_access_expression(result.clone(), "done");
        let get_value = self
            .factory
            .ref_(self)
            .create_property_access_expression(result.clone(), "value");
        let call_return = self.factory.ref_(self).create_function_call_call(
            return_method.clone(),
            iterator.clone(),
            vec![],
        );

        self.context
            .ref_(self)
            .hoist_variable_declaration(error_record);
        self.context
            .ref_(self)
            .hoist_variable_declaration(return_method);

        let initializer = if ancestor_facts.intersects(HierarchyFacts::IterationContainer) {
            self.factory.ref_(self).inline_expressions(&[
                self.factory.ref_(self).create_assignment(
                    error_record.clone(),
                    self.factory.ref_(self).create_void_zero(),
                ),
                call_values,
            ])
        } else {
            call_values
        };

        let for_statement =
            self.factory
                .ref_(self)
                .create_for_statement(
                    Some(
                        self.factory
                            .ref_(self)
                            .create_variable_declaration_list(
                                vec![
                                    self.factory
                                        .ref_(self)
                                        .create_variable_declaration(
                                            Some(iterator.clone()),
                                            None,
                                            None,
                                            Some(initializer),
                                        )
                                        .set_text_range(
                                            Some(&*node_as_for_of_statement.expression.ref_(self)),
                                            self,
                                        ),
                                    self.factory.ref_(self).create_variable_declaration(
                                        Some(result.clone()),
                                        None,
                                        None,
                                        None,
                                    ),
                                ],
                                None,
                            )
                            .set_text_range(
                                Some(&*node_as_for_of_statement.expression.ref_(self)),
                                self,
                            )
                            .set_emit_flags(EmitFlags::NoHoisting, self),
                    ),
                    Some(self.factory.ref_(self).create_comma(
                        self.factory.ref_(self).create_assignment(
                            result.clone(),
                            self.create_downlevel_await(call_next),
                        ),
                        self.factory.ref_(self).create_logical_not(get_done.clone()),
                    )),
                    None,
                    self.convert_for_of_statement_head(node, get_value),
                )
                .set_text_range(Some(&*node.ref_(self)), self)
                .set_emit_flags(EmitFlags::NoTokenTrailingSourceMaps, self);

        Some(
            self.factory
                .ref_(self)
                .create_try_statement(
                    self.factory.ref_(self).create_block(
                        vec![self.factory.ref_(self).restore_enclosing_label(
                            for_statement,
                            outermost_labeled_statement,
                            Option::<fn(Id<Node>)>::None,
                        )],
                        None,
                    ),
                    Some(
                        self.factory.ref_(self).create_catch_clause(
                            Some(self.factory.ref_(self).create_variable_declaration(
                                Some(catch_variable.clone()),
                                None,
                                None,
                                None,
                            )),
                            self.factory
                                .ref_(self)
                                .create_block(
                                    vec![self.factory.ref_(self).create_expression_statement(
                                        self.factory.ref_(self).create_assignment(
                                            error_record.clone(),
                                            self.factory
                                                .ref_(self)
                                                .create_object_literal_expression(
                                                    Some(vec![self
                                                        .factory
                                                        .ref_(self)
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
                                .set_emit_flags(EmitFlags::SingleLine, self),
                        ),
                    ),
                    Some(self.factory.ref_(self).create_block(
                        vec![self.factory.ref_(self).create_try_statement(
                                self.factory.ref_(self).create_block(
                                    vec![self
                                        .factory
                                        .ref_(self)
                                        .create_if_statement(
                                            self.factory.ref_(self).create_logical_and(
                                                self.factory.ref_(self).create_logical_and(
                                                    result,
                                                    self.factory
                                                        .ref_(self)
                                                        .create_logical_not(get_done),
                                                ),
                                                self.factory.ref_(self).create_assignment(
                                                    return_method,
                                                    self.factory
                                                        .ref_(self)
                                                        .create_property_access_expression(
                                                            iterator, "return",
                                                        ),
                                                ),
                                            ),
                                            self.factory.ref_(self).create_expression_statement(
                                                self.create_downlevel_await(call_return),
                                            ),
                                            None,
                                        )
                                        .set_emit_flags(EmitFlags::SingleLine, self)],
                                    None,
                                ),
                                None,
                                Some(
                                    self.factory
                                        .ref_(self)
                                        .create_block(
                                            vec![self
                                                .factory
                                                .ref_(self)
                                                .create_if_statement(
                                                    error_record.clone(),
                                                    self.factory.ref_(self).create_throw_statement(
                                                        self.factory
                                                            .ref_(self)
                                                            .create_property_access_expression(
                                                                error_record,
                                                                "error",
                                                            ),
                                                    ),
                                                    None,
                                                )
                                                .set_emit_flags(EmitFlags::SingleLine, self)],
                                            None,
                                        )
                                        .set_emit_flags(EmitFlags::SingleLine, self),
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
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> Id<Node /*ParameterDeclaration*/> {
        let node_ref = node.ref_(self);
        let node_as_parameter_declaration = node_ref.as_parameter_declaration();
        if node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsObjectRestOrSpread)
        {
            return self.factory.ref_(self).update_parameter_declaration(
                node,
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                node_as_parameter_declaration.dot_dot_dot_token.clone(),
                Some(
                    self.factory
                        .ref_(self)
                        .get_generated_name_for_node(Some(node), None),
                ),
                None,
                None,
                maybe_visit_node(
                    node_as_parameter_declaration.maybe_initializer(),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            );
        }
        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    fn visit_constructor_declaration(
        &self,
        node: Id<Node>, /*ConstructorDeclaration*/
    ) -> VisitResult {
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.ref_(self).update_constructor_declaration(
            node,
            Option::<Id<NodeArray>>::None,
            node.ref_(self).maybe_modifiers(),
            visit_parameter_list(
                Some(node.ref_(self).as_constructor_declaration().parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .unwrap(),
            Some(self.transform_function_body(node)),
        );
        self.set_enclosing_function_flags(saved_enclosing_function_flags);
        Some(updated.into())
    }

    fn visit_get_accessor_declaration(
        &self,
        node: Id<Node>, /*GetAccessorDeclaration*/
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_get_accessor_declaration = node_ref.as_get_accessor_declaration();
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.ref_(self).update_get_accessor_declaration(
            node,
            Option::<Id<NodeArray>>::None,
            node.ref_(self).maybe_modifiers(),
            visit_node(
                node_as_get_accessor_declaration.name(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_property_name(&node.ref_(self))),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            visit_parameter_list(
                Some(node_as_get_accessor_declaration.parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
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
        node: Id<Node>, /*SetAccessorDeclaration*/
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_set_accessor_declaration = node_ref.as_set_accessor_declaration();
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.ref_(self).update_set_accessor_declaration(
            node,
            Option::<Id<NodeArray>>::None,
            node.ref_(self).maybe_modifiers(),
            visit_node(
                node_as_set_accessor_declaration.name(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_property_name(&node.ref_(self))),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            visit_parameter_list(
                Some(node_as_set_accessor_declaration.parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .unwrap(),
            Some(self.transform_function_body(node)),
        );
        self.set_enclosing_function_flags(saved_enclosing_function_flags);
        Some(updated.into())
    }

    fn visit_method_declaration(&self, node: Id<Node> /*MethodDeclaration*/) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_method_declaration = node_ref.as_method_declaration();
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.ref_(self).update_method_declaration(
            node,
            Option::<Id<NodeArray>>::None,
            if self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
            {
                maybe_visit_nodes(
                    node.ref_(self).maybe_modifiers(),
                    Some(|node: Id<Node>| self.visitor_no_async_modifier(node)),
                    Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                    None,
                    None,
                    self,
                )
            } else {
                node.ref_(self).maybe_modifiers()
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
                node_as_method_declaration.name(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_property_name(&node.ref_(self))),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            maybe_visit_node(
                Option::<Id<Node>>::None,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_token(&node.ref_(self))),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            Option::<Id<NodeArray>>::None,
            visit_parameter_list(
                Some(node_as_method_declaration.parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
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

    fn visit_function_declaration(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_function_declaration = node_ref.as_function_declaration();
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.ref_(self).update_function_declaration(
            node,
            Option::<Id<NodeArray>>::None,
            if self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
            {
                maybe_visit_nodes(
                    node.ref_(self).maybe_modifiers(),
                    Some(|node: Id<Node>| self.visitor_no_async_modifier(node)),
                    Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                    None,
                    None,
                    self,
                )
            } else {
                node.ref_(self).maybe_modifiers()
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
            Option::<Id<NodeArray>>::None,
            visit_parameter_list(
                Some(node_as_function_declaration.parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
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

    fn visit_arrow_function(&self, node: Id<Node> /*ArrowFunction*/) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_arrow_function = node_ref.as_arrow_function();
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.ref_(self).update_arrow_function(
            node,
            node.ref_(self).maybe_modifiers(),
            Option::<Id<NodeArray>>::None,
            visit_parameter_list(
                Some(node_as_arrow_function.parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .unwrap(),
            None,
            node_as_arrow_function.equals_greater_than_token.clone(),
            self.transform_function_body(node),
        );
        self.set_enclosing_function_flags(saved_enclosing_function_flags);
        Some(updated.into())
    }

    fn visit_function_expression(&self, node: Id<Node> /*FunctionExpression*/) -> VisitResult {
        let saved_enclosing_function_flags = self.maybe_enclosing_function_flags();
        self.set_enclosing_function_flags(Some(FunctionFlags::Normal));
        let updated = self.factory.ref_(self).update_function_expression(
            node,
            if self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Generator)
                })
            {
                maybe_visit_nodes(
                    node.ref_(self).maybe_modifiers(),
                    Some(|node: Id<Node>| self.visitor_no_async_modifier(node)),
                    Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                    None,
                    None,
                    self,
                )
            } else {
                node.ref_(self).maybe_modifiers()
            },
            if self
                .maybe_enclosing_function_flags()
                .matches(|enclosing_function_flags| {
                    enclosing_function_flags.intersects(FunctionFlags::Async)
                })
            {
                None
            } else {
                node.ref_(self)
                    .as_function_expression()
                    .maybe_asterisk_token()
            },
            released!(node.ref_(self).as_function_expression().maybe_name()),
            Option::<Id<NodeArray>>::None,
            visit_parameter_list(
                released!(Some(node.ref_(self).as_function_expression().parameters())),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
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
        node: Id<Node>, /*MethodDeclaration | AccessorDeclaration | FunctionDeclaration | FunctionExpression*/
    ) -> Id<Node /*FunctionBody*/> {
        let node_ref = node.ref_(self);
        let node_as_function_like_declaration = node_ref.as_function_like_declaration();
        self.context.ref_(self).resume_lexical_environment();
        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
        let statement_offset = self.factory.ref_(self).copy_prologue(
            &node_as_function_like_declaration
                .maybe_body()
                .unwrap()
                .ref_(self)
                .as_block()
                .statements
                .ref_(self),
            &mut statements,
            Some(false),
            Some(|node: Id<Node>| self.visitor(node)),
        );
        let mut statements = self
            .append_object_rest_assignments_if_needed(Some(statements), node)
            .unwrap();

        let saved_captured_super_properties = self.maybe_captured_super_properties().clone();
        let saved_has_super_element_access = self.has_super_element_access();
        self.set_captured_super_properties(Some(Default::default()));
        self.set_has_super_element_access(false);

        let return_statement = self.factory.ref_(self).create_return_statement(Some(
            self.emit_helpers().create_async_generator_helper(
                self.factory.ref_(self).create_function_expression(
                    Option::<Id<NodeArray>>::None,
                    Some(
                        self.factory
                            .ref_(self)
                            .create_token(SyntaxKind::AsteriskToken),
                    ),
                    node_as_function_like_declaration
                        .maybe_name()
                        .map(|node_name| {
                            self.factory
                                .ref_(self)
                                .get_generated_name_for_node(Some(node_name), None)
                        }),
                    Option::<Id<NodeArray>>::None,
                    Some(vec![]),
                    None,
                    self.factory.ref_(self).update_block(
                        node_as_function_like_declaration.maybe_body().unwrap(),
                        visit_lexical_environment(
                            node_as_function_like_declaration
                                .maybe_body()
                                .unwrap()
                                .ref_(self)
                                .as_block()
                                .statements,
                            |node: Id<Node>| self.visitor(node),
                            &*self.context.ref_(self),
                            Some(statement_offset),
                            None,
                            Option::<
                                fn(
                                    Option<Id<NodeArray>>,
                                    Option<&mut dyn FnMut(Id<Node>) -> VisitResult>,
                                    Option<&dyn Fn(Id<Node>) -> bool>,
                                    Option<usize>,
                                    Option<usize>,
                                ) -> Option<Id<NodeArray>>,
                            >::None,
                            self,
                        ),
                    ),
                ),
                self.hierarchy_facts()
                    .intersects(HierarchyFacts::HasLexicalThis),
            ),
        ));

        let emit_super_helpers = self.language_version >= ScriptTarget::ES2015
            && self
                .resolver
                .ref_(self)
                .get_node_check_flags(node)
                .intersects(
                    NodeCheckFlags::AsyncMethodWithSuperBinding
                        | NodeCheckFlags::AsyncMethodWithSuper,
                );

        if emit_super_helpers {
            self.enable_substitution_for_async_methods_with_super();
            let variable_statement = create_super_access_variable_statement(
                &self.factory.ref_(self),
                &**self.resolver.ref_(self),
                node,
                &self.captured_super_properties(),
            );
            self.substituted_super_accessors_mut()
                .insert(get_node_id(&variable_statement.ref_(self)), true);
            insert_statements_after_standard_prologue(
                &mut statements,
                Some(&[variable_statement]),
                self,
            );
        }

        statements.push(return_statement);

        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.ref_(self).end_lexical_environment().as_deref(),
            self,
        );
        let block = self.factory.ref_(self).update_block(
            node_as_function_like_declaration.maybe_body().unwrap(),
            statements,
        );

        if emit_super_helpers && self.has_super_element_access() {
            if self
                .resolver
                .ref_(self)
                .get_node_check_flags(node)
                .intersects(NodeCheckFlags::AsyncMethodWithSuperBinding)
            {
                add_emit_helper(block, advanced_async_super_helper(self), self);
            } else if self
                .resolver
                .ref_(self)
                .get_node_check_flags(node)
                .intersects(NodeCheckFlags::AsyncMethodWithSuper)
            {
                add_emit_helper(block, async_super_helper(self), self);
            }
        }

        self.set_captured_super_properties(saved_captured_super_properties);
        self.set_has_super_element_access(saved_has_super_element_access);

        block
    }

    fn transform_function_body(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration*/
    ) -> Id<Node /*ConciseBody*/> {
        self.context.ref_(self).resume_lexical_environment();
        let mut statement_offset = 0;
        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
        let body = maybe_visit_node(
            released!(node.ref_(self).as_function_like_declaration().maybe_body()),
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_concise_body(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )
        .unwrap_or_else(|| self.factory.ref_(self).create_block(vec![], None));
        if is_block(&body.ref_(self)) {
            statement_offset = self.factory.ref_(self).copy_prologue(
                &body.ref_(self).as_block().statements.ref_(self),
                &mut statements,
                Some(false),
                Some(|node: Id<Node>| self.visitor(node)),
            );
        }
        statements.add_range(
            self.append_object_rest_assignments_if_needed(None, node)
                .as_deref(),
            None,
            None,
        );
        let leading_statements = self.context.ref_(self).end_lexical_environment();
        if statement_offset > 0
            || !statements.is_empty()
            || some(
                leading_statements.as_deref(),
                Option::<fn(&Id<Node>) -> bool>::None,
            )
        {
            let block = self
                .factory
                .ref_(self)
                .converters()
                .convert_to_function_block(body, Some(true));
            insert_statements_after_standard_prologue(
                &mut statements,
                leading_statements.as_deref(),
                self,
            );
            statements.add_range(
                Some(&block.ref_(self).as_block().statements.ref_(self)[statement_offset..]),
                None,
                None,
            );
            return self.factory.ref_(self).update_block(
                block,
                self.factory
                    .ref_(self)
                    .create_node_array(Some(statements), None)
                    .set_text_range(
                        Some(&*released!(block.ref_(self).as_block().statements).ref_(self)),
                        self,
                    ),
            );
        }
        body
    }

    fn append_object_rest_assignments_if_needed(
        &self,
        mut statements: Option<Vec<Id<Node /*Statement*/>>>,
        node: Id<Node>, /*FunctionLikeDeclaration*/
    ) -> Option<Vec<Id<Node /*Statement*/>>> {
        for &parameter in &*node
            .ref_(self)
            .as_function_like_declaration()
            .parameters()
            .ref_(self)
        {
            if parameter
                .ref_(self)
                .transform_flags()
                .intersects(TransformFlags::ContainsObjectRestOrSpread)
            {
                let temp = self
                    .factory
                    .ref_(self)
                    .get_generated_name_for_node(Some(parameter), None);
                let declarations = flatten_destructuring_binding(
                    parameter,
                    |node: Id<Node>| self.visitor(node),
                    self.context.clone(),
                    FlattenLevel::ObjectRest,
                    Some(temp),
                    Some(false),
                    Some(true),
                    self,
                );
                if !declarations.is_empty() {
                    let statement = self.factory.ref_(self).create_variable_statement(
                        Option::<Id<NodeArray>>::None,
                        self.factory
                            .ref_(self)
                            .create_variable_declaration_list(declarations, None),
                    );
                    set_emit_flags(statement, EmitFlags::CustomPrologue, self);
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

            self.context
                .ref_(self)
                .enable_substitution(SyntaxKind::CallExpression);
            self.context
                .ref_(self)
                .enable_substitution(SyntaxKind::PropertyAccessExpression);
            self.context
                .ref_(self)
                .enable_substitution(SyntaxKind::ElementAccessExpression);

            self.context
                .ref_(self)
                .enable_emit_notification(SyntaxKind::ClassDeclaration);
            self.context
                .ref_(self)
                .enable_emit_notification(SyntaxKind::MethodDeclaration);
            self.context
                .ref_(self)
                .enable_emit_notification(SyntaxKind::GetAccessor);
            self.context
                .ref_(self)
                .enable_emit_notification(SyntaxKind::SetAccessor);
            self.context
                .ref_(self)
                .enable_emit_notification(SyntaxKind::Constructor);
            self.context
                .ref_(self)
                .enable_emit_notification(SyntaxKind::VariableStatement);
        }
    }
}

impl TransformerInterface for TransformES2018 {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl_has_arena!(TransformES2018);

struct TransformES2018OnEmitNodeOverrider {
    arena: *const AllArenas,
    transform_es2018: Transformer,
    previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformES2018OnEmitNodeOverrider {
    fn new(
        transform_es2018: Transformer,
        previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_es2018,
            previous_on_emit_node,
        }
    }

    fn transform_es2018(&self) -> debug_cell::Ref<'_, TransformES2018> {
        downcast_transformer_ref(self.transform_es2018, self)
    }

    fn is_super_container(&self, node: Id<Node>) -> bool {
        let kind = node.ref_(self).kind();
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
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        if self
            .transform_es2018()
            .enabled_substitutions()
            .intersects(ESNextSubstitutionFlags::AsyncMethodsWithSuper)
            && self.is_super_container(node)
        {
            let super_container_flags = self
                .transform_es2018()
                .resolver
                .ref_(self)
                .get_node_check_flags(node)
                & (NodeCheckFlags::AsyncMethodWithSuper
                    | NodeCheckFlags::AsyncMethodWithSuperBinding);
            if super_container_flags != self.transform_es2018().enclosing_super_container_flags() {
                let saved_enclosing_super_container_flags =
                    self.transform_es2018().enclosing_super_container_flags();
                self.transform_es2018()
                    .set_enclosing_super_container_flags(super_container_flags);
                self.previous_on_emit_node
                    .ref_(self)
                    .on_emit_node(hint, node, emit_callback)?;
                self.transform_es2018()
                    .set_enclosing_super_container_flags(saved_enclosing_super_container_flags);
                return Ok(());
            }
        } else if self.transform_es2018().enabled_substitutions() != ESNextSubstitutionFlags::None
            && self
                .transform_es2018()
                .substituted_super_accessors()
                .get(&get_node_id(&node.ref_(self)))
                .copied()
                == Some(true)
        {
            let saved_enclosing_super_container_flags =
                self.transform_es2018().enclosing_super_container_flags();
            self.transform_es2018()
                .set_enclosing_super_container_flags(NodeCheckFlags::None);
            self.previous_on_emit_node
                .ref_(self)
                .on_emit_node(hint, node, emit_callback)?;
            self.transform_es2018()
                .set_enclosing_super_container_flags(saved_enclosing_super_container_flags);
            return Ok(());
        }

        self.previous_on_emit_node
            .ref_(self)
            .on_emit_node(hint, node, emit_callback)?;

        Ok(())
    }
}

impl_has_arena!(TransformES2018OnEmitNodeOverrider);

struct TransformES2018OnSubstituteNodeOverrider {
    arena: *const AllArenas,
    transform_es2018: Transformer,
    previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformES2018OnSubstituteNodeOverrider {
    fn new(
        transform_es2018: Transformer,
        previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_es2018,
            previous_on_substitute_node,
        }
    }

    fn transform_es2018(&self) -> debug_cell::Ref<'_, TransformES2018> {
        downcast_transformer_ref(self.transform_es2018, self)
    }

    fn substitute_expression(&self, node: Id<Node> /*Expression*/) -> Id<Node> {
        match node.ref_(self).kind() {
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
        node
    }

    fn substitute_property_access_expression(
        &self,
        node: Id<Node>, /*PropertyAccessExpression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_property_access_expression = node_ref.as_property_access_expression();
        if node_as_property_access_expression
            .expression
            .ref_(self)
            .kind()
            == SyntaxKind::SuperKeyword
        {
            return self
                .transform_es2018()
                .factory
                .ref_(self)
                .create_property_access_expression(
                    self.transform_es2018()
                        .factory
                        .ref_(self)
                        .create_unique_name(
                            "_super",
                            Some(
                                GeneratedIdentifierFlags::Optimistic
                                    | GeneratedIdentifierFlags::FileLevel,
                            ),
                        ),
                    node_as_property_access_expression.name.clone(),
                )
                .set_text_range(Some(&*node.ref_(self)), self);
        }
        node
    }

    fn substitute_element_access_expression(
        &self,
        node: Id<Node>, /*ElementAccessExpression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_element_access_expression = node_ref.as_element_access_expression();
        if node_as_element_access_expression
            .expression
            .ref_(self)
            .kind()
            == SyntaxKind::SuperKeyword
        {
            return self.create_super_element_access_in_async_method(
                node_as_element_access_expression.argument_expression,
                &*node.ref_(self),
            );
        }
        node
    }

    fn substitute_call_expression(&self, node: Id<Node> /*CallExpression*/) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        let expression = node_as_call_expression.expression;
        if is_super_property(expression, self) {
            let argument_expression = if is_property_access_expression(&expression.ref_(self)) {
                self.substitute_property_access_expression(expression)
            } else {
                self.substitute_element_access_expression(expression)
            };
            return self
                .transform_es2018()
                .factory
                .ref_(self)
                .create_call_expression(
                    self.transform_es2018()
                        .factory
                        .ref_(self)
                        .create_property_access_expression(argument_expression, "call"),
                    Option::<Id<NodeArray>>::None,
                    Some(
                        vec![self.transform_es2018().factory.ref_(self).create_this()].and_extend(
                            node_as_call_expression.arguments.ref_(self).iter().cloned(),
                        ),
                    ),
                );
        }
        node
    }

    fn create_super_element_access_in_async_method(
        &self,
        argument_expression: Id<Node>, /*Expression*/
        location: &impl ReadonlyTextRange,
    ) -> Id<Node /*LeftHandSideExpression*/> {
        if self
            .transform_es2018()
            .enclosing_super_container_flags()
            .intersects(NodeCheckFlags::AsyncMethodWithSuperBinding)
        {
            self.transform_es2018()
                .factory
                .ref_(self)
                .create_property_access_expression(
                    self.transform_es2018()
                        .factory
                        .ref_(self)
                        .create_call_expression(
                            self.transform_es2018()
                                .factory
                                .ref_(self)
                                .create_identifier("_superIndex"),
                            Option::<Id<NodeArray>>::None,
                            Some(vec![argument_expression]),
                        ),
                    "value",
                )
                .set_text_range(Some(location), self)
        } else {
            self.transform_es2018()
                .factory
                .ref_(self)
                .create_call_expression(
                    self.transform_es2018()
                        .factory
                        .ref_(self)
                        .create_identifier("_superIndex"),
                    Option::<Id<NodeArray>>::None,
                    Some(vec![argument_expression]),
                )
                .set_text_range(Some(location), self)
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider for TransformES2018OnSubstituteNodeOverrider {
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        let node = self
            .previous_on_substitute_node
            .ref_(self)
            .on_substitute_node(hint, node)?;
        if hint == EmitHint::Expression
            && self.transform_es2018().enclosing_super_container_flags() != NodeCheckFlags::None
        {
            return Ok(self.substitute_expression(node));
        }
        Ok(node)
    }
}

impl_has_arena!(TransformES2018OnSubstituteNodeOverrider);

struct TransformES2018Factory {
    arena: *const AllArenas,
}

impl TransformES2018Factory {
    fn new(arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
        }
    }
}

impl TransformerFactoryInterface for TransformES2018Factory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle(self)
            .ref_(self)
            .call(context.clone(), TransformES2018::new(context, self.arena))
    }
}

impl_has_arena!(TransformES2018Factory);

pub fn transform_es2018(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformES2018Factory::new(arena)))
}
