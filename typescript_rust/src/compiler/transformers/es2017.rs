use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashSet,
};

use bitflags::bitflags;
use gc::{Finalize, Gc, Trace};

use crate::{
    is_expression, is_for_initializer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface, __String, add_emit_helpers, chain_bundle, get_emit_script_target,
    is_effective_strict_mode_source_file, is_identifier, is_node_with_possible_hoisted_declaration,
    is_omitted_expression, is_property_access_expression, is_token, ref_unwrapped,
    visit_each_child, visit_iteration_body, visit_node, with_synthetic_factory,
    BaseNodeFactorySynthetic, CompilerOptions, Debug_, EmitResolver, Node, NodeArray,
    NodeCheckFlags, NodeFactory, NodeInterface, ScriptTarget, SyntaxKind, TransformFlags,
    TransformationContext, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, Transformer, VisitResult,
};

bitflags! {
    pub(crate) struct ES2017SubstitutionFlags: u32 {
        const None = 0;
        const AsyncMethodsWithSuper = 1 << 0;
    }
}

bitflags! {
    pub(crate) struct ContextFlags: u32 {
        const None = 0;
        const NonTopLevel = 1 << 0;
        const HasLexicalThis = 1 << 1;
    }
}

#[derive(Trace, Finalize)]
struct TransformES2017 {
    context: Gc<Box<dyn TransformationContext>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    resolver: Gc<Box<dyn EmitResolver>>,
    compiler_options: Gc<CompilerOptions>,
    #[unsafe_ignore_trace]
    language_version: ScriptTarget,
    #[unsafe_ignore_trace]
    enabled_substitutions: Cell<Option<ES2017SubstitutionFlags>>,
    #[unsafe_ignore_trace]
    enclosing_super_container_flags: Cell<NodeCheckFlags>,
    #[unsafe_ignore_trace]
    enclosing_function_parameter_names: RefCell<Option<HashSet<__String>>>,
    #[unsafe_ignore_trace]
    captured_super_properties: RefCell<Option<HashSet<__String>>>,
    #[unsafe_ignore_trace]
    has_super_element_access: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    substituted_super_accessors: RefCell<Vec<bool>>,
    #[unsafe_ignore_trace]
    context_flags: Cell<ContextFlags>,
}

impl TransformES2017 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        let compiler_options = context.get_compiler_options();

        let ret: Transformer = Gc::new(Box::new(Self {
            factory: context.factory(),
            resolver: context.get_emit_resolver(),
            context: context.clone(),
            language_version: get_emit_script_target(&compiler_options),
            compiler_options,
            enabled_substitutions: Default::default(),
            enclosing_super_container_flags: Cell::new(NodeCheckFlags::None),
            enclosing_function_parameter_names: Default::default(),
            captured_super_properties: Default::default(),
            has_super_element_access: Default::default(),
            substituted_super_accessors: Default::default(),
            context_flags: Cell::new(ContextFlags::None),
        }));
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformES2017OnEmitNodeOverrider::new(
                ret.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformES2017OnSubstituteNodeOverrider::new(
                ret.clone(),
                previous_on_substitute_node,
            )))
        });
        ret
    }

    fn enclosing_function_parameter_names(&self) -> Ref<HashSet<__String>> {
        ref_unwrapped(&self.enclosing_function_parameter_names)
    }

    fn set_enclosing_function_parameter_names(
        &self,
        enclosing_function_parameter_names: HashSet<__String>,
    ) {
        *self.enclosing_function_parameter_names.borrow_mut() =
            Some(enclosing_function_parameter_names);
    }

    fn maybe_captured_super_properties(&self) -> Ref<Option<HashSet<__String>>> {
        self.captured_super_properties.borrow()
    }

    fn maybe_captured_super_properties_mut(&self) -> RefMut<Option<HashSet<__String>>> {
        self.captured_super_properties.borrow_mut()
    }

    fn maybe_has_super_element_access(&self) -> Option<bool> {
        self.has_super_element_access.get()
    }

    fn set_has_super_element_access(&self, has_super_element_access: Option<bool>) {
        self.has_super_element_access.set(has_super_element_access);
    }

    fn context_flags(&self) -> ContextFlags {
        self.context_flags.get()
    }

    fn set_context_flags(&self, flags: ContextFlags) {
        self.context_flags.set(flags);
    }

    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        if node.as_source_file().is_declaration_file() {
            return node.node_wrapper();
        }

        self.set_context_flag(ContextFlags::NonTopLevel, false);
        self.set_context_flag(
            ContextFlags::HasLexicalThis,
            !is_effective_strict_mode_source_file(node, &self.compiler_options),
        );
        let visited = visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
            Option::<
                fn(
                    Option<&NodeArray>,
                    Option<fn(&Node) -> VisitResult>,
                    Option<fn(&Node) -> bool>,
                    Option<usize>,
                    Option<usize>,
                ) -> NodeArray,
            >::None,
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<fn(&Node) -> VisitResult>,
                    Option<fn(&Node) -> bool>,
                    Option<fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .unwrap();
        add_emit_helpers(&visited, self.context.read_emit_helpers().as_deref());
        visited
    }

    fn set_context_flag(&self, flag: ContextFlags, val: bool) {
        self.set_context_flags(if val {
            self.context_flags() | flag
        } else {
            self.context_flags() & !flag
        });
    }

    fn in_context(&self, flags: ContextFlags) -> bool {
        self.context_flags().intersects(flags)
    }

    fn in_top_level_context(&self) -> bool {
        !self.in_context(ContextFlags::NonTopLevel)
    }

    fn in_has_lexical_this_context(&self) -> bool {
        self.in_context(ContextFlags::HasLexicalThis)
    }

    fn do_with_context<TValue, TReturn>(
        &self,
        flags: ContextFlags,
        mut cb: impl FnMut(&TValue) -> TReturn,
        value: &TValue,
    ) -> TReturn {
        let context_flags_to_set = flags & !self.context_flags();
        if context_flags_to_set != ContextFlags::None {
            self.set_context_flag(context_flags_to_set, true);
            let result = cb(value);
            self.set_context_flag(context_flags_to_set, false);
            return result;
        }
        cb(value)
    }

    fn visit_default(&self, node: &Node) -> VisitResult {
        visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
            Option::<
                fn(
                    Option<&NodeArray>,
                    Option<fn(&Node) -> VisitResult>,
                    Option<fn(&Node) -> bool>,
                    Option<usize>,
                    Option<usize>,
                ) -> NodeArray,
            >::None,
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<fn(&Node) -> VisitResult>,
                    Option<fn(&Node) -> bool>,
                    Option<fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .map(Into::into)
    }

    fn visitor(&self, node: &Node) -> VisitResult {
        if !node
            .transform_flags()
            .intersects(TransformFlags::ContainsES2017)
        {
            return Some(node.node_wrapper().into());
        }
        match node.kind() {
            SyntaxKind::AsyncKeyword => None,

            SyntaxKind::AwaitExpression => Some(self.visit_await_expression(node).into()),

            SyntaxKind::MethodDeclaration => Some(
                self.do_with_context(
                    ContextFlags::NonTopLevel | ContextFlags::HasLexicalThis,
                    |node: &Node| self.visit_method_declaration(node),
                    node,
                )
                .into(),
            ),

            SyntaxKind::FunctionDeclaration => self.do_with_context(
                ContextFlags::NonTopLevel | ContextFlags::HasLexicalThis,
                |node: &Node| self.visit_function_declaration(node),
                node,
            ),

            SyntaxKind::FunctionExpression => Some(
                self.do_with_context(
                    ContextFlags::NonTopLevel | ContextFlags::HasLexicalThis,
                    |node: &Node| self.visit_function_expression(node),
                    node,
                )
                .into(),
            ),

            SyntaxKind::ArrowFunction => Some(
                self.do_with_context(
                    ContextFlags::NonTopLevel,
                    |node: &Node| self.visit_arrow_function(node),
                    node,
                )
                .into(),
            ),

            SyntaxKind::PropertyAccessExpression => {
                if let Some(captured_super_properties) =
                    self.maybe_captured_super_properties_mut().as_mut()
                {
                    if is_property_access_expression(node) {
                        let node_as_property_access_expression =
                            node.as_property_access_expression();
                        if node_as_property_access_expression.expression.kind()
                            == SyntaxKind::SuperKeyword
                        {
                            captured_super_properties.insert(
                                node_as_property_access_expression
                                    .name
                                    .as_member_name()
                                    .escaped_text()
                                    .to_owned(),
                            );
                        }
                    }
                }
                visit_each_child(
                    Some(node),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    Option::<
                        fn(
                            Option<&NodeArray>,
                            Option<fn(&Node) -> VisitResult>,
                            Option<fn(&Node) -> bool>,
                            Option<usize>,
                            Option<usize>,
                        ) -> NodeArray,
                    >::None,
                    Option::<fn(&Node) -> VisitResult>::None,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<fn(&Node) -> VisitResult>,
                            Option<fn(&Node) -> bool>,
                            Option<fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
                )
                .map(Into::into)
            }

            SyntaxKind::ElementAccessExpression => {
                if self.maybe_captured_super_properties().is_some()
                    && node.as_element_access_expression().expression.kind()
                        == SyntaxKind::SuperKeyword
                {
                    self.set_has_super_element_access(Some(true));
                }
                visit_each_child(
                    Some(node),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    Option::<
                        fn(
                            Option<&NodeArray>,
                            Option<fn(&Node) -> VisitResult>,
                            Option<fn(&Node) -> bool>,
                            Option<usize>,
                            Option<usize>,
                        ) -> NodeArray,
                    >::None,
                    Option::<fn(&Node) -> VisitResult>::None,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<fn(&Node) -> VisitResult>,
                            Option<fn(&Node) -> bool>,
                            Option<fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
                )
                .map(Into::into)
            }

            SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::Constructor
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::ClassExpression => self.do_with_context(
                ContextFlags::NonTopLevel | ContextFlags::HasLexicalThis,
                |node: &Node| self.visit_default(node),
                node,
            ),

            _ => visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<fn(&Node) -> VisitResult>,
                        Option<fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> NodeArray,
                >::None,
                Option::<fn(&Node) -> VisitResult>::None,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<fn(&Node) -> VisitResult>,
                        Option<fn(&Node) -> bool>,
                        Option<fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
            .map(Into::into),
        }
    }

    fn async_body_visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        if is_node_with_possible_hoisted_declaration(node) {
            return match node.kind() {
                SyntaxKind::VariableStatement => self
                    .visit_variable_statement_in_async_body(node)
                    .map(Into::into),
                SyntaxKind::ForStatement => {
                    Some(self.visit_for_statement_in_async_body(node).into())
                }
                SyntaxKind::ForInStatement => {
                    Some(self.visit_for_in_statement_in_async_body(node).into())
                }
                SyntaxKind::ForOfStatement => {
                    Some(self.visit_for_of_statement_in_async_body(node).into())
                }
                SyntaxKind::CatchClause => Some(self.visit_catch_clause_in_async_body(node).into()),
                SyntaxKind::Block
                | SyntaxKind::SwitchStatement
                | SyntaxKind::CaseBlock
                | SyntaxKind::CaseClause
                | SyntaxKind::DefaultClause
                | SyntaxKind::TryStatement
                | SyntaxKind::DoStatement
                | SyntaxKind::WhileStatement
                | SyntaxKind::IfStatement
                | SyntaxKind::WithStatement
                | SyntaxKind::LabeledStatement => visit_each_child(
                    Some(node),
                    |node: &Node| self.async_body_visitor(node),
                    &**self.context,
                    Option::<
                        fn(
                            Option<&NodeArray>,
                            Option<fn(&Node) -> VisitResult>,
                            Option<fn(&Node) -> bool>,
                            Option<usize>,
                            Option<usize>,
                        ) -> NodeArray,
                    >::None,
                    Option::<fn(&Node) -> VisitResult>::None,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<fn(&Node) -> VisitResult>,
                            Option<fn(&Node) -> bool>,
                            Option<fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
                )
                .map(Into::into),
                _ => Debug_.assert_never(node, Some("Unhandled node.")),
            };
        }
        self.visitor(node)
    }

    fn visit_catch_clause_in_async_body(&self, node: &Node /*CatchClause*/) -> Gc<Node> {
        let mut catch_clause_names: HashSet<__String> = Default::default();
        let node_as_catch_clause = node.as_catch_clause();
        self.record_declaration_name(
            node_as_catch_clause.variable_declaration.as_ref().unwrap(),
            &mut catch_clause_names,
        );

        let mut catch_clause_unshadowed_names: Option<HashSet<__String>> = Default::default();
        catch_clause_names
            .iter()
            .for_each(|escaped_name: &__String| {
                if self
                    .enclosing_function_parameter_names()
                    .contains(escaped_name)
                {
                    catch_clause_unshadowed_names
                        .get_or_insert_with(|| self.enclosing_function_parameter_names().clone())
                        .remove(escaped_name);
                }
            });

        if let Some(catch_clause_unshadowed_names) = catch_clause_unshadowed_names {
            let saved_enclosing_function_parameter_names =
                self.enclosing_function_parameter_names().clone();
            self.set_enclosing_function_parameter_names(catch_clause_unshadowed_names);
            let result = visit_each_child(
                Some(node),
                |node: &Node| self.async_body_visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<fn(&Node) -> VisitResult>,
                        Option<fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> NodeArray,
                >::None,
                Option::<fn(&Node) -> VisitResult>::None,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<fn(&Node) -> VisitResult>,
                        Option<fn(&Node) -> bool>,
                        Option<fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
            .unwrap();
            self.set_enclosing_function_parameter_names(saved_enclosing_function_parameter_names);
            result
        } else {
            visit_each_child(
                Some(node),
                |node: &Node| self.async_body_visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<fn(&Node) -> VisitResult>,
                        Option<fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> NodeArray,
                >::None,
                Option::<fn(&Node) -> VisitResult>::None,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<fn(&Node) -> VisitResult>,
                        Option<fn(&Node) -> bool>,
                        Option<fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
            .unwrap()
        }
    }

    fn visit_variable_statement_in_async_body(
        &self,
        node: &Node, /*VariableStatement*/
    ) -> Option<Gc<Node>> {
        let node_as_variable_statement = node.as_variable_statement();
        if self.is_variable_declaration_list_with_colliding_name(
            &node_as_variable_statement.declaration_list,
        ) {
            let expression = self.visit_variable_declaration_list_with_colliding_names(
                &node_as_variable_statement.declaration_list,
                false,
            );
            return expression.map(|expression| {
                with_synthetic_factory(|synthetic_factory_| {
                    self.factory
                        .create_expression_statement(synthetic_factory_, expression)
                        .into()
                })
            });
        }
        visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
            Option::<
                fn(
                    Option<&NodeArray>,
                    Option<fn(&Node) -> VisitResult>,
                    Option<fn(&Node) -> bool>,
                    Option<usize>,
                    Option<usize>,
                ) -> NodeArray,
            >::None,
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<fn(&Node) -> VisitResult>,
                    Option<fn(&Node) -> bool>,
                    Option<fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
    }

    fn visit_for_in_statement_in_async_body(
        &self,
        node: &Node, /*ForInStatement*/
    ) -> Gc<Node> {
        let node_as_for_in_statement = node.as_for_in_statement();
        with_synthetic_factory(|synthetic_factory_| {
            self.factory.update_for_in_statement(
                synthetic_factory_,
                node,
                if self.is_variable_declaration_list_with_colliding_name(
                    &node_as_for_in_statement.initializer,
                ) {
                    self.visit_variable_declaration_list_with_colliding_names(
                        &node_as_for_in_statement.initializer,
                        true,
                    )
                    .unwrap()
                } else {
                    visit_node(
                        Some(&*node_as_for_in_statement.initializer),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_for_initializer),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )
                    .unwrap()
                },
                visit_node(
                    Some(&*node_as_for_in_statement.expression),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
                .unwrap(),
                visit_iteration_body(
                    &node_as_for_in_statement.statement,
                    |node: &Node| self.async_body_visitor(node),
                    &**self.context,
                ),
            )
        })
    }

    fn visit_for_of_statement_in_async_body(
        &self,
        node: &Node, /*ForOfStatement*/
    ) -> Gc<Node> {
        let node_as_for_of_statement = node.as_for_of_statement();
        with_synthetic_factory(|synthetic_factory_| {
            self.factory.update_for_of_statement(
                synthetic_factory_,
                node,
                visit_node(
                    node_as_for_of_statement.await_modifier.as_deref(),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_token),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ),
                if self.is_variable_declaration_list_with_colliding_name(
                    &node_as_for_of_statement.initializer,
                ) {
                    self.visit_variable_declaration_list_with_colliding_names(
                        &node_as_for_of_statement.initializer,
                        true,
                    )
                    .unwrap()
                } else {
                    visit_node(
                        Some(&*node_as_for_of_statement.initializer),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_for_initializer),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )
                    .unwrap()
                },
                visit_node(
                    Some(&*node_as_for_of_statement.expression),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
                .unwrap(),
                visit_iteration_body(
                    &node_as_for_of_statement.statement,
                    |node: &Node| self.async_body_visitor(node),
                    &**self.context,
                ),
            )
        })
    }

    fn visit_for_statement_in_async_body(&self, node: &Node /*ForStatement*/) -> Gc<Node> {
        unimplemented!()
    }

    fn visit_await_expression(&self, node: &Node /*AwaitExpression*/) -> Gc<Node> {
        unimplemented!()
    }

    fn visit_method_declaration(&self, node: &Node /*MethodDeclaration*/) -> Gc<Node> {
        unimplemented!()
    }

    fn visit_function_declaration(&self, node: &Node /*FunctionDeclaration*/) -> VisitResult /*<Statement>*/
    {
        unimplemented!()
    }

    fn visit_function_expression(
        &self,
        node: &Node, /*FunctionExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    fn visit_arrow_function(&self, node: &Node /*ArrowFunction*/) -> Gc<Node> {
        unimplemented!()
    }

    fn record_declaration_name(&self, name: &Node, names: &mut HashSet<__String>) {
        if is_identifier(name) {
            names.insert(name.as_identifier().escaped_text.clone());
        } else {
            for element in name.as_has_elements().elements() {
                if !is_omitted_expression(element) {
                    self.record_declaration_name(element, names);
                }
            }
        }
    }

    fn is_variable_declaration_list_with_colliding_name(
        &self,
        node: &Node, /*ForInitializer*/
    ) -> bool {
        unimplemented!()
    }

    fn visit_variable_declaration_list_with_colliding_names(
        &self,
        node: &Node, /*VariableDeclarationList*/
        has_receiver: bool,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }
}

impl TransformerInterface for TransformES2017 {
    fn call(&self, node: &Node) -> Gc<Node> {
        self.transform_source_file(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformES2017OnEmitNodeOverrider {
    transform_es2017: Transformer,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformES2017OnEmitNodeOverrider {
    fn new(
        transform_es2017: Transformer,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es2017,
            previous_on_emit_node,
        }
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformES2017OnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: crate::EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(crate::EmitHint, &Node),
    ) {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2017OnSubstituteNodeOverrider {
    transform_es2017: Transformer,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformES2017OnSubstituteNodeOverrider {
    fn new(
        transform_es2017: Transformer,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es2017,
            previous_on_substitute_node,
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider for TransformES2017OnSubstituteNodeOverrider {
    fn on_substitute_node(&self, hint: crate::EmitHint, node: &Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2017Factory {}

impl TransformES2017Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2017Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(context.clone(), TransformES2017::new(context))
    }
}

pub fn transform_es2017() -> TransformerFactory {
    Gc::new(Box::new(TransformES2017Factory::new()))
}
