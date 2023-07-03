use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    io, mem,
};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, Trace};

use crate::{
    is_concise_body, is_expression, is_for_initializer, is_statement, try_map,
    try_visit_each_child, try_visit_function_body, try_visit_iteration_body, try_visit_node,
    try_visit_nodes, try_visit_parameter_list, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface, __String, add_emit_helper, add_emit_helpers, advanced_async_super_helper,
    async_super_helper, chain_bundle, concatenate, for_each, get_emit_script_target,
    get_entity_name_from_type_node, get_function_flags, get_initialized_variables, get_node_id,
    get_original_node, insert_statements_after_standard_prologue, is_block,
    is_effective_strict_mode_source_file, is_entity_name, is_identifier, is_modifier,
    is_node_with_possible_hoisted_declaration, is_omitted_expression,
    is_property_access_expression, is_super_property, is_token, is_variable_declaration_list,
    ref_mut_unwrapped, ref_unwrapped, set_emit_flags, set_original_node, set_source_map_range,
    set_text_range, set_text_range_node_array, set_text_range_rc_node, try_maybe_visit_each_child,
    try_maybe_visit_node, try_maybe_visit_nodes, unescape_leading_underscores,
    BaseNodeFactorySynthetic, CompilerOptions, Debug_, EmitFlags, EmitHint, EmitResolver,
    FunctionFlags, FunctionLikeDeclarationInterface, GeneratedIdentifierFlags,
    HasInitializerInterface, NamedDeclarationInterface, Node, NodeArray, NodeCheckFlags,
    NodeFactory, NodeFlags, NodeId, NodeInterface, NonEmpty, OptionTry, ReadonlyTextRange,
    ScriptTarget, SignatureDeclarationInterface, SyntaxKind, TransformFlags, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TypeReferenceSerializationKind, VisitResult,
};

bitflags! {
    struct ES2017SubstitutionFlags: u32 {
        const None = 0;
        const AsyncMethodsWithSuper = 1 << 0;
    }
}

bitflags! {
    struct ContextFlags: u32 {
        const None = 0;
        const NonTopLevel = 1 << 0;
        const HasLexicalThis = 1 << 1;
    }
}

#[derive(Trace, Finalize)]
struct TransformES2017 {
    _transformer_wrapper: GcCell<Option<Transformer>>,
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
    substituted_super_accessors: RefCell<HashMap<NodeId, bool>>,
    #[unsafe_ignore_trace]
    context_flags: Cell<ContextFlags>,
}

impl TransformES2017 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let compiler_options = context.get_compiler_options();

        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
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
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformES2017OnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformES2017OnSubstituteNodeOverrider::new(
                downcasted.clone(),
                previous_on_substitute_node,
            )))
        });
        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    fn maybe_enabled_substitutions(&self) -> Option<ES2017SubstitutionFlags> {
        self.enabled_substitutions.get()
    }

    fn set_enabled_substitutions(&self, enabled_substitutions: Option<ES2017SubstitutionFlags>) {
        self.enabled_substitutions.set(enabled_substitutions);
    }

    fn enclosing_super_container_flags(&self) -> NodeCheckFlags {
        self.enclosing_super_container_flags.get()
    }

    fn set_enclosing_super_container_flags(&self, enclosing_super_container_flags: NodeCheckFlags) {
        self.enclosing_super_container_flags
            .set(enclosing_super_container_flags);
    }

    fn maybe_enclosing_function_parameter_names(&self) -> Ref<Option<HashSet<__String>>> {
        self.enclosing_function_parameter_names.borrow()
    }

    fn enclosing_function_parameter_names(&self) -> Ref<HashSet<__String>> {
        ref_unwrapped(&self.enclosing_function_parameter_names)
    }

    fn enclosing_function_parameter_names_mut(&self) -> RefMut<HashSet<__String>> {
        ref_mut_unwrapped(&self.enclosing_function_parameter_names)
    }

    fn set_enclosing_function_parameter_names(
        &self,
        enclosing_function_parameter_names: Option<HashSet<__String>>,
    ) {
        *self.enclosing_function_parameter_names.borrow_mut() = enclosing_function_parameter_names;
    }

    fn maybe_captured_super_properties(&self) -> Ref<Option<HashSet<__String>>> {
        self.captured_super_properties.borrow()
    }

    fn captured_super_properties(&self) -> Ref<HashSet<__String>> {
        ref_unwrapped(&self.captured_super_properties)
    }

    fn maybe_captured_super_properties_mut(&self) -> RefMut<Option<HashSet<__String>>> {
        self.captured_super_properties.borrow_mut()
    }

    fn set_captured_super_properties(&self, captured_super_properties: Option<HashSet<__String>>) {
        *self.captured_super_properties.borrow_mut() = captured_super_properties;
    }

    fn maybe_has_super_element_access(&self) -> Option<bool> {
        self.has_super_element_access.get()
    }

    fn set_has_super_element_access(&self, has_super_element_access: Option<bool>) {
        self.has_super_element_access.set(has_super_element_access);
    }

    fn substituted_super_accessors(&self) -> Ref<HashMap<NodeId, bool>> {
        self.substituted_super_accessors.borrow()
    }

    fn substituted_super_accessors_mut(&self) -> RefMut<HashMap<NodeId, bool>> {
        self.substituted_super_accessors.borrow_mut()
    }

    fn context_flags(&self) -> ContextFlags {
        self.context_flags.get()
    }

    fn set_context_flags(&self, flags: ContextFlags) {
        self.context_flags.set(flags);
    }

    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> io::Result<Gc<Node>> {
        if node.as_source_file().is_declaration_file() {
            return Ok(node.node_wrapper());
        }

        self.set_context_flag(ContextFlags::NonTopLevel, false);
        self.set_context_flag(
            ContextFlags::HasLexicalThis,
            !is_effective_strict_mode_source_file(node, &self.compiler_options),
        );
        let visited =
            try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?;
        add_emit_helpers(&visited, self.context.read_emit_helpers().as_deref());
        Ok(visited)
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

    #[allow(dead_code)]
    fn do_with_context<TValue, TReturn>(
        &self,
        flags: ContextFlags,
        mut cb: impl FnMut(&TValue) -> TReturn,
        value: &TValue,
    ) -> TReturn {
        self.try_do_with_context(flags, |a| Ok(cb(a)), value)
            .unwrap()
    }

    fn try_do_with_context<TValue, TReturn>(
        &self,
        flags: ContextFlags,
        mut cb: impl FnMut(&TValue) -> io::Result<TReturn>,
        value: &TValue,
    ) -> io::Result<TReturn> {
        let context_flags_to_set = flags & !self.context_flags();
        if context_flags_to_set != ContextFlags::None {
            self.set_context_flag(context_flags_to_set, true);
            let result = cb(value)?;
            self.set_context_flag(context_flags_to_set, false);
            return Ok(result);
        }
        cb(value)
    }

    fn visit_default(&self, node: &Node) -> io::Result<VisitResult> {
        Ok(try_maybe_visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
        )?
        .map(Into::into))
    }

    fn visitor(&self, node: &Node) -> io::Result<VisitResult> {
        if !node
            .transform_flags()
            .intersects(TransformFlags::ContainsES2017)
        {
            return Ok(Some(node.node_wrapper().into()));
        }
        Ok(match node.kind() {
            SyntaxKind::AsyncKeyword => None,

            SyntaxKind::AwaitExpression => Some(self.visit_await_expression(node)?.into()),

            SyntaxKind::MethodDeclaration => Some(
                self.try_do_with_context(
                    ContextFlags::NonTopLevel | ContextFlags::HasLexicalThis,
                    |node: &Node| self.visit_method_declaration(node),
                    node,
                )?
                .into(),
            ),

            SyntaxKind::FunctionDeclaration => self.try_do_with_context(
                ContextFlags::NonTopLevel | ContextFlags::HasLexicalThis,
                |node: &Node| self.visit_function_declaration(node),
                node,
            )?,

            SyntaxKind::FunctionExpression => Some(
                self.try_do_with_context(
                    ContextFlags::NonTopLevel | ContextFlags::HasLexicalThis,
                    |node: &Node| self.visit_function_expression(node),
                    node,
                )?
                .into(),
            ),

            SyntaxKind::ArrowFunction => Some(
                self.try_do_with_context(
                    ContextFlags::NonTopLevel,
                    |node: &Node| self.visit_arrow_function(node),
                    node,
                )?
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
                try_maybe_visit_each_child(
                    Some(node),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                )?
                .map(Into::into)
            }

            SyntaxKind::ElementAccessExpression => {
                if self.maybe_captured_super_properties().is_some()
                    && node.as_element_access_expression().expression.kind()
                        == SyntaxKind::SuperKeyword
                {
                    self.set_has_super_element_access(Some(true));
                }
                try_maybe_visit_each_child(
                    Some(node),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                )?
                .map(Into::into)
            }

            SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::Constructor
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::ClassExpression => self.try_do_with_context(
                ContextFlags::NonTopLevel | ContextFlags::HasLexicalThis,
                |node: &Node| self.visit_default(node),
                node,
            )?,

            _ => try_maybe_visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
            )?
            .map(Into::into),
        })
    }

    fn async_body_visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/ {
        if is_node_with_possible_hoisted_declaration(node) {
            return Ok(match node.kind() {
                SyntaxKind::VariableStatement => self
                    .visit_variable_statement_in_async_body(node)?
                    .map(Into::into),
                SyntaxKind::ForStatement => {
                    Some(self.visit_for_statement_in_async_body(node)?.into())
                }
                SyntaxKind::ForInStatement => {
                    Some(self.visit_for_in_statement_in_async_body(node)?.into())
                }
                SyntaxKind::ForOfStatement => {
                    Some(self.visit_for_of_statement_in_async_body(node)?.into())
                }
                SyntaxKind::CatchClause => {
                    Some(self.visit_catch_clause_in_async_body(node)?.into())
                }
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
                | SyntaxKind::LabeledStatement => try_maybe_visit_each_child(
                    Some(node),
                    |node: &Node| self.async_body_visitor(node),
                    &**self.context,
                )?
                .map(Into::into),
                _ => Debug_.assert_never(node, Some("Unhandled node.")),
            });
        }
        self.visitor(node)
    }

    fn visit_catch_clause_in_async_body(
        &self,
        node: &Node, /*CatchClause*/
    ) -> io::Result<Gc<Node>> {
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

        Ok(
            if let Some(catch_clause_unshadowed_names) = catch_clause_unshadowed_names {
                let saved_enclosing_function_parameter_names =
                    self.maybe_enclosing_function_parameter_names().clone();
                self.set_enclosing_function_parameter_names(Some(catch_clause_unshadowed_names));
                let result = try_visit_each_child(
                    node,
                    |node: &Node| self.async_body_visitor(node),
                    &**self.context,
                )?;
                self.set_enclosing_function_parameter_names(
                    saved_enclosing_function_parameter_names,
                );
                result
            } else {
                try_visit_each_child(
                    node,
                    |node: &Node| self.async_body_visitor(node),
                    &**self.context,
                )?
            },
        )
    }

    fn visit_variable_statement_in_async_body(
        &self,
        node: &Node, /*VariableStatement*/
    ) -> io::Result<Option<Gc<Node>>> {
        let node_as_variable_statement = node.as_variable_statement();
        if self.is_variable_declaration_list_with_colliding_name(Some(
            &*node_as_variable_statement.declaration_list,
        )) {
            let expression = self.visit_variable_declaration_list_with_colliding_names(
                &node_as_variable_statement.declaration_list,
                false,
            )?;
            return Ok(
                expression.map(|expression| self.factory.create_expression_statement(expression))
            );
        }
        try_maybe_visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
        )
    }

    fn visit_for_in_statement_in_async_body(
        &self,
        node: &Node, /*ForInStatement*/
    ) -> io::Result<Gc<Node>> {
        let node_as_for_in_statement = node.as_for_in_statement();
        Ok(self.factory.update_for_in_statement(
            node,
            if self.is_variable_declaration_list_with_colliding_name(Some(
                &*node_as_for_in_statement.initializer,
            )) {
                self.visit_variable_declaration_list_with_colliding_names(
                    &node_as_for_in_statement.initializer,
                    true,
                )?
                .unwrap()
            } else {
                try_visit_node(
                    &node_as_for_in_statement.initializer,
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_for_initializer),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )?
            },
            try_visit_node(
                &node_as_for_in_statement.expression,
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            try_visit_iteration_body(
                &node_as_for_in_statement.statement,
                |node: &Node| self.async_body_visitor(node),
                &**self.context,
            )?,
        ))
    }

    fn visit_for_of_statement_in_async_body(
        &self,
        node: &Node, /*ForOfStatement*/
    ) -> io::Result<Gc<Node>> {
        let node_as_for_of_statement = node.as_for_of_statement();
        Ok(self.factory.update_for_of_statement(
            node,
            try_maybe_visit_node(
                node_as_for_of_statement.await_modifier.as_deref(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_token),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            if self.is_variable_declaration_list_with_colliding_name(Some(
                &*node_as_for_of_statement.initializer,
            )) {
                self.visit_variable_declaration_list_with_colliding_names(
                    &node_as_for_of_statement.initializer,
                    true,
                )?
                .unwrap()
            } else {
                try_visit_node(
                    &node_as_for_of_statement.initializer,
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_for_initializer),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )?
            },
            try_visit_node(
                &node_as_for_of_statement.expression,
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            try_visit_iteration_body(
                &node_as_for_of_statement.statement,
                |node: &Node| self.async_body_visitor(node),
                &**self.context,
            )?,
        ))
    }

    fn visit_for_statement_in_async_body(
        &self,
        node: &Node, /*ForStatement*/
    ) -> io::Result<Gc<Node>> {
        let node_as_for_statement = node.as_for_statement();
        let initializer = node_as_for_statement.initializer.as_deref();
        Ok(self.factory.update_for_statement(
            node,
            if self.is_variable_declaration_list_with_colliding_name(initializer) {
                self.visit_variable_declaration_list_with_colliding_names(
                    initializer.unwrap(),
                    false,
                )?
            } else {
                try_maybe_visit_node(
                    initializer,
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_for_initializer),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )?
            },
            try_maybe_visit_node(
                node_as_for_statement.condition.as_deref(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            try_maybe_visit_node(
                node_as_for_statement.incrementor.as_deref(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            try_visit_iteration_body(
                &node_as_for_statement.statement,
                |node: &Node| self.async_body_visitor(node),
                &**self.context,
            )?,
        ))
    }

    fn visit_await_expression(
        &self,
        node: &Node, /*AwaitExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        if self.in_top_level_context() {
            return try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context);
        }
        Ok(set_original_node(
            set_text_range_rc_node(
                self.factory.create_yield_expression(
                    None,
                    try_maybe_visit_node(
                        Some(&*node.as_await_expression().expression),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                ),
                Some(node),
            ),
            Some(node.node_wrapper()),
        ))
    }

    fn visit_method_declaration(
        &self,
        node: &Node, /*MethodDeclaration*/
    ) -> io::Result<Gc<Node>> {
        let node_as_method_declaration = node.as_method_declaration();
        Ok(self.factory.update_method_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            try_maybe_visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_modifier),
                None,
                None,
            )?,
            node_as_method_declaration.maybe_asterisk_token(),
            node_as_method_declaration.name(),
            None,
            Option::<Gc<NodeArray>>::None,
            try_visit_parameter_list(
                Some(&node_as_method_declaration.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
            )?
            .unwrap(),
            None,
            if get_function_flags(Some(node)).intersects(FunctionFlags::Async) {
                Some(self.transform_async_function_body(node)?)
            } else {
                try_visit_function_body(
                    node_as_method_declaration.maybe_body().as_deref(),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                )?
            },
        ))
    }

    fn visit_function_declaration(
        &self,
        node: &Node, /*FunctionDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_as_function_declaration = node.as_function_declaration();
        Ok(Some(
            self.factory
                .update_function_declaration(
                    node,
                    Option::<Gc<NodeArray>>::None,
                    try_maybe_visit_nodes(
                        node.maybe_modifiers().as_deref(),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_modifier),
                        None,
                        None,
                    )?,
                    node_as_function_declaration.maybe_asterisk_token(),
                    node_as_function_declaration.maybe_name(),
                    Option::<Gc<NodeArray>>::None,
                    try_visit_parameter_list(
                        Some(&node_as_function_declaration.parameters()),
                        |node: &Node| self.visitor(node),
                        &**self.context,
                    )?
                    .unwrap(),
                    None,
                    if get_function_flags(Some(node)).intersects(FunctionFlags::Async) {
                        Some(self.transform_async_function_body(node)?)
                    } else {
                        try_visit_function_body(
                            node_as_function_declaration.maybe_body().as_deref(),
                            |node: &Node| self.visitor(node),
                            &**self.context,
                        )?
                    },
                )
                .into(),
        ))
    }

    fn visit_function_expression(
        &self,
        node: &Node, /*FunctionExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let node_as_function_expression = node.as_function_expression();
        Ok(self.factory.update_function_expression(
            node,
            try_maybe_visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_modifier),
                None,
                None,
            )?,
            node_as_function_expression.maybe_asterisk_token(),
            node_as_function_expression.maybe_name(),
            Option::<Gc<NodeArray>>::None,
            try_visit_parameter_list(
                Some(&node_as_function_expression.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
            )?
            .unwrap(),
            None,
            if get_function_flags(Some(node)).intersects(FunctionFlags::Async) {
                self.transform_async_function_body(node)?
            } else {
                try_visit_function_body(
                    node_as_function_expression.maybe_body().as_deref(),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                )?
                .unwrap()
            },
        ))
    }

    fn visit_arrow_function(&self, node: &Node /*ArrowFunction*/) -> io::Result<Gc<Node>> {
        let node_as_arrow_function = node.as_arrow_function();
        Ok(self.factory.update_arrow_function(
            node,
            try_maybe_visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_modifier),
                None,
                None,
            )?,
            Option::<Gc<NodeArray>>::None,
            try_visit_parameter_list(
                Some(&node_as_arrow_function.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
            )?
            .unwrap(),
            None,
            node_as_arrow_function.equals_greater_than_token.clone(),
            if get_function_flags(Some(node)).intersects(FunctionFlags::Async) {
                self.transform_async_function_body(node)?
            } else {
                try_visit_function_body(
                    node_as_arrow_function.maybe_body().as_deref(),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                )?
                .unwrap()
            },
        ))
    }

    fn record_declaration_name(
        &self,
        node: &Node, /*ParameterDeclaration | VariableDeclaration | BindingElement*/
        names: &mut HashSet<__String>,
    ) {
        let name = &node.as_named_declaration().name();
        if is_identifier(name) {
            names.insert(name.as_identifier().escaped_text.clone());
        } else {
            for element in &name.as_has_elements().elements() {
                if !is_omitted_expression(element) {
                    self.record_declaration_name(element, names);
                }
            }
        }
    }

    fn is_variable_declaration_list_with_colliding_name(
        &self,
        node: Option<&Node /*ForInitializer*/>,
    ) -> bool {
        if node.is_none() {
            return false;
        }
        let node = node.unwrap();
        is_variable_declaration_list(node)
            && !node.flags().intersects(NodeFlags::BlockScoped)
            && node
                .as_variable_declaration_list()
                .declarations
                .iter()
                .any(|node: &Gc<Node>| {
                    self.collides_with_parameter_name(&node.as_named_declaration().name())
                })
    }

    fn visit_variable_declaration_list_with_colliding_names(
        &self,
        node: &Node, /*VariableDeclarationList*/
        has_receiver: bool,
    ) -> io::Result<Option<Gc<Node>>> {
        let node_as_variable_declaration_list = node.as_variable_declaration_list();
        self.hoist_variable_declaration_list(node);

        let variables = get_initialized_variables(node);
        if variables.is_empty() {
            if has_receiver {
                return try_maybe_visit_node(
                    Some(
                        self.factory
                            .converters()
                            .convert_to_assignment_element_target(
                                &node_as_variable_declaration_list.declarations[0]
                                    .as_variable_declaration()
                                    .name(),
                            ),
                    ),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                );
            }
            return Ok(None);
        }

        Ok(Some(self.factory.inline_expressions(&try_map(
            &variables,
            |variable: &Gc<Node>, _| self.transform_initialized_variable(variable),
        )?)))
    }

    fn hoist_variable_declaration_list(&self, node: &Node /*VariableDeclarationList*/) {
        for_each(
            &node.as_variable_declaration_list().declarations,
            |declaration: &Gc<Node>, _| -> Option<()> {
                self.hoist_variable(&declaration.as_named_declaration().name());
                None
            },
        );
    }

    fn hoist_variable(&self, name: &Node) {
        if is_identifier(name) {
            self.context.hoist_variable_declaration(name);
        } else {
            for element in &name.as_has_elements().elements() {
                if !is_omitted_expression(element) {
                    self.hoist_variable(element);
                }
            }
        }
    }

    fn transform_initialized_variable(
        &self,
        node: &Node, /*VariableDeclaration*/
    ) -> io::Result<Gc<Node>> {
        let node_as_variable_declaration = node.as_variable_declaration();
        let converted = set_source_map_range(
            self.factory.create_assignment(
                self.factory
                    .converters()
                    .convert_to_assignment_element_target(&node_as_variable_declaration.name()),
                node_as_variable_declaration.maybe_initializer().unwrap(),
            ),
            Some(node.into()),
        );
        Ok(try_visit_node(
            &converted,
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?)
    }

    fn collides_with_parameter_name(&self, name: &Node) -> bool {
        if is_identifier(name) {
            return self
                .enclosing_function_parameter_names()
                .contains(&name.as_identifier().escaped_text);
        } else {
            for element in &name.as_has_elements().elements() {
                if !is_omitted_expression(element) && self.collides_with_parameter_name(element) {
                    return true;
                }
            }
        }
        false
    }

    fn transform_async_function_body(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> io::Result<Gc<Node /*ConciseBody*/>> {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        self.context.resume_lexical_environment();

        let ref original = get_original_node(node);
        let node_type = original.as_has_type().maybe_type();
        let promise_constructor = if self.language_version < ScriptTarget::ES2015 {
            self.get_promise_constructor(node_type.as_deref())?
        } else {
            None
        };
        let is_arrow_function = node.kind() == SyntaxKind::ArrowFunction;
        let has_lexical_arguments = self
            .resolver
            .get_node_check_flags(node)
            .intersects(NodeCheckFlags::CaptureArguments);

        let saved_enclosing_function_parameter_names =
            self.maybe_enclosing_function_parameter_names().clone();
        self.set_enclosing_function_parameter_names(Some(HashSet::new()));
        for parameter in &node_as_function_like_declaration.parameters() {
            self.record_declaration_name(
                parameter,
                &mut self.enclosing_function_parameter_names_mut(),
            );
        }

        let saved_captured_super_properties = self.maybe_captured_super_properties().clone();
        let saved_has_super_element_access = self.maybe_has_super_element_access();
        if !is_arrow_function {
            self.set_captured_super_properties(Some(HashSet::new()));
            self.set_has_super_element_access(Some(false));
        }

        let result: Gc<Node /*ConciseBody*/>;
        if !is_arrow_function {
            let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();
            let statement_offset = self.factory.try_copy_prologue(
                &node_as_function_like_declaration
                    .maybe_body()
                    .unwrap()
                    .as_block()
                    .statements,
                &mut statements,
                Some(false),
                Some(|node: &Node| self.visitor(node)),
            )?;
            statements.push(
                self.factory.create_return_statement(Some(
                    self.context
                        .get_emit_helper_factory()
                        .create_awaiter_helper(
                            self.in_has_lexical_this_context(),
                            has_lexical_arguments,
                            promise_constructor.clone(),
                            self.transform_async_function_body_worker(
                                &node_as_function_like_declaration.maybe_body().unwrap(),
                                Some(statement_offset),
                            )?,
                        ),
                )),
            );

            insert_statements_after_standard_prologue(
                &mut statements,
                self.context.end_lexical_environment().as_deref(),
            );

            let emit_super_helpers = self.language_version >= ScriptTarget::ES2015
                && self.resolver.get_node_check_flags(node).intersects(
                    NodeCheckFlags::AsyncMethodWithSuperBinding
                        | NodeCheckFlags::AsyncMethodWithSuper,
                );

            if emit_super_helpers {
                self.enable_substitution_for_async_methods_with_super();
                if !self.captured_super_properties().is_empty() {
                    let variable_statement = create_super_access_variable_statement(
                        &self.factory,
                        &**self.resolver,
                        node,
                        &self.captured_super_properties(),
                    );
                    self.substituted_super_accessors_mut()
                        .insert(get_node_id(&variable_statement), true);
                    insert_statements_after_standard_prologue(
                        &mut statements,
                        Some(&[variable_statement]),
                    );
                }
            }

            let block = self.factory.create_block(statements, Some(true));
            set_text_range(
                &*block,
                node_as_function_like_declaration.maybe_body().as_deref(),
            );

            if emit_super_helpers && self.maybe_has_super_element_access() == Some(true) {
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

            result = block;
        } else {
            let expression = self
                .context
                .get_emit_helper_factory()
                .create_awaiter_helper(
                    self.in_has_lexical_this_context(),
                    has_lexical_arguments,
                    promise_constructor,
                    self.transform_async_function_body_worker(
                        &node_as_function_like_declaration.maybe_body().unwrap(),
                        None,
                    )?,
                );

            let declarations = self.context.end_lexical_environment();
            if let Some(declarations) = declarations.non_empty()
            /*some(declarations)*/
            {
                let block = self
                    .factory
                    .converters()
                    .convert_to_function_block(&expression, None);
                let block_as_block = block.as_block();
                result = self.factory.update_block(
                    &block,
                    set_text_range_node_array(
                        self.factory.create_node_array(
                            Some(concatenate(
                                declarations,
                                block_as_block.statements.to_vec(),
                            )),
                            None,
                        ),
                        Some(&*block_as_block.statements),
                    ),
                );
            } else {
                result = expression;
            }
        }

        self.set_enclosing_function_parameter_names(saved_enclosing_function_parameter_names);
        if !is_arrow_function {
            self.set_captured_super_properties(saved_captured_super_properties);
            self.set_has_super_element_access(saved_has_super_element_access);
        }
        Ok(result)
    }

    fn transform_async_function_body_worker(
        &self,
        body: &Node, /*ConciseBody*/
        start: Option<usize>,
    ) -> io::Result<Gc<Node>> {
        Ok(if is_block(body) {
            self.factory.update_block(
                body,
                try_visit_nodes(
                    &body.as_block().statements,
                    Some(|node: &Node| self.async_body_visitor(node)),
                    Some(is_statement),
                    start,
                    None,
                )?,
            )
        } else {
            self.factory.converters().convert_to_function_block(
                &*try_visit_node(
                    body,
                    Some(|node: &Node| self.async_body_visitor(node)),
                    Some(is_concise_body),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )?,
                None,
            )
        })
    }

    fn get_promise_constructor(
        &self,
        type_: Option<&Node /*TypeNode*/>,
    ) -> io::Result<Option<Gc<Node>>> {
        type_
            .and_then(get_entity_name_from_type_node)
            .filter(|type_name| is_entity_name(type_name))
            .try_and_then(|type_name| -> io::Result<_> {
                let serialization_kind = self
                    .resolver
                    .get_type_reference_serialization_kind(&type_name, None)?;
                Ok(
                    if matches!(
                        serialization_kind,
                        TypeReferenceSerializationKind::TypeWithConstructSignatureAndValue
                            | TypeReferenceSerializationKind::Unknown
                    ) {
                        Some(type_name)
                    } else {
                        None
                    },
                )
            })
    }

    fn enable_substitution_for_async_methods_with_super(&self) {
        if let Some(enabled_substitutions) =
            self.maybe_enabled_substitutions()
                .filter(|enabled_substitutions| {
                    !enabled_substitutions
                        .intersects(ES2017SubstitutionFlags::AsyncMethodsWithSuper)
                })
        {
            self.set_enabled_substitutions(Some(
                enabled_substitutions | ES2017SubstitutionFlags::AsyncMethodsWithSuper,
            ));

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

    fn substitute_expression(&self, node: &Node /*Expression*/) -> Gc<Node> {
        match node.kind() {
            SyntaxKind::PropertyAccessExpression => {
                self.substitute_property_access_expression(node)
            }
            SyntaxKind::ElementAccessExpression => self.substitute_element_access_expression(node),
            SyntaxKind::CallExpression => self.substitute_call_expression(node),
            _ => node.node_wrapper(),
        }
    }

    fn substitute_property_access_expression(
        &self,
        node: &Node, /*PropertyAccessExpression*/
    ) -> Gc<Node> {
        let node_as_property_access_expression = node.as_property_access_expression();
        if node_as_property_access_expression.expression.kind() == SyntaxKind::SuperKeyword {
            return set_text_range_rc_node(
                self.factory.create_property_access_expression(
                    self.factory.create_unique_name(
                        "_super",
                        Some(
                            GeneratedIdentifierFlags::Optimistic
                                | GeneratedIdentifierFlags::FileLevel,
                        ),
                    ),
                    node_as_property_access_expression.name.clone(),
                ),
                Some(node),
            );
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
            return self.factory.create_call_expression(
                self.factory
                    .create_property_access_expression(argument_expression, "call"),
                Option::<Gc<NodeArray>>::None,
                Some(
                    [
                        vec![self.factory.create_this()],
                        node_as_call_expression.arguments.to_vec(),
                    ]
                    .concat(),
                ),
            );
        }
        node.node_wrapper()
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

    fn create_super_element_access_in_async_method(
        &self,
        argument_expression: &Node,        /*Expression*/
        location: &impl ReadonlyTextRange, /*TextRange*/
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        if self
            .enclosing_super_container_flags()
            .intersects(NodeCheckFlags::AsyncMethodWithSuperBinding)
        {
            set_text_range_rc_node(
                self.factory.create_property_access_expression(
                    self.factory.create_call_expression(
                        self.factory.create_unique_name(
                            "_superIndex",
                            Some(
                                GeneratedIdentifierFlags::Optimistic
                                    | GeneratedIdentifierFlags::FileLevel,
                            ),
                        ),
                        Option::<Gc<NodeArray>>::None,
                        Some(vec![argument_expression.node_wrapper()]),
                    ),
                    "value",
                ),
                Some(location),
            )
        } else {
            set_text_range_rc_node(
                self.factory.create_call_expression(
                    self.factory.create_unique_name(
                        "_superIndex",
                        Some(
                            GeneratedIdentifierFlags::Optimistic
                                | GeneratedIdentifierFlags::FileLevel,
                        ),
                    ),
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![argument_expression.node_wrapper()]),
                ),
                Some(location),
            )
        }
    }
}

impl TransformerInterface for TransformES2017 {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        self.transform_source_file(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformES2017OnEmitNodeOverrider {
    transform_es2017: Gc<Box<TransformES2017>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformES2017OnEmitNodeOverrider {
    fn new(
        transform_es2017: Gc<Box<TransformES2017>>,
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
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        if matches!(
            self.transform_es2017.maybe_enabled_substitutions(),
            Some(enabled_substitutions) if enabled_substitutions.intersects(ES2017SubstitutionFlags::AsyncMethodsWithSuper)
        ) && self.transform_es2017.is_super_container(node)
        {
            let super_container_flags = self.transform_es2017.resolver.get_node_check_flags(node)
                & (NodeCheckFlags::AsyncMethodWithSuper
                    | NodeCheckFlags::AsyncMethodWithSuperBinding);
            if super_container_flags != self.transform_es2017.enclosing_super_container_flags() {
                let saved_enclosing_super_container_flags =
                    self.transform_es2017.enclosing_super_container_flags();
                self.transform_es2017
                    .set_enclosing_super_container_flags(super_container_flags);
                self.previous_on_emit_node
                    .on_emit_node(hint, node, emit_callback)?;
                self.transform_es2017
                    .set_enclosing_super_container_flags(saved_enclosing_super_container_flags);
                return Ok(());
            }
        } else if matches!(
            self.transform_es2017.maybe_enabled_substitutions(),
            Some(enabled_substitutions) if enabled_substitutions != ES2017SubstitutionFlags::None
        ) && self
            .transform_es2017
            .substituted_super_accessors()
            .get(&get_node_id(node))
            .cloned()
            == Some(true)
        {
            let saved_enclosing_super_container_flags =
                self.transform_es2017.enclosing_super_container_flags();
            self.transform_es2017
                .set_enclosing_super_container_flags(NodeCheckFlags::None);
            self.previous_on_emit_node
                .on_emit_node(hint, node, emit_callback)?;
            self.transform_es2017
                .set_enclosing_super_container_flags(saved_enclosing_super_container_flags);
            return Ok(());
        }
        self.previous_on_emit_node
            .on_emit_node(hint, node, emit_callback)?;

        Ok(())
    }
}

#[derive(Trace, Finalize)]
struct TransformES2017OnSubstituteNodeOverrider {
    transform_es2017: Gc<Box<TransformES2017>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformES2017OnSubstituteNodeOverrider {
    fn new(
        transform_es2017: Gc<Box<TransformES2017>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es2017,
            previous_on_substitute_node,
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider for TransformES2017OnSubstituteNodeOverrider {
    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> io::Result<Gc<Node>> {
        let node = self
            .previous_on_substitute_node
            .on_substitute_node(hint, node)?;
        if hint == EmitHint::Expression
            && self.transform_es2017.enclosing_super_container_flags() != NodeCheckFlags::None
        {
            return Ok(self.transform_es2017.substitute_expression(&node));
        }

        Ok(node)
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
        chain_bundle().call(
            context.clone(),
            TransformES2017::new(context).as_transformer(),
        )
    }
}

pub fn transform_es2017() -> TransformerFactory {
    Gc::new(Box::new(TransformES2017Factory::new()))
}

pub fn create_super_access_variable_statement(
    factory: &NodeFactory<BaseNodeFactorySynthetic>,
    resolver: &dyn EmitResolver,
    node: &Node, /*FunctionLikeDeclaration*/
    names: &HashSet<__String>,
) -> Gc<Node> {
    let has_binding = resolver
        .get_node_check_flags(node)
        .intersects(NodeCheckFlags::AsyncMethodWithSuperBinding);
    let mut accessors: Vec<Gc<Node /*PropertyAssignment*/>> = Default::default();
    names.iter().for_each(|key| {
        let name = unescape_leading_underscores(key);
        let mut getter_and_setter: Vec<Gc<Node /*PropertyAssignment*/>> = Default::default();
        getter_and_setter.push(factory.create_property_assignment(
            "get",
            factory.create_arrow_function(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                vec![],
                None,
                None,
                set_emit_flags(
                    factory.create_property_access_expression(
                        set_emit_flags(factory.create_super(), EmitFlags::NoSubstitution),
                        name,
                    ),
                    EmitFlags::NoSubstitution,
                ),
            ),
        ));
        if has_binding {
            getter_and_setter.push(factory.create_property_assignment(
                "set",
                factory.create_arrow_function(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    vec![factory.create_parameter_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        None,
                        Some("v"),
                        None,
                        None,
                        None,
                    )],
                    None,
                    None,
                    factory.create_assignment(
                        set_emit_flags(
                            factory.create_property_access_expression(
                                set_emit_flags(factory.create_super(), EmitFlags::NoSubstitution),
                                name,
                            ),
                            EmitFlags::NoSubstitution,
                        ),
                        factory.create_identifier("v"),
                    ),
                ),
            ));
        }
        accessors.push(factory.create_property_assignment(
            name,
            factory.create_object_literal_expression(Some(getter_and_setter), None),
        ));
    });
    factory.create_variable_statement(
        Option::<Gc<NodeArray>>::None,
        factory.create_variable_declaration_list(
            vec![factory.create_variable_declaration(
                Some(factory.create_unique_name(
                    "_super",
                    Some(
                        GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel,
                    ),
                )),
                None,
                None,
                Some(factory.create_call_expression(
                    factory.create_property_access_expression(
                        factory.create_identifier("Object"),
                        "create",
                    ),
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![
                        factory.create_null(),
                        factory.create_object_literal_expression(Some(accessors), Some(true)),
                    ]),
                )),
            )],
            Some(NodeFlags::Const),
        ),
    )
}
