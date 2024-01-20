use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    io, mem,
};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;

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
    set_text_range, set_text_range_node_array, set_text_range_id_node, try_maybe_visit_each_child,
    try_maybe_visit_node, try_maybe_visit_nodes, unescape_leading_underscores,
    BaseNodeFactorySynthetic, CompilerOptions, Debug_, EmitFlags, EmitHint, EmitResolver,
    FunctionFlags, FunctionLikeDeclarationInterface, GeneratedIdentifierFlags,
    HasInitializerInterface, NamedDeclarationInterface, Node, NodeArray, NodeCheckFlags,
    NodeFactory, NodeFlags, NodeId, NodeInterface, NonEmpty, OptionTry, ReadonlyTextRange,
    ScriptTarget, SignatureDeclarationInterface, SyntaxKind, TransformFlags, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TypeReferenceSerializationKind, VisitResult,
    HasArena, AllArenas, InArena, OptionInArena,
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

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> io::Result<Id<Node>> {
        if node.ref_(self).as_source_file().is_declaration_file() {
            return Ok(node);
        }

        self.set_context_flag(ContextFlags::NonTopLevel, false);
        self.set_context_flag(
            ContextFlags::HasLexicalThis,
            !is_effective_strict_mode_source_file(node, &self.compiler_options, self),
        );
        let visited =
            try_visit_each_child(&node.ref_(self), |node: Id<Node>| self.visitor(node), &**self.context)?;
        add_emit_helpers(visited, self.context.read_emit_helpers().as_deref(), self);
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
    fn do_with_context<TReturn>(
        &self,
        flags: ContextFlags,
        mut cb: impl FnMut(Id<Node>) -> TReturn,
        value: Id<Node>,
    ) -> TReturn {
        self.try_do_with_context(flags, |a| Ok(cb(a)), value)
            .unwrap()
    }

    fn try_do_with_context<TReturn>(
        &self,
        flags: ContextFlags,
        mut cb: impl FnMut(Id<Node>) -> io::Result<TReturn>,
        value: Id<Node>,
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

    fn visit_default(&self, node: Id<Node>) -> io::Result<VisitResult> {
        Ok(try_maybe_visit_each_child(
            Some(&node.ref_(self)),
            |node: Id<Node>| self.visitor(node),
            &**self.context,
        )?
        .map(Into::into))
    }

    fn visitor(&self, node: Id<Node>) -> io::Result<VisitResult> {
        if !node
            .ref_(self).transform_flags()
            .intersects(TransformFlags::ContainsES2017)
        {
            return Ok(Some(node.into()));
        }
        Ok(match node.ref_(self).kind() {
            SyntaxKind::AsyncKeyword => None,

            SyntaxKind::AwaitExpression => Some(self.visit_await_expression(node)?.into()),

            SyntaxKind::MethodDeclaration => Some(
                self.try_do_with_context(
                    ContextFlags::NonTopLevel | ContextFlags::HasLexicalThis,
                    |node: Id<Node>| self.visit_method_declaration(node),
                    node,
                )?
                .into(),
            ),

            SyntaxKind::FunctionDeclaration => self.try_do_with_context(
                ContextFlags::NonTopLevel | ContextFlags::HasLexicalThis,
                |node: Id<Node>| self.visit_function_declaration(node),
                node,
            )?,

            SyntaxKind::FunctionExpression => Some(
                self.try_do_with_context(
                    ContextFlags::NonTopLevel | ContextFlags::HasLexicalThis,
                    |node: Id<Node>| self.visit_function_expression(node),
                    node,
                )?
                .into(),
            ),

            SyntaxKind::ArrowFunction => Some(
                self.try_do_with_context(
                    ContextFlags::NonTopLevel,
                    |node: Id<Node>| self.visit_arrow_function(node),
                    node,
                )?
                .into(),
            ),

            SyntaxKind::PropertyAccessExpression => {
                if let Some(captured_super_properties) =
                    self.maybe_captured_super_properties_mut().as_mut()
                {
                    if is_property_access_expression(&node.ref_(self)) {
                        let node_ref = node.ref_(self);
                        let node_as_property_access_expression = node_ref.as_property_access_expression();
                        if node_as_property_access_expression.expression.ref_(self).kind()
                            == SyntaxKind::SuperKeyword
                        {
                            captured_super_properties.insert(
                                node_as_property_access_expression
                                    .name
                                    .ref_(self).as_member_name()
                                    .escaped_text()
                                    .to_owned(),
                            );
                        }
                    }
                }
                try_maybe_visit_each_child(
                    Some(&node.ref_(self)),
                    |node: Id<Node>| self.visitor(node),
                    &**self.context,
                )?
                .map(Into::into)
            }

            SyntaxKind::ElementAccessExpression => {
                if self.maybe_captured_super_properties().is_some()
                    && node.ref_(self).as_element_access_expression().expression.ref_(self).kind()
                        == SyntaxKind::SuperKeyword
                {
                    self.set_has_super_element_access(Some(true));
                }
                try_maybe_visit_each_child(
                    Some(&node.ref_(self)),
                    |node: Id<Node>| self.visitor(node),
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
                |node: Id<Node>| self.visit_default(node),
                node,
            )?,

            _ => try_maybe_visit_each_child(
                Some(&node.ref_(self)),
                |node: Id<Node>| self.visitor(node),
                &**self.context,
            )?
            .map(Into::into),
        })
    }

    fn async_body_visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/ {
        if is_node_with_possible_hoisted_declaration(&node.ref_(self)) {
            return Ok(match node.ref_(self).kind() {
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
                    Some(&node.ref_(self)),
                    |node: Id<Node>| self.async_body_visitor(node),
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
        node: Id<Node>, /*CatchClause*/
    ) -> io::Result<Id<Node>> {
        let mut catch_clause_names: HashSet<__String> = Default::default();
        let node_ref = node.ref_(self);
        let node_as_catch_clause = node_ref.as_catch_clause();
        self.record_declaration_name(
            node_as_catch_clause.variable_declaration.unwrap(),
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
                    &node.ref_(self),
                    |node: Id<Node>| self.async_body_visitor(node),
                    &**self.context,
                )?;
                self.set_enclosing_function_parameter_names(
                    saved_enclosing_function_parameter_names,
                );
                result
            } else {
                try_visit_each_child(
                    &node.ref_(self),
                    |node: Id<Node>| self.async_body_visitor(node),
                    &**self.context,
                )?
            },
        )
    }

    fn visit_variable_statement_in_async_body(
        &self,
        node: Id<Node>, /*VariableStatement*/
    ) -> io::Result<Option<Id<Node>>> {
        let node_ref = node.ref_(self);
        let node_as_variable_statement = node_ref.as_variable_statement();
        if self.is_variable_declaration_list_with_colliding_name(Some(
            node_as_variable_statement.declaration_list,
        )) {
            let expression = self.visit_variable_declaration_list_with_colliding_names(
                node_as_variable_statement.declaration_list,
                false,
            )?;
            return Ok(
                expression.map(|expression| self.factory.create_expression_statement(expression))
            );
        }
        try_maybe_visit_each_child(
            Some(&node.ref_(self)),
            |node: Id<Node>| self.visitor(node),
            &**self.context,
        )
    }

    fn visit_for_in_statement_in_async_body(
        &self,
        node: Id<Node>, /*ForInStatement*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_for_in_statement = node_ref.as_for_in_statement();
        Ok(self.factory.update_for_in_statement(
            node,
            if self.is_variable_declaration_list_with_colliding_name(Some(
                node_as_for_in_statement.initializer,
            )) {
                self.visit_variable_declaration_list_with_colliding_names(
                    node_as_for_in_statement.initializer,
                    true,
                )?
                .unwrap()
            } else {
                try_visit_node(
                    node_as_for_in_statement.initializer,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_for_initializer(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?
            },
            try_visit_node(
                node_as_for_in_statement.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_visit_iteration_body(
                node_as_for_in_statement.statement,
                |node: Id<Node>| self.async_body_visitor(node),
                &**self.context,
            )?,
        ))
    }

    fn visit_for_of_statement_in_async_body(
        &self,
        node: Id<Node>, /*ForOfStatement*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_for_of_statement = node_ref.as_for_of_statement();
        Ok(self.factory.update_for_of_statement(
            node,
            try_maybe_visit_node(
                node_as_for_of_statement.await_modifier,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_token(&node.ref_(self))),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            if self.is_variable_declaration_list_with_colliding_name(Some(
                node_as_for_of_statement.initializer,
            )) {
                self.visit_variable_declaration_list_with_colliding_names(
                    node_as_for_of_statement.initializer,
                    true,
                )?
                .unwrap()
            } else {
                try_visit_node(
                    node_as_for_of_statement.initializer,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_for_initializer(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?
            },
            try_visit_node(
                node_as_for_of_statement.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_visit_iteration_body(
                node_as_for_of_statement.statement,
                |node: Id<Node>| self.async_body_visitor(node),
                &**self.context,
            )?,
        ))
    }

    fn visit_for_statement_in_async_body(
        &self,
        node: Id<Node>, /*ForStatement*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_for_statement = node_ref.as_for_statement();
        let initializer = node_as_for_statement.initializer;
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
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_for_initializer(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?
            },
            try_maybe_visit_node(
                node_as_for_statement.condition,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_maybe_visit_node(
                node_as_for_statement.incrementor,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            try_visit_iteration_body(
                node_as_for_statement.statement,
                |node: Id<Node>| self.async_body_visitor(node),
                &**self.context,
            )?,
        ))
    }

    fn visit_await_expression(
        &self,
        node: Id<Node>, /*AwaitExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        if self.in_top_level_context() {
            return try_visit_each_child(
                &node.ref_(self),
                |node: Id<Node>| self.visitor(node),
                &**self.context,
            );
        }
        Ok(set_original_node(
            set_text_range_id_node(
                self.factory.create_yield_expression(
                    None,
                    try_maybe_visit_node(
                        Some(node.ref_(self).as_await_expression().expression),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                ),
                Some(&*node.ref_(self)),
                self,
            ),
            Some(node),
            self,
        ))
    }

    fn visit_method_declaration(
        &self,
        node: Id<Node>, /*MethodDeclaration*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_method_declaration = node_ref.as_method_declaration();
        Ok(self.factory.update_method_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            try_maybe_visit_nodes(
                node.ref_(self).maybe_modifiers().as_deref(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                None,
                None,
            )?,
            node_as_method_declaration.maybe_asterisk_token(),
            node_as_method_declaration.name(),
            None,
            Option::<Gc<NodeArray>>::None,
            try_visit_parameter_list(
                Some(&node_as_method_declaration.parameters()),
                |node: Id<Node>| self.visitor(node),
                &**self.context,
            )?
            .unwrap(),
            None,
            if get_function_flags(Some(node), self).intersects(FunctionFlags::Async) {
                Some(self.transform_async_function_body(node)?)
            } else {
                try_visit_function_body(
                    node_as_method_declaration.maybe_body(),
                    |node: Id<Node>| self.visitor(node),
                    &**self.context,
                )?
            },
        ))
    }

    fn visit_function_declaration(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_function_declaration = node_ref.as_function_declaration();
        Ok(Some(
            self.factory
                .update_function_declaration(
                    node,
                    Option::<Gc<NodeArray>>::None,
                    try_maybe_visit_nodes(
                        node.ref_(self).maybe_modifiers().as_deref(),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                        None,
                        None,
                    )?,
                    node_as_function_declaration.maybe_asterisk_token(),
                    node_as_function_declaration.maybe_name(),
                    Option::<Gc<NodeArray>>::None,
                    try_visit_parameter_list(
                        Some(&node_as_function_declaration.parameters()),
                        |node: Id<Node>| self.visitor(node),
                        &**self.context,
                    )?
                    .unwrap(),
                    None,
                    if get_function_flags(Some(node), self).intersects(FunctionFlags::Async) {
                        Some(self.transform_async_function_body(node)?)
                    } else {
                        try_visit_function_body(
                            node_as_function_declaration.maybe_body(),
                            |node: Id<Node>| self.visitor(node),
                            &**self.context,
                        )?
                    },
                )
                .into(),
        ))
    }

    fn visit_function_expression(
        &self,
        node: Id<Node>, /*FunctionExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_ref = node.ref_(self);
        let node_as_function_expression = node_ref.as_function_expression();
        Ok(self.factory.update_function_expression(
            node,
            try_maybe_visit_nodes(
                node.ref_(self).maybe_modifiers().as_deref(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                None,
                None,
            )?,
            node_as_function_expression.maybe_asterisk_token(),
            node_as_function_expression.maybe_name(),
            Option::<Gc<NodeArray>>::None,
            try_visit_parameter_list(
                Some(&node_as_function_expression.parameters()),
                |node: Id<Node>| self.visitor(node),
                &**self.context,
            )?
            .unwrap(),
            None,
            if get_function_flags(Some(node), self).intersects(FunctionFlags::Async) {
                self.transform_async_function_body(node)?
            } else {
                try_visit_function_body(
                    node_as_function_expression.maybe_body(),
                    |node: Id<Node>| self.visitor(node),
                    &**self.context,
                )?
                .unwrap()
            },
        ))
    }

    fn visit_arrow_function(&self, node: Id<Node> /*ArrowFunction*/) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_arrow_function = node_ref.as_arrow_function();
        Ok(self.factory.update_arrow_function(
            node,
            try_maybe_visit_nodes(
                node.ref_(self).maybe_modifiers().as_deref(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                None,
                None,
            )?,
            Option::<Gc<NodeArray>>::None,
            try_visit_parameter_list(
                Some(&node_as_arrow_function.parameters()),
                |node: Id<Node>| self.visitor(node),
                &**self.context,
            )?
            .unwrap(),
            None,
            node_as_arrow_function.equals_greater_than_token.clone(),
            if get_function_flags(Some(node), self).intersects(FunctionFlags::Async) {
                self.transform_async_function_body(node)?
            } else {
                try_visit_function_body(
                    node_as_arrow_function.maybe_body(),
                    |node: Id<Node>| self.visitor(node),
                    &**self.context,
                )?
                .unwrap()
            },
        ))
    }

    fn record_declaration_name(
        &self,
        node: Id<Node>, /*ParameterDeclaration | VariableDeclaration | BindingElement*/
        names: &mut HashSet<__String>,
    ) {
        let name = node.ref_(self).as_named_declaration().name();
        if is_identifier(&name.ref_(self)) {
            names.insert(name.ref_(self).as_identifier().escaped_text.clone());
        } else {
            for &element in &name.ref_(self).as_has_elements().elements() {
                if !is_omitted_expression(&element.ref_(self)) {
                    self.record_declaration_name(element, names);
                }
            }
        }
    }

    fn is_variable_declaration_list_with_colliding_name(
        &self,
        node: Option<Id<Node> /*ForInitializer*/>,
    ) -> bool {
        let Some(node) = node else {
            return false;
        };
        is_variable_declaration_list(&node.ref_(self))
            && !node.ref_(self).flags().intersects(NodeFlags::BlockScoped)
            && node
                .ref_(self).as_variable_declaration_list()
                .declarations
                .iter()
                .any(|node: &Id<Node>| {
                    self.collides_with_parameter_name(node.ref_(self).as_named_declaration().name())
                })
    }

    fn visit_variable_declaration_list_with_colliding_names(
        &self,
        node: Id<Node>, /*VariableDeclarationList*/
        has_receiver: bool,
    ) -> io::Result<Option<Id<Node>>> {
        let node_ref = node.ref_(self);
        let node_as_variable_declaration_list = node_ref.as_variable_declaration_list();
        self.hoist_variable_declaration_list(node);

        let variables = get_initialized_variables(node, self);
        if variables.is_empty() {
            if has_receiver {
                return try_maybe_visit_node(
                    Some(
                        self.factory
                            .converters()
                            .convert_to_assignment_element_target(
                                node_as_variable_declaration_list.declarations[0]
                                    .ref_(self).as_variable_declaration()
                                    .name(),
                            ),
                    ),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                );
            }
            return Ok(None);
        }

        Ok(Some(self.factory.inline_expressions(&try_map(
            &variables,
            |&variable: &Id<Node>, _| self.transform_initialized_variable(variable),
        )?)))
    }

    fn hoist_variable_declaration_list(&self, node: Id<Node> /*VariableDeclarationList*/) {
        for_each(
            &node.ref_(self).as_variable_declaration_list().declarations,
            |declaration: &Id<Node>, _| -> Option<()> {
                self.hoist_variable(declaration.ref_(self).as_named_declaration().name());
                None
            },
        );
    }

    fn hoist_variable(&self, name: Id<Node>) {
        if is_identifier(&name.ref_(self)) {
            self.context.hoist_variable_declaration(name);
        } else {
            for &element in &name.ref_(self).as_has_elements().elements() {
                if !is_omitted_expression(&element.ref_(self)) {
                    self.hoist_variable(element);
                }
            }
        }
    }

    fn transform_initialized_variable(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_variable_declaration = node_ref.as_variable_declaration();
        let converted = set_source_map_range(
            self.factory.create_assignment(
                self.factory
                    .converters()
                    .convert_to_assignment_element_target(node_as_variable_declaration.name()),
                node_as_variable_declaration.maybe_initializer().unwrap(),
            ),
            Some((&*node.ref_(self)).into()),
            self,
        );
        Ok(try_visit_node(
            converted,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?)
    }

    fn collides_with_parameter_name(&self, name: Id<Node>) -> bool {
        if is_identifier(&name.ref_(self)) {
            return self
                .enclosing_function_parameter_names()
                .contains(&name.ref_(self).as_identifier().escaped_text);
        } else {
            for &element in &name.ref_(self).as_has_elements().elements() {
                if !is_omitted_expression(&element.ref_(self)) && self.collides_with_parameter_name(element) {
                    return true;
                }
            }
        }
        false
    }

    fn transform_async_function_body(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration*/
    ) -> io::Result<Id<Node /*ConciseBody*/>> {
        let node_ref = node.ref_(self);
        let node_as_function_like_declaration = node_ref.as_function_like_declaration();
        self.context.resume_lexical_environment();

        let ref original = get_original_node(node, self);
        let node_type = original.ref_(self).as_has_type().maybe_type();
        let promise_constructor = if self.language_version < ScriptTarget::ES2015 {
            self.get_promise_constructor(node_type)?
        } else {
            None
        };
        let is_arrow_function = node.ref_(self).kind() == SyntaxKind::ArrowFunction;
        let has_lexical_arguments = self
            .resolver
            .get_node_check_flags(node)
            .intersects(NodeCheckFlags::CaptureArguments);

        let saved_enclosing_function_parameter_names =
            self.maybe_enclosing_function_parameter_names().clone();
        self.set_enclosing_function_parameter_names(Some(HashSet::new()));
        for &parameter in &node_as_function_like_declaration.parameters() {
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

        let result: Id<Node /*ConciseBody*/>;
        if !is_arrow_function {
            let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
            let statement_offset = self.factory.try_copy_prologue(
                &node_as_function_like_declaration
                    .maybe_body()
                    .unwrap()
                    .ref_(self).as_block()
                    .statements,
                &mut statements,
                Some(false),
                Some(|node: Id<Node>| self.visitor(node)),
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
                                node_as_function_like_declaration.maybe_body().unwrap(),
                                Some(statement_offset),
                            )?,
                        ),
                )),
            );

            insert_statements_after_standard_prologue(
                &mut statements,
                self.context.end_lexical_environment().as_deref(),
                self,
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
                        .insert(get_node_id(&variable_statement.ref_(self)), true);
                    insert_statements_after_standard_prologue(
                        &mut statements,
                        Some(&[variable_statement]),
                        self,
                    );
                }
            }

            let block = self.factory.create_block(statements, Some(true));
            set_text_range(
                &*block.ref_(self),
                node_as_function_like_declaration.maybe_body().refed(self),
            );

            if emit_super_helpers && self.maybe_has_super_element_access() == Some(true) {
                if self
                    .resolver
                    .get_node_check_flags(node)
                    .intersects(NodeCheckFlags::AsyncMethodWithSuperBinding)
                {
                    add_emit_helper(block, advanced_async_super_helper(), self);
                } else if self
                    .resolver
                    .get_node_check_flags(node)
                    .intersects(NodeCheckFlags::AsyncMethodWithSuper)
                {
                    add_emit_helper(block, async_super_helper(), self);
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
                        node_as_function_like_declaration.maybe_body().unwrap(),
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
                    .convert_to_function_block(expression, None);
                let block_as_block = block.ref_(self).as_block();
                result = self.factory.update_block(
                    block,
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
        body: Id<Node>, /*ConciseBody*/
        start: Option<usize>,
    ) -> io::Result<Id<Node>> {
        Ok(if is_block(&body.ref_(self)) {
            self.factory.update_block(
                body,
                try_visit_nodes(
                    &body.ref_(self).as_block().statements,
                    Some(|node: Id<Node>| self.async_body_visitor(node)),
                    Some(|node| is_statement(node, self)),
                    start,
                    None,
                )?,
            )
        } else {
            self.factory.converters().convert_to_function_block(
                try_visit_node(
                    body,
                    Some(|node: Id<Node>| self.async_body_visitor(node)),
                    Some(|node| is_concise_body(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?,
                None,
            )
        })
    }

    fn get_promise_constructor(
        &self,
        type_: Option<Id<Node> /*TypeNode*/>,
    ) -> io::Result<Option<Id<Node>>> {
        type_
            .and_then(|type_| get_entity_name_from_type_node(type_, self))
            .filter(|type_name| is_entity_name(&type_name.ref_(self)))
            .try_and_then(|type_name| -> io::Result<_> {
                let serialization_kind = self
                    .resolver
                    .get_type_reference_serialization_kind(type_name, None)?;
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

    fn substitute_expression(&self, node: Id<Node> /*Expression*/) -> Id<Node> {
        match node.ref_(self).kind() {
            SyntaxKind::PropertyAccessExpression => {
                self.substitute_property_access_expression(node)
            }
            SyntaxKind::ElementAccessExpression => self.substitute_element_access_expression(node),
            SyntaxKind::CallExpression => self.substitute_call_expression(node),
            _ => node,
        }
    }

    fn substitute_property_access_expression(
        &self,
        node: Id<Node>, /*PropertyAccessExpression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_property_access_expression = node_ref.as_property_access_expression();
        if node_as_property_access_expression.expression.ref_(self).kind() == SyntaxKind::SuperKeyword {
            return set_text_range_id_node(
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
                Some(&*node.ref_(self)),
                self,
            );
        }
        node
    }

    fn substitute_element_access_expression(
        &self,
        node: Id<Node>, /*ElementAccessExpression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_element_access_expression = node_ref.as_element_access_expression();
        if node_as_element_access_expression.expression.ref_(self).kind() == SyntaxKind::SuperKeyword {
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
        node
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

    fn create_super_element_access_in_async_method(
        &self,
        argument_expression: Id<Node>,     /*Expression*/
        location: &impl ReadonlyTextRange, /*TextRange*/
    ) -> Id<Node /*LeftHandSideExpression*/> {
        if self
            .enclosing_super_container_flags()
            .intersects(NodeCheckFlags::AsyncMethodWithSuperBinding)
        {
            set_text_range_id_node(
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
                        Some(vec![argument_expression]),
                    ),
                    "value",
                ),
                Some(location),
                self,
            )
        } else {
            set_text_range_id_node(
                self.factory.create_call_expression(
                    self.factory.create_unique_name(
                        "_superIndex",
                        Some(
                            GeneratedIdentifierFlags::Optimistic
                                | GeneratedIdentifierFlags::FileLevel,
                        ),
                    ),
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![argument_expression]),
                ),
                Some(location),
                self,
            )
        }
    }
}

impl TransformerInterface for TransformES2017 {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        self.transform_source_file(node)
    }
}

impl HasArena for TransformES2017 {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
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
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
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
            .get(&get_node_id(&node.ref_(self)))
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

impl HasArena for TransformES2017OnEmitNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
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
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        let node = self
            .previous_on_substitute_node
            .on_substitute_node(hint, node)?;
        if hint == EmitHint::Expression
            && self.transform_es2017.enclosing_super_container_flags() != NodeCheckFlags::None
        {
            return Ok(self.transform_es2017.substitute_expression(node));
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
    node: Id<Node>, /*FunctionLikeDeclaration*/
    names: &HashSet<__String>,
) -> Id<Node> {
    let has_binding = resolver
        .get_node_check_flags(node)
        .intersects(NodeCheckFlags::AsyncMethodWithSuperBinding);
    let mut accessors: Vec<Id<Node /*PropertyAssignment*/>> = Default::default();
    names.iter().for_each(|key| {
        let name = unescape_leading_underscores(key);
        let mut getter_and_setter: Vec<Id<Node /*PropertyAssignment*/>> = Default::default();
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
                        set_emit_flags(factory.create_super(), EmitFlags::NoSubstitution, factory),
                        name,
                    ),
                    EmitFlags::NoSubstitution,
                    factory,
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
                                set_emit_flags(factory.create_super(), EmitFlags::NoSubstitution, factory),
                                name,
                            ),
                            EmitFlags::NoSubstitution,
                            factory,
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
