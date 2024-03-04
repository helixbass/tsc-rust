use std::{
    any::Any,
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashMap,
    io,
};

use id_arena::Id;

use crate::{
    CompilerOptions, EmitResolver, Node, NodeFactory, ScriptTarget, TransformationContext,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface, _d,
    chain_bundle, create_empty_exports, create_external_helpers_import_declaration_if_needed,
    downcast_transformer_ref, get_emit_flags, get_emit_script_target,
    get_external_module_name_literal, has_syntactic_modifier, id_text, impl_has_arena,
    insert_statements_after_custom_prologue, is_export_namespace_as_default_declaration,
    is_external_module, is_external_module_import_equals_declaration, is_external_module_indicator,
    is_identifier, is_namespace_export, is_source_file, is_statement, per_arena, ref_mut_unwrapped,
    ref_unwrapped, released, single_or_many_node, try_visit_each_child, try_visit_nodes, AllArenas,
    BoolExt, CoreTransformationContext, Debug_, EmitFlags, EmitHelperFactory, EmitHint, EmitHost,
    GeneratedIdentifierFlags, GetOrInsertDefault, HasArena, HasStatementsInterface, InArena,
    Matches, ModifierFlags, ModuleKind, NamedDeclarationInterface, NodeArray, NodeArrayExt,
    NodeExt, NodeFlags, NodeInterface, SyntaxKind, TransformNodesTransformationResult,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    VecExt, VisitResult,
};

struct TransformEcmascriptModule {
    arena: *const AllArenas,
    context: Id<TransformNodesTransformationResult>,
    factory: Id<NodeFactory>,
    host: Id<Box<dyn EmitHost>>,
    resolver: Id<Box<dyn EmitResolver>>,
    compiler_options: Id<CompilerOptions>,
    language_version: ScriptTarget,
    helper_name_substitutions: RefCell<Option<HashMap<String, Id<Node /*Identifier*/>>>>,
    current_source_file: Cell<Option<Id<Node /*SourceFile*/>>>,
    import_require_statements: Cell<
        Option<(
            Id<Node /*ImportDeclaration*/>,
            Id<Node /*VariableStatement*/>,
        )>,
    >,
}

impl TransformEcmascriptModule {
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
            host: context_ref.get_emit_host(),
            resolver: context_ref.get_emit_resolver(),
            language_version: get_emit_script_target(&compiler_options.ref_(arena_ref)),
            compiler_options,
            context: context.clone(),
            helper_name_substitutions: _d(),
            current_source_file: _d(),
            import_require_statements: _d(),
        }));
        context_ref.override_on_emit_node(&mut |previous_on_emit_node| {
            arena_ref.alloc_transformation_context_on_emit_node_overrider(Box::new(
                TransformEcmascriptModuleOnEmitNodeOverrider::new(
                    ret,
                    previous_on_emit_node,
                    arena_ref,
                ),
            ))
        });
        context_ref.override_on_substitute_node(&mut |previous_on_substitute_node| {
            arena_ref.alloc_transformation_context_on_substitute_node_overrider(Box::new(
                TransformEcmascriptModuleOnSubstituteNodeOverrider::new(
                    ret,
                    previous_on_substitute_node,
                    arena_ref,
                ),
            ))
        });
        context_ref.enable_emit_notification(SyntaxKind::SourceFile);
        context_ref.enable_substitution(SyntaxKind::Identifier);
        ret
    }

    pub(super) fn emit_helpers(&self) -> debug_cell::Ref<'_, EmitHelperFactory> {
        self.context.ref_(self).get_emit_helper_factory().ref_(self)
    }

    fn maybe_helper_name_substitutions(
        &self,
    ) -> Ref<Option<HashMap<String, Id<Node /*Identifier*/>>>> {
        self.helper_name_substitutions.borrow()
    }

    pub(super) fn helper_name_substitutions(
        &self,
    ) -> Ref<HashMap<String, Id<Node /*Identifier*/>>> {
        ref_unwrapped(&self.helper_name_substitutions)
    }

    pub(super) fn helper_name_substitutions_mut(
        &self,
    ) -> RefMut<HashMap<String, Id<Node /*Identifier*/>>> {
        ref_mut_unwrapped(&self.helper_name_substitutions)
    }

    fn set_helper_name_substitutions(
        &self,
        helper_name_substitutions: Option<HashMap<String, Id<Node /*Identifier*/>>>,
    ) {
        *self.helper_name_substitutions.borrow_mut() = helper_name_substitutions;
    }

    fn maybe_current_source_file(&self) -> Option<Id<Node /*SourceFile*/>> {
        self.current_source_file.get()
    }

    fn set_current_source_file(&self, current_source_file: Option<Id<Node /*SourceFile*/>>) {
        self.current_source_file.set(current_source_file);
    }

    fn maybe_import_require_statements(
        &self,
    ) -> Option<(
        Id<Node /*ImportDeclaration*/>,
        Id<Node /*VariableStatement*/>,
    )> {
        self.import_require_statements.get()
    }

    pub(super) fn import_require_statements(
        &self,
    ) -> (
        Id<Node /*ImportDeclaration*/>,
        Id<Node /*VariableStatement*/>,
    ) {
        self.import_require_statements.get().unwrap()
    }

    fn set_import_require_statements(
        &self,
        import_require_statements: Option<(
            Id<Node /*ImportDeclaration*/>,
            Id<Node /*VariableStatement*/>,
        )>,
    ) {
        self.import_require_statements
            .set(import_require_statements);
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> io::Result<Id<Node>> {
        if node.ref_(self).as_source_file().is_declaration_file() {
            return Ok(node);
        }

        if is_external_module(&node.ref_(self))
            || self.compiler_options.ref_(self).isolated_modules == Some(true)
        {
            self.set_current_source_file(Some(node));
            self.set_import_require_statements(None);
            let mut result = self.update_external_module(node)?;
            self.set_current_source_file(None);
            if let Some(import_require_statements) = self.maybe_import_require_statements() {
                result = self.factory.ref_(self).update_source_file(
                    result,
                    self.factory
                        .ref_(self)
                        .create_node_array(
                            {
                                let mut statements = result
                                    .ref_(self)
                                    .as_source_file()
                                    .statements()
                                    .ref_(self)
                                    .to_vec();
                                insert_statements_after_custom_prologue(
                                    &mut statements,
                                    Some(&[
                                        import_require_statements.0.clone(),
                                        import_require_statements.1.clone(),
                                    ]),
                                    self,
                                );
                                Some(statements)
                            },
                            None,
                        )
                        .set_text_range(
                            Some(&*result.ref_(self).as_source_file().statements().ref_(self)),
                            self,
                        ),
                    None,
                    None,
                    None,
                    None,
                    None,
                );
            }
            if !is_external_module(&node.ref_(self))
                || result
                    .ref_(self)
                    .as_source_file()
                    .statements()
                    .ref_(self)
                    .iter()
                    .any(|&statement| is_external_module_indicator(statement, self))
            {
                return Ok(result);
            }
            return Ok(self.factory.ref_(self).update_source_file(
                result,
                self.factory
                    .ref_(self)
                    .create_node_array(
                        Some(
                            released!(result
                                .ref_(self)
                                .as_source_file()
                                .statements()
                                .ref_(self)
                                .to_vec())
                            .and_push(create_empty_exports(&self.factory.ref_(self))),
                        ),
                        None,
                    )
                    .set_text_range(
                        Some(
                            &*released!(result.ref_(self).as_source_file().statements()).ref_(self),
                        ),
                        self,
                    ),
                None,
                None,
                None,
                None,
                None,
            ));
        }

        Ok(node)
    }

    fn update_external_module(&self, node: Id<Node> /*SourceFile*/) -> io::Result<Id<Node>> {
        let external_helpers_import_declaration =
            create_external_helpers_import_declaration_if_needed(
                &self.factory.ref_(self),
                &self.emit_helpers(),
                node,
                &self.compiler_options.ref_(self),
                None,
                None,
                None,
            );
        Ok(match external_helpers_import_declaration {
            Some(external_helpers_import_declaration) => {
                let mut statements: Vec<Id<Node /*Statement*/>> = _d();
                let statement_offset = self.factory.ref_(self).copy_prologue(
                    &node.ref_(self).as_source_file().statements().ref_(self),
                    &mut statements,
                    None,
                    Option::<fn(Id<Node>) -> VisitResult>::None,
                );
                statements.push(external_helpers_import_declaration);
                statements.extend(
                    try_visit_nodes(
                        node.ref_(self).as_source_file().statements(),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_statement(node, self)),
                        Some(statement_offset),
                        None,
                        self,
                    )?
                    .ref_(self)
                    .iter()
                    .copied(),
                );
                self.factory.ref_(self).update_source_file(
                    node,
                    self.factory
                        .ref_(self)
                        .create_node_array(Some(statements), None)
                        .set_text_range(
                            Some(&*node.ref_(self).as_source_file().statements().ref_(self)),
                            self,
                        ),
                    None,
                    None,
                    None,
                    None,
                    None,
                )
            }
            None => try_visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?,
        })
    }

    fn visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/ {
        Ok(match released!(node.ref_(self).kind()) {
            SyntaxKind::ImportEqualsDeclaration => {
                (get_emit_script_target(&self.compiler_options.ref_(self))
                // TODO: this definitely looks like an upstream bug of using ModuleKind instead
                // of ScriptTarget - technically should say ScriptTarget::ES2019 here to be using
                // the same exact enum int value but let's see if this causes any problems
                >= ScriptTarget::ES2020)
                    .try_then_and(|| self.visit_import_equals_declaration(node))?
            }
            SyntaxKind::ExportAssignment => self.visit_export_assignment(node),
            SyntaxKind::ExportDeclaration => {
                let export_decl = node;
                self.visit_export_declaration(export_decl)
            }
            _ => Some(node.into()),
        })
    }

    fn create_require_call(
        &self,
        import_node: Id<Node>, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
    ) -> io::Result<Id<Node>> {
        let module_name = get_external_module_name_literal(
            &self.factory.ref_(self),
            import_node,
            Debug_.check_defined(self.maybe_current_source_file(), None),
            &**self.host.ref_(self),
            &**self.resolver.ref_(self),
            &self.compiler_options.ref_(self),
        )?;
        let mut args: Vec<Id<Node /*Expression*/>> = _d();
        if let Some(module_name) = module_name {
            args.push(module_name);
        }

        if self.maybe_import_require_statements().is_none() {
            let create_require_name = self.factory.ref_(self).create_unique_name(
                "_createRequire",
                Some(GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel),
            );
            let import_statement = self.factory.ref_(self).create_import_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                Some(self.factory.ref_(self).create_import_clause(
                    false,
                    None,
                    Some(self.factory.ref_(self).create_named_imports(vec![
                        self.factory.ref_(self).create_import_specifier(
                            false,
                            Some(self.factory.ref_(self).create_identifier("createRequire")),
                            create_require_name.clone(),
                        ),
                    ])),
                )),
                self.factory
                    .ref_(self)
                    .create_string_literal("module".to_owned(), None, None),
                None,
            );
            let require_helper_name = self.factory.ref_(self).create_unique_name(
                "__require",
                Some(GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel),
            );
            let require_statement = self.factory.ref_(self).create_variable_statement(
                Option::<Id<NodeArray>>::None,
                self.factory.ref_(self).create_variable_declaration_list(
                    vec![self.factory.ref_(self).create_variable_declaration(
                        Some(require_helper_name),
                        None,
                        None,
                        Some(self.factory.ref_(self).create_call_expression(
                            self.factory.ref_(self).clone_node(create_require_name),
                            Option::<Id<NodeArray>>::None,
                            Some(vec![
                                self.factory.ref_(self).create_property_access_expression(
                                    self.factory.ref_(self).create_meta_property(
                                        SyntaxKind::ImportKeyword,
                                        self.factory.ref_(self).create_identifier("meta"),
                                    ),
                                    self.factory.ref_(self).create_identifier("url"),
                                ),
                            ]),
                        )),
                    )],
                    Some(
                        (self.language_version >= ScriptTarget::ES2015)
                            .then_some(NodeFlags::Const)
                            .unwrap_or_default(),
                    ),
                ),
            );
            self.set_import_require_statements(Some((import_statement, require_statement)));
        }

        let name = self
            .import_require_statements()
            .1
            .ref_(self)
            .as_variable_statement()
            .declaration_list
            .ref_(self)
            .as_variable_declaration_list()
            .declarations
            .ref_(self)[0]
            .ref_(self)
            .as_variable_declaration()
            .name();
        Debug_.assert_node(
            Some(name),
            Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
            None,
        );
        Ok(self.factory.ref_(self).create_call_expression(
            self.factory.ref_(self).clone_node(name),
            Option::<Id<NodeArray>>::None,
            Some(args),
        ))
    }

    fn visit_import_equals_declaration(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_import_equals_declaration = node_ref.as_import_equals_declaration();
        Debug_.assert(
            is_external_module_import_equals_declaration(node, self),
            Some("import= for internal module references should be handled in an earlier transformer.")
        );

        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();
        statements.get_or_insert_default_().push(
            self.factory
                .ref_(self)
                .create_variable_statement(
                    Option::<Id<NodeArray>>::None,
                    self.factory.ref_(self).create_variable_declaration_list(
                        vec![self.factory.ref_(self).create_variable_declaration(
                            Some(
                                self.factory
                                    .ref_(self)
                                    .clone_node(node_as_import_equals_declaration.name()),
                            ),
                            None,
                            None,
                            Some(self.create_require_call(node)?),
                        )],
                        Some(
                            (self.language_version >= ScriptTarget::ES2015)
                                .then_some(NodeFlags::Const)
                                .unwrap_or_default(),
                        ),
                    ),
                )
                .set_text_range(Some(&*node.ref_(self)), self)
                .set_original_node(Some(node), self),
        );

        self.append_exports_of_import_equals_declaration(&mut statements, node);

        Ok(statements.map(single_or_many_node))
    }

    fn append_exports_of_import_equals_declaration(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        node: Id<Node>, /*ImportEqualsDeclaration*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_import_equals_declaration = node_ref.as_import_equals_declaration();
        if has_syntactic_modifier(node, ModifierFlags::Export, self) {
            statements.get_or_insert_default_().push(
                self.factory.ref_(self).create_export_declaration(
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
                    node_as_import_equals_declaration.is_type_only,
                    Some(self.factory.ref_(self).create_named_exports(vec![
                        self.factory.ref_(self).create_export_specifier(
                            false,
                            Option::<Id<Node>>::None,
                            id_text(&node_as_import_equals_declaration.name().ref_(self)),
                        ),
                    ])),
                    None,
                    None,
                ),
            );
        }
        // return statements;
    }

    fn visit_export_assignment(&self, node: Id<Node> /*ExportAssignment*/) -> VisitResult /*<ExportAssignment>*/
    {
        let node_ref = node.ref_(self);
        let node_as_export_assignment = node_ref.as_export_assignment();
        (node_as_export_assignment.is_export_equals != Some(true)).then(|| node.into())
    }

    fn visit_export_declaration(&self, node: Id<Node> /*ExportDeclaration*/) -> VisitResult {
        if self
            .compiler_options
            .ref_(self)
            .module
            .matches(|compiler_options_module| compiler_options_module > ModuleKind::ES2015)
        {
            return Some(node.into());
        }

        let node_export_clause = node.ref_(self).as_export_declaration().export_clause;
        let node_module_specifier = node.ref_(self).as_export_declaration().module_specifier;
        if !node_export_clause
            .matches(|node_export_clause| is_namespace_export(&node_export_clause.ref_(self)))
            || node_module_specifier.is_none()
        {
            return Some(node.into());
        }
        let node_export_clause = node_export_clause.unwrap();
        let node_module_specifier = node_module_specifier.unwrap();

        let old_identifier = node_export_clause.ref_(self).as_namespace_export().name;
        let synth_name = self
            .factory
            .ref_(self)
            .get_generated_name_for_node(Some(old_identifier), None);
        let import_decl = self
            .factory
            .ref_(self)
            .create_import_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                Some(
                    self.factory.ref_(self).create_import_clause(
                        false,
                        None,
                        Some(
                            self.factory
                                .ref_(self)
                                .create_namespace_import(synth_name.clone()),
                        ),
                    ),
                ),
                node_module_specifier,
                released!(node.ref_(self).as_export_declaration().assert_clause),
            )
            .set_original_node(Some(node_export_clause), self);

        let export_decl = if is_export_namespace_as_default_declaration(node, self) {
            self.factory.ref_(self).create_export_default(synth_name)
        } else {
            self.factory.ref_(self).create_export_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                false,
                Some(self.factory.ref_(self).create_named_exports(vec![
                    self.factory.ref_(self).create_export_specifier(
                        false,
                        Some(synth_name),
                        old_identifier.clone(),
                    ),
                ])),
                None,
                None,
            )
        }
        .set_original_node(Some(node), self);

        Some(vec![import_decl, export_decl].into())
    }
}

impl TransformerInterface for TransformEcmascriptModule {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        self.transform_source_file(node)
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl_has_arena!(TransformEcmascriptModule);

struct TransformEcmascriptModuleOnEmitNodeOverrider {
    arena: *const AllArenas,
    transform_ecmascript_module: Transformer,
    previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformEcmascriptModuleOnEmitNodeOverrider {
    fn new(
        transform_ecmascript_module: Transformer,
        previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_ecmascript_module,
            previous_on_emit_node,
        }
    }

    fn transform_ecmascript_module(&self) -> debug_cell::Ref<'_, TransformEcmascriptModule> {
        downcast_transformer_ref(self.transform_ecmascript_module, self)
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformEcmascriptModuleOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        if is_source_file(&node.ref_(self)) {
            if (is_external_module(&node.ref_(self))
                || self
                    .transform_ecmascript_module()
                    .compiler_options
                    .ref_(self)
                    .isolated_modules
                    == Some(true))
                && self
                    .transform_ecmascript_module()
                    .compiler_options
                    .ref_(self)
                    .import_helpers
                    == Some(true)
            {
                self.transform_ecmascript_module()
                    .set_helper_name_substitutions(Some(_d()));
            }
            self.previous_on_emit_node
                .ref_(self)
                .on_emit_node(hint, node, emit_callback)?;
            self.transform_ecmascript_module()
                .set_helper_name_substitutions(None);
        } else {
            self.previous_on_emit_node
                .ref_(self)
                .on_emit_node(hint, node, emit_callback)?;
        }

        Ok(())
    }
}

impl_has_arena!(TransformEcmascriptModuleOnEmitNodeOverrider);

struct TransformEcmascriptModuleOnSubstituteNodeOverrider {
    arena: *const AllArenas,
    transform_ecmascript_module: Transformer,
    previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformEcmascriptModuleOnSubstituteNodeOverrider {
    fn new(
        transform_ecmascript_module: Transformer,
        previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_ecmascript_module,
            previous_on_substitute_node,
        }
    }

    fn transform_ecmascript_module(&self) -> debug_cell::Ref<'_, TransformEcmascriptModule> {
        downcast_transformer_ref(self.transform_ecmascript_module, self)
    }

    fn substitute_helper_name(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> Id<Node /*Expression*/> {
        let node_ref = node.ref_(self);
        let name = id_text(&node_ref);
        let mut substitution = self
            .transform_ecmascript_module()
            .helper_name_substitutions()
            .get(name)
            .cloned();
        if substitution.is_none() {
            substitution = Some(
                self.transform_ecmascript_module()
                    .factory
                    .ref_(self)
                    .create_unique_name(
                        name,
                        Some(
                            GeneratedIdentifierFlags::Optimistic
                                | GeneratedIdentifierFlags::FileLevel,
                        ),
                    ),
            );
            self.transform_ecmascript_module()
                .helper_name_substitutions_mut()
                .insert(name.to_owned(), substitution.clone().unwrap());
        }
        substitution.unwrap()
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformEcmascriptModuleOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        let node = self
            .previous_on_substitute_node
            .ref_(self)
            .on_substitute_node(hint, node)?;
        if self
            .transform_ecmascript_module()
            .maybe_helper_name_substitutions()
            .is_some()
            && is_identifier(&node.ref_(self))
            && get_emit_flags(node, self).intersects(EmitFlags::HelperName)
        {
            return Ok(self.substitute_helper_name(node));
        }

        Ok(node)
    }
}

impl_has_arena!(TransformEcmascriptModuleOnSubstituteNodeOverrider);

struct TransformEcmascriptModuleFactory {
    arena: *const AllArenas,
}

impl TransformEcmascriptModuleFactory {
    fn new(arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
        }
    }
}

impl TransformerFactoryInterface for TransformEcmascriptModuleFactory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle(self).ref_(self).call(
            context.clone(),
            TransformEcmascriptModule::new(context, self.arena),
        )
    }
}

impl_has_arena!(TransformEcmascriptModuleFactory);

pub fn transform_ecmascript_module(arena: &impl HasArena) -> TransformerFactory {
    // "static"-ifying this to bypass arena borrow errors
    per_arena!(
        Box<dyn TransformerFactoryInterface>,
        arena,
        arena.alloc_transformer_factory(Box::new(TransformEcmascriptModuleFactory::new(arena))),
    )
}
