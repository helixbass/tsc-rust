use std::{collections::HashMap, io, mem};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use id_arena::Id;

use crate::{
    BaseNodeFactorySynthetic, CompilerOptions, EmitResolver, Node, NodeFactory, ScriptTarget,
    TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface, _d, chain_bundle, create_empty_exports,
    create_external_helpers_import_declaration_if_needed, gc_cell_ref_mut_unwrapped,
    gc_cell_ref_unwrapped, get_emit_flags, get_emit_script_target,
    get_external_module_name_literal, has_syntactic_modifier, id_text,
    insert_statements_after_custom_prologue, is_export_namespace_as_default_declaration,
    is_external_module, is_external_module_import_equals_declaration, is_external_module_indicator,
    is_identifier, is_namespace_export, is_source_file, is_statement, single_or_many_node,
    try_visit_each_child, try_visit_nodes, BoolExt, Debug_, EmitFlags, EmitHelperFactory, EmitHint,
    EmitHost, GeneratedIdentifierFlags, GetOrInsertDefault, HasStatementsInterface, Matches,
    ModifierFlags, ModuleKind, NamedDeclarationInterface, NodeArray, NodeArrayExt, NodeExt,
    NodeFlags, NodeInterface, SyntaxKind, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, VecExt, VisitResult,
};

#[derive(Trace, Finalize)]
struct TransformEcmascriptModule {
    _transformer_wrapper: GcCell<Option<Transformer>>,
    context: Gc<Box<dyn TransformationContext>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    host: Gc<Box<dyn EmitHost>>,
    resolver: Gc<Box<dyn EmitResolver>>,
    compiler_options: Gc<CompilerOptions>,
    #[unsafe_ignore_trace]
    language_version: ScriptTarget,
    helper_name_substitutions: GcCell<Option<HashMap<String, Id<Node /*Identifier*/>>>>,
    current_source_file: GcCell<Option<Id<Node /*SourceFile*/>>>,
    import_require_statements: GcCell<
        Option<(
            Id<Node /*ImportDeclaration*/>,
            Id<Node /*VariableStatement*/>,
        )>,
    >,
}

impl TransformEcmascriptModule {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let compiler_options = context.get_compiler_options();
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: _d(),
            factory: context.factory(),
            host: context.get_emit_host(),
            resolver: context.get_emit_resolver(),
            language_version: get_emit_script_target(&compiler_options),
            compiler_options,
            context: context.clone(),
            helper_name_substitutions: _d(),
            current_source_file: _d(),
            import_require_statements: _d(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformEcmascriptModuleOnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(
                TransformEcmascriptModuleOnSubstituteNodeOverrider::new(
                    downcasted.clone(),
                    previous_on_substitute_node,
                ),
            ))
        });
        context.enable_emit_notification(SyntaxKind::SourceFile);
        context.enable_substitution(SyntaxKind::Identifier);
        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    fn emit_helpers(&self) -> Gc<EmitHelperFactory> {
        self.context.get_emit_helper_factory()
    }

    fn maybe_helper_name_substitutions(
        &self,
    ) -> GcCellRef<Option<HashMap<String, Id<Node /*Identifier*/>>>> {
        self.helper_name_substitutions.borrow()
    }

    pub(super) fn helper_name_substitutions(
        &self,
    ) -> GcCellRef<HashMap<String, Id<Node /*Identifier*/>>> {
        gc_cell_ref_unwrapped(&self.helper_name_substitutions)
    }

    pub(super) fn helper_name_substitutions_mut(
        &self,
    ) -> GcCellRefMut<
        Option<HashMap<String, Id<Node /*Identifier*/>>>,
        HashMap<String, Id<Node /*Identifier*/>>,
    > {
        gc_cell_ref_mut_unwrapped(&self.helper_name_substitutions)
    }

    fn set_helper_name_substitutions(
        &self,
        helper_name_substitutions: Option<HashMap<String, Id<Node /*Identifier*/>>>,
    ) {
        *self.helper_name_substitutions.borrow_mut() = helper_name_substitutions;
    }

    fn maybe_current_source_file(&self) -> Option<Id<Node /*SourceFile*/>> {
        self.current_source_file.borrow().clone()
    }

    fn set_current_source_file(&self, current_source_file: Option<Id<Node /*SourceFile*/>>) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    fn maybe_import_require_statements(
        &self,
    ) -> Option<(
        Id<Node /*ImportDeclaration*/>,
        Id<Node /*VariableStatement*/>,
    )> {
        self.import_require_statements.borrow().clone()
    }

    pub(super) fn import_require_statements(
        &self,
    ) -> (
        Id<Node /*ImportDeclaration*/>,
        Id<Node /*VariableStatement*/>,
    ) {
        self.import_require_statements.borrow().clone().unwrap()
    }

    fn set_import_require_statements(
        &self,
        import_require_statements: Option<(
            Id<Node /*ImportDeclaration*/>,
            Id<Node /*VariableStatement*/>,
        )>,
    ) {
        *self.import_require_statements.borrow_mut() = import_require_statements;
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> io::Result<Id<Node>> {
        let node_as_source_file = node.as_source_file();
        if node_as_source_file.is_declaration_file() {
            return Ok(node.node_wrapper());
        }

        if is_external_module(node) || self.compiler_options.isolated_modules == Some(true) {
            self.set_current_source_file(Some(node.node_wrapper()));
            self.set_import_require_statements(None);
            let mut result = self.update_external_module(node)?;
            self.set_current_source_file(None);
            if let Some(import_require_statements) = self.maybe_import_require_statements() {
                result = self.factory.update_source_file(
                    &result,
                    self.factory
                        .create_node_array(
                            {
                                let mut statements = result.as_source_file().statements().to_vec();
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
                        .set_text_range(Some(&*result.as_source_file().statements())),
                    None,
                    None,
                    None,
                    None,
                    None,
                );
            }
            if !is_external_module(node)
                || result
                    .as_source_file()
                    .statements()
                    .iter()
                    .any(|statement| is_external_module_indicator(statement, self))
            {
                return Ok(result);
            }
            return Ok(self.factory.update_source_file(
                &result,
                self.factory
                    .create_node_array(
                        Some(
                            result
                                .as_source_file()
                                .statements()
                                .to_vec()
                                .and_push(create_empty_exports(&self.factory)),
                        ),
                        None,
                    )
                    .set_text_range(Some(&*result.as_source_file().statements())),
                None,
                None,
                None,
                None,
                None,
            ));
        }

        Ok(node.node_wrapper())
    }

    fn update_external_module(&self, node: Id<Node> /*SourceFile*/) -> io::Result<Id<Node>> {
        let node_as_source_file = node.as_source_file();
        let external_helpers_import_declaration =
            create_external_helpers_import_declaration_if_needed(
                &self.factory,
                &self.emit_helpers(),
                node,
                &self.compiler_options,
                None,
                None,
                None,
            );
        Ok(match external_helpers_import_declaration {
            Some(external_helpers_import_declaration) => {
                let mut statements: Vec<Id<Node /*Statement*/>> = _d();
                let statement_offset = self.factory.copy_prologue(
                    &node_as_source_file.statements(),
                    &mut statements,
                    None,
                    Option::<fn(Id<Node>) -> VisitResult>::None,
                );
                statements.push(external_helpers_import_declaration);
                statements.extend(
                    try_visit_nodes(
                        &node_as_source_file.statements(),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_statement(node, self)),
                        Some(statement_offset),
                        None,
                    )?
                    .owned_iter(),
                );
                self.factory.update_source_file(
                    node,
                    self.factory
                        .create_node_array(Some(statements), None)
                        .set_text_range(Some(&*node_as_source_file.statements())),
                    None,
                    None,
                    None,
                    None,
                    None,
                )
            }
            None => {
                try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context)?
            }
        })
    }

    fn visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/ {
        Ok(match node.kind() {
            SyntaxKind::ImportEqualsDeclaration => (get_emit_script_target(&self.compiler_options)
                // TODO: this definitely looks like an upstream bug of using ModuleKind instead
                // of ScriptTarget - technically should say ScriptTarget::ES2019 here to be using
                // the same exact enum int value but let's see if this causes any problems
                >= ScriptTarget::ES2020)
                .try_then_and(|| self.visit_import_equals_declaration(node))?,
            SyntaxKind::ExportAssignment => self.visit_export_assignment(node),
            SyntaxKind::ExportDeclaration => {
                let export_decl = node;
                self.visit_export_declaration(export_decl)
            }
            _ => Some(node.node_wrapper().into()),
        })
    }

    fn create_require_call(
        &self,
        import_node: Id<Node>, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
    ) -> io::Result<Id<Node>> {
        let module_name = get_external_module_name_literal(
            &self.factory,
            import_node,
            &*Debug_.check_defined(self.maybe_current_source_file(), None),
            &**self.host,
            &**self.resolver,
            &self.compiler_options,
        )?;
        let mut args: Vec<Id<Node /*Expression*/>> = _d();
        if let Some(module_name) = module_name {
            args.push(module_name);
        }

        if self.maybe_import_require_statements().is_none() {
            let create_require_name = self.factory.create_unique_name(
                "_createRequire",
                Some(GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel),
            );
            let import_statement = self.factory.create_import_declaration(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                Some(self.factory.create_import_clause(
                    false,
                    None,
                    Some(self.factory.create_named_imports(vec![
                        self.factory.create_import_specifier(
                            false,
                            Some(self.factory.create_identifier("createRequire")),
                            create_require_name.clone(),
                        ),
                    ])),
                )),
                self.factory
                    .create_string_literal("module".to_owned(), None, None),
                None,
            );
            let require_helper_name = self.factory.create_unique_name(
                "__require",
                Some(GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel),
            );
            let require_statement = self.factory.create_variable_statement(
                Option::<Gc<NodeArray>>::None,
                self.factory.create_variable_declaration_list(
                    vec![self.factory.create_variable_declaration(
                        Some(require_helper_name),
                        None,
                        None,
                        Some(self.factory.create_call_expression(
                            self.factory.clone_node(&create_require_name),
                            Option::<Gc<NodeArray>>::None,
                            Some(vec![self.factory.create_property_access_expression(
                                self.factory.create_meta_property(
                                    SyntaxKind::ImportKeyword,
                                    self.factory.create_identifier("meta"),
                                ),
                                self.factory.create_identifier("url"),
                            )]),
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
            .as_variable_statement()
            .declaration_list
            .as_variable_declaration_list()
            .declarations[0]
            .as_variable_declaration()
            .name();
        Debug_.assert_node(Some(&*name), Some(is_identifier), None);
        Ok(self.factory.create_call_expression(
            self.factory.clone_node(&name),
            Option::<Gc<NodeArray>>::None,
            Some(args),
        ))
    }

    fn visit_import_equals_declaration(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_as_import_equals_declaration = node.as_import_equals_declaration();
        Debug_.assert(
            is_external_module_import_equals_declaration(node),
            Some("import= for internal module references should be handled in an earlier transformer.")
        );

        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();
        statements.get_or_insert_default_().push(
            self.factory
                .create_variable_statement(
                    Option::<Gc<NodeArray>>::None,
                    self.factory.create_variable_declaration_list(
                        vec![self.factory.create_variable_declaration(
                            Some(
                                self.factory
                                    .clone_node(&node_as_import_equals_declaration.name()),
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
                .set_text_range(Some(node))
                .set_original_node(Some(node.node_wrapper())),
        );

        self.append_exports_of_import_equals_declaration(&mut statements, node);

        Ok(statements.map(single_or_many_node))
    }

    fn append_exports_of_import_equals_declaration(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        node: Id<Node>, /*ImportEqualsDeclaration*/
    ) {
        let node_as_import_equals_declaration = node.as_import_equals_declaration();
        if has_syntactic_modifier(node, ModifierFlags::Export, self) {
            statements
                .get_or_insert_default_()
                .push(self.factory.create_export_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    node_as_import_equals_declaration.is_type_only,
                    Some(self.factory.create_named_exports(vec![
                        self.factory.create_export_specifier(
                            false,
                            Option::<Id<Node>>::None,
                            id_text(&node_as_import_equals_declaration.name()),
                        ),
                    ])),
                    None,
                    None,
                ));
        }
        // return statements;
    }

    fn visit_export_assignment(&self, node: Id<Node> /*ExportAssignment*/) -> VisitResult /*<ExportAssignment>*/
    {
        let node_as_export_assignment = node.as_export_assignment();
        (node_as_export_assignment.is_export_equals != Some(true))
            .then(|| node.node_wrapper().into())
    }

    fn visit_export_declaration(&self, node: Id<Node> /*ExportDeclaration*/) -> VisitResult {
        let node_as_export_declaration = node.as_export_declaration();
        if self
            .compiler_options
            .module
            .matches(|compiler_options_module| compiler_options_module > ModuleKind::ES2015)
        {
            return Some(node.node_wrapper().into());
        }

        let node_export_clause = node_as_export_declaration.export_clause.as_ref();
        let node_module_specifier = node_as_export_declaration.module_specifier.as_ref();
        if !node_export_clause.matches(|node_export_clause| is_namespace_export(node_export_clause))
            || node_module_specifier.is_none()
        {
            return Some(node.node_wrapper().into());
        }
        let node_export_clause = node_export_clause.unwrap();
        let node_export_clause_as_namespace_export = node_export_clause.as_namespace_export();
        let node_module_specifier = node_module_specifier.unwrap();

        let old_identifier = &node_export_clause_as_namespace_export.name;
        let synth_name = self
            .factory
            .get_generated_name_for_node(Some(&**old_identifier), None);
        let import_decl = self
            .factory
            .create_import_declaration(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                Some(self.factory.create_import_clause(
                    false,
                    None,
                    Some(self.factory.create_namespace_import(synth_name.clone())),
                )),
                node_module_specifier.clone(),
                node_as_export_declaration.assert_clause.clone(),
            )
            .set_original_node(Some(node_export_clause.clone()));

        let export_decl = if is_export_namespace_as_default_declaration(node, self) {
            self.factory.create_export_default(synth_name)
        } else {
            self.factory.create_export_declaration(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                false,
                Some(self.factory.create_named_exports(vec![
                    self.factory.create_export_specifier(
                        false,
                        Some(synth_name),
                        old_identifier.clone(),
                    ),
                ])),
                None,
                None,
            )
        }
        .set_original_node(Some(node.node_wrapper()));

        Some(vec![import_decl, export_decl].into())
    }
}

impl TransformerInterface for TransformEcmascriptModule {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        self.transform_source_file(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformEcmascriptModuleOnEmitNodeOverrider {
    transform_ecmascript_module: Gc<Box<TransformEcmascriptModule>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformEcmascriptModuleOnEmitNodeOverrider {
    fn new(
        transform_ecmascript_module: Gc<Box<TransformEcmascriptModule>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_ecmascript_module,
            previous_on_emit_node,
        }
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformEcmascriptModuleOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        if is_source_file(node) {
            if (is_external_module(node)
                || self
                    .transform_ecmascript_module
                    .compiler_options
                    .isolated_modules
                    == Some(true))
                && self
                    .transform_ecmascript_module
                    .compiler_options
                    .import_helpers
                    == Some(true)
            {
                self.transform_ecmascript_module
                    .set_helper_name_substitutions(Some(_d()));
            }
            self.previous_on_emit_node
                .on_emit_node(hint, node, emit_callback)?;
            self.transform_ecmascript_module
                .set_helper_name_substitutions(None);
        } else {
            self.previous_on_emit_node
                .on_emit_node(hint, node, emit_callback)?;
        }

        Ok(())
    }
}

#[derive(Trace, Finalize)]
struct TransformEcmascriptModuleOnSubstituteNodeOverrider {
    transform_ecmascript_module: Gc<Box<TransformEcmascriptModule>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformEcmascriptModuleOnSubstituteNodeOverrider {
    fn new(
        transform_ecmascript_module: Gc<Box<TransformEcmascriptModule>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_ecmascript_module,
            previous_on_substitute_node,
        }
    }

    fn substitute_helper_name(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> Id<Node /*Expression*/> {
        let name = id_text(node);
        let mut substitution = self
            .transform_ecmascript_module
            .helper_name_substitutions()
            .get(name)
            .cloned();
        if substitution.is_none() {
            substitution = Some(self.transform_ecmascript_module.factory.create_unique_name(
                name,
                Some(GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel),
            ));
            self.transform_ecmascript_module
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
            .on_substitute_node(hint, node)?;
        if self
            .transform_ecmascript_module
            .maybe_helper_name_substitutions()
            .is_some()
            && is_identifier(&node)
            && get_emit_flags(&node).intersects(EmitFlags::HelperName)
        {
            return Ok(self.substitute_helper_name(&node));
        }

        Ok(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformEcmascriptModuleFactory {}

impl TransformEcmascriptModuleFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformEcmascriptModuleFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformEcmascriptModule::new(context).as_transformer(),
        )
    }
}

pub fn transform_ecmascript_module() -> TransformerFactory {
    Gc::new(Box::new(TransformEcmascriptModuleFactory::new()))
}
