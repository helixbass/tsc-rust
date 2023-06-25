use std::{collections::HashMap, io, mem};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use crate::{
    chain_bundle, BaseNodeFactorySynthetic, CompilerOptions, EmitHelperBase, EmitHint, EmitHost,
    EmitResolver, ExternalModuleInfo, HasStatementsInterface, LiteralLikeNodeInterface, Node,
    NodeArrayExt, NodeExt, NodeFactory, NodeId, NodeInterface, NonEmpty, SyntaxKind,
    TransformationContext, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, _d, add_range, collect_external_module_info,
    for_each, get_emit_flags, get_external_helpers_module_name, get_external_module_name_literal,
    get_local_name_for_external_import, get_original_node_id, get_strict_option_value, id_text,
    insert_statements_after_standard_prologue, is_assignment_operator,
    is_declaration_name_of_enum_or_namespace, is_effective_external_module, is_external_module,
    is_generated_identifier, is_identifier, is_import_clause, is_import_meta, is_import_specifier,
    is_local_name, is_named_exports, is_statement, map, move_emit_helpers, out_file,
    try_get_module_name_from_file, try_maybe_visit_node, try_visit_nodes, Debug_, EmitFlags,
    EmitHelper, Matches, ModifierFlags, NamedDeclarationInterface, NodeArray, TransformFlags,
};

pub(super) struct DependencyGroup {
    pub name: Gc<Node /*StringLiteral*/>,
    pub external_imports:
        Vec<Gc<Node /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/>>,
}

#[derive(Trace, Finalize)]
pub(super) struct TransformSystemModule {
    pub(super) _transformer_wrapper: GcCell<Option<Transformer>>,
    pub(super) context: Gc<Box<dyn TransformationContext>>,
    pub(super) factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    pub(super) compiler_options: Gc<CompilerOptions>,
    pub(super) resolver: Gc<Box<dyn EmitResolver>>,
    pub(super) host: Gc<Box<dyn EmitHost>>,
    pub(super) module_info_map: GcCell<HashMap<NodeId, Gc<ExternalModuleInfo>>>,
    pub(super) deferred_exports: GcCell<HashMap<NodeId, Option<Vec<Gc<Node /*Statement*/>>>>>,
    pub(super) export_functions_map: GcCell<HashMap<NodeId, Gc<Node /*Identifier*/>>>,
    pub(super) no_substitution_map: GcCell<HashMap<NodeId, HashMap<NodeId, bool>>>,
    pub(super) context_object_map: GcCell<HashMap<NodeId, Gc<Node /*Identifier*/>>>,
    pub(super) current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    pub(super) module_info: GcCell<Option<Gc<ExternalModuleInfo>>>,
    pub(super) export_function: GcCell<Option<Gc<Node /*Identifier*/>>>,
    pub(super) context_object: GcCell<Option<Gc<Node /*Identifier*/>>>,
    pub(super) hoisted_statements: GcCell<Option<Vec<Gc<Node /*Statement*/>>>>,
    pub(super) enclosing_block_scoped_container: GcCell<Option<Gc<Node>>>,
    pub(super) no_substitution: GcCell<Option<HashMap<NodeId, bool>>>,
}

impl TransformSystemModule {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            factory: context.factory(),
            compiler_options: context.get_compiler_options(),
            resolver: context.get_emit_resolver(),
            host: context.get_emit_host(),
            context: context.clone(),
            module_info_map: _d(),
            deferred_exports: _d(),
            export_functions_map: _d(),
            no_substitution_map: _d(),
            context_object_map: _d(),
            current_source_file: _d(),
            module_info: _d(),
            export_function: _d(),
            context_object: _d(),
            hoisted_statements: _d(),
            enclosing_block_scoped_container: _d(),
            no_substitution: _d(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformSystemModuleOnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(
                TransformSystemModuleOnSubstituteNodeOverrider::new(
                    downcasted.clone(),
                    previous_on_substitute_node,
                ),
            ))
        });
        context.enable_substitution(SyntaxKind::Identifier);
        context.enable_substitution(SyntaxKind::ShorthandPropertyAssignment);
        context.enable_substitution(SyntaxKind::BinaryExpression);
        context.enable_substitution(SyntaxKind::MetaProperty);
        context.enable_emit_notification(SyntaxKind::SourceFile);
        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    pub(super) fn module_info_map(&self) -> GcCellRef<HashMap<NodeId, Gc<ExternalModuleInfo>>> {
        self.module_info_map.borrow()
    }

    pub(super) fn module_info_map_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, Gc<ExternalModuleInfo>>> {
        self.module_info_map.borrow_mut()
    }

    pub(super) fn deferred_exports(
        &self,
    ) -> GcCellRef<HashMap<NodeId, Option<Vec<Gc<Node /*Statement*/>>>>> {
        self.deferred_exports.borrow()
    }

    pub(super) fn deferred_exports_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, Option<Vec<Gc<Node /*Statement*/>>>>> {
        self.deferred_exports.borrow_mut()
    }

    pub(super) fn export_functions_map(
        &self,
    ) -> GcCellRef<HashMap<NodeId, Gc<Node /*Identifier*/>>> {
        self.export_functions_map.borrow()
    }

    pub(super) fn export_functions_map_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, Gc<Node /*Identifier*/>>> {
        self.export_functions_map.borrow_mut()
    }

    pub(super) fn no_substitution_map(&self) -> GcCellRef<HashMap<NodeId, HashMap<NodeId, bool>>> {
        self.no_substitution_map.borrow()
    }

    pub(super) fn no_substitution_map_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, HashMap<NodeId, bool>>> {
        self.no_substitution_map.borrow_mut()
    }

    pub(super) fn context_object_map(&self) -> GcCellRef<HashMap<NodeId, Gc<Node /*Identifier*/>>> {
        self.context_object_map.borrow()
    }

    pub(super) fn context_object_map_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, Gc<Node /*Identifier*/>>> {
        self.context_object_map.borrow_mut()
    }

    pub(super) fn current_source_file(&self) -> Gc<Node /*SourceFile*/> {
        self.current_source_file.borrow().clone().unwrap()
    }

    pub(super) fn set_current_source_file(
        &self,
        current_source_file: Option<Gc<Node /*SourceFile*/>>,
    ) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    pub(super) fn maybe_module_info(&self) -> Option<Gc<ExternalModuleInfo>> {
        self.module_info.borrow().clone()
    }

    pub(super) fn module_info(&self) -> Gc<ExternalModuleInfo> {
        self.module_info.borrow().clone().unwrap()
    }

    pub(super) fn set_module_info(&self, module_info: Option<Gc<ExternalModuleInfo>>) {
        *self.module_info.borrow_mut() = module_info;
    }

    pub(super) fn maybe_export_function(&self) -> Option<Gc<Node /*Identifier*/>> {
        self.export_function.borrow().clone()
    }

    pub(super) fn export_function(&self) -> Gc<Node /*Identifier*/> {
        self.export_function.borrow().clone().unwrap()
    }

    pub(super) fn set_export_function(&self, export_function: Option<Gc<Node /*Identifier*/>>) {
        *self.export_function.borrow_mut() = export_function;
    }

    pub(super) fn maybe_context_object(&self) -> Option<Gc<Node /*Identifier*/>> {
        self.context_object.borrow().clone()
    }

    pub(super) fn context_object(&self) -> Gc<Node /*Identifier*/> {
        self.context_object.borrow().clone().unwrap()
    }

    pub(super) fn set_context_object(&self, context_object: Option<Gc<Node /*Identifier*/>>) {
        *self.context_object.borrow_mut() = context_object;
    }

    pub(super) fn maybe_hoisted_statements(
        &self,
    ) -> GcCellRef<Option<Vec<Gc<Node /*Statement*/>>>> {
        self.hoisted_statements.borrow()
    }

    pub(super) fn maybe_hoisted_statements_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Node /*Statement*/>>>> {
        self.hoisted_statements.borrow_mut()
    }

    pub(super) fn set_hoisted_statements(
        &self,
        hoisted_statements: Option<Vec<Gc<Node /*Statement*/>>>,
    ) {
        *self.hoisted_statements.borrow_mut() = hoisted_statements;
    }

    pub(super) fn maybe_enclosing_block_scoped_container(&self) -> Option<Gc<Node>> {
        self.enclosing_block_scoped_container.borrow().clone()
    }

    pub(super) fn enclosing_block_scoped_container(&self) -> Gc<Node> {
        self.enclosing_block_scoped_container
            .borrow()
            .clone()
            .unwrap()
    }

    pub(super) fn set_enclosing_block_scoped_container(
        &self,
        enclosing_block_scoped_container: Option<Gc<Node>>,
    ) {
        *self.enclosing_block_scoped_container.borrow_mut() = enclosing_block_scoped_container;
    }

    pub(super) fn maybe_no_substitution(&self) -> GcCellRef<Option<HashMap<NodeId, bool>>> {
        self.no_substitution.borrow()
    }

    pub(super) fn maybe_no_substitution_mut(&self) -> GcCellRefMut<Option<HashMap<NodeId, bool>>> {
        self.no_substitution.borrow_mut()
    }

    pub(super) fn set_no_substitution(&self, no_substitution: Option<HashMap<NodeId, bool>>) {
        *self.no_substitution.borrow_mut() = no_substitution;
    }

    pub(super) fn transform_source_file(
        &self,
        node: &Node, /*SourceFile*/
    ) -> io::Result<Gc<Node>> {
        let node_as_source_file = node.as_source_file();
        if node_as_source_file.is_declaration_file()
            || !(is_effective_external_module(node, &self.compiler_options)
                || node
                    .transform_flags()
                    .intersects(TransformFlags::ContainsDynamicImport))
        {
            return Ok(node.node_wrapper());
        }

        let id = get_original_node_id(node);
        self.set_current_source_file(Some(node.node_wrapper()));
        self.set_enclosing_block_scoped_container(Some(node.node_wrapper()));
        let module_info = Gc::new(collect_external_module_info(
            &**self.context,
            node,
            &**self.resolver,
            &self.compiler_options,
        ));
        self.module_info_map_mut().insert(id, module_info.clone());
        self.set_module_info(Some(module_info));

        self.set_export_function(Some(self.factory.create_unique_name("exports", None)));
        self.export_functions_map_mut()
            .insert(id, self.export_function());
        let context_object = self.factory.create_unique_name("context", None);
        self.context_object_map_mut()
            .insert(id, context_object.clone());
        self.set_context_object(Some(context_object));

        let dependency_groups =
            self.collect_dependency_groups(&self.module_info().external_imports);
        let module_body_block = self.create_system_module_body(node, &dependency_groups)?;
        let module_body_function = self.factory.create_function_expression(
            Option::<Gc<NodeArray>>::None,
            None,
            Option::<Gc<Node>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                self.factory.create_parameter_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    None,
                    self.maybe_export_function(),
                    None,
                    None,
                    None,
                ),
                self.factory.create_parameter_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    None,
                    self.maybe_context_object(),
                    None,
                    None,
                    None,
                ),
            ]),
            None,
            module_body_block.clone(),
        );

        let module_name = try_get_module_name_from_file(
            &self.factory,
            Some(node),
            &**self.host,
            &self.compiler_options,
        );
        let dependencies = self.factory.create_array_literal_expression(
            Some(map(
                &dependency_groups,
                |dependency_group: &DependencyGroup, _| dependency_group.name.clone(),
            )),
            None,
        );
        let updated = self
            .factory
            .update_source_file(
                node,
                self.factory
                    .create_node_array(
                        Some(vec![self.factory.create_expression_statement(
                            self.factory.create_call_expression(
                                self.factory.create_property_access_expression(
                                    self.factory.create_identifier("System"),
                                    "register",
                                ),
                                Option::<Gc<NodeArray>>::None,
                                Some(module_name.map_or_else(
                                    || vec![dependencies.clone(), module_body_function.clone()],
                                    |module_name| {
                                        vec![
                                            module_name,
                                            dependencies.clone(),
                                            module_body_function.clone(),
                                        ]
                                    },
                                )),
                            ),
                        )]),
                        None,
                    )
                    .set_text_range(Some(&*node_as_source_file.statements())),
                None,
                None,
                None,
                None,
                None,
            )
            .set_emit_flags(EmitFlags::NoTrailingComments);

        if !out_file(&self.compiler_options).is_non_empty() {
            move_emit_helpers(&updated, &module_body_block, |helper: &EmitHelper| {
                !helper.scoped()
            });
        }

        if let Some(no_substitution) = self.maybe_no_substitution().clone() {
            self.no_substitution_map_mut().insert(id, no_substitution);
            self.set_no_substitution(None);
        }

        self.set_current_source_file(None);
        self.set_module_info(None);
        self.set_export_function(None);
        self.set_context_object(None);
        self.set_hoisted_statements(None);
        self.set_enclosing_block_scoped_container(None);
        Ok(updated)
    }

    pub(super) fn collect_dependency_groups(
        &self,
        external_imports: &[Gc<
            Node, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
        >],
    ) -> Vec<DependencyGroup> {
        let mut group_indices: HashMap<String, usize> = _d();
        let mut dependency_groups: Vec<DependencyGroup> = _d();
        for external_import in external_imports {
            let external_module_name = get_external_module_name_literal(
                &self.factory,
                external_import,
                &self.current_source_file(),
                &**self.host,
                &**self.resolver,
                &self.compiler_options,
            );
            if let Some(external_module_name) = external_module_name {
                let text = external_module_name.as_string_literal().text();
                let group_index = group_indices.get(&*text).copied();
                if let Some(group_index) = group_index {
                    dependency_groups[group_index]
                        .external_imports
                        .push(external_import.clone());
                } else {
                    group_indices.insert(text.clone(), dependency_groups.len());
                    dependency_groups.push(DependencyGroup {
                        name: external_module_name.clone(),
                        external_imports: vec![external_import.clone()],
                    });
                }
            }
        }

        dependency_groups
    }

    pub(super) fn create_system_module_body(
        &self,
        node: &Node, /*SourceFile*/
        dependency_groups: &[DependencyGroup],
    ) -> io::Result<Gc<Node>> {
        let node_as_source_file = node.as_source_file();
        let mut statements: Vec<Gc<Node /*Statement*/>> = _d();

        self.context.start_lexical_environment();

        let ensure_use_strict = get_strict_option_value(&self.compiler_options, "alwaysStrict")
            || self.compiler_options.no_implicit_use_strict != Some(true)
                && is_external_module(&self.current_source_file());
        let statement_offset = self.factory.try_copy_prologue(
            &node_as_source_file.statements(),
            &mut statements,
            Some(ensure_use_strict),
            Some(|node: &Node| self.top_level_visitor(node)),
        )?;

        statements.push(self.factory.create_variable_statement(
            Option::<Gc<NodeArray>>::None,
            self.factory.create_variable_declaration_list(
                vec![self.factory.create_variable_declaration(
                        Some("__moduleName"),
                        None,
                        None,
                        Some(
                            self.factory.create_logical_and(
                                self.context_object(),
                                self.factory
                                    .create_property_access_expression(self.context_object(), "id"),
                            ),
                        ),
                    )],
                None,
            ),
        ));

        try_maybe_visit_node(
            self.module_info()
                .external_helpers_import_declaration
                .as_deref(),
            Some(|node: &Node| self.top_level_visitor(node)),
            Some(is_statement),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?;

        let execute_statements = try_visit_nodes(
            &node_as_source_file.statements(),
            Some(|node: &Node| self.top_level_visitor(node)),
            Some(is_statement),
            Some(statement_offset),
            None,
        )?;

        add_range(
            &mut statements,
            self.maybe_hoisted_statements().as_deref(),
            None,
            None,
        );

        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.end_lexical_environment().as_deref(),
        );

        let export_star_function = self.add_export_star_if_needed(&mut statements).unwrap();
        let modifiers = node
            .transform_flags()
            .intersects(TransformFlags::ContainsAwait)
            .then(|| {
                self.factory
                    .create_modifiers_from_modifier_flags(ModifierFlags::Async)
            });
        let module_object = self.factory.create_object_literal_expression(
            Some(vec![
                self.factory.create_property_assignment(
                    "setters",
                    self.create_setters_array(export_star_function, dependency_groups),
                ),
                self.factory.create_property_assignment(
                    "execute",
                    self.factory.create_function_expression(
                        modifiers,
                        None,
                        Option::<Gc<Node>>::None,
                        Option::<Gc<NodeArray>>::None,
                        Some(vec![]),
                        None,
                        self.factory.create_block(execute_statements, Some(true)),
                    ),
                ),
            ]),
            Some(true),
        );

        statements.push(self.factory.create_return_statement(Some(module_object)));
        Ok(self.factory.create_block(statements, Some(true)))
    }

    pub(super) fn add_export_star_if_needed(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
    ) -> Option<Gc<Node>> {
        if !self.module_info().has_export_stars_to_export_values {
            return None;
        }

        if self.module_info().exported_names.is_none()
            && self.module_info().export_specifiers.is_empty()
        {
            let mut has_export_declaration_with_export_clause = false;
            for external_import in &self.module_info().external_imports {
                if external_import.kind() == SyntaxKind::ExportDeclaration
                    && external_import
                        .as_export_declaration()
                        .export_clause
                        .is_some()
                {
                    has_export_declaration_with_export_clause = true;
                    break;
                }
            }

            if !has_export_declaration_with_export_clause {
                let export_star_function = self.create_export_star_function(None);
                statements.push(export_star_function.clone());
                return export_star_function.as_function_declaration().maybe_name();
            }
        }

        let mut exported_names: Vec<Gc<Node /*ObjectLiteralElementLike*/>> = _d();
        if let Some(module_info_exported_names) = self.module_info().exported_names.as_ref() {
            for exported_local_name in module_info_exported_names {
                if exported_local_name.as_identifier().escaped_text == "default" {
                    continue;
                }

                exported_names.push(
                    self.factory.create_property_assignment(
                        self.factory
                            .create_string_literal_from_node(exported_local_name),
                        self.factory.create_true(),
                    ),
                );
            }
        }

        let exported_names_storage_ref = self.factory.create_unique_name("exportedNames", None);
        statements.push(self.factory.create_variable_statement(
            Option::<Gc<NodeArray>>::None,
            self.factory.create_variable_declaration_list(
                vec![
                    self.factory.create_variable_declaration(
                        Some(exported_names_storage_ref.clone()),
                        None,
                        None,
                        Some(self.factory.create_object_literal_expression(
                            Some(exported_names),
                            Some(true)
                        ))
                    )
                ],
                None,
            ),
        ));

        let export_star_function =
            self.create_export_star_function(Some(exported_names_storage_ref));
        statements.push(export_star_function.clone());
        export_star_function.as_function_declaration().maybe_name()
    }

    pub(super) fn create_export_star_function(
        &self,
        local_names: Option<Gc<Node /*Identifier*/>>,
    ) -> Gc<Node> {
        let export_star_function = self.factory.create_unique_name("exportStar", None);
        let m = self.factory.create_identifier("m");
        let n = self.factory.create_identifier("n");
        let exports = self.factory.create_identifier("exports");
        let mut condition/*: Expression*/ = self.factory.create_strict_inequality(
            n.clone(),
            self.factory.create_string_literal("default".to_owned(), None, None, ),
        );
        if let Some(local_names) = local_names {
            condition = self.factory.create_logical_and(
                condition,
                self.factory.create_logical_not(
                    self.factory.create_call_expression(
                        self.factory
                            .create_property_access_expression(local_names, "hasOwnProperty"),
                        Option::<Gc<NodeArray>>::None,
                        Some(vec![n.clone()]),
                    ),
                ),
            );
        }

        self.factory.create_function_declaration(
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            None,
            Some(export_star_function),
            Option::<Gc<NodeArray>>::None,
            vec![self.factory.create_parameter_declaration(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                None,
                Some(m.clone()),
                None,
                None,
                None,
            )],
            None,
            Some(self.factory.create_block(
                vec![
                self.factory.create_variable_statement(
                    Option::<Gc<NodeArray>>::None,
                    self.factory.create_variable_declaration_list(
                        vec![
                            self.factory.create_variable_declaration(
                                Some(exports.clone()),
                                None, None,
                                Some(self.factory.create_object_literal_expression(
                                    Some(vec![]),
                                    None,
                                ))
                            ),
                        ],
                        None,
                    ),
                ),
                self.factory.create_for_in_statement(
                    self.factory.create_variable_declaration_list(
                        vec![
                            self.factory.create_variable_declaration(
                                Some(n.clone()),
                                None, None, None,
                            ),
                        ],
                        None,
                    ),
                    m.clone(),
                    self.factory.create_block(
                        vec![
                            self.factory.create_if_statement(
                                condition,
                                self.factory.create_expression_statement(
                                    self.factory.create_assignment(
                                        self.factory.create_element_access_expression(
                                            exports.clone(),
                                            n.clone()
                                        ),
                                        self.factory.create_element_access_expression(
                                            m.clone(),
                                            n.clone()
                                        ),

                                    ),
                                ),
                                None,
                            ).set_emit_flags(EmitFlags::SingleLine),
                        ],
                        None,
                    ),
                ),
                self.factory.create_expression_statement(
                    self.factory.create_call_expression(
                        self.export_function(),
                        Option::<Gc<NodeArray>>::None,
                        Some(vec![exports]))
                ),
            ],
                Some(true),
            )),
        )
    }

    pub(super) fn create_setters_array(
        &self,
        export_star_function: Gc<Node /*Identifier*/>,
        dependency_groups: &[DependencyGroup],
    ) -> Gc<Node> {
        let mut setters: Vec<Gc<Node /*Expression*/>> = _d();
        for group in dependency_groups {
            let local_name = for_each(&group.external_imports, |i: &Gc<Node>, _| {
                get_local_name_for_external_import(&self.factory, i, &self.current_source_file())
            });
            let parameter_name = local_name.map_or_else(
                || self.factory.create_unique_name("", None),
                |local_name| {
                    self.factory
                        .get_generated_name_for_node(Some(local_name), None)
                },
            );
            let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
            for entry in &group.external_imports {
                let import_variable_name = get_local_name_for_external_import(
                    &self.factory,
                    entry,
                    &self.current_source_file(),
                );
                match entry.kind() {
                    SyntaxKind::ImportDeclaration | SyntaxKind::ImportEqualsDeclaration => 'case: {
                        if entry.kind() == SyntaxKind::ImportDeclaration
                            && entry.as_import_declaration().import_clause.is_none()
                        {
                            break 'case;
                        }

                        Debug_.assert(import_variable_name.is_some(), None);
                        let import_variable_name = import_variable_name.unwrap();
                        statements.push(
                            self.factory.create_expression_statement(
                                self.factory.create_assignment(
                                    import_variable_name,
                                    parameter_name.clone(),
                                ),
                            ),
                        );
                    }
                    SyntaxKind::ExportDeclaration => {
                        Debug_.assert(import_variable_name.is_some(), None);
                        if let Some(entry_export_clause) =
                            entry.as_export_declaration().export_clause.as_ref()
                        {
                            if is_named_exports(entry_export_clause) {
                                let mut properties: Vec<Gc<Node /*PropertyAssignment*/>> = _d();
                                for e in &entry_export_clause.as_named_exports().elements {
                                    let e_as_export_specifier = e.as_export_specifier();
                                    properties.push(
                                        self.factory.create_property_assignment(
                                            self.factory.create_string_literal(
                                                id_text(&e_as_export_specifier.name).to_owned(),
                                                None,
                                                None,
                                            ),
                                            self.factory.create_element_access_expression(
                                                parameter_name.clone(),
                                                self.factory.create_string_literal(
                                                    id_text(
                                                        e_as_export_specifier
                                                            .property_name
                                                            .as_ref()
                                                            .unwrap_or(&e_as_export_specifier.name),
                                                    )
                                                    .to_owned(),
                                                    None,
                                                    None,
                                                ),
                                            ),
                                        ),
                                    );
                                }

                                statements.push(self.factory.create_expression_statement(
                                    self.factory.create_call_expression(
                                        self.export_function(),
                                        Option::<Gc<NodeArray>>::None,
                                        Some(vec![self.factory.create_object_literal_expression(
                                            Some(properties),
                                            Some(true),
                                        )]),
                                    ),
                                ));
                            } else {
                                statements.push(self.factory.create_expression_statement(
                                    self.factory.create_call_expression(
                                        self.export_function(),
                                        Option::<Gc<NodeArray>>::None,
                                        Some(vec![
                                                self.factory.create_string_literal(
                                                    id_text(
                                                        &entry_export_clause
                                                            .as_namespace_export()
                                                            .name,
                                                    )
                                                    .to_owned(),
                                                    None,
                                                    None,
                                                ),
                                                parameter_name.clone(),
                                            ]),
                                    ),
                                ));
                            }
                        } else {
                            statements.push(self.factory.create_expression_statement(
                                self.factory.create_call_expression(
                                    export_star_function.clone(),
                                    Option::<Gc<NodeArray>>::None,
                                    Some(vec![parameter_name.clone()]),
                                ),
                            ));
                        }
                    }
                    _ => (),
                }
            }

            setters.push(self.factory.create_function_expression(
                Option::<Gc<NodeArray>>::None,
                None,
                Option::<Gc<Node>>::None,
                Option::<Gc<NodeArray>>::None,
                Some(vec![self.factory.create_parameter_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    None,
                    Some(parameter_name),
                    None,
                    None,
                    None,
                )]),
                None,
                self.factory.create_block(statements, Some(true)),
            ));
        }

        self.factory
            .create_array_literal_expression(Some(setters), Some(true))
    }
}

impl TransformerInterface for TransformSystemModule {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        self.transform_source_file(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformSystemModuleOnEmitNodeOverrider {
    transform_system_module: Gc<Box<TransformSystemModule>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformSystemModuleOnEmitNodeOverrider {
    fn new(
        transform_system_module: Gc<Box<TransformSystemModule>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_system_module,
            previous_on_emit_node,
        }
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformSystemModuleOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        if node.kind() == SyntaxKind::SourceFile {
            let id = get_original_node_id(node);
            self.transform_system_module
                .set_current_source_file(Some(node.node_wrapper()));
            self.transform_system_module.set_module_info(
                self.transform_system_module
                    .module_info_map()
                    .get(&id)
                    .cloned(),
            );
            self.transform_system_module.set_export_function(
                self.transform_system_module
                    .export_functions_map()
                    .get(&id)
                    .cloned(),
            );
            self.transform_system_module.set_no_substitution(
                self.transform_system_module
                    .no_substitution_map()
                    .get(&id)
                    .cloned(),
            );
            self.transform_system_module.set_context_object(
                self.transform_system_module
                    .context_object_map()
                    .get(&id)
                    .cloned(),
            );

            if self
                .transform_system_module
                .maybe_no_substitution()
                .is_some()
            {
                self.transform_system_module
                    .no_substitution_map_mut()
                    .remove(&id);
            }

            self.previous_on_emit_node
                .on_emit_node(hint, node, emit_callback)?;

            self.transform_system_module.set_current_source_file(None);
            self.transform_system_module.set_module_info(None);
            self.transform_system_module.set_export_function(None);
            self.transform_system_module.set_context_object(None);
            self.transform_system_module.set_no_substitution(None);
        } else {
            self.previous_on_emit_node
                .on_emit_node(hint, node, emit_callback)?;
        }

        Ok(())
    }
}

#[derive(Trace, Finalize)]
struct TransformSystemModuleOnSubstituteNodeOverrider {
    transform_system_module: Gc<Box<TransformSystemModule>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformSystemModuleOnSubstituteNodeOverrider {
    fn new(
        transform_system_module: Gc<Box<TransformSystemModule>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_system_module,
            previous_on_substitute_node,
        }
    }

    fn substitute_unspecified(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(match node.kind() {
            SyntaxKind::ShorthandPropertyAssignment => {
                self.substitute_shorthand_property_assignment(node)?
            }
            _ => node.node_wrapper(),
        })
    }

    fn substitute_shorthand_property_assignment(
        &self,
        node: &Node, /*ShorthandPropertyAssignment*/
    ) -> io::Result<Gc<Node>> {
        let node_as_shorthand_property_assignment = node.as_shorthand_property_assignment();
        let name = &node_as_shorthand_property_assignment.name();
        if !is_generated_identifier(name) && !is_local_name(name) {
            let import_declaration = self
                .transform_system_module
                .resolver
                .get_referenced_import_declaration(name)?;
            if let Some(ref import_declaration) = import_declaration {
                if is_import_clause(import_declaration) {
                    return Ok(self
                        .transform_system_module
                        .factory
                        .create_property_assignment(
                            self.transform_system_module.factory.clone_node(name),
                            self.transform_system_module
                                .factory
                                .create_property_access_expression(
                                    self.transform_system_module
                                        .factory
                                        .get_generated_name_for_node(
                                            import_declaration.maybe_parent(),
                                            None,
                                        ),
                                    self.transform_system_module
                                        .factory
                                        .create_identifier("default"),
                                ),
                        )
                        .set_text_range(Some(node)));
                } else if is_import_specifier(import_declaration) {
                    let import_declaration_as_import_specifier =
                        import_declaration.as_import_specifier();
                    return Ok(self
                        .transform_system_module
                        .factory
                        .create_property_assignment(
                            self.transform_system_module.factory.clone_node(name),
                            self.transform_system_module
                                .factory
                                .create_property_access_expression(
                                    self.transform_system_module
                                        .factory
                                        .get_generated_name_for_node(
                                            Some(
                                                import_declaration
                                                    .maybe_parent()
                                                    .and_then(|import_declaration_parent| {
                                                        import_declaration_parent.maybe_parent()
                                                    })
                                                    .and_then(|import_declaration_parent_parent| {
                                                        import_declaration_parent_parent
                                                            .maybe_parent()
                                                    })
                                                    .unwrap_or_else(|| import_declaration.clone()),
                                            ),
                                            None,
                                        ),
                                    self.transform_system_module.factory.clone_node(
                                        import_declaration_as_import_specifier
                                            .property_name
                                            .as_ref()
                                            .unwrap_or(
                                                &import_declaration_as_import_specifier.name,
                                            ),
                                    ),
                                ),
                        )
                        .set_text_range(Some(node)));
                }
            }
        }
        Ok(node.node_wrapper())
    }

    fn substitute_expression(&self, node: &Node /*Expression*/) -> io::Result<Gc<Node>> {
        Ok(match node.kind() {
            SyntaxKind::Identifier => self.substitute_expression_identifier(node)?,
            SyntaxKind::BinaryExpression => self.substitute_binary_expression(node)?,
            SyntaxKind::MetaProperty => self.substitute_meta_property(node),
            _ => node.node_wrapper(),
        })
    }

    fn substitute_expression_identifier(
        &self,
        node: &Node, /*Identifier*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        if get_emit_flags(node).intersects(EmitFlags::HelperName) {
            let external_helpers_module_name = get_external_helpers_module_name(
                &self.transform_system_module.current_source_file(),
            );
            if let Some(external_helpers_module_name) = external_helpers_module_name {
                return Ok(self
                    .transform_system_module
                    .factory
                    .create_property_access_expression(
                        external_helpers_module_name,
                        node.node_wrapper(),
                    ));
            }

            return Ok(node.node_wrapper());
        }

        if !is_generated_identifier(node) && !is_local_name(node) {
            let import_declaration = self
                .transform_system_module
                .resolver
                .get_referenced_import_declaration(node)?;
            if let Some(ref import_declaration) = import_declaration {
                if is_import_clause(import_declaration) {
                    return Ok(self
                        .transform_system_module
                        .factory
                        .create_property_access_expression(
                            self.transform_system_module
                                .factory
                                .get_generated_name_for_node(
                                    import_declaration.maybe_parent(),
                                    None,
                                ),
                            self.transform_system_module
                                .factory
                                .create_identifier("default"),
                        )
                        .set_text_range(Some(node)));
                } else if is_import_specifier(import_declaration) {
                    let import_declaration_as_import_specifier =
                        import_declaration.as_import_specifier();
                    return Ok(self
                        .transform_system_module
                        .factory
                        .create_property_access_expression(
                            self.transform_system_module
                                .factory
                                .get_generated_name_for_node(
                                    Some(
                                        import_declaration
                                            .maybe_parent()
                                            .and_then(|import_declaration_parent| {
                                                import_declaration_parent.maybe_parent()
                                            })
                                            .and_then(|import_declaration_parent_parent| {
                                                import_declaration_parent_parent.maybe_parent()
                                            })
                                            .unwrap_or_else(|| import_declaration.clone()),
                                    ),
                                    None,
                                ),
                            self.transform_system_module.factory.clone_node(
                                import_declaration_as_import_specifier
                                    .property_name
                                    .as_ref()
                                    .unwrap_or(&import_declaration_as_import_specifier.name),
                            ),
                        )
                        .set_text_range(Some(node)));
                }
            }
        }

        Ok(node.node_wrapper())
    }

    fn substitute_binary_expression(
        &self,
        node: &Node, /*BinaryExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let node_as_binary_expression = node.as_binary_expression();
        let node_left = &node_as_binary_expression.left;
        if is_assignment_operator(node_as_binary_expression.operator_token.kind())
            && is_identifier(node_left)
            && !is_generated_identifier(node_left)
            && !is_local_name(node_left)
            && !is_declaration_name_of_enum_or_namespace(node_left)
        {
            let exported_names = self.transform_system_module.get_exports(node_left)?;
            if let Some(exported_names) = exported_names {
                let mut expression/*: Expression*/ = node.node_wrapper();
                for export_name in &exported_names {
                    expression = self.transform_system_module.create_export_expression(
                        export_name,
                        &self
                            .transform_system_module
                            .prevent_substitution(expression),
                    );
                }

                return Ok(expression);
            }
        }

        Ok(node.node_wrapper())
    }

    fn substitute_meta_property(&self, node: &Node /*MetaProperty*/) -> Gc<Node> {
        if is_import_meta(node) {
            return self
                .transform_system_module
                .factory
                .create_property_access_expression(
                    self.transform_system_module.context_object(),
                    self.transform_system_module
                        .factory
                        .create_identifier("meta"),
                );
        }
        node.node_wrapper()
    }

    fn is_substitution_prevented(&self, node: &Node) -> bool {
        self.transform_system_module
            .maybe_no_substitution()
            .as_ref()
            .matches(|no_substitution| {
                node.maybe_id()
                    .matches(|node_id| no_substitution.get(&node_id).copied() == Some(true))
            })
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformSystemModuleOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> io::Result<Gc<Node>> {
        let node = self
            .previous_on_substitute_node
            .on_substitute_node(hint, node)?;
        if self.is_substitution_prevented(&node) {
            return Ok(node);
        }

        Ok(match hint {
            EmitHint::Expression => self.substitute_expression(&node)?,
            EmitHint::Unspecified => self.substitute_unspecified(&node)?,
            _ => node,
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformSystemModuleFactory {}

impl TransformSystemModuleFactory {
    pub(super) fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformSystemModuleFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformSystemModule::new(context).as_transformer(),
        )
    }
}

pub fn transform_system_module() -> TransformerFactory {
    Gc::new(Box::new(TransformSystemModuleFactory::new()))
}
