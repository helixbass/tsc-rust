use std::{collections::HashMap, io, mem, any::Any};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use id_arena::Id;

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
    HasArena, AllArenas, InArena, static_arena, downcast_transformer_ref,
    TransformNodesTransformationResult, CoreTransformationContext,
};

pub(super) struct DependencyGroup {
    pub name: Id<Node /*StringLiteral*/>,
    pub external_imports:
        Vec<Id<Node /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/>>,
}

#[derive(Trace, Finalize)]
pub(super) struct TransformSystemModule {
    #[unsafe_ignore_trace]
    pub(super) _arena: *const AllArenas,
    pub(super) context: Id<TransformNodesTransformationResult>,
    pub(super) factory: Id<NodeFactory>,
    pub(super) compiler_options: Id<CompilerOptions>,
    pub(super) resolver: Id<Box<dyn EmitResolver>>,
    pub(super) host: Id<Box<dyn EmitHost>>,
    pub(super) module_info_map: GcCell<HashMap<NodeId, Gc<ExternalModuleInfo>>>,
    pub(super) deferred_exports: GcCell<HashMap<NodeId, Option<Vec<Id<Node /*Statement*/>>>>>,
    pub(super) export_functions_map: GcCell<HashMap<NodeId, Id<Node /*Identifier*/>>>,
    pub(super) no_substitution_map: GcCell<HashMap<NodeId, HashMap<NodeId, bool>>>,
    pub(super) context_object_map: GcCell<HashMap<NodeId, Id<Node /*Identifier*/>>>,
    pub(super) current_source_file: GcCell<Option<Id<Node /*SourceFile*/>>>,
    pub(super) module_info: GcCell<Option<Gc<ExternalModuleInfo>>>,
    pub(super) export_function: GcCell<Option<Id<Node /*Identifier*/>>>,
    pub(super) context_object: GcCell<Option<Id<Node /*Identifier*/>>>,
    pub(super) hoisted_statements: GcCell<Option<Vec<Id<Node /*Statement*/>>>>,
    pub(super) enclosing_block_scoped_container: GcCell<Option<Id<Node>>>,
    pub(super) no_substitution: GcCell<Option<HashMap<NodeId, bool>>>,
}

impl TransformSystemModule {
    fn new(context: Id<TransformNodesTransformationResult>, arena: *const AllArenas) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        let ret = arena_ref.alloc_transformer(Box::new(Self {
            _arena: arena,
            factory: context_ref.factory(),
            compiler_options: context_ref.get_compiler_options(),
            resolver: context_ref.get_emit_resolver(),
            host: context_ref.get_emit_host(),
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
        context_ref.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformSystemModuleOnEmitNodeOverrider::new(
                ret,
                previous_on_emit_node,
            )))
        });
        context_ref.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(
                TransformSystemModuleOnSubstituteNodeOverrider::new(
                    ret,
                    previous_on_substitute_node,
                ),
            ))
        });
        context_ref.enable_substitution(SyntaxKind::Identifier);
        context_ref.enable_substitution(SyntaxKind::ShorthandPropertyAssignment);
        context_ref.enable_substitution(SyntaxKind::BinaryExpression);
        context_ref.enable_substitution(SyntaxKind::MetaProperty);
        context_ref.enable_emit_notification(SyntaxKind::SourceFile);
        ret
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
    ) -> GcCellRef<HashMap<NodeId, Option<Vec<Id<Node /*Statement*/>>>>> {
        self.deferred_exports.borrow()
    }

    pub(super) fn deferred_exports_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, Option<Vec<Id<Node /*Statement*/>>>>> {
        self.deferred_exports.borrow_mut()
    }

    pub(super) fn export_functions_map(
        &self,
    ) -> GcCellRef<HashMap<NodeId, Id<Node /*Identifier*/>>> {
        self.export_functions_map.borrow()
    }

    pub(super) fn export_functions_map_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, Id<Node /*Identifier*/>>> {
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

    pub(super) fn context_object_map(&self) -> GcCellRef<HashMap<NodeId, Id<Node /*Identifier*/>>> {
        self.context_object_map.borrow()
    }

    pub(super) fn context_object_map_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, Id<Node /*Identifier*/>>> {
        self.context_object_map.borrow_mut()
    }

    pub(super) fn current_source_file(&self) -> Id<Node /*SourceFile*/> {
        self.current_source_file.borrow().clone().unwrap()
    }

    pub(super) fn set_current_source_file(
        &self,
        current_source_file: Option<Id<Node /*SourceFile*/>>,
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

    pub(super) fn maybe_export_function(&self) -> Option<Id<Node /*Identifier*/>> {
        self.export_function.borrow().clone()
    }

    pub(super) fn export_function(&self) -> Id<Node /*Identifier*/> {
        self.export_function.borrow().clone().unwrap()
    }

    pub(super) fn set_export_function(&self, export_function: Option<Id<Node /*Identifier*/>>) {
        *self.export_function.borrow_mut() = export_function;
    }

    pub(super) fn maybe_context_object(&self) -> Option<Id<Node /*Identifier*/>> {
        self.context_object.borrow().clone()
    }

    pub(super) fn context_object(&self) -> Id<Node /*Identifier*/> {
        self.context_object.borrow().clone().unwrap()
    }

    pub(super) fn set_context_object(&self, context_object: Option<Id<Node /*Identifier*/>>) {
        *self.context_object.borrow_mut() = context_object;
    }

    pub(super) fn maybe_hoisted_statements(
        &self,
    ) -> GcCellRef<Option<Vec<Id<Node /*Statement*/>>>> {
        self.hoisted_statements.borrow()
    }

    pub(super) fn maybe_hoisted_statements_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Id<Node /*Statement*/>>>> {
        self.hoisted_statements.borrow_mut()
    }

    pub(super) fn set_hoisted_statements(
        &self,
        hoisted_statements: Option<Vec<Id<Node /*Statement*/>>>,
    ) {
        *self.hoisted_statements.borrow_mut() = hoisted_statements;
    }

    pub(super) fn maybe_enclosing_block_scoped_container(&self) -> Option<Id<Node>> {
        self.enclosing_block_scoped_container.borrow().clone()
    }

    pub(super) fn enclosing_block_scoped_container(&self) -> Id<Node> {
        self.enclosing_block_scoped_container
            .borrow()
            .clone()
            .unwrap()
    }

    pub(super) fn set_enclosing_block_scoped_container(
        &self,
        enclosing_block_scoped_container: Option<Id<Node>>,
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
        node: Id<Node>, /*SourceFile*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        if node_as_source_file.is_declaration_file()
            || !(is_effective_external_module(&node.ref_(self), &self.compiler_options.ref_(self))
                || node
                    .ref_(self).transform_flags()
                    .intersects(TransformFlags::ContainsDynamicImport))
        {
            return Ok(node);
        }

        let id = get_original_node_id(node, self);
        self.set_current_source_file(Some(node));
        self.set_enclosing_block_scoped_container(Some(node));
        let module_info = Gc::new(collect_external_module_info(
            &*self.context.ref_(self),
            node,
            &**self.resolver.ref_(self),
            &self.compiler_options.ref_(self),
            self,
        )?);
        self.module_info_map_mut().insert(id, module_info.clone());
        self.set_module_info(Some(module_info));

        self.set_export_function(Some(self.factory.ref_(self).create_unique_name("exports", None)));
        self.export_functions_map_mut()
            .insert(id, self.export_function());
        let context_object = self.factory.ref_(self).create_unique_name("context", None);
        self.context_object_map_mut()
            .insert(id, context_object.clone());
        self.set_context_object(Some(context_object));

        let dependency_groups =
            self.collect_dependency_groups(&self.module_info().external_imports)?;
        let module_body_block = self.create_system_module_body(node, &dependency_groups)?;
        let module_body_function = self.factory.ref_(self).create_function_expression(
            Option::<Gc<NodeArray>>::None,
            None,
            Option::<Id<Node>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                self.factory.ref_(self).create_parameter_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    None,
                    self.maybe_export_function(),
                    None,
                    None,
                    None,
                ),
                self.factory.ref_(self).create_parameter_declaration(
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
            &self.factory.ref_(self),
            Some(node),
            &**self.host.ref_(self),
            &self.compiler_options.ref_(self),
        );
        let dependencies = self.factory.ref_(self).create_array_literal_expression(
            Some(map(
                &dependency_groups,
                |dependency_group: &DependencyGroup, _| dependency_group.name.clone(),
            )),
            None,
        );
        let updated = self
            .factory
            .ref_(self).update_source_file(
                node,
                self.factory
                    .ref_(self).create_node_array(
                        Some(vec![self.factory.ref_(self).create_expression_statement(
                            self.factory.ref_(self).create_call_expression(
                                self.factory.ref_(self).create_property_access_expression(
                                    self.factory.ref_(self).create_identifier("System"),
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
            .set_emit_flags(EmitFlags::NoTrailingComments, self);

        if !out_file(&self.compiler_options.ref_(self)).is_non_empty() {
            move_emit_helpers(updated, module_body_block, |helper: &EmitHelper| {
                !helper.scoped()
            }, self);
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
        external_imports: &[Id<
            Node, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
        >],
    ) -> io::Result<Vec<DependencyGroup>> {
        let mut group_indices: HashMap<String, usize> = _d();
        let mut dependency_groups: Vec<DependencyGroup> = _d();
        for &external_import in external_imports {
            let external_module_name = get_external_module_name_literal(
                &self.factory.ref_(self),
                external_import,
                self.current_source_file(),
                &**self.host.ref_(self),
                &**self.resolver.ref_(self),
                &self.compiler_options.ref_(self),
            )?;
            if let Some(external_module_name) = external_module_name {
                let external_module_name_ref = external_module_name.ref_(self);
                let text = external_module_name_ref.as_string_literal().text();
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

        Ok(dependency_groups)
    }

    pub(super) fn create_system_module_body(
        &self,
        node: Id<Node>, /*SourceFile*/
        dependency_groups: &[DependencyGroup],
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        let mut statements: Vec<Id<Node /*Statement*/>> = _d();

        self.context.ref_(self).start_lexical_environment();

        let ensure_use_strict = get_strict_option_value(&self.compiler_options.ref_(self), "alwaysStrict")
            || self.compiler_options.ref_(self).no_implicit_use_strict != Some(true)
                && is_external_module(&self.current_source_file().ref_(self));
        let statement_offset = self.factory.ref_(self).try_copy_prologue(
            &node_as_source_file.statements(),
            &mut statements,
            Some(ensure_use_strict),
            Some(|node: Id<Node>| self.top_level_visitor(node)),
        )?;

        statements.push(self.factory.ref_(self).create_variable_statement(
            Option::<Gc<NodeArray>>::None,
            self.factory.ref_(self).create_variable_declaration_list(
                vec![self.factory.ref_(self).create_variable_declaration(
                        Some("__moduleName"),
                        None,
                        None,
                        Some(
                            self.factory.ref_(self).create_logical_and(
                                self.context_object(),
                                self.factory
                                    .ref_(self).create_property_access_expression(self.context_object(), "id"),
                            ),
                        ),
                    )],
                None,
            ),
        ));

        try_maybe_visit_node(
            self.module_info()
                .external_helpers_import_declaration,
            Some(|node: Id<Node>| self.top_level_visitor(node)),
            Some(|node| is_statement(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;

        let execute_statements = try_visit_nodes(
            &node_as_source_file.statements(),
            Some(|node: Id<Node>| self.top_level_visitor(node)),
            Some(|node| is_statement(node, self)),
            Some(statement_offset),
            None,
            self,
        )?;

        add_range(
            &mut statements,
            self.maybe_hoisted_statements().as_deref(),
            None,
            None,
        );

        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.ref_(self).end_lexical_environment().as_deref(),
            self,
        );

        let export_star_function = self.add_export_star_if_needed(&mut statements).unwrap();
        let modifiers = node
            .ref_(self).transform_flags()
            .intersects(TransformFlags::ContainsAwait)
            .then(|| {
                self.factory
                    .ref_(self).create_modifiers_from_modifier_flags(ModifierFlags::Async)
            });
        let module_object = self.factory.ref_(self).create_object_literal_expression(
            Some(vec![
                self.factory.ref_(self).create_property_assignment(
                    "setters",
                    self.create_setters_array(export_star_function, dependency_groups),
                ),
                self.factory.ref_(self).create_property_assignment(
                    "execute",
                    self.factory.ref_(self).create_function_expression(
                        modifiers,
                        None,
                        Option::<Id<Node>>::None,
                        Option::<Gc<NodeArray>>::None,
                        Some(vec![]),
                        None,
                        self.factory.ref_(self).create_block(execute_statements, Some(true)),
                    ),
                ),
            ]),
            Some(true),
        );

        statements.push(self.factory.ref_(self).create_return_statement(Some(module_object)));
        Ok(self.factory.ref_(self).create_block(statements, Some(true)))
    }

    pub(super) fn add_export_star_if_needed(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
    ) -> Option<Id<Node>> {
        if !self.module_info().has_export_stars_to_export_values {
            return None;
        }

        if self.module_info().exported_names.is_none()
            && self.module_info().export_specifiers.is_empty()
        {
            let mut has_export_declaration_with_export_clause = false;
            for &external_import in &self.module_info().external_imports {
                if external_import.ref_(self).kind() == SyntaxKind::ExportDeclaration
                    && external_import
                        .ref_(self).as_export_declaration()
                        .export_clause
                        .is_some()
                {
                    has_export_declaration_with_export_clause = true;
                    break;
                }
            }

            if !has_export_declaration_with_export_clause {
                let export_star_function = self.create_export_star_function(None);
                statements.push(export_star_function);
                return export_star_function.ref_(self).as_function_declaration().maybe_name();
            }
        }

        let mut exported_names: Vec<Id<Node /*ObjectLiteralElementLike*/>> = _d();
        if let Some(module_info_exported_names) = self.module_info().exported_names.as_ref() {
            for &exported_local_name in module_info_exported_names {
                if exported_local_name.ref_(self).as_identifier().escaped_text == "default" {
                    continue;
                }

                exported_names.push(
                    self.factory.ref_(self).create_property_assignment(
                        self.factory
                            .ref_(self).create_string_literal_from_node(exported_local_name),
                        self.factory.ref_(self).create_true(),
                    ),
                );
            }
        }

        let exported_names_storage_ref = self.factory.ref_(self).create_unique_name("exportedNames", None);
        statements.push(self.factory.ref_(self).create_variable_statement(
            Option::<Gc<NodeArray>>::None,
            self.factory.ref_(self).create_variable_declaration_list(
                vec![
                    self.factory.ref_(self).create_variable_declaration(
                        Some(exported_names_storage_ref.clone()),
                        None,
                        None,
                        Some(self.factory.ref_(self).create_object_literal_expression(
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
        statements.push(export_star_function);
        export_star_function.ref_(self).as_function_declaration().maybe_name()
    }

    pub(super) fn create_export_star_function(
        &self,
        local_names: Option<Id<Node /*Identifier*/>>,
    ) -> Id<Node> {
        let export_star_function = self.factory.ref_(self).create_unique_name("exportStar", None);
        let m = self.factory.ref_(self).create_identifier("m");
        let n = self.factory.ref_(self).create_identifier("n");
        let exports = self.factory.ref_(self).create_identifier("exports");
        let mut condition/*: Expression*/ = self.factory.ref_(self).create_strict_inequality(
            n.clone(),
            self.factory.ref_(self).create_string_literal("default".to_owned(), None, None, ),
        );
        if let Some(local_names) = local_names {
            condition = self.factory.ref_(self).create_logical_and(
                condition,
                self.factory.ref_(self).create_logical_not(
                    self.factory.ref_(self).create_call_expression(
                        self.factory
                            .ref_(self).create_property_access_expression(local_names, "hasOwnProperty"),
                        Option::<Gc<NodeArray>>::None,
                        Some(vec![n.clone()]),
                    ),
                ),
            );
        }

        self.factory.ref_(self).create_function_declaration(
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            None,
            Some(export_star_function),
            Option::<Gc<NodeArray>>::None,
            vec![self.factory.ref_(self).create_parameter_declaration(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                None,
                Some(m.clone()),
                None,
                None,
                None,
            )],
            None,
            Some(self.factory.ref_(self).create_block(
                vec![
                self.factory.ref_(self).create_variable_statement(
                    Option::<Gc<NodeArray>>::None,
                    self.factory.ref_(self).create_variable_declaration_list(
                        vec![
                            self.factory.ref_(self).create_variable_declaration(
                                Some(exports.clone()),
                                None, None,
                                Some(self.factory.ref_(self).create_object_literal_expression(
                                    Some(vec![]),
                                    None,
                                ))
                            ),
                        ],
                        None,
                    ),
                ),
                self.factory.ref_(self).create_for_in_statement(
                    self.factory.ref_(self).create_variable_declaration_list(
                        vec![
                            self.factory.ref_(self).create_variable_declaration(
                                Some(n.clone()),
                                None, None, None,
                            ),
                        ],
                        None,
                    ),
                    m.clone(),
                    self.factory.ref_(self).create_block(
                        vec![
                            self.factory.ref_(self).create_if_statement(
                                condition,
                                self.factory.ref_(self).create_expression_statement(
                                    self.factory.ref_(self).create_assignment(
                                        self.factory.ref_(self).create_element_access_expression(
                                            exports.clone(),
                                            n.clone()
                                        ),
                                        self.factory.ref_(self).create_element_access_expression(
                                            m.clone(),
                                            n.clone()
                                        ),

                                    ),
                                ),
                                None,
                            ).set_emit_flags(EmitFlags::SingleLine, self),
                        ],
                        None,
                    ),
                ),
                self.factory.ref_(self).create_expression_statement(
                    self.factory.ref_(self).create_call_expression(
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
        export_star_function: Id<Node /*Identifier*/>,
        dependency_groups: &[DependencyGroup],
    ) -> Id<Node> {
        let mut setters: Vec<Id<Node /*Expression*/>> = _d();
        for group in dependency_groups {
            let local_name = for_each(&group.external_imports, |&i: &Id<Node>, _| {
                get_local_name_for_external_import(&self.factory.ref_(self), i, self.current_source_file())
            });
            let parameter_name = local_name.map_or_else(
                || self.factory.ref_(self).create_unique_name("", None),
                |local_name| {
                    self.factory
                        .ref_(self).get_generated_name_for_node(Some(local_name), None)
                },
            );
            let mut statements: Vec<Id<Node /*Statement*/>> = _d();
            for &entry in &group.external_imports {
                let import_variable_name = get_local_name_for_external_import(
                    &self.factory.ref_(self),
                    entry,
                    self.current_source_file(),
                );
                match entry.ref_(self).kind() {
                    SyntaxKind::ImportDeclaration | SyntaxKind::ImportEqualsDeclaration => 'case: {
                        if entry.ref_(self).kind() == SyntaxKind::ImportDeclaration
                            && entry.ref_(self).as_import_declaration().import_clause.is_none()
                        {
                            break 'case;
                        }

                        Debug_.assert(import_variable_name.is_some(), None);
                        let import_variable_name = import_variable_name.unwrap();
                        statements.push(
                            self.factory.ref_(self).create_expression_statement(
                                self.factory.ref_(self).create_assignment(
                                    import_variable_name,
                                    parameter_name.clone(),
                                ),
                            ),
                        );
                    }
                    SyntaxKind::ExportDeclaration => {
                        Debug_.assert(import_variable_name.is_some(), None);
                        if let Some(entry_export_clause) =
                            entry.ref_(self).as_export_declaration().export_clause
                        {
                            if is_named_exports(&entry_export_clause.ref_(self)) {
                                let mut properties: Vec<Id<Node /*PropertyAssignment*/>> = _d();
                                for e in &entry_export_clause.ref_(self).as_named_exports().elements {
                                    let e_ref = e.ref_(self);
                                    let e_as_export_specifier = e_ref.as_export_specifier();
                                    properties.push(
                                        self.factory.ref_(self).create_property_assignment(
                                            self.factory.ref_(self).create_string_literal(
                                                id_text(&e_as_export_specifier.name.ref_(self)).to_owned(),
                                                None,
                                                None,
                                            ),
                                            self.factory.ref_(self).create_element_access_expression(
                                                parameter_name.clone(),
                                                self.factory.ref_(self).create_string_literal(
                                                    id_text(
                                                        &e_as_export_specifier
                                                            .property_name
                                                            .unwrap_or(e_as_export_specifier.name).ref_(self),
                                                    )
                                                    .to_owned(),
                                                    None,
                                                    None,
                                                ),
                                            ),
                                        ),
                                    );
                                }

                                statements.push(self.factory.ref_(self).create_expression_statement(
                                    self.factory.ref_(self).create_call_expression(
                                        self.export_function(),
                                        Option::<Gc<NodeArray>>::None,
                                        Some(vec![self.factory.ref_(self).create_object_literal_expression(
                                            Some(properties),
                                            Some(true),
                                        )]),
                                    ),
                                ));
                            } else {
                                statements.push(self.factory.ref_(self).create_expression_statement(
                                    self.factory.ref_(self).create_call_expression(
                                        self.export_function(),
                                        Option::<Gc<NodeArray>>::None,
                                        Some(vec![
                                                self.factory.ref_(self).create_string_literal(
                                                    id_text(
                                                        &entry_export_clause
                                                            .ref_(self).as_namespace_export()
                                                            .name.ref_(self),
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
                            statements.push(self.factory.ref_(self).create_expression_statement(
                                self.factory.ref_(self).create_call_expression(
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

            setters.push(self.factory.ref_(self).create_function_expression(
                Option::<Gc<NodeArray>>::None,
                None,
                Option::<Id<Node>>::None,
                Option::<Gc<NodeArray>>::None,
                Some(vec![self.factory.ref_(self).create_parameter_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    None,
                    Some(parameter_name),
                    None,
                    None,
                    None,
                )]),
                None,
                self.factory.ref_(self).create_block(statements, Some(true)),
            ));
        }

        self.factory
            .ref_(self).create_array_literal_expression(Some(setters), Some(true))
    }
}

impl TransformerInterface for TransformSystemModule {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        self.transform_source_file(node)
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl HasArena for TransformSystemModule {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformSystemModuleOnEmitNodeOverrider {
    transform_system_module: Transformer,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformSystemModuleOnEmitNodeOverrider {
    fn new(
        transform_system_module: Transformer,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_system_module,
            previous_on_emit_node,
        }
    }

    fn transform_system_module(&self) -> debug_cell::Ref<'_, TransformSystemModule> {
        downcast_transformer_ref(self.transform_system_module, self)
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformSystemModuleOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        if node.ref_(self).kind() == SyntaxKind::SourceFile {
            let id = get_original_node_id(node, self);
            self.transform_system_module()
                .set_current_source_file(Some(node));
            self.transform_system_module().set_module_info(
                self.transform_system_module()
                    .module_info_map()
                    .get(&id)
                    .cloned(),
            );
            self.transform_system_module().set_export_function(
                self.transform_system_module()
                    .export_functions_map()
                    .get(&id)
                    .cloned(),
            );
            self.transform_system_module().set_no_substitution(
                self.transform_system_module()
                    .no_substitution_map()
                    .get(&id)
                    .cloned(),
            );
            self.transform_system_module().set_context_object(
                self.transform_system_module()
                    .context_object_map()
                    .get(&id)
                    .cloned(),
            );

            if self
                .transform_system_module()
                .maybe_no_substitution()
                .is_some()
            {
                self.transform_system_module()
                    .no_substitution_map_mut()
                    .remove(&id);
            }

            self.previous_on_emit_node
                .on_emit_node(hint, node, emit_callback)?;

            self.transform_system_module().set_current_source_file(None);
            self.transform_system_module().set_module_info(None);
            self.transform_system_module().set_export_function(None);
            self.transform_system_module().set_context_object(None);
            self.transform_system_module().set_no_substitution(None);
        } else {
            self.previous_on_emit_node
                .on_emit_node(hint, node, emit_callback)?;
        }

        Ok(())
    }
}

impl HasArena for TransformSystemModuleOnEmitNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformSystemModuleOnSubstituteNodeOverrider {
    transform_system_module: Transformer,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformSystemModuleOnSubstituteNodeOverrider {
    fn new(
        transform_system_module: Transformer,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_system_module,
            previous_on_substitute_node,
        }
    }

    fn transform_system_module(&self) -> debug_cell::Ref<'_, TransformSystemModule> {
        downcast_transformer_ref(self.transform_system_module, self)
    }

    fn substitute_unspecified(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(match node.ref_(self).kind() {
            SyntaxKind::ShorthandPropertyAssignment => {
                self.substitute_shorthand_property_assignment(node)?
            }
            _ => node,
        })
    }

    fn substitute_shorthand_property_assignment(
        &self,
        node: Id<Node>, /*ShorthandPropertyAssignment*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_shorthand_property_assignment = node_ref.as_shorthand_property_assignment();
        let name = node_as_shorthand_property_assignment.name();
        if !is_generated_identifier(&*name.ref_(self)) && !is_local_name(&*name.ref_(self)) {
            let import_declaration = self
                .transform_system_module()
                .resolver
                .ref_(self).get_referenced_import_declaration(name)?;
            if let Some(import_declaration) = import_declaration {
                if is_import_clause(&import_declaration.ref_(self)) {
                    return Ok(self
                        .transform_system_module()
                        .factory
                        .ref_(self).create_property_assignment(
                            self.transform_system_module().factory.ref_(self).clone_node(name),
                            self.transform_system_module()
                                .factory
                                .ref_(self).create_property_access_expression(
                                    self.transform_system_module()
                                        .factory
                                        .ref_(self).get_generated_name_for_node(
                                            import_declaration.ref_(self).maybe_parent(),
                                            None,
                                        ),
                                    self.transform_system_module()
                                        .factory
                                        .ref_(self).create_identifier("default"),
                                ),
                        )
                        .set_text_range(Some(&*node.ref_(self)), self));
                } else if is_import_specifier(&import_declaration.ref_(self)) {
                    let import_declaration_ref = import_declaration.ref_(self);
                    let import_declaration_as_import_specifier = import_declaration_ref.as_import_specifier();
                    return Ok(self
                        .transform_system_module()
                        .factory
                        .ref_(self).create_property_assignment(
                            self.transform_system_module().factory.ref_(self).clone_node(name),
                            self.transform_system_module()
                                .factory
                                .ref_(self).create_property_access_expression(
                                    self.transform_system_module()
                                        .factory
                                        .ref_(self).get_generated_name_for_node(
                                            Some(
                                                import_declaration
                                                    .ref_(self).maybe_parent()
                                                    .and_then(|import_declaration_parent| {
                                                        import_declaration_parent.ref_(self).maybe_parent()
                                                    })
                                                    .and_then(|import_declaration_parent_parent| {
                                                        import_declaration_parent_parent
                                                            .ref_(self).maybe_parent()
                                                    })
                                                    .unwrap_or(import_declaration),
                                            ),
                                            None,
                                        ),
                                    self.transform_system_module().factory.ref_(self).clone_node(
                                        import_declaration_as_import_specifier
                                            .property_name
                                            .unwrap_or(
                                                import_declaration_as_import_specifier.name,
                                            ),
                                    ),
                                ),
                        )
                        .set_text_range(Some(&*node.ref_(self)), self));
                }
            }
        }
        Ok(node)
    }

    fn substitute_expression(&self, node: Id<Node> /*Expression*/) -> io::Result<Id<Node>> {
        Ok(match node.ref_(self).kind() {
            SyntaxKind::Identifier => self.substitute_expression_identifier(node)?,
            SyntaxKind::BinaryExpression => self.substitute_binary_expression(node)?,
            SyntaxKind::MetaProperty => self.substitute_meta_property(node),
            _ => node,
        })
    }

    fn substitute_expression_identifier(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        if get_emit_flags(&node.ref_(self)).intersects(EmitFlags::HelperName) {
            let external_helpers_module_name = get_external_helpers_module_name(
                self.transform_system_module().current_source_file(),
                self,
            );
            if let Some(external_helpers_module_name) = external_helpers_module_name {
                return Ok(self
                    .transform_system_module()
                    .factory
                    .ref_(self).create_property_access_expression(
                        external_helpers_module_name,
                        node,
                    ));
            }

            return Ok(node);
        }

        if !is_generated_identifier(&node.ref_(self)) && !is_local_name(&node.ref_(self)) {
            let import_declaration = self
                .transform_system_module()
                .resolver
                .ref_(self).get_referenced_import_declaration(node)?;
            if let Some(import_declaration) = import_declaration {
                if is_import_clause(&import_declaration.ref_(self)) {
                    return Ok(self
                        .transform_system_module()
                        .factory
                        .ref_(self).create_property_access_expression(
                            self.transform_system_module()
                                .factory
                                .ref_(self).get_generated_name_for_node(
                                    import_declaration.ref_(self).maybe_parent(),
                                    None,
                                ),
                            self.transform_system_module()
                                .factory
                                .ref_(self).create_identifier("default"),
                        )
                        .set_text_range(Some(&*node.ref_(self)), self));
                } else if is_import_specifier(&import_declaration.ref_(self)) {
                    let import_declaration_ref = import_declaration.ref_(self);
                    let import_declaration_as_import_specifier = import_declaration_ref.as_import_specifier();
                    return Ok(self
                        .transform_system_module()
                        .factory
                        .ref_(self).create_property_access_expression(
                            self.transform_system_module()
                                .factory
                                .ref_(self).get_generated_name_for_node(
                                    Some(
                                        import_declaration
                                            .ref_(self).maybe_parent()
                                            .and_then(|import_declaration_parent| {
                                                import_declaration_parent.ref_(self).maybe_parent()
                                            })
                                            .and_then(|import_declaration_parent_parent| {
                                                import_declaration_parent_parent.ref_(self).maybe_parent()
                                            })
                                            .unwrap_or(import_declaration),
                                    ),
                                    None,
                                ),
                            self.transform_system_module().factory.ref_(self).clone_node(
                                import_declaration_as_import_specifier
                                    .property_name
                                    .unwrap_or(import_declaration_as_import_specifier.name),
                            ),
                        )
                        .set_text_range(Some(&*node.ref_(self)), self));
                }
            }
        }

        Ok(node)
    }

    fn substitute_binary_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        let node_left = node_as_binary_expression.left;
        if is_assignment_operator(node_as_binary_expression.operator_token.ref_(self).kind())
            && is_identifier(&node_left.ref_(self))
            && !is_generated_identifier(&node_left.ref_(self))
            && !is_local_name(&node_left.ref_(self))
            && !is_declaration_name_of_enum_or_namespace(node_left, self)
        {
            let exported_names = self.transform_system_module().get_exports(node_left)?;
            if let Some(exported_names) = exported_names {
                let mut expression/*: Expression*/ = node;
                for &export_name in &exported_names {
                    expression = self.transform_system_module().create_export_expression(
                        export_name,
                        self
                            .transform_system_module()
                            .prevent_substitution(expression),
                    );
                }

                return Ok(expression);
            }
        }

        Ok(node)
    }

    fn substitute_meta_property(&self, node: Id<Node> /*MetaProperty*/) -> Id<Node> {
        if is_import_meta(node, self) {
            return self
                .transform_system_module()
                .factory
                .ref_(self).create_property_access_expression(
                    self.transform_system_module().context_object(),
                    self.transform_system_module()
                        .factory
                        .ref_(self).create_identifier("meta"),
                );
        }
        node
    }

    fn is_substitution_prevented(&self, node: Id<Node>) -> bool {
        self.transform_system_module()
            .maybe_no_substitution()
            .as_ref()
            .matches(|no_substitution| {
                node.ref_(self).maybe_id()
                    .matches(|node_id| no_substitution.get(&node_id).copied() == Some(true))
            })
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformSystemModuleOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        let node = self
            .previous_on_substitute_node
            .on_substitute_node(hint, node)?;
        if self.is_substitution_prevented(node) {
            return Ok(node);
        }

        Ok(match hint {
            EmitHint::Expression => self.substitute_expression(node)?,
            EmitHint::Unspecified => self.substitute_unspecified(node)?,
            _ => node,
        })
    }
}

impl HasArena for TransformSystemModuleOnSubstituteNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
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
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle(self).ref_(self).call(
            context.clone(),
            TransformSystemModule::new(context, &*static_arena()),
        )
    }
}

impl HasArena for TransformSystemModuleFactory {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub fn transform_system_module(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformSystemModuleFactory::new()))
}
