use std::{cell::Cell, cmp, collections::HashMap, io, mem, any::Any};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use id_arena::Id;

use super::dynamic_import_umd_helper;
use crate::{
    BaseNodeFactorySynthetic, CompilerOptions, EmitResolver, ExternalModuleInfo, ModuleKind, Node,
    NodeFactory, NodeId, ScriptTarget, TransformationContext, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, _d, add_emit_helper, add_range, append,
    chain_bundle, collect_external_module_info, get_emit_flags, get_emit_module_kind,
    get_emit_script_target, get_external_helpers_module_name, get_external_module_name_literal,
    get_local_name_for_external_import, get_node_id, get_original_node_id, get_strict_option_value,
    has_json_module_emit_enabled, id_text, insert_statements_after_standard_prologue,
    is_assignment_operator, is_declaration_name_of_enum_or_namespace, is_effective_external_module,
    is_export_declaration, is_export_name, is_external_module, is_generated_identifier,
    is_identifier, is_import_clause, is_import_equals_declaration, is_import_specifier,
    is_json_source_file, is_local_name, is_shorthand_property_assignment, is_statement, out_file,
    reduce_left, set_emit_flags, try_get_module_name_from_file, try_map_defined,
    try_maybe_visit_node, try_visit_node, try_visit_nodes, EmitFlags, EmitHelperFactory, EmitHint,
    EmitHost, GeneratedIdentifierFlags, HasStatementsInterface, Matches, NamedDeclarationInterface,
    NodeArray, NodeArrayExt, NodeExt, NodeInterface, NonEmpty, SyntaxKind, TransformFlags,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    VecExt,
    HasArena, AllArenas, InArena, static_arena, downcast_transformer_ref,
    TransformNodesTransformationResult, CoreTransformationContext,
};

pub(super) struct AsynchronousDependencies {
    pub aliased_module_names: Vec<Id<Node /*Expression*/>>,
    pub unaliased_module_names: Vec<Id<Node /*Expression*/>>,
    pub import_alias_names: Vec<Id<Node /*ParameterDeclaration*/>>,
}

#[derive(Trace, Finalize)]
pub(super) struct TransformModule {
    #[unsafe_ignore_trace]
    pub(super) _arena: *const AllArenas,
    pub(super) context: Id<TransformNodesTransformationResult>,
    pub(super) factory: Id<NodeFactory>,
    pub(super) resolver: Id<Box<dyn EmitResolver>>,
    pub(super) host: Id<Box<dyn EmitHost>>,
    pub(super) compiler_options: Id<CompilerOptions>,
    #[unsafe_ignore_trace]
    pub(super) language_version: ScriptTarget,
    #[unsafe_ignore_trace]
    pub(super) module_kind: ModuleKind,
    pub(super) module_info_map: GcCell<HashMap<NodeId, Gc<ExternalModuleInfo>>>,
    pub(super) deferred_exports: GcCell<HashMap<NodeId, Option<Vec<Id<Node /*Statement*/>>>>>,
    pub(super) current_source_file: GcCell<Option<Id<Node /*SourceFile*/>>>,
    pub(super) current_module_info: GcCell<Option<Gc<ExternalModuleInfo>>>,
    pub(super) no_substitution: GcCell<HashMap<NodeId, bool>>,
    #[unsafe_ignore_trace]
    pub(super) need_umd_dynamic_import_helper: Cell<bool>,
}

impl TransformModule {
    pub(super) fn new(context: Id<TransformNodesTransformationResult>, arena: *const AllArenas) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        let compiler_options = context_ref.get_compiler_options();
        let ret = arena_ref.alloc_transformer(Box::new(Self {
            _arena: arena,
            factory: context_ref.factory(),
            resolver: context_ref.get_emit_resolver(),
            host: context_ref.get_emit_host(),
            language_version: get_emit_script_target(&compiler_options.ref_(arena_ref)),
            module_kind: get_emit_module_kind(&compiler_options.ref_(arena_ref)),
            compiler_options,
            context: context.clone(),
            module_info_map: _d(),
            deferred_exports: _d(),
            current_source_file: _d(),
            current_module_info: _d(),
            no_substitution: _d(),
            need_umd_dynamic_import_helper: _d(),
        }));
        context_ref.override_on_emit_node(&mut |previous_on_emit_node| {
            arena_ref.alloc_transformation_context_on_emit_node_overrider(Box::new(TransformModuleOnEmitNodeOverrider::new(
                ret,
                previous_on_emit_node,
            )))
        });
        context_ref.override_on_substitute_node(&mut |previous_on_substitute_node| {
            arena_ref.alloc_transformation_context_on_substitute_node_overrider(Box::new(TransformModuleOnSubstituteNodeOverrider::new(
                ret,
                previous_on_substitute_node,
            )))
        });
        context_ref.enable_substitution(SyntaxKind::CallExpression);
        context_ref.enable_substitution(SyntaxKind::TaggedTemplateExpression);
        context_ref.enable_substitution(SyntaxKind::Identifier);
        context_ref.enable_substitution(SyntaxKind::BinaryExpression);
        context_ref.enable_substitution(SyntaxKind::ShorthandPropertyAssignment);
        context_ref.enable_emit_notification(SyntaxKind::SourceFile);
        ret
    }

    pub(super) fn emit_helpers(&self) -> debug_cell::Ref<'_, EmitHelperFactory> {
        self.context.ref_(self).get_emit_helper_factory().ref_(self)
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

    pub(super) fn current_source_file(&self) -> Id<Node /*SourceFile*/> {
        self.current_source_file.borrow().clone().unwrap()
    }

    pub(super) fn set_current_source_file(
        &self,
        current_source_file: Option<Id<Node /*SourceFile*/>>,
    ) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    pub(super) fn maybe_current_module_info(&self) -> Option<Gc<ExternalModuleInfo>> {
        self.current_module_info.borrow().clone()
    }

    pub(super) fn current_module_info(&self) -> Gc<ExternalModuleInfo> {
        self.current_module_info.borrow().clone().unwrap()
    }

    pub(super) fn set_current_module_info(
        &self,
        current_module_info: Option<Gc<ExternalModuleInfo>>,
    ) {
        *self.current_module_info.borrow_mut() = current_module_info;
    }

    pub(super) fn no_substitution(&self) -> GcCellRef<HashMap<NodeId, bool>> {
        self.no_substitution.borrow()
    }

    pub(super) fn no_substitution_mut(&self) -> GcCellRefMut<HashMap<NodeId, bool>> {
        self.no_substitution.borrow_mut()
    }

    pub(super) fn need_umd_dynamic_import_helper(&self) -> bool {
        self.need_umd_dynamic_import_helper.get()
    }

    pub(super) fn set_need_umd_dynamic_import_helper(&self, need_umd_dynamic_import_helper: bool) {
        self.need_umd_dynamic_import_helper
            .set(need_umd_dynamic_import_helper);
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
                    .intersects(TransformFlags::ContainsDynamicImport)
                || is_json_source_file(&node.ref_(self))
                    && has_json_module_emit_enabled(&self.compiler_options.ref_(self))
                    && out_file(&self.compiler_options.ref_(self)).is_non_empty())
        {
            return Ok(node);
        }

        self.set_current_source_file(Some(node));
        self.set_current_module_info(Some(Gc::new(collect_external_module_info(
            &*self.context.ref_(self),
            node,
            &**self.resolver.ref_(self),
            &self.compiler_options.ref_(self),
            self,
        )?)));
        self.module_info_map_mut()
            .insert(get_original_node_id(node, self), self.current_module_info());

        let updated = match self.module_kind {
            ModuleKind::AMD => self.transform_amd_module(node)?,
            ModuleKind::UMD => self.transform_umd_module(node)?,
            _ => self.transform_common_js_module(node)?,
        };
        self.set_current_source_file(None);
        self.set_current_module_info(None);
        self.set_need_umd_dynamic_import_helper(false);
        Ok(updated)
    }

    pub(super) fn should_emit_underscore_underscore_es_module(&self) -> bool {
        if self.current_module_info().export_equals.is_none()
            && is_external_module(&self.current_source_file().ref_(self))
        {
            return true;
        }
        false
    }

    pub(super) fn transform_common_js_module(
        &self,
        node: Id<Node>, /*SourceFile*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        self.context.ref_(self).start_lexical_environment();

        let mut statements: Vec<Id<Node /*Statement*/>> = _d();
        let ensure_use_strict = get_strict_option_value(&self.compiler_options.ref_(self), "alwaysStrict")
            || self.compiler_options.ref_(self).no_implicit_use_strict != Some(true)
                && is_external_module(&self.current_source_file().ref_(self));
        let statement_offset = self.factory.ref_(self).try_copy_prologue(
            &node_as_source_file.statements(),
            &mut statements,
            Some(ensure_use_strict && !is_json_source_file(&node.ref_(self))),
            Some(|node: Id<Node>| self.top_level_visitor(node)),
        )?;

        if self.should_emit_underscore_underscore_es_module() {
            statements.push(self.create_underscore_underscore_es_module());
        }
        let current_module_info = self.current_module_info();
        let current_module_info_exported_names = current_module_info.exported_names.as_ref();
        if current_module_info_exported_names.is_non_empty() {
            let current_module_info_exported_names = current_module_info_exported_names.unwrap();
            let chunk_size = 50;
            let mut i = 0;
            while i < current_module_info_exported_names.len() {
                statements.push(self.factory.ref_(self).create_expression_statement(reduce_left(
                    &current_module_info_exported_names
                        [i..cmp::min(i + chunk_size, current_module_info_exported_names.len())],
                    |prev: Id<Node>, next_id: &Id<Node>, _| {
                        self.factory.ref_(self).create_assignment(
                            self.factory.ref_(self).create_property_access_expression(
                                self.factory.ref_(self).create_identifier("exports"),
                                self.factory.ref_(self).create_identifier(id_text(&next_id.ref_(self))),
                            ),
                            prev,
                        )
                    },
                    self.factory.ref_(self).create_void_zero(),
                    None,
                    None,
                )));
                i += chunk_size;
            }
        }

        append(
            &mut statements,
            try_maybe_visit_node(
                current_module_info
                    .external_helpers_import_declaration,
                Some(|node: Id<Node>| self.top_level_visitor(node)),
                Some(|node| is_statement(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
        );
        add_range(
            &mut statements,
            Some(&try_visit_nodes(
                &node_as_source_file.statements(),
                Some(|node: Id<Node>| self.top_level_visitor(node)),
                Some(|node| is_statement(node, self)),
                Some(statement_offset),
                None,
                self,
            )?),
            None,
            None,
        );
        self.add_export_equals_if_needed(&mut statements, false)?;
        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.ref_(self).end_lexical_environment().as_deref(),
            self,
        );

        Ok(self
            .factory
            .ref_(self).update_source_file(
                node,
                self.factory
                    .ref_(self).create_node_array(Some(statements), None)
                    .set_text_range(Some(&*node_as_source_file.statements()), self),
                None,
                None,
                None,
                None,
                None,
            )
            .add_emit_helpers(self.context.ref_(self).read_emit_helpers().as_deref(), self))
    }

    pub(super) fn transform_amd_module(
        &self,
        node: Id<Node>, /*SourceFile*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        let define = self.factory.ref_(self).create_identifier("define");
        let module_name = try_get_module_name_from_file(
            &self.factory.ref_(self),
            Some(node),
            &**self.host.ref_(self),
            &self.compiler_options.ref_(self),
        );
        let json_source_file = is_json_source_file(&node.ref_(self)).then_some(node);

        let AsynchronousDependencies {
            aliased_module_names,
            unaliased_module_names,
            import_alias_names,
        } = self.collect_asynchronous_dependencies(node, true)?;

        Ok(self
            .factory
            .ref_(self).update_source_file(
                node,
                self.factory
                    .ref_(self).create_node_array(
                        Some(vec![self.factory.ref_(self).create_expression_statement(
                            self.factory.ref_(self).create_call_expression(
                                define,
                                Option::<Id<NodeArray>>::None,
                                Some(
                                    if let Some(module_name) = module_name {
                                        vec![module_name]
                                    } else {
                                        _d()
                                    }
                                    .and_extend([
                                        self.factory.ref_(self).create_array_literal_expression(
                                            Some(if json_source_file.is_some() {
                                                _d()
                                            } else {
                                                vec![
                                                    self.factory.ref_(self).create_string_literal(
                                                        "require".to_owned(),
                                                        None,
                                                        None,
                                                    ),
                                                    self.factory.ref_(self).create_string_literal(
                                                        "exports".to_owned(),
                                                        None,
                                                        None,
                                                    ),
                                                ]
                                                .and_extend(aliased_module_names)
                                                .and_extend(unaliased_module_names)
                                            }),
                                            None,
                                        ),
                                        if let Some(json_source_file) = json_source_file {
                                            json_source_file
                                                .ref_(self).as_source_file()
                                                .statements()
                                                .non_empty()
                                                .map_or_else(
                                                    || {
                                                        self.factory
                                                            .ref_(self).create_object_literal_expression(
                                                                Option::<Id<NodeArray>>::None,
                                                                None,
                                                            )
                                                    },
                                                    |json_source_file_statements| {
                                                        json_source_file_statements[0]
                                                            .ref_(self).as_expression_statement()
                                                            .expression
                                                    },
                                                )
                                        } else {
                                            self.factory.ref_(self).create_function_expression(
                                                Option::<Id<NodeArray>>::None,
                                                None,
                                                Option::<Id<Node>>::None,
                                                Option::<Id<NodeArray>>::None,
                                                Some(
                                                    vec![
                                                        self.factory.ref_(self).create_parameter_declaration(
                                                            Option::<Id<NodeArray>>::None,
                                                            Option::<Id<NodeArray>>::None,
                                                            None,
                                                            Some("require"),
                                                            None,
                                                            None,
                                                            None,
                                                        ),
                                                        self.factory.ref_(self).create_parameter_declaration(
                                                            Option::<Id<NodeArray>>::None,
                                                            Option::<Id<NodeArray>>::None,
                                                            None,
                                                            Some("exports"),
                                                            None,
                                                            None,
                                                            None,
                                                        ),
                                                    ]
                                                    .and_extend(import_alias_names),
                                                ),
                                                None,
                                                self.transform_asynchronous_module_body(node)?,
                                            )
                                        },
                                    ]),
                                ),
                            ),
                        )]),
                        None,
                    )
                    .set_text_range(Some(&*node_as_source_file.statements()), self),
                None,
                None,
                None,
                None,
                None,
            )
            .add_emit_helpers(self.context.ref_(self).read_emit_helpers().as_deref(), self))
    }

    pub(super) fn transform_umd_module(
        &self,
        node: Id<Node>, /*SourceFile*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        let AsynchronousDependencies {
            aliased_module_names,
            unaliased_module_names,
            import_alias_names,
        } = self.collect_asynchronous_dependencies(node, false)?;
        let module_name = try_get_module_name_from_file(
            &self.factory.ref_(self),
            Some(node),
            &**self.host.ref_(self),
            &self.compiler_options.ref_(self),
        );
        let umd_header = self.factory.ref_(self).create_function_expression(
            Option::<Id<NodeArray>>::None,
            None,
            Option::<Id<Node>>::None,
            Option::<Id<NodeArray>>::None,
            Some(vec![self.factory.ref_(self).create_parameter_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                None,
                Some("factory"),
                None,
                None,
                None,
            )]),
            None,
            self.factory
                .ref_(self).create_block(
                    vec![self.factory.ref_(self).create_if_statement(
                        self.factory.ref_(self).create_logical_and(
                            self.factory.ref_(self).create_type_check(
                                self.factory.ref_(self).create_identifier("module"),
                                "object",
                            ),
                            self.factory.ref_(self).create_type_check(
                                self.factory.ref_(self).create_property_access_expression(
                                    self.factory.ref_(self).create_identifier("module"),
                                    "exports",
                                ),
                                "object",
                            ),
                        ),
                        self.factory.ref_(self).create_block(
                            vec![
                                self.factory.ref_(self).create_variable_statement(
                                    Option::<Id<NodeArray>>::None,
                                    vec![self.factory.ref_(self).create_variable_declaration(
                                        Some("v"),
                                        None,
                                        None,
                                        Some(self.factory.ref_(self).create_call_expression(
                                            self.factory.ref_(self).create_identifier("factory"),
                                            Option::<Id<NodeArray>>::None,
                                            Some(vec![
                                                self.factory.ref_(self).create_identifier("require"),
                                                self.factory.ref_(self).create_identifier("exports"),
                                            ]),
                                        )),
                                    )],
                                ),
                                self.factory
                                    .ref_(self).create_if_statement(
                                        self.factory.ref_(self).create_strict_inequality(
                                            self.factory.ref_(self).create_identifier("v"),
                                            self.factory.ref_(self).create_identifier("undefined"),
                                        ),
                                        self.factory.ref_(self).create_expression_statement(
                                            self.factory.ref_(self).create_assignment(
                                                self.factory.ref_(self).create_property_access_expression(
                                                    self.factory.ref_(self).create_identifier("module"),
                                                    "exports",
                                                ),
                                                self.factory.ref_(self).create_identifier("v"),
                                            ),
                                        ),
                                        None,
                                    )
                                    .set_emit_flags(EmitFlags::SingleLine, self),
                            ],
                            None,
                        ),
                        Some(self.factory.ref_(self).create_if_statement(
                            self.factory.ref_(self).create_logical_and(
                                self.factory.ref_(self).create_type_check(
                                    self.factory.ref_(self).create_identifier("define"),
                                    "function",
                                ),
                                self.factory.ref_(self).create_property_access_expression(
                                    self.factory.ref_(self).create_identifier("define"),
                                    "amd",
                                ),
                            ),
                            self.factory.ref_(self).create_block(
                                vec![
                                    self.factory.ref_(self).create_expression_statement(
                                        self.factory.ref_(self).create_call_expression(
                                            self.factory.ref_(self).create_identifier("define"),
                                            Option::<Id<NodeArray>>::None,
                                            Some(if let Some(module_name) = module_name {
                                                vec![
                                                    module_name
                                                ]
                                            } else {
                                                _d()
                                            }.and_extend([
                                                self.factory.ref_(self).create_array_literal_expression(
                                                    Some(
                                                        vec![
                                                            self.factory
                                                                .ref_(self).create_string_literal(
                                                                    "require".to_owned(),
                                                                    None,
                                                                    None,
                                                                )
                                                                ,
                                                            self.factory
                                                                .ref_(self).create_string_literal(
                                                                    "exports".to_owned(),
                                                                    None,
                                                                    None,
                                                                )
                                                                ,
                                                        ]
                                                        .and_extend(aliased_module_names)
                                                        .and_extend(unaliased_module_names)
                                                    ),
                                                    None,
                                                ),
                                                self.factory.ref_(self).create_identifier("factory")
                                            ]))
                                        )
                                    )
                                ],
                                None,
                            ),
                            None,
                        )),
                    )],
                    Some(true),
                )
                .set_text_range(Option::<&Node>::None, self),
        );

        Ok(self
            .factory
            .ref_(self).update_source_file(
                node,
                self.factory
                    .ref_(self).create_node_array(
                        Some(vec![self.factory.ref_(self).create_expression_statement(
                            self.factory.ref_(self).create_call_expression(
                                umd_header,
                                Option::<Id<NodeArray>>::None,
                                Some(vec![self.factory.ref_(self).create_function_expression(
                                    Option::<Id<NodeArray>>::None,
                                    None,
                                    Option::<Id<Node>>::None,
                                    Option::<Id<NodeArray>>::None,
                                    Some(
                                        vec![
                                            self.factory.ref_(self).create_parameter_declaration(
                                                Option::<Id<NodeArray>>::None,
                                                Option::<Id<NodeArray>>::None,
                                                None,
                                                Some("require"),
                                                None,
                                                None,
                                                None,
                                            ),
                                            self.factory.ref_(self).create_parameter_declaration(
                                                Option::<Id<NodeArray>>::None,
                                                Option::<Id<NodeArray>>::None,
                                                None,
                                                Some("exports"),
                                                None,
                                                None,
                                                None,
                                            ),
                                        ]
                                        .and_extend(import_alias_names),
                                    ),
                                    None,
                                    self.transform_asynchronous_module_body(node)?,
                                )]),
                            ),
                        )]),
                        None,
                    )
                    .set_text_range(Some(&*node_as_source_file.statements()), self),
                None,
                None,
                None,
                None,
                None,
            )
            .add_emit_helpers(self.context.ref_(self).read_emit_helpers().as_deref(), self))
    }

    pub(super) fn collect_asynchronous_dependencies(
        &self,
        node: Id<Node>, /*SourceFile*/
        include_non_amd_dependencies: bool,
    ) -> io::Result<AsynchronousDependencies> {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        let mut aliased_module_names: Vec<Id<Node /*Expression*/>> = _d();

        let mut unaliased_module_names: Vec<Id<Node /*Expression*/>> = _d();

        let mut import_alias_names: Vec<Id<Node /*ParameterDeclaration*/>> = _d();

        for amd_dependency in &*node_as_source_file.amd_dependencies() {
            if let Some(amd_dependency_name) = amd_dependency.name.as_ref().non_empty() {
                aliased_module_names.push(self.factory.ref_(self).create_string_literal(
                    amd_dependency.path.clone(),
                    None,
                    None,
                ));
                import_alias_names.push(self.factory.ref_(self).create_parameter_declaration(
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
                    None,
                    Some(&**amd_dependency_name),
                    None,
                    None,
                    None,
                ));
            } else {
                unaliased_module_names.push(self.factory.ref_(self).create_string_literal(
                    amd_dependency.path.clone(),
                    None,
                    None,
                ));
            }
        }

        for &import_node in &self.current_module_info().external_imports {
            let external_module_name = get_external_module_name_literal(
                &self.factory.ref_(self),
                import_node,
                self.current_source_file(),
                &**self.host.ref_(self),
                &**self.resolver.ref_(self),
                &self.compiler_options.ref_(self),
            )?;

            let import_alias_name = get_local_name_for_external_import(
                &self.factory.ref_(self),
                import_node,
                self.current_source_file(),
            );
            if let Some(external_module_name) = external_module_name {
                if let Some(import_alias_name) =
                    import_alias_name.filter(|_| include_non_amd_dependencies)
                {
                    set_emit_flags(import_alias_name, EmitFlags::NoSubstitution, self);
                    aliased_module_names.push(external_module_name);
                    import_alias_names.push(self.factory.ref_(self).create_parameter_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        None,
                        Some(import_alias_name),
                        None,
                        None,
                        None,
                    ));
                } else {
                    unaliased_module_names.push(external_module_name);
                }
            }
        }

        Ok(AsynchronousDependencies {
            aliased_module_names,
            unaliased_module_names,
            import_alias_names,
        })
    }

    pub(super) fn get_amd_import_expression_for_import(
        &self,
        node: Id<Node>, /*ImportDeclaration | ExportDeclaration | ImportEqualsDeclaration*/
    ) -> io::Result<Option<Id<Node>>> {
        if is_import_equals_declaration(&node.ref_(self))
            || is_export_declaration(&node.ref_(self))
            || get_external_module_name_literal(
                &self.factory.ref_(self),
                node,
                self.current_source_file(),
                &**self.host.ref_(self),
                &**self.resolver.ref_(self),
                &self.compiler_options.ref_(self),
            )?
            .is_none()
        {
            return Ok(None);
        }
        let name =
            get_local_name_for_external_import(&self.factory.ref_(self), node, self.current_source_file())
                .unwrap();
        let expr = self.get_helper_expression_for_import(node, name.clone());
        if expr == name {
            return Ok(None);
        }
        Ok(Some(self.factory.ref_(self).create_expression_statement(
            self.factory.ref_(self).create_assignment(name, expr),
        )))
    }

    pub(super) fn transform_asynchronous_module_body(
        &self,
        node: Id<Node>, /*SourceFile*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        self.context.ref_(self).start_lexical_environment();

        let mut statements: Vec<Id<Node /*Statement*/>> = _d();
        let statement_offset = self.factory.ref_(self).try_copy_prologue(
            &node_as_source_file.statements(),
            &mut statements,
            Some(self.compiler_options.ref_(self).no_implicit_use_strict != Some(true)),
            Some(|node: Id<Node>| self.top_level_visitor(node)),
        )?;

        if self.should_emit_underscore_underscore_es_module() {
            statements.push(self.create_underscore_underscore_es_module());
        }
        if let Some(current_module_info_exported_names) = self
            .current_module_info()
            .exported_names
            .as_ref()
            .non_empty()
        {
            statements.push(self.factory.ref_(self).create_expression_statement(reduce_left(
                current_module_info_exported_names,
                |prev: Id<Node>, next_id: &Id<Node>, _| {
                    self.factory.ref_(self).create_assignment(
                        self.factory.ref_(self).create_property_access_expression(
                            self.factory.ref_(self).create_identifier("exports"),
                            self.factory.ref_(self).create_identifier(id_text(&next_id.ref_(self))),
                        ),
                        prev,
                    )
                },
                self.factory.ref_(self).create_void_zero(),
                None,
                None,
            )));
        }

        append(
            &mut statements,
            try_maybe_visit_node(
                self.current_module_info()
                    .external_helpers_import_declaration,
                Some(|node: Id<Node>| self.top_level_visitor(node)),
                Some(|node| is_statement(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
        );
        if self.module_kind == ModuleKind::AMD {
            add_range(
                &mut statements,
                Some(&try_map_defined(
                    Some(&self.current_module_info().external_imports),
                    |&external_import: &Id<Node>, _| {
                        self.get_amd_import_expression_for_import(external_import)
                    },
                )?),
                None,
                None,
            );
        }
        add_range(
            &mut statements,
            Some(&try_visit_nodes(
                &node_as_source_file.statements(),
                Some(|node: Id<Node>| self.top_level_visitor(node)),
                Some(|node| is_statement(node, self)),
                Some(statement_offset),
                None,
                self,
            )?),
            None,
            None,
        );

        self.add_export_equals_if_needed(&mut statements, true)?;

        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.ref_(self).end_lexical_environment().as_deref(),
            self,
        );

        let body = self.factory.ref_(self).create_block(statements, Some(true));
        if self.need_umd_dynamic_import_helper() {
            add_emit_helper(body, dynamic_import_umd_helper(self), self);
        }

        Ok(body)
    }

    pub(super) fn add_export_equals_if_needed(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        emit_as_return: bool,
    ) -> io::Result<()> {
        if let Some(current_module_info_export_equals) =
            self.current_module_info().export_equals.as_ref()
        {
            let expression_result = try_visit_node(
                current_module_info_export_equals
                    .ref_(self).as_export_assignment()
                    .expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Option::<fn(Id<Node>) -> bool>::None,
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?;
            // if (expressionResult) {
            if emit_as_return {
                let statement = self
                    .factory
                    .ref_(self).create_return_statement(Some(expression_result))
                    .set_text_range(Some(&*current_module_info_export_equals.ref_(self)), self)
                    .set_emit_flags(EmitFlags::NoTokenSourceMaps | EmitFlags::NoComments, self);
                statements.push(statement);
            } else {
                let statement = self
                    .factory
                    .ref_(self).create_expression_statement(self.factory.ref_(self).create_assignment(
                        self.factory.ref_(self).create_property_access_expression(
                            self.factory.ref_(self).create_identifier("module"),
                            "exports",
                        ),
                        expression_result,
                    ))
                    .set_text_range(Some(&*current_module_info_export_equals.ref_(self)), self)
                    .set_emit_flags(EmitFlags::NoComments, self);
                statements.push(statement);
            }
            // }
        }

        Ok(())
    }
}

impl TransformerInterface for TransformModule {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        self.transform_source_file(node)
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl HasArena for TransformModule {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformModuleOnEmitNodeOverrider {
    transform_module: Transformer,
    previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformModuleOnEmitNodeOverrider {
    fn new(
        transform_module: Transformer,
        previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_module,
            previous_on_emit_node,
        }
    }

    fn transform_module(&self) -> debug_cell::Ref<'_, TransformModule> {
        downcast_transformer_ref(self.transform_module, self)
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformModuleOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        if node.ref_(self).kind() == SyntaxKind::SourceFile {
            self.transform_module()
                .set_current_source_file(Some(node));
            self.transform_module().set_current_module_info(
                self.transform_module()
                    .module_info_map()
                    .get(&get_original_node_id(
                        self.transform_module().current_source_file(),
                        self,
                    ))
                    .cloned(),
            );

            self.previous_on_emit_node
                .ref_(self).on_emit_node(hint, node, emit_callback)?;

            self.transform_module().set_current_source_file(None);
            self.transform_module().set_current_module_info(None);
        } else {
            self.previous_on_emit_node
                .ref_(self).on_emit_node(hint, node, emit_callback)?;
        }

        Ok(())
    }
}

impl HasArena for TransformModuleOnEmitNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformModuleOnSubstituteNodeOverrider {
    transform_module: Transformer,
    previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformModuleOnSubstituteNodeOverrider {
    fn new(
        transform_module: Transformer,
        previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_module,
            previous_on_substitute_node,
        }
    }

    fn transform_module(&self) -> debug_cell::Ref<'_, TransformModule> {
        downcast_transformer_ref(self.transform_module, self)
    }

    fn substitute_shorthand_property_assignment(
        &self,
        node: Id<Node>, /*ShorthandPropertyAssignment*/
    ) -> io::Result<Id<Node /*ObjectLiteralElementLike*/>> {
        let node_ref = node.ref_(self);
        let node_as_shorthand_property_assignment = node_ref.as_shorthand_property_assignment();
        let name = node_as_shorthand_property_assignment.name();
        let exported_or_imported_name = self.substitute_expression_identifier(name)?;
        if exported_or_imported_name != name {
            if let Some(node_object_assignment_initializer) = node_as_shorthand_property_assignment
                .object_assignment_initializer
                .as_ref()
            {
                let initializer = self.transform_module().factory.ref_(self).create_assignment(
                    exported_or_imported_name,
                    node_object_assignment_initializer.clone(),
                );
                return Ok(self
                    .transform_module()
                    .factory
                    .ref_(self).create_property_assignment(name, initializer)
                    .set_text_range(Some(&*node.ref_(self)), self));
            }
            return Ok(self
                .transform_module()
                .factory
                .ref_(self).create_property_assignment(name, exported_or_imported_name)
                .set_text_range(Some(&*node.ref_(self)), self));
        }
        Ok(node)
    }

    fn substitute_expression(&self, node: Id<Node> /*Expression*/) -> io::Result<Id<Node>> {
        Ok(match node.ref_(self).kind() {
            SyntaxKind::Identifier => self.substitute_expression_identifier(node)?,
            SyntaxKind::CallExpression => self.substitute_call_expression(node)?,
            SyntaxKind::TaggedTemplateExpression => {
                self.substitute_tagged_template_expression(node)?
            }
            SyntaxKind::BinaryExpression => self.substitute_binary_expression(node)?,
            _ => node,
        })
    }

    fn substitute_call_expression(
        &self,
        node: Id<Node>, /*CallExpression*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        if is_identifier(&node_as_call_expression.expression.ref_(self)) {
            let expression =
                self.substitute_expression_identifier(node_as_call_expression.expression)?;
            self.transform_module()
                .no_substitution_mut()
                .insert(get_node_id(&expression.ref_(self)), true);
            if !is_identifier(&expression.ref_(self)) {
                return Ok(self
                    .transform_module()
                    .factory
                    .ref_(self).update_call_expression(
                        node,
                        expression,
                        Option::<Id<NodeArray>>::None,
                        node_as_call_expression.arguments.clone(),
                    )
                    .add_emit_flags(EmitFlags::IndirectCall, self));
            }
        }
        Ok(node)
    }

    fn substitute_tagged_template_expression(
        &self,
        node: Id<Node>, /*TaggedTemplateExpression*/
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_tagged_template_expression = node_ref.as_tagged_template_expression();
        if is_identifier(&node_as_tagged_template_expression.tag.ref_(self)) {
            let tag =
                self.substitute_expression_identifier(node_as_tagged_template_expression.tag)?;
            self.transform_module()
                .no_substitution_mut()
                .insert(get_node_id(&tag.ref_(self)), true);
            if !is_identifier(&tag.ref_(self)) {
                return Ok(self
                    .transform_module()
                    .factory
                    .ref_(self).update_tagged_template_expression(
                        node,
                        tag,
                        Option::<Id<NodeArray>>::None,
                        node_as_tagged_template_expression.template.clone(),
                    )
                    .add_emit_flags(EmitFlags::IndirectCall, self));
            }
        }
        Ok(node)
    }

    fn substitute_expression_identifier(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_ref = node.ref_(self);
        let node_as_identifier = node_ref.as_identifier();
        #[allow(clippy::nonminimal_bool)]
        if get_emit_flags(&node.ref_(self)).intersects(EmitFlags::HelperName) {
            let external_helpers_module_name =
                get_external_helpers_module_name(self.transform_module().current_source_file(), self);
            if let Some(external_helpers_module_name) = external_helpers_module_name {
                return Ok(self
                    .transform_module()
                    .factory
                    .ref_(self).create_property_access_expression(
                        external_helpers_module_name,
                        node,
                    ));
            }
            return Ok(node);
        } else if !(is_generated_identifier(&node.ref_(self))
            && !node_as_identifier.maybe_auto_generate_flags().matches(
                |node_auto_generate_flags| {
                    node_auto_generate_flags
                        .intersects(GeneratedIdentifierFlags::AllowNameSubstitution)
                },
            ))
            && !is_local_name(&node.ref_(self))
        {
            let export_container = self
                .transform_module()
                .resolver
                .ref_(self).get_referenced_export_container(node, Some(is_export_name(&node.ref_(self))))?;
            if export_container
                .matches(|export_container| export_container.ref_(self).kind() == SyntaxKind::SourceFile)
            {
                return Ok(self
                    .transform_module()
                    .factory
                    .ref_(self).create_property_access_expression(
                        self.transform_module().factory.ref_(self).create_identifier("exports"),
                        self.transform_module().factory.ref_(self).clone_node(node),
                    )
                    .set_text_range(Some(&*node.ref_(self)), self));
            }
            let import_declaration = self
                .transform_module()
                .resolver
                .ref_(self).get_referenced_import_declaration(node)?;
            if let Some(import_declaration) = import_declaration {
                if is_import_clause(&import_declaration.ref_(self)) {
                    return Ok(self
                        .transform_module()
                        .factory
                        .ref_(self).create_property_access_expression(
                            self.transform_module().factory.ref_(self).get_generated_name_for_node(
                                import_declaration.ref_(self).maybe_parent(),
                                None,
                            ),
                            self.transform_module().factory.ref_(self).create_identifier("default"),
                        )
                        .set_text_range(Some(&*node.ref_(self)), self));
                } else if is_import_specifier(&import_declaration.ref_(self)) {
                    let import_declaration_ref = import_declaration.ref_(self);
                    let import_declaration_as_import_specifier = import_declaration_ref.as_import_specifier();
                    let name = import_declaration_as_import_specifier
                        .property_name
                        .unwrap_or(import_declaration_as_import_specifier.name);
                    return Ok(self
                        .transform_module()
                        .factory
                        .ref_(self).create_property_access_expression(
                            self.transform_module().factory.ref_(self).get_generated_name_for_node(
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
                            self.transform_module().factory.ref_(self).clone_node(name),
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
        if is_assignment_operator(node_as_binary_expression.operator_token.ref_(self).kind())
            && is_identifier(&node_as_binary_expression.left.ref_(self))
            && !is_generated_identifier(&node_as_binary_expression.left.ref_(self))
            && !is_local_name(&node_as_binary_expression.left.ref_(self))
            && !is_declaration_name_of_enum_or_namespace(node_as_binary_expression.left, self)
        {
            let exported_names = self
                .transform_module()
                .get_exports(node_as_binary_expression.left)?;
            if let Some(exported_names) = exported_names {
                let mut expression/*: Expression*/ = node;
                for &export_name in &exported_names {
                    self.transform_module()
                        .no_substitution_mut()
                        .insert(get_node_id(&expression.ref_(self)), true);
                    expression = self.transform_module().create_export_expression(
                        export_name,
                        expression,
                        Some(&*node.ref_(self)),
                        None,
                    );
                }

                return Ok(expression);
            }
        }

        Ok(node)
    }
}

impl TransformationContextOnSubstituteNodeOverrider for TransformModuleOnSubstituteNodeOverrider {
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        let node = self
            .previous_on_substitute_node
            .ref_(self).on_substitute_node(hint, node)?;
        if node.ref_(self).maybe_id().matches(|node_id| {
            self.transform_module()
                .no_substitution()
                .get(&node_id)
                .copied()
                == Some(true)
        }) {
            return Ok(node);
        }

        if hint == EmitHint::Expression {
            return self.substitute_expression(node);
        } else if is_shorthand_property_assignment(&node.ref_(self)) {
            return self.substitute_shorthand_property_assignment(node);
        }

        Ok(node)
    }
}

impl HasArena for TransformModuleOnSubstituteNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformModuleFactory {}

impl TransformModuleFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformModuleFactory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle(self).ref_(self).call(
            context.clone(),
            TransformModule::new(context, &*static_arena()),
        )
    }
}

impl HasArena for TransformModuleFactory {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub fn transform_module(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformModuleFactory::new()))
}
