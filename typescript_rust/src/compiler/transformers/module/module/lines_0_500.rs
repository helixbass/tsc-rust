use std::{cell::Cell, collections::HashMap, io, mem, rc::Rc};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use super::dynamic_import_umd_helper;
use crate::{
    BaseNodeFactorySynthetic, CompilerOptions, EmitResolver, ExternalModuleInfo, ModuleKind, Node,
    NodeFactory, NodeId, ScriptTarget, TransformationContext, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, _d, add_emit_helper, add_range, append,
    chain_bundle, collect_external_module_info, gc_cell_ref_mut_unwrapped, get_emit_module_kind,
    get_emit_script_target, get_external_module_name_literal, get_local_name_for_external_import,
    get_original_node_id, get_strict_option_value, has_json_module_emit_enabled, id_text,
    insert_statements_after_standard_prologue, is_effective_external_module, is_export_declaration,
    is_external_module, is_import_equals_declaration, is_json_source_file, is_statement,
    map_defined, maybe_visit_node, out_file, reduce_left, set_emit_flags,
    try_get_module_name_from_file, visit_node, visit_nodes, EmitFlags, EmitHelperFactory, EmitHint,
    EmitHost, HasStatementsInterface, NodeArray, NodeArrayExt, NodeExt, NodeInterface, NonEmpty,
    SyntaxKind, TransformFlags, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, VecExt,
};

pub(super) struct AsynchronousDependencies {
    pub aliased_module_names: Vec<Gc<Node /*Expression*/>>,
    pub unaliased_module_names: Vec<Gc<Node /*Expression*/>>,
    pub import_alias_names: Vec<Gc<Node /*ParameterDeclaration*/>>,
}

#[derive(Trace, Finalize)]
pub(super) struct TransformModule {
    pub(super) _transformer_wrapper: GcCell<Option<Transformer>>,
    pub(super) context: Gc<Box<dyn TransformationContext>>,
    pub(super) factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    pub(super) resolver: Gc<Box<dyn EmitResolver>>,
    pub(super) host: Gc<Box<dyn EmitHost>>,
    pub(super) compiler_options: Gc<CompilerOptions>,
    #[unsafe_ignore_trace]
    pub(super) language_version: ScriptTarget,
    #[unsafe_ignore_trace]
    pub(super) module_kind: ModuleKind,
    pub(super) module_info_map: GcCell<HashMap<NodeId, Gc<ExternalModuleInfo>>>,
    pub(super) deferred_exports: GcCell<Vec<Option<Vec<Gc<Node /*Statement*/>>>>>,
    pub(super) current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    pub(super) current_module_info: GcCell<Option<Gc<ExternalModuleInfo>>>,
    pub(super) no_substitution: GcCell<HashMap<NodeId, bool>>,
    #[unsafe_ignore_trace]
    pub(super) need_umd_dynamic_import_helper: Cell<bool>,
}

impl TransformModule {
    pub(super) fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let compiler_options = context.get_compiler_options();
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: _d(),
            factory: context.factory(),
            resolver: context.get_emit_resolver(),
            host: context.get_emit_host(),
            language_version: get_emit_script_target(&compiler_options),
            module_kind: get_emit_module_kind(&compiler_options),
            compiler_options,
            context: context.clone(),
            module_info_map: _d(),
            deferred_exports: _d(),
            current_source_file: _d(),
            current_module_info: _d(),
            no_substitution: _d(),
            need_umd_dynamic_import_helper: _d(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformModuleOnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformModuleOnSubstituteNodeOverrider::new(
                downcasted.clone(),
                previous_on_substitute_node,
            )))
        });
        context.enable_substitution(SyntaxKind::CallExpression);
        context.enable_substitution(SyntaxKind::TaggedTemplateExpression);
        context.enable_substitution(SyntaxKind::Identifier);
        context.enable_substitution(SyntaxKind::BinaryExpression);
        context.enable_substitution(SyntaxKind::ShorthandPropertyAssignment);
        context.enable_emit_notification(SyntaxKind::SourceFile);
        downcasted
    }

    pub(super) fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    pub(super) fn emit_helpers(&self) -> Rc<EmitHelperFactory> {
        self.context.get_emit_helper_factory()
    }

    pub(super) fn module_info_map(&self) -> GcCellRef<HashMap<NodeId, Gc<ExternalModuleInfo>>> {
        self.module_info_map.borrow()
    }

    pub(super) fn module_info_map_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, Gc<ExternalModuleInfo>>> {
        self.module_info_map.borrow_mut()
    }

    pub(super) fn set_module_info_map(
        &self,
        module_info_map: HashMap<NodeId, Gc<ExternalModuleInfo>>,
    ) {
        *self.module_info_map.borrow_mut() = module_info_map;
    }

    pub(super) fn deferred_exports(&self) -> GcCellRef<Vec<Option<Vec<Gc<Node /*Statement*/>>>>> {
        self.deferred_exports.borrow()
    }

    pub(super) fn deferred_exports_mut(
        &self,
    ) -> GcCellRefMut<Vec<Option<Vec<Gc<Node /*Statement*/>>>>> {
        self.deferred_exports.borrow_mut()
    }

    pub(super) fn set_deferred_exports(
        &self,
        deferred_exports: Vec<Option<Vec<Gc<Node /*Statement*/>>>>,
    ) {
        *self.deferred_exports.borrow_mut() = deferred_exports;
    }

    pub(super) fn maybe_current_source_file(&self) -> Option<Gc<Node /*SourceFile*/>> {
        self.current_source_file.borrow().clone()
    }

    pub(super) fn current_source_file(&self) -> Gc<Node /*SourceFile*/> {
        self.current_source_file.borrow().clone().unwrap()
    }

    pub(super) fn maybe_current_source_file_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node /*SourceFile*/>>> {
        self.current_source_file.borrow_mut()
    }

    pub(super) fn current_source_file_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node /*SourceFile*/>>, Gc<Node /*SourceFile*/>> {
        gc_cell_ref_mut_unwrapped(&self.current_source_file)
    }

    pub(super) fn set_current_source_file(
        &self,
        current_source_file: Option<Gc<Node /*SourceFile*/>>,
    ) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    pub(super) fn maybe_current_module_info(&self) -> Option<Gc<ExternalModuleInfo>> {
        self.current_module_info.borrow().clone()
    }

    pub(super) fn current_module_info(&self) -> Gc<ExternalModuleInfo> {
        self.current_module_info.borrow().clone().unwrap()
    }

    pub(super) fn maybe_current_module_info_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<ExternalModuleInfo>>> {
        self.current_module_info.borrow_mut()
    }

    pub(super) fn current_module_info_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<ExternalModuleInfo>>, Gc<ExternalModuleInfo>> {
        gc_cell_ref_mut_unwrapped(&self.current_module_info)
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

    pub(super) fn set_no_substitution(&self, no_substitution: HashMap<NodeId, bool>) {
        *self.no_substitution.borrow_mut() = no_substitution;
    }

    pub(super) fn need_umd_dynamic_import_helper(&self) -> bool {
        self.need_umd_dynamic_import_helper.get()
    }

    pub(super) fn set_need_umd_dynamic_import_helper(&self, need_umd_dynamic_import_helper: bool) {
        self.need_umd_dynamic_import_helper
            .set(need_umd_dynamic_import_helper);
    }

    pub(super) fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        if node_as_source_file.is_declaration_file()
            || !(is_effective_external_module(node, &self.compiler_options)
                || node
                    .transform_flags()
                    .intersects(TransformFlags::ContainsDynamicImport)
                || is_json_source_file(node)
                    && has_json_module_emit_enabled(&self.compiler_options)
                    && out_file(&self.compiler_options).is_non_empty())
        {
            return node.node_wrapper();
        }

        self.set_current_source_file(Some(node.node_wrapper()));
        self.set_current_module_info(Some(Gc::new(collect_external_module_info(
            &**self.context,
            node,
            &**self.resolver,
            &self.compiler_options,
        ))));
        self.module_info_map_mut()
            .insert(get_original_node_id(node), self.current_module_info());

        let updated = match self.module_kind {
            ModuleKind::AMD => self.transform_amd_module(node),
            ModuleKind::UMD => self.transform_umd_module(node),
            _ => self.transform_common_js_module(node),
        };
        self.set_current_source_file(None);
        self.set_current_module_info(None);
        self.set_need_umd_dynamic_import_helper(false);
        updated
    }

    pub(super) fn should_emit_underscore_underscore_es_module(&self) -> bool {
        if self.current_module_info().export_equals.is_none()
            && is_external_module(&self.current_source_file())
        {
            return true;
        }
        false
    }

    pub(super) fn transform_common_js_module(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        self.context.start_lexical_environment();

        let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
        let ensure_use_strict = get_strict_option_value(&self.compiler_options, "alwaysStrict")
            || self.compiler_options.no_implicit_use_strict != Some(true)
                && is_external_module(&self.current_source_file());
        let statement_offset = self.factory.copy_prologue(
            &node_as_source_file.statements(),
            &mut statements,
            Some(ensure_use_strict && !is_json_source_file(node)),
            Some(|node: &Node| self.top_level_visitor(node)),
        );

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
                statements.push(
                    self.factory
                        .create_expression_statement(reduce_left(
                            &current_module_info_exported_names[i..i + chunk_size],
                            |prev: Gc<Node>, next_id: &Gc<Node>, _| {
                                self.factory
                                    .create_assignment(
                                        self.factory
                                            .create_property_access_expression(
                                                self.factory.create_identifier("exports"),
                                                self.factory.create_identifier(id_text(next_id)),
                                            )
                                            .wrap(),
                                        prev,
                                    )
                                    .wrap()
                            },
                            self.factory.create_void_zero(),
                            None,
                            None,
                        ))
                        .wrap(),
                );
                i += chunk_size;
            }
        }

        append(
            &mut statements,
            maybe_visit_node(
                current_module_info
                    .external_helpers_import_declaration
                    .as_deref(),
                Some(|node: &Node| self.top_level_visitor(node)),
                Some(is_statement),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            ),
        );
        add_range(
            &mut statements,
            Some(&visit_nodes(
                &node_as_source_file.statements(),
                Some(|node: &Node| self.top_level_visitor(node)),
                Some(is_statement),
                Some(statement_offset),
                None,
            )),
            None,
            None,
        );
        self.add_export_equals_if_needed(&mut statements, false);
        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.end_lexical_environment().as_deref(),
        );

        self.factory
            .update_source_file(
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
            .add_emit_helpers(self.context.read_emit_helpers().as_deref())
    }

    pub(super) fn transform_amd_module(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        let define = self.factory.create_identifier("define");
        let module_name = try_get_module_name_from_file(
            &self.factory,
            Some(node),
            &**self.host,
            &self.compiler_options,
        );
        let json_source_file = is_json_source_file(node).then_some(node);

        let AsynchronousDependencies {
            aliased_module_names,
            unaliased_module_names,
            import_alias_names,
        } = self.collect_asynchronous_dependencies(node, true);

        self.factory.update_source_file(
            node,
            self.factory
                .create_node_array(
                    Some(vec![self
                        .factory
                        .create_expression_statement(
                            self.factory
                                .create_call_expression(
                                    define,
                                    Option::<Gc<NodeArray>>::None,
                                    Some(
                                        if let Some(module_name) = module_name {
                                            vec![module_name]
                                        } else {
                                            _d()
                                        }
                                        .and_extend([
                                            self.factory
                                                .create_array_literal_expression(
                                                    Some(if json_source_file.is_some() {
                                                        _d()
                                                    } else {
                                                        vec![
                                                            self.factory
                                                                .create_string_literal(
                                                                    "require".to_owned(),
                                                                    None,
                                                                    None,
                                                                )
                                                                .wrap(),
                                                            self.factory
                                                                .create_string_literal(
                                                                    "exports".to_owned(),
                                                                    None,
                                                                    None,
                                                                )
                                                                .wrap(),
                                                        ]
                                                        .and_extend(aliased_module_names)
                                                        .and_extend(unaliased_module_names)
                                                    }),
                                                    None,
                                                )
                                                .wrap(),
                                            if let Some(json_source_file) = json_source_file {
                                                json_source_file
                                                    .as_source_file()
                                                    .statements()
                                                    .non_empty()
                                                    .map_or_else(
                                                        || self.factory.create_object_literal_expression(
                                                            Option::<Gc<NodeArray>>::None,
                                                            None,
                                                        ).wrap(),
                                                        |json_source_file_statements| json_source_file_statements[0].as_expression_statement().expression.clone(),
                                                    )
                                            } else {
                                                self.factory.create_function_expression(
                                                    Option::<Gc<NodeArray>>::None,
                                                    None,
                                                    Option::<Gc<Node>>::None,
                                                    Option::<Gc<NodeArray>>::None,
                                                    Some(vec![
                                                        self.factory.create_parameter_declaration(
                                                            Option::<Gc<NodeArray>>::None,
                                                            Option::<Gc<NodeArray>>::None,
                                                            None,
                                                            Some("require"),
                                                            None, None, None,
                                                        ).wrap(),
                                                        self.factory.create_parameter_declaration(
                                                            Option::<Gc<NodeArray>>::None,
                                                            Option::<Gc<NodeArray>>::None,
                                                            None,
                                                            Some("exports"),
                                                            None, None, None,
                                                        ).wrap(),
                                                    ].and_extend(import_alias_names)),
                                                    None,
                                                    self.transform_asynchronous_module_body(
                                                        node,
                                                    )
                                                ).wrap()
                                            },
                                        ]),
                                    ),
                                )
                                .wrap(),
                        )
                        .wrap()]),
                    None,
                )
                .set_text_range(Some(&*node_as_source_file.statements())),
            None,
            None,
            None,
            None,
            None,
        ).add_emit_helpers(self.context.read_emit_helpers().as_deref())
    }

    pub(super) fn transform_umd_module(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        let AsynchronousDependencies {
            aliased_module_names,
            unaliased_module_names,
            import_alias_names,
        } = self.collect_asynchronous_dependencies(node, false);
        let module_name = try_get_module_name_from_file(
            &self.factory,
            Some(node),
            &**self.host,
            &self.compiler_options,
        );
        let umd_header = self
            .factory
            .create_function_expression(
                Option::<Gc<NodeArray>>::None,
                None,
                Option::<Gc<Node>>::None,
                Option::<Gc<NodeArray>>::None,
                Some(vec![self
                    .factory
                    .create_parameter_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        None,
                        Some("factory"),
                        None,
                        None,
                        None,
                    )
                    .wrap()]),
                None,
                self.factory
                    .create_block(
                        vec![self
                            .factory
                            .create_if_statement(
                                self.factory
                                    .create_logical_and(
                                        self.factory.create_type_check(
                                            self.factory.create_identifier("module"),
                                            "object",
                                        ),
                                        self.factory.create_type_check(
                                            self.factory
                                                .create_property_access_expression(
                                                    self.factory.create_identifier("module"),
                                                    "exports",
                                                )
                                                .wrap(),
                                            "object",
                                        ),
                                    )
                                    .wrap(),
                                self.factory
                                    .create_block(
                                        vec![
                                self.factory.create_variable_statement(
                                    Option::<Gc<NodeArray>>::None,
                                    vec![
                                        self.factory.create_variable_declaration(
                                            Some("v"),
                                            None, None,
                                            Some(self.factory.create_call_expression(
                                                self.factory.create_identifier("factory"),
                                                Option::<Gc<NodeArray>>::None,
                                                Some(vec![
                                                    self.factory.create_identifier("require"),
                                                    self.factory.create_identifier("exports"),
                                                ])
                                            ).wrap())
                                        ).wrap()
                                    ]
                                ).wrap(),
                                self.factory.create_if_statement(
                                    self.factory.create_strict_inequality(
                                        self.factory.create_identifier("v"),
                                        self.factory.create_identifier("undefined"),
                                    ).wrap(),
                                    self.factory.create_expression_statement(
                                        self.factory.create_assignment(
                                            self.factory.create_property_access_expression(
                                                self.factory.create_identifier("module"),
                                                "exports"
                                            ).wrap(),
                                            self.factory.create_identifier("v")
                                        ).wrap()
                                    ).wrap(),
                                    None,
                                ).wrap()
                                    .set_emit_flags(EmitFlags::SingleLine)
                            ],
                                        None,
                                    )
                                    .wrap(),
                                Some(
                                    self.factory
                                        .create_if_statement(
                                            self.factory
                                                .create_logical_and(
                                                    self.factory.create_type_check(
                                                        self.factory.create_identifier("define"),
                                                        "function",
                                                    ),
                                                    self.factory
                                                        .create_property_access_expression(
                                                            self.factory
                                                                .create_identifier("define"),
                                                            "amd",
                                                        )
                                                        .wrap(),
                                                )
                                                .wrap(),
                                            self.factory
                                                .create_block(
                                                    vec![
                                    self.factory.create_expression_statement(
                                        self.factory.create_call_expression(
                                            self.factory.create_identifier("define"),
                                            Option::<Gc<NodeArray>>::None,
                                            Some(if let Some(module_name) = module_name {
                                                vec![
                                                    module_name
                                                ]
                                            } else {
                                                _d()
                                            }.and_extend([
                                                self.factory.create_array_literal_expression(
                                                    Some(
                                                        vec![
                                                            self.factory
                                                                .create_string_literal(
                                                                    "require".to_owned(),
                                                                    None,
                                                                    None,
                                                                )
                                                                .wrap(),
                                                            self.factory
                                                                .create_string_literal(
                                                                    "exports".to_owned(),
                                                                    None,
                                                                    None,
                                                                )
                                                                .wrap(),
                                                        ]
                                                        .and_extend(aliased_module_names)
                                                        .and_extend(unaliased_module_names)
                                                    ),
                                                    None,
                                                ).wrap(),
                                                self.factory.create_identifier("factory")
                                            ]))
                                        ).wrap()
                                    ).wrap()
                                ],
                                                    None,
                                                )
                                                .wrap(),
                                            None,
                                        )
                                        .wrap(),
                                ),
                            )
                            .wrap()],
                        Some(true),
                    )
                    .wrap()
                    .set_text_range(Option::<&Node>::None),
            )
            .wrap();

        self.factory
            .update_source_file(
                node,
                self.factory
                    .create_node_array(
                        Some(vec![self
                            .factory
                            .create_expression_statement(
                                self.factory
                                    .create_call_expression(
                                        umd_header,
                                        Option::<Gc<NodeArray>>::None,
                                        Some(vec![self
                                            .factory
                                            .create_function_expression(
                                                Option::<Gc<NodeArray>>::None,
                                                None,
                                                Option::<Gc<Node>>::None,
                                                Option::<Gc<NodeArray>>::None,
                                                Some(
                                                    vec![
                                                        self.factory
                                                            .create_parameter_declaration(
                                                                Option::<Gc<NodeArray>>::None,
                                                                Option::<Gc<NodeArray>>::None,
                                                                None,
                                                                Some("require"),
                                                                None,
                                                                None,
                                                                None,
                                                            )
                                                            .wrap(),
                                                        self.factory
                                                            .create_parameter_declaration(
                                                                Option::<Gc<NodeArray>>::None,
                                                                Option::<Gc<NodeArray>>::None,
                                                                None,
                                                                Some("exports"),
                                                                None,
                                                                None,
                                                                None,
                                                            )
                                                            .wrap(),
                                                    ]
                                                    .and_extend(import_alias_names),
                                                ),
                                                None,
                                                self.transform_asynchronous_module_body(node),
                                            )
                                            .wrap()]),
                                    )
                                    .wrap(),
                            )
                            .wrap()]),
                        None,
                    )
                    .set_text_range(Some(&*node_as_source_file.statements())),
                None,
                None,
                None,
                None,
                None,
            )
            .add_emit_helpers(self.context.read_emit_helpers().as_deref())
    }

    pub(super) fn collect_asynchronous_dependencies(
        &self,
        node: &Node, /*SourceFile*/
        include_non_amd_dependencies: bool,
    ) -> AsynchronousDependencies {
        let node_as_source_file = node.as_source_file();
        let mut aliased_module_names: Vec<Gc<Node /*Expression*/>> = _d();

        let mut unaliased_module_names: Vec<Gc<Node /*Expression*/>> = _d();

        let mut import_alias_names: Vec<Gc<Node /*ParameterDeclaration*/>> = _d();

        for amd_dependency in &*node_as_source_file.amd_dependencies() {
            if let Some(amd_dependency_name) = amd_dependency.name.as_ref().non_empty() {
                aliased_module_names.push(
                    self.factory
                        .create_string_literal(amd_dependency.path.clone(), None, None)
                        .wrap(),
                );
                import_alias_names.push(
                    self.factory
                        .create_parameter_declaration(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            None,
                            Some(&**amd_dependency_name),
                            None,
                            None,
                            None,
                        )
                        .wrap(),
                );
            } else {
                unaliased_module_names.push(
                    self.factory
                        .create_string_literal(amd_dependency.path.clone(), None, None)
                        .wrap(),
                );
            }
        }

        for import_node in &self.current_module_info().external_imports {
            let external_module_name = get_external_module_name_literal(
                &self.factory,
                import_node,
                &self.current_source_file(),
                &**self.host,
                &**self.resolver,
                &self.compiler_options,
            );

            let import_alias_name = get_local_name_for_external_import(
                &self.factory,
                import_node,
                &self.current_source_file(),
            );
            if let Some(external_module_name) = external_module_name {
                if let Some(import_alias_name) =
                    import_alias_name.filter(|_| include_non_amd_dependencies)
                {
                    set_emit_flags(&*import_alias_name, EmitFlags::NoSubstitution);
                    aliased_module_names.push(external_module_name);
                    import_alias_names.push(
                        self.factory
                            .create_parameter_declaration(
                                Option::<Gc<NodeArray>>::None,
                                Option::<Gc<NodeArray>>::None,
                                None,
                                Some(import_alias_name),
                                None,
                                None,
                                None,
                            )
                            .wrap(),
                    );
                } else {
                    unaliased_module_names.push(external_module_name);
                }
            }
        }

        AsynchronousDependencies {
            aliased_module_names,
            unaliased_module_names,
            import_alias_names,
        }
    }

    pub(super) fn get_amd_import_expression_for_import(
        &self,
        node: &Node, /*ImportDeclaration | ExportDeclaration | ImportEqualsDeclaration*/
    ) -> Option<Gc<Node>> {
        if is_import_equals_declaration(node)
            || is_export_declaration(node)
            || get_external_module_name_literal(
                &self.factory,
                node,
                &self.current_source_file(),
                &**self.host,
                &**self.resolver,
                &self.compiler_options,
            )
            .is_none()
        {
            return None;
        }
        let name =
            get_local_name_for_external_import(&self.factory, node, &self.current_source_file())
                .unwrap();
        let expr = self.get_helper_expression_for_import(node, name.clone());
        if Gc::ptr_eq(&expr, &name) {
            return None;
        }
        Some(
            self.factory
                .create_expression_statement(self.factory.create_assignment(name, expr).wrap())
                .wrap(),
        )
    }

    pub(super) fn transform_asynchronous_module_body(
        &self,
        node: &Node, /*SourceFile*/
    ) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        self.context.start_lexical_environment();

        let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
        let statement_offset = self.factory.copy_prologue(
            &node_as_source_file.statements(),
            &mut statements,
            Some(self.compiler_options.no_implicit_use_strict != Some(true)),
            Some(|node: &Node| self.top_level_visitor(node)),
        );

        if self.should_emit_underscore_underscore_es_module() {
            statements.push(self.create_underscore_underscore_es_module());
        }
        if let Some(current_module_info_exported_names) = self
            .current_module_info()
            .exported_names
            .as_ref()
            .non_empty()
        {
            statements.push(
                self.factory
                    .create_expression_statement(reduce_left(
                        current_module_info_exported_names,
                        |prev: Gc<Node>, next_id: &Gc<Node>, _| {
                            self.factory
                                .create_assignment(
                                    self.factory
                                        .create_property_access_expression(
                                            self.factory.create_identifier("exports"),
                                            self.factory.create_identifier(id_text(next_id)),
                                        )
                                        .wrap(),
                                    prev,
                                )
                                .wrap()
                        },
                        self.factory.create_void_zero(),
                        None,
                        None,
                    ))
                    .wrap(),
            );
        }

        append(
            &mut statements,
            maybe_visit_node(
                self.current_module_info()
                    .external_helpers_import_declaration
                    .as_deref(),
                Some(|node: &Node| self.top_level_visitor(node)),
                Some(is_statement),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            ),
        );
        if self.module_kind == ModuleKind::AMD {
            add_range(
                &mut statements,
                Some(&map_defined(
                    Some(&self.current_module_info().external_imports),
                    |external_import: &Gc<Node>, _| {
                        self.get_amd_import_expression_for_import(external_import)
                    },
                )),
                None,
                None,
            );
        }
        add_range(
            &mut statements,
            Some(&visit_nodes(
                &node_as_source_file.statements(),
                Some(|node: &Node| self.top_level_visitor(node)),
                Some(is_statement),
                Some(statement_offset),
                None,
            )),
            None,
            None,
        );

        self.add_export_equals_if_needed(&mut statements, true);

        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.end_lexical_environment().as_deref(),
        );

        let body = self.factory.create_block(statements, Some(true)).wrap();
        if self.need_umd_dynamic_import_helper() {
            add_emit_helper(&body, dynamic_import_umd_helper());
        }

        body
    }

    pub(super) fn add_export_equals_if_needed(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        emit_as_return: bool,
    ) {
        if let Some(current_module_info_export_equals) =
            self.current_module_info().export_equals.as_ref()
        {
            let expression_result = visit_node(
                &current_module_info_export_equals
                    .as_export_assignment()
                    .expression,
                Some(|node: &Node| self.visitor(node)),
                Option::<fn(&Node) -> bool>::None,
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            );
            // if (expressionResult) {
            if emit_as_return {
                let statement = self
                    .factory
                    .create_return_statement(Some(expression_result))
                    .wrap()
                    .set_text_range(Some(&**current_module_info_export_equals))
                    .set_emit_flags(EmitFlags::NoTokenSourceMaps | EmitFlags::NoComments);
                statements.push(statement);
            } else {
                let statement = self
                    .factory
                    .create_expression_statement(
                        self.factory
                            .create_assignment(
                                self.factory
                                    .create_property_access_expression(
                                        self.factory.create_identifier("module"),
                                        "exports",
                                    )
                                    .wrap(),
                                expression_result,
                            )
                            .wrap(),
                    )
                    .wrap()
                    .set_text_range(Some(&**current_module_info_export_equals))
                    .set_emit_flags(EmitFlags::NoComments);
                statements.push(statement);
            }
            // }
        }
    }
}

impl TransformerInterface for TransformModule {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(self.transform_source_file(node))
    }
}

#[derive(Trace, Finalize)]
struct TransformModuleOnEmitNodeOverrider {
    transform_module: Gc<Box<TransformModule>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformModuleOnEmitNodeOverrider {
    fn new(
        transform_module: Gc<Box<TransformModule>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_module,
            previous_on_emit_node,
        }
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformModuleOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        _hint: EmitHint,
        _node: &Node,
        _emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformModuleOnSubstituteNodeOverrider {
    transform_module: Gc<Box<TransformModule>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformModuleOnSubstituteNodeOverrider {
    fn new(
        transform_module: Gc<Box<TransformModule>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_module,
            previous_on_substitute_node,
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider for TransformModuleOnSubstituteNodeOverrider {
    fn on_substitute_node(&self, _hint: EmitHint, _node: &Node) -> io::Result<Gc<Node>> {
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
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformModule::new(context).as_transformer(),
        )
    }
}

pub fn transform_module() -> TransformerFactory {
    Gc::new(Box::new(TransformModuleFactory::new()))
}
