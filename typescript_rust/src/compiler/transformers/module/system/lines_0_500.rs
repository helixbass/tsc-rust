use std::{collections::HashMap, io, mem};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use crate::{
    chain_bundle, BaseNodeFactorySynthetic, CompilerOptions, EmitHelperBase, EmitHint, EmitHost,
    EmitResolver, ExternalModuleInfo, HasStatementsInterface, Node, NodeArrayExt, NodeExt,
    NodeFactory, NodeId, NodeInterface, NonEmpty, SyntaxKind, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface, _d,
    collect_external_module_info, gc_cell_ref_mut_unwrapped, get_original_node_id,
    is_effective_external_module, map, move_emit_helpers, out_file, try_get_module_name_from_file,
    EmitFlags, EmitHelper, NodeArray, TransformFlags,
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
    pub(super) no_substitution_map: GcCell<HashMap<NodeId, Vec<bool>>>,
    pub(super) context_object_map: GcCell<HashMap<NodeId, Gc<Node /*Identifier*/>>>,
    pub(super) current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    pub(super) module_info: GcCell<Option<Gc<ExternalModuleInfo>>>,
    pub(super) export_function: GcCell<Option<Gc<Node /*Identifier*/>>>,
    pub(super) context_object: GcCell<Option<Gc<Node /*Identifier*/>>>,
    pub(super) hoisted_statements: GcCell<Option<Vec<Gc<Node /*Statement*/>>>>,
    pub(super) enclosing_block_scoped_container: GcCell<Option<Gc<Node>>>,
    pub(super) no_substitution: GcCell<Option<Vec<bool>>>,
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

    pub(super) fn set_module_info_map(
        &self,
        module_info_map: HashMap<NodeId, Gc<ExternalModuleInfo>>,
    ) {
        *self.module_info_map.borrow_mut() = module_info_map;
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

    pub(super) fn set_deferred_exports(
        &self,
        deferred_exports: HashMap<NodeId, Option<Vec<Gc<Node /*Statement*/>>>>,
    ) {
        *self.deferred_exports.borrow_mut() = deferred_exports;
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

    pub(super) fn set_export_functions_map(
        &self,
        export_functions_map: HashMap<NodeId, Gc<Node /*Identifier*/>>,
    ) {
        *self.export_functions_map.borrow_mut() = export_functions_map;
    }

    pub(super) fn no_substitution_map(&self) -> GcCellRef<HashMap<NodeId, Vec<bool>>> {
        self.no_substitution_map.borrow()
    }

    pub(super) fn no_substitution_map_mut(&self) -> GcCellRefMut<HashMap<NodeId, Vec<bool>>> {
        self.no_substitution_map.borrow_mut()
    }

    pub(super) fn set_no_substitution_map(&self, no_substitution_map: HashMap<NodeId, Vec<bool>>) {
        *self.no_substitution_map.borrow_mut() = no_substitution_map;
    }

    pub(super) fn context_object_map(&self) -> GcCellRef<HashMap<NodeId, Gc<Node /*Identifier*/>>> {
        self.context_object_map.borrow()
    }

    pub(super) fn context_object_map_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, Gc<Node /*Identifier*/>>> {
        self.context_object_map.borrow_mut()
    }

    pub(super) fn set_context_object_map(
        &self,
        context_object_map: HashMap<NodeId, Gc<Node /*Identifier*/>>,
    ) {
        *self.context_object_map.borrow_mut() = context_object_map;
    }

    pub(super) fn maybe_current_source_file(&self) -> Option<Gc<Node /*SourceFile*/>> {
        self.current_source_file.borrow().clone()
    }

    pub(super) fn maybe_current_source_file_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node /*SourceFile*/>>> {
        self.current_source_file.borrow_mut()
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

    pub(super) fn maybe_module_info_mut(&self) -> GcCellRefMut<Option<Gc<ExternalModuleInfo>>> {
        self.module_info.borrow_mut()
    }

    pub(super) fn module_info_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<ExternalModuleInfo>>, Gc<ExternalModuleInfo>> {
        gc_cell_ref_mut_unwrapped(&self.module_info)
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

    pub(super) fn maybe_export_function_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node /*Identifier*/>>> {
        self.export_function.borrow_mut()
    }

    pub(super) fn export_function_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node /*Identifier*/>>, Gc<Node /*Identifier*/>> {
        gc_cell_ref_mut_unwrapped(&self.export_function)
    }

    pub(super) fn set_export_function(&self, export_function: Option<Gc<Node /*Identifier*/>>) {
        *self.export_function.borrow_mut() = export_function;
    }

    pub(super) fn maybe_context_object(&self) -> Option<Gc<Node /*Identifier*/>> {
        self.context_object.borrow().clone()
    }

    pub(super) fn maybe_context_object_mut(&self) -> GcCellRefMut<Option<Gc<Node /*Identifier*/>>> {
        self.context_object.borrow_mut()
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

    pub(super) fn maybe_enclosing_block_scoped_container_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node>>> {
        self.enclosing_block_scoped_container.borrow_mut()
    }

    pub(super) fn set_enclosing_block_scoped_container(
        &self,
        enclosing_block_scoped_container: Option<Gc<Node>>,
    ) {
        *self.enclosing_block_scoped_container.borrow_mut() = enclosing_block_scoped_container;
    }

    pub(super) fn maybe_no_substitution(&self) -> GcCellRef<Option<Vec<bool>>> {
        self.no_substitution.borrow()
    }

    pub(super) fn maybe_no_substitution_mut(&self) -> GcCellRefMut<Option<Vec<bool>>> {
        self.no_substitution.borrow_mut()
    }

    pub(super) fn set_no_substitution(&self, no_substitution: Option<Vec<bool>>) {
        *self.no_substitution.borrow_mut() = no_substitution;
    }

    pub(super) fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        if node_as_source_file.is_declaration_file()
            || !(is_effective_external_module(node, &self.compiler_options)
                || node
                    .transform_flags()
                    .intersects(TransformFlags::ContainsDynamicImport))
        {
            return node.node_wrapper();
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
        let module_body_block = self.create_system_module_body(node, &dependency_groups);
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
        updated
    }

    pub(super) fn collect_dependency_groups(
        &self,
        _external_imports: &[Gc<
            Node, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
        >],
    ) -> Vec<DependencyGroup> {
        unimplemented!()
    }

    pub(super) fn create_system_module_body(
        &self,
        _node: &Node, /*SourceFile*/
        _dependency_groups: &[DependencyGroup],
    ) -> Gc<Node> {
        unimplemented!()
    }
}

impl TransformerInterface for TransformSystemModule {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(self.transform_source_file(node))
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
        _hint: EmitHint,
        _node: &Node,
        _emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        unimplemented!()
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
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformSystemModuleOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, _hint: EmitHint, _node: &Node) -> io::Result<Gc<Node>> {
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
