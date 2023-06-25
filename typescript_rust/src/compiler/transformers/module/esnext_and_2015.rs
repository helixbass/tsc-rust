use std::{collections::HashMap, io, mem, rc::Rc};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use crate::{
    BaseNodeFactorySynthetic, CompilerOptions, EmitResolver, Node, NodeFactory, ScriptTarget,
    TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface, _d, chain_bundle, create_empty_exports,
    create_external_helpers_import_declaration_if_needed, get_emit_script_target,
    insert_statements_after_custom_prologue, is_external_module, is_external_module_indicator,
    is_statement, visit_each_child, visit_nodes, BoolExt, EmitHelperFactory, EmitHint,
    HasStatementsInterface, NodeArrayExt, NodeInterface, SyntaxKind,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    VecExt, VisitResult,
};

#[derive(Trace, Finalize)]
struct TransformEcmascriptModule {
    _transformer_wrapper: GcCell<Option<Transformer>>,
    context: Gc<Box<dyn TransformationContext>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    resolver: Gc<Box<dyn EmitResolver>>,
    compiler_options: Gc<CompilerOptions>,
    #[unsafe_ignore_trace]
    language_version: ScriptTarget,
    helper_name_substitutions: GcCell<Option<HashMap<String, Gc<Node /*Identifier*/>>>>,
    current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    import_require_statements: GcCell<
        Option<(
            Gc<Node /*ImportDeclaration*/>,
            Gc<Node /*VariableStatement*/>,
        )>,
    >,
}

impl TransformEcmascriptModule {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let compiler_options = context.get_compiler_options();
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: _d(),
            factory: context.factory(),
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

    fn emit_helpers(&self) -> Rc<EmitHelperFactory> {
        self.context.get_emit_helper_factory()
    }

    fn maybe_helper_name_substitutions(
        &self,
    ) -> GcCellRef<Option<HashMap<String, Gc<Node /*Identifier*/>>>> {
        self.helper_name_substitutions.borrow()
    }

    fn maybe_helper_name_substitutions_mut(
        &self,
    ) -> GcCellRefMut<Option<HashMap<String, Gc<Node /*Identifier*/>>>> {
        self.helper_name_substitutions.borrow_mut()
    }

    fn set_helper_name_substitutions(
        &self,
        helper_name_substitutions: Option<HashMap<String, Gc<Node /*Identifier*/>>>,
    ) {
        *self.helper_name_substitutions.borrow_mut() = helper_name_substitutions;
    }

    fn maybe_current_source_file(&self) -> Option<Gc<Node /*SourceFile*/>> {
        self.current_source_file.borrow().clone()
    }

    fn maybe_current_source_file_mut(&self) -> GcCellRefMut<Option<Gc<Node /*SourceFile*/>>> {
        self.current_source_file.borrow_mut()
    }

    fn set_current_source_file(&self, current_source_file: Option<Gc<Node /*SourceFile*/>>) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    fn maybe_import_require_statements(
        &self,
    ) -> Option<(
        Gc<Node /*ImportDeclaration*/>,
        Gc<Node /*VariableStatement*/>,
    )> {
        self.import_require_statements.borrow().clone()
    }

    fn maybe_import_require_statements_mut(
        &self,
    ) -> GcCellRefMut<
        Option<(
            Gc<Node /*ImportDeclaration*/>,
            Gc<Node /*VariableStatement*/>,
        )>,
    > {
        self.import_require_statements.borrow_mut()
    }

    fn set_import_require_statements(
        &self,
        import_require_statements: Option<(
            Gc<Node /*ImportDeclaration*/>,
            Gc<Node /*VariableStatement*/>,
        )>,
    ) {
        *self.import_require_statements.borrow_mut() = import_require_statements;
    }

    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        if node_as_source_file.is_declaration_file() {
            return node.node_wrapper();
        }

        if is_external_module(node) || self.compiler_options.isolated_modules == Some(true) {
            self.set_current_source_file(Some(node.node_wrapper()));
            self.set_import_require_statements(None);
            let mut result = self.update_external_module(node);
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
                    .any(|statement| is_external_module_indicator(statement))
            {
                return result;
            }
            return self.factory.update_source_file(
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
            );
        }

        node.node_wrapper()
    }

    fn update_external_module(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
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
        match external_helpers_import_declaration {
            Some(external_helpers_import_declaration) => {
                let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
                let statement_offset = self.factory.copy_prologue(
                    &node_as_source_file.statements(),
                    &mut statements,
                    None,
                    Option::<fn(&Node) -> VisitResult>::None,
                );
                statements.push(external_helpers_import_declaration);
                statements.extend(
                    visit_nodes(
                        &node_as_source_file.statements(),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_statement),
                        Some(statement_offset),
                        None,
                    )
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
            None => visit_each_child(node, |node: &Node| self.visitor(node), &**self.context),
        }
    }

    fn visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        match node.kind() {
            SyntaxKind::ImportEqualsDeclaration => (get_emit_script_target(&self.compiler_options)
                // TODO: this definitely looks like an upstream bug of using ModuleKind instead
                // of ScriptTarget - technically should say ScriptTarget::ES2019 here to be using
                // the same exact enum int value but let's see if this causes any problems
                >= ScriptTarget::ES2020)
                .then_and(|| self.visit_import_equals_declaration(node)),
            SyntaxKind::ExportAssignment => self.visit_export_assignment(node),
            SyntaxKind::ExportDeclaration => {
                let export_decl = node;
                self.visit_export_declaration(export_decl)
            }
            _ => Some(node.node_wrapper().into()),
        }
    }

    fn visit_import_equals_declaration(
        &self,
        _node: &Node, /*ImportEqualsDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    fn visit_export_assignment(&self, _node: &Node /*ExportAssignment*/) -> VisitResult /*<ExportAssignment>*/
    {
        unimplemented!()
    }

    fn visit_export_declaration(&self, _node: &Node /*ExportDeclaration*/) -> VisitResult {
        unimplemented!()
    }
}

impl TransformerInterface for TransformEcmascriptModule {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(self.transform_source_file(node))
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
        _hint: EmitHint,
        _node: &Node,
        _emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        unimplemented!()
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
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformEcmascriptModuleOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, _hint: EmitHint, _node: &Node) -> io::Result<Gc<Node>> {
        unimplemented!()
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
