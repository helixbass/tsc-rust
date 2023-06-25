use std::{collections::HashMap, io, mem, rc::Rc};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use crate::{
    BaseNodeFactorySynthetic, CompilerOptions, EmitResolver, Node, NodeFactory, ScriptTarget,
    TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface, _d, chain_bundle, get_emit_script_target, EmitHelperFactory, EmitHint,
    SyntaxKind, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider,
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

    fn transform_source_file(&self, _node: &Node /*SourceFile*/) -> Gc<Node> {
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
