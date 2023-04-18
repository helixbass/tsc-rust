use std::{cell::Cell, mem, rc::Rc};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, Trace};

use crate::{
    add_emit_helpers, create_unparsed_source_file, get_emit_module_kind, get_emit_script_target,
    get_strict_option_value, map_defined, BaseNodeFactorySynthetic, CompilerOptions,
    EmitHelperFactory, EmitHint, EmitResolver, ModuleKind, Node, NodeFactory, NodeInterface,
    ScriptTarget, SyntaxKind, TransformationContext, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, UnderscoreEscapedMap,
};

const USE_MEM_TYPE_METADATA_FORMAT: bool = false;

bitflags! {
    #[derive(Default)]
    struct TypeScriptSubstitutionFlags: u32 {
        const None = 0;
        const ClassAliases = 1 << 0;
        const NamespaceExports = 1 << 1;
        const NonQualifiedEnumMembers = 1 << 2;
    }
}

bitflags! {
    struct ClassFacts: u32 {
        const None = 0;
        const HasStaticInitializedProperties = 1 << 0;
        const HasConstructorDecorators = 1 << 1;
        const HasMemberDecorators = 1 << 2;
        const IsExportOfNamespace = 1 << 3;
        const IsNamedExternalExport = 1 << 4;
        const IsDefaultExternalExport = 1 << 5;
        const IsDerivedClass = 1 << 6;
        const UseImmediatelyInvokedFunctionExpression = 1 << 7;

        const HasAnyDecorators = Self::HasConstructorDecorators.bits | Self::HasMemberDecorators.bits;
        const NeedsName = Self::HasStaticInitializedProperties.bits | Self::HasMemberDecorators.bits;
        const MayNeedImmediatelyInvokedFunctionExpression = Self::HasAnyDecorators.bits | Self::HasStaticInitializedProperties.bits;
        const IsExported = Self::IsExportOfNamespace.bits | Self::IsDefaultExternalExport.bits | Self::IsNamedExternalExport.bits;
    }
}

#[derive(Trace, Finalize)]
struct TransformTypeScript {
    _transformer_wrapper: GcCell<Option<Transformer>>,
    context: Gc<Box<dyn TransformationContext>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    base_factory: Gc<BaseNodeFactorySynthetic>,
    resolver: Gc<Box<dyn EmitResolver>>,
    compiler_options: Gc<CompilerOptions>,
    strict_null_checks: bool,
    #[unsafe_ignore_trace]
    language_version: ScriptTarget,
    #[unsafe_ignore_trace]
    module_kind: ModuleKind,
    current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    current_namespace: GcCell<Option<Gc<Node /*ModuleDeclaration*/>>>,
    current_namespace_container_name: GcCell<Option<Gc<Node /*Identifier*/>>>,
    current_lexical_scope:
        GcCell<Option<Gc<Node /*SourceFile | Block | ModuleBlock | CaseBlock*/>>>,
    current_name_scope: GcCell<Option<Gc<Node /*ClassDeclaration*/>>>,
    current_scope_first_declarations_of_name: GcCell<Option<UnderscoreEscapedMap<Gc<Node>>>>,
    #[unsafe_ignore_trace]
    current_class_has_parameter_properties: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    enabled_substitutions: Cell<TypeScriptSubstitutionFlags>,
    class_aliases: GcCell<Option<Vec<Gc<Node /*Identifier*/>>>>,
    #[unsafe_ignore_trace]
    applicable_substitutions: Cell<TypeScriptSubstitutionFlags>,
}

impl TransformTypeScript {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let compiler_options = context.get_compiler_options();
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            factory: context.factory(),
            base_factory: context.base_factory(),
            resolver: context.get_emit_resolver(),
            strict_null_checks: get_strict_option_value(&compiler_options, "strictNullChecks"),
            language_version: get_emit_script_target(&compiler_options),
            module_kind: get_emit_module_kind(&compiler_options),
            compiler_options,
            current_source_file: Default::default(),
            context: context.clone(),
            current_namespace: Default::default(),
            current_namespace_container_name: Default::default(),
            current_lexical_scope: Default::default(),
            current_name_scope: Default::default(),
            current_scope_first_declarations_of_name: Default::default(),
            current_class_has_parameter_properties: Default::default(),
            enabled_substitutions: Default::default(),
            class_aliases: Default::default(),
            applicable_substitutions: Default::default(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformTypeScriptOnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformTypeScriptOnSubstituteNodeOverrider::new(
                downcasted.clone(),
                previous_on_substitute_node,
            )))
        });

        context.enable_substitution(SyntaxKind::PropertyAccessExpression);
        context.enable_substitution(SyntaxKind::ElementAccessExpression);

        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    fn set_current_source_file(&self, current_source_file: Option<Gc<Node>>) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    fn emit_helpers(&self) -> Rc<EmitHelperFactory> {
        self.context.get_emit_helper_factory()
    }

    fn transform_source_file_or_bundle(
        &self,
        node: &Node, /*SourceFile | Bundle*/
    ) -> Gc<Node> {
        if node.kind() == SyntaxKind::Bundle {
            return self.transform_bundle(node);
        }
        self.transform_source_file(node)
    }

    fn transform_bundle(&self, node: &Node /*Bundle*/) -> Gc<Node> {
        let node_as_bundle = node.as_bundle();
        self.factory
            .create_bundle(
                &self.base_factory,
                node_as_bundle
                    .source_files
                    .iter()
                    .map(|source_file| {
                        Some(self.transform_source_file(source_file.as_ref().unwrap()))
                    })
                    .collect::<Vec<_>>(),
                Some(map_defined(
                    Some(&node_as_bundle.prepends),
                    |prepend: &Gc<Node>, _| {
                        if prepend.kind() == SyntaxKind::InputFiles {
                            return Some(create_unparsed_source_file(
                                prepend.clone(),
                                Some("js"),
                                Option::<String>::None,
                            ));
                        }
                        Some(prepend.clone())
                    },
                )),
            )
            .wrap()
    }

    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        if node.as_source_file().is_declaration_file() {
            return node.node_wrapper();
        }

        self.set_current_source_file(Some(node.node_wrapper()));

        let visited = self.save_state_and_invoke(node, |node: &Node| self.visit_source_file(node));
        add_emit_helpers(&visited, self.context.read_emit_helpers().as_deref());

        self.set_current_source_file(None);
        visited
    }

    fn save_state_and_invoke<TReturn>(
        &self,
        node: &Node,
        f: impl FnMut(&Node) -> TReturn,
    ) -> TReturn {
        unimplemented!()
    }

    fn visit_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        unimplemented!()
    }
}

impl TransformerInterface for TransformTypeScript {
    fn call(&self, node: &Node) -> Gc<Node> {
        self.transform_source_file_or_bundle(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformTypeScriptOnEmitNodeOverrider {
    transform_type_script: Gc<Box<TransformTypeScript>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformTypeScriptOnEmitNodeOverrider {
    fn new(
        transform_type_script: Gc<Box<TransformTypeScript>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_type_script,
            previous_on_emit_node,
        }
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformTypeScriptOnEmitNodeOverrider {
    fn on_emit_node(&self, hint: EmitHint, node: &Node, emit_callback: &dyn Fn(EmitHint, &Node)) {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformTypeScriptOnSubstituteNodeOverrider {
    transform_type_script: Gc<Box<TransformTypeScript>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformTypeScriptOnSubstituteNodeOverrider {
    fn new(
        transform_type_script: Gc<Box<TransformTypeScript>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_type_script,
            previous_on_substitute_node,
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformTypeScriptOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformTypeScriptFactory {}

impl TransformTypeScriptFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformTypeScriptFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        TransformTypeScript::new(context).as_transformer()
    }
}

pub fn transform_type_script() -> TransformerFactory {
    Gc::new(Box::new(TransformTypeScriptFactory::new()))
}
