use std::{cell::Cell, collections::HashSet, mem};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRefMut, Trace};

use crate::{
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface, __String,
    add_emit_helpers, chain_bundle, get_emit_script_target, with_synthetic_factory,
    BaseNodeFactorySynthetic, CompilerOptions, EmitHint, EmitResolver, FunctionFlags, Node,
    NodeCheckFlags, NodeFactory, NodeInterface, ScriptTarget, TransformationContext,
};

bitflags! {
    struct ESNextSubstitutionFlags: u32 {
        const None = 0;
        const AsyncMethodsWithSuper = 1 << 0;
    }
}

bitflags! {
    #[derive(Default)]
    struct HierarchyFacts: u32 {
        const None = 0;

        const HasLexicalThis = 1 << 0;
        const IterationContainer = 1 << 1;

        const AncestorFactsMask = (Self::IterationContainer.bits << 1) - 1;

        const SourceFileIncludes = Self::HasLexicalThis.bits;
        const SourceFileExcludes = Self::IterationContainer.bits;
        const StrictModeSourceFileIncludes = Self::None.bits;

        const ClassOrFunctionIncludes = Self::HasLexicalThis.bits;
        const ClassOrFunctionExcludes = Self::IterationContainer.bits;

        const ArrowFunctionIncludes = Self::None.bits;
        const ArrowFunctionExcludes = Self::ClassOrFunctionExcludes.bits;

        const IterationStatementIncludes = Self::IterationContainer.bits;
        const IterationStatementExcludes = Self::None.bits;
    }
}

#[derive(Trace, Finalize)]
struct TransformES2018 {
    _transformer_wrapper: GcCell<Option<Transformer>>,
    context: Gc<Box<dyn TransformationContext>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    resolver: Gc<Box<dyn EmitResolver>>,
    compiler_options: Gc<CompilerOptions>,
    #[unsafe_ignore_trace]
    language_version: ScriptTarget,
    #[unsafe_ignore_trace]
    exported_variable_statement: Cell<bool>,
    #[unsafe_ignore_trace]
    enabled_substitutions: Cell<Option<ESNextSubstitutionFlags>>,
    #[unsafe_ignore_trace]
    enclosing_function_flags: Cell<Option<FunctionFlags>>,
    #[unsafe_ignore_trace]
    enclosing_super_container_flags: Cell<NodeCheckFlags>,
    #[unsafe_ignore_trace]
    hierarchy_facts: Cell<HierarchyFacts>,
    current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    tagged_template_string_declarations: GcCell<Option<Vec<Gc<Node /*VariableDeclaration*/>>>>,
    captured_super_properties: GcCell<Option<HashSet<__String>>>,
    #[unsafe_ignore_trace]
    has_super_element_access: Cell<bool>,
    substituted_super_accessors: GcCell<Vec<bool>>,
}

impl TransformES2018 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let compiler_options = context.get_compiler_options();
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            factory: context.factory(),
            resolver: context.get_emit_resolver(),
            language_version: get_emit_script_target(&compiler_options),
            compiler_options,
            context: context.clone(),
            exported_variable_statement: Default::default(),
            enabled_substitutions: Default::default(),
            enclosing_function_flags: Default::default(),
            enclosing_super_container_flags: Default::default(),
            hierarchy_facts: Default::default(),
            current_source_file: Default::default(),
            tagged_template_string_declarations: Default::default(),
            captured_super_properties: Default::default(),
            has_super_element_access: Default::default(),
            substituted_super_accessors: Default::default(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformES2018OnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformES2018OnSubstituteNodeOverrider::new(
                downcasted.clone(),
                previous_on_substitute_node,
            )))
        });
        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    fn hierarchy_facts(&self) -> HierarchyFacts {
        self.hierarchy_facts.get()
    }

    fn set_hierarchy_facts(&self, hierarchy_facts: HierarchyFacts) {
        self.hierarchy_facts.set(hierarchy_facts);
    }

    fn maybe_tagged_template_string_declarations_mut(&self) -> GcCellRefMut<Option<Vec<Gc<Node>>>> {
        self.tagged_template_string_declarations.borrow_mut()
    }

    fn set_tagged_template_string_declarations(
        &self,
        tagged_template_string_declarations: Option<Vec<Gc<Node>>>,
    ) {
        *self.tagged_template_string_declarations.borrow_mut() =
            tagged_template_string_declarations;
    }

    fn set_current_source_file(&self, current_source_file: Option<Gc<Node>>) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    fn affects_subtree(
        &self,
        exclude_facts: HierarchyFacts,
        include_facts: HierarchyFacts,
    ) -> bool {
        self.hierarchy_facts() != (self.hierarchy_facts() & !exclude_facts | include_facts)
    }

    fn enter_subtree(
        &self,
        exclude_facts: HierarchyFacts,
        include_facts: HierarchyFacts,
    ) -> HierarchyFacts {
        let ancestor_facts = self.hierarchy_facts();
        self.set_hierarchy_facts(
            (self.hierarchy_facts() & !exclude_facts | include_facts)
                & HierarchyFacts::AncestorFactsMask,
        );
        ancestor_facts
    }

    fn exit_subtree(&self, ancestor_facts: HierarchyFacts) {
        self.set_hierarchy_facts(ancestor_facts);
    }

    fn record_tagged_template_string(&self, temp: &Node /*Identifier*/) {
        self.maybe_tagged_template_string_declarations_mut()
            .get_or_insert_with(|| Default::default())
            .push(with_synthetic_factory(|synthetic_factory_| {
                self.factory
                    .create_variable_declaration(
                        synthetic_factory_,
                        Some(temp.node_wrapper()),
                        None,
                        None,
                        None,
                    )
                    .into()
            }));
    }

    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        if node_as_source_file.is_declaration_file() {
            return node.node_wrapper();
        }

        self.set_current_source_file(Some(node.node_wrapper()));
        let visited = self.visit_source_file(node);
        add_emit_helpers(&visited, self.context.read_emit_helpers().as_deref());

        self.set_current_source_file(None);
        self.set_tagged_template_string_declarations(None);
        visited
    }

    fn visit_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node /*SourceFile*/> {
        unimplemented!()
    }
}

impl TransformerInterface for TransformES2018 {
    fn call(&self, node: &Node) -> Gc<Node> {
        self.transform_source_file(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformES2018OnEmitNodeOverrider {
    transform_es2018: Gc<Box<TransformES2018>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformES2018OnEmitNodeOverrider {
    fn new(
        transform_es2018: Gc<Box<TransformES2018>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es2018,
            previous_on_emit_node,
        }
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformES2018OnEmitNodeOverrider {
    fn on_emit_node(&self, hint: EmitHint, node: &Node, emit_callback: &dyn Fn(EmitHint, &Node)) {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2018OnSubstituteNodeOverrider {
    transform_es2018: Gc<Box<TransformES2018>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformES2018OnSubstituteNodeOverrider {
    fn new(
        transform_es2018: Gc<Box<TransformES2018>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es2018,
            previous_on_substitute_node,
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider for TransformES2018OnSubstituteNodeOverrider {
    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2018Factory {}

impl TransformES2018Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2018Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformES2018::new(context).as_transformer(),
        )
    }
}

pub fn transform_es2018() -> TransformerFactory {
    Gc::new(Box::new(TransformES2018Factory::new()))
}
