use std::{borrow::Borrow, cell::Cell, collections::HashSet, mem};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use crate::{
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface, __String,
    add_emit_helpers, chain_bundle, get_emit_script_target, is_property_access_expression,
    visit_each_child, with_synthetic_factory, BaseNodeFactorySynthetic, CompilerOptions, EmitHint,
    EmitResolver, FunctionFlags, Node, NodeArray, NodeCheckFlags, NodeFactory, NodeInterface,
    ScriptTarget, SyntaxKind, TransformFlags, TransformationContext, VisitResult,
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

    fn set_current_source_file(&self, current_source_file: Option<Gc<Node>>) {
        *self.current_source_file.borrow_mut() = current_source_file;
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

    fn maybe_captured_super_properties(&self) -> GcCellRef<Option<HashSet<String>>> {
        self.captured_super_properties.borrow()
    }

    fn maybe_captured_super_properties_mut(&self) -> GcCellRefMut<Option<HashSet<String>>> {
        self.captured_super_properties.borrow_mut()
    }

    fn has_super_element_access(&self) -> bool {
        self.has_super_element_access.get()
    }

    fn set_has_super_element_access(&self, has_super_element_access: bool) {
        self.has_super_element_access.set(has_super_element_access);
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

    fn visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, false)
    }

    fn visitor_with_unused_expression_result(&self, node: &Node) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, true)
    }

    fn visitor_no_async_modifier(&self, node: &Node) -> VisitResult /*<Node>*/ {
        if node.kind() == SyntaxKind::AsyncKeyword {
            return None;
        }
        Some(node.node_wrapper().into())
    }

    fn do_with_hierarchy_facts<TValue, TReturn>(
        &self,
        mut cb: impl FnMut(&TValue) -> TReturn,
        value: &TValue,
        exclude_facts: HierarchyFacts,
        include_facts: HierarchyFacts,
    ) -> TReturn {
        if self.affects_subtree(exclude_facts, include_facts) {
            let ancestor_facts = self.enter_subtree(exclude_facts, include_facts);
            let result = cb(value);
            self.exit_subtree(ancestor_facts);
            return result;
        }
        cb(value)
    }

    fn visit_default(&self, node: &Node) -> VisitResult /*<Node>*/ {
        visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
            Option::<
                fn(
                    Option<&NodeArray>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<usize>,
                    Option<usize>,
                ) -> Option<Gc<NodeArray>>,
            >::None,
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .map(Into::into)
    }

    fn visitor_worker(&self, node: &Node, expression_result_is_unused: bool) -> VisitResult /*<Node>*/
    {
        if !node
            .transform_flags()
            .intersects(TransformFlags::ContainsES2018)
        {
            return Some(node.node_wrapper().into());
        }
        match node.kind() {
            SyntaxKind::AwaitExpression => Some(self.visit_await_expression(node).into()),
            SyntaxKind::YieldExpression => self.visit_yield_expression(node),
            SyntaxKind::ReturnStatement => self.visit_return_statement(node),
            SyntaxKind::LabeledStatement => self.visit_labeled_statement(node),
            SyntaxKind::ObjectLiteralExpression => {
                Some(self.visit_object_literal_expression(node).into())
            }
            SyntaxKind::BinaryExpression => Some(
                self.visit_binary_expression(node, expression_result_is_unused)
                    .into(),
            ),
            SyntaxKind::CommaListExpression => Some(
                self.visit_comma_list_expression(node, expression_result_is_unused)
                    .into(),
            ),
            SyntaxKind::CatchClause => self.visit_catch_clause(node),
            SyntaxKind::VariableStatement => self.visit_variable_statement(node),
            SyntaxKind::VariableDeclaration => self.visit_variable_declaration(node),
            SyntaxKind::DoStatement | SyntaxKind::WhileStatement | SyntaxKind::ForInStatement => {
                self.do_with_hierarchy_facts(
                    |node: &Node| self.visit_default(node),
                    node,
                    HierarchyFacts::IterationStatementExcludes,
                    HierarchyFacts::IterationStatementIncludes,
                )
            }
            SyntaxKind::ForOfStatement => self.visit_for_of_statement(node, Option::<&Node>::None),
            SyntaxKind::ForStatement => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_for_statement(node),
                node,
                HierarchyFacts::IterationStatementExcludes,
                HierarchyFacts::IterationStatementIncludes,
            ),
            SyntaxKind::VoidExpression => self.visit_void_expression(node),
            SyntaxKind::Constructor => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_constructor_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::MethodDeclaration => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_method_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::GetAccessor => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_get_accessor_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::SetAccessor => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_set_accessor_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::FunctionDeclaration => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_function_declaration(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::FunctionExpression => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_function_expression(node),
                node,
                HierarchyFacts::ClassOrFunctionExcludes,
                HierarchyFacts::ClassOrFunctionIncludes,
            ),
            SyntaxKind::ArrowFunction => self.do_with_hierarchy_facts(
                |node: &Node| self.visit_arrow_function(node),
                node,
                HierarchyFacts::ArrowFunctionExcludes,
                HierarchyFacts::ArrowFunctionIncludes,
            ),
            SyntaxKind::Parameter => Some(self.visit_parameter(node).into()),
            SyntaxKind::ExpressionStatement => Some(self.visit_expression_statement(node).into()),
            SyntaxKind::ParenthesizedExpression => Some(
                self.visit_parenthesized_expression(node, expression_result_is_unused)
                    .into(),
            ),
            SyntaxKind::TaggedTemplateExpression => self.visit_tagged_template_expression(node),
            SyntaxKind::PropertyAccessExpression => {
                if let Some(captured_super_properties) =
                    self.maybe_captured_super_properties_mut().as_mut()
                {
                    if is_property_access_expression(node)
                        && node.as_property_access_expression().expression.kind()
                            == SyntaxKind::SuperKeyword
                    {
                        captured_super_properties.insert(
                            node.as_property_access_expression()
                                .name
                                .as_member_name()
                                .escaped_text()
                                .to_owned(),
                        );
                    }
                }
                visit_each_child(
                    Some(node),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    Option::<
                        fn(
                            Option<&NodeArray>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<usize>,
                            Option<usize>,
                        ) -> Option<Gc<NodeArray>>,
                    >::None,
                    Option::<fn(&Node) -> VisitResult>::None,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
                )
                .map(Into::into)
            }
            SyntaxKind::ElementAccessExpression => {
                if self.maybe_captured_super_properties().is_some() {
                    if node.as_element_access_expression().expression.kind()
                        == SyntaxKind::SuperKeyword
                    {
                        self.set_has_super_element_access(true);
                    }
                }
                visit_each_child(
                    Some(node),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    Option::<
                        fn(
                            Option<&NodeArray>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<usize>,
                            Option<usize>,
                        ) -> Option<Gc<NodeArray>>,
                    >::None,
                    Option::<fn(&Node) -> VisitResult>::None,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
                )
                .map(Into::into)
            }
            SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => self
                .do_with_hierarchy_facts(
                    |node: &Node| self.visit_default(node),
                    node,
                    HierarchyFacts::ClassOrFunctionExcludes,
                    HierarchyFacts::ClassOrFunctionIncludes,
                ),
            _ => visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
                Option::<fn(&Node) -> VisitResult>::None,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
            .map(Into::into),
        }
    }

    fn visit_await_expression(
        &self,
        node: &Node, /*AwaitExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    fn visit_yield_expression(&self, node: &Node /*YieldExpression*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_return_statement(&self, node: &Node /*ReturnStatement*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_labeled_statement(&self, node: &Node /*LabeledStatement*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    fn visit_expression_statement(
        &self,
        node: &Node, /*ExpressionStatement*/
    ) -> Gc<Node /*ExpressionStatement*/> {
        unimplemented!()
    }

    fn visit_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
        expression_result_is_unused: bool,
    ) -> Gc<Node /*ParenthesizedExpression*/> {
        unimplemented!()
    }

    fn visit_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node /*SourceFile*/> {
        unimplemented!()
    }

    fn visit_tagged_template_expression(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    fn visit_binary_expression(
        &self,
        node: &Node, /*BinaryExpression*/
        expression_result_is_unused: bool,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    fn visit_comma_list_expression(
        &self,
        node: &Node, /*CommaListExpression*/
        expression_result_is_unused: bool,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    fn visit_catch_clause(&self, node: &Node /*CatchClause*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_variable_statement(&self, node: &Node /*VariableStatement*/) -> VisitResult /*<VariableStatement>*/
    {
        unimplemented!()
    }

    fn visit_variable_declaration(&self, node: &Node /*VariableDeclaration*/) -> VisitResult /*<VariableDeclaration>*/
    {
        unimplemented!()
    }

    fn visit_for_statement(&self, node: &Node /*ForStatement*/) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    fn visit_void_expression(&self, node: &Node /*VoidExpression*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_for_of_statement(
        &self,
        node: &Node, /*ForOfStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    fn visit_parameter(
        &self,
        node: &Node, /*ParameterDeclaration*/
    ) -> Gc<Node /*ParameterDeclaration*/> {
        unimplemented!()
    }

    fn visit_constructor_declaration(
        &self,
        node: &Node, /*ConstructorDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    fn visit_get_accessor_declaration(
        &self,
        node: &Node, /*GetAccessorDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    fn visit_set_accessor_declaration(
        &self,
        node: &Node, /*SetAccessorDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    fn visit_method_declaration(&self, node: &Node /*MethodDeclaration*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_function_declaration(&self, node: &Node /*FunctionDeclaration*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_arrow_function(&self, node: &Node /*ArrowFunction*/) -> VisitResult {
        unimplemented!()
    }

    fn visit_function_expression(&self, node: &Node /*FunctionExpression*/) -> VisitResult {
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
