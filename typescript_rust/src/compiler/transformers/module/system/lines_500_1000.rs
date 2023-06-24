use gc::Gc;

use super::TransformSystemModule;
use crate::{
    Node, NodeInterface, SyntaxKind, VisitResult, _d, flatten_destructuring_assignment,
    get_emit_flags, get_local_name_for_external_import, get_original_node, get_original_node_id,
    has_syntactic_modifier, is_binding_pattern, is_block, is_class_element, is_decorator,
    is_expression, is_external_module_import_equals_declaration, is_heritage_clause, is_modifier,
    is_omitted_expression, is_parameter_declaration, is_statement, maybe_visit_node,
    maybe_visit_nodes, single_or_many_node, visit_each_child, visit_node, visit_nodes,
    ClassLikeDeclarationInterface, Debug_, EmitFlags, FlattenLevel,
    FunctionLikeDeclarationInterface, GetOrInsertDefault, HasInitializerInterface,
    InterfaceOrClassLikeDeclarationInterface, ModifierFlags, NamedDeclarationInterface, NodeArray,
    NodeExt, NodeFlags, ReadonlyTextRange, SignatureDeclarationInterface,
};

impl TransformSystemModule {
    pub(super) fn top_level_visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        match node.kind() {
            SyntaxKind::ImportDeclaration => self.visit_import_declaration(node),
            SyntaxKind::ImportEqualsDeclaration => self.visit_import_equals_declaration(node),
            SyntaxKind::ExportDeclaration => self.visit_export_declaration(node),
            SyntaxKind::ExportAssignment => self.visit_export_assignment(node),
            _ => self.top_level_nested_visitor(node),
        }
    }

    pub(super) fn visit_import_declaration(
        &self,
        node: &Node, /*ImportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_import_declaration = node.as_import_declaration();
        let mut statements: Option<Vec<Gc<Node /*Statement*/>>> = _d();
        if node_as_import_declaration.import_clause.is_some() {
            self.context.hoist_variable_declaration(
                &get_local_name_for_external_import(
                    &self.factory,
                    node,
                    &self.current_source_file(),
                )
                .unwrap(),
            );
        }

        if self.has_associated_end_of_declaration_marker(node) {
            let id = get_original_node_id(node);
            self.append_exports_of_import_declaration(
                self.deferred_exports_mut().entry(id).or_default(),
                node,
            );
        } else {
            self.append_exports_of_import_declaration(&mut statements, node);
        }

        statements.map(single_or_many_node)
    }

    pub(super) fn visit_export_declaration(
        &self,
        _node: &Node, /*ExportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        // Debug.assertIsDefined(node);
        None
    }

    pub(super) fn visit_import_equals_declaration(
        &self,
        node: &Node, /*ImportEqualsDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        Debug_.assert(
            is_external_module_import_equals_declaration(node),
            Some("import= for internal module references should be handled in an earlier transformer.")
        );

        let mut statements: Option<Vec<Gc<Node /*Statement*/>>> = _d();
        self.context.hoist_variable_declaration(
            &get_local_name_for_external_import(&self.factory, node, &self.current_source_file())
                .unwrap(),
        );

        if self.has_associated_end_of_declaration_marker(node) {
            let id = get_original_node_id(node);
            self.append_exports_of_import_equals_declaration(
                self.deferred_exports_mut().entry(id).or_default(),
                node,
            );
        } else {
            self.append_exports_of_import_equals_declaration(&mut statements, node);
        }

        statements.map(single_or_many_node)
    }

    pub(super) fn visit_export_assignment(
        &self,
        node: &Node, /*ExportAssignment*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_export_assignment = node.as_export_assignment();
        if node_as_export_assignment.is_export_equals == Some(true) {
            return None;
        }

        let expression = visit_node(
            &node_as_export_assignment.expression,
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        );
        let original = node.maybe_original();
        match original {
            Some(ref original) if self.has_associated_end_of_declaration_marker(original) => {
                let id = get_original_node_id(node);
                self.append_export_statement(
                    self.deferred_exports_mut().entry(id).or_default(),
                    &self.factory.create_identifier("default"),
                    &expression,
                    Some(true),
                );
                None
            }
            _ => Some(
                self.create_export_statement(
                    &self.factory.create_identifier("default"),
                    &expression,
                    Some(true),
                )
                .into(),
            ),
        }
    }

    pub(super) fn visit_function_declaration(
        &self,
        node: &Node, /*FunctionDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_function_declaration = node.as_function_declaration();
        if has_syntactic_modifier(node, ModifierFlags::Export) {
            self.maybe_hoisted_statements_mut()
                .get_or_insert_default_()
                .push(
                    self.factory.update_function_declaration(
                        node,
                        node.maybe_decorators(),
                        maybe_visit_nodes(
                            node.maybe_modifiers().as_deref(),
                            Some(|node: &Node| self.modifier_visitor(node)),
                            Some(is_modifier),
                            None,
                            None,
                        ),
                        node_as_function_declaration.maybe_asterisk_token(),
                        Some(
                            self.factory
                                .get_declaration_name(Some(node), Some(true), Some(true)),
                        ),
                        Option::<Gc<NodeArray>>::None,
                        visit_nodes(
                            &node_as_function_declaration.parameters(),
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_parameter_declaration),
                            None,
                            None,
                        ),
                        None,
                        maybe_visit_node(
                            node_as_function_declaration.maybe_body(),
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_block),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        ),
                    ),
                );
        } else {
            self.maybe_hoisted_statements_mut()
                .get_or_insert_default_()
                .push(visit_each_child(
                    node,
                    |node: &Node| self.visitor(node),
                    &**self.context,
                ));
        }

        if self.has_associated_end_of_declaration_marker(node) {
            let id = get_original_node_id(node);
            self.append_exports_of_hoisted_declaration(
                self.deferred_exports_mut().entry(id).or_default(),
                node,
            );
        } else {
            self.append_exports_of_hoisted_declaration(
                &mut self.maybe_hoisted_statements_mut(),
                node,
            );
        }

        None
    }

    pub(super) fn visit_class_declaration(
        &self,
        node: &Node, /*ClassDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_class_declaration = node.as_class_declaration();
        let mut statements: Option<Vec<Gc<Node /*Statement*/>>> = _d();

        let name = self.factory.get_local_name(node, None, None);
        self.context.hoist_variable_declaration(&name);

        statements.get_or_insert_default_().push(
            self.factory
                .create_expression_statement(
                    self.factory.create_assignment(
                        name,
                        self.factory
                            .create_class_expression(
                                maybe_visit_nodes(
                                    node.maybe_decorators().as_deref(),
                                    Some(|node: &Node| self.visitor(node)),
                                    Some(is_decorator),
                                    None,
                                    None,
                                ),
                                Option::<Gc<NodeArray>>::None,
                                node_as_class_declaration.maybe_name(),
                                Option::<Gc<NodeArray>>::None,
                                maybe_visit_nodes(
                                    node_as_class_declaration
                                        .maybe_heritage_clauses()
                                        .as_deref(),
                                    Some(|node: &Node| self.visitor(node)),
                                    Some(is_heritage_clause),
                                    None,
                                    None,
                                ),
                                visit_nodes(
                                    &node_as_class_declaration.members(),
                                    Some(|node: &Node| self.visitor(node)),
                                    Some(is_class_element),
                                    None,
                                    None,
                                ),
                            )
                            .set_text_range(Some(node)),
                    ),
                )
                .set_text_range(Some(node)),
        );

        if self.has_associated_end_of_declaration_marker(node) {
            let id = get_original_node_id(node);
            self.append_exports_of_hoisted_declaration(
                self.deferred_exports_mut().entry(id).or_default(),
                node,
            );
        } else {
            self.append_exports_of_hoisted_declaration(&mut statements, node);
        }

        statements.map(single_or_many_node)
    }

    pub(super) fn visit_variable_statement(
        &self,
        node: &Node, /*VariableStatement*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_variable_statement = node.as_variable_statement();
        if !self
            .should_hoist_variable_declaration_list(&node_as_variable_statement.declaration_list)
        {
            return Some(
                visit_node(
                    node,
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_statement),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
                .into(),
            );
        }

        let mut expressions: Option<Vec<Gc<Node /*Expression*/>>> = _d();
        let is_exported_declaration = has_syntactic_modifier(node, ModifierFlags::Export);
        let is_marked_declaration = self.has_associated_end_of_declaration_marker(node);
        for variable in &node_as_variable_statement
            .declaration_list
            .as_variable_declaration_list()
            .declarations
        {
            let variable_as_variable_declaration = variable.as_variable_declaration();
            if variable_as_variable_declaration
                .maybe_initializer()
                .is_some()
            {
                expressions
                    .get_or_insert_default_()
                    .push(self.transform_initialized_variable(
                        variable,
                        is_exported_declaration && !is_marked_declaration,
                    ));
            } else {
                self.hoist_binding_element(variable);
            }
        }

        let mut statements: Option<Vec<Gc<Node /*Statement*/>>> = _d();
        if let Some(expressions) = expressions {
            statements.get_or_insert_default_().push(
                self.factory
                    .create_expression_statement(self.factory.inline_expressions(&expressions))
                    .set_text_range(Some(node)),
            );
        }

        if is_marked_declaration {
            let id = get_original_node_id(node);
            self.append_exports_of_variable_statement(
                self.deferred_exports_mut().entry(id).or_default(),
                node,
                is_exported_declaration,
            );
        } else {
            self.append_exports_of_variable_statement(&mut statements, node, false);
        }

        statements.map(single_or_many_node)
    }

    pub(super) fn hoist_binding_element(
        &self,
        node: &Node, /*VariableDeclaration | BindingElement*/
    ) {
        let node_as_named_declaration = node.as_named_declaration();
        let node_name = node_as_named_declaration.name();
        if is_binding_pattern(Some(&*node_name)) {
            for element in &node_name.as_has_elements().elements() {
                if !is_omitted_expression(element) {
                    self.hoist_binding_element(element);
                }
            }
        } else {
            self.context
                .hoist_variable_declaration(&self.factory.clone_node(&node_name));
        }
    }

    pub(super) fn should_hoist_variable_declaration_list(
        &self,
        node: &Node, /*VariableDeclarationList*/
    ) -> bool {
        !get_emit_flags(node).intersects(EmitFlags::NoHoisting)
            && (self.enclosing_block_scoped_container().kind() == SyntaxKind::SourceFile
                || !get_original_node(node)
                    .flags()
                    .intersects(NodeFlags::BlockScoped))
    }

    pub(super) fn transform_initialized_variable(
        &self,
        node: &Node, /*VariableDeclaration*/
        is_exported_declaration: bool,
    ) -> Gc<Node /*Expression*/> {
        let node_as_variable_declaration = node.as_variable_declaration();
        let create_assignment =
            |name: &Node, value: &Node, location: Option<&dyn ReadonlyTextRange>| {
                if is_exported_declaration {
                    self.create_exported_variable_assignment(name, value, location)
                } else {
                    self.create_non_exported_variable_assignment(name, value, location)
                }
            };
        if is_binding_pattern(node_as_variable_declaration.maybe_name()) {
            flatten_destructuring_assignment(
                node,
                Some(|node: &Node| self.visitor(node)),
                &**self.context,
                FlattenLevel::All,
                Some(false),
                Some(create_assignment),
            )
        } else if let Some(node_initializer) = node_as_variable_declaration.maybe_initializer() {
            create_assignment(
                &node_as_variable_declaration.name(),
                &visit_node(
                    &node_initializer,
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ),
                None,
            )
        } else {
            node_as_variable_declaration.name()
        }
    }

    pub(super) fn create_exported_variable_assignment(
        &self,
        name: &Node,  /*Identifier*/
        value: &Node, /*Expression*/
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    ) -> Gc<Node> {
        self.create_variable_assignment(name, value, location, true)
    }

    pub(super) fn create_non_exported_variable_assignment(
        &self,
        name: &Node,  /*Identifier*/
        value: &Node, /*Expression*/
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    ) -> Gc<Node> {
        self.create_variable_assignment(name, value, location, false)
    }

    pub(super) fn create_variable_assignment(
        &self,
        name: &Node,  /*Identifier*/
        value: &Node, /*Expression*/
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
        is_exported_declaration: bool,
    ) -> Gc<Node> {
        self.context
            .hoist_variable_declaration(&self.factory.clone_node(name));
        if is_exported_declaration {
            self.create_export_expression(
                name,
                &self.prevent_substitution(
                    self.factory
                        .create_assignment(name.node_wrapper(), value.node_wrapper())
                        .set_text_range(location),
                ),
            )
        } else {
            self.prevent_substitution(
                self.factory
                    .create_assignment(name.node_wrapper(), value.node_wrapper())
                    .set_text_range(location),
            )
        }
    }

    pub(super) fn has_associated_end_of_declaration_marker(&self, _node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn append_exports_of_import_declaration(
        &self,
        _statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        _decl: &Node, /*ImportDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn append_exports_of_import_equals_declaration(
        &self,
        _statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        _decl: &Node, /*ImportEqualsDeclaration*/
    ) /*: Statement[] | undefined*/
    {
        unimplemented!()
    }

    pub(super) fn append_exports_of_variable_statement(
        &self,
        _statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        _node: &Node, /*VariableStatement*/
        _export_self: bool,
    ) /*: Statement[] | undefined*/
    {
        unimplemented!()
    }
}
