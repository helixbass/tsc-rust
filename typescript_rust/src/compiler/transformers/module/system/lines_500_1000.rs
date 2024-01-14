use std::io;

use gc::Gc;
use id_arena::Id;

use super::TransformSystemModule;
use crate::{
    Node, NodeInterface, SyntaxKind, VisitResult, _d, get_emit_flags,
    get_local_name_for_external_import, get_original_node, get_original_node_id,
    has_syntactic_modifier, is_binding_pattern, is_block, is_class_element, is_decorator,
    is_expression, is_external_module_import_equals_declaration, is_heritage_clause, is_modifier,
    is_module_or_enum_declaration, is_omitted_expression, is_parameter_declaration, is_statement,
    maybe_visit_nodes, return_if_none, single_or_many_node, try_flatten_destructuring_assignment,
    try_maybe_visit_node, try_maybe_visit_nodes, try_visit_each_child, try_visit_node,
    try_visit_nodes, ClassLikeDeclarationInterface, Debug_, EmitFlags, FlattenLevel,
    FunctionLikeDeclarationInterface, GetOrInsertDefault, HasInitializerInterface,
    InterfaceOrClassLikeDeclarationInterface, ModifierFlags, NamedDeclarationInterface, NodeArray,
    NodeExt, NodeFlags, ReadonlyTextRange, SignatureDeclarationInterface,
};

impl TransformSystemModule {
    pub(super) fn top_level_visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/ {
        Ok(match node.kind() {
            SyntaxKind::ImportDeclaration => self.visit_import_declaration(node),
            SyntaxKind::ImportEqualsDeclaration => self.visit_import_equals_declaration(node),
            SyntaxKind::ExportDeclaration => self.visit_export_declaration(node),
            SyntaxKind::ExportAssignment => self.visit_export_assignment(node)?,
            _ => self.top_level_nested_visitor(node)?,
        })
    }

    pub(super) fn visit_import_declaration(
        &self,
        node: Id<Node>, /*ImportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_import_declaration = node.as_import_declaration();
        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();
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
        _node: Id<Node>, /*ExportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        // Debug.assertIsDefined(node);
        None
    }

    pub(super) fn visit_import_equals_declaration(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        Debug_.assert(
            is_external_module_import_equals_declaration(node),
            Some("import= for internal module references should be handled in an earlier transformer.")
        );

        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();
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
        node: Id<Node>, /*ExportAssignment*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_as_export_assignment = node.as_export_assignment();
        if node_as_export_assignment.is_export_equals == Some(true) {
            return Ok(None);
        }

        let expression = try_visit_node(
            &node_as_export_assignment.expression,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        let original = node.maybe_original();
        Ok(match original {
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
        })
    }

    pub(super) fn visit_function_declaration(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_as_function_declaration = node.as_function_declaration();
        if has_syntactic_modifier(node, ModifierFlags::Export, self) {
            self.maybe_hoisted_statements_mut()
                .get_or_insert_default_()
                .push(
                    self.factory.update_function_declaration(
                        node,
                        node.maybe_decorators(),
                        maybe_visit_nodes(
                            node.maybe_modifiers().as_deref(),
                            Some(|node: Id<Node>| self.modifier_visitor(node)),
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
                        try_visit_nodes(
                            &node_as_function_declaration.parameters(),
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(is_parameter_declaration),
                            None,
                            None,
                        )?,
                        None,
                        try_maybe_visit_node(
                            node_as_function_declaration.maybe_body(),
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(is_block),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )?,
                    ),
                );
        } else {
            self.maybe_hoisted_statements_mut()
                .get_or_insert_default_()
                .push(try_visit_each_child(
                    node,
                    |node: Id<Node>| self.visitor(node),
                    &**self.context,
                )?);
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

        Ok(None)
    }

    pub(super) fn visit_class_declaration(
        &self,
        node: Id<Node>, /*ClassDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_as_class_declaration = node.as_class_declaration();
        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();

        let name = self.factory.get_local_name(node, None, None);
        self.context.hoist_variable_declaration(&name);

        statements.get_or_insert_default_().push(
            self.factory
                .create_expression_statement(
                    self.factory.create_assignment(
                        name,
                        self.factory
                            .create_class_expression(
                                try_maybe_visit_nodes(
                                    node.maybe_decorators().as_deref(),
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Some(is_decorator),
                                    None,
                                    None,
                                )?,
                                Option::<Gc<NodeArray>>::None,
                                node_as_class_declaration.maybe_name(),
                                Option::<Gc<NodeArray>>::None,
                                try_maybe_visit_nodes(
                                    node_as_class_declaration
                                        .maybe_heritage_clauses()
                                        .as_deref(),
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Some(is_heritage_clause),
                                    None,
                                    None,
                                )?,
                                try_visit_nodes(
                                    &node_as_class_declaration.members(),
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Some(is_class_element),
                                    None,
                                    None,
                                )?,
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

        Ok(statements.map(single_or_many_node))
    }

    pub(super) fn visit_variable_statement(
        &self,
        node: Id<Node>, /*VariableStatement*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_as_variable_statement = node.as_variable_statement();
        if !self
            .should_hoist_variable_declaration_list(&node_as_variable_statement.declaration_list)
        {
            return Ok(Some(
                try_visit_node(
                    node,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_statement),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?
                .into(),
            ));
        }

        let mut expressions: Option<Vec<Id<Node /*Expression*/>>> = _d();
        let is_exported_declaration = has_syntactic_modifier(node, ModifierFlags::Export, self);
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
                    )?);
            } else {
                self.hoist_binding_element(variable);
            }
        }

        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();
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

        Ok(statements.map(single_or_many_node))
    }

    pub(super) fn hoist_binding_element(
        &self,
        node: Id<Node>, /*VariableDeclaration | BindingElement*/
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
        node: Id<Node>, /*VariableDeclarationList*/
    ) -> bool {
        !get_emit_flags(node).intersects(EmitFlags::NoHoisting)
            && (self.enclosing_block_scoped_container().kind() == SyntaxKind::SourceFile
                || !get_original_node(node)
                    .flags()
                    .intersects(NodeFlags::BlockScoped))
    }

    pub(super) fn transform_initialized_variable(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
        is_exported_declaration: bool,
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_as_variable_declaration = node.as_variable_declaration();
        let create_assignment =
            |name: Id<Node>, value: Id<Node>, location: Option<&dyn ReadonlyTextRange>| {
                Ok(if is_exported_declaration {
                    self.create_exported_variable_assignment(name, value, location)
                } else {
                    self.create_non_exported_variable_assignment(name, value, location)
                })
            };
        Ok(
            if is_binding_pattern(node_as_variable_declaration.maybe_name()) {
                try_flatten_destructuring_assignment(
                    node,
                    Some(|node: Id<Node>| self.visitor(node)),
                    self.context.clone(),
                    FlattenLevel::All,
                    Some(false),
                    Some(create_assignment),
                )?
            } else if let Some(node_initializer) = node_as_variable_declaration.maybe_initializer()
            {
                create_assignment(
                    &node_as_variable_declaration.name(),
                    &*try_visit_node(
                        &node_initializer,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    None,
                )?
            } else {
                node_as_variable_declaration.name()
            },
        )
    }

    pub(super) fn create_exported_variable_assignment(
        &self,
        name: Id<Node>,  /*Identifier*/
        value: Id<Node>, /*Expression*/
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    ) -> Id<Node> {
        self.create_variable_assignment(name, value, location, true)
    }

    pub(super) fn create_non_exported_variable_assignment(
        &self,
        name: Id<Node>,  /*Identifier*/
        value: Id<Node>, /*Expression*/
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    ) -> Id<Node> {
        self.create_variable_assignment(name, value, location, false)
    }

    pub(super) fn create_variable_assignment(
        &self,
        name: Id<Node>,  /*Identifier*/
        value: Id<Node>, /*Expression*/
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
        is_exported_declaration: bool,
    ) -> Id<Node> {
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

    pub(super) fn visit_merge_declaration_marker(
        &self,
        node: Id<Node>, /*MergeDeclarationMarker*/
    ) -> VisitResult /*<Statement>*/ {
        if self.has_associated_end_of_declaration_marker(node)
            && node.maybe_original().unwrap().kind() == SyntaxKind::VariableStatement
        {
            let id = get_original_node_id(node);
            let is_exported_declaration =
                has_syntactic_modifier(node.maybe_original().unwrap(), ModifierFlags::Export, self);
            self.append_exports_of_variable_statement(
                self.deferred_exports_mut().entry(id).or_default(),
                &node.maybe_original().unwrap(),
                is_exported_declaration,
            );
        }

        Some(node.node_wrapper().into())
    }

    pub(super) fn has_associated_end_of_declaration_marker(&self, node: Id<Node>) -> bool {
        get_emit_flags(node).intersects(EmitFlags::HasEndOfDeclarationMarker)
    }

    pub(super) fn visit_end_of_declaration_marker(
        &self,
        node: Id<Node>, /*EndOfDeclarationMarker*/
    ) -> VisitResult /*<Statement>*/ {
        let id = get_original_node_id(node);
        let mut statements = self.deferred_exports().get(&id).cloned().flatten();
        if let Some(mut statements) = statements {
            self.deferred_exports_mut().remove(&id);
            statements.push(node.node_wrapper());
            return Some(statements.into());
        } else {
            let original = &get_original_node(node);
            if is_module_or_enum_declaration(original) {
                self.append_exports_of_declaration(&mut statements, original, None);
                statements
                    .get_or_insert_default_()
                    .push(node.node_wrapper());
                return statements.map(Into::into);
            }
        }

        Some(node.node_wrapper().into())
    }

    pub(super) fn append_exports_of_import_declaration(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        decl: Id<Node>, /*ImportDeclaration*/
    ) {
        let decl_as_import_declaration = decl.as_import_declaration();
        if self.module_info().export_equals.is_some() {
            return /*statements*/;
        }

        let import_clause = return_if_none!(decl_as_import_declaration.import_clause.as_ref());
        let import_clause_as_import_clause = import_clause.as_import_clause();
        if import_clause_as_import_clause.name.is_some() {
            self.append_exports_of_declaration(statements, import_clause, None);
        }

        let named_bindings = import_clause_as_import_clause.named_bindings.as_ref();
        if let Some(named_bindings) = named_bindings {
            match named_bindings.kind() {
                SyntaxKind::NamespaceImport => {
                    self.append_exports_of_declaration(statements, named_bindings, None);
                }
                SyntaxKind::NamedImports => {
                    for import_binding in &named_bindings.as_named_imports().elements {
                        self.append_exports_of_declaration(statements, import_binding, None);
                    }
                }
                _ => (),
            }
        }

        // return statements;
    }

    pub(super) fn append_exports_of_import_equals_declaration(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        decl: Id<Node>, /*ImportEqualsDeclaration*/
    ) /*: Statement[] | undefined*/
    {
        if self.module_info().export_equals.is_some() {
            return /*statements*/;
        }

        self.append_exports_of_declaration(statements, decl, None);
    }

    pub(super) fn append_exports_of_variable_statement(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        node: Id<Node>, /*VariableStatement*/
        export_self: bool,
    ) /*: Statement[] | undefined*/
    {
        let node_as_variable_statement = node.as_variable_statement();
        if self.module_info().export_equals.is_some() {
            return /*statements*/;
        }

        for decl in &node_as_variable_statement
            .declaration_list
            .as_variable_declaration_list()
            .declarations
        {
            if decl.as_variable_declaration().maybe_initializer().is_some() || export_self {
                self.append_exports_of_binding_element(statements, decl, export_self);
            }
        }

        // return statements;
    }
}
