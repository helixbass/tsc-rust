use gc::Gc;

use super::TransformModule;
use crate::{
    has_syntactic_modifier, is_external_module_import_equals_declaration, Debug_, ModifierFlags,
    ModuleKind, NamedDeclarationInterface, Node, NodeArray, NodeExt, NodeFlags, NodeInterface,
    ReadonlyTextRange, ScriptTarget, VisitResult, _d, flatten_destructuring_assignment,
    get_emit_flags, get_es_module_interop, get_original_node_id, get_text_of_identifier_or_literal,
    id_text, is_arrow_function, is_binding_pattern, is_class_expression, is_export_name,
    is_export_namespace_as_default_declaration, is_function_expression, is_identifier,
    is_local_name, is_modifier, is_named_exports, maybe_visit_each_child, maybe_visit_nodes,
    remove_all_comments, return_if_none, set_emit_flags, single_or_many_node, visit_each_child,
    visit_node, visit_nodes, ClassLikeDeclarationInterface, EmitFlags, FlattenLevel,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasTypeInterface,
    InterfaceOrClassLikeDeclarationInterface, Matches, SignatureDeclarationInterface, SyntaxKind,
};

impl TransformModule {
    pub(super) fn visit_import_equals_declaration(
        &self,
        node: &Node, /*ImportEqualsDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_import_equals_declaration = node.as_import_equals_declaration();
        Debug_.assert(
            is_external_module_import_equals_declaration(node),
            Some("import= for internal module references should be handled in an earlier transformer.")
        );

        let mut statements: Option<Vec<Gc<Node /*Statement*/>>> = _d();
        if self.module_kind != ModuleKind::AMD {
            if has_syntactic_modifier(node, ModifierFlags::Export) {
                statements.get_or_insert_with(|| _d()).push(
                    self.factory
                        .create_expression_statement(self.create_export_expression(
                            &node_as_import_equals_declaration.name(),
                            &self.create_require_call(node),
                            Option::<&Node>::None,
                            None,
                        ))
                        .wrap()
                        .set_text_range(Some(node))
                        .set_original_node(Some(node.node_wrapper())),
                );
            } else {
                statements.get_or_insert_with(|| _d()).push(
                    self.factory
                        .create_variable_statement(
                            Option::<Gc<NodeArray>>::None,
                            self.factory
                                .create_variable_declaration_list(
                                    vec![self
                                        .factory
                                        .create_variable_declaration(
                                            Some(self.factory.clone_node(
                                                &node_as_import_equals_declaration.name(),
                                            )),
                                            None,
                                            None,
                                            Some(self.create_require_call(node)),
                                        )
                                        .wrap()],
                                    Some(
                                        (self.language_version >= ScriptTarget::ES2015)
                                            .then_some(NodeFlags::Const)
                                            .unwrap_or_default(),
                                    ),
                                )
                                .wrap(),
                        )
                        .wrap()
                        .set_text_range(Some(node))
                        .set_original_node(Some(node.node_wrapper())),
                );
            }
        } else {
            if has_syntactic_modifier(node, ModifierFlags::Export) {
                statements.get_or_insert_with(|| _d()).push(
                    self.factory
                        .create_expression_statement(self.create_export_expression(
                            &self.factory.get_export_name(node, None, None),
                            &self.factory.get_local_name(node, None, None),
                            Option::<&Node>::None,
                            None,
                        ))
                        .wrap()
                        .set_text_range(Some(node))
                        .set_original_node(Some(node.node_wrapper())),
                );
            }
        }

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

    pub(super) fn visit_export_declaration(
        &self,
        node: &Node, /*ExportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_export_declaration = node.as_export_declaration();
        if node_as_export_declaration.module_specifier.is_none() {
            return None;
        }

        let generated_name = self.factory.get_generated_name_for_node(Some(node), None);

        if let Some(node_export_clause) = node_as_export_declaration
            .export_clause
            .as_ref()
            .filter(|node_export_clause| is_named_exports(node_export_clause))
        {
            let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
            if self.module_kind != ModuleKind::AMD {
                statements.push(
                    self.factory
                        .create_variable_statement(
                            Option::<Gc<NodeArray>>::None,
                            self.factory
                                .create_variable_declaration_list(
                                    vec![self
                                        .factory
                                        .create_variable_declaration(
                                            Some(generated_name.clone()),
                                            None,
                                            None,
                                            Some(self.create_require_call(node)),
                                        )
                                        .wrap()],
                                    None,
                                )
                                .wrap(),
                        )
                        .wrap()
                        .set_text_range(Some(node))
                        .set_original_node(Some(node.node_wrapper())),
                );
            }
            for specifier in &node_export_clause.as_named_exports().elements {
                let specifier_as_export_specifier = specifier.as_export_specifier();
                if self.language_version == ScriptTarget::ES3 {
                    statements.push(
                        self.factory
                            .create_expression_statement(
                                self.emit_helpers().create_create_binding_helper(
                                    generated_name.clone(),
                                    self.factory
                                        .create_string_literal_from_node(
                                            specifier_as_export_specifier
                                                .property_name
                                                .as_deref()
                                                .unwrap_or(&specifier_as_export_specifier.name),
                                        )
                                        .wrap(),
                                    specifier_as_export_specifier.property_name.as_ref().map(
                                        |_| {
                                            self.factory
                                                .create_string_literal_from_node(
                                                    &specifier_as_export_specifier.name,
                                                )
                                                .wrap()
                                        },
                                    ),
                                ),
                            )
                            .wrap()
                            .set_text_range(Some(&**specifier))
                            .set_original_node(Some(specifier.clone())),
                    );
                } else {
                    let export_needs_import_default = get_es_module_interop(&self.compiler_options)
                        == Some(true)
                        && !get_emit_flags(node).intersects(EmitFlags::NeverApplyImportHelper)
                        && id_text(
                            specifier_as_export_specifier
                                .property_name
                                .as_deref()
                                .unwrap_or(&specifier_as_export_specifier.name),
                        ) == "default";
                    let exported_value = self
                        .factory
                        .create_property_access_expression(
                            if export_needs_import_default {
                                self.emit_helpers()
                                    .create_import_default_helper(generated_name.clone())
                            } else {
                                generated_name.clone()
                            },
                            specifier_as_export_specifier
                                .property_name
                                .clone()
                                .unwrap_or_else(|| specifier_as_export_specifier.name.clone()),
                        )
                        .wrap();
                    statements.push(
                        self.factory
                            .create_expression_statement(self.create_export_expression(
                                &self.factory.get_export_name(specifier, None, None),
                                &exported_value,
                                Option::<&Node>::None,
                                Some(true),
                            ))
                            .wrap()
                            .set_text_range(Some(&**specifier))
                            .set_original_node(Some(specifier.clone())),
                    );
                }
            }

            Some(single_or_many_node(statements))
        } else if let Some(node_export_clause) = node_as_export_declaration.export_clause.as_ref() {
            let node_export_clause_as_namespace_export = node_export_clause.as_namespace_export();
            let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
            statements.push(
                self.factory
                    .create_expression_statement(
                        self.create_export_expression(
                            &self
                                .factory
                                .clone_node(&node_export_clause_as_namespace_export.name),
                            &self.get_helper_expression_for_export(
                                node,
                                if self.module_kind != ModuleKind::AMD {
                                    self.create_require_call(node)
                                } else if is_export_namespace_as_default_declaration(node) {
                                    generated_name
                                } else {
                                    self.factory.create_identifier(id_text(
                                        &node_export_clause_as_namespace_export.name,
                                    ))
                                },
                            ),
                            Option::<&Node>::None,
                            None,
                        ),
                    )
                    .wrap()
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper())),
            );

            Some(single_or_many_node(statements))
        } else {
            Some(
                self.factory
                    .create_expression_statement(self.emit_helpers().create_export_star_helper(
                        if self.module_kind != ModuleKind::AMD {
                            self.create_require_call(node)
                        } else {
                            generated_name
                        },
                        None,
                    ))
                    .wrap()
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper()))
                    .into(),
            )
        }
    }

    pub(super) fn visit_export_assignment(
        &self,
        node: &Node, /*ExportAssignment*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_export_assignment = node.as_export_assignment();
        if node_as_export_assignment.is_export_equals == Some(true) {
            return None;
        }

        let mut statements: Option<Vec<Gc<Node /*Statement*/>>> = _d();
        let original = node.maybe_original();
        if original
            .as_ref()
            .matches(|original| self.has_associated_end_of_declaration_marker(original))
        {
            let id = get_original_node_id(node);
            self.append_export_statement(
                self.deferred_exports_mut().entry(id).or_default(),
                &self.factory.create_identifier("default"),
                &visit_node(
                    &node_as_export_assignment.expression,
                    Some(|node: &Node| self.visitor(node)),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ),
                Some(node),
                Some(true),
                None,
            );
        } else {
            self.append_export_statement(
                &mut statements,
                &self.factory.create_identifier("default"),
                &visit_node(
                    &node_as_export_assignment.expression,
                    Some(|node: &Node| self.visitor(node)),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ),
                Some(node),
                Some(true),
                None,
            );
        }

        statements.map(single_or_many_node)
    }

    pub(super) fn visit_function_declaration(
        &self,
        node: &Node, /*FunctionDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_function_declaration = node.as_function_declaration();
        let mut statements: Option<Vec<Gc<Node /*Statement*/>>> = _d();
        if has_syntactic_modifier(node, ModifierFlags::Export) {
            statements.get_or_insert_with(|| _d()).push(
                self.factory
                    .create_function_declaration(
                        Option::<Gc<NodeArray>>::None,
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
                            Option::<fn(&Node) -> bool>::None,
                            None,
                            None,
                        ),
                        None,
                        maybe_visit_each_child(
                            node_as_function_declaration.maybe_body(),
                            |node: &Node| self.visitor(node),
                            &**self.context,
                        ),
                    )
                    .wrap()
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper())),
            );
        } else {
            statements
                .get_or_insert_with(|| _d())
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
            self.append_exports_of_hoisted_declaration(&mut statements, node);
        }

        statements.map(single_or_many_node)
    }

    pub(super) fn visit_class_declaration(
        &self,
        node: &Node, /*ClassDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_class_declaration = node.as_class_declaration();
        let mut statements: Option<Vec<Gc<Node /*Statement*/>>> = _d();
        if has_syntactic_modifier(node, ModifierFlags::Export) {
            statements.get_or_insert_with(|| _d()).push(
                self.factory
                    .create_class_declaration(
                        Option::<Gc<NodeArray>>::None,
                        maybe_visit_nodes(
                            node.maybe_modifiers().as_deref(),
                            Some(|node: &Node| self.modifier_visitor(node)),
                            Some(is_modifier),
                            None,
                            None,
                        ),
                        Some(
                            self.factory
                                .get_declaration_name(Some(node), Some(true), Some(true)),
                        ),
                        Option::<Gc<NodeArray>>::None,
                        maybe_visit_nodes(
                            node_as_class_declaration
                                .maybe_heritage_clauses()
                                .as_deref(),
                            Some(|node: &Node| self.visitor(node)),
                            Option::<fn(&Node) -> bool>::None,
                            None,
                            None,
                        ),
                        visit_nodes(
                            &node_as_class_declaration.members(),
                            Some(|node: &Node| self.visitor(node)),
                            Option::<fn(&Node) -> bool>::None,
                            None,
                            None,
                        ),
                    )
                    .wrap()
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper())),
            );
        } else {
            statements
                .get_or_insert_with(|| _d())
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
            self.append_exports_of_hoisted_declaration(&mut statements, node);
        }

        statements.map(single_or_many_node)
    }

    pub(super) fn visit_variable_statement(
        &self,
        node: &Node, /*VariableStatement*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_variable_statement = node.as_variable_statement();
        let mut statements: Option<Vec<Gc<Node /*Statement*/>>> = _d();
        let mut variables: Option<Vec<Gc<Node /*VariableDeclaration*/>>> = _d();
        let mut expressions: Option<Vec<Gc<Node /*Expression*/>>> = _d();

        if has_syntactic_modifier(node, ModifierFlags::Export) {
            let mut modifiers: Option<Gc<NodeArray /*Modifier*/>> = _d();
            let mut remove_comments_on_expressions = false;

            for variable in &node_as_variable_statement
                .declaration_list
                .as_variable_declaration_list()
                .declarations
            {
                let variable_as_variable_declaration = variable.as_variable_declaration();
                if is_identifier(&variable_as_variable_declaration.name())
                    && is_local_name(&variable_as_variable_declaration.name())
                {
                    if modifiers.is_none() {
                        modifiers = maybe_visit_nodes(
                            node.maybe_modifiers().as_deref(),
                            Some(|node: &Node| self.modifier_visitor(node)),
                            Some(is_modifier),
                            None,
                            None,
                        );
                    }
                    variables.get_or_insert_with(|| _d()).push(variable.clone());
                } else if let Some(ref variable_initializer) =
                    variable_as_variable_declaration.maybe_initializer()
                {
                    if !is_binding_pattern(variable_as_variable_declaration.maybe_name())
                        && (is_arrow_function(variable_initializer)
                            || is_function_expression(variable_initializer)
                            || is_class_expression(variable_initializer))
                    {
                        let expression = self
                            .factory
                            .create_assignment(
                                self.factory
                                    .create_property_access_expression(
                                        self.factory.create_identifier("exports"),
                                        variable_as_variable_declaration.name(),
                                    )
                                    .wrap()
                                    .set_text_range(Some(
                                        &*variable_as_variable_declaration.name(),
                                    )),
                                self.factory
                                    .create_identifier(&get_text_of_identifier_or_literal(
                                        &variable_as_variable_declaration.name(),
                                    )),
                            )
                            .wrap();
                        let updated_variable = self
                            .factory
                            .create_variable_declaration(
                                Some(variable_as_variable_declaration.name()),
                                variable_as_variable_declaration.exclamation_token.clone(),
                                variable_as_variable_declaration.maybe_type(),
                                Some(visit_node(
                                    variable_initializer,
                                    Some(|node: &Node| self.visitor(node)),
                                    Option::<fn(&Node) -> bool>::None,
                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                )),
                            )
                            .wrap();

                        variables.get_or_insert_with(|| _d()).push(updated_variable);
                        expressions.get_or_insert_with(|| _d()).push(expression);
                        remove_comments_on_expressions = true;
                    } else {
                        expressions
                            .get_or_insert_with(|| _d())
                            .push(self.transform_initialized_variable(variable));
                    }
                }
            }

            if let Some(variables) = variables {
                statements.get_or_insert_with(|| _d()).push(
                    self.factory.update_variable_statement(
                        node,
                        modifiers,
                        self.factory.update_variable_declaration_list(
                            &node_as_variable_statement.declaration_list,
                            variables,
                        ),
                    ),
                );
            }

            if let Some(expressions) = expressions {
                let statement = self
                    .factory
                    .create_expression_statement(self.factory.inline_expressions(&expressions))
                    .wrap()
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper()));
                if remove_comments_on_expressions {
                    remove_all_comments(&*statement);
                }
                statements.get_or_insert_with(|| _d()).push(statement);
            }
        } else {
            statements
                .get_or_insert_with(|| _d())
                .push(visit_each_child(
                    node,
                    |node: &Node| self.visitor(node),
                    &**self.context,
                ));
        }

        if self.has_associated_end_of_declaration_marker(node) {
            let id = get_original_node_id(node);
            self.append_exports_of_variable_statement(
                self.deferred_exports_mut().entry(id).or_default(),
                node,
            );
        } else {
            self.append_exports_of_variable_statement(&mut statements, node);
        }

        statements.map(single_or_many_node)
    }

    pub(super) fn create_all_export_expressions(
        &self,
        name: &Node, /*Identifier*/
        value: Gc<Node /*Expression*/>,
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    ) -> Gc<Node> {
        let exported_names = self.get_exports(name);
        if let Some(exported_names) = exported_names {
            let mut expression/*: Expression*/ = if is_export_name(name) {
                value
            } else {
                self.factory.create_assignment(
                    name.node_wrapper(),
                    value,
                ).wrap()
            };
            for export_name in exported_names {
                set_emit_flags(&*expression, EmitFlags::NoSubstitution);
                expression =
                    self.create_export_expression(&export_name, &expression, location, None);
            }

            return expression;
        }
        self.factory
            .create_assignment(name.node_wrapper(), value)
            .wrap()
    }

    pub(super) fn transform_initialized_variable(
        &self,
        node: &Node, /*InitializedVariableDeclaration*/
    ) -> Gc<Node /*Expression*/> {
        let node_as_variable_declaration = node.as_variable_declaration();
        if is_binding_pattern(node_as_variable_declaration.maybe_name()) {
            flatten_destructuring_assignment(
                &visit_node(
                    node,
                    Some(|node: &Node| self.visitor(node)),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ),
                Option::<fn(&Node) -> VisitResult>::None,
                &**self.context,
                FlattenLevel::All,
                Some(false),
                Some(
                    |name: &Node, value: &Node, location: Option<&dyn ReadonlyTextRange>| {
                        self.create_all_export_expressions(name, value.node_wrapper(), location)
                    },
                ),
            )
        } else {
            self.factory
                .create_assignment(
                    self.factory
                        .create_property_access_expression(
                            self.factory.create_identifier("exports"),
                            node_as_variable_declaration.name(),
                        )
                        .wrap()
                        .set_text_range(Some(&*node_as_variable_declaration.name())),
                    node_as_variable_declaration
                        .maybe_initializer()
                        .map_or_else(
                            || self.factory.create_void_zero(),
                            |ref node_initializer| {
                                visit_node(
                                    node_initializer,
                                    Some(|node: &Node| self.visitor(node)),
                                    Option::<fn(&Node) -> bool>::None,
                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                )
                            },
                        ),
                )
                .wrap()
        }
    }

    pub(super) fn visit_merge_declaration_marker(
        &self,
        node: &Node, /*MergeDeclarationMarker*/
    ) -> VisitResult /*<Statement>*/ {
        if self.has_associated_end_of_declaration_marker(node)
            && node.maybe_original().unwrap().kind() == SyntaxKind::VariableStatement
        {
            let id = get_original_node_id(node);
            self.append_exports_of_variable_statement(
                self.deferred_exports_mut().entry(id).or_default(),
                &node.maybe_original().unwrap(),
            );
        }

        Some(node.node_wrapper().into())
    }

    pub(super) fn has_associated_end_of_declaration_marker(&self, node: &Node) -> bool {
        get_emit_flags(node).intersects(EmitFlags::HasEndOfDeclarationMarker)
    }

    pub(super) fn visit_end_of_declaration_marker(
        &self,
        node: &Node, /*EndOfDeclarationMarker*/
    ) -> VisitResult /*<Statement>*/ {
        let id = get_original_node_id(node);
        let statements = self.deferred_exports().get(&id).cloned().flatten();
        if let Some(mut statements) = statements {
            self.deferred_exports_mut().remove(&id);
            statements.push(node.node_wrapper());
            return Some(statements.into());
        }

        Some(node.node_wrapper().into())
    }

    pub(super) fn append_exports_of_import_declaration(
        &self,
        statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        decl: &Node, /*ImportDeclaration*/
    ) /*: Statement[] | undefined */
    {
        let decl_as_import_declaration = decl.as_import_declaration();
        if self.current_module_info().export_equals.is_some() {
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
                        self.append_exports_of_declaration(statements, import_binding, Some(true));
                    }
                }
                _ => (),
            }
        }

        // return statements;
    }
}
