use std::io;

use id_arena::Id;

use super::TransformModule;
use crate::{
    has_syntactic_modifier, is_external_module_import_equals_declaration, Debug_, ModifierFlags,
    ModuleKind, NamedDeclarationInterface, Node, NodeArray, NodeExt, NodeFlags, NodeInterface,
    OptionTry, ReadonlyTextRange, ScriptTarget, VisitResult, _d, get_emit_flags,
    get_es_module_interop, get_original_node_id, get_text_of_identifier_or_literal, id_text,
    is_arrow_function, is_binding_pattern, is_class_expression, is_export_name,
    is_export_namespace_as_default_declaration, is_function_expression, is_identifier,
    is_local_name, is_modifier, is_named_exports, maybe_visit_nodes, released, remove_all_comments,
    return_if_none, set_emit_flags, single_or_many_node, try_flatten_destructuring_assignment,
    try_maybe_visit_each_child, try_maybe_visit_nodes, try_visit_each_child, try_visit_node,
    try_visit_nodes, ClassLikeDeclarationInterface, EmitFlags, FlattenLevel,
    FunctionLikeDeclarationInterface, GetOrInsertDefault, HasInitializerInterface,
    HasTypeInterface, InArena, InterfaceOrClassLikeDeclarationInterface, Matches, OptionInArena,
    SignatureDeclarationInterface, SyntaxKind,
};

impl TransformModule {
    pub(super) fn visit_import_equals_declaration(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        Debug_.assert(
            is_external_module_import_equals_declaration(node, self),
            Some("import= for internal module references should be handled in an earlier transformer.")
        );

        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();
        if self.module_kind != ModuleKind::AMD {
            if has_syntactic_modifier(node, ModifierFlags::Export, self) {
                statements.get_or_insert_default_().push(
                    self.factory
                        .ref_(self)
                        .create_expression_statement(self.create_export_expression(
                            node.ref_(self).as_import_equals_declaration().name(),
                            self.create_require_call(node)?,
                            Option::<&Node>::None,
                            None,
                        ))
                        .set_text_range(Some(&*node.ref_(self)), self)
                        .set_original_node(Some(node), self),
                );
            } else {
                statements.get_or_insert_default_().push(
                    self.factory
                        .ref_(self)
                        .create_variable_statement(
                            Option::<Id<NodeArray>>::None,
                            self.factory.ref_(self).create_variable_declaration_list(
                                vec![self.factory.ref_(self).create_variable_declaration(
                                    Some(self.factory.ref_(self).clone_node(released!(
                                        node.ref_(self).as_import_equals_declaration().name()
                                    ))),
                                    None,
                                    None,
                                    Some(self.create_require_call(node)?),
                                )],
                                Some(
                                    (self.language_version >= ScriptTarget::ES2015)
                                        .then_some(NodeFlags::Const)
                                        .unwrap_or_default(),
                                ),
                            ),
                        )
                        .set_text_range(Some(&*node.ref_(self)), self)
                        .set_original_node(Some(node), self),
                );
            }
        } else {
            if has_syntactic_modifier(node, ModifierFlags::Export, self) {
                statements.get_or_insert_default_().push(
                    self.factory
                        .ref_(self)
                        .create_expression_statement(self.create_export_expression(
                            self.factory.ref_(self).get_export_name(node, None, None),
                            self.factory.ref_(self).get_local_name(node, None, None),
                            Option::<&Node>::None,
                            None,
                        ))
                        .set_text_range(Some(&*node.ref_(self)), self)
                        .set_original_node(Some(node), self),
                );
            }
        }

        if self.has_associated_end_of_declaration_marker(node) {
            let id = get_original_node_id(node, self);
            self.append_exports_of_import_equals_declaration(
                self.deferred_exports_mut().entry(id).or_default(),
                node,
            );
        } else {
            self.append_exports_of_import_equals_declaration(&mut statements, node);
        }

        Ok(statements.map(single_or_many_node))
    }

    pub(super) fn visit_export_declaration(
        &self,
        node: Id<Node>, /*ExportDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_export_declaration = node_ref.as_export_declaration();
        if node_as_export_declaration.module_specifier.is_none() {
            return Ok(None);
        }

        let generated_name = self
            .factory
            .ref_(self)
            .get_generated_name_for_node(Some(node), None);

        Ok(
            if let Some(node_export_clause) = node_as_export_declaration
                .export_clause
                .filter(|node_export_clause| is_named_exports(&node_export_clause.ref_(self)))
            {
                let mut statements: Vec<Id<Node /*Statement*/>> = _d();
                if self.module_kind != ModuleKind::AMD {
                    statements.push(
                        self.factory
                            .ref_(self)
                            .create_variable_statement(
                                Option::<Id<NodeArray>>::None,
                                self.factory.ref_(self).create_variable_declaration_list(
                                    vec![self.factory.ref_(self).create_variable_declaration(
                                        Some(generated_name.clone()),
                                        None,
                                        None,
                                        Some(self.create_require_call(node)?),
                                    )],
                                    None,
                                ),
                            )
                            .set_text_range(Some(&*node.ref_(self)), self)
                            .set_original_node(Some(node), self),
                    );
                }
                for &specifier in &*node_export_clause
                    .ref_(self)
                    .as_named_exports()
                    .elements
                    .ref_(self)
                {
                    let specifier_ref = specifier.ref_(self);
                    let specifier_as_export_specifier = specifier_ref.as_export_specifier();
                    if self.language_version == ScriptTarget::ES3 {
                        statements.push(
                            self.factory
                                .ref_(self)
                                .create_expression_statement(
                                    self.emit_helpers().create_create_binding_helper(
                                        generated_name.clone(),
                                        self.factory.ref_(self).create_string_literal_from_node(
                                            specifier_as_export_specifier
                                                .property_name
                                                .unwrap_or(specifier_as_export_specifier.name),
                                        ),
                                        specifier_as_export_specifier.property_name.map(|_| {
                                            self.factory.ref_(self).create_string_literal_from_node(
                                                specifier_as_export_specifier.name,
                                            )
                                        }),
                                    ),
                                )
                                .set_text_range(Some(&*specifier.ref_(self)), self)
                                .set_original_node(Some(specifier), self),
                        );
                    } else {
                        let export_needs_import_default =
                            get_es_module_interop(&self.compiler_options.ref_(self)) == Some(true)
                                && !get_emit_flags(node, self)
                                    .intersects(EmitFlags::NeverApplyImportHelper)
                                && id_text(
                                    &specifier_as_export_specifier
                                        .property_name
                                        .unwrap_or(specifier_as_export_specifier.name)
                                        .ref_(self),
                                ) == "default";
                        let exported_value =
                            self.factory.ref_(self).create_property_access_expression(
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
                            );
                        statements.push(
                            self.factory
                                .ref_(self)
                                .create_expression_statement(
                                    self.create_export_expression(
                                        self.factory
                                            .ref_(self)
                                            .get_export_name(specifier, None, None),
                                        exported_value,
                                        Option::<&Node>::None,
                                        Some(true),
                                    ),
                                )
                                .set_text_range(Some(&*specifier.ref_(self)), self)
                                .set_original_node(Some(specifier), self),
                        );
                    }
                }

                Some(single_or_many_node(statements))
            } else if let Some(node_export_clause) = node_as_export_declaration.export_clause {
                let node_export_clause_ref = node_export_clause.ref_(self);
                let node_export_clause_as_namespace_export =
                    node_export_clause_ref.as_namespace_export();
                let mut statements: Vec<Id<Node /*Statement*/>> = _d();
                statements.push(
                    self.factory
                        .ref_(self)
                        .create_expression_statement(
                            self.create_export_expression(
                                self.factory
                                    .ref_(self)
                                    .clone_node(node_export_clause_as_namespace_export.name),
                                self.get_helper_expression_for_export(
                                    node,
                                    if self.module_kind != ModuleKind::AMD {
                                        self.create_require_call(node)?
                                    } else if is_export_namespace_as_default_declaration(node, self)
                                    {
                                        generated_name
                                    } else {
                                        self.factory.ref_(self).create_identifier(id_text(
                                            &node_export_clause_as_namespace_export.name.ref_(self),
                                        ))
                                    },
                                ),
                                Option::<&Node>::None,
                                None,
                            ),
                        )
                        .set_text_range(Some(&*node.ref_(self)), self)
                        .set_original_node(Some(node), self),
                );

                Some(single_or_many_node(statements))
            } else {
                Some(
                    self.factory
                        .ref_(self)
                        .create_expression_statement(self.emit_helpers().create_export_star_helper(
                            if self.module_kind != ModuleKind::AMD {
                                self.create_require_call(node)?
                            } else {
                                generated_name
                            },
                            None,
                        ))
                        .set_text_range(Some(&*node.ref_(self)), self)
                        .set_original_node(Some(node), self)
                        .into(),
                )
            },
        )
    }

    pub(super) fn visit_export_assignment(
        &self,
        node: Id<Node>, /*ExportAssignment*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_export_assignment = node_ref.as_export_assignment();
        if node_as_export_assignment.is_export_equals == Some(true) {
            return Ok(None);
        }

        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();
        let original = node.ref_(self).maybe_original();
        if original.matches(|original| self.has_associated_end_of_declaration_marker(original)) {
            let id = get_original_node_id(node, self);
            self.append_export_statement(
                self.deferred_exports_mut().entry(id).or_default(),
                self.factory.ref_(self).create_identifier("default"),
                try_visit_node(
                    node_as_export_assignment.expression,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Option::<fn(Id<Node>) -> bool>::None,
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?,
                Some(&*node.ref_(self)),
                Some(true),
                None,
            );
        } else {
            self.append_export_statement(
                &mut statements,
                self.factory.ref_(self).create_identifier("default"),
                try_visit_node(
                    node_as_export_assignment.expression,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Option::<fn(Id<Node>) -> bool>::None,
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?,
                Some(&*node.ref_(self)),
                Some(true),
                None,
            );
        }

        Ok(statements.map(single_or_many_node))
    }

    pub(super) fn visit_function_declaration(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_function_declaration = node_ref.as_function_declaration();
        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();
        if has_syntactic_modifier(node, ModifierFlags::Export, self) {
            statements.get_or_insert_default_().push(
                self.factory
                    .ref_(self)
                    .create_function_declaration(
                        Option::<Id<NodeArray>>::None,
                        maybe_visit_nodes(
                            node.ref_(self).maybe_modifiers(),
                            Some(|node: Id<Node>| self.modifier_visitor(node)),
                            Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                            None,
                            None,
                            self,
                        ),
                        node_as_function_declaration.maybe_asterisk_token(),
                        Some(self.factory.ref_(self).get_declaration_name(
                            Some(node),
                            Some(true),
                            Some(true),
                        )),
                        Option::<Id<NodeArray>>::None,
                        try_visit_nodes(
                            node_as_function_declaration.parameters(),
                            Some(|node: Id<Node>| self.visitor(node)),
                            Option::<fn(Id<Node>) -> bool>::None,
                            None,
                            None,
                            self,
                        )?,
                        None,
                        try_maybe_visit_each_child(
                            node_as_function_declaration.maybe_body(),
                            |node: Id<Node>| self.visitor(node),
                            &*self.context.ref_(self),
                            self,
                        )?,
                    )
                    .set_text_range(Some(&*node.ref_(self)), self)
                    .set_original_node(Some(node), self),
            );
        } else {
            statements
                .get_or_insert_default_()
                .push(try_visit_each_child(
                    node,
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )?);
        }

        if self.has_associated_end_of_declaration_marker(node) {
            let id = get_original_node_id(node, self);
            self.append_exports_of_hoisted_declaration(
                self.deferred_exports_mut().entry(id).or_default(),
                node,
            );
        } else {
            self.append_exports_of_hoisted_declaration(&mut statements, node);
        }

        Ok(statements.map(single_or_many_node))
    }

    pub(super) fn visit_class_declaration(
        &self,
        node: Id<Node>, /*ClassDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_class_declaration = node_ref.as_class_declaration();
        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();
        if has_syntactic_modifier(node, ModifierFlags::Export, self) {
            statements.get_or_insert_default_().push(
                self.factory
                    .ref_(self)
                    .create_class_declaration(
                        Option::<Id<NodeArray>>::None,
                        maybe_visit_nodes(
                            node.ref_(self).maybe_modifiers(),
                            Some(|node: Id<Node>| self.modifier_visitor(node)),
                            Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                            None,
                            None,
                            self,
                        ),
                        Some(self.factory.ref_(self).get_declaration_name(
                            Some(node),
                            Some(true),
                            Some(true),
                        )),
                        Option::<Id<NodeArray>>::None,
                        try_maybe_visit_nodes(
                            node_as_class_declaration.maybe_heritage_clauses(),
                            Some(|node: Id<Node>| self.visitor(node)),
                            Option::<fn(Id<Node>) -> bool>::None,
                            None,
                            None,
                            self,
                        )?,
                        try_visit_nodes(
                            node_as_class_declaration.members(),
                            Some(|node: Id<Node>| self.visitor(node)),
                            Option::<fn(Id<Node>) -> bool>::None,
                            None,
                            None,
                            self,
                        )?,
                    )
                    .set_text_range(Some(&*node.ref_(self)), self)
                    .set_original_node(Some(node), self),
            );
        } else {
            statements
                .get_or_insert_default_()
                .push(try_visit_each_child(
                    node,
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )?);
        }

        if self.has_associated_end_of_declaration_marker(node) {
            let id = get_original_node_id(node, self);
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
        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();
        let mut variables: Option<Vec<Id<Node /*VariableDeclaration*/>>> = _d();
        let mut expressions: Option<Vec<Id<Node /*Expression*/>>> = _d();

        if has_syntactic_modifier(node, ModifierFlags::Export, self) {
            let mut modifiers: Option<Id<NodeArray /*Modifier*/>> = _d();
            let mut remove_comments_on_expressions = false;

            for &variable in &*node
                .ref_(self)
                .as_variable_statement()
                .declaration_list
                .ref_(self)
                .as_variable_declaration_list()
                .declarations
                .ref_(self)
            {
                let variable_ref = variable.ref_(self);
                let variable_as_variable_declaration = variable_ref.as_variable_declaration();
                if is_identifier(&variable_as_variable_declaration.name().ref_(self))
                    && is_local_name(variable_as_variable_declaration.name(), self)
                {
                    if modifiers.is_none() {
                        modifiers = maybe_visit_nodes(
                            node.ref_(self).maybe_modifiers(),
                            Some(|node: Id<Node>| self.modifier_visitor(node)),
                            Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                            None,
                            None,
                            self,
                        );
                    }
                    variables.get_or_insert_default_().push(variable.clone());
                } else if let Some(variable_initializer) =
                    variable_as_variable_declaration.maybe_initializer()
                {
                    if !is_binding_pattern(
                        variable_as_variable_declaration
                            .maybe_name()
                            .refed(self)
                            .as_deref(),
                    ) && (is_arrow_function(&variable_initializer.ref_(self))
                        || is_function_expression(&variable_initializer.ref_(self))
                        || is_class_expression(&variable_initializer.ref_(self)))
                    {
                        let expression = self.factory.ref_(self).create_assignment(
                            self.factory
                                .ref_(self)
                                .create_property_access_expression(
                                    self.factory.ref_(self).create_identifier("exports"),
                                    variable_as_variable_declaration.name(),
                                )
                                .set_text_range(
                                    Some(&*variable_as_variable_declaration.name().ref_(self)),
                                    self,
                                ),
                            self.factory.ref_(self).create_identifier(
                                &get_text_of_identifier_or_literal(
                                    &variable_as_variable_declaration.name().ref_(self),
                                ),
                            ),
                        );
                        let updated_variable = self.factory.ref_(self).create_variable_declaration(
                            Some(variable_as_variable_declaration.name()),
                            variable_as_variable_declaration.exclamation_token.clone(),
                            variable_as_variable_declaration.maybe_type(),
                            Some(try_visit_node(
                                variable_initializer,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Option::<fn(Id<Node>) -> bool>::None,
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            )?),
                        );

                        variables.get_or_insert_default_().push(updated_variable);
                        expressions.get_or_insert_default_().push(expression);
                        remove_comments_on_expressions = true;
                    } else {
                        expressions
                            .get_or_insert_default_()
                            .push(self.transform_initialized_variable(variable)?);
                    }
                }
            }

            if let Some(variables) = variables {
                statements.get_or_insert_default_().push(
                    self.factory.ref_(self).update_variable_statement(
                        node,
                        modifiers,
                        self.factory.ref_(self).update_variable_declaration_list(
                            node.ref_(self).as_variable_statement().declaration_list,
                            variables,
                        ),
                    ),
                );
            }

            if let Some(expressions) = expressions {
                let statement = self
                    .factory
                    .ref_(self)
                    .create_expression_statement(
                        self.factory.ref_(self).inline_expressions(&expressions),
                    )
                    .set_text_range(Some(&*node.ref_(self)), self)
                    .set_original_node(Some(node), self);
                if remove_comments_on_expressions {
                    remove_all_comments(statement, self);
                }
                statements.get_or_insert_default_().push(statement);
            }
        } else {
            statements
                .get_or_insert_default_()
                .push(try_visit_each_child(
                    node,
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )?);
        }

        if self.has_associated_end_of_declaration_marker(node) {
            let id = get_original_node_id(node, self);
            self.append_exports_of_variable_statement(
                self.deferred_exports_mut().entry(id).or_default(),
                node,
            );
        } else {
            self.append_exports_of_variable_statement(&mut statements, node);
        }

        Ok(statements.map(single_or_many_node))
    }

    pub(super) fn create_all_export_expressions(
        &self,
        name: Id<Node>, /*Identifier*/
        value: Id<Node /*Expression*/>,
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    ) -> io::Result<Id<Node>> {
        let exported_names = self.get_exports(name)?;
        if let Some(exported_names) = exported_names {
            let mut expression/*: Expression*/ = if is_export_name(name, self) {
                value
            } else {
                self.factory.ref_(self).create_assignment(
                    name,
                    value,
                )
            };
            for export_name in exported_names {
                set_emit_flags(expression, EmitFlags::NoSubstitution, self);
                expression = self.create_export_expression(export_name, expression, location, None);
            }

            return Ok(expression);
        }
        Ok(self.factory.ref_(self).create_assignment(name, value))
    }

    pub(super) fn transform_initialized_variable(
        &self,
        node: Id<Node>, /*InitializedVariableDeclaration*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_ref = node.ref_(self);
        let node_as_variable_declaration = node_ref.as_variable_declaration();
        Ok(
            if is_binding_pattern(
                node_as_variable_declaration
                    .maybe_name()
                    .refed(self)
                    .as_deref(),
            ) {
                try_flatten_destructuring_assignment(
                    try_visit_node(
                        node,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Option::<fn(Id<Node>) -> bool>::None,
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    Option::<fn(Id<Node>) -> io::Result<VisitResult>>::None,
                    self.context.clone(),
                    FlattenLevel::All,
                    Some(false),
                    Some(
                        |name: Id<Node>,
                         value: Id<Node>,
                         location: Option<&dyn ReadonlyTextRange>| {
                            self.create_all_export_expressions(name, value, location)
                        },
                    ),
                    self,
                )?
            } else {
                self.factory.ref_(self).create_assignment(
                    self.factory
                        .ref_(self)
                        .create_property_access_expression(
                            self.factory.ref_(self).create_identifier("exports"),
                            node_as_variable_declaration.name(),
                        )
                        .set_text_range(
                            Some(&*node_as_variable_declaration.name().ref_(self)),
                            self,
                        ),
                    node_as_variable_declaration
                        .maybe_initializer()
                        .try_map_or_else(
                            || Ok(self.factory.ref_(self).create_void_zero()),
                            |node_initializer| {
                                try_visit_node(
                                    node_initializer,
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Option::<fn(Id<Node>) -> bool>::None,
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                )
                            },
                        )?,
                )
            },
        )
    }

    pub(super) fn visit_merge_declaration_marker(
        &self,
        node: Id<Node>, /*MergeDeclarationMarker*/
    ) -> VisitResult /*<Statement>*/ {
        if self.has_associated_end_of_declaration_marker(node)
            && node.ref_(self).maybe_original().unwrap().ref_(self).kind()
                == SyntaxKind::VariableStatement
        {
            let id = get_original_node_id(node, self);
            self.append_exports_of_variable_statement(
                self.deferred_exports_mut().entry(id).or_default(),
                node.ref_(self).maybe_original().unwrap(),
            );
        }

        Some(node.into())
    }

    pub(super) fn has_associated_end_of_declaration_marker(&self, node: Id<Node>) -> bool {
        get_emit_flags(node, self).intersects(EmitFlags::HasEndOfDeclarationMarker)
    }

    pub(super) fn visit_end_of_declaration_marker(
        &self,
        node: Id<Node>, /*EndOfDeclarationMarker*/
    ) -> VisitResult /*<Statement>*/ {
        let id = get_original_node_id(node, self);
        let statements = self.deferred_exports().get(&id).cloned().flatten();
        if let Some(mut statements) = statements {
            self.deferred_exports_mut().remove(&id);
            statements.push(node);
            return Some(statements.into());
        }

        Some(node.into())
    }

    pub(super) fn append_exports_of_import_declaration(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        decl: Id<Node>, /*ImportDeclaration*/
    ) /*: Statement[] | undefined */
    {
        if self
            .current_module_info()
            .ref_(self)
            .export_equals
            .is_some()
        {
            return /*statements*/;
        }

        let import_clause = return_if_none!(decl.ref_(self).as_import_declaration().import_clause);
        if import_clause.ref_(self).as_import_clause().name.is_some() {
            self.append_exports_of_declaration(statements, import_clause, None);
        }

        let named_bindings = import_clause.ref_(self).as_import_clause().named_bindings;
        if let Some(named_bindings) = named_bindings {
            match released!(named_bindings.ref_(self).kind()) {
                SyntaxKind::NamespaceImport => {
                    self.append_exports_of_declaration(statements, named_bindings, None);
                }
                SyntaxKind::NamedImports => {
                    for &import_binding in
                        &*released!(named_bindings.ref_(self).as_named_imports().elements)
                            .ref_(self)
                    {
                        self.append_exports_of_declaration(statements, import_binding, Some(true));
                    }
                }
                _ => (),
            }
        }

        // return statements;
    }
}
