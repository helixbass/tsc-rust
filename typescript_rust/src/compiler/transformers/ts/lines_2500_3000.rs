use std::{io, ptr};

use gc::Gc;

use super::TransformTypeScript;
use crate::{
    is_named_import_bindings, is_statement, Matches, ModuleKind, Node, NodeInterface, VisitResult,
    __String, add_emit_flags, add_range, are_option_gcs_equal, create_expression_from_entity_name,
    get_emit_flags, has_syntactic_modifier, insert_statements_after_standard_prologue,
    is_export_specifier, is_external_module, is_external_module_import_equals_declaration,
    is_identifier, is_import_clause, is_import_specifier, is_modifier, is_named_export_bindings,
    is_namespace_export, maybe_visit_nodes, move_range_pos, set_comment_range, set_emit_flags,
    set_source_map_range, set_synthetic_leading_comments, set_synthetic_trailing_comments,
    set_text_range, set_text_range_node_array, try_maybe_visit_each_child, try_maybe_visit_node,
    try_visit_each_child, try_visit_node, try_visit_nodes, BoolExt, Debug_, EmitFlags,
    ImportsNotUsedAsValues, ModifierFlags, NamedDeclarationInterface, NodeArray, NodeExt,
    NodeFlags, ReadonlyTextRangeConcrete, SingleNodeOrVecNode, SyntaxKind,
};

impl TransformTypeScript {
    pub(super) fn has_namespace_qualified_export_name(&self, node: &Node) -> bool {
        self.is_export_of_namespace(node)
            || self.is_external_module_export(node)
                && !matches!(
                    self.module_kind,
                    ModuleKind::ES2015
                        | ModuleKind::ES2020
                        | ModuleKind::ES2022
                        | ModuleKind::ESNext
                        | ModuleKind::System
                )
    }

    pub(super) fn record_emitted_declaration_in_scope(
        &self,
        node: &Node, /*FunctionDeclaration | ClassDeclaration | ModuleDeclaration | EnumDeclaration*/
    ) {
        let mut current_scope_first_declarations_of_name =
            self.maybe_current_scope_first_declarations_of_name_mut();
        let current_scope_first_declarations_of_name =
            current_scope_first_declarations_of_name.get_or_insert_with(|| Default::default());

        let name = self.declared_name_in_scope(node);
        if !current_scope_first_declarations_of_name.contains_key(&name) {
            current_scope_first_declarations_of_name.insert(name, node.node_wrapper());
        }
    }

    pub(super) fn is_first_emitted_declaration_in_scope(
        &self,
        node: &Node, /*ModuleDeclaration | EnumDeclaration*/
    ) -> bool {
        if let Some(current_scope_first_declarations_of_name) = self
            .maybe_current_scope_first_declarations_of_name()
            .as_ref()
        {
            let name = self.declared_name_in_scope(node);
            return current_scope_first_declarations_of_name
                .get(&name)
                .matches(|value| ptr::eq(&**value, node));
        }
        true
    }

    pub(super) fn declared_name_in_scope(
        &self,
        node: &Node, /*FunctionDeclaration | ClassDeclaration | ModuleDeclaration | EnumDeclaration*/
    ) -> __String {
        let node_as_named_declaration = node.as_named_declaration();
        Debug_.assert_node(
            node_as_named_declaration.maybe_name(),
            Some(is_identifier),
            None,
        );
        node_as_named_declaration
            .name()
            .as_identifier()
            .escaped_text
            .clone()
    }

    pub(super) fn add_var_for_enum_or_module_declaration(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ModuleDeclaration | EnumDeclaration*/
    ) -> bool {
        let statement = self
            .factory
            .create_variable_statement(
                maybe_visit_nodes(
                    node.maybe_modifiers().as_deref(),
                    Some(|node: &Node| self.modifier_visitor(node)),
                    Some(is_modifier),
                    None,
                    None,
                ),
                self.factory
                    .create_variable_declaration_list(
                        vec![self
                            .factory
                            .create_variable_declaration(
                                Some(self.factory.get_local_name(node, Some(false), Some(true))),
                                None,
                                None,
                                None,
                            )
                            .wrap()],
                        Some(
                            if self.current_lexical_scope().kind() == SyntaxKind::SourceFile {
                                NodeFlags::None
                            } else {
                                NodeFlags::Let
                            },
                        ),
                    )
                    .wrap(),
            )
            .wrap()
            .set_original_node(Some(node.node_wrapper()));

        self.record_emitted_declaration_in_scope(node);
        if self.is_first_emitted_declaration_in_scope(node) {
            if node.kind() == SyntaxKind::EnumDeclaration {
                set_source_map_range(
                    &*statement.as_variable_statement().declaration_list,
                    Some(node.into()),
                );
            } else {
                set_source_map_range(&*statement, Some(node.into()));
            }

            set_comment_range(&statement, node);
            add_emit_flags(
                &*statement,
                EmitFlags::NoTrailingComments | EmitFlags::HasEndOfDeclarationMarker,
            );
            statements.push(statement);
            true
        } else {
            let merge_marker = self
                .factory
                .create_merge_declaration_marker(statement)
                .set_emit_flags(EmitFlags::NoComments | EmitFlags::HasEndOfDeclarationMarker);
            statements.push(merge_marker);
            false
        }
    }

    pub(super) fn visit_module_declaration(
        &self,
        node: &Node, /*ModuleDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_as_module_declaration = node.as_module_declaration();
        if !self.should_emit_module_declaration(node) {
            return Ok(Some(
                self.factory
                    .create_not_emitted_statement(node.node_wrapper())
                    .into(),
            ));
        }

        Debug_.assert_node(
            node_as_module_declaration.maybe_name(),
            Some(is_identifier),
            Some("A TypeScript namespace should have an Identifier name."),
        );
        self.enable_substitution_for_namespace_exports();

        let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();

        let mut emit_flags = EmitFlags::AdviseOnEmitNode;

        let var_added = self.add_var_for_enum_or_module_declaration(&mut statements, node);
        if var_added {
            if self.module_kind != ModuleKind::System
                || !are_option_gcs_equal(
                    self.maybe_current_lexical_scope().as_ref(),
                    self.maybe_current_source_file().as_ref(),
                )
            {
                emit_flags |= EmitFlags::NoLeadingComments;
            }
        }

        let parameter_name = self.get_namespace_parameter_name(node);

        let container_name = self.get_namespace_container_name(node);

        let export_name = if has_syntactic_modifier(node, ModifierFlags::Export) {
            self.factory.get_external_module_or_namespace_export_name(
                self.maybe_current_namespace_container_name(),
                node,
                Some(false),
                Some(true),
            )
        } else {
            self.factory.get_local_name(node, Some(false), Some(true))
        };

        let mut module_arg = self
            .factory
            .create_logical_or(
                export_name.clone(),
                self.factory
                    .create_assignment(
                        export_name,
                        self.factory
                            .create_object_literal_expression(Option::<Gc<NodeArray>>::None, None)
                            .wrap(),
                    )
                    .wrap(),
            )
            .wrap();

        if self.has_namespace_qualified_export_name(node) {
            let local_name = self.factory.get_local_name(node, Some(false), Some(true));

            module_arg = self
                .factory
                .create_assignment(local_name, module_arg)
                .wrap();
        }

        let module_statement = self
            .factory
            .create_expression_statement(
                self.factory
                    .create_call_expression(
                        self.factory
                            .create_function_expression(
                                Option::<Gc<NodeArray>>::None,
                                None,
                                Option::<Gc<Node>>::None,
                                Option::<Gc<NodeArray>>::None,
                                Some(vec![self
                                    .factory
                                    .create_parameter_declaration(
                                        Option::<Gc<NodeArray>>::None,
                                        Option::<Gc<NodeArray>>::None,
                                        None,
                                        Some(parameter_name),
                                        None,
                                        None,
                                        None,
                                    )
                                    .wrap()]),
                                None,
                                self.transform_module_body(node, &container_name)?,
                            )
                            .wrap(),
                        Option::<Gc<NodeArray>>::None,
                        Some(vec![module_arg]),
                    )
                    .wrap(),
            )
            .wrap()
            .set_original_node(Some(node.node_wrapper()));

        if var_added {
            set_synthetic_leading_comments(&module_statement, None);
            set_synthetic_trailing_comments(&module_statement, None);
        }
        set_text_range(&*module_statement, Some(node));
        add_emit_flags(&*module_statement, emit_flags);
        statements.push(module_statement);

        statements.push(
            self.factory
                .create_end_of_declaration_marker(node.node_wrapper()),
        );
        Ok(Some(statements.into()))
    }

    pub(super) fn transform_module_body(
        &self,
        node: &Node,                 /*ModuleDeclaration*/
        namespace_local_name: &Node, /*Identifier*/
    ) -> io::Result<Gc<Node>> /*Identifier*/ {
        let node_as_module_declaration = node.as_module_declaration();
        let saved_current_namespace_container_name = self.maybe_current_namespace_container_name();
        let saved_current_namespace = self.maybe_current_namespace();
        let saved_current_scope_first_declarations_of_name = self
            .maybe_current_scope_first_declarations_of_name()
            .clone();
        self.set_current_namespace_container_name(Some(namespace_local_name.node_wrapper()));
        self.set_current_namespace(Some(node.node_wrapper()));
        self.set_current_scope_first_declarations_of_name(None);

        let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();
        self.context.start_lexical_environment();

        let mut statements_location: Option<ReadonlyTextRangeConcrete> = Default::default();
        let mut block_location: Option<Gc<Node> /*TextRange*/> = Default::default();
        if let Some(node_body) = node_as_module_declaration.body.as_ref() {
            if node_body.kind() == SyntaxKind::ModuleBlock {
                self.try_save_state_and_invoke(node_body, |body: &Node| {
                    add_range(
                        &mut statements,
                        Some(&try_visit_nodes(
                            &body.as_module_block().statements,
                            Some(|node: &Node| self.namespace_element_visitor(node)),
                            Some(is_statement),
                            None,
                            None,
                        )?),
                        None,
                        None,
                    );

                    Ok(())
                })?;
                statements_location = Some((&*node_body.as_module_block().statements).into());
                block_location = Some(node_body.clone());
            } else {
                let result = self.visit_module_declaration(node_body)?;
                if let Some(result) = result {
                    match result {
                        SingleNodeOrVecNode::VecNode(result) => {
                            add_range(&mut statements, Some(&result), None, None);
                        }
                        SingleNodeOrVecNode::SingleNode(result) => {
                            statements.push(result);
                        }
                    }
                }

                let module_block = self
                    .get_inner_most_module_declaration_from_dotted_module(node)
                    .unwrap()
                    .as_module_declaration()
                    .body
                    .clone()
                    .unwrap();
                statements_location =
                    Some(move_range_pos(&*module_block.as_module_block().statements, -1).into());
            }
        }

        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.end_lexical_environment().as_deref(),
        );
        self.set_current_namespace_container_name(saved_current_namespace_container_name);
        self.set_current_namespace(saved_current_namespace);
        self.set_current_scope_first_declarations_of_name(
            saved_current_scope_first_declarations_of_name,
        );

        let block = self
            .factory
            .create_block(
                set_text_range_node_array(
                    self.factory.create_node_array(Some(statements), None),
                    statements_location.as_ref(),
                ),
                Some(true),
            )
            .wrap()
            .set_text_range(block_location.as_deref());

        if match node_as_module_declaration.body.as_ref() {
            None => true,
            Some(node_body) => node_body.kind() != SyntaxKind::ModuleBlock,
        } {
            set_emit_flags(&*block, get_emit_flags(&block) | EmitFlags::NoComments);
        }

        Ok(block)
    }

    pub(super) fn get_inner_most_module_declaration_from_dotted_module(
        &self,
        module_declaration: &Node, /*ModuleDeclaration*/
    ) -> Option<Gc<Node>> /*ModuleDeclaration*/ {
        let module_declaration_as_module_declaration = module_declaration.as_module_declaration();
        let module_body = module_declaration_as_module_declaration
            .body
            .as_ref()
            .unwrap();
        if module_body.kind() == SyntaxKind::ModuleDeclaration {
            let recursive_inner_module =
                self.get_inner_most_module_declaration_from_dotted_module(module_body);
            return Some(recursive_inner_module.unwrap_or_else(|| module_body.clone()));
        }

        None
    }

    pub(super) fn visit_import_declaration(
        &self,
        node: &Node, /*ImportDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_as_import_declaration = node.as_import_declaration();
        if node_as_import_declaration.import_clause.is_none() {
            return Ok(Some(node.node_wrapper().into()));
        }
        let node_import_clause = node_as_import_declaration.import_clause.as_ref().unwrap();
        if node_import_clause.as_import_clause().is_type_only {
            return Ok(None);
        }

        let import_clause = try_maybe_visit_node(
            Some(&**node_import_clause),
            Some(|node: &Node| self.visit_import_clause(node)),
            Some(is_import_clause),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?;
        Ok(
            if import_clause.is_some()
                || matches!(
                    self.compiler_options.imports_not_used_as_values,
                    Some(ImportsNotUsedAsValues::Preserve) | Some(ImportsNotUsedAsValues::Error)
                )
            {
                Some(
                    self.factory
                        .update_import_declaration(
                            node,
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            import_clause,
                            node_as_import_declaration.module_specifier.clone(),
                            node_as_import_declaration.assert_clause.clone(),
                        )
                        .into(),
                )
            } else {
                None
            },
        )
    }

    pub(super) fn visit_import_clause(
        &self,
        node: &Node, /*ImportClause*/
    ) -> io::Result<VisitResult> /*<ImportClause>*/ {
        let node_as_import_clause = node.as_import_clause();
        Debug_.assert(!node_as_import_clause.is_type_only, None);
        let name = if self.should_emit_alias_declaration(node)? {
            node_as_import_clause.name.as_ref()
        } else {
            None
        };
        let named_bindings = try_maybe_visit_node(
            node_as_import_clause.named_bindings.as_deref(),
            Some(|node: &Node| self.visit_named_import_bindings(node)),
            Some(is_named_import_bindings),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?;
        Ok(if name.is_some() || named_bindings.is_some() {
            Some(
                self.factory
                    .update_import_clause(node, false, name.cloned(), named_bindings)
                    .into(),
            )
        } else {
            None
        })
    }

    pub(super) fn visit_named_import_bindings(
        &self,
        node: &Node, /*NamedImportBindings*/
    ) -> io::Result<VisitResult> /*<NamedImportBindings>*/ {
        Ok(if node.kind() == SyntaxKind::NamespaceImport {
            if self.should_emit_alias_declaration(node)? {
                Some(node.node_wrapper().into())
            } else {
                None
            }
        } else {
            let allow_empty = self.compiler_options.preserve_value_imports == Some(true)
                && matches!(
                    self.compiler_options.imports_not_used_as_values,
                    Some(ImportsNotUsedAsValues::Preserve) | Some(ImportsNotUsedAsValues::Error)
                );
            let elements = try_visit_nodes(
                &node.as_named_imports().elements,
                Some(|node: &Node| self.visit_import_specifier(node)),
                Some(is_import_specifier),
                None,
                None,
            )?;
            (allow_empty || !elements.is_empty())
                .then(|| self.factory.update_named_imports(node, elements).into())
        })
    }

    pub(super) fn visit_import_specifier(
        &self,
        node: &Node, /*ImportSpecifier*/
    ) -> io::Result<VisitResult> /*<ImportSpecifier>*/ {
        let node_as_import_specifier = node.as_import_specifier();
        Ok(
            (!node_as_import_specifier.is_type_only && self.should_emit_alias_declaration(node)?)
                .then(|| node.node_wrapper().into()),
        )
    }

    pub(super) fn visit_export_assignment(
        &self,
        node: &Node, /*ExportAssignment*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        self.resolver
            .is_value_alias_declaration(node)?
            .try_then_and(|| -> io::Result<_> {
                Ok(try_maybe_visit_each_child(
                    Some(node),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                )?
                .map(Into::into))
            })
    }

    pub(super) fn visit_export_declaration(
        &self,
        node: &Node, /*ExportDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_as_export_declaration = node.as_export_declaration();
        if node_as_export_declaration.is_type_only {
            return Ok(None);
        }

        if !node_as_export_declaration
            .export_clause
            .as_ref()
            .matches(|node_export_clause| !is_namespace_export(node_export_clause))
        {
            return Ok(Some(node.node_wrapper().into()));
        }

        let allow_empty = node_as_export_declaration.module_specifier.is_some()
            && matches!(
                self.compiler_options.imports_not_used_as_values,
                Some(ImportsNotUsedAsValues::Preserve) | Some(ImportsNotUsedAsValues::Error)
            );
        let export_clause = try_maybe_visit_node(
            node_as_export_declaration.export_clause.as_deref(),
            Some(|bindings: &Node /*NamedExportBindings*/| {
                self.visit_named_export_bindings(bindings, allow_empty)
            }),
            Some(is_named_export_bindings),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?;
        Ok(export_clause.map(|export_clause| {
            self.factory
                .update_export_declaration(
                    node,
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    node_as_export_declaration.is_type_only,
                    Some(export_clause),
                    node_as_export_declaration.module_specifier.clone(),
                    node_as_export_declaration.assert_clause.clone(),
                )
                .into()
        }))
    }

    pub(super) fn visit_named_exports(
        &self,
        node: &Node, /*NamedExports*/
        allow_empty: bool,
    ) -> io::Result<VisitResult> /*<NamedExports>*/ {
        let node_as_named_exports = node.as_named_exports();
        let elements = try_visit_nodes(
            &node_as_named_exports.elements,
            Some(|node: &Node| self.visit_export_specifier(node)),
            Some(is_export_specifier),
            None,
            None,
        )?;
        Ok((allow_empty || !elements.is_empty())
            .then(|| self.factory.update_named_exports(node, elements).into()))
    }

    pub(super) fn visit_namespace_exports(
        &self,
        node: &Node, /*NamespaceExport*/
    ) -> io::Result<VisitResult> /*<NamespaceExport>*/ {
        Ok(Some(
            self.factory
                .update_namespace_export(
                    node,
                    try_visit_node(
                        &node.as_namespace_export().name,
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_identifier),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_named_export_bindings(
        &self,
        node: &Node, /*NamedExportBindings*/
        allow_empty: bool,
    ) -> io::Result<VisitResult> /*<NamedExportBindings>*/ {
        Ok(if is_namespace_export(node) {
            self.visit_namespace_exports(node)?
        } else {
            self.visit_named_exports(node, allow_empty)?
        })
    }

    pub(super) fn visit_export_specifier(
        &self,
        node: &Node, /*ExportSpecifier*/
    ) -> io::Result<VisitResult> /*<ExportSpecifier>*/ {
        let node_as_export_specifier = node.as_export_specifier();
        Ok((!node_as_export_specifier.is_type_only
            && self.resolver.is_value_alias_declaration(node)?)
        .then(|| node.node_wrapper().into()))
    }

    pub(super) fn should_emit_import_equals_declaration(
        &self,
        node: &Node, /*ImportEqualsDeclaration*/
    ) -> io::Result<bool> {
        Ok(self.should_emit_alias_declaration(node)?
            || !is_external_module(&self.current_source_file())
                && self
                    .resolver
                    .is_top_level_value_import_equals_with_entity_name(node)?)
    }

    pub(super) fn visit_import_equals_declaration(
        &self,
        node: &Node, /*ImportEqualsDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_as_import_equals_declaration = node.as_import_equals_declaration();
        if node_as_import_equals_declaration.is_type_only {
            return Ok(None);
        }

        if is_external_module_import_equals_declaration(node) {
            let is_referenced = self.should_emit_alias_declaration(node)?;
            if !is_referenced
                && self.compiler_options.imports_not_used_as_values
                    == Some(ImportsNotUsedAsValues::Preserve)
            {
                return Ok(Some(
                    self.factory
                        .create_import_declaration(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            None,
                            node_as_import_equals_declaration
                                .module_reference
                                .as_external_module_reference()
                                .expression
                                .clone(),
                            None,
                        )
                        .wrap()
                        .set_text_range(Some(node))
                        .set_original_node(Some(node.node_wrapper()))
                        .into(),
                ));
            }

            return is_referenced.try_then(|| -> io::Result<_> {
                Ok(
                    try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?
                        .into(),
                )
            });
        }

        if !self.should_emit_import_equals_declaration(node)? {
            return Ok(None);
        }

        let module_reference = create_expression_from_entity_name(
            &*self.factory,
            &node_as_import_equals_declaration.module_reference,
        )
        .set_emit_flags(EmitFlags::NoComments | EmitFlags::NoNestedComments);

        Ok(Some(
            if self.is_named_external_module_export(node) || !self.is_export_of_namespace(node) {
                self.factory
                    .create_variable_statement(
                        maybe_visit_nodes(
                            node.maybe_modifiers().as_deref(),
                            Some(|node: &Node| self.modifier_visitor(node)),
                            Some(is_modifier),
                            None,
                            None,
                        ),
                        self.factory
                            .create_variable_declaration_list(
                                vec![self
                                    .factory
                                    .create_variable_declaration(
                                        node_as_import_equals_declaration.maybe_name(),
                                        None,
                                        None,
                                        Some(module_reference),
                                    )
                                    .wrap()
                                    .set_original_node(Some(node.node_wrapper()))],
                                None,
                            )
                            .wrap(),
                    )
                    .wrap()
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper()))
                    .into()
            } else {
                self.create_namespace_export(
                    &node_as_import_equals_declaration.name(),
                    &module_reference,
                    Some(node),
                )
                .set_original_node(Some(node.node_wrapper()))
                .into()
            },
        ))
    }
}
