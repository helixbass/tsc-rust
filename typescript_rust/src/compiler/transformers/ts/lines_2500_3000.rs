use std::{io, ptr};

use gc::Gc;
use id_arena::Id;

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
    GetOrInsertDefault, ImportsNotUsedAsValues, ModifierFlags, NamedDeclarationInterface,
    NodeArray, NodeExt, NodeFlags, ReadonlyTextRangeConcrete, SingleNodeOrVecNode, SyntaxKind,
    HasArena, InArena, OptionInArena,
    CoreTransformationContext,
};

impl TransformTypeScript {
    pub(super) fn has_namespace_qualified_export_name(&self, node: Id<Node>) -> bool {
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
        node: Id<Node>, /*FunctionDeclaration | ClassDeclaration | ModuleDeclaration | EnumDeclaration*/
    ) {
        let mut current_scope_first_declarations_of_name =
            self.maybe_current_scope_first_declarations_of_name_mut();
        let current_scope_first_declarations_of_name =
            current_scope_first_declarations_of_name.get_or_insert_default_();

        let name = self.declared_name_in_scope(node);
        current_scope_first_declarations_of_name
            .entry(name)
            .or_insert_with(|| node);
    }

    pub(super) fn is_first_emitted_declaration_in_scope(
        &self,
        node: Id<Node>, /*ModuleDeclaration | EnumDeclaration*/
    ) -> bool {
        if let Some(current_scope_first_declarations_of_name) = self
            .maybe_current_scope_first_declarations_of_name()
            .as_ref()
        {
            let name = self.declared_name_in_scope(node);
            return current_scope_first_declarations_of_name
                .get(&name)
                .matches(|&value| value == node);
        }
        true
    }

    pub(super) fn declared_name_in_scope(
        &self,
        node: Id<Node>, /*FunctionDeclaration | ClassDeclaration | ModuleDeclaration | EnumDeclaration*/
    ) -> __String {
        let node_ref = node.ref_(self);
        let node_as_named_declaration = node_ref.as_named_declaration();
        Debug_.assert_node(
            node_as_named_declaration.maybe_name(),
            Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
            None,
        );
        node_as_named_declaration
            .name()
            .ref_(self).as_identifier()
            .escaped_text
            .clone()
    }

    pub(super) fn add_var_for_enum_or_module_declaration(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        node: Id<Node>, /*ModuleDeclaration | EnumDeclaration*/
    ) -> bool {
        let statement = self
            .factory
            .ref_(self).create_variable_statement(
                maybe_visit_nodes(
                    node.ref_(self).maybe_modifiers().as_deref(),
                    Some(|node: Id<Node>| self.modifier_visitor(node)),
                    Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                    None,
                    None,
                    self,
                ),
                self.factory.ref_(self).create_variable_declaration_list(
                    vec![self.factory.ref_(self).create_variable_declaration(
                        Some(self.factory.ref_(self).get_local_name(node, Some(false), Some(true))),
                        None,
                        None,
                        None,
                    )],
                    Some(
                        if self.current_lexical_scope().ref_(self).kind() == SyntaxKind::SourceFile {
                            NodeFlags::None
                        } else {
                            NodeFlags::Let
                        },
                    ),
                ),
            )
            .set_original_node(Some(node), self);

        self.record_emitted_declaration_in_scope(node);
        if self.is_first_emitted_declaration_in_scope(node) {
            if node.ref_(self).kind() == SyntaxKind::EnumDeclaration {
                set_source_map_range(
                    statement.ref_(self).as_variable_statement().declaration_list,
                    Some(self.alloc_source_map_range((&*node.ref_(self)).into())),
                    self,
                );
            } else {
                set_source_map_range(statement, Some(self.alloc_source_map_range((&*node.ref_(self)).into())), self);
            }

            set_comment_range(statement, &*node.ref_(self), self);
            add_emit_flags(
                statement,
                EmitFlags::NoTrailingComments | EmitFlags::HasEndOfDeclarationMarker,
                self,
            );
            statements.push(statement);
            true
        } else {
            let merge_marker = self
                .factory
                .ref_(self).create_merge_declaration_marker(statement)
                .set_emit_flags(EmitFlags::NoComments | EmitFlags::HasEndOfDeclarationMarker, self);
            statements.push(merge_marker);
            false
        }
    }

    pub(super) fn visit_module_declaration(
        &self,
        node: Id<Node>, /*ModuleDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_module_declaration = node_ref.as_module_declaration();
        if !self.should_emit_module_declaration(node) {
            return Ok(Some(
                self.factory
                    .ref_(self).create_not_emitted_statement(node)
                    .into(),
            ));
        }

        Debug_.assert_node(
            node_as_module_declaration.maybe_name(),
            Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
            Some("A TypeScript namespace should have an Identifier name."),
        );
        self.enable_substitution_for_namespace_exports();

        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();

        let mut emit_flags = EmitFlags::AdviseOnEmitNode;

        let var_added = self.add_var_for_enum_or_module_declaration(&mut statements, node);
        if var_added {
            if self.module_kind != ModuleKind::System
                || self.maybe_current_lexical_scope() != self.maybe_current_source_file()
            {
                emit_flags |= EmitFlags::NoLeadingComments;
            }
        }

        let parameter_name = self.get_namespace_parameter_name(node);

        let container_name = self.get_namespace_container_name(node);

        let export_name = if has_syntactic_modifier(node, ModifierFlags::Export, self) {
            self.factory.ref_(self).get_external_module_or_namespace_export_name(
                self.maybe_current_namespace_container_name(),
                node,
                Some(false),
                Some(true),
            )
        } else {
            self.factory.ref_(self).get_local_name(node, Some(false), Some(true))
        };

        let mut module_arg = self.factory.ref_(self).create_logical_or(
            export_name.clone(),
            self.factory.ref_(self).create_assignment(
                export_name,
                self.factory
                    .ref_(self).create_object_literal_expression(Option::<Id<NodeArray>>::None, None),
            ),
        );

        if self.has_namespace_qualified_export_name(node) {
            let local_name = self.factory.ref_(self).get_local_name(node, Some(false), Some(true));

            module_arg = self.factory.ref_(self).create_assignment(local_name, module_arg);
        }

        let module_statement = self
            .factory
            .ref_(self).create_expression_statement(self.factory.ref_(self).create_call_expression(
                self.factory.ref_(self).create_function_expression(
                    Option::<Id<NodeArray>>::None,
                    None,
                    Option::<Id<Node>>::None,
                    Option::<Id<NodeArray>>::None,
                    Some(vec![self.factory.ref_(self).create_parameter_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        None,
                        Some(parameter_name),
                        None,
                        None,
                        None,
                    )]),
                    None,
                    self.transform_module_body(node, container_name)?,
                ),
                Option::<Id<NodeArray>>::None,
                Some(vec![module_arg]),
            ))
            .set_original_node(Some(node), self);

        if var_added {
            set_synthetic_leading_comments(module_statement, None, self);
            set_synthetic_trailing_comments(module_statement, None, self);
        }
        set_text_range(&*module_statement.ref_(self), Some(&*node.ref_(self)));
        add_emit_flags(module_statement, emit_flags, self);
        statements.push(module_statement);

        statements.push(
            self.factory
                .ref_(self).create_end_of_declaration_marker(node),
        );
        Ok(Some(statements.into()))
    }

    pub(super) fn transform_module_body(
        &self,
        node: Id<Node>,                 /*ModuleDeclaration*/
        namespace_local_name: Id<Node>, /*Identifier*/
    ) -> io::Result<Id<Node>> /*Identifier*/ {
        let node_ref = node.ref_(self);
        let node_as_module_declaration = node_ref.as_module_declaration();
        let saved_current_namespace_container_name = self.maybe_current_namespace_container_name();
        let saved_current_namespace = self.maybe_current_namespace();
        let saved_current_scope_first_declarations_of_name = self
            .maybe_current_scope_first_declarations_of_name()
            .clone();
        self.set_current_namespace_container_name(Some(namespace_local_name));
        self.set_current_namespace(Some(node));
        self.set_current_scope_first_declarations_of_name(None);

        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
        self.context.ref_(self).start_lexical_environment();

        let mut statements_location: Option<ReadonlyTextRangeConcrete> = Default::default();
        let mut block_location: Option<Id<Node> /*TextRange*/> = Default::default();
        if let Some(node_body) = node_as_module_declaration.body {
            if node_body.ref_(self).kind() == SyntaxKind::ModuleBlock {
                self.try_save_state_and_invoke(node_body, |body: Id<Node>| {
                    add_range(
                        &mut statements,
                        Some(&try_visit_nodes(
                            &body.ref_(self).as_module_block().statements,
                            Some(|node: Id<Node>| self.namespace_element_visitor(node)),
                            Some(|node| is_statement(node, self)),
                            None,
                            None,
                            self,
                        )?),
                        None,
                        None,
                    );

                    Ok(())
                })?;
                statements_location = Some((&*node_body.ref_(self).as_module_block().statements).into());
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
                    .ref_(self).as_module_declaration()
                    .body
                    .unwrap();
                statements_location =
                    Some(move_range_pos(&*module_block.ref_(self).as_module_block().statements, -1).into());
            }
        }

        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.ref_(self).end_lexical_environment().as_deref(),
            self,
        );
        self.set_current_namespace_container_name(saved_current_namespace_container_name);
        self.set_current_namespace(saved_current_namespace);
        self.set_current_scope_first_declarations_of_name(
            saved_current_scope_first_declarations_of_name,
        );

        let block = self
            .factory
            .ref_(self).create_block(
                set_text_range_node_array(
                    self.factory.ref_(self).create_node_array(Some(statements), None),
                    statements_location.as_ref(),
                    self,
                ),
                Some(true),
            )
            .set_text_range(block_location.refed(self).as_deref(), self);

        if match node_as_module_declaration.body {
            None => true,
            Some(node_body) => node_body.ref_(self).kind() != SyntaxKind::ModuleBlock,
        } {
            set_emit_flags(block, get_emit_flags(&block.ref_(self)) | EmitFlags::NoComments, self);
        }

        Ok(block)
    }

    #[allow(clippy::only_used_in_recursion)]
    pub(super) fn get_inner_most_module_declaration_from_dotted_module(
        &self,
        module_declaration: Id<Node>, /*ModuleDeclaration*/
    ) -> Option<Id<Node>> /*ModuleDeclaration*/ {
        let module_declaration_ref = module_declaration.ref_(self);
        let module_declaration_as_module_declaration = module_declaration_ref.as_module_declaration();
        let module_body = module_declaration_as_module_declaration
            .body
            .unwrap();
        if module_body.ref_(self).kind() == SyntaxKind::ModuleDeclaration {
            let recursive_inner_module =
                self.get_inner_most_module_declaration_from_dotted_module(module_body);
            return Some(recursive_inner_module.unwrap_or_else(|| module_body.clone()));
        }

        None
    }

    pub(super) fn visit_import_declaration(
        &self,
        node: Id<Node>, /*ImportDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_import_declaration = node_ref.as_import_declaration();
        if node_as_import_declaration.import_clause.is_none() {
            return Ok(Some(node.into()));
        }
        let node_import_clause = node_as_import_declaration.import_clause.unwrap();
        if node_import_clause.ref_(self).as_import_clause().is_type_only {
            return Ok(None);
        }

        let import_clause = try_maybe_visit_node(
            Some(node_import_clause),
            Some(|node: Id<Node>| self.visit_import_clause(node)),
            Some(|node: Id<Node>| is_import_clause(&node.ref_(self))),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        Ok(
            if import_clause.is_some()
                || matches!(
                    self.compiler_options.ref_(self).imports_not_used_as_values,
                    Some(ImportsNotUsedAsValues::Preserve) | Some(ImportsNotUsedAsValues::Error)
                )
            {
                Some(
                    self.factory
                        .ref_(self).update_import_declaration(
                            node,
                            Option::<Id<NodeArray>>::None,
                            Option::<Id<NodeArray>>::None,
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
        node: Id<Node>, /*ImportClause*/
    ) -> io::Result<VisitResult> /*<ImportClause>*/ {
        let node_ref = node.ref_(self);
        let node_as_import_clause = node_ref.as_import_clause();
        Debug_.assert(!node_as_import_clause.is_type_only, None);
        let name = if self.should_emit_alias_declaration(node)? {
            node_as_import_clause.name
        } else {
            None
        };
        let named_bindings = try_maybe_visit_node(
            node_as_import_clause.named_bindings,
            Some(|node: Id<Node>| self.visit_named_import_bindings(node)),
            Some(|node: Id<Node>| is_named_import_bindings(&node.ref_(self))),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        Ok(if name.is_some() || named_bindings.is_some() {
            Some(
                self.factory
                    .ref_(self).update_import_clause(node, false, name, named_bindings)
                    .into(),
            )
        } else {
            None
        })
    }

    pub(super) fn visit_named_import_bindings(
        &self,
        node: Id<Node>, /*NamedImportBindings*/
    ) -> io::Result<VisitResult> /*<NamedImportBindings>*/ {
        Ok(if node.ref_(self).kind() == SyntaxKind::NamespaceImport {
            if self.should_emit_alias_declaration(node)? {
                Some(node.into())
            } else {
                None
            }
        } else {
            let allow_empty = self.compiler_options.ref_(self).preserve_value_imports == Some(true)
                && matches!(
                    self.compiler_options.ref_(self).imports_not_used_as_values,
                    Some(ImportsNotUsedAsValues::Preserve) | Some(ImportsNotUsedAsValues::Error)
                );
            let elements = try_visit_nodes(
                &node.ref_(self).as_named_imports().elements,
                Some(|node: Id<Node>| self.visit_import_specifier(node)),
                Some(|node: Id<Node>| is_import_specifier(&node.ref_(self))),
                None,
                None,
                self,
            )?;
            (allow_empty || !elements.is_empty())
                .then(|| self.factory.ref_(self).update_named_imports(node, elements).into())
        })
    }

    pub(super) fn visit_import_specifier(
        &self,
        node: Id<Node>, /*ImportSpecifier*/
    ) -> io::Result<VisitResult> /*<ImportSpecifier>*/ {
        let node_ref = node.ref_(self);
        let node_as_import_specifier = node_ref.as_import_specifier();
        Ok(
            (!node_as_import_specifier.is_type_only && self.should_emit_alias_declaration(node)?)
                .then(|| node.into()),
        )
    }

    pub(super) fn visit_export_assignment(
        &self,
        node: Id<Node>, /*ExportAssignment*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        self.resolver
            .ref_(self).is_value_alias_declaration(node)?
            .try_then_and(|| -> io::Result<_> {
                Ok(try_maybe_visit_each_child(
                    Some(node),
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )?
                .map(Into::into))
            })
    }

    pub(super) fn visit_export_declaration(
        &self,
        node: Id<Node>, /*ExportDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_export_declaration = node_ref.as_export_declaration();
        if node_as_export_declaration.is_type_only {
            return Ok(None);
        }

        if !node_as_export_declaration
            .export_clause
            .matches(|node_export_clause| !is_namespace_export(&node_export_clause.ref_(self)))
        {
            return Ok(Some(node.into()));
        }

        let allow_empty = node_as_export_declaration.module_specifier.is_some()
            && matches!(
                self.compiler_options.ref_(self).imports_not_used_as_values,
                Some(ImportsNotUsedAsValues::Preserve) | Some(ImportsNotUsedAsValues::Error)
            );
        let export_clause = try_maybe_visit_node(
            node_as_export_declaration.export_clause,
            Some(|bindings: Id<Node> /*NamedExportBindings*/| {
                self.visit_named_export_bindings(bindings, allow_empty)
            }),
            Some(|node: Id<Node>| is_named_export_bindings(&node.ref_(self))),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        Ok(export_clause.map(|export_clause| {
            self.factory
                .ref_(self).update_export_declaration(
                    node,
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
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
        node: Id<Node>, /*NamedExports*/
        allow_empty: bool,
    ) -> io::Result<VisitResult> /*<NamedExports>*/ {
        let node_ref = node.ref_(self);
        let node_as_named_exports = node_ref.as_named_exports();
        let elements = try_visit_nodes(
            &node_as_named_exports.elements,
            Some(|node: Id<Node>| self.visit_export_specifier(node)),
            Some(|node: Id<Node>| is_export_specifier(&node.ref_(self))),
            None,
            None,
            self,
        )?;
        Ok((allow_empty || !elements.is_empty())
            .then(|| self.factory.ref_(self).update_named_exports(node, elements).into()))
    }

    pub(super) fn visit_namespace_exports(
        &self,
        node: Id<Node>, /*NamespaceExport*/
    ) -> io::Result<VisitResult> /*<NamespaceExport>*/ {
        Ok(Some(
            self.factory
                .ref_(self).update_namespace_export(
                    node,
                    try_visit_node(
                        node.ref_(self).as_namespace_export().name,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_named_export_bindings(
        &self,
        node: Id<Node>, /*NamedExportBindings*/
        allow_empty: bool,
    ) -> io::Result<VisitResult> /*<NamedExportBindings>*/ {
        Ok(if is_namespace_export(&node.ref_(self)) {
            self.visit_namespace_exports(node)?
        } else {
            self.visit_named_exports(node, allow_empty)?
        })
    }

    pub(super) fn visit_export_specifier(
        &self,
        node: Id<Node>, /*ExportSpecifier*/
    ) -> io::Result<VisitResult> /*<ExportSpecifier>*/ {
        let node_ref = node.ref_(self);
        let node_as_export_specifier = node_ref.as_export_specifier();
        Ok((!node_as_export_specifier.is_type_only
            && self.resolver.ref_(self).is_value_alias_declaration(node)?)
        .then(|| node.into()))
    }

    pub(super) fn should_emit_import_equals_declaration(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration*/
    ) -> io::Result<bool> {
        Ok(self.should_emit_alias_declaration(node)?
            || !is_external_module(&self.current_source_file().ref_(self))
                && self
                    .resolver
                    .ref_(self).is_top_level_value_import_equals_with_entity_name(node)?)
    }

    pub(super) fn visit_import_equals_declaration(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_import_equals_declaration = node_ref.as_import_equals_declaration();
        if node_as_import_equals_declaration.is_type_only {
            return Ok(None);
        }

        if is_external_module_import_equals_declaration(node, self) {
            let is_referenced = self.should_emit_alias_declaration(node)?;
            if !is_referenced
                && self.compiler_options.ref_(self).imports_not_used_as_values
                    == Some(ImportsNotUsedAsValues::Preserve)
            {
                return Ok(Some(
                    self.factory
                        .ref_(self).create_import_declaration(
                            Option::<Id<NodeArray>>::None,
                            Option::<Id<NodeArray>>::None,
                            None,
                            node_as_import_equals_declaration
                                .module_reference
                                .ref_(self).as_external_module_reference()
                                .expression,
                            None,
                        )
                        .set_text_range(Some(&*node.ref_(self)), self)
                        .set_original_node(Some(node), self)
                        .into(),
                ));
            }

            return is_referenced.try_then(|| -> io::Result<_> {
                Ok(try_visit_each_child(
                    node,
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )?
                .into())
            });
        }

        if !self.should_emit_import_equals_declaration(node)? {
            return Ok(None);
        }

        let module_reference = create_expression_from_entity_name(
            &self.factory.ref_(self),
            node_as_import_equals_declaration.module_reference,
        )
        .set_emit_flags(EmitFlags::NoComments | EmitFlags::NoNestedComments, self);

        Ok(Some(
            if self.is_named_external_module_export(node) || !self.is_export_of_namespace(node) {
                self.factory
                    .ref_(self).create_variable_statement(
                        maybe_visit_nodes(
                            node.ref_(self).maybe_modifiers().as_deref(),
                            Some(|node: Id<Node>| self.modifier_visitor(node)),
                            Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                            None,
                            None,
                            self,
                        ),
                        self.factory.ref_(self).create_variable_declaration_list(
                            vec![self
                                .factory
                                .ref_(self).create_variable_declaration(
                                    node_as_import_equals_declaration.maybe_name(),
                                    None,
                                    None,
                                    Some(module_reference),
                                )
                                .set_original_node(Some(node), self)],
                            None,
                        ),
                    )
                    .set_text_range(Some(&*node.ref_(self)), self)
                    .set_original_node(Some(node), self)
                    .into()
            } else {
                self.create_namespace_export(
                    node_as_import_equals_declaration.name(),
                    module_reference,
                    Some(&*node.ref_(self)),
                )
                .set_original_node(Some(node), self)
                .into()
            },
        ))
    }
}
