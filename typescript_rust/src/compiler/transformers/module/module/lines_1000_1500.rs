use gc::Gc;

use super::TransformModule;
use crate::{
    has_syntactic_modifier, is_external_module_import_equals_declaration, Debug_, ModifierFlags,
    ModuleKind, NamedDeclarationInterface, Node, NodeArray, NodeExt, NodeFlags, NodeInterface,
    ReadonlyTextRange, ScriptTarget, VisitResult, _d, get_emit_flags, get_es_module_interop,
    get_original_node_id, id_text, is_export_namespace_as_default_declaration, is_named_exports,
    single_or_many_node, EmitFlags,
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
        _node: &Node, /*ExportAssignment*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_function_declaration(
        &self,
        _node: &Node, /*FunctionDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_class_declaration(
        &self,
        _node: &Node, /*ClassDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_variable_statement(
        &self,
        _node: &Node, /*VariableStatement*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn create_all_export_expressions(
        &self,
        _name: &Node,  /*Identifier*/
        _value: &Node, /*Expression*/
        _location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn visit_merge_declaration_marker(
        &self,
        _node: &Node, /*MergeDeclarationMarker*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn has_associated_end_of_declaration_marker(&self, _node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn visit_end_of_declaration_marker(
        &self,
        _node: &Node, /*EndOfDeclarationMarker*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn append_exports_of_import_declaration(
        &self,
        _statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        _decl: &Node, /*ImportDeclaration*/
    ) /*: Statement[] | undefined */
    {
        unimplemented!()
    }
}
