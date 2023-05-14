use std::ptr;

use gc::Gc;

use crate::{
    Matches, ModuleKind, Node, NodeInterface, VisitResult, __String, add_emit_flags, is_identifier,
    is_modifier, set_comment_range, set_source_map_range, visit_nodes, Debug_, EmitFlags, NodeExt,
    NodeFlags, SyntaxKind,
};

use super::TransformTypeScript;

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
                visit_nodes(
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
        _node: &Node, /*ModuleDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_import_declaration(
        &self,
        _node: &Node, /*ImportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_export_assignment(
        &self,
        _node: &Node, /*ExportAssignment*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_export_declaration(
        &self,
        _node: &Node, /*ExportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_import_equals_declaration(
        &self,
        _node: &Node, /*ImportEqualsDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }
}
