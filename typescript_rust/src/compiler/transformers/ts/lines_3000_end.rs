use gc::Gc;

use crate::{
    create_range, get_original_node_id, id_text, is_static,
    NamedDeclarationInterface, NodeCheckFlags, NodeExt, SyntaxKind,
};
use crate::{Node, NodeInterface, ReadonlyTextRange};

use super::{TransformTypeScript, TypeScriptSubstitutionFlags};
use crate::has_syntactic_modifier;
use crate::ModifierFlags;
use std::io;

impl TransformTypeScript {
    pub(super) fn is_export_of_namespace(&self, node: &Node) -> bool {
        self.maybe_current_namespace().is_some()
            && has_syntactic_modifier(node, ModifierFlags::Export)
    }

    pub(super) fn is_external_module_export(&self, node: &Node) -> bool {
        self.maybe_current_namespace().is_none()
            && has_syntactic_modifier(node, ModifierFlags::Export)
    }

    pub(super) fn is_named_external_module_export(&self, node: &Node) -> bool {
        self.is_external_module_export(node)
            && !has_syntactic_modifier(node, ModifierFlags::Default)
    }

    pub(super) fn is_default_external_module_export(&self, node: &Node) -> bool {
        self.is_external_module_export(node) && has_syntactic_modifier(node, ModifierFlags::Default)
    }

    pub(super) fn expression_to_statement(&self, expression: Gc<Node /*Expression*/>) -> Gc<Node> {
        self.factory.create_expression_statement(expression).wrap()
    }

    pub(super) fn add_export_member_assignment(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ClassDeclaration | FunctionDeclaration*/
    ) {
        let expression = self
            .factory
            .create_assignment(
                self.factory.get_external_module_or_namespace_export_name(
                    self.maybe_current_namespace_container_name(),
                    node,
                    Some(false),
                    Some(true),
                ),
                self.factory.get_local_name(node, None, None),
            )
            .wrap()
            .set_source_map_range(Some(
                (&create_range(
                    node.as_named_declaration()
                        .maybe_name()
                        .map_or_else(|| node.pos(), |node_name| node_name.pos()),
                    Some(node.end()),
                ))
                    .into(),
            ));

        let statement = self
            .factory
            .create_expression_statement(expression)
            .wrap()
            .set_source_map_range(Some((&create_range(-1, Some(node.end()))).into()));
        statements.push(statement);
    }

    pub(super) fn create_namespace_export(
        &self,
        export_name: &Node,  /*Identifier*/
        export_value: &Node, /*Expression*/
        location: Option<&impl ReadonlyTextRange>,
    ) -> Gc<Node> {
        self.factory
            .create_expression_statement(
                self.factory
                    .create_assignment(
                        self.factory.get_namespace_member_name(
                            &self.current_namespace_container_name(),
                            export_name,
                            Some(false),
                            Some(true),
                        ),
                        export_value.node_wrapper(),
                    )
                    .wrap(),
            )
            .wrap()
            .set_text_range(location)
    }

    pub(super) fn create_namespace_export_expression(
        &self,
        export_name: &Node,  /*Identifier*/
        export_value: &Node, /*Expression*/
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    ) -> Gc<Node> {
        self.factory
            .create_assignment(
                self.get_namespace_member_name_with_source_maps_and_without_comments(export_name),
                export_value.node_wrapper(),
            )
            .wrap()
            .set_text_range(location)
    }

    pub(super) fn get_namespace_member_name_with_source_maps_and_without_comments(
        &self,
        name: &Node, /*Identifier*/
    ) -> Gc<Node> {
        self.factory.get_namespace_member_name(
            &self.current_namespace_container_name(),
            name,
            Some(false),
            Some(true),
        )
    }

    pub(super) fn get_namespace_parameter_name(
        &self,
        node: &Node, /*ModuleDeclaration | EnumDeclaration*/
    ) -> Gc<Node> {
        self.factory
            .get_generated_name_for_node(Some(node), None)
            .set_source_map_range(
                node.as_named_declaration()
                    .maybe_name()
                    .as_deref()
                    .map(Into::into),
            )
    }

    pub(super) fn get_namespace_container_name(
        &self,
        node: &Node, /*ModuleDeclaration | EnumDeclaration*/
    ) -> Gc<Node> {
        self.factory.get_generated_name_for_node(Some(node), None)
    }

    pub(super) fn get_class_alias_if_needed(
        &self,
        node: &Node, /*ClassDeclaration*/
    ) -> Option<Gc<Node>> {
        if self
            .resolver
            .get_node_check_flags(node)
            .intersects(NodeCheckFlags::ClassWithConstructorReference)
        {
            self.enable_substitution_for_class_aliases();
            let class_alias = self.factory.create_unique_name(
                node.as_class_declaration()
                    .maybe_name()
                    .as_ref()
                    .map_or_else(|| "default", |node_name| id_text(node_name)),
                None,
            );
            self.class_aliases_mut()
                .insert(get_original_node_id(node), class_alias.clone());
            self.context.hoist_variable_declaration(&class_alias);
            return Some(class_alias);
        }
        None
    }

    pub(super) fn get_class_prototype(
        &self,
        node: &Node, /*ClassExpression | ClassDeclaration*/
    ) -> Gc<Node> {
        self.factory
            .create_property_access_expression(
                self.factory.get_declaration_name(Some(node), None, None),
                "prototype",
            )
            .wrap()
    }

    pub(super) fn get_class_member_prefix(
        &self,
        node: &Node,   /*ClassExpression | ClassDeclaration*/
        member: &Node, /*ClassElement*/
    ) -> Gc<Node> {
        if is_static(member) {
            self.factory.get_declaration_name(Some(node), None, None)
        } else {
            self.get_class_prototype(node)
        }
    }

    pub(super) fn enable_substitution_for_non_qualified_enum_members(&self) {
        if !self
            .enabled_substitutions()
            .intersects(TypeScriptSubstitutionFlags::NonQualifiedEnumMembers)
        {
            self.set_enabled_substitutions(
                self.enabled_substitutions() | TypeScriptSubstitutionFlags::NonQualifiedEnumMembers,
            );
            self.context.enable_substitution(SyntaxKind::Identifier);
        }
    }

    pub(super) fn enable_substitution_for_class_aliases(&self) {
        if !self
            .enabled_substitutions()
            .intersects(TypeScriptSubstitutionFlags::ClassAliases)
        {
            self.set_enabled_substitutions(
                self.enabled_substitutions() | TypeScriptSubstitutionFlags::ClassAliases,
            );

            self.context.enable_substitution(SyntaxKind::Identifier);

            self.set_class_aliases(Some(Default::default()));
        }
    }

    pub(super) fn enable_substitution_for_namespace_exports(&self) {
        if !self
            .enabled_substitutions()
            .intersects(TypeScriptSubstitutionFlags::NamespaceExports)
        {
            self.set_enabled_substitutions(
                self.enabled_substitutions() | TypeScriptSubstitutionFlags::NamespaceExports,
            );

            self.context.enable_substitution(SyntaxKind::Identifier);
            self.context
                .enable_substitution(SyntaxKind::ShorthandPropertyAssignment);

            self.context
                .enable_emit_notification(SyntaxKind::ModuleDeclaration);
        }
    }

    pub(super) fn should_emit_alias_declaration(&self, node: &Node) -> io::Result<bool> {
        Ok(
            if self.compiler_options.preserve_value_imports == Some(true) {
                self.resolver.is_value_alias_declaration(node)?
            } else {
                self.resolver.is_referenced_alias_declaration(node, None)?
            },
        )
    }
}
