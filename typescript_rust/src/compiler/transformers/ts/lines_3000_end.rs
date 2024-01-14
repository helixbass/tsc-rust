use std::io;

use gc::Gc;
use id_arena::Id;

use super::{TransformTypeScript, TypeScriptSubstitutionFlags};
use crate::{
    create_range, get_original_node_id, has_syntactic_modifier, id_text, is_static, ModifierFlags,
    NamedDeclarationInterface, Node, NodeCheckFlags, NodeExt, NodeInterface, ReadonlyTextRange,
    SyntaxKind,
};

impl TransformTypeScript {
    pub(super) fn is_export_of_namespace(&self, node: Id<Node>) -> bool {
        self.maybe_current_namespace().is_some()
            && has_syntactic_modifier(node, ModifierFlags::Export, self)
    }

    pub(super) fn is_external_module_export(&self, node: Id<Node>) -> bool {
        self.maybe_current_namespace().is_none()
            && has_syntactic_modifier(node, ModifierFlags::Export, self)
    }

    pub(super) fn is_named_external_module_export(&self, node: Id<Node>) -> bool {
        self.is_external_module_export(node)
            && !has_syntactic_modifier(node, ModifierFlags::Default, self)
    }

    pub(super) fn is_default_external_module_export(&self, node: Id<Node>) -> bool {
        self.is_external_module_export(node) && has_syntactic_modifier(node, ModifierFlags::Default, self)
    }

    pub(super) fn expression_to_statement(&self, expression: Id<Node /*Expression*/>) -> Id<Node> {
        self.factory.create_expression_statement(expression)
    }

    pub(super) fn add_export_member_assignment(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        node: Id<Node>, /*ClassDeclaration | FunctionDeclaration*/
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
            .set_source_map_range(Some((&create_range(-1, Some(node.end()))).into()));
        statements.push(statement);
    }

    pub(super) fn create_namespace_export(
        &self,
        export_name: Id<Node>,  /*Identifier*/
        export_value: Id<Node>, /*Expression*/
        location: Option<&impl ReadonlyTextRange>,
    ) -> Id<Node> {
        self.factory
            .create_expression_statement(self.factory.create_assignment(
                self.factory.get_namespace_member_name(
                    &self.current_namespace_container_name(),
                    export_name,
                    Some(false),
                    Some(true),
                ),
                export_value.node_wrapper(),
            ))
            .set_text_range(location)
    }

    pub(super) fn create_namespace_export_expression(
        &self,
        export_name: Id<Node>,  /*Identifier*/
        export_value: Id<Node>, /*Expression*/
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    ) -> Id<Node> {
        self.factory
            .create_assignment(
                self.get_namespace_member_name_with_source_maps_and_without_comments(export_name),
                export_value.node_wrapper(),
            )
            .set_text_range(location)
    }

    pub(super) fn get_namespace_member_name_with_source_maps_and_without_comments(
        &self,
        name: Id<Node>, /*Identifier*/
    ) -> Id<Node> {
        self.factory.get_namespace_member_name(
            &self.current_namespace_container_name(),
            name,
            Some(false),
            Some(true),
        )
    }

    pub(super) fn get_namespace_parameter_name(
        &self,
        node: Id<Node>, /*ModuleDeclaration | EnumDeclaration*/
    ) -> Id<Node> {
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
        node: Id<Node>, /*ModuleDeclaration | EnumDeclaration*/
    ) -> Id<Node> {
        self.factory.get_generated_name_for_node(Some(node), None)
    }

    pub(super) fn get_class_alias_if_needed(
        &self,
        node: Id<Node>, /*ClassDeclaration*/
    ) -> Option<Id<Node>> {
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
        node: Id<Node>, /*ClassExpression | ClassDeclaration*/
    ) -> Id<Node> {
        self.factory.create_property_access_expression(
            self.factory.get_declaration_name(Some(node), None, None),
            "prototype",
        )
    }

    pub(super) fn get_class_member_prefix(
        &self,
        node: Id<Node>,   /*ClassExpression | ClassDeclaration*/
        member: Id<Node>, /*ClassElement*/
    ) -> Id<Node> {
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

    pub(super) fn should_emit_alias_declaration(&self, node: Id<Node>) -> io::Result<bool> {
        Ok(
            if self.compiler_options.preserve_value_imports == Some(true) {
                self.resolver.is_value_alias_declaration(node)?
            } else {
                self.resolver.is_referenced_alias_declaration(node, None)?
            },
        )
    }
}
