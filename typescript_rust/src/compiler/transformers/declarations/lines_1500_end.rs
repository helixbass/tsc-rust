use std::io;

use id_arena::Id;

use super::TransformDeclarations;
use crate::{
    create_get_symbol_accessibility_diagnostic_for_node, flatten, for_each_bool,
    get_effective_modifier_flags, get_factory, get_parse_tree_node, has_effective_modifier,
    is_binding_pattern, is_entity_name_expression, is_export_assignment, is_export_declaration,
    is_external_module, is_internal_declaration, released, return_ok_default_if_none, some,
    try_map_defined, try_visit_nodes, AllAccessorDeclarations, Debug_,
    GetSymbolAccessibilityDiagnostic, HasArena, HasTypeInterface, InArena, ModifierFlags,
    NamedDeclarationInterface, Node, NodeArray, NodeArrayOrVec, NodeInterface, OptionTry,
    SignatureDeclarationInterface, SyntaxKind,
};

impl TransformDeclarations {
    pub(super) fn transform_variable_statement(
        &self,
        input: Id<Node>, /*VariableStatement*/
    ) -> io::Result<Option<Id<Node>>> {
        if !for_each_bool(
            &*input
                .ref_(self)
                .as_variable_statement()
                .declaration_list
                .ref_(self)
                .as_variable_declaration_list()
                .declarations
                .ref_(self),
            |&declaration: &Id<Node>, _| self.get_binding_name_visible(declaration),
        ) {
            return Ok(None);
        }
        let nodes = return_ok_default_if_none!(Some(try_visit_nodes(
            released!(
                input
                    .ref_(self)
                    .as_variable_statement()
                    .declaration_list
                    .ref_(self)
                    .as_variable_declaration_list()
                    .declarations
            ),
            Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
            Option::<fn(Id<Node>) -> bool>::None,
            None,
            None,
            self,
        )?));
        if nodes.ref_(self).is_empty() {
            return Ok(None);
        }
        Ok(Some(
            self.factory.ref_(self).update_variable_statement(
                input,
                Some(
                    self.factory
                        .ref_(self)
                        .create_node_array(self.ensure_modifiers(input), None),
                ),
                self.factory.ref_(self).update_variable_declaration_list(
                    released!(input.ref_(self).as_variable_statement().declaration_list),
                    nodes,
                ),
            ),
        ))
    }

    pub(super) fn recreate_binding_pattern(
        &self,
        d: Id<Node>, /*BindingPattern*/
    ) -> io::Result<Vec<Id<Node /*VariableDeclaration*/>>> {
        Ok(flatten(&try_map_defined(
            Some(&*released!(d.ref_(self).as_has_elements().elements()).ref_(self)),
            |&e: &Id<Node>, _| self.recreate_binding_element(e),
        )?))
    }

    pub(super) fn recreate_binding_element(
        &self,
        e: Id<Node>, /*ArrayBindingElement*/
    ) -> io::Result<Option<Vec<Id<Node>>>> {
        if e.ref_(self).kind() == SyntaxKind::OmittedExpression {
            return Ok(None);
        }
        let e_name = e.ref_(self).as_binding_element().maybe_name();
        e_name.try_and_then(|e_name| {
            if !self.get_binding_name_visible(e) {
                return Ok(None);
            }
            Ok(if is_binding_pattern(Some(&*e_name.ref_(self))) {
                Some(self.recreate_binding_pattern(e_name)?)
            } else {
                Some(vec![self.factory.ref_(self).create_variable_declaration(
                    Some(e_name),
                    None,
                    self.ensure_type(e, None, None)?,
                    None,
                )])
            })
        })
    }

    pub(super) fn check_name(
        &self,
        node: Id<Node>, /*DeclarationDiagnosticProducing*/
    ) -> io::Result<()> {
        let mut old_diag: Option<GetSymbolAccessibilityDiagnostic> = None;
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            old_diag = Some(self.get_symbol_accessibility_diagnostic());
            self.set_get_symbol_accessibility_diagnostic(
                create_get_symbol_accessibility_diagnostic_for_node(node, self),
            );
        }
        self.set_error_name_node(node.ref_(self).as_named_declaration().maybe_name());
        Debug_.assert(
            self.resolver.ref_(self).is_late_bound(
                get_parse_tree_node(Some(node), Option::<fn(Id<Node>) -> bool>::None, self)
                    .unwrap(),
            )?,
            None,
        );
        let decl = node;
        let entity_name = decl
            .ref_(self)
            .as_named_declaration()
            .name()
            .ref_(self)
            .as_has_expression()
            .expression();
        self.check_entity_name_visibility(entity_name, self.enclosing_declaration())?;
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(old_diag.unwrap());
        }
        self.set_error_name_node(None);

        Ok(())
    }

    pub(super) fn should_strip_internal(&self, node: Id<Node>) -> bool {
        self.strip_internal == Some(true) &&
            /* !!node &&*/ is_internal_declaration(node, self.current_source_file(), self)
    }

    pub(super) fn is_scope_marker(&self, node: Id<Node>) -> bool {
        is_export_assignment(&node.ref_(self)) || is_export_declaration(&node.ref_(self))
    }

    pub(super) fn has_scope_marker(&self, statements: Id<NodeArray>) -> bool {
        some(
            Some(&*statements.ref_(self)),
            Some(|&statement: &Id<Node>| self.is_scope_marker(statement)),
        )
    }

    pub(super) fn ensure_modifiers(&self, node: Id<Node>) -> Option<NodeArrayOrVec> {
        let current_flags = get_effective_modifier_flags(node, self);
        let new_flags = self.ensure_modifier_flags(node);
        if current_flags == new_flags {
            return node.ref_(self).maybe_modifiers().map(Into::into);
        }
        Some(
            self.factory
                .ref_(self)
                .create_modifiers_from_modifier_flags(new_flags)
                .into(),
        )
    }

    pub(super) fn ensure_modifier_flags(&self, node: Id<Node>) -> ModifierFlags {
        let mut mask = ModifierFlags::All
            ^ (ModifierFlags::Public | ModifierFlags::Async | ModifierFlags::Override);
        let mut additions = if self.needs_declare() && !is_always_type(&node.ref_(self)) {
            ModifierFlags::Ambient
        } else {
            ModifierFlags::None
        };
        let parent_is_file = node.ref_(self).parent().ref_(self).kind() == SyntaxKind::SourceFile;
        if !parent_is_file
            || self.is_bundled_emit()
                && parent_is_file
                && is_external_module(&node.ref_(self).parent().ref_(self))
        {
            mask ^= ModifierFlags::Ambient;
            additions = ModifierFlags::None;
        }
        mask_modifier_flags(node, Some(mask), Some(additions), self)
    }

    pub(super) fn get_type_annotation_from_all_accessor_declarations(
        &self,
        node: Id<Node>, /*AccessorDeclaration*/
        accessors: &AllAccessorDeclarations,
    ) -> Option<Id<Node>> {
        let mut accessor_type = get_type_annotation_from_accessor(node, self);
        if accessor_type.is_none() && node != accessors.first_accessor {
            accessor_type = get_type_annotation_from_accessor(accessors.first_accessor, self);
            self.set_get_symbol_accessibility_diagnostic(
                create_get_symbol_accessibility_diagnostic_for_node(accessors.first_accessor, self),
            );
        }
        if accessor_type.is_none() {
            if let Some(accessors_second_accessor) = accessors
                .second_accessor
                .filter(|&accessors_second_accessor| node != accessors_second_accessor)
            {
                accessor_type = get_type_annotation_from_accessor(accessors_second_accessor, self);
                self.set_get_symbol_accessibility_diagnostic(
                    create_get_symbol_accessibility_diagnostic_for_node(
                        accessors_second_accessor,
                        self,
                    ),
                );
            }
        }
        accessor_type
    }

    pub(super) fn transform_heritage_clauses(
        &self,
        nodes: Option<Id<NodeArray> /*<HeritageClause>*/>,
    ) -> io::Result<Id<NodeArray>> {
        Ok(self.factory.ref_(self).create_node_array(
            nodes.try_map(|nodes| -> io::Result<_> {
                let mut ret = vec![];
                for &clause in &*released!(nodes.ref_(self).clone()) {
                    let clause = self.factory.ref_(self).update_heritage_clause(
                        clause,
                        try_visit_nodes(
                            self.factory.ref_(self).create_node_array(
                                released!(Some(
                                    clause
                                        .ref_(self)
                                        .as_heritage_clause()
                                        .types
                                        .ref_(self)
                                        .iter()
                                        .filter(|t| {
                                            let t_ref = t.ref_(self);
                                            let t_as_expression_with_type_arguments =
                                                t_ref.as_expression_with_type_arguments();
                                            is_entity_name_expression(
                                                t_as_expression_with_type_arguments.expression,
                                                self,
                                            ) || clause.ref_(self).as_heritage_clause().token
                                                == SyntaxKind::ExtendsKeyword
                                                && t_as_expression_with_type_arguments
                                                    .expression
                                                    .ref_(self)
                                                    .kind()
                                                    == SyntaxKind::NullKeyword
                                        })
                                        .cloned()
                                        .collect::<Vec<_>>(),
                                )),
                                None,
                            ),
                            Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                            Option::<fn(Id<Node>) -> bool>::None,
                            None,
                            None,
                            self,
                        )?,
                    );
                    if
                    /*clause.types &&*/
                    !clause
                        .ref_(self)
                        .as_heritage_clause()
                        .types
                        .ref_(self)
                        .is_empty()
                    {
                        ret.push(clause);
                    }
                }
                Ok(ret)
            })?,
            None,
        ))
    }
}

pub(super) fn is_always_type(node: &Node) -> bool {
    if node.kind() == SyntaxKind::InterfaceDeclaration {
        return true;
    }
    false
}

pub(super) fn mask_modifiers(
    node: Id<Node>,
    modifier_mask: Option<ModifierFlags>,
    modifier_additions: Option<ModifierFlags>,
    arena: &impl HasArena,
) -> Vec<Id<Node /*Modifier*/>> {
    get_factory(arena).create_modifiers_from_modifier_flags(mask_modifier_flags(
        node,
        modifier_mask,
        modifier_additions,
        arena,
    ))
}

pub(super) fn mask_modifier_flags(
    node: Id<Node>,
    modifier_mask: Option<ModifierFlags>,
    modifier_additions: Option<ModifierFlags>,
    arena: &impl HasArena,
) -> ModifierFlags {
    let modifier_mask = modifier_mask.unwrap_or(ModifierFlags::All ^ ModifierFlags::Public);
    let modifier_additions = modifier_additions.unwrap_or(ModifierFlags::None);
    let mut flags =
        (get_effective_modifier_flags(node, arena) & modifier_mask) | modifier_additions;
    if flags.intersects(ModifierFlags::Default) && !flags.intersects(ModifierFlags::Export) {
        flags ^= ModifierFlags::Export;
    }
    if flags.intersects(ModifierFlags::Default) && flags.intersects(ModifierFlags::Ambient) {
        flags ^= ModifierFlags::Ambient;
    }
    flags
}

pub(super) fn get_type_annotation_from_accessor(
    accessor: Id<Node>, /*AccessorDeclaration*/
    arena: &impl HasArena,
) -> Option<Id<Node /*TypeNode*/>> {
    // if (accessor) {
    if accessor.ref_(arena).kind() == SyntaxKind::GetAccessor {
        accessor
            .ref_(arena)
            .as_get_accessor_declaration()
            .maybe_type()
    } else {
        let accessor_ref = accessor.ref_(arena);
        let accessor_as_set_accessor_declaration = accessor_ref.as_set_accessor_declaration();
        if !accessor_as_set_accessor_declaration
            .parameters()
            .ref_(arena)
            .is_empty()
        {
            accessor_as_set_accessor_declaration
                .parameters()
                .ref_(arena)[0]
                .ref_(arena)
                .as_parameter_declaration()
                .maybe_type()
        } else {
            None
        }
    }
    // }
}

pub(super) fn can_have_literal_initializer(node: Id<Node>, arena: &impl HasArena) -> bool {
    match node.ref_(arena).kind() {
        SyntaxKind::PropertyDeclaration | SyntaxKind::PropertySignature => {
            !has_effective_modifier(node, ModifierFlags::Private, arena)
        }
        SyntaxKind::Parameter | SyntaxKind::VariableDeclaration => true,
        _ => false,
    }
}

pub(super) fn is_preserved_declaration_statement(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::FunctionDeclaration
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::VariableStatement
            | SyntaxKind::ImportDeclaration
            | SyntaxKind::ExportDeclaration
            | SyntaxKind::ExportAssignment
    )
}

pub(super) fn is_processed_component(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ConstructSignature
            | SyntaxKind::Constructor
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::PropertySignature
            | SyntaxKind::MethodSignature
            | SyntaxKind::CallSignature
            | SyntaxKind::IndexSignature
            | SyntaxKind::VariableDeclaration
            | SyntaxKind::TypeParameter
            | SyntaxKind::ExpressionWithTypeArguments
            | SyntaxKind::TypeReference
            | SyntaxKind::ConditionalType
            | SyntaxKind::FunctionType
            | SyntaxKind::ConstructorType
            | SyntaxKind::ImportType
    )
}
