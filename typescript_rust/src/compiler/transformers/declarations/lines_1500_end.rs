use std::{io, ptr};

use gc::Gc;
use id_arena::Id;

use super::TransformDeclarations;
use crate::{
    create_get_symbol_accessibility_diagnostic_for_node, flatten, for_each_bool,
    get_effective_modifier_flags, get_factory, get_parse_tree_node, has_effective_modifier,
    is_binding_pattern, is_entity_name_expression, is_export_assignment, is_export_declaration,
    is_external_module, is_internal_declaration, return_ok_default_if_none, some, try_map_defined,
    try_visit_nodes, AllAccessorDeclarations, Debug_, GetSymbolAccessibilityDiagnostic,
    HasTypeInterface, ModifierFlags, NamedDeclarationInterface, Node, NodeArray, NodeArrayOrVec,
    NodeInterface, OptionTry, SignatureDeclarationInterface, SyntaxKind, HasArena,
};

impl TransformDeclarations {
    pub(super) fn transform_variable_statement(
        &self,
        input: Id<Node>, /*VariableStatement*/
    ) -> io::Result<Option<Id<Node>>> {
        let input_as_variable_statement = input.as_variable_statement();
        if !for_each_bool(
            &input_as_variable_statement
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
            |declaration: &Id<Node>, _| self.get_binding_name_visible(declaration),
        ) {
            return Ok(None);
        }
        let nodes = return_ok_default_if_none!(Some(try_visit_nodes(
            &input_as_variable_statement
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
            Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
            Option::<fn(Id<Node>) -> bool>::None,
            None,
            None,
        )?));
        if nodes.is_empty() {
            return Ok(None);
        }
        Ok(Some(
            self.factory.update_variable_statement(
                input,
                Some(
                    self.factory
                        .create_node_array(self.ensure_modifiers(input), None),
                ),
                self.factory.update_variable_declaration_list(
                    &input_as_variable_statement.declaration_list,
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
            Some(&d.as_has_elements().elements()),
            |e: &Id<Node>, _| self.recreate_binding_element(e),
        )?))
    }

    pub(super) fn recreate_binding_element(
        &self,
        e: Id<Node>, /*ArrayBindingElement*/
    ) -> io::Result<Option<Vec<Id<Node>>>> {
        if e.kind() == SyntaxKind::OmittedExpression {
            return Ok(None);
        }
        let e_name = e.as_binding_element().maybe_name();
        e_name.try_and_then(|e_name| {
            if !self.get_binding_name_visible(e) {
                return Ok(None);
            }
            Ok(if is_binding_pattern(Some(&*e_name)) {
                Some(self.recreate_binding_pattern(&e_name)?)
            } else {
                Some(vec![self.factory.create_variable_declaration(
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
                create_get_symbol_accessibility_diagnostic_for_node(node),
            );
        }
        self.set_error_name_node(node.as_named_declaration().maybe_name());
        Debug_.assert(
            self.resolver.is_late_bound(
                &get_parse_tree_node(Some(node), Option::<fn(Id<Node>) -> bool>::None).unwrap(),
            )?,
            None,
        );
        let decl = node;
        let ref entity_name = decl
            .as_named_declaration()
            .name()
            .as_has_expression()
            .expression();
        self.check_entity_name_visibility(entity_name, &self.enclosing_declaration())?;
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(old_diag.unwrap());
        }
        self.set_error_name_node(None);

        Ok(())
    }

    pub(super) fn should_strip_internal(&self, node: Id<Node>) -> bool {
        self.strip_internal == Some(true) &&
            /* !!node &&*/ is_internal_declaration(node, &self.current_source_file())
    }

    pub(super) fn is_scope_marker(&self, node: Id<Node>) -> bool {
        is_export_assignment(node) || is_export_declaration(node)
    }

    pub(super) fn has_scope_marker(&self, statements: &NodeArray) -> bool {
        some(
            Some(statements),
            Some(|statement: &Id<Node>| self.is_scope_marker(statement)),
        )
    }

    pub(super) fn ensure_modifiers(&self, node: Id<Node>) -> Option<NodeArrayOrVec> {
        let current_flags = get_effective_modifier_flags(node, self);
        let new_flags = self.ensure_modifier_flags(node);
        if current_flags == new_flags {
            return node.maybe_modifiers().map(Into::into);
        }
        Some(
            self.factory
                .create_modifiers_from_modifier_flags(new_flags)
                .into(),
        )
    }

    pub(super) fn ensure_modifier_flags(&self, node: Id<Node>) -> ModifierFlags {
        let mut mask = ModifierFlags::All
            ^ (ModifierFlags::Public | ModifierFlags::Async | ModifierFlags::Override);
        let mut additions = if self.needs_declare() && !is_always_type(node) {
            ModifierFlags::Ambient
        } else {
            ModifierFlags::None
        };
        let parent_is_file = node.parent().kind() == SyntaxKind::SourceFile;
        if !parent_is_file
            || self.is_bundled_emit() && parent_is_file && is_external_module(&node.parent())
        {
            mask ^= ModifierFlags::Ambient;
            additions = ModifierFlags::None;
        }
        mask_modifier_flags(node, Some(mask), Some(additions))
    }

    pub(super) fn get_type_annotation_from_all_accessor_declarations(
        &self,
        node: Id<Node>, /*AccessorDeclaration*/
        accessors: &AllAccessorDeclarations,
    ) -> Option<Id<Node>> {
        let mut accessor_type = get_type_annotation_from_accessor(node);
        if accessor_type.is_none() && !ptr::eq(node, &*accessors.first_accessor) {
            accessor_type = get_type_annotation_from_accessor(&accessors.first_accessor);
            self.set_get_symbol_accessibility_diagnostic(
                create_get_symbol_accessibility_diagnostic_for_node(&accessors.first_accessor),
            );
        }
        if accessor_type.is_none() {
            if let Some(accessors_second_accessor) = accessors
                .second_accessor
                .as_ref()
                .filter(|accessors_second_accessor| !ptr::eq(node, &***accessors_second_accessor))
            {
                accessor_type = get_type_annotation_from_accessor(accessors_second_accessor);
                self.set_get_symbol_accessibility_diagnostic(
                    create_get_symbol_accessibility_diagnostic_for_node(accessors_second_accessor),
                );
            }
        }
        accessor_type
    }

    pub(super) fn transform_heritage_clauses(
        &self,
        nodes: Option<&NodeArray /*<HeritageClause>*/>,
    ) -> io::Result<Gc<NodeArray>> {
        Ok(self.factory.create_node_array(
            nodes.try_map(|nodes| -> io::Result<_> {
                let mut ret = vec![];
                for clause in nodes {
                    let clause_as_heritage_clause = clause.as_heritage_clause();
                    let clause = self.factory.update_heritage_clause(
                        clause,
                        try_visit_nodes(
                            &self.factory.create_node_array(
                                Some(
                                    clause_as_heritage_clause
                                        .types
                                        .iter()
                                        .filter(|t| {
                                            let t_as_expression_with_type_arguments =
                                                t.as_expression_with_type_arguments();
                                            is_entity_name_expression(
                                                &t_as_expression_with_type_arguments.expression,
                                            ) || clause_as_heritage_clause.token
                                                == SyntaxKind::ExtendsKeyword
                                                && t_as_expression_with_type_arguments
                                                    .expression
                                                    .kind()
                                                    == SyntaxKind::NullKeyword
                                        })
                                        .cloned()
                                        .collect::<Vec<_>>(),
                                ),
                                None,
                            ),
                            Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                            Option::<fn(Id<Node>) -> bool>::None,
                            None,
                            None,
                        )?,
                    );
                    if
                    /*clause.types &&*/
                    !clause.as_heritage_clause().types.is_empty() {
                        ret.push(clause);
                    }
                }
                Ok(ret)
            })?,
            None,
        ))
    }
}

pub(super) fn is_always_type(node: Id<Node>) -> bool {
    if node.kind() == SyntaxKind::InterfaceDeclaration {
        return true;
    }
    false
}

pub(super) fn mask_modifiers(
    node: Id<Node>,
    modifier_mask: Option<ModifierFlags>,
    modifier_additions: Option<ModifierFlags>,
) -> Vec<Id<Node /*Modifier*/>> {
    get_factory().create_modifiers_from_modifier_flags(mask_modifier_flags(
        node,
        modifier_mask,
        modifier_additions,
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
    let mut flags = (get_effective_modifier_flags(node, arena) & modifier_mask) | modifier_additions;
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
) -> Option<Id<Node /*TypeNode*/>> {
    // if (accessor) {
    if accessor.kind() == SyntaxKind::GetAccessor {
        accessor.as_get_accessor_declaration().maybe_type()
    } else {
        let accessor_as_set_accessor_declaration = accessor.as_set_accessor_declaration();
        if !accessor_as_set_accessor_declaration.parameters().is_empty() {
            accessor_as_set_accessor_declaration.parameters()[0]
                .as_parameter_declaration()
                .maybe_type()
        } else {
            None
        }
    }
    // }
}

pub(super) fn can_have_literal_initializer(node: Id<Node>) -> bool {
    match node.kind() {
        SyntaxKind::PropertyDeclaration | SyntaxKind::PropertySignature => {
            !has_effective_modifier(node, ModifierFlags::Private, self)
        }
        SyntaxKind::Parameter | SyntaxKind::VariableDeclaration => true,
        _ => false,
    }
}

pub(super) fn is_preserved_declaration_statement(node: Id<Node>) -> bool {
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

pub(super) fn is_processed_component(node: Id<Node>) -> bool {
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
