use std::ptr;

use gc::Gc;

use super::TransformDeclarations;
use crate::{
    create_get_symbol_accessibility_diagnostic_for_node, flatten, for_each_bool,
    get_effective_modifier_flags, get_parse_tree_node, has_effective_modifier, is_binding_pattern,
    is_entity_name_expression, is_export_assignment, is_export_declaration, is_external_module,
    is_internal_declaration, length, map_defined, some, visit_nodes, with_synthetic_factory,
    with_synthetic_factory_and_factory, AllAccessorDeclarations, AsDoubleDeref, Debug_,
    GetSymbolAccessibilityDiagnostic, HasTypeInterface, ModifierFlags, NamedDeclarationInterface,
    Node, NodeArray, NodeArrayOrVec, NodeInterface, SignatureDeclarationInterface, SyntaxKind,
};

impl TransformDeclarations {
    pub(super) fn transform_variable_statement(
        &self,
        input: &Node, /*VariableStatement*/
    ) -> Option<Gc<Node>> {
        let input_as_variable_statement = input.as_variable_statement();
        if !for_each_bool(
            &input_as_variable_statement
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
            |declaration: &Gc<Node>, _| self.get_binding_name_visible(declaration),
        ) {
            return None;
        }
        let nodes = visit_nodes(
            Some(
                &input_as_variable_statement
                    .declaration_list
                    .as_variable_declaration_list()
                    .declarations,
            ),
            Some(|node: &Node| self.visit_declaration_subtree(node)),
            Option::<fn(&Node) -> bool>::None,
            None,
            None,
        )?;
        if nodes.is_empty() {
            return None;
        }
        Some(with_synthetic_factory(|synthetic_factory_| {
            self.factory.update_variable_statement(
                synthetic_factory_,
                input,
                Some(
                    self.factory
                        .create_node_array(self.ensure_modifiers(input), None),
                ),
                self.factory.update_variable_declaration_list(
                    synthetic_factory_,
                    &input_as_variable_statement.declaration_list,
                    nodes,
                ),
            )
        }))
    }

    pub(super) fn recreate_binding_pattern(
        &self,
        d: &Node, /*BindingPattern*/
    ) -> Vec<Gc<Node /*VariableDeclaration*/>> {
        flatten(&map_defined(
            Some(&d.as_has_elements().elements()),
            |e: &Gc<Node>, _| self.recreate_binding_element(e),
        ))
    }

    pub(super) fn recreate_binding_element(
        &self,
        e: &Node, /*ArrayBindingElement*/
    ) -> Option<Vec<Gc<Node>>> {
        if e.kind() == SyntaxKind::OmittedExpression {
            return None;
        }
        let e_name = e.as_binding_element().maybe_name();
        e_name.and_then(|e_name| {
            if !self.get_binding_name_visible(e) {
                return None;
            }
            if is_binding_pattern(Some(&*e_name)) {
                Some(self.recreate_binding_pattern(&e_name))
            } else {
                Some(vec![with_synthetic_factory(|synthetic_factory_| {
                    self.factory
                        .create_variable_declaration(
                            synthetic_factory_,
                            Some(e_name),
                            None,
                            self.ensure_type(e, None, None),
                            None,
                        )
                        .wrap()
                })])
            }
        })
    }

    pub(super) fn check_name(&self, node: &Node /*DeclarationDiagnosticProducing*/) {
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
                &get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None).unwrap(),
            ),
            None,
        );
        let decl = node;
        let ref entity_name = decl
            .as_named_declaration()
            .name()
            .as_has_expression()
            .expression();
        self.check_entity_name_visibility(entity_name, &self.enclosing_declaration());
        if self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(old_diag.unwrap());
        }
        self.set_error_name_node(None);
    }

    pub(super) fn should_strip_internal(&self, node: &Node) -> bool {
        self.strip_internal == Some(true) &&
            /* !!node &&*/ is_internal_declaration(node, &self.current_source_file())
    }

    pub(super) fn is_scope_marker(&self, node: &Node) -> bool {
        is_export_assignment(node) || is_export_declaration(node)
    }

    pub(super) fn has_scope_marker(&self, statements: &NodeArray) -> bool {
        some(
            Some(statements),
            Some(|statement: &Gc<Node>| self.is_scope_marker(statement)),
        )
    }

    pub(super) fn ensure_modifiers(&self, node: &Node) -> Option<NodeArrayOrVec> {
        let current_flags = get_effective_modifier_flags(node);
        let new_flags = self.ensure_modifier_flags(node);
        if current_flags == new_flags {
            return node.maybe_modifiers().map(Into::into);
        }
        Some(with_synthetic_factory(|synthetic_factory_| {
            self.factory
                .create_modifiers_from_modifier_flags(synthetic_factory_, new_flags)
                .into()
        }))
    }

    pub(super) fn ensure_modifier_flags(&self, node: &Node) -> ModifierFlags {
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
        node: &Node, /*AccessorDeclaration*/
        accessors: &AllAccessorDeclarations,
    ) -> Option<Gc<Node>> {
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
    ) -> Gc<NodeArray> {
        self.factory.create_node_array(
            nodes.map(|nodes| {
                nodes
                    .into_iter()
                    .map(|clause| {
                        let clause_as_heritage_clause = clause.as_heritage_clause();
                        with_synthetic_factory(|synthetic_factory_| {
                            self.factory.update_heritage_clause(
                                synthetic_factory_,
                                clause,
                                visit_nodes(
                                    Some(
                                        &self.factory.create_node_array(
                                            Some(
                                                clause_as_heritage_clause
                                                    .types
                                                    .iter()
                                                    .filter(|t| {
                                                        let t_as_expression_with_type_arguments =
                                                            t.as_expression_with_type_arguments();
                                                        is_entity_name_expression(
                                                            &t_as_expression_with_type_arguments
                                                                .expression,
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
                                    ),
                                    Some(|node: &Node| self.visit_declaration_subtree(node)),
                                    Option::<fn(&Node) -> bool>::None,
                                    None,
                                    None,
                                )
                                .unwrap(),
                            )
                        })
                    })
                    .filter(|clause| {
                        /*clause.types &&*/
                        !clause.as_heritage_clause().types.is_empty()
                    })
                    .collect::<Vec<_>>()
            }),
            None,
        )
    }
}

pub(super) fn is_always_type(node: &Node) -> bool {
    if node.kind() == SyntaxKind::InterfaceDeclaration {
        return true;
    }
    false
}

pub(super) fn mask_modifiers(
    node: &Node,
    modifier_mask: Option<ModifierFlags>,
    modifier_additions: Option<ModifierFlags>,
) -> Vec<Gc<Node /*Modifier*/>> {
    with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
        factory_.create_modifiers_from_modifier_flags(
            synthetic_factory_,
            mask_modifier_flags(node, modifier_mask, modifier_additions),
        )
    })
}

pub(super) fn mask_modifier_flags(
    node: &Node,
    modifier_mask: Option<ModifierFlags>,
    modifier_additions: Option<ModifierFlags>,
) -> ModifierFlags {
    let modifier_mask = modifier_mask.unwrap_or(ModifierFlags::All ^ ModifierFlags::Public);
    let modifier_additions = modifier_additions.unwrap_or(ModifierFlags::None);
    let mut flags = (get_effective_modifier_flags(node) & modifier_mask) | modifier_additions;
    if flags.intersects(ModifierFlags::Default) && !flags.intersects(ModifierFlags::Export) {
        flags ^= ModifierFlags::Export;
    }
    if flags.intersects(ModifierFlags::Default) && flags.intersects(ModifierFlags::Ambient) {
        flags ^= ModifierFlags::Ambient;
    }
    flags
}

pub(super) fn get_type_annotation_from_accessor(
    accessor: &Node, /*AccessorDeclaration*/
) -> Option<Gc<Node /*TypeNode*/>> {
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

pub(super) fn can_have_literal_initializer(node: &Node) -> bool {
    match node.kind() {
        SyntaxKind::PropertyDeclaration | SyntaxKind::PropertySignature => {
            !has_effective_modifier(node, ModifierFlags::Private)
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
