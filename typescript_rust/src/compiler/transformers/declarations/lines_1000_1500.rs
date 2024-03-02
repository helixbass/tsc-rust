use std::io;

use id_arena::Id;

use super::{
    declaration_emit_node_builder_flags, is_preserved_declaration_statement, TransformDeclarations,
};
use crate::{
    can_have_modifiers, can_produce_diagnostics, create_empty_exports,
    create_get_symbol_accessibility_diagnostic_for_node, create_symbol_table,
    get_effective_base_type_node, get_effective_modifier_flags, get_first_constructor_with_body,
    get_original_node_id, get_parse_node_factory, has_dynamic_name, has_effective_modifier,
    has_syntactic_modifier, impl_has_arena, is_binding_pattern, is_declaration,
    is_entity_name_expression, is_external_module_augmentation, is_function_like,
    is_global_scope_augmentation, is_import_equals_declaration, is_omitted_expression,
    is_private_identifier, is_property_access_expression, is_source_file,
    is_string_a_non_contextual_keyword, is_type_node, is_type_parameter_declaration, map,
    map_defined, maybe_concatenate, maybe_get_original_node_id, released,
    return_ok_default_if_none, set_original_node, set_parent, some, try_flat_map, try_map,
    try_map_defined, try_maybe_map, try_maybe_visit_node, try_maybe_visit_nodes, try_visit_node,
    try_visit_nodes, unescape_leading_underscores, visit_nodes, AllArenas,
    ClassLikeDeclarationInterface, Debug_, Diagnostics, GeneratedIdentifierFlags,
    GetOrInsertDefault, GetSymbolAccessibilityDiagnostic,
    GetSymbolAccessibilityDiagnosticInterface, HasArena, HasQuestionTokenInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, InArena,
    InterfaceOrClassLikeDeclarationInterface, ModifierFlags, NamedDeclarationInterface, Node,
    NodeArray, NodeFlags, NodeInterface, OptionInArena, OptionTry, SignatureDeclarationInterface,
    SingleNodeOrVecNode, StringOrNumber, Symbol, SymbolAccessibilityDiagnostic,
    SymbolAccessibilityResult, SymbolInterface, SyntaxKind, VisitResult, VisitResultInterface,
};

impl TransformDeclarations {
    pub(super) fn visit_declaration_subtree_cleanup(
        &self,
        input: Id<Node>,
        can_produce_diagnostic: bool,
        previous_enclosing_declaration: Option<Id<Node>>,
        old_diag: &GetSymbolAccessibilityDiagnostic,
        should_enter_suppress_new_diagnostics_context_context: bool,
        old_within_object_literal_type: Option<bool>,
        return_value: Option<Id<Node>>,
    ) -> io::Result<VisitResult> {
        if return_value.is_some() && can_produce_diagnostic && has_dynamic_name(input, self) {
            self.check_name(input)?;
        }
        if self.is_enclosing_declaration(input) {
            self.set_enclosing_declaration(previous_enclosing_declaration);
        }
        if can_produce_diagnostic && self.maybe_suppress_new_diagnostic_contexts() != Some(true) {
            self.set_get_symbol_accessibility_diagnostic(old_diag.clone());
        }
        if should_enter_suppress_new_diagnostics_context_context {
            self.set_suppress_new_diagnostic_contexts(old_within_object_literal_type);
        }
        if return_value == Some(input) {
            return Ok(return_value.map(Into::into));
        }
        Ok(return_value
            .map(|return_value| {
                set_original_node(self.preserve_js_doc(return_value, input), Some(input), self)
            })
            .map(Into::into))
    }

    pub(super) fn is_private_method_type_parameter(
        &self,
        node: Id<Node>, /*TypeParameterDeclaration*/
    ) -> bool {
        node.ref_(self).parent().ref_(self).kind() == SyntaxKind::MethodDeclaration
            && has_effective_modifier(node.ref_(self).parent(), ModifierFlags::Private, self)
    }

    pub(super) fn visit_declaration_statements(&self, input: Id<Node>) -> io::Result<VisitResult> /*<Node>*/
    {
        if !is_preserved_declaration_statement(&input.ref_(self)) {
            return Ok(None);
        }
        if self.should_strip_internal(input) {
            return Ok(None);
        }

        match input.ref_(self).kind() {
            SyntaxKind::ExportDeclaration => {
                let input_ref = input.ref_(self);
                let input_as_export_declaration = input_ref.as_export_declaration();
                if is_source_file(&input.ref_(self).parent().ref_(self)) {
                    self.set_result_has_external_module_indicator(true);
                }
                self.set_result_has_scope_marker(true);
                return Ok(Some(
                    self.factory
                        .ref_(self)
                        .update_export_declaration(
                            input,
                            Option::<Id<NodeArray>>::None,
                            input.ref_(self).maybe_modifiers(),
                            input_as_export_declaration.is_type_only,
                            input_as_export_declaration.export_clause.clone(),
                            self.rewrite_module_specifier(
                                input,
                                input_as_export_declaration.module_specifier,
                            )?,
                            None,
                        )
                        .into(),
                ));
            }
            SyntaxKind::ExportAssignment => {
                let input_ref = input.ref_(self);
                let input_as_export_assignment = input_ref.as_export_assignment();
                if is_source_file(&input.ref_(self).parent().ref_(self)) {
                    self.set_result_has_external_module_indicator(true);
                }
                self.set_result_has_scope_marker(true);
                if input_as_export_assignment.expression.ref_(self).kind() == SyntaxKind::Identifier
                {
                    return Ok(Some(input.into()));
                } else {
                    let new_id = self
                        .factory
                        .ref_(self)
                        .create_unique_name("_default", Some(GeneratedIdentifierFlags::Optimistic));
                    self.set_get_symbol_accessibility_diagnostic(
                        VisitDeclarationStatementsGetSymbolAccessibilityDiagnostic::new(
                            input, self,
                        ),
                    );
                    self.set_error_fallback_node(Some(input));
                    let var_decl = self.factory.ref_(self).create_variable_declaration(
                        Some(new_id.clone()),
                        None,
                        self.resolver.ref_(self).create_type_of_expression(
                            input_as_export_assignment.expression,
                            input,
                            declaration_emit_node_builder_flags(),
                            self.symbol_tracker(),
                        )?,
                        None,
                    );
                    self.set_error_fallback_node(None);
                    let statement = self.factory.ref_(self).create_variable_statement(
                        if self.needs_declare() {
                            Some(vec![self
                                .factory
                                .ref_(self)
                                .create_modifier(SyntaxKind::DeclareKeyword)])
                        } else {
                            Some(vec![])
                        },
                        self.factory.ref_(self).create_variable_declaration_list(
                            vec![var_decl],
                            Some(NodeFlags::Const),
                        ),
                    );
                    return Ok(Some(
                        vec![
                            statement,
                            self.factory.ref_(self).update_export_assignment(
                                input,
                                input.ref_(self).maybe_decorators(),
                                input.ref_(self).maybe_modifiers(),
                                new_id,
                            ),
                        ]
                        .into(),
                    ));
                }
            }
            _ => (),
        }

        let result = self.transform_top_level_declaration(input)?;
        self.late_statement_replacement_map_mut()
            .insert(get_original_node_id(input, self), result);
        Ok(Some(input.into()))
    }

    pub(super) fn strip_export_modifiers(
        &self,
        statement: Id<Node>, /*Statement*/
    ) -> Id<Node /*Statement*/> {
        if is_import_equals_declaration(&statement.ref_(self))
            || has_effective_modifier(statement, ModifierFlags::Default, self)
            || !can_have_modifiers(&statement.ref_(self))
        {
            return statement;
        }

        let modifiers = self
            .factory
            .ref_(self)
            .create_modifiers_from_modifier_flags(
                get_effective_modifier_flags(statement, self)
                    & (ModifierFlags::All ^ ModifierFlags::Export),
            );
        self.factory
            .ref_(self)
            .update_modifiers(statement, modifiers)
    }

    pub(super) fn transform_top_level_declaration(
        &self,
        input: Id<Node>, /*LateVisibilityPaintedStatement*/
    ) -> io::Result<VisitResult> {
        if self.should_strip_internal(input) {
            return Ok(None);
        }
        match input.ref_(self).kind() {
            SyntaxKind::ImportEqualsDeclaration => {
                return Ok(self
                    .transform_import_equals_declaration(input)?
                    .map(Into::into));
            }
            SyntaxKind::ImportDeclaration => {
                return Ok(self.transform_import_declaration(input)?.map(Into::into));
            }
            _ => (),
        }
        if is_declaration(input, self) && self.is_declaration_and_not_visible(input) {
            return Ok(None);
        }

        if is_function_like(Some(&input.ref_(self)))
            && self
                .resolver
                .ref_(self)
                .is_implementation_of_overload(input)?
                == Some(true)
        {
            return Ok(None);
        }

        let mut previous_enclosing_declaration: Option<Id<Node>> = None;
        if self.is_enclosing_declaration(input) {
            previous_enclosing_declaration = self.maybe_enclosing_declaration();
            self.set_enclosing_declaration(Some(input));
        }

        let can_prodice_diagnostic = can_produce_diagnostics(&input.ref_(self));
        let old_diag = self.get_symbol_accessibility_diagnostic();
        if can_prodice_diagnostic {
            self.set_get_symbol_accessibility_diagnostic(
                create_get_symbol_accessibility_diagnostic_for_node(input, self),
            );
        }

        let previous_needs_declare = self.needs_declare();
        Ok(match released!(input.ref_(self).kind()) {
            SyntaxKind::TypeAliasDeclaration => self.transform_top_level_declaration_cleanup(
                input,
                previous_enclosing_declaration,
                can_prodice_diagnostic,
                &old_diag,
                previous_needs_declare,
                Some(self.factory.ref_(self).update_type_alias_declaration(
                    input,
                    Option::<Id<NodeArray>>::None,
                    self.ensure_modifiers(input),
                    released!(input.ref_(self).as_type_alias_declaration().name()),
                    try_maybe_visit_nodes(
                        released!(input
                                .ref_(self)
                                .as_type_alias_declaration()
                                .maybe_type_parameters()),
                        Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                        Some(|node: Id<Node>| is_type_parameter_declaration(&node.ref_(self))),
                        None,
                        None,
                        self,
                    )?,
                    try_visit_node(
                        released!(input
                                .ref_(self)
                                .as_type_alias_declaration()
                                .maybe_type()
                                .unwrap()),
                        Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                        Some(|node: Id<Node>| is_type_node(&node.ref_(self))),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )),
            ),
            SyntaxKind::InterfaceDeclaration => self.transform_top_level_declaration_cleanup(
                input,
                previous_enclosing_declaration,
                can_prodice_diagnostic,
                &old_diag,
                previous_needs_declare,
                Some(self.factory.ref_(self).update_interface_declaration(
                    input,
                    Option::<Id<NodeArray>>::None,
                    self.ensure_modifiers(input),
                    released!(input.ref_(self).as_interface_declaration().name()),
                    self.ensure_type_params(
                        input,
                        released!(input
                                .ref_(self)
                                .as_interface_declaration()
                                .maybe_type_parameters()),
                    )?,
                    Some(self.transform_heritage_clauses(released!(input
                                    .ref_(self)
                                    .as_interface_declaration()
                                    .maybe_heritage_clauses()))?),
                    try_visit_nodes(
                        released!(input.ref_(self).as_interface_declaration().members),
                        Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                        Option::<fn(Id<Node>) -> bool>::None,
                        None,
                        None,
                        self,
                    )?,
                )),
            ),
            SyntaxKind::FunctionDeclaration => {
                let clean = self.transform_top_level_declaration_cleanup(
                    input,
                    previous_enclosing_declaration,
                    can_prodice_diagnostic,
                    &old_diag,
                    previous_needs_declare,
                    Some(
                        self.factory.ref_(self).update_function_declaration(
                            input,
                            Option::<Id<NodeArray>>::None,
                            self.ensure_modifiers(input),
                            None,
                            released!(input.ref_(self).as_function_declaration().maybe_name()),
                            self.ensure_type_params(
                                input,
                                released!(input
                                    .ref_(self)
                                    .as_function_declaration()
                                    .maybe_type_parameters()),
                            )?,
                            self.update_params_list(
                                input,
                                released!(Some(
                                    input.ref_(self).as_function_declaration().parameters()
                                )),
                                None,
                            )?
                            .unwrap(),
                            self.ensure_type(
                                input,
                                released!(input.ref_(self).as_function_declaration().maybe_type()),
                                None,
                            )?,
                            None,
                        ),
                    ),
                );
                if let Some(clean) = clean.as_ref().try_filter(|_| -> io::Result<_> {
                    Ok(self
                        .resolver
                        .ref_(self)
                        .is_expando_function_declaration(input)?
                        && self.should_emit_function_properties(input))
                })? {
                    let clean = clean.as_single_node();
                    let clean_ref = clean.ref_(self);
                    let clean_as_function_declaration = clean_ref.as_function_declaration();
                    let props = self
                        .resolver
                        .ref_(self)
                        .get_properties_of_container_function(input)?;
                    let fakespace = get_parse_node_factory(self).create_module_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        clean_as_function_declaration
                            .maybe_name()
                            .unwrap_or_else(|| {
                                self.factory.ref_(self).create_identifier("_default")
                            }),
                        Some(self.factory.ref_(self).create_module_block(Some(vec![]))),
                        Some(NodeFlags::Namespace),
                    );
                    set_parent(&fakespace.ref_(self), self.maybe_enclosing_declaration());
                    fakespace.ref_(self).set_locals(Some(
                        self.alloc_symbol_table(create_symbol_table(Some(&props), self)),
                    ));
                    fakespace
                        .ref_(self)
                        .set_symbol(props[0].ref_(self).maybe_parent().unwrap());
                    let mut export_mappings: Vec<(Id<Node /*Identifier*/>, String)> =
                        Default::default();
                    let mut declarations = try_map_defined(
                        Some(&props),
                        |&p: &Id<Symbol>, _| -> io::Result<Option<Id<Node>>> {
                            let p_value_declaration = return_ok_default_if_none!(p
                                .ref_(self)
                                .maybe_value_declaration()
                                .filter(|p_value_declaration| {
                                    is_property_access_expression(&p_value_declaration.ref_(self))
                                }));
                            self.set_get_symbol_accessibility_diagnostic(
                                create_get_symbol_accessibility_diagnostic_for_node(
                                    p_value_declaration,
                                    self,
                                ),
                            );
                            let type_ = self.resolver.ref_(self).create_type_of_declaration(
                                p_value_declaration,
                                fakespace,
                                declaration_emit_node_builder_flags(),
                                self.symbol_tracker(),
                                None,
                            )?;
                            self.set_get_symbol_accessibility_diagnostic(old_diag.clone());
                            let p_ref = p.ref_(self);
                            let name_str = unescape_leading_underscores(p_ref.escaped_name());
                            let is_non_contextual_keyword_name =
                                is_string_a_non_contextual_keyword(name_str);
                            let name = if is_non_contextual_keyword_name {
                                self.factory
                                    .ref_(self)
                                    .get_generated_name_for_node(Some(p_value_declaration), None)
                            } else {
                                self.factory.ref_(self).create_identifier(name_str)
                            };
                            if is_non_contextual_keyword_name {
                                export_mappings.push((name.clone(), name_str.to_owned()));
                            }
                            let var_decl = self.factory.ref_(self).create_variable_declaration(
                                Some(name),
                                None,
                                type_,
                                None,
                            );
                            Ok(Some(
                                self.factory.ref_(self).create_variable_statement(
                                    if is_non_contextual_keyword_name {
                                        None
                                    } else {
                                        Some(vec![self
                                            .factory
                                            .ref_(self)
                                            .create_token(SyntaxKind::ExportKeyword)])
                                    },
                                    self.factory
                                        .ref_(self)
                                        .create_variable_declaration_list(vec![var_decl], None),
                                ),
                            ))
                        },
                    )?;
                    if export_mappings.is_empty() {
                        declarations =
                            map_defined(Some(&declarations), |&declaration: &Id<Node>, _| {
                                Some(
                                    self.factory
                                        .ref_(self)
                                        .update_modifiers(declaration, ModifierFlags::None),
                                )
                            });
                    } else {
                        declarations.push(self.factory.ref_(self).create_export_declaration(
                            Option::<Id<NodeArray>>::None,
                            Option::<Id<NodeArray>>::None,
                            false,
                            Some(self.factory.ref_(self).create_named_exports(map(
                                export_mappings,
                                |(gen, exp), _| {
                                    self.factory.ref_(self).create_export_specifier(
                                        false,
                                        Some(gen),
                                        &*exp,
                                    )
                                },
                            ))),
                            None,
                            None,
                        ));
                    }
                    let namespace_decl = self.factory.ref_(self).create_module_declaration(
                        Option::<Id<NodeArray>>::None,
                        self.ensure_modifiers(input),
                        input.ref_(self).as_function_declaration().name(),
                        Some(
                            self.factory
                                .ref_(self)
                                .create_module_block(Some(declarations)),
                        ),
                        Some(NodeFlags::Namespace),
                    );
                    if !has_effective_modifier(clean, ModifierFlags::Default, self) {
                        return Ok(Some(vec![clean.clone(), namespace_decl].into()));
                    }

                    let modifiers = self
                        .factory
                        .ref_(self)
                        .create_modifiers_from_modifier_flags(
                            (get_effective_modifier_flags(clean, self)
                                & !ModifierFlags::ExportDefault)
                                | ModifierFlags::Ambient,
                        );
                    let clean_declaration = self.factory.ref_(self).update_function_declaration(
                        clean,
                        Option::<Id<NodeArray>>::None,
                        Some(modifiers.clone()),
                        None,
                        clean_as_function_declaration.maybe_name(),
                        clean_as_function_declaration.maybe_type_parameters(),
                        clean_as_function_declaration.parameters(),
                        clean_as_function_declaration.maybe_type(),
                        None,
                    );

                    let namespace_decl_ref = namespace_decl.ref_(self);
                    let namespace_decl_as_module_declaration =
                        namespace_decl_ref.as_module_declaration();
                    let namespace_declaration = self.factory.ref_(self).update_module_declaration(
                        namespace_decl,
                        Option::<Id<NodeArray>>::None,
                        Some(modifiers),
                        namespace_decl_as_module_declaration.name(),
                        namespace_decl_as_module_declaration.body.clone(),
                    );

                    let export_default_declaration =
                        self.factory.ref_(self).create_export_assignment(
                            Option::<Id<NodeArray>>::None,
                            Option::<Id<NodeArray>>::None,
                            Some(false),
                            namespace_decl_as_module_declaration.name(),
                        );

                    if is_source_file(&input.ref_(self).parent().ref_(self)) {
                        self.set_result_has_external_module_indicator(true);
                    }
                    self.set_result_has_scope_marker(true);

                    Some(
                        vec![
                            clean_declaration,
                            namespace_declaration,
                            export_default_declaration,
                        ]
                        .into(),
                    )
                } else {
                    clean
                }
            }
            SyntaxKind::ModuleDeclaration => {
                let input_ref = input.ref_(self);
                let input_as_module_declaration = input_ref.as_module_declaration();
                self.set_needs_declare(false);
                let inner = input_as_module_declaration.body;
                if let Some(inner) =
                    inner.filter(|inner| inner.ref_(self).kind() == SyntaxKind::ModuleBlock)
                {
                    let old_needs_scope_fix = self.needs_scope_fix_marker();
                    let old_has_scope_fix = self.result_has_scope_marker();
                    self.set_result_has_scope_marker(false);
                    self.set_needs_scope_fix_marker(false);
                    let statements = try_visit_nodes(
                        inner.ref_(self).as_module_block().statements,
                        Some(|node: Id<Node>| self.visit_declaration_statements(node)),
                        Option::<fn(Id<Node>) -> bool>::None,
                        None,
                        None,
                        self,
                    )?;
                    let mut late_statements =
                        self.transform_and_replace_late_painted_statements(statements)?;
                    if input.ref_(self).flags().intersects(NodeFlags::Ambient) {
                        self.set_needs_scope_fix_marker(false);
                    }
                    if !is_global_scope_augmentation(&input.ref_(self))
                        && !self.has_scope_marker(late_statements)
                        && !self.result_has_scope_marker()
                    {
                        if self.needs_scope_fix_marker() {
                            late_statements = self.factory.ref_(self).create_node_array(
                                Some({
                                    let mut late_statements = late_statements.ref_(self).to_vec();
                                    late_statements
                                        .push(create_empty_exports(&self.factory.ref_(self)));
                                    late_statements
                                }),
                                None,
                            );
                        } else {
                            late_statements = visit_nodes(
                                late_statements,
                                Some(|node: Id<Node>| {
                                    Some(self.strip_export_modifiers(node).into())
                                }),
                                Option::<fn(Id<Node>) -> bool>::None,
                                None,
                                None,
                                self,
                            );
                        }
                    }
                    let body = self
                        .factory
                        .ref_(self)
                        .update_module_block(inner, late_statements);
                    self.set_needs_declare(previous_needs_declare);
                    self.set_needs_scope_fix_marker(old_needs_scope_fix);
                    self.set_result_has_scope_marker(old_has_scope_fix);
                    let mods = self.ensure_modifiers(input);
                    self.transform_top_level_declaration_cleanup(
                        input,
                        previous_enclosing_declaration,
                        can_prodice_diagnostic,
                        &old_diag,
                        previous_needs_declare,
                        Some(self.factory.ref_(self).update_module_declaration(
                            input,
                            Option::<Id<NodeArray>>::None,
                            mods,
                            if is_external_module_augmentation(input, self) {
                                self.rewrite_module_specifier(
                                    input,
                                    input_as_module_declaration.maybe_name(),
                                )?
                                .unwrap()
                            } else {
                                input_as_module_declaration.name()
                            },
                            Some(body),
                        )),
                    )
                } else {
                    self.set_needs_declare(previous_needs_declare);
                    let mods = self.ensure_modifiers(input);
                    self.set_needs_declare(false);
                    try_maybe_visit_node(
                        inner,
                        Some(|node: Id<Node>| self.visit_declaration_statements(node)),
                        Option::<fn(Id<Node>) -> bool>::None,
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?;
                    let id = maybe_get_original_node_id(inner, self);
                    let body = self
                        .late_statement_replacement_map_mut()
                        .remove(&id)
                        .flatten();
                    self.transform_top_level_declaration_cleanup(
                        input,
                        previous_enclosing_declaration,
                        can_prodice_diagnostic,
                        &old_diag,
                        previous_needs_declare,
                        Some(self.factory.ref_(self).update_module_declaration(
                            input,
                            Option::<Id<NodeArray>>::None,
                            mods,
                            input_as_module_declaration.name(),
                            body.as_ref().map(SingleNodeOrVecNode::as_single_node),
                        )),
                    )
                }
            }
            SyntaxKind::ClassDeclaration => {
                self.set_error_name_node(input.ref_(self).as_class_declaration().maybe_name());
                self.set_error_fallback_node(Some(input));
                let modifiers = self
                    .factory
                    .ref_(self)
                    .create_node_array(self.ensure_modifiers(input), None);
                let type_parameters = self.ensure_type_params(
                    input,
                    input
                        .ref_(self)
                        .as_class_declaration()
                        .maybe_type_parameters(),
                )?;
                let ctor = get_first_constructor_with_body(input, self);
                let mut parameter_properties: Option<Vec<Id<Node /*PropertyDeclaration*/>>> =
                    Default::default();
                if let Some(ref ctor) = ctor {
                    let old_diag = self.get_symbol_accessibility_diagnostic();
                    let ctor_ref = ctor.ref_(self);
                    let ctor_as_constructor_declaration = ctor_ref.as_constructor_declaration();
                    parameter_properties = Some(/*compact(*/ try_flat_map(
                        Some(&*ctor_as_constructor_declaration.parameters().ref_(self)),
                        |&param: &Id<Node>, _| -> io::Result<_> {
                            if !has_syntactic_modifier(
                                param,
                                ModifierFlags::ParameterPropertyModifier,
                                self,
                            ) || self.should_strip_internal(param)
                            {
                                return Ok(vec![]);
                            }
                            self.set_get_symbol_accessibility_diagnostic(
                                create_get_symbol_accessibility_diagnostic_for_node(param, self),
                            );
                            let param_ref = param.ref_(self);
                            let param_as_parameter_declaration =
                                param_ref.as_parameter_declaration();
                            Ok(
                                if param_as_parameter_declaration.name().ref_(self).kind()
                                    == SyntaxKind::Identifier
                                {
                                    vec![self.preserve_js_doc(
                                        self.factory.ref_(self).create_property_declaration(
                                            Option::<Id<NodeArray>>::None,
                                            self.ensure_modifiers(param),
                                            param_as_parameter_declaration.name(),
                                            param_as_parameter_declaration.maybe_question_token(),
                                            self.ensure_type(
                                                param,
                                                param_as_parameter_declaration.maybe_type(),
                                                None,
                                            )?,
                                            self.ensure_no_initializer(param)?,
                                        ),
                                        param,
                                    )]
                                } else {
                                    self.walk_binding_pattern(
                                        param,
                                        param_as_parameter_declaration.name(),
                                    )?
                                    .unwrap_or_default()
                                },
                            )
                        },
                    )? /*)*/);
                    self.set_get_symbol_accessibility_diagnostic(old_diag);
                }

                let has_private_identifier = some(
                    Some(&*input.ref_(self).as_class_declaration().members().ref_(self)),
                    Some(|member: &Id<Node>| {
                        matches!(
                            member.ref_(self).as_named_declaration().maybe_name(),
                            Some(member_name) if is_private_identifier(&member_name.ref_(self))
                        )
                    }),
                );
                let private_identifier = if has_private_identifier {
                    Some(vec![self.factory.ref_(self).create_property_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        self.factory
                            .ref_(self)
                            .create_private_identifier("#private"),
                        None,
                        None,
                        None,
                    )])
                } else {
                    None
                };
                let member_nodes = maybe_concatenate(
                    maybe_concatenate(private_identifier, parameter_properties),
                    try_maybe_visit_nodes(
                        released!(Some(input.ref_(self).as_class_declaration().members())),
                        Some(|node: Id<Node>| self.visit_declaration_subtree(node)),
                        Option::<fn(Id<Node>) -> bool>::None,
                        None,
                        None,
                        self,
                    )?
                    .map(|node_array| node_array.ref_(self).to_vec()),
                );
                let members = self
                    .factory
                    .ref_(self)
                    .create_node_array(member_nodes, None);

                let extends_clause = get_effective_base_type_node(input, self);
                if let Some(extends_clause) = extends_clause.filter(|extends_clause| {
                    let extends_clause_ref = extends_clause.ref_(self);
                    let extends_clause_as_expression_with_type_arguments =
                        extends_clause_ref.as_expression_with_type_arguments();
                    !is_entity_name_expression(
                        extends_clause_as_expression_with_type_arguments.expression,
                        self,
                    ) && extends_clause_as_expression_with_type_arguments
                        .expression
                        .ref_(self)
                        .kind()
                        != SyntaxKind::NullKeyword
                }) {
                    let old_id = if let Some(input_name) =
                        input.ref_(self).as_class_declaration().maybe_name()
                    {
                        unescape_leading_underscores(
                            &input_name.ref_(self).as_identifier().escaped_text,
                        )
                        .to_owned()
                    } else {
                        "default".to_owned()
                    };
                    let new_id = self.factory.ref_(self).create_unique_name(
                        &format!("{old_id}_base"),
                        Some(GeneratedIdentifierFlags::Optimistic),
                    );
                    self.set_get_symbol_accessibility_diagnostic(
                        TransformTopLevelDeclarationGetSymbolAccessibilityDiagnostic::new(
                            extends_clause,
                            input,
                            self,
                        ),
                    );
                    let var_decl = self.factory.ref_(self).create_variable_declaration(
                        Some(new_id.clone()),
                        None,
                        self.resolver.ref_(self).create_type_of_expression(
                            extends_clause
                                .ref_(self)
                                .as_expression_with_type_arguments()
                                .expression,
                            input,
                            declaration_emit_node_builder_flags(),
                            self.symbol_tracker(),
                        )?,
                        None,
                    );
                    let statement = self.factory.ref_(self).create_variable_statement(
                        if self.needs_declare() {
                            Some(vec![self
                                .factory
                                .ref_(self)
                                .create_modifier(SyntaxKind::DeclareKeyword)])
                        } else {
                            Some(vec![])
                        },
                        self.factory.ref_(self).create_variable_declaration_list(
                            vec![var_decl],
                            Some(NodeFlags::Const),
                        ),
                    );
                    let heritage_clauses = self.factory.ref_(self).create_node_array(
                        try_maybe_map(
                            input
                                .ref_(self)
                                .as_class_declaration()
                                .maybe_heritage_clauses()
                                .refed(self)
                                .as_deref(),
                            |&clause: &Id<Node>, _| -> io::Result<_> {
                                let clause_ref = clause.ref_(self);
                                let clause_as_heritage_clause = clause_ref.as_heritage_clause();
                                if clause_as_heritage_clause.token == SyntaxKind::ExtendsKeyword {
                                    let old_diag = self.get_symbol_accessibility_diagnostic();
                                    self.set_get_symbol_accessibility_diagnostic(
                                        create_get_symbol_accessibility_diagnostic_for_node(
                                            clause_as_heritage_clause.types.ref_(self)[0],
                                            self,
                                        ),
                                    );
                                    let new_clause =
                                        self.factory.ref_(self).update_heritage_clause(
                                            clause,
                                            try_map(
                                                &*clause_as_heritage_clause.types.ref_(self),
                                                |&t: &Id<Node>, _| -> io::Result<_> {
                                                    Ok(self
                                                        .factory
                                                        .ref_(self)
                                                        .update_expression_with_type_arguments(
                                                        t,
                                                        new_id.clone(),
                                                        try_maybe_visit_nodes(
                                                            t.ref_(self)
                                                                .as_expression_with_type_arguments()
                                                                .maybe_type_arguments(),
                                                            Some(|node: Id<Node>| {
                                                                self.visit_declaration_subtree(node)
                                                            }),
                                                            Option::<fn(Id<Node>) -> bool>::None,
                                                            None,
                                                            None,
                                                            self,
                                                        )?,
                                                    ))
                                                },
                                            )?,
                                        );
                                    self.set_get_symbol_accessibility_diagnostic(old_diag);
                                    return Ok(new_clause);
                                }
                                Ok(self.factory.ref_(self).update_heritage_clause(
                                    clause,
                                    try_visit_nodes(
                                        self.factory.ref_(self).create_node_array(
                                            Some(
                                                clause_as_heritage_clause
                                                    .types
                                                    .ref_(self)
                                                    .iter()
                                                    .filter(|t| {
                                                        let t_ref = t.ref_(self);
                                                        let t_as_expression_with_type_arguments =
                                                            t_ref
                                                                .as_expression_with_type_arguments(
                                                                );
                                                        is_entity_name_expression(
                                                            t_as_expression_with_type_arguments
                                                                .expression,
                                                            self,
                                                        ) || t_as_expression_with_type_arguments
                                                            .expression
                                                            .ref_(self)
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
                                        self,
                                    )?,
                                ))
                            },
                        )
                        .transpose()?,
                        None,
                    );
                    Some(
                        vec![
                            statement,
                            self.transform_top_level_declaration_cleanup(
                                input,
                                previous_enclosing_declaration,
                                can_prodice_diagnostic,
                                &old_diag,
                                previous_needs_declare,
                                Some(self.factory.ref_(self).update_class_declaration(
                                    input,
                                    Option::<Id<NodeArray>>::None,
                                    Some(modifiers),
                                    input.ref_(self).as_class_declaration().maybe_name(),
                                    type_parameters,
                                    Some(heritage_clauses),
                                    members,
                                )),
                            )
                            .into_single_node(),
                        ]
                        .into(),
                    )
                } else {
                    let heritage_clauses = self.transform_heritage_clauses(released!(input
                        .ref_(self)
                        .as_class_declaration()
                        .maybe_heritage_clauses()))?;
                    self.transform_top_level_declaration_cleanup(
                        input,
                        previous_enclosing_declaration,
                        can_prodice_diagnostic,
                        &old_diag,
                        previous_needs_declare,
                        Some(self.factory.ref_(self).update_class_declaration(
                            input,
                            Option::<Id<NodeArray>>::None,
                            Some(modifiers),
                            released!(input.ref_(self).as_class_declaration().maybe_name()),
                            type_parameters,
                            Some(heritage_clauses),
                            members,
                        )),
                    )
                }
            }
            SyntaxKind::VariableStatement => self.transform_top_level_declaration_cleanup(
                input,
                previous_enclosing_declaration,
                can_prodice_diagnostic,
                &old_diag,
                previous_needs_declare,
                self.transform_variable_statement(input)?,
            ),
            SyntaxKind::EnumDeclaration => {
                let input_ref = input.ref_(self);
                let input_as_enum_declaration = input_ref.as_enum_declaration();
                self.transform_top_level_declaration_cleanup(
                    input,
                    previous_enclosing_declaration,
                    can_prodice_diagnostic,
                    &old_diag,
                    previous_needs_declare,
                    Some(
                        self.factory.ref_(self).update_enum_declaration(
                            input,
                            Option::<Id<NodeArray>>::None,
                            Some(
                                self.factory
                                    .ref_(self)
                                    .create_node_array(self.ensure_modifiers(input), None),
                            ),
                            input_as_enum_declaration.name(),
                            Some(self.factory.ref_(self).create_node_array(
                                Some(try_map_defined(
                                    Some(&*input_as_enum_declaration.members.ref_(self)),
                                    |&m: &Id<Node>, _| -> io::Result<_> {
                                        if self.should_strip_internal(m) {
                                            return Ok(None);
                                        }
                                        let const_value =
                                            self.resolver.ref_(self).get_constant_value(m)?;
                                        Ok(Some(self.preserve_js_doc(
                                            self.factory.ref_(self).update_enum_member(
                                                m,
                                                m.ref_(self).as_enum_member().name(),
                                                const_value.map(|const_value| {
                                                    match const_value {
                                                        StringOrNumber::String(const_value) => self
                                                            .factory
                                                            .ref_(self)
                                                            .create_string_literal(
                                                                const_value,
                                                                None,
                                                                None,
                                                            ),
                                                        StringOrNumber::Number(const_value) => self
                                                            .factory
                                                            .ref_(self)
                                                            .create_numeric_literal(
                                                                const_value,
                                                                None,
                                                            ),
                                                    }
                                                }),
                                            ),
                                            m,
                                        )))
                                    },
                                )?),
                                None,
                            )),
                        ),
                    ),
                )
            }
            _ => Debug_.assert_never(
                input,
                Some(&format!(
                    "Unhandled top-level node in declaration emit: {:?}",
                    input.ref_(self).kind()
                )),
            ),
        })
    }

    pub(super) fn walk_binding_pattern(
        &self,
        param: Id<Node>,
        pattern: Id<Node>, /*BindingPattern*/
    ) -> io::Result<Option<Vec<Id<Node>>>> {
        let mut elems: Option<Vec<Id<Node /*PropertyDeclaration*/>>> = Default::default();
        for &elem in &*pattern.ref_(self).as_has_elements().elements().ref_(self) {
            let elem_ref = elem.ref_(self);
            let elem_as_binding_element = elem_ref.as_binding_element();
            if is_omitted_expression(&elem.ref_(self)) {
                continue;
            }
            if is_binding_pattern(elem_as_binding_element.maybe_name().refed(self).as_deref()) {
                elems = maybe_concatenate(
                    elems,
                    self.walk_binding_pattern(param, elem_as_binding_element.name())?,
                );
            }
            elems.get_or_insert_default_().push(
                self.factory.ref_(self).create_property_declaration(
                    Option::<Id<NodeArray>>::None,
                    self.ensure_modifiers(param),
                    elem_as_binding_element.name(),
                    None,
                    self.ensure_type(elem, None, None)?,
                    None,
                ),
            );
        }
        Ok(elems)
    }

    pub(super) fn transform_top_level_declaration_cleanup(
        &self,
        input: Id<Node>,
        previous_enclosing_declaration: Option<Id<Node>>,
        can_prodice_diagnostic: bool,
        old_diag: &GetSymbolAccessibilityDiagnostic,
        previous_needs_declare: bool,
        node: Option<Id<Node>>,
    ) -> VisitResult {
        if self.is_enclosing_declaration(input) {
            self.set_enclosing_declaration(previous_enclosing_declaration);
        }
        if can_prodice_diagnostic {
            self.set_get_symbol_accessibility_diagnostic(old_diag.clone());
        }
        if input.ref_(self).kind() == SyntaxKind::ModuleDeclaration {
            self.set_needs_declare(previous_needs_declare);
        }
        if node == Some(input) {
            return node.map(Into::into);
        }
        self.set_error_fallback_node(None);
        self.set_error_name_node(None);
        node.map(|node| {
            set_original_node(self.preserve_js_doc(node, input), Some(input), self).into()
        })
    }
}

struct VisitDeclarationStatementsGetSymbolAccessibilityDiagnostic {
    arena: *const AllArenas,
    input: Id<Node>,
}

impl VisitDeclarationStatementsGetSymbolAccessibilityDiagnostic {
    fn new(input: Id<Node>, arena: &impl HasArena) -> GetSymbolAccessibilityDiagnostic {
        arena.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(Self {
            input,
            arena: arena.arena(),
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface
    for VisitDeclarationStatementsGetSymbolAccessibilityDiagnostic
{
    fn call(
        &self,
        _symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        Some(
            self.alloc_symbol_accessibility_diagnostic(SymbolAccessibilityDiagnostic {
                diagnostic_message:
                    &Diagnostics::Default_export_of_the_module_has_or_is_using_private_name_0,
                error_node: self.input.clone(),
                type_name: None,
            }),
        )
    }
}

impl_has_arena!(VisitDeclarationStatementsGetSymbolAccessibilityDiagnostic);

struct TransformTopLevelDeclarationGetSymbolAccessibilityDiagnostic {
    arena: *const AllArenas,
    extends_clause: Id<Node>,
    input: Id<Node>,
}

impl TransformTopLevelDeclarationGetSymbolAccessibilityDiagnostic {
    fn new(
        extends_clause: Id<Node>,
        input: Id<Node>,
        arena: &impl HasArena,
    ) -> GetSymbolAccessibilityDiagnostic {
        arena.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(Self {
            arena: arena.arena(),
            extends_clause,
            input,
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface
    for TransformTopLevelDeclarationGetSymbolAccessibilityDiagnostic
{
    fn call(
        &self,
        _symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        Some(
            self.alloc_symbol_accessibility_diagnostic(SymbolAccessibilityDiagnostic {
                diagnostic_message:
                    &Diagnostics::extends_clause_of_exported_class_0_has_or_is_using_private_name_1,
                error_node: self.extends_clause.clone(),
                type_name: self.input.ref_(self).as_class_declaration().maybe_name(),
            }),
        )
    }
}

impl_has_arena!(TransformTopLevelDeclarationGetSymbolAccessibilityDiagnostic);
